use bumpalo::Bump;
use clap::Parser as ClapParser;
use fzr::{Haystack, Memory, Pattern, Score, find_exact, find_fuzzy, parse_haystack};
use std::{
    cmp::{Ord, Ordering, Reverse},
    fs::File,
    io::{self, BufRead, BufReader, BufWriter, Write},
    ops::{Deref, Range},
    process::Command,
    time::{Duration, Instant},
};
use termwiz::{
    caps::Capabilities,
    cell::{Cell, CellAttributes},
    color::ColorAttribute,
    escape::{
        Action, ControlCode,
        csi::{CSI, Sgr},
        parser::Parser,
    },
    input::{InputEvent, KeyCode, KeyEvent, Modifiers, MouseButtons, MouseEvent},
    surface::{Change, CursorShape, Line, Position, SequenceNo, Surface},
    terminal::{Terminal, buffered::BufferedTerminal},
};

#[derive(ClapParser, Debug)]
#[command(version)]
struct Cli {
    /// Initial query.
    #[arg(short, long, default_value = "")]
    query: String,

    /// Input prompt.
    #[arg(short = 'p', long, default_value = "> ")]
    prompt: String,

    /// Header.
    #[arg(long)]
    header: Vec<String>,

    /// Treat the first N lines as header.
    #[arg(long, value_name = "N", default_value = "0")]
    header_lines: usize,

    /// Automatically accept the only match for the initial query.
    #[arg(short = '1', long)]
    select_1: bool,

    /// Automatically accept the only match for an interactive query.
    #[arg(short = 'a', long)]
    interactive_select_1: bool,

    /// Exit immediately if there is no match for the initial query.
    #[arg(short = '0', long)]
    exit_0: bool,

    /// Accept all matches for the initial query.
    #[arg(short, long)]
    filter: bool,

    /// Read lines delimited by ASCII NUL.
    #[arg(long)]
    read0: bool,

    /// Print lines delimited by ASCII NUL.
    #[arg(long)]
    print0: bool,

    /// Print query as the first line.
    #[arg(long)]
    print_query: bool,

    /// Print 0-based indices of accepted lines.
    #[arg(short = 'i', long)]
    print_index: bool,

    /// Sort (default).
    #[arg(short = 's', long, overrides_with = "sort", overrides_with = "no_sort")]
    sort: bool,

    /// Do not sort.
    #[arg(short = 'S', long, overrides_with = "no_sort")]
    no_sort: bool,

    /// Reverse line order and show prompt at the bottom.
    #[arg(short = 'r', long = "reverse")]
    reverse: bool,
}

fn main() {
    let cli = Cli::parse();

    let read_delim = if cli.read0 { b'\0' } else { b'\n' };
    let print_delim = if cli.print0 { b'\0' } else { b'\n' };
    let sort = !cli.no_sort;

    let mut buf = BufReader::new(io::stdin().lock());

    let headers = {
        let mut headers = cli.header;
        headers.extend((0..cli.header_lines).map_while(|_| {
            let mut v = Vec::new();
            buf.read_until(read_delim, &mut v).ok()?;
            Some(String::from_utf8_lossy(&v).into_owned())
        }));
        headers
    };

    let mut searcher = Searcher::new(Haystacks::parse(buf, read_delim));

    let mut query = cli.query;

    searcher.find(&Needle::parse(&query), sort, false);

    let accept = if cli.exit_0 && searcher.matches_len() == 0 {
        0..0
    } else if cli.select_1 && searcher.matches_len() == 1 {
        0..1
    } else if cli.filter {
        0..searcher.matches_len()
    } else {
        interactive(
            cli.prompt,
            &mut query,
            &mut searcher,
            sort,
            headers,
            cli.reverse,
            cli.interactive_select_1,
        )
    };

    let mut stdout = BufWriter::new(io::stdout().lock());

    macro_rules! print {
        ($expr:expr) => {
            write!(stdout, "{}{}", $expr, char::from(print_delim)).unwrap();
        };
    }

    if cli.print_query {
        print!(query);
    }

    for i in accept.clone() {
        if cli.print_index {
            print!(searcher.get_index(i));
        } else {
            print!(searcher.get_str(i));
        }
    }

    stdout.flush().unwrap();

    std::process::exit(if accept.is_empty() { 1 } else { 0 })
}

fn dev_tty() -> File {
    let opts = {
        let mut opts = std::fs::OpenOptions::new();
        opts.read(true).write(true);
        opts
    };

    opts.open("/dev/tty")
        .or_else(|_| opts.open("/dev/stderr"))
        .expect("terminal device available at /dev/tty")
}

fn interactive(
    prompt: String,
    query: &mut String,
    searcher: &mut Searcher,
    sort: bool,
    headers: Vec<String>,
    reverse: bool,
    select_1: bool,
) -> Range<usize> {
    let mut prev_query = query.clone();
    let mut cur = 0;
    let mut last_click = None;

    let caps = Capabilities::new_from_env().unwrap();

    let mut terminal = {
        #[cfg(target_os = "windows")]
        {
            termwiz::terminal::new_terminal(caps).unwrap()
        }

        #[cfg(not(target_os = "windows"))]
        {
            let file = dev_tty();
            termwiz::terminal::UnixTerminal::new_with(caps, &file, &file).unwrap()
        }
    };

    terminal.set_raw_mode().unwrap();
    terminal.enter_alternate_screen().unwrap();

    let mut buf = BufferedTerminal::new(terminal).unwrap();
    let mut front = Surface::new(buf.width(), buf.height());
    let mut repaint = true;

    let normal = CellAttributes::default();

    let selected = {
        let mut selected = CellAttributes::default();
        selected.set_reverse(true);
        selected
    };

    buf.add_change(Change::CursorShape(CursorShape::SteadyBar));

    loop {
        if searcher.is_dirty() || *query != prev_query {
            let needle = Needle::parse(query);
            let shrink = !searcher.is_dirty()
                && query.starts_with(&prev_query)
                && needle.is_sequential_shrink();

            searcher.find(&needle, sort, shrink);

            if select_1 && searcher.matches_len() == 1 {
                return 0..1;
            }

            prev_query.clone_from(query);
            cur = 0;
        }

        front.add_change(Change::ClearScreen(ColorAttribute::Default));

        let cmdline = Line::from_text(
            &format!("{prompt}{query}"),
            &normal,
            front.current_seqno(),
            None,
        );
        let cmdline_x = cmdline.len();

        let mut y = (0..front.height()).rev_if(reverse);

        let cmdline_y = y.next().unwrap_or(0);
        front.set_cursor(cmdline_y, 0);
        front.set_line_nowrap(cmdline);

        if let Some(y) = y.next() {
            front.set_cursor(y, 0);
            front.set_line_from_text(
                &format!("[{}/{}]", searcher.matches_len(), searcher.len()),
                &normal,
            );
        }

        for (header, y) in headers.iter().zip(y.by_ref()) {
            front.set_cursor(y, 0);
            front.set_line_from_sgr_text(header.as_bytes(), &normal);
        }

        let item_ys = y.clone();

        for (i, y) in (0..searcher.matches_len()).zip(y.by_ref()) {
            front.set_cursor(y, 0);
            front.set_line_from_sgr_text(
                searcher.get_str(i).as_bytes(),
                if i == cur { &selected } else { &normal },
            );
        }

        for y in y {
            front.set_cursor(y, 0);
            front.set_line_from_text("~", &normal);
        }

        buf.draw_from_screen(&front, 0, 0);
        buf.set_cursor(cmdline_y, cmdline_x);

        if repaint {
            buf.repaint().unwrap();
        } else {
            buf.flush().unwrap();
        }
        repaint = false;

        enum Action {
            Accept,
            AcceptAll,
            AcceptNone,
            Select(usize),
            SelectNext,
            SelectPrev,
            InsertChar(char),
            BackwardDeleteChar,
            BackwardDeleteWord,
            BackwardDeleteLine,
            Edit,
            Redraw,
        }

        let action = loop {
            let Some(event) = buf.terminal().poll_input(None).unwrap() else {
                break Action::AcceptNone;
            };

            use KeyCode as K;
            use Modifiers as M;

            break match event {
                InputEvent::Key(KeyEvent { modifiers, key }) => match (modifiers, key) {
                    (M::ALT, K::Char('j')) | (M::NONE, K::DownArrow) => {
                        if reverse {
                            Action::SelectPrev
                        } else {
                            Action::SelectNext
                        }
                    }
                    (M::ALT, K::Char('k')) | (M::NONE, K::UpArrow) => {
                        if reverse {
                            Action::SelectNext
                        } else {
                            Action::SelectPrev
                        }
                    }
                    (M::ALT, K::Char('v')) => Action::Edit,
                    (M::ALT, K::Enter) => Action::AcceptAll,
                    (M::CTRL, K::Char('c')) => Action::AcceptNone,
                    (M::CTRL, K::Char('g')) => Action::AcceptNone,
                    (M::CTRL, K::Char('l')) => Action::Redraw,
                    (M::CTRL, K::Char('n')) => Action::SelectNext,
                    (M::CTRL, K::Char('p')) => Action::SelectPrev,
                    (M::CTRL, K::Char('q')) => Action::AcceptNone,
                    (M::CTRL, K::Char('u')) => Action::BackwardDeleteLine,
                    (M::CTRL, K::Char('v')) => Action::Edit,
                    (M::CTRL, K::Char('w')) => Action::BackwardDeleteWord,
                    (M::NONE, K::Backspace) => Action::BackwardDeleteChar,
                    (M::NONE, K::Char(c)) => Action::InsertChar(c),
                    (M::NONE, K::Enter) => Action::Accept,
                    (M::NONE, K::Escape) => Action::AcceptNone,
                    _ => continue,
                },
                InputEvent::Mouse(MouseEvent { mouse_buttons, .. })
                    if mouse_buttons.contains(MouseButtons::VERT_WHEEL) =>
                {
                    if mouse_buttons.contains(MouseButtons::WHEEL_POSITIVE) == reverse {
                        Action::SelectNext
                    } else {
                        Action::SelectPrev
                    }
                }
                InputEvent::Mouse(MouseEvent {
                    y, mouse_buttons, ..
                }) if mouse_buttons.contains(MouseButtons::LEFT) => {
                    let mouse_y = usize::from(y) - 1;
                    let Some(i) = item_ys
                        .clone()
                        .rev_if(reverse)
                        .take(searcher.matches_len())
                        .position(|y| y == mouse_y)
                    else {
                        continue;
                    };
                    let now = Instant::now();
                    let fast = last_click
                        .is_some_and(|x| now.duration_since(x) <= Duration::from_millis(500));
                    last_click = Some(now);
                    if i == cur && fast {
                        Action::Accept
                    } else {
                        Action::Select(i)
                    }
                }
                InputEvent::Resized { cols, rows } => {
                    buf.resize(cols, rows);
                    front.resize(cols, rows);
                    Action::Redraw
                }
                _ => continue,
            };
        };

        match action {
            Action::Accept => {
                return if cur < searcher.matches_len() {
                    cur..cur + 1
                } else {
                    0..0
                };
            }
            Action::AcceptAll => {
                return 0..searcher.matches_len();
            }
            Action::AcceptNone => return 0..0,
            Action::Select(i) => {
                cur = i;
            }
            Action::SelectNext => {
                if cur + 1 < searcher.matches_len() {
                    cur += 1;
                }
            }
            Action::SelectPrev => {
                cur = cur.saturating_sub(1);
            }
            Action::InsertChar(c) => {
                query.push(c);
            }
            Action::BackwardDeleteChar => {
                query.pop();
            }
            Action::BackwardDeleteWord => {
                *query = query
                    .as_str()
                    .trim_end_matches(|c| !is_wordchar(c))
                    .trim_end_matches(is_wordchar)
                    .to_string();
            }
            Action::BackwardDeleteLine => {
                query.clear();
            }
            Action::Edit => {
                buf.terminal().set_cooked_mode().unwrap();
                buf.terminal().exit_alternate_screen().unwrap();

                editor(query, searcher, cur);

                buf.check_for_resize().unwrap();
                buf.terminal().set_raw_mode().unwrap();
                buf.terminal().enter_alternate_screen().unwrap();
                buf.repaint().unwrap();
            }
            Action::Redraw => {
                repaint = true;
            }
        }
    }
}

fn editor(query: &mut String, searcher: &mut Searcher, cur: usize) {
    let (file, path) = tempfile::NamedTempFile::with_prefix("fzr")
        .unwrap()
        .into_parts();
    let mut stream = BufWriter::new(file);

    for i in 0..searcher.matches_len() {
        writeln!(stream, "{}", searcher.get_str(i)).unwrap();
    }

    stream.flush().unwrap();

    if !Command::new("sh")
        .stdin(dev_tty())
        .stdout(dev_tty())
        .arg("-c")
        .arg(format!(
            "{} +{} -- \"$1\"",
            std::env::var("EDITOR").as_deref().unwrap_or("vi"),
            cur + 1
        ))
        .arg("sh")
        .arg(path.as_os_str())
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .success()
    {
        return;
    }

    let buf = BufReader::new(File::open(path).unwrap());
    *searcher = Searcher::new(Haystacks::parse(buf, b'\n'));
    query.clear();
}

fn is_wordchar(c: char) -> bool {
    c.is_alphanumeric()
}

enum Needle {
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Not(Box<Self>),
    Fuzzy(Pattern),
    Exact {
        pat: Pattern,
        anchor_start: bool,
        anchor_end: bool,
    },
    Always,
    Never,
}

impl Needle {
    fn find(&self, haystack: &Haystack, memory: &mut Memory) -> Option<Score> {
        match &self {
            Needle::And(left, right) => {
                let left = left.find(haystack, memory)?;
                let right = right.find(haystack, memory)?;
                Some(left + right)
            }
            Needle::Or(left, right) => {
                let left = left.find(haystack, memory);
                let right = right.find(haystack, memory);

                match (left, right) {
                    (Some(left), Some(right)) => Some(left.max(right)),
                    (x, None) | (None, x) => x,
                }
            }
            Needle::Not(child) => {
                if child.find(haystack, memory).is_none() {
                    Some(Score::exact_match())
                } else {
                    None
                }
            }
            Needle::Fuzzy(pat) => find_fuzzy(haystack, pat, memory).map(|m| m.score()),
            Needle::Exact {
                pat,
                anchor_start,
                anchor_end,
            } => find_exact(haystack, pat, *anchor_start, *anchor_end, memory).map(|m| m.score()),
            Needle::Always => Some(Score::exact_match()),
            Needle::Never => None,
        }
    }

    fn is_always(&self) -> bool {
        matches!(&self, Needle::Always)
    }

    fn parse(s: &str) -> Needle {
        fn prefix(s: &str, c: char) -> (&str, bool) {
            match s.strip_prefix(c) {
                Some(s) => (s, true),
                None => (s, false),
            }
        }

        fn suffix(s: &str, c: char) -> (&str, bool) {
            match s.strip_suffix(c) {
                Some(s) => (s, true),
                None => (s, false),
            }
        }

        fn atom(s: &str) -> (&str, Needle) {
            let (s, not) = prefix(s, '!');
            let (s, exact) = prefix(s, '\'');
            let (s, anchor_start) = prefix(s, '^');
            let n = s.find([' ', '|']).unwrap_or(s.len());
            let (pat, s) = s.split_at(n);
            let (pat, anchor_end) = suffix(pat, '$');

            if pat.is_empty() {
                return (s, Needle::Always);
            }

            let Ok(pat) = Pattern::new(pat) else {
                return (s, Needle::Never);
            };

            if exact || anchor_start || anchor_end || not {
                let needle = Needle::Exact {
                    pat,
                    anchor_start,
                    anchor_end,
                };
                let needle = if not {
                    Needle::Not(Box::new(needle))
                } else {
                    needle
                };
                (s, needle)
            } else {
                (s, Needle::Fuzzy(pat))
            }
        }

        fn or(s: &str) -> (&str, Needle) {
            let s = s.trim_start_matches(' ');
            let (mut s, mut lhs) = atom(s);

            loop {
                (s, lhs) = {
                    let Some(s) = s.strip_prefix('|') else {
                        return (s, lhs);
                    };
                    let (s, rhs) = or(s);
                    (s, Needle::Or(Box::new(lhs), Box::new(rhs)))
                };
            }
        }

        fn and(s: &str) -> (&str, Needle) {
            let (mut s, mut lhs) = or(s);

            loop {
                if s.is_empty() {
                    return (s, lhs);
                }

                (s, lhs) = {
                    let (s, rhs) = and(s);
                    (s, Needle::And(Box::new(lhs), Box::new(rhs)))
                };
            }
        }

        and(s).1
    }

    fn is_sequential_shrink(&self) -> bool {
        match self {
            Needle::And(left, right) => left.is_sequential_shrink() && right.is_sequential_shrink(),
            Needle::Fuzzy(_) => true,
            Needle::Exact { .. } => true,
            Needle::Always => true,
            _ => false,
        }
    }
}

#[ouroboros::self_referencing]
struct Haystacks {
    bump: Bump,
    #[borrows(bump)]
    #[covariant]
    data: Vec<Haystack<'this>>,
}

impl Haystacks {
    fn parse<B>(mut buf: B, delim: u8) -> Self
    where
        B: BufRead,
    {
        HaystacksBuilder {
            bump: Bump::new(),
            data_builder: |bump| {
                let mut haystacks = Vec::with_capacity(128);
                let mut bytes = Vec::with_capacity(256);
                let mut tokens = Vec::with_capacity(64);

                while buf.read_until(delim, &mut bytes).is_ok() {
                    if bytes.is_empty() {
                        break;
                    } else if bytes[bytes.len() - 1] == delim {
                        bytes.pop();
                    }
                    if let Ok(s) = std::str::from_utf8(&bytes) {
                        tokens.clear();
                        parse_haystack(s, &mut tokens);

                        let value = &*bump.alloc_str(s);
                        let tokens = &*bump.alloc_slice_copy(tokens.as_slice());

                        // SAFETY: `tokens` derived from `value`.
                        haystacks.push(unsafe { Haystack::from_parts(value, tokens) });
                    }
                    bytes.clear();
                }

                haystacks
            },
        }
        .build()
    }
}

struct Searcher {
    haystacks: Haystacks,
    keys: Vec<HaystackKey>,
    matches_len: usize,
    memory: Memory,
    dirty: bool,
}

impl Searcher {
    fn new(haystacks: Haystacks) -> Self {
        Self {
            keys: {
                // Don't convert `Vec` into `Box` since that makes the code slower for some reason.
                (0..haystacks.borrow_data().len())
                    .map(|index| HaystackKey { score: None, index })
                    .collect()
            },
            haystacks,
            matches_len: 0,
            memory: Memory::new(),
            dirty: true,
        }
    }

    /// Returns total number of haystacks.
    ///
    /// It includes both matching and non-matching items.
    fn len(&self) -> usize {
        self.haystacks.borrow_data().len()
    }

    /// Returns number of matching haystacks.
    fn matches_len(&self) -> usize {
        self.matches_len
    }

    fn get_index(&self, visual_index: usize) -> usize {
        self.keys[visual_index].index
    }

    fn get_str(&self, visual_index: usize) -> &str {
        self.haystacks.borrow_data()[self.get_index(visual_index)].value()
    }

    /// Whether haystacks changed and a call to [`find`][Self::find] is required to make
    /// [`get_index`][Self::get_index]/[`get_str`][Self::get_str] and
    /// [`matches_len`][Self::matches_len] up-to-date.
    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn find(&mut self, needle: &Needle, sort: bool, shrink: bool) {
        self.matches_len = if needle.is_always() {
            self.keys.sort_unstable_by_key(HaystackKey::cmp_index);
            self.len()
        } else {
            let index = if shrink {
                0..self.matches_len()
            } else {
                0..self.len()
            };

            let haystacks = self.haystacks.borrow_data();
            let keys = &mut self.keys[index];

            if keys.len() < 4096 {
                for key in keys.iter_mut() {
                    let s = &haystacks[key.index].as_ref();
                    key.apply_match(needle.find(s, &mut self.memory));
                }

                if sort {
                    keys.sort_unstable_by_key(HaystackKey::cmp_score);
                } else {
                    keys.sort_unstable_by_key(HaystackKey::cmp_matched);
                }
            } else {
                use rayon::prelude::*;

                keys.par_iter_mut()
                    .for_each_init(Memory::default, |memory, key| {
                        let s = &haystacks[key.index].as_ref();
                        key.apply_match(needle.find(s, memory));
                    });

                if sort {
                    keys.par_sort_unstable_by_key(HaystackKey::cmp_score);
                } else {
                    keys.par_sort_unstable_by_key(HaystackKey::cmp_matched);
                }
            }

            keys.binary_search_by(|probe| {
                if probe.is_match() {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            })
            .unwrap_err()
        };
        self.dirty = false;
    }
}

#[derive(Eq, PartialEq)]
struct HaystackKey {
    /// Match score for the given haystack.
    score: Option<Score>,
    /// Haystack index that this object points to.
    index: usize,
}

impl HaystackKey {
    fn is_match(&self) -> bool {
        self.score.is_some()
    }

    fn apply_match(&mut self, score: Option<Score>) {
        self.score = score;
    }

    fn cmp_index(&self) -> impl Ord + use<> {
        self.index
    }

    fn cmp_score(&self) -> impl Ord + use<> {
        (Reverse(self.score), self.index)
    }

    fn cmp_matched(&self) -> impl Ord + use<> {
        (Reverse(self.score.is_some()), self.index)
    }
}

/// An iterator that either iterates forward or backward.
///
/// This `struct` is created by the [`IteratorExt::rev_if`] method.
struct RevIf<I> {
    inner: I,
    rev: bool,
}

impl<I: DoubleEndedIterator> Iterator for RevIf<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.rev {
            self.inner.next_back()
        } else {
            self.inner.next()
        }
    }
}

impl<I> Deref for RevIf<I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

trait IteratorExt: Iterator + Sized {
    /// Reverses an iterator's direction based on a flag.
    ///
    /// It is similar to [`Iterator::rev`] but can be used in situations when iteration order is
    /// dynamic and an uniform iterator `struct` is required.
    fn rev_if(self, rev: bool) -> RevIf<Self>;
}

impl<I: Iterator<Item = T> + Sized, T> IteratorExt for I {
    fn rev_if(self, rev: bool) -> RevIf<Self> {
        RevIf { inner: self, rev }
    }
}

trait CellAttributesExt {
    fn apply_sgr(&mut self, sgr: Sgr, default_attrs: &CellAttributes);
}

impl CellAttributesExt for CellAttributes {
    fn apply_sgr(&mut self, sgr: Sgr, default_attrs: &CellAttributes) {
        match sgr {
            Sgr::Reset => {
                self.clone_from(default_attrs);
            }
            Sgr::Foreground(x) => {
                self.set_foreground(x);
            }
            Sgr::Background(x) => {
                self.set_background(x);
            }
            Sgr::Intensity(x) => {
                self.set_intensity(x);
            }
            Sgr::Underline(x) => {
                self.set_underline(x);
            }
            Sgr::Overline(x) => {
                self.set_overline(x);
            }
            Sgr::UnderlineColor(x) => {
                self.set_underline_color(x);
            }
            Sgr::Blink(x) => {
                self.set_blink(x);
            }
            Sgr::Italic(x) => {
                self.set_italic(x);
            }
            Sgr::Inverse(x) => {
                self.set_reverse(x);
            }
            Sgr::Invisible(x) => {
                self.set_invisible(x);
            }
            Sgr::StrikeThrough(x) => {
                self.set_strikethrough(x);
            }
            Sgr::Font(_) => {}
            Sgr::VerticalAlign(x) => {
                self.set_vertical_align(x);
            }
        }
    }
}

trait LineExt {
    fn from_sgr_text(bytes: &[u8], default_attrs: &CellAttributes, seqno: SequenceNo) -> Self;
    fn append_char(&mut self, c: char, attrs: CellAttributes, seqno: SequenceNo);
    fn append_str(&mut self, s: &str, attrs: &CellAttributes, seqno: SequenceNo);
}

impl LineExt for Line {
    fn from_sgr_text(bytes: &[u8], default_attrs: &CellAttributes, seqno: SequenceNo) -> Self {
        const TABSTOP: usize = 8;

        let mut line = Line::new(seqno);
        let mut attrs = default_attrs.clone();

        Parser::new().parse(bytes, |action| {
            match action {
                Action::Print(c) => {
                    line.append_char(c, attrs.clone(), seqno);
                }
                Action::Control(ControlCode::HorizontalTab) => {
                    for _ in line.len() % TABSTOP..TABSTOP {
                        line.append_char(' ', attrs.clone(), seqno);
                    }
                }
                Action::PrintString(s) => {
                    line.append_str(&s, &attrs, seqno);
                }
                Action::CSI(CSI::Sgr(sgr)) => {
                    attrs.apply_sgr(sgr, default_attrs);
                }
                _ => {
                    // Ignore.
                }
            }
        });

        line
    }

    fn append_char(&mut self, c: char, attrs: CellAttributes, seqno: SequenceNo) {
        let cell = Cell::new(c, attrs);
        self.insert_cell(self.len(), cell, usize::MAX, seqno);
    }

    fn append_str(&mut self, s: &str, attrs: &CellAttributes, seqno: SequenceNo) {
        self.append_line(Line::from_text(s, attrs, seqno, None), seqno);
    }
}

trait SurfaceExt {
    fn set_line_from_text(&mut self, s: &str, attrs: &CellAttributes);
    fn set_line_from_sgr_text(&mut self, bytes: &[u8], default_attrs: &CellAttributes);
    fn set_line_nowrap(&mut self, line: Line);
    fn set_cursor(&mut self, line: usize, column: usize);
    fn cursor_y(&self) -> usize;
    fn width(&self) -> usize;
    fn height(&self) -> usize;
}

impl SurfaceExt for Surface {
    fn set_line_from_sgr_text(&mut self, bytes: &[u8], default_attrs: &CellAttributes) {
        self.set_line_nowrap(Line::from_sgr_text(
            bytes,
            default_attrs,
            self.current_seqno(),
        ));
    }

    fn set_line_from_text(&mut self, s: &str, attrs: &CellAttributes) {
        self.set_line_nowrap(Line::from_text(s, attrs, self.current_seqno(), None));
    }

    fn set_line_nowrap(&mut self, line: Line) {
        let changes = self.diff_against_numbered_line(self.cursor_y(), &line);
        self.add_changes(changes);
    }

    fn set_cursor(&mut self, line: usize, column: usize) {
        self.add_change(Change::CursorPosition {
            x: Position::Absolute(column),
            y: Position::Absolute(line),
        });
    }

    fn cursor_y(&self) -> usize {
        self.cursor_position().1
    }

    fn width(&self) -> usize {
        self.dimensions().0
    }

    fn height(&self) -> usize {
        self.dimensions().1
    }
}
