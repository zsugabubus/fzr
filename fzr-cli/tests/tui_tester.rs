use std::{
    fs::read_to_string,
    io::Write,
    process::Command,
    thread,
    time::{Duration, Instant},
};
use tempfile::{NamedTempFile, TempPath};

fn tmux() -> Command {
    let mut cmd = Command::new("tmux");
    cmd.args(["-L", "test"]);
    cmd
}

fn check_output(cmd: &mut Command) -> String {
    let output = cmd.output().unwrap();

    assert_eq!(std::str::from_utf8(&output.stderr).unwrap(), "");
    assert!(output.status.success());

    String::from_utf8(output.stdout).unwrap().trim().to_string()
}

#[track_caller]
fn retry_eq<T: std::cmp::PartialEq>(left: impl Fn() -> T, right: &T) -> Result<(), T> {
    let start = Instant::now();
    let mut backoff = Duration::from_millis(10);

    loop {
        match left() {
            x if &x == right => return Ok(()),
            x if start.elapsed() > Duration::from_secs(1) => return Err(x),
            _ => {
                thread::sleep(backoff);
                backoff *= 2;
            }
        }
    }
}

#[track_caller]
fn assert_retry_eq<T: std::fmt::Debug + std::cmp::PartialEq>(actual: impl Fn() -> T, expected: T) {
    if let Err(actual) = retry_eq(actual, &expected) {
        assert_eq!(actual, expected);
        unreachable!();
    }
}

#[derive(Clone)]
pub struct TuiTesterBuilder {
    executable: String,
    width: usize,
    height: usize,
    args: Vec<String>,
    stdin: String,
    envs: Vec<(String, String)>,
}

impl TuiTesterBuilder {
    pub fn new(executable: impl Into<String>) -> Self {
        Self {
            executable: executable.into(),
            width: 80,
            height: 24,
            args: Vec::new(),
            stdin: String::new(),
            envs: Vec::new(),
        }
    }

    pub fn env(&mut self, key: impl Into<String>, value: impl Into<String>) -> &mut Self {
        self.envs.push((key.into(), value.into()));
        self
    }

    pub fn height(&mut self, height: usize) -> &mut Self {
        self.height = height;
        self
    }

    pub fn stdin(&mut self, stdin: impl Into<String>) -> &mut Self {
        self.stdin = stdin.into();
        self
    }

    pub fn args(&mut self, args: impl IntoIterator<Item = impl Into<String>>) -> &mut Self {
        self.args.extend(args.into_iter().map(Into::into));
        self
    }

    pub fn spawn(&self) -> TuiTester {
        let (mut stdin, stdin_path) = NamedTempFile::with_prefix("test").unwrap().into_parts();
        stdin.write_all(self.stdin.as_bytes()).unwrap();
        drop(stdin);

        let stdout_path = NamedTempFile::with_prefix("test").unwrap().into_temp_path();

        println!("Spawn {:?}", &self.args);

        let target = check_output(
            tmux()
                .args(["-f", "/dev/null"])
                .args(["start-server", ";"])
                .args(["set", "-g", "exit-empty", "off", ";"])
                .args(["set", "-g", "remain-on-exit", "on", ";"])
                .args(["set", "-g", "remain-on-exit-format", "", ";"])
                .args([
                    "new-session",
                    "-d",
                    "-x",
                    &self.width.to_string(),
                    "-y",
                    &self.height.to_string(),
                    "-P",
                    "-F",
                    "#{pane_id}",
                ])
                .args(
                    self.envs
                        .iter()
                        .map(|(name, value)| format!("-e{name}={value}")),
                )
                .arg(format!(
                    "-eTEST_STDIN={}",
                    stdin_path.as_os_str().to_str().unwrap()
                ))
                .arg(format!(
                    "-eTEST_STDOUT={}",
                    stdout_path.as_os_str().to_str().unwrap()
                ))
                .args([
                    "sh",
                    "-c",
                    r#""$@" < "$TEST_STDIN" > "$TEST_STDOUT""#,
                    "sh",
                    &self.executable,
                ])
                .args(self.args.iter().map(|x| x.as_str())),
        );

        TuiTester {
            target,
            _stdin_path: stdin_path,
            stdout_path,
        }
    }
}

pub struct TuiTester {
    target: String,
    _stdin_path: TempPath,
    stdout_path: TempPath,
}

impl TuiTester {
    fn expand(&self, s: &str) -> String {
        check_output(tmux().args(["-N", "display-message", "-p", "-t", &self.target, "--", s]))
    }

    fn capture_screen(&self, ansi: bool) -> Vec<String> {
        check_output(tmux().args([
            "-N",
            "capture-pane",
            "-p",
            "-t",
            &self.target,
            if ansi { "-e" } else { "--" },
        ]))
        .split('\n')
        // FIXME: Maybe something related to tmux in CI?
        .map(|s| s.strip_suffix("\x1b[39m\x1b[49m").unwrap_or(s).to_owned())
        .collect::<Vec<_>>()
    }

    fn print_screen(screen: &[String]) {
        const TOP_LEFT: &str = "┌";
        const TOP_RIGHT: &str = "┐";
        const HORIZONTAL: &str = "─";
        const VERTICAL: &str = "│";
        const BOTTOM_LEFT: &str = "└";
        const BOTTOM_RIGHT: &str = "┘";
        fn width(line: &str) -> usize {
            line.chars()
                .fold((0, false), |(width, ansi), c| {
                    if ansi && c == 'm' {
                        (width, false)
                    } else if ansi || c == '\x1b' {
                        (width, true)
                    } else {
                        (width + 1, false)
                    }
                })
                .0
        }
        let screen_width = screen.iter().map(|line| width(line)).max().unwrap();
        println!(
            "{}{}{}",
            TOP_LEFT,
            HORIZONTAL.repeat(screen_width),
            TOP_RIGHT
        );
        for line in screen.iter() {
            println!(
                "{}{}{}{}",
                VERTICAL,
                line,
                " ".repeat(screen_width - width(line)),
                VERTICAL
            );
        }
        println!(
            "{}{}{}",
            BOTTOM_LEFT,
            HORIZONTAL.repeat(screen_width),
            BOTTOM_RIGHT
        );
    }

    pub fn resize_height(&self, height: usize) -> &Self {
        println!("Resize height {height}");
        check_output(tmux().args([
            "-N",
            "resize-window",
            "-t",
            &self.target,
            "-y",
            &height.to_string(),
        ]));
        self
    }

    pub fn key(&self, key: impl Into<String>) -> &Self {
        self.keys([key])
    }

    pub fn keys(&self, keys: impl IntoIterator<Item = impl Into<String>>) -> &Self {
        let keys = keys.into_iter().map(Into::into).collect::<Vec<_>>();
        println!("Send keys {:?}", &keys);
        check_output(
            tmux()
                .args(["-N", "send-keys", "-t", &self.target, "--"])
                .args(keys),
        );
        self
    }

    #[track_caller]
    pub fn expect(&self, to_be_visible: impl AsRef<str>) -> &Self {
        println!("Expect {:?}", to_be_visible.as_ref());
        assert_retry_eq(
            || {
                let screen = self.capture_screen(false);
                Self::print_screen(&screen);
                screen
                    .into_iter()
                    .any(|x| x.contains(to_be_visible.as_ref()))
            },
            true,
        );
        self
    }

    #[track_caller]
    fn assert_screen_helper(
        &self,
        screen_lines: impl IntoIterator<Item = impl Into<String>>,
        ansi: bool,
    ) -> &Self {
        assert_retry_eq(
            || {
                let screen = self.capture_screen(ansi);
                Self::print_screen(&screen);
                screen
            },
            screen_lines.into_iter().map(Into::into).collect::<Vec<_>>(),
        );
        self
    }

    #[track_caller]
    pub fn assert_screen(
        &self,
        screen_lines: impl IntoIterator<Item = impl Into<String>>,
    ) -> &Self {
        self.assert_screen_helper(screen_lines, false)
    }

    #[track_caller]
    pub fn assert_screen_ansi(
        &self,
        screen_lines: impl IntoIterator<Item = impl Into<String>>,
    ) -> &Self {
        self.assert_screen_helper(screen_lines, true)
    }

    #[track_caller]
    pub fn assert_stdout(&self, stdout: impl Into<String>) -> &Self {
        assert_retry_eq(|| read_to_string(&self.stdout_path).unwrap(), stdout.into());
        self
    }

    #[track_caller]
    pub fn assert_exit_status(&self, exit_status: u32) -> &Self {
        assert_retry_eq(
            || self.expand("exited=#{pane_dead} status=#{pane_dead_status}"),
            format!("exited=1 status={exit_status}"),
        );
        self
    }
}
