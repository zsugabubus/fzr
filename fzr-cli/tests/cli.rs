use std::{
    io::Write,
    process::{Command, Stdio},
};

#[track_caller]
fn assert_output<A: IntoIterator<Item = &'static str>>(
    args: A,
    stdin: &str,
    (success, stdout): (bool, &str),
) {
    let mut child = Command::new(env!("CARGO_BIN_EXE_fzr-cli"))
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();

    let output = child.wait_with_output().unwrap();

    let expected = (success, stdout);

    let actual = (
        output.status.success(),
        std::str::from_utf8(output.stdout.as_slice()).unwrap(),
    );

    assert_eq!(std::str::from_utf8(output.stderr.as_slice()).unwrap(), "");
    assert_eq!(actual, expected);
}

#[test]
fn filter() {
    assert_output(["-f"], "", (false, ""));
    assert_output(["-f"], "a", (true, "a\n"));
}

#[test]
fn read() {
    assert_output(["-f"], "a\nb", (true, "a\nb\n"));
    assert_output(["-f"], "a\nb\n", (true, "a\nb\n"));
}

#[test]
fn read0() {
    assert_output(["-f", "--read0"], "a\0b\0c", (true, "a\nb\nc\n"));
}

#[test]
fn print0() {
    assert_output(["-f", "--print0"], "a\nb\nc", (true, "a\0b\0c\0"));
}

#[test]
fn query() {
    assert_output(["-fqa"], "a\nb", (true, "a\n"));
}

#[test]
fn print_query() {
    assert_output(["-fqa", "--print-query"], "", (false, "a\n"));
    assert_output(["-fqa", "--print-query"], "ab", (true, "a\nab\n"));
    assert_output(["-fqa", "--print-query", "--print0"], "", (false, "a\0"));
}

#[test]
fn print_index() {
    assert_output(["-f", "--print-index"], "a\nb", (true, "0\n1\n"));
    assert_output(["-fqb", "--print-index"], "a\nb", (true, "1\n"));
    assert_output(["-fqb", "--print-index", "--print0"], "a\nb", (true, "1\0"));
}

#[test]
fn exit_0() {
    assert_output(["--exit-0"], "", (false, ""));
    assert_output(["-0qa"], "b", (false, ""));
}

#[test]
fn select_1() {
    assert_output(["--select-1"], "a", (true, "a\n"));
    assert_output(["-1qa"], "a\nb", (true, "a\n"));
}

#[test]
fn sort() {
    let input = "1 ab\n2 a\n3 b";
    let sorted = (true, "2 a\n1 ab\n");
    let not_sorted = (true, "1 ab\n2 a\n");

    assert_output(["-fqa"], input, sorted);

    assert_output(["-fqa", "--sort"], input, sorted);
    assert_output(["-fqa", "--sort", "--sort"], input, sorted);
    assert_output(["-fqa", "--no-sort", "--sort"], input, sorted);

    assert_output(["-fqa", "--no-sort"], input, not_sorted);
    assert_output(["-fqa", "--no-sort", "--no-sort"], input, not_sorted);
    assert_output(["-fqa", "--sort", "--no-sort"], input, not_sorted);
}

#[test]
fn crlf() {
    assert_output(["-f"], "a\r\n", (true, "a\r\n"));
    assert_output(["-f", "--print0"], "a\r\n", (true, "a\r\0"));
}

#[test]
fn and() {
    assert_output(["-fqac b"], "abc\nac\nb", (true, "abc\n"));
}

#[test]
fn or() {
    assert_output(["-fqa|b"], "a\nb\nc", (true, "a\nb\n"));
    assert_output(["-fqa|"], "a\nb\nc", (true, "a\nb\nc\n"));
}

#[test]
fn and_or() {
    assert_output(["-fqa 1|2"], "a1\na2\nb1\nb2\nx", (true, "a1\na2\n"));
}

#[test]
fn starts_with() {
    assert_output(["-fq^a"], "a\nba", (true, "a\n"));
    assert_output(["-fq!^a"], "a\nba", (true, "ba\n"));
}

#[test]
fn ends_with() {
    assert_output(["-fqa$"], "a\nab", (true, "a\n"));
    assert_output(["-fq!a$"], "a\nab", (true, "ab\n"));
}

#[test]
fn contains_exact() {
    assert_output(["-fq'ab"], "ab\naxb", (true, "ab\n"));
    assert_output(["-fq!ab"], "ab\naxb", (true, "axb\n"));
}
