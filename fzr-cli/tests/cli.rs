use std::{
    io::Write,
    process::{Command, Stdio},
};

#[track_caller]
fn check<'a, A: IntoIterator<Item = &'a str>>(args: A, stdin: &str, expected: Result<&str, &str>) {
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

    let stdout = std::str::from_utf8(output.stdout.as_slice()).unwrap();
    let actual = if output.status.success() {
        Ok(stdout)
    } else {
        Err(stdout)
    };

    assert_eq!(std::str::from_utf8(output.stderr.as_slice()).unwrap(), "");
    assert_eq!(actual, expected);
}

#[test]
fn filter() {
    check(["-f"], "", Err(""));
    check(["-f"], "a", Ok("a\n"));
}

#[test]
fn read() {
    check(["-f"], "a\nb", Ok("a\nb\n"));
    check(["-f"], "a\nb\n", Ok("a\nb\n"));
}

#[test]
fn read0() {
    check(["-f", "--read0"], "a\0b\0c", Ok("a\nb\nc\n"));
}

#[test]
fn print0() {
    check(["-f", "--print0"], "a\nb\nc", Ok("a\0b\0c\0"));
}

#[test]
fn query() {
    check(["-fqa"], "a\nb", Ok("a\n"));
}

#[test]
fn print_query() {
    check(["-fqa", "--print-query"], "", Err("a\n"));
    check(["-fqa", "--print-query"], "ab", Ok("a\nab\n"));
    check(["-fqa", "--print-query", "--print0"], "", Err("a\0"));
}

#[test]
fn print_index() {
    check(["-f", "--print-index"], "a\nb", Ok("0\n1\n"));
    check(["-fqb", "--print-index"], "a\nb", Ok("1\n"));
    check(["-fqb", "--print-index", "--print0"], "a\nb", Ok("1\0"));
}

#[test]
fn exit_0() {
    check(["--exit-0"], "", Err(""));
    check(["-0qa"], "b", Err(""));
}

#[test]
fn select_1() {
    check(["--select-1"], "a", Ok("a\n"));
    check(["-1qa"], "a\nb", Ok("a\n"));
}

#[test]
fn sort() {
    let input = "1 ab\n2 a\n3 b";
    let sorted = Ok("2 a\n1 ab\n");
    let not_sorted = Ok("1 ab\n2 a\n");

    check(["-fqa"], input, sorted);

    check(["-fqa", "--sort"], input, sorted);
    check(["-fqa", "--sort", "--sort"], input, sorted);
    check(["-fqa", "--no-sort", "--sort"], input, sorted);

    check(["-fqa", "--no-sort"], input, not_sorted);
    check(["-fqa", "--no-sort", "--no-sort"], input, not_sorted);
    check(["-fqa", "--sort", "--no-sort"], input, not_sorted);
}

#[test]
fn crlf() {
    check(["-f"], "a\r\n", Ok("a\r\n"));
    check(["-f", "--print0"], "a\r\n", Ok("a\r\0"));
}

#[test]
fn and() {
    check(["-fqac b"], "abc\nac\nb", Ok("abc\n"));
}

#[test]
fn or() {
    check(["-fqa|b"], "a\nb\nc", Ok("a\nb\n"));
    check(["-fqa|"], "a\nb\nc", Ok("a\nb\nc\n"));
}

#[test]
fn and_or() {
    check(["-fqa 1|2"], "a1\na2\nb1\nb2\nx", Ok("a1\na2\n"));
}

#[test]
fn starts_with() {
    check(["-fq^a"], "a\nba", Ok("a\n"));
    check(["-fq!^a"], "a\nba", Ok("ba\n"));
}

#[test]
fn ends_with() {
    check(["-fqa$"], "a\nab", Ok("a\n"));
    check(["-fq!a$"], "a\nab", Ok("ab\n"));
}

#[test]
fn contains_exact() {
    check(["-fq'ab"], "ab\naxb", Ok("ab\n"));
    check(["-fq!ab"], "ab\naxb", Ok("axb\n"));
}

#[test]
fn too_long_query() {
    let s = "a".repeat(100);
    check(
        [&format!("-fq{}", &s), "--print-query"],
        &s,
        Err(&format!("{}\n", &s)),
    );
}
