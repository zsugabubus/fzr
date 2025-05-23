mod tui_tester;

use tui_tester::{TuiTester, TuiTesterBuilder};

fn fzr() -> TuiTesterBuilder {
    TuiTesterBuilder::new(env!("CARGO_BIN_EXE_fzr-cli"))
}

trait FzrTesterExt {
    fn assert_success(&self, stdout: impl Into<String>) -> &Self;
    fn assert_fail(&self, stdout: impl Into<String>) -> &Self;
}

impl FzrTesterExt for TuiTester {
    #[track_caller]
    fn assert_success(&self, stdout: impl Into<String>) -> &Self {
        self.assert_exit_status(0)
            .assert_stdout(stdout)
            .assert_screen([""])
    }

    #[track_caller]
    fn assert_fail(&self, stdout: impl Into<String>) -> &Self {
        self.assert_exit_status(1)
            .assert_stdout(stdout)
            .assert_screen([""])
    }
}

#[track_caller]
fn assert_select3(keys: impl IntoIterator<Item = impl Into<String>>, expected_line: &str) {
    fzr()
        .stdin("1\n2\n3")
        .spawn()
        .expect(">")
        .keys(keys)
        .assert_success(format!("{expected_line}\n"));
}

#[track_caller]
fn assert_select3_rev(keys: impl IntoIterator<Item = impl Into<String>>, expected_line: &str) {
    fzr()
        .stdin("1\n2\n3")
        .args(["--reverse"])
        .spawn()
        .expect(">")
        .keys(keys)
        .assert_success(format!("{expected_line}\n"));
}

#[track_caller]
fn assert_query_editor(keys: impl IntoIterator<Item = impl Into<String>>, expected_line: &str) {
    fzr()
        .height(1)
        .spawn()
        .keys(keys)
        .assert_screen([expected_line]);
}

#[test]
fn empty_screen() {
    fzr()
        .height(4)
        .spawn()
        .assert_screen([">", "[0/0]", "~", "~"]);
}

#[test]
fn insert_char() {
    assert_query_editor(["ab"], "> ab");
}

#[test]
fn backward_delete_char() {
    assert_query_editor(["C-H"], ">");
    assert_query_editor(["ab", "C-H"], "> a");
    assert_query_editor(["aő", "C-H"], "> a");
}

#[test]
fn backward_delete_word() {
    assert_query_editor(["C-W"], ">");
    assert_query_editor(["ab", "C-W"], ">");
    assert_query_editor(["a  b", "C-W", "."], "> a  .");
    assert_query_editor(["a  bb", "C-W", "."], "> a  .");
    assert_query_editor(["a  b  ", "C-W", "."], "> a  .");
    assert_query_editor(["a  bb  ", "C-W", "."], "> a  .");
}

#[test]
fn backward_delete_line() {
    assert_query_editor(["C-U"], ">");
    assert_query_editor(["ab", "C-U"], ">");
}

#[test]
fn long_query() {
    assert_query_editor([&"a".repeat(50)], &format!("> {}", "a".repeat(50)));
}

#[test]
fn type_ahead() {
    fzr().height(1).spawn().key("123").assert_screen(["> 123"]);
}

#[test]
fn abort() {
    for key in ["Escape", "C-C", "C-G", "C-Q"] {
        fzr().spawn().expect(">").key(key).assert_fail("");
    }
}

#[test]
fn accept_empty() {
    fzr().spawn().expect(">").key("Enter").assert_fail("");
}

#[test]
fn headers() {
    for reverse in [false, true] {
        fzr()
            .height(9)
            .stdin("h3\nh4\nx\nl1\nl2")
            .args(["--header=h1", "--header", "h2", "--header-lines=2"])
            .args(reverse.then_some("--reverse"))
            .spawn()
            .key("l")
            .assert_screen({
                let mut lines = ["> l", "[2/3]", "h1", "h2", "h3", "h4", "l1", "l2", "~"];
                if reverse {
                    lines.reverse()
                }
                lines
            });
    }
}

#[test]
fn accept() {
    fzr()
        .stdin("a\nb")
        .spawn()
        .expect(">")
        .key("Enter")
        .assert_success("a\n");
}

#[test]
fn accept_all() {
    fzr()
        .stdin("a\nb")
        .spawn()
        .expect(">")
        .key("M-Enter")
        .assert_success("a\nb\n");
}

#[test]
fn select_arrows() {
    assert_select3(["Up", "Enter"], "1");
    assert_select3(["Down", "Enter"], "2");
    assert_select3(["Down", "Up", "Enter"], "1");
    assert_select3(["Down", "Down", "Enter"], "3");
    assert_select3(["Down", "Down", "Up", "Enter"], "2");
    assert_select3(["Down", "Down", "Down", "Enter"], "3");

    assert_select3_rev(["Down", "Enter"], "1");
    assert_select3_rev(["Up", "Enter"], "2");
    assert_select3_rev(["Up", "Down", "Enter"], "1");
    assert_select3_rev(["Up", "Up", "Enter"], "3");
    assert_select3_rev(["Up", "Up", "Down", "Enter"], "2");
    assert_select3_rev(["Up", "Up", "Up", "Enter"], "3");
}

#[test]
fn select_ctrl_pn() {
    assert_select3(["Down", "C-P", "Enter"], "1");
    assert_select3(["Down", "C-N", "Enter"], "3");

    assert_select3_rev(["Up", "C-P", "Enter"], "1");
    assert_select3_rev(["Up", "C-N", "Enter"], "3");
}

#[test]
fn select_meta_kj() {
    assert_select3(["Down", "M-k", "Enter"], "1");
    assert_select3(["Down", "M-j", "Enter"], "3");

    assert_select3_rev(["Up", "M-k", "Enter"], "3");
    assert_select3_rev(["Up", "M-j", "Enter"], "1");
}

#[test]
fn query_change_resets_selection() {
    fzr()
        .height(4)
        .stdin("a1\na2")
        .spawn()
        .expect(">")
        .key("Down")
        .assert_screen([">", "[2/2]", "a1", "a2"])
        .keys([" ", "Enter"])
        .assert_success("a1\n");
}

#[test]
fn editor() {
    fzr()
        .height(5)
        .stdin("a\0a2\0b")
        .args(["--read0"])
        .spawn()
        .expect(">")
        .key("a")
        .assert_screen(["> a", "[2/3]", "a", "a2", "~"])
        .key("C-V")
        .expect("1,1")
        .key("rcZZ")
        .assert_screen([">", "[2/2]", "c", "a2", "~"])
        .key("Enter")
        .assert_success("c\n");
    fzr()
        .height(4)
        .stdin("a\nb")
        .spawn()
        .key("a")
        .assert_screen(["> a", "[1/2]", "a", "~"])
        .key("C-V")
        .expect("1,1")
        .keys(["rc:w", "Enter", ":cq", "Enter"])
        .assert_screen(["> a", "[1/2]", "a", "~"]);
    fzr()
        .env("EDITOR", "clear && echo foo && cat </dev/tty #")
        .height(1)
        .spawn()
        .expect(">")
        .key("C-V")
        .assert_screen(["foo"]);
}

#[test]
fn prompt() {
    for arg in ["--prompt=foo", "-pfoo"] {
        fzr()
            .height(1)
            .args([arg])
            .spawn()
            .key(".")
            .assert_screen(["foo."]);
    }
}

#[test]
fn query() {
    for arg in ["--query=a", "-qa"] {
        fzr()
            .height(4)
            .stdin("a\nb")
            .args([arg])
            .spawn()
            .assert_screen(["> a", "[1/2]", "a", "~"])
            .key("Enter")
            .assert_success("a\n");
    }
}

#[test]
fn select_1() {
    fzr()
        .height(2)
        .stdin("a\nb")
        .args(["--select-1"])
        .spawn()
        .key("a")
        .assert_screen(["> a", "[1/2]"]);
}

#[test]
fn exit_0() {
    fzr()
        .height(2)
        .stdin("a")
        .args(["--exit-0"])
        .spawn()
        .assert_screen([">", "[1/1]"])
        .key("x")
        .assert_screen(["> x", "[0/1]"]);
}

#[test]
fn interactive_select_1() {
    fzr()
        .height(2)
        .stdin("a")
        .args(["--interactive-select-1"])
        .spawn()
        .assert_screen([">", "[1/1]"]);

    for arg in ["--interactive-select-1", "-a"] {
        fzr()
            .stdin("a\nb")
            .args([arg])
            .spawn()
            .expect(">")
            .key("a")
            .assert_success("a\n");
    }
}

#[test]
fn tabstop() {
    fzr()
        .height(6)
        .stdin("\t.\n1\t.\n1234567\t.\n12345678\t.")
        .spawn()
        .assert_screen([
            ">",
            "[4/4]",
            &format!("{}.", " ".repeat(8)),
            &format!("1{}.", " ".repeat(7)),
            "1234567 .",
            &format!("12345678{}.", " ".repeat(8)),
        ]);
}

#[test]
fn sgr() {
    fzr()
        .height(5)
        .stdin("\x1b[1;33;104mtest\ntest")
        .spawn()
        .assert_screen_ansi([
            ">",
            "[2/2]",
            "\x1b[1;7m\x1b[33m\x1b[104mtest\x1b[0m",
            "test",
            "~",
        ])
        .key("Down")
        .assert_screen_ansi([
            ">",
            "[2/2]",
            "\x1b[1m\x1b[33m\x1b[104mtest\x1b[0m",
            "\x1b[7mtest\x1b[0m",
            "~",
        ]);
}

#[test]
fn resize() {
    fzr()
        .height(3)
        .stdin("1\n2\n3\n4\n5\n6")
        .spawn()
        .assert_screen([">", "[6/6]", "1"])
        .resize_height(5)
        .assert_screen([">", "[6/6]", "1", "2", "3"])
        .resize_height(4)
        .key("6")
        .assert_screen(["> 6", "[1/6]", "6", "~"]);
}
