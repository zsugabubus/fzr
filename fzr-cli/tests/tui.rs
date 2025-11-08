mod tui_tester;

use tui_tester::{TuiTester, TuiTesterBuilder};

fn fzr() -> TuiTesterBuilder {
    TuiTesterBuilder::new(env!("CARGO_BIN_EXE_fzr-cli"))
}

trait FzrTester {
    fn assert_success(&self, stdout: impl Into<String>) -> &Self;
    fn assert_fail(&self, stdout: impl Into<String>) -> &Self;
}

impl FzrTester for TuiTester {
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
fn check_select(keys: impl IntoIterator<Item = impl Into<String>>, expected_line: &str) {
    fzr()
        .stdin("1\n2\n3")
        .spawn()
        .expect(">")
        .keys(keys)
        .assert_success(format!("{expected_line}\n"));
}

#[track_caller]
fn check_select_rev(keys: impl IntoIterator<Item = impl Into<String>>, expected_line: &str) {
    fzr()
        .stdin("1\n2\n3")
        .args(["--reverse"])
        .spawn()
        .expect(">")
        .keys(keys)
        .assert_success(format!("{expected_line}\n"));
}

#[track_caller]
fn check_query_editor(keys: impl IntoIterator<Item = impl Into<String>>, expected_line: &str) {
    fzr()
        .height(1)
        .spawn()
        .keys(keys)
        .assert_screen([expected_line]);
}

#[test]
fn empty() {
    fzr()
        .height(4)
        .spawn()
        .assert_screen([">", "[0/0]", "~", "~"]);
}

#[test]
fn typing() {
    check_query_editor(["ab"], "> ab");
    check_query_editor([&"a".repeat(50)], &format!("> {}", "a".repeat(50)));
}

#[test]
fn ctrl_h() {
    check_query_editor(["C-H"], ">");
    check_query_editor(["ab", "C-H"], "> a");
    check_query_editor(["aő", "C-H"], "> a");
}

#[test]
fn ctrl_w() {
    check_query_editor(["C-W"], ">");
    check_query_editor(["ab", "C-W"], ">");
    check_query_editor(["a  b", "C-W", "."], "> a  .");
    check_query_editor(["a  bb", "C-W", "."], "> a  .");
    check_query_editor(["a  b  ", "C-W", "."], "> a  .");
    check_query_editor(["a  bb  ", "C-W", "."], "> a  .");
}

#[test]
fn ctrl_u() {
    check_query_editor(["C-U"], ">");
    check_query_editor(["ab", "C-U"], ">");
}

#[test]
fn type_ahead() {
    fzr().height(1).spawn().key("123").assert_screen(["> 123"]);
}

#[test]
fn escape() {
    for key in ["Escape", "C-C", "C-G", "C-Q"] {
        fzr().spawn().expect(">").key(key).assert_fail("");
    }
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
                    lines.reverse();
                }
                lines
            });
    }
}

#[test]
fn enter() {
    fzr()
        .stdin("a\nb")
        .spawn()
        .expect(">")
        .key("Enter")
        .assert_success("a\n");
}

#[test]
fn alt_enter() {
    fzr()
        .stdin("a\nx\nb")
        .spawn()
        .expect(">")
        .keys(["!x", "M-Enter"])
        .assert_success("a\nb\n");
}

#[test]
fn enter_and_alt_enter_no_selection() {
    for key in ["Enter", "M-Enter"] {
        fzr().spawn().expect(">").key(key).assert_fail("");
        fzr()
            .stdin("a")
            .spawn()
            .expect(">")
            .keys(["x", key])
            .assert_fail("");
    }
}

#[test]
fn arrow_up_and_arrow_down() {
    check_select(["Up", "Enter"], "1");
    check_select(["Down", "Enter"], "2");
    check_select(["Down", "Up", "Enter"], "1");
    check_select(["Down", "Down", "Enter"], "3");
    check_select(["Down", "Down", "Up", "Enter"], "2");
    check_select(["Down", "Down", "Down", "Enter"], "3");

    check_select_rev(["Down", "Enter"], "1");
    check_select_rev(["Up", "Enter"], "2");
    check_select_rev(["Up", "Down", "Enter"], "1");
    check_select_rev(["Up", "Up", "Enter"], "3");
    check_select_rev(["Up", "Up", "Down", "Enter"], "2");
    check_select_rev(["Up", "Up", "Up", "Enter"], "3");
}

#[test]
fn ctrl_p_and_ctrl_n() {
    check_select(["Down", "C-P", "Enter"], "1");
    check_select(["Down", "C-N", "Enter"], "3");

    check_select_rev(["Up", "C-P", "Enter"], "1");
    check_select_rev(["Up", "C-N", "Enter"], "3");
}

#[test]
fn alt_k_and_alt_j() {
    check_select(["Down", "M-k", "Enter"], "1");
    check_select(["Down", "M-j", "Enter"], "3");

    check_select_rev(["Up", "M-k", "Enter"], "3");
    check_select_rev(["Up", "M-j", "Enter"], "1");
}

#[test]
fn selection_after_query_change() {
    for [key, expected] in [[" ", "1\n"], ["", "2\n"]] {
        fzr()
            .height(4)
            .stdin("1\n2")
            .spawn()
            .expect(">")
            .keys(["Down", key, "Enter"])
            .assert_success(expected);
    }
}

#[test]
fn ctrl_v() {
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
}

#[test]
fn editor_failure() {
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
}

#[test]
fn editor_is_a_shell_function() {
    fzr()
        .env("EDITOR", "f() { echo hello > \"$3\"; }; f")
        .height(6)
        .spawn()
        .expect(">")
        .keys(["C-V", "Enter"])
        .assert_success("hello\n");
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
    for arg in ["--interactive-select-1", "-a"] {
        fzr()
            .height(2)
            .stdin("1")
            .args([arg])
            .spawn()
            .assert_screen([">", "[1/1]"])
            .key(" ")
            .assert_success("1\n");
        fzr()
            .stdin("1\n2")
            .args([arg])
            .spawn()
            .expect(">")
            .key("2")
            .assert_success("2\n");
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
