mod tui_tester;

use tui_tester::TuiTester;

macro_rules! selected {
    ($s:expr) => {
        &format!("\x1b[7m{}\x1b[0m", $s)
    };
}

#[track_caller]
fn assert_select3(keys: &[&str], expected_line: &str) {
    TuiTester::new()
        .height(2)
        .stdin("1\n2\n3")
        .keys(keys)
        .assert_screen(&[expected_line, "[exit 0]"]);
}

#[track_caller]
fn assert_select3_rev(keys: &[&str], expected_line: &str) {
    TuiTester::new()
        .height(2)
        .stdin("1\n2\n3")
        .args("--reverse")
        .keys(keys)
        .assert_screen(&[expected_line, "[exit 0]"]);
}

#[track_caller]
fn assert_query_editor(send_keys: &[&str], expected_lines: &str) {
    TuiTester::new()
        .height(1)
        .keys(send_keys)
        .assert_screen([expected_lines]);
}

#[test]
fn empty_screen() {
    TuiTester::new()
        .height(4)
        .assert_screen([">", "[0/0]", "~", "~"]);
}

#[test]
fn filled_screen() {
    TuiTester::new()
        .height(9)
        .stdin("3\n4\n5\n61\n62")
        .args("--header='1' --header '2' --header-lines=2")
        .key("6")
        .assert_screen([
            "> 6",
            "[2/3]",
            "1",
            "2",
            "3",
            "4",
            selected!("61"),
            "62",
            "~",
        ]);
}

#[test]
fn filled_screen_rev() {
    TuiTester::new()
        .height(9)
        .stdin("3\n4\n5\n61\n62")
        .args("--header='1' --header '2' --header-lines=2 --reverse")
        .key("6")
        .assert_screen([
            "~",
            "62",
            selected!("61"),
            "4",
            "3",
            "2",
            "1",
            "[2/3]",
            "> 6",
        ]);
}

#[test]
fn insert_char() {
    assert_query_editor(&["ab"], "> ab");
}

#[test]
fn backward_delete_char() {
    assert_query_editor(&["C-H"], ">");
    assert_query_editor(&["ab", "C-H"], "> a");
    assert_query_editor(&["aő", "C-H"], "> a");
}

#[test]
fn backward_delete_word() {
    assert_query_editor(&["C-W"], ">");
    assert_query_editor(&["ab", "C-W"], ">");
    assert_query_editor(&["a  b", "C-W", "."], "> a  .");
    assert_query_editor(&["a  bb", "C-W", "."], "> a  .");
    assert_query_editor(&["a  b  ", "C-W", "."], "> a  .");
    assert_query_editor(&["a  bb  ", "C-W", "."], "> a  .");
}

#[test]
fn backward_delete_line() {
    assert_query_editor(&["C-U"], ">");
    assert_query_editor(&["ab", "C-U"], ">");
}

#[test]
fn abort() {
    TuiTester::new()
        .height(1)
        .key("Escape")
        .assert_screen(&["[exit 1]"]);
    TuiTester::new()
        .height(1)
        .key("C-C")
        .assert_screen(&["[exit 1]"]);
    TuiTester::new()
        .height(1)
        .key("C-G")
        .assert_screen(&["[exit 1]"]);
    TuiTester::new()
        .height(1)
        .key("C-Q")
        .assert_screen(&["[exit 1]"]);
}

#[test]
fn accept_empty() {
    TuiTester::new()
        .height(1)
        .key("Enter")
        .assert_screen(&["[exit 1]"]);
}

#[test]
fn accept() {
    TuiTester::new()
        .height(2)
        .stdin("a\nb")
        .key("Enter")
        .assert_screen(&["a", "[exit 0]"]);
}

#[test]
fn accept_all() {
    TuiTester::new()
        .height(3)
        .stdin("a\nb")
        .stdin("a\nb")
        .key("M-Enter")
        .assert_screen(&["a", "b", "[exit 0]"]);
}

#[test]
fn select_arrows() {
    assert_select3(&["Up", "Enter"], "1");
    assert_select3(&["Down", "Enter"], "2");
    assert_select3(&["Down", "Up", "Enter"], "1");
    assert_select3(&["Down", "Down", "Enter"], "3");
    assert_select3(&["Down", "Down", "Up", "Enter"], "2");
    assert_select3(&["Down", "Down", "Down", "Enter"], "3");
}

#[test]
fn select_arrows_rev() {
    assert_select3_rev(&["Down", "Enter"], "1");
    assert_select3_rev(&["Up", "Enter"], "2");
    assert_select3_rev(&["Up", "Down", "Enter"], "1");
    assert_select3_rev(&["Up", "Up", "Enter"], "3");
    assert_select3_rev(&["Up", "Up", "Down", "Enter"], "2");
    assert_select3_rev(&["Up", "Up", "Up", "Enter"], "3");
}

#[test]
fn select_ctrl_pn() {
    assert_select3(&["Down", "C-P", "Enter"], "1");
    assert_select3(&["Down", "C-N", "Enter"], "3");

    assert_select3_rev(&["Up", "C-P", "Enter"], "1");
    assert_select3_rev(&["Up", "C-N", "Enter"], "3");
}

#[test]
fn select_meta_kj() {
    assert_select3(&["Down", "M-k", "Enter"], "1");
    assert_select3(&["Down", "M-j", "Enter"], "3");

    assert_select3_rev(&["Up", "M-k", "Enter"], "3");
    assert_select3_rev(&["Up", "M-j", "Enter"], "1");
}

#[test]
fn select_screen() {
    TuiTester::new()
        .height(4)
        .stdin("a\nb")
        .screen([">", "[2/2]", selected!("a"), "b"])
        .key("Down")
        .screen([">", "[2/2]", "a", selected!("b")])
        .assert();
}

#[test]
fn query_change_resets_selection() {
    TuiTester::new()
        .height(4)
        .stdin("a1\na2")
        .key("Down")
        .screen([">", "[2/2]", "a1", selected!("a2")])
        .key("a")
        .screen(["> a", "[2/2]", selected!("a1"), "a2"])
        .assert();
}

#[test]
fn editor() {
    // FIXME: I have no more lifetime to fix this.
    if option_env!("CI").is_some() {
        return;
    }
    TuiTester::new()
        .height(5)
        .args("--read0")
        .stdin("a\0a2\0b")
        .key("a")
        .screen(["> a", "[2/3]", selected!("a"), "a2", "~"])
        .key("C-V")
        .wait_millis(50)
        .key("rcZZ")
        .assert_screen([">", "[2/2]", selected!("c"), "a2", "~"]);
    TuiTester::new()
        .height(4)
        .stdin("a\nb")
        .key("a")
        .screen(["> a", "[1/2]", selected!("a"), "~"])
        .key("C-V")
        .wait_millis(50)
        .keys(["rc:w", "Enter", ":cq", "Enter"])
        .assert_screen(["> a", "[1/2]", selected!("a"), "~"]);
}

#[test]
fn prompt() {
    for args in ["--prompt=foo", "-p foo"] {
        TuiTester::new()
            .height(1)
            .args(args)
            .key(".")
            .assert_screen(["foo."]);
    }
}

#[test]
fn query() {
    TuiTester::new()
        .height(4)
        .stdin("a\nb")
        .args("--query=a")
        .assert_screen(["> a", "[1/2]", selected!("a"), "~"]);
}

#[test]
fn select_1() {
    TuiTester::new()
        .height(2)
        .stdin("a\nb")
        .args("--select-1")
        .key("a")
        .assert_screen(["> a", "[1/2]"]);
}

#[test]
fn exit_0() {
    TuiTester::new()
        .height(2)
        .stdin("a")
        .args("--exit-0")
        .assert_screen([">", "[1/1]"]);
}

#[test]
fn interactive_select_1_initial() {
    TuiTester::new()
        .height(2)
        .stdin("a")
        .args("--interactive-select-1")
        .assert_screen([">", "[1/1]"]);
}

#[test]
fn interactive_select_1() {
    for args in ["--interactive-select-1", "-a"] {
        TuiTester::new()
            .height(2)
            .stdin("a\nb")
            .args(args)
            .key("a")
            .assert_screen(["a", "[exit 0]"]);
    }
}

#[test]
fn tabstop() {
    TuiTester::new()
        .height(6)
        .stdin("\t.\n1\t.\n1234567\t.\n12345678\t.")
        .assert_screen([
            ">",
            "[4/4]",
            selected!(format!("{}.", " ".repeat(8))),
            &format!("1{}.", " ".repeat(7)),
            "1234567 .",
            &format!("12345678{}.", " ".repeat(8)),
        ]);
}

#[test]
fn sgr() {
    TuiTester::new()
        .height(3)
        .stdin("\x1b[1;33;104mtest")
        .assert_screen([">", "[1/1]", "\x1b[1;7m\x1b[33m\x1b[104mtest\x1b[0m"]);
}
