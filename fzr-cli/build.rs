fn main() {
    println!("cargo:rustc-env=FZR_VERSION={}", git_describe());
    println!("cargo:rerun-if-changed=../.git/HEAD");
    println!("cargo:rerun-if-changed=../.git/refs");
}

fn git_describe() -> String {
    let child = std::process::Command::new("git")
        .args(["describe", "--always", "--dirty"])
        .stdout(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(output.status.success());
    String::from_utf8(output.stdout).unwrap()
}
