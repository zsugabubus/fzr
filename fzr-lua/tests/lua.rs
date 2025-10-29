use std::{path::Path, process::Command};

#[test]
fn luajit() {
    let tmp_dir = Path::new(env!("CARGO_TARGET_TMPDIR"));
    let target_dir = tmp_dir.parent().unwrap().to_path_buf();
    let lib_path = target_dir.join("debug/libfzr_lua.so");
    let package_path = tmp_dir.join("fzr.so");
    let package_search = tmp_dir.join("?.so");

    let build = Command::new("cargo")
        .args(["build", "--package", "fzr-lua", "--profile", "test"])
        .status()
        .unwrap();
    assert!(build.success());

    let _ = std::fs::remove_file(&package_path);
    std::os::unix::fs::symlink(&lib_path, &package_path).unwrap();

    let luajit = Command::new("luajit")
        .env("LUA_PATH", "")
        .env("LUA_CPATH", package_search)
        .args(["-e", include_str!("test.lua")])
        .status()
        .unwrap();
    assert!(luajit.success());
}
