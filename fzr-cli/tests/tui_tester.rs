use std::{process::Command, time::Duration};

#[derive(Clone)]
enum Step {
    SendKey(String),
    Wait(Duration),
    AssertScreen(Vec<String>),
}

#[must_use]
#[derive(Clone)]
pub struct TuiTester {
    width: usize,
    height: usize,
    args: String,
    stdin: String,
    steps: Vec<Step>,
    envs: Vec<(String, String)>,
}

impl TuiTester {
    pub fn new() -> Self {
        Self {
            width: 60,
            height: 80,
            args: String::new(),
            stdin: String::new(),
            steps: vec![],
            envs: vec![],
        }
        .wait_millis(50)
    }

    pub fn height(mut self, height: usize) -> Self {
        self.height = height;
        self
    }

    pub fn stdin<T: AsRef<str>>(mut self, s: T) -> Self {
        self.stdin = s.as_ref().to_string();
        self
    }

    pub fn args<T: AsRef<str>>(mut self, s: T) -> Self {
        self.args = s.as_ref().to_string();
        self
    }

    pub fn env<K: Into<String>, V: Into<String>>(mut self, key: K, value: V) -> Self {
        self.envs.push((key.into(), value.into()));
        self
    }

    fn step(mut self, step: Step) -> Self {
        self.steps.push(step);
        self
    }

    pub fn key<T: AsRef<str>>(self, key: T) -> Self {
        self.step(Step::SendKey(key.as_ref().to_string()))
    }

    pub fn keys<I: IntoIterator<Item = T>, T: AsRef<str>>(mut self, keys: I) -> Self {
        for key in keys {
            self = self.key(key)
        }
        self
    }

    pub fn wait(self, duration: Duration) -> Self {
        self.step(Step::Wait(duration))
    }

    pub fn wait_millis(self, millis: u64) -> Self {
        self.wait(Duration::from_millis(millis))
    }

    pub fn screen<I: IntoIterator<Item = T>, T: AsRef<str>>(self, lines: I) -> Self {
        self.wait_millis(16).step(Step::AssertScreen(
            lines.into_iter().map(|s| s.as_ref().to_string()).collect(),
        ))
    }

    #[track_caller]
    pub fn assert_screen<I: IntoIterator<Item = T>, T: AsRef<str>>(self, lines: I) {
        self.screen(lines).assert()
    }

    #[track_caller]
    pub fn assert(&self) {
        fn tmux() -> Command {
            let mut cmd = Command::new("tmux");
            cmd.args(["-L", "cargo-test"]);
            cmd
        }

        let session_name = format!("thread-{:?}", std::thread::current().id());
        let buffer_name = session_name.clone();
        let pane_target = format!("{}:", session_name);

        tmux()
            .args(["kill-session", "-t", &session_name, ";"])
            .output()
            .unwrap();

        {
            let output = tmux()
                .envs(self.envs.iter().map(|(k, v)| (k.as_str(), v.as_str())))
                .args(["-T", "mouse"])
                .args(["set", "-g", "mouse", "on", ";"])
                .args([
                    "set",
                    "-g",
                    "update-environment",
                    &self
                        .envs
                        .iter()
                        .map(|(k, _)| k.as_str())
                        .collect::<Vec<_>>()
                        .join(" "),
                    ";",
                ])
                .args([
                    "new-session",
                    "-d",
                    "-s",
                    &session_name,
                    "-x",
                    &format!("{}", self.width),
                    "-y",
                    &format!("{}", self.height),
                    "sh",
                    "-c",
                    &format!(
                        r#"printf '{}' | {} {}; printf "[exit $?]"; sleep 10"#,
                        self.stdin.replace('\0', "\\0"),
                        env!("CARGO_BIN_EXE_fzr-cli"),
                        self.args,
                    ),
                    ";",
                ])
                .output()
                .unwrap();

            assert_eq!(String::from_utf8_lossy(&output.stderr), "");
            assert!(output.status.success());
        }

        let mut prev_cmd = None;

        for step in &self.steps {
            let mut cmd = prev_cmd.unwrap_or_else(tmux);

            prev_cmd = match step {
                Step::SendKey(key) => {
                    cmd.args(["send-keys", "-t", &pane_target, key, ";"]);
                    Some(cmd)
                }
                Step::Wait(duration) => {
                    cmd.args([
                        "run-shell",
                        "-d",
                        &format!("{}", duration.as_secs_f64()),
                        ";",
                    ]);
                    Some(cmd)
                }
                Step::AssertScreen(expected) => {
                    let output = cmd
                        .args([
                            "capture-pane",
                            "-e",
                            "-t",
                            &pane_target,
                            "-b",
                            &buffer_name,
                            ";",
                        ])
                        .args(["show-buffer", "-b", &buffer_name, ";"])
                        .args(["delete-buffer", "-b", &buffer_name, ";"])
                        .output()
                        .unwrap();

                    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
                    assert!(output.status.success());

                    let stdout = String::from_utf8_lossy(&output.stdout);
                    let mut lines = stdout
                        .split('\n')
                        // FIXME: Maybe something related to tmux in CI?
                        .map(|s| s.strip_suffix("\x1b[39m\x1b[49m").unwrap_or(s).to_owned())
                        .collect::<Vec<_>>();
                    assert_eq!(lines.pop().unwrap(), "");
                    assert_eq!(&lines, expected);

                    None
                }
            }
        }

        assert!(prev_cmd.is_none());
    }
}
