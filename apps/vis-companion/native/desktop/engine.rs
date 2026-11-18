//! Vis on this computer, for a person who has only the desktop app: report the
//! installed engine, install or update it, and start its gateway.
//!
//! The page names a task; it never sends a command. Each command line is fixed here.
use std::ffi::OsString;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Mutex, OnceLock};
use std::time::Duration;

const INSTALLER: &str =
    "https://github.com/Blockether/vis/releases/download/installer/install-vis-agent";
const LATEST: &str = "https://github.com/Blockether/vis/releases/latest";
/// The file that names the newest published beta: `beta-<commit>`.
const LATEST_BETA: &str =
    "https://github.com/Blockether/vis/releases/download/installer/native-beta";
/// The references that the repository shows to a Git client.
const REFERENCES: &str = "https://github.com/Blockether/vis.git/info/refs?service=git-upload-pack";

/// The newest lines of a task that the page can read.
const LOG_LINES: usize = 200;
/// The lines of the gateway log that a failed start shows.
const GATEWAY_LOG_LINES: usize = 20;

/// The process id of the gateway that `engine_gateway_start` started, while it runs.
static GATEWAY_PID: AtomicU32 = AtomicU32::new(0);

/// The engine has no build for Windows or for an Intel Mac.
const SUPPORTED: bool = cfg!(any(
    all(target_os = "macos", target_arch = "aarch64"),
    all(
        target_os = "linux",
        any(target_arch = "x86_64", target_arch = "aarch64")
    )
));

/// The installer without the terminal client. `desktop` makes each later update
/// refresh this app too.
const INSTALL: &str = r#"exec 2>&1
set -o pipefail
curl -fsSL "$VIS_INSTALLER" | bash -s -- --components web,desktop
"#;

/// Update the saved track. A plain update selects the release track. Then stop the
/// gateway that this app started, so that the page starts the new one.
const UPDATE: &str = r#"exec 2>&1
set -e
"$VIS_AGENT" update --track "$VIS_TRACK" --components "$VIS_COMPONENTS"
pid="$(cat "$VIS_GATEWAY_PID" 2>/dev/null || true)"
if [ -n "$pid" ] && ps -p "$pid" -o command= 2>/dev/null | grep -q 'gateway start'; then
  "$VIS_AGENT" gateway stop || true
fi
"#;

fn env_dir(name: &str) -> Option<PathBuf> {
    std::env::var_os(name)
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
}

fn home() -> PathBuf {
    env_dir("HOME").unwrap_or_default()
}

/// The state directory of Vis, as the `vis-agent` command reads it.
fn vis_home() -> PathBuf {
    env_dir("VIS_HOME").unwrap_or_else(|| home().join(".vis"))
}

/// The directory of the `vis-agent` command, as the installer chooses it.
fn bin_dir() -> PathBuf {
    env_dir("VIS_INSTALL_DIR").unwrap_or_else(|| home().join(".local").join("bin"))
}

fn launcher() -> PathBuf {
    bin_dir().join("vis-agent")
}

fn gateway_pid_file() -> PathBuf {
    vis_home().join("desktop-gateway.pid")
}

fn gateway_log_file() -> PathBuf {
    vis_home().join("logs").join("desktop-gateway.log")
}

/// The first line of a state file that is not blank.
fn first_line(path: &Path) -> Option<String> {
    fs::read_to_string(path)
        .ok()?
        .lines()
        .map(str::trim)
        .find(|line| !line.is_empty())
        .map(String::from)
}

/// The installed track. An installation without a saved track is a release.
fn track() -> String {
    match first_line(&vis_home().join("install").join("track")).as_deref() {
        Some("beta") => "beta",
        Some("dev") => "dev",
        _ => "release",
    }
    .into()
}

/// The stamp beside the native engine: `<version> <commit> <track> <metadata>`.
fn stamp() -> Option<String> {
    [
        bin_dir().join("vis-agent-native.build"),
        vis_home().join("install").join("vis-agent-native.build"),
    ]
    .iter()
    .find_map(|stamp| first_line(stamp))
}

/// The version that the track runs: the source version for dev, else the first word
/// of the stamp. An earlier native stamp can stay beside a dev installation.
fn version(track: &str) -> Option<String> {
    if track == "dev" {
        return first_line(&vis_home().join("install").join("src").join("VIS_VERSION"));
    }
    stamp().and_then(|line| line.split_whitespace().next().map(String::from))
}

/// The commit that a beta or dev installation runs: the pin of the source for dev,
/// the second word of the stamp for beta. A release has only its version.
fn build(track: &str) -> Option<String> {
    match track {
        "dev" => first_line(&vis_home().join("install").join("ref")),
        "beta" => stamp().and_then(|line| line.split_whitespace().nth(1).map(String::from)),
        _ => None,
    }
}

/// The saved optional parts, with the desktop app.
fn components() -> String {
    let saved = first_line(&vis_home().join("install").join("components"))
        .unwrap_or_else(|| "tui,web".into());
    if saved == "none" {
        "desktop".into()
    } else if saved == "all" || saved.split(',').any(|part| part == "desktop") {
        saved
    } else {
        format!("{saved},desktop")
    }
}

/// An app that Finder or a desktop entry opens has only the system directories on
/// PATH. Add the directory of the command and the usual directories of local tools.
fn path_env() -> OsString {
    let mut dirs = vec![
        bin_dir(),
        PathBuf::from("/opt/homebrew/bin"),
        PathBuf::from("/usr/local/bin"),
    ];
    dirs.extend(std::env::split_paths(
        &std::env::var_os("PATH").unwrap_or_default(),
    ));
    std::env::join_paths(dirs).unwrap_or_default()
}

/// Give the process its own group: a stop ends the whole task, and a gateway does
/// not end with the app.
#[cfg(unix)]
fn own_group(command: &mut Command) {
    use std::os::unix::process::CommandExt;
    command.process_group(0);
}

#[cfg(not(unix))]
fn own_group(_command: &mut Command) {}

#[derive(Default)]
struct Job {
    task: String,
    running: bool,
    ok: bool,
    pid: u32,
    log: Vec<String>,
}

/// The one install or update task. The host owns it, so a reloaded page finds it again.
fn job() -> &'static Mutex<Job> {
    static JOB: OnceLock<Mutex<Job>> = OnceLock::new();
    JOB.get_or_init(Mutex::default)
}

#[derive(serde::Serialize)]
pub struct EngineStatus {
    supported: bool,
    installed: bool,
    version: Option<String>,
    track: String,
    build: Option<String>,
}

/// What is installed on this computer. An installation is the command with an engine.
#[tauri::command]
pub async fn engine_status() -> EngineStatus {
    let track = track();
    let version = if SUPPORTED && launcher().is_file() {
        version(&track)
    } else {
        None
    };
    let build = version.as_ref().and_then(|_| build(&track));
    EngineStatus {
        supported: SUPPORTED,
        installed: version.is_some(),
        version,
        track,
        build,
    }
}

/// Start `install` or `update`. The call returns at once; `engine_progress` reports it.
#[tauri::command]
pub async fn engine_run(task: String) -> Result<(), String> {
    if !SUPPORTED {
        return Err("Vis has no engine for this computer".into());
    }
    let script = match task.as_str() {
        "install" => INSTALL,
        "update" => UPDATE,
        _ => return Err("Expected install or update".into()),
    };
    let mut state = job().lock().map_err(|e| e.to_string())?;
    if state.running {
        return Err("Another task is in progress".into());
    }
    let mut command = Command::new("/bin/bash");
    command
        .args(["-c", script])
        .current_dir(home())
        .env("PATH", path_env())
        .env("VIS_INSTALLER", INSTALLER)
        .env("VIS_AGENT", launcher())
        .env("VIS_TRACK", track())
        .env("VIS_COMPONENTS", components())
        .env("VIS_GATEWAY_PID", gateway_pid_file())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    own_group(&mut command);
    let mut child = command.spawn().map_err(|e| e.to_string())?;
    let output = child.stdout.take();
    *state = Job {
        task,
        running: true,
        ok: false,
        pid: child.id(),
        log: Vec::new(),
    };
    drop(state);
    if let Some(output) = output {
        std::thread::spawn(move || {
            for line in BufReader::new(output).split(b'\n').map_while(Result::ok) {
                // A progress line rewrites itself with carriage returns: keep its last state.
                let text = String::from_utf8_lossy(&line);
                let text = text.trim_end_matches('\r');
                let text = text.rsplit('\r').next().unwrap_or(text).trim_end();
                if text.is_empty() {
                    continue;
                }
                if let Ok(mut state) = job().lock() {
                    if state.log.len() == LOG_LINES {
                        state.log.remove(0);
                    }
                    state.log.push(text.to_string());
                }
            }
        });
    }
    // The task ends with its process, not with its output: a process that it leaves
    // behind can hold the output open.
    std::thread::spawn(move || {
        let ok = child.wait().map(|status| status.success()).unwrap_or(false);
        // Let the reader take the last lines first.
        std::thread::sleep(Duration::from_millis(150));
        if let Ok(mut state) = job().lock() {
            state.running = false;
            state.ok = ok;
        }
    });
    Ok(())
}

#[derive(serde::Serialize)]
pub struct EngineProgress {
    task: String,
    running: bool,
    ok: bool,
    log: Vec<String>,
}

/// The task that `engine_run` started last: its state and its newest lines.
#[tauri::command]
pub async fn engine_progress() -> Result<EngineProgress, String> {
    let state = job().lock().map_err(|e| e.to_string())?;
    Ok(EngineProgress {
        task: state.task.clone(),
        running: state.running,
        ok: state.ok,
        log: state.log.clone(),
    })
}

/// Stop the task in progress, with each process that it started.
#[tauri::command]
pub async fn engine_stop() -> Result<(), String> {
    let pid = {
        let state = job().lock().map_err(|e| e.to_string())?;
        if !state.running {
            return Ok(());
        }
        state.pid
    };
    Command::new("/bin/sh")
        .args(["-c", &format!("kill -TERM -{pid}")])
        .status()
        .map(|_| ())
        .map_err(|e| e.to_string())
}

/// Start the gateway of the installed engine. It keeps running after the app quits.
/// The call returns at once; the page waits for the gateway to answer.
#[tauri::command]
pub async fn engine_gateway_start() -> Result<(), String> {
    if !SUPPORTED {
        return Err("Vis has no engine for this computer".into());
    }
    let log = gateway_log_file();
    if let Some(directory) = log.parent() {
        fs::create_dir_all(directory).map_err(|e| e.to_string())?;
    }
    let output = fs::File::create(&log).map_err(|e| e.to_string())?;
    let errors = output.try_clone().map_err(|e| e.to_string())?;
    let mut command = Command::new(launcher());
    command
        .args(["gateway", "start"])
        .current_dir(home())
        .env("PATH", path_env())
        .stdin(Stdio::null())
        .stdout(output)
        .stderr(errors);
    own_group(&mut command);
    let mut child = command.spawn().map_err(|e| e.to_string())?;
    let pid = child.id();
    GATEWAY_PID.store(pid, Ordering::SeqCst);
    // An update restarts only the gateway that this file names.
    fs::write(gateway_pid_file(), pid.to_string()).map_err(|e| e.to_string())?;
    std::thread::spawn(move || {
        let _ = child.wait();
        // A later start owns the value when it started another gateway.
        let _ = GATEWAY_PID.compare_exchange(pid, 0, Ordering::SeqCst, Ordering::SeqCst);
    });
    Ok(())
}

/// True while the gateway that `engine_gateway_start` started runs. A gateway that
/// stops before it answers did not start, and its log tells why.
#[tauri::command]
pub async fn engine_gateway_running() -> bool {
    GATEWAY_PID.load(Ordering::SeqCst) != 0
}

/// The newest lines that the gateway wrote since `engine_gateway_start`.
#[tauri::command]
pub async fn engine_gateway_log() -> Vec<String> {
    let text = fs::read_to_string(gateway_log_file()).unwrap_or_default();
    let lines: Vec<&str> = text.lines().filter(|line| !line.trim().is_empty()).collect();
    lines[lines.len().saturating_sub(GATEWAY_LOG_LINES)..]
        .iter()
        .map(|line| line.to_string())
        .collect()
}

/// The bearer token of the gateway on this computer, from the file that its local
/// clients read. The page uses it only for the loopback address.
#[tauri::command]
pub async fn engine_gateway_token() -> Option<String> {
    first_line(&vis_home().join("gateway.token"))
}

/// What `curl` prints for `arguments`, within ten seconds.
fn fetch(arguments: &[&str]) -> Option<String> {
    let output = Command::new("curl")
        .args(["--silent", "--max-time", "10"])
        .args(arguments)
        .env("PATH", path_env())
        .output()
        .ok()?;
    output
        .status
        .success()
        .then(|| String::from_utf8_lossy(&output.stdout).into_owned())
}

/// `text` when it is the full name of a commit.
fn commit(text: &str) -> Option<String> {
    (text.len() == 40 && text.bytes().all(|byte| byte.is_ascii_hexdigit()))
        .then(|| text.to_string())
}

/// The newest build of `track`: the version of the newest published release, the
/// commit of the newest published beta, or the commit at the head of `main` for dev.
/// None of these requests counts against the API limit.
fn newest(track: &str) -> Option<String> {
    match track {
        // Each reference is one line: a length of four characters, the commit, the name.
        "dev" => fetch(&["--fail", "--location", REFERENCES])?
            .lines()
            .find_map(|line| line.strip_suffix(" refs/heads/main"))
            .and_then(|line| commit(line.get(line.len().checked_sub(40)?..)?)),
        "beta" => commit(
            fetch(&["--fail", "--location", LATEST_BETA])?
                .trim()
                .strip_prefix("beta-")?,
        ),
        // GitHub sends the address of the newest release for `releases/latest`.
        _ => fetch(&["--head", LATEST])?.lines().find_map(|line| {
            let (name, value) = line.split_once(':')?;
            if !name.eq_ignore_ascii_case("location") {
                return None;
            }
            value
                .trim()
                .rsplit_once("/releases/tag/v")
                .map(|(_, version)| version.to_string())
        }),
    }
}

#[derive(serde::Serialize)]
pub struct EngineLatest {
    track: String,
    build: String,
}

/// The newest build of the installed track. The page compares it with the
/// installed build of that track.
#[tauri::command]
pub async fn engine_latest() -> Option<EngineLatest> {
    tauri::async_runtime::spawn_blocking(|| {
        let track = track();
        let build = newest(&track)?;
        Some(EngineLatest { track, build })
    })
    .await
    .ok()
    .flatten()
}
