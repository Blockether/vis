//! Vis on this computer, for a person who has only the desktop app: report the
//! installed engine, install or update it, and start its gateway.
//!
//! The page names a task; it never sends a command. Each command line is fixed here.
use std::ffi::OsString;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Mutex, OnceLock};
use std::time::Duration;

const INSTALLER: &str =
    "https://github.com/Blockether/vis/releases/download/installer/install-vis-agent";
const LATEST: &str = "https://github.com/Blockether/vis/releases/latest";

/// The newest lines of a task that the page can read.
const LOG_LINES: usize = 200;
/// The lines of the gateway log that a failed start shows.
const GATEWAY_LOG_LINES: usize = 20;

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

/// The version that the track runs: the source version for dev, else the first word
/// of the stamp beside the native engine. An earlier native stamp can stay beside a
/// dev installation.
fn version(track: &str) -> Option<String> {
    if track == "dev" {
        return first_line(&vis_home().join("install").join("src").join("VIS_VERSION"));
    }
    [
        bin_dir().join("vis-agent-native.build"),
        vis_home().join("install").join("vis-agent-native.build"),
    ]
    .iter()
    .find_map(|stamp| first_line(stamp))
    .and_then(|line| line.split_whitespace().next().map(String::from))
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
    EngineStatus {
        supported: SUPPORTED,
        installed: version.is_some(),
        version,
        track,
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
    // An update restarts only the gateway that this file names.
    fs::write(gateway_pid_file(), child.id().to_string()).map_err(|e| e.to_string())?;
    std::thread::spawn(move || {
        let _ = child.wait();
    });
    Ok(())
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

/// The version of the newest published release, from the address that GitHub sends
/// for `releases/latest`. This request does not count against the API limit.
#[tauri::command]
pub async fn engine_latest() -> Option<String> {
    tauri::async_runtime::spawn_blocking(|| {
        let output = Command::new("curl")
            .args(["-sI", "--max-time", "10", LATEST])
            .env("PATH", path_env())
            .output()
            .ok()?;
        String::from_utf8_lossy(&output.stdout)
            .lines()
            .find_map(|line| {
                let (name, value) = line.split_once(':')?;
                if !name.eq_ignore_ascii_case("location") {
                    return None;
                }
                value
                    .trim()
                    .rsplit_once("/releases/tag/v")
                    .map(|(_, version)| version.to_string())
            })
    })
    .await
    .ok()
    .flatten()
}
