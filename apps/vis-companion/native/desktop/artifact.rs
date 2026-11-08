//! Artifact hand-off: retain a copy in Downloads, then open it or show it in its folder,
//! or save it where the person chooses in the system save panel.
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};
use tauri::Manager;
use tauri_plugin_opener::OpenerExt;

static NEXT_FILE: AtomicU64 = AtomicU64::new(0);

fn unique_directory() -> String {
    let time = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!(
        "{time}-{}-{}",
        std::process::id(),
        NEXT_FILE.fetch_add(1, Ordering::Relaxed)
    )
}

/// A plain filename: no folder part, no hidden or control character, no Windows
/// stream or reserved character. Validate at the native boundary, not only in the page.
fn is_plain_filename(filename: &str) -> bool {
    !filename.is_empty()
        && !filename.starts_with('.')
        && !filename
            .chars()
            .any(|c| c.is_control() || "\\/:*?\"<>|".contains(c))
}

/// A new, empty folder below `Downloads/Vis Artifacts`.
fn new_directory(downloads: &Path) -> Result<PathBuf, String> {
    let directory = downloads.join("Vis Artifacts").join(unique_directory());
    fs::create_dir_all(directory.parent().unwrap()).map_err(|e| e.to_string())?;
    fs::create_dir(&directory).map_err(|e| e.to_string())?;
    Ok(directory)
}

/// Write `data` as `filename` in a new folder below `Downloads/Vis Artifacts`.
fn stage_file(downloads: &Path, filename: &str, data: &[u8]) -> Result<PathBuf, String> {
    let directory = new_directory(downloads)?;
    let path = directory.join(filename);
    fs::write(&path, data).map_err(|e| e.to_string())?;
    Ok(path)
}

fn stage_workbook(downloads: &Path, filename: &str, data: &[u8]) -> Result<PathBuf, String> {
    if !is_plain_filename(filename) || !filename.to_ascii_lowercase().ends_with(".xlsx") {
        return Err("Expected a plain XLSX filename".into());
    }
    if data.is_empty() {
        return Err("The workbook is empty".into());
    }
    stage_file(downloads, filename, data)
}

fn stage_artifact(downloads: &Path, filename: &str, data: &[u8]) -> Result<PathBuf, String> {
    if !is_plain_filename(filename) {
        return Err("Expected a plain filename".into());
    }
    stage_file(downloads, filename, data)
}

#[derive(serde::Serialize)]
pub struct OpenWorkbookResult {
    opened: bool,
}

#[tauri::command]
pub async fn open_xlsx(
    app: tauri::AppHandle,
    filename: String,
    data: Vec<u8>,
) -> Result<OpenWorkbookResult, String> {
    let downloads = app.path().download_dir().map_err(|e| e.to_string())?;
    let path = stage_workbook(&downloads, &filename, &data)?;
    // Keep the file even when there is no association. It also must outlive
    // this call so that Excel, Numbers or LibreOffice can read and save it.
    let opened = app
        .opener()
        .open_path(path.to_string_lossy(), None::<&str>)
        .is_ok();
    Ok(OpenWorkbookResult { opened })
}

#[derive(serde::Serialize)]
pub struct RevealArtifactResult {
    revealed: bool,
    path: String,
}

/// Keep an artifact that has no viewer, such as a ZIP archive, and show it in
/// Finder, Explorer or the Linux file manager. The page names the saved path
/// also when no file manager answers.
#[tauri::command]
pub async fn reveal_artifact(
    app: tauri::AppHandle,
    filename: String,
    data: Vec<u8>,
) -> Result<RevealArtifactResult, String> {
    let downloads = app.path().download_dir().map_err(|e| e.to_string())?;
    let path = stage_artifact(&downloads, &filename, &data)?;
    let revealed = app.opener().reveal_item_in_dir(&path).is_ok();
    Ok(RevealArtifactResult {
        revealed,
        path: path.to_string_lossy().into_owned(),
    })
}

/// Write the answer of `url` as `filename` in a new folder below `Downloads/Vis Artifacts`,
/// one part at a time. The file of a reference attachment can be hundreds of megabytes
/// (#451), so its bytes never go through the page. The name is the real name only after
/// the last part, and a failed download leaves no folder.
async fn stage_download(downloads: &Path, filename: &str, url: &str) -> Result<PathBuf, String> {
    if !is_plain_filename(filename) {
        return Err("Expected a plain filename".into());
    }
    let url = tauri::Url::parse(url).map_err(|e| e.to_string())?;
    if url.scheme() != "http" && url.scheme() != "https" {
        return Err("Expected an http or https address".into());
    }
    let mut response = tauri_plugin_http::reqwest::get(url)
        .await
        .map_err(|e| e.to_string())?;
    if !response.status().is_success() {
        return Err(format!("The gateway answered HTTP {}", response.status()));
    }
    let directory = new_directory(downloads)?;
    let path = directory.join(filename);
    let part = directory.join(format!("{filename}.part"));
    let written: Result<(), String> = async {
        let mut file = fs::File::create(&part).map_err(|e| e.to_string())?;
        while let Some(chunk) = response.chunk().await.map_err(|e| e.to_string())? {
            file.write_all(&chunk).map_err(|e| e.to_string())?;
        }
        file.flush().map_err(|e| e.to_string())?;
        fs::rename(&part, &path).map_err(|e| e.to_string())
    }
    .await;
    if let Err(error) = written {
        let _ = fs::remove_dir_all(&directory);
        return Err(error);
    }
    Ok(path)
}

#[derive(serde::Serialize)]
pub struct OpenRemoteArtifactResult {
    opened: bool,
    path: String,
}

/// Download a file that the session keeps only as a reference on the gateway machine,
/// from its signed address, then open it in its system app or, with `reveal`, show it
/// in Finder, Explorer or the Linux file manager. The page names the saved path also
/// when no app answers.
#[tauri::command]
pub async fn open_remote_artifact(
    app: tauri::AppHandle,
    url: String,
    filename: String,
    reveal: bool,
) -> Result<OpenRemoteArtifactResult, String> {
    let downloads = app.path().download_dir().map_err(|e| e.to_string())?;
    let path = stage_download(&downloads, &filename, &url).await?;
    let opened = if reveal {
        app.opener().reveal_item_in_dir(&path).is_ok()
    } else {
        app.opener()
            .open_path(path.to_string_lossy(), None::<&str>)
            .is_ok()
    };
    Ok(OpenRemoteArtifactResult {
        opened,
        path: path.to_string_lossy().into_owned(),
    })
}

const LAST_DIRECTORY_FILE: &str = "last-save-directory";

/// The folder of the previous save, if it still exists.
fn last_directory(config: &Path) -> Option<PathBuf> {
    let text = fs::read_to_string(config.join(LAST_DIRECTORY_FILE)).ok()?;
    let directory = PathBuf::from(text.trim_end_matches(['\r', '\n']));
    directory.is_dir().then_some(directory)
}

/// Write `data` to the chosen path, then remember its folder for the next save.
/// A failed write leaves the remembered folder as it was.
fn write_chosen(config: &Path, path: &Path, data: &[u8]) -> Result<(), String> {
    fs::write(path, data).map_err(|e| e.to_string())?;
    if let Some(directory) = path.parent() {
        // The save succeeded, so a folder that cannot be remembered is not an error.
        let _ = fs::create_dir_all(config).and_then(|_| {
            fs::write(
                config.join(LAST_DIRECTORY_FILE),
                directory.to_string_lossy().as_bytes(),
            )
        });
    }
    Ok(())
}

#[derive(serde::Serialize)]
pub struct SaveArtifactResult {
    /// False when the person closed the panel without a choice.
    saved: bool,
    path: Option<String>,
}

/// Save the original bytes where the person chooses. The panel opens in the
/// previous folder, else in Downloads, with the artifact's own name.
#[tauri::command]
pub async fn save_artifact_as(
    app: tauri::AppHandle,
    window: tauri::WebviewWindow,
    filename: String,
    data: Vec<u8>,
) -> Result<SaveArtifactResult, String> {
    if !is_plain_filename(&filename) {
        return Err("Expected a plain filename".into());
    }
    let config = app.path().app_config_dir().map_err(|e| e.to_string())?;
    let directory = match last_directory(&config) {
        Some(directory) => directory,
        None => app.path().download_dir().map_err(|e| e.to_string())?,
    };
    let chosen = rfd::AsyncFileDialog::new()
        .set_parent(&window)
        .set_directory(&directory)
        .set_file_name(&filename)
        .save_file()
        .await;
    let Some(chosen) = chosen else {
        return Ok(SaveArtifactResult {
            saved: false,
            path: None,
        });
    };
    let path = chosen.path().to_path_buf();
    write_chosen(&config, &path, &data)?;
    Ok(SaveArtifactResult {
        saved: true,
        path: Some(path.to_string_lossy().into_owned()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Downloads(PathBuf);

    impl Downloads {
        fn new() -> Self {
            let path =
                std::env::temp_dir().join(format!("vis-workbook-test-{}", unique_directory()));
            fs::create_dir(&path).unwrap();
            Self(path)
        }
    }

    impl Drop for Downloads {
        fn drop(&mut self) {
            fs::remove_dir_all(&self.0).unwrap();
        }
    }

    #[test]
    fn preserves_the_original_bytes_and_filename() {
        let downloads = Downloads::new();
        let data = [80, 75, 3, 4, 0, 255, 128];
        let path = stage_workbook(&downloads.0, "Q3 report.XLSX", &data).unwrap();
        assert_eq!(path.file_name().unwrap(), "Q3 report.XLSX");
        assert!(path.starts_with(downloads.0.join("Vis Artifacts")));
        assert_eq!(fs::read(path).unwrap(), data);
    }

    #[test]
    fn never_overwrites_an_earlier_workbook() {
        let downloads = Downloads::new();
        let first = stage_workbook(&downloads.0, "report.xlsx", b"first").unwrap();
        let second = stage_workbook(&downloads.0, "report.xlsx", b"second").unwrap();
        assert_ne!(first, second);
        assert_eq!(fs::read(first).unwrap(), b"first");
        assert_eq!(fs::read(second).unwrap(), b"second");
    }

    #[test]
    fn rejects_paths_executables_and_windows_special_names() {
        let downloads = Downloads::new();
        for name in [
            "../report.xlsx",
            "/report.xlsx",
            "dir\\report.xlsx",
            "report.exe",
            "report.xlsx.exe",
            "report.xlsx:stream.xlsx",
            ".xlsx",
            "report\0.xlsx",
        ] {
            assert!(
                stage_workbook(&downloads.0, name, b"bytes").is_err(),
                "{name:?}"
            );
        }
        assert_eq!(fs::read_dir(&downloads.0).unwrap().count(), 0);
    }

    #[test]
    fn rejects_empty_workbooks_before_creating_a_file() {
        let downloads = Downloads::new();
        assert!(stage_workbook(&downloads.0, "report.xlsx", b"").is_err());
        assert_eq!(fs::read_dir(&downloads.0).unwrap().count(), 0);
    }

    #[test]
    fn keeps_any_plain_artifact_with_its_original_bytes() {
        let downloads = Downloads::new();
        let data = [80, 75, 5, 6, 0, 0];
        let path = stage_artifact(&downloads.0, "bundle 2.zip", &data).unwrap();
        assert_eq!(path.file_name().unwrap(), "bundle 2.zip");
        assert!(path.starts_with(downloads.0.join("Vis Artifacts")));
        assert_eq!(fs::read(path).unwrap(), data);
    }

    #[test]
    fn remembers_the_folder_of_the_last_save() {
        let root = Downloads::new();
        let config = root.0.join("config");
        let reports = root.0.join("Reports");
        fs::create_dir(&reports).unwrap();
        assert_eq!(last_directory(&config), None);
        let path = reports.join("plan.pdf");
        write_chosen(&config, &path, b"%PDF").unwrap();
        assert_eq!(fs::read(&path).unwrap(), b"%PDF");
        assert_eq!(last_directory(&config), Some(reports.clone()));
        // A second save of the same name replaces the file, as the panel confirmed.
        write_chosen(&config, &path, b"%PDF-2").unwrap();
        assert_eq!(fs::read(&path).unwrap(), b"%PDF-2");
    }

    #[test]
    fn keeps_the_remembered_folder_when_a_write_fails() {
        let root = Downloads::new();
        let config = root.0.join("config");
        let reports = root.0.join("Reports");
        fs::create_dir(&reports).unwrap();
        write_chosen(&config, &reports.join("a.pdf"), b"a").unwrap();
        let missing = root.0.join("gone").join("b.pdf");
        assert!(write_chosen(&config, &missing, b"b").is_err());
        assert_eq!(last_directory(&config), Some(reports));
    }

    #[test]
    fn forgets_a_remembered_folder_that_no_longer_exists() {
        let root = Downloads::new();
        let config = root.0.join("config");
        let reports = root.0.join("Reports");
        fs::create_dir(&reports).unwrap();
        write_chosen(&config, &reports.join("a.pdf"), b"a").unwrap();
        fs::remove_dir_all(&reports).unwrap();
        assert_eq!(last_directory(&config), None);
    }

    #[test]
    fn rejects_artifact_paths_before_creating_a_file() {
        let downloads = Downloads::new();
        for name in [
            "../bundle.zip",
            "/bundle.zip",
            "dir\\bundle.zip",
            ".zip",
            "",
            "a:b.zip",
        ] {
            assert!(
                stage_artifact(&downloads.0, name, b"bytes").is_err(),
                "{name:?}"
            );
        }
        assert_eq!(fs::read_dir(&downloads.0).unwrap().count(), 0);
    }

    /// One HTTP answer on a local port, for the download tests.
    fn serve_once(status: &str, body: &'static [u8]) -> String {
        use std::io::Read;
        let listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let address = listener.local_addr().unwrap();
        let status = status.to_string();
        std::thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            let mut request = [0u8; 4096];
            let _ = stream.read(&mut request);
            let head = format!(
                "HTTP/1.1 {status}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                body.len()
            );
            stream.write_all(head.as_bytes()).unwrap();
            stream.write_all(body).unwrap();
        });
        format!("http://{address}/v1/sessions/s/iterations/i/attachments/0?expires=1&ticket=t")
    }

    #[test]
    fn downloads_a_reference_under_its_own_name() {
        let downloads = Downloads::new();
        let url = serve_once("200 OK", b"%PDF-1.7 whole file");
        let path =
            tauri::async_runtime::block_on(stage_download(&downloads.0, "Atlas 2.pdf", &url))
                .unwrap();
        assert_eq!(path.file_name().unwrap(), "Atlas 2.pdf");
        assert!(path.starts_with(downloads.0.join("Vis Artifacts")));
        assert_eq!(fs::read(&path).unwrap(), b"%PDF-1.7 whole file");
        assert_eq!(fs::read_dir(path.parent().unwrap()).unwrap().count(), 1);
    }

    #[test]
    fn leaves_no_file_when_the_gateway_refuses_the_download() {
        let downloads = Downloads::new();
        let url = serve_once("401 Unauthorized", b"{\"error\":\"ticket-expired\"}");
        let error = tauri::async_runtime::block_on(stage_download(&downloads.0, "a.xlsx", &url))
            .unwrap_err();
        assert!(error.contains("401"), "{error}");
        assert!(!downloads.0.join("Vis Artifacts").exists());
    }

    #[test]
    fn refuses_a_download_with_a_path_or_another_scheme() {
        let downloads = Downloads::new();
        for (name, url) in [
            ("../a.zip", "http://127.0.0.1:9/a"),
            ("a.zip", "file:///etc/passwd"),
        ] {
            assert!(
                tauri::async_runtime::block_on(stage_download(&downloads.0, name, url)).is_err()
            );
        }
        assert_eq!(fs::read_dir(&downloads.0).unwrap().count(), 0);
    }
}
