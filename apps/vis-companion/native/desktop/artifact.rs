//! XLSX hand-off: retain an editable copy and use the OS file association.
use std::fs;
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

fn stage_workbook(downloads: &Path, filename: &str, data: &[u8]) -> Result<PathBuf, String> {
    // Validate at the native boundary, not just in the page's filename helper.
    if filename.starts_with('.')
        || !filename.to_ascii_lowercase().ends_with(".xlsx")
        || filename
            .chars()
            .any(|c| c.is_control() || "\\/:*?\"<>|".contains(c))
    {
        return Err("Expected a plain XLSX filename".into());
    }
    if data.is_empty() {
        return Err("The workbook is empty".into());
    }
    let directory = downloads.join("Vis Artifacts").join(unique_directory());
    fs::create_dir_all(directory.parent().unwrap()).map_err(|e| e.to_string())?;
    fs::create_dir(&directory).map_err(|e| e.to_string())?;
    let path = directory.join(filename);
    fs::write(&path, data).map_err(|e| e.to_string())?;
    Ok(path)
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
}
