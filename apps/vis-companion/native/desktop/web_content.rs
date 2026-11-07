//! Reload the window after WebKit ends its web content process.
//!
//! WebKit reloads a page by itself after its web content process ends, but only
//! when the navigation delegate does not implement
//! `webViewWebContentProcessDidTerminate:`. The wry delegate always implements
//! it and only calls a handler that Tauri never sets. Thus when macOS ends that
//! process, for example under memory pressure during sleep, the window stays
//! white until the user reloads it.

#[cfg(target_os = "macos")]
use std::{
    sync::Mutex,
    time::{Duration, Instant},
};

/// WebKit also reloads a page only once in this interval: a page that ends
/// its process again at once stays white instead of reloading in a loop.
#[cfg(target_os = "macos")]
const RELOAD_INTERVAL: Duration = Duration::from_secs(30);

/// The last reload of each web view, by address. Entries older than the
/// interval go away, so a new web view at a used address is not blocked long.
#[cfg(target_os = "macos")]
static RECENT_RELOADS: Mutex<Vec<(usize, Instant)>> = Mutex::new(Vec::new());

/// Make the delegate of each window reload its page when the web content
/// process ends. All wry delegates share one class, so one call is enough.
#[cfg(target_os = "macos")]
pub fn reload_after_web_content_exit(window: &tauri::WebviewWindow) {
    use objc2::ffi::{class_replaceMethod, object_getClass};
    use objc2::runtime::{AnyClass, AnyObject, Imp, Sel};
    use objc2::{msg_send, sel};

    extern "C-unwind" fn reload(_delegate: *mut AnyObject, _cmd: Sel, web_view: *mut AnyObject) {
        let Some(web_view) = (unsafe { web_view.as_ref() }) else {
            return;
        };
        let address = web_view as *const AnyObject as usize;
        {
            let mut reloads = RECENT_RELOADS
                .lock()
                .unwrap_or_else(|error| error.into_inner());
            reloads.retain(|(_, at)| at.elapsed() < RELOAD_INTERVAL);
            if reloads.iter().any(|(view, _)| *view == address) {
                return;
            }
            reloads.push((address, Instant::now()));
        }
        // The lock is free here: WebKit can call this function again before the reload returns.
        let _: *mut AnyObject = unsafe { msg_send![web_view, reload] };
    }

    let _ = window.with_webview(|webview| unsafe {
        let view = &*(webview.inner() as *const AnyObject);
        let delegate: *mut AnyObject = msg_send![view, navigationDelegate];
        if delegate.is_null() {
            return;
        }
        let imp: Imp = std::mem::transmute(
            reload as extern "C-unwind" fn(*mut AnyObject, Sel, *mut AnyObject),
        );
        class_replaceMethod(
            object_getClass(delegate) as *mut AnyClass,
            sel!(webViewWebContentProcessDidTerminate:),
            imp,
            c"v@:@".as_ptr(),
        );
    });
}

#[cfg(not(target_os = "macos"))]
pub fn reload_after_web_content_exit(_window: &tauri::WebviewWindow) {}
