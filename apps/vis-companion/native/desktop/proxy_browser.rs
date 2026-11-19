//! The in-app browser for private links: links to hosts that only the gateway can reach.
//!
//! A web view proxy cannot send credentials, and the proxy of the gateway needs the gateway
//! token. So a relay on the loopback interface is the proxy of the browser window. The relay
//! sends the allowed domains to the gateway proxy with the token, and the other hosts direct.
//! The relay starts once and keeps its port, because WebView2 starts each browser of one data
//! directory with the same proxy argument. Only the browser window uses the relay.
//!
//! The rules are the rules of the gateway and of `src/lib/private-links.ts`. The shared cases
//! are in `vis-contract/fixtures/proxy-routes.json`.

use std::io::{self, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::sync::{OnceLock, RwLock};
use std::thread;
use std::time::Duration;

/// The label of the browser window. The capability of the main window does not name it, so
/// the pages in it get no Vis commands.
const LABEL: &str = "vis-private-link";
/// The largest request head that the relay reads.
const MAX_HEAD: usize = 64 * 1024;

/// The proxy of the gateway and its rules. `token` is secret: keep it out of logs.
#[derive(Clone)]
struct Route {
    host: String,
    port: u16,
    token: String,
    allowed: Vec<String>,
    denied: Vec<String>,
    ports: Vec<u16>,
}

static ROUTE: RwLock<Option<Route>> = RwLock::new(None);
static RELAY_PORT: OnceLock<u16> = OnceLock::new();

fn host_key(host: &str) -> String {
    host.trim()
        .trim_start_matches('[')
        .trim_end_matches(']')
        .to_ascii_lowercase()
        .trim_end_matches('.')
        .to_string()
}

/// `example.com` and `*.example.com` match example.com and each subdomain.
fn matches(pattern: &str, host: &str) -> bool {
    let key = host_key(pattern);
    let base = key.strip_prefix("*.").unwrap_or(&key);
    !base.is_empty() && base != "*" && (host == base || host.ends_with(&format!(".{base}")))
}

impl Route {
    /// True when the gateway opens this host and port.
    fn covers(&self, host: &str, port: u16) -> bool {
        let host = host_key(host);
        !self.denied.iter().any(|p| matches(p, &host))
            && self.allowed.iter().any(|p| matches(p, &host))
            && self.ports.contains(&port)
    }
}

/// True when this build can send a browser window through a proxy. A macOS build needs the
/// `macos-proxy` feature, which Pake sets on macOS 14 and later.
#[tauri::command]
pub fn proxy_browser_available() -> bool {
    cfg!(any(not(target_os = "macos"), feature = "macos-proxy"))
}

/// Open `url` in the browser window, through the gateway proxy for the allowed domains.
#[allow(clippy::too_many_arguments)]
#[tauri::command]
pub async fn proxy_browser_open(
    app: tauri::AppHandle,
    url: String,
    proxy_host: String,
    proxy_port: u16,
    token: String,
    allowed_domains: Vec<String>,
    denied_domains: Vec<String>,
    ports: Vec<u16>,
) -> Result<(), String> {
    use tauri::Manager;

    if !proxy_browser_available() {
        return Err("This build cannot send a window through a proxy.".into());
    }
    let page = tauri::Url::parse(&url).map_err(|error| error.to_string())?;
    if !matches!(page.scheme(), "http" | "https") || proxy_host.is_empty() || token.is_empty() {
        return Err("The private link or the gateway proxy is not valid.".into());
    }
    *ROUTE.write().unwrap_or_else(|error| error.into_inner()) = Some(Route {
        host: host_key(&proxy_host),
        port: proxy_port,
        token,
        allowed: allowed_domains,
        denied: denied_domains,
        ports,
    });
    let relay = relay_port().map_err(|error| error.to_string())?;
    if let Some(window) = app.get_webview_window(LABEL) {
        window.navigate(page).map_err(|error| error.to_string())?;
        let _ = window.unminimize();
        return window.set_focus().map_err(|error| error.to_string());
    }
    let proxy = tauri::Url::parse(&format!("http://127.0.0.1:{relay}"))
        .map_err(|error| error.to_string())?;
    let title = page.host_str().unwrap_or("Private link").to_string();
    let builder = tauri::WebviewWindowBuilder::new(&app, LABEL, tauri::WebviewUrl::External(page))
        .title(title)
        .inner_size(1100.0, 800.0)
        .proxy_url(proxy);
    // A data directory of its own keeps the proxy away from the main window: a WebView2
    // environment on Windows, a web context on Linux. On macOS a private data store does that.
    #[cfg(not(target_os = "macos"))]
    let builder = builder.data_directory(
        app.path()
            .app_local_data_dir()
            .map_err(|error| error.to_string())?
            .join("private-links"),
    );
    #[cfg(target_os = "macos")]
    let builder = builder.incognito(true);
    builder
        .build()
        .map(|_| ())
        .map_err(|error| error.to_string())
}

/// The port of the relay. The first call starts it.
fn relay_port() -> io::Result<u16> {
    if let Some(port) = RELAY_PORT.get() {
        return Ok(*port);
    }
    let listener = TcpListener::bind(("127.0.0.1", 0))?;
    let port = listener.local_addr()?.port();
    if RELAY_PORT.set(port).is_err() {
        // Another call started the relay first. Its port is the port of the window.
        return Ok(*RELAY_PORT.get().unwrap_or(&port));
    }
    thread::spawn(move || {
        for client in listener.incoming().flatten() {
            thread::spawn(move || {
                let _ = relay(client);
            });
        }
    });
    Ok(port)
}

/// The parts of one proxy request head.
struct Head {
    method: String,
    host: String,
    port: u16,
    /// The path of a plain HTTP request. A CONNECT request has none.
    path: Option<String>,
    version: String,
    /// The header lines without the request line.
    headers: Vec<String>,
}

fn split_host_port(authority: &str, default_port: u16) -> Option<(String, u16)> {
    if let Some(rest) = authority.strip_prefix('[') {
        let (host, tail) = rest.split_once(']')?;
        let port = match tail.strip_prefix(':') {
            Some(port) => port.parse().ok()?,
            None => default_port,
        };
        return Some((host.to_string(), port));
    }
    match authority.rsplit_once(':') {
        Some((host, port)) => Some((host.to_string(), port.parse().ok()?)),
        None => Some((authority.to_string(), default_port)),
    }
}

fn parse_head(text: &str) -> Option<Head> {
    let mut lines = text.split("\r\n");
    let mut request = lines.next()?.split(' ');
    let (method, target, version) = (request.next()?, request.next()?, request.next()?);
    let headers = lines
        .filter(|line| !line.is_empty())
        .map(str::to_string)
        .collect();
    if method.eq_ignore_ascii_case("CONNECT") {
        let (host, port) = split_host_port(target, 443)?;
        return Some(Head {
            method: method.into(),
            host,
            port,
            path: None,
            version: version.into(),
            headers,
        });
    }
    // A browser sends plain HTTP to a proxy in absolute form. It sends HTTPS through CONNECT.
    let rest = target.strip_prefix("http://")?;
    let (authority, path) = match rest.find('/') {
        Some(at) => (&rest[..at], &rest[at..]),
        None => (rest, "/"),
    };
    let (host, port) = split_host_port(authority, 80)?;
    Some(Head {
        method: method.into(),
        host,
        port,
        path: Some(path.into()),
        version: version.into(),
        headers,
    })
}

/// The header lines that go on: no proxy headers, and one request on each connection, so
/// that a second request to another host does not use the route of the first.
fn kept_headers(headers: &[String]) -> String {
    let mut text = String::new();
    for line in headers {
        let name = line
            .split(':')
            .next()
            .unwrap_or("")
            .trim()
            .to_ascii_lowercase();
        if !matches!(
            name.as_str(),
            "proxy-authorization" | "proxy-connection" | "connection" | "keep-alive"
        ) {
            text.push_str(line);
            text.push_str("\r\n");
        }
    }
    text.push_str("Connection: close\r\n");
    text
}

fn relay(mut client: TcpStream) -> io::Result<()> {
    client.set_read_timeout(Some(Duration::from_secs(30)))?;
    let mut buffer = Vec::new();
    let mut chunk = [0u8; 8192];
    let end = loop {
        let read = client.read(&mut chunk)?;
        if read == 0 {
            return Ok(());
        }
        buffer.extend_from_slice(&chunk[..read]);
        if let Some(at) = buffer.windows(4).position(|window| window == b"\r\n\r\n") {
            break at + 4;
        }
        if buffer.len() > MAX_HEAD {
            return refuse(&mut client, "431 Request Header Fields Too Large");
        }
    };
    client.set_read_timeout(None)?;
    let Some(head) = std::str::from_utf8(&buffer[..end])
        .ok()
        .and_then(parse_head)
    else {
        return refuse(&mut client, "400 Bad Request");
    };
    let body = &buffer[end..];
    let route = ROUTE
        .read()
        .unwrap_or_else(|error| error.into_inner())
        .clone();
    let through_gateway = route
        .as_ref()
        .filter(|route| route.covers(&head.host, head.port));
    let connect = head.path.is_none();
    let mut upstream = match through_gateway {
        Some(route) => {
            let mut upstream = TcpStream::connect((route.host.as_str(), route.port))?;
            let target = match &head.path {
                None => format!("{}:{}", authority_host(&head.host), head.port),
                Some(path) => format!(
                    "http://{}:{}{}",
                    authority_host(&head.host),
                    head.port,
                    path
                ),
            };
            let headers = if connect {
                let mut text = String::new();
                for line in head.headers.iter().filter(|line| {
                    !line
                        .to_ascii_lowercase()
                        .starts_with("proxy-authorization:")
                }) {
                    text.push_str(line);
                    text.push_str("\r\n");
                }
                text
            } else {
                kept_headers(&head.headers)
            };
            let auth = base64(format!("{}:vis", route.token).as_bytes());
            write!(
                upstream,
                "{} {} {}\r\nProxy-Authorization: Basic {}\r\n{}\r\n",
                head.method, target, head.version, auth, headers
            )?;
            upstream
        }
        None => {
            let Ok(mut upstream) = TcpStream::connect((head.host.as_str(), head.port)) else {
                return refuse(&mut client, "502 Bad Gateway");
            };
            match &head.path {
                None => client.write_all(b"HTTP/1.1 200 Connection Established\r\n\r\n")?,
                Some(path) => write!(
                    upstream,
                    "{} {} {}\r\n{}\r\n",
                    head.method,
                    path,
                    head.version,
                    kept_headers(&head.headers)
                )?,
            }
            upstream
        }
    };
    upstream.write_all(body)?;
    pipe(client, upstream)
}

fn authority_host(host: &str) -> String {
    if host.contains(':') {
        format!("[{host}]")
    } else {
        host.to_string()
    }
}

fn refuse(client: &mut TcpStream, status: &str) -> io::Result<()> {
    write!(
        client,
        "HTTP/1.1 {status}\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    )
}

/// Copy both ways until one side closes.
fn pipe(client: TcpStream, upstream: TcpStream) -> io::Result<()> {
    let (mut client_read, mut upstream_write) = (client.try_clone()?, upstream.try_clone()?);
    let up = thread::spawn(move || {
        let _ = io::copy(&mut client_read, &mut upstream_write);
        let _ = upstream_write.shutdown(Shutdown::Write);
    });
    let (mut upstream_read, mut client_write) = (upstream, client);
    let _ = io::copy(&mut upstream_read, &mut client_write);
    let _ = client_write.shutdown(Shutdown::Both);
    let _ = up.join();
    Ok(())
}

fn base64(bytes: &[u8]) -> String {
    const ALPHABET: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut text = String::new();
    for group in bytes.chunks(3) {
        let n = group
            .iter()
            .enumerate()
            .fold(0u32, |n, (i, byte)| n | u32::from(*byte) << (16 - 8 * i));
        for i in 0..4 {
            text.push(if i <= group.len() {
                ALPHABET[(n >> (18 - 6 * i) & 63) as usize] as char
            } else {
                '='
            });
        }
    }
    text
}
