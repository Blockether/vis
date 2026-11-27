"""Serve the factory pages and relay calls to one Vis gateway."""

from __future__ import annotations

import hashlib
import hmac
import http.client
import ipaddress
import json
import logging
import mimetypes
import re
import socket
import threading
import time
from http.cookies import CookieError, SimpleCookie
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Protocol
from urllib.parse import parse_qs, unquote, urlsplit

from factory import settings

WEB = Path(__file__).resolve().parents[1] / "web"
BASE = settings.BASE
COOKIE = "factory_signin"
PAGES = {BASE: "runs.html", f"{BASE}/workflows": "workflows.html", f"{BASE}/suggestions": "suggestions.html", f"{BASE}/memory": "memory.html"}
VIS = f"{BASE}/vis"
LOGIN = f"{BASE}/login"
ASSETS = f"{BASE}/assets/"
API = f"{BASE}/api/"
LAUNCHES = "/v1/web/launches"
REDEEM = f"{LAUNCHES}/redeem"
BODY_LIMIT = 64 * 1024 * 1024
TOKEN_LIMIT = 8192
LOG = logging.getLogger(__name__)

SECURITY = {
    "strict-transport-security": "max-age=63072000",
    "x-content-type-options": "nosniff",
    "referrer-policy": "no-referrer",
    "x-frame-options": "DENY",
}
PAGE_POLICY = (
    "default-src 'self'; img-src 'self' data: blob:; style-src 'self'; script-src 'self'; "
    "connect-src 'self'; font-src 'self'; object-src 'none'; base-uri 'none'; form-action 'self'; frame-ancestors 'none'"
)
WEB_POLICY = (
    "default-src 'self'; script-src 'self' 'unsafe-inline' 'wasm-unsafe-eval'; style-src 'self' 'unsafe-inline'; "
    "img-src 'self' data: blob:; font-src 'self' data:; media-src 'self' data: blob:; connect-src 'self' blob: data:; "
    "worker-src 'self' blob:; frame-src 'self' blob: data:; object-src 'none'; base-uri 'self'; "
    "form-action 'self'; frame-ancestors 'none'"
)
RELAY_POLICY = "sandbox allow-downloads; default-src 'none'; style-src 'unsafe-inline'; img-src data: blob:; font-src data:; base-uri 'none'; form-action 'none'; frame-ancestors 'none'"
RELAYED = {"content-type", "content-encoding", "content-disposition", "content-range", "accept-ranges", "cache-control", "etag", "last-modified", "retry-after", "x-vis-client-lease-unknown"}
LOCAL_HEADERS = {
    "authorization",
    "cookie",
    "host",
    "content-length",
    "transfer-encoding",
    "connection",
    "proxy-connection",
    "keep-alive",
    "proxy-authenticate",
    "proxy-authorization",
    "te",
    "trailer",
    "upgrade",
    "expect",
    "x-factory-token",
    "x-vis-gateway-secret",
}


class App(Protocol):
    """The factory API and its scheduler."""

    def handle(self, method: str, target: str, body: bytes, headers: dict[str, str]) -> tuple[int, str, bytes]:
        """Answer a factory API call."""
        ...

    def tick(self) -> None:
        """Check for work that is due."""
        ...


def same(left: str, right: str) -> bool:
    """Compare two strings without stopping at their first different byte."""
    return hmac.compare_digest(left.encode(), right.encode())


def loopback(host: str) -> bool:
    """Return True only for a loopback address or localhost."""
    try:
        return ipaddress.ip_address(host).is_loopback
    except ValueError:
        return host == "localhost"


def signature(expiry: str) -> str:
    """Sign the expiry of a factory sign-in."""
    return hmac.new(settings.TOKEN.encode(), f"factory_signin:{expiry}".encode(), hashlib.sha256).hexdigest()


def login_cookie(secure: bool) -> str:
    """Make the cookie for a successful sign-in."""
    expiry = str(int(time.time()) + settings.LOGIN_S)
    cookie = f"{COOKIE}={expiry}.{signature(expiry)}; Path=/; Max-Age={settings.LOGIN_S}; HttpOnly; SameSite=Lax"
    return cookie + ("; Secure" if secure else "")


def signed_in(headers: dict[str, str]) -> bool:
    """Check the bearer token or the signed cookie."""
    if not settings.TOKEN:
        return loopback(settings.HOST)
    if same(headers.get("authorization", ""), f"Bearer {settings.TOKEN}"):
        return True
    try:
        cookie = SimpleCookie(headers.get("cookie", ""))
    except CookieError:
        return False
    value = cookie.get(COOKIE)
    if value is None or not re.fullmatch(r"[0-9]{1,16}\.[0-9a-f]{64}", value.value):
        return False
    expiry, proof = value.value.split(".", 1)
    return same(signature(expiry), proof) and int(expiry) > time.time()


def gateway_connection() -> http.client.HTTPConnection:
    """Connect to the configured gateway, not an address from the caller."""
    url = urlsplit(settings.GATEWAY_URL)
    connection = http.client.HTTPSConnection if url.scheme == "https" else http.client.HTTPConnection
    return connection(url.hostname, url.port, timeout=settings.API_S)


def gateway_headers(headers: dict[str, str]) -> dict[str, str]:
    """Remove credentials and connection headers, then set the gateway token."""
    ignored = LOCAL_HEADERS | {word.strip().lower() for word in headers.get("connection", "").split(",")}
    clean = {name: value for name, value in headers.items() if name not in ignored}
    token = Path(settings.TOKEN_FILE).read_text().strip()
    if token:
        clean["authorization"] = f"Bearer {token}"
    return clean


def factory_link() -> bytes:
    """Return the script that keeps a link to the factory pages after the wordmark of Vis web.

    Vis web has no setting for such a link. It draws its header again when the layout
    changes, so the script looks at the header after each change of the page.
    """
    label = json.dumps(f"Open {settings.NAME}").replace("<", "\\u003c")
    style = "margin-left:4px;padding:3px 10px;border:1px solid;font-size:12px;font-weight:600;color:inherit;text-decoration:none;white-space:nowrap"
    return (
        "<script>{const place = () => {"
        "const mark = document.querySelector('[data-app-header] [data-wordmark]');"
        "if (!mark || mark.parentNode.querySelector('[data-factory]')) return;"
        f"const link = Object.assign(document.createElement('a'), {{href: {json.dumps(BASE)}, textContent: {label}}});"
        f"link.dataset.factory = ''; link.style.cssText = {json.dumps(style)};"
        "mark.after(link);};"
        "new MutationObserver(place).observe(document.documentElement, {childList: true, subtree: true});}</script>"
    ).encode()


def launch_script() -> bytes:
    """Let Vis web pair with this origin and keep a session link open."""
    connection = gateway_connection()
    try:
        connection.request("POST", LAUNCHES, body=b"", headers=gateway_headers({"accept": "application/json"}))
        response = connection.getresponse()
        if response.status != 200:
            return b""
        launch = json.loads(response.read(TOKEN_LIMIT))
        code, gateway = launch["code"], launch["gateway_id"]
        if not all(isinstance(value, str) and re.fullmatch(r"[\w-]+", value, re.ASCII) for value in (code, gateway)):
            return b""
        values = json.dumps({"launch": code, "gw": gateway})
        return (
            "<script>{const launch = new URLSearchParams(" + values + ");"
            "if (location.hash.startsWith('#/s/')) launch.set('open', location.hash);"
            "history.replaceState(history.state, '', location.pathname + location.search + '#' + launch);}</script>"
        ).encode()
    except (OSError, http.client.HTTPException, ValueError, KeyError, TypeError):
        return b""
    finally:
        connection.close()


def handler(app: App, web: Path = WEB) -> type[BaseHTTPRequestHandler]:
    """Build a request handler for this app and web directory."""
    web = web.resolve()
    assets = web / "assets"

    class Handler(BaseHTTPRequestHandler):
        """Answer one HTTP connection at a time."""

        protocol_version = "HTTP/1.1"
        timeout = settings.API_S

        def log_message(self, format: str, *args: object) -> None:
            """Do not log a request target: the query can hold a hook token."""

        def head(self, status: int, headers: dict[str, str]) -> None:
            """Send the status and the headers of an answer."""
            self.send_response(status)
            for name, value in {**headers, **SECURITY}.items():
                self.send_header(name, value)
            self.end_headers()

        def has_body(self, status: int) -> bool:
            """Return True when this answer can have a body."""
            return self.command != "HEAD" and status >= 200 and status not in (204, 205, 304)

        def answer(self, status: int, mime: str, data: bytes, **headers: str) -> None:
            """Send a complete answer or its headers for HEAD."""
            size = len(data) if self.has_body(status) or (self.command == "HEAD" and status not in (204, 205, 304)) else 0
            self.head(status, {"content-type": mime, "content-length": str(size), "cache-control": "no-store", "content-security-policy": RELAY_POLICY, **headers})
            if self.has_body(status):
                self.wfile.write(data)

        def error(self, status: int, reason: str) -> None:
            """Send a JSON error without the request data."""
            self.answer(status, "application/json", json.dumps({"error": reason}).encode())

        def static(self, directory: Path, name: str, *, status: int = 200) -> None:
            """Serve one regular file below a directory."""
            path = (directory / name).resolve()
            if not path.is_relative_to(directory) or not path.is_file():
                return self.error(404, "No such file.")
            mime = mimetypes.guess_type(path)[0] or "application/octet-stream"
            data = path.read_bytes()
            if path.suffix in (".html", ".css", ".js"):
                mime += "; charset=utf-8"
            if status == 401 and name == "login.html":
                data = data.replace(b"<!-- Sign-in error -->", b'<p class="bad-text" role="alert">The token is not correct.</p>', 1)
            self.answer(status, mime, data, **{"content-security-policy": PAGE_POLICY})

        def login(self, body: bytes, headers: dict[str, str]) -> None:
            """Check the form and set the sign-in cookie, or show the failure."""
            if not headers.get("content-type", "").lower().startswith("application/x-www-form-urlencoded"):
                return self.error(415, "Send a sign-in form.")
            fields = parse_qs(body.decode(errors="replace"), max_num_fields=10)
            if not same(fields.get("token", [""])[0], settings.TOKEN) or not settings.TOKEN:
                return self.static(web, "login.html", status=401)
            secure = headers.get("x-forwarded-proto", "").strip().lower() == "https"
            self.answer(303, "text/plain", b"", location=BASE, **{"set-cookie": login_cookie(secure)})

        def relay(self, target: str, path: str, body: bytes, incoming: dict[str, str]) -> None:
            """Send a gateway answer as chunks without waiting for the whole body."""
            connection = gateway_connection()
            try:
                try:
                    sent = gateway_headers(incoming)
                    if path.rstrip("/") == REDEEM or path == VIS:
                        sent["accept-encoding"] = "identity"
                    if path == VIS:
                        sent.pop("if-none-match", None)
                        sent.pop("if-modified-since", None)
                    connection.request(self.command, target, body=body or None, headers=sent)
                    response = connection.getresponse()
                    status = response.status
                    if not 200 <= status <= 599 or (300 <= status < 400 and status != 304):
                        return self.error(502, "The gateway sent a redirect or a status that is not valid.")
                    if path.rstrip("/") == REDEEM and status == 200 and self.command == "POST":
                        # The factory cookie gives Vis web access. Keep the gateway token here.
                        grant = json.loads(response.read(TOKEN_LIMIT))
                        safe = {key: grant[key] for key in ("gateway_id", "machine_name") if key in grant}
                        return self.answer(200, "application/json", json.dumps({**safe, "token": "", "require_token": False}).encode())
                    if path == VIS and status == 200 and self.command == "GET" and response.getheader("content-type", "").startswith("text/html"):
                        page = response.read()
                        page = page.replace(b"<head>", b"<head>" + launch_script() + factory_link(), 1)
                        return self.answer(200, "text/html; charset=utf-8", page, **{"content-security-policy": WEB_POLICY})
                    headers = {name.lower(): value for name, value in response.getheaders() if name.lower() in RELAYED}
                    trusted = self.command in ("GET", "HEAD") and (path == VIS or path == "/index.html" or path.startswith("/assets/"))
                    headers["content-security-policy"] = WEB_POLICY if trusted else RELAY_POLICY
                    headers["cache-control"] = "no-store"
                except (OSError, http.client.HTTPException, ValueError, TypeError):
                    return self.error(502, "The Vis gateway did not answer.")
                if not self.has_body(status):
                    length = response.getheader("content-length")
                    if self.command == "HEAD" and length is not None and length.isascii() and length.isdigit():
                        headers["content-length"] = length
                    self.head(status, headers)
                    return
                headers["transfer-encoding"] = "chunked"
                headers["x-accel-buffering"] = "no"
                self.head(status, headers)
                while part := response.read1(64 * 1024):
                    self.wfile.write(f"{len(part):x}\r\n".encode() + part + b"\r\n")
                    self.wfile.flush()
                self.wfile.write(b"0\r\n\r\n")
            except (OSError, http.client.HTTPException):
                # An interrupted stream cannot end with a success chunk.
                self.close_connection = True
            finally:
                connection.close()

        def route(self, path: str, target: str, body: bytes, headers: dict[str, str]) -> None:
            """Choose a factory page, the factory API or the gateway."""
            method = self.command
            read = method in ("GET", "HEAD")
            hook = method == "POST" and path.startswith(f"{API}hooks/")
            if not read and not hook and headers.get("sec-fetch-site") in ("same-site", "cross-site"):
                return self.error(403, "Open this page before you make changes.")
            if path == LOGIN:
                if read:
                    return self.static(web, "login.html")
                if method == "POST":
                    return self.login(body, headers)
                return self.error(405, "This method is not allowed.")
            if path == f"{ASSETS}app.css" and read:
                return self.static(assets, "app.css")
            if not hook and not signed_in(headers):
                if read and (path in PAGES or path in ("/", VIS)):
                    return self.static(web, "login.html")
                return self.error(401, "Sign in to the factory.")
            if path.startswith(API):
                status, mime, data = app.handle(method, target, body, headers)
                return self.answer(status, mime, data)
            if read:
                if path == "/":
                    return self.answer(302, "text/plain", b"", location=BASE)
                if path in PAGES:
                    return self.static(web, PAGES[path])
                if path.startswith(ASSETS):
                    return self.static(assets, path[len(ASSETS) :])
                if path == VIS:
                    query = urlsplit(target).query
                    return self.relay("/?" + query if query else "/", VIS, body, headers)
            if path == BASE or path.startswith(BASE + "/"):
                return self.error(404, "No such path.")
            return self.relay(target, path, body, headers)

        def request(self) -> None:
            """Read one bounded request and send its answer."""
            try:
                raw = self.path
                target = urlsplit(raw)
                path = unquote(target.path, errors="strict")
                if target.scheme or target.netloc or target.fragment or not path.startswith("/") or re.search(r"[\x00-\x1f\x7f\\]", path):
                    self.close_connection = True
                    return self.error(400, "The request path is not valid.")
                if any(part in (".", "..") for part in path.split("/")):
                    self.close_connection = True
                    return self.error(404, "No such path.")
                lengths = self.headers.get_all("content-length", failobj=["0"])
                if self.headers.get("transfer-encoding") is not None or len(lengths) != 1 or not re.fullmatch(r"[0-9]{1,12}", lengths[0]):
                    self.close_connection = True
                    return self.error(400, "The body length is not valid.")
                size = int(lengths[0])
                if size > (TOKEN_LIMIT if path == LOGIN else BODY_LIMIT):
                    self.close_connection = True
                    return self.error(413, "The body is too large.")
                body = self.rfile.read(size)
                if len(body) != size:
                    self.close_connection = True
                    return self.error(400, "The body did not arrive in full.")
                self.route(path, raw, body, {name.lower(): value for name, value in self.headers.items()})
            except (BrokenPipeError, ConnectionResetError, TimeoutError):
                self.close_connection = True
            except (UnicodeError, ValueError):
                self.close_connection = True
                self.error(400, "The request is not valid.")
            except Exception as failure:
                LOG.error("Request failed: %s", type(failure).__name__)
                self.close_connection = True
                self.error(500, "The factory could not answer the call.")

        do_GET = do_HEAD = do_POST = do_PUT = do_PATCH = do_DELETE = do_OPTIONS = request

    return Handler


def scheduler(app: App, stop: threading.Event) -> None:
    """Check for work now and then again after each interval."""
    while not stop.is_set():
        try:
            app.tick()
        except Exception as failure:
            LOG.error("Scheduler tick failed: %s", type(failure).__name__)
        if stop.wait(settings.TICK_S):
            return


def serve(app: App) -> None:
    """Run the HTTP server and its scheduler until the process stops."""
    if not settings.TOKEN and not loopback(settings.HOST):
        raise SystemExit("Set FACTORY_TOKEN to serve the factory on a non-loopback address.")
    stop = threading.Event()
    if ":" in settings.HOST:

        class Server(ThreadingHTTPServer):
            """Listen on the configured IPv6 address."""

            address_family = socket.AF_INET6
    else:
        Server = ThreadingHTTPServer
    with Server((settings.HOST, settings.PORT), handler(app)) as http:
        worker = threading.Thread(target=scheduler, args=(app, stop), name="factory-scheduler", daemon=True)
        worker.start()
        LOG.info("Factory listens on %s:%d", settings.HOST, http.server_port)
        try:
            http.serve_forever()
        except KeyboardInterrupt:
            pass
        finally:
            stop.set()
