def __vis_install_httpx__():
    import sys as _sys, types as _types, datetime as _dt, time as _time

    _bi = _sys.modules["builtins"]

    def _req():
        import requests as _r

        return _r

    class Headers:
        def __init__(self, data=None):
            self._store = {}
            if data:
                items = data.items() if hasattr(data, "items") else data
                for k, v in items:
                    self._store[str(k).lower()] = (str(k), v)

        def get(self, key, default=None):
            e = self._store.get(str(key).lower())
            return e[1] if e else default

        def __getitem__(self, key):
            e = self._store.get(str(key).lower())
            if e is None:
                raise KeyError(key)
            return e[1]

        def __contains__(self, key):
            return str(key).lower() in self._store

        def __iter__(self):
            return iter(k for (k, _v) in self._store.values())

        def items(self):
            return [(k, v) for (k, v) in self._store.values()]

        def keys(self):
            return [k for (k, _v) in self._store.values()]

        def values(self):
            return [v for (_k, v) in self._store.values()]

        def __repr__(self):
            return "Headers(" + repr(self.items()) + ")"

    class URL:
        def __init__(self, raw):
            self._raw = str(raw)

        @property
        def _parts(self):
            from urllib.parse import urlsplit as _s

            return _s(self._raw)

        @property
        def scheme(self):
            return self._parts.scheme

        @property
        def host(self):
            return self._parts.hostname or ""

        @property
        def port(self):
            return self._parts.port

        @property
        def path(self):
            return self._parts.path or "/"

        @property
        def query(self):
            # httpx hands the raw query back as BYTES.
            return (self._parts.query or "").encode("ascii", "ignore")

        @property
        def params(self):
            from urllib.parse import parse_qsl as _q

            return dict(_q(self._parts.query or ""))

        @property
        def raw_path(self):
            p = self._parts.path or "/"
            if self._parts.query:
                p = p + "?" + self._parts.query
            return p.encode("ascii", "ignore")

        @property
        def netloc(self):
            return (self._parts.netloc or "").encode("ascii", "ignore")

        @property
        def fragment(self):
            return self._parts.fragment or ""

        @property
        def username(self):
            return self._parts.username or ""

        @property
        def password(self):
            return self._parts.password or ""

        @property
        def is_relative_url(self):
            return not self._parts.scheme

        def join(self, url):
            # Resolve a relative reference against this URL, like httpx.URL.join
            # — the call every scraper makes on hrefs pulled out of a page.
            from urllib.parse import urljoin as _j

            return URL(_j(self._raw, str(url)))

        def copy_with(self, **kw):
            p = self._parts
            netloc = kw.pop("netloc", None)
            if netloc is None:
                host = kw.pop("host", p.hostname or "")
                port = kw.pop("port", p.port)
                netloc = host + (":" + str(port) if port else "")
            else:
                kw.pop("host", None)
                kw.pop("port", None)
            from urllib.parse import urlunsplit as _u

            return URL(
                _u(
                    (
                        kw.pop("scheme", p.scheme),
                        netloc,
                        kw.pop("path", p.path),
                        kw.pop("query", p.query),
                        kw.pop("fragment", p.fragment),
                    )
                )
            )

        def __str__(self):
            return self._raw

        def __repr__(self):
            return "URL(" + repr(self._raw) + ")"

        def __eq__(self, other):
            return str(self) == str(other)

        def __hash__(self):
            return hash(self._raw)

    class HTTPError(Exception):
        pass

    class RequestError(HTTPError):
        def __init__(self, message, request=None):
            super().__init__(message)
            self.request = request

    class TimeoutException(RequestError):
        pass

    class TooManyRedirects(RequestError):
        pass

    class DecodingError(RequestError):
        pass

    class StreamError(RuntimeError):
        pass

    class ConnectTimeout(TimeoutException):
        pass

    class ReadTimeout(TimeoutException):
        pass

    class ConnectError(RequestError):
        pass

    class InvalidURL(RequestError):
        pass

    class UnsupportedProtocol(RequestError):
        pass

    class NetworkError(RequestError):
        pass

    class ReadError(NetworkError):
        pass

    class WriteError(NetworkError):
        pass

    class ProxyError(ConnectError):
        pass

    class WriteTimeout(TimeoutException):
        pass

    class PoolTimeout(TimeoutException):
        pass

    class RemoteProtocolError(RequestError):
        pass

    class LocalProtocolError(RequestError):
        pass

    class HTTPStatusError(HTTPError):
        def __init__(self, message, request=None, response=None):
            super().__init__(message)
            self.request = request
            self.response = response

    class Request:
        def __init__(self, method="GET", url="", headers=None, content=None, **_kw):
            self.method = str(method).upper()
            self.url = url if isinstance(url, URL) else URL(url)
            self.headers = Headers(headers or {})
            self.content = content if content is not None else b""

        def read(self):
            return self.content

        def __repr__(self):
            return "<Request(" + self.method + ", " + str(self.url) + ")>"

    class Response:
        def __init__(self, rr, req_url=None, elapsed=None, request=None):
            self._rr = rr
            self.status_code = rr.status_code
            self.headers = Headers(
                rr.headers.items() if hasattr(rr.headers, "items") else rr.headers
            )
            self.url = URL(getattr(rr, "url", req_url) or req_url)
            self.encoding = getattr(rr, "encoding", "utf-8")
            # httpx reports the round trip as a timedelta. The dispatcher times the
            # whole call; a hand-built Response falls back to the wrapped response.
            if elapsed is None:
                elapsed = getattr(rr, "elapsed", None)
            self.elapsed = (
                elapsed if isinstance(elapsed, _dt.timedelta) else _dt.timedelta(0)
            )
            # httpx always carries the originating request and the cookie jar; both
            # used to be missing outright (AttributeError on r.request / r.cookies).
            self.request = request
            self.cookies = getattr(rr, "cookies", None) or {}
            self.history = list(getattr(rr, "history", None) or [])
            self.http_version = "HTTP/1.1"
            self.next_request = None

        @property
        def content(self):
            return self._rr.content

        @property
        def text(self):
            return self._rr.text

        @property
        def reason_phrase(self):
            return getattr(self._rr, "reason", "")

        @property
        def is_success(self):
            return 200 <= self.status_code < 300

        @property
        def is_error(self):
            return self.status_code >= 400

        @property
        def has_redirect_location(self):
            return self.status_code in (301, 302, 303, 307, 308) and (
                "location" in self.headers
            )

        @property
        def is_redirect(self):
            # httpx only calls it a redirect when a Location actually came back.
            return self.has_redirect_location

        @property
        def charset_encoding(self):
            return self.encoding

        @property
        def is_client_error(self):
            return 400 <= self.status_code < 500

        @property
        def is_server_error(self):
            return 500 <= self.status_code < 600

        def json(self, **kw):
            return self._rr.json(**kw)

        def read(self):
            return self.content

        def close(self):
            return None

        @property
        def num_bytes_downloaded(self):
            return len(self.content or b"")

        def iter_bytes(self, chunk_size=None):
            data = self.content or b""
            step = max(1, int(chunk_size or len(data) or 1))
            for i in range(0, len(data), step):
                yield data[i : i + step]

        def iter_raw(self, chunk_size=None):
            return self.iter_bytes(chunk_size)

        def iter_text(self, chunk_size=None):
            data = self.text or ""
            step = max(1, int(chunk_size or len(data) or 1))
            for i in range(0, len(data), step):
                yield data[i : i + step]

        def iter_lines(self):
            # httpx yields decoded text lines here (requests yields bytes).
            for line in (self.text or "").splitlines():
                yield line

        def raise_for_status(self):
            if self.is_success:
                return self
            # httpx raises for EVERY non-2xx, and names the class of failure --
            # a 500 reported as "Client error" sent people hunting the wrong bug.
            error_types = {
                1: "Informational response",
                3: "Redirect response",
                4: "Client error",
                5: "Server error",
            }
            error_type = error_types.get(self.status_code // 100, "Invalid status code")
            message = (
                error_type
                + " '"
                + str(self.status_code)
                + " "
                + str(self.reason_phrase or "")
                + "' for url '"
                + str(self.url)
                + "'"
            )
            raise HTTPStatusError(message, request=self.request, response=self)

        def __repr__(self):
            return "<Response [" + str(self.status_code) + "]>"

    def _dispatch(method, url, kw):
        rq = _req()
        params = kw.pop("params", None)
        headers = kw.pop("headers", None)
        json_body = kw.pop("json", None)
        data = kw.pop("data", None)
        files = kw.pop("files", None)
        content = kw.pop("content", None)
        if content is not None and data is None:
            data = content
            if not isinstance(content, (bytes, bytearray, str)) and not hasattr(
                content, "read"
            ):
                # httpx `content=` is RAW: an Iterable[bytes] is a streamed body,
                # never a form -- even when it is a plain list of chunks.
                data = iter(content)
        timeout = kw.pop("timeout", None)
        if not isinstance(timeout, (int, float, type(None))):
            # httpx.Timeout carries four legs; the transport takes one number, and
            # the READ leg is the one callers actually set.
            read = getattr(timeout, "read", None)
            connect = getattr(timeout, "connect", None)
            timeout = read if read is not None else connect
        cookies = kw.pop("cookies", None)
        auth = kw.pop("auth", None)
        # httpx spells TLS as verify=/cert= too, and additionally accepts a
        # ready ssl.SSLContext as `verify` -- all three reach the transport.
        verify = kw.pop("verify", None)
        cert = kw.pop("cert", None)
        follow = kw.pop("follow_redirects", None)
        if follow is None:
            # httpx does NOT follow redirects unless asked -- unlike requests.
            follow = kw.pop("allow_redirects", False)
        request = Request(method, url, headers=headers, content=content)
        started = _time.monotonic()
        try:
            rr = rq.request(
                str(method).upper(),
                str(url),
                params=params,
                data=data,
                files=files,
                json=json_body,
                headers=headers,
                cookies=cookies,
                timeout=timeout,
                auth=auth,
                allow_redirects=bool(follow),
                verify=verify,
                cert=cert,
            )
        except PermissionError:
            raise  # vis network guard denial -- keep the clear message legible
        except Exception as e:
            if not isinstance(e, rq.exceptions.RequestException):
                # Not a transport failure: an unreadable CA bundle or client-cert
                # path, a refused option. httpx raises those verbatim too, and
                # RequestError would hide the OSError that says which file.
                raise
            en = type(e).__name__
            msg = str(e) or en
            # Map the requests-shim exception onto its httpx counterpart by CLASS,
            # so a read timeout is not reported as a connect timeout.
            mapped = {
                "ConnectTimeout": ConnectTimeout,
                "ReadTimeout": ReadTimeout,
                "Timeout": TimeoutException,
                "ConnectionError": ConnectError,
                "ProxyError": ConnectError,
                "SSLError": ConnectError,
                "InvalidSchema": UnsupportedProtocol,
                "MissingSchema": InvalidURL,
                "InvalidURL": InvalidURL,
                "URLRequired": InvalidURL,
                "ChunkedEncodingError": NetworkError,
                "ContentDecodingError": NetworkError,
                "TooManyRedirects": TooManyRedirects,
            }.get(en)
            if mapped is None:
                if "Timeout" in en:
                    mapped = TimeoutException
                elif "Schema" in en or "URL" in en:
                    mapped = InvalidURL
                elif "Connection" in en:
                    mapped = ConnectError
                else:
                    mapped = RequestError
            raise mapped(msg, request=request)
        return Response(
            rr,
            str(url),
            _dt.timedelta(seconds=_time.monotonic() - started),
            request=request,
        )

    class _StreamContext:
        # httpx hands `client.stream(...)` back as a context manager; the shim has
        # no streaming transport, so the body is already read when it opens.
        def __init__(self, response):
            self._response = response

        def __enter__(self):
            return self._response

        def __exit__(self, *a):
            self._response.close()
            return False

        async def __aenter__(self):
            return self._response

        async def __aexit__(self, *a):
            self._response.close()
            return False

    class Timeout:
        def __init__(
            self, timeout=None, connect=None, read=None, write=None, pool=None
        ):
            self.connect = connect if connect is not None else timeout
            self.read = read if read is not None else timeout
            self.write = write if write is not None else timeout
            self.pool = pool if pool is not None else timeout

    class Client:
        def __init__(
            self,
            base_url="",
            headers=None,
            params=None,
            timeout=None,
            follow_redirects=False,
            auth=None,
            cookies=None,
            verify=True,
            cert=None,
            **_ignored,
        ):
            self.base_url = str(base_url or "")
            self.headers = Headers(headers or {})
            self._params = params or {}
            self._timeout = timeout
            self._follow = follow_redirects
            self._auth = auth
            self._cookies = cookies
            self._verify = verify
            self._cert = cert

        def _abs(self, url):
            u = str(url)
            if self.base_url and not (
                u.startswith("http://") or u.startswith("https://")
            ):
                return self.base_url.rstrip("/") + "/" + u.lstrip("/")
            return u

        def _merged(self, kw):
            # Header names are case-insensitive: "x-a" from the call REPLACES
            # "X-A" from the client instead of being sent next to it.
            hdr = {}
            for k, v in self.headers.items():
                hdr[str(k).lower()] = (k, v)
            over = kw.get("headers") or {}
            over = over.items() if hasattr(over, "items") else over
            for k, v in over:
                hdr[str(k).lower()] = (k, v)
            if hdr:
                kw["headers"] = {k: v for (k, v) in hdr.values()}
            prm = dict(self._params)
            prm.update(kw.get("params") or {})
            if prm:
                kw["params"] = prm
            kw.setdefault("timeout", self._timeout)
            kw.setdefault("follow_redirects", self._follow)
            kw.setdefault("auth", self._auth)
            kw.setdefault("cookies", self._cookies)
            kw.setdefault("verify", self._verify)
            kw.setdefault("cert", self._cert)
            return kw

        def request(self, method, url, **kw):
            return _dispatch(method, self._abs(url), self._merged(kw))

        def get(self, url, **kw):
            return self.request("GET", url, **kw)

        def post(self, url, **kw):
            return self.request("POST", url, **kw)

        def put(self, url, **kw):
            return self.request("PUT", url, **kw)

        def patch(self, url, **kw):
            return self.request("PATCH", url, **kw)

        def delete(self, url, **kw):
            return self.request("DELETE", url, **kw)

        def head(self, url, **kw):
            return self.request("HEAD", url, **kw)

        def options(self, url, **kw):
            return self.request("OPTIONS", url, **kw)

        def stream(self, method, url, **kw):
            return _StreamContext(self.request(method, url, **kw))

        def close(self):
            return None

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

    class AsyncClient:
        def __init__(
            self,
            base_url="",
            headers=None,
            params=None,
            timeout=None,
            follow_redirects=False,
            auth=None,
            cookies=None,
            verify=True,
            cert=None,
            **_ignored,
        ):
            self._client = Client(
                base_url=base_url,
                headers=headers,
                params=params,
                timeout=timeout,
                follow_redirects=follow_redirects,
                auth=auth,
                cookies=cookies,
                verify=verify,
                cert=cert,
            )

        @property
        def base_url(self):
            return self._client.base_url

        @property
        def headers(self):
            return self._client.headers

        async def request(self, method, url, **kw):
            return self._client.request(method, url, **kw)

        async def get(self, url, **kw):
            return self._client.request("GET", url, **kw)

        async def post(self, url, **kw):
            return self._client.request("POST", url, **kw)

        async def put(self, url, **kw):
            return self._client.request("PUT", url, **kw)

        async def patch(self, url, **kw):
            return self._client.request("PATCH", url, **kw)

        async def delete(self, url, **kw):
            return self._client.request("DELETE", url, **kw)

        async def head(self, url, **kw):
            return self._client.request("HEAD", url, **kw)

        async def options(self, url, **kw):
            return self._client.request("OPTIONS", url, **kw)

        def stream(self, method, url, **kw):
            return _StreamContext(self._client.request(method, url, **kw))

        async def aclose(self):
            return None

        async def __aenter__(self):
            return self

        async def __aexit__(self, *a):
            await self.aclose()
            return False

    def _mod_request(method, url, **kw):
        return _dispatch(method, url, kw)

    def _get(url, **kw):
        return _dispatch("GET", url, kw)

    def _post(url, **kw):
        return _dispatch("POST", url, kw)

    def _put(url, **kw):
        return _dispatch("PUT", url, kw)

    def _patch(url, **kw):
        return _dispatch("PATCH", url, kw)

    def _delete(url, **kw):
        return _dispatch("DELETE", url, kw)

    def _head(url, **kw):
        return _dispatch("HEAD", url, kw)

    def _options(url, **kw):
        return _dispatch("OPTIONS", url, kw)

    class Auth:
        pass

    class BasicAuth(Auth):
        # The requests shim reads .username/.password off any auth object.
        def __init__(self, username, password):
            self.username = username
            self.password = password

    class DigestAuth(BasicAuth):
        pass

    class _Codes:
        @staticmethod
        def is_informational(code):
            return 100 <= int(code) < 200

        @staticmethod
        def is_success(code):
            return 200 <= int(code) < 300

        @staticmethod
        def is_redirect(code):
            return 300 <= int(code) < 400

        @staticmethod
        def is_client_error(code):
            return 400 <= int(code) < 500

        @staticmethod
        def is_server_error(code):
            return 500 <= int(code) < 600

        @staticmethod
        def is_error(code):
            return 400 <= int(code) < 600

    codes = _Codes()
    try:
        import http as _http

        for _st in _http.HTTPStatus:
            setattr(codes, _st.name, int(_st.value))
    except Exception:
        for _n, _c in (
            ("OK", 200),
            ("CREATED", 201),
            ("NO_CONTENT", 204),
            ("MOVED_PERMANENTLY", 301),
            ("FOUND", 302),
            ("NOT_MODIFIED", 304),
            ("BAD_REQUEST", 400),
            ("UNAUTHORIZED", 401),
            ("FORBIDDEN", 403),
            ("NOT_FOUND", 404),
            ("TOO_MANY_REQUESTS", 429),
            ("INTERNAL_SERVER_ERROR", 500),
            ("BAD_GATEWAY", 502),
            ("SERVICE_UNAVAILABLE", 503),
        ):
            setattr(codes, _n, _c)

    def _stream(method, url, **kw):
        return _StreamContext(_dispatch(method, url, kw))

    mod = _types.ModuleType("httpx")
    mod.__doc__ = (
        "`httpx` subset wrapping requests: get/post, `Client`/`AsyncClient`, `Response`, "
        "`raise_for_status`. `AsyncClient` coroutines use synchronous I/O. Not supported: "
        "HTTP/2, concurrent async I/O."
    )
    mod.Response = Response
    mod.Headers = Headers
    mod.URL = URL
    mod.Client = Client
    mod.AsyncClient = AsyncClient
    mod.Timeout = Timeout
    mod.HTTPError = HTTPError
    mod.RequestError = RequestError
    mod.HTTPStatusError = HTTPStatusError
    mod.TimeoutException = TimeoutException
    mod.ConnectTimeout = ConnectTimeout
    mod.ReadTimeout = ReadTimeout
    mod.ConnectError = ConnectError
    mod.InvalidURL = InvalidURL
    mod.UnsupportedProtocol = UnsupportedProtocol
    mod.NetworkError = NetworkError
    mod.TooManyRedirects = TooManyRedirects
    mod.DecodingError = DecodingError
    mod.StreamError = StreamError
    mod.ReadError = ReadError
    mod.WriteError = WriteError
    mod.WriteTimeout = WriteTimeout
    mod.PoolTimeout = PoolTimeout
    mod.ProxyError = ProxyError
    mod.RemoteProtocolError = RemoteProtocolError
    mod.LocalProtocolError = LocalProtocolError
    mod.Request = Request
    mod.Auth = Auth
    mod.BasicAuth = BasicAuth
    mod.DigestAuth = DigestAuth
    mod.codes = codes
    mod.stream = _stream
    mod.request = _mod_request
    mod.get = _get
    mod.post = _post
    mod.put = _put
    mod.patch = _patch
    mod.delete = _delete
    mod.head = _head
    mod.options = _options
    mod.__version__ = "0.27.0-vis"
    _sys.modules["httpx"] = mod
    try:
        _bi.httpx = mod
    except Exception:
        pass


__vis_install_httpx__()
del __vis_install_httpx__
