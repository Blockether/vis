def __vis_install_urllib3__():
    import sys as _sys, types as _types, json as _json, os as _os
    import urllib.parse as _up

    _bi = _sys.modules["builtins"]

    def _req():
        import requests as _r

        return _r

    class HTTPError(Exception):
        pass

    class PoolError(HTTPError):
        pass

    class RequestError(PoolError):
        pass

    class MaxRetryError(RequestError):
        pass

    class TimeoutError(HTTPError):
        pass

    class ConnectTimeoutError(TimeoutError):
        pass

    class ReadTimeoutError(TimeoutError, RequestError):
        pass

    class NewConnectionError(ConnectTimeoutError):
        pass

    class ProtocolError(HTTPError):
        pass

    class SSLError(HTTPError):
        pass

    class ProxyError(HTTPError):
        pass

    class DecodeError(HTTPError):
        pass

    class ResponseError(HTTPError):
        pass

    class LocationValueError(ValueError, HTTPError):
        pass

    class LocationParseError(LocationValueError):
        pass

    class Retry:
        def __init__(
            self,
            total=10,
            connect=None,
            read=None,
            redirect=None,
            status=None,
            backoff_factor=0,
            status_forcelist=None,
            **_ignored,
        ):
            self.total = total
            self.connect = connect
            self.read = read
            self.redirect = redirect
            self.status = status
            self.backoff_factor = backoff_factor
            self.status_forcelist = status_forcelist or frozenset()

        @classmethod
        def from_int(cls, retries, **kw):
            if isinstance(retries, cls):
                return retries
            return cls(total=retries)

        def __repr__(self):
            return "Retry(total=" + str(self.total) + ")"

    class HTTPWarning(Warning):
        pass

    class SecurityWarning(HTTPWarning):
        pass

    class InsecureRequestWarning(SecurityWarning):
        pass

    class InsecurePlatformWarning(SecurityWarning):
        pass

    class NotOpenSSLWarning(SecurityWarning):
        pass

    class SystemTimeWarning(SecurityWarning):
        pass

    class DependencyWarning(HTTPWarning):
        pass

    class NameResolutionError(NewConnectionError):
        pass

    class ClosedPoolError(PoolError):
        pass

    class EmptyPoolError(PoolError):
        pass

    class FullPoolError(PoolError):
        pass

    class HostChangedError(RequestError):
        pass

    class InvalidHeader(HTTPError):
        pass

    class HeaderParsingError(HTTPError):
        pass

    class UnrewindableBodyError(HTTPError):
        pass

    class BodyNotHttplibCompatible(HTTPError):
        pass

    class IncompleteRead(ProtocolError):
        def __init__(self, partial, expected):
            self.partial = partial
            self.expected = expected
            super().__init__(
                "IncompleteRead("
                + str(partial)
                + " bytes read, "
                + str(expected)
                + " more expected)"
            )

    class InvalidChunkLength(ProtocolError):
        pass

    class ResponseNotChunked(ProtocolError, ValueError):
        pass

    class URLSchemeUnknown(LocationValueError):
        def __init__(self, scheme):
            self.scheme = scheme
            super().__init__("Not supported URL scheme " + str(scheme))

    class ProxySchemeUnknown(AssertionError, URLSchemeUnknown):
        def __init__(self, scheme):
            self.scheme = scheme
            # Skip URLSchemeUnknown's own prefix: the message would read twice.
            AssertionError.__init__(
                self,
                "Proxy URL had no scheme, should start with http:// or https://"
                if scheme is None
                else "Proxy URL had unsupported scheme "
                + str(scheme)
                + ", should use http:// or https://",
            )

    class Timeout:
        """urllib3.util.Timeout: a total, or a connect/read split.

        The requests shim takes a `(connect, read)` tuple, so `as_requests()` is
        the only conversion the transport needs.
        """

        DEFAULT_TIMEOUT = None

        def __init__(self, total=None, connect=None, read=None):
            self.total = total
            self._connect = connect
            self._read = read

        @classmethod
        def from_float(cls, timeout):
            if isinstance(timeout, cls):
                return timeout
            return cls(read=timeout, connect=timeout)

        def clone(self):
            return Timeout(total=self.total, connect=self._connect, read=self._read)

        @property
        def connect_timeout(self):
            return self.total if self._connect is None else self._connect

        @property
        def read_timeout(self):
            return self.total if self._read is None else self._read

        def as_requests(self):
            c, r = self.connect_timeout, self.read_timeout
            return None if c is None and r is None else (c, r)

        def __repr__(self):
            return (
                "Timeout(connect="
                + repr(self.connect_timeout)
                + ", read="
                + repr(self.read_timeout)
                + ", total="
                + repr(self.total)
                + ")"
            )

    class Url:
        """urllib3.util.Url: the seven parsed URL parts, in order."""

        _fields = ("scheme", "auth", "host", "port", "path", "query", "fragment")

        def __init__(
            self,
            scheme=None,
            auth=None,
            host=None,
            port=None,
            path=None,
            query=None,
            fragment=None,
        ):
            self.scheme = scheme
            self.auth = auth
            self.host = host
            self.port = port
            self.path = path
            self.query = query
            self.fragment = fragment

        @property
        def hostname(self):
            return self.host

        @property
        def netloc(self):
            if self.host is None:
                return None
            return self.host if self.port is None else self.host + ":" + str(self.port)

        @property
        def authority(self):
            nl = self.netloc
            return nl if not self.auth else self.auth + "@" + (nl or "")

        @property
        def request_uri(self):
            uri = self.path or "/"
            return uri if self.query is None else uri + "?" + self.query

        @property
        def url(self):
            out = ""
            if self.scheme is not None:
                out += self.scheme + "://"
            if self.auth is not None:
                out += self.auth + "@"
            if self.host is not None:
                out += self.host
            if self.port is not None:
                out += ":" + str(self.port)
            if self.path is not None:
                out += self.path
            if self.query is not None:
                out += "?" + self.query
            if self.fragment is not None:
                out += "#" + self.fragment
            return out

        def __iter__(self):
            return iter([getattr(self, f) for f in Url._fields])

        def __eq__(self, other):
            if isinstance(other, Url):
                return tuple(self) == tuple(other)
            if isinstance(other, tuple):
                return tuple(self) == other
            return NotImplemented

        def __str__(self):
            return self.url

        def __repr__(self):
            return "Url(" + repr(tuple(self)) + ")"

    def parse_url(url):
        """Parses `url` into a `Url`, like urllib3.util.parse_url."""
        if not url:
            return Url()
        s = str(url)
        try:
            if "://" in s:
                parts = _up.urlsplit(s)
                scheme = parts.scheme.lower() or None
            elif s[0] in "/?#":
                parts = _up.urlsplit(s)
                scheme = None
            else:
                # "host:8080/p" has no scheme; urlsplit needs the // to see a host.
                parts = _up.urlsplit("//" + s)
                scheme = None
            host = parts.hostname
            port = parts.port
        except ValueError as exc:
            raise LocationParseError(s) from exc
        auth = None
        if "@" in parts.netloc:
            auth = parts.netloc.rsplit("@", 1)[0] or None
        return Url(
            scheme=scheme,
            auth=auth,
            host=host,
            port=port,
            path=parts.path or None,
            query=parts.query or None,
            fragment=parts.fragment or None,
        )

    SKIP_HEADER = "@@@SKIP_HEADER@@@"
    SKIPPABLE_HEADERS = frozenset(["accept-encoding", "host", "user-agent"])

    def make_headers(
        keep_alive=None,
        accept_encoding=None,
        user_agent=None,
        basic_auth=None,
        proxy_basic_auth=None,
        disable_cache=None,
    ):
        """Builds a request header dict, like urllib3.util.make_headers."""
        import base64 as _b64

        headers = {}
        if accept_encoding:
            if isinstance(accept_encoding, str):
                pass
            elif isinstance(accept_encoding, (list, tuple)):
                accept_encoding = ",".join(accept_encoding)
            else:
                accept_encoding = "gzip,deflate"
            headers["accept-encoding"] = accept_encoding
        if user_agent:
            headers["user-agent"] = user_agent
        if keep_alive:
            headers["connection"] = "keep-alive"
        if basic_auth:
            headers["authorization"] = "Basic " + _b64.b64encode(
                basic_auth.encode("utf-8")
            ).decode("utf-8")
        if proxy_basic_auth:
            headers["proxy-authorization"] = "Basic " + _b64.b64encode(
                proxy_basic_auth.encode("utf-8")
            ).decode("utf-8")
        if disable_cache:
            headers["cache-control"] = "no-cache"
        return headers

    def format_header_param(name, value):
        v = str(value).replace("\\", "\\\\").replace('"', '\\"')
        return str(name) + '="' + v + '"'

    def guess_content_type(filename, default="application/octet-stream"):
        if filename:
            import mimetypes as _mt

            return _mt.guess_type(filename)[0] or default
        return default

    class RequestField:
        """urllib3.fields.RequestField: one multipart part and its headers."""

        def __init__(self, name, data, filename=None, headers=None):
            self._name = name
            self._filename = filename
            self.data = data
            self.headers = dict(headers) if headers else {}

        @classmethod
        def from_tuples(cls, fieldname, value):
            filename = None
            content_type = None
            if isinstance(value, (list, tuple)):
                if len(value) == 2:
                    filename, data = value
                else:
                    filename, data, content_type = value
            else:
                data = value
            field = cls(fieldname, data, filename=filename)
            field.make_multipart(content_type=content_type)
            return field

        @property
        def name(self):
            return self._name

        @property
        def filename(self):
            return self._filename

        def _render_parts(self, header_parts):
            parts = (
                header_parts.items() if hasattr(header_parts, "items") else header_parts
            )
            return "; ".join(
                format_header_param(k, v) for k, v in parts if v is not None
            )

        def render_headers(self):
            lines = []
            ordered = ("Content-Disposition", "Content-Type", "Content-Location")
            for k in ordered:
                v = self.headers.get(k)
                if v:
                    lines.append(str(k) + ": " + str(v))
            for k, v in self.headers.items():
                if k not in ordered and v:
                    lines.append(str(k) + ": " + str(v))
            lines.append("\r\n")
            return "\r\n".join(lines)

        def make_multipart(
            self, content_disposition=None, content_type=None, content_location=None
        ):
            parts = self._render_parts(
                (("name", self._name), ("filename", self._filename))
            )
            self.headers["Content-Disposition"] = (
                content_disposition or "form-data"
            ) + "; ".join(["", parts])
            self.headers["Content-Type"] = content_type
            self.headers["Content-Location"] = content_location

        def __repr__(self):
            return "RequestField(" + repr(self._name) + ")"

    class HTTPHeaderDict:
        """Case-insensitive header mapping; repeated keys join with ", "."""

        def __init__(self, data=None, **kw):
            self._store = {}
            if data:
                items = data.items() if hasattr(data, "items") else data
                for k, v in items:
                    self.add(k, v)
            for k, v in kw.items():
                self.add(k, v)

        def add(self, key, value):
            lk = str(key).lower()
            entry = self._store.get(lk)
            if entry is None:
                self._store[lk] = (str(key), [value])
            else:
                entry[1].append(value)

        def __setitem__(self, key, value):
            self._store[str(key).lower()] = (str(key), [value])

        def __delitem__(self, key):
            del self._store[str(key).lower()]

        def get(self, key, default=None):
            e = self._store.get(str(key).lower())
            return ", ".join(str(v) for v in e[1]) if e else default

        def __getitem__(self, key):
            e = self._store.get(str(key).lower())
            if e is None:
                raise KeyError(key)
            return ", ".join(str(v) for v in e[1])

        def getlist(self, key, default=None):
            e = self._store.get(str(key).lower())
            return list(e[1]) if e else ([] if default is None else default)

        getall = getlist

        def __contains__(self, key):
            return str(key).lower() in self._store

        def __iter__(self):
            return iter([k for (k, _v) in self._store.values()])

        def __len__(self):
            return len(self._store)

        def __eq__(self, other):
            if not hasattr(other, "items"):
                return NotImplemented
            mine = {k.lower(): v for k, v in self.items()}
            theirs = {str(k).lower(): v for k, v in other.items()}
            return mine == theirs

        def items(self):
            return [
                (k, ", ".join(str(x) for x in v)) for (k, v) in self._store.values()
            ]

        def keys(self):
            return [k for (k, _v) in self._store.values()]

        def values(self):
            return [v for (_k, v) in self.items()]

        def setdefault(self, key, default=None):
            if key in self:
                return self[key]
            self[key] = default
            return default

        def pop(self, key, *default):
            lk = str(key).lower()
            if lk in self._store:
                return ", ".join(str(v) for v in self._store.pop(lk)[1])
            if default:
                return default[0]
            raise KeyError(key)

        def update(self, other=None, **kw):
            if other:
                items = other.items() if hasattr(other, "items") else other
                for k, v in items:
                    self[k] = v
            for k, v in kw.items():
                self[k] = v

        def copy(self):
            new = HTTPHeaderDict()
            for k, (ok, vals) in self._store.items():
                new._store[k] = (ok, list(vals))
            return new

        def __repr__(self):
            return "HTTPHeaderDict(" + repr(self.items()) + ")"

    class BaseHTTPResponse:
        """urllib3.response.BaseHTTPResponse -- the response base class."""

    class HTTPResponse(BaseHTTPResponse):
        version = 11
        retries = None

        def __init__(self, rr):
            self._rr = rr
            self.status = rr.status_code
            self.reason = getattr(rr, "reason", "")
            self.headers = HTTPHeaderDict(
                rr.headers.items() if hasattr(rr.headers, "items") else rr.headers
            )
            self.data = rr.content
            self.url = getattr(rr, "url", None)
            self._pos = 0

        @property
        def status_code(self):
            return self.status

        def read(self, amt=None, decode_content=None, cache_content=False):
            if amt is None:
                chunk = self.data[self._pos :]
                self._pos = len(self.data)
                return chunk
            chunk = self.data[self._pos : self._pos + amt]
            self._pos += len(chunk)
            return chunk

        def readinto(self, b):
            chunk = self.read(len(b))
            b[: len(chunk)] = chunk
            return len(chunk)

        def readable(self):
            return True

        def stream(self, amt=2**16, decode_content=None):
            while True:
                chunk = self.read(amt)
                if not chunk:
                    return
                yield chunk

        def __iter__(self):
            buf = b""
            for chunk in self.stream():
                buf += chunk
                while b"\n" in buf:
                    line, buf = buf.split(b"\n", 1)
                    yield line + b"\n"
            if buf:
                yield buf

        def json(self):
            return _json.loads(self.data.decode("utf-8"))

        def geturl(self):
            return self.url

        def info(self):
            return self.headers

        def getheader(self, name, default=None):
            return self.headers.get(name, default)

        def getheaders(self):
            return self.headers

        def drain_conn(self):
            return None

        def release_conn(self):
            return None

        def close(self):
            self._pos = len(self.data)

        @property
        def closed(self):
            return self._pos >= len(self.data)

        def __repr__(self):
            return "<HTTPResponse status=" + str(self.status) + ">"

    def choose_boundary():
        """Random multipart boundary. No `uuid`: that import is not portable here."""
        return _os.urandom(16).hex()

    def _pairs(fields):
        return list(fields.items() if hasattr(fields, "items") else fields)

    def _field_objects(fields):
        for field in _pairs(fields):
            yield (
                field
                if isinstance(field, RequestField)
                else (RequestField.from_tuples(*field))
            )

    def encode_multipart_formdata(fields, boundary=None):
        """Encodes `fields` as multipart/form-data, like urllib3.filepost."""
        if boundary is None:
            boundary = choose_boundary()
        out = []
        for field in _field_objects(fields):
            out.append(("--" + boundary + "\r\n").encode("utf-8"))
            out.append(field.render_headers().encode("utf-8"))
            value = field.data
            if isinstance(value, str):
                value = value.encode("utf-8")
            elif not isinstance(value, (bytes, bytearray)):
                value = str(value).encode("utf-8")
            out.append(bytes(value) + b"\r\n")
        out.append(("--" + boundary + "--\r\n").encode("utf-8"))
        return b"".join(out), "multipart/form-data; boundary=" + boundary

    # ---- TLS ---------------------------------------------------------------
    # urllib3 carries its TLS options on the pool/manager; the requests shim
    # underneath spells them verify=/cert=. Mapping one onto the other is what
    # makes `PoolManager(cert_reqs="CERT_NONE")` reach the socket instead of
    # being swallowed as an unknown kwarg.
    _VERSION_KEYS = ("ssl_version", "ssl_minimum_version", "ssl_maximum_version")

    _TLS_KEYS = (
        "cert_reqs",
        "ca_certs",
        "ca_cert_dir",
        "cert_file",
        "key_file",
        "assert_hostname",
        "assert_fingerprint",
        "ciphers",
        "ssl_context",
    ) + _VERSION_KEYS

    # Options this transport cannot honour. They are REFUSED, never ignored:
    # dropping a fingerprint pin or a cipher list reports a guarantee the
    # connection does not have -- the caller pinned something and nothing was
    # pinned. They are in _TLS_KEYS precisely so they are SEEN and refused.
    _UNSUPPORTED_TLS = (
        (
            "assert_fingerprint",
            "the peer certificate never reaches this transport, so no fingerprint"
            " can be compared -- pin the issuing CA with ca_certs= instead",
        ),
        (
            "ciphers",
            "this runtime's JSSE-backed ssl cannot select an OpenSSL cipher list"
            " ('No cipher can be selected.') -- pin ssl_minimum_version= instead",
        ),
    )

    _LEGACY_TLS_VERSIONS = (
        ("PROTOCOL_SSLv3", "SSLv3"),
        ("PROTOCOL_TLSv1", "TLSv1"),
        ("PROTOCOL_TLSv1_1", "TLSv1_1"),
        ("PROTOCOL_TLSv1_2", "TLSv1_2"),
    )

    def _split_tls(kw):
        """Picks urllib3's TLS options out of a pool's kwargs (the rest is dropped)."""
        return {k: kw[k] for k in _TLS_KEYS if k in kw}

    def resolve_cert_reqs(candidate):
        """urllib3.util.ssl_.resolve_cert_reqs: None means verify; an ssl.CERT_*
        member or its int passes through; a string may be spelled with or without
        the CERT_ prefix, because upstream resolves it with a getattr on ssl --
        `cert_reqs="NONE"` is as real as `cert_reqs="CERT_NONE"`. An unknown name
        is REFUSED: quietly verifying a name the caller meant as CERT_NONE turns
        their mistake into a mystery handshake failure."""
        import ssl as _ssl

        if candidate is None:
            return _ssl.CERT_REQUIRED
        if isinstance(candidate, bool):
            return _ssl.CERT_REQUIRED if candidate else _ssl.CERT_NONE
        if isinstance(candidate, int):
            return candidate
        name = str(getattr(candidate, "name", candidate)).upper()
        if not name.startswith("CERT_"):
            name = "CERT_" + name
        value = getattr(_ssl, name, None)
        if value is None:
            raise ValueError(
                "urllib3 shim: unknown cert_reqs "
                + repr(candidate)
                + " -- expected CERT_NONE, CERT_OPTIONAL or CERT_REQUIRED"
            )
        return value

    def resolve_ssl_version(candidate):
        """urllib3.util.ssl_.resolve_ssl_version: a PROTOCOL_* constant, its bare
        name, or None for this runtime's default client protocol."""
        import ssl as _ssl

        if candidate is None:
            return getattr(_ssl, "PROTOCOL_TLS_CLIENT", None)
        if isinstance(candidate, int):
            return candidate
        name = str(candidate)
        value = getattr(_ssl, name, None)
        if value is None:
            value = getattr(_ssl, "PROTOCOL_" + name, None)
        if value is None:
            raise ValueError("urllib3 shim: unknown ssl_version " + repr(candidate))
        return value

    def _refuse_unsupported_tls(opts):
        """Raises for a TLS option that would otherwise be silently dropped."""
        for key, why in _UNSUPPORTED_TLS:
            if opts.get(key) is not None:
                raise NotImplementedError(
                    "urllib3 shim: " + key + " is not supported -- " + why
                )

    def _apply_tls_versions(ctx, opts):
        """Applies urllib3's version bounds to a context. `ssl_minimum_version` /
        `ssl_maximum_version` are ssl.TLSVersion members; the deprecated
        `ssl_version` names ONE protocol, so it pins both bounds to it (a
        PROTOCOL_TLS / PROTOCOL_TLS_CLIENT means "any version" and pins nothing).
        """
        import ssl as _ssl

        low = opts.get("ssl_minimum_version")
        high = opts.get("ssl_maximum_version")
        legacy = opts.get("ssl_version")
        if legacy is not None and low is None and high is None:
            legacy = resolve_ssl_version(legacy)
            for attr, version in _LEGACY_TLS_VERSIONS:
                if getattr(_ssl, attr, None) == legacy:
                    low = high = getattr(_ssl.TLSVersion, version, None)
                    break
        if low is not None:
            ctx.minimum_version = low
        if high is not None:
            ctx.maximum_version = high
        return ctx

    def create_urllib3_context(
        ssl_version=None,
        cert_reqs=None,
        options=None,
        ciphers=None,
        ssl_minimum_version=None,
        ssl_maximum_version=None,
    ):
        """urllib3.util.ssl_.create_urllib3_context: the ssl.SSLContext real code
        builds, tweaks and then hands back as `ssl_context=` -- which this shim
        gives the transport verbatim."""
        import ssl as _ssl

        _refuse_unsupported_tls({"ciphers": ciphers})
        ctx = _ssl.create_default_context()
        if cert_reqs is not None:
            reqs = resolve_cert_reqs(cert_reqs)
            if reqs == _ssl.CERT_NONE:
                # CERT_NONE while the name check is still on is a ValueError.
                ctx.check_hostname = False
            ctx.verify_mode = reqs
        if options is not None:
            ctx.options |= options
        return _apply_tls_versions(
            ctx,
            {
                "ssl_version": ssl_version,
                "ssl_minimum_version": ssl_minimum_version,
                "ssl_maximum_version": ssl_maximum_version,
            },
        )

    def _tls_pair(opts):
        """Maps urllib3's TLS options onto the requests shim's (verify, cert).

        `cert_reqs=CERT_NONE` -- the string, the int or the ssl enum -- turns
        verification off; `ca_certs` / `ca_cert_dir` name a CA bundle;
        `cert_file` + `key_file` are the client certificate; a ready
        `ssl_context` is handed through, because the requests shim accepts an
        SSLContext as `verify`; and `assert_hostname=False` is applied to the
        built context, since requests' own vocabulary cannot say "verify the
        chain but skip the name".
        """
        if not opts:
            return None, None
        _refuse_unsupported_tls(opts)
        verify = None
        reqs = opts.get("cert_reqs")
        if reqs is not None:
            verify = resolve_cert_reqs(reqs) != 0
        bundle = opts.get("ca_certs") or opts.get("ca_cert_dir")
        if bundle and verify is not False:
            verify = bundle
        cert_file, key_file = opts.get("cert_file"), opts.get("key_file")
        cert = (cert_file, key_file) if (cert_file and key_file) else cert_file
        if opts.get("ssl_context") is not None:
            verify = opts["ssl_context"]
        skip_name = opts.get("assert_hostname") is False
        if skip_name or any(opts.get(k) is not None for k in _VERSION_KEYS):
            # Anything the requests vocabulary cannot say -- "verify the chain but
            # skip the name", a version bound -- needs the context built HERE.
            ctx = _req()._vis_tls_context(
                verify, cert, check_hostname=False if skip_name else None
            )
            if ctx is None:
                # "change nothing" answers None -- but a version bound has to be
                # SET on something, so the default context is built here.
                ctx = create_urllib3_context()
            verify = _apply_tls_versions(ctx, opts)
        return verify, cert

    def _dispatch(
        method,
        url,
        fields=None,
        body=None,
        headers=None,
        json_body=None,
        timeout=None,
        preload_content=True,
        encode_multipart=True,
        multipart_boundary=None,
        tls=None,
        **_ignored,
    ):
        rq = _req()
        # Pool-level TLS options, overridden by anything given on the call.
        verify, cert = _tls_pair(dict(tls or {}, **_split_tls(_ignored)))
        if isinstance(timeout, Timeout):
            timeout = timeout.as_requests()
        m = str(method).upper()
        params = None
        data = None
        hdr = dict(headers) if headers else {}
        has_ct = any(str(k).lower() == "content-type" for k in hdr)
        if fields is not None:
            if m in ("GET", "HEAD", "DELETE", "OPTIONS"):
                params = fields
            elif encode_multipart:
                data, ct = encode_multipart_formdata(fields, multipart_boundary)
                if not has_ct:
                    hdr["Content-Type"] = ct
            else:
                data = _up.urlencode(_pairs(fields)).encode("utf-8")
                if not has_ct:
                    hdr["Content-Type"] = "application/x-www-form-urlencoded"
        if body is not None:
            data = body
        try:
            rr = rq.request(
                m,
                str(url),
                params=params,
                data=data,
                json=json_body,
                headers=hdr or None,
                timeout=timeout,
                verify=verify,
                cert=cert,
            )
        except PermissionError:
            raise  # vis network guard denial -- keep the clear message legible
        except Exception as e:
            if not isinstance(e, rq.exceptions.RequestException):
                # Not a transport failure at all: an unreadable CA bundle, a
                # refused option, a bad argument. Upstream lets these through
                # verbatim, and dressing one as ProtocolError hides what to fix.
                raise
            en = type(e).__name__
            msg = str(e) or en
            if "ConnectTimeout" in en:
                raise ConnectTimeoutError(msg)
            if "ReadTimeout" in en:
                raise ReadTimeoutError(msg)
            if "Timeout" in en:
                raise TimeoutError(msg)
            if "Schema" in en or "URL" in en or "Location" in en:
                raise LocationParseError(msg)
            if "SSL" in en or "Certificate" in en:
                # A certificate failure is urllib3's SSLError -- the class real
                # code catches. It used to arrive as a bare ProtocolError, so
                # `except urllib3.exceptions.SSLError` never fired.
                raise SSLError(msg)
            if "Connection" in en:
                raise NewConnectionError(msg)
            raise ProtocolError(msg)
        return HTTPResponse(rr)

    class PoolManager:
        def __init__(self, num_pools=10, headers=None, **kw):
            self._headers = dict(headers or {})
            self._tls = _split_tls(kw)

        def request(
            self, method, url, fields=None, headers=None, body=None, json=None, **kw
        ):
            hdr = dict(self._headers)
            if headers:
                hdr.update(headers)
            return _dispatch(
                method,
                url,
                fields=fields,
                body=body,
                headers=hdr or None,
                json_body=json,
                tls=self._tls,
                **kw,
            )

        def urlopen(self, method, url, body=None, headers=None, **kw):
            return _dispatch(
                method, url, body=body, headers=headers, tls=self._tls, **kw
            )

        def clear(self):
            return None

        def connection_from_host(
            self, host, port=None, scheme="http", pool_kwargs=None
        ):
            cls = HTTPSConnectionPool if scheme == "https" else HTTPConnectionPool
            return cls(
                host,
                port=port,
                headers=self._headers,
                **dict(self._tls, **(pool_kwargs or {})),
            )

        def connection_from_url(self, url, pool_kwargs=None):
            u = parse_url(url)
            return self.connection_from_host(
                u.host, port=u.port, scheme=u.scheme or "http", pool_kwargs=pool_kwargs
            )

        def __enter__(self):
            return self

        def __exit__(self, *a):
            return False

    class HTTPConnectionPool:
        scheme = "http"
        port_by_scheme = {"http": 80, "https": 443}

        def __init__(self, host, port=None, headers=None, **kw):
            self.host = host
            self.port = port
            self._headers = dict(headers or {})
            self._tls = _split_tls(kw)

        def _url(self, path):
            base = self.scheme + "://" + str(self.host)
            if self.port and int(self.port) != self.port_by_scheme[self.scheme]:
                base = base + ":" + str(self.port)
            return base + str(path)

        def request(self, method, url, fields=None, headers=None, body=None, **kw):
            return _dispatch(
                method,
                self._url(url),
                fields=fields,
                body=body,
                headers=headers or self._headers,
                tls=self._tls,
                **kw,
            )

        urlopen = request

    class HTTPSConnectionPool(HTTPConnectionPool):
        scheme = "https"

        def __init__(self, host, port=443, **kw):
            super().__init__(host, port=port, **kw)

    class ProxyManager(PoolManager):
        """Records the proxy and otherwise behaves like a PoolManager: the sandbox
        already routes every request through its own egress proxy."""

        def __init__(
            self, proxy_url, num_pools=10, headers=None, proxy_headers=None, **_ignored
        ):
            super().__init__(num_pools=num_pools, headers=headers, **_ignored)
            if isinstance(proxy_url, HTTPConnectionPool):
                proxy_url = (
                    proxy_url.scheme
                    + "://"
                    + str(proxy_url.host)
                    + ":"
                    + str(proxy_url.port)
                )
            self.proxy = parse_url(proxy_url)
            self.proxy_headers = dict(proxy_headers or {})
            if self.proxy.scheme not in ("http", "https"):
                raise ProxySchemeUnknown(self.proxy.scheme)

        def request(self, method, url, headers=None, **kw):
            hdr = dict(self.proxy_headers)
            hdr.update(headers or {})
            return super().request(method, url, headers=hdr or None, **kw)

    def proxy_from_url(url, **kw):
        return ProxyManager(proxy_url=url, **kw)

    def connection_from_url(url, **kw):
        return PoolManager(**kw).connection_from_url(url)

    def _top_request(method, url, **kw):
        return PoolManager().request(method, url, **kw)

    def disable_warnings(category=None):
        """Silences the shim's security warnings -- the very call real code makes
        before an intentionally unverified request."""
        import warnings as _warnings

        _warnings.simplefilter("ignore", category or InsecureRequestWarning)

    def add_stderr_logger(level=None):
        return None

    def _mk(name, parent, **attrs):
        """Creates a submodule, registers it in sys.modules and hangs it off `parent`.

        Every submodule real code imports (`urllib3.util.retry`, `urllib3.response`,
        ...) must exist in sys.modules up front: these modules have no loader of
        their own, so an unregistered name fails with "'urllib3' is not a package".
        """
        m = _types.ModuleType(name)
        for k, v in attrs.items():
            setattr(m, k, v)
        _sys.modules[name] = m
        setattr(parent, name.rsplit(".", 1)[-1], m)
        return m

    mod = _types.ModuleType("urllib3")
    mod.__doc__ = (
        "vis sandbox urllib3-compat shim (thin wrapper over the requests shim)."
    )
    mod.__path__ = []
    mod.__version__ = "2.2.0-vis"
    _sys.modules["urllib3"] = mod

    _mk(
        "urllib3.exceptions",
        mod,
        HTTPError=HTTPError,
        HTTPWarning=HTTPWarning,
        PoolError=PoolError,
        RequestError=RequestError,
        MaxRetryError=MaxRetryError,
        TimeoutError=TimeoutError,
        ConnectTimeoutError=ConnectTimeoutError,
        ReadTimeoutError=ReadTimeoutError,
        NewConnectionError=NewConnectionError,
        NameResolutionError=NameResolutionError,
        ProtocolError=ProtocolError,
        ConnectionError=ProtocolError,
        SSLError=SSLError,
        ProxyError=ProxyError,
        DecodeError=DecodeError,
        ResponseError=ResponseError,
        LocationValueError=LocationValueError,
        LocationParseError=LocationParseError,
        URLSchemeUnknown=URLSchemeUnknown,
        ProxySchemeUnknown=ProxySchemeUnknown,
        ClosedPoolError=ClosedPoolError,
        EmptyPoolError=EmptyPoolError,
        FullPoolError=FullPoolError,
        HostChangedError=HostChangedError,
        InvalidHeader=InvalidHeader,
        HeaderParsingError=HeaderParsingError,
        IncompleteRead=IncompleteRead,
        InvalidChunkLength=InvalidChunkLength,
        ResponseNotChunked=ResponseNotChunked,
        UnrewindableBodyError=UnrewindableBodyError,
        BodyNotHttplibCompatible=BodyNotHttplibCompatible,
        SecurityWarning=SecurityWarning,
        InsecureRequestWarning=InsecureRequestWarning,
        InsecurePlatformWarning=InsecurePlatformWarning,
        NotOpenSSLWarning=NotOpenSSLWarning,
        SystemTimeWarning=SystemTimeWarning,
        DependencyWarning=DependencyWarning,
    )
    _mk(
        "urllib3.filepost",
        mod,
        encode_multipart_formdata=encode_multipart_formdata,
        choose_boundary=choose_boundary,
    )
    _mk(
        "urllib3.fields",
        mod,
        RequestField=RequestField,
        guess_content_type=guess_content_type,
        format_header_param=format_header_param,
    )
    _mk("urllib3._collections", mod, HTTPHeaderDict=HTTPHeaderDict)
    _mk(
        "urllib3.response",
        mod,
        HTTPResponse=HTTPResponse,
        BaseHTTPResponse=BaseHTTPResponse,
    )
    _mk(
        "urllib3.poolmanager",
        mod,
        PoolManager=PoolManager,
        ProxyManager=ProxyManager,
        proxy_from_url=proxy_from_url,
    )
    _mk(
        "urllib3.connectionpool",
        mod,
        HTTPConnectionPool=HTTPConnectionPool,
        HTTPSConnectionPool=HTTPSConnectionPool,
        connection_from_url=connection_from_url,
    )
    _util_mod = _mk(
        "urllib3.util",
        mod,
        Retry=Retry,
        Timeout=Timeout,
        Url=Url,
        parse_url=parse_url,
        make_headers=make_headers,
        SKIP_HEADER=SKIP_HEADER,
        SKIPPABLE_HEADERS=SKIPPABLE_HEADERS,
        create_urllib3_context=create_urllib3_context,
        resolve_cert_reqs=resolve_cert_reqs,
        resolve_ssl_version=resolve_ssl_version,
    )
    _util_mod.__path__ = []
    _mk(
        "urllib3.util.ssl_",
        _util_mod,
        create_urllib3_context=create_urllib3_context,
        resolve_cert_reqs=resolve_cert_reqs,
        resolve_ssl_version=resolve_ssl_version,
    )
    _mk("urllib3.util.retry", _util_mod, Retry=Retry)
    _mk("urllib3.util.timeout", _util_mod, Timeout=Timeout)
    _mk("urllib3.util.url", _util_mod, Url=Url, parse_url=parse_url)
    _mk(
        "urllib3.util.request",
        _util_mod,
        make_headers=make_headers,
        SKIP_HEADER=SKIP_HEADER,
        SKIPPABLE_HEADERS=SKIPPABLE_HEADERS,
    )

    _exports = (
        ("HTTPConnectionPool", HTTPConnectionPool),
        ("HTTPSConnectionPool", HTTPSConnectionPool),
        ("PoolManager", PoolManager),
        ("ProxyManager", ProxyManager),
        ("HTTPResponse", HTTPResponse),
        ("BaseHTTPResponse", BaseHTTPResponse),
        ("HTTPHeaderDict", HTTPHeaderDict),
        ("HTTPError", HTTPError),
        ("MaxRetryError", MaxRetryError),
        ("Retry", Retry),
        ("Timeout", Timeout),
        ("Url", Url),
        ("parse_url", parse_url),
        ("make_headers", make_headers),
        ("connection_from_url", connection_from_url),
        ("proxy_from_url", proxy_from_url),
        ("encode_multipart_formdata", encode_multipart_formdata),
        ("request", _top_request),
        ("disable_warnings", disable_warnings),
        ("add_stderr_logger", add_stderr_logger),
    )
    for _n, _o in _exports:
        setattr(mod, _n, _o)
    mod.__all__ = [_n for _n, _o in _exports]
    try:
        _bi.urllib3 = mod
    except Exception:
        pass


__vis_install_urllib3__()
del __vis_install_urllib3__
