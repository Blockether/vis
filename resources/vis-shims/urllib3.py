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

    class InsecureRequestWarning(Warning):
        pass

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

    class HTTPResponse:
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

    def _pairs(fields):
        return list(fields.items() if hasattr(fields, "items") else fields)

    def encode_multipart_formdata(fields, boundary=None):
        """Encodes `fields` as multipart/form-data, like urllib3.filepost."""
        if boundary is None:
            boundary = _os.urandom(16).hex()
        out = []
        for k, v in _pairs(fields):
            filename = None
            content_type = None
            if isinstance(v, (list, tuple)):
                if len(v) == 2:
                    filename, value = v
                else:
                    filename, value, content_type = v
            else:
                value = v
            head = "--" + boundary + "\r\n"
            head += 'Content-Disposition: form-data; name="' + str(k) + '"'
            if filename is not None:
                head += '; filename="' + str(filename) + '"'
            head += "\r\n"
            if content_type is not None:
                head += "Content-Type: " + str(content_type) + "\r\n"
            elif filename is not None:
                head += "Content-Type: application/octet-stream\r\n"
            out.append(head.encode("utf-8") + b"\r\n")
            if isinstance(value, str):
                value = value.encode("utf-8")
            elif not isinstance(value, (bytes, bytearray)):
                value = str(value).encode("utf-8")
            out.append(bytes(value) + b"\r\n")
        out.append(("--" + boundary + "--\r\n").encode("utf-8"))
        return b"".join(out), "multipart/form-data; boundary=" + boundary

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
        **_ignored,
    ):
        rq = _req()
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
            )
        except PermissionError:
            raise  # vis network guard denial -- keep the clear message legible
        except Exception as e:
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
            if "Connection" in en:
                raise NewConnectionError(msg)
            raise ProtocolError(msg)
        return HTTPResponse(rr)

    class PoolManager:
        def __init__(self, num_pools=10, headers=None, **_ignored):
            self._headers = dict(headers or {})

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
                **kw,
            )

        def urlopen(self, method, url, body=None, headers=None, **kw):
            return _dispatch(method, url, body=body, headers=headers, **kw)

        def clear(self):
            return None

        def __enter__(self):
            return self

        def __exit__(self, *a):
            return False

    class HTTPConnectionPool:
        scheme = "http"
        port_by_scheme = {"http": 80, "https": 443}

        def __init__(self, host, port=None, headers=None, **_ignored):
            self.host = host
            self.port = port
            self._headers = dict(headers or {})

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
                **kw,
            )

        urlopen = request

    class HTTPSConnectionPool(HTTPConnectionPool):
        scheme = "https"

        def __init__(self, host, port=443, **kw):
            super().__init__(host, port=port, **kw)

    def _top_request(method, url, **kw):
        return PoolManager().request(method, url, **kw)

    def disable_warnings(category=None):
        return None

    def add_stderr_logger(level=None):
        return None

    exc_mod = _types.ModuleType("urllib3.exceptions")
    exc_mod.HTTPError = HTTPError
    exc_mod.PoolError = PoolError
    exc_mod.RequestError = RequestError
    exc_mod.MaxRetryError = MaxRetryError
    exc_mod.TimeoutError = TimeoutError
    exc_mod.ConnectTimeoutError = ConnectTimeoutError
    exc_mod.ReadTimeoutError = ReadTimeoutError
    exc_mod.NewConnectionError = NewConnectionError
    exc_mod.ProtocolError = ProtocolError
    exc_mod.SSLError = SSLError
    exc_mod.ProxyError = ProxyError
    exc_mod.DecodeError = DecodeError
    exc_mod.ResponseError = ResponseError
    exc_mod.LocationValueError = LocationValueError
    exc_mod.LocationParseError = LocationParseError
    exc_mod.InsecureRequestWarning = InsecureRequestWarning

    mod = _types.ModuleType("urllib3")
    mod.__doc__ = (
        "vis sandbox urllib3-compat shim (thin wrapper over the requests shim)."
    )
    mod.PoolManager = PoolManager
    mod.HTTPConnectionPool = HTTPConnectionPool
    mod.HTTPSConnectionPool = HTTPSConnectionPool
    mod.HTTPResponse = HTTPResponse
    mod.HTTPHeaderDict = HTTPHeaderDict
    mod.request = _top_request
    mod.disable_warnings = disable_warnings
    mod.add_stderr_logger = add_stderr_logger
    mod.exceptions = exc_mod
    mod.HTTPError = HTTPError
    mod.MaxRetryError = MaxRetryError
    mod.Retry = Retry
    mod.encode_multipart_formdata = encode_multipart_formdata
    _filepost_mod = _types.ModuleType("urllib3.filepost")
    _filepost_mod.encode_multipart_formdata = encode_multipart_formdata
    mod.filepost = _filepost_mod
    _sys.modules["urllib3.filepost"] = _filepost_mod
    _util_mod = _types.ModuleType("urllib3.util")
    _util_mod.Retry = Retry
    _retry_mod = _types.ModuleType("urllib3.util.retry")
    _retry_mod.Retry = Retry
    _util_mod.retry = _retry_mod
    mod.util = _util_mod
    _sys.modules["urllib3.util"] = _util_mod
    _sys.modules["urllib3.util.retry"] = _retry_mod
    mod.__version__ = "2.2.0-vis"
    _sys.modules["urllib3"] = mod
    _sys.modules["urllib3.exceptions"] = exc_mod
    try:
        _bi.urllib3 = mod
    except Exception:
        pass


__vis_install_urllib3__()
del __vis_install_urllib3__
