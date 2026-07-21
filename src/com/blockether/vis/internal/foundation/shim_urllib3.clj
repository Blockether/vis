(ns com.blockether.vis.internal.foundation.shim-urllib3
  "Built-in sandbox SHIM: a `urllib3`-compatible module for the model's Python
   sandbox, implemented as a thin wrapper over the already-installed `requests`
   shim (which rides the sandbox socket via stdlib urllib and honours the network
   guard). No pip, no native wheel, no host bridge.

   The preamble publishes a `urllib3` module (plus `urllib3.exceptions`) into
   `sys.modules` and staples it onto builtins. It exposes the surface agents
   reach for: `PoolManager` / `HTTPConnectionPool` / `HTTPSConnectionPool` with
   `.request(method, url, fields=, headers=, body=, json=)`, a top-level
   `urllib3.request(...)` (urllib3 2.x), an `HTTPResponse` (`.status`, `.data`,
   `.headers`, `.json()`, `.read()`, `.getheader()`), `HTTPHeaderDict`,
   `disable_warnings()`, and the `urllib3.exceptions` tree (`HTTPError`,
   `MaxRetryError`, `NewConnectionError`, `ReadTimeoutError`, `ProtocolError`,
   `InsecureRequestWarning`). Real connection pooling / retries are no-ops."
  (:require [com.blockether.vis.core :as vis]))

(def ^:private urllib3-compat-shim-src
  "
def __vis_install_urllib3__():
    import sys as _sys, types as _types, json as _json
    _bi = _sys.modules['builtins']

    def _req():
        import requests as _r
        return _r

    class HTTPError(Exception):
        pass
    class RequestError(HTTPError):
        pass
    class MaxRetryError(RequestError):
        pass
    class NewConnectionError(RequestError):
        pass
    class ConnectTimeoutError(RequestError):
        pass
    class ReadTimeoutError(RequestError):
        pass
    class TimeoutError(RequestError):
        pass
    class ProtocolError(HTTPError):
        pass
    class LocationParseError(HTTPError):
        pass
    class Retry:
        def __init__(self, total=10, connect=None, read=None, redirect=None,
                     status=None, backoff_factor=0, status_forcelist=None, **_ignored):
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
            return 'Retry(total=' + str(self.total) + ')'
    class InsecureRequestWarning(Warning):
        pass

    class HTTPHeaderDict:
        def __init__(self, data=None):
            self._store = {}
            self._order = []
            if data:
                items = data.items() if hasattr(data, 'items') else data
                for k, v in items:
                    self._set(k, v)
        def _set(self, k, v):
            lk = str(k).lower()
            if lk not in self._store:
                self._order.append(str(k))
            self._store[lk] = (str(k), v)
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

    class HTTPResponse:
        def __init__(self, rr):
            self._rr = rr
            self.status = rr.status_code
            self.reason = getattr(rr, 'reason', '')
            self.headers = HTTPHeaderDict(rr.headers.items() if hasattr(rr.headers, 'items') else rr.headers)
            self.data = rr.content
            self._consumed = False
        @property
        def status_code(self):
            return self.status
        def read(self, amt=None):
            if self._consumed:
                return b''
            self._consumed = True
            return self.data
        def json(self):
            return _json.loads(self.data.decode('utf-8'))
        def getheader(self, name, default=None):
            return self.headers.get(name, default)
        def getheaders(self):
            return self.headers.items()
        def release_conn(self):
            return None
        def close(self):
            return None
        def __repr__(self):
            return '<HTTPResponse status=' + str(self.status) + '>'

    def _dispatch(method, url, fields=None, body=None, headers=None,
                  json_body=None, timeout=None, preload_content=True, **_ignored):
        rq = _req()
        m = str(method).upper()
        params = None
        data = None
        if fields is not None:
            if m in ('GET', 'HEAD', 'DELETE'):
                params = fields
            else:
                data = fields
        if body is not None:
            data = body
        try:
            rr = rq.request(m, str(url), params=params, data=data, json=json_body,
                            headers=headers, timeout=timeout)
        except PermissionError:
            raise  # vis network guard denial -- keep the clear message legible
        except Exception as e:
            en = type(e).__name__
            msg = str(e) or en
            if 'Timeout' in en:
                raise TimeoutError(msg)
            if 'Schema' in en or 'URL' in en or 'Location' in en:
                raise LocationParseError(msg)
            if 'Connection' in en:
                raise NewConnectionError(msg)
            raise ProtocolError(msg)
        return HTTPResponse(rr)

    class PoolManager:
        def __init__(self, num_pools=10, headers=None, **_ignored):
            self._headers = dict(headers or {})
        def request(self, method, url, fields=None, headers=None, body=None,
                    json=None, **kw):
            hdr = dict(self._headers)
            if headers:
                hdr.update(headers)
            return _dispatch(method, url, fields=fields, body=body,
                             headers=hdr or None, json_body=json, **kw)
        def urlopen(self, method, url, body=None, headers=None, **kw):
            return _dispatch(method, url, body=body, headers=headers, **kw)
        def clear(self):
            return None
        def __enter__(self):
            return self
        def __exit__(self, *a):
            return False

    class HTTPConnectionPool:
        def __init__(self, host, port=None, headers=None, **_ignored):
            self.host = host
            self.port = port
            self._headers = dict(headers or {})
        def _url(self, path):
            scheme = 'https' if str(self.port) == '443' else 'http'
            base = scheme + '://' + str(self.host)
            if self.port and str(self.port) not in ('80', '443'):
                base = base + ':' + str(self.port)
            return base + str(path)
        def request(self, method, url, fields=None, headers=None, body=None, **kw):
            return _dispatch(method, self._url(url), fields=fields, body=body,
                             headers=headers or self._headers, **kw)
        urlopen = request

    class HTTPSConnectionPool(HTTPConnectionPool):
        def __init__(self, host, port=443, **kw):
            super().__init__(host, port=port, **kw)

    def _top_request(method, url, **kw):
        return PoolManager().request(method, url, **kw)

    def disable_warnings(category=None):
        return None
    def add_stderr_logger(level=None):
        return None

    exc_mod = _types.ModuleType('urllib3.exceptions')
    exc_mod.HTTPError = HTTPError
    exc_mod.RequestError = RequestError
    exc_mod.MaxRetryError = MaxRetryError
    exc_mod.NewConnectionError = NewConnectionError
    exc_mod.ConnectTimeoutError = ConnectTimeoutError
    exc_mod.ReadTimeoutError = ReadTimeoutError
    exc_mod.TimeoutError = TimeoutError
    exc_mod.ProtocolError = ProtocolError
    exc_mod.LocationParseError = LocationParseError
    exc_mod.InsecureRequestWarning = InsecureRequestWarning

    mod = _types.ModuleType('urllib3')
    mod.__doc__ = 'vis sandbox urllib3-compat shim (thin wrapper over the requests shim).'
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
    _util_mod = _types.ModuleType('urllib3.util')
    _util_mod.Retry = Retry
    _retry_mod = _types.ModuleType('urllib3.util.retry')
    _retry_mod.Retry = Retry
    _util_mod.retry = _retry_mod
    mod.util = _util_mod
    _sys.modules['urllib3.util'] = _util_mod
    _sys.modules['urllib3.util.retry'] = _retry_mod
    mod.__version__ = '2.2.0-vis'
    _sys.modules['urllib3'] = mod
    _sys.modules['urllib3.exceptions'] = exc_mod
    try:
        _bi.urllib3 = mod
    except Exception:
        pass

__vis_install_urllib3__()
del __vis_install_urllib3__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-urllib3"
     :ext/description
     "Sandbox shim: a `urllib3`-compatible module (import urllib3 / PoolManager / urllib3.request) implemented as a thin wrapper over the requests shim. No pip, no native wheel, no host bridge."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "urllib3"
       :shim/description
       "urllib3-compatible `urllib3` (PoolManager/HTTPResponse/request) wrapping the requests shim. Not supported: retries, connection pooling, and low-level TLS options are best-effort no-ops."
       :shim/preamble urllib3-compat-shim-src}]}))

(vis/register-extension! vis-extension)
