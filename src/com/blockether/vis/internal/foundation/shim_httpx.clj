(ns com.blockether.vis.internal.foundation.shim-httpx
  "Built-in sandbox SHIM: an `httpx`-compatible module for the model's Python
   sandbox, implemented as a thin synchronous wrapper over the already-installed
   `requests` shim (which itself rides the sandbox socket via stdlib urllib and
   honours the network guard). No pip, no native wheel, no host bridge.

   The preamble publishes an `httpx` module into `sys.modules` (so `import httpx`
   and `httpx.get(...)` work) and staples it onto builtins. It exposes the sync
   surface agents actually reach for: module-level `get/post/put/patch/delete/
   head/options/request`, a `Client` (with `base_url`, default headers/params,
   context-manager support), an httpx-style `Response` (`.status_code`, `.text`,
   `.content`, `.json()`, `.headers`, `.url`, `.is_success/.is_error/.is_redirect`,
   `.raise_for_status()`), `Headers`, `URL`, `Timeout`, and the `httpx` exception
   tree (`HTTPError`, `RequestError`, `HTTPStatusError`, `TimeoutException`,
   `ConnectError`). Async (`AsyncClient`) is intentionally omitted."
  (:require [com.blockether.vis.core :as vis]))

(def ^:private httpx-compat-shim-src
  "
def __vis_install_httpx__():
    import sys as _sys, types as _types
    _bi = _sys.modules['builtins']

    def _req():
        import requests as _r
        return _r

    class Headers:
        def __init__(self, data=None):
            self._store = {}
            if data:
                items = data.items() if hasattr(data, 'items') else data
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
            return 'Headers(' + repr(self.items()) + ')'

    class URL:
        def __init__(self, raw):
            self._raw = str(raw)
        def __str__(self):
            return self._raw
        def __repr__(self):
            return 'URL(' + repr(self._raw) + ')'
        def __eq__(self, other):
            return str(self) == str(other)

    class HTTPError(Exception):
        pass
    class RequestError(HTTPError):
        def __init__(self, message, request=None):
            super().__init__(message)
            self.request = request
    class TimeoutException(RequestError):
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
    class HTTPStatusError(HTTPError):
        def __init__(self, message, request=None, response=None):
            super().__init__(message)
            self.request = request
            self.response = response

    class Response:
        def __init__(self, rr, req_url=None):
            self._rr = rr
            self.status_code = rr.status_code
            self.headers = Headers(rr.headers.items() if hasattr(rr.headers, 'items') else rr.headers)
            self.url = URL(getattr(rr, 'url', req_url) or req_url)
            self.encoding = getattr(rr, 'encoding', 'utf-8')
        @property
        def content(self):
            return self._rr.content
        @property
        def text(self):
            return self._rr.text
        @property
        def reason_phrase(self):
            return getattr(self._rr, 'reason', '')
        @property
        def is_success(self):
            return 200 <= self.status_code < 300
        @property
        def is_error(self):
            return self.status_code >= 400
        @property
        def is_redirect(self):
            return self.status_code in (301, 302, 303, 307, 308)
        @property
        def is_client_error(self):
            return 400 <= self.status_code < 500
        @property
        def is_server_error(self):
            return 500 <= self.status_code < 600
        def json(self, **kw):
            return self._rr.json(**kw)
        def raise_for_status(self):
            if self.status_code >= 400:
                raise HTTPStatusError('Client error ' + str(self.status_code) + ' for url ' + str(self.url), request=None, response=self)
            return self
        def __repr__(self):
            return '<Response [' + str(self.status_code) + ']>'

    def _dispatch(method, url, kw):
        rq = _req()
        params = kw.pop('params', None)
        headers = kw.pop('headers', None)
        json_body = kw.pop('json', None)
        data = kw.pop('data', None)
        content = kw.pop('content', None)
        if content is not None and data is None:
            data = content
        timeout = kw.pop('timeout', None)
        if not isinstance(timeout, (int, float, type(None))):
            timeout = getattr(timeout, 'connect', None) or None
        cookies = kw.pop('cookies', None)
        auth = kw.pop('auth', None)
        follow = kw.pop('follow_redirects', None)
        if follow is None:
            follow = kw.pop('allow_redirects', True)
        try:
            rr = rq.request(str(method).upper(), str(url), params=params, data=data,
                            json=json_body, headers=headers, cookies=cookies,
                            timeout=timeout, auth=auth, allow_redirects=bool(follow))
        except Exception as e:
            en = type(e).__name__
            msg = str(e) or en
            if 'Timeout' in en:
                raise ConnectTimeout(msg, request=None)
            if 'Schema' in en or 'URL' in en:
                raise InvalidURL(msg, request=None)
            if 'Connection' in en:
                raise ConnectError(msg, request=None)
            raise RequestError(msg, request=None)
        return Response(rr, str(url))

    class Timeout:
        def __init__(self, timeout=None, connect=None, read=None, write=None, pool=None):
            self.connect = connect if connect is not None else timeout
            self.read = read if read is not None else timeout
            self.write = write if write is not None else timeout
            self.pool = pool if pool is not None else timeout

    class Client:
        def __init__(self, base_url='', headers=None, params=None, timeout=None,
                     follow_redirects=True, auth=None, cookies=None, **_ignored):
            self.base_url = str(base_url or '')
            self.headers = Headers(headers or {})
            self._params = params or {}
            self._timeout = timeout
            self._follow = follow_redirects
            self._auth = auth
            self._cookies = cookies
        def _abs(self, url):
            u = str(url)
            if self.base_url and not (u.startswith('http://') or u.startswith('https://')):
                return self.base_url.rstrip('/') + '/' + u.lstrip('/')
            return u
        def _merged(self, kw):
            hdr = {}
            for k, v in self.headers.items():
                hdr[k] = v
            for k, v in (kw.get('headers') or {}).items():
                hdr[k] = v
            if hdr:
                kw['headers'] = hdr
            prm = dict(self._params)
            prm.update(kw.get('params') or {})
            if prm:
                kw['params'] = prm
            kw.setdefault('timeout', self._timeout)
            kw.setdefault('follow_redirects', self._follow)
            kw.setdefault('auth', self._auth)
            kw.setdefault('cookies', self._cookies)
            return kw
        def request(self, method, url, **kw):
            return _dispatch(method, self._abs(url), self._merged(kw))
        def get(self, url, **kw):
            return self.request('GET', url, **kw)
        def post(self, url, **kw):
            return self.request('POST', url, **kw)
        def put(self, url, **kw):
            return self.request('PUT', url, **kw)
        def patch(self, url, **kw):
            return self.request('PATCH', url, **kw)
        def delete(self, url, **kw):
            return self.request('DELETE', url, **kw)
        def head(self, url, **kw):
            return self.request('HEAD', url, **kw)
        def options(self, url, **kw):
            return self.request('OPTIONS', url, **kw)
        def close(self):
            return None
        def __enter__(self):
            return self
        def __exit__(self, *a):
            self.close()
            return False

    def _mod_request(method, url, **kw):
        return _dispatch(method, url, kw)
    def _get(url, **kw):
        return _dispatch('GET', url, kw)
    def _post(url, **kw):
        return _dispatch('POST', url, kw)
    def _put(url, **kw):
        return _dispatch('PUT', url, kw)
    def _patch(url, **kw):
        return _dispatch('PATCH', url, kw)
    def _delete(url, **kw):
        return _dispatch('DELETE', url, kw)
    def _head(url, **kw):
        return _dispatch('HEAD', url, kw)
    def _options(url, **kw):
        return _dispatch('OPTIONS', url, kw)

    mod = _types.ModuleType('httpx')
    mod.__doc__ = 'vis sandbox httpx-compat shim (thin sync wrapper over the requests shim).'
    mod.Response = Response
    mod.Headers = Headers
    mod.URL = URL
    mod.Client = Client
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
    mod.request = _mod_request
    mod.get = _get
    mod.post = _post
    mod.put = _put
    mod.patch = _patch
    mod.delete = _delete
    mod.head = _head
    mod.options = _options
    mod.__version__ = '0.27.0-vis'
    _sys.modules['httpx'] = mod
    try:
        _bi.httpx = mod
    except Exception:
        pass

__vis_install_httpx__()
del __vis_install_httpx__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-httpx"
     :ext/description
     "Sandbox shim: an `httpx`-compatible module (import httpx / httpx.get / httpx.Client) implemented as a thin sync wrapper over the requests shim. No pip, no native wheel, no host bridge."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "httpx"
       :shim/description
       "httpx-compatible `httpx` (get/post/Client/Response/raise_for_status) wrapping the requests shim. Not supported: async `AsyncClient` and HTTP/2 (sync API only)."
       :shim/preamble httpx-compat-shim-src}]}))

(vis/register-extension! vis-extension)
