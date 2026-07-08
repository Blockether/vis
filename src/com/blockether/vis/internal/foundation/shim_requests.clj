(ns com.blockether.vis.internal.foundation.shim-requests
  "Built-in sandbox SHIM: a `requests`-compatible module for the model's Python
   sandbox, backed PURELY by the stdlib `urllib.request` — NO host/JVM bridge,
   NOT a line of Clojure or babashka. `requests` is a third-party wheel that does
   not ship in GraalPy, so agents that reach for `import requests` out of habit
   would otherwise hit ModuleNotFoundError; this extension contributes a
   `:ext/sandbox-shims` entry that `env-python/build-agent-context` installs into
   every sandbox Context (main + every `sub_loop` fork).

   Because every call travels through the sandbox's OWN socket (urllib ->
   http.client -> socket), it automatically honours the network toggle
   (`allowHostSocketAccess`) AND the allow/deny + anti-SSRF `network-guard-python`
   — a JVM `babashka.http-client` bridge would open an egress path OUTSIDE the
   sandbox and disarm all of that, which is exactly why this stays 100% Python.

   Unlike `shim-yaml`/`shim-matplotlib` there are NO `:shim/bindings`: the shim is
   a self-contained Python preamble with zero host callables. It publishes a
   `requests` module into `sys.modules` (so `import requests` works) and staples
   it onto builtins (so `requests.get(...)` works with NO import, like json/os)."
  (:require [com.blockether.vis.core :as vis]))

(def ^:private requests-compat-shim-src
  "Pure-Python preamble that publishes a `requests`-compatible module backed by
   the stdlib `urllib.request`. Zero host callables. Published into `sys.modules`
   under `requests` (so `import requests` finds it) AND stapled onto builtins (so
   `requests.get(...)` works with NO import). INLINED here so it ships in-jar with
   no separate `.py` resource. Installed once per sandbox context (main + every
   `sub_loop` fork), BEFORE the baseline snapshot so its `__vis_*` names are
   filtered out of the model-visible live-vars view. The Python body uses ONLY
   single-quoted string literals and `chr(...)` for double-quote/CRLF, so this
   Clojure string needs zero backslash escaping."
  "# vis sandbox requests-compat shim.
#
# The agent sandbox ships no third-party `requests` wheel. This shim publishes a
# requests-compatible module whose HTTP verbs delegate to the stdlib
# urllib.request (pure Python, no host/JVM bridge), so every request rides the
# sandbox's own socket and honours the network toggle + allow/deny guard.
# Published into sys.modules so `import requests` works, and stapled onto
# builtins so requests.get(...) needs no import (mirrors json/os/yaml).

def __vis_install_requests_compat__():
    import sys
    import types
    import os as _os
    import base64 as _b64
    import datetime as _dt
    import json as _json
    import urllib.request as _ur
    import urllib.parse as _up
    import urllib.error as _ue

    _DEFAULT_TIMEOUT = 30
    _UA = 'vis-requests-shim/2.0 (urllib)'
    _Q = chr(34)
    _SQ = chr(39)
    _CRLF = chr(13) + chr(10)

    # ---- exceptions -------------------------------------------------------
    class RequestException(IOError):
        def __init__(self, *args, response=None, request=None):
            super().__init__(*args)
            self.response = response
            self.request = request

    class HTTPError(RequestException):
        pass

    class ConnectionError(RequestException):
        pass

    class ProxyError(ConnectionError):
        pass

    class SSLError(ConnectionError):
        pass

    class Timeout(RequestException):
        pass

    class ConnectTimeout(ConnectionError, Timeout):
        pass

    class ReadTimeout(Timeout):
        pass

    class URLRequired(RequestException):
        pass

    class TooManyRedirects(RequestException):
        pass

    class MissingSchema(RequestException, ValueError):
        pass

    class InvalidSchema(RequestException, ValueError):
        pass

    class InvalidURL(RequestException, ValueError):
        pass

    class InvalidProxyURL(InvalidURL):
        pass

    class InvalidHeader(RequestException, ValueError):
        pass

    class InvalidJSONError(RequestException):
        pass

    class JSONDecodeError(InvalidJSONError, ValueError):
        pass

    class ChunkedEncodingError(RequestException):
        pass

    class ContentDecodingError(RequestException):
        pass

    class StreamConsumedError(RequestException, TypeError):
        pass

    class RetryError(RequestException):
        pass

    class UnrewindableBodyError(RequestException):
        pass

    class RequestsWarning(Warning):
        pass

    class FileModeWarning(RequestsWarning, DeprecationWarning):
        pass

    class RequestsDependencyWarning(RequestsWarning):
        pass

    _EXC = {
        'RequestException': RequestException, 'HTTPError': HTTPError,
        'ConnectionError': ConnectionError, 'ProxyError': ProxyError,
        'SSLError': SSLError, 'Timeout': Timeout, 'ConnectTimeout': ConnectTimeout,
        'ReadTimeout': ReadTimeout, 'URLRequired': URLRequired,
        'TooManyRedirects': TooManyRedirects, 'MissingSchema': MissingSchema,
        'InvalidSchema': InvalidSchema, 'InvalidURL': InvalidURL,
        'InvalidProxyURL': InvalidProxyURL, 'InvalidHeader': InvalidHeader,
        'InvalidJSONError': InvalidJSONError, 'JSONDecodeError': JSONDecodeError,
        'ChunkedEncodingError': ChunkedEncodingError,
        'ContentDecodingError': ContentDecodingError,
        'StreamConsumedError': StreamConsumedError, 'RetryError': RetryError,
        'UnrewindableBodyError': UnrewindableBodyError,
        'RequestsWarning': RequestsWarning, 'FileModeWarning': FileModeWarning,
        'RequestsDependencyWarning': RequestsDependencyWarning,
    }

    # ---- structures -------------------------------------------------------
    class CaseInsensitiveDict(dict):
        # Minimal case-insensitive headers mapping: preserves the last-set casing
        # but get/__getitem__/__contains__/__delitem__ match case-insensitively,
        # like requests.structures.CaseInsensitiveDict.
        def __init__(self, data=None):
            super().__init__()
            if data:
                items = data.items() if hasattr(data, 'items') else data
                for k, v in items:
                    self[k] = v

        def _canon(self, key):
            lk = str(key).lower()
            for k in list(super().keys()):
                if str(k).lower() == lk:
                    return k
            return None

        def __getitem__(self, key):
            k = self._canon(key)
            if k is None:
                raise KeyError(key)
            return super().__getitem__(k)

        def __setitem__(self, key, value):
            k = self._canon(key)
            if k is not None:
                super().__delitem__(k)
            super().__setitem__(key, value)

        def __delitem__(self, key):
            k = self._canon(key)
            if k is None:
                raise KeyError(key)
            super().__delitem__(k)

        def __contains__(self, key):
            return self._canon(key) is not None

        def get(self, key, default=None):
            k = self._canon(key)
            return super().__getitem__(k) if k is not None else default

    class LookupDict(dict):
        # Attribute-accessible dict (like requests.structures.LookupDict): values
        # live in __dict__ so both `d.ok` and `d['ok']` resolve.
        def __init__(self, name=None):
            self.name = name
            super().__init__()

        def __repr__(self):
            return '<lookup ' + _SQ + str(self.name) + _SQ + '>'

        def __getitem__(self, key):
            return self.__dict__.get(key, None)

        def get(self, key, default=None):
            return self.__dict__.get(key, default)

    class RequestsCookieJar(dict):
        # Dict-backed cookie jar: enough for get/set/update + name access.
        def get_dict(self, domain=None, path=None):
            return dict(self)

        def set(self, name, value, **kw):
            self[name] = value
            return value

        def update(self, other=None, **kw):
            if other:
                items = other.items() if hasattr(other, 'items') else other
                for k, v in items:
                    self[k] = v
            for k, v in kw.items():
                self[k] = v

    # ---- auth -------------------------------------------------------------
    def _basic_auth_str(username, password):
        if isinstance(username, str):
            username = username.encode('latin1')
        if isinstance(password, str):
            password = password.encode('latin1')
        token = _b64.b64encode(username + b':' + password).decode('ascii')
        return 'Basic ' + token

    class AuthBase:
        def __call__(self, r):
            raise NotImplementedError('Auth hooks must be callable.')

    class HTTPBasicAuth(AuthBase):
        def __init__(self, username, password):
            self.username = username
            self.password = password

        def __eq__(self, other):
            return (self.username == getattr(other, 'username', None)
                    and self.password == getattr(other, 'password', None))

        def __call__(self, r):
            r.headers['Authorization'] = _basic_auth_str(self.username, self.password)
            return r

    class HTTPProxyAuth(HTTPBasicAuth):
        def __call__(self, r):
            r.headers['Proxy-Authorization'] = _basic_auth_str(self.username, self.password)
            return r

    class HTTPDigestAuth(AuthBase):
        # Digest challenge/response is not implemented over plain urllib here;
        # this is a construct-and-import placeholder so code that references it
        # does not explode (it applies no header).
        def __init__(self, username, password):
            self.username = username
            self.password = password

        def __call__(self, r):
            return r

    class _PreparedShim:
        # Lightweight object handed to AuthBase.__call__ so custom auth callables
        # can mutate the outgoing headers before the urllib Request is built.
        def __init__(self, method, url, headers, body):
            self.method = method
            self.url = url
            self.headers = headers
            self.body = body

    def _apply_auth(auth, prep):
        if auth is None:
            return
        if isinstance(auth, (tuple, list)) and len(auth) == 2:
            prep.headers['Authorization'] = _basic_auth_str(auth[0], auth[1])
            return
        if callable(auth):
            auth(prep)
            return
        u = getattr(auth, 'username', None)
        if u is not None:
            prep.headers['Authorization'] = _basic_auth_str(u, getattr(auth, 'password', None))

    # ---- codes ------------------------------------------------------------
    codes = LookupDict(name='status_codes')
    _CODE_TITLES = {
        100: ('continue',), 101: ('switching_protocols',), 102: ('processing',),
        200: ('ok', 'okay', 'all_ok', 'all_okay'), 201: ('created',),
        202: ('accepted',), 203: ('non_authoritative_info',),
        204: ('no_content',), 205: ('reset_content', 'reset'),
        206: ('partial_content', 'partial'),
        300: ('multiple_choices',), 301: ('moved_permanently', 'moved'),
        302: ('found',), 303: ('see_other', 'other'),
        304: ('not_modified',), 307: ('temporary_redirect', 'temporary'),
        308: ('permanent_redirect', 'resume_incomplete', 'resume'),
        400: ('bad_request', 'bad'), 401: ('unauthorized',),
        402: ('payment_required', 'payment'), 403: ('forbidden',),
        404: ('not_found',), 405: ('method_not_allowed', 'not_allowed'),
        406: ('not_acceptable',), 407: ('proxy_authentication_required',),
        408: ('request_timeout', 'timeout'), 409: ('conflict',),
        410: ('gone',), 411: ('length_required',),
        412: ('precondition_failed', 'precondition'),
        413: ('request_entity_too_large',), 414: ('request_uri_too_large',),
        415: ('unsupported_media_type', 'unsupported_media'),
        416: ('requested_range_not_satisfiable',), 417: ('expectation_failed',),
        418: ('im_a_teapot', 'teapot', 'i_am_a_teapot'),
        422: ('unprocessable_entity', 'unprocessable'),
        423: ('locked',), 424: ('failed_dependency', 'dependency'),
        426: ('upgrade_required', 'upgrade'), 428: ('precondition_required',),
        429: ('too_many_requests', 'too_many'),
        431: ('header_fields_too_large',), 451: ('unavailable_for_legal_reasons',),
        500: ('internal_server_error', 'server_error'),
        501: ('not_implemented',), 502: ('bad_gateway',),
        503: ('service_unavailable', 'unavailable'), 504: ('gateway_timeout',),
        505: ('http_version_not_supported', 'http_version'),
    }
    for _code, _titles in _CODE_TITLES.items():
        for _t in _titles:
            setattr(codes, _t, _code)
            setattr(codes, _t.upper(), _code)

    # ---- helpers ----------------------------------------------------------
    def _charset(headers):
        ct = headers.get('Content-Type') if headers else None
        if not ct:
            return None
        for part in ct.split(';'):
            part = part.strip()
            if part.lower().startswith('charset='):
                val = part.split('=', 1)[1].strip()
                val = val.strip(_Q).strip(_SQ)
                return val or None
        return None

    def _apply_params(url, params):
        if not params:
            return url
        if isinstance(params, str):
            q = params.lstrip('?&')
        elif isinstance(params, (bytes, bytearray)):
            q = bytes(params).decode('utf-8').lstrip('?&')
        else:
            pairs = list(params.items()) if hasattr(params, 'items') else list(params)
            q = _up.urlencode(pairs, doseq=True)
        if not q:
            return url
        return url + ('&' if '?' in url else '?') + q

    def _encode_body(data, json_body):
        auto = {}
        body = None
        if json_body is not None:
            body = _json.dumps(json_body).encode('utf-8')
            auto['Content-Type'] = 'application/json'
        elif data is not None:
            if isinstance(data, (bytes, bytearray)):
                body = bytes(data)
            elif isinstance(data, str):
                body = data.encode('utf-8')
            else:
                pairs = list(data.items()) if hasattr(data, 'items') else list(data)
                body = _up.urlencode(pairs, doseq=True).encode('utf-8')
                auto['Content-Type'] = 'application/x-www-form-urlencoded'
        return body, auto

    def _encode_multipart(fields, files):
        boundary = 'visBoundary' + _b64.b16encode(_os.urandom(12)).decode('ascii')
        chunks = []

        def add(s):
            chunks.append(s.encode('utf-8') if isinstance(s, str) else s)

        if fields and hasattr(fields, 'items'):
            for k, v in fields.items():
                add('--' + boundary + _CRLF)
                add('Content-Disposition: form-data; name=' + _Q + str(k) + _Q + _CRLF + _CRLF)
                if isinstance(v, (bytes, bytearray)):
                    add(bytes(v))
                else:
                    add(str(v))
                add(_CRLF)
        if files:
            fitems = files.items() if hasattr(files, 'items') else files
            for k, fv in fitems:
                filename = str(k)
                content = fv
                ctype = 'application/octet-stream'
                if isinstance(fv, (tuple, list)):
                    if len(fv) > 0 and fv[0]:
                        filename = str(fv[0])
                    content = fv[1] if len(fv) > 1 else b''
                    if len(fv) > 2 and fv[2]:
                        ctype = str(fv[2])
                if hasattr(content, 'read'):
                    content = content.read()
                if isinstance(content, str):
                    content = content.encode('utf-8')
                add('--' + boundary + _CRLF)
                add('Content-Disposition: form-data; name=' + _Q + str(k) + _Q
                    + '; filename=' + _Q + filename + _Q + _CRLF)
                add('Content-Type: ' + ctype + _CRLF + _CRLF)
                add(bytes(content) if content else b'')
                add(_CRLF)
        add('--' + boundary + '--' + _CRLF)
        return b''.join(chunks), 'multipart/form-data; boundary=' + boundary

    def _cookie_header(cookies):
        if not cookies:
            return None
        items = cookies.items() if hasattr(cookies, 'items') else cookies
        parts = [str(k) + '=' + str(v) for k, v in items]
        return '; '.join(parts) if parts else None

    def _parse_set_cookie(headers):
        jar = RequestsCookieJar()
        if not headers:
            return jar
        raw = []
        try:
            raw = headers.get_all('Set-Cookie') or []
        except Exception:
            v = headers.get('Set-Cookie') if hasattr(headers, 'get') else None
            if v:
                raw = [v]
        for line in raw:
            first = str(line).split(';', 1)[0].strip()
            if '=' in first:
                k, v = first.split('=', 1)
                jar[k.strip()] = v.strip()
        return jar

    # ---- Response ---------------------------------------------------------
    class Response:
        def __init__(self):
            self.status_code = None
            self.headers = CaseInsensitiveDict()
            self.url = None
            self.encoding = 'utf-8'
            self.content = b''
            self.reason = ''
            self.request = None
            self.cookies = RequestsCookieJar()
            self.history = []
            self.elapsed = _dt.timedelta(0)
            self.raw = None
            self.next = None

        @property
        def ok(self):
            return self.status_code is not None and self.status_code < 400

        @property
        def is_redirect(self):
            loc = 'location' in {str(k).lower() for k in self.headers}
            return loc and self.status_code in (301, 302, 303, 307, 308)

        @property
        def is_permanent_redirect(self):
            return self.status_code in (301, 308)

        @property
        def apparent_encoding(self):
            return self.encoding or 'utf-8'

        @property
        def links(self):
            header = self.headers.get('link')
            resolved = {}
            if not header:
                return resolved
            for val in header.split(','):
                parts = val.split(';')
                link = {'url': parts[0].strip().strip('<>')}
                for p in parts[1:]:
                    if '=' in p:
                        key, v = p.split('=', 1)
                        link[key.strip()] = v.strip().strip(_Q)
                resolved[link.get('rel') or link.get('url')] = link
            return resolved

        @property
        def text(self):
            enc = self.encoding or 'utf-8'
            try:
                return self.content.decode(enc)
            except Exception:
                return self.content.decode('utf-8', 'replace')

        def json(self, **kwargs):
            try:
                return _json.loads(self.text or 'null')
            except ValueError as e:
                raise JSONDecodeError(str(e), response=self)

        def raise_for_status(self):
            if self.status_code is not None and self.status_code >= 400:
                kind = 'Client' if self.status_code < 500 else 'Server'
                raise HTTPError(
                    str(self.status_code) + ' ' + kind + ' Error: '
                    + (self.reason or '') + ' for url: ' + str(self.url),
                    response=self)
            return None

        def iter_content(self, chunk_size=1, decode_unicode=False):
            data = self.text if decode_unicode else self.content
            step = max(1, int(chunk_size or 1))
            for i in range(0, len(data), step):
                yield data[i:i + step]

        def iter_lines(self, **kwargs):
            for line in self.text.splitlines():
                yield line

        def __iter__(self):
            return self.iter_content(128)

        def close(self):
            return None

        def __bool__(self):
            return self.ok

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

        def __repr__(self):
            return '<Response [' + str(self.status_code) + ']>'

    class _NoRedirect(_ur.HTTPRedirectHandler):
        def redirect_request(self, *a, **k):
            return None

    _no_redirect_opener = _ur.build_opener(_NoRedirect())

    # ---- request ----------------------------------------------------------
    def request(method, url, params=None, data=None, json=None, headers=None,
                cookies=None, auth=None, timeout=None, allow_redirects=True,
                files=None, **kwargs):
        method = str(method).upper()
        if not isinstance(url, str):
            raise URLRequired('Invalid URL ' + repr(url) + ': must be a string')
        if '://' not in url:
            raise MissingSchema('Invalid URL ' + repr(url) + ': No scheme supplied.')
        low = url.lower()
        if not (low.startswith('http://') or low.startswith('https://')):
            raise InvalidSchema('No connection adapters were found for ' + repr(url))

        full = _apply_params(url, params)
        if files:
            body, ctype = _encode_multipart(data, files)
            auto_h = {'Content-Type': ctype}
        else:
            body, auto_h = _encode_body(data, json)

        h = CaseInsensitiveDict()
        for k, v in auto_h.items():
            h[k] = v
        if headers:
            items = headers.items() if hasattr(headers, 'items') else headers
            for k, v in items:
                if v is None:
                    continue
                h[k] = v
        ck = _cookie_header(cookies)
        if ck:
            existing = h.get('Cookie')
            h['Cookie'] = (existing + '; ' + ck) if existing else ck

        prep = _PreparedShim(method, full, h, body)
        _apply_auth(auth, prep)
        h = prep.headers
        if 'User-Agent' not in h:
            h['User-Agent'] = _UA

        if isinstance(timeout, (tuple, list)):
            timeout = timeout[-1] if timeout else None
        t = _DEFAULT_TIMEOUT if timeout is None else timeout

        req = _ur.Request(full, data=body, method=method)
        for k, v in h.items():
            req.add_header(str(k), str(v))

        resp = Response()
        resp.url = full
        resp.request = req
        opener = _ur.urlopen if allow_redirects else _no_redirect_opener.open
        start = _dt.datetime.now()
        try:
            raw = opener(req, timeout=t)
        except _ue.HTTPError as e:
            # A 4xx/5xx (or a blocked redirect) is a REAL Response in requests,
            # not an exception -- surface it and let raise_for_status() decide.
            resp.status_code = e.code
            resp.reason = str(getattr(e, 'reason', '') or '')
            src_headers = e.headers
            try:
                resp.headers = CaseInsensitiveDict(e.headers.items() if e.headers else [])
            except Exception:
                pass
            try:
                resp.content = e.read()
            except Exception:
                resp.content = b''
            resp.encoding = _charset(resp.headers) or 'utf-8'
            resp.cookies = _parse_set_cookie(src_headers)
            resp.elapsed = _dt.datetime.now() - start
            return resp
        except _ue.URLError as e:
            reason = getattr(e, 'reason', e)
            rs = str(reason).lower()
            if 'timed out' in rs or isinstance(reason, TimeoutError):
                raise ReadTimeout(str(reason), request=req)
            if 'ssl' in rs or 'certificate' in rs:
                raise SSLError(str(reason), request=req)
            raise ConnectionError(str(reason), request=req)
        except TimeoutError as e:
            raise ReadTimeout(str(e), request=req)
        except OSError as e:
            raise ConnectionError(str(e), request=req)

        try:
            resp.status_code = raw.status
            resp.reason = str(getattr(raw, 'reason', '') or '')
            src_headers = raw.headers
            try:
                resp.headers = CaseInsensitiveDict(raw.headers.items())
            except Exception:
                pass
            resp.url = raw.geturl() or full
            resp.content = raw.read()
            resp.encoding = _charset(resp.headers) or 'utf-8'
            resp.cookies = _parse_set_cookie(src_headers)
        finally:
            try:
                raw.close()
            except Exception:
                pass
        resp.elapsed = _dt.datetime.now() - start
        return resp

    def get(url, params=None, **kwargs):
        return request('GET', url, params=params, **kwargs)

    def options(url, **kwargs):
        return request('OPTIONS', url, **kwargs)

    def head(url, **kwargs):
        kwargs.setdefault('allow_redirects', False)
        return request('HEAD', url, **kwargs)

    def post(url, data=None, json=None, **kwargs):
        return request('POST', url, data=data, json=json, **kwargs)

    def put(url, data=None, **kwargs):
        return request('PUT', url, data=data, **kwargs)

    def patch(url, data=None, **kwargs):
        return request('PATCH', url, data=data, **kwargs)

    def delete(url, **kwargs):
        return request('DELETE', url, **kwargs)

    # ---- Session ----------------------------------------------------------
    class Session:
        # Thin session: merges default headers/params/cookies/auth into every
        # call and persists response cookies. urllib opens a fresh connection per
        # request (no pooling), which is fine for the sandbox.
        def __init__(self):
            self.headers = CaseInsensitiveDict({'User-Agent': _UA})
            self.params = {}
            self.auth = None
            self.cookies = RequestsCookieJar()
            self.verify = True
            self.cert = None
            self.max_redirects = 30
            self.hooks = {'response': []}
            self.adapters = {}

        def request(self, method, url, headers=None, params=None, cookies=None,
                    auth=None, **kwargs):
            merged_h = CaseInsensitiveDict(self.headers)
            if headers:
                items = headers.items() if hasattr(headers, 'items') else headers
                for k, v in items:
                    merged_h[k] = v
            if isinstance(params, str):
                merged_p = params
            else:
                merged_p = dict(self.params)
                if params:
                    pairs = params.items() if hasattr(params, 'items') else params
                    for k, v in pairs:
                        merged_p[k] = v
            merged_c = RequestsCookieJar()
            merged_c.update(self.cookies)
            if cookies:
                merged_c.update(cookies)
            use_auth = auth if auth is not None else self.auth
            resp = request(method, url, headers=merged_h, params=merged_p,
                           cookies=merged_c, auth=use_auth, **kwargs)
            try:
                self.cookies.update(resp.cookies)
            except Exception:
                pass
            return resp

        def get(self, url, **kw):
            return self.request('GET', url, **kw)

        def options(self, url, **kw):
            return self.request('OPTIONS', url, **kw)

        def head(self, url, **kw):
            kw.setdefault('allow_redirects', False)
            return self.request('HEAD', url, **kw)

        def post(self, url, **kw):
            return self.request('POST', url, **kw)

        def put(self, url, **kw):
            return self.request('PUT', url, **kw)

        def patch(self, url, **kw):
            return self.request('PATCH', url, **kw)

        def delete(self, url, **kw):
            return self.request('DELETE', url, **kw)

        def mount(self, prefix, adapter):
            self.adapters[prefix] = adapter

        def get_adapter(self, url):
            return self.adapters.get(url)

        def close(self):
            return None

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

    # ---- Request / PreparedRequest models --------------------------------
    class PreparedRequest:
        def __init__(self):
            self.method = None
            self.url = None
            self.headers = CaseInsensitiveDict()
            self.body = None

        def prepare(self, method=None, url=None, headers=None, data=None,
                    params=None, json=None, **kw):
            self.method = str(method).upper() if method else None
            self.url = _apply_params(url, params) if url else url
            self.headers = CaseInsensitiveDict()
            if headers:
                items = headers.items() if hasattr(headers, 'items') else headers
                for k, v in items:
                    self.headers[k] = v
            self.body, auto = _encode_body(data, json)
            for k, v in auto.items():
                if k not in self.headers:
                    self.headers[k] = v
            return self

        def __repr__(self):
            return '<PreparedRequest [' + str(self.method) + ']>'

    class Request:
        def __init__(self, method=None, url=None, headers=None, files=None,
                     data=None, params=None, auth=None, cookies=None, json=None, **kw):
            self.method = method
            self.url = url
            self.headers = headers or {}
            self.files = files
            self.data = data
            self.params = params or {}
            self.auth = auth
            self.cookies = cookies
            self.json = json

        def prepare(self):
            p = PreparedRequest()
            return p.prepare(method=self.method, url=self.url, headers=self.headers,
                             data=self.data, params=self.params, json=self.json)

    def session():
        return Session()

    # ---- submodules -------------------------------------------------------
    def _mk_module(name, attrs):
        m = types.ModuleType(name)
        for k, v in attrs.items():
            setattr(m, k, v)
        sys.modules[name] = m
        return m

    exc = _mk_module('requests.exceptions', dict(_EXC))

    structures = _mk_module('requests.structures', {
        'CaseInsensitiveDict': CaseInsensitiveDict,
        'LookupDict': LookupDict,
    })

    auth_mod = _mk_module('requests.auth', {
        'AuthBase': AuthBase, 'HTTPBasicAuth': HTTPBasicAuth,
        'HTTPProxyAuth': HTTPProxyAuth, 'HTTPDigestAuth': HTTPDigestAuth,
        '_basic_auth_str': _basic_auth_str,
    })

    models = _mk_module('requests.models', {
        'Request': Request, 'PreparedRequest': PreparedRequest, 'Response': Response,
    })

    def _dict_from_cookiejar(cj):
        return dict(cj) if cj else {}

    def _cookiejar_from_dict(d, cookiejar=None):
        jar = cookiejar if cookiejar is not None else RequestsCookieJar()
        if d:
            for k, v in d.items():
                jar[k] = v
        return jar

    cookies_mod = _mk_module('requests.cookies', {
        'RequestsCookieJar': RequestsCookieJar,
        'cookiejar_from_dict': _cookiejar_from_dict,
        'dict_from_cookiejar': _dict_from_cookiejar,
    })

    def _get_encoding_from_headers(headers):
        cs = _charset(headers)
        if cs:
            return cs
        ct = (headers.get('Content-Type') if headers else '') or ''
        if 'text' in ct:
            return 'ISO-8859-1'
        return None

    def _default_headers():
        return CaseInsensitiveDict({
            'User-Agent': _UA, 'Accept-Encoding': 'identity',
            'Accept': '*/*', 'Connection': 'keep-alive',
        })

    utils = _mk_module('requests.utils', {
        'quote': _up.quote, 'unquote': _up.unquote,
        'quote_plus': _up.quote_plus, 'unquote_plus': _up.unquote_plus,
        'urlparse': _up.urlparse, 'urlencode': _up.urlencode,
        'dict_from_cookiejar': _dict_from_cookiejar,
        'get_encoding_from_headers': _get_encoding_from_headers,
        'default_headers': _default_headers,
    })

    status_codes = _mk_module('requests.status_codes', {'codes': codes})

    api = _mk_module('requests.api', {
        'request': request, 'get': get, 'options': options, 'head': head,
        'post': post, 'put': put, 'patch': patch, 'delete': delete,
    })

    sessions = _mk_module('requests.sessions', {
        'Session': Session, 'session': session,
    })

    # ---- top-level requests module ---------------------------------------
    mod = types.ModuleType('requests')
    mod.__doc__ = 'vis requests-compatible shim backed by urllib (pure Python, no host bridge).'
    mod.__version__ = '2.0-vis-urllib'
    mod.__build__ = 0x022000
    mod.__title__ = 'requests'
    mod.__author__ = 'vis'
    mod.__license__ = 'Apache-2.0'

    mod.Response = Response
    mod.Request = Request
    mod.PreparedRequest = PreparedRequest
    mod.Session = Session
    mod.session = session
    mod.CaseInsensitiveDict = CaseInsensitiveDict
    mod.RequestsCookieJar = RequestsCookieJar
    mod.codes = codes

    mod.request = request
    mod.get = get
    mod.options = options
    mod.head = head
    mod.post = post
    mod.put = put
    mod.patch = patch
    mod.delete = delete

    for _n, _c in _EXC.items():
        setattr(mod, _n, _c)

    mod.exceptions = exc
    mod.structures = structures
    mod.auth = auth_mod
    mod.models = models
    mod.cookies = cookies_mod
    mod.utils = utils
    mod.status_codes = status_codes
    mod.api = api
    mod.sessions = sessions

    sys.modules['requests'] = mod

    # Autoload: staple onto builtins so requests.get(...) works in every
    # run_python block WITHOUT an explicit `import requests` (mirrors json/os).
    try:
        import builtins as _b
        _b.requests = mod
    except Exception:
        pass

__vis_install_requests_compat__()
del __vis_install_requests_compat__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-requests"
     :ext/description
     "Sandbox shim: a `requests`-compatible module (import requests / requests.get) backed PURELY by the stdlib urllib. No pip, no native wheel, no host bridge — rides the sandbox socket and honours the network guard."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "requests"
       :shim/description
       "requests-compatible `requests` module backed by stdlib urllib (pure Python, no host bridge)."
       :shim/preamble requests-compat-shim-src}]}))

(vis/register-extension! vis-extension)
