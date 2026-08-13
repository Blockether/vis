(ns com.blockether.vis.internal.urllib3-compat-shim-test
  "The urllib3-compat shim: a urllib3 module (PoolManager/HTTPResponse) published
   into sys.modules, wrapping the requests shim. Tested offline by monkeypatching
   requests.request with a canned echo Response (no network)."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

;; A namespace-local context avoids paying GraalPy + shim bootstrap per assertion.
(defonce ^:private python-context* (delay (ep/create-python-context {})))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context @python-context*)]
     ~@body))

;; Deterministic offline harness: monkeypatch the requests shim (which httpx and
;; urllib3 delegate to) with a canned echo Response, so the wrapper logic is
;; exercised with zero network. `fake` must be prepended to each snippet.
(def ^:private fake
  "import requests as _rq, json as _json
_last = {}
def _fake(method, url, params=None, data=None, json=None, headers=None,
          cookies=None, auth=None, timeout=None, allow_redirects=True, **kw):
    _last.clear()
    _last.update(kw)
    resp = _rq.Response()
    m = str(method).upper()
    resp.status_code = 404 if 'missing' in url else (201 if m == 'POST' else 200)
    resp.url = url
    resp.reason = 'OK'
    resp.encoding = 'utf-8'
    resp.headers['Content-Type'] = 'application/json'
    payload = {'method': m, 'url': url, 'params': params, 'data': data,
               'json': json, 'headers': dict(headers) if headers else None,
               'follow': allow_redirects, 'timeout': timeout}
    resp.content = _json.dumps(payload).encode('utf-8')
    return resp
_rq.request = _fake
")

(defdescribe
  urllib3-module-test
  (it "publishes urllib3 under sys.modules and works with no import"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  fake
                  "import sys, urllib3\n"
                  "sys.modules['urllib3'] is urllib3 and urllib3.__version__.endswith('-vis')"))))))
  (it "exposes the urllib3.exceptions tree"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str
                       fake
                       "issubclass(urllib3.exceptions.MaxRetryError, urllib3.exceptions.HTTPError) "
                       "and urllib3.exceptions is sys.modules['urllib3.exceptions']")))))))

(defdescribe
  urllib3-request-test
  (it "routes GET fields to query params and merges pool headers"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  fake
                  "pm = urllib3.PoolManager(headers={'User-Agent': 'vis'})\n"
                  "r = pm.request('GET', 'http://svc/d', fields={'a': 'b'})\n"
                  "r.status == 200 and r.status_code == 200 and r.json()['params'] == {'a': 'b'} "
                  "and r.json()['headers'].get('User-Agent') == 'vis'"))))))
  (it "routes a POST body to the request data and reports 201"
      (with-python-context
        (expect (true? (ev python-context
                           (str fake
                                "pm = urllib3.PoolManager()\n"
                                "r = pm.request('POST', 'http://svc/e', body='raw-body')\n"
                                "r.status == 201 and r.json()['data'] == 'raw-body'"))))))
  (it "reads the body once then returns empty (consume semantics)"
      (with-python-context
        (expect (true? (ev python-context
                           (str fake
                                "r = urllib3.PoolManager().request('GET', 'http://svc/d')\n"
                                "first = r.read()\nsecond = r.read()\n"
                                "len(first) > 0 and second == b''"))))))
  (it "supports the top-level urllib3.request and getheader"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str fake
                     "r = urllib3.request('GET', 'http://svc/f')\n"
                     "r.status == 200 and r.getheader('content-type') == 'application/json'")))))))

;; A second offline fake: the echo above JSON-encodes `data`, which explodes on the
;; multipart bytes urllib3 now sends, so this one renders the body as text first.
(def ^:private echo
  "import requests as _rq, json as _json
def _echo(method, url, params=None, data=None, json=None, headers=None,
          cookies=None, auth=None, timeout=None, allow_redirects=True, **kw):
    resp = _rq.Response()
    resp.status_code = 200
    resp.url = url
    resp.reason = 'OK'
    resp.encoding = 'utf-8'
    resp.headers['Content-Type'] = 'application/json'
    body = data
    if isinstance(body, (bytes, bytearray)):
        body = bytes(body).decode('utf-8', 'replace')
    resp.content = _json.dumps({'method': str(method).upper(), 'url': url,
                                'params': params, 'data': body,
                                'headers': dict(headers) if headers else {}}).encode('utf-8')
    return resp
_rq.request = _echo
")

(defn- true-py?
  "Evaluates `snippet` (prefixed with the offline `echo` fake) and expects Python True."
  [^Context c snippet]
  (true? (ev c (str echo snippet))))

;; Fidelity against real urllib3 2.x: `fields=` on a body method is multipart/form-data,
;; a pool knows its own scheme, and the timeout errors are distinguishable classes.
(defdescribe
  urllib3-transport-fidelity-test
  (it
    "encodes POST fields as multipart/form-data (they used to be urlencoded)"
    (with-python-context
      (expect
        (true-py?
          python-context
          (str
            "r = urllib3.PoolManager().request('POST', 'http://svc/u', fields={'a': '1'}, multipart_boundary='BB')\n"
            "d = r.json()\n"
            "d['data'] == '--BB\\r\\nContent-Disposition: form-data; name=\"a\"\\r\\n\\r\\n1\\r\\n--BB--\\r\\n' "
            "and d['headers']['Content-Type'] == 'multipart/form-data; boundary=BB'")))))
  (it
    "carries filename and content type for file fields"
    (with-python-context
      (expect
        (true-py?
          python-context
          (str
            "r = urllib3.PoolManager().request('POST', 'http://svc/u', "
            "fields={'f': ('n.txt', b'hi', 'text/plain')}, multipart_boundary='B2')\n"
            "d = r.json()['data']\n"
            "'filename=\"n.txt\"' in d and 'Content-Type: text/plain' in d and d.endswith('--B2--\\r\\n')")))))
  (it "falls back to urlencoding when encode_multipart is false"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str
                    "r = urllib3.PoolManager().request('POST', 'http://svc/u', "
                    "fields=[('a', '1'), ('b', '2')], encode_multipart=False)\n" "d = r.json()\n"
                    "d['data'] == 'a=1&b=2' "
                    "and d['headers']['Content-Type'] == 'application/x-www-form-urlencoded'")))))
  (it "keeps a caller supplied Content-Type instead of overwriting it"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str
                    "r = urllib3.PoolManager().request('POST', 'http://svc/u', fields={'a': '1'}, "
                    "headers={'content-type': 'application/x-custom'})\n"
                    "r.json()['headers'] == {'content-type': 'application/x-custom'}")))))
  (it
    "exposes encode_multipart_formdata on urllib3 and urllib3.filepost"
    (with-python-context
      (expect
        (true-py?
          python-context
          (str
            "import urllib3.filepost as fp\n"
            "b, ct = fp.encode_multipart_formdata({'a': '1'}, 'BX')\n"
            "b == b'--BX\\r\\nContent-Disposition: form-data; name=\"a\"\\r\\n\\r\\n1\\r\\n--BX--\\r\\n' "
            "and ct == 'multipart/form-data; boundary=BX' "
            "and fp.encode_multipart_formdata is urllib3.encode_multipart_formdata")))))
  (it
    "builds https URLs from an HTTPSConnectionPool (the port used to pick the scheme)"
    (with-python-context
      (expect
        (true-py?
          python-context
          (str
            "a = urllib3.HTTPSConnectionPool('h', 8443).request('GET', '/p').json()['url']\n"
            "b = urllib3.HTTPSConnectionPool('h').request('GET', '/p').json()['url']\n"
            "c = urllib3.HTTPConnectionPool('h', 80).request('GET', '/p').json()['url']\n"
            "d = urllib3.HTTPConnectionPool('h', 8080).request('GET', '/p').json()['url']\n"
            "[a, b, c, d] == ['https://h:8443/p', 'https://h/p', 'http://h/p', 'http://h:8080/p']")))))
  (it "models the real exception tree"
      (with-python-context
        (expect (true-py? python-context
                          (str "E = urllib3.exceptions\n" "issubclass(E.TimeoutError, E.HTTPError) "
                               "and issubclass(E.ConnectTimeoutError, E.TimeoutError) "
                               "and issubclass(E.ReadTimeoutError, E.TimeoutError) "
                               "and not issubclass(E.ReadTimeoutError, E.ConnectTimeoutError) "
                               "and issubclass(E.NewConnectionError, E.ConnectTimeoutError) "
                               "and issubclass(E.RequestError, E.PoolError) "
                               "and issubclass(E.LocationParseError, E.LocationValueError)")))))
  (it "raises ReadTimeoutError, not a generic timeout, on a read timeout"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str "import requests as _rq2\n" "def _boom(*a, **k):\n"
                       "    raise _rq2.exceptions.ReadTimeout('slow')\n" "_rq2.request = _boom\n"
                       "out = 'none'\n" "try:\n"
                       "    urllib3.request('GET', 'http://svc/x')\n"
                       "except urllib3.exceptions.ConnectTimeoutError:\n"
                       "    out = 'connect'\n" "except urllib3.exceptions.ReadTimeoutError:\n"
                       "    out = 'read'\n" "out == 'read'")))))
  (it "raises ConnectTimeoutError on a connect timeout"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str "import requests as _rq2\n" "def _boom(*a, **k):\n"
                       "    raise _rq2.exceptions.ConnectTimeout('dead')\n" "_rq2.request = _boom\n"
                       "out = 'none'\n" "try:\n"
                       "    urllib3.request('GET', 'http://svc/x')\n"
                       "except urllib3.exceptions.ReadTimeoutError:\n"
                       "    out = 'read'\n" "except urllib3.exceptions.ConnectTimeoutError:\n"
                       "    out = 'connect'\n" "out == 'connect'"))))))

;; HTTPHeaderDict used to be read-only: no __setitem__, __len__, add or getlist.
(defdescribe
  urllib3-header-dict-test
  (it "joins repeated headers with ', ' and keeps them in getlist"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str
                    "h = urllib3.HTTPHeaderDict({'Set-Cookie': 'a=1'})\n"
                    "h.add('set-cookie', 'b=2')\n"
                    "h['Set-Cookie'] == 'a=1, b=2' and h.getlist('SET-COOKIE') == ['a=1', 'b=2'] "
                    "and len(h) == 1")))))
  (it "assigns and replaces case-insensitively"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str "h = urllib3.HTTPHeaderDict()\n"
                       "h['Content-Type'] = 'text/plain'\n" "h['CONTENT-TYPE'] = 'text/html'\n"
                       "h['x-a'] = '1'\n" "del h['X-A']\n"
                       "h['content-type'] == 'text/html' and len(h) == 1 and 'Content-Type' in h "
                       "and 'x-a' not in h and list(h.values()) == ['text/html']")))))
  (it "copies, updates, pops and compares case-insensitively"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str
                    "h = urllib3.HTTPHeaderDict({'A': '1'})\n" "c = h.copy()\n"
                    "c.update({'b': '2'})\n" "same = urllib3.HTTPHeaderDict({'a': '1'})\n"
                    "h == same and len(h) == 1 and len(c) == 2 and c.setdefault('B', '9') == '2' "
                    "and c.pop('a') == '1' and c.pop('zz', 'dflt') == 'dflt' and len(c) == 1"))))))

;; HTTPResponse ignored read(amt) and had no stream/iteration/geturl/info surface.
(defdescribe
  urllib3-response-surface-test
  (it "honours read(amt) and resumes from the offset"
      (with-python-context
        (expect
          (true-py?
            python-context
            (str "r = urllib3.PoolManager().request('GET', 'http://svc/d')\n" "head = r.read(4)\n"
                 "rest = r.read()\n"
                 "len(head) == 4 and head + rest == r.data and r.read() == b'' and r.closed")))))
  (it "streams in chunks and iterates by line"
      (with-python-context
        (expect
          (true-py?
            python-context
            (str "import requests as _rq3\n"
                 "rr = _rq3.Response()\n" "rr.status_code = 200\n"
                 "rr.url = 'http://x/y'\n" "rr.content = b'a\\nb\\nc'\n"
                 "rr.headers['X-A'] = '1'\n" "chunks = list(urllib3.HTTPResponse(rr).stream(2))\n"
                 "lines = list(urllib3.HTTPResponse(rr))\n"
                 "chunks == [b'a\\n', b'b\\n', b'c'] and lines == [b'a\\n', b'b\\n', b'c']")))))
  (it "exposes geturl, url, info, version, readinto and drain_conn"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str "import requests as _rq3\n" "rr = _rq3.Response()\n"
                       "rr.status_code = 204\n" "rr.url = 'http://x/y'\n"
                       "rr.content = b'abcd'\n" "resp = urllib3.HTTPResponse(rr)\n"
                       "buf = bytearray(2)\n" "n = resp.readinto(buf)\n"
                       "resp.geturl() == 'http://x/y' and resp.url == 'http://x/y' "
                       "and resp.info() is resp.headers and resp.version == 11 and resp.readable() "
                       "and n == 2 and bytes(buf) == b'ab' and resp.drain_conn() is None "
                       "and resp.status == 204 and resp.status_code == 204"))))))

;; Real code imports urllib3 by SUBMODULE (`from urllib3.util.retry import Retry`,
;; `urllib3.response.HTTPResponse`, ...). A shim module has no loader of its own,
;; so a submodule without its own sys.modules entry fails with
;; "'urllib3' is not a package" even though plain `import urllib3` works.
(defdescribe
  urllib3-package-surface-test
  (it "resolves every submodule real code imports from"
      (with-python-context
        (expect
          (true-py? python-context
                    (str
                      "import sys\n" "from urllib3.util.retry import Retry\n"
                      "from urllib3.util.timeout import Timeout\n"
                      "from urllib3.util.url import parse_url, Url\n"
                      "from urllib3.util.request import make_headers, SKIP_HEADER\n"
                      "from urllib3.response import HTTPResponse, BaseHTTPResponse\n"
                      "from urllib3.poolmanager import PoolManager, ProxyManager, proxy_from_url\n"
                      "from urllib3.connectionpool import HTTPConnectionPool, HTTPSConnectionPool\n"
                      "from urllib3.fields import RequestField, guess_content_type\n"
                      "from urllib3._collections import HTTPHeaderDict\n"
                      "from urllib3.filepost import encode_multipart_formdata\n"
                      "Retry is urllib3.util.Retry and Timeout is urllib3.Timeout "
                      "and HTTPResponse is urllib3.HTTPResponse "
                      "and issubclass(HTTPResponse, BaseHTTPResponse) "
                      "and sys.modules['urllib3.util.retry'] is urllib3.util.retry "
                      "and 'parse_url' in urllib3.__all__")))))
  (it "carries the exception names real code catches"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str "E = urllib3.exceptions\n" "issubclass(E.IncompleteRead, E.ProtocolError) "
                       "and issubclass(E.URLSchemeUnknown, E.LocationValueError) "
                       "and issubclass(E.ProxySchemeUnknown, E.URLSchemeUnknown) "
                       "and issubclass(E.NameResolutionError, E.NewConnectionError) "
                       "and issubclass(E.InsecureRequestWarning, E.SecurityWarning) "
                       "and issubclass(E.EmptyPoolError, E.PoolError) "
                       "and E.ConnectionError is E.ProtocolError "
                       ;; message reads once, not twice through URLSchemeUnknown's prefix
                       "and str(E.ProxySchemeUnknown('ftp')) == "
                       "'Proxy URL had unsupported scheme ftp, should use http:// or https://'")))))
  (it "parses a URL into the seven-part Url record"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str "u = urllib3.util.parse_url('https://user:pw@Example.COM:8443/a/b?q=1#f')\n"
                       "bare = urllib3.parse_url('google.com/mail')\n"
                       "tuple(u) == ('https', 'user:pw', 'example.com', 8443, '/a/b', 'q=1', 'f') "
                       "and u.request_uri == '/a/b?q=1' and u.netloc == 'example.com:8443' "
                       "and u.url == 'https://user:pw@example.com:8443/a/b?q=1#f' "
                       "and (bare.host, bare.scheme, bare.path) == ('google.com', None, '/mail') "
                       "and tuple(urllib3.parse_url('')) == (None,) * 7")))))
  (it "hands a Timeout to the transport as a connect/read pair"
      (with-python-context
        (expect (true? (ev python-context
                           (str fake
                                "r = urllib3.request('GET', 'http://svc/a', "
                                "timeout=urllib3.Timeout(connect=1, read=5))\n"
                                "t = urllib3.Timeout(total=7, read=3)\n"
                                "r.json()['timeout'] == [1, 5] "
                                "and (t.connect_timeout, t.read_timeout) == (7, 3) "
                                "and urllib3.Timeout.from_float(2).as_requests() == (2, 2)"))))))
  (it "builds auth and encoding headers with make_headers"
      (with-python-context
        (expect (true-py? python-context
                          (str "h = urllib3.make_headers(basic_auth='a:b', accept_encoding=True, "
                               "user_agent='vis', keep_alive=True, disable_cache=True)\n"
                               "h == {'accept-encoding': 'gzip,deflate', 'user-agent': 'vis', "
                               "'connection': 'keep-alive', 'authorization': 'Basic YTpi', "
                               "'cache-control': 'no-cache'} "
                               "and urllib3.util.SKIP_HEADER == '@@@SKIP_HEADER@@@'")))))
  (it
    "encodes a RequestField the same way as a plain field tuple"
    (with-python-context
      (expect
        (true-py?
          python-context
          (str
            "f = urllib3.fields.RequestField('f', b'hi', filename='n.txt')\n"
            "f.make_multipart(content_type='text/plain')\n"
            "a, _ct = urllib3.encode_multipart_formdata([f], 'B9')\n"
            "b, _ct2 = urllib3.encode_multipart_formdata({'f': ('n.txt', b'hi', 'text/plain')}, 'B9')\n"
            "a == b and f.name == 'f' and f.filename == 'n.txt' "
            "and urllib3.fields.guess_content_type('x.txt') == 'text/plain'")))))
  (it "builds typed pools from a URL"
      (with-python-context
        (expect (true-py?
                  python-context
                  (str
                    "s = urllib3.connection_from_url('https://h:8443/x')\n"
                    "p = urllib3.PoolManager(headers={'User-Agent': 'vis'})\n"
                    "c = p.connection_from_url('http://h/x')\n"
                    "type(s).__name__ == 'HTTPSConnectionPool' and (s.host, s.port) == ('h', 8443) "
                    "and type(c).__name__ == 'HTTPConnectionPool' "
                    "and c.request('GET', '/x').json()['headers'].get('User-Agent') == 'vis'")))))
  (it "merges proxy headers and refuses a non-HTTP proxy scheme"
      (with-python-context
        (expect
          (true-py?
            python-context
            (str "pm = urllib3.proxy_from_url('http://10.0.0.5:3128', proxy_headers={'X-P': '1'})\n"
                 "h = pm.request('GET', 'http://svc/a', headers={'X-Q': '2'}).json()['headers']\n"
                 "bad = 'none'\n"
                 "try:\n" "    urllib3.ProxyManager('ftp://10.0.0.5')\n"
                 "except urllib3.exceptions.ProxySchemeUnknown:\n" "    bad = 'refused'\n"
                 "h.get('X-P') == '1' and h.get('X-Q') == '2' "
                 "and pm.proxy.port == 3128 and bad == 'refused'"))))))

;; Regression, CI ubuntu-latest: the shim imported the stdlib `uuid` at install
;; time for one multipart boundary. `uuid` is platform-divergent AT IMPORT --
;; the darwin branch is a no-op while Linux pulls in `platform` -- so on the
;; Ubuntu runner every urllib3 test died with
;; `ModuleNotFoundError: No module named 'urllib3'`: the lazy loader swallowed
;; the shim's own exception and let the import machinery blame a missing module.
(defdescribe urllib3-load-independence-test
             (it "loads and encodes multipart with the stdlib uuid module unavailable"
                 (let [c (:python-context (ep/create-python-context {}))]
                   (expect (true? (ev c
                                      (str "import sys\n"
                                           "sys.modules['uuid'] = None\n" "import urllib3\n"
                                           "b, ct = urllib3.encode_multipart_formdata({'a': '1'})\n"
                                           "ct.startswith('multipart/form-data; boundary=') "
                                           "and b.startswith(('--' + ct.split('=')[1]).encode()) "
                                           "and b.endswith(b'--\\r\\n')"))))))
             (it "names the shim and the real cause when a shim cannot load"
                 (let [c (:python-context (ep/create-python-context {}))]
                   (expect (true? (ev c
                                      (str "import sys\n" "sys.modules['json'] = None\n"
                                           "out = 'imported'\n" "try:\n"
                                           "    import urllib3\n" "except ImportError as e:\n"
                                           "    out = str(e)\n"
                                           "'json' in out and 'urllib3' in out")))))))

;; Regression, issue #141: PoolManager swallowed every TLS option in `**_ignored`,
;; so `cert_reqs='CERT_NONE'` (and ca_certs / cert_file) never reached the socket
;; and an unverified request was impossible through urllib3.
(defdescribe
  urllib3-tls-options-test
  (it "maps pool-level cert_reqs=CERT_NONE onto the transport's verify=False"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str fake
                     "urllib3.PoolManager(cert_reqs='CERT_NONE').request('GET', 'https://svc/d')\n"
                     "_last['verify'] is False"))))))
  (it "maps ca_certs and cert_file/key_file given on the call"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  fake
                  "pm = urllib3.PoolManager()\n"
                  "pm.request('GET', 'https://svc/d', ca_certs='/etc/ca.pem',\n"
                  "           cert_file='/c.pem', key_file='/k.pem')\n"
                  "_last['verify'] == '/etc/ca.pem' and _last['cert'] == ('/c.pem', '/k.pem')"))))))
  (it "an HTTPSConnectionPool keeps the TLS options it was built with"
      (with-python-context
        (expect (true? (ev python-context
                           (str fake
                                "p = urllib3.HTTPSConnectionPool('svc', cert_reqs='CERT_NONE')\n"
                                "p.request('GET', '/d')\n"
                                "_last['verify'] is False"))))))
  (it "assert_hostname=False skips the NAME check and still verifies the chain"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str fake
                     "import ssl\n"
                     "urllib3.PoolManager(assert_hostname=False).request('GET', 'https://svc/d')\n"
                     "c = _last['verify']\n"
                     "c.check_hostname is False and c.verify_mode == ssl.CERT_REQUIRED"))))))
  (it "leaves an ordinary request alone, so the default CA store decides"
      (with-python-context
        (expect (true? (ev python-context
                           (str fake
                                "urllib3.PoolManager().request('GET', 'https://svc/d')\n"
                                "_last['verify'] is None and _last['cert'] is None"))))))
  (it "disable_warnings() really silences the InsecureRequestWarning category"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str fake
                          "import warnings\n" "with warnings.catch_warnings(record=True) as w:\n"
                          "    warnings.simplefilter('always')\n" "    urllib3.disable_warnings()\n"
                          "    warnings.warn('x', urllib3.exceptions.InsecureRequestWarning)\n"
                          "len(w) == 0")))))))

;; Regression, issue #141 (cross-validation follow-up): a certificate failure
;; arrived as a bare ProtocolError, so `except urllib3.exceptions.SSLError` --
;; the class real code catches -- never fired; an unreadable CA bundle was
;; dressed up as a transport error too; `cert_reqs='NONE'` (upstream's own bare
;; spelling) and an unknown name both silently VERIFIED; and a fingerprint or
;; cipher pin was swallowed whole, reporting a guarantee nothing enforced.
(defdescribe
  urllib3-tls-fidelity-test
  (it "raises exceptions.SSLError for a certificate failure (was: ProtocolError)"
      (with-python-context
        (expect (= "SSLError:certificate verify failed"
                   (ev python-context
                       (str fake
                            "def _boom(*a, **k):\n"
                            "    raise _rq.exceptions.SSLError('certificate verify failed')\n"
                            "_rq.request = _boom\n"
                            "try:\n" "    urllib3.PoolManager().request('GET', 'https://svc/d')\n"
                            "    out = 'NOT RAISED'\n" "except urllib3.exceptions.SSLError as e:\n"
                            "    out = 'SSLError:' + str(e)\n" "out"))))))
  (it "lets a configuration error through verbatim (was: dressed as ProtocolError)"
      (with-python-context
        (expect
          (= "OSError"
             (ev python-context
                 (str fake
                      "def _boom(*a, **k):\n"
                      "    raise OSError('Could not find a suitable TLS CA certificate bundle, "
                      "invalid path: /nope')\n" "_rq.request = _boom\n"
                      "try:\n"
                      "    urllib3.PoolManager(ca_certs='/nope').request('GET', 'https://svc/d')\n"
                      "    out = 'NOT RAISED'\n" "except urllib3.exceptions.HTTPError as e:\n"
                      "    out = 'WRAPPED:' + type(e).__name__\n" "except OSError as e:\n"
                      "    out = type(e).__name__\n" "out"))))))
  (it "resolves cert_reqs like upstream and refuses an unknown name (was: silently verified)"
      (with-python-context
        (expect
          (= [true "ValueError" 0 2]
             (ev python-context
                 (str fake
                      "urllib3.PoolManager(cert_reqs='NONE').request('GET', 'https://svc/d')\n"
                      "bare = _last['verify'] is False\n" "try:\n"
                      "    urllib3.PoolManager(cert_reqs='NOPE').request('GET', 'https://svc/d')\n"
                      "    bad = 'NOT RAISED'\n"
                      "except ValueError:\n" "    bad = 'ValueError'\n"
                      "[bare, bad, int(urllib3.util.ssl_.resolve_cert_reqs('CERT_NONE')),\n"
                      " int(urllib3.util.ssl_.resolve_cert_reqs(None))]"))))))
  (it "refuses assert_fingerprint and ciphers instead of dropping them silently"
      (with-python-context
        (expect
          (= ["NotImplementedError" "NotImplementedError"]
             (ev python-context
                 (str fake
                      "out = []\n"
                      "for opts in ({'assert_fingerprint': 'aa:bb'}, {'ciphers': 'AES256-SHA'}):\n"
                      "    try:\n"
                      "        urllib3.PoolManager(**opts).request('GET', 'https://svc/d')\n"
                      "        out.append('NOT RAISED')\n" "    except NotImplementedError as e:\n"
                      "        out.append(type(e).__name__)\n" "out"))))))
  (it
    "publishes util.ssl_.create_urllib3_context and hands that context to the transport"
    (with-python-context
      (expect
        (=
          [true false true]
          (ev
            python-context
            (str
              fake
              "import ssl\n"
              "ctx = urllib3.util.ssl_.create_urllib3_context(cert_reqs='CERT_NONE')\n"
              "urllib3.PoolManager(ssl_context=ctx).request('GET', 'https://svc/d')\n"
              "[ctx.verify_mode == ssl.CERT_NONE, ctx.check_hostname, _last['verify'] is ctx]"))))))
  (it "sets ssl_minimum_version / ssl_maximum_version on the context (was: dropped)"
      (with-python-context
        (expect (= [true true]
                   (ev python-context
                       (str fake
                            "import ssl\n"
                            "urllib3.PoolManager(ssl_minimum_version=ssl.TLSVersion.TLSv1_2,\n"
                            "                    ssl_maximum_version=ssl.TLSVersion.TLSv1_2\n"
                            "                    ).request('GET', 'https://svc/d')\n"
                            "c = _last['verify']\n"
                            "[c.minimum_version == ssl.TLSVersion.TLSv1_2,\n"
                            " c.maximum_version == ssl.TLSVersion.TLSv1_2]")))))))
