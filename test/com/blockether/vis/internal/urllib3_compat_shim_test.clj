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
def _fake(method, url, params=None, data=None, json=None, headers=None,
          cookies=None, auth=None, timeout=None, allow_redirects=True, **kw):
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
