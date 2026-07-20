(ns com.blockether.vis.internal.urllib3-compat-shim-test
  "The urllib3-compat shim: a urllib3 module (PoolManager/HTTPResponse) published
   into sys.modules, wrapping the requests shim. Tested offline by monkeypatching
   requests.request with a canned echo Response (no network)."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defmacro with-python-context
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ~'python-context)))))

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
                  "import sys\n"
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
