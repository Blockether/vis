(ns com.blockether.vis.internal.httpx-compat-shim-test
  "The httpx-compat shim: a synchronous httpx module published into sys.modules,
   wrapping the requests shim. Tested offline by monkeypatching requests.request
   with a canned echo Response (no network)."
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
  httpx-module-test
  (it
    "publishes httpx under sys.modules and works with no import"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              fake
              "import sys, httpx\n"
              "sys.modules['httpx'] is httpx and httpx.get('http://svc/a').status_code == 200"))))))
  (it "reports a vis version marker"
      (with-python-context (expect (true? (ev python-context
                                              (str fake "httpx.__version__.endswith('-vis')")))))))

(defdescribe
  httpx-request-test
  (it "echoes query params on GET and wraps status/success"
      (with-python-context (expect (= {"q" "1"}
                                      (ev python-context
                                          (str fake
                                               "r = httpx.get('http://svc/a', params={'q': '1'})\n"
                                               "assert r.is_success\nr.json()['params']"))))))
  (it "exposes case-insensitive headers, a URL wrapper and an httpx repr"
      (with-python-context
        (expect (true? (ev python-context
                           (str fake
                                "r = httpx.get('http://svc/a')\n"
                                "r.headers.get('content-type') == 'application/json' "
                                "and str(r.url) == 'http://svc/a' and r.url == 'http://svc/a' "
                                "and repr(r) == '<Response [200]>'"))))))
  (it "round-trips a JSON body on POST and reports 201"
      (with-python-context
        (expect (true? (ev python-context
                           (str fake
                                "r = httpx.post('http://svc/b', json={'x': 5})\n"
                                "r.status_code == 201 and r.json()['json'] == {'x': 5}")))))))

(defdescribe
  httpx-client-test
  (it
    "joins base_url and merges default with per-call headers"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              fake
              "c = httpx.Client(base_url='http://svc', headers={'X-Base': 'b'})\n"
              "r = c.get('/c', headers={'X-Call': 'k'})\n" "h = r.json()['headers']\n"
              "c.close()\n"
              "r.json()['url'] == 'http://svc/c' and h.get('X-Base') == 'b' and h.get('X-Call') == 'k'")))))))

(defdescribe httpx-errors-test
             (it "raise_for_status raises HTTPStatusError carrying the response"
                 (with-python-context
                   (expect (true? (ev python-context
                                      (str fake
                                           "r = httpx.get('http://svc/missing')\n"
                                           "ok = r.status_code == 404 and r.is_error\n"
                                           "raised = False\n" "try:\n"
                                           "    r.raise_for_status()\n"
                                           "except httpx.HTTPStatusError as e:\n"
                                           "    raised = (e.response.status_code == 404)\n"
                                           "ok and raised")))))))

(defdescribe
  httpx-async-client-test
  (it "AsyncClient exposes awaitable verbs that drive the sync core"
      (with-python-context
        (expect (true? (ev python-context
                           (str fake
                                "import httpx\n"
                                "c = httpx.AsyncClient(base_url='http://svc')\n"
                                "coro = c.get('/a', params={'q': '1'})\n"
                                "assert hasattr(coro, 'send')\n" "val = None\n"
                                "try:\n" "    coro.send(None)\n"
                                "except StopIteration as e:\n" "    val = e.value\n"
                                "val.status_code == 200 and val.json()['params'] == {'q': '1'} "
                                "and val.json()['url'] == 'http://svc/a'"))))))
  (it "publishes AsyncClient on the httpx module"
      (with-python-context
        (expect (true? (ev python-context
                           (str fake
                                "import httpx\n"
                                "httpx.AsyncClient is not None "
                                "and isinstance(httpx.AsyncClient(), httpx.AsyncClient)")))))))
