(ns com.blockether.vis.internal.sandbox.network-guard-test
  "The Vis-owned Python network stack: the runtime enforces the binary capability,
   while a session worker receives the host proxy and CA environment before CPython
   starts. A real urllib request proves the worker can reach only that proxy door."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.sandbox.egress-proxy :as egress]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net InetSocketAddress ServerSocket)))

(defn- sandbox
  [network-opts]
  (tpc/new-context {} nil (merge {:enabled? false :jail-enabled? true :worker? true} network-opts)))

(defn- dispose!
  [sandbox]
  (env/dispose-python-context! (:python-context sandbox))
  (try (runtime/network! true) (catch Throwable _ nil)))

(defn- python-json
  [sandbox source]
  (let [{:keys [stdout error]} (env/run-python-block (:python-context sandbox) source)]
    (when error (throw (ex-info (str "Python network probe failed: " error) {:error error})))
    (json/read-json (str/trim stdout) :key-fn keyword)))

(defn- start-origin!
  []
  (let [server
        (doto (ServerSocket.) (.bind (InetSocketAddress. "127.0.0.1" 0)))

        running
        (atom true)

        requests
        (atom [])

        serve
        (future
          (while @running
            (try (with-open [client
                             (.accept server)

                             in
                             (BufferedReader. (InputStreamReader. (.getInputStream client)))]

                   (let [line (.readLine in)]
                     (loop []

                       (when-not (= "" (.readLine in)) (recur)))
                     (swap! requests conj line)
                     (doto (.getOutputStream client)
                       (.write
                         (.getBytes
                           "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"))
                       (.flush))))
                 (catch Throwable _ nil))))]

    {:port (.getLocalPort server)
     :requests requests
     :stop! (fn []
              (reset! running false)
              (.close server)
              (future-cancel serve))}))

(defdescribe
  network-runtime-configuration-test
  (it "passes the disabled network capability to the runtime"
      (let [sandbox (sandbox nil)]
        (try (expect (= "blocked"
                        (python-json sandbox
                                     (str "import json, socket
"
                                          "try:
    socket.socket()
    result = 'open'
"
                                          "except PermissionError:
    result = 'blocked'
"
                                          "print(json.dumps(result))"))))
             (finally (dispose! sandbox)))))
  (it "passes proxy and CA settings into the interpreter environment"
      (let [sandbox (sandbox {:enabled? true :proxy-port 65500 :ca-file "/tmp/vis-fake-ca.pem"})]
        (try (expect (= ["http://127.0.0.1:65500" "http://127.0.0.1:65500" "/tmp/vis-fake-ca.pem"
                         "/tmp/vis-fake-ca.pem"]
                        (python-json sandbox
                                     (str "import json, os
"
                                          "print(json.dumps([os.environ.get('http_proxy'), "
                                          "os.environ.get('https_proxy'), "
                                          "os.environ.get('REQUESTS_CA_BUNDLE'), "
                                          "os.environ.get('SSL_CERT_FILE')]))"))))
             (finally (dispose! sandbox)))))
  (it
    "routes every Python HTTP stack through one attributed policy door"
    (let [origin
          (start-origin!)

          token
          "python-session"

          logs
          (atom [])

          policy
          (egress/compile-policy {:allowed-domains ["localhost"]
                                  :rules [{:host "localhost" :access "read-only"}]})

          proxy
          (egress/start! {:policy-fn (fn [_]
                                       policy)
                          :on-log #(swap! logs conj (assoc % :token token))})

          sandbox
          (sandbox {:enabled? true :proxy-port (:port proxy) :proxy-token token})

          url
          (str "http://localhost:" (:port origin) "/probe")]

      (try
        (expect
          (= ["ok" "ok" "ok" "ok" "ok"]
             (python-json sandbox
                          (str "import importlib, json, urllib.request\n"
                               "import aiohttp, httpx, requests\n"
                               "from pip._internal.network.session import PipSession\n"
                               "real_asyncio = importlib.import_module('asyncio')\n"
                               "url = " (pr-str url)
                               "\n" "async def aio():\n"
                               "    async with aiohttp.ClientSession(trust_env=True) as session:\n"
                               "        async with session.get(url) as response:\n"
                               "            return await response.text()\n"
                               "aio_result = real_asyncio.run(aio())\n"
                               "with PipSession() as pip_session:\n"
                               "    pip_result = pip_session.get(url, timeout=5).text\n"
                               "print(json.dumps([\n"
                               "    urllib.request.urlopen(url, timeout=5).read().decode(),\n"
                               "    requests.get(url, timeout=5).text,\n"
                               "    httpx.get(url, timeout=5).text,\n"
                               "    aio_result,\n" "    pip_result]))"))))
        ;; Regression: closing with an unread POST body discarded the 403 response.
        (expect (= 403
                   (python-json sandbox
                                (str "import json, urllib.error, urllib.request\n"
                                     "try:\n"
                                     "    urllib.request.urlopen(urllib.request.Request("
                                     (pr-str url)
                                     ", data=b'x', method='POST'), timeout=5)\n"
                                     "    result = 200\n"
                                     "except urllib.error.HTTPError as error:\n"
                                     "    result = error.code\n" "print(json.dumps(result))"))))
        (expect (= 5 (count @(:requests origin))))
        (expect
          (some
            #(and (= token (:token %)) (= "localhost" (:host %)) (= "GET" (:method %)) (:allow? %))
            @logs))
        (expect (some #(and (= token (:token %)) (= "POST" (:method %)) (false? (:allow? %)))
                      @logs))
        (finally (dispose! sandbox) ((:stop! proxy)) ((:stop! origin)))))))
