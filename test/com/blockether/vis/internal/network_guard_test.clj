(ns com.blockether.vis.internal.network-guard-test
  "The Vis-owned side of Python network setup. Network-policy semantics belong to
   vis-python-runtime; these tests only prove capability and proxy configuration
   reach the embedded interpreter."
  (:require [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- sandbox
  [network-opts]
  (tpc/new-context {} nil (merge {:enabled? false :jail-enabled? true} network-opts)))

(defn- dispose!
  [sandbox]
  (env/dispose-python-context! (:python-context sandbox))
  (try (runtime/network! true) (catch Throwable _ nil)))

(defn- python-eval [sandbox source] (tpc/ev (:python-context sandbox) source))

(defdescribe
  network-runtime-configuration-test
  (it "passes the disabled network capability to the runtime"
      (let [sandbox (sandbox nil)]
        (try (expect (= "blocked"
                        (python-eval sandbox
                                     (str "import socket
"
                                          "try:
    socket.socket()
    result = 'open'
"
                                          "except PermissionError:
    result = 'blocked'
"
                                          "result"))))
             (finally (dispose! sandbox)))))
  (it "passes proxy and CA settings into the interpreter environment"
      (let [sandbox (sandbox {:enabled? true :proxy-port 65500 :ca-file "/tmp/vis-fake-ca.pem"})]
        (try (expect (= ["http://127.0.0.1:65500" "http://127.0.0.1:65500" "/tmp/vis-fake-ca.pem"
                         "/tmp/vis-fake-ca.pem"]
                        (python-eval sandbox
                                     (str "import os
"
                                          "[os.environ.get('http_proxy'), "
                                          " os.environ.get('https_proxy'), "
                                          " os.environ.get('REQUESTS_CA_BUNDLE'), "
                                          " os.environ.get('SSL_CERT_FILE')]"))))
             (finally (dispose! sandbox))))))
