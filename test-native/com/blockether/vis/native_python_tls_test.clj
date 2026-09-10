(ns com.blockether.vis.native-python-tls-test
  "Vis #185: the configured TLS policy reaches both packaged native workers."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis-python-runtime :as runtime]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest native-python-tls-policy-test
  (let [library
        (python-runtime/ensure-library!)

        executable
        (runtime/resolve-worker {:path library})

        project
        (.getCanonicalPath (io/file "."))

        home
        (.toFile (java.nio.file.Files/createTempDirectory
                   (.toPath (doto (io/file "target") .mkdirs))
                   "native-tls-"
                   (make-array java.nio.file.attribute.FileAttribute 0)))

        source
        (str "import ssl\n" "c = ssl.create_default_context()\n"
             "assert c.verify_mode == ssl.CERT_REQUIRED and c.check_hostname\n"
             "print(bool(c.verify_flags & ssl.VERIFY_X509_STRICT))")]

    (is (some? executable) "Build or stage the native Python worker before running test-native")
    (try
      (with-redefs-fn {#'worker/child-argv (fn [_ socket guest-dir]
                                             [executable
                                              (str "-Duser.home=" (.getCanonicalPath home)) socket
                                              guest-dir])}
        (fn []
          (doseq [strict [nil true false]]
            (with-redefs [config/load-config-raw
                          (constantly (if (nil? strict) {} {"python" {"tls_strict" strict}}))]
              (let [made (env/create-python-context
                           {}
                           (constantly [project])
                           {:worker? true :jail-enabled? true :enabled? false}
                           nil)
                    session (:python-context made)
                    extension-worker (worker/extension-worker-key session)
                    ctx (atom nil)]

                (try (let [result (env/run-python-block session source)]
                       (is (nil? (:error result)))
                       (is (= (if (false? strict) "False\n" "True\n") (:stdout result))))
                     (reset! ctx (pyx/build-context extension-worker "native-tls"))
                     (pyx/bind-host! @ctx "native-tls")
                     (worker/exec! extension-worker @ctx pyx/bootstrap-python)
                     (worker/exec! extension-worker @ctx source)
                     (is (= (if (false? strict) "False" "True")
                            (worker/eval-str extension-worker
                                             @ctx
                                             "str(bool(c.verify_flags & ssl.VERIFY_X509_STRICT))")))
                     (finally (when @ctx (#'pyx/discard-context! @ctx))
                              (worker/stop-worker! extension-worker)
                              (env/dispose-python-context! session))))))))
      (finally (doseq [file (reverse (file-seq home))]
                 (io/delete-file file true))))))
