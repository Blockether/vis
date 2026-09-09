(ns com.blockether.vis.internal.python.extension-center-test
  "SDK source installer -> uv -> trusted worker -> source reload.
   All storage and dependency indexes are test-owned. No live gateway or PyPI."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.extensions-test :as fixtures]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [java.io ByteArrayOutputStream]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.util.zip ZipEntry ZipOutputStream]
           [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]))

(defn- zip-bytes
  [entries]
  (let [buffer (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. buffer)]
      (doseq [[path text] entries]
        (.putNextEntry zip (ZipEntry. ^String path))
        (.write zip (.getBytes ^String text StandardCharsets/UTF_8))
        (.closeEntry zip)))
    (.toByteArray buffer)))

(defn- wheel
  [name version module source]
  (let [dist (str name "-" version ".dist-info/")]
    (zip-bytes {module source
                (str dist "METADATA")
                (str "Metadata-Version: 2.1\nName: " name "\nVersion: " version "\n")
                (str dist "WHEEL") "Wheel-Version: 1.0\nRoot-Is-Purelib: true\nTag: py3-none-any\n"
                (str dist "RECORD") ""})))

(deftest install-subdirectory-prepare-call-and-reload
  (#'fixtures/with-shared-packages
   (fn [_]
     (#'fixtures/with-fresh-loaded
      {}
      (fn [_ {:keys [ext-dir]}]
        (let
          [index
           (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)

           wheels
           {"vis_agent-0.1.0-py3-none-any.whl" (wheel "vis_agent" "0.1.0"
                                                      "sdk_fixture.py" "VALUE = 1\n")
            "vis_center_dep-1.0.0-py3-none-any.whl" (wheel "vis_center_dep" "1.0.0"
                                                           "vis_center_dep.py" "VALUE = 42\n")}

           repository
           (io/file ext-dir "repository")

           source
           (doto (io/file repository "plugins/greeting") .mkdirs)

           opts
           {:dirs [(str (io/file ext-dir "extensions"))]}

           calls
           (atom 0)

           diagnostics
           (atom [])

           code
           "import vis_center_dep\ndef greet():\n    \"Return the installed dependency value.\"\n    return vis_center_dep.VALUE\n"

           run-uv
           (fn [project args]
             (let [process
                   (.start (doto (ProcessBuilder. ^java.util.List args)
                             (.directory project)
                             (.redirectErrorStream true)))

                   output
                   (slurp (.getInputStream process))

                   exit
                   (.waitFor process)]

               (swap! diagnostics conj {:exit exit :output output})
               (when-not (zero? exit) (throw (ex-info "Fixture uv failed" {})))))]

          (.createContext
            index
            "/"
            (reify
              HttpHandler
                (handle [_ exchange]
                  (let [^HttpExchange exchange
                        exchange

                        file
                        (last (str/split (.getPath (.getRequestURI exchange)) #"/"))

                        bytes
                        (or (get wheels file)
                            (.getBytes (str/join
                                         ""
                                         (for [name
                                               (keys wheels)

                                               :when (str/starts-with?
                                                       name
                                                       (str (str/replace file "-" "_") "-"))]

                                           (str "<a href='/files/" name "'>" name "</a>")))
                                       StandardCharsets/UTF_8))]

                    (.set (.getResponseHeaders exchange)
                          "Content-Type"
                          (if (get wheels file) "application/octet-stream" "text/html"))
                    (.sendResponseHeaders exchange 200 (alength ^bytes bytes))
                    (with-open [out (.getResponseBody exchange)]
                      (.write out ^bytes bytes))
                    (.close exchange)))))
          (.start index)
          (try
            ;; The installer executes Git in its trusted interpreter, not the model sandbox.
            (is (str/starts-with? (#'pyx/package-call "_git('version')") "git version"))
            (spit (io/file source "pyproject.toml")
                  (str "[project]\nname='vis-center-greeter'\nversion='1.0.0'\n"
                       "description='End-to-end greeting tools'\nrequires-python='>=3.11'\n"
                       "dependencies=['vis-agent>=0.1.0','vis-center-dep==1.0.0']\n"
                       "[tool.vis]\ncategory='tools'\nsource_paths=['src']\n"))
            (spit
              (io/file source "extension.py")
              (str
                "import blockether.vis.extension as vis\nfrom center_logic import greet\n"
                "vis.register(vis.Extension(name='vis-center-greeter',description='Greeting tools',"
                "alias='center',symbols=[vis.Symbol(greet)]))\n"))
            (.mkdirs (io/file source "src"))
            (spit (io/file source "src/center_logic.py") code)
            (is (= "source"
                   (get (pyx/install-package! (str repository)
                                              {:trust true
                                               :subdirectory "plugins/greeting"
                                               :directory (first (:dirs opts))})
                        "mode")))
            (with-redefs [config/load-config-raw
                          (constantly {"python" {"index_url" (str "http://127.0.0.1:"
                                                                  (.getPort (.getAddress index))
                                                                  "/simple")}})

                          python-runtime/run-uv!
                          (fn [project args]
                            (swap! calls inc)
                            (run-uv project args))]

              (let [invoke #(:result ((#'fixtures/symbol-fn
                                       (#'fixtures/registered "vis-center-greeter")
                                       'greet)))]
                (is (not (.exists (io/file source "uv.lock"))))
                (is (= 0 (:failed (pyx/reload-python-extensions! opts)))
                    (pr-str {:failures (pyx/load-failures) :uv @diagnostics}))
                (is (= 42 (invoke)))
                (is (.isFile (io/file source "uv.lock")))
                (is (not (.exists (io/file source ".venv"))))
                (is (= 3 @calls) "Resolve, export, install; no separate manual command")
                (pyx/reload-python-extensions! opts)
                (is (= 3 @calls) "An unchanged project skips installation")
                (spit (io/file source "src/center_logic.py")
                      (str/replace code
                                   "return vis_center_dep.VALUE"
                                   "return vis_center_dep.VALUE + 1"))
                (is (= 42 (invoke)))
                (is (= 0 (:failed (pyx/reload-python-extensions! opts))))
                (is (= 43 (invoke)))
                (is (= 3 @calls))
                (spit (io/file source "extension.py") "raise ValueError('broken edit')\n")
                (is (= 1 (:failed (pyx/reload-python-extensions! opts))))
                (is (= 43 (invoke)))))
            (finally (.stop index 0)))))))))
