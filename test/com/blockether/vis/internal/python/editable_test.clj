(ns com.blockether.vis.internal.python.editable-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.python.extensions :as extensions]
            [com.blockether.vis.internal.python.extensions-test :as fixtures]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.loop :as loop]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(defn- probe
  [context]
  (let [ext (#'fixtures/registered "editable-fixture")]
    (loop/sync-active-extension-symbols!
      {:python-context context :extensions (atom [ext]) :active-extensions (atom [])}
      [ext])
    (env/run-python-block
      context
      "import vis_editable_fixture\nfrom vis_editable_fixture import value\nassert await editable_value() == value.answer()\nprint(value.answer(), vis_editable_fixture.__file__)")))

(deftest editable-sync-and-reload-test
  ;; #175: real uv install -> sandbox import -> edit -> /reload, without reinstalling.
  (#'fixtures/with-shared-packages
   (fn [packages]
     (#'fixtures/with-fresh-loaded
      {}
      (fn [_ {:keys [ext-dir]}]
        (let [project
              (io/file ext-dir "project")

              source
              (io/file project "src/vis_editable_fixture")

              value
              (io/file source "value.py")

              entries
              (io/file ext-dir ".vis/extensions")

              contexts
              (atom [])

              make-context
              (fn []
                (let [ctx (:python-context (env/create-python-context
                                             {}
                                             (constantly [(str ext-dir)])
                                             {:worker? true :jail-enabled? true :enabled? false}
                                             nil))]
                  (swap! contexts conj ctx)
                  ctx))

              reload!
              #(let [result (extensions/reload-python-extensions! {:dirs [(str entries)]})]
                 (is (zero? (:failed result)) (str (extensions/load-failures))) result)]

          (.mkdirs source)
          (.mkdirs entries)
          (spit
            (io/file project "pyproject.toml")
            "[project]\nname = 'vis-editable-fixture'\nversion = '0.0.1'\nrequires-python = '>=3.12'\n[build-system]\nrequires = []\nbuild-backend = 'backend'\nbackend-path = ['.']\n")
          (io/copy (io/file "test/com/blockether/vis/internal/python/fixtures/editable_backend.py")
                   (io/file project "backend.py"))
          (spit (io/file source "__init__.py") "from .value import answer\n")
          (spit value "def answer():\n    return 41\n")
          (spit
            (io/file entries "editable.py")
            "import blockether.vis.extension as vis\nfrom vis_editable_fixture.value import answer\ndef editable_value():\n    \"Return the editable source value.\"\n    return answer()\nvis.register(vis.Extension(name='editable-fixture', alias='editable', description='Editable fixture', symbols=[vis.Symbol(editable_value)]))\n")
          (try
            (#'fixtures/run-fixture-uv!
             project
             ["uv" "lock" "--project" (str project) "--offline" "--no-python-downloads" "--python"
              (com.blockether.vispython.Interpreter/pythonExecutable)])
            (with-redefs-fn {#'python-runtime/project-home (fn [_]
                                                             (io/file ext-dir "prepared"))
                             #'python-runtime/run-uv! @#'fixtures/run-fixture-uv!}
              (fn []
                (is (= 0
                       (:exit (python-runtime/uv-command! ["sync" "--project" (str project)
                                                           "--locked" "--offline"]))))))
            (is (some #(str/ends-with? (.getName ^java.io.File %) ".pth") (.listFiles packages)))
            (is (not (.exists (io/file packages "vis_editable_fixture"))))
            (is (= {:loaded 1 :failed 0 :changed? true} (reload!)))
            (let [first-context
                  (make-context)

                  id
                  (java.util.UUID/randomUUID)

                  original
                  (probe first-context)

                  invoke
                  #(let [ext (#'fixtures/registered "editable-fixture")] (:result
                                                                           ((#'fixtures/symbol-fn
                                                                             ext
                                                                             'editable_value))))]

              (is (nil? (:error original)) (str original))
              (is (str/includes? (:stdout original) (str source "/__init__.py")))
              (is (str/starts-with? (:stdout original) "41 "))
              (is (= 41 (invoke)))
              ;; Same timestamp and size deliberately exercise stale .pyc handling.
              (let [mtime (.lastModified value)]
                (spit value "def answer():\n    return 42\n")
                (.setLastModified value mtime))
              (with-redefs [python-runtime/uv-sync!
                            (fn [& _]
                              (throw (ex-info "Unexpected reinstall" {})))

                            loop/cache
                            (atom {id (#'loop/new-cache-entry {:python-context first-context})})

                            loop/policy-reload-epoch
                            (atom 0)]

                (is (= {:loaded 1 :failed 0 :changed? true} (reload!)))
                ((get @@#'extension/reload-hooks
                      :com.blockether.vis.internal.loop/security-policy-reload))
                (is (not (worker/worker-live? first-context)))
                (let [fresh (probe (make-context))]
                  (is (nil? (:error fresh)) (str fresh))
                  (is (str/starts-with? (:stdout fresh) "42 ")))
                (is (= 42 (invoke)))))
            (finally (doseq [ctx @contexts]
                       (env/dispose-python-context! ctx))))))))))
