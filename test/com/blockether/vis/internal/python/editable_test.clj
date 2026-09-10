(ns com.blockether.vis.internal.python.editable-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.python.extensions :as extensions]
            [com.blockether.vis.internal.python.extensions-test :as fixtures]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.loop :as loop]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(defn- entry-source
  [typed? description]
  (str
    "# /// script\n# dependencies = []\n# [tool.vis]\n"
    "# project = '../../project'\n# ///\n"
    "from __future__ import annotations\n"
    "from dataclasses import dataclass\nimport blockether.vis.extension as vis\n"
    "import vis_editable_fixture\nfrom vis_editable_fixture.value import answer\n"
    "def editable_value():\n    \"Return the editable source value.\"\n    return answer()\n"
    "def editable_source():\n    \"Return the editable import location.\"\n    return vis_editable_fixture.__file__\n"
    "@dataclass(frozen=True)\nclass HelpDocument:\n    text: str\n"
    "class Doctor:\n    def help(self"
    (when typed? ", tool: str = 'doctor'")
    ")"
    (when typed? " -> HelpDocument")
    ":\n        \""
    description
    "\"\n"
    "        return HelpDocument(" (if typed? "tool" "'doctor'")
    " + ':' + str(answer()))\n"
    "vis.register(vis.Extension(name='editable-fixture', alias='editable', "
    "description='Editable fixture', symbols=[vis.Symbol(editable_value), vis.Symbol(editable_source), "
    "vis.Symbol(Doctor(), name='doctor')]))\n"))

(defn- probe
  [context typed? description]
  (let [ext (#'fixtures/registered "editable-fixture")]
    (loop/sync-active-extension-symbols!
      {:python-context context :extensions (atom [ext]) :active-extensions (atom [])}
      [ext])
    (env/run-python-block
      context
      (str
        "import inspect, importlib.util\n"
        "assert importlib.util.find_spec('vis_editable_fixture') is None\n"
        "value = await editable_value()\n"
        "assert str(inspect.signature(doctor.help)) == '"
        (if typed? "(tool=Ellipsis)" "()")
        "'\n"
        "assert '"
        description
        "' in doc('doctor.help')\n"
        (when typed?
          (str
            "assert doctor.help.contract['parameters'][0]['name'] == 'tool'\n"
            "assert doctor.help.contract['returns'].get('fields', [{}])[0].get('name') == 'text', repr(doctor.help.contract['returns'])\n"
            "assert 'tool: str' in doc('doctor.help')\n"
            "assert (await doctor.help('other')).text == 'other:' + str(value)\n"))
        "print(value, await editable_source())"))))

(deftest editable-sync-and-reload-test
  ;; #175 and #178: real manual uv preparation, source/API reload, stale status and retry.
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

              entry
              (io/file entries "editable.py")

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
              (fn [sync?]
                (extensions/reload-python-extensions! {:dirs [(str entries)]
                                                       :sync-projects? sync?}))]

          (.mkdirs source)
          (.mkdirs entries)
          (spit
            (io/file project "pyproject.toml")
            "[project]\nname = 'vis-editable-fixture'\nversion = '0.0.1'\nrequires-python = '>=3.12'\n[build-system]\nrequires = []\nbuild-backend = 'backend'\nbackend-path = ['.']\n")
          (io/copy (io/file "test/com/blockether/vis/internal/python/fixtures/editable_backend.py")
                   (io/file project "backend.py"))
          (spit (io/file source "__init__.py") "from .value import answer\n")
          (spit value "def answer():\n    return 41\n")
          (spit entry (entry-source false "Original help."))
          (try
            (python-runtime/ensure-library!)
            (is (zero? (python-runtime/uv-command!
                         ["lock" "--project" (str project) "--offline" "--no-python-downloads"
                          "--python" (com.blockether.vispython.Interpreter/pythonExecutable)])))
            (let [prepared #(python-runtime/prepared-project project)]
              (is (= 0
                     (python-runtime/uv-command!
                       ["sync" "--project" (str project) "--locked" "--offline" "--python"
                        (com.blockether.vispython.Interpreter/pythonExecutable)])))
              (is (some #(str/ends-with? (.getName ^java.io.File %) ".pth")
                        (.listFiles ^java.io.File (prepared))))
              (is (not (.exists (io/file packages "vis_editable_fixture"))))
              (is (= {:loaded 1 :failed 0 :changed? true} (reload! false)))
              (let [first-context (make-context)
                    id (java.util.UUID/randomUUID)
                    original (probe first-context false "Original help.")
                    invoke #(let [ext (#'fixtures/registered "editable-fixture")]
                              (:result ((#'fixtures/symbol-fn ext 'editable_value))))]

                (is (nil? (:error original)) (str original))
                (is (str/includes? (:stdout original) (str source "/__init__.py")))
                (is (str/starts-with? (:stdout original) "41 "))
                (is (= 41 (invoke)))
                ;; Same timestamp and size deliberately exercise stale .pyc handling.
                (let [mtime (.lastModified value)]
                  (spit value "def answer():\n    return 42\n")
                  (.setLastModified value mtime))
                (spit entry (entry-source true "Typed help."))
                (with-redefs [python-runtime/ensure-project!
                              (fn [& _]
                                (throw (ex-info "Unexpected reinstall" {})))
                              loop/cache (atom {id (#'loop/new-cache-entry
                                                    {:python-context first-context})})
                              loop/policy-reload-epoch (atom @loop/policy-reload-epoch)]

                  (is (= {:loaded 1 :failed 0 :changed? true} (reload! false)))
                  ((get @@#'extension/reload-hooks
                        :com.blockether.vis.internal.loop/security-policy-reload))
                  (is (not (worker/worker-live? first-context)))
                  (let [fresh (probe (make-context) true "Typed help.")]
                    (is (nil? (:error fresh)) (str fresh))
                    (is (str/starts-with? (:stdout fresh) "42 ")))
                  (is (= 42 (invoke)))))
              ;; A genuine readiness change retains a visibly stale, internally consistent API.
              (spit (io/file project "pyproject.toml")
                    (str/replace (slurp (io/file project "pyproject.toml"))
                                 "version = '0.0.1'"
                                 "version = '0.0.2'"))
              (spit (io/file project "backend.py")
                    (str/replace (slurp (io/file project "backend.py")) "0.0.1" "0.0.2"))
              (spit entry (entry-source true "Retried help."))
              (is (= 1 (:failed (reload! false))))
              (let [failure (first (extensions/load-failures))
                    prompt (:ext/prompt-fn (#'fixtures/registered "python-extensions"))
                    stale (probe (make-context) true "Typed help.")]

                (is (nil? (:error stale)) (str stale))
                (is (true? (:stale? failure)))
                (is (str/includes? (:error failure) "uv sync"))
                (is (not= (:loaded-fingerprint failure) (:requested-fingerprint failure)))
                (is (str/includes? (prompt {}) "tools and docs are stale")))
              ;; Explicit host preparation does not depend on the assistant's shell toggle.
              (with-redefs [toggles/enabled? (constantly false)]
                (is (= {:loaded 1 :failed 0 :changed? true} (reload! true))))
              (is (empty? (extensions/load-failures)))
              (is (nil? ((:ext/prompt-fn (#'fixtures/registered "python-extensions")) {})))
              (let [fresh (probe (make-context) true "Retried help.")]
                (is (nil? (:error fresh)) (str fresh))
                (is (str/starts-with? (:stdout fresh) "42 "))))
            (finally (doseq [ctx @contexts]
                       (env/dispose-python-context! ctx))))))))))
