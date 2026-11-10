(ns com.blockether.vis.internal.python.shell-handle-contract-test
  "The shell handle of an extension gives the answers of the contract fixture
   `vis-contract/fixtures/shell-handle.json`. One real extension worker runs each case
   through `vis.shell` (the SDK handle) and through `vis.tools.shell` (the runtime
   handle). The SDK test `test_shell_contract.py` runs the same cases outside Vis."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- fixture-text [] (slurp (io/resource "vis-contract/fixtures/shell-handle.json")))

(defn- extension-source
  "A Python extension that runs each case of the fixture through one door."
  [text]
  (str
    "import json\n"
    "import blockether.vis.extension as vis\n"
    "\n"
    "TABLE = json.loads("
    (pr-str text)
    ")\n"
    "exec(\"\\n\".join(TABLE[\"runner\"]), globals())\n" "\n"
    "\n" "def sdk() -> str:\n"
    "    \"Run each case through vis.shell.\"\n"
    "    return json.dumps([drive(vis.shell, case) for case in TABLE[\"cases\"]])\n"
    "\n" "\n"
    "def tools() -> str:\n" "    \"Run each case through vis.tools.shell.\"\n"
    "    return json.dumps([drive(vis.tools.shell, case) for case in TABLE[\"cases\"]])\n" "\n"
    "\n"
    "vis.register_extension(vis.Extension(name=\"shellcontract\", description=\"Shell contract fixture\", alias=\"shellcontract\", symbols=[vis.Symbol(sdk), vis.Symbol(tools)]))\n"))

(defn- answers
  "The answers of both doors, by door name, from one real extension worker."
  [text]
  (let [dir
        (.toFile (Files/createTempDirectory "vis-shell-contract" (make-array FileAttribute 0)))

        store
        (ps/db-create-connection! :memory)

        session-id
        (h/store-session! store {:title "Shell contract"})]

    (try (spit (io/file dir "shellcontract.py") (extension-source text))
         (pyx/reload-python-extensions! {:dirs [(str dir)]})
         (let [ext
               (some #(when (= "shellcontract" (:ext/name %)) %) (extension/registered-extensions))

               active
               [ext
                (some #(when (= "foundation-core" (:ext/name %)) %)
                      (extension/registered-extensions))]

               env
               {:db-info store
                :session-id session-id
                :extensions (atom active)
                :active-extensions (atom active)}]

           (binding [extension/*current-environment* env]
             (into {}
                   (map (fn [door]
                          [(name door)
                           (json/read-str (extension/invoke-symbol-wrapper
                                            ext
                                            (some #(when (= door (:ext.symbol/symbol %)) %)
                                                  (extension/ext-symbols ext))
                                            []
                                            env))]))
                   ['sdk 'tools])))
         (finally (pyx/reload-python-extensions! {:dirs []})
                  (ps/db-dispose-connection! store)
                  (doseq [f (reverse (file-seq dir))]
                    (io/delete-file f true))))))

(defn- differences
  "The cases where a door did not give the answer of the fixture. `door` names the
   exception object of the case that holds the answer of that door."
  [cases got door]
  (vec (keep (fn [[case answer]]
               (let [expected (get (get case door case) "expect")]
                 (when (not= expected answer)
                   {:case (get case "name") :expect expected :answer answer})))
             (map vector cases got))))

(defdescribe
  shell-handle-contract-test
  (it "gives the answers of the fixture through the SDK handle and the runtime handle"
      (foundation/register!)
      (let [text
            (fixture-text)

            fixture
            (json/read-str text)

            cases
            (get fixture "cases")

            got
            (answers text)]

        (expect (seq cases))
        (expect (= (count cases) (count (got "sdk")) (count (got "tools"))))
        (expect (= [] (differences cases (got "sdk") nil)))
        (expect (= [] (differences cases (got "tools") "tools")))
        ;; A difference is a named exception, and it is a real difference.
        (expect (= []
                   (vec (for [case
                              cases

                              door
                              ["tools" "outside"]

                              :let [exception
                                    (get case door)]
                              :when (and exception
                                         (or (not (contains? (get fixture "exceptions")
                                                             (get exception "reason")))
                                             (= (get exception "expect") (get case "expect"))))]

                          (get case "name"))))))))
