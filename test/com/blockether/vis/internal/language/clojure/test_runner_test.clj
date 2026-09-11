(ns com.blockether.vis.internal.language.clojure.test-runner-test
  "Unit tests for the in-REPL test path's failure handling — specifically that a
   server that vanishes mid-run surfaces as a structured result the model can act
   on, never a raw connect exception that eats the turn."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :as ct]
            [com.blockether.vis.contract.surface :as contract]
            [com.blockether.vis.internal.language.clojure.nrepl-client :as nc]
            [com.blockether.vis.internal.language.clojure.repl-manager :as repl-manager]
            [com.blockether.vis.internal.language.clojure.shadow-cljs :as shadow]
            [com.blockether.vis.internal.language.clojure.test-runner :as tr]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.foundation.shell :as shell-owner]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private run-via-repl
  @#'com.blockether.vis.internal.language.clojure.test-runner/run-via-repl)

(def ^:private normalize-faults
  @#'com.blockether.vis.internal.language.clojure.test-runner/normalize-faults)

(defn- connect-failed-throw
  [_]
  (throw (ex-info "nREPL connect failed on localhost:54749 — is the REPL running?"
                  {:type :clj/nrepl-connect-failed :port 54749})))

(defdescribe run-via-repl-connect-failure-test
             (it "returns a structured 'server down' result when the probe reports down"
                 (with-redefs [nc/probe! (fn [_]
                                           {:status :down})]
                   (let [r (run-via-repl "." ["some.ns-test"] {} 54749)]
                     (expect (= "repl" (get r "mode")))
                     (expect (= 54749 (get r "port")))
                     (expect (re-find #"down or unresponsive" (get r "error")))
                     (expect (true? (get r "repl_unusable"))))))
             (it "converts a mid-run connect failure (probe passed, eval! then failed) into data"
                 ;; The TOCTOU window: probe answered :up, but the server crashed / was reaped /
                 ;; killed before the eval landed. Must NOT bubble a raw :clj/nrepl-connect-failed.
                 (with-redefs [nc/probe!
                               (fn [_]
                                 {:status :up})

                               nc/eval!
                               connect-failed-throw]

                   (let [r (run-via-repl "." ["some.ns-test"] {} 54749)]
                     (expect (map? r))
                     (expect (= "repl" (get r "mode")))
                     (expect (= 54749 (get r "port")))
                     (expect (re-find #"went down mid-run" (get r "error")))
                     (expect (re-find #"repl" (get r "error")))
                     (expect (true? (get r "repl_unusable"))))))
             (it "still propagates an unrelated ExceptionInfo instead of swallowing it"
                 (with-redefs [nc/probe!
                               (fn [_]
                                 {:status :up})

                               nc/eval!
                               (fn [_]
                                 (throw (ex-info "boom" {:type :something-else})))]

                   ;; lazytest has no `thrown?` macro — assert the throw with try/catch.
                   (let [thrown? (try (run-via-repl "." ["some.ns-test"] {} 54749)
                                      false
                                      (catch clojure.lang.ExceptionInfo e
                                        (= :something-else (:type (ex-data e)))))]
                     (expect (true? thrown?))))))

(def ^:private recover-if-unusable
  @#'com.blockether.vis.internal.language.clojure.test-runner/recover-if-unusable)

(defdescribe
  recover-if-unusable-test
  ;; Recovery never SPAWNS: an unusable REPL means the clean-JVM CLI runs the suite
  ;; THIS turn, and bringing a REPL back is the caller's own `repl_start` call.
  (it "runs the CLI suite in a clean JVM when the reused server was unusable"
      (with-redefs [com.blockether.vis.internal.language.clojure.test-runner/run-via-cli
                    (fn [_root _norm]
                      {"mode" "cli" "is_pass" true "note" "7 cases"})]
        (let [r (recover-if-unusable "/proj" {} {"repl_unusable" true "error" "down"})]
          (expect (= "cli" (get r "mode")))
          (expect (true? (get r "recovered")))
          (expect (re-find #"clean JVM" (get r "note")))
          (expect (re-find #"7 cases" (get r "note"))))))
  (it "keeps the timeout error for a wedged eval and shells no CLI"
      (let [cli-called (atom false)]
        (with-redefs [com.blockether.vis.internal.language.clojure.test-runner/run-via-cli
                      (fn [_root _norm]
                        (reset! cli-called true)
                        {})]
          (let [r (recover-if-unusable "/proj" {} {"repl_wedged" true "error" "timed out"})]
            (expect (false? @cli-called))
            (expect (re-find #"clean JVM" (get r "error")))))))
  (it "passes a healthy result through untouched (no CLI, nothing spawned)"
      (let [orig {"mode" "repl" "pass" 5}]
        (expect (= orig (recover-if-unusable "/proj" {} orig))))))

;; Regression: a lazytest fault whose location came from a stack frame the JVM
;; could not place carried `"file" "Unknown"` (or `NO_SOURCE_PATH`) and
;; `"line" -1` (`StackTraceElement.getLineNumber`, -2 for a native frame).
;; Both were handed on untouched, so `run_tests` threw
;; `language-surface contract violation for :test-fn` — a single unlocatable
;; failure destroyed the whole run's result instead of reporting the failing
;; test.
(defdescribe
  normalize-faults-location-test
  "`normalize-faults` drops a location the runtime could not resolve, so every
   fault stays inside the `:test-fn` contract (`\"line\"` is a NON-NEGATIVE count)
   and no digest prints a `(Unknown:-1)` that points nowhere."
  (it "drops unresolved file/line sentinels and keeps the result conformant"
      (let [parsed
            {"mode" "repl"
             "language" "clojure"
             "framework" "lazytest"
             "total" 26
             "pass" 25
             "fail" 1
             "failures" [{"ns" "a.core-test"
                          "test" "boom"
                          "type" "fail"
                          "message" "KeyError: 0"
                          "file" "Unknown"
                          "line" -1}
                         {"ns" "a.core-test"
                          "test" "bang"
                          "type" "error"
                          "message" "nope"
                          "file" "NO_SOURCE_PATH"
                          "line" -2}]}

            normalized
            (normalize-faults "." parsed)]

        (expect (nil? (get-in normalized ["failures" 0 "file"])))
        (expect (nil? (get-in normalized ["failures" 0 "line"])))
        (expect (nil? (get-in normalized ["failures" 1 "file"])))
        (expect (nil? (get-in normalized ["failures" 1 "line"])))
        ;; the message still identifies the failure — only the fake location goes
        (expect (= "KeyError: 0" (get-in normalized ["failures" 0 "message"])))
        (expect (contract/valid? :test-fn normalized))))
  (it "leaves a real file and line alone"
      (let [parsed
            {"mode" "repl"
             "language" "clojure"
             "failures" [{"ns" "a.core-test" "file" "test/a/core_test.clj" "line" 12}]}

            normalized
            (normalize-faults "." parsed)]

        (expect (= "test/a/core_test.clj" (get-in normalized ["failures" 0 "file"])))
        (expect (= 12 (get-in normalized ["failures" 0 "line"])))
        (expect (contract/valid? :test-fn normalized)))))

(defn- with-project
  "Build a throwaway project tree from `files` ({relpath contents}) and hand its
   root PATH to `f`, deleting the tree afterwards. Every clj-test-fn case needs
   one: selection is resolved against the workspace, so nothing can be chosen
   without files on disk to walk."
  [files f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                        "vis-clj-test"
                        (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try (doseq [[rel content] files]
           (let [^java.io.File file (io/file root rel)]
             (.mkdirs (.getParentFile file))
             (spit file content)))
         (f (.getPath root))
         (finally (doseq [x (reverse (file-seq root))]
                    (io/delete-file x true))))))

(defn- run-capturing
  "Run `clj-test-fn` for workspace `ws` with the nREPL and the repl runner
   stubbed, answering `{:root <where the nREPL booted> :nses <what the runner was
   handed> :sel <the resolved selector map>}` — the three facts every selector
   case is about."
  [ws arg]
  (let [seen (atom {})]
    (with-redefs [repl-manager/live-repl-for-dir (fn [_sid root]
                                                   (swap! seen assoc :root root)
                                                   {:port 12345})
                  com.blockether.vis.internal.language.clojure.test-runner/run-via-repl
                  (fn [_root nses sel _port]
                    (swap! seen assoc :nses nses :sel sel)
                    {"mode" "repl" "ns" (first nses)})]

      (tr/clj-test-fn {:workspace/root ws :session-id "sid"} arg)
      @seen)))

(def ^:private thing-test-file {"test/com/example/thing_test.clj" "(ns com.example.thing-test)\n"})

(defdescribe
  clj-test-fn-cwd-root-test
  "An explicit `cwd` arg roots the run — and thus nREPL selection — at THAT
   project, so tests in a sibling / added-folder project run against their own
   nREPL classpath instead of booting the workspace-root REPL (regression:
   run_tests silently dropped `cwd`, FileNotFoundException on the wrong REPL)."
  (it "boots/selects the nREPL at the cwd arg, not the workspace root"
      (with-project thing-test-file
                    (fn [root]
                      (let [seen (run-capturing "/ws" {"cwd" root "paths" ["test"]})]
                        (expect (= root (:root seen)))
                        (expect (= ["com.example.thing-test"] (:nses seen)))))))
  (it "falls back to the workspace root when no cwd arg is given"
      (with-project thing-test-file
                    (fn [root]
                      (expect (= root (:root (run-capturing root {"paths" ["test"]}))))))))

;; PATHS are the primary selector — clj-test-fn is WHERE a file becomes a
;; namespace — and a call that names the NAMESPACE itself resolves through the
;; same translation.
(defdescribe
  clj-test-fn-path-discovery-test
  (it "resolves a test FILE to the namespace it declares"
      (with-project
        thing-test-file
        (fn [root]
          (expect (= ["com.example.thing-test"]
                     (:nses (run-capturing root {"paths" ["test/com/example/thing_test.clj"]})))))))
  (it "resolves a SOURCE file to its *-test namespace"
      ;; The translation the paths-only contract rests on: name the code you
      ;; changed, run the tests that cover it.
      (with-project (assoc thing-test-file "src/com/example/thing.clj" "(ns com.example.thing)\n")
                    (fn [root]
                      (expect
                        (= ["com.example.thing-test"]
                           (:nses (run-capturing root {"paths" ["src/com/example/thing.clj"]})))))))
  ;; Regression, user report: selecting a nested source file with tests owned by
  ;; its nearest parent suite was answered with no test namespaces.
  (it "resolves a nested SOURCE file to the nearest parent *-test namespace"
      (with-project
        (assoc thing-test-file "src/com/example/thing/detail.clj" "(ns com.example.thing.detail)\n")
        (fn [root]
          (expect (= ["com.example.thing-test"]
                     (:nses (run-capturing root
                                           {"paths" ["src/com/example/thing/detail.clj"]})))))))
  (it "walks a directory for every test namespace under it"
      (with-project (assoc thing-test-file
                      "test/com/example/other_test.clj" "(ns com.example.other-test)\n")
                    (fn [root]
                      (expect (= ["com.example.other-test" "com.example.thing-test"]
                                 (:nses (run-capturing root {"paths" ["test"]})))))))
  (it "runs every test namespace in the workspace when no path is named"
      (with-project thing-test-file
                    (fn [root]
                      (expect (= ["com.example.thing-test"] (:nses (run-capturing root {})))))))
  (it "errors on a location that EXISTS yet holds no tests, never falling back to everything"
      (with-project (assoc thing-test-file "src/com/example/lonely.clj" "(ns com.example.lonely)\n")
                    (fn [root]
                      (let [e (try (run-capturing root {"paths" ["src"]})
                                   (catch clojure.lang.ExceptionInfo e e))]
                        (expect (= :clj/bad-args (:type (ex-data e))))
                        (expect (re-find #"no test namespaces.*under" (ex-message e)))))))
  ;; Regression, user report (paraphrased: ONE directory segment of an otherwise
  ;; correct path was misspelled, and the runner answered "no *_test.clj
  ;; namespaces under <that very test file>" — so the caller read it as a project
  ;; whose tests do not exist, retried the same typo, then went digging through
  ;; the build file instead of the spelling).
  (it "refuses a path that is NOT ON DISK as a typo, naming the part that is"
      (with-project
        thing-test-file
        (fn [root]
          (let [typo
                (str root "/repositories/nope/test/com/example/thing_test.clj")

                e
                (try (run-capturing root {"paths" [typo]}) (catch clojure.lang.ExceptionInfo e e))]

            (expect (= :clj/bad-args (:type (ex-data e))))
            (expect (= [typo] (:missing (ex-data e))))
            (expect (str/includes? (ex-message e) "no such path"))
            ;; The deepest live ancestor is the last segment that was still right.
            (expect (str/includes? (ex-message e) (str "exists up to " (pr-str root))))))))
  (it "reads a MISSING path handed to a namespace key as a missing path too"
      (with-project thing-test-file
                    (fn [root]
                      (let [e (try (run-capturing root {"ns" "test/com/example/nope_test.clj"})
                                   (catch clojure.lang.ExceptionInfo e e))]
                        (expect (str/includes? (ex-message e) "no such path"))))))
  ;; Regression, user report (paraphrased: a `.cljc` test file was invisible to
  ;; selection — naming the file, or the directory holding it, reported no tests
  ;; at all, though `clojure -M:test` loads it exactly like a `.clj`).
  (it "resolves a .cljc test file, and a .cljc source file to its *-test ns"
      (with-project
        {"test/com/example/cross_test.cljc" "(ns com.example.cross-test)\n"
         "src/com/example/cross.cljc" "(ns com.example.cross)\n"}
        (fn [root]
          (expect (= ["com.example.cross-test"]
                     (:nses (run-capturing root {"paths" ["test/com/example/cross_test.cljc"]}))))
          (expect (= ["com.example.cross-test"] (:nses (run-capturing root {"paths" ["test"]}))))
          (expect (= ["com.example.cross-test"]
                     (:nses (run-capturing root {"paths" ["src/com/example/cross.cljc"]})))))))
  ;; Regression, user report (paraphrased: the model kept naming a NAMESPACE, the
  ;; runner refused the call by that key's name, and the turn went into the
  ;; spelling instead of into the tests).
  (it "runs the NAMESPACE a call names, in every spelling"
      (with-project thing-test-file
                    (fn [root]
                      (doseq [k ["ns" "nses" "namespace" "namespaces"]]
                        (expect (= ["com.example.thing-test"]
                                   (:nses (run-capturing root {k "com.example.thing-test"}))))))))
  (it "takes a LIST of namespaces, and maps a SOURCE namespace to its *-test ns"
      (with-project (assoc thing-test-file "src/com/example/thing.clj" "(ns com.example.thing)\n")
                    (fn [root]
                      (expect (= ["com.example.thing-test"]
                                 (:nses (run-capturing root {"nses" ["com.example.thing"]})))))))
  (it "narrows to ONE var through the `ns/var` spelling `clojure -M:test` takes"
      (with-project thing-test-file
                    (fn [root]
                      (let [seen (run-capturing root {"ns" "com.example.thing-test/adds-test"})]
                        (expect (= ["com.example.thing-test"] (:nses seen)))
                        (expect (= [{:ns "com.example.thing-test" :name "adds-test"}]
                                   (:vars (:sel seen))))))))
  (it "reads a PATH handed to a namespace key as the path it obviously is"
      (with-project
        thing-test-file
        (fn [root]
          (expect (= ["com.example.thing-test"]
                     (:nses (run-capturing root {"ns" "test/com/example/thing_test.clj"})))))))
  (it "reads `path`, the singular spelling, alongside `paths`"
      (with-project thing-test-file
                    (fn [root]
                      (expect (= ["com.example.thing-test"]
                                 (:nses (run-capturing root {"path" "test"})))))))
  (it "roots the run at the project the named NAMESPACE lives in"
      ;; A namespace carries no location, so its own test FILE is the location:
      ;; a nested project must still be tested against its own deps.edn.
      (with-project {"deps.edn" "{}"
                     "sub/deps.edn" "{}"
                     "sub/test/com/example/thing_test.clj" "(ns com.example.thing-test)\n"}
                    (fn [root]
                      (expect (= (str root java.io.File/separator "sub")
                                 (:root (run-capturing root {"ns" "com.example.thing-test"}))))))))

;; A path may carry the TEST NAME too — `<path>::<name>`, pytest's node-id
;; grammar — so one selector says where AND which. `only` / `var` say the same
;; thing in the spelling `clojure -M:test` and lein take, and resolve the same
;; way.
(defdescribe
  clj-test-fn-node-id-test
  (it "narrows a test FILE to the one var its node id names"
      (with-project
        thing-test-file
        (fn [root]
          (let [seen (run-capturing root {"paths" ["test/com/example/thing_test.clj::adds-test"]})]
            (expect (= ["com.example.thing-test"] (:nses seen)))
            (expect (= [{:ns "com.example.thing-test" :name "adds-test"}] (:vars (:sel seen))))))))
  (it "carries a SOURCE file's node id onto the *-test namespace it resolves to"
      ;; The file half translates (thing.clj -> thing-test); the name half is
      ;; matched against LIVE vars in the repl, where `adds` also finds `adds-test`.
      (with-project (assoc thing-test-file "src/com/example/thing.clj" "(ns com.example.thing)\n")
                    (fn [root]
                      (let [seen (run-capturing root {"paths" ["src/com/example/thing.clj::adds"]})]
                        (expect (= ["com.example.thing-test"] (:nses seen)))
                        (expect (= [{:ns "com.example.thing-test" :name "adds"}]
                                   (:vars (:sel seen))))))))
  (it "runs the whole workspace narrowed by a PATHLESS id"
      ;; `::name` names no location, so it must not read as explicit-but-empty:
      ;; every test ns runs and the var filter (nil :ns) does the narrowing.
      (with-project
        (assoc thing-test-file "test/com/example/other_test.clj" "(ns com.example.other-test)\n")
        (fn [root]
          (let [seen (run-capturing root {"paths" ["::adds-test"]})]
            (expect (= ["com.example.other-test" "com.example.thing-test"] (:nses seen)))
            (expect (= [{:ns nil :name "adds-test"}] (:vars (:sel seen))))))))
  (it "pairs each name with its OWN file instead of cross-producting them"
      (with-project
        (assoc thing-test-file "test/com/example/other_test.clj" "(ns com.example.other-test)\n")
        (fn [root]
          (let [seen (run-capturing root
                                    {"paths" ["test/com/example/thing_test.clj::adds-test"
                                              "test/com/example/other_test.clj::subs-test"]})]
            (expect (= ["com.example.other-test" "com.example.thing-test"] (:nses seen)))
            (expect (= [{:ns "com.example.thing-test" :name "adds-test"}
                        {:ns "com.example.other-test" :name "subs-test"}]
                       (:vars (:sel seen))))))))
  (it "narrows by a bare `only` name, wherever it lives"
      ;; The same thing a pathless `::name` says: no location, so every test ns
      ;; runs and the var filter does the narrowing.
      (with-project
        (assoc thing-test-file "test/com/example/other_test.clj" "(ns com.example.other-test)\n")
        (fn [root]
          (let [seen (run-capturing root {"only" ["adds-test"]})]
            (expect (= ["com.example.other-test" "com.example.thing-test"] (:nses seen)))
            (expect (= [{:ns nil :name "adds-test"}] (:vars (:sel seen))))))))
  (it "pairs `only`'s `ns/var` spelling with that ONE namespace"
      (with-project
        (assoc thing-test-file "test/com/example/other_test.clj" "(ns com.example.other-test)\n")
        (fn [root]
          (let [seen (run-capturing root {"only" "com.example.thing-test/adds-test"})]
            (expect (= ["com.example.thing-test"] (:nses seen)))
            (expect (= [{:ns "com.example.thing-test" :name "adds-test"}] (:vars (:sel seen)))))))))

(defn- run-form-selecting
  "Evaluate `run-form` over a throwaway namespace holding `var-names` as
   clojure.test vars, under the RESOLVED selector map `sel`, and return its
   result map. Every var-granularity case needs the same fixture, so it is built
   once here instead of re-interned per test."
  [ns-sym var-names sel]
  (let [n
        (create-ns ns-sym)

        run-form
        @#'com.blockether.vis.internal.language.clojure.test-runner/run-form]

    (try (doseq [nm var-names]
           (alter-meta! (intern n
                                nm
                                (fn []))
                        assoc
                        :test
                        (fn [])))
         (with-redefs [clojure.core/require (fn [& _])]
           ((eval run-form) [ns-sym] sel {}))
         (finally (remove-ns ns-sym)))))

(defdescribe
  var-miss-output-test
  (it "reports a bounded selector miss without dumping namespaces or vars"
      (let [result
            (run-form-selecting 'vis.test-runner-var-miss-fixture
                                '[first-test second-test]
                                {:vars [{:ns nil :name "missing-test"}]})

            error
            (get result "error")]

        (expect
          (= "no test var matched [\"::missing-test\"] (searched 2 test vars across 1 namespace)"
             error))
        (expect (not (re-find #"first-test|second-test|vis\\.test-runner" error))))))

;; The var-level half of the path translation: `thing.clj` runs `thing-test`, and
;; one step down `::adds` runs `adds-test`. Without it a node id copied off a
;; SOURCE var would be a hard miss.
(defdescribe node-id-var-name-test
             (it "selects the *-test var when the id named the SOURCE var it covers"
                 (let [result (run-form-selecting 'vis.test-runner-node-id-fixture
                                                  '[adds-test subtracts-test]
                                                  {:vars [{:ns nil :name "adds"}]})]
                   (expect (nil? (get result "error")))
                   (expect (= 1 (get result "selected")))
                   (expect (= 1 (get result "skipped")))))
             (it "selects the test var when the id named it outright"
                 (let [result (run-form-selecting 'vis.test-runner-node-id-fixture
                                                  '[adds-test subtracts-test]
                                                  {:vars [{:ns nil :name "adds-test"}]})]
                   (expect (= 1 (get result "selected")))))
             (it "scopes the name to the namespace its path resolved to"
                 ;; A node id that named a FILE must not select the same var name in some
                 ;; other namespace — that is the cross-product `only` used to do.
                 (let [result (run-form-selecting 'vis.test-runner-node-id-fixture
                                                  '[adds-test]
                                                  {:vars [{:ns "com.example.elsewhere-test"
                                                           :name "adds-test"}]})]
                   (expect (re-find #"no test var matched" (get result "error"))))))

(def ^:private run-via-cli @#'com.blockether.vis.internal.language.clojure.test-runner/run-via-cli)

(defn- with-cli-run
  "Run the cli fallback against a canned shell result, with no project on disk."
  [{:keys [exit out err]}]
  (with-redefs [com.blockether.vis.internal.language.clojure.test-runner/cli-command-for
                (fn [_root _sel _aliases]
                  {:tool :clj :cmd ["clojure" "-M:test"]})

                tr/run-command
                (fn [& _]
                  {:exit exit :out out :err err})]

    (run-via-cli "/proj" {})))

(defdescribe
  cli-summary-counts-test
  ;; The cli path used to retell its summary line as a NOTE ("12 cases, 3
  ;; failures") and leave total / fail / errored nil, so one run read as counts
  ;; on the repl path and as prose on the cli path.
  (it "reads the shelled runner's summary line into total / fail / errored"
      (let [r (with-cli-run {:exit 1
                             :out "Ran 12 test cases in 0.4 seconds.\n1 failures, 2 errors.\n"})]
        (expect (= 12 (get r "total")))
        ;; fail is every test that did not pass ...
        (expect (= 3 (get r "fail")))
        ;; ... and errored is the subset of it that THREW
        (expect (= 2 (get r "errored")))
        (expect (nil? (get r "note")))
        (expect (false? (get r "is_pass")))))
  (it "reports a green cli run as ZERO failures, never unknown"
      (let [r (with-cli-run {:exit 0
                             :out "Ran 12 test cases in 0.4 seconds.\n0 failures, 0 errors.\n"})]
        (expect (= 12 (get r "total")))
        (expect (= 0 (get r "fail")))
        (expect (= 0 (get r "errored")))
        (expect (true? (get r "is_pass"))))))

(defdescribe
  cli-failure-diagnostics-test
  ;; Regression #184: retain the full report and every fault before any display
  ;; renderer limits its preview, including failures before noisy stdout/stderr.
  (it "retains Kaocha failure evidence preceding a noisy report"
      (let [report
            (str "FAIL in sample.core-test/broken (core_test.clj:7)\n"
                 "Expected: 1\nActual: 2\n\n"
                 (apply str (repeat 80 "ordinary log line\n"))
                 "1 tests, 1 assertions, 1 failures.\n")

            r
            (with-cli-run {:exit 1 :out report})

            fault
            (first (get r "failures"))]

        (expect (false? (get r "is_pass")))
        (expect (= 1 (get r "fail")))
        (expect (= report (get r "output")))
        (expect (= 1 (count (get r "failures"))))
        (expect (= {"ns" "sample.core-test"
                    "test" "broken"
                    "type" "fail"
                    "file" "core_test.clj"
                    "line" 7
                    "expected" "1"
                    "actual" "2"}
                   (dissoc fault "message")))
        (expect (contract/valid? :test-fn (assoc r "language" "clojure")))))
  (it
    "keeps stdout faults when stderr contains more than forty warnings"
    (let
      [out
       "FAIL in (sample.core-test/broken) (core_test.clj:7)\nexpected: 1\n  actual: 2\n\n1 tests, 1 assertions, 1 failures."

       err
       (apply str (repeat 80 "compiler warning\n"))

       r
       (with-cli-run {:exit 1 :out out :err err})]

      (expect (= (str out "\n" err) (get r "output")))
      (expect (= "broken" (get-in r ["failures" 0 "test"])))
      (expect (= "2" (get-in r ["failures" 0 "actual"])))))
  (it "collects multiple assertions and thrown errors without deduplicating tests"
      (let [trace
            (apply str (repeat 80 "    at sample.core_test.invoke(core_test.clj:19)\n"))

            report
            (str "Testing sample.core-test\n\n"
                 "FAIL in (broken) (core_test.clj:7)\nexpected: 1\n  actual: 2\n\n"
                 "FAIL in (broken) (core_test.clj:8)\nexpected: true\n  actual: false\n\n"
                 "Testing sample.other-test\n\n"
                 "ERROR in (throws) (other_test.clj:19:4)\n"
                 "Uncaught exception, not in assertion.\nexpected: nil\n"
                 "  actual: clojure.lang.ExceptionInfo: boom\n"
                 trace
                 "\nRan 2 tests containing 3 assertions.\n2 failures, 1 errors.\n")

            r
            (with-cli-run {:exit 1 :err report})

            faults
            (get r "failures")]

        (expect (= report (get r "output")))
        (expect (= 3 (get r "fail")))
        (expect (= 1 (get r "errored")))
        (expect (= ["broken" "broken" "throws"] (mapv #(get % "test") faults)))
        (expect (= ["fail" "fail" "error"] (mapv #(get % "type") faults)))
        (expect (= "sample.other-test" (get-in faults [2 "ns"])))
        (expect (= 19 (get-in faults [2 "line"])))
        (expect (str/includes? (str (get-in faults [2 "message"])) "ExceptionInfo: boom"))
        (expect (str/includes? (str (get-in faults [2 "message"])) trace))))
  (it
    "reads a real clojure.test reporter rather than only canned output"
    (let [report
          (with-out-str
            (binding [ct/*test-out*
                      *out*

                      ct/*testing-vars*
                      (list #'with-cli-run)

                      ct/*report-counters*
                      (ref ct/*initial-report-counters*)]

              (ct/report {:type :begin-test-ns
                          :ns (the-ns
                                'com.blockether.vis.internal.language.clojure.test-runner-test)})
              (ct/report {:type :fail
                          :file "core_test.clj"
                          :line 7
                          :expected '(= 1 2)
                          :actual '(not (= 1 2))})
              (ct/report {:type :error
                          :file "core_test.clj"
                          :line 9
                          :expected nil
                          :actual (ex-info "reporter boom" {})})
              (ct/report {:type :summary :test 1 :pass 0 :fail 1 :error 1})))

          r
          (with-cli-run {:exit 1 :out report})]

      (expect (= report (get r "output")))
      (expect (= ["fail" "error"] (mapv #(get % "type") (get r "failures"))))
      (expect (= ["with-cli-run" "with-cli-run"] (mapv #(get % "test") (get r "failures"))))
      (expect (str/includes? (str (get-in r ["failures" 1 "message"])) "reporter boom"))))
  (it "preserves multiline Kaocha assertions and normalizes unknown locations"
      (let [report
            (str "\u001b[31mFAIL in sample.core-test/broken (Unknown:-1)\u001b[0m\n"
                 "Expected:\n  {:value 1}\nActual:\n  - {:value 1}\n  + {:value 2}\n\n"
                 "1 tests, 1 assertions, 1 failures.\n")

            r
            (with-cli-run {:exit 1 :out report})]

        (expect (not (str/includes? (get r "output") "\u001b")))
        (expect (= "{:value 1}" (get-in r ["failures" 0 "expected"])))
        (expect (= "- {:value 1}\n  + {:value 2}" (get-in r ["failures" 0 "actual"])))
        (expect (nil? (get-in r ["failures" 0 "file"])))
        (expect (nil? (get-in r ["failures" 0 "line"])))
        (expect (contract/valid? :test-fn (assoc r "language" "clojure")))))
  (it "collects Lazytest default result blocks including exception details"
      (let [report
            (str "sample.core-test\n  broken-test\n    checks equality:\n\n"
                 "values differ\nExpected: (= 1 2)\nActual: false\n"
                 "Evaluated arguments:\n * 1\n * 2\n\nin core_test.clj:7\n\n"
                 "sample.core-test\n  throws-test\n    throws:\n\n"
                 "clojure.lang.ExceptionInfo: boom\nExpected: nil\nActual: nil\n\n"
                 "Originating error:\n    sample.core_test.invoke(core_test.clj:19)\n\n"
                 "in core_test.clj:19\n\nRan 2 test cases in 0.01 seconds.\n2 failures.\n")

            r
            (with-cli-run {:exit 1 :out report})]

        (expect (= report (get r "output")))
        (expect (= ["broken-test" "throws-test"] (mapv #(get % "test") (get r "failures"))))
        (expect (= ["fail" "error"] (mapv #(get % "type") (get r "failures"))))
        (expect (= [7 19] (mapv #(get % "line") (get r "failures"))))
        (expect (str/includes? (str (get-in r ["failures" 1 "message"])) "ExceptionInfo: boom"))))
  (it "keeps long passing and unrecognized failing reports complete"
      (doseq [[exit summary] [[0 "Ran 1 test cases.\n0 failures.\n"]
                              [1 "custom reporter: assertion failed\n"]]]
        (let [report (str "first log line\n" (apply str (repeat 100 "log line\n")) summary)
              r (with-cli-run {:exit exit :out report})]

          (expect (= report (get r "output")))
          (expect (= [] (get r "failures")))))))

(defdescribe repl-errored-count-test
             (it "counts a test that THREW into errored as well as fail"
                 (let [fixture-ns
                       'vis.test-runner-errored-fixture

                       n
                       (create-ns fixture-ns)

                       run-form
                       @#'com.blockether.vis.internal.language.clojure.test-runner/run-form]

                   (try (alter-meta! (intern n
                                             'throws-test
                                             (fn []))
                                     assoc
                                     :test
                                     (fn []
                                       (throw (ex-info "boom" {}))))
                        (with-redefs [clojure.core/require (fn [& _])]
                          (let [r ((eval run-form) [fixture-ns] {} {})]
                            (expect (= 1 (get r "fail")))
                            ;; the erroring SUBSET of fail — nothing is listed twice
                            (expect (= 1 (get r "errored")))
                            (expect (= ["error"] (mapv #(get % "type") (get r "failures"))))))
                        (finally (remove-ns fixture-ns))))))

(def ^:private ns-of-file @#'com.blockether.vis.internal.language.clojure.test-runner/ns-of-file)

(def ^:private resolve-selection
  @#'com.blockether.vis.internal.language.clojure.test-runner/resolve-selection)

(defn- temp-project!
  "Write `files` ({relative-path body}) under a fresh temp root and answer the
   root as a File."
  [files]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                        "vis-ns-of-file"
                        (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [[rel body] files]
      (let [f (io/file root rel)]
        (io/make-parents f)
        (spit f body)))
    root))

;; The real shape: a namespace that carries a clj-kondo config map, with comment
;; lines INSIDE the metadata, exactly like the sqlite store's own test namespace.
(def ^:private metadata-test-source
  (str "(ns ^{:clj-kondo/config\n"
       "      ;; Aggregator file: many blocks bind ids for side effect only.\n"
       "      '{:linters {:redundant-let {:level :off} :unused-binding {:level :off}}}}\n"
       "    vis.fixture.core-test\n"
       "  (:require [lazytest.core :refer [defdescribe expect it]]))\n"
       "\n" "(defdescribe adds-test (it \"adds\" (expect (= 2 (+ 1 1)))))\n"))

;; Regression, user report (paraphrased: the sqlite store's test namespace was
;; invisible to run_tests — its `ns` form carries a `^{:clj-kondo/config …}`
;; metadata map, so no name was read from the file, the namespace was missing
;; from the workspace index, and naming that very file selected nothing).
(defdescribe
  ns-of-file-metadata-test
  (it "reads the name through a metadata MAP on the ns symbol"
      (let [root (temp-project! {"test/vis/fixture/core_test.clj" metadata-test-source})]
        (expect (= "vis.fixture.core-test"
                   (ns-of-file (io/file root "test/vis/fixture/core_test.clj"))))))
  (it "reads the name through a metadata KEYWORD on the ns symbol"
      (let [root (temp-project! {"test/vis/fixture/plain_test.clj"
                                 "(ns ^:no-doc vis.fixture.plain-test)\n"})]
        (expect (= "vis.fixture.plain-test"
                   (ns-of-file (io/file root "test/vis/fixture/plain_test.clj"))))))
  (it "answers nil for a file that declares no namespace"
      (let [root (temp-project! {"test/vis/fixture/none_test.clj" ";; no ns here\n(def x 1)\n"})]
        (expect (nil? (ns-of-file (io/file root "test/vis/fixture/none_test.clj"))))))
  (it "SELECTS the namespace when its own test file is named"
      (let [root
            (temp-project! {"test/vis/fixture/core_test.clj" metadata-test-source})

            selected
            (resolve-selection
              (.getPath root)
              [{:path "test/vis/fixture/core_test.clj" :var nil}]
              []
              #'com.blockether.vis.internal.language.clojure.test-runner/test-source-file?)]

        (expect (= {:nses ["vis.fixture.core-test"] :vars [] :files []}
                   (dissoc selected :ns-files)))
        ;; The FILE behind each namespace is what routes the run to its runtime.
        (expect (= "core_test.clj"
                   (.getName ^java.io.File (get (:ns-files selected) "vis.fixture.core-test"))))))
  (it
    "SELECTS it from the SOURCE file it covers, metadata on both"
    (let
      [root
       (temp-project!
         {"test/vis/fixture/core_test.clj" metadata-test-source
          "src/blockether/vis/fixture/core.clj"
          "(ns ^{:clj-kondo/config '{:linters {:unused-public-var {:level :off}}}}\n    vis.fixture.core)\n"})]
      (expect
        (= ["vis.fixture.core-test"]
           (:nses
             (resolve-selection
               (.getPath root)
               [{:path "src/blockether/vis/fixture/core.clj" :var nil}]
               []
               #'com.blockether.vis.internal.language.clojure.test-runner/test-source-file?))))))
  (it "finds a namespace declared after a leading form"
      (let [root (temp-project!
                   {"test/vis/fixture/late_test.clj"
                    "(comment \"a note before the ns\")\n(ns vis.fixture.late-test)\n"})]
        (expect (= "vis.fixture.late-test"
                   (ns-of-file (io/file root "test/vis/fixture/late_test.clj")))))))

;; ── ClojureScript: the shadow-cljs build IS the runtime ──────────────────────
;; Regression, issue #150 (paraphrased: a project whose tests are all
;; `*_test.cljs` under a shadow-cljs build was answered "found no test
;; namespaces (*_test.clj / *_test.cljc) under <the very test file the caller
;; named>" — only JVM extensions were indexed, so a correctly named
;; ClojureScript test could not be SELECTED at all, let alone run).

(def ^:private cljs-project
  "The issue's shape: a shadow-cljs.edn with a :node-test build, an
   npm-installed shadow-cljs binary, and a test that only ever loads in
   ClojureScript."
  {"shadow-cljs.edn"
   (str "{:source-paths [\"src\" \"test\"]\n"
        " :builds {:test {:target :node-test :output-to \"target/node-tests.js\"}}}\n")
   "package.json" "{\"devDependencies\": {\"shadow-cljs\": \"2.28.20\"}}\n"
   "node_modules/.bin/shadow-cljs" "#!/bin/sh\n"
   "test/repro/core_test.cljs" "(ns repro.core-test)\n"})

(defn- run-capturing-cljs
  "Run `clj-test-fn` with EVERY runner stubbed, answering `{:root :nses :build}`
   from the shadow-cljs runner and `{:cli-root :cli-nses}` from the JVM one — so
   a case that took the WRONG runtime is caught by which key appeared."
  [ws arg]
  (let [seen (atom {})]
    (with-redefs [repl-manager/live-repl-for-dir (fn [_sid _root]
                                                   nil)
                  com.blockether.vis.internal.language.clojure.test-runner/run-via-cli
                  (fn [root norm]
                    (swap! seen assoc :cli-root root :cli-nses (:nses norm))
                    {"mode" "cli" "ns" (first (:nses norm))})
                  com.blockether.vis.internal.language.clojure.test-runner/run-via-shadow
                  (fn [root nses norm]
                    (swap! seen assoc :root root :nses nses :build (:build norm))
                    {"mode" "cli" "tool" "shadow-cljs" "ns" (first nses)})]

      (tr/clj-test-fn {:workspace/root ws :session-id "sid"} arg)
      @seen)))

(defdescribe
  clj-test-fn-cljs-dispatch-test
  (it "runs a *_test.cljs the caller named, instead of refusing it as no test at all"
      (with-project cljs-project
                    (fn [root]
                      (let [seen (run-capturing-cljs root {"paths" ["test/repro/core_test.cljs"]})]
                        (expect (= ["repro.core-test"] (:nses seen)))
                        (expect (= root (:root seen)))
                        (expect (nil? (:cli-nses seen)))))))
  (it "selects a ClojureScript namespace BY NAME, the same as a JVM one"
      (with-project cljs-project
                    (fn [root]
                      (expect (= ["repro.core-test"]
                                 (:nses (run-capturing-cljs root {"ns" "repro.core-test"})))))))
  (it "walks a directory into the ClojureScript tests under it"
      (with-project cljs-project
                    (fn [root]
                      (expect (= ["repro.core-test"]
                                 (:nses (run-capturing-cljs root {"paths" ["test"]})))))))
  (it "runs every ClojureScript namespace when no location is named"
      (with-project cljs-project
                    (fn [root]
                      (expect (= ["repro.core-test"] (:nses (run-capturing-cljs root {})))))))
  (it "passes an explicit build through to the runner"
      (with-project
        cljs-project
        (fn [root]
          (expect (= "ci-test"
                     (:build (run-capturing-cljs root {"paths" ["test"] "build" "ci-test"})))))))
  (it "refuses a mixed JVM/JS selection instead of silently dropping tests"
      (with-project (assoc cljs-project "test/repro/jvm_test.clj" "(ns repro.jvm-test)\n")
                    (fn [root]
                      (let [e (try (run-capturing-cljs root {"paths" ["test"]})
                                   (catch clojure.lang.ExceptionInfo e e))]
                        (expect (instance? clojure.lang.ExceptionInfo e))
                        (expect (str/includes? (ex-message e) "JVM and ClojureScript"))))))
  (it "runs the issue's own call: a `cwd` project plus a path relative to it"
      (with-project
        cljs-project
        (fn [root]
          (let [seen (run-capturing-cljs "/ws" {"cwd" root "paths" ["test/repro/core_test.cljs"]})]
            (expect (= ["repro.core-test"] (:nses seen)))
            (expect (= root (:root seen)))))))
  (it "roots the run at the NESTED shadow-cljs.edn, not at the workspace root"
      ;; A shadow project is usually npm-only: the deps.edn / project.clj search
      ;; would climb straight past it to a parent that cannot run the tests.
      (with-project (into {}
                          (map (fn [[rel body]]
                                 [(str "repositories/app/" rel) body]))
                          cljs-project)
                    (fn [root]
                      (let [seen (run-capturing-cljs
                                   root
                                   {"paths" ["repositories/app/test/repro/core_test.cljs"]})]
                        (expect (= (str root "/repositories/app") (:root seen))))))))

(def ^:private run-via-shadow
  @#'com.blockether.vis.internal.language.clojure.test-runner/run-via-shadow)

(defn- sh-answering
  "Record commands and answer the execution result. Shadow compile steps get a
   successful compiler report; tests of compiler failure stub tr/run-command directly."
  [calls exit out]
  (fn [_dir args _deadline]
    (swap! calls conj (vec (take-while string? args)))
    (if-let [build (second (drop-while #(not= "compile" %) args))]
      {:exit 0 :out (str "[:" build "] Build completed. (1 files)\n") :err ""}
      {:exit exit
       :out (if (= "node" (first args)) (str "Testing repro.core-test\n" out) out)
       :err ""})))

(defdescribe shadow-failure-diagnostics-test
             ;; Regression #184 also applied to shadow-cljs via the shared CLI tail.
             (it "keeps compilation warnings and complete Node failure diagnostics"
                 (let [compile-out
                       (str "WARNING: A terminally deprecated method was called\n"
                            "[:test] Build completed. (1 files)\n")

                       test-out
                       (str "Testing repro.core-test\n\n"
                            "FAIL in (broken) (core_test.cljs:7:2)\n"
                            "expected: 1\n  actual: 2\n\n"
                            (apply str (repeat 80 "ordinary log line\n"))
                            "Ran 1 tests containing 1 assertions.\n1 failures, 0 errors.\n")

                       err
                       (apply str (repeat 80 "compiler warning\n"))]

                   (with-project
                     cljs-project
                     (fn [root]
                       (with-redefs [tr/run-command (fn [_dir args _deadline]
                                                      (if (= "node" (first args))
                                                        {:exit 1 :out test-out :err err}
                                                        {:exit 0 :out compile-out :err ""}))]
                         (let [r (run-via-shadow root ["repro.core-test"] {})]
                           (expect (false? (get r "is_pass")))
                           (expect (= (str compile-out test-out err) (get r "output")))
                           (expect (= "repro.core-test" (get-in r ["failures" 0 "ns"])))
                           (expect (= "broken" (get-in r ["failures" 0 "test"])))
                           (expect (= "core_test.cljs" (get-in r ["failures" 0 "file"])))
                           (expect (= 7 (get-in r ["failures" 0 "line"]))))))))))

(defdescribe
  run-via-shadow-verdict-test
  "shadow-cljs exits ZERO whether its tests passed, failed, or never ran — it
   exits zero even after printing its help text for an argument it rejected. The
   verdict therefore comes from the printed COUNTS, and a run with no counts at
   all is reported as not passing rather than as a green suite."
  (it "reports the counts of a clean run"
      (let [calls (atom [])]
        (with-redefs [tr/run-command
                      (sh-answering
                        calls
                        0
                        "Ran 3 tests containing 5 assertions.\n0 failures, 0 errors.\n")]
          (with-project cljs-project
                        (fn [root]
                          (let [r (run-via-shadow root ["repro.core-test"] {})]
                            (expect (true? (get r "is_pass")))
                            (expect (= 3 (get r "total")))
                            (expect (= 0 (get r "fail")))
                            (expect (= "test" (get r "build")))
                            (expect (= "shadow-cljs" (get r "tool")))
                            (expect (= 2 (count @calls)))))))))
  (it "FAILS a run whose tests failed, even though shadow-cljs exited 0"
      (let [calls (atom [])]
        (with-redefs [tr/run-command
                      (sh-answering
                        calls
                        0
                        "Ran 3 tests containing 5 assertions.\n2 failures, 1 errors.\n")]
          (with-project cljs-project
                        (fn [root]
                          (let [r (run-via-shadow root ["repro.core-test"] {})]
                            (expect (false? (get r "is_pass")))
                            (expect (= 3 (get r "total")))
                            (expect (= 3 (get r "fail")))
                            (expect (= 1 (get r "errored")))))))))
  (it "FAILS a run that printed no summary at all, rather than calling it green"
      (let [calls (atom [])]
        (with-redefs [tr/run-command
                      (sh-answering calls 0 "shadow-cljs - HELP\n  compile <build>\n")]
          (with-project cljs-project
                        (fn [root]
                          (let [r (run-via-shadow root ["repro.core-test"] {})]
                            (expect (false? (get r "is_pass")))
                            (expect (str/includes? (get r "error") "nothing verified"))))))))
  (it "names the build when the selected namespaces compiled but ran nothing"
      (let [calls (atom [])]
        (with-redefs [tr/run-command
                      (sh-answering
                        calls
                        0
                        "Ran 0 tests containing 0 assertions.\n0 failures, 0 errors.\n")]
          (with-project cljs-project
                        (fn [root]
                          (let [r (run-via-shadow root ["repro.core-test"] {})]
                            (expect (false? (get r "is_pass")))
                            (expect (str/includes? (get r "error") "classpath"))))))))
  (it "reports a project it cannot run as data, and shells nothing"
      (let [calls (atom [])]
        (with-redefs [tr/run-command (sh-answering calls 0 "")]
          (with-project {"test/repro/core_test.cljs" "(ns repro.core-test)\n"}
                        (fn [root]
                          (let [r (run-via-shadow root ["repro.core-test"] {})]
                            (expect (false? (get r "is_pass")))
                            (expect (str/includes? (get r "error") "no shadow-cljs.edn"))
                            (expect (empty? @calls)))))))))

(defdescribe
  shadow-cljs-command-test
  "Which command runs the tests: how shadow-cljs is INSTALLED here, which build
   is the test build, and the `--config-merge` that both runs it and narrows it."
  (it "compiles the focused node-test build and executes Node with its own exit status"
      (with-project cljs-project
                    (fn [root]
                      (let [{:keys [steps build target kind]}
                            (shadow/run-steps root {:nses ["repro.core-test"]})

                            argv
                            (vec (:argv (first steps)))]

                        (expect (= ["test" :node-test :npm] [build target kind]))
                        (expect (= 2 (count steps)))
                        (expect (str/ends-with? (first argv) "node_modules/.bin/shadow-cljs"))
                        (expect (= ["compile" "test" "--config-merge"] (subvec argv 1 4)))
                        ;; Autorun loses Node's exit status. Run Node separately and print
                        ;; the regexp with pr-str so shadow reads legal EDN, not CLI help.
                        (expect (= "{:autorun false, :ns-regexp \"^(repro\\\\.core-test)$\"}"
                                   (peek argv)))))))
  (it "leaves namespace discovery to the build when no namespace was selected"
      (with-project cljs-project
                    (fn [root]
                      (expect (= "{:autorun false}"
                                 (peek (vec (:argv (first (:steps (shadow/run-steps root
                                                                                    {})))))))))))
  (it "anchors the regexp so a namespace never drags in its longer neighbours"
      (expect (= "^(a\\.b-test|a\\.b-test-helpers)$"
                 (shadow/ns-regexp ["a.b-test-helpers" "a.b-test"]))))
  (it "runs the CLI through the deps.edn ALIAS that carries shadow-cljs"
      ;; Same project, other install: shadow-cljs as a tool dependency. The alias
      ;; has to reach the command line or `-m shadow.cljs.devtools.cli` resolves
      ;; nothing.
      (with-project
        (->
          cljs-project
          (dissoc "node_modules/.bin/shadow-cljs" "package.json")
          (assoc
            "deps.edn"
            "{:aliases {:cljs {:extra-deps {thheller/shadow-cljs {:mvn/version \"2.28.20\"}}}}}\n"))
        (fn [root]
          (expect (= ["clojure" "-M:cljs" "-m" "shadow.cljs.devtools.cli" "compile" "test"]
                     (vec (take 6 (:argv (first (:steps (shadow/run-steps root {})))))))))))
  (it "runs the CLI plainly when the project's own :deps carry shadow-cljs"
      (with-project
        (-> cljs-project
            (dissoc "node_modules/.bin/shadow-cljs" "package.json")
            (assoc "deps.edn" "{:deps {thheller/shadow-cljs {:mvn/version \"2.28.20\"}}}\n"))
        (fn [root]
          (expect (= ["clojure" "-M" "-m" "shadow.cljs.devtools.cli"]
                     (vec (take 4 (:argv (first (:steps (shadow/run-steps root {})))))))))))
  (it "answers a declared-but-uninstalled shadow-cljs with the install, not with 'no runner'"
      (with-project (dissoc cljs-project "node_modules/.bin/shadow-cljs")
                    (fn [root]
                      (expect (str/includes? (:error (shadow/run-steps root {})) "npm install")))))
  (it "names the builds it had when none of them runs tests"
      (with-project (assoc cljs-project
                      "shadow-cljs.edn"
                      "{:builds {:app {:target :node-script :output-to \"target/app.js\"}}}\n")
                    (fn [root]
                      (let [error (:error (shadow/run-steps root {}))]
                        (expect (str/includes? error "declares no test build"))
                        (expect (str/includes? error ":app (:node-script)"))))))
  (it "names the builds it had when the requested one is not among them"
      (with-project cljs-project
                    (fn [root]
                      (expect (str/includes? (:error (shadow/run-steps root {:build "nope"}))
                                             "has no build :nope")))))
  (it "refuses a :browser-test build BEFORE anything about the install"
      ;; Its tests run inside a browser that connects back to the build, so
      ;; compiling it asserts nothing — and installing shadow-cljs would not
      ;; change that, which is why the browser fact is the one reported.
      (with-project (assoc (dissoc cljs-project "node_modules/.bin/shadow-cljs")
                      "shadow-cljs.edn"
                      "{:builds {:ui-test {:target :browser-test :test-dir \"public/test\"}}}\n")
                    (fn [root]
                      (let [error (:error (shadow/run-steps root {}))]
                        (expect (str/includes? error ":browser-test"))
                        (expect (str/includes? error "shadow-cljs watch ui-test"))
                        (expect (not (str/includes? error "npm install")))))))
  (it "compiles a :karma build and then runs karma once"
      (with-project
        (assoc cljs-project
          "shadow-cljs.edn"
          "{:builds {:karma-test {:target :karma :output-to \"target/karma.js\"}}}\n"
          "node_modules/.bin/karma" "#!/bin/sh\n")
        (fn [root]
          (let [steps (:steps (shadow/run-steps root {}))]
            (expect (= 2 (count steps)))
            ;; karma runs the compiled output itself, so :autorun would be wrong.
            (expect (not (str/includes? (str/join " " (:argv (first steps))) ":autorun")))
            (expect (= ["start" "--single-run"] (vec (rest (:argv (second steps))))))))))
  (it "refuses a :karma build whose karma binary is not installed"
      (with-project (assoc cljs-project
                      "shadow-cljs.edn"
                      "{:builds {:karma-test {:target :karma :output-to \"target/karma.js\"}}}\n")
                    (fn [root]
                      (expect (str/includes? (:error (shadow/run-steps root {}))
                                             "node_modules/.bin/karma is missing")))))
  (it
    "reads a config that carries a reader tag it does not know"
    ;; `#shadow/env` is ordinary in a real shadow-cljs.edn; losing the whole
    ;; run to an unknown tag would be a worse answer than any of the above.
    (with-project
      (assoc cljs-project
        "shadow-cljs.edn"
        "{:builds {:test {:target :node-test :output-to \"target/node-tests.js\" :closure-defines {api #shadow/env \"API\"}}}}\n")
      (fn [root]
        (expect (= "test" (:build (shadow/run-steps root {}))))))))

(def ^:private normalize-arg
  @#'com.blockether.vis.internal.language.clojure.test-runner/normalize-arg)

(def ^:private cli-command-for
  @#'com.blockether.vis.internal.language.clojure.test-runner/cli-command-for)

(def ^:private note-unapplied-aliases
  @#'com.blockether.vis.internal.language.clojure.test-runner/note-unapplied-aliases)

;; Issue #157: a classpath alias is not an executable runner, and selecting
;; a runner must not discard namespace/var focus.
(defdescribe
  executable-runner-discovery-test
  (it "discovers Kaocha by its entry point, not an alias name"
      (with-project {"deps.edn" (pr-str {:aliases {:test {:extra-paths ["test"]}
                                                   :verify {:main-opts ["-m" "kaocha.runner"]}}})}
                    (fn [root]
                      (let [picked (cli-command-for root {:nses ["sample.core-test"]} [])]
                        (expect (= ["-M:test:verify" "--focus" "sample.core-test"]
                                   (vec (take-last 3 (:cmd picked)))))))))
  (it "keeps focus and reports selected tests with explicit runner aliases"
      (with-project
        {"deps.edn" (pr-str {:aliases {:test {:extra-paths ["test"]}
                                       :runner {:main-opts ["-m" "kaocha.runner"]}}})}
        (fn [root]
          (let [calls (atom [])]
            (with-redefs [tr/run-command
                          (sh-answering calls 0 "3 tests, 5 assertions, 0 failures, 0 errors.\n")]
              (let [r (run-via-cli root {:nses ["sample.core-test"] :aliases ["runner"]})]
                (expect (str/includes? (get r "command") "--focus sample.core-test"))
                (expect (true? (get r "is_pass")))
                (expect (= 3 (get r "selected")))))))))
  (it "refuses a non-executable alias before opening a REPL"
      (with-project {"deps.edn" "{:aliases {:test {:extra-paths [\"test\"]}}}"}
                    (fn [root]
                      (let [calls (atom [])]
                        (with-redefs [tr/run-command
                                      (sh-answering calls 0 "Clojure 1.12\nuser=>\n")]
                          (expect (string? (get (run-via-cli root {}) "error")))
                          (expect (empty? @calls)))))))
  (it "refuses unsupported focus before launching a custom runner"
      (with-project
        {"deps.edn" "{:aliases {:test {:main-opts [\"-m\" \"custom.runner\"]}}}"}
        (fn [root]
          (let [calls (atom [])]
            (with-redefs [tr/run-command
                          (sh-answering
                            calls
                            0
                            "Ran 100 tests containing 100 assertions.\n0 failures, 0 errors.\n")]
              (expect (string? (get (run-via-cli root {:nses ["sample.core-test"]}) "error")))
              (expect (empty? @calls)))))))
  (it "does not call a zero-test focused run a pass"
      (with-project {"deps.edn" "{:aliases {:test {:main-opts [\"-m\" \"lazytest.main\"]}}}"}
                    (fn [root]
                      (with-redefs [tr/run-command
                                    (sh-answering (atom []) 0 "Ran 0 test cases.\n0 failures.\n")]
                        (expect (false? (get (run-via-cli root {:nses ["missing.ns-test"]})
                                             "is_pass"))))))))

(defdescribe
  runner-configuration-matrix-test
  (it
    "supports arbitrary runner names and Kaocha exec-fn selectors"
    (doseq [[aliases supplied expected] [[{:checks {:main-opts ["-m" "lazytest.main"]}} []
                                          ["-M:checks" "--namespace" "sample.core-test"]]
                                         [{:checks {:exec-fn 'kaocha.runner/exec-fn}} []
                                          ["-X:checks" ":kaocha.filter/focus" "[sample.core-test]"]]
                                         [{:test {:main-opts ["-m" "lazytest.main"]}
                                           :verify {:main-opts ["-m" "kaocha.runner"]}} ["verify"]
                                          ["-M:test:verify" "--focus" "sample.core-test"]]]]
      (with-project {"deps.edn" (pr-str {:aliases aliases})}
                    (fn [root]
                      (expect (= expected
                                 (vec (take-last 3
                                                 (:cmd (cli-command-for root
                                                                        {:nses ["sample.core-test"]}
                                                                        supplied))))))))))
  (it "refuses ambiguous, unknown, cancelled and unrelated entry points"
      (doseq [[aliases supplied]
              [[{:one {:main-opts ["-m" "kaocha.runner"]} :two {:main-opts ["-m" "lazytest.main"]}}
                []] [{:test {:main-opts ["-m" "lazytest.main"]}} ["missing"]]
               [{:test {:main-opts ["-m" "lazytest.main"]} :empty {:main-opts []}} ["empty"]]
               [{:test {:extra-paths ["test"]} :deploy {:exec-fn 'app/deploy!}} []]]]
        (with-project {"deps.edn" (pr-str {:aliases aliases})}
                      (fn [root]
                        (expect (string? (:error (cli-command-for root {} supplied))))))))
  (it "translates vars and metadata using Kaocha's vocabulary"
      (with-project
        {"deps.edn" "{:aliases {:check {:main-opts [\"-m\" \"kaocha.runner\"]}}}"}
        (fn [root]
          (expect (= ["--focus" "sample.core-test/adds-test"]
                     (vec (take-last 2
                                     (:cmd (cli-command-for root
                                                            {:nses ["sample.core-test"]
                                                             :vars [{:ns "sample.core-test"
                                                                     :name "adds-test"}]}
                                                            []))))))
          (expect (= ["--focus-meta" "slow"]
                     (vec (take-last 2 (:cmd (cli-command-for root {:include ["slow"]} []))))))
          (expect (string? (:error (cli-command-for root
                                                    {:nses ["sample.core-test"] :include ["slow"]}
                                                    [])))))))
  (it
    "preserves focused path selection across the public language-pack boundary"
    (with-project
      (assoc thing-test-file
        "deps.edn"
        "{:aliases {:test {:extra-paths [\"test\"]} :runner {:main-opts [\"-m\" \"kaocha.runner\"]}}}")
      (fn [root]
        (let [calls (atom [])]
          (with-redefs [repl-manager/live-repl-for-dir (constantly nil)
                        tr/run-command
                        (sh-answering calls 0 "1 tests, 1 assertions, 0 failures.\n")]

            (tr/clj-test-fn {:workspace/root root}
                            {"paths" ["test/com/example/thing_test.clj"] "aliases" ["runner"]})
            (expect (some #{"--focus"} (first @calls)))
            (expect (some #{"com.example.thing-test"} (first @calls)))))))))

(defdescribe
  generated-test-copy-selection-test
  (it "ignores target copies for file, directory, source and namespace selectors"
      (with-project
        {"deps.edn"
         "{:aliases {:test {:extra-paths [\"test\"] :main-opts [\"-m\" \"lazytest.main\"]}}}"
         "src/sample/core.clj" "(ns sample.core)"
         "test/sample/core_test.clj" "(ns sample.core-test)"
         "target/copy/test/sample/core_test.clj" "(ns sample.core-test)"}
        (fn [root]
          (doseq [selector [{"path" "test/sample/core_test.clj"}
                            {"path" "target/copy/test/sample/core_test.clj"} {"path" "target/copy"}
                            {"path" "."} {"path" "src/sample/core.clj"} {"ns" "sample.core-test"}
                            {"ns" "sample.core-test/adds-test"}]]
            (let [calls (atom [])]
              (with-redefs [repl-manager/live-repl-for-dir (constantly nil)
                            tr/run-command
                            (sh-answering calls 0 "Ran 1 test cases.\n0 failures.\n")]

                (let [result (:result (tr/clj-test-fn {:workspace/root root} selector))]
                  (expect (= {"is_pass" true} (select-keys result ["is_pass" "error"])))
                  (expect (= 1 (count @calls)))
                  (expect (some
                            #{(if (contains? selector "ns") (get selector "ns") "sample.core-test")}
                            (first @calls)))))))))))

;; Regression: session 633cdc58-89fe-4b3d-88ec-caa624d118ed indexed a hidden
;; worktree as part of the workspace, blocking even a file plus namespace selector.
(defdescribe
  hidden-test-copy-selection-test
  (it "ignores hidden copies and their extra tests during automatic discovery"
      (doseq [copy [".gitworktrees/council" ".worktrees/feature" "test/.cache"]]
        (with-project
          (assoc thing-test-file
            "src/com/example/thing.clj" "(ns com.example.thing)\n"
            (str copy "/test/com/example/thing_test.clj") "(ns com.example.thing-test)\n"
            (str copy "/test/com/example/hidden_test.clj") "(ns com.example.hidden-test)\n")
          (fn [root]
            (doseq [selector [{} {"path" "."} {"path" "test"}
                              {"path" "test/com/example/thing_test.clj"}
                              {"path" "src/com/example/thing.clj"} {"ns" "com.example.thing-test"}
                              {"ns" "com.example.thing-test/adds-test"}
                              {"path" "test/com/example/thing_test.clj"
                               "ns" "com.example.thing-test/adds-test"}]]
              (expect (= ["com.example.thing-test"] (:nses (run-capturing root selector)))
                      (pr-str selector)))))))
  (it "accepts an explicitly selected hidden worktree as a project, file or directory"
      (with-project
        (assoc thing-test-file
          "deps.edn" "{}"
          ".gitworktrees/council/deps.edn" "{}"
          ".gitworktrees/council/src/com/example/thing.clj" "(ns com.example.thing)\n"
          ".gitworktrees/council/test/com/example/thing_test.clj" "(ns com.example.thing-test)\n")
        (fn [root]
          (doseq [selector [{"cwd" ".gitworktrees/council"}
                            {"cwd" ".gitworktrees/council" "path" "src/com/example/thing.clj"}
                            {"path" ".gitworktrees/council"}
                            {"path" ".gitworktrees/council/test/com/example/thing_test.clj"}]]
            (let [seen (run-capturing root selector)]
              (expect (= ["com.example.thing-test"] (:nses seen)))
              (expect (= (.getPath (io/file root ".gitworktrees/council")) (:root seen))))))))
  (it "does not load hidden worktree tests through the REPL source fallback"
      (with-project {".gitworktrees/council/test/com/example/hidden_test.clj"
                     "(ns com.example.hidden-test)\n"}
                    (fn [root]
                      (expect (nil? (#'tr/test-file-for root "com.example.hidden-test")))
                      (expect (string? (#'tr/test-file-for
                                        (io/file root ".gitworktrees/council")
                                        "com.example.hidden-test"))))))
  (it "still rejects genuinely duplicated test namespaces in selected source trees"
      (with-project
        (assoc thing-test-file
          "other/test/com/example/thing_test.clj" "(ns com.example.thing-test)\n")
        (fn [root]
          (doseq [selector [{} {"path" "."} {"ns" "com.example.thing-test"}]]
            (let [error
                  (try (run-capturing root selector) nil (catch clojure.lang.ExceptionInfo e e))]
              (expect (= :clj/bad-args (:type (ex-data error))))
              (expect (str/includes? (or (ex-message error) "") "ambiguous test namespace"))))))))

(defdescribe
  cljs-configuration-selection-test
  (it
    "uses configured namespace patterns instead of requiring a _test filename"
    (with-project
      (assoc (dissoc cljs-project "test/repro/core_test.cljs")
        "shadow-cljs.edn"
        "{:source-paths [\"spec\"] :builds {:checks {:target :node-test :ns-regexp \"-spec$\" :output-to \"out/checks.js\"}}}"
        "spec/sample/core_spec.cljs" "(ns sample.core-spec)\n")
      (fn [root]
        (doseq [arg [{"path" "spec/sample/core_spec.cljs"} {"path" "spec"}
                     {"ns" "sample.core-spec"}]]
          (expect (= ["sample.core-spec"] (:nses (run-capturing-cljs root arg))))))))
  (it "runs shared cljc tests in JS alongside cljs, or when build is explicit"
      (with-project (assoc cljs-project "test/repro/shared_test.cljc" "(ns repro.shared-test)\n")
                    (fn [root]
                      (expect (= ["repro.core-test" "repro.shared-test"]
                                 (:nses (run-capturing-cljs root {"path" "test"}))))
                      (expect (= ["repro.shared-test"]
                                 (:nses (run-capturing-cljs root
                                                            {"path" "test/repro/shared_test.cljc"
                                                             "build" "test"})))))))
  (it "lets an unfiltered build run its configured namespaces without local filename discovery"
      (with-project (dissoc cljs-project "test/repro/core_test.cljs")
                    (fn [root]
                      (expect (= root (:root (run-capturing-cljs root {"build" "test"})))))))
  (it "refuses var/tag/alias selectors not implemented by the Vis adapter, before spawning"
      (with-project
        cljs-project
        (fn [root]
          (doseq [norm [{:vars [{:ns "repro.core-test" :name "one-test"}]} {:include ["slow"]}
                        {:exclude ["slow"]} {:aliases ["frontend"]}]]
            (let [calls (atom [])]
              (with-redefs [tr/run-command
                            (sh-answering calls 0 "Ran 1 tests.\n0 failures, 0 errors.\n")]
                (expect (string? (get (run-via-shadow root ["repro.core-test"] norm) "error")))
                (expect (empty? @calls)))))))))

(defdescribe
  shadow-build-shapes-test
  (it "accepts shadow's vector build configuration"
      (with-project (assoc cljs-project
                      "shadow-cljs.edn"
                      "{:builds [{:id :checks :target :node-test :output-to \"out/checks.js\"}]}")
                    (fn [root]
                      (expect (= "checks" (:build (shadow/run-steps root {})))))))
  (it
    "does not broaden focus when :namespaces overrides :ns-regexp"
    (with-project
      (assoc cljs-project
        "shadow-cljs.edn"
        "{:builds {:checks {:target :node-test :namespaces [repro.core-test repro.other-test] :output-to \"out/checks.js\"}}}")
      (fn [root]
        (expect (string? (:error (shadow/run-steps root {:nses ["repro.core-test"]})))))))
  (it "reads Karma's own completed summary, not an imagined cljs.test summary"
      (with-project
        (assoc cljs-project
          "shadow-cljs.edn" "{:builds {:checks {:target :karma :output-to \"out/checks.js\"}}}"
          "node_modules/.bin/karma" "#!/bin/sh\n")
        (fn [root]
          (doseq [[out pass? n faults]
                  [["HeadlessChrome: Executed 2 of 2 SUCCESS (0.01 secs / 0.01 secs)\n" true 2 0]
                   ["TOTAL: 1 FAILED, 2 SUCCESS\n" false 3 1]
                   ;; Issue #157: a later browser/batch must not erase an earlier failure.
                   ["Chrome: Executed 1 of 1 (1 FAILED)\nFirefox: Executed 1 of 1 SUCCESS\n" false 2
                    1] ["TOTAL: 1 FAILED, 0 SUCCESS\nTOTAL: 1 SUCCESS\n" false 2 1]
                   ["Chrome: Executed 1 of 2 SUCCESS\nFirefox: Executed 1 of 1 SUCCESS\n" false nil
                    nil] ["Chrome: Executed 1 of 1 (1 FAILED)\nTOTAL: 1 SUCCESS\n" false nil nil]]]
            (with-redefs [tr/run-command (sh-answering (atom []) 0 out)]
              (let [r (run-via-shadow root ["repro.core-test"] {})]
                (expect (= pass? (get r "is_pass")))
                (expect (= n (get r "selected")))
                (expect (= faults (get r "fail"))))))))))

;; The shadow-cljs user guide specifies that :deps/:lein own the classpath,
;; and that :node-test/:karma/:browser-test are different JS runtimes.
(defdescribe
  shadow-configuration-regression-test
  (it "keeps every configured deps alias, even when shadow is a root dependency"
      (with-project {"shadow-cljs.edn" "{:deps {:aliases [:frontend :checks]} :builds {}}"
                     "deps.edn" (pr-str {:deps {'thheller/shadow-cljs {:mvn/version "2.28.20"}}
                                         :aliases {:frontend {:extra-paths ["src"]}
                                                   :checks {:extra-paths ["test"]}}})}
                    (fn [root]
                      (expect (= ["clojure" "-M:frontend:checks" "-m" "shadow.cljs.devtools.cli"]
                                 (:argv (shadow/launcher root)))))))
  (it "honors :lein instead of an unrelated deps.edn"
      (with-project {"shadow-cljs.edn" "{:lein {:profile \"+frontend\"} :builds {}}"
                     "project.clj" "(defproject sample \"0.1.0\")"
                     "deps.edn" "{:deps {thheller/shadow-cljs {:mvn/version \"2.28.20\"}}}"}
                    (fn [root]
                      (expect (= ["lein" "with-profile" "+frontend" "run" "-m"
                                  "shadow.cljs.devtools.cli"]
                                 (:argv (shadow/launcher root)))))))
  (it "does not guess between independently configured test builds"
      (expect (string? (:error (shadow/test-build {:builds {:unit {:target :node-test}
                                                            :integration {:target :node-test}}}
                                                  nil)))))
  (it "escapes regex metacharacters in namespace focus"
      (let [pattern (re-pattern (shadow/ns-regexp ["sample.price$-test"]))]
        (expect (re-matches pattern "sample.price$-test"))
        (expect (nil? (re-matches pattern "sample.price-test"))))))

;; A project whose tests need more than its `:test` alias declares had no way to
;; say so: run_tests shelled a hardcoded `clojure -M:test`, and `aliases` existed
;; only on repl_start.
(defdescribe
  run-tests-aliases-test
  (it "passes selectors through the upstream Lazytest CLI"
      (let [root
            (temp-project! {"deps.edn" (pr-str {:aliases {:test {:main-opts ["-m"
                                                                             "lazytest.main"]}}})})

            picked
            (cli-command-for (.getPath root) {:nses ['suite.a]} [])]

        (expect (:selectors? picked))
        (expect (= ["--namespace" "suite.a"] (vec (take-last 2 (:cmd picked)))))))
  (it "reads `aliases` as alias NAMES — colon or not, scalar or list"
      (expect (= ["bench"] (:aliases (normalize-arg {"aliases" "bench"}))))
      (expect (= ["bench"] (:aliases (normalize-arg {"aliases" ":bench"}))))
      (expect (= ["dev" "bench"] (:aliases (normalize-arg {"aliases" [":dev" "bench"]}))))
      (expect (= [] (:aliases (normalize-arg {"paths" ["test"]})))))
  (it "appends classpath aliases to a declared executable :test"
      (let [root
            (temp-project! {"deps.edn" (pr-str {:aliases {:test {:main-opts ["-m" "lazytest.main"]}
                                                          :bench {}
                                                          :dev {}}})})

            picked
            (cli-command-for (.getPath root) {} ["bench" "dev"])]

        (expect (= :clj (:tool picked)))
        (expect (some #{"-M:test:bench:dev"} (:cmd picked)))
        ;; The declared :test remains the runner when extras only add classpath.
        (expect (not (some #{"-M:bench:dev"} (:cmd picked))))))
  (it "leaves the command alone when no alias was asked for"
      (let [root (temp-project! {"deps.edn" (pr-str {:aliases {:test {:main-opts
                                                                      ["-m" "lazytest.main"]}}})})]
        (expect (some #{"-M:test"} (:cmd (cli-command-for (.getPath root) {} []))))))
  (it "refuses deps.edn aliases for a lein / bb project instead of running without them"
      (with-redefs [com.blockether.vis.internal.language.clojure.test-runner/cli-command-for
                    (fn [_root _sel _aliases]
                      {:tool :lein :cmd ["lein" "test"]})]
        (let [r (run-via-cli "/proj" {:nses ["a.core-test"] :aliases ["bench"]})]
          (expect (nil? (get r "total")))
          (expect (str/includes? (get r "error") "bench"))
          (expect (str/includes? (get r "error") "lein")))))
  (it "says the aliases did NOT apply when the run reused a REPL"
      (let [r (note-unapplied-aliases ["bench"] {"mode" "repl" "is_pass" true})]
        (expect (str/includes? (get r "note") "did NOT apply"))
        (expect (str/includes? (get r "note") "repl_stop"))))
  (it "stays silent on the clean-JVM path, and keeps a note it found"
      (expect (= {"mode" "cli"} (note-unapplied-aliases ["bench"] {"mode" "cli"})))
      (expect (= {"mode" "repl"} (note-unapplied-aliases [] {"mode" "repl"})))
      (expect (str/includes? (get (note-unapplied-aliases ["bench"] {"mode" "shadow" "note" "kept"})
                                  "note")
                             "kept")))
  ;; The seam that matters: the CALL's key must survive normalize-arg, the
  ;; selector rebuild and the routing, and reach the shelled command.
  (it "carries `aliases` from the CALL all the way into the clean-JVM command"
      (let [root
            (temp-project! {"deps.edn" "{:deps {}}\n"
                            "test/vis/fixture/alias_test.clj"
                            (str "(ns vis.fixture.alias-test\n"
                                 "  (:require [lazytest.core :refer [defdescribe expect it]]))\n"
                                 "(defdescribe adds-test (it \"adds\" (expect (= 2 (+ 1 1)))))\n")})

            seen
            (atom :never-called)]

        (with-redefs [com.blockether.vis.internal.language.clojure.test-runner/cli-command-for
                      (fn [_root _sel aliases]
                        (reset! seen aliases)
                        {:tool :clj :cmd ["clojure" "-M:test:bench"]})

                      tr/run-command
                      (fn [& _]
                        {:exit 0
                         :out "Ran 1 test cases in 0.1 seconds.\n0 failures, 0 errors.\n"
                         :err ""})]

          (tr/clj-test-fn {:workspace/root (.getPath root)}
                          {"paths" ["test/vis/fixture/alias_test.clj"] "aliases" [":bench"]}))
        (expect (= ["bench"] @seen)))))

;; Issue #157 cross-validation: refuse selections/configurations we cannot execute faithfully.
(defdescribe
  cross-validation-regression-test
  (it "does not append focus to an alias that already focuses another suite"
      (with-project {"deps.edn" (pr-str {:aliases {:test {:main-opts ["-m" "kaocha.runner" "--focus"
                                                                      "other.core-test"]}}})}
                    (fn [root]
                      (expect (string?
                                (:error (cli-command-for root {:nses ["sample.core-test"]} [])))))))
  (it "refuses persistent watch commands for a one-shot test operation"
      (doseq [opts [{:main-opts ["-m" "kaocha.runner" "--watch"]}
                    {:exec-fn 'kaocha.runner/exec-fn :exec-args {:kaocha/watch? true}}]]
        (with-project {"deps.edn" (pr-str {:aliases {:test opts}})}
                      (fn [root]
                        (expect (string? (:error (cli-command-for root {} []))))))))
  (it "does not combine inherited exec metadata focus with requested namespace focus"
      (with-project
        {"deps.edn" (pr-str {:aliases {:test {:exec-fn 'kaocha.runner/exec-fn
                                              :exec-args {:kaocha.filter/focus-meta ['slow]}}}})}
        (fn [root]
          (expect (string? (:error (cli-command-for root {:nses ["sample.core-test"]} [])))))))
  (it "routes nested shared tests using their own shadow manifest"
      (with-project
        {"apps/web/shadow-cljs.edn" "{:builds {:test {:target :node-test :output-to \"out.js\"}}}"
         "apps/web/test/shared_test.cljc" "(ns shared-test)"}
        (fn [root]
          (expect (= (str root "/apps/web")
                     (:root (run-capturing-cljs root
                                                {"path" "apps/web/test/shared_test.cljc"})))))))
  (it "refuses same-named JVM and JS test namespaces rather than overwriting an index entry"
      (with-project {"shadow-cljs.edn"
                     "{:builds {:test {:target :node-test :output-to \"out.js\"}}}"
                     "test/shared_test.clj" "(ns shared-test)"
                     "test/shared_test.cljs" "(ns shared-test)"}
                    (fn [root]
                      (doseq [arg [{"path" "test"} {"ns" "shared-test"} {}]]
                        (expect (try (run-capturing-cljs root arg)
                                     false
                                     (catch clojure.lang.ExceptionInfo _ true)))))))
  (it "refuses a selection spanning independent shadow projects"
      (with-project
        {"shadow-cljs.edn" "{:builds {:test {:target :node-test :output-to \"out.js\"}}}"
         "apps/a/shadow-cljs.edn" "{:builds {:test {:target :node-test :output-to \"out.js\"}}}"
         "apps/b/shadow-cljs.edn" "{:builds {:test {:target :node-test :output-to \"out.js\"}}}"
         "apps/a/test/a_test.cljs" "(ns a-test)"
         "apps/b/test/b_test.cljs" "(ns b-test)"}
        (fn [root]
          (expect (try (run-capturing-cljs root {"paths" ["apps/a/test" "apps/b/test"]})
                       false
                       (catch clojure.lang.ExceptionInfo _ true))))))
  (it
    "does not let an unrelated later count erase failures in a completed report"
    (with-redefs
      [tr/run-command
       (sh-answering
         (atom [])
         0
         "Ran 2 tests containing 2 assertions.\n1 failures, 0 errors.\nfixture log: 0 failures\n")]
      (with-project cljs-project
                    (fn [root]
                      (expect (false? (get (run-via-shadow root [] {}) "is_pass")))))))
  (it
    "does not pass a later green summary after an earlier failing node run"
    (with-redefs
      [tr/run-command
       (sh-answering
         (atom [])
         0
         "Ran 1 tests containing 1 assertions.\n1 failures, 0 errors.\nRan 1 tests containing 1 assertions.\n0 failures, 0 errors.\n")]
      (with-project cljs-project
                    (fn [root]
                      (expect (false? (get (run-via-shadow root [] {}) "is_pass")))))))
  (it
    "requires the complete cljs.test failure/error pair"
    (with-redefs [tr/run-command
                  (sh-answering (atom []) 0 "Ran 2 tests containing 2 assertions.\n0 failures.\n")]
      (with-project cljs-project
                    (fn [root]
                      (expect (false? (get (run-via-shadow root [] {}) "is_pass"))))))))

(defdescribe
  node-execution-boundary-test
  (it
    "uses Node's exit status even after a passing printed summary"
    (with-project
      cljs-project
      (fn [root]
        (with-redefs
          [tr/run-command
           (fn [_dir args _deadline]
             (if (= "node" (first args))
               {:exit 1
                :out "Ran 1 tests containing 1 assertions.\n0 failures, 0 errors.\n"
                :err "late runtime error"}
               {:exit 0
                :out
                "[:test] Build completed. (1 files)\nRan 1 tests containing 1 assertions.\n0 failures, 0 errors.\n"
                :err ""}))]
          (expect (false? (get (run-via-shadow root [] {}) "is_pass")))))))
  (it "does not execute stale JavaScript after shadow prints help with exit zero"
      (with-project cljs-project
                    (fn [root]
                      (let [calls (atom [])]
                        (with-redefs [tr/run-command
                                      (fn [_dir args _deadline]
                                        (swap! calls conj (first args))
                                        {:exit 0 :out "shadow-cljs - HELP\n" :err ""})]
                          (expect (false? (get (run-via-shadow root [] {}) "is_pass")))
                          (expect (= 1 (count @calls))))))))
  (it
    "disables autorun even in :dev and executes the effective :output-to"
    (with-project
      (assoc cljs-project
        "shadow-cljs.edn"
        "{:builds {:test {:target :node-test :output-to \"out.js\" :dev {:autorun true :output-to \"dev-out.js\"}}}}")
      (fn [root]
        (let [steps (:steps (shadow/run-steps root {}))]
          (expect (= ["node" "dev-out.js"] (:argv (second steps))))))))
  (it
    "refuses a dynamic output path instead of mistaking an environment variable name for JavaScript"
    (with-project (assoc cljs-project
                    "shadow-cljs.edn"
                    "{:builds {:test {:target :node-test :output-to #shadow/env \"TEST_OUTPUT\"}}}")
                  (fn [root]
                    (expect (string? (:error (shadow/run-steps root {}))))))))

(defdescribe
  shadow-output-isolation-test
  (it "isolates output/dev paths and cleans successful, failed and unlaunchable commands"
      ;; Issue #157: never execute or remove a watch-owned bundle.
      (with-project
        (assoc cljs-project
          "watch.js" "watch-owned"
          "shadow-cljs.edn" (pr-str {:builds {:test {:target :node-test
                                                     :output-to "watch.js"
                                                     :dev {:autorun true
                                                           :output-to "watch.js"
                                                           :output-dir "watch-out"}}}}))
        (fn [root]
          (let [outputs (atom [])]
            (doseq [outcome [:pass :compile-failure :node-failure :launch-failure]]
              (let [calls (atom [])]
                (with-redefs
                  [tr/run-command
                   (fn [_dir args _deadline]
                     (swap! calls conj (first args))
                     (if (= "node" (first args))
                       (do (expect (= (last @outputs) (second args)))
                           (if (= :launch-failure outcome)
                             {:exit -1 :out "" :err "fixture launch failure"}
                             {:exit (if (= :node-failure outcome) 1 0)
                              :out "Ran 1 tests containing 1 assertions.\n0 failures, 0 errors.\n"
                              :err ""}))
                       (let [overrides (edn/read-string
                                         (second (drop-while #(not= "--config-merge" %) args)))
                             output (:output-to overrides)]

                         (swap! outputs conj output)
                         (expect (str/includes? output ".vis-shadow-run-"))
                         (expect (str/starts-with? (:output-dir overrides)
                                                   (str (.getParent (io/file output)) "/")))
                         (expect (= (dissoc overrides :dev) (:dev overrides)))
                         (expect (false? (:autorun overrides)))
                         (spit output "fixture bundle")
                         {:exit (if (= :compile-failure outcome) 1 0)
                          :out "[:test] Build completed.\n"
                          :err ""})))]
                  (expect (= (= :pass outcome) (get (run-via-shadow root [] {}) "is_pass"))))
                (expect (not (.exists (.getParentFile (io/file (last @outputs))))))
                (when (= :compile-failure outcome) (expect (= 1 (count @calls))))))
            (expect (= "watch-owned" (slurp (io/file root "watch.js"))))
            (expect (= 4 (count (set @outputs)))))))))

;; Opt-in real toolchain checks. Only the throwaway fixture depends on shadow-cljs.
;; Run with VIS_TEST_SHADOW_CLJS=<npm/Maven version> and --var .../live-shadow-toolchain-test.
(when-let [version (System/getenv "VIS_TEST_SHADOW_CLJS")]
  (defdescribe
    live-shadow-toolchain-test
    (it
      "cross-validates npm/deps launchers and build shapes against a real shadow compiler"
      (with-project
        {"package.json"
         (str "{\"private\":true,\"devDependencies\":{\"shadow-cljs\":\"" version "\"}}")
         "test/repro/core_test.cljs"
         "(ns repro.core-test (:require [cljs.test :refer-macros [deftest is]]))\n(deftest passes (is (= 2 (+ 1 1))))\n"
         "test/repro/fail_test.cljs"
         "(ns repro.fail-test (:require [cljs.test :refer-macros [deftest is]]))\n(deftest fails (is (= 3 (+ 1 1))))\n"
         "spec/repro/core_spec.cljs"
         "(ns repro.core-spec (:require [cljs.test :refer-macros [deftest is]]))\n(deftest passes (is true))\n"}
        (fn [root]
          (let [install (shell/sh "npm" "install"
                                  "--ignore-scripts" "--no-audit"
                                  "--no-fund" "--no-package-lock"
                                  :dir root)
                build {:target :node-test :output-to "out/tests.js"}
                cfg {:source-paths ["test" "spec"] :builds {:test build}}
                run! (fn [config arg passed total]
                       (spit (io/file root "shadow-cljs.edn") (pr-str config))
                       (let [response (tr/clj-test-fn {:workspace/root root
                                                       :session-id "live-shadow-test"}
                                                      arg)
                             r (:result response)]

                         (when-not (and (:success? response) (map? r))
                           (throw (ex-info "invalid test result envelope" {:response response})))
                         (when (or (not= passed (get r "is_pass")) (not= total (get r "selected")))
                           (throw (ex-info (str "real shadow-cljs result disagrees with selection "
                                                (pr-str arg)
                                                ": " (get r "error")
                                                "\n" (get r "output"))
                                           {:request arg :result r})))
                         (expect (= passed (get r "is_pass")))
                         (expect (= total (get r "selected")))))]

            (when-not (zero? (:exit install))
              (throw (ex-info "fixture npm install failed"
                              (select-keys install [:exit :out :err]))))
            (run! cfg {} false 2)
            (run! cfg {"path" "test/repro/core_test.cljs"} true 1)
            (run! (assoc cfg
                    :builds [(assoc build
                               :id :test
                               :ns-regexp "-spec$")])
                  {"path" "spec/repro/core_spec.cljs"}
                  true
                  1)
            (run! (assoc-in cfg [:builds :test :dev] {:autorun true :output-to "out/dev-tests.js"})
                  {"ns" "repro.core-test"}
                  true
                  1)
            (spit (io/file root "deps.edn")
                  (pr-str {:aliases {:frontend {:extra-paths ["test" "spec"]
                                                :extra-deps {'thheller/shadow-cljs {:mvn/version
                                                                                    version}}}}}))
            (let [deps-cfg (assoc cfg :deps {:aliases [:frontend]})]
              (run! deps-cfg {"ns" "repro.core-test"} true 1)
              (io/delete-file (io/file root "node_modules/.bin/shadow-cljs"))
              (run! deps-cfg {"ns" "repro.core-test"} true 1)
              ;; Issue #157: a focused CLI run must not reconfigure an existing watch.
              (let [log (io/file root "server.log")
                    server (.start (doto (ProcessBuilder. ^java.util.List
                                                          ["clojure" "-M:frontend" "-m"
                                                           "shadow.cljs.devtools.cli" "server"])
                                     (.directory (io/file root))
                                     (.redirectErrorStream true)
                                     (.redirectOutput log)))]

                (try
                  (let
                    [port-file (io/file root ".shadow-cljs/nrepl.port")
                     deadline (+ (System/currentTimeMillis) 180000)
                     port (loop []

                            (cond (.isFile port-file) (parse-long (str/trim (slurp port-file)))
                                  (or (not (.isAlive server))
                                      (> (System/currentTimeMillis) deadline))
                                  (throw (ex-info "fixture shadow server did not start"
                                                  {:output (slurp log)}))
                                  :else (do (Thread/sleep 100) (recur))))
                     eval! (fn [code]
                             (let [r
                                   (nc/eval!
                                     {:host "127.0.0.1" :port port :timeout-ms 180000 :code code})]
                               (when (or (get r "ex") (get r "timed_out"))
                                 (throw (ex-info "fixture shadow eval failed" r)))
                               (get r "value")))
                     snapshot
                     "(let [w (shadow.cljs.devtools.api/get-worker :test)] [(System/identityHashCode w) (select-keys @(:state-ref w) [:build-config :autobuild])])"]

                    (expect
                      (=
                        ":watching"
                        (eval!
                          "(do (require 'shadow.cljs.devtools.api) (shadow.cljs.devtools.api/watch :test))")))
                    (let [before (eval! snapshot)]
                      (expect (string? before))
                      ;; Force the watch write into the compile/Node gap, not a timing lottery.
                      (let [real-command @#'tr/run-command
                            interposed? (atom false)]

                        (with-redefs
                          [tr/run-command
                           (fn [dir args deadline]
                             (when (and (= "node" (first args))
                                        (compare-and-set! interposed? false true))
                               (spit (io/file root "test/repro/fail_test.cljs")
                                     "\n;; Force a watch rebuild at the execution boundary.\n"
                                     :append
                                     true)
                               (eval! "(shadow.cljs.devtools.api/watch-compile! :test)"))
                             (real-command dir args deadline))]
                          (run! deps-cfg {"ns" "repro.core-test"} true 1))
                        (expect @interposed?))
                      ;; Both compilers must finish before either Node starts. This also
                      ;; exercises shadow's shared compiler cache under overlapping runs.
                      (dotimes [_ 3]
                        (let [real-command @#'tr/run-command
                              ready (java.util.concurrent.CountDownLatch. 2)
                              outputs (atom [])]

                          (with-redefs [tr/run-command
                                        (fn [dir args deadline]
                                          (when (= "node" (first args))
                                            (swap! outputs conj (second args))
                                            (.countDown ready)
                                            (when-not (.await ready
                                                              180
                                                              java.util.concurrent.TimeUnit/SECONDS)
                                              (throw (ex-info "parallel Node barrier timed out"
                                                              {}))))
                                          (real-command dir args deadline))]
                            (let [runs (mapv (fn [ns-name]
                                               (future (:result (tr/clj-test-fn {:workspace/root
                                                                                 root}
                                                                                {"ns" ns-name}))))
                                             ["repro.core-test" "repro.fail-test"])]
                              (try (doseq [[run passed] (map vector runs [true false])]
                                     (let [r (deref run 240000 ::timeout)]
                                       (expect (not= ::timeout r))
                                       (expect (= passed (get r "is_pass")))
                                       (expect (= 1 (get r "selected")))))
                                   (finally (doseq [run runs]
                                              (future-cancel run))))))
                          (expect (= 2 (count (set @outputs))))
                          (doseq [output @outputs]
                            (expect (not (.exists (.getParentFile (io/file output))))))))
                      (expect (.isAlive server))
                      (expect (= before (eval! snapshot)))
                      (expect (= ":ok" (eval! "(shadow.cljs.devtools.api/watch-compile! :test)")))
                      (let [full (shell/sh "node" "out/tests.js" :dir root)]
                        (expect (not (zero? (:exit full))))
                        (expect (str/includes? (:out full) "Testing repro.fail-test"))
                        (expect (str/includes? (:out full) "Ran 2 tests")))))
                  (finally
                    ;; Only the fixture-owned process tree is terminated.
                    (with-open [children (.descendants server)]
                      (doseq [^java.lang.ProcessHandle child (reverse (vec (.toArray children)))]
                        (.destroyForcibly child)))
                    (.destroyForcibly server)
                    (.waitFor server 10 java.util.concurrent.TimeUnit/SECONDS)))))))))))

(defdescribe
  effective-configuration-boundaries-test
  (it
    "merges all :extra-deps before applying the combined :replace-deps classpath"
    (with-project
      {"shadow-cljs.edn" "{:deps {:aliases [:shadow :isolated]}}"
       "deps.edn"
       "{:aliases {:shadow {:extra-deps {thheller/shadow-cljs {:mvn/version \"3.5.0\"}}} :isolated {:replace-deps {org.clojure/clojure {:mvn/version \"1.12.0\"}}}}}"}
      (fn [root]
        (expect (= :deps (:kind (shadow/launcher root)))))))
  (it
    "does not guess the effective build when global shadow configuration can override dev settings"
    (with-project (assoc cljs-project
                    ".shadow-cljs/config.edn"
                    "{:build-defaults {:dev {:output-to \"different.js\"}}}")
                  (fn [root]
                    (let [home (System/getProperty "user.home")]
                      (try (System/setProperty "user.home" root)
                           (expect (string? (:error (shadow/run-steps root {}))))
                           (finally (System/setProperty "user.home" home)))))))
  (it "does not silently drop one explicit path that contains no matching tests"
      (with-project
        (assoc cljs-project "empty/README.txt" "No test sources here.")
        (fn [root]
          (expect (try (run-capturing-cljs root {"paths" ["test/repro/core_test.cljs" "empty"]})
                       false
                       (catch clojure.lang.ExceptionInfo _ true))))))
  (it
    "does not pass partially matched Node namespace focus"
    (with-project
      cljs-project
      (fn [root]
        (with-redefs
          [tr/run-command
           (sh-answering
             (atom [])
             0
             "Testing repro.core-test\nRan 1 tests containing 1 assertions.\n0 failures, 0 errors.\n")]
          (expect (false? (get (run-via-shadow root ["repro.core-test" "repro.missing-test"] {})
                               "is_pass"))))))))

(defdescribe
  subprocess-test-timeout-test
  ;; Regression #207: the clean-JVM path ignored the REPL's run_tests budget.
  ;; This fixture finishes on its own even against the unbounded implementation.
  (it "times out and reaps a launcher, child and TERM-resistant grandchild"
      (with-project
        {}
        (fn [root]
          (with-redefs [rt/RUN_TESTS_TIMEOUT_MS
                        500

                        tr/cli-command-for
                        (fn [& _]
                          {:tool :clj
                           :cmd ["bash" "-c"
                                 (str
                                   "printf 'PARENT=%s\n' $$; "
                                   "bash -c 'trap \"\" TERM HUP; sleep 5 & "
                                   "printf \"CHILD=%s GRANDCHILD=%s\\n\" $$ $!; wait' & wait")]})]

            (let [r
                  (run-via-cli root {})

                  pids
                  (map (comp parse-long second)
                       (re-seq #"(?:PARENT|CHILD|GRANDCHILD)=(\d+)" (get r "output")))]

              (expect (true? (get r "timed_out")))
              (expect (false? (get r "is_pass")))
              (expect (str/includes? (str (get r "error")) "500ms"))
              (expect (= 3 (count pids)))
              (doseq [pid pids]
                (let [h (.orElse (java.lang.ProcessHandle/of (long pid)) nil)]
                  (try (expect (or (nil? h) (not (.isAlive ^java.lang.ProcessHandle h))))
                       (finally (when h (.destroyForcibly ^java.lang.ProcessHandle h))))))))))))

(defdescribe
  subprocess-command-contract-test
  ;; #207: exercise the actual process boundary, not just a canned shell result.
  (it
    "retains complete stdout/stderr, exit status and closes stdin"
    (with-project
      {}
      (fn [root]
        (let
          [r
           (#'tr/run-command
            root
            ["bash" "-c"
             "cat; printf 'first\n'; for ((i=0;i<20000;i++)); do echo output; echo warning >&2; done; printf 'last\n'; exit 7"]
            (#'tr/test-deadline))]
          (expect (= 7 (:exit r)))
          (expect (false? (:timed-out r)))
          (expect (= (str "first\n" (apply str (repeat 20000 "output\n")) "last\n") (:out r)))
          (expect (= (apply str (repeat 20000 "warning\n")) (:err r)))))))
  (it "does not launch another process after its shared deadline expires"
      (with-project {}
                    (fn [root]
                      (let [r (#'tr/run-command root ["bash" "-c" "touch unexpected"] 0)]
                        (expect (:timed-out r))
                        (expect (not (.exists (io/file root "unexpected"))))))))
  (it "reports a failed spawn as data and removes its capture directory"
      (with-project
        {}
        (fn [root]
          (let [dirs
                (fn []
                  (set (filter #(str/starts-with? % "vis-test-run-")
                               (.list (io/file (System/getProperty "java.io.tmpdir"))))))

                before
                (dirs)

                r
                (#'tr/run-command root [(str root "/missing-command")] (#'tr/test-deadline))]

            (expect (= -1 (:exit r)))
            (expect (str/includes? (:err r) "missing-command"))
            (expect (= before (dirs)))))))
  (it "cannot pass a timed-out process even if it printed a pass and exited zero on TERM"
      (with-project {}
                    (fn [root]
                      (with-redefs [tr/cli-command-for
                                    (fn [& _]
                                      {:tool :clj :cmd ["fixture"]})

                                    tr/run-command
                                    (fn [& _]
                                      {:exit 0
                                       :out "Ran 1 tests.\n0 failures, 0 errors.\n"
                                       :err "last warning"
                                       :timed-out true})]

                        (let [r (run-via-cli root {})]
                          (expect (false? (get r "is_pass")))
                          (expect (true? (get r "timed_out")))
                          (expect (str/includes? (get r "output") "last warning"))))))))

(defdescribe
  shadow-shared-timeout-test
  ;; #207: compilation and execution consume ONE REPL-sized budget.
  (it "shares a deadline, preserves both steps' output and refuses a timed-out pass"
      (with-project
        cljs-project
        (fn [root]
          (let [deadlines (atom [])]
            (with-redefs [rt/RUN_TESTS_TIMEOUT_MS 1234
                          tr/run-command
                          (fn [_root _argv deadline]
                            (swap! deadlines conj deadline)
                            (if (= 1 (count @deadlines))
                              {:exit 0 :out "[:test] Build completed.\n" :err "compiler warning"}
                              {:exit 0
                               :out "Ran 1 tests.\n0 failures, 0 errors.\n"
                               :err "test warning"
                               :timed-out true}))]

              (let [r (run-via-shadow root [] {})]
                (expect (= 2 (count @deadlines)))
                (expect (apply = @deadlines))
                (expect (false? (get r "is_pass")))
                (expect (true? (get r "timed_out")))
                (expect (str/includes? (get r "error") "1234ms"))
                (expect (str/includes? (get r "output") "compiler warning"))
                (expect (str/includes? (get r "output") "test warning"))))))))
  (it "does not run Node after a compiler timeout, even with a completion banner"
      (with-project
        cljs-project
        (fn [root]
          (let [calls (atom 0)]
            (with-redefs [tr/run-command
                          (fn [& _]
                            (swap! calls inc)
                            {:exit 0 :out "[:test] Build completed.\n" :err "" :timed-out true})]
              (let [r (run-via-shadow root [] {})]
                (expect (= 1 @calls))
                (expect (true? (get r "timed_out")))
                (expect (false? (get r "is_pass"))))))))))

(defdescribe
  subprocess-cancellation-test
  ;; #207: interrupt the real CLI and shadow workers after their grandchildren
  ;; have started. All fixture processes self-expire even if cleanup regresses.
  (it
    "propagates interruption only after terminating the owned process tree"
    (doseq [kind [:cli :shadow]]
      (with-project
        {}
        (fn [root]
          (let [pid-file (io/file root "pids")
                command ["bash" "-c"
                         (str "exec > pids; printf '%s\n' $$; "
                              "bash -c 'trap \"\" TERM HUP; sleep 8 & "
                              "printf \"%s %s\\n\" $$ $!; wait' & wait")]
                result (promise)
                run! (fn []
                       (try (deliver
                              result
                              (if (= :cli kind) (run-via-cli root {}) (run-via-shadow root [] {})))
                            (catch InterruptedException _
                              (deliver result
                                       {:cancelled true
                                        :interrupted (.isInterrupted (Thread/currentThread))}))
                            (catch Throwable e (deliver result e))))]

            (with-redefs [rt/RUN_TESTS_TIMEOUT_MS 60000
                          tr/cli-command-for (fn [& _]
                                               {:tool :clj :cmd command})
                          shadow/run-steps
                          (fn [& _]
                            {:build "test" :target :node-test :steps [{:argv command}]})]

              (let [worker (doto (Thread. ^Runnable run!) (.start))
                    pids (loop [attempts 200]
                           (let [pids (when (.exists pid-file)
                                        (mapv parse-long (re-seq #"\d+" (slurp pid-file))))]
                             (if (or (= 3 (count pids)) (zero? attempts))
                               pids
                               (do (Thread/sleep 10) (recur (dec attempts))))))
                    handles (mapv #(.orElse (java.lang.ProcessHandle/of (long %)) nil) pids)]

                (try (expect (= 3 (count pids)))
                     (.interrupt worker)
                     (expect (= {:cancelled true :interrupted true} (deref result 6000 ::pending)))
                     (doseq [^java.lang.ProcessHandle h handles]
                       (expect (or (nil? h) (not (.isAlive h)))))
                     (finally (.interrupt worker)
                              (doseq [^java.lang.ProcessHandle h handles]
                                (when h (.destroyForcibly h)))
                              (.join worker 3000)))))))))))

(defdescribe repl-timeout-ownership-test
             (it "uses the same budget but never kills an external REPL's process"
                 (let [seen
                       (atom nil)

                       kills
                       (atom 0)]

                   (with-redefs [rt/RUN_TESTS_TIMEOUT_MS
                                 1234

                                 nc/probe!
                                 (constantly {:status :up})

                                 nc/eval!
                                 (fn [opts]
                                   (reset! seen (:timeout-ms opts))
                                   {"timed_out" true})

                                 shell-owner/kill-tree!
                                 (fn [_]
                                   (swap! kills inc))]

                     (let [r (run-via-repl "." [] {} 54749)]
                       (expect (= 1234 @seen))
                       (expect (true? (get r "timed_out")))
                       (expect (zero? @kills)))))))
