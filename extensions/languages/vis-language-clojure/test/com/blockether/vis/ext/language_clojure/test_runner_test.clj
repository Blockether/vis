(ns com.blockether.vis.ext.language-clojure.test-runner-test
  "Unit tests for the in-REPL test path's failure handling — specifically that a
   server that vanishes mid-run surfaces as a structured result the model can act
   on, never a raw connect exception that eats the turn."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nc]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [com.blockether.vis.ext.language-clojure.shadow-cljs :as shadow]
            [com.blockether.vis.ext.language-clojure.test-runner :as tr]
            [com.blockether.vis.internal.foundation.surface-contract :as contract]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private run-via-repl @#'com.blockether.vis.ext.language-clojure.test-runner/run-via-repl)
(def ^:private normalize-faults
  @#'com.blockether.vis.ext.language-clojure.test-runner/normalize-faults)

(defn- connect-failed-throw
  [_]
  (throw (ex-info "nREPL connect failed on localhost:54749 — is the REPL running?"
                  {:type :clj/nrepl-connect-failed :port 54749})))

(defdescribe run-via-repl-connect-failure-test
             (it "returns a structured 'server down' result when the probe reports down"
                 (with-redefs
                   [nc/probe! (fn [_]
                                {:status :down})]
                   (let [r (run-via-repl "." ["some.ns-test"] {} 54749)]
                     (expect (= "repl" (get r "mode")))
                     (expect (= 54749 (get r "port")))
                     (expect (re-find #"down or unresponsive" (get r "error")))
                     (expect (true? (get r "repl_unusable"))))))
             (it "converts a mid-run connect failure (probe passed, eval! then failed) into data"
                 ;; The TOCTOU window: probe answered :up, but the server crashed / was reaped /
                 ;; killed before the eval landed. Must NOT bubble a raw :clj/nrepl-connect-failed.
                 (with-redefs
                   [nc/probe!
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
                 (with-redefs
                   [nc/probe!
                    (fn [_]
                      {:status :up})

                    nc/eval!
                    (fn [_]
                      (throw (ex-info "boom" {:type :something-else})))]

                   ;; lazytest has no `thrown?` macro — assert the throw with try/catch.
                   (let
                     [thrown? (try (run-via-repl "." ["some.ns-test"] {} 54749)
                                   false
                                   (catch clojure.lang.ExceptionInfo e
                                     (= :something-else (:type (ex-data e)))))]
                     (expect (true? thrown?))))))

(def ^:private recover-if-unusable
  @#'com.blockether.vis.ext.language-clojure.test-runner/recover-if-unusable)

(defdescribe recover-if-unusable-test
             ;; Recovery never SPAWNS: an unusable REPL means the clean-JVM CLI runs the suite
             ;; THIS turn, and bringing a REPL back is the caller's own `repl` call.
             (it "runs the CLI suite in a clean JVM when the reused server was unusable"
                 (with-redefs
                   [com.blockether.vis.ext.language-clojure.test-runner/run-via-cli
                    (fn [_root _norm]
                      {"mode" "cli" "is_pass" true "note" "7 cases"})]
                   (let [r (recover-if-unusable "/proj" {} {"repl_unusable" true "error" "down"})]
                     (expect (= "cli" (get r "mode")))
                     (expect (true? (get r "recovered")))
                     (expect (re-find #"clean JVM" (get r "note")))
                     (expect (re-find #"7 cases" (get r "note"))))))
             (it "keeps the timeout error for a wedged eval and shells no CLI"
                 (let [cli-called (atom false)]
                   (with-redefs
                     [com.blockether.vis.ext.language-clojure.test-runner/run-via-cli
                      (fn [_root _norm]
                        (reset! cli-called true)
                        {})]
                     (let
                       [r (recover-if-unusable "/proj" {} {"repl_wedged" true "error" "timed out"})]
                       (expect (false? @cli-called))
                       (expect (re-find #"clean JVM" (get r "error")))))))
             (it "passes a healthy result through untouched (no CLI, nothing spawned)"
                 (let [orig {"mode" "repl" "pass" 5}]
                   (expect (= orig (recover-if-unusable "/proj" {} orig))))))

(defdescribe
  group-faults-by-cwd-test
  "`group-faults-by-cwd` folds the ONE flat failures vector into the same
   directory-nested `by-cwd` shape lint and format expose — the file's dir written
   once, basename inner, edge files handled. An erroring test is not split into a
   parallel bucket: it rides `failures` carrying `\"type\" \"error\"`."
  (it "nests faults by directory then basename, writing each dir prefix once"
      (let
        [f1
         {"ns" "a.core" "test" "adds" "type" "fail" "file" "src/a/core.clj" "line" 12}

         f2
         {"ns" "a.core" "test" "subs" "type" "fail" "file" "src/a/core.clj" "line" 20}

         f3
         {"ns" "a.util" "test" "trim" "type" "fail" "file" "src/a/util.clj" "line" 3}

         e1
         {"ns" "a.core" "test" "boom" "type" "error" "file" "src/a/core.clj" "line" 99}

         grouped
         (tr/group-faults-by-cwd [f1 f2 f3 e1])]

        (expect (= #{"src/a"} (set (keys grouped))))
        (expect (= #{"core.clj" "util.clj"} (set (keys (get grouped "src/a")))))
        (expect (= [f1 f2 e1] (get-in grouped ["src/a" "core.clj" "failures"])))
        (expect (= [f3] (get-in grouped ["src/a" "util.clj" "failures"])))
        ;; ONE fault kind per file — the erroring test is typed, not re-listed
        (expect (= #{"failures"} (set (keys (get-in grouped ["src/a" "core.clj"])))))))
  (it "buckets a bare JVM frame (no parent dir) under \".\" by its basename"
      (let
        [e
         {"ns" "a.core" "type" "error" "file" "Numbers.java" "line" 7}

         grouped
         (tr/group-faults-by-cwd [e])]

        (expect (= [e] (get-in grouped ["." "Numbers.java" "failures"])))))
  (it "buckets a fileless fault under \".\"/\"<unknown>\""
      (let
        [f
         {"ns" "a.core" "test" "nofile"}

         grouped
         (tr/group-faults-by-cwd [f])]

        (expect (= [f] (get-in grouped ["." "<unknown>" "failures"])))))
  (it "treats a blank file string as fileless"
      (let
        [f
         {"ns" "a.core" "file" "   "}

         grouped
         (tr/group-faults-by-cwd [f])]

        (expect (= [f] (get-in grouped ["." "<unknown>" "failures"])))))
  (it "returns an empty map when there is nothing to group"
      (expect (= {} (tr/group-faults-by-cwd [])))))

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
      (let
        [parsed
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
      (let
        [parsed
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
  (let
    [root (.toFile (java.nio.file.Files/createTempDirectory
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
    (with-redefs
      [repl-manager/live-repl-for-dir (fn [_sid root]
                                        (swap! seen assoc :root root)
                                        {:port 12345})
       com.blockether.vis.ext.language-clojure.test-runner/run-via-repl
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
      (with-project
        (assoc thing-test-file "src/com/example/lonely.clj" "(ns com.example.lonely)\n")
        (fn [root]
          (let
            [e (try (run-capturing root {"paths" ["src"]}) (catch clojure.lang.ExceptionInfo e e))]
            (expect (= :clj/bad-args (:type (ex-data e))))
            (expect (re-find
                      #"no test namespaces \(\*_test\.clj / \*_test\.cljc / \*_test\.cljs\) under"
                      (ex-message e)))))))
  ;; Regression, user report (paraphrased: ONE directory segment of an otherwise
  ;; correct path was misspelled, and the runner answered "no *_test.clj
  ;; namespaces under <that very test file>" — so the caller read it as a project
  ;; whose tests do not exist, retried the same typo, then went digging through
  ;; the build file instead of the spelling).
  (it "refuses a path that is NOT ON DISK as a typo, naming the part that is"
      (with-project
        thing-test-file
        (fn [root]
          (let
            [typo
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
                      (let
                        [e (try (run-capturing root {"ns" "test/com/example/nope_test.clj"})
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
          (let
            [seen (run-capturing root
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
  (let
    [n
     (create-ns ns-sym)

     run-form
     @#'com.blockether.vis.ext.language-clojure.test-runner/run-form]

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
      (let
        [result
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
                 (let
                   [result (run-form-selecting 'vis.test-runner-node-id-fixture
                                               '[adds-test subtracts-test]
                                               {:vars [{:ns nil :name "adds"}]})]
                   (expect (nil? (get result "error")))
                   (expect (= 1 (get result "selected")))
                   (expect (= 1 (get result "skipped")))))
             (it "selects the test var when the id named it outright"
                 (let
                   [result (run-form-selecting 'vis.test-runner-node-id-fixture
                                               '[adds-test subtracts-test]
                                               {:vars [{:ns nil :name "adds-test"}]})]
                   (expect (= 1 (get result "selected")))))
             (it "scopes the name to the namespace its path resolved to"
                 ;; A node id that named a FILE must not select the same var name in some
                 ;; other namespace — that is the cross-product `only` used to do.
                 (let
                   [result (run-form-selecting 'vis.test-runner-node-id-fixture
                                               '[adds-test]
                                               {:vars [{:ns "com.example.elsewhere-test"
                                                        :name "adds-test"}]})]
                   (expect (re-find #"no test var matched" (get result "error"))))))
(def ^:private run-via-cli @#'com.blockether.vis.ext.language-clojure.test-runner/run-via-cli)

(defn- with-cli-run
  "Run the cli fallback against a canned shell result, with no project on disk."
  [{:keys [exit out]}]
  (with-redefs
    [com.blockether.vis.ext.language-clojure.test-runner/cli-command-for
     (fn [_root _sel]
       {:tool :clj :cmd ["clojure" "-M:test"]})

     shell/sh
     (fn [& _]
       {:exit exit :out out :err ""})]

    (run-via-cli "/proj" {})))

(defdescribe
  cli-summary-counts-test
  ;; The cli path used to retell its summary line as a NOTE ("12 cases, 3
  ;; failures") and leave total / fail / errored nil, so one run read as counts
  ;; on the repl path and as prose on the cli path.
  (it "reads the shelled runner's summary line into total / fail / errored"
      (let
        [r (with-cli-run {:exit 1
                          :out "Ran 12 test cases in 0.4 seconds.\n1 failures, 2 errors.\n"})]
        (expect (= 12 (get r "total")))
        ;; fail is every test that did not pass ...
        (expect (= 3 (get r "fail")))
        ;; ... and errored is the subset of it that THREW
        (expect (= 2 (get r "errored")))
        (expect (nil? (get r "note")))
        (expect (false? (get r "is_pass")))))
  (it "reports a green cli run as ZERO failures, never unknown"
      (let
        [r (with-cli-run {:exit 0
                          :out "Ran 12 test cases in 0.4 seconds.\n0 failures, 0 errors.\n"})]
        (expect (= 12 (get r "total")))
        (expect (= 0 (get r "fail")))
        (expect (= 0 (get r "errored")))
        (expect (true? (get r "is_pass"))))))

(defdescribe repl-errored-count-test
             (it "counts a test that THREW into errored as well as fail"
                 (let
                   [fixture-ns
                    'vis.test-runner-errored-fixture

                    n
                    (create-ns fixture-ns)

                    run-form
                    @#'com.blockether.vis.ext.language-clojure.test-runner/run-form]

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

(def ^:private ns-of-file @#'com.blockether.vis.ext.language-clojure.test-runner/ns-of-file)

(def ^:private resolve-selection
  @#'com.blockether.vis.ext.language-clojure.test-runner/resolve-selection)

(defn- temp-project!
  "Write `files` ({relative-path body}) under a fresh temp root and answer the
   root as a File."
  [files]
  (let
    [root (.toFile (java.nio.file.Files/createTempDirectory
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
      (let
        [root (temp-project! {"test/vis/fixture/plain_test.clj"
                              "(ns ^:no-doc vis.fixture.plain-test)\n"})]
        (expect (= "vis.fixture.plain-test"
                   (ns-of-file (io/file root "test/vis/fixture/plain_test.clj"))))))
  (it "answers nil for a file that declares no namespace"
      (let [root (temp-project! {"test/vis/fixture/none_test.clj" ";; no ns here\n(def x 1)\n"})]
        (expect (nil? (ns-of-file (io/file root "test/vis/fixture/none_test.clj"))))))
  (it
    "SELECTS the namespace when its own test file is named"
    (let
      [root
       (temp-project! {"test/vis/fixture/core_test.clj" metadata-test-source})

       selected
       (resolve-selection (.getPath root) [{:path "test/vis/fixture/core_test.clj" :var nil}] [])]

      (expect (= {:nses ["vis.fixture.core-test"] :vars [] :files []} (dissoc selected :ns-files)))
      ;; The FILE behind each namespace is what routes the run to its runtime.
      (expect (= "core_test.clj"
                 (.getName ^java.io.File (get (:ns-files selected) "vis.fixture.core-test"))))))
  (it
    "SELECTS it from the SOURCE file it covers, metadata on both"
    (let
      [root
       (temp-project!
         {"test/vis/fixture/core_test.clj" metadata-test-source
          "src/vis/fixture/core.clj"
          "(ns ^{:clj-kondo/config '{:linters {:unused-public-var {:level :off}}}}\n    vis.fixture.core)\n"})]
      (expect (= ["vis.fixture.core-test"]
                 (:nses (resolve-selection (.getPath root)
                                           [{:path "src/vis/fixture/core.clj" :var nil}]
                                           []))))))
  (it "finds a namespace declared after a leading form"
      (let
        [root (temp-project! {"test/vis/fixture/late_test.clj"
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
    (with-redefs
      [repl-manager/live-repl-for-dir (fn [_sid _root]
                                        nil)
       com.blockether.vis.ext.language-clojure.test-runner/run-via-cli
       (fn [root norm]
         (swap! seen assoc :cli-root root :cli-nses (:nses norm))
         {"mode" "cli" "ns" (first (:nses norm))})
       com.blockether.vis.ext.language-clojure.test-runner/run-via-shadow
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
  (it "keeps the JVM path for a mixed selection, dropping the namespaces it could never require"
      ;; One runtime per run: a JVM `require` of a .cljs file is a hard failure,
      ;; so the JVM run takes what it CAN load rather than dying on the first one.
      (with-project (assoc cljs-project "test/repro/jvm_test.clj" "(ns repro.jvm-test)\n")
                    (fn [root]
                      (let [seen (run-capturing-cljs root {"paths" ["test"]})]
                        (expect (= ["repro.jvm-test"] (:cli-nses seen)))
                        (expect (nil? (:nses seen)))))))
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
                      (let
                        [seen (run-capturing-cljs root
                                                  {"paths"
                                                   ["repositories/app/test/repro/core_test.cljs"]})]
                        (expect (= (str root "/repositories/app") (:root seen))))))))

(def ^:private run-via-shadow @#'com.blockether.vis.ext.language-clojure.test-runner/run-via-shadow)

(defn- sh-answering
  "A `clojure.java.shell/sh` stand-in that answers `out` with `exit`, recording
   every argv it was handed."
  [calls exit out]
  (fn [& args]
    (swap! calls conj (vec (take-while string? args)))
    {:exit exit :out out :err ""}))

(defdescribe
  run-via-shadow-verdict-test
  "shadow-cljs exits ZERO whether its tests passed, failed, or never ran — it
   exits zero even after printing its help text for an argument it rejected. The
   verdict therefore comes from the printed COUNTS, and a run with no counts at
   all is reported as not passing rather than as a green suite."
  (it "reports the counts of a clean run"
      (let [calls (atom [])]
        (with-redefs
          [shell/sh
           (sh-answering calls 0 "Ran 3 tests containing 5 assertions.\n0 failures, 0 errors.\n")]
          (with-project cljs-project
                        (fn [root]
                          (let [r (run-via-shadow root ["repro.core-test"] {})]
                            (expect (true? (get r "is_pass")))
                            (expect (= 3 (get r "total")))
                            (expect (= 0 (get r "fail")))
                            (expect (= "test" (get r "build")))
                            (expect (= "shadow-cljs" (get r "tool")))
                            (expect (= 1 (count @calls)))))))))
  (it "FAILS a run whose tests failed, even though shadow-cljs exited 0"
      (let [calls (atom [])]
        (with-redefs
          [shell/sh
           (sh-answering calls 0 "Ran 3 tests containing 5 assertions.\n2 failures, 1 errors.\n")]
          (with-project cljs-project
                        (fn [root]
                          (let [r (run-via-shadow root ["repro.core-test"] {})]
                            (expect (false? (get r "is_pass")))
                            (expect (= 3 (get r "total")))
                            (expect (= 3 (get r "fail")))
                            (expect (= 1 (get r "errored")))))))))
  (it "FAILS a run that printed no summary at all, rather than calling it green"
      (let [calls (atom [])]
        (with-redefs [shell/sh (sh-answering calls 0 "shadow-cljs - HELP\n  compile <build>\n")]
          (with-project cljs-project
                        (fn [root]
                          (let [r (run-via-shadow root ["repro.core-test"] {})]
                            (expect (false? (get r "is_pass")))
                            (expect (str/includes? (get r "error") "nothing ran"))))))))
  (it "names the build when the selected namespaces compiled but ran nothing"
      (let [calls (atom [])]
        (with-redefs
          [shell/sh
           (sh-answering calls 0 "Ran 0 tests containing 0 assertions.\n0 failures, 0 errors.\n")]
          (with-project cljs-project
                        (fn [root]
                          (let [r (run-via-shadow root ["repro.core-test"] {})]
                            (expect (false? (get r "is_pass")))
                            (expect (str/includes? (get r "error") ":source-paths"))))))))
  (it "reports a project it cannot run as data, and shells nothing"
      (let [calls (atom [])]
        (with-redefs [shell/sh (sh-answering calls 0 "")]
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
  (it "autoruns the node-test build through the npm binary, focused on the selected namespaces"
      (with-project cljs-project
                    (fn [root]
                      (let
                        [{:keys [steps build target kind]}
                         (shadow/run-steps root {:nses ["repro.core-test"]})

                         argv
                         (vec (:argv (first steps)))]

                        (expect (= ["test" :node-test :npm] [build target kind]))
                        (expect (= 1 (count steps)))
                        (expect (str/ends-with? (first argv) "node_modules/.bin/shadow-cljs"))
                        (expect (= ["compile" "test" "--config-merge"] (subvec argv 1 4)))
                        ;; `:autorun` is what makes `compile` RUN, and the regexp is printed
                        ;; with pr-str because `\.` is not a legal EDN escape — shadow-cljs
                        ;; answers an argument it cannot read by printing help and exiting 0.
                        (expect (= "{:autorun true, :ns-regexp \"^(repro\\\\.core-test)$\"}"
                                   (peek argv)))))))
  (it "narrows to nothing when no namespace was selected"
      (with-project cljs-project
                    (fn [root]
                      (expect (= "{:autorun true}"
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
