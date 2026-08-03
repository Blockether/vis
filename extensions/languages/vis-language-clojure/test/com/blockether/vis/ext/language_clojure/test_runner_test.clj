(ns com.blockether.vis.ext.language-clojure.test-runner-test
  "Unit tests for the in-REPL test path's failure handling — specifically that a
   server that vanishes mid-run surfaces as a structured result the model can act
   on, never a raw connect exception that eats the turn."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nc]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [com.blockether.vis.ext.language-clojure.test-runner :as tr]
            [com.blockether.vis.internal.extension :as extension]
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

(defdescribe
  recover-if-unusable-test
  (it "runs the CLI suite and relaunches the nREPL when the server was unusable"
      (let [relaunched (atom nil)]
        (with-redefs
          [com.blockether.vis.ext.language-clojure.test-runner/relaunch-repl-async!
           (fn [_sid dir]
             (reset! relaunched dir)
             nil)
           com.blockether.vis.ext.language-clojure.test-runner/run-via-cli
           (fn [_root _norm]
             {"mode" "cli" "is_pass" true "note" "7 cases"})]

          (let [r (recover-if-unusable "sid" "/proj" {} {"repl_unusable" true "error" "down"})]
            (expect (= "/proj" @relaunched))
            (expect (= "cli" (get r "mode")))
            (expect (true? (get r "recovered")))
            (expect (re-find #"ran the suite via CLI" (get r "note")))
            (expect (re-find #"7 cases" (get r "note")))))))
  (it "relaunches the nREPL but keeps the timeout error for a wedged eval (no CLI)"
      (let
        [relaunched
         (atom nil)

         cli-called
         (atom false)]

        (with-redefs
          [com.blockether.vis.ext.language-clojure.test-runner/relaunch-repl-async!
           (fn [_sid dir]
             (reset! relaunched dir)
             nil)

           com.blockether.vis.ext.language-clojure.test-runner/run-via-cli
           (fn [_root _norm]
             (reset! cli-called true)
             {})]

          (let [r (recover-if-unusable "sid" "/proj" {} {"repl_wedged" true "error" "timed out"})]
            (expect (= "/proj" @relaunched))
            (expect (false? @cli-called))
            (expect (re-find #"background" (get r "error")))))))
  (it "passes a healthy result through untouched (no relaunch, no CLI)"
      (let [relaunched (atom false)]
        (with-redefs
          [com.blockether.vis.ext.language-clojure.test-runner/relaunch-repl-async!
           (fn [& _]
             (reset! relaunched true)
             nil)]
          (let
            [orig {"mode" "repl" "pass" 5}
             r (recover-if-unusable "sid" "/proj" {} orig)]

            (expect (= orig r))
            (expect (false? @relaunched)))))))

(defdescribe
  group-faults-by-cwd-test
  "`group-faults-by-cwd` folds the flat failures/errors vectors into the same
   directory-nested `by-cwd` shape lint and format expose — the file's dir written
   once, basename inner, failures/errors kinds separated, edge files handled."
  (it "nests faults by directory then basename, writing each dir prefix once"
      (let
        [f1
         {"ns" "a.core" "test" "adds" "file" "src/a/core.clj" "line" 12}

         f2
         {"ns" "a.core" "test" "subs" "file" "src/a/core.clj" "line" 20}

         f3
         {"ns" "a.util" "test" "trim" "file" "src/a/util.clj" "line" 3}

         e1
         {"ns" "a.core" "test" "boom" "file" "src/a/core.clj" "line" 99}

         grouped
         (tr/group-faults-by-cwd [f1 f2 f3] [e1])]

        (expect (= #{"src/a"} (set (keys grouped))))
        (expect (= #{"core.clj" "util.clj"} (set (keys (get grouped "src/a")))))
        (expect (= [f1 f2] (get-in grouped ["src/a" "core.clj" "failures"])))
        (expect (= [e1] (get-in grouped ["src/a" "core.clj" "errors"])))
        (expect (= [f3] (get-in grouped ["src/a" "util.clj" "failures"])))
        (expect (nil? (get-in grouped ["src/a" "util.clj" "errors"])))))
  (it "buckets a bare JVM frame (no parent dir) under \".\" by its basename"
      (let
        [e
         {"ns" "a.core" "file" "Numbers.java" "line" 7}

         grouped
         (tr/group-faults-by-cwd [] [e])]

        (expect (= [e] (get-in grouped ["." "Numbers.java" "errors"])))))
  (it "buckets a fileless fault under \".\"/\"<unknown>\""
      (let
        [f
         {"ns" "a.core" "test" "nofile"}

         grouped
         (tr/group-faults-by-cwd [f] [])]

        (expect (= [f] (get-in grouped ["." "<unknown>" "failures"])))))
  (it "treats a blank file string as fileless"
      (let
        [f
         {"ns" "a.core" "file" "   "}

         grouped
         (tr/group-faults-by-cwd [f] [])]

        (expect (= [f] (get-in grouped ["." "<unknown>" "failures"])))))
  (it "returns an empty map when there is nothing to group"
      (expect (= {} (tr/group-faults-by-cwd [] [])))))

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
                       "line" -1}]
          "errors" [{"ns" "a.core-test"
                     "test" "bang"
                     "type" "error"
                     "message" "nope"
                     "file" "NO_SOURCE_PATH"
                     "line" -2}]}

         normalized
         (normalize-faults "." parsed)]

        (expect (nil? (get-in normalized ["failures" 0 "file"])))
        (expect (nil? (get-in normalized ["failures" 0 "line"])))
        (expect (nil? (get-in normalized ["errors" 0 "file"])))
        (expect (nil? (get-in normalized ["errors" 0 "line"])))
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

(defdescribe
  clj-test-fn-cwd-root-test
  "An explicit `cwd` arg roots the run — and thus nREPL selection — at THAT
   project, so tests in a sibling / added-folder project run against their own
   nREPL classpath instead of booting the workspace-root REPL (regression:
   run_tests silently dropped `cwd`, FileNotFoundException on the wrong REPL)."
  (it "boots/selects the nREPL at the cwd arg, not the workspace root"
      (let [seen-root (atom nil)]
        (with-redefs
          [extension/run-outside-tool-wall (fn [_env thunk]
                                             (thunk))
           repl-manager/ensure-repl-for-dir! (fn [_sid root]
                                               (reset! seen-root root)
                                               {:port 12345})
           com.blockether.vis.ext.language-clojure.test-runner/run-via-repl
           (fn [root _nses _sel _port]
             {"mode" "repl" "ns" "x.y-test" "root" root})]

          (let
            [r (:result (tr/clj-test-fn {:workspace/root "/ws" :session-id "sid"}
                                        {"cwd" "/some/proj" "namespaces" ["x.y-test"]}))]
            (expect (= "/some/proj" @seen-root))
            (expect (= "/some/proj" (get r "root")))))))
  (it "falls back to the workspace root when no cwd arg is given"
      (let [seen-root (atom nil)]
        (with-redefs
          [extension/run-outside-tool-wall (fn [_env thunk]
                                             (thunk))
           repl-manager/ensure-repl-for-dir! (fn [_sid root]
                                               (reset! seen-root root)
                                               {:port 12345})
           com.blockether.vis.ext.language-clojure.test-runner/run-via-repl
           (fn [root _nses _sel _port]
             {"mode" "repl" "ns" "x.y-test" "root" root})]

          (let
            [r (:result (tr/clj-test-fn {:workspace/root "/ws" :session-id "sid"}
                                        {"namespaces" ["x.y-test"]}))]
            (expect (= "/ws" @seen-root))
            (expect (= "/ws" (get r "root"))))))))

(defdescribe
  clj-test-fn-path-discovery-test
  (it
    "treats empty namespaces from the facade as absent when paths are present"
    (let
      [root-file
       (.toFile (java.nio.file.Files/createTempDirectory
                  "vis-clj-test"
                  (make-array java.nio.file.attribute.FileAttribute 0)))

       test-dir
       (io/file root-file "test/com/example")

       test-file
       (io/file test-dir "thing_test.clj")

       seen-nses
       (atom nil)]

      (try (.mkdirs test-dir)
           (spit test-file "(ns com.example.thing-test)\n")
           (with-redefs
             [extension/run-outside-tool-wall
              (fn [_env thunk]
                (thunk))

              repl-manager/ensure-repl-for-dir!
              (fn [_sid _root]
                {:port 12345})

              com.blockether.vis.ext.language-clojure.test-runner/run-via-repl
              (fn [_root nses _sel _port]
                (reset! seen-nses nses)
                {"mode" "repl" "ns" (first nses)})]

             (tr/clj-test-fn
               {:workspace/root (.getPath root-file) :session-id "sid"}
               {"paths" ["test/com/example"] "namespaces" [] "only" [] "include" [] "exclude" []})
             (expect (= ["com.example.thing-test"] @seen-nses)))
           (finally (doseq [f (reverse (file-seq root-file))]
                      (io/delete-file f true)))))))

(defdescribe
  only-miss-output-test
  (it
    "reports a bounded selector miss without dumping namespaces or vars"
    (let
      [fixture-ns
       'vis.test-runner-only-miss-fixture

       n
       (create-ns fixture-ns)

       run-form
       @#'com.blockether.vis.ext.language-clojure.test-runner/run-form]

      (try
        (doseq [test-name '[first-test second-test]]
          (alter-meta! (intern n
                               test-name
                               (fn []))
                       assoc
                       :test
                       (fn [])))
        (with-redefs [clojure.core/require (fn [& _])]
          (let
            [result ((eval run-form) [fixture-ns] {:only ["missing-test"]} {})
             error (get result "error")]

            (expect
              (=
                "only [\"missing-test\"] matched no test vars (searched 2 test vars across 1 namespace)"
                error))
            (expect (not (re-find #"first-test|second-test|vis\\.test-runner" error)))))
        (finally (remove-ns fixture-ns))))))
