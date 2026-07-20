(ns com.blockether.vis.ext.language-clojure.test-runner-test
  "Unit tests for the in-REPL test path's failure handling — specifically that a
   server that vanishes mid-run surfaces as a structured result the model can act
   on, never a raw connect exception that eats the turn."
  (:require [com.blockether.vis.ext.language-clojure.nrepl-client :as nc]
            [com.blockether.vis.ext.language-clojure.test-runner :as tr]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private run-via-repl @#'com.blockether.vis.ext.language-clojure.test-runner/run-via-repl)

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
                     (expect (re-find #"repl_start" (get r "error")))
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
  (it "runs the CLI suite and restarts the nREPL when the server was unusable"
      (let [restarted (atom nil)]
        (with-redefs
          [com.blockether.vis.ext.language-clojure.test-runner/restart-repl-async!
           (fn [_sid dir]
             (reset! restarted dir)
             nil)
           com.blockether.vis.ext.language-clojure.test-runner/run-via-cli
           (fn [_root _norm]
             {"mode" "cli" "is_pass" true "note" "7 cases"})]

          (let [r (recover-if-unusable "sid" "/proj" {} {"repl_unusable" true "error" "down"})]
            (expect (= "/proj" @restarted))
            (expect (= "cli" (get r "mode")))
            (expect (true? (get r "recovered")))
            (expect (re-find #"ran the suite via CLI" (get r "note")))
            (expect (re-find #"7 cases" (get r "note")))))))
  (it "restarts the nREPL but keeps the timeout error for a wedged eval (no CLI)"
      (let
        [restarted
         (atom nil)

         cli-called
         (atom false)]

        (with-redefs
          [com.blockether.vis.ext.language-clojure.test-runner/restart-repl-async!
           (fn [_sid dir]
             (reset! restarted dir)
             nil)

           com.blockether.vis.ext.language-clojure.test-runner/run-via-cli
           (fn [_root _norm]
             (reset! cli-called true)
             {})]

          (let [r (recover-if-unusable "sid" "/proj" {} {"repl_wedged" true "error" "timed out"})]
            (expect (= "/proj" @restarted))
            (expect (false? @cli-called))
            (expect (re-find #"background" (get r "error")))))))
  (it "passes a healthy result through untouched (no restart, no CLI)"
      (let [restarted (atom false)]
        (with-redefs
          [com.blockether.vis.ext.language-clojure.test-runner/restart-repl-async!
           (fn [& _]
             (reset! restarted true)
             nil)]
          (let
            [orig {"mode" "repl" "pass" 5}
             r (recover-if-unusable "sid" "/proj" {} orig)]

            (expect (= orig r))
            (expect (false? @restarted)))))))

(defdescribe
  group-faults-by-dir-test
  "`group-faults-by-dir` folds the flat failures/errors vectors into the same
   directory-nested `by-dir` shape lint and format expose — the file's dir written
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
         (tr/group-faults-by-dir [f1 f2 f3] [e1])]

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
         (tr/group-faults-by-dir [] [e])]

        (expect (= [e] (get-in grouped ["." "Numbers.java" "errors"])))))
  (it "buckets a fileless fault under \".\"/\"<unknown>\""
      (let
        [f
         {"ns" "a.core" "test" "nofile"}

         grouped
         (tr/group-faults-by-dir [f] [])]

        (expect (= [f] (get-in grouped ["." "<unknown>" "failures"])))))
  (it "treats a blank file string as fileless"
      (let
        [f
         {"ns" "a.core" "file" "   "}

         grouped
         (tr/group-faults-by-dir [f] [])]

        (expect (= [f] (get-in grouped ["." "<unknown>" "failures"])))))
  (it "returns an empty map when there is nothing to group"
      (expect (= {} (tr/group-faults-by-dir [] [])))))
