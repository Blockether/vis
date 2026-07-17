(ns com.blockether.vis.ext.language-clojure.test-runner-test
  "Unit tests for the in-REPL test path's failure handling — specifically that a
   server that vanishes mid-run surfaces as a structured result the model can act
   on, never a raw connect exception that eats the turn."
  (:require [com.blockether.vis.ext.language-clojure.nrepl-client :as nc]
            [com.blockether.vis.ext.language-clojure.test-runner]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private run-via-repl @#'com.blockether.vis.ext.language-clojure.test-runner/run-via-repl)

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
                     (expect (re-find #"repl_start" (get r "error")))
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
  @#'com.blockether.vis.ext.language-clojure.test-runner/recover-if-unusable)

(defdescribe
  recover-if-unusable-test
  (it "runs the CLI suite and restarts the nREPL when the server was unusable"
      (let [restarted (atom nil)]
        (with-redefs [com.blockether.vis.ext.language-clojure.test-runner/restart-repl-async!
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
      (let [restarted
            (atom nil)

            cli-called
            (atom false)]

        (with-redefs [com.blockether.vis.ext.language-clojure.test-runner/restart-repl-async!
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
        (with-redefs [com.blockether.vis.ext.language-clojure.test-runner/restart-repl-async!
                      (fn [& _]
                        (reset! restarted true)
                        nil)]
          (let [orig {"mode" "repl" "pass" 5}
                r (recover-if-unusable "sid" "/proj" {} orig)]

            (expect (= orig r))
            (expect (false? @restarted)))))))
