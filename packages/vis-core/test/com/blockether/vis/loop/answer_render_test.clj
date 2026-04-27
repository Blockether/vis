(ns com.blockether.vis.loop.answer-render-test
  "Final `:answer` rendering. ONE mode: every answer is a Mustache
   template against sandbox vars. Plain text / markdown without
   `{{...}}` tags renders verbatim. No `:answer-type`, no auto-detect,
   no SCI eval at finalize time -- if the model wants computation,
   it does it in :code and references the result via `{{var}}`.

   The render call site lives inside a `let` in
   `iteration.core/run-iteration`'s FINAL path. We exercise the same
   primitive (`mustache/render`) against a freshly-built sandbox so
   the test is independent of the LLM round-trip."
  (:require
   [com.blockether.vis.loop.mustache :as mustache]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as env-core]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

(defn- fresh-environment []
  (env-core/create-sci-context nil))

(defn- eval-in [{:keys [sci-ctx]} source]
  (:val (sci/eval-string+ sci-ctx source
          {:ns (sci/find-ns sci-ctx 'sandbox)})))

(defn- get-locals [{:keys [sci-ctx initial-ns-keys]}]
  (let [sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox])]
    (persistent!
      (reduce-kv (fn [acc k v]
                   (if (or (contains? initial-ns-keys k) (keyword? k))
                     acc
                     (assoc! acc k (if (instance? clojure.lang.IDeref v) @v v))))
        (transient {}) sandbox))))

(defn- render [environment raw-answer]
  (mustache/render raw-answer (get-locals environment)))

(defdescribe answer-render-test

  (it "passes plain prose through unchanged"
    (let [environment (fresh-environment)]
      (expect (= "Done. The cache is now warm."
                (render environment "Done. The cache is now warm.")))))

  (it "passes markdown through unchanged"
    (let [environment (fresh-environment)
          markdown    "## Summary\n- Patched 3 files\n- All tests green"]
      (expect (= markdown (render environment markdown)))))

  (it "interpolates sandbox vars via {{var}}"
    (let [environment (fresh-environment)]
      (eval-in environment "(def hits 12)")
      (eval-in environment "(def files 3)")
      (expect (= "Found 12 hits across 3 files."
                (render environment "Found {{hits}} hits across {{files}} files.")))))

  (it "supports computed answers via def + {{var}}"
    ;; The model defs the computed value in :code, then references
    ;; it from :answer. No SCI eval at finalize time; just Mustache.
    (let [environment (fresh-environment)]
      (eval-in environment "(def summary (clojure.string/join \"\\n\" [\"a\" \"b\" \"c\"]))")
      (expect (= "a\nb\nc" (render environment "{{summary}}")))))

  (it "iterates collections via Mustache sections"
    (let [environment (fresh-environment)]
      (eval-in environment "(def rows [\"r1\" \"r2\" \"r3\"])")
      (expect (= "r1\nr2\nr3\n"
                (render environment "{{#rows}}{{.}}\n{{/rows}}")))))

  (it "raises on a missing var so the iteration handler can surface it"
    (let [environment (fresh-environment)]
      (expect (try
                (render environment "Hi {{undefined-var}}")
                false
                (catch Exception _ true))))))
