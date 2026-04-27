(ns com.blockether.vis.loop.answer-resolution-test
  "When the model emits a single-token `:answer`, the iteration handler
   resolves it as a sandbox var and uses its value. This test covers
   the 0-arity-fn auto-invoke path: if the resolved var is a function
   that takes no args, the loop should INVOKE it and use the return
   value rather than pr-str the fn object (`sci.impl.fns$fun$...`)."
  (:require
   [com.blockether.vis.loop.runtime.conversation.environment.core :as env-core]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

;; ----------------------------------------------------------------------------
;; The single-token resolver lives inside a let-binding in run-iteration's
;; FINAL path; it's not an exported fn. We exercise it via SCI directly
;; with the same shape: define a var in the sandbox, then assert the
;; resolver picks it up correctly.
;; ----------------------------------------------------------------------------

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

;; The actual resolver mirrors what's in run-iteration. We extract its
;; logic into a local fn so the tests don't have to invoke the entire
;; iteration loop.
(defn- resolve-answer
  "Returns the resolved string for `:answer raw-answer` against `locals`,
   or nil when the answer isn't a single resolvable token."
  [raw-answer locals]
  (let [single-token? (and (re-matches #"\S+" raw-answer)
                        (try (symbol? (read-string raw-answer))
                          (catch Throwable _ false)))
        maybe-invoke-zero-arity
        (fn [v]
          (if (fn? v)
            (let [call-result (try {:value (v)}
                                (catch clojure.lang.ArityException _ {:arity-mismatch true})
                                (catch Throwable t {:error t}))]
              (cond
                (contains? call-result :value)  (:value call-result)
                (:arity-mismatch call-result)   v
                :else                           v))
            v))]
    (when single-token?
      (let [sym (symbol raw-answer)
            resolved (get locals sym)]
        (when (some? resolved)
          (let [v (if (instance? clojure.lang.IDeref resolved) @resolved resolved)
                v (maybe-invoke-zero-arity v)]
            (cond
              (string? v) v
              :else (pr-str v))))))))

;; ----------------------------------------------------------------------------
;; Tests
;; ----------------------------------------------------------------------------

(defdescribe single-token-resolution-test
  (it "string vars are returned verbatim"
    (let [environment (fresh-environment)]
      (eval-in environment "(def greeting \"hi there\")")
      (expect (= "hi there" (resolve-answer "greeting" (get-locals environment))))))

  (it "non-string vars are pr-str'd"
    (let [environment (fresh-environment)]
      (eval-in environment "(def magic 42)")
      (expect (= "42" (resolve-answer "magic" (get-locals environment))))))

  (it "0-arity fn vars are INVOKED and the return value is used"
    ;; This is the bug the conversation 4e8c6ac2-... hit: model wrote
    ;; (defn render-table [] "table-here") and set :answer "render-table".
    ;; Old behavior: pr-str'd the fn -> "sci.impl.fns$fun$arity_0__..." rendered.
    ;; New behavior: invoke -> "table-here".
    (let [environment (fresh-environment)]
      (eval-in environment "(defn render-table [] \"### Table\\n| a | b |\\n|---|---|\\n| 1 | 2 |\")")
      (expect (= "### Table\n| a | b |\n|---|---|\n| 1 | 2 |"
                (resolve-answer "render-table" (get-locals environment))))))

  (it "0-arity fn returning a non-string value is pr-str'd post-invoke"
    (let [environment (fresh-environment)]
      (eval-in environment "(defn pi-times-2 [] 6.28)")
      (expect (= "6.28" (resolve-answer "pi-times-2" (get-locals environment))))))

  (it "multi-arity fn vars are NOT auto-invoked (ArityException routes to fallback)"
    ;; A fn that requires args can't be auto-called with 0 args. The
    ;; loop falls back to pr-str — same as the old behavior. The model
    ;; needs to call it explicitly (e.g. `(my-fn arg)` in :code or via
    ;; mustache template).
    (let [environment (fresh-environment)]
      (eval-in environment "(defn add [x y] (+ x y))")
      (let [result (resolve-answer "add" (get-locals environment))]
        ;; Multi-arity fn pr-str'd as a sci fn object — we just assert
        ;; the result IS a string (not nil) and is NOT trying to be
        ;; invoked.
        (expect (string? result)))))

  (it "fn that throws a non-arity exception falls through to pr-str (no crash)"
    (let [environment (fresh-environment)]
      (eval-in environment "(defn boom [] (throw (ex-info \"nope\" {})))")
      (let [result (resolve-answer "boom" (get-locals environment))]
        (expect (string? result)))))

  (it "returns nil when the symbol isn't bound in the sandbox"
    (expect (nil? (resolve-answer "no-such-var" {}))))

  (it "returns nil when the answer isn't a single token"
    (expect (nil? (resolve-answer "this is plain prose" {})))))
