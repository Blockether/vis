(ns hooks.com.blockether.vis.defdelegate
  "clj-kondo hook for `com.blockether.vis.core/defdelegate`.

   The macro generates a `defn` that resolves a backend var at call
   time and applies it. clj-kondo can't expand the runtime resolution
   (it never evaluates code), so without this hook every `defdelegate`
   call lights up as an unresolved-symbol error.

   Strategy
   ========

   Rewrite the call into a synthetic `(defn sym arglist (do args...))`.
   That gives clj-kondo:
     - a real `defn` to register so callers resolve
     - the right arity from the arglist
     - argument references in the body so `unused-binding` stays quiet

   The body is just `(do arg1 arg2 ...)` - the implementation lives at
   runtime (resolved dynamically), so the lint pass focuses on the
   declared shape and skips the body entirely."
  (:require [clj-kondo.hooks-api :as api]))

;; Attr-map slot for the synthetic `defn`. clj-kondo treats anything
;; under `:clj-kondo/ignore` as a per-form linter suppression list, so
;; even an editor running single-file analysis with `:unused-public-var`
;; turned on never flags a delegated fn as unused. `:export true` is
;; the historical equivalent and is also respected.
(def ^:private attr-map-node
  (api/map-node
    [(api/keyword-node :clj-kondo/ignore)
     (api/vector-node [(api/keyword-node :unused-public-var)])
     (api/keyword-node :export)
     (api/token-node true)]))

(defn defdelegate
  [{:keys [node]}]
  (let [[_op sym-node arglist-node] (:children node)]
    (when (and (api/token-node? sym-node)
            (api/vector-node? arglist-node))
      (let [arg-tokens (filter api/token-node? (:children arglist-node))
            body       (api/list-node
                         (cons (api/token-node 'do) arg-tokens))]
        {:node
         (api/list-node
           [(api/token-node 'clojure.core/defn)
            sym-node
            attr-map-node
            arglist-node
            body])}))))
