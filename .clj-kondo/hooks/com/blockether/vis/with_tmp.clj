(ns hooks.com.blockether.vis.with-tmp
  "clj-kondo hook for `with-tmp`-style fixture macros in extension
   tests.

   Real shape:

       (defmacro with-tmp [sym & body]
         `(let [tmp# (fs/create-temp-dir ...)
                ~sym (->root tmp#)]
            (try ~@body (finally (fs/delete-tree tmp#)))))

   That binds `sym` to a new path inside the macro body. clj-kondo
   never evaluates code, so it can't see the binding - every
   reference to `sym` inside the call lights up as an
   unresolved-symbol error.

   Rewrite the call into the equivalent of `(let [sym nil] body...)`
   so clj-kondo sees `sym` as a normal lexical binding and the body
   typechecks against it."
  (:require [clj-kondo.hooks-api :as api]))

(defn with-tmp
  [{:keys [node]}]
  (let [[_op sym-node & body] (:children node)]
    (when (api/token-node? sym-node)
      {:node
       (api/list-node
         (concat
           [(api/token-node 'clojure.core/let)
            (api/vector-node [sym-node (api/token-node 'nil)])]
           body))})))
