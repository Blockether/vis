(ns hooks.com.blockether.vis.with-own-sandbox
  "clj-kondo hook for `with-own` / `with-own-env` (`test-python-context`).

   Real shape:

       (with-own [ctx bindings roots-fn network-opts] & body)

   The vector is NOT a `let` binding vector: the head is the symbol the sandbox
   is bound to and the tail is `create-python-context`'s arguments. Linting it
   as a `let` reads the second argument as a value and the third as a binding
   FORM, so every call that states its own roots is an error — which is every
   call that needs a jail.

   Rewrite it into `(let [sym nil] arg... body...)`: the symbol resolves in the
   body, and the arguments are still linted as ordinary expressions."
  (:require [clj-kondo.hooks-api :as api]))

(defn- rewrite
  [{:keys [node]}]
  (let [[_op binding-node & body] (:children node)]
    (when (api/vector-node? binding-node)
      (let [[sym & args] (:children binding-node)]
        {:node (api/list-node
                 (list* (api/token-node 'let)
                        (api/vector-node [sym (api/token-node nil)])
                        (concat args body)))}))))

(def with-own rewrite)
(def with-own-env rewrite)
