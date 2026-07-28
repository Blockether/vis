(ns com.blockether.vis.internal.import
  "In-house `import-vars` — re-export public vars from other namespaces under the
   current namespace, carrying their :doc/:arglists so the facade reads like the
   originals and `doc`/editor help work on a re-export. A tiny, dependency-free
   stand-in for potemkin's import-vars (the project keeps no separate lib for it).

   Value semantics are identical to a plain `(def alias src)`: the alias captures
   the source var's value at load time — it re-exports a fn/const, it does NOT
   track later redefinitions, and (like a def-alias always did) it is not a
   `binding`-rebindable handle for a dynamic source var.")

(defmacro import-var
  "Define `alias` in the current namespace as a re-export of `src` (a
   fully-qualified, in-scope symbol). Copies the value and merges the source
   var's :doc/:arglists onto the alias var. Returns the new var."
  [alias src]
  `(let
     [sv#
      (var ~src)

      v#
      (def ~alias ~src)]

     (alter-meta! v# merge (select-keys (meta sv#) [:doc :arglists]))
     v#))

(defmacro import-vars
  "Re-export many vars in one block. Each spec is either a fully-qualified symbol
   (the alias is its short name) or an explicit `[alias fully/qualified]` pair:

     (import-vars
       cancellation/cancel!                    ;; -> alias `cancel!`
       [worker-runtime cancellation/worker-runtime])

   Replaces a wall of hand-written `(def alias ns/src)` lines with one declarative
   list — same value semantics, plus the source doc/arglists carried across."
  [& specs]
  `(do ~@(for [spec specs]
           (if (vector? spec)
             `(import-var ~(first spec) ~(second spec))
             `(import-var ~(symbol (name spec)) ~spec)))
       nil))
