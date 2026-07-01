(ns hooks.com.blockether.vis.import-vars
  "clj-kondo hooks for `com.blockether.vis.internal.import/{import-var,import-vars}`.

   Those macros re-export public vars from other namespaces under the current
   namespace (a dependency-free stand-in for potemkin's `import-vars`). clj-kondo
   never evaluates code, so without a hook every re-exported alias shows up as an
   `Unresolved var: <facade>/<alias>` warning at each call site across the repo.

   Strategy
   ========

   Rewrite each import into a synthetic `(def alias src)`. That gives clj-kondo:
     - a real `def` to register, so `facade/alias` resolves at every call site
     - a usage of the source var, so the source stays 'used'

   `(import-var alias src)`      -> (def alias src)
   `(import-vars spec ...)`      -> (do (def a1 s1) (def a2 s2) ...)
     where each spec is either `ns/name` (alias = short name `name`) or an
     explicit `[alias ns/qualified]` pair."
  (:require [clj-kondo.hooks-api :as api]))

(defn- ignore-meta
  "Attach `:clj-kondo/ignore [:unused-public-var]` metadata to a node so the
   synthetic re-export def is never flagged as an unused public var (the whole
   point of the facade is to be consumed elsewhere)."
  [node]
  (with-meta node (assoc (meta node) :clj-kondo/ignore [:unused-public-var])))

(defn- def-node
  "Build a synthetic `(def alias src)` from an alias symbol node + a src node.
   A plain 2-arg def keeps clj-kondo happy (no `Too many arguments to def`)
   while still registering `alias` and marking `src` as used."
  [alias-node src-node]
  (api/list-node
   [(api/token-node 'def)
    (ignore-meta alias-node)
    src-node]))

(defn- spec->def
  "Turn one import spec node into a `(def alias src)` node, or nil if unusable."
  [spec]
  (cond
    (api/vector-node? spec)
    (let [[alias-node src-node] (:children spec)]
      (when (and alias-node src-node)
        (def-node alias-node src-node)))

    (api/token-node? spec)
    (let [sym (api/sexpr spec)]
      (when (symbol? sym)
        (def-node (api/token-node (symbol (name sym))) spec)))))

(defn import-var
  [{:keys [node]}]
  (let [[_op alias-node src-node] (:children node)]
    (when (and alias-node src-node)
      {:node (def-node alias-node src-node)})))

(defn import-vars
  [{:keys [node]}]
  (let [specs (rest (:children node))
        defs  (keep spec->def specs)]
    {:node (api/list-node (cons (api/token-node 'do) defs))}))
