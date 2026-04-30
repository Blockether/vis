(ns com.blockether.vis.ext.foundation.core
  "vis-foundation aggregator. ONE extension under ONE alias `v/`,
   bundling the agent's read + write + introspect surface so the
   model never has to switch alias-namespaces in the middle of a
   form. Three sources, each contributing its `:ext/symbols` vec
   and its share of the merged `:ext/prompt`:

     introspection.clj          (v/turn, v/conversation,
                                 v/diagnose, v/var-history,
                                 v/find-attempts, v/failures,
                                 v/extensions, v/extension-doc, \u2026)
     editing/core.clj          (v/cat, v/ls, v/rg, v/edit,
                                 v/write)
     environment/core.clj       (v/snapshot,
                                 v/git,
                                 v/languages,
                                 v/monorepo,
                                 v/render,
                                 v/refresh!)\n
   Plus a live `<environment>` block in the system prompt, owned by
   `environment/core.clj`'s `environment-prompt`.\n
   The legacy `fs/` alias (babashka.fs path-math wrappers) was\n   dropped: the `v/` editing tools resolve every path internally\n   against `(fs/cwd)` already, and the model has no need to do path\n   math directly inside the sandbox. If it ever does, the host's\n   `clojure.java.io` namespace is still available."
  (:require
   [com.blockether.vis.core :as sdk]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.ext.foundation.environment.core :as environment]
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   ;; Side-effect require: registers the `md/` extension. The aggregator
   ;; does not pull markdown symbols into its own `:ext/symbols` vec —
   ;; markdown ships as a sibling extension under its own alias so
   ;; `(md/h1 …)` lives in `vis.ext.md` instead of mingling with `v/`.
   [com.blockether.vis.ext.foundation.markdown]))

(defn- combined-prompt
  "Stitch the per-area prompt fragments together. The environment
   fragment is a fn (it renders the live snapshot every time the
   system prompt is assembled); introspection + editing fragments
   are static strings that we concatenate directly."
  [env]
  (str (environment/environment-prompt env)
    "\n\n"
    introspection/introspection-prompt
    "\n\n"
    editing/editing-prompt))

(def vis-extension
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.foundation.core
     :ext/doc       "Foundation extension. ONE alias (`v/`) bundling introspection (turn / conversation / diagnose / failures / var-history / find-attempts / extensions catalog), file I/O (cat / ls / rg / edit / write), and environment awareness (snapshot / git / languages / monorepo). Owns the `<environment>` block in the system prompt."
     :ext/version   "0.7.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.v :alias 'v}
     :ext/kind      "foundation"
     :ext/prompt    combined-prompt
     :ext/symbols   (vec (concat introspection/all-symbols
                           editing/editing-symbols
                           environment/environment-symbols))}))

(sdk/register-extension! vis-extension)
