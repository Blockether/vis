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
     editing/core.clj          (v/cat, v/ls, v/rg,
                                 thin babashka.fs wrappers)
     environment/core.clj       (v/snapshot,
                                 v/git,
                                 v/languages,
                                 v/monorepo,
                                 v/render,
                                 v/refresh!)\n
   Plus a live `<environment>` block in the system prompt, owned by
   `environment/core.clj`'s `environment-prompt`.\n
   The legacy `fs/` alias was dropped, but the useful babashka.fs\n   surface is now inlined back under `v/` as thin cwd-safe wrappers\n   (`v/read-all-lines`, `v/write-lines`, `v/update-file`, `v/glob`,\n   `v/list-dir`, ...). The model still gets one short alias, but it\n   acts through normal Clojure code instead of bespoke edit DSLs."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.doctor :as doctor]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.ext.foundation.environment.core :as environment]
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [com.blockether.vis.ext.foundation.transcript :as transcript]
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
  (vis/extension
    {:ext/namespace      'com.blockether.vis.ext.foundation.core
     :ext/doc            "Foundation extension. ONE alias (`v/`) bundling introspection (turn / conversation / diagnose / failures / var-history / find-attempts / extensions catalog), file I/O (cat / ls / rg plus thin babashka.fs wrappers like read-all-lines / write-lines / update-file / glob / list-dir), and environment awareness (snapshot / git / languages / monorepo / project-guidance / skills). Owns the `<environment>`, `<project-guidance>`, `<skills>`, `<scan-warnings>` blocks in the system prompt and the `vis doctor` CLI command."
     :ext/version        "0.7.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/ns-alias       {:ns 'vis.ext.v :alias 'v}
     :ext/kind           "foundation"
     :ext/prompt         combined-prompt
     :ext/symbols        (vec (concat introspection/all-symbols
                                [transcript/transcript-symbol]
                                editing/editing-symbols
                                environment/environment-symbols))
     :ext/doctor-check-fn doctor/check-fn}))

(vis/register-extension! vis-extension)

;; Register the top-level `vis doctor` CLI command. Foundation owns
;; this now (lifted out of `internal/main.clj`'s built-ins) — see
;; plan §1 Q18. Direct `register-cmd!` (NOT `:ext/cli`) because the
;; command must live at the top of the tree, not under `vis extensions`.
(doctor/register-cli!)

;; Register the top-level `vis diagnose <CONVERSATION-ID>` CLI
;; command. Same pattern as `vis doctor` — the data + Markdown
;; renderer + CLI surface are one cohesive feature owned by
;; foundation, not host plumbing.
(transcript/register-cli!)
