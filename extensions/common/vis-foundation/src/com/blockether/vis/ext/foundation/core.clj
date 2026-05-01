(ns com.blockether.vis.ext.foundation.core
  "vis-foundation aggregator. ONE extension under ONE alias `v/`,
   bundling the agent's read + write + introspect + answer-build
   surface so the model never has to switch alias-namespaces in the
   middle of a form. Four sources, each contributing its
   `:ext/symbols` vec and its share of the merged `:ext/prompt`:

     introspection.clj          (v/inspect, v/report,
                                 v/extensions, v/extension-doc, …)
     editing/core.clj           (v/cat, v/ls, v/rg, v/bash,
                                 thin babashka.fs wrappers)
     markdown.clj               (v/h1, v/p, v/table,
                                 v/file-link, v/join, …)
     environment/core.clj       (v/snapshot,
                                 v/git,
                                 v/languages,
                                 v/monorepo,
                                 v/render,
                                 v/refresh!)

   Plus a live `<environment>` block in the system prompt, owned by
   `environment/core.clj`'s `environment-prompt`.

   The legacy `fs/` alias was dropped, but the useful babashka.fs
   surface is now inlined back under `v/` as thin cwd-safe wrappers
   (`v/read-all-lines`, `v/write-lines`, `v/update-file`, `v/glob`,
   `v/list-dir`, ...). The markdown builders were merged too, so the
   model now uses `(answer (v/join ...))`, `(v/h1 ...)`,
   `(v/file-link ...)`, etc. through the same `v/` prefix. One short
   alias, normal Clojure code, no bespoke edit DSLs, no alias
   switching mid-iteration."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.doctor :as doctor]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.ext.foundation.environment.core :as environment]
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [com.blockether.vis.ext.foundation.markdown :as markdown]
   [com.blockether.vis.ext.foundation.transcript :as transcript]))

(defn- combined-prompt
  "Stitch the per-area prompt fragments together. The environment
   fragment is a fn (it renders the live snapshot every time the
   system prompt is assembled); introspection + editing + markdown
   fragments are static strings that we concatenate directly."
  [env]
  (str (environment/environment-prompt env)
    "\n\n"
    introspection/introspection-prompt
    "\n\n"
    editing/editing-prompt
    "\n\n"
    markdown/markdown-prompt))

(def vis-extension
  (vis/extension
    {:ext/namespace      'com.blockether.vis.ext.foundation.core
     :ext/doc            "Foundation `v/`: inspect/report, file I/O, bash, markdown answer builders (h1/p/table/file-link/join/code-block/details), env snapshot, project guidance, skills, scan warnings, doctor/report CLI."
     :ext/version        "0.7.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/ns-alias       {:ns 'vis.ext.v :alias 'v}
     :ext/kind           "foundation"
     :ext/prompt         combined-prompt
     :ext/symbols        (vec (concat introspection/all-symbols
                                editing/editing-symbols
                                markdown/markdown-symbols
                                environment/environment-symbols))
     :ext/doctor-check-fn doctor/check-fn}))

(vis/register-extension! vis-extension)

;; Register the top-level `vis doctor` CLI command. Foundation owns
;; this now (lifted out of `internal/main.clj`'s built-ins) — see
;; plan §1 Q18. Direct `register-cmd!` (NOT `:ext/cli`) because the
;; command must live at the top of the tree, not under `vis extensions`.
(doctor/register-cli!)

;; Register the top-level `vis report <CONVERSATION-ID>` CLI command.
;; Same pattern as `vis doctor` — the data + Markdown renderer + CLI
;; surface are one cohesive feature owned by foundation, not host
;; plumbing.
(transcript/register-cli!)
