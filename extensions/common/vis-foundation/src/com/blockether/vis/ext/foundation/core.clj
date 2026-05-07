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

   Plus a live `<environment>` block in the system prompt, contributed
   through `environment/core.clj`'s `environment-info` hook.

   The separate `fs/` alias was dropped, but the useful babashka.fs
   surface is now inlined under `v/` as thin cwd-safe wrappers
   (`v/cat`, `v/patch`, `v/glob`, ...). The markdown builders were merged too, so the
   model now uses `(answer (v/join ...))`, `(v/h1 ...)`,
   `(v/file-link ...)`, etc. through the same `v/` prefix. One short
   alias, normal Clojure code, no bespoke edit DSLs, no alias
   switching mid-iteration."
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.ext.foundation.environment.core :as environment]
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [com.blockether.vis.ext.foundation.markdown :as markdown]))

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

(defn- call-resolved!
  [sym & args]
  (apply (or (requiring-resolve sym)
           (throw (ex-info "Foundation helper did not resolve"
                    {:type :foundation/missing-helper
                     :symbol sym})))
    args))

(defn- lazy-doctor-check-fn
  [env]
  (call-resolved! 'com.blockether.vis.ext.foundation.doctor/check-fn env))

(defn- lazy-cli-run-fn
  [command-sym parsed residual]
  (let [cmd (call-resolved! command-sym)]
    ((:cmd/run-fn cmd) parsed residual)))

(defn- doctor-cli-command
  []
  {:cmd/name   "doctor"
   :cmd/doc    "Run cross-extension diagnostics. Prints info / warn / error messages contributed by every loaded extension; exits 0 (clean) / 1 (warnings) / 2 (errors)."
   :cmd/usage  "vis extensions doctor"
   :cmd/run-fn #(lazy-cli-run-fn 'com.blockether.vis.ext.foundation.doctor/cli-command %1 %2)})

(defn- transcript-cli-command
  []
  {:cmd/name  "reproduction"
   :cmd/doc   "Print a complete, flag-free Markdown reproduction artifact for a conversation. It is always complete: every turn, iteration, prompt body, message envelope, executed code block, var, reasoning trace, final answer, and raw LLM diagnostic. Resolves an unambiguous id prefix the same way `vis conversations --fork` does."
   :cmd/usage "vis extensions reproduction <CONVERSATION-ID>"
   :cmd/args  [{:name "conversation-id" :kind :positional :type :string
                :doc  "Conversation id (full UUID or unambiguous prefix)."}]
   :cmd/examples ["vis extensions reproduction eeaf9651-06c7-4dda-9e97-877fcef06337"
                  "vis extensions reproduction eeaf9651"
                  "vis extensions reproduction eeaf9651 > REPRODUCTION.md"]
   :cmd/run-fn #(lazy-cli-run-fn 'com.blockether.vis.ext.foundation.transcript/cli-command %1 %2)})

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
     :ext/environment-info-fn environment/environment-info
     :ext/prompt         combined-prompt
     :ext/rendering-kinds editing/rendering-kind-fns
     :ext/symbols        (vec (concat introspection/all-symbols
                                editing/editing-symbols
                                markdown/markdown-symbols
                                environment/environment-symbols))
     :ext/doctor-check-fn lazy-doctor-check-fn
     :ext/cli            [(doctor-cli-command)
                          (transcript-cli-command)]}))

(vis/register-extension! vis-extension)
