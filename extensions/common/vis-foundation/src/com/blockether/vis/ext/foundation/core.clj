(ns com.blockether.vis.ext.foundation.core
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.ext.foundation.environment.core :as environment]
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [com.blockether.vis.ext.foundation.nudges :as nudges]))

(defn- combined-prompt
  "Stitch the per-area prompt fragments together. The environment
   fragment is a fn (it renders the live snapshot every time the
   system prompt is assembled); introspection + editing + answer-IR
   fragments are static strings that we concatenate directly."
  [env]
  (str (environment/environment-prompt env)
    "\n\n"
    introspection/introspection-prompt
    "\n\n"
    (editing/available-editing-prompt)))

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
     :ext/doc            "Foundation `v/`: conversation-state/conversation-report, file I/O (cat/ls/rg/patch/bash), markdown answer builders (h1/p/table/file-link/join/code-block), env snapshot, project guidance, scan warnings, doctor/reproduction CLI."
     :ext/version        "0.7.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/alias       {:ns 'vis.ext.v :alias 'v}
     :ext/kind           "foundation"
     :ext/environment-prompt-fn environment/environment-info
     :ext/hooks          nudges/hooks
     :ext/prompt         combined-prompt
     :ext/symbols        (vec (concat introspection/all-symbols
                                (editing/available-editing-symbols)
                                environment/environment-symbols))
     :ext/doctor-check-fn lazy-doctor-check-fn
     :ext/cli            [(doctor-cli-command)
                          (transcript-cli-command)]}))

(vis/register-extension! vis-extension)
