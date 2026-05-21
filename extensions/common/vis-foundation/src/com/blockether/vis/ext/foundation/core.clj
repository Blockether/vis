(ns com.blockether.vis.ext.foundation.core
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.ext.foundation.environment.core :as environment]
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [com.blockether.vis.ext.foundation.hints :as hints]))

(defn- combined-prompt
  "Stitch foundation-owned tool strategy prompt text. Structured runtime
   and project guidance flow through `ctx`, not prompt labels."
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

(doseq [[op tag] [[:v/session-state :op.tag/observation]
                  [:v/session-report :op.tag/observation]
                  [:v/engine-symbol-documentation :op.tag/observation]
                  [:v/engine-symbol-source-code :op.tag/observation]
                  [:v/engine-symbol-metadata :op.tag/observation]
                  [:v/engine-symbol-apropos :op.tag/observation]
                  [:v/cat :op.tag/observation]
                  [:v/ls :op.tag/observation]
                  [:v/rg :op.tag/observation]
                  [:v/exists? :op.tag/observation]
                  [:v/snapshot :op.tag/observation]
                  [:v/repositories :op.tag/observation]
                  [:v/git :op.tag/observation]
                  [:v/languages :op.tag/observation]
                  [:v/monorepo :op.tag/observation]
                  [:v/main-agent-instructions :op.tag/observation]
                  [:v/patch :op.tag/mutation]
                  [:v/create-dirs :op.tag/mutation]
                  [:v/copy :op.tag/mutation]
                  [:v/move :op.tag/mutation]
                  [:v/delete :op.tag/mutation]
                  [:v/delete-if-exists :op.tag/mutation]
                  [:v/refresh! :op.tag/mutation]
                  [:v/reload-extensions! :op.tag/mutation]]]
  (vis/register-op! op {:tag tag}))

(defn- lazy-doctor-fn
  [env]
  (call-resolved! 'com.blockether.vis.ext.foundation.doctor/doctor-fn env))

(defn- lazy-cli-run-fn
  [command-sym parsed residual]
  (let [cmd (call-resolved! command-sym)]
    ((:cmd/run-fn cmd) parsed residual)))

(defn- transcript-cli-command
  []
  {:cmd/name  "repro"
   :cmd/doc   "Reproduce a session as a Markdown transcript (turns, code, vars, reasoning, answers)."
   :cmd/usage "vis ext repro <SESSION-ID>"
   :cmd/args  [{:name "session-id" :kind :positional :type :string
                :doc  "Session id (full UUID or unambiguous prefix)."}]
   :cmd/examples ["vis ext repro eeaf9651-06c7-4dda-9e97-877fcef06337"
                  "vis ext repro eeaf9651"
                  "vis ext repro eeaf9651 > REPRODUCTION.md"]
   :cmd/run-fn #(lazy-cli-run-fn 'com.blockether.vis.ext.foundation.transcript/cli-command %1 %2)})

(def vis-extension
  (vis/extension
    {:ext/name           "foundation"
     :ext/description    "Foundation `v/`: session-state/session-report, file I/O (cat/ls/rg/patch), markdown answer builders (h1/p/table/file-link/join/code-block), env snapshot, project guidance, reproduction CLI."
     :ext/version        "0.7.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/sci            {:ext.sci/alias 'v
                          :ext.sci/symbols (vec (concat introspection/all-symbols
                                                  (editing/available-editing-symbols)
                                                  environment/environment-symbols))}
     :ext/kind           "foundation"
     :ext/hooks          hints/hooks
     :ext/ctx            environment/environment-ctx
     :ext/prompt         combined-prompt
     :ext/doctor-fn       lazy-doctor-fn
     :ext/cli            [(transcript-cli-command)]}))

(vis/register-extension! vis-extension)
