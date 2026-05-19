(ns com.blockether.vis.ext.foundation.core
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.ext.foundation.environment.core :as environment]
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [com.blockether.vis.ext.foundation.hints :as hints]))

(defn- combined-prompt
  "Stitch the foundation-owned model prompt fragment together.
   Foundation owns environment facts directly here; core only orders active
   extension prompt fragments."
  [env]
  (str (environment/environment-info env)
    "\n\n"
    (environment/environment-prompt env)
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
                  [:v/scan-warnings :op.tag/observation]
                  [:v/patch :op.tag/mutation]
                  [:v/create-dirs :op.tag/mutation]
                  [:v/copy :op.tag/mutation]
                  [:v/move :op.tag/mutation]
                  [:v/delete :op.tag/mutation]
                  [:v/delete-if-exists :op.tag/mutation]
                  [:v/refresh! :op.tag/mutation]
                  [:v/reload-extensions! :op.tag/mutation]]]
  (vis/register-op! op {:tag tag}))

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
   :cmd/doc   "Print a complete, flag-free Markdown reproduction artifact for a session. It is always complete: every turn, iteration, prompt body, message envelope, executed code block, var, reasoning trace, final answer, and raw LLM diagnostic. Resolves an unambiguous id prefix the same way `vis sessions --fork` does."
   :cmd/usage "vis extensions reproduction <SESSION-ID>"
   :cmd/args  [{:name "session-id" :kind :positional :type :string
                :doc  "Session id (full UUID or unambiguous prefix)."}]
   :cmd/examples ["vis extensions reproduction eeaf9651-06c7-4dda-9e97-877fcef06337"
                  "vis extensions reproduction eeaf9651"
                  "vis extensions reproduction eeaf9651 > REPRODUCTION.md"]
   :cmd/run-fn #(lazy-cli-run-fn 'com.blockether.vis.ext.foundation.transcript/cli-command %1 %2)})

(def vis-extension
  (vis/extension
    {:ext/name           "foundation"
     :ext/description    "Foundation `v/`: session-state/session-report, file I/O (cat/ls/rg/patch), markdown answer builders (h1/p/table/file-link/join/code-block), env snapshot, project guidance, scan warnings, doctor/reproduction CLI."
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
     :ext/prompt         combined-prompt
     :ext/doctor-check-fn lazy-doctor-check-fn
     :ext/cli            [(doctor-cli-command)
                          (transcript-cli-command)]}))

(vis/register-extension! vis-extension)
