(ns com.blockether.vis.ext.foundation-core.core
  (:require
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation-core.editing.core :as editing]
   [com.blockether.vis.ext.foundation-core.environment.core :as environment]
   [com.blockether.vis.ext.foundation-core.introspection :as introspection]
   [com.blockether.vis.ext.foundation-core.hints :as hints]))

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

(doseq [[op tag] [[:v/session-state :observation]
                  [:v/session-report :observation]
                  [:v/engine-symbol-documentation :observation]
                  [:v/engine-symbol-source-code :observation]
                  [:v/engine-symbol-metadata :observation]
                  [:v/engine-symbol-apropos :observation]
                  [:v/cat :observation]
                  [:v/ls :observation]
                  [:v/rg :observation]
                  [:v/exists? :observation]
                  [:v/snapshot :observation]
                  [:v/repositories :observation]
                  [:v/git :observation]
                  [:v/languages :observation]
                  [:v/monorepo :observation]
                  [:v/main-agent-instructions :observation]
                  [:v/patch :mutation]
                  [:v/create-dirs :mutation]
                  [:v/copy :mutation]
                  [:v/move :mutation]
                  [:v/delete :mutation]
                  [:v/delete-if-exists :mutation]
                  [:v/refresh! :mutation]
                  [:v/reload-extensions! :mutation]]]
  (vis/register-op! op {:tag tag}))

(defn- lazy-doctor-fn
  [env]
  (call-resolved! 'com.blockether.vis.ext.foundation-core.doctor/doctor-fn env))

(def vis-extension
  (vis/extension
    {:ext/name           "foundation-core"
     :ext/description    "Foundation `v/` kernel: session-state/session-report, file I/O (cat/ls/rg/patch/copy/move/delete/exists?), SCI symbol introspection (engine-symbol-{documentation,source-code,metadata,apropos}), env snapshot + project guidance (snapshot/repositories/git/languages/monorepo/main-agent-instructions/reload-extensions!). Answers are plain markdown strings — no DSL."
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
     :ext/doctor-fn       lazy-doctor-fn}))

(vis/register-extension! vis-extension)
