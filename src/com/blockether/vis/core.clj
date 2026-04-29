(ns com.blockether.vis.core
  "vis — public API.

   This is the ONLY namespace external consumers (extensions, channel
   adapters, embedded SDK callers, tests) should import. Every public
   symbol the project exposes is re-exported here from one of the
   `com.blockether.vis.internal.*` namespaces. The internal tree is
   not stable — it's where modules get split, merged, and renamed as
   the architecture evolves. The names below ARE the contract.

   Six conceptual groups, alphabetic within each:

     Cancellation        cancel!, cancellation-atom, cancellation-set-future!,
                         cancellation-token, cancellation?
     Error formatting    error-message, error->user-message, format-error,
                         final-answer-code-error-message
     Format helpers      format-clojure, format-date, format-duration
     Progress tracker    make-progress-tracker
     CLI dispatcher      command, dispatch!, find-leaf, parse-args,
                         render-command, render-tree, validate-args,
                         register-cmd!, deregister-cmd!,
                         registered-commands, registered-under
     Persistance facade  create-store-connection, dispose-store-connection!,
                         log!, ds, now-ms, every store-*/db-*/update-*/etc.
                         delegating fn, sweep-orphaned-running-queries!
     Extension contract  extension, symbol, value, register-extension!,
                         registered-extensions, registered-extension-ids,
                         extension-id-of-ns, extension-doc, extension-docs,
                         render-prompt, invoke-symbol-wrapper,
                         try-rescue-parse-error
     Configuration       init!, init-cli!, shutdown!, load-config,
                         load-config-raw, save-config!, reload-config!,
                         tty-in, tty-out, original-stdout,
                         display-label, model-name, provider-base-url,
                         provider-presets, provider-template
     SCI sandbox         create-sci-context, build-var-index,
                         restore-sandbox!, system-var-sym?, SYSTEM_VAR_NAMES
     Iteration loop      query!, send!, create!, by-id, by-channel,
                         for-telegram-chat!, env-for, close!, close-all!,
                         delete!, set-title!, effective-system-prompt,
                         create-environment, dispose-environment!,
                         get-router, rebuild-router!, refresh-cached-routers!,
                         resolve-effective-model, ask-with-schema-retry!,
                         render, db-info, auto-forget-stale-vars!,
                         auto-forget-candidates, dedup-cache-lookup,
                         dedup-cache-record!, count-duplicates,
                         canonical-expression-hash, extract-defining-name,
                         try-extension-parse-rescue
     Iteration spec      iteration-spec, ITERATION_SPEC_*
     Prompt builders     active-extensions, assemble-system-prompt,
                         build-iteration-context,
                         safe-pr-str

     Binary entry        -main  (invoked by `clojure -M:vis`)"
  (:refer-clojure :exclude [agent run! symbol])
  (:require
   [com.blockether.vis.internal.cancellation :as cancellation]
   [com.blockether.vis.internal.commandline  :as commandline]
   [com.blockether.vis.internal.env          :as env]
   [com.blockether.vis.internal.error        :as error]
   [com.blockether.vis.internal.format       :as fmt]
   [com.blockether.vis.internal.loop         :as lp]
   [com.blockether.vis.internal.main         :as binary]
   [com.blockether.vis.internal.progress     :as progress]
   [com.blockether.vis.internal.prompt       :as prompt]
   [com.blockether.vis.internal.registry     :as registry]
   [com.blockether.vis.internal.sdk          :as sdk]
   [com.blockether.vis.internal.spec         :as spec]))

;; =============================================================================
;; Cancellation
;; =============================================================================
(def cancellation-token        cancellation/cancellation-token)
(def cancellation-atom         cancellation/cancellation-atom)
(def cancellation-set-future!  cancellation/cancellation-set-future!)
(def cancel!                   cancellation/cancel!)
(def cancelled?                cancellation/cancelled?)
(def cancellation?             cancellation/cancellation?)

;; =============================================================================
;; Error formatting
;; =============================================================================
(def error-message                    error/error-message)
(def format-error                     error/format-error)
(def final-answer-code-error-message  error/final-answer-code-error-message)
(def error->user-message              sdk/error->user-message)

;; =============================================================================
;; Format helpers
;; =============================================================================
(def format-date     fmt/format-date)
(def format-clojure  fmt/format-clojure)
(def format-duration fmt/format-duration)

;; =============================================================================
;; Progress tracker
;; =============================================================================
(def make-progress-tracker progress/make-progress-tracker)

;; =============================================================================
;; CLI dispatcher
;; =============================================================================
(def command              registry/command)
(def resolve-subcommands  registry/resolve-subcommands)
(def find-leaf            commandline/find-leaf)
(def find-named           commandline/find-named)
(def parse-args           commandline/parse-args)
(def validate-args        commandline/validate-args)
(def pad-right            commandline/pad-right)
(def pad-left             commandline/pad-left)
(def render-command       commandline/render-command)
(def render-tree          commandline/render-tree)
(def dispatch!            commandline/dispatch!)
(def register-cmd!        registry/register-cmd!)
(def deregister-cmd!      registry/deregister-cmd!)
(def registered-commands  registry/registered-commands)
(def registered-under     registry/registered-under)

;; =============================================================================
;; Channel registry
;; =============================================================================
(def channel               registry/channel)
(def register-channel!     registry/register-channel!)
(def deregister-channel!   registry/deregister-channel!)
(def registered-channels   registry/registered-channels)
(def channel-by-id         registry/channel-by-id)

;; =============================================================================
;; Provider registry
;; =============================================================================
(def provider              registry/provider)
(def register-provider!    registry/register-provider!)
(def deregister-provider!  registry/deregister-provider!)
(def registered-providers  registry/registered-providers)
(def provider-by-id        registry/provider-by-id)

;; =============================================================================
;; Persistance facade
;; =============================================================================
(def ds                                  sdk/ds)
(def now-ms                              sdk/now-ms)
(def ->id                                sdk/->id)
(def ->uuid                              sdk/->uuid)
(def ->ref                               sdk/->ref)
(def ->kw                                sdk/->kw)
(def ->kw-back                           sdk/->kw-back)
(def ->epoch-ms                          sdk/->epoch-ms)
(def ->date                              sdk/->date)
(def create-store-connection             sdk/create-store-connection)
(def dispose-store-connection!           sdk/dispose-store-connection!)
(def shared-conn!                        sdk/shared-conn!)
(def dispose-shared-conn!                sdk/dispose-shared-conn!)
(def register-backend!                   sdk/register-backend!)
(def deregister-backend!                 sdk/deregister-backend!)
(def registered-backends                 sdk/registered-backends)

;; Logging
(def log!                                sdk/log!)

;; Conversation lifecycle (storage facade)
(def store-conversation!                 sdk/store-conversation!)
(def db-get-conversation                 sdk/db-get-conversation)
(def db-resolve-conversation-id          sdk/db-resolve-conversation-id)
(def db-list-conversations               sdk/db-list-conversations)
(def db-find-conversation-by-external    sdk/db-find-conversation-by-external)
(def db-update-conversation-title!       sdk/db-update-conversation-title!)
(def delete-conversation-tree!           sdk/delete-conversation-tree!)
(def fork-conversation!                  sdk/fork-conversation!)

;; Query lifecycle
(def store-query!                        sdk/store-query!)
(def update-query!                       sdk/update-query!)
(def db-list-queries-by-status           sdk/db-list-queries-by-status)
(def db-list-conversation-queries        sdk/db-list-conversation-queries)
(def retry-query!                        sdk/retry-query!)

;; Iteration lifecycle
(def store-iteration!                    sdk/store-iteration!)
(def db-list-query-iterations            sdk/db-list-query-iterations)
(def db-list-iteration-vars              sdk/db-list-iteration-vars)
(def db-list-iteration-expressions       sdk/db-list-iteration-expressions)

;; Var registry & history
(def db-latest-var-registry              sdk/db-latest-var-registry)
(def db-var-history                      sdk/db-var-history)
(def db-query-history                    sdk/db-query-history)

;; Dependencies
(def store-dependency!                   sdk/store-dependency!)
(def db-list-dependencies                sdk/db-list-dependencies)

;; Restore
(def db-restore-expressions              sdk/db-restore-expressions)

;; Process-restart cleanup
(def sweep-orphaned-running-queries!     sdk/sweep-orphaned-running-queries!)

;; =============================================================================
;; Extension contract
;; =============================================================================
(def extension                           sdk/extension)
(def symbol                              sdk/symbol)
(def value                               sdk/value)
(def render-prompt                       sdk/render-prompt)
(def register-extension!                 sdk/register-extension!)
(def registered-extensions               sdk/registered-extensions)
(def registered-extension-ids            sdk/registered-extension-ids)
(def extension-namespaces                sdk/extension-namespaces)
(def extension-id-of-ns                  sdk/extension-id-of-ns)
(def extension-doc                       sdk/extension-doc)
(def extension-docs                      sdk/extension-docs)
(def extension-doc-content               sdk/extension-doc-content)
(def extension-doc-abstract              sdk/extension-doc-abstract)
(def extension-doc-summary               sdk/extension-doc-summary)
(def extension-doc-names                 sdk/extension-doc-names)
(def registered-extensions-summary       sdk/registered-extensions-summary)
(def invoke-symbol-wrapper               sdk/invoke-symbol-wrapper)
(def try-rescue-parse-error              sdk/try-rescue-parse-error)
(def discover-extensions!                sdk/discover-extensions!)
(def rediscover!                         sdk/rediscover!)

;; =============================================================================
;; Configuration / paths / logging
;; =============================================================================
(def init!                               sdk/init!)
(def init-cli!                           sdk/init-cli!)
(def shutdown!                           sdk/shutdown!)
(def tty-in                              sdk/tty-in)
(def tty-out                             sdk/tty-out)
(def original-stdout                     sdk/original-stdout)
(def load-config-raw                     sdk/load-config-raw)
(def load-config                         sdk/load-config)
(def save-config!                        sdk/save-config!)
(def reload-config!                      sdk/reload-config!)
(def resolve-config                      sdk/resolve-config)
(def resolve-db-spec                     sdk/resolve-db-spec)
(def current-config                      sdk/current-config)
(def active-provider                     sdk/active-provider)
(def active-model                        sdk/active-model)
(def provider-ids                        sdk/provider-ids)
(def has-provider?                       sdk/has-provider?)
(def display-label                       sdk/display-label)
(def model-name                          sdk/model-name)
(def provider-base-url                   sdk/provider-base-url)
(def provider-presets                    sdk/provider-presets)
(def provider-template                   sdk/provider-template)
(def ->svar-model                        sdk/->svar-model)
(def ->svar-provider                     sdk/->svar-provider)

;; =============================================================================
;; SCI sandbox
;; =============================================================================
(def SYSTEM_VAR_NAMES   env/SYSTEM_VAR_NAMES)
(def system-var-sym?    env/system-var-sym?)
(def create-sci-context env/create-sci-context)
(def build-var-index    env/build-var-index)
(def restore-sandbox!   env/restore-sandbox!)
(def sci-update-binding! env/sci-update-binding!)
(def bump-var-index!    env/bump-var-index!)
(def bind-and-bump!     env/bind-and-bump!)

;; =============================================================================
;; Iteration loop / query / environment / conversations
;; =============================================================================
(def query!                       lp/query!)
(def ask!                         lp/ask!)
(def ask-with-schema-retry!       lp/ask-with-schema-retry!)
(def render                       lp/render)
(def get-router                   lp/get-router)
(def reset-router!                lp/reset-router!)
(def rebuild-router!              lp/rebuild-router!)
(def resolve-effective-model      lp/resolve-effective-model)
(def provider-has-reasoning?      lp/provider-has-reasoning?)
(def set-provider!                lp/set-provider!)
(def remove-provider!             lp/remove-provider!)

;; SCI execution helpers (used by extensions for parse-rescue + dedup)
(def canonical-expression-hash    lp/canonical-expression-hash)
(def count-duplicates             lp/count-duplicates)
(def dedup-cache-lookup           lp/dedup-cache-lookup)
(def dedup-cache-record!          lp/dedup-cache-record!)
(def extract-defining-name        lp/extract-defining-name)

;; Environment lifecycle
(def create-environment           lp/create-environment)
(def dispose-environment!         lp/dispose-environment!)
(def install-extension!           lp/install-extension!)

;; Auto-forget
(def auto-forget-candidates       lp/auto-forget-candidates)
(def auto-forget-stale-vars!      lp/auto-forget-stale-vars!)

;; Conversations
(def db-info                      lp/db-info)
(def custom-bindings              lp/custom-bindings)
(def get-locals                   lp/get-locals)
(def cache-env!                   lp/cache-env!)
(def refresh-cached-routers!      lp/refresh-cached-routers!)
(def create!                      lp/create!)
(def by-id                        lp/by-id)
(def by-channel                   lp/by-channel)
(def for-telegram-chat!           lp/for-telegram-chat!)
(def set-title!                   lp/set-title!)
(def env-for                      lp/env-for)
(def effective-system-prompt-for-query lp/effective-system-prompt-for-query)
(def effective-system-prompt      lp/effective-system-prompt)
(def send!                        lp/send!)
(def close!                       lp/close!)
(def delete!                      lp/delete!)
(def close-all!                   lp/close-all!)

;; =============================================================================
;; Iteration spec (plan validation + svar shapes)
;; =============================================================================
(def iteration-spec         spec/iteration-spec)
(def ITERATION_SPEC_BASE             spec/ITERATION_SPEC_BASE)
(def ITERATION_SPEC_REASONING        spec/ITERATION_SPEC_REASONING)
(def ITERATION_SPEC_NON_REASONING    spec/ITERATION_SPEC_NON_REASONING)

;; =============================================================================
;; Prompt builders
;; =============================================================================
(def active-extensions          prompt/active-extensions)
(def assemble-system-prompt     prompt/assemble-system-prompt)
(def build-system-prompt        prompt/build-system-prompt)
(def build-iteration-context    prompt/build-iteration-context)

(def safe-pr-str                prompt/safe-pr-str)
(def truncated-pr-str           prompt/truncated-pr-str)
(def assemble-initial-messages  prompt/assemble-initial-messages)
(def trim-to-initial-history    prompt/trim-to-initial-history)
(def CORE_SYSTEM_PROMPT         prompt/CORE_SYSTEM_PROMPT)

;; =============================================================================
;; Binary entry point
;;
;; `clojure -M:vis` invokes this `-main`. The dispatcher / built-in
;; commands / agent helper / :db Telemere handler all live in
;; `com.blockether.vis.internal.main`; this fn is a thin trampoline
;; so the binary entry has the same on-disk address as the public
;; library API.
;; =============================================================================

(defn -main [& args]
  (apply binary/-main args))
