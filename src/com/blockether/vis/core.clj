(ns com.blockether.vis.core
  "vis — public API.

   This is the ONLY namespace external consumers (extensions, channel
   adapters, embedded SDK callers, tests) should import. Every public
   symbol the project exposes is re-exported here from one of the
   `com.blockether.vis.internal.*` namespaces. The internal tree is
   not stable — it's where modules get split, merged, and renamed as
   the architecture evolves. The names below ARE the contract.

   Conceptual groups, alphabetic within each:

     Cancellation        cancel!, cancellation-atom, cancellation-set-future!,
                         cancellation-token, cancellation?
     Channel registry    channel, register-channel!, registered-channels,
                         channel-by-id, by-cmd
     CLI dispatcher      command, dispatch!, find-leaf, parse-args,
                         render-command, render-tree, validate-args,
                         register-cmd!, deregister-cmd!,
                         registered-commands, registered-under
     Configuration       init!, init-cli!, shutdown!, load-config,
                         load-config-raw, save-config!, reload-config!,
                         tty-in, tty-out, original-stdout,
                         display-label, model-name, provider-base-url,
                         provider-presets, provider-template
     Error formatting    error-message, db-error->user-message, format-error,
                         final-answer-code-error-message
     Extension contract  extension, symbol, value, register-extension!,
                         registered-extensions, registered-extension-ids,
                         extension-id-of-ns, extension-doc, extension-docs,
                         render-prompt, invoke-symbol-wrapper,
                         try-rescue-parse-error, discover-extensions!
     Format helpers      format-clojure, format-date, format-duration
     Markdown export     conversation->markdown
     Notifications      notify!, notifications, dismiss!, dismiss-all!,
                         watch-notifications!, unwatch-notifications!
     Iteration loop      query!, send!, create!, by-id, by-channel,
                         for-telegram-chat!, env-for, close!, close-all!,
                         delete!, set-title!,
                         create-environment, dispose-environment!,
                         get-router, rebuild-router!, refresh-cached-routers!,
                         resolve-effective-model,
                         db-info, auto-forget-stale-vars!,
                         extract-defining-name
     Manifest discovery  rediscover!  (test/REPL utility)
     Persistance facade  db-create-connection!, db-dispose-connection!,
                         log!, ds, now-ms, every store-*/db-*/update-*/etc.
                         delegating fn, db-sweep-orphaned-running-queries!
     Progress tracker    make-progress-tracker
     Prompt builders     active-extensions, assemble-system-prompt,
                         build-iteration-context, safe-pr-str
     Provider registry   provider, register-provider!, registered-providers,
                         provider-by-id
     SCI sandbox         create-sci-context, build-var-index,
                         restore-sandbox!, system-var-sym?, SYSTEM_VAR_NAMES

     Binary entry        -main  (invoked by `clojure -M:vis`)"
  (:refer-clojure :exclude [symbol])
  (:require
   [com.blockether.vis.internal.cancellation :as cancellation]
   [com.blockether.vis.internal.commandline  :as commandline]
   [com.blockether.vis.internal.config       :as config]
   [com.blockether.vis.internal.doctor       :as doctor]
   [com.blockether.vis.internal.env          :as env]
   [com.blockether.vis.internal.error        :as error]
   [com.blockether.vis.internal.extension    :as extension]
   [com.blockether.vis.internal.format       :as fmt]
   [com.blockether.vis.internal.loop         :as lp]
   [com.blockether.vis.internal.main         :as binary]
   [com.blockether.vis.internal.manifest     :as manifest]
   [com.blockether.vis.internal.markdown     :as markdown]
   [com.blockether.vis.internal.notifications :as notifications]
   [com.blockether.vis.internal.persistance  :as persistance]
   [com.blockether.vis.internal.progress     :as progress]
   [com.blockether.vis.internal.prompt       :as prompt]
   [com.blockether.vis.internal.registry     :as registry]))

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
(def db-error->user-message              persistance/db-error->user-message)

;; =============================================================================
;; Format helpers
;; =============================================================================
(def format-date       fmt/format-date)
(def format-clojure    fmt/format-clojure)
(def format-duration   fmt/format-duration)
(def format-tokens     fmt/format-tokens)
(def format-cost       fmt/format-cost)
(def format-iterations fmt/format-iterations)
(def format-meta-line  fmt/format-meta-line)

;; =============================================================================
;; Notifications
;;
;; Cross-channel ephemeral signals — \"copied\", \"verify.sh passed\",
;; \"provider switched\". Any extension or channel can push via
;; `notify!`; the TUI banner / Telegram chat / CLI agent each
;; subscribe with `watch!` and surface entries in their own visual
;; idiom. Levels (`:info` / `:success` / `:warn` / `:error`) are
;; advisory metadata for the consumer.
;; =============================================================================
(def notify!         notifications/notify!)
(def notifications   notifications/notifications)
(def dismiss!        notifications/dismiss!)
(def dismiss-all!    notifications/dismiss-all!)
(def watch-notifications!   notifications/watch!)
(def unwatch-notifications! notifications/unwatch!)

;; =============================================================================
;; Markdown export
;;
;; Single host-runtime helper for projecting a persisted conversation
;; (every turn: user prompt + final answer + optional metadata) into a
;; Markdown string. Lives in the runtime so EVERY channel — TUI,
;; Telegram, CLI agent, third-party plug-ins — can ship a `Copy as
;; Markdown` / `Export conversation` affordance without re-implementing
;; the projection. See `internal.markdown/DEFAULT_OPTS` for tunables.
;; =============================================================================
(def conversation->markdown markdown/conversation->markdown)

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
(def unknown-flags        commandline/unknown-flags)
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
(def ds                                  persistance/ds)
(def now-ms                              persistance/now-ms)
(def ->id                                persistance/->id)
(def ->uuid                              persistance/->uuid)
(def ->ref                               persistance/->ref)
(def ->kw                                persistance/->kw)
(def ->kw-back                           persistance/->kw-back)
(def ->epoch-ms                          persistance/->epoch-ms)
(def ->date                              persistance/->date)
(def db-create-connection!             persistance/db-create-connection!)
(def db-dispose-connection!           persistance/db-dispose-connection!)
(def db-shared-connection!                        persistance/db-shared-connection!)
(def db-dispose-shared-connection!                persistance/db-dispose-shared-connection!)
(def register-backend!                   persistance/register-backend!)
(def deregister-backend!                 persistance/deregister-backend!)
(def registered-backends                 persistance/registered-backends)

;; Logging
(def db-log!                             persistance/db-log!)

;; Conversation lifecycle (storage facade)
(def db-store-conversation!                 persistance/db-store-conversation!)
(def db-get-conversation                 persistance/db-get-conversation)
(def db-resolve-conversation-id          persistance/db-resolve-conversation-id)
(def db-list-conversations               persistance/db-list-conversations)
(def db-find-conversation-by-external    persistance/db-find-conversation-by-external)
(def db-update-conversation-title!       persistance/db-update-conversation-title!)
(def db-delete-conversation-tree!           persistance/db-delete-conversation-tree!)
(def db-fork-conversation!                  persistance/db-fork-conversation!)
(def db-list-conversation-states            persistance/db-list-conversation-states)
(def db-latest-conversation-state-id        persistance/db-latest-conversation-state-id)

;; Query lifecycle
(def db-store-query!                        persistance/db-store-query!)
(def db-update-query!                       persistance/db-update-query!)
(def db-list-queries-by-status           persistance/db-list-queries-by-status)
(def db-list-conversation-queries        persistance/db-list-conversation-queries)
(def db-retry-query!                        persistance/db-retry-query!)
(def db-list-query-states                   persistance/db-list-query-states)

;; Iteration lifecycle
(def db-store-iteration!                    persistance/db-store-iteration!)
(def db-list-query-iterations            persistance/db-list-query-iterations)
(def db-list-iteration-vars              persistance/db-list-iteration-vars)
(def db-list-iteration-blocks       persistance/db-list-iteration-blocks)

;; Var registry & history
(def db-latest-var-registry              persistance/db-latest-var-registry)
(def db-var-history                      persistance/db-var-history)
(def db-query-history                    persistance/db-query-history)

;; Dependencies
(def db-store-dependency!                   persistance/db-store-dependency!)
(def db-list-dependencies                persistance/db-list-dependencies)

;; Restore
(def db-restore-blocks              persistance/db-restore-blocks)

;; Process-restart cleanup
(def db-sweep-orphaned-running-queries!     persistance/db-sweep-orphaned-running-queries!)

;; =============================================================================
;; Extension contract
;; =============================================================================
(def extension                           extension/extension)
(def symbol                              extension/symbol)
(def value                               extension/value)
(def render-prompt                       extension/render-prompt)
(def register-extension!                 extension/register-extension!)
(def registered-extensions               extension/registered-extensions)
(def registered-extension-ids            extension/registered-extension-ids)
(def extension-namespaces                extension/extension-namespaces)
(def extension-id-of-ns                  extension/extension-id-of-ns)
(def extension-doc                       extension/extension-doc)
(def extension-docs                      extension/extension-docs)
(def extension-doc-content               extension/extension-doc-content)
(def extension-doc-abstract              extension/extension-doc-abstract)
(def extension-doc-summary               extension/extension-doc-summary)
(def extension-doc-names                 extension/extension-doc-names)
(def registered-extensions-summary       extension/registered-extensions-summary)
(def invoke-symbol-wrapper               extension/invoke-symbol-wrapper)
(def try-rescue-parse-error              extension/try-rescue-parse-error)
(def discover-extensions!                extension/discover-extensions!)
(def rediscover!                         manifest/rediscover!)
(def deregister-extension!               extension/deregister-extension!)
(def extension-source-markers-of         extension/extension-source-markers-of)
(def reload-extensions!                  lp/reload-extensions!)

;; =============================================================================
;; Doctor protocol
;;
;; Cross-cutting diagnostic surface. Every extension can declare
;; `:ext/doctor-checks` (vec of `{:check/id :check/name :check/description
;; :check/run-fn}`); `vis doctor` aggregates them all into a level-aware
;; (info / warn / error) report with 0 / 1 / 2 exit code by max level.
;; See plan §1 Q19 + §10.
;; =============================================================================
(def run-doctor-checks    doctor/run-checks)
(def doctor-exit-code     doctor/exit-code)
(def doctor-format-output doctor/format-output)
(def doctor-startup-hint  doctor/startup-hint-line)

;; =============================================================================
;; Configuration / paths / logging
;; =============================================================================
(def init!                               config/init!)
(def init-cli!                           config/init-cli!)
(def shutdown!                           config/shutdown!)
(def tty-in                              config/tty-in)
(def tty-out                             config/tty-out)
(def original-stdout                     config/original-stdout)
(def load-config-raw                     config/load-config-raw)
(def load-config                         config/load-config)
(def save-config!                        config/save-config!)
(def reload-config!                      config/reload-config!)
(def resolve-config                      config/resolve-config)
(def resolve-db-spec                     config/resolve-db-spec)
(def current-config                      config/current-config)
(def active-provider                     config/active-provider)
(def active-model                        config/active-model)
(def provider-ids                        config/provider-ids)
(def has-provider?                       config/has-provider?)
(def display-label                       config/display-label)
(def model-name                          config/model-name)
(def provider-base-url                   config/provider-base-url)
(def provider-presets                    config/provider-presets)
(def provider-template                   config/provider-template)
(def ->svar-model                        config/->svar-model)
(def ->svar-provider                     config/->svar-provider)

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
(def ask-code!                    lp/ask-code!)
(def get-router                   lp/get-router)
(def reset-router!                lp/reset-router!)
(def rebuild-router!              lp/rebuild-router!)
(def resolve-effective-model      lp/resolve-effective-model)
(def provider-has-reasoning?      lp/provider-has-reasoning?)
(def set-provider!                lp/set-provider!)
(def remove-provider!             lp/remove-provider!)

;; SCI execution helpers (used by extensions for parse-rescue)
(def extract-defining-name        lp/extract-defining-name)
(def answer-form-error              lp/answer-form-error)
(def answer-position-violation?              lp/answer-position-violation?)
(def answer-position-error-message           lp/answer-position-error-message)
(def parinfer-rebalance             lp/parinfer-rebalance)
(def split-top-level-forms          lp/split-top-level-forms)

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
(def add-title-listener!          lp/add-title-listener!)
(def remove-title-listener!       lp/remove-title-listener!)
(def env-for                      lp/env-for)
(def send!                        lp/send!)
(def close!                       lp/close!)
(def delete!                      lp/delete!)
(def close-all!                   lp/close-all!)

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
