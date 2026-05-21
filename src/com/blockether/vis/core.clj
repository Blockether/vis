(ns com.blockether.vis.core
  "vis - broad host facade.

   This is the ONLY namespace extensions, channel adapters, embedded
   callers, and tests should import. It deliberately re-exports host,
   registry, runtime, persistence, prompt, diagnostic, and sandbox
   helpers from `com.blockether.vis.internal.*`. The internal tree is
   not stable; the names exposed here are the host contract.

   Canonical runtime language:
     Session -> Turn -> Iteration -> Block.

   A Turn is one user request plus assistant answer inside a Session.
   New code and documentation should use turn/user-request language.

   Primary surfaces:
     - Session / turn runtime: create!, send!, turn!, by-id,
       by-channel, env-for, close!, delete!, set-title!.
     - Environment runtime: create-environment, dispose-environment!,
       get-router, rebuild-router!, resolve-effective-model.
     - Extension contract: extension, symbol, value, render-prompt,
       register-extension!, registered-extensions, discovery, reload.
     - Registries: command, channel, provider, and backend registration
       helpers for host-owned and embedded use.
     - Persistence facade: db-* functions and connection helpers. The
       implementation namespace / extension slot are spelled
       `persistance`; human-facing language is Persistence.
     - Prompt / SCI / formatting / cancellation / notifications /
       doctor helpers shared by channels and extensions.

   Not every export is equally high-level. `send!`, extension maps,
   registry builders, read-side persistence helpers, and Markdown export
   are the preferred integration surface. Low-level SCI, parse-repair,
   dispatcher, and write-side db helpers are exported because this is a
   host facade, but ordinary extensions should avoid depending on them
   unless they are implementing host-level behavior.

   Binary entry: -main (invoked by `clojure -M:vis`)."
  (:refer-clojure :exclude [symbol])
  (:require
   [com.blockether.vis.internal.cancellation :as cancellation]
   [com.blockether.vis.internal.commandline  :as commandline]
   [com.blockether.vis.internal.config       :as config]
   [com.blockether.vis.internal.doctor       :as doctor]
   [com.blockether.vis.internal.env          :as env]
   [com.blockether.vis.internal.error        :as error]
   [com.blockether.vis.internal.extension    :as extension]
   [com.blockether.vis.internal.extension-aggregate :as extension-aggregate]
   [com.blockether.vis.internal.format       :as fmt]
   [com.blockether.vis.internal.loop         :as lp]
   [com.blockether.vis.internal.main         :as binary]
   [com.blockether.vis.internal.manifest     :as manifest]
   [com.blockether.vis.internal.render       :as ir]
   [com.blockether.vis.internal.notifications :as notifications]
   [com.blockether.vis.internal.persistance  :as persistance]
   [com.blockether.vis.internal.progress     :as progress]
   [com.blockether.vis.internal.prompt       :as prompt]
   [com.blockether.vis.internal.provider-limits :as provider-limits]
   [com.blockether.vis.internal.registry     :as registry]
   [com.blockether.vis.internal.theme        :as theme]
   [com.blockether.vis.internal.workspace    :as workspace]))

;; =============================================================================
;; Cancellation
;; =============================================================================
(def cancellation-token        cancellation/cancellation-token)
(def cancellation-atom         cancellation/cancellation-atom)
(def cancellation-set-future!  cancellation/cancellation-set-future!)
(def cancel!                   cancellation/cancel!)
(def cancelled?                cancellation/cancelled?)
(def cancellation?             cancellation/cancellation?)
(def virtual-threads-available? cancellation/virtual-threads-available?)
(def worker-runtime             cancellation/worker-runtime)
(def worker-future              cancellation/worker-future)

;; =============================================================================
;; Workspace (PLAN.md §3)
;; =============================================================================
(def workspace-cwd               workspace/cwd)
(def workspace-root              workspace/workspace-root)
(def workspace-normalize-root    workspace/normalize-root)
(def workspace-get               workspace/get)
(def workspace-list-active       workspace/list-active)
(def workspace-list-finished     workspace/list-finished)
(def workspace-for-session       workspace/for-session)
(def workspace-status            workspace/status)
(def workspace-trunk-info        workspace/trunk-info)
(def workspace-ensure-trunk!     workspace/ensure-trunk!)
(def workspace-spawn-branch!     workspace/spawn-branch!)
(def workspace-apply-to-trunk!   workspace/apply-to-trunk!)
(def workspace-discard!          workspace/discard!)
(def workspace-register-hook!    workspace/register-hook!)
(def workspace-delete-legacy-edn! workspace/delete-legacy-edn!)

;; =============================================================================
;; Theme facade
;; =============================================================================
(def default-theme-id          theme/default-theme-id)
(def default-theme             theme/default-theme)
(def vis-light                 theme/vis-light)
(def vis-dark                  theme/vis-dark)
(def themes                    theme/themes)
(def palette                   theme/palette)
(def pallete                   theme/pallete)
(def theme                     theme/theme)
(def color                     theme/color)
(def theme-registry            theme/theme-registry)
(def register-theme!           theme/register-theme!)
(def register-themes!          theme/register-themes!)
(def unregister-theme!         theme/unregister-theme!)
(def unregister-themes!        theme/unregister-themes!)
(def reset-themes!             theme/reset-themes!)
(def extension-theme-settings  theme/extension-theme-settings)
(def available-theme-ids       theme/available-theme-ids)

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
;; Cross-channel ephemeral signals - \"copied\", \"verify.sh passed\",
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
;; Single host-runtime helper for projecting a persisted session
;; (every turn: user prompt + final answer + optional metadata) into a
;; Markdown string. Lives in the runtime so EVERY channel - TUI,
;; Telegram, CLI agent, third-party plug-ins - can ship a `Copy as
;; Markdown` / `Export session` affordance without re-implementing
;; the projection. Lives in `internal.render` alongside the IR pipeline.
;; =============================================================================
(def session->markdown ir/session->markdown)

;; =============================================================================
;; Answer IR rendering — pure-Clojure walker for the 21-tag Hiccup-EDN IR.
;; Channels register their preferred renderer via
;; `:channel/messages-renderer-fn` and call it through `tg/send-message!`
;; or the TUI screen-emit boundary. See `docs/specs/01-streaming-and-markdown.md`.
;; =============================================================================
(def render               ir/render)
(def ->ast                ir/->ast)
(def markdown->ir         ir/markdown->ir)
(def answer->ir           ir/answer->ir)
(def search-text          ir/search-text)
(def extract-code         ir/extract-code)
(def extract-text         ir/extract-text)
(def parse-block-display  ir/parse-block-display)

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
(def by-cmd                registry/by-cmd)

;; =============================================================================
;; Provider registry
;; =============================================================================
(def provider              registry/provider)
(def register-provider!    registry/register-provider!)
(def deregister-provider!  registry/deregister-provider!)
(def registered-providers  registry/registered-providers)
(def provider-by-id        registry/provider-by-id)
(def provider-limits       provider-limits/provider-limits)
(def all-provider-limits   provider-limits/all-provider-limits)

;; =============================================================================
;; Persistence facade
;;
;; The namespace and extension slot are spelled `persistance`; public
;; prose uses the correct domain word: Persistence.
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

;; Session lifecycle (storage facade)
(def db-store-session!                 persistance/db-store-session!)
(def db-get-session                 persistance/db-get-session)
(def db-resolve-session-id          persistance/db-resolve-session-id)
(def db-list-sessions               persistance/db-list-sessions)
(def db-find-session-by-external    persistance/db-find-session-by-external)
(def db-update-session-title!       persistance/db-update-session-title!)
(def db-delete-session-tree!           persistance/db-delete-session-tree!)
(def db-fork-session!                  persistance/db-fork-session!)
(def db-list-session-states            persistance/db-list-session-states)
(def db-latest-session-state-id        persistance/db-latest-session-state-id)

;; Turn lifecycle
(def db-store-session-turn!                        persistance/db-store-session-turn!)
(def db-update-session-turn!                       persistance/db-update-session-turn!)
(def db-list-session-turns-by-status           persistance/db-list-session-turns-by-status)
(def db-list-session-turns        persistance/db-list-session-turns)
(def db-retry-session-turn!                        persistance/db-retry-session-turn!)
(def db-list-session-turn-states                   persistance/db-list-session-turn-states)

;; Iteration lifecycle
(def db-store-iteration!                    persistance/db-store-iteration!)
(def db-list-session-turn-iterations            persistance/db-list-session-turn-iterations)
(def db-list-iteration-vars              persistance/db-list-iteration-vars)

;; Full-text search
(def db-search                          persistance/db-search)

;; Var registry & turn history
(def db-latest-var-registry              persistance/db-latest-var-registry)
(def db-var-history-index                persistance/db-var-history-index)
(def db-var-history                      persistance/db-var-history)
(def db-var-history-timeline             persistance/db-var-history-timeline)
(def db-turn-history                     persistance/db-turn-history)

;; Dependencies
(def db-store-dependency!                   persistance/db-store-dependency!)
(def db-list-dependencies                persistance/db-list-dependencies)

;; Restore
(def db-restore-blocks              persistance/db-restore-blocks)

;; Extension aggregate admin/read facade.
;; Writes go through ext-* helpers so extension_id is runtime-owned.
(def db-get-extension-aggregate        persistance/db-get-extension-aggregate)
(def db-list-extension-aggregates      persistance/db-list-extension-aggregates)

;; Process-restart cleanup
(def db-sweep-orphaned-running-turns! lp/db-sweep-orphaned-running-turns!)

;; =============================================================================
;; Extension contract
;; =============================================================================
(defmacro extension
  "Build extension spec and stamp caller namespace for reload/source tracking."
  [spec]
  `(extension/extension
     (assoc ~spec :ext/source-nses ['~(ns-name *ns*)])))
(def symbol                              extension/symbol)
(def value                               extension/value)
(def render-prompt                       extension/render-prompt)
(def render-tool-result                  extension/render-tool-result)
(def default-error-ir                    extension/default-error-ir)
(def render-string                       extension/render-string)
(def combine-render-values               extension/combine-render-values)
(def render-ir?                          extension/render-ir?)
(def render-value?                       extension/render-value?)
(def literal-ir                          extension/literal-ir)
(def normalize-render-value              extension/normalize-render-value)
(def register-op!                        extension/register-op!)
(def registered-op?                      extension/registered-op?)
(def op-tag                              extension/op-tag)
(def op-presentation                     extension/op-presentation)
(def register-extension!                 extension/register-extension!)
(def registered-extensions               extension/registered-extensions)
(def registered-extension-ids            extension/registered-extension-ids)
(def extension-namespaces                extension/extension-namespaces)
(def extension-id-of-ns                  extension/extension-id-of-ns)
(def registered-extensions-summary       extension/registered-extensions-summary)
(def invoke-symbol-wrapper               extension/invoke-symbol-wrapper)
(def discover-extensions!                extension/discover-extensions!)
(def rediscover!                         manifest/rediscover!)
(def extension-load-failures             manifest/load-failures)
(def deregister-extension!               extension/deregister-extension!)
(def extension-source-markers-of         extension/extension-source-markers-of)
(def channel-contributions-for           extension/channel-contributions-for)

;; Extension-owned durable sidecar helpers. These are for extension callbacks;
;; they fill extension id from the current extension context and reject caller-
;; supplied :extension-id.
(def extension-aggregate-create!         extension-aggregate/extension-aggregate-create!)
(def extension-aggregate-put!            extension-aggregate/extension-aggregate-put!)
(def extension-aggregate-get             extension-aggregate/extension-aggregate-get)
(def extension-list-aggregates           extension-aggregate/extension-list-aggregates)
(def extension-delete-aggregate!         extension-aggregate/extension-delete-aggregate!)
(def extension-update-aggregate!         extension-aggregate/extension-update-aggregate!)

;; =============================================================================
;; Doctor protocol
;;
;; Cross-cutting diagnostic surface. Every extension can declare
;; `:ext/doctor-fn` - a single `(fn [env]) -> seq<msg>` returning
;; diagnostic messages. The host auto-injects `:ext` on each emitted message;
;; the extension self-stamps `:check-id` when it wants the formatter's
;; per-section grouping. `vis doctor` aggregates across every registered
;; extension into a level-aware (info / warn / error) report with 0 / 1 / 2
;; exit code by max level. See plan §1 Q19 + §10.
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
(def config-path                         config/config-path)
(def tty-in                              config/tty-in)
(def tty-out                             config/tty-out)
(def original-stdout                     config/original-stdout)
(def load-config-raw                     config/load-config-raw)
(def load-config                         config/load-config)
(def save-config!                        config/save-config!)
(def remove-config-provider!             config/remove-config-provider!)
(def extension-env-overrides             config/extension-env-overrides)
(def extension-env-status                config/extension-env-status)
(def extension-env-value                 config/extension-env-value)
(def save-extension-env-var!             config/save-extension-env-var!)
(def reload-config!                      config/reload-config!)
(def resolve-config                      config/resolve-config)
(def resolve-db-spec                     config/resolve-db-spec)
(def current-config                      config/current-config)
(def router-opts                         config/router-opts)
(def active-provider                     config/active-provider)
(def active-model                        config/active-model)
(def provider-ids                        config/provider-ids)
(def has-provider?                       config/has-provider?)
(def display-label                       config/display-label)
(def model-name                          config/model-name)
(def provider-base-url                   config/provider-base-url)
(def provider-model-visible?             config/provider-model-visible?)
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
(def restore-sandbox!   env/restore-sandbox!)
(def sci-update-binding! env/sci-update-binding!)
(def bind-and-bump!     env/bind-and-bump!)

;; =============================================================================
;; Turn runtime / iteration loop / environment / sessions
;; =============================================================================
(def turn!                        lp/turn!)
(def ask-code!                    lp/ask-code!)
(def llm-text!                    lp/llm-text!)
(def get-router                   lp/get-router)
(def rebuild-router!              lp/rebuild-router!)
(def resolve-effective-model      lp/resolve-effective-model)
(def set-provider!                lp/set-provider!)

;; SCI execution helpers.
(def answer-form-error              lp/answer-form-error)
;; Historical public helpers removed:
;;   `parinfer-rebalance` + `split-top-level-forms`. Delimiter repair now
;;   happens internally at eval time and is disclosed per block as
;;   `:repaired-source` / `:repaired?`.

;; Environment lifecycle
(def create-environment           lp/create-environment)
(def dispose-environment!         lp/dispose-environment!)
(def install-extension!           lp/install-extension!)
(def sync-active-extension-symbols! lp/sync-active-extension-symbols!)

;; Auto-archive
(def auto-archive-candidates       lp/auto-archive-candidates)
(def auto-archive-hot-symbols!     lp/auto-archive-hot-symbols!)

;; Sessions
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
(def active-extensions                prompt/active-extensions)
(def assemble-stable-prompt-messages prompt/assemble-stable-prompt-messages)
(def build-system-prompt              prompt/build-system-prompt)
(def stable-prompt-text               prompt/stable-prompt-text)
;; `vis.core/build-iteration-context` previously re-exported a
;; retired prompt-control assembly. Replaced with
;; `prompt/build-iteration-context`; the re-export is dropped
;; rather than aliased so any straggler caller fails loud.

(def assemble-initial-messages        prompt/assemble-initial-messages)
;; =============================================================================
;; Channel event bus
;; =============================================================================
(def add-channel-event-listener!
  (requiring-resolve 'com.blockether.vis.internal.channel-events/add-channel-event-listener!))
(def remove-channel-event-listener!
  (requiring-resolve 'com.blockether.vis.internal.channel-events/remove-channel-event-listener!))
(def publish-channel-event!
  (requiring-resolve 'com.blockether.vis.internal.channel-events/publish-channel-event!))
(def channel-event-listeners
  (requiring-resolve 'com.blockether.vis.internal.channel-events/channel-event-listeners))

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
