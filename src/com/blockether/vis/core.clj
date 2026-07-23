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
     - Prompt / Python-sandbox / formatting / cancellation /
       notifications / doctor helpers shared by channels and extensions.

   Not every export is equally high-level. `send!`, extension maps,
   registry builders, read-side persistence helpers, and Markdown export
   are the preferred integration surface. Low-level sandbox, parse-repair,
   dispatcher, and write-side db helpers are exported because this is a
   host facade, but ordinary extensions should avoid depending on them
   unless they are implementing host-level behavior.

   Binary entry: -main (invoked by `clojure -M:vis`, AOT'd to a Java
   entry class via `:gen-class` for the GraalVM native-image build)."
  (:refer-clojure :exclude [symbol])
  (:gen-class)
  (:require [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.ctx-renderer :as ctx-renderer]
            [com.blockether.vis.internal.doctor :as doctor]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.error :as error]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.extension-aggregate :as extension-aggregate]
            [com.blockether.vis.internal.form :as form]
            [com.blockether.vis.internal.import :refer [import-vars]]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.server :as gateway]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.limits-format :as limits-format]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.titling :as titling]
            [com.blockether.vis.internal.main :as binary]
            [com.blockether.vis.internal.manifest :as manifest]
            [com.blockether.vis.internal.render :as ir]
            [com.blockether.vis.internal.notifications :as notifications]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.progress :as progress]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.pyfmt :as pyfmt]
            [com.blockether.vis.internal.python-extensions :as python-extensions]
            [com.blockether.vis.internal.python-test-runner :as python-test-runner]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.session-model :as session-model]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.process-jail :as process-jail]
            [com.blockether.vis.internal.slash :as slash]
            [com.blockether.vis.internal.theme :as theme]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.workspace :as workspace]))

;; =============================================================================
;; Gateway (HTTP/SSE server over the session/turn runtime)
;;
;; `gateway-register-routes!` is the classpath auto-mount seam: an
;; extension registers a route contribution at
;; namespace load — and namespaces load via the META-INF/vis-extension
;; manifest scan, so dropping the jar mounts the routes with zero wiring.
;; The gateway-session/turn helpers below are the host contract a route
;; contribution builds its handlers from.
;; =============================================================================
(import-vars [gateway-start! gateway/start!]
             [gateway-stop! gateway/stop!]
             [gateway-running? gateway/running?]
             [gateway-auth-required? gateway/auth-required?]
             [gateway-register-routes! gateway/register-routes!]
             [gateway-deregister-routes! gateway/deregister-routes!]
             [gateway-ensure! gateway-client/ensure-gateway!]
             [gateway-ensure-serving! gateway-client/ensure-gateway-serving!]
             [gateway-daemon-status gateway-client/status]
             [gateway-daemon-stop! gateway-client/stop-daemon!]
             [gateway-create-session! gateway-client/create-session!]
             [gateway-soul gateway-client/soul]
             [gateway-list-sessions gateway-client/list-sessions]
             [gateway-close-session! gateway-client/close-session!]
             [gateway-list-projects gateway-client/list-projects]
             [gateway-get-project gateway-client/get-project]
             [gateway-create-project! gateway-client/create-project!]
             [gateway-ensure-project-for-root! gateway-client/ensure-project-for-root!]
             [gateway-update-project! gateway-client/update-project!]
             [gateway-delete-project! gateway-client/delete-project!]
             [gateway-assign-project! gateway-client/assign-project!]
             [gateway-reorder-project-sessions! gateway-client/reorder-project-sessions!])

(import-vars [gateway-release-session! gateway-client/release-session!]
             [gateway-release-session-runtime! gateway-client/release-session-runtime!]
             [gateway-submit-turn! gateway-client/submit-turn!])

(import-vars [gateway-submit-turn-sync! gateway-client/submit-turn-sync!]
             [gateway-attach-turn-sync! gateway-client/attach-turn-sync!]
             [gateway-update-queued-turn! gateway-client/update-queued-turn!]
             [gateway-delete-queued-turn! gateway-client/delete-queued-turn!]
             [gateway-cancel-turn! gateway-client/cancel-turn!]
             [gateway-cancel-current-turn! gateway-client/cancel-current-turn!]
             [gateway-drain-idle! gateway-client/drain-idle!]
             [gateway-get-turn gateway-client/get-turn]
             [gateway-list-turns gateway-client/list-turns]
             [gateway-transcript gateway-client/transcript]
             [gateway-transcript-md gateway-client/transcript-md]
             [gateway-transcript-html gateway-client/transcript-html]
             [gateway-turn-trace gateway-client/turn-trace]
             [gateway-reconcile-running-turns! gateway-client/reconcile-running-turns!]
             [gateway-context-snapshot gateway-client/context-snapshot]
             [gateway-subscribe! gateway-client/subscribe!]
             [gateway-unsubscribe! gateway-client/unsubscribe!]
             [gateway-current-seq gateway-client/current-seq]
             [gateway-mux-subscribe! gateway-client/mux-subscribe!]
             [gateway-mux-unsubscribe! gateway-client/mux-unsubscribe!])

(import-vars [gateway-list-resources gateway-client/list-resources]
             [gateway-list-resources-cached gateway-client/list-resources-cached]
             [gateway-stop-resource! gateway-client/stop-resource!]
             [gateway-restart-resource! gateway-client/restart-resource!]
             [gateway-resource-logs gateway-client/resource-logs]
             [gateway-list-startables gateway-client/list-startables]
             [gateway-start-resource! gateway-client/start-resource!]
             [gateway-iteration-attachment-bytes gateway-client/iteration-attachment-bytes])

(import-vars [gateway-events-since gateway-client/events-since]
             [gateway-session-model gateway-client/session-model]
             [gateway-session-model-cached gateway-client/session-model-cached]
             [gateway-set-session-model! gateway-client/set-session-model!]
             [gateway-provider-status gateway-client/provider-status]
             [gateway-provider-limits gateway-client/provider-limits])
;; Channel-neutral per-session model preference (shared store). The TUI uses
;; these directly; the gateway aliases above delegate to the same store, so
;; web + TUI route a session through the same persisted model.
(import-vars [session-model-of session-model/model-of]
             [session-model-of-cached session-model/model-of-cached]
             [set-session-model! session-model/set-model!]
             [gateway-session-workspace gateway-client/session-workspace-info]
             [gateway-add-filesystem-root! gateway-client/add-filesystem-root!]
             [gateway-remove-filesystem-root! gateway-client/remove-filesystem-root!]
             [gateway-change-root! gateway-client/change-root!]
             [gateway-list-drafts gateway-client/list-drafts]
             [gateway-stash-draft! gateway-client/stash-draft!]
             [gateway-resume-draft! gateway-client/resume-draft!]
             [gateway-create-draft! gateway-client/create-draft!]
             [gateway-abandon-draft! gateway-client/abandon-draft!])

;; =============================================================================
;; Cancellation
;; =============================================================================
(import-vars [cancellation-token cancellation/cancellation-token]
             [cancellation-atom cancellation/cancellation-atom]
             [cancellation-set-future! cancellation/cancellation-set-future!]
             [on-cancel! cancellation/on-cancel!]
             [cancel! cancellation/cancel!]
             [cancelled? cancellation/cancelled?]
             [cancellation? cancellation/cancellation?]
             [virtual-threads-available? cancellation/virtual-threads-available?]
             [worker-runtime cancellation/worker-runtime]
             [worker-future cancellation/worker-future])

;; =============================================================================
;; Feature toggles (channels + extensions read this; TUI settings flips it)
;; =============================================================================
(import-vars [register-toggle! toggles/register-toggle!]
             [register-toggles! toggles/register-toggles!]
             [registered-toggles toggles/registered-toggles]
             [toggle-spec toggles/toggle-spec]
             [toggle-enabled? toggles/enabled?]
             [toggle-value toggles/value-of]
             [toggle-choices toggles/choices-of]
             [toggle-type toggles/type-of]
             [toggle-set-enabled! toggles/set-enabled!]
             [toggle-set-value! toggles/set-value!]
             [toggle-cycle-value! toggles/cycle-value!]
             [toggle-reset-to-default! toggles/reset-to-default!]
             [toggles-snapshot toggles/snapshot]
             [toggles-hydrate-from-config! toggles/hydrate-from-config!]
             [visible-toggles toggles/visible-toggles]
             [toggles-for-channel toggles/toggles-for-channel]
             [toggle-add-listener! toggles/add-listener!])

;; =============================================================================
;; Workspace
;; =============================================================================
(import-vars [workspace-cwd workspace/cwd]
             [workspace-root workspace/workspace-root]
             [workspace-normalize-root workspace/normalize-root]
             [workspace-subdirs workspace/subdirs]
             [workspace-create-dir! workspace/create-dir!]
             [workspace-filesystem-roots workspace/filesystem-roots]
             [workspace-add-filesystem-root! workspace/add-filesystem-root!]
             [workspace-remove-filesystem-root! workspace/remove-filesystem-root!]
             [workspace-change-root! workspace/change-root!]
             [workspace-backend workspace/workspace-backend]
             [workspace-register-backend! workspace/register-backend!]
             [workspace-deregister-backend! workspace/deregister-backend!]
             [workspace-registered-backends workspace/registered-backends]
             [workspace-capability-matrix workspace/capability-matrix]
             [workspace-capabilities-for workspace/workspace-capability-matrix]
             [workspace-supports? workspace/supports?]
             [workspace-isolation-supported? workspace/isolated-workspaces-supported?]
             [workspace-get workspace/get]
             [workspace-list-active workspace/list-active]
             [workspace-list-finished workspace/list-finished]
             [workspace-for-session workspace/for-session]
             [workspace-status workspace/status]
             [workspace-trunk-info workspace/trunk-info]
             [workspace-ensure-workspace! workspace/ensure-workspace!]
             [workspace-create! workspace/create!]
             [workspace-create-trunk-at! workspace/create-trunk-at!]
             [workspace-apply! workspace/apply!]
             [workspace-abandon! workspace/abandon!]
             [workspace-set-label! workspace/set-label!]
             [workspace-focus! workspace/focus!]
             [workspace-last-focused workspace/last-focused]
             [workspace-display-label workspace/display-label]
             [workspace-with-session workspace/workspace-with-session]
             [workspace-list-active-with-sessions workspace/list-active-with-sessions]
             [workspace-register-hook! workspace/register-hook!])

;; =============================================================================
;; Slash commands (declarative `:ext/slash-commands`)
;; =============================================================================
(import-vars [active-slashes slash/active-slashes]
             [registered-slashes slash/registered-slashes]
             [registered-startable-resources extension/registered-startable-resources]
             [slash-by-path slash/slash-by-path]
             [slash-children slash/slash-children]
             [slash-palette slash/slash-palette]
             [slash-parse slash/parse]
             [slash-dispatch slash/dispatch])

;; =============================================================================
;; Theme facade
;; =============================================================================
(import-vars [default-theme-id theme/default-theme-id]
             [default-theme theme/default-theme]
             [vis-light theme/vis-light]
             [vis-dark theme/vis-dark]
             [themes theme/themes]
             [palette theme/palette]
             [pallete theme/pallete]
             [theme theme/theme]
             [color theme/color]
             [theme-registry theme/theme-registry]
             [register-theme! theme/register-theme!]
             [register-themes! theme/register-themes!]
             [unregister-theme! theme/unregister-theme!]
             [unregister-themes! theme/unregister-themes!]
             [reset-themes! theme/reset-themes!]
             [extension-theme-settings theme/extension-theme-settings]
             [available-theme-ids theme/available-theme-ids]
             [theme->web-css-vars theme/theme->web-css-vars]
             [web-css-root theme/web-css-root])

;; =============================================================================
;; Error formatting
;; =============================================================================
(import-vars [error-message error/error-message]
             [format-error error/format-error]
             [final-answer-code-error-message error/final-answer-code-error-message]
             [db-error->user-message persistance/db-error->user-message])

;; =============================================================================
;; Format helpers
;; =============================================================================
(import-vars [format-date fmt/format-date]
             [format-clojure fmt/format-clojure]
             [format-duration fmt/format-duration]
             [format-tokens fmt/format-tokens]
             [format-cost fmt/format-cost]
             [format-iterations fmt/format-iterations]
             [format-meta-line fmt/format-meta-line]
             [display-model-name fmt/display-model-name]
             [humanize-fact-key fmt/humanize-fact-key]
             [meta-summary-line fmt/meta-summary-line]
             [meta-tokens fmt/meta-tokens]
             [meta-cost fmt/meta-cost]
             [meta-fallback-note fmt/meta-fallback-note])

;; Per-model price table (USD / MILLION tokens) — the SAME table the loop bills
;; against, exposed read-only so channel model pickers show the real charge.
(import-vars [model-pricing lp/model-pricing])

;; =============================================================================
;; Notifications
;;
;; Cross-channel ephemeral signals - \"copied\", \"tests passed\",
;; \"provider switched\". Any extension or channel can push via
;; `notify!`; the TUI banner / CLI agent each
;; subscribe with `watch!` and surface entries in their own visual
;; idiom. Levels (`:info` / `:success` / `:warn` / `:error`) are
;; advisory metadata for the consumer.
;; =============================================================================
(import-vars [notify! notifications/notify!]
             [notifications notifications/notifications]
             [dismiss! notifications/dismiss!]
             [dismiss-all! notifications/dismiss-all!]
             [watch-notifications! notifications/watch!]
             [unwatch-notifications! notifications/unwatch!])

;; =============================================================================
;; Markdown export
;;
;; Single host-runtime helper for projecting a persisted session
;; (every turn: user prompt + final answer + optional metadata) into a
;; Markdown parsing/rendering helpers. Parsed Markdown trees are renderer-local;
;; canonical answers are string-keyed content blocks.
;; =============================================================================
(import-vars [session->markdown ir/session->markdown])

(import-vars [render ir/render] [->ast ir/->ast])
;; Canonical per-form DISPLAY contract — channels project the whole form-display
;; field set through ONE list (see internal/form.clj): `->display` outbound,
;; `<-wire` inbound (tolerant of the gateway wire's snake_case + keyword values).
(import-vars [form-display-keys form/display-keys]
             [form->display form/->display]
             [form-with-display-code form/with-display-code]
             [form<-wire form/<-wire]
             [tool-label form/tool-label]
             [result-card form/result-card]
             [result-cards form/result-cards]
             [native-tool-form? form/native-tool-form?]
             [hide-tool-code? form/hide-tool-code?]
             [coalesce-forms form/coalesce-forms])
;; Canonical native-tool badge colour-role set — each channel's colour map must
;; cover every role here (guard tests lock it), so the two maps can't drift.
(import-vars [tool-color-roles form/tool-color-roles])

(import-vars [markdown->ast ir/markdown->ast])
;; Shared unified-diff line classifier — TUI maps the kind to ANSI, web to a CSS
;; class, so a diff fence colours identically in both from ONE source.
(import-vars [diff-line-kind ir/diff-line-kind])
;; Shared reasoning/thinking formatting — every channel (TUI bubble + web
;; thinking card) MUST render traces through these so they stay identical.
(import-vars [normalize-reasoning ir/normalize-reasoning]
             [reasoning->ast ir/reasoning->ast]
             [reasoning-preview-line-limit ir/reasoning-preview-line-limit]
             [reasoning-collapse-min-hidden ir/reasoning-collapse-min-hidden])
;; ruff-beautify model Python before display (gateway code blocks). Cached +
;; falls back to verbatim source when ruff is unavailable.
(import-vars [beautify-python pyfmt/beautify-python])
;; Canonical wire JSON (gateway/wire.clj ->wire shape: snake keys,
;; keywords as strings). The pretty variant is for human-facing views.
(import-vars [wire-json-str wire/json-str] [wire-json-pretty wire/json-str-pretty])
;; Canonical gateway values use snake_case STRING keys at every depth.
(import-vars [wire-canonical wire/canonical])
;; ONE key policy: keyword/symbol key -> canonical snake_case STRING
;; (namespace dropped, `foo?` -> `is_foo`) — the exact spelling `->wire`
;; emits, for readers that project canonical maps back by engine keyword.
(import-vars [wire-key wire/wire-key])

;; The gateway serves ONE canonical wire shape on BOTH transports
;; (snake_case STRING keys, `wire/canonical`), so there is NO inbound
;; re-normalization step anymore: channels read `(get event "field")`
;; directly, same-process or remote.
;; THE compressed model-facing string for one form/tool VALUE (internal/
;; ctx_renderer.clj) — the exact dispatch trailer pins, so a channel can show a
;; result the way the MODEL reads it (rg gutter, shell model-render, Python
;; printer) instead of pr-str'd Clojure.
(import-vars [render-form-value ctx-renderer/render-form-value]
             [search-text ir/search-text]
             [extract-code ir/extract-code]
             [extract-text ir/extract-text]
             [parse-block-display ir/parse-block-display])

;; =============================================================================
;; Progress tracker
;; =============================================================================
(import-vars [make-progress-tracker progress/make-progress-tracker])

;; =============================================================================
;; CLI dispatcher
;; =============================================================================
(import-vars [command registry/command]
             [resolve-subcommands registry/resolve-subcommands]
             [find-leaf commandline/find-leaf]
             [find-named commandline/find-named]
             [parse-args commandline/parse-args]
             [validate-args commandline/validate-args]
             [unknown-flags commandline/unknown-flags]
             [pad-right commandline/pad-right]
             [pad-left commandline/pad-left]
             [render-command commandline/render-command]
             [render-tree commandline/render-tree]
             [dispatch! commandline/dispatch!]
             [register-cmd! registry/register-cmd!]
             [deregister-cmd! registry/deregister-cmd!]
             [registered-commands registry/registered-commands]
             [registered-under registry/registered-under])

;; =============================================================================
;; Channel registry
;; =============================================================================
(import-vars [channel registry/channel]
             [register-channel! registry/register-channel!]
             [deregister-channel! registry/deregister-channel!]
             [registered-channels registry/registered-channels]
             [channel-by-id registry/channel-by-id]
             [by-cmd registry/by-cmd])

;; =============================================================================
;; Provider registry
;; =============================================================================
(import-vars [provider registry/provider]
             [register-provider! registry/register-provider!]
             [deregister-provider! registry/deregister-provider!]
             [registered-providers registry/registered-providers]
             [provider-by-id registry/provider-by-id]
             [provider-limits provider-limits/provider-limits]
             [all-provider-limits provider-limits/all-provider-limits])

;; =============================================================================
;; Provider management service (channel-neutral; internal/providers.clj)
;;
;; The SAME primitives behind the TUI Router dialog and the web
;; Providers modal: fleet, presets, status probing, account limits,
;; live model catalogs, persistence. Channels add only interaction.
;; =============================================================================
(import-vars [provider-auth-kind providers/auth-kind]
             [provider-oauth-ids providers/oauth-provider-ids]
             [provider-local-no-auth-ids providers/local-no-auth-provider-ids]
             [provider-url-host providers/url-host]
             [provider-fetch-models providers/fetch-models]
             [provider-model-options providers/model-options]
             [provider-default-model-names providers/default-model-names]
             [provider-status providers/provider-status]
             [provider-status-of-registered providers/safe-provider-status]
             [provider-limits-safe providers/provider-limits-safe]
             [provider-initial-status providers/initial-provider-status]
             [provider-initial-limits providers/initial-provider-limits]
             [provider-status-text providers/status-text]
             [provider-status-md providers/status-md]
             [configured-providers providers/configured-providers]
             [configured-providers-cached providers/configured-providers-cached]
             [provider-presets-available providers/available-presets]
             [provider-ensure-base-url providers/ensure-base-url]
             [provider-persisted-config providers/persisted-provider-config]
             [provider-default-model-configs providers/default-model-configs]
             [provider-config-with-models providers/provider-config-with-models]
             [save-config-providers! providers/save-providers!]
             [add-config-provider! providers/add-config-provider!]
             [update-config-provider! providers/update-config-provider!]
             [remove-provider! providers/remove-provider!])

;; Channel-neutral limits row formatting (internal/limits_format.clj):
;; the compact quota summaries every surface (TUI footer + cards, web
;; cards) renders identically.
(import-vars [limits-dynamic-summary limits-format/dynamic-summary]
             [limits-label+usage limits-format/label+usage]
             [limits-format-usage limits-format/format-limit-usage]
             [limits-format-number limits-format/format-limit-number]
             [limits-generic-label limits-format/generic-limit-label]
             [limits-percentage-row? limits-format/percentage-limit-row?]
             [limits-row-has-signal? limits-format/generic-limit-has-signal?])

;; =============================================================================
;; Persistence facade
;;
;; The namespace and extension slot are spelled `persistance`; public
;; prose uses the correct domain word: Persistence.
;; =============================================================================
(import-vars [ds persistance/ds]
             [now-ms persistance/now-ms]
             [->id persistance/->id]
             [->uuid persistance/->uuid]
             [->ref persistance/->ref]
             [->kw persistance/->kw]
             [->kw-back persistance/->kw-back]
             [->epoch-ms persistance/->epoch-ms]
             [->date persistance/->date]
             [db-create-connection! persistance/db-create-connection!]
             [db-dispose-connection! persistance/db-dispose-connection!]
             [db-shared-connection! persistance/db-shared-connection!]
             [db-dispose-shared-connection! persistance/db-dispose-shared-connection!]
             [register-backend! persistance/register-backend!]
             [deregister-backend! persistance/deregister-backend!]
             [registered-backends persistance/registered-backends])

;; Logging
(import-vars [db-log! persistance/db-log!])

;; Session lifecycle (storage facade)
(import-vars [db-store-session! persistance/db-store-session!]
             [db-workspace-insert! persistance/db-workspace-insert!]
             [db-get-session persistance/db-get-session]
             [db-resolve-session-id persistance/db-resolve-session-id]
             [db-list-sessions persistance/db-list-sessions]
             [db-find-session-by-external persistance/db-find-session-by-external]
             [db-update-session-title! persistance/db-update-session-title!]
             [db-claim-session! persistance/db-claim-session!]
             [db-delete-session-tree! persistance/db-delete-session-tree!]
             [db-fork-session! persistance/db-fork-session!]
             [db-fork-session-at-turn! persistance/db-fork-session-at-turn!]
             [db-list-session-states persistance/db-list-session-states]
             [db-latest-session-state-id persistance/db-latest-session-state-id])

(import-vars [db-store-session-turn! persistance/db-store-session-turn!]
             [db-update-session-turn! persistance/db-update-session-turn!]
             [db-list-session-turns-by-status persistance/db-list-session-turns-by-status]
             [db-list-session-turns persistance/db-list-session-turns]
             [db-retry-session-turn! persistance/db-retry-session-turn!]
             [db-list-session-turn-states persistance/db-list-session-turn-states]
             [db-list-turn-attachments persistance/db-list-turn-attachments]
             [db-list-turns-attachments persistance/db-list-turns-attachments]
             [db-list-turn-all-attachments persistance/db-list-turn-all-attachments]
             [db-list-session-attachments persistance/db-list-session-attachments])

;; Iteration lifecycle
(import-vars [db-store-iteration! persistance/db-store-iteration!]
             [db-list-session-turn-iterations persistance/db-list-session-turn-iterations]
             [db-list-iteration-attachments persistance/db-list-iteration-attachments]
             [db-list-iterations-attachments persistance/db-list-iterations-attachments]
             [db-read-attachment persistance/db-read-attachment])

;; Full-text search
(import-vars [db-search persistance/db-search])

;; Turn history
(import-vars [db-turn-history persistance/db-turn-history]
             [db-load-latest-ctx persistance/db-load-latest-ctx]
             [db-load-ctx-history persistance/db-load-ctx-history])

;; Extension aggregate admin/read facade.
;; Writes go through ext-* helpers so extension_id is runtime-owned.
(import-vars [db-get-extension-aggregate persistance/db-get-extension-aggregate]
             [db-list-extension-aggregates persistance/db-list-extension-aggregates])

;; Process-restart cleanup
(import-vars [db-sweep-orphaned-running-turns! lp/db-sweep-orphaned-running-turns!])

;; =============================================================================
;; Extension contract
;; =============================================================================
(defmacro extension
  "Build extension spec and stamp caller namespace for reload/source tracking."
  [spec]
  `(extension/extension (assoc ~spec :ext/source-nses ['~(ns-name *ns*)])))

(import-vars [symbol extension/symbol]
             [value extension/value]
             [render-prompt extension/render-prompt]
             [op-tag extension/op-tag]
             [op-presentation extension/op-presentation]
             [register-extension! extension/register-extension!]
             [register-op-hook! extension/register-op-hook!]
             [unregister-op-hooks-for-owner! extension/unregister-op-hooks-for-owner!]
             [registered-extensions extension/registered-extensions]
             [registered-extension-ids extension/registered-extension-ids]
             [extension-namespaces extension/extension-namespaces]
             [extension-id-of-ns extension/extension-id-of-ns]
             [registered-extensions-summary extension/registered-extensions-summary]
             [invoke-symbol-wrapper extension/invoke-symbol-wrapper]
             [discover-extensions! extension/discover-extensions!]
             [rediscover! manifest/rediscover!]
             [extension-load-failures manifest/load-failures]
             [deregister-extension! extension/deregister-extension!]
             [extension-source-markers-of extension/extension-source-markers-of]
             [channel-contributions-for extension/channel-contributions-for])

;; Project-local Python extensions (`~/.vis/extensions` + `.vis/extensions`,
;; trusted GraalPy contexts — see `internal.python-extensions`).
(import-vars [load-python-extensions! python-extensions/load-python-extensions!]
             [reload-python-extensions! python-extensions/reload-python-extensions!]
             [python-extension-load-failures python-extensions/load-failures]
             [loaded-python-extensions python-extensions/loaded-python-extensions]
             [add-python-extension-change-listener! python-extensions/add-change-listener!]
             [remove-python-extension-change-listener! python-extensions/remove-change-listener!]
             [test-python-extensions! python-test-runner/test-python-extensions!])

;; Extension-owned durable sidecar helpers. These are for extension callbacks;
;; they fill extension id from the current extension context and reject caller-
;; supplied :extension-id.
(import-vars [extension-aggregate-create! extension-aggregate/extension-aggregate-create!]
             [extension-aggregate-put! extension-aggregate/extension-aggregate-put!]
             [extension-aggregate-get extension-aggregate/extension-aggregate-get]
             [extension-list-aggregates extension-aggregate/extension-list-aggregates]
             [extension-delete-aggregate! extension-aggregate/extension-delete-aggregate!]
             [extension-update-aggregate! extension-aggregate/extension-update-aggregate!])

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
(import-vars [run-doctor-checks doctor/run-checks]
             [doctor-exit-code doctor/exit-code]
             [doctor-format-output doctor/format-output]
             [doctor-startup-hint doctor/startup-hint-line])

;; =============================================================================
;; Configuration / paths / logging
;; =============================================================================
(import-vars [init! config/init!]
             [init-cli! config/init-cli!]
             [shutdown! config/shutdown!]
             [state-path config/state-path]
             [tty-in config/tty-in]
             [tty-out config/tty-out]
             [original-stdout config/original-stdout]
             [load-config-raw config/load-config-raw]
             [runtime-config config/runtime-config]
             [load-config config/load-config]
             [save-config! config/save-config!]
             [remove-config-provider! config/remove-config-provider!]
             [extension-env-overrides config/extension-env-overrides]
             [extension-env-status config/extension-env-status]
             [extension-env-value config/extension-env-value]
             [save-extension-env-var! config/save-extension-env-var!]
             [reload-config! config/reload-config!]
             [resolve-config config/resolve-config]
             [provider-configured? config/provider-configured?]
             [first-run? config/first-run?]
             [resolve-db-spec config/resolve-db-spec]
             [current-config config/current-config]
             [router-opts config/router-opts]
             [active-provider config/active-provider]
             [active-model config/active-model]
             [provider-ids config/provider-ids]
             [has-provider? config/has-provider?]
             [display-label config/display-label]
             [model-name config/model-name]
             [provider-base-url config/provider-base-url]
             [provider-model-visible? config/provider-model-visible?]
             [provider-presets config/provider-presets]
             [provider-template config/provider-template]
             [->svar-model config/->svar-model]
             [->svar-provider config/->svar-provider])

;; =============================================================================
;; Python sandbox
;; =============================================================================
(import-vars [SYSTEM_VAR_NAMES env/SYSTEM_VAR_NAMES]
             [system-var-sym? env/system-var-sym?]
             [create-python-context env/create-python-context]
             [set-python-binding! env/set-python-binding!]
             [bind-and-bump! env/bind-and-bump!])

;; =============================================================================
;; Stateful-resource registry — the canonical interface owners use to register a
;; long-lived thing vis manages (nREPL, daemon, watch…). Session-scoped: every
;; verb takes the owning session id. See `internal.resources`.
;; =============================================================================

(import-vars [register-resource! resources/register!]
             [update-resource! resources/update!]
             [unregister-resource! resources/unregister!]
             [list-resources resources/list-resources]
             [get-resource resources/get-resource]
             [resource-logs resources/logs]
             [stop-resource! resources/stop!]
             [restart-resource! resources/restart!])

;; Standard language-process jail contract — packs obtain argv + proxy env in one
;; fail-closed policy resolution before spawning a managed REPL or test runner.
(import-vars [prepare-session-jail! process-jail/prepare-session-jail!]
             [session-process-launch process-jail/session-process-launch])

;; =============================================================================
;; Turn runtime / iteration loop / environment / sessions
;; =============================================================================
(import-vars [turn! lp/turn!]
             [ask-code! lp/ask-code!]
             [llm-text! lp/llm-text!]
             [get-router lp/get-router]
             [router-initialized? lp/router-initialized?]
             [rebuild-router! lp/rebuild-router!]
             [resolve-effective-model lp/resolve-effective-model]
             [model-routing-status lp/model-routing-status]
             [set-provider! lp/set-provider!])

;; Historical public helpers removed:
;;   `parinfer-rebalance` + `split-top-level-forms`. Delimiter repair now
;;   happens internally at eval time and is disclosed per block as
;;   `:repaired-source` / `:repaired?`.

;; Environment lifecycle
(import-vars [create-environment lp/create-environment]
             [dispose-environment! lp/dispose-environment!]
             [install-extension! lp/install-extension!]
             [sync-active-extension-symbols! lp/sync-active-extension-symbols!])

;; Auto-archive
(import-vars [auto-archive-hot-symbols! lp/auto-archive-hot-symbols!])

;; Sessions
(import-vars [db-info lp/db-info]
             [custom-bindings lp/custom-bindings]
             [get-locals lp/get-locals]
             [cache-env! lp/cache-env!]
             [refresh-cached-routers! lp/refresh-cached-routers!]
             [create! lp/create!]
             [by-id lp/by-id]
             [by-channel lp/by-channel]
             [set-title! lp/set-title!]
             [add-title-listener! titling/add-title-listener!]
             [remove-title-listener! titling/remove-title-listener!]
             [add-title-pending-listener! titling/add-title-pending-listener!]
             [remove-title-pending-listener! titling/remove-title-pending-listener!]
             [env-for lp/env-for]
             [send! lp/send!]
             [close! lp/close!]
             [delete! lp/delete!]
             [close-all! lp/close-all!])

;; =============================================================================
;; Prompt builders
;; =============================================================================
(import-vars [active-extensions prompt/active-extensions]
             [assemble-stable-prompt-messages prompt/assemble-stable-prompt-messages]
             [build-system-prompt prompt/build-system-prompt]
             [stable-prompt-text prompt/stable-prompt-text])
;; `vis.core/build-iteration-context` is intentionally NOT re-exported:
;; callers must use `prompt/build-iteration-context` directly.

(import-vars [assemble-initial-messages prompt/assemble-initial-messages])
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

(defn -main [& args] (apply binary/-main args))
