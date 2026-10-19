(ns com.blockether.vis.core
  "vis - binary entry and a small host facade for tests and REPL work.

   `-main` is the binary entry. `clojure -M:vis` and the native-image build
   use it through `:gen-class`. The commands live in
   `com.blockether.vis.internal.main`.

   The other names re-export persistence, registry, toggle and config helpers
   that tests and `dev/` use. Nothing under `com.blockether.vis.internal`
   requires this namespace. Extensions use the Python SDK
   `blockether.vis.extension`, not this namespace."
  (:gen-class)
  (:require [com.blockether.vis.internal.extension.capability :as capability]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.environment.repositories :as repositories]
            [com.blockether.vis.internal.import :refer [import-vars]]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.main :as binary]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.config.toggles :as toggles]))

;; A pack that RECORDS a terminal verdict must be able to drop it again in its own
;; tests: the registry is process-wide by design, so one such test would otherwise
;; answer every later test in that JVM from memory.
(import-vars [capability-forget-verdicts! capability/forget-verdicts!])

;; Feature toggles (channels + extensions read this; TUI settings flips it)
(import-vars [registered-toggles toggles/registered-toggles]
             [toggle-spec toggles/toggle-spec]
             [toggle-enabled? toggles/enabled?])

(import-vars [repository-inventory repositories/inventory])

(import-vars [provider-by-id registry/provider-by-id]
             [provider-limits provider-limits/provider-limits])

;; Persistence facade
;;
;; The namespace and extension slot are spelled `persistance`; public
;; prose uses the correct domain word: Persistence.
(import-vars [db-create-connection! persistance/db-create-connection!]
             [db-dispose-connection! persistance/db-dispose-connection!])

;; Logging
(import-vars [db-log! persistance/db-log!])

;; Session lifecycle (storage facade)
(import-vars [db-store-session! persistance/db-store-session!]
             [db-workspace-insert! persistance/db-workspace-insert!]
             [db-get-session persistance/db-get-session]
             [db-resolve-session-id persistance/db-resolve-session-id]
             [db-list-sessions persistance/db-list-sessions]
             [db-search-session-ids persistance/db-search-session-ids]
             [db-search-session-matches persistance/db-search-session-matches]
             [db-find-session-by-external persistance/db-find-session-by-external]
             [db-update-session-title! persistance/db-update-session-title!]
             [db-claim-session! persistance/db-claim-session!]
             [db-delete-session-tree! persistance/db-delete-session-tree!]
             [db-fork-session! persistance/db-fork-session!]
             [db-fork-session-at-turn! persistance/db-fork-session-at-turn!]
             [db-list-session-states persistance/db-list-session-states])

(import-vars [db-store-session-turn! persistance/db-store-session-turn!]
             [db-update-session-turn! persistance/db-update-session-turn!]
             [db-list-session-turns persistance/db-list-session-turns]
             [db-retry-session-turn! persistance/db-retry-session-turn!]
             [db-list-session-turn-states persistance/db-list-session-turn-states]
             [db-list-turn-attachments persistance/db-list-turn-attachments]
             [db-set-turn-attachment-transcription!
              persistance/db-set-turn-attachment-transcription!]
             [db-list-turns-attachments persistance/db-list-turns-attachments]
             [db-list-turn-all-attachments persistance/db-list-turn-all-attachments]
             [db-list-session-attachments persistance/db-list-session-attachments]
             [db-list-session-attachments-meta persistance/db-list-session-attachments-meta])

;; Iteration lifecycle
(import-vars [db-store-iteration! persistance/db-store-iteration!]
             [db-list-session-turn-iterations persistance/db-list-session-turn-iterations]
             [db-list-iteration-attachments persistance/db-list-iteration-attachments]
             [db-list-iterations-attachments persistance/db-list-iterations-attachments]
             [db-list-iteration-attachments-meta persistance/db-list-iteration-attachments-meta]
             [db-list-iterations-attachments-meta persistance/db-list-iterations-attachments-meta]
             [db-read-attachment persistance/db-read-attachment])

;; Full-text search
(import-vars [db-search persistance/db-search])

(import-vars [db-turn-history persistance/db-turn-history])

;; Extension aggregate reads
(import-vars [db-list-extension-aggregates persistance/db-list-extension-aggregates])

;; Process-restart cleanup
(import-vars [db-sweep-orphaned-running-turns! lp/db-sweep-orphaned-running-turns!])

;; Extension registry. Built-in extensions require `internal.extension.core`.
(import-vars [registered-extensions extension/registered-extensions])

;; Configuration / paths / logging
(import-vars [current-config config/current-config])

;; Sessions
(import-vars [db-info lp/db-info])

;; Binary entry point
;;
;; `clojure -M:vis` and the native image call this `-main`. The commands live in
;; `com.blockether.vis.internal.main`; this fn only delegates to them.
(defn -main
  "The binary entry; Python workers have their own entrypoint in the runtime."
  [& args]
  (apply binary/-main args))
