(ns com.blockether.vis.internal.foundation.doctor
  "Foundation's contribution to the host `vis doctor` aggregator. ONE fn
   (`doctor-fn`) returns the full message stream from two logical
   sections, each stamping its own `:check-id` so the formatter
   groups them under the same banner the original four-checks-vec
   shape produced (plan §1 Q18 / §10):

     ::agents-md         AGENTS.md presence / source / size; one
                          :info line when found, one :warn line
                          when neither AGENTS.md nor CLAUDE.md exists
                          (rules silently absent is worth flagging
                          even though it isn't an error per se).

     ::provider-env      providers whose `${NAME}` config references point at
                          environment variables that are not set. Config loads
                          leniently by design, so `vis doctor` is the moment a
                          user actually LOOKS — the right place to fail fast
                          without failing the gateway.

   These section fns are pure data -> message-seq; they don't mutate
   anything and don't depend on the runtime environment beyond
   what's needed to read the existing scanners. Activation
   contract per plan: every registered extension's `:ext/doctor-fn`
   runs regardless of `:ext/activation-fn`, so the section fns must
   NOT assume `:db-info` or other env keys are present."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.foundation.environment.agents :as agents]
            [com.blockether.vis.internal.foundation.housekeeping :as housekeeping]))

;; ---------------------------------------------------------------------------
;; ::agents-md - project guidance presence
;; ---------------------------------------------------------------------------

(defn- agents-md-diagnostics
  [_environment]
  (let [{:keys [found? source path bytes files]} (agents/instructions)]
    (if found?
      ;; Stacked AGENTS.md / CLAUDE.md files are inlined verbatim into the
      ;; PROJECT-INSTRUCTIONS system block; no truncation, no remediation.
      [{:level :info
        :message (if (> (count files) 1)
                   (str "Project guidance loaded from " (count files)
                        " stacked files (user-global → ancestors → workspace root), "
                        (housekeeping/format-bytes (long (or bytes 0)))
                        " total; innermost: " path)
                   (str "Project guidance loaded from "
                        path
                        " ("
                        (housekeeping/format-bytes (long (or bytes 0)))
                        ", source: "
                        (name source)
                        ")"))}]
      [{:level :warn
        :message
        "No project guidance found (no AGENTS.md / CLAUDE.md at the workspace root, its ancestors, or ~/.vis)."
        :remediation
        "Add `AGENTS.md` to your repo root with the rules / conventions you want vis to follow every turn."}])))

;; ---------------------------------------------------------------------------
;; ::provider-env - unresolved ${NAME} references in provider config
;; ---------------------------------------------------------------------------

(defn- provider-env-diagnostics
  [_environment]
  ;; Never throws: a broken/absent config must not take the whole doctor run
  ;; down, and "no config" is simply nothing to report here.
  (let [gaps (try (config/provider-env-gaps (config/load-config)) (catch Throwable _ nil))]
    (mapv (fn [[provider-id env-vars]]
            {:level :warn
             :message (str (config/provider-env-message provider-id env-vars)
                           " — the provider is configured but stays unusable")
             :remediation (str "export "
                               (str/join " and " env-vars)
                               ", or remove the '"
                               (name provider-id)
                               "' provider from your config.")
             :data {:provider provider-id :env-vars env-vars}})
          gaps)))

;; ---------------------------------------------------------------------------
;; ::housekeeping - stale drafts and gateway journals
;; ---------------------------------------------------------------------------

(defn- housekeeping-diagnostics
  "Vis owns two directories that only ever grow: `~/.vis/drafts` (a full
   working-tree clone per draft, the biggest thing on disk by far) and
   `~/.vis/gateway/events` (one NDJSON journal per session). Nothing reclaims
   an abandoned draft the operator never abandoned explicitly, so after a few
   busy days the store quietly holds gigabytes nobody is going to open again.

   Reported as :info, never :warn: stale state is untidy, not broken, and
   `vis doctor` must not start exiting 1 on a machine that is working fine."
  [environment]
  (let
    [{:keys [days bytes drafts journals] :as report}
     (try (housekeeping/scan {:db-info (:db-info environment)
                              :days (:housekeeping-days environment)})
          (catch Throwable _ nil))

     n
     (long (or (:count report) 0))]

    (when (pos? n)
      [{:level :info
        :message (str n
                      (if (= 1 n) " item" " items")
                      " untouched for over "
                      days
                      " days: "
                      (count (:reclaimable drafts))
                      " draft workspace(s) and "
                      (count (:reclaimable journals))
                      " session journal(s), "
                      (housekeeping/format-bytes (long (or bytes 0)))
                      " reclaimable.")
        :remediation
        "Review with `vis doctor --purge --dry-run`, then reclaim with `vis doctor --purge` (add `--days N` to change the 14-day cutoff)."
        :data {:days days
               :count n
               :bytes bytes
               :drafts-root (:root drafts)
               :journals-root (:root journals)}}])))

;; ---------------------------------------------------------------------------
;; The single fn the foundation extension wires into
;; `:ext/doctor-fn`. Order is intentional and scoped to foundation-owned
;; diagnostics. Each section stamps its own `:check-id` for formatter labels.
;; ---------------------------------------------------------------------------

(defn- stamp [check-id msgs] (mapv #(assoc % :check-id check-id) msgs))

(defn doctor-fn
  "Foundation's `:ext/doctor-fn`. Concatenates the foundation
   diagnostic streams into a single message seq."
  [environment]
  (vec (concat (stamp ::agents-md (agents-md-diagnostics environment))
               (stamp ::provider-env (provider-env-diagnostics environment))
               (stamp ::housekeeping (housekeeping-diagnostics environment)))))
