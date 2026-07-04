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

   These section fns are pure data -> message-seq; they don't mutate
   anything and don't depend on the runtime environment beyond
   what's needed to read the existing scanners. Activation
   contract per plan: every registered extension's `:ext/doctor-fn`
   runs regardless of `:ext/activation-fn`, so the section fns must
   NOT assume `:db-info` or other env keys are present."
  (:require
   [com.blockether.vis.internal.foundation.environment.agents :as agents]))

(defn- format-bytes [^long n]
  ;; Locale-stable formatting - explicit Locale.US so output is
  ;; deterministic across machines (no `253,1 MB` from a comma-decimal
  ;; locale).
  (cond
    (< n 1024)              (str n " B")
    (< n (* 1024 1024))     (String/format java.util.Locale/US "%.1f KB"
                              (object-array [(/ (double n) 1024.0)]))
    (< n (* 1024 1024 1024)) (String/format java.util.Locale/US "%.1f MB"
                               (object-array [(/ (double n) (* 1024.0 1024.0))]))
    :else                   (String/format java.util.Locale/US "%.1f GB"
                              (object-array [(/ (double n) (* 1024.0 1024.0 1024.0))]))))

;; ---------------------------------------------------------------------------
;; ::agents-md - project guidance presence
;; ---------------------------------------------------------------------------

(defn- agents-md-diagnostics [_environment]
  (let [{:keys [found? source path bytes files]} (agents/instructions)]
    (if found?
      ;; Stacked AGENTS.md / CLAUDE.md files are inlined verbatim into the
      ;; PROJECT-INSTRUCTIONS system block; no truncation, no remediation.
      [{:level   :info
        :message (if (> (count files) 1)
                   (str "Project guidance loaded from " (count files)
                     " stacked files (user-global → ancestors → workspace root), "
                     (format-bytes (long (or bytes 0))) " total; innermost: " path)
                   (str "Project guidance loaded from " path
                     " (" (format-bytes (long (or bytes 0))) ", source: " (name source) ")"))}]
      [{:level       :warn
        :message     "No project guidance found (no AGENTS.md / CLAUDE.md at the workspace root, its ancestors, or ~/.vis)."
        :remediation "Add `AGENTS.md` to your repo root with the rules / conventions you want vis to follow every turn."}])))

;; ---------------------------------------------------------------------------
;; The single fn the foundation extension wires into
;; `:ext/doctor-fn`. Order is intentional and scoped to foundation-owned
;; diagnostics. Each section stamps its own `:check-id` for formatter labels.
;; ---------------------------------------------------------------------------

(defn- stamp [check-id msgs]
  (mapv #(assoc % :check-id check-id) msgs))

(defn doctor-fn
  "Foundation's `:ext/doctor-fn`. Concatenates the foundation
   diagnostic streams into a single message seq."
  [environment]
  (vec
    (stamp ::agents-md (agents-md-diagnostics environment))))
