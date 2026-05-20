(ns com.blockether.vis.ext.foundation.doctor
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
     ::scan-warnings     Aggregated warnings from project-guidance
                          scanners (already surfaced via
                          `(v/scan-warnings)`); promoted to
                          :error level for prominence in the
                          doctor output.

   These section fns are pure data -> message-seq; they don't mutate
   anything and don't depend on the runtime environment beyond
   what's needed to read the existing scanners. Activation
   contract per plan: every registered extension's `:ext/doctor-fn`
   runs regardless of `:ext/activation-fn`, so the section fns must
   NOT assume `:db-info` or other env keys are present."
  (:require
   [com.blockether.vis.ext.foundation.environment.agents :as agents]))

(set! *warn-on-reflection* true)

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
  (let [{:keys [found? source path bytes truncated? original-bytes]} (agents/instructions)]
    (if found?
      [(cond-> {:level   :info
                :message (str "Project guidance loaded from " path
                           " (" (format-bytes (long (or bytes 0))) ", source: " (name source) ")")}
         truncated?
         (assoc :remediation
           (str "File is " (format-bytes (long original-bytes))
             " on disk; only the first 16 KB are inlined. Trim to "
             "~8 KB of essential rules, or read the full content via "
             "`(vis/main-agent-instructions)` from `:code`.")))]
      [{:level       :warn
        :message     "No project guidance found (neither AGENTS.md nor CLAUDE.md in the repo root)."
        :remediation "Add `AGENTS.md` to your repo root with the rules / conventions you want vis to follow every turn."}])))

;; ---------------------------------------------------------------------------
;; ::scan-warnings - promotes scanner warnings into doctor :error msgs
;; ---------------------------------------------------------------------------

(defn- scan-warnings-diagnostics [_environment]
  (let [warnings (agents/scan-warnings)]
    (if (empty? warnings)
      []
      (mapv (fn [{:keys [path reason source]}]
              {:level       :error
               :message     (str path ": " reason)
               :remediation (case source
                              :agents-md
                              (str "Verify the file is readable; project guidance auto-refreshes "
                                "when AGENTS.md/CLAUDE.md markers change. Run `bin/vis doctor` to revalidate now.")
                              :claude-md-fallback
                              (str "Verify the file is readable; or add a proper `AGENTS.md` instead of "
                                "relying on the CLAUDE.md fallback.")
                              "Investigate, fix, then revalidate via `bin/vis doctor`.")
               :data        {:path path :source source}})
        warnings))))

;; ---------------------------------------------------------------------------
;; The single fn the foundation extension wires into
;; `:ext/doctor-fn`. Order is intentional: project-guidance presence,
;; then any scan failures. Each section stamps its own
;; `:check-id` so the formatter still groups the output under the
;; documented prefixes.
;; ---------------------------------------------------------------------------

(defn- stamp [check-id msgs]
  (mapv #(assoc % :check-id check-id) msgs))

(defn doctor-fn
  "Foundation's `:ext/doctor-fn`. Concatenates the foundation
   diagnostic streams into a single message seq."
  [environment]
  (vec
    (concat
      (stamp ::agents-md     (agents-md-diagnostics     environment))
      (stamp ::scan-warnings (scan-warnings-diagnostics environment)))))
