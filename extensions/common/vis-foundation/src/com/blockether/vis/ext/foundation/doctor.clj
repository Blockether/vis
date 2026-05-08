(ns com.blockether.vis.ext.foundation.doctor
  "Foundation's contribution to the `vis extensions doctor` aggregator. ONE fn
   (`check-fn`) returns the full message stream from five logical
   sections, each stamping its own `:check-id` so the formatter
   groups them under the same banner the original four-checks-vec
   shape produced (plan §1 Q18 / §10):

     ::system            JVM / OS / Clojure / memory / DB-path facts
                          (lifted from the old host built-in
                          `cli-doctor!`)
     ::agents-md         AGENTS.md presence / source / size; one
                          :info line when found, one :warn line
                          when neither AGENTS.md nor CLAUDE.md exists
                          (rules silently absent is worth flagging
                          even though it isn't an error per se).
     ::skills            Skills catalog summary: count + breakdown
                          by source; :error lines per malformed
                          SKILL.md file.
     ::voice             Optional voice runtime readiness: voice feature,
                          ffmpeg, and Piper espeak-ng-data presence.
     ::scan-warnings     Aggregated warnings from agents + skills
                          scanners (already surfaced via
                          `(v/scan-warnings)`); promoted to
                          :error level for prominence in the
                          doctor output.

   These section fns are pure data -> message-seq; they don't mutate
   anything and don't depend on the runtime environment beyond
   what's needed to read the existing scanners. Activation
   contract per plan: every registered extension's `:ext/doctor-check-fn`
   runs regardless of `:ext/activation-fn`, so the section fns must
   NOT assume `:db-info` or other env keys are present."
  (:require
   [babashka.process :as process]
   [clojure.java.io :as io]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.environment.agents :as agents]
   [com.blockether.vis.internal.skills :as skills]))

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
;; ::system - lifted from the old internal/main.clj's cli-doctor!
;; ---------------------------------------------------------------------------

(defn- system-check-fn
  "JVM + OS + Clojure + memory + DB-path facts. Defensive about
   `:db-info`: per the activation contract, the env may not have
   one (see plan Q19 / §1)."
  [environment]
  (let [rt        (Runtime/getRuntime)
        used      (- (.totalMemory rt) (.freeMemory rt))
        max-mem   (.maxMemory rt)
        db-path   (or (some-> environment :db-info :path) "(no DB)")]
    [{:level   :info
      :message (str "OS: "       (System/getProperty "os.name") " "
                 (System/getProperty "os.arch") " "
                 (System/getProperty "os.version"))}
     {:level   :info
      :message (str "Java: "     (System/getProperty "java.version")
                 " (" (System/getProperty "java.vendor") ")")}
     {:level   :info
      :message (str "Clojure: "  (clojure-version))}
     {:level   :info
      :message (str "Memory: "   (format-bytes used) " / " (format-bytes max-mem))}
     {:level   :info
      :message (str "DB path: "  db-path)}]))

;; ---------------------------------------------------------------------------
;; ::agents-md - project guidance presence
;; ---------------------------------------------------------------------------

(defn- agents-md-check-fn [_environment]
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
;; ::skills - catalog summary
;; ---------------------------------------------------------------------------

(defn- skills-check-fn [_environment]
  (let [skills-list (skills/list-all)
        by-source   (frequencies (mapv :source skills-list))
        repo-n      (or (:repo by-source) 0)
        user-n      (or (:user-global by-source) 0)
        total       (count skills-list)]
    (if (zero? total)
      [{:level   :info
        :message "No skills installed (neither <repo>/.agents/skills/ nor ~/.agents/skills/ has any SKILL.md files)."}]
      [{:level   :info
        :message (str total " skill" (when (not= 1 total) "s")
                   " loaded (" repo-n " repo, " user-n " user-global)")}])))

;; ---------------------------------------------------------------------------
;; ::voice - optional voice runtime checks
;; ---------------------------------------------------------------------------

(defn- executable? [cmd]
  (try
    (zero? (:exit (process/sh {:out :string :err :string :continue true}
                    "command" "-v" cmd)))
    (catch Throwable _ false)))

(defn- resolved? [sym]
  (boolean (requiring-resolve sym)))

(defn- call-resolved [sym & args]
  (when-let [f (requiring-resolve sym)]
    (apply f args)))

(defn- voice-extension-message []
  (let [asr? (resolved? 'com.blockether.vis.ext.voice.asr/transcribe-file!)
        tts? (resolved? 'com.blockether.vis.ext.voice.core/synthesize-file!)]
    (if (or asr? tts?)
      {:level :info
       :message (str "Voice:"
                  " input=" (if asr? "loaded" "missing")
                  ", output=" (if tts? "loaded" "missing"))}
      {:level       :warn
       :message     "Voice: not loaded; voice menus/commands are disabled."
       :remediation "Add/load vis-voice, then restart the channel."})))

(defn- ffmpeg-message []
  (if (executable? "ffmpeg")
    {:level :info
     :message "ffmpeg: installed"}
    {:level       :warn
     :message     "ffmpeg: missing; Telegram voice input cannot convert .oga/.opus to WAV for ASR."
     :remediation "Install ffmpeg and ensure it is on PATH for the Vis/Telegram process."}))

(defn- piper-espeak-message []
  (try
    (if-let [files (call-resolved 'com.blockether.vis.ext.voice.core/model-files)]
      (let [data-dir (:data files)]
        (if (.isDirectory (io/file data-dir))
          {:level :info
           :message (str "Piper espeak-ng-data: installed - " data-dir)}
          {:level       :warn
           :message     (str "Piper espeak-ng-data: missing - " data-dir)
           :remediation "Run `vis extensions voice models download --piper` or set VIS_PIPER_MODEL_DIR to a complete Piper model directory."}))
      {:level :info
       :message "Piper espeak-ng-data: skipped; voice TTS is not loaded."})
    (catch Throwable t
      {:level       :warn
       :message     (str "Piper espeak-ng-data: check failed: " (or (ex-message t) t))
       :remediation "Run `vis extensions voice models status` for detailed voice model diagnostics."})))

(defn- voice-check-fn [_environment]
  [(voice-extension-message)
   (ffmpeg-message)
   (piper-espeak-message)])

;; ---------------------------------------------------------------------------
;; ::scan-warnings - promotes scanner warnings into doctor :error msgs
;; ---------------------------------------------------------------------------

(defn- scan-warnings-check-fn [_environment]
  (let [warnings (concat (agents/scan-warnings) (skills/scan-warnings))]
    (if (empty? warnings)
      []
      (mapv (fn [{:keys [path reason source]}]
              {:level       :error
               :message     (str path ": " reason)
               :remediation (case source
                              :skill-frontmatter
                              (str "Fix the YAML frontmatter (required fields: name, description), "
                                "then run `(reload-skills!)` from `:code` or `bin/vis extensions doctor` "
                                "to revalidate.")
                              :agents-md
                              (str "Verify the file is readable; then run `(vis/reload-instructions!)` "
                                "or `bin/vis extensions doctor` to revalidate.")
                              :claude-md-fallback
                              (str "Verify the file is readable; or add a proper `AGENTS.md` instead of "
                                "relying on the CLAUDE.md fallback.")
                              "Investigate, fix, then revalidate via `bin/vis extensions doctor`.")
               :data        {:path path :source source}})
        warnings))))

;; ---------------------------------------------------------------------------
;; The single fn the foundation extension wires into
;; `:ext/doctor-check-fn`. Order is intentional: system facts first,
;; then project-guidance presence, then skills summary, then voice
;; readiness, then any scan failures. Each section stamps its own
;; `:check-id` so the formatter still groups the output under the
;; documented prefixes.
;; ---------------------------------------------------------------------------

(defn- stamp [check-id msgs]
  (mapv #(assoc % :check-id check-id) msgs))

(defn check-fn
  "Foundation's `:ext/doctor-check-fn`. Concatenates the four
   logical section streams into a single message seq."
  [environment]
  (vec
    (concat
      (stamp ::system        (system-check-fn        environment))
      (stamp ::agents-md     (agents-md-check-fn     environment))
      (stamp ::skills        (skills-check-fn        environment))
      (stamp ::voice         (voice-check-fn         environment))
      (stamp ::scan-warnings (scan-warnings-check-fn environment)))))

;; ---------------------------------------------------------------------------
;; `vis extensions doctor` CLI command. Foundation owns the diagnostics,
;; but extension-owned CLI must mount under `vis extensions ...` through
;; `:ext/cli`, never by direct global command registration.
;; ---------------------------------------------------------------------------

(defn- println-original!
  "Print to `original-stdout` (the unredirected stream captured
   before `init-cli!`). The CLI dispatcher redirects `*out*` to
   silence stray prints from the iteration loop / Telemere /
   svar; doctor output bypasses that to land on the user's
   terminal."
  [^String s]
  (.println ^java.io.PrintStream vis/original-stdout s)
  (.flush ^java.io.PrintStream vis/original-stdout))

(defn- cli-doctor-run!
  "CLI handler for `vis extensions doctor`. Init host runtime, walk every
   extension's `:ext/doctor-check-fn` via [[vis/run-doctor-checks]],
   format + print to original-stdout, exit with 0 / 1 / 2 by max
   level."
  [_parsed _residual]
  (vis/init-cli!)
  (let [env  {:db-info (vis/resolve-db-spec)}
        msgs (vis/run-doctor-checks env)]
    (println-original! (vis/doctor-format-output msgs))
    (System/exit (int (vis/doctor-exit-code msgs)))))

(defn cli-command []
  {:cmd/name   "doctor"
   :cmd/doc    "Run cross-extension diagnostics. Prints info / warn / error messages contributed by every loaded extension; exits 0 (clean) / 1 (warnings) / 2 (errors)."
   :cmd/usage  "vis extensions doctor"
   :cmd/run-fn cli-doctor-run!})
