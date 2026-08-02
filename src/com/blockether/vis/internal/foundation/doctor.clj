(ns com.blockether.vis.internal.foundation.doctor
  "Foundation's contribution to the host `vis-agent doctor` aggregator. ONE fn
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
                          leniently by design, so `vis-agent doctor` is the moment a
                          user actually LOOKS — the right place to fail fast
                           without failing the gateway.

   ::image-render      one real SVG rasterized through the attachment path, so
                        a build whose imaging cdylib is missing or unloadable
                        SAYS SO instead of silently dropping every diagram.

   These section fns are pure data -> message-seq; they don't mutate
   anything and don't depend on the runtime environment beyond
   what's needed to read the existing scanners. Activation
   contract per plan: every registered extension's `:ext/doctor-fn`
   runs regardless of `:ext/activation-fn`, so the section fns must
   NOT assume `:db-info` or other env keys are present."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.foundation.environment.agents :as agents]
            [com.blockether.vis.internal.foundation.housekeeping :as housekeeping]
            [com.blockether.vis.internal.image-convert :as image-convert])
  (:import (java.nio.charset StandardCharsets)))

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
   `vis-agent doctor` must not start exiting 1 on a machine that is working fine."
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
        "Review with `vis-agent doctor --purge --dry-run`, then reclaim with `vis-agent doctor --purge` (add `--days N` to change the 14-day cutoff)."
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

;; ---------------------------------------------------------------------------
;; ::image-render - the imaging rasterizer actually works in THIS binary
;; ---------------------------------------------------------------------------

(def ^:private probe-svg
  "Smallest document that still exercises the whole rasterizer: a shape (the
   vector pipeline), a fill (raster + colour handling) and a glyph (the embedded
   font faces, the part a native-image binary has to be taught about). Text is
   deliberate -- a shape-only probe passes on a binary where every real chart
   still dies."
  (str
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"32\" height=\"16\">"
    "<rect width=\"32\" height=\"16\" fill=\"#3366cc\"/>"
    "<text x=\"2\" y=\"12\" font-size=\"10\" font-family=\"sans-serif\" fill=\"white\">vis</text>"
    "</svg>"))

(defn- image-render-diagnostics
  "Attachments degrade SILENTLY: `image-convert` swallows every failure and
   returns nil, so an SVG the build cannot rasterize is simply DROPPED and the
   model never sees the figure at all. A binary whose imaging native library
   failed to load therefore looks healthy while quietly dropping every
   diagram a user attaches -- exactly what `vis-agent doctor` exists to surface."
  [_environment]
  (let
    [started
     (System/nanoTime)

     out
     (try (binding [image-convert/*enabled?* true]
            (image-convert/rasterize-svg (.getBytes ^String probe-svg StandardCharsets/UTF_8) nil))
          (catch Throwable _ nil))

     ms
     (quot (- (System/nanoTime) started) 1000000)]

    (if out
      [{:level :info
        :message (str "Image rendering available (SVG probe rasterized to "
                      (:width out)
                      "x"
                      (:height out)
                      " "
                      (:media-type out)
                      " in "
                      ms
                      " ms).")
        :data {:width (:width out) :height (:height out) :media-type (:media-type out) :ms ms}}]
      [{:level :warn
        :message
        "Image rendering unavailable: this build cannot rasterize an SVG, so attached .svg files are dropped."
        :remediation
        "Report this build (`vis-agent --version`) — a native binary needs the bundled `imaging` native library (libimaging_c) for its platform."}])))

(defn- stamp [check-id msgs] (mapv #(assoc % :check-id check-id) msgs))

(defn doctor-fn
  "Foundation's `:ext/doctor-fn`. Concatenates the foundation
   diagnostic streams into a single message seq."
  [environment]
  (vec (concat (stamp ::agents-md (agents-md-diagnostics environment))
               (stamp ::provider-env (provider-env-diagnostics environment))
               (stamp ::housekeeping (housekeeping-diagnostics environment))
               (stamp ::image-render (image-render-diagnostics environment)))))
