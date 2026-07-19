(ns measure-trailer
  "Trailer render measurement harness — REAL data, no API spend.

   Replays every persisted form envelope from the session DB pointed at
   by VIS_DB_PATH (use a COPY of the real ~/.vis DB; never the live
   one) through the trailer pin renderer and token-counts the output,
   so render optimizations are judged on the actual distribution of
   results (file reads, tool maps, errors) instead of synthetic pins.

   Two renderers run side by side over the same corpus:
     :legacy  — frozen replica of the pre-optimization render
                (<results scope> tag + the whole projected pin as one
                Python dict) so the diff survives the optimization
                landing in ctx-renderer.
     :current — whatever `ctx-renderer/render-trailer-pin` does NOW.

   Usage:
     cp ~/.vis/vis.db* /tmp/vis-measure/
     VIS_DB_PATH=/tmp/vis-measure clojure -M:dev -e \\
       \"(require 'measure-trailer) (measure-trailer/report!)\"

   Pins are reconstructed the way `advance-iter` would build them TODAY
   (model-form-envelope projection + the done()/silent filter), so the
   numbers reflect what would actually ride prompts now — not the fat
   historical envelopes."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-renderer :as r]
   [com.blockether.vis.internal.env-python :as env]
   [com.blockether.vis.internal.tokens :as tokens]))

(def ^:private project-trailer-pin
  @#'com.blockether.vis.internal.ctx-renderer/project-trailer-pin)

;; ---------------------------------------------------------------------------
;; Pin reconstruction — mirror advance-iter's keepable filter + projection
;; ---------------------------------------------------------------------------

(defn- keepable-form? [f]
  (not (or (str/starts-with? (str/triml (str (:src f))) "done(")
         (= "vis_silent" (:result f))
         (:vis/silent f))))

(defn- iteration->pin [turn-pos iter]
  (let [forms (->> (or (:forms iter) [])
                (filter keepable-form?)
                (mapv eng/model-form-envelope))]
    (when (seq forms)
      {:scope (str "t" turn-pos "/i" (:position iter))
       :forms forms})))

(defn- session-pins [db-info session-id]
  (vec
    (for [turn (try (vis/db-list-session-turns db-info session-id)
                 (catch Throwable _ []))
          iter (try (vis/db-list-session-turn-iterations db-info (:id turn))
                 (catch Throwable _ []))
          :let [pin (iteration->pin (:position turn) iter)]
          :when pin]
      pin)))

;; ---------------------------------------------------------------------------
;; Renderers under measurement
;; ---------------------------------------------------------------------------

(defn- legacy-render
  "Frozen replica of the PRE-optimization `render-trailer-pin`: scope in
   the tag AND the whole projected pin (scope + forms with per-form
   scopes) as one Python dict."
  [pin]
  (let [scope (or (:scope pin)
                (when (:scope-start pin)
                  (str (:scope-start pin) ".." (:scope-end pin))))]
    (str "<results" (when scope (str " scope=\"" scope "\"")) ">\n"
      (env/ctx->python-str (project-trailer-pin pin))
      "\n</results>")))

(def ^:private renderers
  {:legacy  legacy-render
   :current r/render-trailer-pin})

;; ---------------------------------------------------------------------------
;; Stats
;; ---------------------------------------------------------------------------

(defn- form-kind [f]
  (cond
    (:error f)                    :error
    (string? (:result f))         :string
    (map? (:result f))            :map
    (sequential? (:result f))     :list
    (contains? f :result)         :scalar
    :else                         :no-output))

(defn- pin-kind
  "Dominant kind for breakdown: a pin with ANY string result counts as
   :string (that's where the escaping tax lives)."
  [pin]
  (let [kinds (set (map form-kind (:forms pin)))]
    (cond
      (kinds :string) :string
      (kinds :map)    :map
      (kinds :error)  :error
      (kinds :list)   :list
      :else           :other)))

(defn- percentile [sorted-v p]
  (when (seq sorted-v)
    (nth sorted-v (min (dec (count sorted-v))
                    (long (Math/floor (* p (count sorted-v))))))))

(defn- summarize-tokens [label tokens-v]
  (let [sorted (vec (sort tokens-v))
        total  (reduce + 0 tokens-v)]
    {:renderer label
     :pins     (count tokens-v)
     :total    total
     :mean     (when (seq tokens-v) (long (/ total (count tokens-v))))
     :p50      (percentile sorted 0.5)
     :p90      (percentile sorted 0.9)
     :max      (last sorted)}))

;; ---------------------------------------------------------------------------
;; Report
;; ---------------------------------------------------------------------------

(defn report!
  "Measure every renderer over every pin of the most recent `n-sessions`
   (default 40) across all channels. Prints aggregate + per-kind tables
   and the head-to-head saving."
  ([] (report! 40))
  ([n-sessions]
   (let [db-info  (vis/db-info)
         sessions (->> [:tui :cli :api]
                    (mapcat #(try (vis/db-list-sessions db-info %)
                               (catch Throwable _ [])))
                    (sort-by :created-at)
                    reverse
                    (take n-sessions))
         pins     (vec (mapcat #(session-pins db-info (:id %)) sessions))
         _        (println (format "corpus: %d sessions, %d pins, %d forms"
                             (count sessions) (count pins)
                             (reduce + 0 (map (comp count :forms) pins))))
         measured (into {}
                    (map (fn [[label render]]
                           [label (mapv (fn [pin]
                                          {:pin pin
                                           :kind (pin-kind pin)
                                           :tokens (tokens/count-tokens (render pin))})
                                    pins)]))
                    renderers)]
     (doseq [[label rows] measured]
       (println (pr-str (summarize-tokens label (mapv :tokens rows))))
       (doseq [[kind krows] (sort-by key (group-by :kind rows))]
         (println (format "  %-8s pins=%4d total=%7d mean=%5d"
                    (name kind) (count krows)
                    (reduce + 0 (map :tokens krows))
                    (long (/ (reduce + 0 (map :tokens krows))
                            (max 1 (count krows))))))))
     (let [t-legacy  (reduce + 0 (map :tokens (:legacy measured)))
           t-current (reduce + 0 (map :tokens (:current measured)))]
       (println (format "TOTAL legacy=%d current=%d saving=%d (%.1f%%)"
                  t-legacy t-current (- t-legacy t-current)
                  (if (pos? t-legacy)
                    (* 100.0 (/ (- t-legacy t-current) (double t-legacy)))
                    0.0)))
       {:legacy t-legacy :current t-current}))))
