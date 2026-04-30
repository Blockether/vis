(ns com.blockether.vis.ext.channel-tui.footer
  "Dedicated one-row status footer rendered below the input box.

   Codex-style three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       glm-5.1 (balanced)                                   $0.04

   Each region holds a list of `{:text :fg :bold? :region :priority}`
   spans separated by ' · ' in muted color. The full segment list is
   built up-front and shrunk by dropping the highest `:priority`
   number (least important) until it fits the available width.

   States (driven by `:loading?`, `:cancelling?` from app-db):
     idle       → LEFT=model    CENTER=∅           RIGHT=cost
     running    → LEFT=model    CENTER=∅           RIGHT=cost
     cancelling → LEFT=model    CENTER=cancelling…  RIGHT=cost

   Run-state (spinner, iteration counter, elapsed time, current
   phase) lives EXCLUSIVELY in the assistant bubble's `progress->text`
   block. Putting it in the footer too was a duplicate — same
   `\u280b 11.2s` showing twice on screen. The footer keeps slow-changing
   identity + budget bits; the bubble keeps the live activity story
   («Vis is thinking (iter 3)… 4.1s · Esc to cancel»).

   Every numeric format uses `Locale/ROOT` so a Polish JVM doesn't
   produce mixed `5,8k` next to English `k`. The previous footer
   embedded the status into the input box's bottom border via
   `embed-in-bar`, which forced single-color rendering and was the
   reason run-state had to live inside the assistant bubble; this
   namespace replaces that whole path."
  (:require [com.blockether.vis.core :as lp]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t])
  (:import [java.util Locale]))

;;; ── Number formatting (locale-safe) ────────────────────────────────────────

(defn- fmt-double [pattern v]
  (String/format Locale/ROOT ^String pattern (into-array Object [(double v)])))

(defn- format-cost
  "$0.0042 / $0.034 / $1.20 / nil. Always Locale/ROOT (no decimal comma)."
  [usd]
  (when (and usd (pos? usd))
    (cond
      (< usd 0.01) (fmt-double "$%.4f" usd)
      (< usd 1.0)  (fmt-double "$%.3f" usd)
      :else        (fmt-double "$%.2f" usd))))

;;; ── Data extraction from app-db ────────────────────────────────────────────

(def ^:private default-reasoning-level
  :balanced)

(defn- chosen-model-info
  "Resolved model map for the configured root model, or nil."
  []
  (when-let [r (try (lp/get-router) (catch Throwable _ nil))]
    (try (lp/resolve-effective-model r) (catch Throwable _ nil))))

(defn- session-cost
  "Cumulative session cost in USD across all assistant turns, or nil
   when no turns carried `:cost`. Vis already tracks `:total-cost` per
   turn (used by the meta-line under each bubble); this just sums."
  [messages]
  (let [total (reduce (fn [acc m]
                        (if-let [c (some-> m :cost :total-cost)]
                          (+ acc (double c))
                          acc))
                0.0 messages)]
    (when (pos? total) total)))

;;; ── Segment list ───────────────────────────────────────────────────────────

(defn- build-segments
  "Vector of `{:text :fg :bold? :region :priority}`.

   `:priority` semantics: 1 = critical (never drop), higher = drop first
   when the row overflows. The full priority hierarchy:
     1  run-state spinner, cancelling…
     2  model name, elapsed (running)
     3  iter counter, model reasoning suffix
     4  cost"
  [db _now-ms]
  (let [{:keys [messages cancelling? settings]} db
        info       (chosen-model-info)
        model      (:name info)
        provider   (:provider info)
        ;; Show the provider/model lineage in the footer so the user
        ;; never has to guess which backend is on the wire. Falls
        ;; back to plain model name when the resolver did not
        ;; surface a provider id (e.g. legacy router shape).
        model-display (if (and provider model)
                        (str (name provider) "/" model)
                        model)
        reasoning? (boolean (:reasoning? info))
        reasoning-level (or (:reasoning-level settings) default-reasoning-level)
        cost-str   (format-cost (session-cost messages))]
    (cond-> []
      ;; ── LEFT ──────────────────────────────────────────────────────────────
      model-display
      (conj {:text model-display
             :fg t/footer-fg-strong :bold? true
             :region :left :priority 2})

      reasoning?
      (conj {:text (str "(" (name reasoning-level) ")")
             :fg t/footer-fg-muted :bold? false
             :region :left :priority 3})

      ;; ── CENTER ────────────────────────────────────────────────────────────
      cancelling?
      (conj {:text "cancelling…"
             :fg t/footer-warning-fg :bold? true
             :region :center :priority 1})

      ;; Spinner / iter-counter / elapsed: deliberately NOT here.
      ;; The bubble's `progress->text` already carries that, with
      ;; phase ("Vis is calling the provider" / "Vis is thinking
      ;; (iter 3)"). Duplicating it in the footer is what the user
      ;; complained about — same `⠋ 11.2s` shown twice. `cancelling…`
      ;; stays because it's a whole-conversation status, not just
      ;; current-iteration.

      ;; ── RIGHT ─────────────────────────────────────────────────────────────
      cost-str
      (conj {:text cost-str
             :fg t/footer-fg-muted :bold? false
             :region :right :priority 4}))))

;;; ── Width fitting ──────────────────────────────────────────────────────────

(def ^:private sep "  ·  ")
(def ^:private sep-narrow " · ")

(defn- region-spans [segments region]
  (filterv #(= region (:region %)) segments))

(defn- spans-width [spans separator]
  (if (empty? spans)
    0
    (+ (reduce + 0 (map #(count (:text %)) spans))
      (* (dec (count spans)) (count separator)))))

(defn- total-width
  "Width of all three regions plus mandatory inter-region gaps and edge
   padding. Used by `shrink-to-fit` to decide whether the current
   segment list fits `cols`."
  [segments separator]
  (let [l (region-spans segments :left)
        c (region-spans segments :center)
        r (region-spans segments :right)
        edge-pad 2 ;; one space on each end of the row
        gap 2      ;; minimum gap between adjacent regions
        n-gaps (cond-> 0
                 (and (seq l) (or (seq c) (seq r))) inc
                 (and (seq c) (seq r))             inc)]
    (+ edge-pad
      (* gap n-gaps)
      (spans-width l separator)
      (spans-width c separator)
      (spans-width r separator))))

(defn- shrink-to-fit
  "Drop highest-:priority segments one at a time until the row fits.
   Tries the wide separator first, then collapses to a narrow one
   before sacrificing segments — looks the same on a wide terminal,
   reads the same on a 80-col one."
  [segments cols]
  (let [fit? (fn [segs sepa] (<= (total-width segs sepa) cols))]
    (cond
      (fit? segments sep)        [segments sep]
      (fit? segments sep-narrow) [segments sep-narrow]
      :else
      (loop [segs segments]
        (cond
          (empty? segs)
          [segs sep-narrow]

          (fit? segs sep-narrow)
          [segs sep-narrow]

          :else
          (let [worst-priority (apply max (map :priority segs))
                ;; Drop one occurrence with the worst priority.
                victim (some #(when (= worst-priority (:priority %)) %) segs)]
            (recur (vec (remove #(identical? victim %) segs)))))))))

;;; ── Drawing ────────────────────────────────────────────────────────────────

(defn- draw-spans!
  "Draw spans left-to-right starting at `col`. Each span uses its own
   fg + optional bold; separators are rendered in muted fg. Returns
   final col after the last span."
  [g start-col row spans separator]
  (reduce
    (fn [c [i s]]
      (let [c (if (zero? i)
                c
                (do (p/clear-styles! g)
                  (p/set-colors! g t/footer-fg-muted t/terminal-bg)
                  (p/put-str! g c row separator)
                  (+ c (count separator))))]
        (p/clear-styles! g)
        (p/set-colors! g (or (:fg s) t/footer-fg) t/terminal-bg)
        (when (:bold? s) (p/enable! g p/BOLD))
        (p/put-str! g c row (:text s))
        (p/clear-styles! g)
        (+ c (count (:text s)))))
    start-col
    (map-indexed vector spans)))

(defn draw-footer!
  "Paint the footer row at `footer-row`, full width `cols`. Pure draw —
   reads `db` once, computes segments, fits to width, writes cells.
   Safe to call every frame (cheap; no allocations on the hot path
   beyond the spans vector)."
  [g db footer-row cols now-ms]
  ;; Background fill: default footer fg on terminal bg, full row.
  (p/clear-styles! g)
  (p/set-colors! g t/footer-fg t/terminal-bg)
  (p/fill-rect! g 0 footer-row cols 1)

  (let [[segs separator] (shrink-to-fit (build-segments db now-ms) cols)
        l        (region-spans segs :left)
        c        (region-spans segs :center)
        r        (region-spans segs :right)
        edge-pad 2
        l-w      (spans-width l separator)
        c-w      (spans-width c separator)
        r-w      (spans-width r separator)
        l-col    edge-pad
        r-col    (max (+ l-col l-w 2) (- cols edge-pad r-w))
        ;; Center between L's right edge and R's left edge.
        l-end    (+ l-col l-w)
        c-col    (max (+ l-end (if (seq l) 2 0))
                   (- (quot (+ l-end r-col) 2) (quot c-w 2)))]
    (when (seq l) (draw-spans! g l-col   footer-row l separator))
    (when (seq c) (draw-spans! g c-col   footer-row c separator))
    (when (seq r) (draw-spans! g r-col   footer-row r separator)))

  ;; Restore neutral state for whatever paints next.
  (p/clear-styles! g)
  (p/set-colors! g t/text-fg t/terminal-bg))
