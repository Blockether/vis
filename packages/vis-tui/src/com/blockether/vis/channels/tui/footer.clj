(ns com.blockether.vis.channels.tui.footer
  "Dedicated one-row status footer rendered below the input box.

   Codex-style three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       glm-5.1 (balanced)        \u280B iter 7 \u00b7 4.1s              97% left \u00b7 $0.04

   Each region holds a list of `{:text :fg :bold? :region :priority}`
   spans separated by ' \u00b7 ' in muted color. The full segment list is
   built up-front and shrunk by dropping the highest `:priority`
   number (least important) until it fits the available width.

   States (driven by `:loading?`, `:cancelling?` from app-db):
     idle       \u2192 LEFT=model    CENTER=\u2205                    RIGHT=ctx-left% \u00b7 cost
     running    \u2192 LEFT=model    CENTER=spinner+iter+elapsed   RIGHT=ctx-left%
     cancelling \u2192 LEFT=model    CENTER=cancelling\u2026             RIGHT=ctx-left%

   Every numeric format uses `Locale/ROOT` so a Polish JVM doesn't
   produce mixed `5,8k` next to English `k`. The previous footer
   embedded the status into the input box's bottom border via
   `embed-in-bar`, which forced single-color rendering and was the
   reason run-state had to live inside the assistant bubble; this
   namespace replaces that whole path."
  (:require [com.blockether.svar.internal.router :as router]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.channels.tui.primitives :as p]
            [com.blockether.vis.channels.tui.render :as render]
            [com.blockether.vis.channels.tui.theme :as t]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query-core])
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
  "Mirrors `query/core.clj`'s `balanced-reasoning` constant. The TUI
   doesn't expose a per-conversation override yet."
  :balanced)

(defn- chosen-model-info
  "Resolved model map for the configured root model, or nil."
  []
  (when-let [r (try (query-core/get-router) (catch Throwable _ nil))]
    (try (query-core/resolve-effective-model r) (catch Throwable _ nil))))

(defn- last-assistant-tokens
  "Token map `{:input n :output n}` of the most recent finalized assistant
   message, or nil. Used to estimate next-query context fill from a real
   measurement instead of the chars/4 heuristic."
  [messages]
  (some->> (reverse messages)
    (some (fn [m] (when (and (= :assistant (:role m)) (:tokens m)) (:tokens m))))))

(defn- estimate-next-context-chars
  "Crude pre-tokenizer estimate: total chars across all chat messages so
   far. Off by 30-50% on first turn vs real tokenization \u2014 we mark the
   resulting % with a leading `~` so the user knows it's an estimate."
  ^long [messages]
  (long (reduce + 0
          (keep (fn [m] (let [tx (:text m)] (when (string? tx) (count tx))))
            messages))))

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

(defn- ctx-left-info
  "{:pct long, :estimated? bool} or nil. `pct` is **% left**, not used:
   97 means '97% of context still free', the universal fuel-gauge framing
   Codex/Claude Code converged on. `:estimated?` true when the value
   came from chars/4, false when from a real prior-turn :input count."
  [messages model-name]
  (when-let [ctx-max (and model-name
                       (try (router/context-limit model-name) (catch Throwable _ nil)))]
    (when (pos? ctx-max)
      (let [last-tok (last-assistant-tokens messages)
            actual?  (some? (:input last-tok))
            used     (or (:input last-tok)
                       (let [chars (estimate-next-context-chars messages)]
                         (when (pos? chars) (long (/ chars 4)))))]
        (when used
          (let [used    (min ctx-max used)
                left    (- ctx-max used)
                pct     (long (Math/round (* 100.0 (/ (double left) (double ctx-max)))))]
            {:pct (max 0 (min 100 pct))
             :estimated? (not actual?)}))))))

(defn- ctx-color
  "Color for the ctx-left segment. Less left \u2192 hotter color."
  [pct]
  (cond
    (<= pct 10) t/footer-error-fg
    (<= pct 30) t/footer-warning-fg
    :else       t/footer-fg-muted))

;;; ── Segment list ───────────────────────────────────────────────────────────

(defn- build-segments
  "Vector of `{:text :fg :bold? :region :priority}`.

   `:priority` semantics: 1 = critical (never drop), higher = drop first
   when the row overflows. The full priority hierarchy:
     1  ctx %, run-state spinner, cancelling\u2026
     2  model name, elapsed (running)
     3  iter counter, model reasoning suffix
     4  cost"
  [db now-ms]
  (let [{:keys [messages loading? cancelling? progress query-start-ms]} db
        info       (chosen-model-info)
        model      (:name info)
        reasoning? (boolean (:reasoning? info))
        ctx        (ctx-left-info messages model)
        cost-str   (format-cost (session-cost messages))
        iter-n     (count (or (:iterations progress) []))]
    (cond-> []
      ;; ── LEFT ──────────────────────────────────────────────────────────────
      model
      (conj {:text model
             :fg t/footer-fg-strong :bold? true
             :region :left :priority 2})

      reasoning?
      (conj {:text (str "(" (name default-reasoning-level) ")")
             :fg t/footer-fg-muted :bold? false
             :region :left :priority 3})

      ;; ── CENTER ────────────────────────────────────────────────────────────
      cancelling?
      (conj {:text "cancelling\u2026"
             :fg t/footer-warning-fg :bold? true
             :region :center :priority 1})

      (and loading? (not cancelling?))
      (conj {:text (render/spinner-frame now-ms)
             :fg t/footer-spinner-fg :bold? true
             :region :center :priority 1})

      (and loading? (not cancelling?) (pos? iter-n))
      (conj {:text (str "iter " iter-n)
             :fg t/footer-fg :bold? false
             :region :center :priority 3})

      (and loading? query-start-ms)
      (conj {:text (or (channels/format-duration
                         (max 0 (- now-ms (long query-start-ms))))
                     "0ms")
             :fg t/footer-fg-muted :bold? false
             :region :center :priority 2})

      ;; ── RIGHT ─────────────────────────────────────────────────────────────
      ctx
      (conj {:text (str (when (:estimated? ctx) "~") (:pct ctx) "% left")
             :fg (ctx-color (:pct ctx))
             :bold? (<= (:pct ctx) 30)
             :region :right :priority 1})

      cost-str
      (conj {:text cost-str
             :fg t/footer-fg-muted :bold? false
             :region :right :priority 4}))))

;;; ── Width fitting ──────────────────────────────────────────────────────────

(def ^:private sep "  \u00b7  ")
(def ^:private sep-narrow " \u00b7 ")

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
   before sacrificing segments \u2014 looks the same on a wide terminal,
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
  "Paint the footer row at `footer-row`, full width `cols`. Pure draw \u2014
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
        edge-pad 1
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
