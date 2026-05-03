(ns com.blockether.vis.ext.channel-tui.footer
  "Dedicated one-row status footer rendered below the input box.

   Codex-style three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       glm-5.1 (balanced)              total ↑12000 (cached 8000) ↓800  $0.04

   Each region holds a list of `{:text :fg :bold? :region :priority}`
   spans separated by ' · ' in muted color. The full segment list is
   built up-front and shrunk by dropping the highest `:priority`
   number (least important) until it fits the available width.

   States (driven by `:loading?`, `:cancelling?` from app-db):
     idle       → LEFT=model    CENTER=∅           RIGHT=tokens/cost
     running    → LEFT=model    CENTER=∅           RIGHT=tokens/cost
     cancelling → LEFT=model    CENTER=cancelling…  RIGHT=tokens/cost

   Run-state (spinner, iteration counter, elapsed time, current
   phase) lives EXCLUSIVELY in the assistant bubble's `progress->text`
   block. Putting it in the footer too was a duplicate — same
   `\u280b 11.2s` showing twice on screen. The footer keeps slow-changing
   identity + budget bits; the bubble keeps the live activity story
   («Vis is thinking (iter 3)… 4.1s · Esc to cancel»).

   The first footer row carries repository context on the right:
   repo/branch, modified/created/deleted counts, and ahead/behind counts
   when an upstream is configured. The second footer row carries provider
   budgets and cumulative usage under that git context. Git status is
   cached briefly so repainting the TUI does not run JGit on every frame.

   Every numeric format uses `Locale/ROOT` so a Polish JVM doesn't
   produce mixed `5,8k` next to English `k`. The previous footer
   embedded the status into the input box's bottom border via
   `embed-in-bar`, which forced single-color rendering and was the
   reason run-state had to live inside the assistant bubble; this
   namespace replaces that whole path."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as lp]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.git :as git])
  (:import [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util Locale]))

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

(def ^:private default-codex-verbosity
  :low)

(defn- chosen-model-info
  "Resolved model map for the configured root model, or nil."
  []
  (when-let [r (try (lp/get-router) (catch Throwable _ nil))]
    (try (lp/resolve-effective-model r) (catch Throwable _ nil))))

(def ^:private git-glyph "")

(defn- git-repo-label
  [{:keys [repo branch]}]
  (str "~/" (or repo "?") " (" (or branch "?") ")"))

(defn- git-dirty-label
  [{:keys [modified created deleted]}]
  (let [modified (long (or modified 0))
        created  (long (or created 0))
        deleted  (long (or deleted 0))]
    (if (zero? (+ modified created deleted))
      "files: clean"
      (String/format Locale/ROOT "files: %d modified, %d created, %d deleted"
        (into-array Object [modified created deleted])))))

(defn- git-sync-label
  [{:keys [upstream? ahead behind]}]
  (let [ahead  (long (or ahead 0))
        behind (long (or behind 0))]
    (cond
      (not upstream?) "commits: no upstream"
      (zero? (+ ahead behind)) "commits: up to date"
      :else (str "commits:"
              (when (pos? ahead) (str " ⇡" ahead))
              (when (pos? behind) (str " ⇣" behind))))))

(defn- git-footer-spans
  [{:keys [workspace?] :as status}]
  (if workspace?
    [{:text (str git-glyph " " (git-repo-label status))
      :fg t/footer-fg-strong :bold? true
      :region :right :priority 2}
     {:text (git-dirty-label status)
      :fg t/footer-fg-muted :bold? false
      :region :right :priority 3}
     {:text (git-sync-label status)
      :fg t/footer-fg-muted :bold? false
      :region :right :priority 4}]
    [{:text (str "No " git-glyph)
      :fg t/footer-error-fg :bold? true
      :region :right :priority 2}]))

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

(defn- first-token-number
  [tokens ks]
  (some (fn [k]
          (let [v (get tokens k)]
            (when (number? v) v)))
    ks))

(defn- add-token-slot
  [acc tokens out-k aliases]
  (if-let [v (first-token-number tokens aliases)]
    (update acc out-k (fnil + 0) (long v))
    acc))

(defn- add-message-tokens
  [acc {:keys [tokens]}]
  (if (map? tokens)
    (-> acc
      (add-token-slot tokens :input [:input])
      (add-token-slot tokens :output [:output])
      (add-token-slot tokens :cached-input [:cached-input :input-cached :cached]))
    acc))

(defn- session-tokens
  "Cumulative session token usage across assistant turns. Returns nil
   when no message carried usage. The legacy `:cached` field is cached
   input."
  [messages]
  (let [totals (reduce add-message-tokens {} messages)]
    (when (seq totals)
      (merge {:input 0 :output 0 :cached-input 0}
        totals))))

(def ^:private one-week-ms
  (* 7 24 60 60 1000))

(def ^:private short-reset-formatter
  (DateTimeFormatter/ofPattern "EEE h:mm a" Locale/ROOT))

(def ^:private long-reset-formatter
  (DateTimeFormatter/ofPattern "MMM d h:mm a" Locale/ROOT))

(defn- format-relative-reset
  [now-ms reset-ms]
  (when reset-ms
    (let [total-seconds (max 0 (quot (- (long reset-ms) (long now-ms)) 1000))
          days          (quot total-seconds 86400)
          hours         (quot (mod total-seconds 86400) 3600)
          minutes       (quot (mod total-seconds 3600) 60)]
      (cond
        (pos? days)    (str days "d" hours "h")
        (pos? hours)   (str hours "h" minutes "m")
        (pos? minutes) (str minutes "m")
        :else          (str total-seconds "s")))))

(defn- format-absolute-reset
  [now-ms reset-ms]
  (when reset-ms
    (let [zoned     (.atZone (Instant/ofEpochMilli (long reset-ms)) (ZoneId/systemDefault))
          formatter (if (and (>= (- (long reset-ms) (long now-ms)) 0)
                          (< (- (long reset-ms) (long now-ms)) one-week-ms))
                      short-reset-formatter
                      long-reset-formatter)]
      (.format formatter zoned))))

(defn- format-reset
  [now-ms reset-ms]
  (let [relative (format-relative-reset now-ms reset-ms)
        absolute (format-absolute-reset now-ms reset-ms)]
    (cond
      (and relative absolute) (str "↺" relative " @ " absolute)
      relative               (str "↺" relative)
      absolute               (str "↺" absolute)
      :else                  "↺--")))

(defn- format-percent-left
  [remaining]
  (if (number? remaining)
    (str (long (Math/round (double remaining))) "% left")
    "-- left"))

(defn- codex-limit-label
  [row first?]
  (case (:id row)
    :codex-5h (if first? "Codex 5h" "5h")
    :codex-7d (if first? "Codex 7d" "7d")
    (or (:label row) "Codex")))

(defn- format-codex-limit-row
  [now-ms first? row]
  (str (codex-limit-label row first?) " "
    (format-percent-left (:remaining row)) " "
    (format-reset now-ms (get-in row [:window :resets-at-ms]))))

(defn- codex-limits-footer-text
  [db now-ms]
  (let [report (get-in db [:provider-limits :report])
        rows   (->> (get-in report [:dynamic :limits])
                 (filter #(contains? #{:codex-5h :codex-7d} (:id %)))
                 (sort-by (fn [row]
                            (case (:id row)
                              :codex-5h 0
                              :codex-7d 1
                              2))))]
    (when (seq rows)
      (str/join " "
        (map-indexed (fn [idx row]
                       (format-codex-limit-row now-ms (zero? idx) row))
          rows)))))

;;; ── Segment list ───────────────────────────────────────────────────────────

(defn- build-segments
  "Vector of `{:text :fg :bold? :region :priority}`.

   `:priority` semantics: 1 = critical (never drop), higher = drop first
   when the row overflows. The full priority hierarchy:
     1  run-state spinner, cancelling…
     2  model name, provider dynamic limits
     3  iter counter, model reasoning suffix
     4  cost
     5  keyboard shortcut hints"
  [db _now-ms]
  (let [{:keys [cancelling? settings]} db
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
        codex-provider? (= :openai-codex provider)
        codex-verbosity (or (:openai-codex-verbosity settings) default-codex-verbosity)
        git-spans  (git-footer-spans (git/cached-workspace-status))]
    (cond-> (vec git-spans)
      ;; ── LEFT ──────────────────────────────────────────────────────────────
      model-display
      (conj {:text model-display
             :fg t/footer-fg-strong :bold? true
             :region :left :priority 2})

      model-display
      (conj {:text "(Ctrl+T)"
             :join-left? true
             :fg t/footer-fg-muted :bold? false
             :region :left :priority 5})

      reasoning?
      (conj {:text (str "reasoning: " (name reasoning-level))
             :fg t/footer-fg-muted :bold? false
             :region :left :priority 3})

      reasoning?
      (conj {:text "(Ctrl+R)"
             :join-left? true
             :fg t/footer-fg-muted :bold? false
             :region :left :priority 5})

      codex-provider?
      (conj {:text (str "verbosity: " (name codex-verbosity))
             :fg t/footer-fg-muted :bold? false
             :region :left :priority 3})

      codex-provider?
      (conj {:text "(Ctrl+L)"
             :join-left? true
             :fg t/footer-fg-muted :bold? false
             :region :left :priority 5})

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
      ;; Git lives here. Provider usage moved to the second row so it sits
      ;; directly under the repository state instead of competing with it.
      )))

(defn- build-usage-segments
  [{:keys [messages]}]
  (let [tokens-str (some-> (session-tokens messages) fmt/format-tokens)
        cost-str   (format-cost (session-cost messages))]
    (cond-> []
      tokens-str
      (conj {:text (str "total " tokens-str)
             :fg t/footer-fg-muted :bold? false
             :region :right :priority 2})

      cost-str
      (conj {:text cost-str
             :fg t/footer-fg-muted :bold? false
             :region :right :priority 3}))))

(defn- build-limits-segments
  [db now-ms]
  (let [provider (some-> (chosen-model-info) :provider)
        text     (when (= :openai-codex provider)
                   (codex-limits-footer-text db now-ms))]
    (into (cond-> []
            text
            (conj {:text text
                   :fg t/footer-fg-muted :bold? false
                   :region :left :priority 1}))
      (build-usage-segments db))))

;;; ── Width fitting ──────────────────────────────────────────────────────────

(def ^:private sep "  ·  ")
(def ^:private sep-narrow " · ")

(defn- region-spans [segments region]
  (filterv #(= region (:region %)) segments))

(defn- separator-before
  [span separator]
  (if (:join-left? span) " " separator))

(defn- spans-width [spans separator]
  (reduce
    (fn [w [i span]]
      (+ w
        (if (zero? i) 0 (count (separator-before span separator)))
        (count (:text span))))
    0
    (map-indexed vector spans)))

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
                (do
                  (p/clear-styles! g)
                  (p/set-colors! g t/footer-fg-muted t/terminal-bg)
                  (let [separator (separator-before s separator)]
                    (p/put-str! g c row separator)
                    (+ c (count separator)))))]
        (p/clear-styles! g)
        (p/set-colors! g (or (:fg s) t/footer-fg) t/terminal-bg)
        (when (:bold? s) (p/enable! g p/BOLD))
        (p/put-str! g c row (:text s))
        (p/clear-styles! g)
        (+ c (count (:text s)))))
    start-col
    (map-indexed vector spans)))

(defn- draw-footer-row!
  [g db row cols now-ms build-fn]
  (let [[segs separator] (shrink-to-fit (build-fn db now-ms) cols)
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
    (when (seq l) (draw-spans! g l-col row l separator))
    (when (seq c) (draw-spans! g c-col row c separator))
    (when (seq r) (draw-spans! g r-col row r separator))))

(defn draw-footer!
  "Paint the two footer rows starting at `footer-row`, full width `cols`. Pure draw —
   reads `db` once, computes segments, fits to width, writes cells.
   Safe to call every frame (cheap; no allocations on the hot path
   beyond the spans vector)."
  [g db footer-row cols now-ms]
  ;; Background fill: default footer fg on terminal bg, full row.
  (p/clear-styles! g)
  (p/set-colors! g t/footer-fg t/terminal-bg)
  (p/fill-rect! g 0 footer-row cols 2)
  (draw-footer-row! g db footer-row cols now-ms build-segments)
  (draw-footer-row! g db (inc footer-row) cols now-ms build-limits-segments)

  ;; Restore neutral state for whatever paints next.
  (p/clear-styles! g)
  (p/set-colors! g t/text-fg t/terminal-bg))
