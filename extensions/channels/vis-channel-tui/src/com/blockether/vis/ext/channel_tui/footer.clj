(ns com.blockether.vis.ext.channel-tui.footer
  "Dedicated one-row status footer rendered below the input box.

   Codex-style three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       glm-5.1 (balanced)              total ↑12000 (cached 8000) ↓800  $0.04

   Each region holds a list of `{:text :fg :bold? :region :priority}`
   spans separated by ' / ' in muted color. The full segment list is
   built up-front and shrunk by dropping the highest `:priority`
   number (least important) until it fits the available width.

   Footer deliberately avoids transient run-state and cancellation
   banners. Those live in the assistant bubble and host notifications;
   this row keeps slow-changing identity + budget bits.

   Run-state (spinner, iteration counter, elapsed time, current
   phase) lives EXCLUSIVELY in the assistant bubble's `progress->text`
   block. Putting it in the footer too was a duplicate - same
   `\u280b 11.2s` showing twice on screen. The footer keeps slow-changing
   identity + budget bits; the bubble keeps the live activity story
   («Vis is thinking (iter 3)... 4.1s / Esc to cancel»).

   The first footer row carries repository context on the right:
   repo/branch, one compact changed-file count, and ahead/behind counts
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
            [com.blockether.vis.ext.channel-tui.limits-fmt :as lfmt]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.git :as git])
  (:import [java.io File]
           [java.time Instant ZoneId]
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

(defn- reasoning-effort-configurable?
  [info]
  (and (boolean (:reasoning? info))
    (not= false (:reasoning-effort? info))
    (not= :zai-thinking (:reasoning-style info))))

(def ^:private git-label "git")

(defn- git-repo-label
  [{:keys [repo branch]}]
  (str "~/" (or repo "?") " (" (or branch "?") ")"))

(defn- git-dirty-label
  [{:keys [modified created deleted]}]
  (let [changed (+ (long (or modified 0))
                  (long (or created 0))
                  (long (or deleted 0)))]
    (if (zero? changed)
      "files: clean"
      (String/format Locale/ROOT "%d modified"
        (into-array Object [changed])))))

(defn- git-sync-label
  [{:keys [upstream? ahead behind]}]
  (let [ahead  (long (or ahead 0))
        behind (long (or behind 0))]
    (cond
      (not upstream?) "(no upstream)"
      (zero? (+ ahead behind)) "(up to date)"
      :else (str "commits:"
              (when (pos? ahead) (str " ⇡" ahead))
              (when (pos? behind) (str " ⇣" behind))))))

(defn- git-footer-spans
  [{:keys [workspace?] :as status}]
  (if workspace?
    [{:text (str git-label " " (git-repo-label status))
      :fg t/footer-fg-strong :bold? true
      :region :right :priority 2}
     {:text (git-dirty-label status)
      :fg t/footer-fg-muted :bold? false
      :region :right :priority 3}
     {:text (git-sync-label status)
      :fg t/footer-fg-muted :bold? false
      :region :right :priority 4}]
    [{:text (str "No " git-label)
      :fg t/footer-error-fg :bold? true
      :region :right :priority 2}]))

(def ^:private session-cost-keys
  [:input-cost
   :input-uncached-cost
   :input-cached-cost
   :input-cache-write-cost
   :cache-read-cost
   :cache-write-cost
   :output-cost
   :total-cost])

(defn- add-cost-slot
  [acc cost k]
  (let [v (get cost k)]
    (if (number? v)
      (update acc k (fnil + 0.0) (double v))
      acc)))

(defn- add-message-cost
  [acc {:keys [cost]}]
  (cond
    (map? cost)    (reduce #(add-cost-slot %1 cost %2) acc session-cost-keys)
    (number? cost) (update acc :total-cost (fnil + 0.0) (double cost))
    :else          acc))

(defn- session-cost
  "Cumulative session cost across assistant turns. Preserves detailed
   input / cached-input / output / total slots so the footer can show the
   same split as per-bubble meta lines."
  [messages]
  (let [totals (reduce add-message-cost {} messages)]
    (when (seq totals) totals)))

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

(defn- report-for-current-provider
  "Report belonging to `provider`, or nil when the polled report is for
   a different provider (stale after a provider switch). Callers can
   treat nil as \"loading\" — the polling thread populates it on its
   next tick."
  [db provider]
  (let [provider-limits (:provider-limits db)
        report          (:report provider-limits)
        report-provider (or (:provider-id provider-limits) (:provider-id report))]
    (when (and report (= provider report-provider))
      report)))

(defn- limits-status-text
  "Render an explicit placeholder when the report is missing or the
   provider's `:provider/limits-fn` reported a non-ok status. The host
   wrapper (`com.blockether.vis.internal.provider-limits`) is the
   single source of these statuses: providers signal success/failure by
   returning a `::report` with `:status :ok` / `:error` /
   `:unauthenticated` / `:unsupported` / `:unknown-provider`, so the
   footer doesn't need a separate `notify-error!` / `notify-success!`
   side channel — it just reads the envelope."
  [db provider]
  (let [provider-limits (:provider-limits db)
        report          (report-for-current-provider db provider)
        status          (:status report)]
    (cond
      ;; No envelope at all (first paint after launch / provider switch),
      ;; or polled report is for a different provider — show \"loading\".
      (or (nil? provider-limits) (nil? report))
      "limits: loading…"

      (= :error status)
      (let [msg (or (get-in report [:error :message]) "unavailable")]
        (str "limits: error (" msg ")"))

      (= :unauthenticated status) "limits: sign in required"

      ;; :unsupported / :unknown-provider / :ok with empty rows fall
      ;; through to nil so the row stays clean for providers that
      ;; legitimately have no quota story.
      :else nil)))

(defn- format-generic-limit-row
  [now-ms row]
  (let [usage (lfmt/format-limit-usage row)
        reset (some->> (get-in row [:window :resets-at-ms]) (format-reset now-ms))]
    (str (lfmt/generic-limit-label row)
      (when usage (str " " usage))
      (when reset (str " " reset)))))

(defn- generic-limit-sort-key
  [row]
  [(case (:id row)
     :premium_interactions 0
     :premium-interactions 0
     :codex-5h 1
     :zai-coding-5h 1
     :codex-7d 2
     :zai-coding-7d 2
     3)
   (if (lfmt/generic-limit-has-signal? row) 0 1)
   (or (:label row) (name (:id row)))])

(defn- generic-limits-footer-text
  "Footer-left text for the limits row. Returns either:
     - the formatted limit rows (`:status :ok` with at least one row), or
     - a placeholder produced by `limits-status-text` (loading / error /
       unauthenticated), or
     - nil when the provider legitimately has no quota story
       (`:unsupported` / `:unknown-provider` / `:ok` with empty rows)."
  [db provider now-ms]
  (let [report    (report-for-current-provider db provider)
        raw-rows  (get-in report [:dynamic :limits])
        rows      (->> (or (seq (filter lfmt/generic-limit-has-signal? raw-rows))
                         raw-rows)
                    (sort-by generic-limit-sort-key))]
    (if (seq rows)
      (str/join "  "
        (map #(format-generic-limit-row now-ms %)
          (take 2 rows)))
      (limits-status-text db provider))))

;;; ── Segment list ───────────────────────────────────────────────────────────

(comment "Channel statuses and transient notifications render in the header; footer owns model, git, and budgets only.")

(defn- build-segments
  "Vector of `{:text :fg :bold? :region :priority}`.

   `:priority` semantics: 1 = critical (never drop), higher = drop first
   when the row overflows. The full priority hierarchy:
     2  model name, provider dynamic limits
     3  model reasoning suffix
     4  cost
     5  keyboard shortcut hints"
  [db _now-ms]
  (let [{:keys [settings]} db
        info       (chosen-model-info)
        provider   (:provider info)
        ;; Past life: `model` + `model-display` strings were also
        ;; destructured here for a footer label that no longer exists.
        ;; Resurrect via `(let [model (:name info) ...] ...)` if a
        ;; future renderer needs them. `provider` stays — used by
        ;; `codex-provider?` below.
        reasoning? (reasoning-effort-configurable? info)
        reasoning-level (or (:reasoning-level settings) default-reasoning-level)
        codex-provider? (= :openai-codex provider)
        codex-verbosity (or (:openai-codex-verbosity settings) default-codex-verbosity)
        git-spans  (git-footer-spans
                     (if-let [root (:workspace/root db)]
                       (git/cached-workspace-status (File. (str root)))
                       (git/cached-workspace-status)))]
    (cond-> (vec git-spans)
      ;; ── LEFT ──────────────────────────────────────────────────────────────
      ;; Model display + (Ctrl+T) hint moved to builtin_hooks.clj
      ;; (`:tui.builtin.model/footer-segment`).
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

      ;; Spinner / iter-counter / elapsed / cancellation: deliberately NOT here.
      ;; The bubble's `progress->text` already carries live activity, and
      ;; user-facing cancellation feedback is emitted as a host notification.
      ;; Channel statuses (voice recording, transcription, etc.) also stay out
      ;; of the footer. The header's left banner is their single owner.

      ;; ── RIGHT ─────────────────────────────────────────────────────────────
      ;; Git lives here. Provider usage moved to the second row so it sits
      ;; directly under the repository state instead of competing with it.
      )))

(defn- build-usage-segments
  [{:keys [messages]}]
  (let [tokens-str (some-> (session-tokens messages) fmt/format-tokens)
        cost-str   (some-> (session-cost messages) fmt/format-cost)]
    (cond-> []
      tokens-str
      (conj {:text (str "total " tokens-str)
             :fg t/footer-fg-muted :bold? false
             :region :right :priority 2})

      cost-str
      (conj {:text (str "cost " cost-str)
             :fg t/footer-fg-muted :bold? false
             :region :right :priority 3}))))

(defn- build-limits-segments
  [db now-ms]
  (let [provider (some-> (chosen-model-info) :provider)
        text     (when provider
                   (generic-limits-footer-text db provider now-ms))]
    (into (cond-> []
            text
            (conj {:text text
                   :fg t/footer-fg-muted :bold? false
                   :region :left :priority 1}))
      (build-usage-segments db))))

;;; ── Extension footer segments (channel-hooks) ─────────────────────────────
;;
;; Extensions contribute footer segments by adding entries to their
;; `:ext/channel-hooks` vec:
;;
;;   {:channel-id :tui
;;    :hook-id    :*/footer-segment       ;; any keyword whose `name` is
;;                                        ;; "footer-segment"; the namespace
;;                                        ;; identifies the owner. Or use the
;;                                        ;; literal `:tui/footer-segment`.
;;    :render-fn  (fn [db now-ms]
;;                  -> seg-map | [seg-map seg-map ...] | nil)}
;;
;; Each seg-map:
;;   {:ir         [:ir {?:fg-role} & blocks]    ;; required
;;    :region     :left|:center|:right          ;; default :left
;;    :priority   N                             ;; default 3
;;    :row        0|1                           ;; default 0
;;    :join-left? bool                          ;; default false
;;    :fg-role    :default|:muted|:warn|...     ;; default :default
;;    :bold?      bool                          ;; default false}
;;
;; Render-fn may return ONE seg-map or a VECTOR of seg-maps so a
;; single hook can contribute multiple related segments
;; (e.g. "model-display" + "(Ctrl+T)" hint side-by-side).
;;
;; The render-fn returns CANONICAL IR + layout hints; the TUI walks
;; the IR to a plain styled string and packs it into the segment
;; vec built-ins also feed. Other channels translate the same IR
;; differently (Telegram: emit as inline markdown; web: HTML span).
;;
;; `:fg-role` is a channel-agnostic intent keyword (`:warn`,
;; `:muted`, `:default`, `:success`, `:error`); each channel maps
;; it to its palette. Unknown roles fall back to `:default`.

(defn- fg-role->color
  [role]
  (case role
    :muted   t/footer-fg-muted
    :warn    t/footer-warning-fg
    :error   t/footer-error-fg
    :success t/footer-fg-strong
    t/footer-fg))

(defn- ir->footer-text
  "Walk IR to a single PLAIN-text string for the footer packer.

   Footer rows are painted by `draw-spans!` via `p/put-str!`, which
   writes characters into terminal cells verbatim — it has no
   sentinel/block-marker decoder like the bubble painter. So we
   must NOT route through `lines->sentinel-strings`: that prepends
   a block-marker codepoint (e.g. `MARKER_ANSWER_TXT` = `\u206E`)
   which would land in a real cell as a stray 1-column blank
   (the historical \"leading space\" footer bug — conversation
   39a73cfb) and also throw off `spans-width`.

   Styling for footer segments comes from the seg-map's
   `:fg-role` / `:bold?`, not from inline span sentinels, so we
   drop those too and just concat the runs' `:text`.  Multi-line
   IR output is joined with a single space so a misbehaving
   multi-block IR still fits one footer row."
  ^String [ir]
  (let [lines (ir-tui/ir->lines ir 1024)
        line-strs (mapv (fn [{:keys [runs]}]
                          (apply str (map :text runs)))
                    lines)]
    (str/join " " (remove str/blank? line-strs))))

(defn- footer-hook?
  "True when `hook-id` is recognised as a footer-segment contributor."
  [hook-id]
  (let [n (some-> hook-id name)]
    (or (= "footer-segment" n)
      (= :tui/footer-segment hook-id))))

(defn- seg->packed
  "Convert one extension seg-map into the internal segment shape.
   Returns nil for invalid / out-of-row entries."
  [seg ^long row]
  (when (and (map? seg)
          (= row (long (or (:row seg) 0)))
          (vector? (:ir seg))
          (= :ir (first (:ir seg))))
    (let [text (ir->footer-text (:ir seg))]
      (when (and (string? text) (not (str/blank? text)))
        {:text       text
         :fg         (fg-role->color (or (:fg-role seg) :default))
         :bold?      (boolean (:bold? seg))
         :region     (or (:region seg) :left)
         :priority   (long (or (:priority seg) 3))
         :join-left? (boolean (:join-left? seg))}))))

(defn- extension-footer-segments
  "Vector of segments contributed by extensions for footer row `row`
   (0 = top footer row, 1 = limits row).

   Each hook's render-fn may return a single seg-map OR a vec of
   seg-maps (so one hook can contribute multiple related segments).
   Hook crashes never propagate — a misbehaving extension just loses
   its segment that frame. Settings can disable hooks via
   `:contributors-disabled`."
  [db now-ms ^long row]
  (let [disabled (let [s (get-in db [:settings :contributors-disabled])]
                   (when (set? s) s))]
    (vec
      (for [{:keys [hook-id render-fn]} (lp/channel-hooks-for :tui)
            :when (and (ifn? render-fn)
                    (not (and disabled (contains? disabled hook-id)))
                    (footer-hook? hook-id))
            :let [out (try (render-fn db now-ms) (catch Throwable _ nil))
                  segs (cond (sequential? out) out
                         (map? out)        [out]
                         :else             nil)]
            seg segs
            :let [packed (seg->packed seg row)]
            :when packed]
        packed))))

;;; ── Width fitting ──────────────────────────────────────────────────────────

(def ^:private sep "  /  ")
(def ^:private sep-narrow " / ")

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
   before sacrificing segments - looks the same on a wide terminal,
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
  [g db row cols now-ms build-fn row-idx]
  ;; Extension-contributed segments are appended AFTER built-ins so
  ;; they sort to the back of their region; existing built-ins keep
  ;; their visual position. shrink-to-fit drops by `:priority` so
  ;; extensions can opt in to early-drop with a high :priority.
  (let [built-in (build-fn db now-ms)
        ext-segs (extension-footer-segments db now-ms (long row-idx))
        all-segs (into (vec built-in) ext-segs)
        [segs separator] (shrink-to-fit all-segs cols)
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
  "Paint the two footer rows starting at `footer-row`, full width `cols`. Pure draw -
   reads `db` once, computes segments, fits to width, writes cells.
   Safe to call every frame (cheap; no allocations on the hot path
   beyond the spans vector)."
  [g db footer-row cols now-ms]
  ;; Background fill: default footer fg on terminal bg, full row.
  (p/clear-styles! g)
  (p/set-colors! g t/footer-fg t/terminal-bg)
  (p/fill-rect! g 0 footer-row cols 2)
  (draw-footer-row! g db footer-row cols now-ms build-segments 0)
  (draw-footer-row! g db (inc footer-row) cols now-ms build-limits-segments 1)

  ;; Restore neutral state for whatever paints next.
  (p/clear-styles! g)
  (p/set-colors! g t/text-fg t/terminal-bg))
