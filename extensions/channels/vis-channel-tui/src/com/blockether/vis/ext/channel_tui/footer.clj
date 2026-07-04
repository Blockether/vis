(ns com.blockether.vis.ext.channel-tui.footer
  "Dedicated one-row status footer rendered below the input box.

   Codex-style three-region layout:

       [LEFT]                    [CENTER]                    [RIGHT]
       glm-5.1 (balanced)              total tok 12000→800 (cached 8000)  $0.04

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
            [com.blockether.vis.ext.channel-tui.components :as components]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.render-ir :as ir-tui]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.git :as git])
  (:import [java.io File]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util Locale]))

;;; ── Data extraction from app-db ────────────────────────────────────────────
(def ^:private default-reasoning-level :balanced)
(def ^:private default-codex-verbosity :low)
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
(defn- git-change-bits
  "Per-kind changed-file counts `~modified +created -deleted` (only nonzero
   segments shown), or nil when the working tree is clean."
  [{:keys [modified created deleted]}]
  (let [m (long (or modified 0))
        c (long (or created 0))
        d (long (or deleted 0))
        parts (cond-> []
                (pos? m) (conj (str "~" m))
                (pos? c) (conj (str "+" c))
                (pos? d) (conj (str "-" d)))]
    (when (seq parts) (str/join " " parts))))
(defn- git-status-bits
  "Status fragment shown *inside* the `(branch …)` parens, codex/git-prompt
   style: changed-file counts then `⇡ahead ⇣behind`; a branch with NO
   upstream configured (`:upstream?` explicitly false) shows `∅` — no
   remote to compare against, so ahead/behind are meaningless. `∅` (not a
   warning glyph: nothing is wrong) is a bare NARROW char, safe for the
   lanterna cell grid (VS-16 emoji are wide and desync the paint). Clean
   *and* synced yields nil so the branch name stands alone — no glyph."
  [{:keys [ahead behind upstream?], :as status}]
  (let [ahead (long (or ahead 0))
        behind (long (or behind 0))
        change (git-change-bits status)
        sync (cond-> []
               (pos? ahead) (conj (str "⇡" ahead))
               (pos? behind) (conj (str "⇣" behind)))
        parts (cond-> []
                change (conj change)
                (seq sync) (conj (str/join " " sync))
                (false? upstream?) (conj "∅"))]
    (when (seq parts) (str/join " " parts))))
(defn- git-repo-label
  "`~/repo (branch)` when clean+synced, otherwise the status bits ride inside
   the parens, e.g. `~/vis (main ~2 +3 -1 ⇡4)`."
  [{:keys [repo branch], :as status}]
  (str "~/" (or repo "?") " ("
    (or branch "?")
    (when-let [bits (git-status-bits status)] (str " " bits))
    ")"))
(defn- abbreviate-home
  "Shorten an absolute path by replacing the user's home dir with `~`."
  [^String path]
  (let [home (System/getProperty "user.home")]
    (if (and path home (str/starts-with? path home))
      (str "~" (subs path (count home)))
      (str path))))
(defn- git-footer-spans
  [{:keys [workspace? draft? draft-root], :as status}]
  (cond
    ;; In a draft, the clone's internal git details (clone dir name,
    ;; detached HEAD, no-upstream) are noise — show the draft's location
    ;; (so the user knows WHERE the isolated tree lives) and how many
    ;; files differ, all in one chunk.
    draft? [{:text (str "DRAFT " (if draft-root (abbreviate-home draft-root) "draft")
                     (when-let [bits (git-change-bits status)] (str " (" bits ")"))),
             :fg t/footer-warning-fg,
             :bold? true,
             :region :right,
             :priority 2}]
    workspace? [{:text (str git-label " " (git-repo-label status)),
                 :fg t/footer-fg-strong,
                 :bold? true,
                 :region :right,
                 :priority 2}]
    :else [{:text (str "No " git-label),
            :fg t/footer-error-fg,
            :bold? true,
            :region :right,
            :priority 2}]))
(def ^:private session-cost-keys
  [:input-cost :input-uncached-cost :input-cached-cost :input-cache-write-cost :cache-read-cost
   :cache-write-cost :output-cost :total-cost])
(defn- add-cost-slot
  [acc cost k]
  (let [v (get cost k)] (if (number? v) (update acc k (fnil + 0.0) (double v)) acc)))
(defn- add-message-cost
  [acc {:keys [cost]}]
  (cond (map? cost) (reduce #(add-cost-slot %1 cost %2) acc session-cost-keys)
    (number? cost) (update acc :total-cost (fnil + 0.0) (double cost))
    :else acc))
(defn- session-cost
  "Cumulative session cost across assistant turns. Preserves detailed
   input / cached-input / output / total slots so the footer can show the
   same split as per-bubble meta lines."
  [messages]
  (let [totals (reduce add-message-cost {} messages)] (when (seq totals) totals)))
(defn- first-token-number
  [tokens ks]
  (some (fn [k] (let [v (get tokens k)] (when (number? v) v))) ks))
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
    (when (seq totals) (merge {:input 0, :output 0, :cached-input 0} totals))))
(def ^:private one-week-ms (* 7 24 60 60 1000))
(def ^:private short-reset-formatter (DateTimeFormatter/ofPattern "EEE h:mm a" Locale/ROOT))
(def ^:private long-reset-formatter (DateTimeFormatter/ofPattern "MMM d h:mm a" Locale/ROOT))
(defn- format-relative-reset
  [now-ms reset-ms]
  (when reset-ms
    (let [total-seconds (max 0 (quot (- (long reset-ms) (long now-ms)) 1000))
          days (quot total-seconds 86400)
          hours (quot (mod total-seconds 86400) 3600)
          minutes (quot (mod total-seconds 3600) 60)]
      (cond (pos? days) (str days "d" hours "h")
        (pos? hours) (str hours "h" minutes "m")
        (pos? minutes) (str minutes "m")
        :else (str total-seconds "s")))))
(defn- format-absolute-reset
  [now-ms reset-ms]
  (when reset-ms
    (let [zoned (.atZone (Instant/ofEpochMilli (long reset-ms)) (ZoneId/systemDefault))
          formatter (if (and (>= (- (long reset-ms) (long now-ms)) 0)
                          (< (- (long reset-ms) (long now-ms)) one-week-ms))
                      short-reset-formatter
                      long-reset-formatter)]
      (.format ^DateTimeFormatter formatter zoned))))
(defn- format-reset
  [now-ms reset-ms]
  (let [relative (format-relative-reset now-ms reset-ms)
        absolute (format-absolute-reset now-ms reset-ms)]
    (cond (and relative absolute) (str "↺" relative " @ " absolute)
      relative (str "↺" relative)
      absolute (str "↺" absolute)
      :else "↺--")))
(defn- report-for-current-provider
  "Report belonging to `provider`, or nil when the polled report is for
   a different provider (stale after a provider switch). Callers can
   treat nil as \"loading\" — the polling thread populates it on its
   next tick."
  [db provider]
  (let [provider-limits (:provider-limits db)
        report (:report provider-limits)
        report-provider (or (:provider-id provider-limits) (:provider-id report))]
    (when (and report (= provider report-provider)) report)))
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
        report (report-for-current-provider db provider)
        status (:status report)]
    (cond
      ;; No envelope at all (first paint after launch / provider switch),
      ;; or polled report is for a different provider — show \"loading\".
      (or (nil? provider-limits) (nil? report)) "limits: loading…"
      (= :error status) (let [msg (or (get-in report [:error :message]) "unavailable")]
                          (str "limits: error (" msg ")"))
      (= :unauthenticated status) "limits: sign in required"
      ;; :unsupported / :unknown-provider / :ok with empty rows fall
      ;; through to nil so the row stays clean for providers that
      ;; legitimately have no quota story.
      :else nil)))
(defn- format-generic-limit-row
  [now-ms row]
  (let [usage (lfmt/format-limit-usage row)
        reset (some->> (get-in row [:window :resets-at-ms])
                (format-reset now-ms))]
    (str (lfmt/generic-limit-label row) (when usage (str " " usage)) (when reset (str " " reset)))))
(defn- generic-limit-sort-key
  [row]
  [(case (:id row)
     :premium_interactions 0
     :premium-interactions 0
     :codex-5h 1
     :zai-coding-plan-5h 1
     :codex-7d 2
     :zai-coding-plan-7d 2
     3) (if (lfmt/generic-limit-has-signal? row) 0 1) (or (:label row) (name (:id row)))])
(defn- generic-limits-footer-text
  "Footer-left text for the limits row. Returns either:
     - the formatted limit rows (`:status :ok` with at least one row), or
     - a placeholder produced by `limits-status-text` (loading / error /
       unauthenticated), or
     - nil when the provider legitimately has no quota story
       (`:unsupported` / `:unknown-provider` / `:ok` with empty rows)."
  [db provider now-ms]
  (let [report (report-for-current-provider db provider)
        raw-rows (get-in report [:dynamic :limits])
        rows (->> (or (seq (filter lfmt/generic-limit-has-signal? raw-rows)) raw-rows)
               (sort-by generic-limit-sort-key))]
    (if (seq rows)
      (str/join "  " (map #(format-generic-limit-row now-ms %) (take 2 rows)))
      (limits-status-text db provider))))
;;; ── Segment list ───────────────────────────────────────────────────────────
(comment
  "Channel statuses and transient notifications render in the header; footer owns model, git, and budgets only.")
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
        info (chosen-model-info)
        provider (:provider info)
        ;; Past life: `model` + `model-display` strings were also
        ;; destructured here for a footer label that no longer exists.
        ;; Resurrect via `(let [model (:name info) ...] ...)` if a
        ;; future renderer needs them. `provider` stays — used by
        ;; `codex-provider?` below.
        reasoning? (reasoning-effort-configurable? info)
        reasoning-level (or (:reasoning-level settings) default-reasoning-level)
        codex-provider? (= :openai-codex provider)
        codex-verbosity (or (:openai-codex-verbosity settings) default-codex-verbosity)
        ws (:workspace db)
        ws-root (or (:workspace/root db) (:root ws))
        in-draft? (some? (:fork-ms ws))
        git-spans (git-footer-spans (cond-> (if ws-root
                                              (git/cached-working-tree-status (File. (str ws-root)))
                                              (git/cached-working-tree-status))
                                      in-draft? (assoc :draft?
                                                  true :draft-root
                                                  (str ws-root))))
        ;; Session-scoped managed resources (nREPLs, daemons…). Rendered as a
        ;; bracketed "resources N (C-x s)" button — no glyph (a width-1 icon read
        ;; as noise; the word carries the meaning). Shown only when this
        ;; session owns ≥1.
        res-count (count (try (lp/list-resources (get-in db [:session :id]))
                           (catch Throwable _ nil)))
        ;; Filesystem: the session root + any extra dirs granted via /fs or the picker.
        ;; Surfaced in the footer (BOTH channels) so the add-directory
        ;; affordance is discoverable here, not buried; /fs and /root manage it
        ;; (the web twin is the clickable footer dirs button).
        dir-count (inc (count (try (lp/workspace-filesystem-roots ws) (catch Throwable _ nil))))]
    (cond-> (vec git-spans)
      ;; ── LEFT ──────────────────────────────────────────────────────────────
      ;; Model display + (Ctrl+T) hint moved to builtin_hooks.clj
      ;; (`:tui.builtin.model/footer`).
      reasoning? (conj {:text (str "reasoning: " (name reasoning-level)),
                        :fg t/footer-fg-muted,
                        :bold? false,
                        :region :left,
                        :priority 3})
      reasoning? (conj {:text (str "(" (keymap/label-for :cycle-reasoning) ")"),
                        :join-left? true,
                        :fg t/footer-fg-muted,
                        :bold? false,
                        :region :left,
                        :priority 5})
      codex-provider? (conj {:text (str "verbosity: " (name codex-verbosity)),
                             :fg t/footer-fg-muted,
                             :bold? false,
                             :region :left,
                             :priority 3})
      codex-provider? (conj {:text (str "(" (keymap/label-for :cycle-verbosity) ")"),
                             :join-left? true,
                             :fg t/footer-fg-muted,
                             :bold? false,
                             :region :left,
                             :priority 5})
      ;; ── RIGHT: managed-resource BUTTON. One span, bracketed + bold like a real
      ;; TUI button (the web twin has a clickable "Manage" button; this is its
      ;; terminal mirror). C-x s opens resources directly; C-x p remains the
      ;; global command palette.
      true (conj {:text     (str " resources " res-count " (" (keymap/label-for :open-resources) ") ")
                  :fg       t/footer-fg-strong
                  :bold?    true
                  :region   :right
                  :priority 2
                  :kind     :footer-resources})
      ;; ── RIGHT: filesystem-root count as a CLICKABLE button (web-footer parity).
      ;; Clicking it — or pressing C-x d — opens the file-explorer picker; the
      ;; binding rides ON the chip so it's discoverable. `/fs` opens the same
      ;; picker; `/root <path>` changes the session's root by typing.
      ;; Rendered as a bare "filesystem N (C-x d)" button — no glyph (the word
      ;; is the affordance).
      true (conj {:text     (str " filesystem " dir-count " (" (keymap/label-for :open-dirs) ") "),
                  :fg       t/footer-fg-strong,
                  :bold?    true,
                  :region   :right,
                  :priority 3,
                  :kind     :footer-dirs})
      ;; Spinner / iter-counter / elapsed / cancellation: deliberately NOT here.
      ;; The bubble's `progress->text` already carries live activity, and
      ;; user-facing cancellation feedback is emitted as a host notification.
      ;; Channel statuses (voice recording, transcription, etc.) also stay out
      ;; of the footer. The header's left banner is their single owner.
      ;; ── RIGHT ─────────────────────────────────────────────────────────────
      ;; Git lives here. Provider usage moved to the second row so it sits
      ;; directly under the repository state instead of competing with it.
      )))
(defn- build-usage-segments "Right-side cumulative session usage rendered with the SAME canonical\n   helpers as the per-bubble meta line (`fmt/meta-tokens` / `fmt/meta-cost`),\n   so the footer and the bubble can never drift in shape — tokens read as\n   `11.5k→35 (cached 4.1k)` and cost as `~$0.0070`. The numbers stay\n   cumulative across the session; only the FORMAT is shared." [{:keys [messages]}] (let [toks (session-tokens messages) cost (session-cost messages) tok-text (when toks (fmt/meta-tokens toks)) cost-text (fmt/meta-cost cost)] (cond-> [] tok-text (conj {:text tok-text, :fg t/footer-fg-muted, :bold? false, :region :right, :priority 2}) cost-text (conj {:text cost-text, :fg t/footer-fg-strong, :bold? false, :region :right, :priority 3}))))
(defn- build-limits-segments
  [db now-ms]
  ;; Limits/usage belong to the provider the SESSION actually routes through —
  ;; the same per-session pref the model label (builtin_hooks) and the engine
  ;; use. Reading `chosen-model-info` here showed the GLOBAL router default
  ;; (e.g. zai) even after the user switched the session to Claude, so the
  ;; "request usages" row reported the wrong coding plan. Fall back to the
  ;; router default only when the session has no explicit pick.
  (let [provider (or (some-> (:session-model-pref db) :provider not-empty keyword)
                   (when-let [sid (get-in db [:session :id])]
                     (some-> (lp/gateway-session-model-cached sid)
                       :provider not-empty keyword))
                   (some-> (chosen-model-info) :provider))
        text (when provider (generic-limits-footer-text db provider now-ms))]
    (into (cond-> []
            text (conj
                   {:text text, :fg t/footer-fg-muted, :bold? false, :region :left, :priority 1}))
      (build-usage-segments db))))
;;; ── Echo area (which-key strip + transient messages) ────────────────
(defn- hint-segment
  [text priority]
  {:text text, :fg t/footer-fg-muted, :bold? false, :region :center, :priority priority})
(defn- which-key-segments
  "Emacs `which-key` strip. When the C-x prefix is ARMED (`:prefix` set on the
   input state), the echo area stops staying blank and instead lists
   the prefix you just pressed plus EVERY plain second key and the verb it runs
   — so the next keystroke in the chord is always discoverable, exactly like
   Emacs' which-key popup. The `C-x-` header survives width-shrinking
   (priority 0); the least-used verbs drop first on a narrow terminal."
  []
  (into [{:text     (str (keymap/chord keymap/prefix-key) \-)
          :fg       t/footer-fg-strong
          :bold?    true
          :region   :center
          :priority 0}
         {:text     (str keymap/prefix-palette-key " palette")
          :fg       t/footer-fg
          :bold?    true
          :region   :center
          :priority 1}]
    (map-indexed
      (fn [i {:keys [key label]}]
        {:text     (str key \space label)
         :fg       t/footer-fg
         :bold?    true
         :region   :center
         :priority (+ i 2)})
      keymap/prefix-commands)))

;;; ── Extension footer segments (channel contributions) ─────────────────────
;;
;; Extensions contribute footer / hint segments by adding entries to
;; their `:ext/channel-contributions` map:
;;
;;   {:tui.slot/footer-segment
;;    [{:id :my.extension/footer
;;      :fn (fn [db now-ms]
;;            -> seg-map | [seg-map seg-map ...] | nil)}]}
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
;; Fn may return ONE seg-map or a VECTOR of seg-maps so a
;; single hook can contribute multiple related segments
;; (e.g. "model-display" + "(Alt+M)" hint side-by-side).
;;
;; The fn returns CANONICAL IR + layout hints; the TUI walks
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
    :muted t/footer-fg-muted
    :warn t/footer-warning-fg
    :error t/footer-error-fg
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
   (the historical \"leading space\" footer bug — session
   39a73cfb) and also throw off `spans-width`.

   Styling for footer segments comes from the seg-map's
   `:fg-role` / `:bold?`, not from inline span sentinels, so we
   drop those too and just concat the runs' `:text`.  Multi-line
   IR output is joined with a single space so a misbehaving
   multi-block IR still fits one footer row."
  ^String [ir]
  (let [lines (ir-tui/ir->lines ir 1024)
        line-strs (mapv (fn [{:keys [runs]}] (apply str (map :text runs))) lines)]
    (str/join " " (remove str/blank? line-strs))))
(defn- seg->packed
  "Convert one extension seg-map into the internal segment shape.
   Returns nil for invalid / out-of-row entries."
  [seg ^long row]
  (when
    (and (map? seg) (= row (long (or (:row seg) 0))) (vector? (:ir seg)) (= :ir (first (:ir seg))))
    (let [raw (ir->footer-text (:ir seg))
          ;; Chip kinds render through `components/button!`, whose cap wants the
          ;; label PRE-padded ` like this ` (the resources / dirs chips do the
          ;; same). The IR walker trims trailing whitespace, so re-pad here
          ;; centrally instead of relying on it surviving the IR round-trip.
          text (if (:kind seg) (str " " (str/trim raw) " ") raw)]
      (when (and (string? text) (not (str/blank? text)))
        {:text text,
         :fg (fg-role->color (or (:fg-role seg) :default)),
         :bold? (boolean (:bold? seg)),
         :region (or (:region seg) :left),
         :priority (long (or (:priority seg) 3)),
         :join-left? (boolean (:join-left? seg)),
         :kind (:kind seg)}))))
(defn- extension-segments
  "Vector of segments contributed by extensions for `slot` / `row`.

   Each contribution fn may return a single seg-map OR a vec of
   seg-maps. Contribution crashes never propagate — a misbehaving
   extension just loses its segment that frame. Settings can disable
   contributions via `:contributors-disabled`; `undisableable` ids
   bypass that guard for core identity chrome."
  [slot undisableable db now-ms row]
  (let [disabled (let [s (get-in db [:settings :contributors-disabled])] (when (set? s) s))]
    (vec (for [{:keys [id], f :fn} (lp/channel-contributions-for :tui slot)
               :when (and (ifn? f)
                       (or (contains? undisableable id)
                         (not (and disabled (contains? disabled id)))))
               :let [out (try (f db now-ms) (catch Throwable _ nil))
                     segs (cond (sequential? out) out
                            (map? out) [out]
                            :else nil)]
               seg segs
               :let [packed (seg->packed seg row)]
               :when packed]
           packed))))
(defn- extension-footer-segments
  [db now-ms ^long row]
  ;; `:tui.builtin.model/footer` is core identity (provider /
  ;; model display) and CANNOT be disabled. Even if a settings round-
  ;; trip placed it into `:contributors-disabled`, the renderer ignores
  ;; it for this contribution so the user never accidentally hides the model
  ;; label.
  (extension-segments :tui.slot/footer-segment #{:tui.builtin.model/footer} db now-ms row))
;;; ── Width fitting ──────────────────────────────────────────────────────────
(def ^:private sep "  /  ")
(def ^:private sep-narrow " / ")
(defn- region-spans [segments region] (filterv #(= region (:region %)) segments))
(defn- separator-before [span separator] (if (:join-left? span) " " separator))
(defn- spans-width
  [spans separator]
  (reduce (fn [w [i span]]
            (+ w (if (zero? i) 0 (p/display-width (separator-before span separator))) (p/display-width (:text span))))
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
                 (and (seq c) (seq r)) inc)]
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
    (cond (fit? segments sep) [segments sep]
      (fit? segments sep-narrow) [segments sep-narrow]
      :else (loop [segs segments]
              (cond (empty? segs) [segs sep-narrow]
                (fit? segs sep-narrow) [segs sep-narrow]
                :else (let [worst-priority (apply max (map :priority segs))
                                    ;; Drop one occurrence with the worst priority.
                            victim (some #(when (= worst-priority (:priority %)) %) segs)]
                        (recur (vec (remove #(identical? victim %) segs)))))))))
;;; ── Drawing ────────────────────────────────────────────────────────────────
(defn- draw-spans!
  "Draw spans left-to-right starting at `col`. Each span uses its own
   fg + optional bold; separators are rendered in muted fg. Returns
   final col after the last span."
  [g start-col row spans separator]
  (reduce (fn [c [i s]]
            (let [c (if (zero? i)
                      c
                      (do (p/clear-styles! g)
                        (p/set-colors! g t/footer-fg-muted t/terminal-bg)
                        (let [separator (separator-before s separator)]
                          (p/put-str! g c row separator)
                          (+ c (p/display-width separator)))))]
              (if (:kind s)
                ;; Real button chip via the shared `components/button!` — the SAME
                ;; component the header right-side buttons use (filled inverted cap,
                ;; accent on hover, click region registered under `:kind`).
                (do (components/button! g c row (:text s) (:kind s)
                      {:register? true})
                  (+ c (p/display-width (:text s))))
                (do
                  (p/clear-styles! g)
                  (p/set-colors! g (or (:fg s) t/footer-fg) t/terminal-bg)
                  (when (:bold? s) (p/enable! g p/BOLD))
                  (p/put-str! g c row (:text s))
                  (p/clear-styles! g)
                  (+ c (p/display-width (:text s)))))))
    start-col
    (map-indexed vector spans)))

(defn- draw-footer-row!
  [g db row cols now-ms build-fn row-idx]
  ;; Extension-contributed segments are PREPENDED before built-ins so
  ;; primary-identity content (model display, provider label) paints
  ;; leftmost. Built-in config toggles (reasoning level, verbosity)
  ;; come after. shrink-to-fit still drops by `:priority`.
  ;; Without this ordering the model paints to the right of `reasoning:` /
  ;; `verbosity:` and gets clipped on narrow terminals.
  (let [built-in (build-fn db now-ms)
        ext-segs (extension-footer-segments db now-ms (long row-idx))
        all-segs (into (vec ext-segs) built-in)
        [segs separator] (shrink-to-fit all-segs cols)
        l (region-spans segs :left)
        c (region-spans segs :center)
        r (region-spans segs :right)
        edge-pad 2
        l-w (spans-width l separator)
        c-w (spans-width c separator)
        r-w (spans-width r separator)
        l-col edge-pad
        r-col (max (+ l-col l-w 2) (- cols edge-pad r-w))
        ;; Center between L's right edge and R's left edge.
        l-end (+ l-col l-w)
        c-col (max (+ l-end (if (seq l) 2 0)) (- (quot (+ l-end r-col) 2) (quot c-w 2)))]
    (when (seq l) (draw-spans! g l-col row l separator))
    (when (seq c) (draw-spans! g c-col row c separator))
    (when (seq r) (draw-spans! g r-col row r separator))))
(defn- echo-segments
  "Content for the Emacs echo-area row directly above the input box.

   NORMALLY EMPTY — the row lights up only when there is something to say:
     - C-x prefix armed        → the `which-key` strip (the next keys in the chord)
     - live turn / cancelling   → the `C-g cancel` abort hint
     - a transient `:echo` msg  → a one-shot message (the caller clears it)

   No idle keybinding nags: discovery of every C-x verb lives in the help
   overlay (C-x h), so extensions advertise their chords THERE, not here."
  [{:keys [loading? cancelling? input echo]}]
  (cond
    (:prefix input) (which-key-segments)
    cancelling?     [(hint-segment "Cancelling... please wait" 1)]
    loading?        [(hint-segment (str (keymap/chord keymap/abort-key) " cancel") 1)]
    (not (str/blank? (str echo)))
    [(hint-segment (str/trim (str echo)) 1)]
    :else           []))
(defn draw-echo-area!
  "Emacs echo area / minibuffer analogue: ONE flat row directly above the
   input box. Terminal background, NO box and NO side rails — the input box
   owns its own border below. Blank until `echo-segments` has something to
   say (a chord in progress, the mid-turn cancel hint, or a transient
   message). Left-aligned at the input's horizontal pad so it tracks the
   prompt column."
  [g db echo-row cols _now-ms]
  (p/clear-styles! g)
  (p/set-colors! g t/footer-fg t/terminal-bg)
  (p/fill-rect! g 0 echo-row cols 1)
  (let [[segs separator] (shrink-to-fit (echo-segments db) cols)
        spans (region-spans segs :center)]
    (when (seq spans)
      (draw-spans! g 2 echo-row spans separator)))
  ;; Restore neutral state for whatever paints next.
  (p/clear-styles! g)
  (p/set-colors! g t/text-fg t/terminal-bg))
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
