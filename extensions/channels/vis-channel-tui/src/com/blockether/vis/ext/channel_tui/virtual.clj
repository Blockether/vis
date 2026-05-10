(ns com.blockether.vis.ext.channel-tui.virtual
  "Virtualized chat-panel layout - the React-window pattern for our
   Lanterna scrollback.

   ── Why this exists ────────────────────────────────────────────────────────
   The pre-virtualisation render path projected (i.e. ran
   `format-answer-with-thinking` over) EVERY assistant message on
   EVERY frame, even ones scrolled far off-screen. For finalized
   bubbles the outer `format-answer-with-thinking` LRU made each
   per-frame call ~315 ns once warm, but cold-opening a long
   conversation paid the full ~500 ms format cost per big trace
   bubble before the FIRST frame ever made it to the terminal.
   Conversation 954bf315 (2 x ~500ms bubbles -> ~870 ms cold paint
   plus a separate `parse-md-refs` regex bug - see `links.clj`)
   was the live trigger: the screen stayed bg-fill until both big
   bubbles formatted, which read as \"frozen TUI on open\".

   ── The shape of the fix ───────────────────────────────────────────────────
   1. Cheap row-height ESTIMATE per message, derived from the trace
      shape (iteration count, code-form count, answer length) without
      touching the markdown tokenizer or the wrap-text engine.
   2. `layout` plans the next paint: estimates every message's height,
      pins the auto-bottom scroll position, and - only for messages
      whose viewport interval is non-empty - runs the FULL projection
      + real `bubble-height` measurement. Off-screen bubbles never
      trigger `format-answer-with-thinking`.
   3. The result carries the visible subset (with screen-row
      coordinates) plus a refined `total-h` and `eff-scroll` for
      scrollbar geometry. Callers paint `visible` directly.

   ── What this is NOT ───────────────────────────────────────────────────────
   * Not a windowing library - we already have the bubble painter.
     This namespace is pure layout planning.
   * Not a height-precision oracle - the estimates are intentionally
     rough (within ~2x). Off-screen accuracy only nudges the
     scrollbar thumb; visible bubbles always paint at their REAL
     height because they go through the full projection pass.
   * Not a cache - the cache layers live in `render.clj`
     (`format-answer-with-thinking`, `bubble-height`,
     `format-answer-markdown`). This namespace just decides which
     messages get to consult those caches each frame.

   ── Caveat: scrollbar drift on first scroll up ─────────────────────────────
   When the user scrolls UP into a region whose estimates were
   wrong, the next layout will re-project those messages and refine
   their heights. The scrollbar thumb may shift one row as the
   total-h sum corrects. Mirrors `react-window`'s
   `VariableSizeList` behaviour; eye-acceptable in practice.

   `(set! *warn-on-reflection* true)` and `*unchecked-math*
   :warn-on-boxed` clean across this namespace per the repo's GraalVM
   ratchet."
  (:require
   [com.blockether.vis.ext.channel-tui.render :as render]
   [com.blockether.vis.internal.render :as ir])
  (:import
   [java.util LinkedHashMap]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;;; ── Sticky real-height cache ───────────────────────────────────────────────────
;;
;; Why this exists: with the layout planner alone, off-screen
;; messages got `estimated-height` (cheap) and visible ones got
;; `bubble-height` (real). When the user scrolled, the visible set
;; changed, different messages flipped between estimate <-> real, and
;; `total-h` jittered - the scrollbar thumb position drifted on
;; every scroll frame, and click-to-position landed somewhere else
;; than where the user clicked because the click computed a target
;; against the OLD total-h while the next paint used a different
;; one. Conversation 7b18414d showed the symptom on first click.
;;
;; The fix: ONCE we've measured a message's real bubble-height we
;; remember it forever (or until LRU eviction). Off-screen messages
;; we've already seen keep their real height; only never-seen
;; messages fall back to the estimate. The pre-warmer writes here
;; too, so within ~1 s of opening a conversation EVERY assistant
;; has its real height pinned and the scrollbar is perfectly
;; stable.
;;
;; Cache key tuple: `[(identityHashCode message) bubble-w
;; (identityHashCode settings)]`. Identity-hash on `message` is
;; safe because chat-state messages are immutable once added
;; (`update :messages conj` only ever appends; finalised assistant
;; messages get replaced as a whole map exactly once). Width and
;; settings identity capture the cache-busting axes we care about.
;; Detail expansion state is intentionally NOT in the key: a toggle
;; remeasures the visible clicked message, while off-screen measured
;; heights stay sticky so the scrollback does not jump.
;;
;; Cap is generous (8192) because each entry is just a long height
;; - a few KB of overhead total even at 8k entries.

(def ^:private ^:const height-cache-cap 8192)

(defonce ^:private ^LinkedHashMap height-cache
  (proxy [LinkedHashMap] [128 0.75 true]
    (removeEldestEntry [_eldest]
      (> (.size ^LinkedHashMap this) (long height-cache-cap)))))

(defn- height-key [message bubble-w settings _detail-expansions]
  [(System/identityHashCode message)
   (long bubble-w)
   (System/identityHashCode settings)])

(defn- height-cache-get
  "Peek the sticky height cache. Returns a long or nil."
  [message bubble-w settings detail-expansions]
  (locking height-cache
    (.get height-cache (height-key message bubble-w settings detail-expansions))))

(defn- height-cache-put!
  "Pin `h` as the real height for `message` at `bubble-w` under
   `settings`. Returns `h` so callers can chain."
  [message bubble-w settings detail-expansions h]
  (locking height-cache
    (.put height-cache (height-key message bubble-w settings detail-expansions) h))
  h)

(defn invalidate-heights!
  "Drop the entire sticky height cache. Tests + width changes call this."
  []
  (locking height-cache (.clear height-cache)))

(defn height-cache-size
  "Current sticky-height entry count (handy for tests / diagnostics)."
  ^long []
  (locking height-cache (.size height-cache)))

;;; ── Estimated-height heuristic ─────────────────────────────────────────────

(defn- char-count ^long [s]
  (long (count (or s ""))))

(defn- div-ceil
  "Ceiling division that handles 0/negative `b` gracefully (returns 0)."
  ^long [^long a ^long b]
  (if (or (<= b 0) (<= a 0))
    0
    (long (Math/ceil (double (/ (double a) (double b)))))))

(defn estimated-height
  "Cheap estimate of how many rows a message will paint at width
   `bubble-w`. Order of magnitude only - the full painter is
   authoritative for visible messages. NEVER calls `wrap-text`,
   `markdown->lines`, or `format-answer-with-thinking`.

   Heuristic:
     * label row + meta row + gap row -> chrome-rows = 3
     * user msg -> add  text-len / content-w  rows
     * assistant w/ trace -> add
         iteration-headers (1 per iter)
         + code-forms (1 per form)
         + result-lines (1 per result)
         + thinking-chars / 80     (rough wrap)
         + answer-chars / 60       (rough wrap, narrower)
     * plain assistant -> add  text-len / content-w  rows

   The constants are tuned against conversation 954bf315; they
   over-estimate slightly on dense bubbles and under-estimate on
   answer-heavy bubbles, but stay within ~2x of real either way."
  ^long [message ^long bubble-w]
  (let [content-w (max 1 (- bubble-w 4))
        chrome-rows 3
        role     (:role message)
        trace    (:traces message)
        ;; Pre-projection rough text estimate: extract plain text from
        ;; `:ir` so the height heuristic doesn't depend on `:text`
        ;; (which is only set by the walker AFTER projection).
        text     (or (:text message)
                   (some-> (:ir message) ir/extract-text))]
    (cond
      (= role :user)
      (long
        (+ chrome-rows
          (div-ceil (char-count text) content-w)))

      (and (= role :assistant) trace)
      (let [n-iter   (long (count trace))
            code-fs  (long (reduce (fn [^long acc it]
                                     (+ acc (long (count (:code it)))))
                             0 trace))
            res-fs   (long (reduce (fn [^long acc it]
                                     (+ acc (long (count (:results it)))))
                             0 trace))
            think-c  (long (reduce (fn [^long acc it]
                                     (+ acc (char-count (:thinking it))))
                             0 trace))
            ;; Heuristic for answer width: `:text` is the rendered
            ;; markdown string (assistant-message stores it eagerly
            ;; via render-answer). The layout pipeline re-wraps from
            ;; `:ir` IR; this size estimate just picks a
            ;; ballpark row count.
            ans-c    (char-count text)]
        (long
          (+ chrome-rows
            n-iter                              ;; iteration headers
            code-fs                             ;; code-form rows
            res-fs                              ;; result rows
            (div-ceil think-c 80)
            (div-ceil ans-c 60))))

      (= role :assistant)
      (long
        (+ chrome-rows
          (div-ceil (char-count text) content-w)))

      :else
      (long (+ chrome-rows (div-ceil (char-count text) content-w))))))

(defn- turn-separator?
  [messages settings ^long idx]
  (and (true? (get settings :differentiate-turns true))
    (pos? idx)
    (= :user (:role (nth messages idx)))
    (= :assistant (:role (nth messages (dec idx))))))

(defn- estimated-height-with-turn-separator
  [messages settings bubble-w idx message]
  (let [bubble-w (long bubble-w)
        idx      (long idx)]
    (long (+ (estimated-height message bubble-w)
            (if (turn-separator? messages settings idx) 2 0)))))

(defn- with-turn-separator
  [message messages settings ^long idx]
  (if (turn-separator? messages settings idx)
    (assoc message :turn-separator? true)
    (dissoc message :turn-separator?)))

;;; ── Per-message projection ─────────────────────────────────────────────────

(defn- turn-identity
  "Stable per-turn key for detail-disclosure node ids. The progress
   bubble live-streams while `:conversation-turn-id` is still nil
   (the server only assigns it once the turn lands), so we MUST
   prefer `:client-turn-id` - that one exists from the moment the
   user submits, and gets carried through into the persisted
   assistant message. Picking conversation-turn-id here would
   change the disclosure node id at the live -> done flip; every
   open <details> / REASONING toggle the user expanded during
   streaming would silently snap back to its default state.

   Old conversations loaded from disk lack `:client-turn-id`
   entirely, so we fall back to `:conversation-turn-id` for them.
   Both still scope the click region by turn so a click on one
   answer can't toggle a disclosure in another."
  [message]
  (or (:client-turn-id message) (:conversation-turn-id message)))

(defn project-message
  "Apply the same `:text` projection `screen/apply-settings` used to
   apply, but for ONE message at a time so the virtual layer can call
   it lazily per-bubble. Hits the same caches `apply-settings` did."
  ([message ^long bubble-w settings] (project-message message bubble-w settings nil))
  ([message ^long bubble-w settings {:keys [conversation-id detail-expansions]}]
   (let [show-timestamps? (boolean (get settings :show-timestamps false))
         strip-ts (fn [m] (if show-timestamps? m (dissoc m :timestamp)))]
     (cond
       (and (= :assistant (:role message)) (:traces message))
       (let [{:keys [text lines line-meta]}
             (render/format-answer-with-thinking-data
               (:ir message) (:traces message) bubble-w settings
               (:confidence message)
               (= :cancelled (:status message))
               {:conversation-id      conversation-id
                :conversation-turn-id (turn-identity message)
                :detail-expansions   detail-expansions})]
         (-> message
           (assoc :text text :prewrapped-lines lines :line-meta line-meta)
           strip-ts))

       ;; Both assistant- and user-messages now carry canonical IR on
       ;; `:ir` (chat/assistant-message + chat/user-message lift
       ;; via vis/text->ir at construction). The walker is the single
       ;; bubble layout engine; the rendered markdown string stays in
       ;; `:text` for clipboard/copy.
       (#{:assistant :user} (:role message))
       (let [ir (:ir message)
             {:keys [text lines line-meta]}
             (render/format-answer-markdown-data
               ir bubble-w
               {:conversation-id      conversation-id
                :conversation-turn-id (turn-identity message)
                :detail-expansions   detail-expansions})]
         (-> message
           (assoc :text text :prewrapped-lines lines :line-meta line-meta)
           strip-ts))

       :else (strip-ts message)))))

;;; ── Layout plan ────────────────────────────────────────────────────────────
;;
;; A frame's plan: total height (for scrollbar geometry), clamped scroll
;; offset, full per-message offsets vec (so callers can locate any
;; message later if needed), and the visible subset - each entry carries
;; the projected message map + its REAL height + its top row relative to
;; the messages-area top.

(defn- visible?
  "True iff the closed-open interval [top, top+h) intersects [0, inner-h)."
  [^long top ^long h ^long inner-h]
  (and (> (+ top h) 0) (< top inner-h)))

(defn- assoc-vec
  "Like `assoc` for vectors but pads with `pad` if the index is past
   the end (defensive - we never expect that in practice)."
  [v ^long i x pad]
  (let [n (long (count v))]
    (cond
      (< i n)  (assoc v i x)
      (= i n)  (conj v x)
      :else    (recur (conj v pad) i x pad))))

(defn layout
  "Plan a paint of `messages` into a vertical viewport of `inner-h`
   rows at width `bubble-w`. Returns:

     {:total-h    <long>     ;; sum of (real-or-estimated) heights
      :eff-scroll <long>     ;; clamped scroll offset
      :heights    <vec long> ;; one per message; real for visible, est for off-screen
      :offsets    <vec long> ;; cumulative running sum, one entry longer than messages
      :visible    [{:idx N :top R :height H :projected M} ...]}

   `scroll` is `nil` for auto-bottom (jump to the latest message) or
   a non-negative long for a specific row offset.

   `loading?` swaps the LAST assistant message's `:text` to the live
   spinner-led progress block (`render/progress->text`) only when that
   bubble intersects the viewport. Auto-bottom keeps live progress visible;
   manual scrollback does not re-project off-screen live progress.

   The visible set is computed in TWO passes:

     1. Estimate every height -> cumulative offsets -> clamp scroll
        -> identify candidate visible idxs.
     2. Project + measure ONLY those candidates. Refine `:heights`,
        recompute `:offsets`, re-clamp `:eff-scroll`, recompute the
        visible interval (a bubble whose real height is much smaller
        than its estimate may have neighbours that NOW intersect the
        viewport - so we widen by one on each side as a cheap
        no-extra-projection fallback if you want strict pixel-perfect
        scrolling, run `layout` twice; in practice one pass is fine
        because the height delta is bounded by the heuristic).

   No I/O, no Lanterna, no draw side effects. Pure planning.

   NOTE on the signature: Clojure caps primitive-hinted fns at 4
   args, and this one takes 6, so `bubble-w` / `inner-h` are
   plain Object args here - we cast with `long` inside the body."
  [messages bubble-w settings scroll inner-h
   {:keys [progress loading? progress-extra] :or {progress nil loading? false}}
   & [{:keys [conversation-id detail-expansions]}]]
  (let [bubble-w          (long bubble-w)
        inner-h           (long inner-h)
        detail-expansions detail-expansions
        n                 (long (count messages))
        ;; Pass 1 ── sticky-real height if we've measured this
        ;; message before, otherwise the cheap estimate. Identity-
        ;; keyed cache means once a message has been visible (or
        ;; touched by `pre-warm!`) we keep its real height forever
        ;; - no more `total-h` jitter on scroll, no more
        ;; click-to-position landing in the wrong row.
        est      (mapv (fn [idx m]
                         (or (height-cache-get m bubble-w settings detail-expansions)
                           (estimated-height-with-turn-separator messages settings bubble-w idx m)))
                   (range n) messages)
        est-off  (vec (reductions + 0 (map long est)))
        est-tot  (long (peek est-off))
        scroll-1 (long (or scroll (max 0 (- est-tot inner-h))))
        eff-1    (long (min scroll-1 (max 0 (- est-tot inner-h))))
        ;; Pass 1b ── candidate visible idxs from estimates.
        cand-idxs
        (filterv (fn [^long i]
                   (let [top (- (long (nth est-off i)) eff-1)
                         h   (long (nth est i))]
                     (visible? top h inner-h)))
          (range n))
        ;; Pass 2 ── project + REAL height for candidates only.
        ;; Loading bubble gets live progress only if it is already visible.
        loading-last-idx
        (when (and loading? (pos? n)
                (= :assistant (:role (peek messages))))
          (long (dec n)))
        visible-idxs
        cand-idxs
        projected
        (mapv
          (fn [^long i]
            (let [m (nth messages i)
                  loading-bubble? (= i loading-last-idx)
                  pm (if loading-bubble?
                       (let [{:keys [text lines line-meta]}
                             (render/progress->lines-data progress bubble-w settings
                               (assoc progress-extra
                                 :conversation-id conversation-id
                                 ;; Live progress bubble must use the
                                 ;; same turn key the completed answer
                                 ;; will use a moment later; otherwise
                                 ;; the REASONING / <details> toggle
                                 ;; node-ids change at the live -> done
                                 ;; flip and the user's expansion state
                                 ;; resets to its default. See
                                 ;; `turn-identity` for the precedence.
                                 :conversation-turn-id (turn-identity m)
                                 :detail-expansions detail-expansions))]
                         (assoc m
                           :text text
                           :prewrapped-lines lines
                           :line-meta line-meta))
                       (project-message m bubble-w settings
                         {:conversation-id conversation-id
                          :detail-expansions detail-expansions}))
                  pm (with-turn-separator pm messages settings i)
                  real-h (long (render/bubble-height pm bubble-w))]
              ;; Pin the real height in the sticky cache for the
              ;; raw message identity (NOT the loading bubble - its
              ;; height changes every spinner tick by definition).
              (when-not loading-bubble?
                (height-cache-put! m bubble-w settings detail-expansions real-h))
              {:idx i :projected pm :height real-h}))
          visible-idxs)
        ;; Refine heights vec with real measurements.
        heights' (reduce
                   (fn [hs {:keys [^long idx ^long height]}]
                     (assoc-vec hs idx height 0))
                   est projected)
        offsets' (vec (reductions + 0 (map long heights')))
        total-h' (long (peek offsets'))
        ;; Re-clamp against the REAL `total-h'`, not the
        ;; pass-1 `eff-1`. When estimates undershoot the real
        ;; total, max-scroll grows in pass-2 - piping `eff-1`
        ;; through `min` here would pin the user above the true
        ;; bottom. Clamp the ORIGINAL scroll value (or `nil` for
        ;; auto-bottom) against the refined ceiling.
        eff-2    (long
                   (if (nil? scroll)
                     (max 0 (- total-h' inner-h))
                     (max 0 (min (long scroll)
                              (max 0 (- total-h' inner-h))))))
        visible-set
        (mapv
          (fn [{:keys [^long idx ^long height projected]}]
            {:idx       idx
             :height    height
             :projected projected
             :top       (- (long (nth offsets' idx)) eff-2)})
          projected)]
    {:total-h    total-h'
     :eff-scroll eff-2
     :heights    heights'
     :offsets    offsets'
     :visible    visible-set}))

;;; ── Background pre-warmer ────────────────────────────────────────────────────
;;
;; Why this exists, top of mind: with virtualisation alone, scrolling
;; UP through a long conversation pays the FULL
;; `format-answer-with-thinking*` cost (~500 ms / big trace bubble)
;; on the render thread the FIRST time a never-seen bubble enters
;; the viewport - the user feels that as a frame-stall mid-scroll.
;; Pre-warming runs the same projection on a background daemon
;; thread right after the first paint succeeds, so by the time the
;; user scrolls, every bubble's `format-answer-with-thinking` /
;; `bubble-height` entries are already in the LRU. Subsequent
;; layouts hit the cache and return in microseconds.
;;
;; Concurrency contract:
;;   * `cached*` in render.clj uses double-checked locking - the
;;     pre-warm thread NEVER blocks the render thread on the same
;;     key, even mid-compute.
;;   * The thread is a daemon at `Thread/MIN_PRIORITY + 1` so a
;;     busy JVM still services UI before warming. Honestly the JVM
;;     barely respects priority, but setting it documents purpose.
;;   * Cancellation is via `Thread.interrupt()` - we check between
;;     bubbles, not inside `format-answer-with-thinking*` (no way
;;     to interrupt a CPU-bound function mid-call without
;;     instrumenting the tokenizer, which is way more invasive than
;;     the win is worth).
;;   * Returns the `Thread`. Caller stores it; on conversation
;;     switch / shutdown, call `stop-pre-warm!` to interrupt.

(defn pre-warm!
  "Spawn a daemon worker thread that calls `project-message` and
   `bubble-height` for every message in `messages` so the LRU is
   hot by the time the user scrolls. Walks in REVERSE order - the
   user almost always scrolls UP from the bottom, so the next-to-
   last message warms first, the FIRST message last.

   Returns the `Thread` so the caller can stop it on conversation
   switch / shutdown via `stop-pre-warm!`. Returns `nil` for an
   empty conversation (nothing to warm).

   Settings parity with `layout`: pass the SAME settings map so the
   cached entries match the keys subsequent layout passes will
   look up. Mismatched settings = wasted compute (the layout pass
   will format again with different cache keys)."
  ^Thread [messages bubble-w settings & [{:keys [conversation-id detail-expansions]}]]
  (let [n (long (count messages))]
    (when (pos? n)
      (let [bubble-w (long bubble-w)
            t (Thread.
                ^Runnable
                (fn []
                  (try
                    (loop [i (dec n)]
                      (when (and (>= i 0) (not (.isInterrupted (Thread/currentThread))))
                        ;; Warm EVERY message, not just assistants.
                        ;; User-message bubble-height is cheap, but
                        ;; the *real* value is `chrome+lines+refs+1`
                        ;; while `estimated-height` undershoots by 1
                        ;; for short user prompts. Skipping users
                        ;; means total-h drifts by 1 per user msg
                        ;; the first time it scrolls into view -
                        ;; user-visible as a one-row scrollbar nudge.
                        ;; Pin everyone; stability beats the cycles.
                        (let [m (nth messages i)
                              pm (project-message m bubble-w settings
                                   {:conversation-id conversation-id
                                    :detail-expansions detail-expansions})
                              pm (with-turn-separator pm messages settings i)
                              h  (long (render/bubble-height pm bubble-w))]
                          (height-cache-put! m bubble-w settings detail-expansions h))
                        (recur (dec i))))
                    (catch InterruptedException _
                      ;; Cooperative cancellation - nothing to do.
                      nil)
                    (catch Throwable t
                      ;; Pre-warming must NEVER bring the TUI down.
                      ;; Swallow + lose silently; the next layout
                      ;; pass will pay the un-warmed cost itself.
                      (try
                        (require 'taoensso.telemere)
                        ((resolve 'taoensso.telemere/log!)
                         :warn
                         (str "vis-channel-tui pre-warm threw: "
                           (or (ex-message t) (str t))))
                        (catch Throwable _ nil)))))
                "vis-channel-tui-prewarm")]
        (.setDaemon t true)
        (.setPriority t (max Thread/MIN_PRIORITY
                          (dec Thread/NORM_PRIORITY)))
        (.start t)
        t))))

(defn stop-pre-warm!
  "Interrupt and forget a pre-warm thread previously returned by
   `pre-warm!`. Safe on `nil` and on already-finished threads."
  [^Thread t]
  (when (and t (.isAlive t))
    (.interrupt t)))
