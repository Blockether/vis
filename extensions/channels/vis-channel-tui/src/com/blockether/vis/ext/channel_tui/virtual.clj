(ns com.blockether.vis.ext.channel-tui.virtual
  "Virtualized chat-panel layout — the React-window pattern for our
   Lanterna scrollback.

   ── Why this exists ────────────────────────────────────────────────────────
   The pre-virtualisation render path projected (i.e. ran
   `format-answer-with-thinking` over) EVERY assistant message on
   EVERY frame, even ones scrolled far off-screen. For finalized
   bubbles the outer `format-answer-with-thinking` LRU made each
   per-frame call ~315 ns once warm, but cold-opening a long
   conversation paid the full ~500 ms format cost per big trace
   bubble before the FIRST frame ever made it to the terminal.
   Conversation 954bf315 (2 \u00d7 ~500ms bubbles \u2192 ~870 ms cold paint
   plus a separate `parse-md-refs` regex bug \u2014 see `links.clj`)
   was the live trigger: the screen stayed bg-fill until both big
   bubbles formatted, which read as \"frozen TUI on open\".

   ── The shape of the fix ───────────────────────────────────────────────────
   1. Cheap row-height ESTIMATE per message, derived from the trace
      shape (iteration count, code-form count, answer length) without
      touching the markdown tokenizer or the wrap-text engine.
   2. `layout` plans the next paint: estimates every message's height,
      pins the auto-bottom scroll position, and \u2014 only for messages
      whose viewport interval is non-empty \u2014 runs the FULL projection
      + real `bubble-height` measurement. Off-screen bubbles never
      trigger `format-answer-with-thinking`.
   3. The result carries the visible subset (with screen-row
      coordinates) plus a refined `total-h` and `eff-scroll` for
      scrollbar geometry. Callers paint `visible` directly.

   ── What this is NOT ───────────────────────────────────────────────────────
   * Not a windowing library \u2014 we already have the bubble painter.
     This namespace is pure layout planning.
   * Not a height-precision oracle \u2014 the estimates are intentionally
     rough (within ~2\u00d7). Off-screen accuracy only nudges the
     scrollbar thumb; visible bubbles always paint at their REAL
     height because they go through the full projection pass.
   * Not a cache \u2014 the cache layers live in `render.clj`
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
   [com.blockether.vis.ext.channel-tui.render :as render]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

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
   `bubble-w`. Order of magnitude only \u2014 the full painter is
   authoritative for visible messages. NEVER calls `wrap-text`,
   `markdown->lines`, or `format-answer-with-thinking`.

   Heuristic:
     * label row + meta row + gap row \u2192 chrome-rows = 3
     * user msg \u2192 add  text-len / content-w  rows
     * assistant w/ trace \u2192 add
         iteration-headers (1 per iter)
         + code-forms (1 per form)
         + result-lines (1 per result)
         + thinking-chars / 80     (rough wrap)
         + answer-chars / 60       (rough wrap, narrower)
     * plain assistant \u2192 add  text-len / content-w  rows

   The constants are tuned against conversation 954bf315; they
   over-estimate slightly on dense bubbles and under-estimate on
   answer-heavy bubbles, but stay within ~2\u00d7 of real either way."
  ^long [message ^long bubble-w]
  (let [content-w (max 1 (- bubble-w 4))
        chrome-rows 3
        role     (:role message)
        trace    (:trace message)
        text     (:text message)
        ans      (:raw-answer message)]
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
            ans-c    (char-count ans)]
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

;;; ── Per-message projection ─────────────────────────────────────────────────

(defn project-message
  "Apply the same `:text` projection `screen/apply-settings` used to
   apply, but for ONE message at a time so the virtual layer can call
   it lazily per-bubble. Hits the same caches `apply-settings` did.

   `live-text` (optional) overrides the projected text \u2014 used by the
   loading-bubble path where the text changes every spinner tick and
   bypasses the cache."
  [message ^long bubble-w settings]
  (let [show-timestamps? (boolean (get settings :show-timestamps false))
        strip-ts (fn [m] (if show-timestamps? m (dissoc m :timestamp)))]
    (cond
      (and (= :assistant (:role message)) (:trace message))
      (-> message
        (assoc :text
          (render/format-answer-with-thinking
            (:raw-answer message) (:trace message) bubble-w settings
            (:confidence message)
            (= :cancelled (:status message))))
        strip-ts)

      (= :assistant (:role message))
      (-> message
        (assoc :text
          (render/format-answer-markdown (:text message) bubble-w))
        strip-ts)

      :else (strip-ts message))))

;;; ── Layout plan ────────────────────────────────────────────────────────────
;;
;; A frame's plan: total height (for scrollbar geometry), clamped scroll
;; offset, full per-message offsets vec (so callers can locate any
;; message later if needed), and the visible subset \u2014 each entry carries
;; the projected message map + its REAL height + its top row relative to
;; the messages-area top.

(defn- visible?
  "True iff the closed-open interval [top, top+h) intersects [0, inner-h)."
  [^long top ^long h ^long inner-h]
  (and (> (+ top h) 0) (< top inner-h)))

(defn- assoc-vec
  "Like `assoc` for vectors but pads with `pad` if the index is past
   the end (defensive \u2014 we never expect that in practice)."
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
   spinner-led progress block (`render/progress->text`). That bubble
   is treated as visible by definition (it's at the bottom and the
   user is staring at it). Its height comes from the full painter
   pipeline, not the estimator, so the spinner row count is right
   even on the first loading frame.

   The visible set is computed in TWO passes:

     1. Estimate every height \u2192 cumulative offsets \u2192 clamp scroll
        \u2192 identify candidate visible idxs.
     2. Project + measure ONLY those candidates. Refine `:heights`,
        recompute `:offsets`, re-clamp `:eff-scroll`, recompute the
        visible interval (a bubble whose real height is much smaller
        than its estimate may have neighbours that NOW intersect the
        viewport \u2014 so we widen by one on each side as a cheap
        no-extra-projection fallback if you want strict pixel-perfect
        scrolling, run `layout` twice; in practice one pass is fine
        because the height delta is bounded by the heuristic).

   No I/O, no Lanterna, no draw side effects. Pure planning.

   NOTE on the signature: Clojure caps primitive-hinted fns at 4
   args, and this one takes 6, so `bubble-w` / `inner-h` are
   plain Object args here — we cast with `long` inside the body."
  [messages bubble-w settings scroll inner-h
   {:keys [progress loading? progress-extra] :or {progress nil loading? false}}]
  (let [bubble-w (long bubble-w)
        inner-h  (long inner-h)
        n        (long (count messages))
        ;; Pass 1 ── estimate every message.
        est      (mapv (fn [m] (estimated-height m bubble-w)) messages)
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
        ;; Loading bubble is forced visible at the end (last index).
        loading-last-idx
        (when (and loading? (pos? n)
                (= :assistant (:role (peek messages))))
          (long (dec n)))
        forced-idxs
        (cond-> cand-idxs
          (and loading-last-idx
            (not (some #(= % loading-last-idx) cand-idxs)))
          (conj loading-last-idx))
        projected
        (mapv
          (fn [^long i]
            (let [m (nth messages i)
                  pm (if (= i loading-last-idx)
                       (assoc m :text
                         (render/progress->text progress bubble-w settings progress-extra))
                       (project-message m bubble-w settings))
                  real-h (long (render/bubble-height pm bubble-w))]
              {:idx i :projected pm :height real-h}))
          forced-idxs)
        ;; Refine heights vec with real measurements.
        heights' (reduce
                   (fn [hs {:keys [^long idx ^long height]}]
                     (assoc-vec hs idx height 0))
                   est projected)
        offsets' (vec (reductions + 0 (map long heights')))
        total-h' (long (peek offsets'))
        ;; Re-clamp against the REAL `total-h'`, not the
        ;; pass-1 `eff-1`. When estimates undershoot the real
        ;; total, max-scroll grows in pass-2 — piping `eff-1`
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
;; the viewport — the user feels that as a frame-stall mid-scroll.
;; Pre-warming runs the same projection on a background daemon
;; thread right after the first paint succeeds, so by the time the
;; user scrolls, every bubble's `format-answer-with-thinking` /
;; `bubble-height` entries are already in the LRU. Subsequent
;; layouts hit the cache and return in microseconds.
;;
;; Concurrency contract:
;;   * `cached*` in render.clj uses double-checked locking — the
;;     pre-warm thread NEVER blocks the render thread on the same
;;     key, even mid-compute.
;;   * The thread is a daemon at `Thread/MIN_PRIORITY + 1` so a
;;     busy JVM still services UI before warming. Honestly the JVM
;;     barely respects priority, but setting it documents intent.
;;   * Cancellation is via `Thread.interrupt()` — we check between
;;     bubbles, not inside `format-answer-with-thinking*` (no way
;;     to interrupt a CPU-bound function mid-call without
;;     instrumenting the tokenizer, which is way more invasive than
;;     the win is worth).
;;   * Returns the `Thread`. Caller stores it; on conversation
;;     switch / shutdown, call `stop-pre-warm!` to interrupt.

(defn pre-warm!
  "Spawn a daemon worker thread that calls `project-message` and
   `bubble-height` for every message in `messages` so the LRU is
   hot by the time the user scrolls. Walks in REVERSE order — the
   user almost always scrolls UP from the bottom, so the next-to-
   last message warms first, the FIRST message last.

   Returns the `Thread` so the caller can stop it on conversation
   switch / shutdown via `stop-pre-warm!`. Returns `nil` for an
   empty conversation (nothing to warm).

   Settings parity with `layout`: pass the SAME settings map so the
   cached entries match the keys subsequent layout passes will
   look up. Mismatched settings = wasted compute (the layout pass
   will format again with different cache keys)."
  ^Thread [messages bubble-w settings]
  (let [n (long (count messages))]
    (when (pos? n)
      (let [bubble-w (long bubble-w)
            t (Thread.
                ^Runnable
                (fn []
                  (try
                    (loop [i (dec n)]
                      (when (and (>= i 0) (not (.isInterrupted (Thread/currentThread))))
                        (let [m (nth messages i)
                              ;; project-message hits the same caches
                              ;; layout will hit; bubble-height too.
                              ;; Skip user messages — they're cheap and
                              ;; the cache hit alone isn't worth a
                              ;; thread context switch.
                              project? (= :assistant (:role m))]
                          (when project?
                            (let [pm (project-message m bubble-w settings)]
                              (render/bubble-height pm bubble-w))))
                        (recur (dec i))))
                    (catch InterruptedException _
                      ;; Cooperative cancellation — nothing to do.
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
