(ns com.blockether.vis.ext.channel-tui.virtual
  "Virtualized chat-panel layout - the React-window pattern for our
   Lanterna scrollback.

   ── Why this exists ────────────────────────────────────────────────────────
   The pre-virtualisation render path projected (i.e. ran
   `format-answer-with-thinking` over) EVERY assistant message on
   EVERY frame, even ones scrolled far off-screen. For finalized
   bubbles the outer `format-answer-with-thinking` LRU made each
   per-frame call ~315 ns once warm, but cold-opening a long
   session paid the full ~500 ms format cost per big trace
   bubble before the FIRST frame ever made it to the terminal.
   Session 954bf315 (2 x ~500ms bubbles -> ~870 ms cold paint)
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
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.markdown-layout :as layout]
            [com.blockether.vis.internal.render :as ast])
  (:import [java.util IdentityHashMap LinkedHashMap]
           [java.util.concurrent.atomic AtomicLong]))

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
;; one.
;;
;; The fix: ONCE we've measured a message's real bubble-height we
;; remember it forever (or until LRU eviction). Off-screen messages
;; we've already seen keep their real height; only never-seen
;; messages fall back to the estimate. The pre-warmer writes here
;; too, so within ~1 s of opening a session EVERY assistant
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
    (removeEldestEntry [_eldest] (> (.size ^LinkedHashMap this) (long height-cache-cap)))))

;; Identity memo for the message fingerprint. `layout` pass-1 recomputed
;; `(hash (dissoc message ...))` for EVERY message EVERY tick (~2x per bubble,
;; via height-cache-get + the projection height-key). The `dissoc` allocates a
;; fresh map whose hashCode is never cached, so each call fully re-walks the
;; map — ~6 us / 20 KB per 60-message pass, on every 80ms live tick.
;;
;; Messages are immutable once finalised into app-db (`update :messages conj`
;; only appends; see render.clj), so the STABLE prefix keeps object identity
;; across live ticks — only the growing live/last bubble churns its identity.
;; Memoize the fingerprint under the message OBJECT (IdentityHashMap == key
;; equality). Identity ⟹ identical content ⟹ identical fingerprint, so a hit is
;; always correct; a miss (churned/rebuilt object) recomputes exactly today's
;; value. This never becomes the CACHE KEY (that stays the content fingerprint),
;; so it can't reintroduce the identityHashCode scroll-jump the doc below warns of.
;; Bounded by clear-on-overflow; messages are alive in app-db anyway, so the map
;; holds no reference the session doesn't already.
(defonce ^:private ^IdentityHashMap message-fp-memo (IdentityHashMap. 256))

(defn- message-content-fingerprint
  "Content-derived fingerprint of a message map for the height-cache.
   Stable across reducer rebuilds (assoc creates a new map identity
   but content is unchanged), so the cache hits across renders. The
   prior key used `(System/identityHashCode message)` which churned
   on every workspace state change — every scroll into a previously-
   measured bubble re-measured and shifted total-h by the height
   delta, producing the visible scrollbar jumps.

   Excludes volatile keys the painter assoc's into a copy of the
   message during projection (`:text`, `:prewrapped-lines`,
   `:line-meta`, `:turn-separator?`) so cache hits even after
   projection has decorated the map.

   Memoized by object identity (see comment above): a stable bubble
   computes its fingerprint once, then every later tick is a lookup."
  [message]
  (locking message-fp-memo
    (or (.get message-fp-memo message)
        (let [fp (hash (dissoc message :text :prewrapped-lines :line-meta :turn-separator?))]
          (when (> (.size message-fp-memo) 4096) (.clear message-fp-memo))
          (.put message-fp-memo message fp)
          fp))))

(def ^:private settings-fingerprint-keys
  [:show-thinking :show-iterations :show-silent :show-iteration-headers :preview/default-lines])

;; One-slot identity memo. `layout` threads the SAME `settings` object into
;; `height-key` for every message (~2x per bubble -> ~120 calls/tick), and each
;; call re-ran `select-keys`+`hash` to the identical result — ~37% of the warm
;; tick's allocation was this one redundant fingerprint. Cache the last
;; (settings-identity -> fingerprint) pair: a hit is a reference compare, a miss
;; recomputes (pure fn of the map, so a stale/raced slot only ever recomputes,
;; never returns a wrong value). No lock: worst case two threads with different
;; settings thrash the slot and recompute, which is exactly today's cost.
(defonce ^:private settings-fp-cell (volatile! nil))

(defn- settings-fingerprint
  "Content-derived fingerprint of the subset of settings keys that
   actually affect bubble height. Same rationale as the message
   fingerprint: identity is fragile, content is stable."
  [settings]
  (let [cached (deref settings-fp-cell)]
    (if (and cached (identical? (nth cached 0) settings))
      (nth cached 1)
      (let [fp (hash (select-keys settings settings-fingerprint-keys))]
        (vreset! settings-fp-cell [settings fp])
        fp))))

(defn- height-key
  [message bubble-w settings detail-expansions session-id]
  [(message-content-fingerprint message) (long bubble-w) (settings-fingerprint settings)
   ;; This message's OWN expand/collapse state. Without it a height measured
   ;; while a block was expanded (or collapsed) leaked across the toggle —
   ;; the layout then used a stale height, total-h was wrong, and the scroll
   ;; jumped on the next expand. Scoped to the message's turn so toggling one
   ;; disclosure doesn't bust every other message's cached height.
   (render/message-detail-expansions-key session-id message detail-expansions)])

(defn- height-cache-get
  "Peek the sticky height cache. Returns a long or nil."
  [message bubble-w settings detail-expansions session-id]
  (locking height-cache
    (.get height-cache (height-key message bubble-w settings detail-expansions session-id))))

(defn- height-cache-put!
  "Pin `h` as the real height for `message` at `bubble-w` under `settings`
   + its current expansion state. Returns `h` so callers can chain."
  [message bubble-w settings detail-expansions session-id h]
  (locking height-cache
    (.put height-cache (height-key message bubble-w settings detail-expansions session-id) h))
  h)

;; Estimate memo — same key axes as the sticky height cache. `layout`
;; pass-1 estimates every message the height cache misses ON EVERY
;; FRAME; on a big cold session (reopen, toggle-resync, resize) that is
;; O(session-chars) per frame while the scroll ease ticks at ~60fps.
;; The estimate is pure in (message content, bubble-w, settings subset,
;; expansion key), so memoizing under the SAME key turns every frame
;; after the first into hash lookups. Entries become garbage once the
;; real height lands (the height cache is checked FIRST) — the LRU cap
;; simply ages them out.
(defonce ^:private ^LinkedHashMap estimate-cache
  (proxy [LinkedHashMap] [128 0.75 true]
    (removeEldestEntry [_eldest] (> (.size ^LinkedHashMap this) (long height-cache-cap)))))

;; Sticky PROJECTION cache — the pass-2 companion to `height-cache`.
;; `layout` re-projects every VISIBLE bubble each frame; during a live turn
;; the ~12Hz tick re-runs `project-message` (markdown re-wrap -> a fresh
;; `:prewrapped-lines` vector, ~25-30 KB) for every STABLE bubble above the
;; growing live one, even though their content is byte-identical frame to
;; frame. That churn dominated layout allocation (~1 MB / call -> ~10-14 MB/s
;; of garbage while streaming -> Serial-GC pauses in the log's slow frames).
;; Memoize the projection under the SAME content key the height cache uses.
;; The live / last bubble and windowed (`:tail-lines`) slices are NEVER routed
;; here, so a hit only ever returns a projection identical to a fresh render.
;; Smaller cap than the height cache: each entry pins a wrapped-line vector.
(defonce ^:private ^LinkedHashMap projection-cache
  (proxy [LinkedHashMap] [128 0.75 true]
    (removeEldestEntry [_eldest] (> (.size ^LinkedHashMap this) 512))))

(defonce ^:private ^AtomicLong estimate-cache-gen (AtomicLong.))
(defonce ^:private ^AtomicLong projection-cache-gen (AtomicLong.))

;; Sticky TAIL-PROJECTION cache. The bottom-locked last bubble (auto-scroll or
;; scroll clamped to max) is rendered with `:tail-lines` so only the visible tail
;; is styled - but `project-message` still parses the WHOLE message Markdown each
;; frame to find that tail, so a long completed answer pinned at the bottom re-
;; parses + re-wraps its full body on every relayout (~0.85 ms / ~4.4 MB for a
;; 640-row bubble here). The tail projection is a pure function of the message +
;; `tail-n`, and the LIVE streaming bubble never reaches this path (it is peeled
;; off as the loading bubble), so a completed last bubble's tail is byte-identical
;; frame to frame - memoize it under the height key plus `tail-n`. Tiny cap: only
;; the last bubble (plus a couple during a resize) ever tail-walks.
(defonce ^:private ^LinkedHashMap tail-projection-cache
  (proxy [LinkedHashMap] [16 0.75 true]
    (removeEldestEntry [_eldest] (> (.size ^LinkedHashMap this) 32))))
(defonce ^:private ^AtomicLong tail-projection-cache-gen (AtomicLong.))

(defn invalidate-heights!
  "Drop the sticky height cache AND the estimate memo. Tests + whole-cache
   busts (registry-toggle resync) call this."
  []
  (locking height-cache (.clear height-cache))
  (render/clear-cache! estimate-cache estimate-cache-gen)
  (render/clear-cache! projection-cache projection-cache-gen)
  (render/clear-cache! tail-projection-cache tail-projection-cache-gen))

(defn height-cache-size
  "Current sticky-height entry count (handy for tests / diagnostics)."
  ^long []
  (locking height-cache (.size height-cache)))

;;; ── Estimated-height heuristic ─────────────────────────────────────────────

(defn- wrapped-rows-est
  "Rows string `s` occupies when hard-folded at width `w`: Σ per input
   line of `ceil(len/w)`, blank interior lines counting 1. Single indexed
   pass — no regex, no split allocation — because this runs once per COLD
   message per layout frame. Slightly OVER-counts trailing newlines
   (`split-lines` drops those in the painter) — overshoot is the safe
   direction: `total-h` may shrink when the real height lands, never grow,
   so the scroll anchor (and the scrollbar thumb) holds."
  ^long [s ^long w]
  (let
    [^String s
     (if (string? s) s (str (or s "")))

     n
     (.length s)

     w
     (max 1 w)]

    (loop
      [i
       0

       seg
       0

       rows
       0]

      (if (>= i n)
        (if (pos? seg) (+ rows (quot (+ seg (dec w)) w)) rows)
        (let [nl? (= \newline (.charAt s i))]
          (recur (inc i)
                 (if nl? 0 (inc seg))
                 (if nl? (+ rows (max 1 (quot (+ seg (dec w)) w))) rows)))))))

(defn- prose-rows-est
  "Upper-bound rows for MARKDOWN-rendered text at width `w`:
   `wrapped-rows-est` plus block-chrome slack. The IR walker renders a
   line that STARTS with a list/quote/heading marker (`- + * > #`,
   `1.`) as its own block with blank chrome rows around it — a pasted
   diff (every line `+ …`) explodes ~5x that way, width-independently.
   Charge marker lines +5 so those pastes stay on the overshoot side.
   Single indexed pass, no regex, no split allocation."
  ^long [s ^long w]
  (let
    [^String s
     (if (string? s) s (str (or s "")))

     n
     (.length s)

     w
     (max 1 w)]

    (loop
      [start
       0

       rows
       0]

      (if (>= start n)
        rows
        (let
          [e
           (.indexOf s "\n" start)

           end
           (if (neg? e) n e)

           len
           (- end start)

           ;; First non-space char within the first 4 columns — markdown
           ;; treats deeper indents as code, not as a block marker.
           j
           (long
             (loop [k start]
               (if (and (< k end) (< (- k start) 4) (= \space (.charAt s k))) (recur (inc k)) k)))

           marker?
           (and (< j end)
                (let [c (.charAt s j)]
                  (or (and (case c
                             (\- \+ \* \> \#)
                             true

                             false)
                           (or (= (inc j) end) (= \space (.charAt s (inc j)))))
                      ;; ordered list: digits then `.` then space/eol
                      (and (Character/isDigit c)
                           (let
                             [d (long (loop [k j]
                                        (if (and (< k end) (Character/isDigit (.charAt s k)))
                                          (recur (inc k))
                                          k)))]
                             (and (< d end)
                                  (= \. (.charAt s d))
                                  (or (= (inc d) end) (= \space (.charAt s (inc d))))))))))

           base
           (max 1 (quot (+ len (dec w)) w))]

          (recur (inc end) (+ rows base (if marker? 5 0))))))))

(defn- any-details-expanded?
  "True when this message has ANY disclosure expanded (or the global
   `:expand-all` flag is on). The estimate then sizes thinking/result
   sections at their FULL wrapped height instead of the collapsed preview
   cap — an expanded off-screen block must never make the estimate
   undershoot (that is the direction that jumps the scrollbar)."
  [message detail-expansions session-id]
  (boolean (when detail-expansions
             (let [k (render/message-detail-expansions-key session-id message detail-expansions)]
               ;; `k` is `:expand-all`, or a vector whose FIRST element may be the bulk
               ;; `:baseline` keyword (`:collapse` / `:expand`) prepended by
               ;; `message-detail-expansions-key`, followed by `[node-id exp?]` pairs.
               ;; `:expand` means every disclosure is open; otherwise ANY per-node
               ;; override that is `true` counts. Skip the non-pair keyword element —
               ;; destructuring it as a pair blows up with `nth on Keyword` and takes
               ;; the height/paint thread down (freezing the TUI).
               (or (= :expand-all k)
                   (and (vector? k)
                        (boolean (some (fn [e]
                                         (cond (= e :expand) true
                                               (vector? e) (true? (second e))
                                               :else false))
                                       k))))))))

(defn estimated-height
  "Cheap estimate of how many rows a message will paint at width
   `bubble-w`. Order of magnitude only - the full painter is
   authoritative for visible messages. NEVER calls `wrap-text`,
   `markdown->lines`, or `format-answer-with-thinking`.

   INVARIANT: estimates must OVERSHOOT. When the real height replaces an
   estimate mid-scroll, `total-h` may shrink but never grow; growth moves
   the thumb TOWARD the bottom while the user scrolls UP — the visible
   scrollbar jump. So every section is sized as `ceil` of its folded rows
   (`wrapped-rows-est`) against a slightly NARROWER width than the
   painter's, and collapsed-by-default sections (thinking peek, result
   preview) are capped at the painter's preview limit + chrome ONLY when
   nothing in the message is expanded.

   Shape mirrored from `format-iteration-entry-entries`:
     * code       — soft-folded at the bubble edge (`p/fold-cols`), painted
                    in full: fold rows + 2 pad rows.
     * result     — markdown at fill-w, collapsed to a
                    `reasoning-preview-line-limit` peek + `+N more` row
                    (tool cards collapse to a headline — the peek cap
                    over-estimates those by a few rows, which is fine).
     * thinking   — ALWAYS behind the ▸ THINKING accordion: peek rows +
                    ~5 band-chrome rows; full height when expanded.
     * answer     — folded at min(60, fill-w): markdown chrome (headings,
                    fences, list gaps) makes /60 the safe historical
                    ballpark at any width ≥ 60, and narrower terminals
                    fold at their own width.
   User / plain-assistant text goes through the markdown walker, which
   word-wraps and inserts block chrome — fold at 3/4 width to stay above
   it."
  (^long [message ^long bubble-w] (estimated-height message bubble-w nil nil))
  (^long [message ^long bubble-w detail-expansions session-id]
   (let
     [content-w
      (max 1 (- bubble-w 4))

      ;; Word-wrap + markdown block chrome cost more rows than a pure
      ;; column fold; folding at 3/4 of the real width absorbs that.
      prose-w
      (max 1 (quot (* 3 content-w) 4))

      role
      (:role message)

      trace
      (:traces message)

      text
      (:text message)]

     (cond
       (= role :user)
       ;; label + top/bottom pad + gap (see bubble-height*) + 2 rows of
       ;; markdown block-gap slack — pasted JSON/log blobs grow a couple of
       ;; walker-inserted blank rows that per-line math can't see.
       (long (+ 6 (prose-rows-est text prose-w)))
       (and (= role :assistant) trace)
       (let
         [n-iter
          (long (count trace))

          ;; The painter folds code/results at `fill-w` (≈ bubble-w - 6);
          ;; estimate against a slightly narrower width so rounding lands
          ;; on the overshoot side.
          fold-w
          (max 1 (- bubble-w 8))

          expanded?
          (any-details-expanded? message detail-expansions session-id)

          ;; Collapsed-default caps: preview rows + the `+N more` row.
          peek
          (long ast/reasoning-preview-line-limit)

          ;; A collapse only fires when it hides ≥ reasoning-collapse-min-hidden
          ;; rows; below that the section renders in full. Cap at the
          ;; largest height a collapsed-or-inline section can paint.
          cap
          (+ peek (long ast/reasoning-collapse-min-hidden))

          section-rows
          (fn ^long [s]
            (let [full (prose-rows-est s fold-w)]
              (cond (zero? full) 0
                    expanded? (+ full 3)
                    :else (+ (min full cap) 3))))

          form-rows
          (long
            (reduce
              (fn [^long acc it]
                (+ acc
                   (long
                     (reduce
                       (fn [^long a f]
                         (let
                           [c
                            (:code f)

                            cr
                            (long (if (and (string? c) (not (str/blank? c)))
                                    (+ (wrapped-rows-est c fold-w) 2)
                                    0))

                            rr
                            #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
                            (long (section-rows (or (:result-render f) (:result f))))

                            cardr
                            (long (reduce (fn [^long x card]
                                            (+ x
                                               1
                                               #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
                                               (long (section-rows (:body card)))))
                                          0
                                          (:cards f)))

                            comr
                            (long (let [cm (:comment f)]
                                    (if (and (string? cm) (not (str/blank? cm)))
                                      (+ (wrapped-rows-est cm fold-w) 2)
                                      0)))]

                           (+ a
                              cr
                              rr
                              cardr
                              comr
                              ;; Error band: pads + caret rows + the headline
                              ;; WRAPPED at fill-w — a long provider/exception
                              ;; message paints multiple rows, a flat charge
                              ;; undershoots. PROVIDER errors additionally paint
                              ;; explanation + next-step + facts + two 600-char
                              ;; raw trims, all uncollapsed — bound those by the
                              ;; trim budget instead of reproducing perr here.
                              (long
                                (if-let [err (:error f)]
                                  (let
                                    [data (:data err)
                                     provider? (and (map? data)
                                                    (or (:status data)
                                                        (:body data)
                                                        (:request-id data)
                                                        (:request_id data)))]

                                    (+ 4
                                       (prose-rows-est (str (or (:message err) err)) fold-w)
                                       (if provider? (+ 12 (quot (+ 1200 (dec fold-w)) fold-w)) 0)))
                                  0)))))
                       0
                       (:forms it)))))
              0
              trace))

          ;; ▸ THINKING accordion: blank + band-top + header + band-gap
          ;; + peek/full + bottom edge ≈ content + 5 chrome rows. The LIVE
          ;; `:content-stream` merges into the same band while no form has
          ;; landed yet — size them together under the same collapse cap.
          think-rows
          (long (reduce (fn [^long acc it]
                          (+ acc
                             (long (let
                                     [live
                                      (when (empty? (:forms it)) (:content-stream it))

                                      full
                                      (+ (prose-rows-est (:thinking it) fold-w)
                                         (prose-rows-est live fold-w))]

                                     (cond (zero? full) 0
                                           expanded? (+ full 5)
                                           :else (+ (min full cap) 5))))))
                        0
                        trace))

          ;; Per-iteration `:assistant-prose` paints as its OWN full block
          ;; (never collapsed) between thinking and code+result.
          prose-rows
          (long (reduce (fn [^long acc it]
                          (+ acc
                             (long (let [r (prose-rows-est (:assistant-prose it) fold-w)]
                                     (if (pos? r) (+ r 2) 0)))))
                        0
                        trace))]

         (long (+ 5          ;; label + footer + note + gap
                  n-iter     ;; iteration headers
                  form-rows  ;; code + result (+ error) rows
                  think-rows ;; per-iteration reasoning + band
                  prose-rows ;; per-iteration assistant prose
                  (prose-rows-est text (max 1 (min 60 fold-w))))))
       (= role :assistant)
       ;; label + footer + gap chrome for a plain answer bubble.
       (long (+ 5 (prose-rows-est text prose-w)))
       :else (long (+ 6 (prose-rows-est text prose-w)))))))

(defn- estimated-height-with-turn-separator
  [_messages _settings bubble-w _idx message detail-expansions session-id]
  (estimated-height message (long bubble-w) detail-expansions session-id))

(defn- estimated-height-cached
  "Memoized `estimated-height-with-turn-separator` under the SAME key the
   sticky height cache uses. Callers must check the height cache first -
   a real measurement always outranks the memoized estimate. NOTE: >4 args,
   so no primitive hints (same Clojure cap `layout` documents)."
  [messages settings bubble-w idx message detail-expansions session-id]
  (render/with-cache estimate-cache
                     estimate-cache-gen
                     (height-key message bubble-w settings detail-expansions session-id)
                     (estimated-height-with-turn-separator messages
                                                           settings
                                                           bubble-w
                                                           idx
                                                           message
                                                           detail-expansions
                                                           session-id)))

(defn- with-turn-separator [message _messages _settings _idx] (dissoc message :turn-separator?))

;;; ── Cross-turn error squash ────────────────────────────────────────
;;
;; When a provider keeps emitting the same transport-level
;; failure (e.g. `:svar.core/stream-truncated`), every turn lands
;; an assistant message whose ONLY content is that error — the
;; user sees N identical "ERROR — Stream ended before terminal
;; marker." bubbles back-to-back. The per-iteration collapser in
;; `render/collapse-repeated-error-runs` cannot reach across
;; bubbles, so the spam survives.
;;
;; We fix it here: before layout estimates / projects anything,
;; walk the messages vec once and merge runs of consecutive
;; assistant messages that are made up entirely of identical
;; error iterations into one bubble. The merged bubble's trace
;; iterations are concatenated, so the existing per-iteration
;; collapser turns them into a single `ERROR x N` row when the
;; bubble renders.

(defn- error-only-iteration?
  "True when an iteration's only content is a transport-level provider
   error. Two flavours land here:

   1. iter-level `:error` map with no forms / thinking. Live
      progress emits this shape when the loop never even reached the
      first form.
   2. form-error-only iteration: a single zero-code placeholder form
      whose `:result-kind` is `:error` carrying the truncation /
      transport error map. Persisted DB rows for
      `:svar.core/stream-truncated` end up in this shape."
  [iter]
  (or (and (map? (:error iter)) (empty? (:forms iter)) (str/blank? (str (:thinking iter))))
      (and (not (map? (:error iter)))
           (str/blank? (str (:thinking iter)))
           (let [forms (:forms iter)]
             (and (= 1 (count forms))
                  (let [f (first forms)]
                    (and (str/blank? (str (:code f)))
                         (map? (:error f))
                         (= :error (:result-kind f)))))))))

(defn- error-only-assistant?
  [message]
  (and (= :assistant (:role message))
       (not= :cancelled (:status message))
       (let [iters (:traces message)]
         (and (sequential? iters) (seq iters) (every? error-only-iteration? iters)))))

(defn- message-error-signature
  [message]
  (when (error-only-assistant? message)
    ;; `render/error-signature` already handles both the iter-level
    ;; and form-error-only shapes, so one call covers both routes.
    (some-> message
            :traces
            first
            render/error-signature)))

(defn- squash-cross-message-errors
  "Collapse maximal runs of consecutive assistant messages whose
   only visible content is the same provider-error iteration into
   a single message. The merged message keeps the FIRST message's
   metadata (ids, timestamps) and concatenates the runs'
   iteration vectors so `format-answer-with-thinking` →
   `collapse-repeated-error-runs` will render one
   `ERROR x N` row at draw time.

   Non-error messages, cancellation notices, and runs with mixed
   signatures pass through untouched."
  [messages]
  (let
    [v
     (vec messages)

     n
     (long (count v))]

    (loop
      [acc
       (transient [])

       i
       (long 0)]

      (if (>= i n)
        (persistent! acc)
        (let
          [head
           (nth v i)

           sig
           (message-error-signature head)]

          (if (nil? sig)
            (recur (conj! acc head) (unchecked-inc i))
            (let
              [run-end (long (loop [j (unchecked-inc i)]
                               (if (and (< j n) (= sig (message-error-signature (nth v j))))
                                 (recur (unchecked-inc j))
                                 j)))]
              (if (= run-end (unchecked-inc i))
                (recur (conj! acc head) (unchecked-inc i))
                (let
                  [merged-iters (vec (mapcat :traces (subvec v i run-end)))
                   merged (-> head
                              (assoc :traces merged-iters)
                              (assoc ::squashed-run-count (unchecked-subtract run-end i)))]

                  (recur (conj! acc merged) run-end))))))))))

;;; ── Per-message projection ─────────────────────────────────────────────────

(defn- turn-identity
  "Stable per-turn key for detail-disclosure node ids. The progress
   bubble live-streams while `:session-turn-id` is still nil
   (the server only assigns it once the turn lands), so we MUST
   prefer `:client-turn-id` - that one exists from the moment the
   user submits, and gets carried through into the persisted
   assistant message. Picking session-turn-id here would
   change the disclosure node id at the live -> done flip; every
   open <details> / REASONING toggle the user expanded during
   streaming would silently snap back to its default state.

   Old sessions loaded from disk lack `:client-turn-id`
   entirely, so we fall back to `:session-turn-id` for them.
   Both still scope the click region by turn so a click on one
   answer can't toggle a disclosure in another."
  [message]
  (or (:client-turn-id message) (:session-turn-id message)))

(defn project-message
  "Apply the same `:text` projection `screen/apply-settings` used to
   apply, but for ONE message at a time so the virtual layer can call
   it lazily per-bubble. Hits the same caches `apply-settings` did.

   `:tail-lines` opt (when present, positive long) routes the IR
   walker through `layout/ast->lines-tail` so only the LAST tail-lines
   styled lines are produced - O(visible-tail) instead of O(body).
   Used by `layout` for the auto-scrolled tail-pinned bubble where
   the user only sees the bottom of the message. See A3 in
   autoresearch."
  ([message ^long bubble-w settings] (project-message message bubble-w settings nil))
  ([message ^long bubble-w settings
    {:keys [session-id detail-expansions tail-lines window-start window-num window-total-h]}]
   (let
     [show-timestamps?
      (boolean (get settings :show-timestamps false))

      strip-ts
      (fn [m]
        (cond-> m
          (not show-timestamps?)
          (dissoc :timestamp)))

      ;; Mid-window fast path parses the message's Markdown projection only
      ;; for the visible window; no renderer tree is stored on the message.
      windowed?
      (and window-start
           window-num
           (not (:traces message))
           (#{:assistant :user} (:role message))
           (not (str/blank? (:text message))))]

     (cond windowed?
           (let
             [ast
              (ast/markdown->ast (:text message))

              content-w
              (max 10 (- bubble-w 4))

              window-lines
              (layout/ast->lines-window ast content-w (long window-start) (long window-num))

              ;; Render through the entries adapter for parity with
              ;; the non-windowed path: produce sentinel-prefixed
              ;; strings the bubble painter expects. We bypass
              ;; format-answer-markdown-data's cache because window
              ;; opts shift each frame; not worth keying.
              entry-strs
              (layout/lines->sentinel-strings window-lines {:mode :answer})

              ;; The painter consumes `:prewrapped-lines` as a vec of
              ;; sentinel-strings (one per content row) for the
              ;; clip-lines-preserving-markers pass. Each entry is
              ;; just the string; line-meta starts as nil per row.
              prewrapped
              (mapv (fn [^String s]
                      s)
                    entry-strs)

              text-display
              (str/join "\n" entry-strs)]

             (-> message
                 (assoc :text text-display
                        :prewrapped-lines prewrapped
                        :line-meta (vec (repeat (count prewrapped) nil))
                        :lines-window {:start (long window-start)
                                       :total-h (long (or window-total-h
                                                          (+ (count prewrapped)
                                                             (long window-start))))})
                 strip-ts))
           (and (= :assistant (:role message)) (:traces message))
           (let
             [{:keys [text lines line-meta]} (render/format-answer-with-thinking-data
                                               (:text message)
                                               (:traces message)
                                               bubble-w
                                               settings
                                               (:confidence message)
                                               (= :cancelled (:status message))
                                               (cond->
                                                 {:session-id session-id
                                                  :session-turn-id (turn-identity message)
                                                  :detail-expansions detail-expansions}
                                                 tail-lines
                                                 (assoc :tail-lines tail-lines)))]
             (-> message
                 (assoc :text text
                        :prewrapped-lines lines
                        :line-meta line-meta)
                 strip-ts))
           (#{:assistant :user} (:role message)) (let
                                                   [ast
                                                    (ast/markdown->ast (or (:text message) ""))

                                                    {:keys [text lines line-meta]}
                                                    (render/format-answer-markdown-data
                                                      ast
                                                      bubble-w
                                                      (cond->
                                                        {:session-id session-id
                                                         :session-turn-id (turn-identity message)
                                                         :detail-expansions detail-expansions
                                                         :section (:role message)}
                                                        tail-lines
                                                        (assoc :tail-lines tail-lines)))]

                                                   (-> message
                                                       (assoc :text text
                                                              :prewrapped-lines lines
                                                              :line-meta line-meta)
                                                       strip-ts))
           :else (strip-ts message)))))

(defn- project-message-cached
  "Memoized `project-message` for STABLE (non-live) bubbles, keyed exactly
   like `height-cache-get`. Callers MUST NOT route the live / last bubble or
   a windowed (`:tail-lines`) slice through here - those change every tick."
  [message bubble-w settings detail-expansions session-id]
  (render/with-cache projection-cache
                     projection-cache-gen
                     (height-key message bubble-w settings detail-expansions session-id)
                     (project-message message
                                      bubble-w
                                      settings
                                      {:session-id session-id
                                       :detail-expansions detail-expansions})))

(defn- project-message-tail-cached
  "Memoized tail-walker projection for the bottom-locked LAST bubble. Keyed like
   `project-message-cached` plus `tail-n` (the tail height, which shifts with the
   viewport), so the full-body and tail projections never collide. Callers MUST
   route only a STABLE (non-live) last bubble here - the live streaming bubble is
   the loading bubble and never tail-walks."
  [message bubble-w settings detail-expansions session-id tail-n]
  (render/with-cache
    tail-projection-cache
    tail-projection-cache-gen
    (conj (height-key message bubble-w settings detail-expansions session-id) (long tail-n))
    (project-message
      message
      bubble-w
      settings
      {:session-id session-id :detail-expansions detail-expansions :tail-lines (long tail-n)})))

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
    (cond (< i n) (assoc v i x)
          (= i n) (conj v x)
          :else (recur (conj v pad) i x pad))))

(defn- cumulative-offsets
  "Running-sum offsets vector, one entry longer than `heights`:
   `[0 h0 h0+h1 ...]`. A primitive loop into a transient — replaces
   `(vec (reductions + 0 (map long heights)))`, whose lazy-seq +
   `map long` boxing was ~26% of a warm layout tick's allocation
   (15 KB -> ~2 KB for a 60-message session, byte-identical output)."
  [heights]
  (let [n (long (count heights))]
    (loop
      [i 0
       acc 0
       out (transient [0])]

      (if (< i n)
        (let [acc (+ acc (long (nth heights i)))]
          (recur (inc i) acc (conj! out acc)))
        (persistent! out)))))

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
   {:keys [progress loading? progress-extra] :or {progress nil loading? false}} &
   [{:keys [session-id detail-expansions prev-offsets]}]]
  (let
    [bubble-w
     (long bubble-w)

     inner-h
     (long inner-h)

     ;; Captured BEFORE the anchoring rebind of `scroll` below so
     ;; the return map can tell auto-bottom (nil, never persisted)
     ;; apart from a concrete offset (persisted back so the input
     ;; thread's next wheel math runs against the anchored value).
     scroll-given?
     (some? scroll)

     detail-expansions
     detail-expansions

     ;; Pre-pass: squash maximal runs of consecutive assistant
     ;; messages that are made up entirely of the same provider
     ;; error. The downstream iteration collapser then renders the
     ;; merged bubble as one `ERROR x N` row instead of N separate
     ;; identical bubbles.
     messages
     (squash-cross-message-errors messages)

     n
     (long (count messages))

     ;; Pass 1 ── sticky-real height if we've measured this
     ;; message before, otherwise the cheap estimate. Identity-
     ;; keyed cache means once a message has been visible (or
     ;; touched by `pre-warm!`) we keep its real height forever
     ;; - no more `total-h` jitter on scroll, no more
     ;; click-to-position landing in the wrong row.
     est
     ;; Primitive-indexed loop into a transient, not `(mapv f (range n)
     ;; messages)`: the two-collection mapv builds a chunked `(range n)`
     ;; seq and boxes each index every tick — ~14KB of pure iteration
     ;; garbage per 60-message layout that the transient loop avoids. This
     ;; runs on EVERY layout (live + scroll), so it scales with session
     ;; length. `nth` on the messages vector is O(1).
     (loop
       [i
        0

        acc
        (transient [])]

       (if (< i n)
         (let
           [m
            (nth messages i)

            h
            (or (height-cache-get m bubble-w settings detail-expansions session-id)
                (estimated-height-cached messages
                                         settings
                                         bubble-w
                                         i
                                         m
                                         detail-expansions
                                         session-id))]

           (recur (unchecked-inc i) (conj! acc h)))
         (persistent! acc)))

     est-off
     (cumulative-offsets est)

     est-tot
     (long (peek est-off))

     ;; ── Scroll anchoring ──────────────────────────────────────
     ;; `scroll` is an ABSOLUTE row offset. Off-screen heights are
     ;; estimates until a bubble is measured (visible) or warmed;
     ;; trace-bubble estimates systematically OVER-shoot, so as the
     ;; user scrolls UP into never-measured bubbles `total-h`
     ;; shrinks by a large amount and a fixed `scroll` suddenly
     ;; points at a different turn — the viewport (and scrollbar
     ;; thumb) lurch, worst near the top of a long reopened session
     ;; where the most unmeasured content gets corrected at once.
     ;;
     ;; Fix: pin the scroll to the message that was at the top of
     ;; the viewport last frame. Given the PREVIOUS frame's
     ;; cumulative `offsets`, find that anchor message and shift
     ;; `scroll` by however much the content ABOVE it changed height
     ;; between frames (`est-off[anchor] - prev-off[anchor]`). The
     ;; anchor message then stays visually put regardless of how the
     ;; off-screen estimate ↔ real corrections move `total-h`.
     ;;
     ;; nil scroll = auto-bottom: never anchored (always tracks the
     ;; latest message). Guarded on offsets-vec shape so an append
     ;; (n changed) skips anchoring and falls through to the raw
     ;; value rather than mis-indexing.
     anchored
     (when
       (and (some? scroll) (pos? n) (vector? prev-offsets) (= (long (count prev-offsets)) (inc n)))
       (let
         [s
          (long scroll)

          idx
          (long (loop [i 0]
                  (cond (>= i (dec n)) (dec n)
                        (> (long (nth prev-offsets (inc i))) s) i
                        :else (recur (inc i)))))

          delta
          (- (long (nth est-off idx)) (long (nth prev-offsets idx)))]

         (max 0 (+ s delta))))

     scroll
     (if (some? anchored) anchored scroll)

     scroll-1
     (long (or scroll (max 0 (- est-tot inner-h))))

     eff-1
     (long (min scroll-1 (max 0 (- est-tot inner-h))))

     ;; Pass 2 ── project + REAL height for candidates only.
     ;; Loading bubble gets live progress only if it is already visible.
     loading-last-idx
     (when (and loading? (pos? n) (= :assistant (:role (peek messages)))) (long (dec n)))

     ;; Per-message projection extracted so pass-2 AND the recovery
     ;; pass-3 (below) can reuse the same logic. `eff` is the
     ;; effective scroll currently in play; it only matters for the
     ;; bottom-locked tail-walker shortcut on the live bubble.
     project-idx!
     (fn [^long i ^long eff]
       (let
         [m
          (nth messages i)

          loading-bubble?
          (= i loading-last-idx)

          pm
          (if loading-bubble?
            (let
              [{:keys [text lines line-meta]} (render/progress->lines-data
                                                progress
                                                bubble-w
                                                settings
                                                (assoc progress-extra
                                                  :session-id session-id
                                                  ;; Live progress bubble must use the
                                                  ;; same turn key the completed answer
                                                  ;; will use a moment later; otherwise
                                                  ;; the REASONING / <details> toggle
                                                  ;; node-ids change at the live -> done
                                                  ;; flip and the user's expansion state
                                                  ;; resets to its default. See
                                                  ;; `turn-identity` for the precedence.
                                                  :session-turn-id (turn-identity m)
                                                  :detail-expansions detail-expansions))]
              (assoc m
                :text text
                :prewrapped-lines lines
                :line-meta line-meta))
            ;; Two projection paths for the LAST bubble:
            ;;   1. Bottom-locked (auto-scroll OR scroll clamped to
            ;;      max): tail-walker (A4/A6) — renders only the
            ;;      visible tail, cheap even for a huge body.
            ;;   2. Genuine mid-scroll into a tall body: the message is
            ;;      STABLE (the LIVE streaming bubble is peeled off
            ;;      above via `loading-bubble?`), so route it through
            ;;      the same projection cache every other stable bubble
            ;;      uses — otherwise each scroll frame re-parses the
            ;;      Markdown and re-wraps the whole body from scratch.
            (let
              [last?
               (= i (long (dec n)))

               max-scroll
               (max 0 (- est-tot inner-h))

               bottom-locked?
               (and last? (= eff max-scroll))

               tail-n
               (when bottom-locked? (long (* 2 inner-h)))]

              (if (and last? bottom-locked?)
                (project-message-tail-cached m
                                             bubble-w
                                             settings
                                             detail-expansions
                                             session-id
                                             tail-n)
                (project-message-cached m bubble-w settings detail-expansions session-id))))

          pm
          (with-turn-separator pm messages settings i)

          window-total-h
          (some-> pm
                  :lines-window
                  :total-h
                  long)

          real-h
          (long (or window-total-h (render/bubble-height pm bubble-w)))]

         ;; Pin the real height in the sticky cache for the raw
         ;; message identity. Skip live progress and windowed
         ;; slices: neither is a full stable bubble measurement.
         (when (and (not loading-bubble?) (nil? window-total-h))
           (height-cache-put! m bubble-w settings detail-expansions session-id real-h))
         {:idx i :projected pm :height real-h}))

     result
     (if (nil? scroll)
       ;; ── Auto-bottom: stable real-tail back-walk ──────────────────
       ;; Pin the visible tail from REAL heights, walking UP from the
       ;; last message until `inner-h` rows are covered (plus the one
       ;; partially-visible message that crosses the top edge). The
       ;; painted `:top`s are differences of real tail heights — the
       ;; estimate terms for everything above cancel out — so the tail
       ;; sits rock-still frame-to-frame even while a fast-growing live
       ;; bubble's cheap estimate undershoots by 10–100×. That kills the
       ;; eff lurch (the old eff-1→eff-2 two-phase correction) that made
       ;; the view flicker right as a running op flips to success/error.
       ;; `total-h'` still folds in estimates for the off-screen messages
       ;; ABOVE — but those only drive the scrollbar thumb, never the
       ;; pinned tail.
       (let
         [tail
          (loop
            [i
             (dec n)

             acc-h
             0

             acc
             []]

            (if (neg? i)
              acc
              (let
                [pj
                 (project-idx! i eff-1)

                 acc'
                 (cons pj acc)

                 acc-h'
                 (+ acc-h (long (:height pj)))]

                (if (>= acc-h' inner-h) acc' (recur (dec i) acc-h' acc')))))

          heights'
          (reduce (fn [hs {:keys [^long idx ^long height]}]
                    (assoc-vec hs idx height 0))
                  est
                  tail)

          offsets'
          (cumulative-offsets heights')

          total-h'
          (long (peek offsets'))

          eff
          (long (max 0 (- total-h' inner-h)))

          visible-set
          (mapv
            (fn [{:keys [^long idx ^long height projected]}]
              {:idx idx :height height :projected projected :top (- (long (nth offsets' idx)) eff)})
            tail)]

         {:total-h total-h'
          :eff-scroll eff
          :heights heights'
          :offsets offsets'
          :visible visible-set})
       ;; ── Explicit scroll: estimate-anchored multi-pass ────────────
       (let
         [;; Pass 1b ── candidate visible idxs from estimates. ONLY the
          ;; explicit-scroll path needs them; auto-bottom uses the real-
          ;; tail back-walk and discarded these, so computing them in the
          ;; outer let made every live/auto-bottom tick pay an O(n)
          ;; `filterv`. Scoped here → skipped on the hot streaming path.
          cand-idxs
          (filterv (fn [^long i]
                     (let
                       [top
                        (- (long (nth est-off i)) eff-1)

                        h
                        (long (nth est i))]

                       (visible? top h inner-h)))
            (range n))

          projected
          (mapv #(project-idx! % eff-1) cand-idxs)

          ;; Refine heights vec with real measurements.
          heights'
          (reduce (fn [hs {:keys [^long idx ^long height]}]
                    (assoc-vec hs idx height 0))
                  est
                  projected)

          offsets'
          (cumulative-offsets heights')

          total-h'
          (long (peek offsets'))

          ;; Re-clamp against the REAL `total-h'`, not the pass-1
          ;; `eff-1`. When estimates undershoot the real total,
          ;; max-scroll grows in pass-2 - piping `eff-1` through `min`
          ;; here would pin the user above the true bottom.
          eff-2
          (long (max 0 (min (long scroll) (max 0 (- total-h' inner-h)))))

          ;; Pass 3 ── recovery for messages visible at the refined
          ;; `eff-2` but missed at the cheap `eff-1`. A stable bubble
          ;; whose interval intersects the viewport at `eff-2` but not
          ;; `eff-1` would otherwise never project → blink out.
          projected-by-idx
          (into {} (map (juxt :idx identity) projected))

          missing-idxs
          (vec (for
                 [^long i
                  (range n)

                  :let [top
                        (- (long (nth offsets' i)) eff-2)

                        height
                        (long (nth heights' i))]
                  :when (and (visible? top height inner-h) (not (contains? projected-by-idx i)))]

                 i))

          extra-projected
          (mapv #(project-idx! % eff-2) missing-idxs)

          projected-all
          (into projected extra-projected)

          heights''
          (reduce (fn [hs {:keys [^long idx ^long height]}]
                    (assoc-vec hs idx height 0))
                  heights'
                  extra-projected)

          offsets''
          (if (seq extra-projected) (cumulative-offsets heights'') offsets')

          total-h''
          (long (peek offsets''))

          eff-3
          (long (max 0 (min (long scroll) (max 0 (- total-h'' inner-h)))))

          visible-set
          (mapv (fn [{:keys [^long idx ^long height projected]}]
                  {:idx idx
                   :height height
                   :projected projected
                   :top (- (long (nth offsets'' idx)) eff-3)})
                projected-all)]

         {:total-h total-h''
          :eff-scroll eff-3
          :heights heights''
          :offsets offsets''
          :visible visible-set}))]

    {:total-h (:total-h result)
     :eff-scroll (:eff-scroll result)
     :heights (:heights result)
     :offsets (:offsets result)
     :visible (:visible result)
     ;; Clamped, anchor-corrected absolute scroll the caller should
     ;; write back into app-db (`:set-scroll`) so the input thread and
     ;; the next layout agree. nil for auto-bottom — never persist that,
     ;; or auto-bottom stickiness breaks.
     :anchored-scroll (when scroll-given? (:eff-scroll result))}))

;;; ── Background pre-warmer ────────────────────────────────────────────────────
;;
;; Why this exists, top of mind: with virtualisation alone, scrolling
;; UP through a long session pays the FULL
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
;;   * Returns the `Thread`. Caller stores it; on session
;;     switch / shutdown, call `stop-pre-warm!` to interrupt.

(defn- warm-message-height!
  [messages idx bubble-w settings session-id detail-expansions]
  ;; Warm EVERY message, not just assistants. User-message
  ;; bubble-height is cheap, but the *real* value is
  ;; `chrome+lines+1` while `estimated-height` can undershoot
  ;; by 1 for short prompts. Skipping users means total-h drifts the
  ;; first time they scroll into view.
  (let
    [m
     (nth messages idx)

     pm
     (project-message m
                      bubble-w
                      settings
                      {:session-id session-id :detail-expansions detail-expansions})

     pm
     (with-turn-separator pm messages settings idx)

     h
     (long (render/bubble-height pm bubble-w))]

    (height-cache-put! m bubble-w settings detail-expansions session-id h)
    h))

(defn pre-warm-recent!
  "Synchronously warm the RECENT tail of a session before the
   background worker kicks in.

   Why: `pre-warm!` is async by design (fast startup), but if the
   user wheel-scrolls immediately after opening a heavy session,
   they can still hit a cold big-trace bubble before the daemon gets
   there. Warming the newest tail on the caller thread eliminates that
   first-scroll cliff while still keeping the full-history warm async.

   Options:
   - `:count`     max number of newest messages to warm (default 16)
   - `:budget-ms` wall-clock budget for this sync pass (default 120)

   Returns the number of warmed messages. Safe on empty input."
  ([messages bubble-w settings] (pre-warm-recent! messages bubble-w settings nil))
  ([messages bubble-w settings {:keys [session-id detail-expansions] :as opts}]
   (let
     [tail-count*
      (long (or (:count opts) 16))

      tail-count
      (if (neg? tail-count*) 0 tail-count*)

      budget-ms
      (long (if (contains? opts :budget-ms) (or (:budget-ms opts) 0) 120))

      n
      (long (count messages))

      bubble-w
      (long bubble-w)

      start-idx
      (long (max 0 (- n tail-count)))

      deadline-ns
      (when-not (neg? budget-ms) (+ (System/nanoTime) (* 1000000 budget-ms)))]

     (loop
       [i
        (dec n)

        warmed
        0]

       (if (or (< i start-idx) (and (some? deadline-ns) (>= (System/nanoTime) (long deadline-ns))))
         warmed
         (do (warm-message-height! messages i bubble-w settings session-id detail-expansions)
             (recur (dec i) (inc warmed))))))))

(defn pre-warm!
  "Spawn a daemon worker thread that calls `project-message` and
   `bubble-height` for every message in `messages` so the LRU is
   hot by the time the user scrolls. Walks in REVERSE order - the
   user almost always scrolls UP from the bottom, so the next-to-
   last message warms first, the FIRST message last.

   Returns the `Thread` so the caller can stop it on session
   switch / shutdown via `stop-pre-warm!`. Returns `nil` for an
   empty session (nothing to warm).

   Settings parity with `layout`: pass the SAME settings map so the
   cached entries match the keys subsequent layout passes will
   look up. Mismatched settings = wasted compute (the layout pass
   will format again with different cache keys).

   `:on-warm` (0-arg fn, optional) fires on the worker thread as
   heights land in the sticky cache — periodically (every
   `warm-bump-every` bubbles) AND once when the whole session is
   warmed. Callers wire this to a render-version bump so `total-h`
   SETTLES to its fully-measured value while the user is still idle
   at auto-bottom (where the thumb is pinned to the bottom, so the
   settle is invisible) instead of snapping in one ~20% step on the
   first wheel-up. Without it the background warm populates the
   cache silently and the correction is deferred — and applied all
   at once — to the next render the user happens to trigger by
   scrolling. See the `:caveat` note at the top of this ns."
  ^Thread [messages bubble-w settings & [{:keys [session-id detail-expansions on-warm]}]]
  (let [n (long (count messages))]
    (when (pos? n)
      (let
        [bubble-w (long bubble-w)
         warm-bump-every 8
         on-warm (when (fn? on-warm) on-warm)
         fire-warm! (fn []
                      (when on-warm (try (on-warm) (catch Throwable _ nil))))
         t
         (Thread.
           ^Runnable
           (fn []
             (try
               (loop
                 [i (dec n)
                  since-bump 0]

                 (when (and (>= i 0) (not (.isInterrupted (Thread/currentThread))))
                   (warm-message-height! messages i bubble-w settings session-id detail-expansions)
                   (if (>= (inc since-bump) warm-bump-every)
                     (do (fire-warm!) (recur (dec i) 0))
                     (recur (dec i) (inc since-bump)))))
               ;; Final settle: ensure the LAST batch (< warm-bump-every
               ;; bubbles) also triggers one re-layout so total-h reaches
               ;; its terminal value. Skipped on interrupt below.
               (fire-warm!)
               (catch InterruptedException _
                 ;; Cooperative cancellation - nothing to do.
                 nil)
               (catch Throwable t
                 ;; Pre-warming must NEVER bring the TUI down.
                 ;; Swallow + lose silently; the next layout
                 ;; pass will pay the un-warmed cost itself.
                 (try (require 'taoensso.telemere)
                      ((resolve 'taoensso.telemere/log!)
                        :warn
                        (str "vis-channel-tui pre-warm threw: " (or (ex-message t) (str t))))
                      (catch Throwable _ nil)))))
           "vis-channel-tui-prewarm")]

        (.setDaemon t true)
        (.setPriority t (max Thread/MIN_PRIORITY (dec Thread/NORM_PRIORITY)))
        (.start t)
        t))))

(defn stop-pre-warm!
  "Interrupt a pre-warm thread previously returned by `pre-warm!` and
   wait briefly for it to acknowledge the interrupt. Safe on `nil` and
   on already-finished threads.

   Why join instead of fire-and-forget: the daemon checks
   `(.isInterrupted …)` BETWEEN `warm-message-height!` calls, not
   inside one. A fire-and-forget interrupt lets the daemon finish its
   in-flight bubble and write one more cache entry AFTER this returns,
   which (a) flakes the next test that just called `invalidate-heights!`
   and (b) in production silently leaks one cache entry for the
   previous session across a session switch. Joining with a tight
   budget bounds both: a typical interrupt lands within microseconds;
   the 200 ms ceiling covers the worst-case bubble already mid-format.

   Optional second arg overrides the join budget (ms). Pass 0 for the
   old fire-and-forget behavior."
  ([^Thread t] (stop-pre-warm! t 200))
  ([^Thread t ^long join-ms]
   (when (and t (.isAlive t))
     (.interrupt t)
     (when (pos? join-ms)
       (try (.join t join-ms)
            (catch InterruptedException _ (.interrupt (Thread/currentThread))))))))

;;; ── Managed re-warm (single owner for the background warm thread) ──────────
;;
;; The startup path warms once, but three runtime events resurrect
;; estimates and used to leave them cold until the user scrolled into
;; them — which is exactly when a correction moves `total-h` and jumps
;; the scrollbar thumb:
;;   1. registry-toggle flips (`:resync-toggle-settings` drops the WHOLE
;;      height cache),
;;   2. terminal resizes (`bubble-w` is part of the cache key, so every
;;      entry misses at the new width),
;;   3. session/tab switches into a not-yet-warmed history.
;; `rewarm!` gives those paths one call that stops any in-flight warm
;; worker and starts a fresh one, so the cache re-settles while the user
;; is still idle instead of correcting mid-scroll.

(defonce ^:private rewarm-thread* (atom nil))

(defn rewarm!
  "Stop the in-flight background warm (if any) and start warming
   `messages` at `bubble-w`. Owns ONE worker thread process-wide - callers
   never juggle the `Thread` themselves. `opts` is passed to `pre-warm!`
   verbatim (`:session-id`, `:detail-expansions`, `:on-warm`). Returns the
   new worker `Thread`, or nil for an empty history.

   The previous worker is interrupted fire-and-forget (join 0): callers
   sit on the input/event thread, and a stale worker finishing one extra
   bubble writes a VALID cache entry - unlike the invalidate-then-stop
   test flow `stop-pre-warm!`'s docstring worries about."
  ^Thread [messages bubble-w settings opts]
  (let [[^Thread old _] (reset-vals! rewarm-thread* nil)]
    (when old (stop-pre-warm! old 0)))
  (when (seq messages)
    (let [t (pre-warm! messages bubble-w settings opts)]
      (reset! rewarm-thread* t)
      t)))

(defn stop-rewarm!
  "Interrupt + briefly join the managed re-warm worker (shutdown / session
   close). Safe when none is running."
  []
  (let [[^Thread old _] (reset-vals! rewarm-thread* nil)]
    (when old (stop-pre-warm! old))))
