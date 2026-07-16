(ns com.blockether.vis.ext.channel-tui.cinema
  "Headless session → MP4 screencast. Replays a persisted session's transcript
   through the REAL TUI render pipeline (`screen/render-frame!` painting into a
   Lanterna `VirtualTerminal` — no TTY needed), and encodes the captured frames
   to `.mp4`:

     pure-JVM H.264 via jcodec's `AWTSequenceEncoder` (each frame rendered to a
     `BufferedImage` with Java2D). Pixel-exact because WE draw the glyphs — no
     external player font drift. No native deps, GraalVM-friendly.

   The replay is HUMANIZED rather than a flat scroll-through: every disclosure
   COLLAPSED, the session is re-enacted turn by turn as if it were happening
   live — a user turn is TYPED IN character by character; before each answer a
   WORK beat holds the live `Vis is calling the provider…` spinner in the
   assistant's place; then the finished answer JUMPS IN and SLOWLY scrolls into
   view. See `session->frames`.

   The heavy TUI stack (`screen`, Lanterna) is reached via `requiring-resolve`
   so merely loading this namespace stays cheap."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [com.blockether.vis.ext.channel-tui.theme :as tui-theme]
            [com.blockether.vis.internal.theme :as theme])
  (:import [com.googlecode.lanterna TerminalSize SGR]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]
           [java.awt Color Font RenderingHints]
           [java.awt.image BufferedImage]
           [java.io File]))

;; ── Tunables ────────────────────────────────────────────────────────────────

(def ^:private defaults
  {:cols 120
   :rows 42
   ;; Frame rate. Pacing is by FRAME COUNT (the encoder ignores per-frame
   ;; delays), so every "beat" below is spent as a whole number of frames at
   ;; this rate.
   :fps 12
   ;; ── Humanized reveal knobs ──────────────────────────────────────────────
   ;; USER turns are TYPED IN: this many characters land per second (a brisk
   ;; human at the keyboard), floored so even at a low fps a few glyphs appear
   ;; per frame.
   :type-cps 90
   :type-min-cpf 3
   ;; A runaway paste can't type forever — cap the typing frames per message
   ;; (chars-per-frame is widened to fit the whole text into this budget).
   :max-type-frames 90
   ;; Beat after a message finishes typing, before the turn "sends".
   :post-user-ms 300
   ;; WORK beat before each ASSISTANT answer: the LIVE progress bubble holds in
   ;; the assistant's place — "⠿ Vis is calling the provider… 1.2s / Esc to
   ;; cancel", spinner animating and elapsed clock ticking — so it reads as real
   ;; work being done, not a replay.
   :work-ms 1000
   ;; The finished answer then JUMPS IN whole (a chat bubble popping in near the
   ;; bottom of the viewport); this hold lets it register before the scroll.
   :jump-hold-ms 350
   ;; Progressive reveal: the trace/tool blocks stream IN at most this many
   ;; chunks per assistant turn (a huge 40-tool turn is grouped so the clip
   ;; stays sane). `pop-hold` is the brief beat each chunk holds as it lands
   ;; (before it scrolls into view); `trace-hold` the settle beat after.
   :trace-hold-ms 130
   :pop-hold-ms 160
   :max-reveal-steps 4
   ;; Then a SLOW downward scroll rides the answer into view: rows advanced per
   ;; frame (smaller = slower/smoother), capped per message so a giant answer
   ;; can't produce a thousand-frame crawl.
   :scroll-rows-per-frame 1.8
   :max-scroll-frames 60
   ;; Beat to read the settled answer before the next turn begins.
   :post-assist-ms 450
   ;; Hold the final frame briefly so the ending doesn't cut abruptly.
   :end-hold-ms 1400
   ;; Hard ceiling on frame count so a pathological session can't blow up the
   ;; encoder / output size.
   :max-frames 3000
   ;; MP4 glyph cell size in pixels (width is derived from the mono font).
   :font-size 18
   :theme theme/default-theme-id})

;; ── Headless capture ────────────────────────────────────────────────────────

(defn- render-frame-fn
  []
  (or (requiring-resolve 'com.blockether.vis.ext.channel-tui.screen/render-frame!)
      (throw (ex-info "cinema: screen/render-frame! did not resolve" {}))))

(defn- rebuild-history-fn
  []
  (or (requiring-resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)
      (throw (ex-info "cinema: chat/rebuild-history did not resolve" {}))))

(defn- markdown->ir-fn
  []
  (or (requiring-resolve 'com.blockether.vis.core/markdown->ir)
      (throw (ex-info "cinema: core/markdown->ir did not resolve" {}))))

(defn- empty-ir-fn
  []
  (or (some-> (requiring-resolve 'com.blockether.vis.ext.channel-tui.chat/empty-ir)
              deref)
      [:ir]))

(defn- gateway-soul-fn [] (requiring-resolve 'com.blockether.vis.core/gateway-soul))

(defn- session-workspace-fn
  []
  (requiring-resolve 'com.blockether.vis.core/gateway-session-workspace))

(defn- rgb [^java.awt.Color color] [(.getRed color) (.getGreen color) (.getBlue color)])

(defn- capture-grid
  "Snapshot the Lanterna back-buffer as a rows×cols vector of cell maps carrying
   glyph + fg/bg RGB + SGR modifiers — the full-fidelity view."
  [^TerminalScreen scr cols rows]
  (vec (for [y (range rows)]
         (vec (for [x (range cols)]
                (let [tc (.getBackCharacter scr (int x) (int y))]
                  (if tc
                    (let [mods (.getModifiers tc)]
                      {:ch (or (.getCharacterString tc) " ")
                       :fg (rgb (.getForegroundColor tc))
                       :bg (rgb (.getBackgroundColor tc))
                       :bold (boolean (.contains mods SGR/BOLD))
                       :italic (boolean (.contains mods SGR/ITALIC))
                       :underline (boolean (.contains mods SGR/UNDERLINE))
                       :reverse (boolean (.contains mods SGR/REVERSE))})
                    {:ch " " :fg [0 0 0] :bg [0 0 0]})))))))

(defn session->frames
  "Replay `session-id` headless and return
   `{:cols :rows :frames [{:grid :delay-ms} …] :video-ms}`.

   `opts` overrides `defaults`. The replay is HUMANIZED: rather than dumping the
   whole transcript and scrolling through it, it RE-ENACTS the session as if it
   were happening live, one message at a time, with every disclosure COLLAPSED
   (the natural resting view a live session shows, not an expanded dump):

     • a USER turn is TYPED IN character by character — the bubble grows as the
       text lands, like a human at the keyboard — then a short beat;
     • before each ASSISTANT answer there is a WORK beat: the LIVE progress
       bubble holds in the assistant's place (`⠿ Vis is calling the provider…
       1.2s / Esc to cancel`, spinner animating, elapsed clock ticking) so it
       reads as real work being done;
     • the finished answer then JUMPS IN whole (a bubble popping in near the
       bottom of the viewport), holds a beat, then SLOWLY scrolls into view;
     • a beat to read it, and on to the next turn.

   Pacing is by FRAME COUNT at a constant `fps` (the encoder ignores per-frame
   delays): every 'beat' is that many repeated frames, and the whole clip
   length scales with the session's content. `scroll/follow` carries no `:pos`,
   so each captured frame SNAPS to its target — deterministic, no stray ease."
  [session-id opts]
  (let [{:keys [cols rows fps type-cps type-min-cpf max-type-frames post-user-ms work-ms
                jump-hold-ms scroll-rows-per-frame max-scroll-frames post-assist-ms end-hold-ms
                max-frames trace-hold-ms max-reveal-steps pop-hold-ms]}
        (merge defaults opts)

        history
        (vec ((rebuild-history-fn) session-id))

        n
        (count history)

        render!
        (render-frame-fn)

        md->ir
        (markdown->ir-fn)

        empty-ir
        (empty-ir-fn)

        vt
        (DefaultVirtualTerminal. (TerminalSize. (int cols) (int rows)))

        scr
        (doto (TerminalScreen. vt) (.startScreen))

        base
        (do (state/init!) @state/app-db)

        ;; Session title + pinned workspace so the header tab shows the REAL
        ;; title (not the "Untitled session" placeholder) and the footer shows
        ;; the session's git context — matching a live open of this session
        ;; rather than a bare replay. Both resolve lazily and tolerate a
        ;; missing soul/workspace (nil → header/footer fall back gracefully).
        soul
        (try ((gateway-soul-fn) session-id) (catch Throwable _ nil))

        session-title
        (some-> soul
                :title
                str
                not-empty)

        session-ws
        (try ((session-workspace-fn) session-id) (catch Throwable _ nil))

        db0
        (assoc base
          :session {:id session-id}
          :title session-title
          :workspace session-ws
          :workspace/root (:root session-ws)
          ;; Headless replay: no live provider-limits polling, so tell the
          ;; footer to keep the "limits:" row clean.
          :cinema? true
          ;; Collapsed resting view — disclosures closed, exactly like a
          ;; fresh live session rather than a full expanded dump.
          :detail-expansions {:vis.channel-tui/baseline :collapse})

        frame-dt
        (max 1 (long (/ 1000 (long fps))))

        ms->frames
        (fn [ms]
          (max 1 (long (Math/ceil (/ (double ms) (double frame-dt))))))

        ;; Render a message vector at a scroll intent, with optional db `extra`
        ;; overrides (e.g. the live-progress "working" bubble) and an explicit
        ;; `now-ms` that drives the spinner glyph + elapsed clock. Optionally
        ;; feeds a stable layout so parked offsets aren't re-clamped. → {:grid :ly}.
        render-state
        (fn [msgs scroll layout extra now-ms]
          (let [db
                (cond-> (merge (assoc db0
                                 :messages (vec msgs)
                                 :scroll scroll)
                               extra)
                  layout
                  (assoc :layout layout))

                ly
                (render! scr cols rows db (long now-ms))]

            (.refresh scr)
            {:grid (capture-grid scr cols rows) :ly ly}))

        ;; A user-typed prefix as a live message: truncated raw text + IR
        ;; re-derived so the bubble grows exactly as typed.
        typed-msg
        (fn [msg s]
          (assoc msg
            :text s
            :ir (md->ir s {:soft-break :hard})))

        ;; A placeholder assistant bubble so `loading?` renders the live
        ;; "Vis is calling the provider…" spinner in its place.
        placeholder-assistant
        {:role :assistant :text "" :ir nil}

        ;; Append `grid` `k` times to the transient frame accumulator.
        add
        (fn [acc grid k]
          (loop [a
                 acc

                 k
                 (long k)]

            (if (pos? k) (recur (conj! a {:grid grid :delay-ms frame-dt}) (dec k)) a)))

        frames0
        (loop [i
               0

               acc
               (transient [])]

          (if (< i n)
            (let [msg
                  (nth history i)

                  prior
                  (subvec history 0 i)

                  full
                  (subvec history 0 (inc i))

                  role
                  (:role msg)]

              (cond
                (= :user role)
                (let [text
                      (or (:text msg) "")

                      len
                      (count text)

                      ;; Chars revealed per frame — widened so a huge paste
                      ;; still fits inside `max-type-frames`.
                      cpf
                      (max (long type-min-cpf)
                           (long (Math/round (/ (double type-cps) (double fps))))
                           (long (Math/ceil (/ (double len) (double max-type-frames)))))

                      steps
                      (if (pos? len) (concat (range cpf len cpf) [len]) [0])

                      acc*
                      (reduce (fn [a l]
                                (let [{g :grid}
                                      (render-state
                                        (conj prior
                                              (typed-msg msg (subs text 0 (min len (long l)))))
                                        scroll/follow
                                        nil
                                        nil
                                        0)]
                                  (add a g 1)))
                              acc
                              steps)

                      {gf :grid}
                      (render-state full scroll/follow nil nil 0)]

                  (recur (inc i) (add acc* gf (ms->frames post-user-ms))))
                (= :assistant role)
                (let [;; WORK beat: the user bubble + a live "calling the
                      ;; provider" spinner in the assistant's place, elapsed
                      ;; clock ticking (synthetic now-ms advances per frame).
                      work-frames
                      (ms->frames work-ms)

                      acc1
                      (reduce (fn [a k]
                                (let [{g :grid} (render-state (conj prior placeholder-assistant)
                                                              scroll/follow
                                                              nil
                                                              {:loading? true
                                                               :turn-start-ms 0
                                                               :progress {:iterations []}}
                                                              (* (long k) frame-dt))]
                                  (add a g 1)))
                              acc
                              (range work-frames))

                      ;; ── Progressive reveal ─────────────────────────────
                      ;; Rather than dumping the whole answer and flying the
                      ;; viewport past every tool block at once, the trace
                      ;; blocks stream IN a few at a time (answer elided) —
                      ;; each new chunk pops in below and the view scrolls
                      ;; down to follow, exactly like a live session where
                      ;; tool results arrive one after another — then the
                      ;; finished answer text reveals last.
                      traces
                      (vec (:traces msg))

                      tcount
                      (count traces)

                      chunk
                      (max 1
                           (long (Math/ceil (/ (double tcount)
                                               (double (max 1 (long max-reveal-steps)))))))

                      ;; Trace counts revealed at each stage: chunk, 2·chunk … all.
                      stage-counts
                      (if (pos? tcount)
                        (distinct (conj (vec (range chunk tcount chunk)) tcount))
                        [])

                      ;; Park at `cur` (the previous bottom), let the new
                      ;; content pop in, hold a beat, then slow-scroll down to
                      ;; this stage's settled bottom. → [acc' new-bottom].
                      reveal
                      (fn [a msgs cur pop-frames hold-frames]
                        (let [{gf :grid ly :ly}
                              (render-state msgs scroll/follow nil nil 0)

                              total-h
                              (long (or (:total-h ly) 0))

                              inner-h
                              (long (or (:inner-h ly) 0))

                              max-s
                              (max 0 (- total-h inner-h))

                              start
                              (long (max 0 (min max-s (long cur))))]

                          (if (<= max-s start)
                            [(add a gf hold-frames) max-s]
                            (let [{gp :grid}
                                  (render-state msgs (scroll/parked start) ly nil 0)

                                  a1
                                  (add a gp pop-frames)

                                  dist
                                  (- max-s start)

                                  nsteps
                                  (max 1
                                       (min (long max-scroll-frames)
                                            (long (Math/ceil (/ (double dist)
                                                                (double scroll-rows-per-frame))))))

                                  a2
                                  (reduce (fn [aa k]
                                            (let [frac
                                                  (/ (double k) (double nsteps))

                                                  st
                                                  (long (max 0
                                                             (min max-s
                                                                  (Math/round
                                                                    (+ (double start)
                                                                       (* frac (double dist)))))))

                                                  {g :grid}
                                                  (render-state msgs (scroll/parked st) ly nil 0)]

                                              (add aa g 1)))
                                          a1
                                          (range 1 (inc nsteps)))]

                              [(add a2 gf hold-frames) max-s]))))

                      ;; Initial scroll position: the spinner view's bottom,
                      ;; so the first chunk continues smoothly from there.
                      {sly :ly}
                      (render-state (conj prior placeholder-assistant) scroll/follow nil nil 0)

                      cur0
                      (max 0 (- (long (or (:total-h sly) 0)) (long (or (:inner-h sly) 0))))

                      ;; Stream the trace chunks in (answer elided) …
                      [acc2 cur1]
                      (reduce (fn [[a cur] c]
                                (reveal a
                                        (conj prior
                                              (assoc msg
                                                :ir empty-ir
                                                :traces (subvec traces 0 c)))
                                        cur
                                        (ms->frames pop-hold-ms)
                                        (ms->frames trace-hold-ms)))
                              [acc1 cur0]
                              stage-counts)

                      ;; … then the finished answer text lands and settles.
                      [acc3 _]
                      (reveal acc2
                              full
                              cur1
                              (ms->frames jump-hold-ms)
                              (+ (long (ms->frames jump-hold-ms))
                                 (long (ms->frames post-assist-ms))))]

                  (recur (inc i) acc3))
                :else (let [{g :grid} (render-state full scroll/follow nil nil 0)]
                        (recur (inc i) (add acc g (ms->frames post-user-ms))))))
            (persistent! acc)))

        ;; Empty session: a single blank frame so the encoder still emits a clip.
        frames0
        (if (seq frames0)
          frames0
          [(assoc (render-state [] scroll/follow nil nil 0) :delay-ms frame-dt)])

        ;; Tail hold so the ending doesn't cut abruptly.
        held
        (into (vec frames0) (repeat (dec (long (ms->frames end-hold-ms))) (peek frames0)))

        ;; Hard frame cap — downsample preserving even spacing.
        frames
        (if (> (count held) (long max-frames))
          (let [step (/ (double (count held)) (double max-frames))]
            (mapv #(nth held (min (dec (count held)) (long (* (long %) step)))) (range max-frames)))
          held)

        video-ms
        (* (count frames) frame-dt)]

    (.stopScreen scr)
    {:cols cols :rows rows :frames frames :video-ms video-ms}))

;; ── MP4 (jcodec) emitter ────────────────────────────────────────────────────

(defn- awt-color ^Color [[r g b]] (Color. (int r) (int g) (int b)))

(defn- even2 ^long [^long v] (if (odd? v) (inc v) v))

(defn- io-file ^File [x] (if (instance? File x) x (File. (str x))))

(defn- grid->image
  "Render one captured grid to a BufferedImage with Java2D. `cw`/`ch` are cell
   pixel dimensions, `font`/`bold-font` the mono glyph fonts, `ascent` the
   baseline offset."
  ^BufferedImage [grid cols rows cw ch font bold-font ascent]
  (let [cols
        (long cols)

        rows
        (long rows)

        cw
        (long cw)

        ch
        (long ch)

        ascent
        (long ascent)

        w
        (even2 (* cols cw))

        h
        (even2 (* rows ch))

        img
        (BufferedImage. w h BufferedImage/TYPE_INT_RGB)

        g
        (.createGraphics img)]

    (.setRenderingHint g
                       RenderingHints/KEY_TEXT_ANTIALIASING
                       RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
    (doseq [[y row]
            (map-indexed vector grid)

            [x c]
            (map-indexed vector row)]

      (let [px
            (* (long x) cw)

            py
            (* (long y) ch)]

        (.setColor g (awt-color (:bg c)))
        (.fillRect g px py cw ch)
        (let [ch* (:ch c)]
          (when (and ch* (pos? (count (str/trim ch*))))
            (.setFont g (if (:bold c) bold-font font))
            (.setColor g (awt-color (:fg c)))
            (.drawString g ^String ch* (int px) (int (+ py ascent)))))))
    (.dispose g)
    img))

(defn- mono-fonts
  [^long size]
  (let [base (Font. Font/MONOSPACED Font/PLAIN (int size))]
    {:font base :bold (.deriveFont base Font/BOLD)}))

(defn frames->mp4!
  "Encode a `session->frames` result to `out` (an MP4 File/path) via jcodec's
   pure-Java `AWTSequenceEncoder`. Returns the output File."
  [{:keys [cols rows frames]} out {:keys [font-size fps] :or {font-size 18 fps 8}}]
  (let [out-file
        (io-file out)

        {:keys [font bold]}
        (mono-fonts font-size)

        ;; Measure the mono cell from a throwaway image's FontMetrics.
        probe
        (.createGraphics (BufferedImage. 8 8 BufferedImage/TYPE_INT_RGB))

        _
        (.setFont probe font)

        fm
        (.getFontMetrics probe)

        cw
        (max 1 (.charWidth fm \M))

        ch
        (max 1 (.getHeight fm))

        ascent
        (.getAscent fm)

        _
        (.dispose probe)

        enc
        (org.jcodec.api.awt.AWTSequenceEncoder/createSequenceEncoder out-file (int fps))]

    (try (doseq [f frames]
           (.encodeImage enc (grid->image (:grid f) cols rows cw ch font bold ascent)))
         (.finish enc)
         (finally (try (.finish enc) (catch Throwable _ nil))))
    out-file))

;; ── Orchestrator ────────────────────────────────────────────────────────────

(defn- with-theme
  "Run `f` with the TUI theme vars forced to `theme-id`, restoring the prior
   theme afterwards (the export must look like the requested theme without
   permanently mutating a live session's palette)."
  [theme-id f]
  (let [prior @tui-theme/active-theme-id]
    (try (tui-theme/apply-theme! theme-id)
         (f)
         (finally (try (tui-theme/apply-theme! prior) (catch Throwable _ nil))))))

(defn export!
  "Render `session-id` to an MP4 screencast file.

   opts:
     :out      output path (default: <session-id>.mp4 in the cwd)
     :cols :rows :fps :theme :font-size
     :type-cps :work-ms :jump-hold-ms :scroll-rows-per-frame :post-user-ms :post-assist-ms  — see `defaults`

   Returns `{:path :format :frames :video-ms :cols :rows}`."
  [session-id opts]
  (let [{:keys [out theme] :or {theme "vis-light"} :as opts}
        opts

        out
        (or out (str session-id ".mp4"))]

    (with-theme theme
                (fn []
                  (let [{:keys [frames video-ms cols rows] :as cap}
                        (session->frames session-id (dissoc opts :format :out))]
                    (frames->mp4! cap out opts)
                    {:path (str (.getAbsolutePath (io-file out)))
                     :format :mp4
                     :frames (count frames)
                     :video-ms video-ms
                     :cols cols
                     :rows rows})))))
