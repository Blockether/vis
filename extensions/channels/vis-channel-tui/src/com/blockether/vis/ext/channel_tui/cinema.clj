(ns com.blockether.vis.ext.channel-tui.cinema
  "Headless session → MP4 screencast. Replays a persisted session's transcript
   through the REAL TUI render pipeline (`screen/render-frame!` painting into a
   Lanterna `VirtualTerminal` — no TTY needed) at full fidelity, with every
   disclosure UNCOLLAPSED, and encodes the captured frames to `.mp4`:

     pure-JVM H.264 via jcodec's `AWTSequenceEncoder` (each frame rendered to a
     `BufferedImage` with Java2D). Pixel-exact because WE draw the glyphs — no
     external player font drift. No native deps, GraalVM-friendly.

   The viewport rides the generation frontier straight DOWN the transcript
   at a constant reading speed (ms per row), so a tall turn scrolls in over
   proportionally more time and the WHOLE replay length scales with the
   session's content — no fixed budget everything is crammed into. Motion is
   monotonic (driven off one stable full-layout pass), so it never bounces
   back up: you watch the session stream in exactly as it looked live.

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
  {:cols        120
   :rows        42
   :fps         8
   ;; Constant downward scroll speed: ms of video per transcript ROW. The
   ;; viewport rides the "generation frontier" straight down the transcript at
   ;; this rate, so a TALL turn takes proportionally longer to stream in and
   ;; the whole replay length scales with the session's content instead of
   ;; being crammed into a fixed budget.
   :scroll-ms-per-row 22
   ;; Per-message floor so a short turn still gets a readable beat before the
   ;; scroll moves on to the next one.
   :min-msg-ms  1100
   ;; Overall length guard rails — a tiny session is stretched to at least
   ;; min, a giant one compressed to at most max (relative pacing preserved).
   :min-total-ms 10000
   :max-total-ms 240000
   ;; Hold the final (bottom) frame briefly so the ending doesn't cut abruptly.
   :end-hold-ms 1200
   ;; Hard ceiling on frame count so a pathological session can't blow up the
   ;; encoder / output size.
   :max-frames  2400
   ;; MP4 glyph cell size in pixels (width is derived from the mono font).
   :font-size   18
   :theme       theme/default-theme-id})

;; ── Headless capture ────────────────────────────────────────────────────────

(defn- render-frame-fn []
  (or (requiring-resolve 'com.blockether.vis.ext.channel-tui.screen/render-frame!)
    (throw (ex-info "cinema: screen/render-frame! did not resolve" {}))))

(defn- rebuild-history-fn []
  (or (requiring-resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)
    (throw (ex-info "cinema: chat/rebuild-history did not resolve" {}))))

(defn- rgb [color]
  [(.getRed color) (.getGreen color) (.getBlue color)])

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

   `opts` overrides `defaults`. The transcript is fully EXPANDED. We take ONE
   full-layout pass (every message shown) to capture the stable per-message row
   offsets and total height, then drive a MONOTONIC downward scroll off those
   fixed offsets: the viewport bottom rides each message's row span across a
   time slice proportional to that message's height (constant reading speed).

   Because the scroll target comes from the stable full layout — never a
   per-frame total that the virtualizer keeps revising — the motion only ever
   travels DOWN and never bounces back up. It reads like the session streaming
   in live: content scrolls up from the bottom as the frontier descends, tall
   turns take longer, and the overall length scales with the session."
  [session-id opts]
  (let [{:keys [cols rows fps scroll-ms-per-row min-msg-ms min-total-ms
                max-total-ms end-hold-ms max-frames]} (merge defaults opts)
        history (vec ((rebuild-history-fn) session-id))
        n       (count history)
        render! (render-frame-fn)
        vt (DefaultVirtualTerminal. (TerminalSize. (int cols) (int rows)))
        scr (doto (TerminalScreen. vt) (.startScreen))
        base (do (state/init!) @state/app-db)
        db0 (assoc base
              :session {:id session-id}
              ;; Headless replay: no live provider-limits polling, so tell the
              ;; footer to keep the "limits:" row clean instead of "loading…".
              :cinema? true
              ;; Every disclosure open — this is a full-fidelity replay.
              :detail-expansions {:vis.channel-tui/baseline :expand})
        ;; ONE full layout pass (every message shown) → STABLE per-message row
        ;; offsets + total height. Driving the scroll off these FIXED offsets
        ;; (never a per-frame total) is what keeps the motion monotonic: the
        ;; view can only travel DOWN, so it never yanks back up when the
        ;; virtualized layout revises an off-screen height estimate.
        full-db (assoc db0 :messages history :scroll scroll/follow)
        full-ly (render! scr cols rows full-db (System/currentTimeMillis))
        offsets (vec (:offsets full-ly))
        total-h (long (or (:total-h full-ly) 0))
        inner-h (long (or (:inner-h full-ly) 0))
        max-s   (max 0 (- total-h inner-h))
        ;; Top / bottom row of each message. `offsets` is length n+1 (last =
        ;; total-h); fall back to total-h defensively for the final bottom.
        tops    (mapv #(long (nth offsets % 0)) (range n))
        bottoms (mapv (fn [i] (long (nth offsets (inc i) total-h))) (range n))
        heights (mapv (fn [i] (max 0 (- (long (bottoms i)) (long (tops i))))) (range n))
        ;; Time each message's scroll slice gets: proportional to its height
        ;; (constant speed), floored so short turns still read.
        raw-win (mapv #(max (long min-msg-ms) (long (* (long scroll-ms-per-row) (long %)))) heights)
        raw-total (double (max 1 (reduce + 0 raw-win)))
        scale   (cond
                  (> raw-total (double max-total-ms)) (/ (double max-total-ms) raw-total)
                  (< raw-total (double min-total-ms)) (/ (double min-total-ms) raw-total)
                  :else 1.0)
        wins    (mapv #(* scale (double %)) raw-win)
        wstart  (vec (reductions + 0.0 wins))          ; length n+1
        scroll-ms (long (peek wstart))
        video-ms  (+ scroll-ms (long end-hold-ms))
        ;; Viewport bottom (the "generation frontier") at wall-clock t: walk
        ;; message i's [top,bottom] row span across its time window. Monotonic
        ;; in t, so the derived scroll offset only ever increases.
        reveal-bottom
        (fn [t]
          (if (zero? n)
            0.0
            (let [t  (double (min (double t) (double scroll-ms)))
                  i  (max 0 (min (dec n) (dec (count (take-while #(<= (double %) t) wstart)))))
                  w0 (double (wstart i)) w1 (double (wstart (inc i)))
                  frac (if (> w1 w0) (min 1.0 (/ (- t w0) (- w1 w0))) 1.0)
                  o0 (double (tops i)) o1 (double (bottoms i))]
              (+ o0 (* frac (- o1 o0))))))
        frame-dt  (max 1 (long (/ 1000 (long fps))))
        raw-times (vec (range 0 (inc video-ms) frame-dt))
        times     (if (> (count raw-times) max-frames)
                    ;; Downsample to the cap, preserving even spacing.
                    (let [step (/ (double (count raw-times)) max-frames)]
                      (mapv #(nth raw-times (min (dec (count raw-times)) (long (* % step))))
                        (range max-frames)))
                    raw-times)
        frames
        (loop [ts times, acc (transient [])]
          (if (seq ts)
            (let [t   (first ts)
                  rb  (reveal-bottom t)
                  ;; Anchor the viewport so its BOTTOM edge sits at the frontier.
                  st  (long (max 0 (min max-s (Math/round (- rb (double inner-h))))))
                  db  (assoc db0
                        :messages history
                        :scroll (scroll/parked st)
                        ;; Feed the stable full layout so `render-frame!` derives
                        ;; the real `prev-max-s` and never clamps our offset.
                        :layout full-ly)
                  _ (render! scr cols rows db (System/currentTimeMillis))
                  _ (.refresh scr)
                  grid (capture-grid scr cols rows)]
              (recur (rest ts) (conj! acc {:grid grid :delay-ms frame-dt})))
            (persistent! acc)))]
    (.stopScreen scr)
    {:cols cols :rows rows :frames frames :video-ms video-ms}))

;; ── MP4 (jcodec) emitter ────────────────────────────────────────────────────

(defn- awt-color ^Color [[r g b]]
  (Color. (int r) (int g) (int b)))

(defn- even2 ^long [^long v] (if (odd? v) (inc v) v))

(defn- io-file ^File [x]
  (if (instance? File x) x (File. (str x))))

(defn- grid->image
  "Render one captured grid to a BufferedImage with Java2D. `cw`/`ch` are cell
   pixel dimensions, `font`/`bold-font` the mono glyph fonts, `ascent` the
   baseline offset."
  ^BufferedImage [grid cols rows cw ch font bold-font ascent]
  (let [w (even2 (* cols cw))
        h (even2 (* rows ch))
        img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        g (.createGraphics img)]
    (.setRenderingHint g RenderingHints/KEY_TEXT_ANTIALIASING
      RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING
      RenderingHints/VALUE_RENDER_QUALITY)
    (doseq [[y row] (map-indexed vector grid)
            [x c] (map-indexed vector row)]
      (let [px (* x cw) py (* y ch)]
        (.setColor g (awt-color (:bg c)))
        (.fillRect g px py cw ch)
        (let [ch* (:ch c)]
          (when (and ch* (pos? (count (str/trim ch*))))
            (.setFont g (if (:bold c) bold-font font))
            (.setColor g (awt-color (:fg c)))
            (.drawString g ^String ch* (int px) (int (+ py ascent)))))))
    (.dispose g)
    img))

(defn- mono-fonts [^long size]
  (let [base (Font. Font/MONOSPACED Font/PLAIN (int size))]
    {:font base
     :bold (.deriveFont base Font/BOLD)}))

(defn frames->mp4!
  "Encode a `session->frames` result to `out` (an MP4 File/path) via jcodec's
   pure-Java `AWTSequenceEncoder`. Returns the output File."
  [{:keys [cols rows frames]} out {:keys [font-size fps] :or {font-size 18 fps 8}}]
  (let [out-file (io-file out)
        {:keys [font bold]} (mono-fonts font-size)
        ;; Measure the mono cell from a throwaway image's FontMetrics.
        probe (.createGraphics (BufferedImage. 8 8 BufferedImage/TYPE_INT_RGB))
        _ (.setFont probe font)
        fm (.getFontMetrics probe)
        cw (max 1 (.charWidth fm \M))
        ch (max 1 (.getHeight fm))
        ascent (.getAscent fm)
        _ (.dispose probe)
        enc (org.jcodec.api.awt.AWTSequenceEncoder/createSequenceEncoder out-file (int fps))]
    (try
      (doseq [f frames]
        (.encodeImage enc (grid->image (:grid f) cols rows cw ch font bold ascent)))
      (.finish enc)
      (finally
        (try (.finish enc) (catch Throwable _ nil))))
    out-file))

;; ── Orchestrator ────────────────────────────────────────────────────────────

(defn- with-theme
  "Run `f` with the TUI theme vars forced to `theme-id`, restoring the prior
   theme afterwards (the export must look like the requested theme without
   permanently mutating a live session's palette)."
  [theme-id f]
  (let [prior @tui-theme/active-theme-id]
    (try
      (tui-theme/apply-theme! theme-id)
      (f)
      (finally
        (try (tui-theme/apply-theme! prior) (catch Throwable _ nil))))))

(defn export!
  "Render `session-id` to an MP4 screencast file.

   opts:
     :out      output path (default: <session-id>.mp4 in the cwd)
     :cols :rows :fps :scroll-ms-per-row :theme :font-size  — see `defaults`

   Returns `{:path :format :frames :video-ms :cols :rows}`."
  [session-id opts]
  (let [{:keys [out theme] :or {theme "vis-light"} :as opts} opts
        out (or out (str session-id ".mp4"))]
    (with-theme
      theme
      (fn []
        (let [{:keys [frames video-ms cols rows] :as cap}
              (session->frames session-id (dissoc opts :format :out))]
          (frames->mp4! cap out opts)
          {:path (str (.getAbsolutePath (io-file out)))
           :format :mp4
           :frames (count frames)
           :video-ms video-ms
           :cols cols :rows rows})))))
