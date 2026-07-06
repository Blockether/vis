(ns com.blockether.vis.ext.channel-tui.cinema
  "Headless session → screencast. Replays a persisted session's transcript
   through the REAL TUI render pipeline (`screen/render-frame!` painting into a
   Lanterna `VirtualTerminal` — no TTY needed) at full fidelity, with every
   disclosure UNCOLLAPSED, and serialises the captured frames two ways:

     - `.cast`  — asciinema v2. Truecolor-perfect, tiny, scrubbable in any
                  asciinema player. This is the literal \"cinema\" format.
     - `.mp4`   — pure-JVM H.264 via jcodec's `AWTSequenceEncoder` (each frame
                  rendered to a `BufferedImage` with Java2D). Pixel-exact because
                  WE draw the glyphs — no external player font drift. No native
                  deps, GraalVM-friendly.

   Both come off ONE paced grid stream. Pacing honours the recorded per-turn
   `:duration-ms` (a slow turn stays proportionally slow, a fast one fast),
   uniformly compressed to a watchable target length — no per-element speeds
   are invented. The reveal is a smooth top→bottom autoscroll of the fully
   expanded transcript: you watch the session progress exactly as it looked
   live, just piece by piece.

   The heavy TUI stack (`screen`, Lanterna) is reached via `requiring-resolve`
   so merely loading this namespace stays cheap."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
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
   ;; Uniform time budget the whole replay is compressed into. Relative pacing
   ;; between turns is preserved; only the global scale changes.
   :target-ms   30000
   ;; Hard ceiling on frame count so a pathological session can't blow up the
   ;; encoder / output size.
   :max-frames  600
   ;; Minimum on-screen dwell (ms, pre-scaling) so instant user turns and
   ;; zero-duration in-flight turns still get a readable beat.
   :min-user-ms 1800
   :min-turn-ms 1200
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

(defn- msg-duration-ms
  [{:keys [min-user-ms min-turn-ms]} m]
  (let [floor (if (= :assistant (:role m)) min-turn-ms min-user-ms)]
    (max (long floor) (long (or (:duration-ms m) 0)))))

(defn- offset-at
  "Piecewise-linear time→scroll-offset lookup over `ctrl` [[t-ms offset] …]
   (t-ms ascending). Clamps outside the range."
  [ctrl ^double t]
  (let [n (count ctrl)]
    (loop [i 1]
      (if (>= i n)
        (double (second (nth ctrl (dec n))))
        (let [[t0 o0] (nth ctrl (dec i))
              [t1 o1] (nth ctrl i)]
          (cond
            (<= t (double t0)) (double o0)
            (<= t (double t1)) (let [span (- (double t1) (double t0))]
                                 (if (<= span 0.0)
                                   (double o1)
                                   (+ (double o0)
                                     (* (- (double o1) (double o0))
                                       (/ (- t (double t0)) span)))))
            :else (recur (inc i))))))))

(defn session->frames
  "Replay `session-id` headless and return
   `{:cols :rows :frames [{:grid :delay-ms} …] :video-ms}`.

   `opts` overrides `defaults`. The transcript is fully expanded and the
   viewport autoscrolls top→bottom paced by scaled per-turn durations."
  [session-id opts]
  (let [{:keys [cols rows fps target-ms max-frames] :as cfg} (merge defaults opts)
        history ((rebuild-history-fn) session-id)
        render! (render-frame-fn)
        vt (DefaultVirtualTerminal. (TerminalSize. (int cols) (int rows)))
        scr (doto (TerminalScreen. vt) (.startScreen))
        base (do (state/init!) @state/app-db)
        db (assoc base
             :session {:id session-id}
             :messages (vec history)
             :detail-expansions {:vis.channel-tui/baseline :expand})
        ;; One full render establishes the expanded layout geometry.
        layout (render! scr cols rows (assoc db :scroll scroll/follow)
                 (System/currentTimeMillis))
        total-h (long (:total-h layout))
        inner-h (long (:inner-h layout))
        offsets (mapv long (:offsets layout))
        max-s (max 0 (- total-h inner-h))
        durs (mapv #(msg-duration-ms cfg %) history)
        cum (vec (reductions + 0 durs))            ; length (count history)+1
        total-real (double (max 1 (peek cum)))
        scale (/ (double target-ms) total-real)
        ;; Control points: message boundary i reveals at scaled time, viewport
        ;; anchored to that message's document row (clamped to max scroll).
        n-ctrl (min (count offsets) (count cum))
        ctrl (mapv (fn [i]
                     [(* (double (nth cum i)) scale)
                      (double (min (nth offsets i) max-s))])
               (range n-ctrl))
        video-ms (long (* total-real scale))
        frame-dt (max 1 (long (/ 1000 (long fps))))
        raw-times (range 0 (inc video-ms) frame-dt)
        times (if (> (count raw-times) max-frames)
                ;; Downsample to the cap, preserving even spacing.
                (let [step (/ (double (count raw-times)) max-frames)]
                  (map #(nth (vec raw-times) (min (dec (count raw-times))
                                               (long (* % step))))
                    (range max-frames)))
                raw-times)
        frames (vec
                 (for [t times]
                   (let [off (long (Math/round (offset-at ctrl (double t))))]
                     (render! scr cols rows
                       (assoc db :scroll (scroll/parked off))
                       (System/currentTimeMillis))
                     (.refresh scr)
                     {:grid (capture-grid scr cols rows)
                      :delay-ms frame-dt})))]
    (.stopScreen scr)
    {:cols cols :rows rows :frames frames :video-ms video-ms}))

;; ── asciinema .cast emitter ─────────────────────────────────────────────────

(defn- sgr-seq
  "ANSI SGR to enter a cell's style (reset first, then truecolor + modifiers)."
  [{:keys [fg bg bold italic underline reverse]}]
  (let [[fr fg* fb] fg [br bg* bb] bg]
    (str "\u001b[0"
      ";38;2;" fr ";" fg* ";" fb
      ";48;2;" br ";" bg* ";" bb
      (when bold ";1") (when italic ";3") (when underline ";4") (when reverse ";7")
      "m")))

(defn- style-key [c]
  [(:fg c) (:bg c) (:bold c) (:italic c) (:underline c) (:reverse c)])

(defn- grid->ansi
  "Full-frame repaint: home cursor, then every row with run-length SGR."
  [grid]
  (let [sb (StringBuilder.)]
    (.append sb "\u001b[H")
    (doseq [[y row] (map-indexed vector grid)]
      (when (pos? y) (.append sb "\r\n"))
      (loop [cells row prev nil]
        (when-let [c (first cells)]
          (let [k (style-key c)]
            (when (not= k prev) (.append sb (sgr-seq c)))
            (.append sb (:ch c))
            (recur (next cells) k))))
      (.append sb "\u001b[0m"))
    (str sb)))

(defn frames->cast
  "Serialise a `session->frames` result to an asciinema v2 `.cast` string."
  [{:keys [cols rows frames]}]
  (let [header {:version 2 :width cols :height rows
                :timestamp (quot (System/currentTimeMillis) 1000)
                :env {"TERM" "xterm-256color" "SHELL" "vis"}}
        sb (StringBuilder.)]
    (.append sb (json/write-str header))
    (.append sb "\n")
    ;; Clear once up front, then a full repaint per frame at its cumulative time.
    (.append sb (json/write-str [0.0 "o" "\u001b[2J\u001b[H"]))
    (.append sb "\n")
    (loop [fs frames t 0.0]
      (when-let [f (first fs)]
        (.append sb (json/write-str [(/ (double t) 1000.0) "o" (grid->ansi (:grid f))]))
        (.append sb "\n")
        (recur (next fs) (+ t (double (:delay-ms f))))))
    (str sb)))

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
  "Render `session-id` to a screencast file.

   opts:
     :format   :cast (default) | :mp4
     :out      output path (default: <session-id>.<ext> in the cwd)
     :cols :rows :fps :target-ms :theme :font-size  — see `defaults`

   Returns `{:path :format :frames :video-ms :cols :rows}`."
  [session-id opts]
  (let [{:keys [format out theme] :or {format :cast theme "vis-light"} :as opts} opts
        ext (case format :mp4 "mp4" "cast")
        out (or out (str session-id "." ext))]
    (with-theme
      theme
      (fn []
        (let [{:keys [frames video-ms cols rows] :as cap}
              (session->frames session-id (dissoc opts :format :out))]
          (case format
            :mp4 (frames->mp4! cap out opts)
            (spit out (frames->cast cap)))
          {:path (str (.getAbsolutePath (io-file out)))
           :format format
           :frames (count frames)
           :video-ms video-ms
           :cols cols :rows rows})))))
