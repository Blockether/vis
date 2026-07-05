(ns flicker-harness
  "Objective flicker / delta-correctness harness for the Lanterna TerminalScreen
   delta emitter (the vis.20 run-batched refresh).

   FLICKER = the terminal showing a cell in the WRONG place or a stale ghost.
   That can only happen if the byte/op stream `refresh(DELTA)` emits does not
   reproduce the screen's intended back buffer when applied to a faithful
   terminal. So we measure it directly:

     1. Back a TerminalScreen with a DefaultVirtualTerminal (a reference
        terminal that applies setCursorPosition / putString / colours with a
        real cursor + autowrap model).
     2. Paint a baseline, refresh(COMPLETE).
     3. Mutate the back buffer (simulate a scroll shift / style flip / wide
        char / right-margin fill).
     4. refresh(DELTA) — the emitter streams its runs into the reference
        terminal.
     5. Assert the reference terminal's buffer == the screen's back buffer,
        cell for cell. A mismatch is a real, reproducible flicker bug and the
        harness prints exactly which cell diverged.

   Run: clojure -M:flicker   (alias added to deps.edn)"
  (:import [com.googlecode.lanterna TextCharacter TextColor$ANSI TerminalSize TerminalPosition SGR]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]
           [com.googlecode.lanterna.screen TerminalScreen Screen$RefreshType]))

(def COLS 80)
(def ROWS 24)

(defn tc
  "One TextCharacter with char `ch` and an fg/bg/bold triple keyed by `style`."
  ^TextCharacter [ch style]
  (let [[fg bg sgrs] (case (int (mod style 4))
                       0 [TextColor$ANSI/DEFAULT TextColor$ANSI/DEFAULT []]
                       1 [TextColor$ANSI/RED     TextColor$ANSI/DEFAULT []]
                       2 [TextColor$ANSI/GREEN   TextColor$ANSI/BLACK   [SGR/BOLD]]
                       3 [TextColor$ANSI/BLUE    TextColor$ANSI/DEFAULT [SGR/UNDERLINE]])]
    (-> (TextCharacter/fromString (str ch) ^TextColor$ANSI fg ^TextColor$ANSI bg
          (into-array SGR sgrs))
      (aget 0))))

(defn paint-back!
  "Write `cellfn`(x,y)->TextCharacter (or nil for default) into the back buffer."
  [^TerminalScreen s cols rows cellfn]
  (dotimes [y rows]
    (dotimes [x cols]
      (when-let [c (cellfn x y)]
        (.setCharacter s (int x) (int y) ^TextCharacter c)))))

(defn buffers-diverge
  "Return the first [x y back ref] cell where the screen back buffer and the
   reference terminal buffer disagree, or nil when they match everywhere."
  [^TerminalScreen s ^DefaultVirtualTerminal vt cols rows]
  (first
    (for [y (range rows) x (range cols)
          :let [back (.getBackCharacter s (int x) (int y))
                ref  (.getBufferCharacter vt (int x) (int y))]
          :when (not (.equals back ref))]
      [x y (.getCharacterString back) (.getCharacterString ref)
       (str "back=" back " ref=" ref)])))

(defn run-scenario
  "Paint `baseline`, COMPLETE-refresh, paint `mutation`, DELTA-refresh, then
   check reference==back. Returns nil on pass or a divergence descriptor."
  [name baseline mutation]
  (let [vt (DefaultVirtualTerminal. (TerminalSize. COLS ROWS))
        s  (TerminalScreen. vt)]
    (.startScreen s)
    (paint-back! s COLS ROWS baseline)
    (.refresh s Screen$RefreshType/COMPLETE)
    ;; Clear back to default, then apply the mutation fresh (mirrors how the
    ;; app rebuilds the whole back buffer each frame).
    (dotimes [y ROWS] (dotimes [x COLS] (.setCharacter s (int x) (int y) TextCharacter/DEFAULT_CHARACTER)))
    (paint-back! s COLS ROWS mutation)
    (.refresh s Screen$RefreshType/DELTA)
    (when-let [d (buffers-diverge s vt COLS ROWS)]
      {:scenario name :cell d})))

;; ── Scenario generators ──────────────────────────────────────────────────
(defn row-text
  "A cell fn painting `s` starting at column 0 of row `y0`, style `st`."
  [y0 s st]
  (fn [x y] (when (and (= y y0) (< x (count s))) (tc (nth s x) st))))

(defn full-rows
  "Every row filled with a repeating pattern shifted by `off` (simulates a
   scroll). Style varies per row so runs break."
  [off]
  (fn [x y]
    (let [ch (char (+ (int \A) (mod (+ x y off) 26)))]
      (tc ch (mod (+ y off) 4)))))

(defn wide-row
  "Row y0: double-width glyphs (CJK) interleaved with ASCII, to exercise the
   trailing-cell ghost rules."
  [y0]
  (fn [x y]
    (when (= y y0)
      (cond
        (< x 6) (tc (nth "你好世界ab" x) 0)   ;; 你好世界 are double-width
        :else   (tc (char (+ (int \0) (mod x 10))) 1)))))

(defn margin-fill
  "Fill row y0 completely to the RIGHT EDGE (last column), style st — the
   autowrap / pending-wrap hazard for the emitter's cursor arithmetic."
  [y0 st]
  (fn [x y] (when (= y y0) (tc (char (+ (int \!) (mod x 90))) st))))

(defn -main [& _]
  (let [results
        (concat
          [(run-scenario "empty->empty" (constantly nil) (constantly nil))
           (run-scenario "text appears" (constantly nil) (row-text 3 "Hello, world" 1))
           (run-scenario "text style flip" (row-text 3 "Hello, world" 1) (row-text 3 "Hello, world" 2))
           (run-scenario "scroll shift +1" (full-rows 0) (full-rows 1))
           (run-scenario "scroll shift +7" (full-rows 0) (full-rows 7))
           (run-scenario "wide chars appear" (constantly nil) (wide-row 5))
           (run-scenario "wide->narrow (ghost)" (wide-row 5) (row-text 5 "narrow now" 0))
           (run-scenario "margin fill row 3" (constantly nil) (margin-fill 3 1))
           (run-scenario "margin fill then edit" (margin-fill 3 1) (margin-fill 3 2))
           (run-scenario "margin fill last-row-1" (constantly nil) (margin-fill (- ROWS 2) 3))]
          ;; Deterministic fuzz: many random full-screen baseline->mutation
          ;; pairs. Seeded so failures reproduce.
          (let [rng (java.util.Random. 42)]
            (for [i (range 400)]
              (let [b (fn [_x _y] (when (< (.nextDouble rng) 0.6)
                                    (tc (char (+ 33 (.nextInt rng 90))) (.nextInt rng 4))))
                    ;; Snapshot baseline into a vec so mutation can perturb ~15% of cells.
                    base (vec (for [y (range ROWS) x (range COLS)] (b x y)))
                    mut  (mapv (fn [c] (if (< (.nextDouble rng) 0.15)
                                         (when (< (.nextDouble rng) 0.6)
                                           (tc (char (+ 33 (.nextInt rng 90))) (.nextInt rng 4)))
                                         c))
                           base)
                    idx (fn [x y] (+ (* y COLS) x))]
                (run-scenario (str "fuzz#" i)
                  (fn [x y] (nth base (idx x y)))
                  (fn [x y] (nth mut (idx x y))))))))
        failures (remove nil? results)]
    (println (format "flicker-harness: %d scenarios, %d PASS, %d FAIL"
               (count results) (- (count results) (count failures)) (count failures)))
    (doseq [f (take 12 failures)]
      (println "  FAIL" (:scenario f) "->" (:cell f)))
    (when (seq failures) (System/exit 1))
    (println "ALL PASS — delta emitter reproduces the intended buffer on a faithful terminal.")))
