(ns com.blockether.vis.ext.channel-tui.components-test
  (:require [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.components :as comps]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- noop-graphics
  "A no-op TextGraphics so a paint helper can run headless — we only assert the
   click regions it registers, not the painted cells."
  []
  (let [active (atom #{})]
    (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
      (clearModifiers [] (reset! active #{}) this)
      (enableModifiers [^"[Lcom.googlecode.lanterna.SGR;" arr] (swap! active into (seq arr)) this)
      (getActiveModifiers []
        (if (empty? @active)
          (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR)
          (java.util.EnumSet/copyOf ^java.util.Collection @active)))
      (setForegroundColor [_] this)
      (setBackgroundColor [_] this)
      (fillRectangle [_ _ _] this)
      (putString ([col row text] this)))))

(defdescribe jump-bottom-button-clickable-test
             ;; The `↓ latest` jump-to-bottom chip is a `button!` with `:kind :jump-bottom`;
             ;; the screen's click handler `cr/lookup`s the cell under the cursor on
             ;; CLICK_DOWN and dispatches by that kind. This locks that the painted chip
             ;; registers a HITTABLE region at its cells (the click half of the feature,
             ;; without synthesizing terminal mouse events).
             (it "paints a hittable :jump-bottom region across exactly its label cells"
                 (cr/begin-frame!)
                 (let [w (comps/button! (noop-graphics) 148 32 " ↓ latest " :jump-bottom)]
                   (cr/commit-frame!)
                   (expect (= 10 w))                                    ;; " ↓ latest " is 10 narrow cells
                   (expect (= :jump-bottom (:kind (cr/lookup 151 32)))) ;; inside the chip
                   (expect (= :jump-bottom (:kind (cr/lookup 148 32)))) ;; left edge inclusive
                   (expect (nil? (cr/lookup 158 32)))                   ;; right edge exclusive (148+10)
                   (expect (nil? (cr/lookup 151 31))))                  ;; a row above → miss
                 (cr/reset!)))

(defdescribe
  find-bar-cursor-test
  (describe "terminal cursor rides the find bar's query field"
            ;; While the find bar is open it owns the keyboard, so the blinking
            ;; terminal cursor must sit INSIDE its white query field — not stay parked
            ;; in the prompt input. `find-bar-cursor` is the single geometry source
            ;; both render paths use; `find-bar!` returns the same cell by construction.
            (it "is nil while inactive — the prompt input keeps the cursor"
                (expect (nil? (comps/find-bar-cursor 120 5 {:active? false :query "ab"})))
                (expect (nil? (comps/find-bar-cursor 120 5 nil))))
            (it "sits on the bar's content row; typing advances it cell by cell"
                (let [s0
                      {:active? true :query "" :hits [] :index 0 :case? false :total 0}

                      [cx0 cy0]
                      (comps/find-bar-cursor 120 5 s0)

                      [cx2 cy2]
                      (comps/find-bar-cursor 120 5 (assoc s0 :query "ab"))]

                  (expect (= 6 cy0))           ;; text-top+1 — the row between the borders
                  (expect (= cy0 cy2))
                  (expect (= (+ cx0 2) cx2)))) ;; "ab" → 2 cells right of the empty-query start
            (it "caps at the field's right edge once the query overflows into ellipsis"
                (let [s0
                      {:active? true :query "" :hits [] :index 0 :case? false :total 0}

                      [cx0 _]
                      (comps/find-bar-cursor 120 5 s0)

                      [cxl _]
                      (comps/find-bar-cursor 120 5 (assoc s0 :query (apply str (repeat 80 \x))))]

                  (expect (<= (- cxl cx0) 22)))))) ;; never escapes the 22-cell white field

(defn- well-formed-line?
  "A LINE is a non-empty vec of SEGs; a SEG is [text color bold?]."
  [line]
  (and (vector? line)
       (seq line)
       (every? (fn [seg]
                 (and (vector? seg) (string? (first seg))))
               line)))

(defn- line-w-survives?
  "Mirror the F2 panel's line-w: map display-width over (first seg). Must not
   throw — a bare seg leaking in as a line made (first seg) a Character."
  [line]
  (number? (reduce + 0 (map (comp p/display-width first) line))))

(defdescribe
  context-overlay-lines-test
  (describe "task-overlay-lines emits well-formed LINES (regression: TUI freeze)"
            ;; The acceptance sub-line was spliced with `into`, leaking a bare
            ;; [text color bold] SEG where a LINE (vec-of-segs) belongs. The F2 panel's
            ;; line-w then mapped display-width over (first seg) = a Character →
            ;; ClassCastException every render frame → frozen TUI. Every element must
            ;; be a vec of segs so line-w only ever sees strings.
            ;; Cards are now MULTI-ROW (wrapped title + meta row + optional
            ;; acceptance/deps sub-lines + blank spacer). Exact counts are an
            ;; implementation detail of the layout; the load-bearing invariants are
            ;; that EVERY row stays a well-formed vec-of-segs and line-w never sees a
            ;; bare Character — that is what guards against the freeze regression.
            (it "a task WITH :acceptance yields a well-formed multi-row card"
                (let [lines (#'comps/task-overlay-lines
                             {:work
                              {:title "do x" :status :doing :acceptance "x compiles; tests pass"}}
                             60)]
                  (expect (>= (count lines) 3))
                  (expect (every? well-formed-line? lines))
                  (expect (every? line-w-survives? lines))))
            (it "a task WITHOUT :acceptance yields a well-formed multi-row card"
                (let [lines (#'comps/task-overlay-lines {:work {:title "y" :status :done}} 60)]
                  (expect (>= (count lines) 2))
                  (expect (every? well-formed-line? lines))
                  (expect (every? line-w-survives? lines))))
            (it "empty tasks yields a well-formed placeholder line"
                (let [lines (#'comps/task-overlay-lines {} 60)]
                  (expect (every? well-formed-line? lines))
                  (expect (every? line-w-survives? lines)))))
  (describe "fact-overlay-lines emits well-formed LINES"
            (it "facts (with + without :files) and the empty case stay well-formed"
                (let [lines
                      (#'comps/fact-overlay-lines
                       {:a {:content "alpha" :status :active :files [{:path "x.clj"}]}
                        :b {:content "beta" :status :superseded}}
                       60
                       #{})

                      empty-lines
                      (#'comps/fact-overlay-lines {} 60 #{})]

                  (expect (every? well-formed-line? lines))
                  (expect (every? line-w-survives? lines))
                  (expect (every? well-formed-line? empty-lines))))))
