(ns com.blockether.vis.ext.channel-tui.components-test
  (:require
   [com.blockether.vis.ext.channel-tui.components :as comps]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- well-formed-line?
  "A LINE is a non-empty vec of SEGs; a SEG is [text color bold?]."
  [line]
  (and (vector? line)
    (seq line)
    (every? (fn [seg] (and (vector? seg) (string? (first seg)))) line)))

(defn- line-w-survives?
  "Mirror the F2 panel's line-w: map display-width over (first seg). Must not
   throw — a bare seg leaking in as a line made (first seg) a Character."
  [line]
  (number? (reduce + 0 (map (comp p/display-width first) line))))

(defdescribe context-overlay-lines-test
  (describe "task-overlay-lines emits well-formed LINES (regression: TUI freeze)"
    ;; The acceptance sub-line was spliced with `into`, leaking a bare
    ;; [text color bold] SEG where a LINE (vec-of-segs) belongs. The F2 panel's
    ;; line-w then mapped display-width over (first seg) = a Character →
    ;; ClassCastException every render frame → frozen TUI. Every element must
    ;; be a vec of segs so line-w only ever sees strings.
    (it "a task WITH :acceptance yields two well-formed lines (primary + sub)"
      (let [lines (#'comps/task-overlay-lines
                    {:work {:title "do x" :status :doing
                            :acceptance "x compiles; tests pass"}}
                    60)]
        (expect (= 2 (count lines)))
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))))

    (it "a task WITHOUT :acceptance yields one well-formed line"
      (let [lines (#'comps/task-overlay-lines {:work {:title "y" :status :done}} 60)]
        (expect (= 1 (count lines)))
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))))

    (it "empty tasks yields a well-formed placeholder line"
      (let [lines (#'comps/task-overlay-lines {} 60)]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines)))))

  (describe "fact-overlay-lines emits well-formed LINES"
    (it "facts (with + without :files) and the empty case stay well-formed"
      (let [lines (#'comps/fact-overlay-lines
                    {:a {:content "alpha" :status :active
                         :files [{:path "x.clj"}]}
                     :b {:content "beta" :status :superseded}}
                    60)
            empty-lines (#'comps/fact-overlay-lines {} 60)]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))
        (expect (every? well-formed-line? empty-lines))))))
