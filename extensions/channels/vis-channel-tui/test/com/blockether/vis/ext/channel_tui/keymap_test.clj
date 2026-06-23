(ns com.blockether.vis.ext.channel-tui.keymap-test
  (:require [clojure.set :as set]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe chord-label-test
  (it "single letters upper-case; named keys pass through — same on every platform"
    (expect (= "Ctrl+F" (keymap/chord \f)))
    (expect (= "Ctrl+R" (keymap/chord \r)))
    (expect (= "Ctrl+Enter" (keymap/chord "Enter"))))
  (it "the palette opener is Ctrl+P"
    (expect (= "Ctrl+P" keymap/palette-chord))))

(defdescribe dispatch-table-test
  (it "action-for resolves the direct Ctrl chords, case-insensitively"
    (expect (= :search-open     (keymap/action-for \f)))
    (expect (= :search-open     (keymap/action-for \F)))
    (expect (= :cycle-reasoning (keymap/action-for \r)))
    (expect (= :cycle-verbosity (keymap/action-for \l)))
    (expect (= :cycle-model     (keymap/action-for \t)))
    (expect (= :providers       (keymap/action-for \b)))
    (expect (= :open-dirs       (keymap/action-for \g))))
  (it "action-for returns nil for unbound chars (so dispatch falls through to editing)"
    ;; The emacs editing keys must NOT be shadowed by an app verb.
    (expect (nil? (keymap/action-for \a)))
    (expect (nil? (keymap/action-for \e)))
    (expect (nil? (keymap/action-for \k)))
    (expect (nil? (keymap/action-for \z)))
    (expect (nil? (keymap/action-for nil))))
  (it "label-for round-trips a direct-chord action to its Ctrl chord"
    (expect (= "Ctrl+F" (keymap/label-for :search-open)))
    (expect (= "Ctrl+T" (keymap/label-for :cycle-model)))
    ;; A palette-only verb has no direct chord.
    (expect (nil? (keymap/label-for :open-resources)))
    (expect (nil? (keymap/label-for :no-such-action))))
  (it "label-or-palette always returns a working chord (direct or the palette)"
    (expect (= "Ctrl+G" (keymap/label-or-palette :open-dirs)))
    (expect (= keymap/palette-chord (keymap/label-or-palette :open-resources)))
    (expect (= keymap/palette-chord (keymap/label-or-palette :toggle-voice-recording))))
  (it "bindings have unique chord letters (no shadowing)"
    (let [keys (map :key keymap/bindings)]
      (expect (= (count keys) (count (distinct keys))))))
  (it "no binding collides with a kept emacs editing key"
    ;; Ctrl+A/E/K/U/W/D stay editing; Ctrl+P is the palette; Ctrl+C quits.
    (let [reserved #{\a \e \k \u \w \d \p \c \n}
          keys (set (map :key keymap/bindings))]
      (expect (empty? (set/intersection reserved keys))))))
