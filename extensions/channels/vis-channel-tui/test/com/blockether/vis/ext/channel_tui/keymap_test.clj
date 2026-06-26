(ns com.blockether.vis.ext.channel-tui.keymap-test
  (:require [clojure.set :as set]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe chord-label-test
  (it "single letters upper-case; named keys pass through — same on every platform"
    (expect (= "Ctrl+F" (keymap/chord \f)))
    (expect (= "Ctrl+R" (keymap/chord \r)))
    (expect (= "Ctrl+Enter" (keymap/chord "Enter"))))
  (it "the palette opener is M-x (Alt+x; the Emacs command launcher)"
    (expect (= "M-x" keymap/palette-chord))
    (expect (= \x keymap/palette-meta-key))))

;; The Emacs editing keys (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-u/C-w/C-d/C-t) are
;; first-class in every input, so NO direct app-verb chord may use those letters.
(def ^:private emacs-letters #{\a \e \b \f \p \n \k \u \w \d \t})

(defdescribe dispatch-table-test
  (it "NO direct verb chords remain — action-for is nil; verbs are C-x prefixed"
    (expect (empty? keymap/bindings))
    (expect (nil? (keymap/action-for \r)))
    (expect (nil? (keymap/action-for \x)))
    (expect (nil? (keymap/action-for \g))))
  (it "the C-x prefix resolves the vis commands, case-insensitively"
    (expect (= :cycle-model     (keymap/prefix-action-for \m)))
    (expect (= :cycle-model     (keymap/prefix-action-for \M)))
    (expect (= :cycle-reasoning (keymap/prefix-action-for \r)))
    (expect (= :cycle-verbosity (keymap/prefix-action-for \v)))
    (expect (= :open-dirs       (keymap/prefix-action-for \d)))
    (expect (= :open-resources  (keymap/prefix-action-for \s)))
    (expect (nil? (keymap/prefix-action-for \z)))
    (expect (= \x keymap/prefix-key)))
  (it "no emacs editing key is a direct app verb (action-for returns nil)"
    ;; The C-x prefix's second-keys (m/r/v/d/s) live behind C-x — a different
    ;; keyspace — so they don't shadow the editing chords.
    (doseq [c emacs-letters]
      (expect (nil? (keymap/action-for c))))
    (expect (nil? (keymap/action-for nil))))
  (it "search / providers / new-session are PALETTE-ONLY (no chord at all)"
    (expect (nil? (keymap/label-for :search-open)))
    (expect (nil? (keymap/label-for :providers)))
    (expect (nil? (keymap/label-for :new-session))))
  (it "label-for round-trips a verb to its C-x prefix sequence"
    (expect (= "Ctrl+X M" (keymap/label-for :cycle-model)))
    (expect (= "Ctrl+X R" (keymap/label-for :cycle-reasoning)))
    (expect (= "Ctrl+X S" (keymap/label-for :open-resources)))
    (expect (nil? (keymap/label-for :no-such-action))))
  (it "label-or-palette always returns a working chord (prefix or the palette)"
    (expect (= "Ctrl+X D" (keymap/label-or-palette :open-dirs)))
    (expect (= "Ctrl+X S" (keymap/label-or-palette :open-resources)))
    ;; A palette-only verb falls back to the palette chord (M-x).
    (expect (= keymap/palette-chord (keymap/label-or-palette :search-open)))
    (expect (= keymap/palette-chord (keymap/label-or-palette :toggle-voice-recording))))
  (it "bindings is empty, so nothing collides with an emacs editing key"
    (expect (empty? keymap/bindings))
    (let [reserved (conj emacs-letters \c)
          keys     (set (map :key keymap/bindings))]
      (expect (empty? (set/intersection reserved keys))))))

(defdescribe structural-chords-registry-test
  ;; keymap.clj is the ONE registry for vis-side chords (editing keys are the
  ;; sole exception — they live in lanterna's TextEditKeymap). Lock the
  ;; structural keys so nothing drifts into a clash.
  (it "help / quit chords are not app-verb letters"
    (let [verb-letters (set (map :key keymap/bindings))]
      (expect (not (contains? verb-letters keymap/help-key)))
      (expect (not (contains? verb-letters keymap/quit-key)))))
  (it "the palette triggers can't collide with the Ctrl editing/verb letters"
    ;; M-x is Alt+x — a META chord, a different keyspace from every Ctrl editing
    ;; key and verb, so reusing the letter `x` there clashes with nothing (Ctrl+X
    ;; resources stays separate). The Ctrl+] fallback is non-letter.
    (expect (= \x keymap/palette-meta-key))
    (expect (seq keymap/palette-trigger-chars))
    (expect (not-any? #(Character/isLetter ^char %) keymap/palette-trigger-chars)))
  (it "picker reorder reuses the Emacs prev/next-line keys (intentional modal reuse)"
    ;; A picker is modal — the text editor never runs at the same time — so
    ;; reusing C-p/C-n for row up/down is consistent, not a clash.
    (expect (= \p keymap/picker-reorder-up))
    (expect (= \n keymap/picker-reorder-down))))
