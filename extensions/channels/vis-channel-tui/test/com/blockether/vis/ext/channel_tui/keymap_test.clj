(ns com.blockether.vis.ext.channel-tui.keymap-test
  (:require [clojure.set :as set]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe chord-label-test
  (it "single letters upper-case; named keys pass through — same on every platform"
    (expect (= "Ctrl+F" (keymap/chord \f)))
    (expect (= "Ctrl+R" (keymap/chord \r)))
    (expect (= "Ctrl+Enter" (keymap/chord "Enter"))))
  (it "the palette opener is C-x C-p (Emacs C-x prefix + Ctrl+P); M-x is the alias"
    (expect (= "C-x C-p" keymap/palette-chord))
    (expect (= \p keymap/prefix-palette-key))
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
    ;; C-x C-f / C-x C-a — the Emacs-idiomatic file commands (search / attach).
    ;; The second key resolves the same with or without its own Ctrl.
    (expect (= :search-open     (keymap/prefix-action-for \f)))
    (expect (= :pick-file       (keymap/prefix-action-for \a)))
    (expect (nil? (keymap/prefix-action-for \z)))
    (expect (= \x keymap/prefix-key)))
  (it "no emacs editing key is a direct app verb (action-for returns nil)"
    ;; The C-x prefix's second-keys (m/r/v/d/s) live behind C-x — a different
    ;; keyspace — so they don't shadow the editing chords.
    (doseq [c emacs-letters]
      (expect (nil? (keymap/action-for c))))
    (expect (nil? (keymap/action-for nil))))
  (it "providers / new-session are PALETTE-ONLY (no chord at all)"
    (expect (nil? (keymap/label-for :providers)))
    (expect (nil? (keymap/label-for :new-session))))
  (it "label-for round-trips a verb to its C-x prefix sequence"
    (expect (= "Ctrl+X M" (keymap/label-for :cycle-model)))
    (expect (= "Ctrl+X R" (keymap/label-for :cycle-reasoning)))
    (expect (= "Ctrl+X S" (keymap/label-for :open-resources)))
    ;; `:ctrl?` commands render in the compact Emacs C-x C-<key> form.
    (expect (= "C-x C-f" (keymap/label-for :search-open)))
    (expect (= "C-x C-a" (keymap/label-for :pick-file)))
    (expect (nil? (keymap/label-for :no-such-action))))
  (it "label-or-palette always returns a working chord (prefix or the palette)"
    (expect (= "Ctrl+X D" (keymap/label-or-palette :open-dirs)))
    (expect (= "Ctrl+X S" (keymap/label-or-palette :open-resources)))
    ;; search now has its own chord, so it returns that, not the palette.
    (expect (= "C-x C-f" (keymap/label-or-palette :search-open)))
    ;; A genuinely palette-only verb still falls back to the palette chord.
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
    ;; C-x C-p lives behind the C-x prefix (`prefix-palette-key` = p), a separate
    ;; keyspace from the direct Ctrl editing keys, so reusing `p` there clashes
    ;; with nothing. M-x is Alt+x — a META chord, again a different keyspace, so
    ;; reusing `x` there is fine too (Ctrl+X is the prefix, never an editing key).
    (expect (= \p keymap/prefix-palette-key))
    (expect (= \x keymap/palette-meta-key)))
  (it "picker reorder reuses the Emacs prev/next-line keys (intentional modal reuse)"
    ;; A picker is modal — the text editor never runs at the same time — so
    ;; reusing C-p/C-n for row up/down is consistent, not a clash.
    (expect (= \p keymap/picker-reorder-up))
    (expect (= \n keymap/picker-reorder-down))))
