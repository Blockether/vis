(ns com.blockether.vis.ext.channel-tui.keymap-test
  (:require [clojure.set :as set]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe chord-label-test
  (it "single letters upper-case; named keys pass through — same on every platform"
    (expect (= "Ctrl+F" (keymap/chord \f)))
    (expect (= "Ctrl+R" (keymap/chord \r)))
    (expect (= "Ctrl+Enter" (keymap/chord "Enter"))))
  (it "the palette opener is Ctrl+] (reliable cross-platform; emacs keys own the letters)"
    (expect (= "Ctrl+]" keymap/palette-chord))))

;; The Emacs editing keys (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-u/C-w/C-d) are
;; first-class in every input, so NO app-verb chord may use those letters.
(def ^:private emacs-letters #{\a \e \b \f \p \n \k \u \w \d})

(defdescribe dispatch-table-test
  (it "action-for resolves ONLY the collision-free direct chords, case-insensitively"
    (expect (= :cycle-reasoning (keymap/action-for \r)))
    (expect (= :cycle-reasoning (keymap/action-for \R)))
    (expect (= :cycle-verbosity (keymap/action-for \l)))
    (expect (= :cycle-model     (keymap/action-for \t)))
    (expect (= :open-dirs       (keymap/action-for \g)))
    (expect (= :open-resources  (keymap/action-for \x))))
  (it "no emacs editing key is shadowed by an app verb (action-for returns nil)"
    ;; If any of these resolved to a verb, the editing chord would never reach
    ;; the editor — the whole point of the change.
    (doseq [c emacs-letters]
      (expect (nil? (keymap/action-for c))))
    (expect (nil? (keymap/action-for \z)))
    (expect (nil? (keymap/action-for nil))))
  (it "search / providers / new-session are PALETTE-ONLY (no direct chord)"
    (expect (nil? (keymap/action-for \f)))   ; was search — now forward-char
    (expect (nil? (keymap/action-for \b)))   ; was providers — now backward-char
    (expect (nil? (keymap/action-for \n)))   ; was new-session — now next-line
    (expect (nil? (keymap/label-for :search-open)))
    (expect (nil? (keymap/label-for :providers)))
    (expect (nil? (keymap/label-for :new-session))))
  (it "label-for round-trips a direct-chord action to its Ctrl chord"
    (expect (= "Ctrl+R" (keymap/label-for :cycle-reasoning)))
    (expect (= "Ctrl+T" (keymap/label-for :cycle-model)))
    ;; Resources has a direct chord because the footer exposes it as a live affordance.
    (expect (= "Ctrl+X" (keymap/label-for :open-resources)))
    (expect (nil? (keymap/label-for :no-such-action))))
  (it "label-or-palette always returns a working chord (direct or the palette)"
    (expect (= "Ctrl+G" (keymap/label-or-palette :open-dirs)))
    (expect (= "Ctrl+X" (keymap/label-or-palette :open-resources)))
    ;; A palette-only verb falls back to the palette chord (now Ctrl+]).
    (expect (= keymap/palette-chord (keymap/label-or-palette :search-open)))
    (expect (= keymap/palette-chord (keymap/label-or-palette :toggle-voice-recording))))
  (it "bindings have unique chord letters (no shadowing)"
    (let [keys (map :key keymap/bindings)]
      (expect (= (count keys) (count (distinct keys))))))
  (it "NO binding collides with an emacs editing key (the hard requirement)"
    ;; Every emacs editing letter + Ctrl+C must stay free of app verbs.
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
  (it "the palette trigger is NON-letter (Ctrl+]) — can't collide with any letter chord"
    (expect (seq keymap/palette-trigger-chars))
    (expect (not-any? #(Character/isLetter ^char %) keymap/palette-trigger-chars)))
  (it "picker reorder reuses the Emacs prev/next-line keys (intentional modal reuse)"
    ;; A picker is modal — the text editor never runs at the same time — so
    ;; reusing C-p/C-n for row up/down is consistent, not a clash.
    (expect (= \p keymap/picker-reorder-up))
    (expect (= \n keymap/picker-reorder-down))))
