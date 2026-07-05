(ns com.blockether.vis.ext.channel-tui.keymap-test
  (:require [clojure.set :as set]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe chord-label-test
  (it "Emacs notation, lower-cased; named keys pass through — same on every platform"
    (expect (= "C-f" (keymap/chord \f)))
    (expect (= "C-r" (keymap/chord \r)))
    (expect (= "C-Enter" (keymap/chord "Enter"))))
  (it "the palette opener is C-x p (Emacs C-x prefix + plain p); M-x is the alias"
    (expect (= "C-x p" keymap/palette-chord))
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
    (expect (= :cycle-verbosity (keymap/prefix-action-for \l)))
    (expect (= :open-dirs       (keymap/prefix-action-for \d)))
    (expect (= :open-resources  (keymap/prefix-action-for \s)))
    ;; C-x C-f search · C-x C-a attach · C-x C-v voice · C-x C-h help — the
    ;; second key resolves the same with or without its own Ctrl.
    (expect (= :search-open            (keymap/prefix-action-for \f)))
    (expect (= :pick-file              (keymap/prefix-action-for \a)))
    (expect (= :toggle-voice-recording (keymap/prefix-action-for \v)))
    (expect (= :toggle-help            (keymap/prefix-action-for \h)))
      ;; C-x j → jump to bottom (the discoverable keymap for the `↓ latest` chip).
    (expect (= :recenter               (keymap/prefix-action-for \j)))
      ;; C-x t → vim-style jump labels overlay (toggle one visible fold).
    (expect (= :toggle-detail-labels    (keymap/prefix-action-for \t)))
    (expect (nil? (keymap/prefix-action-for \z)))
    (expect (= \x keymap/prefix-key)))
  (it "no emacs editing key is a direct app verb (action-for returns nil)"
    ;; The C-x prefix's second-keys (m/r/v/d/s) live behind C-x — a different
    ;; keyspace — so they don't shadow the editing chords.
    (doseq [c emacs-letters]
      (expect (nil? (keymap/action-for c))))
    (expect (nil? (keymap/action-for nil))))
  (it "providers is PALETTE-ONLY (no chord); new-session is C-x n"
    (expect (nil? (keymap/label-for :providers)))
    (expect (= :new-session (keymap/prefix-action-for \n)))
    (expect (= "C-x n" (keymap/label-for :new-session))))
  (it "label-for renders EVERY prefix verb in the uniform plain C-x <key> form"
    ;; PLAIN second key (not C-x C-<key>): Ctrl+S is tty flow-control and
    ;; Ctrl+M is Enter, so a Ctrl'd second key is unusable for some letters.
    (expect (= "C-x m" (keymap/label-for :cycle-model)))
    (expect (= "C-x r" (keymap/label-for :cycle-reasoning)))
    (expect (= "C-x l" (keymap/label-for :cycle-verbosity)))
    (expect (= "C-x f" (keymap/label-for :search-open)))
    (expect (= "C-x a" (keymap/label-for :pick-file)))
    (expect (= "C-x v" (keymap/label-for :toggle-voice-recording)))
    (expect (= "C-x s" (keymap/label-for :open-resources)))
    (expect (= "C-x h" (keymap/label-for :toggle-help)))
    (expect (nil? (keymap/label-for :no-such-action))))
  (it "label-or-palette always returns a working chord (prefix or the palette)"
    (expect (= "C-x d" (keymap/label-or-palette :open-dirs)))
    (expect (= "C-x s" (keymap/label-or-palette :open-resources)))
    ;; search / voice now have their own chord, so they return that, not the palette.
    (expect (= "C-x f" (keymap/label-or-palette :search-open)))
    (expect (= "C-x v" (keymap/label-or-palette :toggle-voice-recording)))
    ;; A genuinely palette-only verb still falls back to the palette chord.
    (expect (= keymap/palette-chord (keymap/label-or-palette :providers))))
  (it "bindings is empty, so nothing collides with an emacs editing key"
    (expect (empty? keymap/bindings))
    (let [reserved (conj emacs-letters \c)
          keys     (set (map :key keymap/bindings))]
      (expect (empty? (set/intersection reserved keys))))))

(defdescribe structural-chords-registry-test
  ;; keymap.clj is the ONE registry for vis-side chords (editing keys are the
  ;; sole exception — they live in lanterna's TextEditKeymap). Lock the
  ;; structural keys so nothing drifts into a clash.
  (it "help moved into the C-x prefix (C-x h); C-c/C-g stay out of the prefix"
    ;; help is no longer a standalone const — it's the C-x h prefix command.
    (expect (= :toggle-help (keymap/prefix-action-for \h)))
    (expect (= "C-x h" (keymap/label-for :toggle-help)))
    ;; quit (C-c) and abort (C-g) are always-direct terminal reflexes, never a
    ;; second key behind C-x. (Recenter C-l intentionally shares its letter with
    ;; C-x C-l length — different keyspaces, like the picker-reorder reuse.)
    (expect (nil? (keymap/prefix-action-for keymap/quit-key)))
    (expect (nil? (keymap/prefix-action-for keymap/abort-key))))
  (it "every C-x prefix key is distinct, including the palette's second key"
    (let [keys (mapv :key keymap/prefix-commands)]
      (expect (= (count keys) (count (distinct keys))))
      (expect (not (contains? (set keys) keymap/prefix-palette-key)))))
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
