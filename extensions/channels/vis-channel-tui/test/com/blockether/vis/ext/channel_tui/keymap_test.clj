(ns com.blockether.vis.ext.channel-tui.keymap-test
  (:require [clojure.set :as set]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.transient :as tr]
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
                 (expect (= :cycle-model (keymap/prefix-action-for \m)))
                 (expect (= :cycle-model (keymap/prefix-action-for \M)))
                 (expect (= :cycle-reasoning (keymap/prefix-action-for \r)))
                 (expect (= :cycle-verbosity (keymap/prefix-action-for \l)))
                 (expect (= :open-drafts (keymap/prefix-action-for \d)))
                 (expect (nil? (keymap/prefix-action-for \e)))
                 (expect (= :pick-model (keymap/prefix-action-for \c)))
                 (expect (= :show-sessions (keymap/prefix-action-for \s)))
                 (expect (= :fork-session (keymap/prefix-action-for \y)))
                 (expect (nil? (keymap/prefix-action-for \b)))
                 ;; C-x C-f search · C-x C-a attach · C-x C-v voice · C-x C-h help — the
                 ;; second key resolves the same with or without its own Ctrl.
                 (expect (= :search-open (keymap/prefix-action-for \f)))
                 (expect (= :pick-file (keymap/prefix-action-for \a)))
                 (expect (= :toggle-voice-recording (keymap/prefix-action-for \v)))
                 (expect (= :toggle-help (keymap/prefix-action-for \h)))
                 ;; C-x j → jump to bottom (the discoverable keymap for the `↓ latest` chip).
                 (expect (= :recenter (keymap/prefix-action-for \j)))
                 ;; C-x t → fork the session AT a chosen turn; C-x z → vim-style jump labels.
                 (expect (= :fork-at-turn (keymap/prefix-action-for \t)))
                 (expect (= :toggle-detail-labels (keymap/prefix-action-for \z)))
                 (expect (nil? (keymap/prefix-action-for \q)))
                 (expect (= \x keymap/prefix-key)))
             (it "no emacs editing key is a direct app verb (action-for returns nil)"
                 ;; The C-x prefix's second-keys (m/r/v/d/s) live behind C-x — a different
                 ;; keyspace — so they don't shadow the editing chords.
                 (doseq [c emacs-letters]
                   (expect (nil? (keymap/action-for c))))
                 (expect (nil? (keymap/action-for nil))))
             (it "providers opens Providers via C-x o; model picker is C-x c"
                 (expect (= "C-x o" (keymap/label-for :providers)))
                 (expect (= :pick-model (keymap/prefix-action-for \c)))
                 (expect (= "C-x c" (keymap/label-for :pick-model))))
             (it "label-for renders EVERY prefix verb in the uniform plain C-x <key> form"
                 ;; PLAIN second key (not C-x C-<key>): Ctrl+S is tty flow-control and
                 ;; Ctrl+M is Enter, so a Ctrl'd second key is unusable for some letters.
                 (expect (= "C-x m" (keymap/label-for :cycle-model)))
                 (expect (= "C-x r" (keymap/label-for :cycle-reasoning)))
                 (expect (= "C-x l" (keymap/label-for :cycle-verbosity)))
                 (expect (= "C-x f" (keymap/label-for :search-open)))
                 (expect (= "C-x a" (keymap/label-for :pick-file)))
                 (expect (= "C-x v" (keymap/label-for :toggle-voice-recording)))
                 (expect (= "C-x d" (keymap/label-for :open-drafts)))
                 (expect (nil? (keymap/label-for :open-resources)))
                 (expect (= "C-x h" (keymap/label-for :toggle-help)))
                 (expect (= "C-x c" (keymap/label-for :pick-model)))
                 (expect (nil? (keymap/label-for :no-such-action))))
             (it "label-or-palette returns the registered C-x chord"
                 (expect (= "C-x f" (keymap/label-or-palette :search-open)))
                 (expect (= "C-x v" (keymap/label-or-palette :toggle-voice-recording)))
                 (expect (= "C-x c" (keymap/label-or-palette :pick-model))))
             (it "bindings is empty, so nothing collides with an emacs editing key"
                 (expect (empty? keymap/bindings))
                 (let
                   [reserved
                    (conj emacs-letters \c)

                    keys
                    (set (map :key keymap/bindings))]

                   (expect (empty? (set/intersection reserved keys))))))

(defdescribe structural-chords-registry-test
             ;; keymap.clj is the ONE registry for vis-side chords (editing keys are the
             ;; sole exception — they live in lanterna's TextEditKeymap). Lock the
             ;; structural keys so nothing drifts into a clash.
             (it "help and model picking live in the C-x prefix; C-g remains the direct abort"
                 (expect (= :toggle-help (keymap/prefix-action-for \h)))
                 (expect (= "C-x h" (keymap/label-for :toggle-help)))
                 ;; The plain `c` after C-x is distinct from the direct Ctrl+C quit chord.
                 (expect (= :pick-model (keymap/prefix-action-for keymap/quit-key)))
                 (expect (= :open-magit (keymap/prefix-action-for keymap/abort-key))))
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
                 (expect (= \x keymap/palette-meta-key))))

(defdescribe
  prefix-hydra-test
  ;; C-x is a TRANSIENT band (an Emacs hydra / doom leader), not a dead prefix
  ;; waiting in the echo area: the spec is what the band paints, and it may never
  ;; offer a key the chord itself does not resolve.
  (let
    [spec (fn [db]
            (keymap/prefix-spec db))]
    (it "is a legal transient the band can paint" (expect (nil? (tr/check (spec {})))))
    (it "every row is a real C-x chord, and every reachable verb has a row"
        (let [items (mapcat :items (:groups (spec {})))]
          (expect (seq items))
          (doseq [{:keys [key id]} items]
            (expect
              (= id
                 (if (= id :show-palette) :show-palette (keymap/prefix-action-for (first key))))))
          (expect (= (set (map :action (keymap/available-prefix-commands {})))
                     (disj (set (map :id items)) :show-palette)))))
    ;; Regression, issue: the palette used to buy a whole "Everything else"
    ;; heading of its own — it is a TOOL, and it is the last one in that column.
    (it "the palette is the last tool, not a heading of its own"
        (let [tools (first (filter #(= "Tools" (:title %)) (:groups (spec {}))))]
          (expect (some? tools))
          (expect (= :show-palette (:id (last (:items tools)))))
          (expect (= (str keymap/prefix-palette-key) (:key (last (:items tools)))))
          (expect (= ["Tools"]
                     (mapv :title
                           (filter (fn [g]
                                     (some #(= :show-palette (:id %)) (:items g)))
                                   (:groups (spec {}))))))))
    (it "context-only verbs appear only where they can act"
        (let
          [ids (fn [db]
                 (set (map :id (mapcat :items (:groups (spec db))))))]
          (expect (not (contains? (ids {}) :close-tab)))
          (expect (not (contains? (ids {}) :fork-at-turn)))
          (expect (contains? (ids {:tabs [{:id :a} {:id :b}]}) :close-tab))
          (expect (contains? (ids {:messages [{:role :user}]}) :fork-at-turn))
          ;; palette-only verbs are never painted
          (expect (not (contains? (ids {:tabs [{:id :a} {:id :b}] :messages [{:role :user}]})
                                  :fork-session)))))
    (it "every verb declares a heading the hydra knows"
        (expect (every? (set keymap/prefix-groups) (map :group keymap/prefix-commands))))))
