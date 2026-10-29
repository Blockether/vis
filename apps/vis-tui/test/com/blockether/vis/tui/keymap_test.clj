(ns com.blockether.vis.tui.keymap-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.transient :as tr]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

;; The Emacs editing keys (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-u/C-w/C-d/C-t) are
;; first-class in every input, so NO direct app-verb chord may use those letters.
(def ^:private emacs-letters #{\a \e \b \f \p \n \k \u \w \d \t})

(defdescribe dispatch-table-test
             (it "the C-x prefix resolves the vis commands, case-insensitively"
                 (expect (= :cycle-model (keymap/prefix-action-for \m)))
                 (expect (= :cycle-model (keymap/prefix-action-for \M)))
                 (expect (= :pick-reasoning (keymap/prefix-action-for \r)))
                 (expect (= :toggle-codex-fast (keymap/prefix-action-for \q)))
                 (expect (= :cycle-verbosity (keymap/prefix-action-for \l)))
                 (expect (= :toggle-thinking-summary (keymap/prefix-action-for \x)))
                 ;; `d` files a session under one of its project's groups (BLO-167).
                 (expect (= :session-group (keymap/prefix-action-for \d)))
                 ;; `p` opens the palette in `input`, so it binds no verb.
                 (expect (nil? (keymap/prefix-action-for \p)))
                 (expect (= :pick-model (keymap/prefix-action-for \c)))
                 (expect (= :show-sessions (keymap/prefix-action-for \s)))
                 (expect (= :fork-session (keymap/prefix-action-for \y)))
                 ;; `b` = the voice conversation MODE, one letter from `v` = one recording.
                 (expect (= :toggle-voice-conversation (keymap/prefix-action-for \b)))
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
                 (expect (= :focus-attachments (keymap/prefix-action-for \i)))
                 ;; C-x I (Shift+i) opens the image viewer over every session picture.
                 (expect (= :view-images (keymap/prefix-action-for \I)))
                 (expect (= \x keymap/prefix-key)))
             (it "no emacs editing key is a direct app verb (action-for returns nil)"
                 ;; The C-x prefix's second-keys (m/r/v/s) live behind C-x — a different
                 ;; keyspace — so they don't shadow the editing chords.
                 (doseq [c emacs-letters]
                   (expect (nil? (keymap/action-for c))))
                 (expect (nil? (keymap/action-for nil)))))

(defdescribe structural-chords-registry-test
             ;; keymap.clj is the ONE registry for vis-side chords (editing keys are the
             ;; sole exception — they live in lanterna's TextEditKeymap). Lock the
             ;; structural keys so nothing drifts into a clash.
             (it "help and model picking live in the C-x prefix; C-g remains the direct abort"
                 (expect (= :toggle-help (keymap/prefix-action-for \h)))
                 ;; The plain `c` after C-x is distinct from the direct Ctrl+C quit chord.
                 (expect (= :pick-model (keymap/prefix-action-for keymap/quit-key)))
                 (expect (nil? (keymap/prefix-action-for keymap/abort-key))))
             (it "every C-x prefix key is distinct, including the palette's second key"
                 (let [keys (mapv :key keymap/prefix-commands)]
                   (expect (= (count keys) (count (distinct keys))))
                   (expect (not (contains? (set keys) keymap/prefix-palette-key))))))

(defdescribe
  prefix-hydra-test
  ;; C-x is a TRANSIENT band (an Emacs hydra / doom leader), not a dead prefix
  ;; waiting in the echo area: the spec is what the band paints, and it may never
  ;; offer a key the chord itself does not resolve.
  (let [spec (fn [db]
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
    (it "full-session and at-turn forks are distinct, shown only with turns"
        (let [items (fn [db]
                      (->> (spec db)
                           :groups
                           (filter #(= "Session" (:title %)))
                           first
                           :items))
              by-id (fn [db id]
                      (first (filter #(= id (:id %)) (items db))))
              with-turns {:messages [{:role :user}]}]

          (expect (nil? (by-id {} :fork-session)))
          (expect (nil? (by-id {} :fork-at-turn)))
          (expect (= {:key "y" :type :action :id :fork-session :label "fork session"}
                     (by-id with-turns :fork-session)))
          (expect (= {:key "t" :type :action :id :fork-at-turn :label "fork at turn"}
                     (by-id with-turns :fork-at-turn)))))
    (it "context-only verbs appear only where they can act"
        (let [ids (fn [db]
                    (set (map :id (mapcat :items (:groups (spec db))))))]
          (expect (not (contains? (ids {}) :close-tab)))
          (expect (not (contains? (ids {:tabs [{:id :a} {:id :b}]}) :close-tab)))
          ;; Palette-only verbs are never painted. The voice conversation MODE is
          ;; one of them ON PURPOSE: a fifth verb in Tools re-packs the hydra and
          ;; costs six rows of transcript (see `band-top-row` in screen-test).
          (let [painted (ids {:tabs [{:id :a} {:id :b}] :messages [{:role :user}]})]
            (expect (not (contains? painted :toggle-voice-conversation))))))
    (it "every verb declares a heading the hydra knows"
        (expect (every? (set keymap/prefix-groups) (map :group keymap/prefix-commands))))))

;; User report: C-x had no key to delete a session, and the hydra's Session pane did
;; not show one. Every lower-case letter is taken, so Shift+d carries it, like the
;; sidebar's D. The user rejected a glyph key, and a DEL key column made the hydra
;; too wide for four panes at 100 columns.
(defdescribe delete-session-chord-test
             (it "C-x D, C-x Backspace and C-x Delete delete the current session; C-x d does not"
                 (expect (= \D keymap/prefix-delete-key))
                 (expect (= :session-group (keymap/prefix-action-for \d)))
                 (doseq [key [(KeyStroke. \D false false true) (KeyStroke. KeyType/Backspace)
                              (KeyStroke. KeyType/Delete)]]
                   (expect (= :delete-session
                              (:action (input/resolve-prefix-key key {:prefix :cx}))))))
             (it "the hydra shows it under Session, also without turns"
                 (let [session-ids (fn [db]
                                     (->> (keymap/prefix-spec db)
                                          :groups
                                          (filter #(= "Session" (:title %)))
                                          first
                                          :items
                                          (mapv :id)))]
                   (expect (some #{:delete-session} (session-ids {})))
                   (expect (some #{:delete-session} (session-ids {:messages [{:role :user}]}))))))

(defdescribe
  archive-session-chord-test
  (it "C-x A archives the current session; C-x a still attaches a file"
      (expect (= :archive-session
                 (:action (input/resolve-prefix-key (KeyStroke. \A false false true)
                                                    {:prefix :cx}))))
      (expect (= :pick-file (keymap/prefix-action-for \a))))
  (it "the hydra offers it under Session"
      (expect (some #(= {:key "A" :type :action :id :archive-session :label "archive session"} %)
                    (mapcat :items (:groups (keymap/prefix-spec {})))))))

(defdescribe queue-row-chord-test
             ;; User report: the queue's Send buttons had no keys. C-x 1-9 presses the
             ;; button of that queue row; C-x k stays the header's Send all now.
             (it "binds the digits 1-9 to queue rows and leaves 0 and letters alone"
                 (expect (= [1 5 9] (mapv keymap/queue-row-index [\1 \5 \9])))
                 (expect (every? nil? (map keymap/queue-row-index [\0 \k \a nil])))
                 ;; No letter verb uses a digit, so the two keyspaces cannot collide.
                 (expect (not-any? #(Character/isDigit ^char (:key %)) keymap/prefix-commands)))
             (it "labels rows 1-9 with their chord and gives later rows none"
                 (expect (nil? (keymap/queue-row-chord 10)))))

;; Regression for #333: the hydra offered Fast mode in sessions that do not use Codex.
(defdescribe codex-fast-hydra-test
             (it "shows Fast mode only in a session on OpenAI Codex"
                 (let [rows (fn [db]
                              (set (map :id (mapcat :items (:groups (keymap/prefix-spec db))))))]
                   (expect (not (contains? (rows {}) :toggle-codex-fast)))
                   (expect (not (contains? (rows {:codex-session? false}) :toggle-codex-fast)))
                   (expect (contains? (rows {:codex-session? true}) :toggle-codex-fast))
                   ;; The chord still resolves; its handler gives the reason.
                   (expect (= :toggle-codex-fast (keymap/prefix-action-for \q))))))

(defdescribe
  view-images-hydra-test
  (let [rows (fn [db]
               (set (map :id (mapcat :items (:groups (keymap/prefix-spec db))))))]
    (it "shows View images only in a graphical terminal, in a session with a picture"
        (expect (not (contains? (rows {}) :view-images)))
        (expect (not (contains? (rows {:has-images? true}) :view-images))
                "A plain terminal cannot show the image viewer")
        (expect (not (contains? (rows {:graphical-terminal? true}) :view-images)))
        (expect (contains? (rows {:has-images? true :graphical-terminal? true}) :view-images))
        (expect (= :view-images (keymap/prefix-action-for \I))))
    (it "shows Open picture in every terminal, in a session with a picture"
        (expect (not (contains? (rows {}) :open-newest-image)))
        (expect (contains? (rows {:has-images? true}) :open-newest-image))
        (expect (contains? (rows {:has-images? true :graphical-terminal? true}) :open-newest-image))
        (expect (= :open-newest-image (keymap/prefix-action-for \O)))
        (expect (= :providers (keymap/prefix-action-for \o)) "C-x O stays apart from C-x o"))))

(defn- with-extension-bindings
  "Run `f` with extension `bindings` in the C-x keymap, then restore the built-in
   keymap."
  [bindings f]
  (try (keymap/set-keybindings! bindings) (f) (finally (keymap/set-keybindings! []))))

(defdescribe
  extension-keybindings-test
  (it "a command binding takes a free key under the Extensions heading"
      (with-extension-bindings [{:key "E" :command "/review" :label "Review" :extension "acme"}]
                               (fn []
                                 (let [cmd (keymap/prefix-command-for \E)]
                                   (expect (= "/review" (:command cmd)))
                                   (expect (= "Extensions" (:group cmd)))
                                   (expect (= "acme" (:extension cmd))))
                                 (expect (= :queue-edit (:action (keymap/prefix-command-for \e)))
                                         "C-x E stays apart from C-x e")
                                 (expect (some #(and (= "Extensions" (:title %))
                                                     (some (fn [i]
                                                             (= {:key "E" :label "Review"}
                                                                (select-keys i [:key :label])))
                                                           (:items %)))
                                               (:groups (keymap/prefix-spec {})))))))
  (it "C-x and the bound key resolve to the command, as typed"
      (with-extension-bindings [{:key "E" :command "/review now" :label "Review" :extension "acme"}]
                               (fn []
                                 (let [r (input/resolve-prefix-key (KeyStroke. \E false false true)
                                                                   {:prefix :cx})]
                                   (expect (= :run-keybinding (:action r)))
                                   (expect (= "/review now" (get-in r [:keybinding :command])))))))
  (it "an action binding moves a built-in verb; the old holder keeps only the palette"
      (with-extension-bindings
        [{:key "s" :action "new-session" :extension "acme"}]
        (fn []
          (expect (= :new-session (keymap/prefix-action-for \s)))
          (expect (= "C-x s" (keymap/label-for :new-session)))
          (expect (nil? (keymap/label-for :show-sessions)))
          (expect (nil? (keymap/prefix-action-for \n)) "C-x n is free after the move")
          (expect (not (some #(= :show-sessions %)
                             (map :id (mapcat :items (:groups (keymap/prefix-spec {})))))))
          (expect (empty? (keymap/refused-keybindings))))))
  (it "a moved verb frees its key for a command"
      (with-extension-bindings [{:key "e" :action "new-session"}
                                {:key "n" :command "/notes" :label "Notes"}]
                               (fn []
                                 (expect (= :new-session (keymap/prefix-action-for \e)))
                                 (expect (= "/notes" (:command (keymap/prefix-command-for \n)))))))
  (it "names the letters that are still free after C-x"
      (let [free (keymap/free-prefix-letters)]
        (expect (str/includes? free "E"))
        (expect (not (str/includes? free "e")) "e focuses the queue")
        (expect (not (str/includes? free "T")) "T shows the agent table")
        (expect (str/includes? free "L"))
        (expect (not (str/includes? free "g")) "g is reserved")
        (expect (not (str/includes? free "i")) "i inspects attachments"))
      (with-extension-bindings [{:key "L" :command "/apps" :label "Live apps"}]
                               (fn []
                                 (expect (not (str/includes? (keymap/free-prefix-letters) "L"))))))
  (it "refuses reserved, taken, malformed and unknown bindings, with a reason"
      (let [{:keys [refused commands]}
            (keymap/merge-keybindings
              [{:key "p" :command "/x" :label "X"} {:key "5" :command "/x" :label "X"}
               {:key "m" :command "/x" :label "X"} {:key "ab" :command "/x" :label "X"}
               {:key "e" :command "x" :label "X"} {:key "e" :action "no-such-verb"}
               {:key "e" :action "new-session"} {:key "E" :action "new-session"}])]
        ;; Action bindings merge first, then command bindings, each in order.
        (expect (= ["the TUI has no such verb" "the verb moved already" "the key is reserved"
                    "the key is reserved" "the key is taken"
                    "the key is not one printable character" "the command does not start with /"]
                   (mapv :reason refused)))
        (expect (= :cycle-model (:action (first (filter #(= \m (:key %)) commands)))))))
  (it "an empty binding list restores the built-in keymap"
      (keymap/set-keybindings! [{:key "E" :command "/review" :label "Review"}])
      (keymap/set-keybindings! [])
      (expect (nil? (:command (keymap/prefix-command-for \E))))
      (expect (= "C-x n" (keymap/label-for :new-session)))
      (expect (empty? (keymap/refused-keybindings)))))
