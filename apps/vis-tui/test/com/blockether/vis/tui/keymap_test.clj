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

(defn- press
  "The result of the C-x chord `keys`, pressed one after the other."
  [& keys]
  (reduce (fn [r k]
            (input/resolve-prefix-key (if (char? k) (KeyStroke. ^Character k false false) k)
                                      (:state r)))
          {:state {:prefix :cx}}
          keys))

(defn- ids
  "Every row id of the C-x band of `group` (nil = the top band) in `db`."
  [db group]
  (set (map :id
            (mapcat :items
                    (:groups (if group (keymap/prefix-spec db group) (keymap/prefix-spec db)))))))

(defdescribe dispatch-table-test
             (it "a group key opens its group, and the next key runs one of its verbs"
                 (let [r (press \s)]
                   (expect (= :continue (:action r)))
                   (expect (= {:prefix :cx :prefix-group :session} (:state r))))
                 (expect (= :new-session (:action (press \s \n))))
                 (expect (= :cycle-model (:action (press \m \m))))
                 (expect (= :pick-reasoning (:action (press \m \r))))
                 (expect (= :search-open (:action (press \b \f))))
                 (expect (= :pick-file (:action (press \a \a))))
                 (expect (= :toggle-voice-conversation (:action (press \v \c))))
                 (expect (= :queue-edit (:action (press \q \e))))
                 (expect (= {} (:state (press \s \n))) "the verb ends the prefix"))
             (it "the same letter runs a different verb in each group"
                 (expect (= [:archive-session :pick-file :toggle-agent-dock]
                            (mapv #(:action (press % \a)) [\s \a \b]))))
             (it "the top level keeps help, the palette and the bulk fold"
                 (expect (= :toggle-help (:action (press \h))))
                 (expect (= :show-palette (:action (press \p))))
                 (expect (= :show-palette (:action (press (KeyStroke. \p true false)))))
                 (expect (= :toggle-all-details (:action (press (KeyStroke. KeyType/Tab))))))
             (it "a group key is case-insensitive; a verb key is exact first"
                 (expect (= :new-session (:action (press \S \n))))
                 (expect (= :toggle-zen-mode (:action (press \b \Z))))
                 (expect (= :toggle-detail-labels (:action (press \b \z))))
                 (expect (= :search-open (:action (press \b \F)))))
             (it "C-g, Esc and an unbound key abort the prefix at either level"
                 (doseq [r [(press (KeyStroke. \g true false)) (press \s (KeyStroke. \g true false))
                            (press \s (KeyStroke. KeyType/Escape)) (press \y) (press \s \y)]]
                   (expect (= {:action :continue :state {}} r))))
             (it "no emacs editing key is a direct app verb (action-for returns nil)"
                 (doseq [c emacs-letters]
                   (expect (nil? (keymap/action-for c))))
                 (expect (nil? (keymap/action-for nil)))))

(defdescribe structural-chords-registry-test
             (it "every key is distinct inside its group, and no group key is a top-level verb"
                 (let [pairs
                       (mapv (juxt :group :key) keymap/prefix-commands)

                       top
                       (set (keep #(when-not (:group %) (:key %)) keymap/prefix-commands))

                       group-keys
                       (map :key keymap/prefix-groups)]

                   (expect (= (count pairs) (count (distinct pairs))))
                   (expect (= (count group-keys) (count (distinct group-keys))))
                   (expect (not-any? top group-keys))
                   (expect (not-any? #{keymap/prefix-palette-key} (concat top group-keys)))))
             (it "every verb sits in a group that the keymap knows"
                 (expect (every? (set (map :id keymap/prefix-groups))
                                 (keep :group keymap/prefix-commands)))))

(defdescribe prefix-hydra-test
             ;; C-x is a TRANSIENT band (an Emacs hydra / doom leader), not a dead prefix
             ;; waiting in the echo area: the spec is what the band paints, and it may never
             ;; offer a key the chord itself does not resolve.
             (let [full {:messages [{:role :user}]
                         :codex-session? true
                         :has-images? true
                         :graphical-terminal? true
                         :session {:group-id "g1"}
                         :tabs [{:id :a} {:id :b}]}]
               (it "every band is a legal transient the band can paint"
                   (expect (nil? (tr/check (keymap/prefix-spec {}))))
                   (doseq [{:keys [id key]} keymap/prefix-groups
                           :when (keymap/prefix-group-for key)]

                     (expect (nil? (tr/check (keymap/prefix-spec full id))) (str id))))
               (it "every top row opens its group or runs its verb"
                   (doseq [{:keys [key id]} (mapcat :items (:groups (keymap/prefix-spec full)))]
                     (let [r (press (first key))]
                       (expect (= id (or (get-in r [:state :prefix-group]) (:action r))) key))))
               ;; User report 2026-10-30: C-x had no keys for groups.
               (it "offers the verbs of the session's group only while the session is in one"
                   (expect (= [:group-new :group-rename :group-recolour :group-settings
                               :group-archive :group-delete]
                              (mapv :action (keymap/available-prefix-commands full :group))))
                   (expect (= [:group-new]
                              (mapv :action (keymap/available-prefix-commands {} :group))))
                   (expect (= :group-rename (:action (press \g \r)))))
               (it "every group row runs its verb, and every reachable verb has a row"
                   (doseq [{group :id gkey :key} keymap/prefix-groups
                           :when (not= :extensions group)]

                     (let [items (mapcat :items (:groups (keymap/prefix-spec full group)))]
                       (doseq [{:keys [key id]} items]
                         (expect (= id (:action (press gkey (first key)))) (str gkey key)))
                       (expect (= (set (map :action (keymap/available-prefix-commands full group)))
                                  (disj (set (map :id items)) :queue-send-nth))))))
               ;; Regression, issue: the palette used to buy a whole "Everything else"
               ;; heading of its own. It is the first top-level command, never a group.
               (it "the palette is a top-level command, not a group"
                   (let [items (mapcat :items (:groups (keymap/prefix-spec {})))
                         group-ids (set (map :id keymap/prefix-groups))
                         palette (first (remove #(group-ids (:id %)) items))]

                     (expect (= :show-palette (:id palette)))
                     (expect (= (str keymap/prefix-palette-key) (:key palette)))))
               (it "the top band lists Extensions only when an extension binds a key"
                   (expect (not (contains? (ids {} nil) :extensions)))
                   (expect (nil? (keymap/prefix-group-for \e))))
               (it "full-session and at-turn forks are distinct, shown only with turns"
                   (expect (not (contains? (ids {} :session) :fork-session)))
                   (expect (not (contains? (ids {} :session) :fork-at-turn)))
                   (expect (contains? (ids full :session) :fork-session))
                   (expect (contains? (ids full :session) :fork-at-turn))
                   (expect (= :fork-session (:action (press \s \f))))
                   (expect (= :fork-at-turn (:action (press \s \t)))))))

;; User report: C-x had no key to delete a session, and the band's Session pane did
;; not show one. Backspace and Delete are aliases, because the Mac delete key sends
;; Backspace.
(defdescribe delete-session-chord-test
             (it "C-x s d, C-x s Backspace and C-x s Delete delete the current session"
                 (doseq [key [\d (KeyStroke. KeyType/Backspace) (KeyStroke. KeyType/Delete)]]
                   (expect (= :delete-session (:action (press \s key)))))
                 (expect (= :session-group (:action (press \s \m)))))
             (it "Backspace deletes nothing outside the Session group"
                 (expect (= :continue (:action (press \m (KeyStroke. KeyType/Backspace)))))
                 (expect (= :continue (:action (press (KeyStroke. KeyType/Backspace))))))
             (it "the band shows it under Session, also without turns"
                 (expect (contains? (ids {} :session) :delete-session))
                 (expect (contains? (ids {:messages [{:role :user}]} :session) :delete-session))))

(defdescribe archive-session-chord-test
             (it "C-x s a archives the current session; C-x a a still attaches a file"
                 (expect (= :archive-session (:action (press \s \a))))
                 (expect (= :pick-file (:action (press \a \a))))))

(defdescribe queue-row-chord-test
             ;; User report: the queue's Send buttons had no keys. C-x q 1-9 presses the
             ;; button of that queue row; C-x q s stays the header's Send all now.
             (it "binds the digits 1-9 to queue rows and leaves 0 and letters alone"
                 (expect (= [1 5 9] (mapv keymap/queue-row-index [\1 \5 \9])))
                 (expect (every? nil? (map keymap/queue-row-index [\0 \k \a nil])))
                 (expect (= {:action :queue-send-nth :queue-index 3 :state {}} (press \q \3)))
                 (expect (= "C-x q 3" (keymap/queue-row-chord 3)))
                 (expect (nil? (keymap/queue-row-chord 10)))
                 (expect (= :queue-send-all-now (:action (press \q \s)))))
             (it "a digit sends no queue row outside the Queue group"
                 (expect (= :continue (:action (press \3))))
                 (expect (= :continue (:action (press \s \3))))))

;; Regression for #333: the hydra offered Fast mode in sessions that do not use Codex.
(defdescribe codex-fast-hydra-test
             (it "shows Fast mode only in a session on OpenAI Codex"
                 (expect (not (contains? (ids {} :model) :toggle-codex-fast)))
                 (expect (not (contains? (ids {:codex-session? false} :model) :toggle-codex-fast)))
                 (expect (contains? (ids {:codex-session? true} :model) :toggle-codex-fast))
                 ;; The chord still resolves; its handler gives the reason.
                 (expect (= :toggle-codex-fast (:action (press \m \f))))))

(defdescribe view-images-hydra-test
             (it "shows View images only in a graphical terminal, in a session with a picture"
                 (expect (not (contains? (ids {} :attachments) :view-images)))
                 (expect (not (contains? (ids {:has-images? true} :attachments) :view-images))
                         "A plain terminal cannot show the image viewer")
                 (expect (not (contains? (ids {:graphical-terminal? true} :attachments)
                                         :view-images)))
                 (expect (contains? (ids {:has-images? true :graphical-terminal? true} :attachments)
                                    :view-images))
                 (expect (= :view-images (:action (press \a \v)))))
             (it "shows Open picture in every terminal, in a session with a picture"
                 (expect (not (contains? (ids {} :attachments) :open-newest-image)))
                 (expect (contains? (ids {:has-images? true} :attachments) :open-newest-image))
                 (expect (= :open-newest-image (:action (press \a \o))))))

(defn- with-extension-bindings
  "Run `f` with extension `bindings` in the C-x keymap, then restore the built-in
   keymap."
  [bindings f]
  (try (keymap/set-keybindings! bindings) (f) (finally (keymap/set-keybindings! []))))

(defdescribe
  extension-keybindings-test
  (it "a command binding takes a key in the Extensions group by default"
      (with-extension-bindings
        [{:key "r" :command "/review" :label "Review" :extension "acme"}]
        (fn []
          (let [cmd (keymap/prefix-command-for :extensions \r)]
            (expect (= "/review" (:command cmd)))
            (expect (= "acme" (:extension cmd)))
            (expect (= "C-x e r" (keymap/label-for (:action cmd)))))
          (expect (= :pick-reasoning (:action (press \m \r))) "C-x m r stays the reasoning picker")
          (expect (contains? (ids {} nil) :extensions))
          (expect (= [{:key "r" :label "Review"}]
                     (mapv #(select-keys % [:key :label])
                           (mapcat :items (:groups (keymap/prefix-spec {} :extensions)))))))))
  (it "a group that names a built-in group places the key there, in any case"
      (with-extension-bindings [{:key "x" :command "/x" :label "X" :group "session"}
                                {:key "y" :command "/y" :label "Y" :group "EXTENSIONS"}]
                               (fn []
                                 (expect (= "/x" (get-in (press \s \x) [:keybinding :command])))
                                 (expect (= "/y" (get-in (press \e \y) [:keybinding :command]))))))
  (it "any other group name makes a subgroup of C-x e with the first letter of its name"
      (with-extension-bindings
        [{:key "s" :command "/release start" :label "Start" :group "Release" :extension "acme"}
         {:key "p" :command "/release publish" :label "Publish" :group "release"}]
        (fn []
          (let [opened (press \e \r)]
            (expect (= :continue (:action opened)))
            (expect (= [:extensions "release"] (get-in opened [:state :prefix-group]))))
          (expect (= "/release start" (get-in (press \e \r \s) [:keybinding :command])))
          (expect (= "/release publish" (get-in (press \e \r \p) [:keybinding :command]))
                  "names that differ only in case share one subgroup")
          (expect (= "C-x e r s"
                     (keymap/label-for (:action (keymap/prefix-command-for [:extensions "release"]
                                                                           \s)))))
          (expect (contains? (ids {} nil) :extensions) "a subgroup alone shows C-x e")
          (expect (= [{:key "r" :label "Release…"}]
                     (mapv #(select-keys % [:key :label])
                           (mapcat :items (:groups (keymap/prefix-spec {} :extensions))))))
          (let [spec (keymap/prefix-spec {} [:extensions "release"])]
            (expect (nil? (tr/check spec)))
            (expect (= "C-x e r — Release" (:title spec)))
            (expect (= ["s" "p"] (mapv :key (mapcat :items (:groups spec)))))))))
  (it "a subgroup takes the next free letter of its name; a direct C-x e key wins"
      (with-extension-bindings
        [{:key "r" :command "/review" :label "Review"}
         {:key "a" :command "/rel" :label "Rel" :group "Release"}
         {:key "a" :command "/rep" :label "Rep" :group "Reports"}]
        (fn []
          (expect (= "/review" (get-in (press \e \r) [:keybinding :command])))
          (expect (= "/rel" (get-in (press \e \e \a) [:keybinding :command])))
          (expect (= "/rep" (get-in (press \e \p \a) [:keybinding :command])))
          (expect (not (str/includes? (keymap/free-prefix-letters) "e"))
                  "a subgroup key is not free"))))
  (it "an action binding with an extension group moves the verb into that subgroup"
      (with-extension-bindings [{:key "n" :action "new-session" :group "Flow"}]
                               (fn []
                                 (expect (= :new-session (:action (press \e \f \n))))
                                 (expect (= "C-x e f n" (keymap/label-for :new-session))))))
  (it "a run binding resolves to its function, named as the gateway lists it"
      (with-extension-bindings
        [{:key "d" :run true :label "Deploy" :group "Release" :extension "acme"}
         {:key "x" :run true :label "Check" :group "Extensions" :extension "acme"}]
        (fn []
          (let [r (press \e \r \d)]
            (expect (= :run-keybinding (:action r)))
            (expect (= {:extension "acme" :group "Release" :key "d"}
                       (get-in r [:keybinding :run]))))
          (expect (= {:extension "acme" :group "Extensions" :key "x"}
                     (get-in (press \e \x) [:keybinding :run]))))))
  (it "C-x e and the bound key resolve to the command, as typed"
      (with-extension-bindings [{:key "E" :command "/review now" :label "Review" :extension "acme"}]
                               (fn []
                                 (let [r (press \e \E)]
                                   (expect (= :run-keybinding (:action r)))
                                   (expect (= "/review now" (get-in r [:keybinding :command])))))))
  (it "an action binding moves a verb inside its group; the old holder keeps only the palette"
      (with-extension-bindings [{:key "s" :action "new-session" :extension "acme"}]
                               (fn []
                                 (expect (= :new-session (:action (press \s \s))))
                                 (expect (= "C-x s s" (keymap/label-for :new-session)))
                                 (expect (nil? (keymap/label-for :show-sessions)))
                                 (expect (= :continue (:action (press \s \n)))
                                         "C-x s n is free after the move")
                                 (expect (not (contains? (ids {} :session) :show-sessions)))
                                 (expect (empty? (keymap/refused-keybindings))))))
  (it "an action binding with a group moves the verb to that group"
      (with-extension-bindings [{:key "n" :action "new-session" :group "Model"}]
                               (fn []
                                 (expect (= "C-x m n" (keymap/label-for :new-session)))
                                 (expect (= :continue (:action (press \s \n)))))))
  (it "a moved verb frees its key for a command"
      (with-extension-bindings [{:key "x" :action "new-session"}
                                {:key "n" :command "/notes" :label "Notes" :group "Session"}]
                               (fn []
                                 (expect (= :new-session (:action (press \s \x))))
                                 (expect (= "/notes"
                                            (get-in (press \s \n) [:keybinding :command]))))))
  (it "names the letters that are still free in a group"
      (let [free (keymap/free-prefix-letters :session)]
        (expect (str/includes? free "E"))
        (expect (not (str/includes? free "n")) "n starts a session")
        (expect (not (str/includes? free "g")) "g is reserved"))
      (expect (str/includes? (keymap/free-prefix-letters) "n") "Extensions starts empty")
      (with-extension-bindings [{:key "L" :command "/apps" :label "Live apps"}]
                               (fn []
                                 (expect (not (str/includes? (keymap/free-prefix-letters) "L"))))))
  (it "refuses reserved, taken, malformed and unknown bindings, with a reason"
      (let [{:keys [refused commands]}
            (keymap/merge-keybindings
              [{:key "g" :command "/x" :label "X"} {:key "5" :command "/x" :label "X"}
               {:key "m" :command "/x" :label "X" :group "Model"}
               {:key "ab" :command "/x" :label "X"} {:key "e" :command "x" :label "X"}
               {:key "e" :action "no-such-verb"} {:key "e" :action "new-session"}
               {:key "E" :action "new-session"}])]
        ;; Action bindings merge first, then command bindings, each in order.
        (expect (= ["the TUI has no such verb" "the verb moved already" "the key is reserved"
                    "the key is reserved" "the key is taken"
                    "the key is not one printable character" "the command does not start with /"]
                   (mapv :reason refused)))
        (expect (= :cycle-model
                   (:action (first (filter #(= [:model \m] [(:group %) (:key %)]) commands)))))))
  (it "an empty binding list restores the built-in keymap"
      (keymap/set-keybindings! [{:key "E" :command "/review" :label "Review"}])
      (keymap/set-keybindings! [])
      (expect (nil? (keymap/prefix-command-for :extensions \E)))
      (expect (= "C-x s n" (keymap/label-for :new-session)))
      (expect (empty? (keymap/refused-keybindings)))))
