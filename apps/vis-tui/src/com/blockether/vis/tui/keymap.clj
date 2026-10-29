(ns com.blockether.vis.tui.keymap
  "Single source of truth for the TUI's keyboard shortcuts.

   Shortcuts are CTRL chords, identical on every platform (macOS / Linux).
   The old Alt/Option chords were removed: stock macOS terminals send
   Option+letter as a special character, never an Alt modifier, so those chords
   silently did nothing. Ctrl always reaches the app.

   ONE registry, ONE exception: every vis-side chord is defined HERE — verbs in
   `bindings`, structural keys (palette / help / quit) as the constants below.
   constants below. The SOLE exception is the Emacs EDITING chords
   (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-u/C-w/C-d), which live in lanterna's
   `TextEditKeymap` so they work in EVERY input (prompt + dialogs); they are
   never re-declared here, and no vis chord may reuse one of those letters
   (enforced by `keymap-test`).

   Chord labels use ONE format everywhere: Emacs notation, lower-cased —
   `C-c`, `C-g`, `C-l`, and `C-x <group> <key>` (plain keys) for the prefixed vis
   commands (e.g. `C-x m m`, `C-x s n`). `chord` / `label-for` / `palette-chord` all
   emit this shape, so a hint never drifts between `Ctrl+X` and `C-x`.

   Tiers:
   - `palette-chord` (C-x p) opens the searchable command palette, which
     can run EVERY app verb. It is the discoverable entry point. (A PLAIN
     second key after C-x always reaches the app — unlike Ctrl+S/Ctrl+M — so
     the palette is `C-x p`, not `C-x C-p`.)
   named verbs (model, reasoning, length, search, attach, voice,
     named verbs (model, reasoning, length, search, attach, voice, dirs,
     resources, help) — the C-x prefix keeps them off the editing letters, and a
     PLAIN second key keeps them clear of unusable Ctrl bytes (Ctrl+S=flow
     control, Ctrl+M=Enter).
   - `bindings` (direct Ctrl chords) is EMPTY: every Ctrl letter is an Emacs
     editing key, so vis verbs all live behind the C-x prefix or in the palette.

   Every surface — the input dispatcher, the pickers, footer hints, the help
   overlay, the clickable header chips — reads this namespace, so a shortcut is
   defined once and stays in sync."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.header-model :as vh])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

(def
  ^{:const true
    :doc
    "Label for the chord that opens the searchable Command Palette. It is
   C-x p — the Emacs C-x prefix followed by a PLAIN `p` (`prefix-palette-key`).
   This is the RELIABLE, no-config trigger: both bytes reach the app on macOS
   AND Linux, nothing OS-grabs them, and it touches no editing key. C-x C-p
   still ALSO opens it (old muscle memory), and M-x (Alt/Option+x) is the
   canonical Emacs alias (needs \"Use Option as Meta\" on macOS), but C-x p is
   the displayed binding."}
  palette-chord
  "C-x p")

(defn chord
  "Human label for a Ctrl + `key` chord in Emacs notation, e.g. `(chord \\f)` →
   `\"C-f\"`. Single letters are LOWER-cased (Emacs style); named keys pass
   through verbatim (`(chord \"Enter\")` → `\"C-Enter\"`). This is the ONE chord
   format across the whole TUI — `label-for`/`palette-chord` use the same
   `C-x C-…` shape, so a hint never drifts between `Ctrl+X` and `C-x`. Identical
   on every platform."
  [key]
  (let [s (str key)]
    (str "C-" (if (= 1 (count s)) (str/lower-case s) s))))

;; ── App-verb bindings ────────────────────────────────────────────────────────
;;
;; Direct single-Ctrl-letter verb chords are GONE: every Ctrl letter is now a
;; FAITHFUL Emacs key — editing (C-a/C-e/C-b/C-f/C-p/C-n/C-k/C-d/C-t/C-u/C-w via
;; lanterna `TextEditKeymap`), abort (C-g = keyboard-quit). vis's own commands —
;; INCLUDING help (C-x h) — live behind the Emacs PREFIX key: C-x, then a PLAIN
;; letter (like Emacs `C-x b` / `C-x o`), or in the C-x C-p palette. So
;; `bindings` (direct chords) is now EMPTY.

(def ^:const prefix-key
  "The Emacs prefix key for vis commands: C-x, then a `prefix-groups` key or a
   top-level key of `prefix-commands`. C-x is Emacs's own command prefix, so this
   is the faithful home for vis verbs."
  \x)

(def prefix-groups
  "The C-x groups, in band order. C-x and a group key open the band of that group;
   the next key runs one of its verbs. Two levels give each group its own letters,
   so the C-x keys no longer run out. The Extensions group shows only when an
   extension binds a key."
  [{:id :session :key \s :title "Session"} {:id :model :key \m :title "Model"}
   {:id :view :key \b :title "View"} {:id :attachments :key \a :title "Attachments"}
   {:id :voice :key \v :title "Voice"} {:id :queue :key \q :title "Queue"}
   {:id :extensions :key \e :title "Extensions"}])

(def ^:private group-by-id (into {} (map (juxt :id identity)) prefix-groups))

(def default-binding-group
  "The C-x group of an extension binding without `:group`. A `:group` that names no
   built-in group makes a subgroup of it, such as `[:extensions \"release\"]`."
  :extensions)

(def prefix-commands
  "C-x <group key> <key> → app verb. `:group` is a `prefix-groups` id; a verb
   without `:group` sits on the top level, so C-x and its key run it. `:key` is
   pressed as a PLAIN character: a Ctrl'd key is not reliable, because `Ctrl+S` and
   `Ctrl+Q` are tty flow control and `Ctrl+M` is Enter. Keys are case-sensitive
   inside a group: an exact match wins, then the lower-case key.

   An optional `:show-when` tag gates the band row to the context where the verb can
   act (`verb-available?`). The chord, the palette and help still list every verb."
  [{:action :toggle-help :key \h :label "help"}
   {:action :new-session :key \n :label "new session" :group :session}
   {:action :show-sessions :key \s :label "switch session" :group :session}
   ;; C-x s Backspace and C-x s Delete also delete (`input/resolve-prefix-key`).
   {:action :delete-session :key \d :label "delete session" :group :session}
   {:action :archive-session :key \a :label "archive session" :group :session}
   {:action :fork-session :key \f :label "fork session" :group :session :show-when :has-turns}
   {:action :fork-at-turn :key \t :label "fork at turn" :group :session :show-when :has-turns}
   ;; BLO-167: file the session under one of its project's groups.
   {:action :session-group :key \m :label "move to group" :group :session}
   {:action :session-metrics :key \u :label "session metrics" :group :session}
   {:action :cycle-model :key \m :label "next model" :group :model}
   {:action :pick-model :key \c :label "choose model" :group :model}
   {:action :pick-reasoning :key \r :label "reasoning" :group :model}
   ;; Fast mode acts only on OpenAI Codex, so other sessions do not see it (#333).
   {:action :toggle-codex-fast :key \f :label "fast mode" :group :model :show-when :codex}
   {:action :cycle-verbosity :key \l :label "length" :group :model}
   {:action :toggle-thinking-summary :key \t :label "thinking summary" :group :model}
   {:action :providers :key \p :label "providers" :group :model}
   {:action :search-open :key \f :label "search" :group :view}
   {:action :recenter :key \j :label "jump to bottom" :group :view}
   ;; `z` = vim's fold prefix: the jump-label overlay toggles folds.
   {:action :toggle-detail-labels :key \z :label "label folds" :group :view}
   ;; Zen mode shows only the transcript and the prompt.
   {:action :toggle-zen-mode :key \Z :label "zen mode" :group :view}
   {:action :live-panel :key \p :label "side panel" :group :view :show-when :has-panel}
   {:action :switch-project :key \w :label "project sidebar" :group :view}
   {:action :toggle-agent-dock :key \a :label "agent details" :group :view}
   {:action :pick-file :key \a :label "attach file" :group :attachments}
   {:action :focus-attachments :key \i :label "inspect attachments" :group :attachments}
   ;; Only a graphical terminal can show the image viewer.
   {:action :view-images :key \v :label "view images" :group :attachments :show-when :image-viewer}
   ;; The system viewer works on every terminal.
   {:action :open-newest-image
    :key \o
    :label "open picture"
    :group :attachments
    :show-when :has-images} {:action :toggle-voice-recording :key \v :label "record" :group :voice}
   ;; The conversation MODE speaks the answers of this conversation, and only this one.
   {:action :toggle-voice-conversation :key \c :label "conversation" :group :voice}
   ;; Mark every queued message for the running turn's next step.
   {:action :queue-send-all-now :key \s :label "send all now" :group :queue}
   ;; The keys act on the Queued block until Esc (`input/queue-focus-event`).
   {:action :queue-edit :key \e :label "edit queued" :group :queue}])

(def bindings
  "Direct (single-chord) app verbs — EMPTY now. Every verb moved behind the C-x
   prefix or into the M-x palette, freeing the Ctrl letters for Emacs editing."
  [])

(def ^:private action-by-char
  "Lowercase DIRECT-chord char → action (empty now)."
  (into {} (map (juxt :key :action)) bindings))

(def ^:private binding-by-action (into {} (map (juxt :action identity)) bindings))

(defn- index-commands
  "The lookup maps of the merged C-x commands `commands` and the extension
   `subgroups`."
  [commands refused subgroups]
  {:commands commands
   :refused refused
   :subgroups subgroups
   :subgroup-by-id (into {} (map (juxt :id identity)) subgroups)
   :by-key (into {} (keep #(when (:key %) [[(:group %) (:key %)] %])) commands)
   :by-action (into {} (map (juxt :action identity)) commands)})

(defonce ^:private merged-keymap
  ;; The C-x commands after the key bindings of the session's extensions.
  (atom (index-commands prefix-commands [] [])))

(def reserved-prefix-keys
  "Keys inside a C-x group that no extension can bind: the abort key and the queue
   digits."
  (into #{\g} (map char) (range (int \0) (inc (int \9)))))

(defn- binding-char
  "The one printable ASCII character of binding key `k`, or nil."
  [k]
  (when (and (string? k) (= 1 (count k)))
    (let [c (.charAt ^String k 0)]
      (when (<= 33 (long (int c)) 126) c))))

(defn- named-group
  "The id of the built-in group that `title` names, case-insensitively, or nil."
  [title]
  (let [t (str/lower-case (str/trim (str title)))]
    (some #(when (= t (str/lower-case (:title %))) (:id %)) prefix-groups)))

(defn- binding-group
  "The C-x group of binding `b`. A `:group` that names a built-in group places the
   key there. Any other name places it in an Extensions subgroup; names that differ
   only in case share one. Without `:group`, the key stays in `fallback`."
  [b fallback]
  (let [title (str/trim (str (:group b)))]
    (cond (str/blank? title) (or fallback default-binding-group)
          (named-group title) (named-group title)
          :else [default-binding-group (str/lower-case title)])))

(defn- with-group
  "`cmd` placed in `group`. A subgroup also keeps the title that `b` gives it."
  [cmd group b]
  (cond-> (assoc cmd :group group)
    (vector? group)
    (assoc :group-title (str/trim (str (:group b))))))

(defn- refuse [b reason] {:binding b :reason reason})

(defn- move-verb
  "Phase 1 of the merge: binding `b` moves a built-in verb to its key."
  [{:keys [commands claimed] :as acc} b]
  (let [c
        (binding-char (:key b))

        verb
        (keyword (str (:action b)))

        idx
        (first (keep-indexed #(when (= verb (:action %2)) %1) commands))

        group
        (when idx (binding-group b (:group (nth commands idx))))]

    (cond (nil? c) (update acc :refused conj (refuse b "the key is not one printable character"))
          (reserved-prefix-keys c) (update acc :refused conj (refuse b "the key is reserved"))
          (nil? idx) (update acc :refused conj (refuse b "the TUI has no such verb"))
          (contains? (:moved acc) verb)
          (update acc :refused conj (refuse b "the verb moved already"))
          (contains? claimed [group c])
          (update acc :refused conj (refuse b "another binding has the key"))
          :else (let [commands (mapv #(if (and (= [group c] [(:group %) (:key %)])
                                               (not= verb (:action %)))
                                        (dissoc % :key)
                                        %)
                                     commands)]
                  (-> acc
                      (assoc :commands (update commands
                                               idx
                                               #(cond-> (with-group (assoc % :key c) group b)
                                                  (:label b) (assoc :label (:label b)))))
                      (update :claimed conj [group c])
                      (update :moved conj verb))))))

(defn- add-command
  "Phase 2 of the merge: binding `b` runs a slash command or, with `:run`, an
   extension function from a free key. A run command keeps in `:run` what the
   gateway needs to find the function: the extension, the listed group and the key."
  [{:keys [commands] :as acc} b]
  (let [c
        (binding-char (:key b))

        run?
        (true? (:run b))

        command
        (when-not run? (str/trim (str (:command b))))

        group
        (binding-group b nil)]

    (cond (nil? c) (update acc :refused conj (refuse b "the key is not one printable character"))
          (reserved-prefix-keys c) (update acc :refused conj (refuse b "the key is reserved"))
          (and (not run?) (not (str/starts-with? command "/")))
          (update acc :refused conj (refuse b "the command does not start with /"))
          (some #(= [group c] [(:group %) (:key %)]) commands)
          (update acc :refused conj (refuse b "the key is taken"))
          :else (update acc
                        :commands
                        conj
                        (with-group (cond-> {:action
                                             (keyword "keybinding"
                                                      (str/join "-" (concat (flatten [group]) [c])))
                                             :key c
                                             :label (or (not-empty (str (:label b))) command)
                                             :extension (:extension b)}
                                      run?
                                      (assoc :run
                                        (cond-> {:extension (:extension b) :key (:key b)}
                                          (:group b)
                                          (assoc :group (:group b))))

                                      (not run?)
                                      (assoc :command command))
                                    group
                                    b)))))

(def ^:private key-letters
  "The letters that a key can use, lower case first."
  (concat (map char (range (int \a) (inc (int \z)))) (map char (range (int \A) (inc (int \Z))))))

(defn- subgroup-key
  "The key of the subgroup `title`: its first letter that is not `taken`, else the
   first free letter, or nil."
  [title taken]
  (some #(when-not (or (taken %) (reserved-prefix-keys %)) %)
        (concat (filter #(Character/isLetter (char %)) (str/lower-case title)) key-letters)))

(defn- assign-subgroups
  "Phase 3 of the merge: give each Extensions subgroup a key, in the order that its
   first binding comes. The direct C-x e keys claim their keys first. A subgroup
   without a free key keeps its commands for the palette only."
  [{:keys [commands] :as acc}]
  (let [direct
        (set (keep #(when (= default-binding-group (:group %)) (:key %)) commands))

        groups
        (reduce (fn [gs {:keys [group group-title]}]
                  (if (or (not (vector? group)) (some #(= group (:id %)) gs))
                    gs
                    (conj gs
                          {:id group
                           :title group-title
                           :key (subgroup-key group-title (into direct (keep :key) gs))})))
                []
                commands)

        keyless
        (set (keep #(when-not (:key %) (:id %)) groups))]

    (assoc acc
      :subgroups (filterv :key groups)
      :commands (mapv #(cond-> % (keyless (:group %)) (dissoc :key)) commands)
      :refused (into (:refused acc)
                     (keep #(when (keyless (:group %))
                              (refuse (select-keys % [:key :label :command :run :extension])
                                      "the Extensions group has no free key")))
                     commands))))

(defn merge-keybindings
  "PURE: the C-x commands after extension `bindings` (maps with `:key`, `:command`,
   `:action` or `:run`, `:label`, `:group` and `:extension`, in gateway order).

   First every `:action` binding moves its built-in verb to its key, in the verb's
   own group unless `:group` names another one. A built-in verb that had that key
   keeps only the palette. Then every `:command` or `:run` binding takes a free key in its
   group, Extensions by default. Last, each Extensions subgroup gets a key
   (`assign-subgroups`). A reserved, taken or malformed binding is refused.
   Answers `{:commands [...] :refused [{:binding b :reason text}] :subgroups [...]}`."
  [bindings]
  (let [moved
        (reduce move-verb
                {:commands prefix-commands :claimed #{} :moved #{} :refused []}
                (filter :action bindings))

        added
        (reduce add-command moved (remove :action bindings))]

    (select-keys (assign-subgroups added) [:commands :refused :subgroups])))

(defn set-keybindings!
  "Use extension `bindings` for C-x from now on. Answers the refused bindings."
  [bindings]
  (let [{:keys [commands refused subgroups]} (merge-keybindings bindings)]
    (reset! merged-keymap (index-commands commands refused subgroups))
    refused))

(defn refused-keybindings
  "The extension bindings that the current keymap refused."
  []
  (:refused @merged-keymap))

(defn current-prefix-commands
  "Every C-x command after the extension bindings, built-in verbs first."
  []
  (:commands @merged-keymap))

(defn free-prefix-letters
  "The letters that an extension can still bind in C-x `group` (Extensions by
   default), as one string: lower case first, then upper case."
  ([] (free-prefix-letters default-binding-group))
  ([group]
   (let [taken (cond-> (set (keep #(when (= group (:group %)) (:key %)) (current-prefix-commands)))
                 (= default-binding-group group)
                 (into (map :key) (:subgroups @merged-keymap)))]
     (apply str (remove #(or (taken %) (reserved-prefix-keys %)) key-letters)))))

(defn action-for
  "The action bound to a DIRECT Ctrl + `ch` chord, or nil — always nil now (verbs
   are C-x-prefixed). Kept so the dispatcher's direct-chord clause stays total."
  [ch]
  (when ch (action-by-char (Character/toLowerCase ^char ch))))

(defn- exact-or-lower
  "The value of `f` for `ch`, else for lower-case `ch`."
  [f ch]
  (when ch (or (f ch) (f (Character/toLowerCase ^char ch)))))

(defn prefix-command-for
  "The C-x command bound to `ch` in `group` (nil = the top level), or nil. An exact
   match wins, so C-x b Z is not C-x b z; any other key is lower-cased."
  [group ch]
  (let [by-key (:by-key @merged-keymap)]
    (exact-or-lower #(by-key [group %]) ch)))

(defn prefix-action-for
  "The verb action bound to `ch` in C-x `group` (nil = the top level), or nil."
  [group ch]
  (:action (prefix-command-for group ch)))

(defn prefix-group-for
  "The id of the C-x group that `ch` opens, or nil. A group without a command, such
   as Extensions with no extension binding, opens nothing."
  [ch]
  (let [live (set (keep #(let [g (:group %)] (if (vector? g) (first g) g))
                        (current-prefix-commands)))]
    (exact-or-lower #(some (fn [g]
                             (when (and (= % (:key g)) (live (:id g))) (:id g)))
                           prefix-groups)
                    ch)))

(defn subgroup-for
  "The id of the Extensions subgroup that `ch` opens after C-x e, or nil."
  [ch]
  (let [subgroups (:subgroups @merged-keymap)]
    (exact-or-lower #(some (fn [g]
                             (when (= % (:key g)) (:id g)))
                           subgroups)
                    ch)))

(defn- group-info
  "The `{:id :key :title}` of `group`: a built-in group or an Extensions subgroup."
  [group]
  (if (vector? group) (get-in @merged-keymap [:subgroup-by-id group]) (group-by-id group)))

(defn group-chord
  "The chord that opens C-x `group`, such as `C-x s`, or `C-x e r` for a subgroup."
  [group]
  (str (if (vector? group) (group-chord (first group)) "C-x") " " (:key (group-info group))))

(defn queue-row-index
  "The 1-based queue row that C-x q followed by `ch` toggles for the next step, or
   nil. Only the digits 1-9 bind; row 1 is the next message to send."
  [ch]
  (when (and ch (Character/isDigit ^char ch) (not= \0 ch)) (long (Character/digit ^char ch 10))))

(defn queue-row-chord
  "The chord label that toggles queue row `n` (1-based), or nil past row 9."
  [n]
  (when (<= 1 (long n) 9) (str (group-chord :queue) " " n)))

(defn label-for
  "Display label for `action`'s shortcut, or nil. A top-level verb → `C-x h`; a
   group verb → `C-x <group key> <key>`, such as `C-x s n`. Every key is PLAIN,
   never `C-x C-<key>`: `Ctrl+S`/`Ctrl+Q` are tty flow control and `Ctrl+M` is
   Enter, while a plain letter always reaches the app. A palette-only verb → nil."
  [action]
  (or (when (= action :toggle-all-details) "C-x TAB")
      (some-> (binding-by-action action)
              :key
              chord)
      (when-let [{:keys [key group]} (get-in @merged-keymap [:by-action action])]
        (when key (str (if group (group-chord group) "C-x") " " key)))))

(defn label-or-palette
  "A WORKING chord hint for `action`: its direct/prefix chord if it has one, else
   `palette-chord` (C-x C-p) — every verb is in the palette, so this never
   advertises a dead key."
  [action]
  (or (label-for action) palette-chord))

;; ── Structural chords (handled directly by the input dispatcher / pickers) ───
;; The vis-side chords that are NOT app verbs. Defined here so the dispatcher and
;; the pickers reference one place instead of hardcoding magic chars.
;; Global help is the C-x h verb in `prefix-commands`. The sidebar also owns
;; shorter help keys while it has focus.
(def ^:const quit-key "C-c — quit on an empty draft, else clear it (terminal reflex)." \c)

(def ^:const focus-key
  "C-o — give the keyboard to the next open area: the project sidebar, the chat or
   the side panel. Emacs moves between windows with C-x o; Vis drops the prefix."
  \o)

(defn focus-key?
  "True when `key` is the `focus-key` chord."
  [key]
  (and (instance? KeyStroke key)
       (= KeyType/Character (.getKeyType ^KeyStroke key))
       (.isCtrlDown ^KeyStroke key)
       (not (.isAltDown ^KeyStroke key))
       (= focus-key (Character/toLowerCase (.charValue (.getCharacter ^KeyStroke key))))))

(def ^:const sidebar-help-key "C-h — open or close help while the sidebar has focus." \h)

(def ^:const sidebar-help-alias-key
  "? — sidebar help outside an inline search or project field."
  \?)

(def sidebar-help-hint
  "The sidebar help keys, shared by dispatch, the key list and the help card."
  (str (chord sidebar-help-key) " / " sidebar-help-alias-key))

(def sidebar-commands
  "The project sidebar's own keys while it has focus, in the order help lists
   them. `:key` is the plain character the sidebar reads; `:keys` labels a key
   that is not one plain character. Row menus show the same keys beside their
   items, and the help keys list this table inside the sidebar."
  [{:id :open :keys "Enter" :label "Open the row"} {:id :menu :key \g :label "Open the row menu"}
   {:id :new-session :key \n :label "Start a session here"}
   {:id :new-group :key \G :label "Create a group"}
   {:id :add-project :key \a :label "Add a project"}
   {:id :settings :key \s :label "Open settings for the row"}
   {:id :select :key \space :label "Select or clear a session"}
   {:id :move :key \m :label "Move sessions to a group"}
   {:id :ungroup :key \u :label "Take sessions out of a group"}
   {:id :details :key \d :label "Show session details"}
   {:id :star :key \f :label "Star or unstar a session"}
   {:id :rename :key \R :label "Rename the row"}
   {:id :recolour :key \c :label "Change the group colour"}
   {:id :archive :key \A :label "Archive or restore the row"}
   {:id :show-archived :key \v :label "Show or hide archived rows"}
   {:id :delete :key \D :label "Delete the row"}
   {:id :add-here :key \+ :label "Add a group, session or project here"}
   {:id :search :key \/ :label "Search sessions"} {:id :refresh :key \r :label "Reload projects"}
   {:id :help :key sidebar-help-alias-key :keys sidebar-help-hint :label "Show these keys"}
   {:id :back :keys "Esc · Tab" :label "Return to the chat"}
   {:id :areas :keys (chord focus-key) :label "Move to the next area"}
   {:id :hide :keys (label-for :switch-project) :label "Hide the sidebar"}
   {:id :rows :keys "↑ ↓ · C-p C-n" :label "Move between rows"}
   {:id :fold :keys "← →" :label "Fold or unfold the row"}
   {:id :jump :keys "PgUp PgDn · Home End" :label "Jump a page or to an end"}])

(defn sidebar-key
  "The plain character of the `sidebar-commands` entry `id`, or nil."
  [id]
  (some #(when (= id (:id %)) (:key %)) sidebar-commands))

(defn sidebar-key-label
  "The printed key of a `sidebar-commands` entry: its `:keys` label, `Space` or
   the character itself."
  [{:keys [key keys]}]
  (cond keys keys
        (= \space key) "Space"
        key (str key)
        :else ""))

(def ^:const abort-key
  "C-g — Emacs `keyboard-quit` (abort): cancel a running turn / close a
   dialog / clear the draft. The ctrl-char the dispatcher matches; Esc is its
   mirror. See `abort-keys` for the full advertised set."
  \g)

(def abort-keys
  "The COMPLETE set of keys that abort (cancel a running turn / close a dialog /
   clear the draft): C-g (Emacs `keyboard-quit`) and its Esc mirror. ONE source
   of truth — the dispatcher recognizes all of them and the footer/echo hint
   advertises all of them, so an abort key never fires without being shown, and
   the two never drift apart. Ordered for a stable hint (`abort-hint`)."
  [(chord abort-key) "Esc"])

(defn abort-hint
  "The advertised abort chord for a footer/echo hint — every `abort-keys` label
   joined, e.g. `C-g / Esc`."
  []
  (str/join " / " abort-keys))

(def ^:const recenter-key
  "C-l — Emacs `recenter`: jump the conversation to the bottom + repaint."
  \l)

(def ^:const prefix-palette-key
  "C-x p — after the C-x prefix, this key opens the Command Palette. A PLAIN `p`
   is the primary, reliable, no-config, Emacs-idiomatic palette trigger; a Ctrl'd
   second key (C-x C-p) is ALSO accepted for old muscle memory."
  \p)

(def ^:const palette-meta-key
  "M-x palette alias: Alt/Option + this key also opens the palette (the canonical
   Emacs command launcher; needs \"Use Option as Meta\" on macOS).
   `x` = `execute-extended-command`."
  \x)

;; ── The C-x hydra ───────────────────────────────────────────────────────────
;; C-x is not a dead prefix waiting in the echo area: it opens a
;; TRANSIENT band listing every verb the next keystroke can run, exactly like an
;; Emacs hydra / doom leader. The spec is PURE data here; `dialogs/prefix-band!`
;; paints it and `input/resolve-prefix-key` still owns the keystroke, so the band
;; can never bind a key the chord itself does not.

(defn verb-available?
  "Does a prefix-command's `:show-when` tag hold for the current `db`? Untagged
   verbs always apply; the tagged ones surface in the band ONLY where they can
   act — `:multi-tab` needs a second tab to close, `:has-turns` needs a turn to
   fork at, `:codex` needs a session on OpenAI Codex (the screen marks it as
   `:codex-session?`), `:has-images` needs a picture in the session (the screen
   marks it as `:has-images?`), `:image-viewer` also needs a graphical terminal (the
   screen marks it as `:graphical-terminal?`) and `:has-panel` needs a panel view.
   Resolving a chord does not bypass its handler's feature gate."
  [db {:keys [show-when]}]
  (case show-when
    :multi-tab
    (> (count (vh/project-tabs db)) 1)

    :has-turns
    (boolean (seq (:messages db)))

    :codex
    (true? (:codex-session? db))

    :has-images
    (true? (:has-images? db))

    :image-viewer
    (and (true? (:has-images? db)) (true? (:graphical-terminal? db)))

    :has-panel
    (boolean (some #(= :panel (get-in % [:view :placement])) (:live-views db)))

    true))

(defn available-prefix-commands
  "The C-x commands of `group` (nil = the top level) that can act in `db`, in
   declaration order."
  [db group]
  (filterv #(and (:key %) (= group (:group %)) (verb-available? db %)) (current-prefix-commands)))

(defn- band-row
  "The band row of C-x command `cmd`."
  [{:keys [action key label]}]
  {:key (str key) :type :action :id action :label label})

(defn- visible-subgroups
  "The Extensions subgroups that have a command to show in `db`."
  [db]
  (filterv #(seq (available-prefix-commands db (:id %))) (:subgroups @merged-keymap)))

(defn- group-visible?
  "Does C-x `group` have a row to show in `db`?"
  [db group]
  (boolean (or (seq (available-prefix-commands db group))
               (and (= default-binding-group group) (seq (visible-subgroups db))))))

(defn prefix-spec
  "PURE: the `transient/run!` spec of a C-x band in `db`. Without `group`, the top
   band lists the groups that have a verb to show, then the palette and the
   top-level verbs. With `group`, the band lists the verbs of that group. Queue also
   lists its row digits as one `1` row, and Extensions also lists its subgroups.

   Rows are display-only: the band paints them and hands the raw keystroke back
   to `input/resolve-prefix-key`, so the label and the key can never drift from
   the chord that actually fires."
  ([db]
   {:title (str (chord prefix-key) " — vis commands")
    :groups
    [{:title "Groups"
      :items
      (into []
            (keep
              (fn [{:keys [id key title]}]
                (when (group-visible? db id)
                  {:key (str key) :type :action :id id :label (str (str/lower-case title) "…")})))
            prefix-groups)}
     {:title "Commands"
      :items
      (into
        [{:key (str prefix-palette-key) :type :action :id :show-palette :label "command palette…"}]
        (map band-row)
        (available-prefix-commands db nil))}]})
  ([db group]
   (let [{:keys [title]} (group-info group)]
     {:title (str (group-chord group) " — " title)
      :groups [{:title title
                :items (cond-> (mapv band-row (available-prefix-commands db group))
                         (= group :queue)
                         (conj {:key "1" :type :action :id :queue-send-nth :label "send row 1-9"})

                         (= group default-binding-group)
                         (into (map (fn [{:keys [id key title]}]
                                      {:key (str key) :type :action :id id :label (str title "…")}))
                               (visible-subgroups db)))}]})))
