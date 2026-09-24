(ns com.blockether.vis.tui.improve
  "Improve — the register Vis keeps about its own behaviour — as a TUI surface:
   the projects it groups work under, the issues inside them, the Markdown
   analysis behind one issue, and the mode that decides whether the register is
   written by hand alone or also reviewed on a schedule of its own.

   The gateway owns the records (`/v1/improve`); everything here is pure shaping
   plus the modal components, so the register can be laid out, grouped, closed
   and reviewed in tests without a daemon. Every write stays with the caller:
   these components answer WHAT the human asked for, never perform it."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]))

(set! *unchecked-math* :warn-on-boxed)

(def modes
  "The three Improve modes, in the order the chooser offers them. `:id` is what
   the gateway stores as `mode`; `:hint` says what choosing it authorizes, so
   nobody turns on scheduled model calls by accident."
  [{:id :off :label "Off" :hint "Improve stays hidden and nothing is reviewed"}
   {:id :human :label "Governed by human" :hint "You write, group and close; no model calls"}
   {:id :automatic
    :label "Automatic"
    :hint "Vis also reviews on a schedule with the model you choose"}])

(defn- text
  "`v` as display text — keywords by name, nil as the empty string."
  [v]
  (cond (nil? v) ""
        (keyword? v) (name v)
        :else (str v)))

(defn- field
  "Read `k` from a gateway document, whether it arrived string- or keyword-keyed."
  [m k]
  (when (map? m) (or (get m k) (get m (keyword k)))))

(defn mode
  "The Improve mode of a settings document as `:off`, `:human` or `:automatic`.
   Anything unknown — including a settings document the daemon could not answer
   — is `:off`: the surface stays hidden until the gateway says otherwise."
  [doc]
  (case (str/lower-case (str/trim (text (field doc "mode"))))
    "human"
    :human

    "automatic"
    :automatic

    :off))

(defn mode-label
  "The human label of a mode id, e.g. `:human` → \"Governed by human\"."
  [id]
  (or (some #(when (= id (:id %)) (:label %)) modes) "Off"))

(defn enabled?
  "Is Improve showing at all? False for `:off`, which keeps the C-x e verb out
   of the hydra and the palette."
  [doc]
  (not= :off (mode doc)))

(defn automatic?
  "Does this settings document authorize scheduled model calls?"
  [doc]
  (= :automatic (mode doc)))

(defn settings
  "Normalize `/v1/improve/settings` into the map the TUI keeps in app-db. Idempotent:
   feeding it back its own result answers the same map, so app-db can hold it."
  [doc]
  (let [interval (or (field doc "interval_minutes") (:interval-minutes doc))]
    {:mode (mode doc)
     :provider (not-empty (str/trim (text (field doc "provider"))))
     :model (not-empty (str/trim (text (field doc "model"))))
     :interval-minutes (if (number? interval) (long interval) (parse-long (text interval)))}))

(defn model-label
  "How the chosen review model reads in the chooser."
  [doc]
  (let [{:keys [provider model]} (settings doc)]
    (if (and provider model) (str provider " / " model) "not chosen")))

(defn interval-label
  "How often Automatic reviews, in words."
  [doc]
  (if-let [minutes (:interval-minutes (settings doc))]
    (str minutes " minutes")
    "not set"))

(defn record
  "One improve record normalized for the TUI. `:status` is `:open` or `:closed`;
   an empty title is still selectable, so it gets a placeholder instead of a
   blank row."
  [m]
  (let [title (str/trim (text (or (field m "title") (:title m))))]
    {:id (or (field m "id") (:id m))
     :entry-id (or (field m "entry_id") (:entry-id m))
     :project-id (or (field m "project_id") (:project-id m))
     :title (if (str/blank? title) "(untitled)" title)
     :content (text (or (field m "content") (:content m)))
     :status
     (if (= "closed" (str/lower-case (text (or (field m "status") (:status m))))) :closed :open)
     :parent-id (or (field m "parent_id") (:parent-id m))
     :session-id (text (or (field m "session_id") (:session-id m)))
     :source-content (text (or (field m "source_content") (:source-content m)))
     :source-ref (or (field m "source_ref") (:source-ref m))
     :version (or (field m "version") (:version m))
     :created-at (text (or (field m "created_at") (:created-at m)))
     :updated-at (text (or (field m "updated_at") (:updated-at m)))}))

(defn records
  "Every record of a `/v1/improve` payload, whether it arrived as a bare list or
   under `records`/`items`."
  [payload]
  (let [raw (cond (sequential? payload) payload
                  (map? payload) (or (field payload "records") (field payload "items") [])
                  :else [])]
    (mapv record raw)))

(defn project-names
  "Project id → name from a `/v1/improve` payload. A project the payload does not
   name still gets a header from its id, so no issue hides under a missing project."
  [payload]
  (let [raw (when (map? payload) (field payload "projects"))]
    (into {}
          (keep (fn [project]
                  (when-let [id (or (field project "id") (:id project))]
                    [id
                     (not-empty (str/trim (text (or (field project "name")
                                                    (field project "title")))))])))
          (if (sequential? raw) raw []))))

(defn project-label
  "The header a project's issues sit under."
  [projects project-id]
  (or (not-empty (text (get projects project-id)))
      (when project-id (str "Project " (text project-id)))
      "No project"))

(defn- by-id [records] (into {} (map (juxt :id identity)) records))

(defn descendant-ids
  "Every record grouped BELOW `id`, transitively. A parent chain that loops back
   on itself stops at the record already seen, so a cycle cannot hang a close
   confirmation."
  [records id]
  (let [children (group-by :parent-id records)]
    (loop [queue (mapv :id (get children id))
           seen #{}]

      (if-let [next-id (first queue)]
        (if (contains? seen next-id)
          (recur (vec (rest queue)) seen)
          (recur (into (vec (rest queue)) (map :id) (get children next-id)) (conj seen next-id)))
        seen))))

(defn ancestor-ids
  "Every record `id` is grouped UNDER, nearest parent first, cycle-safe."
  [records id]
  (let [index (by-id records)]
    (loop [current (:parent-id (get index id))
           seen []]

      (if (and current (not (some #{current} seen)) (contains? index current))
        (recur (:parent-id (get index current)) (conj seen current))
        seen))))

(defn close-plan
  "What pressing Close on `id` actually closes: the record itself plus every
   descendant still open. Closure cascades, so the confirmation has to name the
   whole set before a single record is written."
  [records id]
  (let [index
        (by-id records)

        ids
        (into [id] (sort-by text (descendant-ids records id)))

        affected
        (filterv #(= :open (:status %)) (keep index ids))]

    {:ids (mapv :id affected)
     :descendant-count (long (count (remove #(= id (:id %)) affected)))
     :title (:title (get index id))}))

(defn reopen-plan
  "Reopening is deliberately SAFE: it reopens the record and only the closed
   parents that would otherwise keep it hidden, never a descendant that was
   closed on its own merits."
  [records id]
  (let [index
        (by-id records)

        chain
        (into [id] (ancestor-ids records id))

        affected
        (filterv #(= :closed (:status %)) (keep index chain))]

    {:ids (mapv :id affected)
     :parent-count (long (count (remove #(= id (:id %)) affected)))
     :title (:title (get index id))}))

(defn parent-candidates
  "The records `row` may be grouped under: the same project only, never itself,
   and never one of its own descendants — that would close the tree into a loop."
  [records row]
  (let [blocked (conj (descendant-ids records (:id row)) (:id row))]
    (filterv #(and (not (contains? blocked (:id %))) (= (:project-id %) (:project-id row)))
      records)))

(defn parent-items
  "Rows for the parent chooser: one per candidate, plus the ungrouping row that
   lifts the issue back to the top of its project."
  [records row]
  (into [{:label "— no parent — keep it at the top of the project" :parent-id nil}]
        (map (fn [candidate]
               {:label (:title candidate)
                :hint (str "#" (text (:id candidate)))
                :parent-id (:id candidate)}))
        (parent-candidates records row)))

(defn project-choices
  "Rows for the project chooser a NEW improvement is filed into: `No project`
   first, then every project the gateway knows, merged with the headers the
   register itself carries. A project whose issues are all closed — or that has
   no issues at all yet — never appears in the register, so the gateway's own
   list is what keeps it choosable."
  [projects gateway-projects]
  (let [named (reduce (fn [acc project]
                        (if-let [id (or (field project "id") (:id project))]
                          (update acc
                                  id
                                  #(or (not-empty %)
                                       (not-empty (str/trim (text (or (field project "name")
                                                                      (field project "title")))))))
                          acc))
                      (into {}
                            (map (fn [[id name]]
                                   [id (not-empty (str/trim (text name)))]))
                            projects)
                      (if (sequential? gateway-projects) gateway-projects []))]
    (into [{:label (project-label named nil) :project-id nil}]
          (map (fn [id]
                 {:label (project-label named id) :project-id id}))
          (sort-by #(str/lower-case (project-label named %)) (keys named)))))

(defn- row-sort-key [record] [(if (= :open (:status record)) 0 1) (str/lower-case (:title record))])

(defn- issue-rows
  [record children depth]
  (let [kids (sort-by row-sort-key (get children (:id record)))]
    (into [{:kind :issue
            :record record
            :depth (long depth)
            :child-count (long (count kids))
            :label (str (apply str (repeat (* 2 (long depth)) \space))
                        (if (= :closed (:status record)) "✓ " "• ")
                        (:title record)
                        (when (seq kids) (str "  +" (count kids)))
                        "  #"
                        (text (:id record)))}]
          (mapcat #(issue-rows % children (inc (long depth))))
          kids)))

(defn browser-rows
  "Display rows of the whole register: a header per project, then its issues with
   every grouped child indented under its parent. Open issues come first; a closed
   parent still carries its children, so a cascade stays visible after it ran."
  [records projects]
  (let [grouped
        (group-by :project-id records)

        ordered
        (sort-by (fn [[project-id items]]
                   [(if project-id 0 1) (str/lower-case (project-label projects project-id))
                    (long (count items))])
                 grouped)]

    (into []
          (mapcat
            (fn [[project-id items]]
              (let [ids
                    (set (map :id items))

                    children
                    (group-by :parent-id items)

                    roots
                    (sort-by row-sort-key (filterv #(not (contains? ids (:parent-id %))) items))

                    open-count
                    (long (count (filter #(= :open (:status %)) items)))]

                (into [{:kind :project
                        :project-id project-id
                        :label (str (project-label projects project-id)
                                    " · " open-count
                                    " open of " (count items))}]
                      (mapcat #(issue-rows % children 0))
                      roots))))
          ordered)))

(defn browser-display
  "`browser-rows` with the rows that explain an empty or unavailable register.
   An empty register is not a failure: it says so and offers the first record."
  [records projects load-error]
  (let [rows (browser-rows records projects)]
    (cond-> []
      load-error
      (conj {:kind :notice :label load-error})

      (seq rows)
      (into rows)

      (and (empty? rows) (not load-error))
      (conj {:kind :notice
             :label "Nothing recorded yet — press n to write the first improvement"}))))

(defn detail-title
  "The title bar of one record's Markdown view."
  [record]
  (str "Improve #" (text (:id record)) " · " (if (= :closed (:status record)) "closed" "open")))

(defn detail-markdown
  "One record as Markdown: its own analysis first, then the provenance the
   register keeps — what was observed, which session saw it and when it moved."
  [record project-name]
  (let [provenance (cond-> []
                     (seq (text project-name))
                     (conj (str "- Project: " (text project-name)))

                     (:parent-id record)
                     (conj (str "- Grouped under: #" (text (:parent-id record))))

                     (seq (:session-id record))
                     (conj (str "- Session: " (:session-id record)))

                     (:entry-id record)
                     (conj (str "- Council entry: " (text (:entry-id record))))

                     (seq (:created-at record))
                     (conj (str "- Recorded: " (:created-at record)))

                     (seq (:updated-at record))
                     (conj (str "- Updated: " (:updated-at record))))]
    (str/join
      "\n"
      (cond-> [(str "# " (:title record)) ""
               (if (str/blank? (:content record)) "_No analysis written yet._" (:content record))]
        (seq provenance)
        (into (cons "" (cons "## Where it came from" (cons "" provenance))))

        (seq (str/trim (:source-content record)))
        (into ["" "## What was observed" "" (str/trim (:source-content record))])))))

(defn- selected-display-index
  "Where the `selected` issue sits among the painted rows (headers included)."
  [display selected]
  (let [issue-indexes (vec (keep-indexed (fn [i row]
                                           (when (= :issue (:kind row)) i))
                                         display))]
    (long (or (nth issue-indexes selected nil) 0))))

(defn- list-measure
  [display footer cols rows-count]
  (let [content-w
        (dlg/default-content-width cols)

        content-h-req
        (min 18 (max 3 (inc (count display))))

        bounds
        (dlg/dialog-bounds cols rows-count content-w content-h-req)

        {:keys [content-top content-h hint-row]}
        (dlg/dialog-layout bounds)]

    {:cols cols
     :rows rows-count
     :footer footer
     :content-w content-w
     :content-h-req content-h-req
     :bounds bounds
     :content-top content-top
     :content-h content-h
     :hint-row hint-row
     :list-h (max 1 (long content-h))}))

(defn- reconcile-list
  [{:keys [selected scroll] :as state} display total list-h]
  (let [selected
        (p/clamp (long selected) 0 (max 0 (dec (long total))))

        display-index
        (long (selected-display-index display selected))

        max-scroll
        (max 0 (- (long (count display)) (long list-h)))

        scroll
        (-> (long scroll)
            (min display-index)
            (max (inc (- display-index (long list-h))))
            (p/clamp 0 max-scroll))]

    (assoc state
      :selected selected
      :scroll scroll)))

(defn- paint-list!
  [g display title
   {:keys [cols rows footer content-w content-h-req bounds content-top content-h hint-row list-h]}
   scroll selected-display]
  (let [{:keys [left inner-w]} bounds]
    (dlg/draw-dialog-chrome! g cols rows title content-w content-h-req)
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc (long left)) content-top inner-w content-h)
    (dotimes [i (min (long list-h) (- (count display) (long scroll)))]
      (let [display-index (+ (long scroll) (long i))
            item (nth display display-index)
            row (+ (long content-top) (long i))]

        (if (#{:project :notice} (:kind item))
          (do (p/set-colors! g t/dialog-hint t/dialog-bg)
              (p/put-str! g
                          (+ (long left) 2)
                          row
                          (p/ellipsize (:label item) (max 1 (- (long inner-w) 3)))))
          (dlg/draw-selectable-row! g
                                    left
                                    row
                                    inner-w
                                    (= display-index (long selected-display))
                                    (:label item)))))
    (dlg/draw-hint-bar! g left hint-row inner-w footer)
    nil))

(def ^:private browser-footer
  "Every chord the register answers, ordered by how GUESSABLE it is. A narrow box
   drops whole TRAILING pairs (`dlg/fit-hint-pairs`), so the verbs nobody can
   guess — the mode chooser and the way back out — stay ahead of the arrows
   anyone would try anyway."
  [["Enter" "read"] ["e" "edit"] ["s" "mode"] ["Esc" "back"] ["c" "close"] ["n" "new"] ["g" "group"]
   ["r" "reopen"] ["↑/↓" "move"]])

(defn browser-modal-component
  "Pure Improve register: every project with its issues, and the keys that act on
   the selected one. Returns `{:action … :row …}` — reading, editing, grouping,
   closing, reopening and the mode chooser are all the CALLER's writes, so a
   cascade is never started by the paint."
  [records projects load-error mode-settings]
  (let [display
        (browser-display records projects load-error)

        issues
        (filterv #(= :issue (:kind %)) display)

        total
        (long (count issues))]

    {:init {:selected 0 :scroll 0}
     :measure (fn [_ cols rows-count]
                (list-measure display browser-footer cols rows-count))
     :reconcile (fn [state {:keys [list-h]}]
                  (reconcile-list state display total list-h))
     :paint (fn [g {:keys [selected scroll]} layout]
              (paint-list! g
                           display
                           (str "Improve · " (mode-label (mode mode-settings)))
                           layout
                           scroll
                           (selected-display-index display selected)))
     :on-key
     (fn [{:keys [selected] :as state} ^KeyStroke key {:keys [list-h]}]
       (let [clamp-selected
             #(p/clamp % 0 (max 0 (dec total)))

             row
             (:record (nth issues selected nil))]

         (condp = (.getKeyType key)
           KeyType/Escape {::dlg/done nil}
           KeyType/ArrowUp (assoc state :selected (clamp-selected (dec (long selected))))
           KeyType/ArrowDown (assoc state :selected (clamp-selected (inc (long selected))))
           KeyType/PageUp
           (assoc state
             :selected (dlg/page-selected-index display selected list-h -1 #(= :issue (:kind %))))
           KeyType/PageDown
           (assoc state
             :selected (dlg/page-selected-index display selected list-h 1 #(= :issue (:kind %))))
           KeyType/Home (assoc state :selected 0)
           KeyType/End (assoc state :selected (max 0 (dec total)))
           KeyType/Enter {::dlg/done (when row {:action :read :row row})}
           KeyType/Character
           (let [c (Character/toLowerCase ^char (.getCharacter key))]
             (case c
               \e
               (if row {::dlg/done {:action :edit :row row}} state)

               \g
               (if row {::dlg/done {:action :group :row row}} state)

               \c
               (if (and row (= :open (:status row))) {::dlg/done {:action :close :row row}} state)

               \r
               (if (and row (= :closed (:status row)))
                 {::dlg/done {:action :reopen :row row}}
                 state)

               \n
               {::dlg/done {:action :new :row row}}

               \s
               {::dlg/done {:action :settings}}

               state))
           state)))}))

(defn settings-rows
  "Rows of the Improve mode chooser: the three modes, then — for Automatic only —
   the model it reviews with, how often, and a single review on demand. Nothing
   schedules a model call in the other two modes, so those rows do not exist there."
  [mode-settings]
  (let [current (mode mode-settings)]
    (cond-> (mapv (fn [{:keys [id label hint]}]
                    {:kind :mode
                     :mode id
                     :current? (= id current)
                     :label (str (if (= id current) "● " "○ ") label " · " hint)})
                  modes)
      (= :automatic current)
      (into [{:kind :model :label (str "  Model · " (model-label mode-settings))}
             {:kind :interval :label (str "  Reviews every " (interval-label mode-settings))}
             {:kind :review :label "  Review now · one review with the chosen model"}]))))

(defn settings-modal-component
  "Pure Improve mode chooser. Returns the chosen intent — `{:action :set-mode
   :mode …}`, `:pick-model`, `:set-interval` or `:review` — and never writes."
  [mode-settings]
  (let [display
        (settings-rows mode-settings)

        total
        (long (count display))]

    {:init {:selected 0 :scroll 0}
     :measure
     (fn [_ cols rows-count]
       (list-measure display [["↑/↓" "move"] ["Enter" "choose"] ["Esc" "close"]] cols rows-count))
     :reconcile (fn [state {:keys [list-h]}]
                  (reconcile-list state display total list-h))
     :paint (fn [g {:keys [selected scroll]} layout]
              (paint-list! g display "Improve mode" layout scroll selected))
     :on-key (fn [{:keys [selected] :as state} ^KeyStroke key {:keys [list-h]}]
               (let [clamp-selected
                     #(p/clamp % 0 (max 0 (dec total)))

                     row
                     (nth display selected nil)]

                 (condp = (.getKeyType key)
                   KeyType/Escape {::dlg/done nil}
                   KeyType/ArrowUp (assoc state :selected (clamp-selected (dec (long selected))))
                   KeyType/ArrowDown (assoc state :selected (clamp-selected (inc (long selected))))
                   KeyType/PageUp (assoc state
                                    :selected (clamp-selected (- (long selected) (long list-h))))
                   KeyType/PageDown (assoc state
                                      :selected (clamp-selected (+ (long selected) (long list-h))))
                   KeyType/Home (assoc state :selected 0)
                   KeyType/End (assoc state :selected (max 0 (dec total)))
                   KeyType/Enter {::dlg/done (case (:kind row)
                                               :mode
                                               {:action :set-mode :mode (:mode row)}

                                               :model
                                               {:action :pick-model}

                                               :interval
                                               {:action :set-interval}

                                               :review
                                               {:action :review}

                                               nil)}
                   state)))}))

(def ^:private page-limit "Records per `/v1/improve` page. The contract caps a window at 200." 200)

(def ^:private max-pages
  "How many windows one register read walks before it stops asking. A register
   this long is already unreadable; the rows in hand beat an endless fetch."
  10)

;;; ── The analysis editor ────────────────────────────────────────────────────
;;
;; Markdown is written in paragraphs, so the analysis behind an improvement is
;; edited in a MULTILINE modal instead of a one-line prompt: Enter adds a line,
;; ^S (or F2) saves, Esc writes nothing. The buffer, the editing chords and the
;; word motion all come from the composer's own text model (`input`), so this is
;; one more modal over the shared editor, never a second editor.

(def ^:private editor-footer [["Enter" "newline"] ["^S" "save"] ["Esc" "cancel"]])

(def ^:private paste-markers
  "The private-use sentinels lanterna wraps a bracketed paste in. They are
   protocol, not text: a marker left in the analysis would be SAVED into the
   record."
  #"[\uE200\uE201]")

(defn editor-state
  "The editor's opening state for `text`: the Markdown already written, held as
   the same `{:lines :crow :ccol}` buffer the composer types into, with the
   cursor at its end. Blank text opens the pristine empty buffer."
  [text]
  (let [start (assoc (input/empty-input) :scroll 0)]
    (if (seq (str text)) (input/paste-text start (str text)) start)))

(defn editor-text
  "Exactly what the editor would save — every line, blank ones included, joined
   the way they were typed."
  [state]
  (input/input->text state))

(defn editor-key
  "One keystroke against the editor state. Answers the NEXT state, `{::dlg/done
   {:text …}}` when the human saves with ^S or F2, and `{::dlg/done nil}` when
   they cancel with Esc or C-g. Enter is a NEWLINE, so a paragraph can be typed
   and nothing saves by accident. A bracketed PASTE is literal text, never a run
   of commands; nothing here writes — the caller owns that."
  [state ^KeyStroke key]
  (let [ktype
        (.getKeyType key)

        modified?
        (or (.isCtrlDown key) (.isAltDown key))

        character
        (when (= KeyType/Character ktype) (.getCharacter key))

        pasted
        (when (:pasting? state)
          (or (.getText key)
              (some-> character
                      str)))]

    ;; Clipboard text is CONTENT, never a command: between the bracketed-paste
    ;; markers a ^S or an Esc inside the payload is typed INTO the Markdown
    ;; instead of saving the analysis or throwing it away.
    (cond (= KeyType/PasteStart ktype) (assoc state :pasting? true)
          (= KeyType/PasteEnd ktype) (dissoc state :pasting?)
          (:pasting? state) (cond (seq pasted)
                                  (input/paste-text state (str/replace pasted paste-markers ""))
                                  (= KeyType/Enter ktype) (input/insert-newline state)
                                  :else state)
          (input/abort-key? key) {::dlg/done nil}
          (= KeyType/F2 ktype) {::dlg/done {:text (editor-text state)}}
          (and character (.isCtrlDown key) (= \s (Character/toLowerCase ^char character)))
          {::dlg/done {:text (editor-text state)}}
          (= KeyType/Enter ktype) (input/insert-newline state)
          (= KeyType/Backspace ktype)
          (if modified? (input/delete-word-backward state) (input/delete-backward state))
          (= KeyType/Delete ktype) (input/delete-forward state)
          (= KeyType/ArrowLeft ktype)
          (if modified? (input/move-word-left state) (input/move-left state))
          (= KeyType/ArrowRight ktype)
          (if modified? (input/move-word-right state) (input/move-right state))
          (= KeyType/ArrowUp ktype) (input/move-up state)
          (= KeyType/ArrowDown ktype) (input/move-down state)
          (= KeyType/Home ktype) (input/move-line-start state)
          (= KeyType/End ktype) (input/move-line-end state)
          character (or (input/emacs-edit key state)
                        (if modified? state (input/insert-char state character)))
          :else state)))

(defn- editor-measure
  [{:keys [lines]} cols rows-count]
  (let [content-w
        (dlg/default-content-width cols)

        content-h-req
        (min 18 (max 6 (inc (count lines))))

        bounds
        (dlg/dialog-bounds cols rows-count content-w content-h-req)

        {:keys [content-top content-h hint-row]}
        (dlg/dialog-layout bounds)]

    {:cols cols
     :rows rows-count
     :footer editor-footer
     :content-w content-w
     :content-h-req content-h-req
     :bounds bounds
     :content-top content-top
     :content-h content-h
     :hint-row hint-row
     :list-h (max 1 (long content-h))}))

(defn- reconcile-editor
  "Keep the line being typed inside the window — the box scrolls to the cursor,
   never the other way round."
  [{:keys [crow scroll] :as state} {:keys [list-h]}]
  (let [crow
        (long crow)

        window
        (long list-h)]

    (assoc state
      :scroll (-> (long (or scroll 0))
                  (min crow)
                  (max (inc (- crow window)))
                  (max 0)))))

(defn- editor-window
  "The slice of `line` that fits `width` CELLS once the cursor's horizontal offset
   is taken off: a long Markdown line scrolls sideways instead of being rewrapped.
   The cut is measured in terminal COLUMNS, so a wide glyph (CJK, emoji) is never
   split in half and never pushes the box border sideways."
  [line h-off width]
  (p/ansi-slice-cols (str line) (long h-off) (long width)))

(defn- caret-cell
  "Where the caret sits on its own line, in CELLS. `ccol` counts Java chars while a
   wide glyph in front of it occupies two columns, so the cursor is placed by
   MEASURED width — otherwise it drifts left of the character being typed."
  ^long [lines crow ccol]
  (let [line
        (str (nth (vec lines) (long crow) ""))

        upto
        (min (long (count line)) (max 0 (long ccol)))]

    (long (p/display-width (subs line 0 upto)))))

(defn- paint-editor!
  [g title {:keys [lines crow ccol scroll]}
   {:keys [cols rows footer content-w content-h-req bounds content-top content-h hint-row list-h]}]
  (let [{:keys [left inner-w]}
        bounds

        text-w
        (max 1 (- (long inner-w) 3))

        caret
        (caret-cell lines crow ccol)

        h-off
        (max 0 (- (long caret) (dec (long text-w))))]

    (dlg/draw-dialog-chrome! g cols rows title content-w content-h-req)
    (p/set-colors! g t/dialog-fg t/dialog-bg)
    (p/fill-rect! g (inc (long left)) content-top inner-w content-h)
    (dotimes [i (min (long list-h) (- (count lines) (long scroll)))]
      (p/put-str! g
                  (+ (long left) 2)
                  (+ (long content-top) (long i))
                  (editor-window (nth lines (+ (long scroll) (long i))) h-off text-w)))
    (dlg/draw-hint-bar! g left hint-row inner-w footer)
    (p/cursor-pos (+ (long left) 2 (- (long caret) (long h-off)))
                  (+ (long content-top) (- (long crow) (long scroll))))))

(defn analysis-editor-component
  "Pure multiline Markdown editor for one improvement's analysis: the register's
   own modal over the composer's text model. Answers `{:text …}` when the human
   saves and nil when they cancel — the WRITE always stays with the caller."
  [title text]
  {:init (editor-state text)
   :measure editor-measure
   :reconcile reconcile-editor
   :paint (fn [g state geom]
            (paint-editor! g title state geom))
   :on-key (fn [state ^KeyStroke key {:keys [list-h]}]
             (if (:pasting? state)
               (editor-key state key)
               (condp = (.getKeyType key)
                 KeyType/PageUp (nth (iterate input/move-up state) (long list-h))
                 KeyType/PageDown (nth (iterate input/move-down state) (long list-h))
                 (editor-key state key))))})

(defn fetch-register!
  "Read the WHOLE register through the facade, one paged window at a time.
   Returns `{:records […] :projects {…}}`, or an `:error` row when the daemon
   cannot answer the FIRST window — an EMPTY register is not an error and must
   not read like one. A window that fails mid-walk keeps the rows already read."
  []
  (loop [after
         nil

         acc
         []

         projects
         {}

         pages
         0]

    (let [payload (vis/improve-records (cond-> {:limit page-limit}
                                         (some? after)
                                         (assoc :after after)))]
      (cond (and (nil? payload) (zero? (long pages)))
            {:records [] :projects {} :error "Improve register unavailable"}
            (nil? payload) {:records acc :projects projects}
            :else (let [window (records payload)
                        projects (merge projects (project-names payload))
                        acc (into acc window)
                        next-after (field payload "after")]

                    (if (and (true? (field payload "has_more"))
                             (seq window)
                             (some? next-after)
                             (not= next-after after)
                             (< (inc (long pages)) (long max-pages)))
                      (recur next-after acc projects (inc (long pages)))
                      {:records acc :projects projects}))))))

(defn fetch-settings!
  "The current Improve settings, or Off when the daemon cannot answer."
  []
  (settings (vis/improve-settings)))

(defn- review-rows
  "The per-project rows of a finished review, whatever else the payload carries."
  [payload]
  (let [rows (field payload "projects")]
    (if (sequential? rows) rows [])))

(defn- review-status
  "What the gateway called this project: `reviewed`, `failed` or `skipped`."
  [row]
  (str/lower-case (str/trim (text (or (field row "status") (:status row))))))

(defn review-failed?
  "Did any project of a finished review FAIL? A review answers with a per-project
   result, so a run that reports a failure must not be painted as a clean save."
  [payload]
  (boolean (some #(= "failed" (review-status %)) (review-rows payload))))

(defn review-summary
  "What one review actually DID, in words a reader can act on. A review reads
   and writes analysis only — nothing is reproduced and no fix is executed — so
   the summary says that instead of implying one."
  [payload]
  (let [counts
        (frequencies (mapv review-status (review-rows payload)))

        part
        (fn [k label]
          (when-let [n (get counts k)]
            (str n " " label)))

        parts
        (remove nil?
          [(part "reviewed" "reviewed") (part "failed" "failed") (part "skipped" "skipped")])]

    (if (seq parts)
      (str "Review finished — " (str/join ", " parts) ". Nothing was reproduced.")
      "Review finished — no project was reviewed.")))

(defn show-browser!
  "Open the register and return the human's chosen action, or nil on close."
  [^TerminalScreen screen records projects load-error mode-settings]
  (dlg/run-modal! screen (browser-modal-component records projects load-error mode-settings)))

(defn show-settings!
  "Open the mode chooser and return the chosen intent, or nil on close."
  [^TerminalScreen screen mode-settings]
  (dlg/run-modal! screen (settings-modal-component mode-settings)))

(defn edit-analysis!
  "Open the analysis editor and answer the Markdown the human saved, or nil when
   they cancelled. An empty string is a SAVED empty analysis; nil is no write."
  [^TerminalScreen screen title text]
  (:text (dlg/run-modal! screen (analysis-editor-component title text))))
