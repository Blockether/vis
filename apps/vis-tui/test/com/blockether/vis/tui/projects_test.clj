(ns com.blockether.vis.tui.projects-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.header-model :as model]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.projects :as projects]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.theme :as theme]
            [com.blockether.vis.tui.shared-theme :as shared-theme]
            [com.blockether.vis.tui.theme-test :as theme-test]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlTerminal]))

(def project-a {"id" "a" "name" "Vis" "workspace_root" "/work/vis" "session_count" 2})

(def project-b {"id" "b" "name" "Companion" "workspace_root" "/work/companion" "session_count" 1})

(def browse-listing
  "One `GET /v1/fs` answer: two git working trees and a plain folder beside them."
  {"path" "/work"
   "parent" "/"
   "home" "/work"
   "is_truncated" false
   "entries" [{"name" "vis" "path" "/work/vis" "entry_count" 12 "is_repo" true "branch" "main"}
              {"name" "vis-python-runtime"
               "path" "/work/vis-python-runtime"
               "entry_count" 8
               "is_repo" true
               "branch" "release"}
              {"name" "notes" "path" "/work/notes" "entry_count" 3 "is_repo" false "branch" nil}]})

(defn fixture-db
  "Deterministic production-screen fixture, shared with live HTML review."
  []
  {:tabs [{:id :tab-1 :label "Project sidebar" :project-id "a" :active? true}
          {:id :tab-2 :label "Rendering tests" :project-id "a"}
          {:id :tab-3 :label "Mobile navigation" :project-id "b"}]
   :active-tab-id :tab-1
   :active-project-id "a"
   :launch-project-id "a"
   :session {:id "a1"}
   :input (input/paste-text (input/empty-input) "Keep this draft")
   :messages [{:role :user :text "Add project navigation to the TUI."}]
   :tab-locals {:tab-2 {:session {:id "a2"} :input (input/empty-input)}
                :tab-3 {:session {:id "b1"}
                        :loading? true
                        :gateway-turn-id "background-turn"
                        :input (input/paste-text (input/empty-input) "Mobile draft")
                        :pending-sends [{:text "Continue tests" :client-id "queued"}]}}
   :project-sidebar {:open? true :focused? true :index 1 :items [project-a project-b]}})

(defn attention-fixture-db
  "One waiting tab and a separate running tab in a background project."
  []
  (-> (fixture-db)
      (update :tabs conj {:id :tab-4 :label "Snapshot tests" :project-id "b"})
      (assoc-in [:tab-locals :tab-4]
                {:session {:id "b2"} :loading? true :gateway-turn-id "second-background-turn"})
      (assoc-in [:tab-locals :tab-3 :human-input]
                (hi/init-form {:id "request-b"
                               :session-id "b1"
                               :title "Choose platform"
                               :fields [{:id "platform" :type :plaintext :label "Platform"}]
                               :is-cancellable true}))))

(defn news-fixture-db
  "Waiting, running and a finished unread reply in one background project."
  []
  (-> (attention-fixture-db)
      (update :tabs conj {:id :tab-5 :label "Keyboard navigation" :project-id "b" :unread? true})
      (assoc-in [:tab-locals :tab-5]
                {:session {:id "b3"}
                 :messages [{:role :assistant
                             :text "Keyboard navigation is ready."
                             :content [{:type :text :text "Keyboard navigation is ready."}]}]})))

(deftest project-view-isolation-test
  (with-redefs [state/app-db (atom (fixture-db))]
    (let [background (get-in @state/app-db [:tab-locals :tab-3])]
      (state/dispatch [:select-tab-index :next])
      (is (= :tab-2 (:active-tab-id @state/app-db)))
      (state/dispatch [:select-tab-index :next])
      (is (= :tab-1 (:active-tab-id @state/app-db)))
      (is (= background (get-in @state/app-db [:tab-locals :tab-3])))))
  (with-redefs [state/app-db (atom (fixture-db))]
    (state/dispatch [:select-tab-index 1])
    (state/dispatch [:select-project "b" [] "unused"])
    (is (= :tab-3 (:active-tab-id @state/app-db)))
    (is (= [:tab-3] (mapv :id (model/project-tabs @state/app-db))))
    (is (= "background-turn" (:gateway-turn-id @state/app-db)))
    (is (= [{:text "Continue tests" :client-id "queued"}] (:pending-sends @state/app-db)))
    (state/dispatch [:select-project "a" [] "unused"])
    (is (= :tab-2 (:active-tab-id @state/app-db)))
    (is (= 3 (count (:tabs @state/app-db))))
    (state/dispatch [:select-tab-index 0])
    (is (= "Keep this draft" (input/input->text (:input @state/app-db))))
    (is (true? (get-in @state/app-db [:tab-locals :tab-3 :loading?])))
    (state/dispatch [:select-project "a" [] "unused"])
    (is (= :tab-1 (:active-tab-id @state/app-db)))))

(deftest project-background-hydration-test
  ;; Regression: slow hydration used to focus its tab after the user had switched away.
  (with-redefs [state/app-db (atom (fixture-db))]
    (state/dispatch [:preallocate-project-tabs
                     [{:session-id "c1" :project-id "c" :label "Pending"}]])
    (state/dispatch [:open-session-tab {:id "c1"} [{:role :user :text "Restored"}] nil true])
    (is (= :tab-1 (:active-tab-id @state/app-db)))
    (is (= "a" (:active-project-id @state/app-db)))
    (is (= "Keep this draft" (input/input->text (:input @state/app-db))))
    (state/dispatch [:select-project "c" [] "unused"])
    (is (= "c1" (str (get-in @state/app-db [:session :id]))))
    (is (= "Restored" (get-in @state/app-db [:messages 0 :text])))))

(deftest project-empty-and-building-test
  (with-redefs [state/app-db (atom (fixture-db))]
    (state/dispatch [:select-project "empty" [] "new-project"])
    (is (= 1 (count (model/project-tabs @state/app-db))))
    (state/dispatch [:select-project "a" [] "unused"])
    (state/dispatch [:bind-built-session "new-project" {:id "new-session"} [] {:root "/work/new"}])
    (is (= "a" (:active-project-id @state/app-db)))
    (is (= "Keep this draft" (input/input->text (:input @state/app-db))))
    (state/dispatch [:select-project "empty" [] "unused"])
    (is (= "new-session" (get-in @state/app-db [:session :id])))
    (is (= 1 (count (model/project-tabs @state/app-db))))))

(deftest project-close-keeps-last-tab-test
  (with-redefs [state/app-db (atom (fixture-db))]
    (state/dispatch [:select-project "b" [] "unused"])
    (state/dispatch [:close-tab])
    (is (= 3 (count (:tabs @state/app-db))))
    (is (= :tab-3 (:active-tab-id @state/app-db)))))

(deftest project-close-selects-visible-neighbor-test
  (let [db
        (fixture-db)

        [a1 a2 b1]
        (:tabs db)]

    (with-redefs [state/app-db (atom (assoc db
                                       :session nil
                                       :tabs [b1 a1 a2 {:id :tab-4 :project-id "a"}]))]
      (state/dispatch [:close-tab])
      (is (= :tab-2 (:active-tab-id @state/app-db)))
      (is (= "background-turn" (get-in @state/app-db [:tab-locals :tab-3 :gateway-turn-id]))))))

(deftest project-startup-does-not-steal-focus-test
  ;; A launch-root lookup can finish after the user has already selected a project.
  (with-redefs [state/app-db
                (atom (dissoc (fixture-db) :active-project-id :launch-project-id))

                vis/gateway-ensure-project-for-root!
                (fn [& _]
                  (state/dispatch [:select-project "b" [] "unused"])
                  project-a)]

    (is (= "a" (#'screen/ensure-launch-project-id!)))
    (is (= "b" (:active-project-id @state/app-db)))
    (is (= "a" (:launch-project-id @state/app-db)))
    (is (= "a" (#'screen/ensure-launch-project-id!)))
    (is (= "background-turn" (:gateway-turn-id @state/app-db)))))

(deftest project-persistence-is-scoped-test
  (let [calls (atom [])]
    (with-redefs [state/app-db (atom (fixture-db))
                  vis/gateway-reorder-project-sessions! #(swap! calls conj [%1 %2])]

      (#'screen/persist-tabs-once!)
      (is (= [["a" ["a1" "a2"]] ["b" ["b1"]]] @calls)))))

(deftest project-request-order-and-race-test
  (let [workers
        (atom [])

        requests
        (atom [])]

    (with-redefs [state/app-db
                  (atom (fixture-db))

                  vis/worker-future
                  (fn [_ f]
                    (swap! workers conj f))

                  vis/gateway-list-session-groups-page
                  (fn [opts]
                    (swap! requests conj [:groups opts])
                    {:groups [] :total 0})

                  vis/gateway-list-sessions-page
                  (fn [opts]
                    (swap! requests conj [:sessions opts])
                    (if (:ids opts)
                      {:sessions [{"id" "a1" "title" "Current"}]}
                      {:sessions [{"id" "saved-outside-tui" "title" "Saved"}]
                       :total 40
                       :has-more true
                       :next-cursor "next"}))]

      (#'screen/request-project! project-a nil)
      (is (empty? @requests) "No gateway I/O or local view creation on input")
      ((first @workers))
      (is (= [:groups :sessions :sessions] (mapv first @requests)))
      (is (= :aside (get-in @requests [1 1 :grouped])))
      (is (<= (get-in @requests [1 1 :limit]) 30))
      (is (= ["a1"] (get-in @requests [2 1 :ids])))
      (is (= "a1" (get-in @state/app-db [:project-sidebar :pages "a" :current "id"])))
      (is (= "saved-outside-tui"
             (get-in @state/app-db [:project-sidebar :pages "a" :sessions 0 "id"])))
      (is (= 3 (count (:tabs @state/app-db))) "Pages must not allocate open views"))))

(deftest project-errors-and-add-test
  (with-redefs [state/app-db
                (atom (fixture-db))

                vis/worker-future
                (fn [_ f]
                  (f))

                vis/gateway-list-session-groups-page
                (fn [_]
                  (throw (ex-info "offline" {})))

                vis/gateway-list-projects
                (fn []
                  (throw (ex-info "offline" {})))]

    (#'screen/request-project!
     {"id" "c"}
     (fn [_]
       (throw (ex-info "Must not open" {}))))
    (is (= "a" (:active-project-id @state/app-db)))
    (is (str/includes? (get-in @state/app-db [:project-sidebar :pages "c" :error]) "Load failed"))
    (#'screen/refresh-projects!)
    (is (= [project-a project-b] (get-in @state/app-db [:project-sidebar :items])))
    (is (false? (get-in @state/app-db [:project-sidebar :loading?]))))
  (let [calls (atom [])]
    (with-redefs-fn {#'state/app-db (atom (fixture-db))
                     #'vis/worker-future (fn [_ f]
                                           (f))
                     #'vis/gateway-list-projects (constantly [project-a project-b])
                     #'vis/gateway-browse-directories (constantly browse-listing)
                     #'vis/gateway-ensure-project-for-root! (fn [path]
                                                              (swap! calls conj path)
                                                              project-b)}
      #(let [select!
             (fn [project]
               (swap! calls conj project)) press!
             (fn [k]
               (#'screen/project-sidebar-key!
                (cap/key-stroke k)
                (fn [_])
                (fn [path]
                  (#'screen/add-project! path select!))
                (fn [_])
                (fn [_])
                (fn [_])))]
         ;; Adding a project is a rail action, not a dialog: `+` opens the field in
         ;; place, the path is typed into it and Enter files it.
         (press! \+) (is (= {:text "" :cursor 0}
                            (select-keys (get-in @state/app-db [:project-sidebar :adding])
                                         [:text :cursor]))) (doseq [c " /work/new "]
                                                              (press! c)) (is (= " /work/new "
                                                                                 (get-in
                                                                                   @state/app-db
                                                                                   [:project-sidebar
                                                                                    :adding
                                                                                    :text])))
         (press! :enter) (is (nil? (get-in @state/app-db [:project-sidebar :adding])))
         (is (= ["/work/new" project-b] @calls))))))

(deftest project-sidebar-input-test
  (let [db (fixture-db)]
    (is (= :switch-project (keymap/prefix-action-for \w)))
    (is (= "C-x w" (keymap/label-for :switch-project)))
    (is (= [:add] (projects/key-action db (cap/key-stroke \+))))
    (is (= [:select project-a] (projects/key-action db (cap/key-stroke :enter))))
    (is (= [:move 1] (projects/key-action db (cap/key-stroke :down))))
    (is (= [:blur] (projects/key-action db (cap/key-stroke :esc))))
    (is (nil? (projects/key-action db (KeyStroke. \x true false))))
    (is (nil? (projects/key-action (assoc-in db [:project-sidebar :open?] false)
                                   (cap/key-stroke \+)))))
  ;; With its field open the rail is an editor: ordinary keys are the path, Enter
  ;; files the trimmed path and Esc closes the field without leaving the rail.
  (let [typing (assoc-in (fixture-db) [:project-sidebar :adding] {:text "/work/vi" :cursor 8})]
    (is (= [:adding {:text "/work/vis" :cursor 9}]
           (projects/key-action typing (cap/key-stroke \s))))
    (is (= [:adding {:text "/work/v" :cursor 7}]
           (projects/key-action typing (cap/key-stroke :backspace))))
    (is (= [:adding {:text "/work/vi" :cursor 0}]
           (projects/key-action typing (cap/key-stroke :home))))
    (is (= [:adding {:text "/work/vi" :cursor 7}]
           (projects/key-action typing (cap/key-stroke :left))))
    (is (= [:add-commit "/work/vi"] (projects/key-action typing (cap/key-stroke :enter))))
    (is (= [:adding nil] (projects/key-action typing (cap/key-stroke :esc))))
    ;; Nothing is listed yet, so there is no completion to highlight or fill in:
    ;; the field is left exactly as it was.
    (is (= [:adding {:text "/work/vi" :cursor 8}]
           (projects/key-action typing (cap/key-stroke :down))))
    (is (= [:noop] (projects/key-action typing (cap/key-stroke :tab))))
    ;; C-x still reaches global navigation from inside the field.
    (is (nil? (projects/key-action typing (KeyStroke. \x true false))))
    ;; A pasted path arrives as one line: newlines and paste markers never enter it.
    (is (= {:text "/work/v is" :cursor 10}
           (projects/add-field-insert {:text "/work/v" :cursor 7} "\n is\uE201")))))

(deftest project-sidebar-width-test
  ;; Keep the sidebar bounded without squeezing the conversation below 60 columns.
  (doseq [[cols width chat-cols] [[24 24 24] [40 40 40] [80 40 80] [99 40 99] [100 40 60]
                                  [120 40 80] [144 48 96] [168 56 112] [240 56 184]]]
    (let [db (fixture-db)]
      (is (= {:left 0
              :width width
              :rows 24
              :chat-cols chat-cols
              :chat-left (if (= cols chat-cols) 0 width)}
             (projects/geometry db cols 24)))
      (is (= cols (projects/chat-cols (assoc-in db [:project-sidebar :open?] false) cols)))))
  (doseq [cols (range 24 241)]
    (let [{:keys [left width chat-left chat-cols]} (projects/geometry (fixture-db) cols 24)]
      (is (<= (min cols 40) width 56))
      (is (zero? left))
      (is (= cols (+ chat-left chat-cols)))
      (is (or (= cols chat-cols) (>= chat-cols 60))))))

(deftest project-sidebar-grid-test
  (doseq [cols [24 26 40 72 80 85 86 96 120 144 240]]
    (let [db (fixture-db)
          capture (cap/capture! {:cols cols
                                 :rows 18
                                 :paint! (fn [{:keys [screen]}]
                                           (projects/paint! (.newTextGraphics screen) db cols 18))})
          text (cap/frame-text capture)
          width (:width (projects/geometry db cols 18))]

      (is (nil? (:error capture)))
      (is (str/includes? text "Projects"))
      (when (>= cols 40)
        (is (str/includes? text "Companion"))
        (is (re-find #"Companion +1 tab · 1 running" text)))
      (when (>= cols 26) (is (str/includes? text "1 running")))
      (let [footer (nth (str/split-lines text) 16)]
        (doseq [label ["↑↓" "↵" "C-x w" "Esc"]]
          (is (str/includes? footer label)))
        (when (>= width 40)
          (doseq [label ["open" "hide" "chat"]]
            (is (str/includes? footer label)))))
      (is (= "├" (get-in capture [:frames 0 15 0 :ch])))
      (is (= :project-add (:kind (.lookup projects/hit-map (- width 8) 1))))
      (is (= :project-hide (:kind (.lookup projects/hit-map (- width 4) 1))))
      (is (= "┌" (get-in capture [:frames 0 0 0 :ch])))
      (is (= "┤" (get-in capture [:frames 0 2 (dec width) :ch])))
      (is (= "┘" (get-in capture [:frames 0 17 (dec width) :ch])))
      (is (= [:select project-b]
             (projects/key-action
               db
               (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 4 5))))))))

(deftest project-sidebar-inline-add-test
  ;; The whole add lives on the rail: its field, its caret and the hint that ends
  ;; it. Nothing here opens a dialog.
  (doseq [cols [40 120]]
    (let [db (assoc-in (fixture-db) [:project-sidebar :adding] {:text "/work/new" :cursor 9})
          caret (atom nil)
          capture (cap/capture! {:cols cols
                                 :rows 18
                                 :paint!
                                 (fn [{:keys [screen]}]
                                   (reset! caret
                                     (projects/paint! (.newTextGraphics screen) db cols 18)))})
          lines (str/split-lines (cap/frame-text capture))]

      (is (nil? (:error capture)))
      (is (str/includes? (nth lines 3) "› /work/new"))
      (is (str/includes? (nth lines 16) "Esc cancel"))
      (is (= 12 (.getColumn ^TerminalPosition @caret)))
      (is (= 3 (.getRow ^TerminalPosition @caret)))))
  ;; A resting rail claims no caret, so the chat keeps its own.
  (let [capture (cap/capture! {:cols 40
                               :rows 18
                               :paint!
                               (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) (fixture-db) 40 18))})]
    (is (nil? (:error capture)))
    (is (nil? (:ret capture)))))

(deftest project-rail-takes-a-pasted-path-test
  ;; A path pasted while the field is open belongs to the field — not to the chat
  ;; composer, and not to attachment intake.
  (with-redefs [state/app-db
                (atom (assoc-in (fixture-db) [:project-sidebar :adding] {:text "/work/" :cursor 6}))

                vis/worker-future
                (fn [_ f]
                  (f))

                vis/gateway-browse-directories
                (constantly browse-listing)]

    (let [before
          (:input @state/app-db)

          field
          #(get-in @state/app-db [:project-sidebar :adding])]

      (#'screen/insert-pasted-text! "vis")
      (is (= {:text "/work/vis" :cursor 9} (select-keys (field) [:text :cursor])))
      ;; The paste lands with the directories it completes to already read.
      (is (= ["vis" "vis-python-runtime"]
             (mapv #(get % "name") (#'projects/add-field-matches (field)))))
      (is (= before (:input @state/app-db)))))
  ;; With no field open the composer keeps the paste.
  (with-redefs [state/app-db (atom (fixture-db))]
    (let [before (:input @state/app-db)]
      (#'screen/insert-pasted-text! "vis")
      (is (nil? (get-in @state/app-db [:project-sidebar :adding])))
      (is (not= before (:input @state/app-db))))))

(deftest project-rail-completes-a-typed-path-test
  ;; The field is a path completer: ONE listing per directory, narrowed locally,
  ;; and Enter adds whichever directory is highlighted.
  (is (= "/work/" (projects/add-field-dir "/work/vi")))
  (is (= "/work/" (projects/add-field-dir "/work/")))
  (is (= "" (projects/add-field-dir "work")))
  (let [field
        (projects/add-field-listing {:text "/work/vis" :cursor 9}
                                    "/work/"
                                    (get browse-listing "entries"))

        db
        #(assoc-in (fixture-db) [:project-sidebar :adding] %)

        action
        (fn [f key]
          (projects/key-action (db f) (cap/key-stroke key)))]

    ;; The last typed segment narrows the listing without another round trip.
    (is (= ["vis" "vis-python-runtime"]
           (mapv #(get % "name") (#'projects/add-field-matches field))))
    (let [[verb filled] (action field :tab)]
      (is (= :adding verb))
      (is (= {:text "/work/vis/" :cursor 10} (select-keys filled [:text :cursor]))))
    (let [[_ first-row]
          (action field :down)

          [_ second-row]
          (action first-row :down)

          [_ released]
          (action second-row \s)]

      (is (= 0 (:index first-row)))
      (is (= 1 (:index second-row)))
      (is (= [:add-commit "/work/vis-python-runtime"] (action second-row :enter)))
      ;; Typing releases the highlight, so Enter adds the typed path again.
      (is (nil? (:index released)))
      (is (= [:add-commit "/work/viss"] (action released :enter)))
      ;; Stepping above the first row hands the keyboard back to the text.
      (is (nil? (:index (second (action first-row :up))))))
    ;; Enter with nothing highlighted still adds what the human typed.
    (is (= [:add-commit "/work/vis"] (action field :enter)))
    ;; A read still in flight says so instead of claiming there is no match.
    (is (true? (:loading? (projects/add-field-listing field "/other/" nil))))
    (is (= [] (:rows (projects/add-field-listing field "/other/" nil))))))

(deftest project-rail-paints-its-completions-test
  ;; While the field is open the rows area belongs to the directories it offers,
  ;; and a git working tree shows the branch that tells it apart from a folder.
  (let [field
        (projects/add-field-listing {:text "/work/" :cursor 6}
                                    "/work/"
                                    (get browse-listing "entries"))

        db
        (assoc-in (fixture-db) [:project-sidebar :adding] field)

        capture
        (cap/capture! {:cols 120
                       :rows 18
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db 120 18))})

        text
        (cap/frame-text capture)

        lines
        (str/split-lines text)]

    (is (nil? (:error capture)))
    (is (str/includes? (nth lines 4) "vis/"))
    (is (str/includes? (nth lines 4) "main"))
    (is (str/includes? (nth lines 6) "notes/"))
    ;; The project list yields its rows to the open field.
    (is (not (str/includes? text "Companion")))
    (is (str/includes? (nth lines 16) "Esc cancel")))
  ;; An empty directory answers in words rather than with a blank rail.
  (let [db
        (assoc-in (fixture-db)
          [:project-sidebar :adding]
          (projects/add-field-listing {:text "/work/zz" :cursor 8} "/work/" []))

        capture
        (cap/capture! {:cols 120
                       :rows 18
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db 120 18))})]

    (is (nil? (:error capture)))
    (is (str/includes? (cap/frame-text capture) "No matching directory"))))

(deftest project-rail-suggests-directories-from-the-gateway-test
  ;; The directories come off the GATEWAY host, one listing per directory, and a
  ;; highlighted row is what Enter adds.
  (let [asked
        (atom [])

        added
        (atom [])]

    (with-redefs [state/app-db
                  (atom (fixture-db))

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-browse-directories
                  (fn [dir]
                    (swap! asked conj dir)
                    browse-listing)]

      (let [press!
            (fn [key]
              (#'screen/project-sidebar-key!
               (cap/key-stroke key)
               (fn [_])
               #(swap! added conj %)
               (fn [_])
               (fn [_])
               (fn [_])))

            field
            #(get-in @state/app-db [:project-sidebar :adding])]

        (press! \+)
        ;; A blank field asks for the gateway user's own home.
        (is (= [""] @asked))
        (is (= 3 (count (:rows (field)))))
        (is (false? (:loading? (field))))
        ;; Typing inside the SAME directory reuses the listing it already has.
        (press! \v)
        (press! \i)
        (is (= [""] @asked))
        ;; Tab fills the obvious match in and reads the directory it opened.
        (press! :tab)
        (is (= "/work/vis/" (:text (field))))
        (is (= ["" "/work/vis/"] @asked))
        (press! :down)
        (press! :enter)
        (is (= ["/work/vis"] @added))
        (is (nil? (field)))))))

(deftest project-sidebar-footer-error-test
  (let [db
        (assoc-in (fixture-db) [:project-sidebar :error] "Project lookup failed")

        capture
        (cap/capture! {:cols 144
                       :rows 18
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db 144 18))})

        lines
        (str/split-lines (cap/frame-text capture))]

    (is (str/includes? (nth lines 14) "Project lookup failed"))
    (doseq [label ["↑↓" "↵" "C-x w" "Esc"]]
      (is (str/includes? (nth lines 16) label)))
    (is (= 8
           (count (projects/visible-entries
                    (assoc-in db [:project-sidebar :items] (vec (repeat 30 project-a)))
                    16))))))

(deftest project-sidebar-label-width-test
  (doseq [[cols index label fits?] [[40 0 "vis-python-runtime" true]
                                    [40 1 "vis-extension-center-demo" false]
                                    [168 1 "vis-extension-center-demo" true]]]
    (let [db (assoc-in (fixture-db) [:project-sidebar :items index "name"] label)
          capture (cap/capture! {:cols cols
                                 :rows 18
                                 :paint! (fn [{:keys [screen]}]
                                           (projects/paint! (.newTextGraphics screen) db cols 18))})
          row (nth (str/split-lines (cap/frame-text capture)) (+ 4 index))]

      (is (nil? (:error capture)))
      (is (= fits? (str/includes? row label)))
      (is (= (not fits?) (str/includes? row "…"))))))

(deftest project-name-reads-as-a-home-path-test
  ;; A project nobody renamed is named by its own ROOT, and the rail painted that
  ;; absolute path in full while every other path in the TUI reads `~/…`.
  (let [home
        (System/getProperty "user.home")

        cols
        40

        rows
        18

        db
        (assoc-in (fixture-db) [:project-sidebar :items 0 "name"] (str home "/CryptoSyf"))

        capture
        (cap/capture! {:cols cols
                       :rows rows
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db cols rows))})

        row
        (nth (str/split-lines (cap/frame-text capture)) 4)]

    (is (= "~/CryptoSyf" (projects/project-label {"name" (str home "/CryptoSyf")})))
    (is (= "Vis" (projects/project-label project-a)))
    (is (= "/opt/shared/vis" (projects/project-label {"name" "/opt/shared/vis"})))
    (is (= "Untitled project" (projects/project-label {})))
    (is (nil? (:error capture)))
    (is (str/includes? row "~/CryptoSyf"))
    (is (not (str/includes? row home)))))

(deftest project-sidebar-overflow-test
  (let [sidebar
        {:items (vec (repeat 50 project-a)) :index 50}

        visible
        (projects/visible-entries {:project-sidebar sidebar} 16)]

    (is (= 9 (count visible)))
    (is (= 50 (:index (last visible))))))

(defn review-terminal
  "Production terminal defaults shared by backend parity and live project review."
  ^HtmlTerminal [cols rows]
  (-> (HtmlTerminal/builder)
      (.initialSize (TerminalSize. cols rows))
      (.defaultForeground theme/text-fg)
      (.defaultBackground theme/terminal-bg)
      (.title "Vis · Projects")
      (.build)))

(deftest project-full-frame-and-input-test
  (with-redefs [timg/images-protocol
                (constantly nil)

                vis/get-router
                (constantly nil)]

    (doseq [cols [40 80 85 86 96 120 144]]
      (let [capture (cap/capture!
                      {:cols cols
                       :rows 24
                       :paint! (fn [{:keys [screen]}]
                                 (let [db (assoc-in (fixture-db) [:project-sidebar :focused?] false)
                                       layout (#'screen/render-frame! screen cols 24 db 1000)]

                                   (#'screen/paint-frame! screen :input cols 24 db 1000 layout)
                                   layout))})
            hidden (cap/capture! {:cols cols
                                  :rows 24
                                  :paint! (fn [{:keys [screen]}]
                                            (#'screen/render-frame!
                                             screen
                                             cols
                                             24
                                             (assoc-in (fixture-db) [:project-sidebar :open?] false)
                                             1000))})]

        (is (nil? (:error capture)))
        ;; A narrow rail overlays the composer: input-only paint must not erase it.
        (is (= (cap/frame-text capture 0) (cap/frame-text capture)))
        (is (str/includes? (cap/frame-text capture) "C-x w hide"))
        (is (= (projects/chat-cols (fixture-db) cols) (get-in capture [:ret :cols])))
        (is (nil? (:error hidden)))
        (is (= cols (get-in hidden [:ret :cols])))
        (is (not (str/includes? (cap/frame-text hidden) "Projects")))
        (is (not-any? #(= :project-select (:kind %)) (.current projects/hit-map)))))))

(deftest project-screen-backend-parity-test
  (with-redefs [timg/images-protocol
                (constantly nil)

                vis/get-router
                (constantly nil)]

    (doseq [db
            [(fixture-db) (attention-fixture-db) (news-fixture-db)]

            cols
            [24 26 40 80 85 86 96 120 144]]

      (with-open [html
                  (review-terminal cols 24)

                  html-screen
                  (doto (TerminalScreen. html) (.startScreen))]

        (let [capture (cap/capture! {:cols cols
                                     :rows 24
                                     :paint! (fn [{:keys [^TerminalScreen screen]}]
                                               (#'screen/render-frame! screen cols 24 db 1000)
                                               (#'screen/render-frame! html-screen cols 24 db 1000)
                                               (is (= (for [y (range 24)
                                                            x (range cols)]

                                                        (.getFrontCharacter screen x y))
                                                      (for [y (range 24)
                                                            x (range cols)]

                                                        (.getFrontCharacter html-screen x y)))))})]
          (is (nil? (:error capture)))
          (is (str/includes? (.renderHtml html) "Projects")))))))

(deftest project-sidebar-dispatch-test
  (let [added (atom [])]
    (with-redefs [state/app-db (atom (fixture-db))
                  vis/gateway-list-projects (constantly [project-a project-b])
                  vis/gateway-browse-directories (constantly browse-listing)
                  vis/worker-future (fn [_ f]
                                      (f))
                  timg/images-protocol (constantly nil)]

      (let [select! #(state/dispatch [:select-project (get % "id") [] "unused"])
            add! #(swap! added conj %)
            refresh! (fn [_])
            menu! (fn [_]
                    (throw (ex-info "Wrong menu action" {})))]

        (is
          (true?
            (#'screen/project-sidebar-key! (cap/key-stroke :down) select! add! refresh! menu! nil)))
        (#'screen/project-sidebar-key! (cap/key-stroke :enter) select! add! refresh! menu! nil)
        (is (= "b" (:active-project-id @state/app-db)))
        (is (= "background-turn" (:gateway-turn-id @state/app-db)))
        (#'screen/project-sidebar-key! (cap/key-stroke \+) select! add! refresh! menu! nil)
        (is (= {:text "" :cursor 0}
               (select-keys (get-in @state/app-db [:project-sidebar :adding]) [:text :cursor])))
        (#'screen/project-sidebar-key! (cap/key-stroke \/) select! add! refresh! menu! nil)
        (#'screen/project-sidebar-key! (cap/key-stroke :enter) select! add! refresh! menu! nil)
        (is (= ["/"] @added))
        (is (nil? (get-in @state/app-db [:project-sidebar :adding])))
        (#'screen/project-sidebar-key! (cap/key-stroke :esc) select! add! refresh! menu! nil)
        (is (false? (get-in @state/app-db [:project-sidebar :focused?])))
        (is (nil?
              (#'screen/project-sidebar-key! (cap/key-stroke \a) select! add! refresh! menu! nil)))
        (let [capture (cap/capture! {:keys [\w]
                                     :paint! (fn [{:keys [screen]}]
                                               (#'screen/resolve-prefix!
                                                screen
                                                @state/app-db
                                                (input/handle-key (KeyStroke. \x true false)
                                                                  (:input @state/app-db))))})]
          (is (nil? (:error capture)))
          (is (= :switch-project (get-in capture [:ret :action]))))
        (#'screen/toggle-project-sidebar!)
        (is (false? (get-in @state/app-db [:project-sidebar :open?])))
        (#'screen/toggle-project-sidebar!)
        (is (true? (get-in @state/app-db [:project-sidebar :open?])))
        (is (= 0 (get-in @state/app-db [:project-sidebar :index])))
        (is (= 3 (count (:tabs @state/app-db))))))))

(deftest project-sidebar-cursor-test
  (with-redefs [timg/images-protocol
                (constantly nil)

                vis/get-router
                (constantly nil)]

    (doseq [cols
            [40 120]

            focused?
            [false true]]

      (let [capture (cap/capture! {:cols cols
                                   :rows 24
                                   :paint!
                                   (fn [{:keys [^TerminalScreen screen]}]
                                     (#'screen/render-frame!
                                      screen
                                      cols
                                      24
                                      (assoc-in (fixture-db) [:project-sidebar :focused?] focused?)
                                      1000)
                                     (.getCursorPosition screen))})]
        (is (nil? (:error capture)))
        (is (= (or focused? (= cols 40)) (nil? (:ret capture))))))))

(deftest project-rail-caret-in-the-render-frame-test
  ;; The whole frame parks the terminal caret in the rail's own field while it is
  ;; open, so a typed path has a cursor without the chat lending one.
  (with-redefs [timg/images-protocol
                (constantly nil)

                vis/get-router
                (constantly nil)]

    (let [db
          (assoc-in (fixture-db) [:project-sidebar :adding] {:text "/work/new" :cursor 9})

          capture
          (cap/capture! {:cols 120
                         :rows 24
                         :paint! (fn [{:keys [^TerminalScreen screen]}]
                                   (#'screen/render-frame! screen 120 24 db 1000)
                                   (.getCursorPosition screen))})]

      (is (nil? (:error capture)))
      (is (= 12 (.getColumn ^TerminalPosition (:ret capture))))
      (is (= 3 (.getRow ^TerminalPosition (:ret capture)))))))

(deftest project-chat-pointer-surface-test
  (with-redefs [timg/images-protocol
                (constantly nil)

                vis/get-router
                (constantly nil)]

    (let [capture (cap/capture!
                    {:cols 144
                     :rows 24
                     :paint!
                     (fn [{:keys [screen]}]
                       (#'screen/render-frame! screen 144 24 (fixture-db) 1000)
                       (let [hit (first (filter #(>= (long (get-in % [:bounds :col] 0)) 48)
                                                (.current interactions/hit-map)))
                             {:keys [col row]} (:bounds hit)
                             mouse (MouseAction. MouseActionType/CLICK_DOWN
                                                 1
                                                 (TerminalPosition. (+ 48 (int col)) (int row)))
                             local ^MouseAction (projects/chat-key mouse 48)]

                         (is (some? hit))
                         (is (= hit
                                (.lookup interactions/hit-map
                                         (.getColumn (.getPosition local))
                                         (.getRow (.getPosition local)))))
                         (is (not= :select (first (projects/key-action (fixture-db) mouse))))))})]
      (is (nil? (:error capture)))))
  (let [mouse
        (MouseAction. MouseActionType/SCROLL_DOWN 0 (TerminalPosition. 70 8) 7)

        local
        ^MouseAction (projects/chat-key mouse 40)]

    (is (= (TerminalPosition. 30 8) (.getPosition local)))
    (is (= 7 (.getCount local)))
    (is (= (.getScrollDelta mouse) (.getScrollDelta local)))
    (is (identical? mouse (projects/chat-key mouse 0))))
  (is (= (cap/key-stroke \a) (projects/chat-key (cap/key-stroke \a) 40))))

(deftest project-surface-cells-and-cursor-test
  (let [capture (cap/capture!
                  {:cols 30
                   :rows 6
                   :paint! (fn [{:keys [^TerminalScreen screen]}]
                             (binding [frame/*column-offset* 10]
                               (.putString (frame/surface-graphics screen 20 6) 0 1 "界 hello")
                               (is (= "界" (.getCharacterString (frame/back-character screen 0 1))))
                               (frame/set-character! screen 5 2 (frame/back-character screen 3 1))
                               (frame/set-cursor! screen (TerminalPosition. 5 2))
                               (is (= (TerminalPosition. 15 2) (.getCursorPosition screen)))
                               (is (= "h" (.getCharacterString (.getBackCharacter screen 15 2))))
                               (frame/set-cursor! screen nil)
                               (is (nil? (.getCursorPosition screen)))))})]
    (is (nil? (:error capture)))
    (is (= "界" (get-in capture [:frames 0 1 10 :ch])))
    (is (= "h" (get-in capture [:frames 0 1 13 :ch])))))

(deftest project-overlay-and-media-origin-test
  (with-redefs [timg/images-protocol
                (constantly nil)

                vis/get-router
                (constantly nil)]

    (doseq [cols [80 144]]
      (let [placed (atom nil)
            capture (cap/capture!
                      {:cols cols
                       :rows 24
                       :paint! (fn [{:keys [screen]}]
                                 (with-redefs [screen/fitting-image-placements
                                               (fn [& _]
                                                 [{:col 2 :row 5 :img {:id "fixture"}}])
                                               screen/paint-terminal-images! #(reset! placed %)]

                                   (#'screen/render-frame! screen cols 24 (fixture-db) 1000)))})]

        (is (nil? (:error capture)))
        (is (= (if (= cols 80) [] [{:col 50 :row 5 :img {:id "fixture"}}]) @placed))))
    (let [capture
          (cap/capture!
            {:cols 144
             :rows 24
             :paint!
             (fn [{:keys [screen]}]
               (#'screen/render-frame! screen 144 24 (assoc (fixture-db) :help-open? true) 1000))})]
      (is (nil? (:error capture)))
      (is (= 144 (get-in capture [:ret :cols])))
      (is (zero? (get-in capture [:ret :chat-left])))
      (is (empty? (.current projects/hit-map))))))

(deftest project-input-counts-and-lifecycle-test
  ;; A parked turn still has :loading? true, but needs input rather than running.
  (with-redefs [state/app-db (atom (attention-fixture-db))]
    (let [summary #(mapv (fn [entry]
                           (select-keys entry [:tab-count :running :needs-input]))
                         (filter (fn [entry]
                                   (= :project-select (:kind entry)))
                                 (projects/sidebar-entries @state/app-db)))]
      (is (= [{:tab-count 2 :running 0 :needs-input 0} {:tab-count 2 :running 1 :needs-input 1}]
             (summary)))
      (is (= [:select :select :session]
             (mapv (comp first :action) (projects/sidebar-entries @state/app-db))))
      (let [form (get-in @state/app-db [:tab-locals :tab-3 :human-input])]
        (state/dispatch [:human-input-open (assoc-in form [:request :id] "request-b-next")]))
      (state/dispatch [:select-tab-by-session "b1"])
      (is (= 1 (:needs-input (second (summary)))))
      (state/dispatch [:human-input-close "request-b"])
      (is (= "request-b-next" (get-in @state/app-db [:human-input :request :id])))
      (is (= 1 (:needs-input (second (summary)))) "Count tabs, not queued requests")
      (state/dispatch [:human-input-close "request-b-next"])
      (is (= {:tab-count 2 :running 2 :needs-input 0} (second (summary))))
      (is (= 2 (count (projects/sidebar-entries @state/app-db)))))))

(deftest project-header-counts-are-the-gateways-test
  ;; The rail counted the tabs open in THIS terminal, so a run in another process -
  ;; or a conversation nobody opened here - counted zero on the project header. The
  ;; gateway tallies every session in the project; the rail paints that tally, with
  ;; the local tab flags (covered above, with no gateway counts at all) as the
  ;; instant overlay on top of it.
  (let [overview
        {"projects"
         [{"root" "/work/vis" "project_id" "a" "live_count" 3 "awaiting_count" 1 "unread_count" 2}
          ;; A root nobody named has no project id: matched by root.
          {"root" "/work/companion"
           "project_id" ""
           "live_count" 1
           "awaiting_count" 0
           "unread_count" 0}]}

        items
        (projects/with-gateway-counts [project-a project-b] overview)

        headers
        (filterv #(= :project-select (:kind %))
          (projects/sidebar-entries {:project-sidebar {:items items} :tabs []}))]

    (is (= [{"live_count" 3 "awaiting_count" 1 "unread_count" 2}
            {"live_count" 1 "awaiting_count" 0 "unread_count" 0}]
           (mapv #(select-keys % ["live_count" "awaiting_count" "unread_count"]) items)))
    ;; A session parked on a human is a LIVE session: 3 live beside 1 waiting is
    ;; 2 running, never 3.
    (is (= [{:tab-count 2 :running 2 :needs-input 1 :unread 2}
            {:tab-count 1 :running 1 :needs-input 0 :unread 0}]
           (mapv #(select-keys % [:tab-count :running :needs-input :unread]) headers)))
    (is (= "2 tabs · 2 run · 1 input · 2 NEW" (#'projects/row-status (first headers) nil 60)))
    ;; A project the overview never mentions keeps exactly what it came with.
    (is (= [project-a] (projects/with-gateway-counts [project-a] {"projects" []})))
    (is (= [project-a] (projects/with-gateway-counts [project-a] nil)))))

(deftest project-input-grid-and-navigation-test
  (doseq [pointer? [true false]]
    (let [refreshes (atom [])]
      (with-redefs [state/app-db (atom (assoc (attention-fixture-db)
                                         :project-active-tabs {"b" :tab-4}))
                    timg/images-protocol (constantly nil)]

        (state/dispatch [:project-sidebar {:focused? true :index 2}])
        (let [background (get-in @state/app-db [:tab-locals :tab-4])
              capture (cap/capture!
                        {:cols 144
                         :rows 24
                         :paint!
                         (fn [{:keys [screen]}]
                           (projects/paint! (.newTextGraphics screen) @state/app-db 144 24))})
              text (cap/frame-text capture)
              hit (first (filter #(= :project-input (:kind %)) (.current projects/hit-map)))
              {:keys [col row]} (:bounds hit)
              handle! #(#'screen/project-sidebar-key!
                         %
                         (fn [_]
                           (throw (ex-info "Wrong project action" {})))
                         (fn []
                           (throw (ex-info "Wrong add action" {})))
                         (fn [notify?]
                           (swap! refreshes conj notify?))
                         (fn [_]
                           (throw (ex-info "Wrong menu action" {})))
                         (fn [_]
                           nil))]

          (is (nil? (:error capture)))
          (is (re-find #"Companion +2 tabs · 1 running · 1 needs input" text))
          (is (re-find #"! Mobile navigation +needs input" text))
          (is (= 6 row) "The alert appears immediately below its project")
          (is (= (#'theme-test/rgb-tuple theme/warning-button-bg)
                 (get-in capture [:frames 0 row 43 :bg])))
          (is (true? (get-in capture [:frames 0 row 43 :bold])))
          (if pointer?
            (handle!
              (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. (int col) (int row))))
            (do (handle! (cap/key-stroke :down)) (handle! (cap/key-stroke :enter))))
          (is (= :tab-3 (:active-tab-id @state/app-db))
              "Open the waiting tab, not the remembered project tab")
          (is (= "b" (:active-project-id @state/app-db)))
          (is (= "request-b" (get-in @state/app-db [:human-input :request :id])))
          (is (= "Keep this draft"
                 (input/input->text (get-in @state/app-db [:tab-locals :tab-1 :input]))))
          (is (= background (get-in @state/app-db [:tab-locals :tab-4])))
          (is (= [false] @refreshes))
          (is (false? (get-in @state/app-db [:project-sidebar :focused?]))))))))

(deftest project-input-scroll-test
  (let [tabs
        (mapv (fn [n]
                {:id (str n) :project-id "b" :label (str "Session " n)})
              (range 30))

        db
        (assoc (fixture-db)
          :tabs tabs
          :tab-locals (into {}
                            (map (fn [{:keys [id]}]
                                   [id {:session {:id id} :human-input {:request {:id id}}}])
                                 tabs))
          :project-sidebar {:open? true :focused? true :index 31 :items [project-b]})

        visible
        (projects/visible-entries db 16)]

    (is (= 9 (count visible)))
    (is (= :project-select (:kind (first visible)))
        "Keep the parent visible above a long waiting group")
    (is (= 31 (:index (last visible))))
    (is (= [:session "29"] (projects/key-action db (cap/key-stroke :enter))))
    (is (empty? (projects/visible-entries db 7)))))

(deftest project-input-band-keeps-docked-sidebar-test
  (with-redefs [timg/images-protocol
                (constantly nil)

                vis/get-router
                (constantly nil)

                state/app-db
                (atom (attention-fixture-db))]

    (state/dispatch [:select-tab-by-session "b1"])
    (state/dispatch [:project-sidebar {:focused? false}])
    (doseq [cols [40 80 144]]
      (let [capture (cap/capture! {:cols cols
                                   :rows 30
                                   :paint!
                                   (fn [{:keys [screen]}]
                                     (#'screen/render-frame! screen cols 30 @state/app-db 1000))})
            text (cap/frame-text capture)]

        (is (nil? (:error capture)))
        (is (str/includes? text "Choose platform"))
        (is (= (= cols 144) (str/includes? text "Projects")))
        (is (= (if (= cols 144) 56 0) (get-in capture [:ret :chat-left])))))))

(deftest project-input-prefix-navigation-test
  (with-redefs [state/app-db
                (atom (attention-fixture-db))

                vis/gateway-list-projects
                (constantly [project-a project-b])

                vis/worker-future
                (fn [_ f]
                  (f))

                timg/images-protocol
                (constantly nil)]

    (state/dispatch [:select-tab-by-session "b1"])
    (let [form
          (:human-input @state/app-db)

          prefix
          (KeyStroke. \x true false)

          capture
          (cap/capture! {:keys [\w]
                         :paint! (fn [{:keys [screen]}]
                                   (#'screen/resolve-prefix!
                                    screen
                                    @state/app-db
                                    (input/handle-key prefix (:input @state/app-db))))})]

      (is (nil? (:error capture)))
      (is (not (#'screen/human-input-owns-key? @state/app-db prefix)))
      (is (#'screen/human-input-owns-key? @state/app-db (cap/key-stroke \w)))
      (is (#'screen/human-input-owns-key? @state/app-db (cap/key-stroke :esc)))
      (is (= :switch-project (get-in capture [:ret :action])))
      (#'screen/toggle-project-sidebar!)
      (is (false? (get-in @state/app-db [:project-sidebar :open?])))
      (#'screen/toggle-project-sidebar!)
      (is (true? (get-in @state/app-db [:project-sidebar :open?])))
      (is (= form (:human-input @state/app-db))))))

(deftest project-sidebar-theme-contrast-test
  (let [before @theme/active-theme-id]
    (try (doseq [id (shared-theme/available-theme-ids)]
           (theme/apply-theme! (keyword id))
           (doseq [fg [theme/dialog-fg theme/dialog-hint-key theme/warning-fg]
                   bg [theme/terminal-bg theme/input-field-bg]]

             (is (>= (#'theme-test/contrast-ratio fg bg) 4.5) (str id " sidebar text")))
           (let [highlight (theme/mix-color theme/terminal-bg theme/header-active-tab-bg 0.14)]
             (doseq [[label fg] [[:text theme/dialog-fg] [:hint theme/dialog-hint-key]
                                 [:warning theme/warning-fg]]]
               (is (>= (#'theme-test/contrast-ratio fg highlight) 4.5)
                   (str id " focused project row " label))))
           (is (>= (#'theme-test/contrast-ratio theme/dialog-hint theme/terminal-bg) 4.5)
               (str id " sidebar hints and borders"))
           (let [[fg bg] (theme/chip-tint :warning)]
             (is (>= (#'theme-test/contrast-ratio fg bg) 4.5) (str id " yellow button text"))
             (is (= theme/warning-button-bg bg))
             (is (> (.getGreen ^com.googlecode.lanterna.TextColor bg)
                    (.getBlue ^com.googlecode.lanterna.TextColor bg)))))
         (finally (theme/apply-theme! before)))))

(deftest project-unread-completion-and-read-test
  ;; Completed background replies were marked in the tab strip but absent from Projects.
  (with-redefs [state/app-db (atom (attention-fixture-db))]
    (let [entries #(projects/sidebar-entries @state/app-db)
          summary #(first (filter (fn [entry]
                                    (= project-b (:project entry)))
                                  (entries)))
          answer [{:type :text :text "Snapshot tests passed."}]]

      (state/dispatch [:message-received :tab-4 answer {:status :completed}])
      (is (= {:running 0 :needs-input 1 :unread 1}
             (select-keys (summary) [:running :needs-input :unread])))
      (is (= [[:select project-a] [:select project-b] [:session "b1"] [:session "b2"]]
             (mapv :action (entries))))
      (is (pos? (long (:render-version @state/app-db 0))) "A background answer repaints the rail")
      (state/dispatch [:select-tab-by-session "a2"])
      (is (= 1 (:unread (summary))) "Reading another tab must not clear NEW")
      (state/dispatch [:select-tab-by-session "b1"])
      (is (= 1 (:unread (summary))) "Opening the waiting tab must not clear a different reply")
      (state/dispatch [:select-tab-by-session "b2"])
      (is (= 0 (:unread (summary))))
      (is (not-any? #(= :project-unread (:kind %)) (entries)))
      (is (= answer (:content (last (:messages @state/app-db)))))
      (state/dispatch [:select-tab-by-session "a1"])
      (is (= 0 (:unread (summary))) "NEW does not return after leaving a read reply")
      (state/dispatch [:message-received :tab-1 answer {:status :completed}])
      (is (every? #(zero? (:unread %)) (filter #(= :project-select (:kind %)) (entries)))))))

(deftest project-unread-cancel-and-replay-test
  (with-redefs [state/app-db (atom (attention-fixture-db))]
    (state/dispatch [:message-received :tab-4 [] {:status :cancelled}])
    (is (not-any? #(= :project-unread (:kind %)) (projects/sidebar-entries @state/app-db)))
    (state/dispatch [:message-received :tab-4 [{:type :text :text "Old result"}]
                     {:status :completed :client-turn-id "already-settled"}])
    (is (not-any? #(= :project-unread (:kind %)) (projects/sidebar-entries @state/app-db)))))

(deftest project-unread-grid-and-navigation-test
  (doseq [pointer? [true false]]
    (let [refreshes (atom [])]
      (with-redefs [state/app-db (atom (assoc (news-fixture-db) :project-active-tabs {"b" :tab-4}))]
        (state/dispatch [:project-sidebar {:focused? true :index 3}])
        (let [waiting (get-in @state/app-db [:tab-locals :tab-3])
              running (get-in @state/app-db [:tab-locals :tab-4])
              capture (cap/capture!
                        {:cols 144
                         :rows 24
                         :paint!
                         (fn [{:keys [screen]}]
                           (projects/paint! (.newTextGraphics screen) @state/app-db 144 24))})
              text (cap/frame-text capture)
              hit (first (filter #(= :project-unread (:kind %)) (.current projects/hit-map)))
              row (get-in hit [:bounds :row])
              handle! #(#'screen/project-sidebar-key!
                         %
                         (fn [_]
                           (throw (ex-info "Must open the exact unread session" {})))
                         (fn []
                           (throw (ex-info "Must not add a project" {})))
                         (fn [notify?]
                           (swap! refreshes conj notify?))
                         (fn [_]
                           (throw (ex-info "Wrong menu action" {})))
                         (fn [_]
                           nil))]

          (is (nil? (:error capture)))
          (is (str/includes? text "3 tabs · 1 run · 1 input · 1 NEW"))
          (is (re-find #"Keyboard navigation +NEW" text))
          (is (= 7 row))
          (is (= (#'theme-test/rgb-tuple theme/warning-button-bg)
                 (get-in capture [:frames 0 row 51 :bg])))
          (if pointer?
            ;; Click the yellow cap, not only the row's title.
            (handle! (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 51 (int row))))
            (do (handle! (cap/key-stroke :down)) (handle! (cap/key-stroke :enter))))
          (is (= :tab-5 (:active-tab-id @state/app-db)))
          (is (= "b" (:active-project-id @state/app-db)))
          (is (= "Keyboard navigation is ready." (:text (last (:messages @state/app-db)))))
          (is (not-any? #(= :project-unread (:kind %)) (projects/sidebar-entries @state/app-db)))
          (is (= waiting (get-in @state/app-db [:tab-locals :tab-3])))
          (is (= running (get-in @state/app-db [:tab-locals :tab-4])))
          (is (= "Keep this draft"
                 (input/input->text (get-in @state/app-db [:tab-locals :tab-1 :input]))))
          (is (= [false] @refreshes)))))))

(deftest project-unread-and-input-share-one-row-test
  (let [db
        (update (news-fixture-db)
                :tabs
                #(mapv (fn [tab]
                         (cond-> tab
                           (= :tab-3 (:id tab))
                           (assoc :unread? true)))
                       %))

        entries
        (projects/sidebar-entries db)

        capture
        (cap/capture! {:cols 144
                       :rows 24
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db 144 24))})]

    (is (= 2 (:unread (second entries))))
    (is (= 1 (count (filter #(= :tab-3 (:tab-id %)) entries))))
    (is (str/includes? (cap/frame-text capture) "NEW · needs input"))))

(deftest project-alert-buttons-hover-and-width-test
  (let [before @theme/active-theme-id]
    (try
      (doseq [id (shared-theme/available-theme-ids)
              cols [40 44 80 144]]

        (theme/apply-theme! (keyword id))
        (with-redefs [state/app-db (atom (news-fixture-db))]
          (let [paint! (fn [{:keys [screen]}]
                         (projects/paint! (.newTextGraphics screen) @state/app-db cols 24))
                initial (cap/capture! {:cols cols :rows 24 :paint! paint!})
                width (:width (projects/geometry @state/app-db cols 24))
                col (- width 4)
                hover! #(#'screen/project-sidebar-key!
                          (MouseAction. MouseActionType/MOVE 0 (TerminalPosition. (int %) (int %2)))
                          identity
                          (constantly nil)
                          (constantly nil)
                          (constantly nil)
                          (constantly nil))]

            (is (nil? (:error initial)))
            (is (str/includes? (cap/frame-text initial) "needs input"))
            (is (str/includes? (cap/frame-text initial) "NEW"))
            (when (= cols 44)
              (is (str/includes? (cap/frame-text initial) "Companion"))
              (is (str/includes? (cap/frame-text initial) "3 tabs 1 run 1 input 1 NEW")))
            (doseq [row [6 7]]
              (hover! col row)
              (let [hovered (cap/capture! {:cols cols :rows 24 :paint! paint!})
                    cell (get-in hovered [:frames 0 row col])]

                (is (= (#'theme-test/rgb-tuple theme/warning-button-bg) (:bg cell)))
                (is (true? (:bold cell)))
                (is (true? (:underline cell)))))
            (hover! 0 0)
            (let [away (cap/capture! {:cols cols :rows 24 :paint! paint!})]
              (is (false? (get-in away [:frames 0 7 col :underline])))))))
      (finally (theme/apply-theme! before)))))

(deftest project-unread-scroll-keeps-parent-test
  (let [tabs
        (mapv (fn [n]
                {:id n :project-id "b" :label (str "Reply " n) :unread? true})
              (range 30))

        db
        (assoc (fixture-db)
          :tabs tabs
          :tab-locals (into {}
                            (map (fn [{:keys [id]}]
                                   [id {:session {:id (str id)}}])
                                 tabs))
          :project-sidebar {:open? true :focused? true :index 31 :items [project-b]})

        visible
        (projects/visible-entries db 16)]

    (is (= 9 (count visible)))
    (is (= :project-select (:kind (first visible))))
    (is (= 30 (:unread (first visible))))
    (is (= 31 (:index (last visible))))
    (is (= [:session "29"] (projects/key-action db (cap/key-stroke :enter))))))

(def group-release {"id" "g1" "name" "Release apps" "color" "violet" "session_count" 3})

(def group-gateway {"id" "g2" "name" "Gateway" "color" "cyan" "session_count" 1})

(defn grouped-db
  "Project rail with two groups filed under the first project (BLO-167)."
  []
  (-> (fixture-db)
      (assoc-in [:project-sidebar :groups] {"a" [group-release group-gateway]})))

(deftest project-groups-nest-under-their-project-test
  ;; BLO-167: a group is the human's own division INSIDE one project, so its row
  ;; sits under that project and above the attention rows, inked in the palette
  ;; colour the group carries, and selecting it still opens the project.
  (let [db
        (grouped-db)

        entries
        (projects/sidebar-entries db)

        groups
        (filterv #(= :project-group (:kind %)) entries)

        capture
        (cap/capture! {:cols 144
                       :rows 24
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db 144 24))})]

    (is (= [:project-select :project-group :project-group :project-select] (mapv :kind entries)))
    (is (= ["Release apps" "Gateway"] (mapv :label groups)))
    (is (= ["violet" "cyan"] (mapv :color groups)))
    (is (= [3 1] (mapv :group-count groups)))
    (is (= [2 3] (mapv :index groups)))
    (is (= [:select project-a] (:action (first groups))))
    (is (= "3 sessions" (#'projects/row-status (first groups) nil 40)))
    (is (= "1 session" (#'projects/row-status (second groups) nil 40)))
    (is (nil? (:error capture)))
    (is (str/includes? (cap/frame-text capture) "Release apps"))
    (is (str/includes? (cap/frame-text capture) "3 sessions"))
    (is (str/includes? (cap/frame-text capture) "g menu"))
    (doseq [[group hit]
            (map vector groups (filter #(= :project-group (:kind %)) (.current projects/hit-map)))]
      (is (= (#'theme-test/rgb-tuple (theme/group-ink (:color group)))
             (get-in capture [:frames 0 (get-in hit [:bounds :row]) 1 :fg]))))))

(deftest project-rail-g-opens-the-row-menu-test
  ;; `g` acts on the row under the cursor: group verbs on a group row, project
  ;; verbs on a project row, and nothing above the first row.
  (let [db
        (grouped-db)

        action
        #(projects/key-action (assoc-in db [:project-sidebar :index] %) (cap/key-stroke \g))]

    (is (= :project-select (:kind (second (action 1)))))
    (is (= :menu (first (action 2))))
    (is (= "Release apps" (:label (second (action 2)))))
    (is (= "Gateway" (:label (second (action 3)))))
    (is (= [:noop] (action 0)))
    (is (= [:noop] (action 99)))))

(deftest saved-project-page-shows-sessions-not-opened-here-test
  ;; The project navigator reads gateway pages rather than the TUI's open views.
  (let [page
        {:sessions [{"id" "never-opened" "title" "Saved elsewhere" "group_id" nil}
                    {"id" "loose-2" "title" "Another saved session" "group_id" nil}]
         :grouped [{"id" "filed" "title" "Filed elsewhere" "group_id" "g1"}]
         :total 32
         :next-cursor "next"
         :has-more true}

        db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"] page)
            (assoc-in [:project-sidebar :groups "a"] [group-release]))

        entries
        (projects/sidebar-entries db)]

    (is (= [:project-select :project-set :project-group :project-session :project-set
            :project-session :project-session :project-page :project-select]
           (mapv :kind entries)))
    (is (= ["filed" "never-opened" "loose-2"]
           (mapv #(get-in % [:session "id"]) (filter #(= :project-session (:kind %)) entries))))
    (is (= [:session "never-opened"] (:action (nth entries 5))))
    (is (= [:page "a" :next] (:action (nth entries 7))))
    (is (= [:toggle-project "a"] (:action (first entries))))))

(deftest saved-project-pins-attention-and-current-off-page-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions [{"id" "loose" "title" "A saved row"}]
                       :grouped [{"id" "filed" "group_id" "g1"}]
                       :awaiting [{"id" "waiting" "title" "Needs input"}
                                  {"id" "filed" "group_id" "g1"}]
                       :current {"id" "a1" "title" "Active off page"}})
            (assoc-in [:project-sidebar :groups "a"] [group-release]))

        entries
        (projects/sidebar-entries db)

        sessions
        (filterv #(= :project-session (:kind %)) entries)]

    (is (= ["waiting" "a1" "filed" "loose"] (mapv #(get-in % [:session "id"]) sessions)))
    (is (= ["Attention" "Groups" "Sessions"]
           (mapv :label (filter #(= :project-set (:kind %)) entries))))))

(deftest saved-project-off-page-attention-opens-through-the-rail-test
  (let [asked
        (atom [])

        opened
        (atom [])]

    (with-redefs [state/app-db
                  (atom (fixture-db))

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-list-session-groups-page
                  (fn [_]
                    {:groups [] :total 0})

                  vis/gateway-list-sessions-page
                  (fn [opts]
                    (swap! asked conj opts)
                    (if (:ids opts)
                      {:sessions [{"id" "a1" "title" "Current"}]}
                      {:sessions [{"id" "idle" "title" "On this page"}]
                       :awaiting [{"id" "waiting"
                                   "title" "Needs a reply"
                                   "live" true
                                   "is_awaiting_input" true}]
                       :total 40
                       :has-more true
                       :next-cursor "next"}))]

      (#'screen/load-project-page! "a")
      (state/dispatch [:project-sidebar {:expanded #{"a"} :index 3}])
      (let [entries (projects/sidebar-entries @state/app-db)]
        (is (= "waiting" (get-in (nth entries 2) [:session "id"])))
        (is (= "INPUT NEEDED" (:status (nth entries 2))))
        (is (= [:session "waiting"] (projects/key-action @state/app-db (cap/key-stroke :enter)))))
      (#'screen/project-sidebar-key!
       (cap/key-stroke :enter)
       (fn [_])
       (fn [_])
       (fn [_])
       (fn [_])
       #(swap! opened conj %))
      (is (= ["waiting"] @opened))
      (is (= "a" (:project-id (first @asked))))
      (is (= 3 (count (:tabs @state/app-db))) "A pinned row is not an open TUI view"))))

(deftest saved-project-row-state-and-metadata-test
  (let [rows
        [{"id" "need"
          "title" "A question"
          "is_awaiting_input" true
          "awaiting_input_count" 2
          "live" true} {"id" "live" "title" "Running" "live" true}
         {"id" "stopped"
          "title" "Interrupted"
          "was_interrupted" true
          "is_unread" true
          "unread_answers" 1}
         {"id" "new"
          "title" "Unread"
          "is_unread" true
          "unread_answers" 2
          "favorite_rank" 3
          "turn_count" 4
          "modified_at" "2026-09-24T00:00:00Z"}
         {"id" "waiting" "title" "Suspended" "status" "suspended"}
         {"id" "a1" "title" nil "turn_count" 1}
         {"id" "archived" "title" "Archived" "archived_at" "2026-09-24T00:00:00Z"}
         {"id" "idle" "title" "Quiet"}]

        db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"] {:sessions rows :grouped []}))

        entries
        (filterv #(= :project-session (:kind %)) (projects/sidebar-entries db))]

    (is (= ["INPUT NEEDED ×2" "LIVE" "STOPPED" "NEW ×2" "WAITING" "DIRTY" "ARCHIVED" "IDLE"]
           (mapv :status entries)))
    (is (= "Keep this draft" (:label (nth entries 5))))
    (is (true? (:favorite? (nth entries 3))))
    (is (= 4 (:turns (nth entries 3))))
    (is (= "2026-09-24T00:00:00Z" (:modified-at (nth entries 3))))))

(deftest saved-project-attachment-only-draft-test
  (let [db
        (-> (fixture-db)
            (assoc :input (input/empty-input)
                   :attachments [{:id "image"}])
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions [{"id" "a1" "title" nil}] :grouped []}))

        row
        (first (filter #(= :project-session (:kind %)) (projects/sidebar-entries db)))]

    (is (= "1 unsent attachment" (:label row)))
    (is (= "DIRTY" (:status row)))))

(deftest saved-session-menu-actions-use-the-gateway-test
  (let [calls
        (atom [])

        entry
        {:kind :project-session
         :label "Saved elsewhere"
         :project project-a
         :session {"id" "saved" "title" "Saved elsewhere"}}]

    (with-redefs-fn {#'screen/with-dialog-lock (fn [f]
                                                 (f))
                     #'screen/refresh-projects! #(swap! calls conj [:refresh])
                     #'vis/worker-future (fn [_ f]
                                           (f))
                     #'vis/gateway-set-session-favorite! (fn [sid value]
                                                           (swap! calls conj [:star sid value]))
                     #'vis/gateway-set-session-title! (fn [sid title]
                                                        (swap! calls conj [:rename sid title]))
                     #'dlg/select-dialog! (fn [& _]
                                            {:id :favorite})
                     #'dlg/text-input-dialog! (fn [& _]
                                                "  New title  ")
                     #'dlg/session-metrics-dialog! (fn [_ sid session]
                                                     (swap! calls conj
                                                       [:details sid (get session "id")]))}
      (fn []
        (#'screen/sidebar-row-menu! nil entry nil)
        (#'screen/sidebar-row-menu! nil (assoc entry :favorite? true) nil)
        (is (= [[:star "saved" true] [:refresh] [:star "saved" false] [:refresh]] @calls))
        (reset! calls [])
        (with-redefs [dlg/select-dialog! (fn [& _]
                                           {:id :rename-session})]
          (#'screen/sidebar-row-menu! nil entry nil))
        (is (= [[:rename "saved" "New title"] [:refresh]] @calls))
        (reset! calls [])
        (#'screen/sidebar-row-menu! nil (assoc entry :show-details? true) nil)
        (is (= [[:details "saved" "saved"]] @calls))
        (reset! calls [])
        (with-redefs [vis/gateway-set-session-favorite!
                      (fn [& _]
                        (throw (ex-info "offline" {})))

                      vis/notify!
                      (fn [message & _]
                        (swap! calls conj [:warning message]))]

          (#'screen/sidebar-row-menu! nil entry nil))
        (is (= [[:warning "Could not change favorite"]] @calls))))))

(deftest saved-session-archive-view-is-independent-and-gateway-paged-test
  (let [asked
        (atom [])

        db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :groups "a"] [group-release])
            (assoc-in [:project-sidebar :pages "a"]
                      {:after "old-cursor"
                       :history [nil]
                       :group-offset 0
                       :sessions [{"id" "loose" "title" "Normal"}]}))]

    (with-redefs [state/app-db
                  (atom db)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-list-session-groups-page
                  (fn [opts]
                    (swap! asked conj [:groups opts])
                    {:groups [group-release] :total 1})

                  vis/gateway-list-sessions-page
                  (fn [opts]
                    (swap! asked conj [:sessions opts])
                    (cond (:ids opts) {:sessions (if (= :only (:archived opts)) [] [{"id" "a1"}])}
                          (= :only (:archived opts))
                          {:sessions
                           [{"id" "away" "title" "Put away" "archived_at" "2026-09-24T00:00:00Z"}]
                           :grouped [{"id" "filed-away" "group_id" "g1"}]
                           :total 1}
                          :else {:sessions [{"id" "loose" "title" "Normal"}]
                                 :grouped [{"id" "filed-active" "group_id" "g1"}]
                                 :total 1}))]

      (state/dispatch [:project-session-archive-toggle "a"])
      (is (true? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
      (is (nil? (get-in @state/app-db [:project-sidebar :pages "a" :after])))
      (is (empty? (get-in @state/app-db [:project-sidebar :pages "a" :history])))
      (#'screen/load-project-page! "a")
      (is (= :only (get-in @asked [1 1 :archived])))
      (is (= :exclude (get-in @asked [2 1 :archived])))
      (let [entries (projects/sidebar-entries @state/app-db)]
        (is (some #(= "Sessions · Archived" (:label %)) entries))
        (is (= ["filed-active" "away"]
               (mapv #(get-in % [:session "id"])
                     (filter #(= :project-session (:kind %)) entries)))))
      (state/dispatch [:project-session-archive-toggle "a"])
      (is (false? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
      (is (= [group-release] (get-in @state/app-db [:project-sidebar :groups "a"])))
      (#'screen/load-project-page! "a")
      (is (= :exclude (get-in @asked [5 1 :archived])))
      (is (= ["a1" "filed-active" "loose"]
             (mapv #(get-in % [:session "id"])
                   (filter #(= :project-session (:kind %))
                           (projects/sidebar-entries @state/app-db))))))))

(deftest saved-session-archive-and-delete-confirmation-test
  (let [calls
        (atom [])

        pick
        (atom :archive-session)

        confirmed?
        (atom false)

        entry
        {:kind :project-session
         :label "Saved elsewhere"
         :project project-a
         :session {"id" "saved" "title" "Saved elsewhere"}}]

    (with-redefs [state/app-db
                  (atom (fixture-db))

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-set-session-archived!
                  (fn [sid away]
                    (swap! calls conj [:archive sid away]))

                  vis/gateway-close-session!
                  (fn [sid]
                    (swap! calls conj [:delete sid]))

                  vis/notify!
                  (fn [& _]
                    nil)

                  screen/refresh-projects!
                  (fn []
                    (swap! calls conj [:refresh]))

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/select-dialog!
                  (fn [& _]
                    {:id @pick})

                  dlg/confirm-dialog!
                  (fn [& _]
                    @confirmed?)]

      (#'screen/sidebar-row-menu!
       nil
       (assoc entry :session (assoc (:session entry) "live" true))
       nil)
      (is (empty? @calls) "Busy sessions must not be archived")
      (#'screen/sidebar-row-menu! nil entry nil)
      (is (= [[:archive "saved" true] [:refresh]] @calls))
      (reset! calls [])
      (reset! pick :unarchive-session)
      (#'screen/sidebar-row-menu!
       nil
       (assoc entry :session (assoc (:session entry) "archived_at" "now"))
       nil)
      (is (= [[:archive "saved" false] [:refresh]] @calls))
      (reset! calls [])
      (reset! pick :delete-session)
      (#'screen/sidebar-row-menu! nil entry nil)
      (is (empty? @calls) "Canceling deletion leaves the gateway untouched")
      (reset! confirmed? true)
      (#'screen/sidebar-row-menu! nil entry nil)
      (is (= [[:delete "saved"] [:refresh]] @calls))
      (is (= :tab-1 (:active-tab-id @state/app-db))
          "Deleting an unopened row does not change focus"))))

(deftest set-creation-shortcuts-and-session-button-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"] {:sessions [] :grouped []}))

        entries
        (projects/sidebar-entries db)

        group-entry
        (first (filter #(= :groups (:set %)) entries))

        session-entry
        (first (filter #(= :sessions (:set %)) entries))]

    (cap/capture! {:cols 120
                   :rows 24
                   :paint! (fn [{:keys [screen]}]
                             (projects/paint! (.newTextGraphics screen) db 120 24))})
    (let [hits
          (.current projects/hit-map)

          session-hit
          (first (filter #(= :project-session-add (:kind %)) hits))]

      (is (not-any? #(= :project-group-add (:kind %)) hits))
      (is (not-any? #(and (= :project-set-menu (:kind %)) (= :groups (:set %))) hits))
      (is (some? session-hit))
      (when session-hit
        (let [{:keys [col row]}
              (:bounds session-hit)

              click
              (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. (int col) (int row)))]

          (is (= [:menu (assoc session-entry :initial-action :new-session)]
                 (projects/key-action db click)))))
      (is (some #(and (= :project-set-menu (:kind %)) (= :sessions (:set %))) hits)))
    (doseq [[entry choice] [[group-entry :new] [session-entry :new-session]]]
      (is (= [:menu (assoc entry :initial-action choice)]
             (projects/key-action (assoc-in db [:project-sidebar :index] (:index entry))
                                  (cap/key-stroke \+)))))
    (is (= [:menu group-entry]
           (projects/key-action (assoc-in db [:project-sidebar :index] (:index group-entry))
                                (cap/key-stroke \g))))))

(deftest set-buttons-use-web-bands-and-direct-actions-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"] {:sessions [] :grouped []}))

        capture
        (cap/capture! {:cols 120
                       :rows 24
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db 120 24))})

        entries
        (projects/sidebar-entries db)

        rows
        (into {} (map (juxt :kind :bounds) (.current projects/hit-map)))

        project-row
        (get-in rows [:project-select :row])

        groups-row
        (->> (.current projects/hit-map)
             (filter #(and (= :project-set (:kind %)) (= :groups (:set %))))
             first
             :bounds
             :row)

        sessions-row
        (->> (.current projects/hit-map)
             (filter #(and (= :project-set (:kind %)) (= :sessions (:set %))))
             first
             :bounds
             :row)

        bg
        (fn [row]
          (get-in capture [:frames 0 row 1 :bg]))

        palette
        (fn [ink fraction]
          (#'theme-test/rgb-tuple (theme/mix-color theme/terminal-bg ink fraction)))

        calls
        (atom [])]

    (is (= (palette theme/text-fg 0.04) (bg project-row)))
    (is (= (palette theme/header-active-tab-bg 0.08) (bg groups-row)))
    (is (= (palette theme/text-fg 0.06) (bg sessions-row)))
    (with-redefs [screen/create-group!
                  (fn [_ pid]
                    (swap! calls conj [:group pid]))

                  dlg/select-dialog!
                  (fn [& _]
                    (throw (ex-info "Unexpected menu" {})))]

      (#'screen/sidebar-row-menu!
       nil
       (assoc (first (filter #(= :groups (:set %)) entries)) :initial-action :new)
       nil)
      (#'screen/sidebar-row-menu!
       nil
       (assoc (first (filter #(= :sessions (:set %)) entries)) :initial-action :new-session)
       (fn [gid root]
         (swap! calls conj [:session gid root]))))
    (is (= [[:group "a"] [:session nil "/work/vis"]] @calls))))

(deftest project-rail-focus-highlights-instead-of-leading-dot-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :groups "a"] [group-release])
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions [{"id" "saved" "title" "Saved session"}] :grouped []}))

        entries
        (projects/sidebar-entries db)

        indices
        (mapv :index
              (filter #(#{:project-select :project-group :project-session} (:kind %)) entries))

        highlight
        (#'theme-test/rgb-tuple
         (theme/mix-color theme/terminal-bg theme/header-active-tab-bg 0.14))]

    (doseq [index indices]
      (let [selected (assoc-in db [:project-sidebar :index] index)
            capture (cap/capture! {:cols 120
                                   :rows 24
                                   :paint!
                                   (fn [{:keys [screen]}]
                                     (projects/paint! (.newTextGraphics screen) selected 120 24))})
            hit (first (filter #(= index (:index %)) (.current projects/hit-map)))
            row (get-in hit [:bounds :row])
            height (get-in hit [:bounds :height])
            lines (str/split-lines (cap/frame-text capture))]

        (is (nil? (:error capture)))
        (is (not (str/includes? (nth lines row) "•")))
        (is (every? #(= highlight (get-in capture [:frames 0 % 1 :bg])) (range row (+ row height))))
        (when (= :project-group (:kind hit))
          (is (= (#'theme-test/rgb-tuple theme/dialog-fg) (get-in capture [:frames 0 row 6 :fg])))
          (is (= (#'theme-test/rgb-tuple (theme/group-ink "violet"))
                 (get-in capture [:frames 0 row 1 :fg]))))))
    (let [db
          (assoc-in (fixture-db) [:project-sidebar :focused?] false)

          capture
          (cap/capture! {:cols 120
                         :rows 24
                         :paint! (fn [{:keys [screen]}]
                                   (projects/paint! (.newTextGraphics screen) db 120 24))})

          lines
          (str/split-lines (cap/frame-text capture))

          palette
          (fn [ink fraction]
            (#'theme-test/rgb-tuple (theme/mix-color theme/terminal-bg ink fraction)))]

      (is (str/includes? (nth lines 4) "Vis"))
      (is (not (str/includes? (nth lines 4) "● Vis")))
      (is (not (str/includes? (nth lines 4) "▸ Vis")))
      (is (= (palette theme/header-active-tab-bg 0.10) (get-in capture [:frames 0 4 1 :bg])))
      (is (= (palette theme/text-fg 0.04) (get-in capture [:frames 0 5 1 :bg]))))))

(deftest saved-session-grid-separates-title-and-status-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions [{"id" "live-session"
                                   "title" "Writing the tests"
                                   "live" true
                                   "turn_count" 12
                                   "favorite_rank" 1}
                                  {"id" "idle-session" "title" "Finished review" "turn_count" 7}]
                       :grouped []}))

        capture
        (cap/capture! {:cols 168
                       :rows 24
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db 168 24))})

        hits
        (filter #(= :project-session (:kind %)) (.current projects/hit-map))

        [live idle]
        hits

        lines
        (str/split-lines (cap/frame-text capture))

        live-row
        (get-in live [:bounds :row])

        idle-row
        (get-in idle [:bounds :row])

        status-cell
        (get-in capture [:frames 0 live-row (- 56 21)])]

    (is (nil? (:error capture)))
    (is (= 3 (get-in live [:bounds :height])))
    (is (= (+ live-row 3) idle-row))
    (is (str/includes? (nth lines live-row) "Writing the tests"))
    (is (str/includes? (nth lines live-row) "★ ● LIVE"))
    (is (str/includes? (nth lines idle-row) "IDLE"))
    (is (str/includes? (nth lines (inc live-row)) "live-sess"))
    (is (str/includes? (nth lines (inc live-row)) "12t"))
    (is (str/includes? (nth lines (inc idle-row)) "7t"))
    (is (= (#'theme-test/rgb-tuple theme/status-ok) (:fg status-cell)))
    (is (= [:session "live-session"]
           (projects/key-action db
                                (MouseAction. MouseActionType/CLICK_DOWN
                                              1
                                              (TerminalPosition. 18 (int (inc live-row)))))))))

(deftest saved-session-grid-narrow-status-and-metadata-test
  (doseq [cols [32 120]]
    (let [db (-> (fixture-db)
                 (assoc-in [:project-sidebar :expanded] #{"a"})
                 (assoc-in
                   [:project-sidebar :pages "a"]
                   {:sessions
                    [{"id" "live-session" "title" "Writing the tests" "live" true "turn_count" 12}]
                    :grouped []}))
          capture (cap/capture! {:cols cols
                                 :rows 24
                                 :paint! (fn [{:keys [screen]}]
                                           (projects/paint! (.newTextGraphics screen) db cols 24))})
          live (first (filter #(= :project-session (:kind %)) (.current projects/hit-map)))
          row (get-in live [:bounds :row])
          lines (str/split-lines (cap/frame-text capture))
          status-cell (get-in capture [:frames 0 (inc row) 5])]

      (is (nil? (:error capture)))
      (is (str/includes? (nth lines row) (if (= cols 32) "Writing the tes" "Writing the tests")))
      (is (not (str/includes? (nth lines row) "LIVE")))
      (is (str/includes? (nth lines (inc row)) "LIVE"))
      (is (= (>= cols 38) (str/includes? (nth lines (inc row)) "12t")))
      (is (= (#'theme-test/rgb-tuple theme/status-ok) (:fg status-cell))))))

(deftest saved-session-grid-scroll-keeps-focused-card-and-parent-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions (mapv (fn [n]
                                         {"id" (str "s" n) "title" (str "Card " n)})
                                       (range 30))
                       :grouped []})
            (assoc-in [:project-sidebar :index] 33))

        visible
        (projects/visible-entries db 16)

        capture
        (cap/capture! {:cols 120
                       :rows 16
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db 120 16))})

        hits
        (.current projects/hit-map)]

    (is (= :project-select (:kind (first visible))))
    (is (= "s29" (get-in (last visible) [:session "id"])))
    (is (<= (reduce + (map #(if (= :project-session (:kind %)) 3 1) visible)) 9))
    (is (every? #(<= (+ (get-in % [:bounds :row]) (get-in % [:bounds :height] 1)) 13)
                (filter #(= :project-session (:kind %)) hits)))
    (is (nil? (:error capture)))))

(deftest saved-session-archive-set-menu-has-keyboard-and-pointer-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"] {:sessions [] :grouped []}))

        entry
        (first (filter #(= "Sessions" (:label %)) (projects/sidebar-entries db)))

        selected
        (assoc-in db [:project-sidebar :index] (:index entry))

        choice
        (atom nil)

        loaded
        (atom [])]

    (is (= [:menu entry] (projects/key-action selected (cap/key-stroke \g))))
    (cap/capture! {:cols 120
                   :rows 24
                   :paint! (fn [{:keys [screen]}]
                             (projects/paint! (.newTextGraphics screen) selected 120 24))})
    (let [hit
          (first (filter #(and (= :project-set (:kind %)) (= "Sessions" (:label %)))
                         (.current projects/hit-map)))

          {:keys [col row]}
          (:bounds hit)]

      (is (= :menu
             (first (projects/key-action selected
                                         (MouseAction. MouseActionType/CLICK_DOWN
                                                       3
                                                       (TerminalPosition. (int col) (int row))))))))
    (with-redefs-fn {#'state/app-db (atom selected)
                     #'screen/with-dialog-lock (fn [f]
                                                 (f))
                     #'screen/load-project-page! #(swap! loaded conj %)
                     #'dlg/select-dialog! (fn [_ _ items]
                                            (reset! choice items)
                                            {:id :toggle-session-archive})}
      (fn []
        (#'screen/sidebar-row-menu! nil entry nil)
        (is (= "Show archived sessions" (:label (first @choice))))
        (is (true? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
        (is (= ["a"] @loaded))
        (#'screen/sidebar-row-menu! nil (assoc entry :archived? true) nil)
        (is (= "Hide archived sessions" (:label (first @choice))))
        (is (false? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))))))

(deftest saved-session-deleting-the-active-row-reconciles-its-id-test
  (let [attempts
        (atom [])

        fail?
        (atom true)

        entry
        {:kind :project-session
         :label "Current"
         :project project-a
         :session {"id" "a1" "title" "Current"}}]

    (with-redefs [state/app-db
                  (atom (assoc-in (fixture-db) [:project-sidebar :selected "a"] #{"a1"}))

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-close-session!
                  (fn [sid]
                    (swap! attempts conj sid)
                    (when @fail? (throw (ex-info "offline" {}))))

                  vis/notify!
                  (fn [& _]
                    nil)

                  screen/refresh-projects!
                  (fn []
                    nil)

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/select-dialog!
                  (fn [& _]
                    {:id :delete-session})

                  dlg/confirm-dialog!
                  (fn [& _]
                    true)]

      (#'screen/sidebar-row-menu! nil entry nil)
      (is (= "a1" (get-in @state/app-db [:session :id])) "A failed DELETE keeps the active view")
      (is (= #{"a1"} (get-in @state/app-db [:project-sidebar :selected "a"])))
      (reset! fail? false)
      (#'screen/sidebar-row-menu! nil entry nil)
      (is (= ["a1" "a1"] @attempts))
      (is (= "a2" (get-in @state/app-db [:session :id])))
      (is (= [:tab-2 :tab-3] (mapv :id (:tabs @state/app-db))))
      (is (empty? (get-in @state/app-db [:project-sidebar :selected "a"]))))))

(deftest saved-session-selection-has-keyboard-and-pointer-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions [{"id" "s1" "title" "First"} {"id" "s2" "title" "Second"}]
                       :grouped []}))

        entry
        (first (filter #(= "s1" (get-in % [:session "id"])) (projects/sidebar-entries db)))

        selected
        (assoc-in db [:project-sidebar :index] (:index entry))]

    (is (= [:toggle-session "a" "s1"] (projects/key-action selected (cap/key-stroke \space))))
    (with-redefs [state/app-db (atom selected)]
      (#'screen/project-sidebar-key!
       (cap/key-stroke \space)
       identity
       identity
       identity
       identity
       nil)
      (is (= #{"s1"} (get-in @state/app-db [:project-sidebar :selected "a"])))
      (let [capture (cap/capture!
                      {:cols 120
                       :rows 24
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) @state/app-db 120 24))})
            hit (first (filter #(and (= :project-session (:kind %))
                                     (= "s1" (get-in % [:session "id"])))
                               (.current projects/hit-map)))
            {:keys [col row]} (:bounds hit)]

        (is (nil? (:error capture)))
        (is (str/includes? (cap/frame-text capture) "☑ First"))
        (is (= [:menu hit]
               (projects/key-action @state/app-db
                                    (MouseAction. MouseActionType/CLICK_DOWN
                                                  3
                                                  (TerminalPosition. (int col) (int row))))))
        (is (= [:toggle-session "a" "s1"]
               (projects/key-action @state/app-db
                                    (MouseAction. MouseActionType/CLICK_DOWN
                                                  1
                                                  (TerminalPosition. (int (inc col)) (int row))))))
        (#'screen/project-sidebar-key!
         (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. (int (inc col)) (int row)))
         identity
         identity
         identity
         identity
         nil)
        (is (empty? (get-in @state/app-db [:project-sidebar :selected "a"]))))
      (#'screen/project-sidebar-key!
       (cap/key-stroke \space)
       identity
       identity
       identity
       identity
       nil)
      (is (= #{"s1"} (get-in @state/app-db [:project-sidebar :selected "a"])))
      (#'screen/project-sidebar-key!
       (cap/key-stroke \space)
       identity
       identity
       identity
       identity
       nil)
      (is (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
      (state/dispatch [:project-session-select-toggle "a" "s1"])
      (state/dispatch [:project-session-archive-toggle "a"])
      (is (empty? (get-in @state/app-db [:project-sidebar :selected "a"]))))))

(deftest selected-session-move-keeps-failed-rows-and-reports-partial-error-test
  (let [initial
        (-> (grouped-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :selected "a"] #{"s1" "s2"})
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions [{"id" "s1" "title" "First"} {"id" "s2" "title" "Second"}]
                       :grouped []}))

        assigned
        (atom {})

        calls
        (atom [])

        notices
        (atom [])

        group-entry
        (first (filter #(= "g1" (get-in % [:group "id"])) (projects/sidebar-entries initial)))

        loose-entry
        (first (filter #(= :sessions (:set %)) (projects/sidebar-entries initial)))]

    (with-redefs [state/app-db
                  (atom initial)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-assign-session-group!
                  (fn [sid gid]
                    (swap! calls conj [sid gid])
                    (when (= sid "s2") (throw (ex-info "offline" {})))
                    (swap! assigned assoc sid gid))

                  screen/refresh-projects!
                  (fn []
                    (let [rows (get-in @state/app-db [:project-sidebar :pages "a" :sessions])]
                      (state/dispatch
                        [:project-sidebar
                         {:pages
                          {"a" {:sessions (filterv #(not (contains? @assigned (get % "id"))) rows)
                                :grouped (mapv #(assoc % "group_id" (get @assigned (get % "id")))
                                               (filter #(contains? @assigned (get % "id"))
                                                       rows))}}}]))
                    (swap! calls conj [:refresh]))

                  vis/notify!
                  (fn [message & _]
                    (swap! notices conj message))

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/select-dialog!
                  (fn [_ _ _]
                    {:id :move-selected})]

      (#'screen/sidebar-row-menu! nil group-entry nil)
      (is (= [["s1" "g1"] ["s2" "g1"] [:refresh]] @calls))
      (is (= #{"s2"} (get-in @state/app-db [:project-sidebar :selected "a"])))
      (is (= ["s1"]
             (mapv #(get-in % [:session "id"])
                   (filter (fn [row]
                             (= "g1" (get-in row [:session "group_id"])))
                           (projects/sidebar-entries @state/app-db)))))
      (is (= ["s2"]
             (mapv #(get % "id") (get-in @state/app-db [:project-sidebar :pages "a" :sessions]))))
      (is (some #(str/includes? % "1 of 2") @notices))
      (reset! calls [])
      (reset! notices [])
      (with-redefs [dlg/select-dialog! (fn [_ _ _]
                                         {:id :ungroup-selected})]
        (#'screen/sidebar-row-menu! nil loose-entry nil))
      (is (= [["s2" nil] [:refresh]] @calls))
      (is (= #{"s2"} (get-in @state/app-db [:project-sidebar :selected "a"])))
      (is (some #(str/includes? % "1 of 1") @notices)))))

(deftest single-and-selected-session-move-uses-project-groups-test
  (let [db
        (-> (grouped-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions [{"id" "s1" "title" "First"} {"id" "s2" "title" "Second"}]
                       :grouped []}))

        entry
        (first (filter #(= "s1" (get-in % [:session "id"])) (projects/sidebar-entries db)))

        calls
        (atom [])

        options
        (atom [])

        pick
        (atom "g2")]

    (with-redefs [state/app-db
                  (atom db)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-list-session-groups
                  (fn [opts]
                    (is (= {:project-id "a"} opts))
                    [group-release group-gateway])

                  vis/gateway-assign-session-group!
                  (fn [sid gid]
                    (swap! calls conj [sid gid]))

                  screen/refresh-projects!
                  #(swap! calls conj [:refresh])

                  vis/notify!
                  (fn [& _]
                    nil)

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/select-dialog!
                  (fn [_ _ items]
                    (is (some #(= :move-session (:id %)) items))
                    {:id :move-session})

                  dlg/searchable-select!
                  (fn [_ _ items _]
                    (reset! options items)
                    {:id @pick})]

      (#'screen/sidebar-row-menu! nil entry nil)
      (is (= [["s1" "g2"] [:refresh]] @calls))
      (is (= ["g1" "g2" ::screen/new-group ::screen/remove-group] (mapv :id @options)))
      (reset! calls [])
      (state/dispatch [:project-session-select-toggle "a" "s1"])
      (state/dispatch [:project-session-select-toggle "a" "s2"])
      (state/dispatch [:project-session-select-toggle "b" "other"])
      (reset! pick ::screen/remove-group)
      (#'screen/sidebar-row-menu! nil entry nil)
      (is (= [["s1" nil] ["s2" nil] [:refresh]] @calls))
      (is (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
      (is (= #{"other"} (get-in @state/app-db [:project-sidebar :selected "b"]))))))

(deftest group-creation-edit-and-new-session-refresh-test
  (let [initial
        (-> (grouped-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"] {:sessions [] :grouped []}))

        entries
        (projects/sidebar-entries initial)

        group-entry
        (first (filter #(= "g1" (get-in % [:group "id"])) entries))

        loose-entry
        (first (filter #(= :sessions (:set %)) entries))

        calls
        (atom [])

        notices
        (atom [])

        choice
        (atom :new)

        typed
        (atom "  New group  ")

        fail?
        (atom false)]

    (with-redefs [state/app-db
                  (atom initial)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/text-input-dialog!
                  (fn [& _]
                    @typed)

                  dlg/select-dialog!
                  (fn [_ title items]
                    (if (= title "Group colour")
                      {:id "cyan"}
                      (do (is (some #(= :new-session (:id %)) items)) {:id @choice})))

                  vis/gateway-create-session-group!
                  (fn [opts]
                    (swap! calls conj [:create opts])
                    {"id" "new"})

                  vis/gateway-update-session-group!
                  (fn [gid opts]
                    (swap! calls conj [:update gid opts])
                    (when @fail? (throw (ex-info "offline" {}))))

                  screen/refresh-projects!
                  #(swap! calls conj [:refresh])

                  vis/notify!
                  (fn [message & _]
                    (swap! notices conj message))]

      (#'screen/sidebar-row-menu! nil loose-entry nil)
      (is (= [[:create {:name "New group" :project-id "a" :color "cyan"}] [:refresh]] @calls))
      (reset! choice :rename)
      (reset! typed "  Renamed  ")
      (#'screen/sidebar-row-menu! nil group-entry nil)
      (reset! choice :recolour)
      (#'screen/sidebar-row-menu! nil group-entry nil)
      (is (= [[:update "g1" {:name "Renamed"}] [:refresh] [:update "g1" {:color "cyan"}] [:refresh]]
             (subvec @calls 2)))
      (reset! fail? true)
      (reset! notices [])
      (reset! choice :rename)
      (#'screen/sidebar-row-menu! nil group-entry nil)
      (is (= [:update "g1" {:name "Renamed"}] (last @calls))
          "A failed rename does not refresh as though it succeeded")
      (is (some #(str/includes? % "Could not rename group") @notices))
      (reset! choice :new-session)
      (#'screen/sidebar-row-menu! nil loose-entry #(swap! calls conj [:start %1 %2]))
      (#'screen/sidebar-row-menu! nil group-entry #(swap! calls conj [:start %1 %2]))
      (is (= [[:start nil "/work/vis"] [:start "g1" "/work/vis"]]
             (subvec @calls (- (count @calls) 2)))))))

(deftest saved-project-pagers-and-pointer-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :groups "a"] [group-release group-gateway])
            (assoc-in [:project-sidebar :group-total "a"] 12)
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions [{"id" "saved-1" "title" "First saved"}]
                       :grouped []
                       :next-cursor "next"
                       :has-more true
                       :total 90
                       :group-size 2
                       :group-offset 0}))

        entries
        (projects/sidebar-entries db)

        paint
        (cap/capture! {:cols 144
                       :rows 24
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) db 144 24))})

        row
        (first (filter #(= :project-session (:kind %)) (.current projects/hit-map)))

        {:keys [col row]}
        (:bounds row)]

    (is (nil? (:error paint)))
    (is (= [:session "saved-1"]
           (projects/key-action
             db
             (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. (int col) (int row))))))
    (let [detail
          (first (filter #(= :project-details (:kind %)) (.current projects/hit-map)))

          bounds
          (:bounds detail)

          selected
          (assoc-in db
            [:project-sidebar :index]
            (:index (first (filter #(= :project-session (:kind %)) entries))))]

      (is (= [:details "saved-1"]
             (projects/key-action db
                                  (MouseAction. MouseActionType/CLICK_DOWN
                                                1
                                                (TerminalPosition. (int (:col bounds))
                                                                   (int (:row bounds)))))))
      (is (= [:details "saved-1"] (projects/key-action selected (cap/key-stroke \d))))
      (is (= :menu
             (first (projects/key-action db
                                         (MouseAction. MouseActionType/CLICK_DOWN
                                                       3
                                                       (TerminalPosition. (int col) (int row))))))))
    (is (some #(= [:group-page "a" :next] (:action %)) entries))
    (with-redefs [state/app-db (atom db)]
      (state/dispatch [:project-group-turn "a" :next])
      (is (= 2 (get-in @state/app-db [:project-sidebar :pages "a" :group-offset])))
      (state/dispatch [:project-group-turn "a" :previous])
      (is (= 0 (get-in @state/app-db [:project-sidebar :pages "a" :group-offset])))
      (state/dispatch [:project-page-turn "a" :next])
      (is (= "next" (get-in @state/app-db [:project-sidebar :pages "a" :after])))
      (state/dispatch [:project-page-request "a" "new"])
      (state/dispatch [:project-page-loaded "a" "stale" {:sessions [{"id" "stale"}]}
                       {:groups [] :total 0}])
      (is (= "saved-1" (get-in @state/app-db [:project-sidebar :pages "a" :sessions 0 "id"])))
      (state/dispatch [:project-page-turn "a" :previous])
      (is (nil? (get-in @state/app-db [:project-sidebar :pages "a" :after]))))))

(deftest group-archive-view-is-independent-of-loose-sessions-test
  (let [requests
        (atom [])

        db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"]
                      {:sessions [{"id" "loose" "title" "Loose"}]
                       :grouped [{"id" "filed-active" "group_id" "g1"}]
                       :group-size 2
                       :group-offset 2
                       :request-id "old"})
            (assoc-in [:project-sidebar :groups "a"] [group-release])
            (assoc-in [:project-sidebar :selected "a"] #{"filed-active"}))]

    (with-redefs [state/app-db
                  (atom db)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-list-session-groups-page
                  (fn [opts]
                    (swap! requests conj [:groups opts])
                    {:groups (if (= :only (:archived opts))
                               [(assoc group-gateway "archived_at" "today")]
                               [group-release])
                     :total 1})

                  vis/gateway-list-sessions-page
                  (fn [opts]
                    (swap! requests conj [:sessions opts])
                    {:sessions (if (= :only (:archived opts)) [] [{"id" "loose" "title" "Loose"}])
                     :grouped [(if (= :only (:archived opts))
                                 {"id" "filed-archived" "group_id" "g2"}
                                 {"id" "filed-active" "group_id" "g1"})]})]

      (state/dispatch [:project-group-archive-toggle "a"])
      (is (true? (get-in @state/app-db [:project-sidebar :group-archived? "a"])))
      (is (nil? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
      (is (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
      (is (= 0 (get-in @state/app-db [:project-sidebar :pages "a" :group-offset])))
      (state/dispatch [:project-page-loaded "a" "old" {:sessions [{"id" "stale"}]} {:groups []}])
      (#'screen/load-project-page! "a")
      (let [entries (projects/sidebar-entries @state/app-db)]
        (is (= ["Groups · Archived" "Sessions"]
               (mapv :label (filter #(= :project-set (:kind %)) entries))))
        (is (= ["Gateway"] (mapv :label (filter #(= :project-group (:kind %)) entries))))
        (is (= "ARCHIVED" (:status (first (filter #(= :project-session (:kind %)) entries)))))
        (is (= ["filed-archived" "loose"]
               (mapv #(get-in % [:session "id"])
                     (filter #(= :project-session (:kind %)) entries)))))
      (is (= :only (:archived (second (first @requests)))))
      (is (= :exclude (:archived (second (second @requests)))))
      (is (some #(= :only (:archived (second %))) (filter #(= :sessions (first %)) @requests)))
      (state/dispatch [:project-session-archive-toggle "a"])
      (#'screen/load-project-page! "a")
      (is (= ["filed-archived"]
             (mapv #(get-in % [:session "id"])
                   (filter #(= :project-session (:kind %))
                           (projects/sidebar-entries @state/app-db)))))
      (state/dispatch [:project-group-archive-toggle "a"])
      (#'screen/load-project-page! "a")
      (is (= ["Release apps"]
             (mapv :label
                   (filter #(= :project-group (:kind %))
                           (projects/sidebar-entries @state/app-db)))))
      (state/dispatch [:project-group-archive-toggle "a"])
      (state/dispatch [:project-page-request "a" "empty"])
      (state/dispatch [:project-page-loaded "a" "empty"
                       {:sessions [] :grouped [] :current nil :loading? false}
                       {:groups [] :total 0}])
      (is (some #(= "No archived groups" (:label %)) (projects/sidebar-entries @state/app-db))))))

(deftest group-archive-and-delete-actions-keep-failures-visible-test
  (let [choice
        (atom :archive-group)

        mode
        (atom nil)

        calls
        (atom [])

        notices
        (atom [])

        fail?
        (atom false)

        db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :selected "a"] #{"a2"})
            (assoc-in [:project-sidebar :groups "a"] [group-release]))

        group-entry
        {:kind :project-group :project project-a :group group-release}]

    (with-redefs [state/app-db
                  (atom db)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/select-dialog!
                  (fn [_ _ items]
                    (if (or (some #(= :archive-group (:id %)) items)
                            (some #(= :unarchive-group (:id %)) items))
                      (do (when (some #(= :unarchive-group (:id %)) items)
                            (is (not-any? #(= :new-session (:id %)) items)))
                          {:id @choice})
                      (when @mode {:id @mode})))

                  vis/gateway-update-session-group!
                  (fn [gid opts]
                    (swap! calls conj [:archive gid opts])
                    (when @fail? (throw (ex-info "gateway unavailable" {}))))

                  vis/gateway-delete-session-group!
                  (fn [gid selected-mode]
                    (swap! calls conj [:delete gid selected-mode])
                    (when @fail? (throw (ex-info "gateway unavailable" {})))
                    (if (= :detach selected-mode)
                      {"scattered_session_ids" ["a2"] "deleted_session_ids" []}
                      {"scattered_session_ids" [] "deleted_session_ids" ["a2"]}))

                  screen/refresh-projects!
                  #(swap! calls conj [:refresh])

                  vis/notify!
                  (fn [message & _]
                    (swap! notices conj message))]

      (#'screen/sidebar-row-menu! nil group-entry nil)
      (is (= [[:archive "g1" {:archived true}] [:refresh]] @calls))
      (reset! fail? true)
      (reset! choice :unarchive-group)
      (#'screen/sidebar-row-menu!
       nil
       (assoc group-entry :group (assoc group-release "archived_at" "today"))
       nil)
      (is (= [:archive "g1" {:archived false}] (last @calls)))
      (is (some #(str/includes? % "Could not unarchive group") @notices))
      (reset! fail? false)
      (reset! choice :delete)
      (#'screen/sidebar-row-menu! nil group-entry nil)
      (is (= 3 (count @calls)) "Cancel at the choice does not touch the gateway")
      (reset! mode :detach)
      (#'screen/sidebar-row-menu! nil group-entry nil)
      (is (= [[:delete "g1" :detach] [:refresh]] (take-last 2 @calls)))
      (is (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
      (is (some #(= :tab-2 (:id %)) (:tabs @state/app-db)) "Detach keeps the open session")
      (reset! mode :with-sessions)
      (#'screen/sidebar-row-menu! nil group-entry nil)
      (is (= [[:delete "g1" :with-sessions] [:refresh]] (take-last 2 @calls)))
      (is (not-any? #(= :tab-2 (:id %)) (:tabs @state/app-db))
          "Delete prunes only its returned IDs")
      (reset! fail? true)
      (#'screen/sidebar-row-menu! nil group-entry nil)
      (is (= [:delete "g1" :with-sessions] (last @calls)))
      (is (some #(str/includes? % "Could not delete group") @notices)))))

(deftest groups-set-menu-reveals-its-own-archive-test
  (let [db
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages "a"] {:sessions [] :grouped []})
            (assoc-in [:project-sidebar :selected "a"] #{"a2"}))

        reads
        (atom [])]

    (with-redefs [state/app-db
                  (atom db)

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/select-dialog!
                  (fn [_ _ items]
                    (is (some #(= :toggle-group-archive (:id %)) items))
                    {:id :toggle-group-archive})]

      (with-redefs-fn {#'screen/load-project-page! #(swap! reads conj %)}
        (fn []
          (#'screen/sidebar-row-menu!
           nil
           (first (filter #(= :groups (:set %)) (projects/sidebar-entries @state/app-db)))
           nil)
          (is (= ["a"] @reads))
          (is (true? (get-in @state/app-db [:project-sidebar :group-archived? "a"])))
          (is (nil? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
          (is (empty? (get-in @state/app-db [:project-sidebar :selected "a"]))))))))

(deftest project-inventory-refresh-keeps-its-row-test
  (let [saved
        {"id" "a1" "title" "Pinned"}

        base
        (-> (fixture-db)
            (assoc-in [:project-sidebar :pages] {"a" {:sessions [saved]} "b" {:sessions []}})
            (assoc-in [:project-sidebar :expanded] #{"a"}))

        focused
        (first (filter #(= "a1" (get-in % [:session "id"])) (projects/sidebar-entries base)))

        db
        (assoc-in base [:project-sidebar :index] (:index focused))]

    (with-redefs [state/app-db
                  (atom db)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-list-projects
                  (constantly [project-b project-a])

                  vis/gateway-projects-overview
                  (constantly {})

                  screen/load-project-page!
                  (fn [_])]

      (#'screen/refresh-projects!)
      (is (= "a1"
             (get-in (nth (projects/sidebar-entries @state/app-db)
                          (dec (get-in @state/app-db [:project-sidebar :index])))
                     [:session "id"])))
      (is (true? (get-in @state/app-db [:project-sidebar :focused?]))))))

(deftest project-chooser-creates-a-gateway-folder-test
  (let [field
        (assoc (projects/add-field-listing {:text "/work/" :cursor 6}
                                           "/work/"
                                           (get browse-listing "entries"))
          :listing-path "/work")

        calls
        (atom [])

        db
        (assoc-in (fixture-db) [:project-sidebar :adding] field)]

    (is (= [:add-folder] (projects/key-action db (KeyStroke. \n true false))))
    (with-redefs [state/app-db
                  (atom db)

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/text-input-dialog!
                  (fn [_ _ _ & _]
                    "new")

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-create-directory!
                  (fn [parent name]
                    (swap! calls conj [:mkdir parent name])
                    {"path" "/work/new"})]

      (#'screen/create-project-folder! nil field #(swap! calls conj [:add %]))
      (is (= [[:mkdir "/work" "new"] [:add "/work/new"]] @calls)))))

(deftest project-chooser-folder-cancel-and-failure-test
  (let [field
        {:listing-path "/work"}

        calls
        (atom [])]

    (with-redefs [state/app-db
                  (atom (fixture-db))

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/text-input-dialog!
                  (fn [_ _ _ & _]
                    nil)

                  vis/worker-future
                  (fn [_ _]
                    (swap! calls conj :worker))]

      (#'screen/create-project-folder! nil field #(swap! calls conj [:add %]))
      (is (empty? @calls))
      (is (not (get-in @state/app-db [:project-sidebar :saving?]))))
    (with-redefs [state/app-db
                  (atom (fixture-db))

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/text-input-dialog!
                  (fn [_ _ _ & _]
                    "new")

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-create-directory!
                  (fn [_ _]
                    (throw (ex-info "No permission" {})))]

      (#'screen/create-project-folder! nil field #(swap! calls conj [:add %]))
      (is (empty? @calls))
      (is (not (get-in @state/app-db [:project-sidebar :saving?])))
      (is (re-find #"No permission" (get-in @state/app-db [:project-sidebar :error]))))))

(deftest project-chooser-opens-existing-or-saved-session-test
  (let [opened
        (atom [])

        pages
        (atom 0)]

    (with-redefs [state/app-db
                  (atom (fixture-db))

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-list-sessions-page
                  (fn [_]
                    (swap! pages inc)
                    {:sessions [{"id" "saved"}]})]

      (#'screen/choose-project!
       project-b
       #(swap! opened conj [:open %])
       #(swap! opened conj [:new %]))
      (is (= 0 @pages) "An open view switches without a gateway read")
      (is (= "b" (:active-project-id @state/app-db)))
      (is (empty? @opened))
      (swap! state/app-db update
        :tabs
        #(filterv (fn [tab]
                    (= "a" (:project-id tab)))
           %))
      (#'screen/choose-project!
       project-b
       #(swap! opened conj [:open %])
       #(swap! opened conj [:new %]))
      (is (= [[:open "saved"]] @opened))
      (is (= 1 @pages)))))

(deftest project-removal-is-confirmed-and-failure-preserves-sessions-test
  (let [answer
        (atom false)

        failure
        (atom false)

        calls
        (atom [])

        initial
        (-> (fixture-db)
            (assoc-in [:project-sidebar :pages] {"a" {:sessions [{"id" "a2"}]}})
            (assoc-in [:project-sidebar :selected "a"] #{"a2"}))]

    (with-redefs [state/app-db
                  (atom initial)

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/confirm-dialog!
                  (fn [_ title message]
                    (is (= "Remove project" title))
                    (is (str/includes? (str message) "cannot be undone"))
                    @answer)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-delete-project!
                  (fn [pid opts]
                    (swap! calls conj [:delete pid opts])
                    (when @failure (throw (ex-info "offline" {})))
                    {"deleted_session_ids" ["a2"]})

                  screen/refresh-projects!
                  #(swap! calls conj [:refresh])

                  vis/notify!
                  (fn [& _])]

      (#'screen/remove-project! nil project-a)
      (is (empty? @calls) "Cancel sends no destructive request")
      (reset! answer true)
      (reset! failure true)
      (#'screen/remove-project! nil project-a)
      (is (= [[:delete "a" {:is-recursive? true}]] @calls))
      (is (= "a2" (get-in @state/app-db [:project-sidebar :pages "a" :sessions 0 "id"])))
      (is (some #(= :tab-2 (:id %)) (:tabs @state/app-db)))
      (is (str/includes? (get-in @state/app-db [:project-sidebar :error]) "Remove failed"))
      (reset! failure false)
      (#'screen/remove-project! nil project-a)
      (is (= [:refresh] (last @calls)))
      (is (not-any? #(= :tab-2 (:id %)) (:tabs @state/app-db)))
      (is (= "b" (:active-project-id @state/app-db))
          "Removing the active project focuses a surviving one")
      (is (not-any? #(= "a" (:project-id %)) (:tabs @state/app-db)))
      (is (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
      (is (nil? (get-in @state/app-db [:project-sidebar :removing]))))))

(deftest project-menu-chooses-or-removes-only-the-project-row-test
  (let [choice
        (atom :use-project)

        calls
        (atom [])]

    (with-redefs [state/app-db
                  (atom (fixture-db))

                  screen/with-dialog-lock
                  (fn [f]
                    (f))

                  dlg/select-dialog!
                  (fn [_ _ items]
                    (is (= #{:use-project :delete-project}
                           (set (map :id
                                     (filter #(#{:use-project :delete-project} (:id %)) items)))))
                    {:id @choice})

                  screen/remove-project!
                  (fn [_ project]
                    (swap! calls conj [:remove (get project "id")]))]

      (#'screen/sidebar-row-menu!
       nil
       {:kind :project-select :project project-a}
       nil
       #(swap! calls conj [:choose (get % "id")]))
      (reset! choice :delete-project)
      (#'screen/sidebar-row-menu!
       nil
       {:kind :project-select :project project-b}
       nil
       #(swap! calls conj [:choose (get % "id")]))
      (is (= [[:choose "a"] [:remove "b"]] @calls)))))

(deftest empty-project-chooses-its-own-root-test
  (let [started (atom nil)]
    (with-redefs [state/app-db (atom (update (fixture-db)
                                             :tabs
                                             #(filterv (fn [tab]
                                                         (= "a" (:project-id tab)))
                                                %)))
                  vis/worker-future (fn [_ f]
                                      (f))
                  vis/gateway-list-sessions-page (constantly {:sessions [] :grouped []})]

      (#'screen/choose-project!
       project-b
       (fn [_]
         (throw (ex-info "No saved row" {})))
       (fn [root build-id]
         (reset! started [root build-id])))
      (is (= "/work/companion" (first @started)))
      (is (= "b" (:active-project-id @state/app-db)))
      (is (= (second @started) (:build-id (last (:tabs @state/app-db))))))))

(deftest project-removal-shows-progress-before-request-test
  (let [pending (atom nil)]
    (with-redefs [state/app-db (atom (fixture-db))
                  screen/with-dialog-lock (fn [f]
                                            (f))
                  dlg/confirm-dialog! (fn [& _]
                                        true)
                  vis/worker-future (fn [_ f]
                                      (reset! pending f))]

      (#'screen/remove-project! nil project-a)
      (is (= "a" (get-in @state/app-db [:project-sidebar :removing])))
      (is (fn? @pending))
      (let [capture (cap/capture!
                      {:cols 100
                       :rows 18
                       :paint!
                       (fn [{:keys [screen]}]
                         (projects/paint! (.newTextGraphics screen) @state/app-db 100 18))})]
        (is (str/includes? (cap/frame-text capture) "Removing…"))))))

(deftest project-page-refresh-keeps-focus-on-the-same-project-test
  (let [base
        (-> (fixture-db)
            (assoc :active-project-id "b"
                   :session {:id "b1"})
            (assoc-in [:project-sidebar :pages] {"a" {:sessions [{"id" "old"}]} "b" {:sessions []}})
            (assoc-in [:project-sidebar :expanded] #{"a"}))

        b-row
        (first (filter #(and (= :project-select (:kind %)) (= "b" (get-in % [:project "id"])))
                       (projects/sidebar-entries base)))

        db
        (assoc-in base [:project-sidebar :index] (:index b-row))]

    (with-redefs [state/app-db
                  (atom db)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-list-session-groups-page
                  (constantly {:groups [] :total 0})

                  vis/gateway-list-sessions-page
                  (constantly {:sessions [{"id" "old"} {"id" "new" "title" "New session"}]
                               :grouped []
                               :awaiting []
                               :has-more false})]

      (#'screen/load-project-page! "a")
      (is (= "b"
             (get-in (nth (projects/sidebar-entries @state/app-db)
                          (dec (get-in @state/app-db [:project-sidebar :index])))
                     [:project "id"]))))))

(deftest project-search-finds-unloaded-and-archived-results-test
  (let [db (-> (fixture-db)
               (assoc-in [:project-sidebar :pages] {"a" {:sessions []}})
               (assoc-in [:project-sidebar :search]
                         {:text "needle"
                          :cursor 6
                          :loading? false
                          :matches [{:id "b9" :in-reply? true}]
                          :rows [{:session {"id" "b9"
                                            "project_id" "b"
                                            "archived_at" "yesterday"
                                            "title" "Unopened result"}
                                  :match {:id "b9" :in-reply? true}}]}))]
    (is (= [:search] (projects/key-action (fixture-db) (KeyStroke. \/ false false))))
    (is (= ["b9"]
           (->> (projects/sidebar-entries db)
                (keep #(get-in % [:session "id"]))
                vec)))
    (is (= "b" (get-in (first (filter :session (projects/sidebar-entries db))) [:project "id"])))))

(deftest project-automatic-refresh-holds-new-rows-until-accepted-test
  (let [db (-> (fixture-db)
               (assoc-in [:project-sidebar :expanded] #{"a"})
               (assoc-in [:project-sidebar :pages]
                         {"a" {:sessions [{"id" "a1" "title" "First"}]
                               :grouped []
                               :after nil
                               :history []
                               :request-id "first"}}))]
    (with-redefs [state/app-db (atom db)]
      (state/dispatch [:project-page-loaded "a" "first"
                       {:sessions [{"id" "a-new" "title" "New"} {"id" "a1" "title" "Updated"}]
                        :grouped []
                        :total 2} {:groups [] :total 0} nil true])
      (is (= ["a1"]
             (mapv #(get % "id") (get-in @state/app-db [:project-sidebar :pages "a" :sessions]))))
      (is (= "Updated" (get-in @state/app-db [:project-sidebar :pages "a" :sessions 0 "title"])))
      (is (= [:updates "a"]
             (:action (first (filter #(= :project-updates (:kind %))
                                     (projects/sidebar-entries @state/app-db)))))))))

(deftest project-automatic-refresh-holds-arrivals-after-empty-page-test
  (let [db (-> (fixture-db)
               (assoc-in [:project-sidebar :expanded] #{"a"})
               (assoc-in [:project-sidebar :pages]
                         {"a" {:sessions [] :grouped [] :request-id "empty" :has-more false}}))]
    (with-redefs [state/app-db (atom db)]
      (state/dispatch
        [:project-page-loaded "a" "empty"
         {:sessions [{"id" "new" "title" "Arrived"}] :grouped [] :has-more false :total 1}
         {:groups [] :total 0} nil true])
      (is (empty? (get-in @state/app-db [:project-sidebar :pages "a" :sessions])))
      (is (= 1 (get-in @state/app-db [:project-sidebar :pages "a" :pending-count])))
      (let [update-row (first (filter #(= :project-updates (:kind %))
                                      (projects/sidebar-entries @state/app-db)))]
        (is (= [:updates "a"] (:action update-row)))
        (is (= "1 new update · Enter to show" (:label update-row)))))))

(deftest project-search-hydrates-ranked-unloaded-group-and-archive-test
  (let [db
        (-> (fixture-db)
            (assoc :layout {:rows 18})
            (assoc-in [:project-sidebar :index] 1)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :group-folds] {"a" #{"g"}})
            (assoc-in [:project-sidebar :pages]
                      {"a" {:sessions [{"id" "a1" "title" "Already loaded"}]
                            :after "cursor"
                            :history [nil]}}))

        original
        (:project-sidebar db)

        matches
        [{:id "b9" :in-reply? true :reply-snippet "answer found"}
         {:id "a5" :in-request? true :request-snippet "request found"}]

        calls
        (atom [])]

    (with-redefs [state/app-db
                  (atom db)

                  vis/worker-future
                  (fn [_ f]
                    (f))

                  vis/gateway-search-session-matches
                  (fn [q]
                    (swap! calls conj [:search q])
                    matches)

                  vis/gateway-list-sessions-page
                  (fn [options]
                    (swap! calls conj [:hydrate options])
                    {:sessions [{"id" "a5" "project_id" "a" "group_id" "g" "title" "Grouped"}
                                {"id" "b9"
                                 "project_id" "b"
                                 "archived_at" "yesterday"
                                 "title" "Archived reply"}]})]

      (let [press! (fn [key]
                     (#'screen/project-sidebar-key!
                      (cap/key-stroke key)
                      identity
                      identity
                      identity
                      identity
                      nil))]
        (press! \/)
        (is (= "" (get-in @state/app-db [:project-sidebar :search :text])))
        (press! \q)
        (is (= [[:search "q"] [:hydrate {:ids ["b9" "a5"] :limit 5 :archived :include}]] @calls))
        (let [entries (projects/sidebar-entries @state/app-db)]
          (is (= ["b9" "a5"] (mapv #(get-in % [:session "id"]) (filter :session entries))))
          (is (= ["Companion / Archived reply" "Vis / Grouped"]
                 (mapv :label (filter :session entries))))
          (is (some #(str/includes? (:label %) "reply: answer found") entries))
          (is (some #(str/includes? (:label %) "request: request found") entries)))
        (press! :esc)
        (is (nil? (get-in @state/app-db [:project-sidebar :search])))
        (is (= (select-keys original [:pages :expanded :group-folds :index])
               (select-keys (:project-sidebar @state/app-db)
                            [:pages :expanded :group-folds :index])))))))

(deftest project-search-stale-requests-pagination-and-recovery-test
  (let [jobs
        (atom [])

        calls
        (atom [])

        failure?
        (atom false)

        matches
        (mapv (fn [i]
                {:id (str "s" i) :in-title? true})
              (range 7))]

    (with-redefs [state/app-db
                  (atom (assoc (fixture-db) :layout {:rows 18}))

                  vis/worker-future
                  (fn [_ f]
                    (swap! jobs conj f))

                  vis/gateway-search-session-matches
                  (fn [q]
                    (swap! calls conj [:search q])
                    (if (= q "none") [] matches))

                  vis/gateway-list-sessions-page
                  (fn [options]
                    (swap! calls conj [:hydrate options])
                    (when @failure? (throw (ex-info "offline" {})))
                    {:sessions (mapv (fn [id]
                                       {"id" id "project_id" "a" "title" id})
                                     (reverse (:ids options)))})]

      (state/dispatch [:project-search-open])
      (#'screen/search-projects! {:text "old" :cursor 3})
      (#'screen/search-projects! {:text "new" :cursor 3})
      (is (some #(= "Searching sessions…" (:label %)) (projects/sidebar-entries @state/app-db)))
      ((second @jobs))
      ((first @jobs))
      (is (= [[:search "new"]
              [:hydrate {:ids ["s0" "s1" "s2" "s3" "s4"] :limit 5 :archived :include}]]
             @calls))
      (is (= ["s0" "s1" "s2" "s3" "s4"]
             (mapv #(get-in % [:session "id"])
                   (get-in @state/app-db [:project-sidebar :search :rows]))))
      (let [more (first (filter #(= [:search-page :next] (:action %))
                                (projects/sidebar-entries @state/app-db)))]
        (state/dispatch [:project-sidebar {:index (:index more)}])
        (#'screen/project-sidebar-key!
         (cap/key-stroke :enter)
         identity
         identity
         identity
         identity
         nil))
      ((last @jobs))
      (is (= 5 (get-in @state/app-db [:project-sidebar :search :offset])))
      (is (= ["s5" "s6"]
             (mapv #(get-in % [:session "id"])
                   (get-in @state/app-db [:project-sidebar :search :rows]))))
      (is (false? (get-in @state/app-db [:project-sidebar :search :has-more?])))
      (let [previous (first (filter #(= [:search-page :previous] (:action %))
                                    (projects/sidebar-entries @state/app-db)))]
        (state/dispatch [:project-sidebar {:index (:index previous)}])
        (#'screen/project-sidebar-key!
         (cap/key-stroke :enter)
         identity
         identity
         identity
         identity
         nil)
        ((last @jobs)))
      (is (zero? (get-in @state/app-db [:project-sidebar :search :offset])))
      (let [stale-id (get-in @state/app-db [:project-sidebar :search :request-id])]
        (#'screen/search-projects! {:text "none" :cursor 4})
        (state/dispatch [:project-search-loaded stale-id matches [] 5 5 false])
        ((last @jobs)))
      (is (some #(= "No saved sessions match" (:label %)) (projects/sidebar-entries @state/app-db)))
      (reset! failure? true)
      (#'screen/search-projects! {:text "error" :cursor 5})
      ((last @jobs))
      (is (some #(str/includes? (:label %) "Search failed · gateway unavailable")
                (projects/sidebar-entries @state/app-db)))
      (reset! failure? false)
      (#'screen/search-projects! {:text "retry" :cursor 5})
      ((last @jobs))
      (is (nil? (get-in @state/app-db [:project-sidebar :search :error])))
      (is (= 5 (count (get-in @state/app-db [:project-sidebar :search :rows])))))))

(deftest project-update-affordance-preserves-focused-session-test
  (let [base
        (-> (fixture-db)
            (assoc-in [:project-sidebar :expanded] #{"a"})
            (assoc-in [:project-sidebar :pages]
                      {"a" {:sessions [{"id" "a1" "title" "Current"}]
                            :grouped []
                            :request-id "loaded"
                            :has-more false}}))

        focused
        (->> (projects/sidebar-entries base)
             (filter #(= "a1" (get-in % [:session "id"])))
             first
             :index)

        db
        (assoc-in base [:project-sidebar :index] focused)]

    (with-redefs [state/app-db (atom db)]
      (state/dispatch [:project-page-loaded "a" "loaded"
                       {:sessions [{"id" "new" "title" "Incoming"}
                                   {"id" "a1" "title" "Current revised"}]
                        :grouped []
                        :has-more false
                        :total 2} {:groups [] :total 0} nil true])
      (is (= "a1"
             (get-in (nth (projects/sidebar-entries @state/app-db)
                          (dec (get-in @state/app-db [:project-sidebar :index])))
                     [:session "id"])))
      (is (= 1 (get-in @state/app-db [:project-sidebar :pages "a" :pending-count])))
      (let [capture (cap/capture!
                      {:cols 144
                       :rows 24
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) @state/app-db 144 24))})
            hit (first (filter #(= :project-updates (:kind %)) (.current projects/hit-map)))
            {:keys [col row]} (:bounds hit)]

        (is (nil? (:error capture)))
        (is (= [:updates "a"]
               (projects/key-action @state/app-db
                                    (MouseAction. MouseActionType/CLICK_DOWN
                                                  1
                                                  (TerminalPosition. (int col) (int row))))))
        (#'screen/project-sidebar-key!
         (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. (int col) (int row)))
         identity
         identity
         identity
         identity
         nil))
      (is (= ["new" "a1"]
             (mapv #(get % "id") (get-in @state/app-db [:project-sidebar :pages "a" :sessions]))))
      (is (zero? (get-in @state/app-db [:project-sidebar :pages "a" :pending-count])))
      (is (nil? (get-in @state/app-db [:project-sidebar :pages "a" :incoming])))
      (is (= "a1"
             (get-in (nth (projects/sidebar-entries @state/app-db)
                          (dec (get-in @state/app-db [:project-sidebar :index])))
                     [:session "id"]))))))

(deftest project-search-mouse-button-and-empty-state-test
  (with-redefs [state/app-db (atom (fixture-db))]
    (let [paint! (fn []
                   (cap/capture!
                     {:cols 144
                      :rows 24
                      :paint! (fn [{:keys [screen]}]
                                (projects/paint! (.newTextGraphics screen) @state/app-db 144 24))}))
          press! (fn [key]
                   (#'screen/project-sidebar-key! key identity identity identity identity nil))]

      (is (nil? (:error (paint!))))
      (let [hit (first (filter #(= :project-search (:kind %)) (.current projects/hit-map)))
            {:keys [col row]} (:bounds hit)]

        (is (some? hit))
        (press!
          (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. (int col) (int row)))))
      (is (some? (get-in @state/app-db [:project-sidebar :search])))
      (is (some #(= "Type to search saved sessions" (:label %))
                (projects/sidebar-entries @state/app-db)))
      (is (nil? (:error (paint!))))
      (is (some #(= :project-search-field (:kind %)) (.current projects/hit-map)))
      (press! (cap/key-stroke :esc))
      (is (nil? (get-in @state/app-db [:project-sidebar :search]))))))

(deftest project-refresh-subscription-lifecycle-test
  (let [sink
        (atom nil)

        stops
        (atom 0)

        reads
        (atom [])

        db
        (assoc-in (fixture-db) [:project-sidebar :open?] false)]

    (with-redefs [state/app-db
                  (atom db)

                  vis/gateway-fleet-subscribe!
                  (fn [callback]
                    (reset! sink callback)
                    #(swap! stops inc))

                  screen/refresh-projects!
                  (fn [automatic?]
                    (swap! reads conj automatic?))]

      (let [stop (#'screen/start-projects-refresh!)]
        (try (is (fn? @sink))
             (@sink {"type" "session.title_updated"})
             (Thread/sleep 100)
             (is (empty? @reads) "Closed panes do not refresh")
             (state/dispatch [:project-sidebar {:open? true}])
             (loop [attempt 0]
               (when (and (empty? @reads) (< attempt 40)) (Thread/sleep 50) (recur (inc attempt))))
             (is (= [true] @reads))
             (finally (stop)))
        (is (= 1 @stops))))))
