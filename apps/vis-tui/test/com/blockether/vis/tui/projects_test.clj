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
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize TextColor$RGB]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
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

(defn- tuple-rgb
  "A captured `[r g b]` cell colour as a Lanterna colour."
  [[r g b]]
  (TextColor$RGB. (int r) (int g) (int b)))

(defn- cell-contrast
  "The WCAG contrast between the ink and the paper of one captured cell."
  [{:keys [fg bg]}]
  (theme/contrast-ratio (tuple-rgb fg) (tuple-rgb bg)))

(defn- cell-text
  "The characters of captured `row` from column `from` up to column `to`."
  [capture row from to]
  (apply str (map #(str (get-in capture [:frames 0 row % :ch])) (range from to))))

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

(defdescribe project-view-isolation-test
             (it "project view isolation"
                 (with-redefs [state/app-db (atom (fixture-db))]
                   (let [background (get-in @state/app-db [:tab-locals :tab-3])]
                     (state/dispatch [:select-tab-index :next])
                     (expect (= :tab-2 (:active-tab-id @state/app-db)))
                     (state/dispatch [:select-tab-index :next])
                     (expect (= :tab-1 (:active-tab-id @state/app-db)))
                     (expect (= background (get-in @state/app-db [:tab-locals :tab-3])))))
                 (with-redefs [state/app-db (atom (fixture-db))]
                   (state/dispatch [:select-tab-index 1])
                   (state/dispatch [:select-project "b" [] "unused"])
                   (expect (= :tab-3 (:active-tab-id @state/app-db)))
                   (expect (= [:tab-3] (mapv :id (model/project-tabs @state/app-db))))
                   (expect (= "background-turn" (:gateway-turn-id @state/app-db)))
                   (expect (= [{:text "Continue tests" :client-id "queued"}]
                              (:pending-sends @state/app-db)))
                   (state/dispatch [:select-project "a" [] "unused"])
                   (expect (= :tab-2 (:active-tab-id @state/app-db)))
                   (expect (= 3 (count (:tabs @state/app-db))))
                   (state/dispatch [:select-tab-index 0])
                   (expect (= "Keep this draft" (input/input->text (:input @state/app-db))))
                   (expect (true? (get-in @state/app-db [:tab-locals :tab-3 :loading?])))
                   (state/dispatch [:select-project "a" [] "unused"])
                   (expect (= :tab-1 (:active-tab-id @state/app-db))))))

(defdescribe project-background-hydration-test
             (it "project background hydration"
                 ;; Regression: slow hydration used to focus its tab after the user had switched away.
                 (with-redefs [state/app-db (atom (fixture-db))]
                   (state/dispatch [:preallocate-project-tabs
                                    [{:session-id "c1" :project-id "c" :label "Pending"}]])
                   (state/dispatch [:open-session-tab {:id "c1"} [{:role :user :text "Restored"}]
                                    nil true])
                   (expect (= :tab-1 (:active-tab-id @state/app-db)))
                   (expect (= "a" (:active-project-id @state/app-db)))
                   (expect (= "Keep this draft" (input/input->text (:input @state/app-db))))
                   (state/dispatch [:select-project "c" [] "unused"])
                   (expect (= "c1" (str (get-in @state/app-db [:session :id]))))
                   (expect (= "Restored" (get-in @state/app-db [:messages 0 :text]))))))

(defdescribe project-empty-and-building-test
             (it "project empty and building"
                 (with-redefs [state/app-db (atom (fixture-db))]
                   (state/dispatch [:select-project "empty" [] "new-project"])
                   (expect (= 1 (count (model/project-tabs @state/app-db))))
                   (state/dispatch [:select-project "a" [] "unused"])
                   (state/dispatch [:bind-built-session "new-project" {:id "new-session"} []
                                    {"root" "/work/new"}])
                   (expect (= "a" (:active-project-id @state/app-db)))
                   (expect (= "Keep this draft" (input/input->text (:input @state/app-db))))
                   (state/dispatch [:select-project "empty" [] "unused"])
                   (expect (= "new-session" (get-in @state/app-db [:session :id])))
                   (expect (= "/work/new" (:workspace/root @state/app-db)))
                   (expect (= 1 (count (model/project-tabs @state/app-db)))))))

(defdescribe project-close-keeps-last-tab-test
             (it "project close keeps last tab"
                 (with-redefs [state/app-db (atom (fixture-db))]
                   (state/dispatch [:select-project "b" [] "unused"])
                   (state/dispatch [:close-tab])
                   (expect (= 3 (count (:tabs @state/app-db))))
                   (expect (= :tab-3 (:active-tab-id @state/app-db))))))

(defdescribe project-close-selects-visible-neighbor-test
             (it "project close selects visible neighbor"
                 (let [db
                       (fixture-db)

                       [a1 a2 b1]
                       (:tabs db)]

                   (with-redefs [state/app-db (atom (assoc db
                                                      :session nil
                                                      :tabs [b1 a1 a2
                                                             {:id :tab-4 :project-id "a"}]))]
                     (state/dispatch [:close-tab])
                     (expect (= :tab-2 (:active-tab-id @state/app-db)))
                     (expect (= "background-turn"
                                (get-in @state/app-db [:tab-locals :tab-3 :gateway-turn-id])))))))

(defdescribe project-startup-does-not-steal-focus-test
             (it "project startup does not steal focus"
                 ;; A launch-root lookup can finish after the user has already selected a project.
                 (with-redefs [state/app-db
                               (atom (dissoc (fixture-db) :active-project-id :launch-project-id))

                               vis/gateway-ensure-project-for-root!
                               (fn [& _]
                                 (state/dispatch [:select-project "b" [] "unused"])
                                 project-a)]

                   (expect (= "a" (#'screen/ensure-launch-project-id!)))
                   (expect (= "b" (:active-project-id @state/app-db)))
                   (expect (= "a" (:launch-project-id @state/app-db)))
                   (expect (= "a" (#'screen/ensure-launch-project-id!)))
                   (expect (= "background-turn" (:gateway-turn-id @state/app-db))))))

(defdescribe project-persistence-is-scoped-test
             (it "project persistence is scoped"
                 (let [calls (atom [])]
                   (with-redefs [state/app-db (atom (fixture-db))
                                 vis/gateway-reorder-project-sessions! #(swap! calls conj [%1 %2])]

                     (#'screen/persist-tabs-once!)
                     (expect (= [["a" ["a1" "a2"]] ["b" ["b1"]]] @calls))))))

(defdescribe
  project-request-order-and-race-test
  (it
    "project request order and race"
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
        (expect (empty? @requests) "No gateway I/O or local view creation on input")
        ((first @workers))
        (expect (= [:groups :sessions :sessions] (mapv first @requests)))
        (expect (= :aside (get-in @requests [1 1 :grouped])))
        (expect (<= (get-in @requests [1 1 :limit]) 30))
        (expect (= ["a1"] (get-in @requests [2 1 :ids])))
        (expect (= "a1" (get-in @state/app-db [:project-sidebar :pages "a" :current "id"])))
        (expect (= "saved-outside-tui"
                   (get-in @state/app-db [:project-sidebar :pages "a" :sessions 0 "id"])))
        (expect (= 3 (count (:tabs @state/app-db))) "Pages must not allocate open views")))))

(defdescribe
  project-errors-and-add-test
  (it
    "project errors and add"
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
      (expect (= "a" (:active-project-id @state/app-db)))
      (expect (str/includes? (get-in @state/app-db [:project-sidebar :pages "c" :error])
                             "Load failed"))
      (#'screen/refresh-projects!)
      (expect (= [project-a project-b] (get-in @state/app-db [:project-sidebar :items])))
      (expect (false? (get-in @state/app-db [:project-sidebar :loading?]))))
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
           (press! \+) (expect (= {:text "" :cursor 0}
                                  (select-keys (get-in @state/app-db [:project-sidebar :adding])
                                               [:text :cursor]))) (doseq [c " /work/new "]
                                                                    (press! c))
           (expect (= " /work/new " (get-in @state/app-db [:project-sidebar :adding :text])))
           (press! :enter) (expect (nil? (get-in @state/app-db [:project-sidebar :adding])))
           (expect (= ["/work/new" project-b] @calls)))))))

(defdescribe
  project-sidebar-input-test
  (it "project sidebar input"
      (let [db (fixture-db)]
        (expect (= :switch-project (keymap/prefix-action-for \w)))
        (expect (= "C-x w" (keymap/label-for :switch-project)))
        (expect (= [:add] (projects/key-action db (cap/key-stroke \+))))
        (expect (= [:select project-a] (projects/key-action db (cap/key-stroke :enter))))
        (expect (= [:move 1] (projects/key-action db (cap/key-stroke :down))))
        (expect (= [:blur] (projects/key-action db (cap/key-stroke :esc))))
        (expect (nil? (projects/key-action db (KeyStroke. \x true false))))
        (expect (nil? (projects/key-action (assoc-in db [:project-sidebar :open?] false)
                                           (cap/key-stroke \+)))))
      ;; With its field open the rail is an editor: ordinary keys are the path, Enter
      ;; files the trimmed path and Esc closes the field without leaving the rail.
      (let [typing (assoc-in (fixture-db) [:project-sidebar :adding] {:text "/work/vi" :cursor 8})]
        (expect (= [:adding {:text "/work/vis" :cursor 9}]
                   (projects/key-action typing (cap/key-stroke \s))))
        (expect (= [:adding {:text "/work/v" :cursor 7}]
                   (projects/key-action typing (cap/key-stroke :backspace))))
        (expect (= [:adding {:text "/work/vi" :cursor 0}]
                   (projects/key-action typing (cap/key-stroke :home))))
        (expect (= [:adding {:text "/work/vi" :cursor 7}]
                   (projects/key-action typing (cap/key-stroke :left))))
        (expect (= [:add-commit "/work/vi"] (projects/key-action typing (cap/key-stroke :enter))))
        (expect (= [:adding nil] (projects/key-action typing (cap/key-stroke :esc))))
        ;; Nothing is listed yet, so there is no completion to highlight or fill in:
        ;; the field is left exactly as it was.
        (expect (= [:adding {:text "/work/vi" :cursor 8}]
                   (projects/key-action typing (cap/key-stroke :down))))
        (expect (= [:noop] (projects/key-action typing (cap/key-stroke :tab))))
        ;; C-x still reaches global navigation from inside the field.
        (expect (nil? (projects/key-action typing (KeyStroke. \x true false))))
        ;; A pasted path arrives as one line: newlines and paste markers never enter it.
        (expect (= {:text "/work/v is" :cursor 10}
                   (projects/add-field-insert {:text "/work/v" :cursor 7} "\n is\uE201"))))))

(defdescribe
  project-sidebar-width-test
  (it "project sidebar width"
      ;; Target 40%, preserving 40 cells (56 for alerts) and 60 for docked chat.
      (doseq [[cols width chat-cols] [[24 24 24] [40 40 40] [80 40 80] [99 40 99] [100 40 60]
                                      [120 48 72] [144 57 87] [168 67 101] [240 96 144]]]
        (let [db (fixture-db)]
          (expect (= {:left 0
                      :width width
                      :rows 24
                      :chat-cols chat-cols
                      :chat-left (if (= cols chat-cols) 0 width)}
                     (projects/geometry db cols 24)))
          (expect (= cols
                     (projects/chat-cols (assoc-in db [:project-sidebar :open?] false) cols)))))
      (doseq [cols (range 24 241)]
        (let [{:keys [left width chat-left chat-cols]} (projects/geometry (fixture-db) cols 24)]
          (expect (= (min cols (max 40 (quot (* cols 2) 5))) width))
          (expect (zero? left))
          (expect (= cols (+ chat-left chat-cols)))
          (expect (or (= cols chat-cols) (>= chat-cols 60)))))
      (doseq [[cols width chat-cols] [[80 56 80] [120 56 64] [144 57 87] [240 96 144]]]
        (let [geom (projects/geometry (attention-fixture-db) cols 24)]
          (expect (= width (:width geom)))
          (expect (= chat-cols (:chat-cols geom)))))))

(defdescribe
  project-sidebar-grid-test
  (it
    "project sidebar grid"
    (doseq [cols [24 26 40 72 80 85 86 96 120 144 240]]
      (let [db (fixture-db)
            capture (cap/capture! {:cols cols
                                   :rows 18
                                   :paint!
                                   (fn [{:keys [screen]}]
                                     (projects/paint! (.newTextGraphics screen) db cols 18))})
            text (cap/frame-text capture)
            width (:width (projects/geometry db cols 18))]

        (expect (nil? (:error capture)))
        (expect (str/includes? text "Projects"))
        (when (>= cols 40)
          (expect (str/includes? text "Companion"))
          (expect (str/includes? text (if (< width 48) "1 tab|1 LIVE" "1 tab | 1 LIVE"))))
        (when (>= cols 26) (expect (str/includes? text "1 LIVE")))
        (let [footer (nth (str/split-lines text) 16)
              inside (subs footer 1 (dec (count footer)))
              lead (count (take-while #(= \space %) inside))
              trail (count (take-while #(= \space %) (reverse inside)))]

          ;; Whole pairs only, the key list first, and centered on the rail.
          (doseq [label ["? keys" "g menu"]]
            (expect (str/includes? footer label)))
          (when (>= width 40) (expect (str/includes? footer "s settings")))
          (when (>= width 96) (expect (str/includes? footer "Esc chat")))
          (expect (<= (abs (- lead trail)) 1)))
        (expect (= "├" (get-in capture [:frames 0 15 0 :ch])))
        (let [header (nth (str/split-lines text) 1)]
          ;; Keep one unfilled cell between the title and the Add button.
          (expect (= "│ Projects  + " (subs header 0 14)))
          (expect (not (str/includes? header "⌕"))))
        (expect (= :project-rail (:kind (.lookup projects/hit-map 10 1))))
        (expect (= :project-add (:kind (.lookup projects/hit-map 12 1))))
        (expect (= [:add]
                   (projects/key-action
                     db
                     (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 12 1)))))
        (expect (not-any? #(= :project-search (:kind %)) (.current projects/hit-map)))
        (expect (= :project-hide (:kind (.lookup projects/hit-map (- width 4) 1))))
        (expect (= "┌" (get-in capture [:frames 0 0 0 :ch])))
        (expect (= "┤" (get-in capture [:frames 0 2 (dec width) :ch])))
        (expect (= "┘" (get-in capture [:frames 0 17 (dec width) :ch])))
        (expect (= [:select project-b]
                   (projects/key-action
                     db
                     (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 4 5)))))))))

(defdescribe
  project-sidebar-header-fit-test
  (it "keeps the spaced header controls separate on narrow rails"
      (doseq [cols [18 19]]
        (let [db (fixture-db)
              capture (cap/capture! {:cols cols
                                     :rows 18
                                     :paint!
                                     (fn [{:keys [screen]}]
                                       (projects/paint! (.newTextGraphics screen) db cols 18))})
              hits (.current projects/hit-map)
              add-bounds (:bounds (first (filter #(= :project-add (:kind %)) hits)))
              close-bounds (:bounds (first (filter #(= :project-hide (:kind %)) hits)))]

          (expect (nil? (:error capture)))
          (if (= cols 18)
            (expect (nil? add-bounds))
            (do (expect (= {:col 11 :row 1 :width 3} add-bounds))
                (expect (<= (+ (:col add-bounds) (:width add-bounds)) (:col close-bounds)))))))))

(defdescribe
  project-sidebar-inline-add-test
  (it "project sidebar inline add"
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

          (expect (nil? (:error capture)))
          (expect (str/includes? (nth lines 3) "› /work/new"))
          (expect (str/includes? (nth lines 16) "Esc cancel"))
          (expect (= 12 (.getColumn ^TerminalPosition @caret)))
          (expect (= 3 (.getRow ^TerminalPosition @caret)))))
      ;; A resting rail claims no caret, so the chat keeps its own.
      (let [capture (cap/capture!
                      {:cols 40
                       :rows 18
                       :paint! (fn [{:keys [screen]}]
                                 (projects/paint! (.newTextGraphics screen) (fixture-db) 40 18))})]
        (expect (nil? (:error capture)))
        (expect (nil? (:ret capture))))))

(defdescribe
  project-rail-takes-a-pasted-path-test
  (it "project rail takes a pasted path"
      ;; A path pasted while the field is open belongs to the field — not to the chat
      ;; composer, and not to attachment intake.
      (with-redefs [state/app-db
                    (atom
                      (assoc-in (fixture-db) [:project-sidebar :adding] {:text "/work/" :cursor 6}))

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
          (expect (= {:text "/work/vis" :cursor 9} (select-keys (field) [:text :cursor])))
          ;; The paste lands with the directories it completes to already read.
          (expect (= ["vis" "vis-python-runtime"]
                     (mapv #(get % "name") (#'projects/add-field-matches (field)))))
          (expect (= before (:input @state/app-db)))))
      ;; With no field open the composer keeps the paste.
      (with-redefs [state/app-db (atom (fixture-db))]
        (let [before (:input @state/app-db)]
          (#'screen/insert-pasted-text! "vis")
          (expect (nil? (get-in @state/app-db [:project-sidebar :adding])))
          (expect (not= before (:input @state/app-db)))))))

(defdescribe
  project-rail-completes-a-typed-path-test
  (it "project rail completes a typed path"
      ;; The field is a path completer: ONE listing per directory, narrowed locally,
      ;; and Enter adds whichever directory is highlighted.
      (expect (= "/work/" (projects/add-field-dir "/work/vi")))
      (expect (= "/work/" (projects/add-field-dir "/work/")))
      (expect (= "" (projects/add-field-dir "work")))
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
        (expect (= ["vis" "vis-python-runtime"]
                   (mapv #(get % "name") (#'projects/add-field-matches field))))
        (let [[verb filled] (action field :tab)]
          (expect (= :adding verb))
          (expect (= {:text "/work/vis/" :cursor 10} (select-keys filled [:text :cursor]))))
        (let [[_ first-row]
              (action field :down)

              [_ second-row]
              (action first-row :down)

              [_ released]
              (action second-row \s)]

          (expect (= 0 (:index first-row)))
          (expect (= 1 (:index second-row)))
          (expect (= [:add-commit "/work/vis-python-runtime"] (action second-row :enter)))
          ;; Typing releases the highlight, so Enter adds the typed path again.
          (expect (nil? (:index released)))
          (expect (= [:add-commit "/work/viss"] (action released :enter)))
          ;; Stepping above the first row hands the keyboard back to the text.
          (expect (nil? (:index (second (action first-row :up))))))
        ;; Enter with nothing highlighted still adds what the human typed.
        (expect (= [:add-commit "/work/vis"] (action field :enter)))
        ;; A read still in flight says so instead of claiming there is no match.
        (expect (true? (:loading? (projects/add-field-listing field "/other/" nil))))
        (expect (= [] (:rows (projects/add-field-listing field "/other/" nil)))))))

(defdescribe
  project-rail-paints-its-completions-test
  (it "project rail paints its completions"
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

        (expect (nil? (:error capture)))
        (expect (str/includes? (nth lines 4) "vis/"))
        (expect (str/includes? (nth lines 4) "main"))
        (expect (str/includes? (nth lines 6) "notes/"))
        ;; The project list yields its rows to the open field.
        (expect (not (str/includes? text "Companion")))
        (expect (str/includes? (nth lines 16) "Esc cancel")))
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

        (expect (nil? (:error capture)))
        (expect (str/includes? (cap/frame-text capture) "No matching directory")))))

(defdescribe
  project-rail-suggests-directories-from-the-gateway-test
  (it "project rail suggests directories from the gateway"
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
            (expect (= [""] @asked))
            (expect (= 3 (count (:rows (field)))))
            (expect (false? (:loading? (field))))
            ;; Typing inside the SAME directory reuses the listing it already has.
            (press! \v)
            (press! \i)
            (expect (= [""] @asked))
            ;; Tab fills the obvious match in and reads the directory it opened.
            (press! :tab)
            (expect (= "/work/vis/" (:text (field))))
            (expect (= ["" "/work/vis/"] @asked))
            (press! :down)
            (press! :enter)
            (expect (= ["/work/vis"] @added))
            (expect (nil? (field))))))))

(defdescribe
  project-sidebar-footer-error-test
  (it "project sidebar footer error"
      (let [db
            (assoc-in (fixture-db) [:project-sidebar :error] "Project lookup failed")

            capture
            (cap/capture! {:cols 144
                           :rows 18
                           :paint! (fn [{:keys [screen]}]
                                     (projects/paint! (.newTextGraphics screen) db 144 18))})

            lines
            (str/split-lines (cap/frame-text capture))]

        (expect (str/includes? (nth lines 14) "Project lookup failed"))
        (doseq [label ["? keys" "g menu" "s settings" "a add project"]]
          (expect (str/includes? (nth lines 16) label)))
        (expect (= 8
                   (count (projects/visible-entries
                            (assoc-in db [:project-sidebar :items] (vec (repeat 30 project-a)))
                            16)))))))

(defdescribe
  project-sidebar-label-width-test
  (it "project sidebar label width"
      (doseq [[cols index label fits?] [[40 0 "vis-python-runtime" true]
                                        [40 1 "vis-extension-center-demo-with-long-name" false]
                                        [168 1 "vis-extension-center-demo-with-long-name" true]]]
        (let [db (assoc-in (fixture-db) [:project-sidebar :items index "name"] label)
              capture (cap/capture! {:cols cols
                                     :rows 18
                                     :paint!
                                     (fn [{:keys [screen]}]
                                       (projects/paint! (.newTextGraphics screen) db cols 18))})
              row (nth (str/split-lines (cap/frame-text capture)) (+ 4 index))]

          (expect (nil? (:error capture)))
          (expect (= fits? (str/includes? row label)))
          (expect (= (not fits?) (str/includes? row "…")))))))

(defdescribe
  project-name-reads-as-a-home-path-test
  (it "project name reads as a home path"
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

        (expect (= "~/CryptoSyf" (projects/project-label {"name" (str home "/CryptoSyf")})))
        (expect (= "Vis" (projects/project-label project-a)))
        (expect (= "/opt/shared/vis" (projects/project-label {"name" "/opt/shared/vis"})))
        (expect (= "Untitled project" (projects/project-label {})))
        (expect (nil? (:error capture)))
        (expect (str/includes? row "~/CryptoSyf"))
        (expect (not (str/includes? row home))))))

(defdescribe project-sidebar-overflow-test
             (it "project sidebar overflow"
                 (let [sidebar
                       {:items (vec (repeat 50 project-a)) :index 50}

                       visible
                       (projects/visible-entries {:project-sidebar sidebar} 16)]

                   (expect (= 9 (count visible)))
                   (expect (= 50 (:index (last visible)))))))

(defn review-terminal
  "Production terminal defaults shared by backend parity and live project review."
  ^HtmlTerminal [cols rows]
  (-> (HtmlTerminal/builder)
      (.initialSize (TerminalSize. cols rows))
      (.defaultForeground theme/text-fg)
      (.defaultBackground theme/terminal-bg)
      (.title "Vis · Projects")
      (.build)))

(defdescribe
  project-full-frame-and-input-test
  (it
    "project full frame and input"
    (with-redefs [timg/images-protocol
                  (constantly nil)

                  vis/get-router
                  (constantly nil)]

      (doseq [cols [40 80 85 86 96 120 144]]
        (let [capture (cap/capture!
                        {:cols cols
                         :rows 24
                         :paint! (fn [{:keys [screen]}]
                                   (let [db
                                         (assoc-in (fixture-db) [:project-sidebar :focused?] false)
                                         layout (#'screen/render-frame! screen cols 24 db 1000)]

                                     (#'screen/paint-frame! screen :input cols 24 db 1000 layout)
                                     layout))})
              hidden (cap/capture! {:cols cols
                                    :rows 24
                                    :paint!
                                    (fn [{:keys [screen]}]
                                      (#'screen/render-frame!
                                       screen
                                       cols
                                       24
                                       (assoc-in (fixture-db) [:project-sidebar :open?] false)
                                       1000))})]

          (expect (nil? (:error capture)))
          ;; A narrow rail overlays the composer: input-only paint must not erase it.
          (expect (= (cap/frame-text capture 0) (cap/frame-text capture)))
          (expect (str/includes? (cap/frame-text capture) "? keys"))
          (expect (= (projects/chat-cols (fixture-db) cols) (get-in capture [:ret :cols])))
          (expect (nil? (:error hidden)))
          (expect (= cols (get-in hidden [:ret :cols])))
          (expect (not (str/includes? (cap/frame-text hidden) "Projects")))
          (expect (not-any? #(= :project-select (:kind %)) (.current projects/hit-map))))))))

(defdescribe
  project-screen-backend-parity-test
  (it "project screen backend parity"
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
                                         :paint!
                                         (fn [{:keys [^TerminalScreen screen]}]
                                           (#'screen/render-frame! screen cols 24 db 1000)
                                           (#'screen/render-frame! html-screen cols 24 db 1000)
                                           (expect (= (for [y (range 24)
                                                            x (range cols)]

                                                        (.getFrontCharacter screen x y))
                                                      (for [y (range 24)
                                                            x (range cols)]

                                                        (.getFrontCharacter html-screen x y)))))})]
              (expect (nil? (:error capture)))
              (expect (str/includes? (.renderHtml html) "Projects"))))))))

(defdescribe
  project-sidebar-dispatch-test
  (it
    "project sidebar dispatch"
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

          (expect (true? (#'screen/project-sidebar-key!
                          (cap/key-stroke :down)
                          select!
                          add!
                          refresh!
                          menu!
                          nil)))
          (#'screen/project-sidebar-key! (cap/key-stroke :enter) select! add! refresh! menu! nil)
          (expect (= "b" (:active-project-id @state/app-db)))
          (expect (= "background-turn" (:gateway-turn-id @state/app-db)))
          (#'screen/project-sidebar-key! (cap/key-stroke \+) select! add! refresh! menu! nil)
          (expect (= {:text "" :cursor 0}
                     (select-keys (get-in @state/app-db [:project-sidebar :adding])
                                  [:text :cursor])))
          (#'screen/project-sidebar-key! (cap/key-stroke \/) select! add! refresh! menu! nil)
          (#'screen/project-sidebar-key! (cap/key-stroke :enter) select! add! refresh! menu! nil)
          (expect (= ["/"] @added))
          (expect (nil? (get-in @state/app-db [:project-sidebar :adding])))
          (#'screen/project-sidebar-key! (cap/key-stroke :esc) select! add! refresh! menu! nil)
          (expect (false? (get-in @state/app-db [:project-sidebar :focused?])))
          (expect
            (nil?
              (#'screen/project-sidebar-key! (cap/key-stroke \a) select! add! refresh! menu! nil)))
          (let [capture (cap/capture! {:keys [\w]
                                       :paint! (fn [{:keys [screen]}]
                                                 (#'screen/resolve-prefix!
                                                  screen
                                                  @state/app-db
                                                  (input/handle-key (KeyStroke. \x true false)
                                                                    (:input @state/app-db))))})]
            (expect (nil? (:error capture)))
            (expect (= :switch-project (get-in capture [:ret :action]))))
          (#'screen/toggle-project-sidebar!)
          (expect (false? (get-in @state/app-db [:project-sidebar :open?])))
          (#'screen/toggle-project-sidebar!)
          (expect (true? (get-in @state/app-db [:project-sidebar :open?])))
          (expect (= 0 (get-in @state/app-db [:project-sidebar :index])))
          (expect (= 3 (count (:tabs @state/app-db)))))))))

(defdescribe project-sidebar-cursor-test
             (it "project sidebar cursor"
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
                                                  :paint! (fn [{:keys [^TerminalScreen screen]}]
                                                            (#'screen/render-frame!
                                                             screen
                                                             cols
                                                             24
                                                             (assoc-in (fixture-db)
                                                               [:project-sidebar :focused?]
                                                               focused?)
                                                             1000)
                                                            (.getCursorPosition screen))})]
                       (expect (nil? (:error capture)))
                       (expect (= (or focused? (= cols 40)) (nil? (:ret capture)))))))))

(defdescribe
  project-rail-caret-in-the-render-frame-test
  (it "project rail caret in the render frame"
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

          (expect (nil? (:error capture)))
          (expect (= 12 (.getColumn ^TerminalPosition (:ret capture))))
          (expect (= 3 (.getRow ^TerminalPosition (:ret capture))))))))

(defdescribe
  project-chat-pointer-surface-test
  (it "project chat pointer surface"
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

                             (expect (some? hit))
                             (expect (= hit
                                        (.lookup interactions/hit-map
                                                 (.getColumn (.getPosition local))
                                                 (.getRow (.getPosition local)))))
                             (expect (not= :select
                                           (first (projects/key-action (fixture-db) mouse))))))})]
          (expect (nil? (:error capture)))))
      (let [mouse
            (MouseAction. MouseActionType/SCROLL_DOWN 0 (TerminalPosition. 70 8) 7)

            local
            ^MouseAction (projects/chat-key mouse 40)]

        (expect (= (TerminalPosition. 30 8) (.getPosition local)))
        (expect (= 7 (.getCount local)))
        (expect (= (.getScrollDelta mouse) (.getScrollDelta local)))
        (expect (identical? mouse (projects/chat-key mouse 0))))
      (expect (= (cap/key-stroke \a) (projects/chat-key (cap/key-stroke \a) 40)))))

(defdescribe
  project-surface-cells-and-cursor-test
  (it "project surface cells and cursor"
      (let [capture (cap/capture!
                      {:cols 30
                       :rows 6
                       :paint!
                       (fn [{:keys [^TerminalScreen screen]}]
                         (binding [frame/*column-offset* 10]
                           (.putString (frame/surface-graphics screen 20 6) 0 1 "界 hello")
                           (expect (= "界" (.getCharacterString (frame/back-character screen 0 1))))
                           (frame/set-character! screen 5 2 (frame/back-character screen 3 1))
                           (frame/set-cursor! screen (TerminalPosition. 5 2))
                           (expect (= (TerminalPosition. 15 2) (.getCursorPosition screen)))
                           (expect (= "h" (.getCharacterString (.getBackCharacter screen 15 2))))
                           (frame/set-cursor! screen nil)
                           (expect (nil? (.getCursorPosition screen)))))})]
        (expect (nil? (:error capture)))
        (expect (= "界" (get-in capture [:frames 0 1 10 :ch])))
        (expect (= "h" (get-in capture [:frames 0 1 13 :ch]))))))

(defdescribe
  project-overlay-and-media-origin-test
  (it
    "project overlay and media origin"
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

          (expect (nil? (:error capture)))
          (expect (= (if (= cols 80)
                       []
                       [{:col (+ 2 (:width (projects/geometry (fixture-db) cols 24)))
                         :row 5
                         :img {:id "fixture"}}])
                     @placed))))
      (let [capture (cap/capture! {:cols 144
                                   :rows 24
                                   :paint! (fn [{:keys [screen]}]
                                             (#'screen/render-frame!
                                              screen
                                              144
                                              24
                                              (assoc (fixture-db) :help-open? true)
                                              1000))})]
        (expect (nil? (:error capture)))
        (expect (= 144 (get-in capture [:ret :cols])))
        (expect (zero? (get-in capture [:ret :chat-left])))
        (expect (empty? (.current projects/hit-map)))))))

(defdescribe
  project-input-counts-and-lifecycle-test
  (it "project input counts and lifecycle"
      ;; A parked turn still has :loading? true, but needs input rather than running.
      (with-redefs [state/app-db (atom (attention-fixture-db))]
        (let [summary #(mapv (fn [entry]
                               (select-keys entry [:tab-count :running :needs-input]))
                             (filter (fn [entry]
                                       (= :project-select (:kind entry)))
                                     (projects/sidebar-entries @state/app-db)))]
          (expect (= [{:tab-count 2 :running 0 :needs-input 0}
                      {:tab-count 2 :running 1 :needs-input 1}]
                     (summary)))
          (expect (= [:select :select :session]
                     (mapv (comp first :action) (projects/sidebar-entries @state/app-db))))
          (let [form (get-in @state/app-db [:tab-locals :tab-3 :human-input])]
            (state/dispatch [:human-input-open (assoc-in form [:request :id] "request-b-next")]))
          (state/dispatch [:select-tab-by-session "b1"])
          (expect (= 1 (:needs-input (second (summary)))))
          (state/dispatch [:human-input-close "request-b"])
          (expect (= "request-b-next" (get-in @state/app-db [:human-input :request :id])))
          (expect (= 1 (:needs-input (second (summary)))) "Count tabs, not queued requests")
          (state/dispatch [:human-input-close "request-b-next"])
          (expect (= {:tab-count 2 :running 2 :needs-input 0} (second (summary))))
          (expect (= 2 (count (projects/sidebar-entries @state/app-db))))))))

(defdescribe
  project-header-counts-are-the-gateways-test
  (it
    "project header counts are the gateways"
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

      (expect (= [{"live_count" 3 "awaiting_count" 1 "unread_count" 2}
                  {"live_count" 1 "awaiting_count" 0 "unread_count" 0}]
                 (mapv #(select-keys % ["live_count" "awaiting_count" "unread_count"]) items)))
      ;; A session parked on a human is a LIVE session: 3 live beside 1 waiting is
      ;; 2 running, never 3.
      (expect (= [{:tab-count 2 :running 2 :needs-input 1 :unread 2}
                  {:tab-count 1 :running 1 :needs-input 0 :unread 0}]
                 (mapv #(select-keys % [:tab-count :running :needs-input :unread]) headers)))
      (expect (= "2 tabs | 1 HITL · 2 LIVE · 2 NEW" (#'projects/row-status (first headers) nil 60)))
      ;; A project the overview never mentions keeps exactly what it came with.
      (expect (= [project-a] (projects/with-gateway-counts [project-a] {"projects" []})))
      (expect (= [project-a] (projects/with-gateway-counts [project-a] nil))))))

(defdescribe
  project-input-grid-and-navigation-test
  (it
    "project input grid and navigation"
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

            (expect (nil? (:error capture)))
            (expect (str/includes? text "2 tabs | 1 HITL · 1 LIVE"))
            (expect (re-find #"Mobile navigation +HITL" text))
            (expect (not (str/includes? text "! Mobile")))
            (expect (= (inc (get-in (first (filter #(and (= :project-select (:kind %))
                                                         (= "b" (get-in % [:project "id"])))
                                                   (.current projects/hit-map)))
                                    [:bounds :row]))
                       row)
                    "The alert appears immediately below its project")
            (let [col (inc (long (str/index-of (nth (str/split-lines text) row) "HITL")))
                  cell (get-in capture [:frames 0 row col])]

              (expect (true? (:bold cell)))
              (expect (= (#'theme-test/rgb-tuple
                          (theme/legible-ink theme/warning-fg (tuple-rgb (:bg cell))))
                         (:fg cell))))
            (if pointer?
              (handle!
                (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. (int col) (int row))))
              (do (handle! (cap/key-stroke :down)) (handle! (cap/key-stroke :enter))))
            (expect (= :tab-3 (:active-tab-id @state/app-db))
                    "Open the waiting tab, not the remembered project tab")
            (expect (= "b" (:active-project-id @state/app-db)))
            (expect (= "request-b" (get-in @state/app-db [:human-input :request :id])))
            (expect (= "Keep this draft"
                       (input/input->text (get-in @state/app-db [:tab-locals :tab-1 :input]))))
            (expect (= background (get-in @state/app-db [:tab-locals :tab-4])))
            (expect (= [false] @refreshes))
            (expect (false? (get-in @state/app-db [:project-sidebar :focused?])))))))))

(defdescribe project-input-scroll-test
             (it "project input scroll"
                 (let [tabs
                       (mapv (fn [n]
                               {:id (str n) :project-id "b" :label (str "Session " n)})
                             (range 30))

                       db
                       (assoc (fixture-db)
                         :tabs tabs
                         :tab-locals
                         (into {}
                               (map (fn [{:keys [id]}]
                                      [id {:session {:id id} :human-input {:request {:id id}}}])
                                    tabs))
                         :project-sidebar {:open? true :focused? true :index 31 :items [project-b]})

                       visible
                       (projects/visible-entries db 16)]

                   (expect (= 9 (count visible)))
                   (expect (= :project-select (:kind (first visible)))
                           "Keep the parent visible above a long waiting group")
                   (expect (= 31 (:index (last visible))))
                   (expect (= [:session "29"] (projects/key-action db (cap/key-stroke :enter))))
                   (expect (empty? (projects/visible-entries db 7))))))

(defdescribe
  project-input-band-keeps-docked-sidebar-test
  (it "project input band keeps docked sidebar"
      (with-redefs [timg/images-protocol
                    (constantly nil)

                    vis/get-router
                    (constantly nil)

                    state/app-db
                    (atom (attention-fixture-db))]

        (state/dispatch [:select-tab-by-session "b1"])
        (state/dispatch [:project-sidebar {:focused? false}])
        (doseq [cols [40 80 144]]
          (let [capture (cap/capture!
                          {:cols cols
                           :rows 30
                           :paint! (fn [{:keys [screen]}]
                                     (#'screen/render-frame! screen cols 30 @state/app-db 1000))})
                text (cap/frame-text capture)]

            (expect (nil? (:error capture)))
            (expect (str/includes? text "Choose platform"))
            (expect (= (= cols 144) (str/includes? text "Projects")))
            (expect (= (if (= cols 144) 57 0) (get-in capture [:ret :chat-left]))))))))

(defdescribe
  project-input-prefix-navigation-test
  (it
    "project input prefix navigation"
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

        (expect (nil? (:error capture)))
        (expect (not (#'screen/human-input-owns-key? @state/app-db prefix)))
        (expect (#'screen/human-input-owns-key? @state/app-db (cap/key-stroke \w)))
        (expect (#'screen/human-input-owns-key? @state/app-db (cap/key-stroke :esc)))
        (expect (= :switch-project (get-in capture [:ret :action])))
        (#'screen/toggle-project-sidebar!)
        (expect (false? (get-in @state/app-db [:project-sidebar :open?])))
        (#'screen/toggle-project-sidebar!)
        (expect (true? (get-in @state/app-db [:project-sidebar :open?])))
        (expect (= form (:human-input @state/app-db)))))))

(defdescribe
  project-sidebar-theme-contrast-test
  (it "project sidebar theme contrast"
      (let [before @theme/active-theme-id]
        (try (doseq [id (shared-theme/available-theme-ids)]
               (theme/apply-theme! (keyword id))
               (doseq [fg [theme/dialog-fg theme/dialog-hint-key theme/warning-fg]
                       bg [theme/terminal-bg theme/input-field-bg]]

                 (expect (>= (#'theme-test/contrast-ratio fg bg) 4.5) (str id " sidebar text")))
               (let [highlight (theme/mix-color theme/terminal-bg theme/header-active-tab-bg 0.14)]
                 (doseq [[label fg] [[:text theme/dialog-fg] [:hint theme/dialog-hint-key]
                                     [:warning theme/warning-fg]]]
                   (expect (>= (#'theme-test/contrast-ratio fg highlight) 4.5)
                           (str id " focused project row " label))))
               (expect (>= (#'theme-test/contrast-ratio theme/dialog-hint theme/terminal-bg) 4.5)
                       (str id " sidebar hints and borders"))
               (let [[fg bg] (theme/chip-tint :warning)]
                 (expect (>= (#'theme-test/contrast-ratio fg bg) 4.5)
                         (str id " yellow button text"))
                 (expect (= theme/warning-button-bg bg))
                 (expect (> (.getGreen ^com.googlecode.lanterna.TextColor bg)
                            (.getBlue ^com.googlecode.lanterna.TextColor bg)))))
             (finally (theme/apply-theme! before))))))

(defdescribe
  project-unread-completion-and-read-test
  (it "project unread completion and read"
      ;; Completed background replies were marked in the tab strip but absent from Projects.
      (with-redefs [state/app-db (atom (attention-fixture-db))]
        (let [entries #(projects/sidebar-entries @state/app-db)
              summary #(first (filter (fn [entry]
                                        (= project-b (:project entry)))
                                      (entries)))
              answer [{:type :text :text "Snapshot tests passed."}]]

          (state/dispatch [:message-received :tab-4 answer {:status :completed}])
          (expect (= {:running 0 :needs-input 1 :unread 1}
                     (select-keys (summary) [:running :needs-input :unread])))
          (expect (= [[:select project-a] [:select project-b] [:session "b1"] [:session "b2"]]
                     (mapv :action (entries))))
          (expect (pos? (long (:render-version @state/app-db 0)))
                  "A background answer repaints the rail")
          (state/dispatch [:select-tab-by-session "a2"])
          (expect (= 1 (:unread (summary))) "Reading another tab must not clear NEW")
          (state/dispatch [:select-tab-by-session "b1"])
          (expect (= 1 (:unread (summary)))
                  "Opening the waiting tab must not clear a different reply")
          (state/dispatch [:select-tab-by-session "b2"])
          (expect (= 0 (:unread (summary))))
          (expect (not-any? #(= :project-unread (:kind %)) (entries)))
          (expect (= answer (:content (last (:messages @state/app-db)))))
          (state/dispatch [:select-tab-by-session "a1"])
          (expect (= 0 (:unread (summary))) "NEW does not return after leaving a read reply")
          (state/dispatch [:message-received :tab-1 answer {:status :completed}])
          (expect (every? #(zero? (:unread %))
                          (filter #(= :project-select (:kind %)) (entries))))))))

(defdescribe project-unread-cancel-and-replay-test
             (it "project unread cancel and replay"
                 (with-redefs [state/app-db (atom (attention-fixture-db))]
                   (state/dispatch [:message-received :tab-4 [] {:status :cancelled}])
                   (expect (not-any? #(= :project-unread (:kind %))
                                     (projects/sidebar-entries @state/app-db)))
                   (state/dispatch [:message-received :tab-4 [{:type :text :text "Old result"}]
                                    {:status :completed :client-turn-id "already-settled"}])
                   (expect (not-any? #(= :project-unread (:kind %))
                                     (projects/sidebar-entries @state/app-db))))))

(defdescribe
  project-unread-grid-and-navigation-test
  (it
    "project unread grid and navigation"
    (doseq [pointer? [true false]]
      (let [refreshes (atom [])]
        (with-redefs [state/app-db (atom (assoc (news-fixture-db)
                                           :project-active-tabs {"b" :tab-4}))]
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

            (expect (nil? (:error capture)))
            (expect (str/includes? text "3 tabs | 1 HITL · 1 LIVE · 1 NEW"))
            (expect (re-find #"Keyboard navigation +New" text))
            (expect (= 7 row))
            (let [cell (get-in capture
                               [:frames 0 row
                                (str/index-of (nth (str/split-lines text) row) "New")])]
              (expect (= "N" (str (:ch cell))))
              (expect (true? (:bold cell)))
              (expect (= (#'theme-test/rgb-tuple
                          (theme/legible-ink theme/header-active-tab-bg (tuple-rgb (:bg cell))))
                         (:fg cell))))
            (if pointer?
              ;; Click the status, not only the row's title.
              (handle! (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 51 (int row))))
              (do (handle! (cap/key-stroke :down)) (handle! (cap/key-stroke :enter))))
            (expect (= :tab-5 (:active-tab-id @state/app-db)))
            (expect (= "b" (:active-project-id @state/app-db)))
            (expect (= "Keyboard navigation is ready." (:text (last (:messages @state/app-db)))))
            (expect (not-any? #(= :project-unread (:kind %))
                              (projects/sidebar-entries @state/app-db)))
            (expect (= waiting (get-in @state/app-db [:tab-locals :tab-3])))
            (expect (= running (get-in @state/app-db [:tab-locals :tab-4])))
            (expect (= "Keep this draft"
                       (input/input->text (get-in @state/app-db [:tab-locals :tab-1 :input]))))
            (expect (= [false] @refreshes))))))))

(defdescribe project-unread-and-input-share-one-row-test
             (it "project unread and input share one row"
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
                                      :paint!
                                      (fn [{:keys [screen]}]
                                        (projects/paint! (.newTextGraphics screen) db 144 24))})]

                   (expect (= 2 (:unread (second entries))))
                   (expect (= 1 (count (filter #(= :tab-3 (:tab-id %)) entries))))
                   (expect (re-find #"Mobile navigation +HITL +│" (cap/frame-text capture))))))

(defdescribe
  project-alert-status-test
  (it "shows project alerts as bold Title case statuses that stay legible"
      (let [before @theme/active-theme-id]
        (try (doseq [id (shared-theme/available-theme-ids)
                     cols [40 44 80 144]]

               (theme/apply-theme! (keyword id))
               (let [db (news-fixture-db)
                     capture (cap/capture!
                               {:cols cols
                                :rows 24
                                :paint! (fn [{:keys [screen]}]
                                          (projects/paint! (.newTextGraphics screen) db cols 24))})
                     text (cap/frame-text capture)
                     width (long (:width (projects/geometry db cols 24)))
                     rows (mapv #(get-in % [:bounds :row])
                                (filter #(#{:project-input :project-unread} (:kind %))
                                        (.current projects/hit-map)))]

                 (expect (nil? (:error capture)))
                 (expect (seq rows))
                 (when (= cols 44)
                   (expect (str/includes? text "Companion"))
                   (expect (str/includes? text "3 tabs|1 HITL·1 LIVE·1 NEW")))
                 (doseq [row rows]
                   (let [line (cell-text capture row 0 width)]
                     (expect (not (str/includes? line "NEW")))
                     (expect (re-find #"(HITL|New) │$" line))
                     (expect (not (re-find #"[!●]" line))))
                   (doseq [col (range (- width 5) (- width 2))
                           :let [cell (get-in capture [:frames 0 row col])]]

                     (expect (true? (:bold cell)))
                     (expect (<= theme/legible-contrast (cell-contrast cell)))))))
             (finally (theme/apply-theme! before))))))

(defdescribe project-unread-scroll-keeps-parent-test
             (it "project unread scroll keeps parent"
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

                   (expect (= 9 (count visible)))
                   (expect (= :project-select (:kind (first visible))))
                   (expect (= 30 (:unread (first visible))))
                   (expect (= 31 (:index (last visible))))
                   (expect (= [:session "29"] (projects/key-action db (cap/key-stroke :enter)))))))

(def group-release {"id" "g1" "name" "Release apps" "color" "violet" "session_count" 3})

(def group-gateway {"id" "g2" "name" "Gateway" "color" "cyan" "session_count" 1})

(defn grouped-db
  "Project rail with two groups filed under the first project (BLO-167)."
  []
  (-> (fixture-db)
      (assoc-in [:project-sidebar :groups] {"a" [group-release group-gateway]})))

(defdescribe
  project-groups-nest-under-their-project-test
  (it "project groups nest under their project"
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

        (expect (= [:project-select :project-group :project-group :project-select]
                   (mapv :kind entries)))
        (expect (= ["Release apps" "Gateway"] (mapv :label groups)))
        (expect (= ["violet" "cyan"] (mapv :color groups)))
        (expect (= [3 1] (mapv :group-count groups)))
        (expect (= [2 3] (mapv :index groups)))
        (expect (= [:select project-a] (:action (first groups))))
        (expect (= "" (#'projects/row-status (first groups) nil 40)))
        (expect (= "" (#'projects/row-status (second groups) nil 40)))
        (expect (nil? (:error capture)))
        (expect (str/includes? (cap/frame-text capture) "Release apps"))
        (expect (not (str/includes? (cap/frame-text capture) "3 sessions")))
        (expect (str/includes? (cap/frame-text capture) "g menu"))
        (doseq [[group hit] (map vector
                                 groups
                                 (filter #(= :project-group (:kind %))
                                         (.current projects/hit-map)))]
          (expect (= (#'theme-test/rgb-tuple (theme/group-ink (:color group)))
                     (get-in capture [:frames 0 (get-in hit [:bounds :row]) 1 :fg])))))))

(defdescribe project-rail-g-opens-the-row-menu-test
             (it "project rail g opens the row menu"
                 ;; `g` acts on the row under the cursor: group verbs on a group row, project
                 ;; verbs on a project row, and nothing above the first row.
                 (let [db
                       (grouped-db)

                       action
                       #(projects/key-action (assoc-in db [:project-sidebar :index] %)
                                             (cap/key-stroke \g))]

                   (expect (= :project-select (:kind (second (action 1)))))
                   (expect (= :menu (first (action 2))))
                   (expect (= "Release apps" (:label (second (action 2)))))
                   (expect (= "Gateway" (:label (second (action 3)))))
                   (expect (= [:noop] (action 0)))
                   (expect (= [:noop] (action 99))))))

(defdescribe saved-project-page-shows-sessions-not-opened-here-test
             (it "saved project page shows sessions not opened here"
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

                   (expect (= [:project-select :project-set :project-group :project-session
                               :project-set :project-session :project-session :project-page
                               :project-select]
                              (mapv :kind entries)))
                   (expect (= ["filed" "never-opened" "loose-2"]
                              (mapv #(get-in % [:session "id"])
                                    (filter #(= :project-session (:kind %)) entries))))
                   (expect (= [:session "never-opened"] (:action (nth entries 5))))
                   (expect (= [:page "a" :next] (:action (nth entries 7))))
                   (expect (= [:toggle-project "a"] (:action (first entries)))))))

(defdescribe
  sidebar-status-color-consistency-test
  (it
    "uses the status helper for header counts and session rows in every theme"
    (let [before
          @theme/active-theme-id

          sessions
          [{"id" "input"
            "group_id" "g1"
            "title" "Input session"
            "live" true
            "is_awaiting_input" true}
           {"id" "working" "group_id" "g1" "title" "Working session" "live" true}
           {"id" "unread"
            "group_id" "g1"
            "title" "Unread session"
            "is_unread" true
            "unread_answers" 1}]

          db
          (-> (fixture-db)
              (assoc :active-project-id "b"
                     :session {:id "not-current"})
              (assoc-in [:project-sidebar :items]
                        [(assoc project-a
                           "session_count" 3
                           "live_count" 2
                           "awaiting_count" 1
                           "unread_count" 1)])
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :groups "a"] [group-release])
              (assoc-in [:project-sidebar :pages "a"] {:sessions [] :grouped sessions}))]

      (try
        (doseq [theme-id
                (shared-theme/available-theme-ids)

                index
                [0 1 3 4 5 6]]

          (theme/apply-theme! (keyword theme-id))
          (let [capture
                (cap/capture! {:cols 200
                               :rows 30
                               :paint! (fn [{:keys [screen]}]
                                         (projects/paint!
                                           (.newTextGraphics screen)
                                           (assoc-in db [:project-sidebar :index] index)
                                           200
                                           30))})

                text
                (cap/frame-text capture)]

            (expect (nil? (:error capture)))
            (expect (str/includes? text "1 LIVE"))
            (expect (str/includes? text "Working session"))
            (doseq [[row line]
                    (map-indexed vector (str/split-lines text))

                    [label status]
                    [["1 HITL" "HITL"] ["1 LIVE" "Live"] ["1 NEW" "New"] ["HITL" "HITL"]
                     ["Live" "Live"] ["New" "New"]]

                    :let [col
                          (str/index-of line label)]
                    :when (some? col)
                    offset
                    (range (count label))

                    :when (not= \space (nth label offset))]

              (let [cell
                    (get-in capture [:frames 0 row (+ col offset)])

                    expected
                    (#'theme-test/rgb-tuple (dlg/session-status-ink status (tuple-rgb (:bg cell))))]

                (expect (= expected (:fg cell)) (str theme-id " " index " " label))
                (expect (<= theme/legible-contrast (cell-contrast cell)))))))
        (finally (theme/apply-theme! before)))))
  (it "uses the same colors for uppercase labels and labels with counts"
      (doseq [status ["Live" "New" "Waiting" "Stopped" "Dirty" "Archived" "Idle" "HITL"]]
        (expect (= (dlg/session-status-ink status)
                   (dlg/session-status-ink (str (str/upper-case status) " ×2")))))))

(defdescribe
  sidebar-header-status-format-test
  (it "separates session totals from uppercase status counts"
      (doseq [[counts expected] [[{} "14 sessions"] [{:running 2} "14 sessions | 2 LIVE"]
                                 [{:needs-input 1 :unread 3} "14 sessions | 1 HITL · 3 NEW"]
                                 [{:running 2 :needs-input 1 :unread 3}
                                  "14 sessions | 1 HITL · 2 LIVE · 3 NEW"]]]
        (let [entry (merge {:kind :project-select
                            :project project-a
                            :tab-count 14
                            :running 0
                            :needs-input 0
                            :unread 0
                            :action [:toggle-project "a"]}
                           counts)]
          (expect (= expected (#'projects/row-status entry nil 60)))
          (expect (= (-> expected
                         (str/replace " | " "|")
                         (str/replace " · " "·"))
                     (#'projects/row-status entry nil 40))))))
  (it
    "counts each grouped session once, including folded groups and later loose pages"
    ;; Group totals use the complete sidecar, not visible rows or prompt counts.
    (let [sessions
          (mapv (fn [index]
                  (cond-> {"id" (str "group-" index)
                           "group_id" "g1"
                           "title" (str "Grouped session " index)}
                    (< index 3)
                    (assoc "live" true)

                    (< index 2)
                    (assoc "is_awaiting_input"
                      true "awaiting_input_count"
                      3)

                    (#{3 4} index)
                    (assoc "is_unread"
                      true "unread_answers"
                      5)))
                (range 14))

          project
          (assoc project-a
            "session_count" 14
            "live_count" 3
            "awaiting_count" 2
            "unread_count" 2)

          db
          (-> (fixture-db)
              (assoc-in [:project-sidebar :items] [project])
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :groups "a"] [group-release])
              (assoc-in [:project-sidebar :group-folds "a"] #{"g1"})
              (assoc-in [:project-sidebar :pages "a"]
                        {:sessions [{"id" "later-loose"} (first sessions) (nth sessions 3)]
                         :after "later-page"
                         :grouped sessions
                         :awaiting (vec (take 2 sessions))}))

          entries
          (projects/sidebar-entries db)

          group
          (first (filter #(= :project-group (:kind %)) entries))]

      (expect (= {:running 1 :needs-input 2 :unread 2}
                 (select-keys (first entries) [:running :needs-input :unread])))
      (expect (= {:running 1 :needs-input 2 :unread 2}
                 (select-keys group [:running :needs-input :unread])))
      (expect (= "2 HITL · 1 LIVE · 2 NEW" (#'projects/row-status group nil 60)))
      (expect (not-any? #(= :project-session (:kind %)) (filter :nested? entries)))
      (let [visited
            (assoc db :session {:id "group-3"})

            group
            (first (filter #(= :project-group (:kind %)) (projects/sidebar-entries visited)))]

        (expect (= 1 (:unread group))))
      (let [archived
            (assoc-in db [:project-sidebar :groups "a" 0 "archived_at"] "2026-10-01")

            group
            (first (filter #(= :project-group (:kind %)) (projects/sidebar-entries archived)))]

        (expect (= "" (#'projects/row-status group nil 60))))
      (doseq [cols [100 144 168]]
        (let [capture (cap/capture! {:cols cols
                                     :rows 24
                                     :paint!
                                     (fn [{:keys [screen]}]
                                       (projects/paint! (.newTextGraphics screen) db cols 24))})
              line (first (filter #(str/includes? % "Release apps")
                                  (str/split-lines (cap/frame-text capture))))]

          (expect (nil? (:error capture)))
          (expect (re-find (if (< (:width (projects/geometry db cols 24)) 48)
                             #"Release apps +2 HITL·1 LIVE·2 NEW"
                             #"Release apps +2 HITL · 1 LIVE · 2 NEW")
                           (or line ""))))))))

(defdescribe saved-project-pins-attention-and-current-off-page-test
             (it "saved project pins attention and current off page"
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

                   (expect (= ["waiting" "a1" "filed" "loose"]
                              (mapv #(get-in % [:session "id"]) sessions)))
                   (expect (= ["Attention" "Groups" "Sessions"]
                              (mapv :label (filter #(= :project-set (:kind %)) entries)))))))

(defdescribe
  saved-project-off-page-attention-opens-through-the-rail-test
  (it
    "saved project off page attention opens through the rail"
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
          (expect (= "waiting" (get-in (nth entries 2) [:session "id"])))
          (expect (= "HITL" (:status (nth entries 2))))
          (expect (= [:session "waiting"]
                     (projects/key-action @state/app-db (cap/key-stroke :enter)))))
        (#'screen/project-sidebar-key!
         (cap/key-stroke :enter)
         (fn [_])
         (fn [_])
         (fn [_])
         (fn [_])
         #(swap! opened conj %))
        (expect (= ["waiting"] @opened))
        (expect (= "a" (:project-id (first @asked))))
        (expect (= 3 (count (:tabs @state/app-db))) "A pinned row is not an open TUI view")))))

(defdescribe
  saved-project-row-state-and-metadata-test
  (it
    "saved project row state and metadata"
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
           {"id" "idle" "title" "Quiet"}
           {"id" "failed"
            "title" "Failed run"
            "was_failed" true
            "is_unread" true
            "unread_answers" 1}]

          db
          (-> (fixture-db)
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :pages "a"] {:sessions rows :grouped []}))

          entries
          (filterv #(= :project-session (:kind %)) (projects/sidebar-entries db))]

      (expect (= ["HITL ×2" "Live" "Stopped" "New ×2" "Waiting" "Dirty" "Archived" "Idle" "Stopped"]
                 (mapv :status entries)))
      (expect (= "Keep this draft" (:label (nth entries 5))))
      (expect (true? (:favorite? (nth entries 3))))
      (expect (= 4 (:turns (nth entries 3))))
      (expect (= "2026-09-24T00:00:00Z" (:modified-at (nth entries 3))))))
  (it "keeps HITL badges in the attention color"
      (with-open [terminal (review-terminal 20 2)]
        (let [g (.newTextGraphics terminal)]
          (doseq [status ["HITL" "HITL ×2"]]
            (#'projects/paint-session-status! g {:status status} 0 0 20)
            (expect (= theme/warning-fg (.getForegroundColor g))))))))

(defdescribe saved-project-attachment-only-draft-test
             (it "saved project attachment only draft"
                 (let [db
                       (-> (fixture-db)
                           (assoc :input (input/empty-input)
                                  :attachments [{:id "image"}])
                           (assoc-in [:project-sidebar :expanded] #{"a"})
                           (assoc-in [:project-sidebar :pages "a"]
                                     {:sessions [{"id" "a1" "title" nil}] :grouped []}))

                       row
                       (first (filter #(= :project-session (:kind %))
                                      (projects/sidebar-entries db)))]

                   (expect (= "1 unsent attachment" (:label row)))
                   (expect (= "Dirty" (:status row))))))

(defdescribe
  saved-session-menu-actions-use-the-gateway-test
  (it
    "saved session menu actions use the gateway"
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
          (expect (= [[:star "saved" true] [:refresh] [:star "saved" false] [:refresh]] @calls))
          (reset! calls [])
          (with-redefs [dlg/select-dialog! (fn [& _]
                                             {:id :rename-session})]
            (#'screen/sidebar-row-menu! nil entry nil))
          (expect (= [[:rename "saved" "New title"] [:refresh]] @calls))
          (reset! calls [])
          (#'screen/sidebar-row-menu! nil (assoc entry :show-details? true) nil)
          (expect (= [[:details "saved" "saved"]] @calls))
          (reset! calls [])
          (with-redefs [vis/gateway-set-session-favorite!
                        (fn [& _]
                          (throw (ex-info "offline" {})))

                        vis/notify!
                        (fn [message & _]
                          (swap! calls conj [:warning message]))]

            (#'screen/sidebar-row-menu! nil entry nil))
          (expect (= [[:warning "Could not change favorite"]] @calls)))))))

(defdescribe
  saved-session-archive-view-is-independent-and-gateway-paged-test
  (it
    "saved session archive view is independent and gateway paged"
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
        (expect (true? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
        (expect (nil? (get-in @state/app-db [:project-sidebar :pages "a" :after])))
        (expect (empty? (get-in @state/app-db [:project-sidebar :pages "a" :history])))
        (#'screen/load-project-page! "a")
        (expect (= :only (get-in @asked [1 1 :archived])))
        (expect (= :exclude (get-in @asked [2 1 :archived])))
        (let [entries (projects/sidebar-entries @state/app-db)]
          (expect (some #(= "Sessions · Archived" (:label %)) entries))
          (expect (= ["filed-active" "away"]
                     (mapv #(get-in % [:session "id"])
                           (filter #(= :project-session (:kind %)) entries)))))
        (state/dispatch [:project-session-archive-toggle "a"])
        (expect (false? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
        (expect (= [group-release] (get-in @state/app-db [:project-sidebar :groups "a"])))
        (#'screen/load-project-page! "a")
        (expect (= :exclude (get-in @asked [5 1 :archived])))
        (expect (= ["a1" "filed-active" "loose"]
                   (mapv #(get-in % [:session "id"])
                         (filter #(= :project-session (:kind %))
                                 (projects/sidebar-entries @state/app-db)))))))))

(defdescribe
  saved-session-archive-and-delete-confirmation-test
  (it
    "saved session archive and delete confirmation"
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
        (expect (empty? @calls) "Busy sessions must not be archived")
        (#'screen/sidebar-row-menu! nil entry nil)
        (expect (= [[:archive "saved" true] [:refresh]] @calls))
        (reset! calls [])
        (reset! pick :unarchive-session)
        (#'screen/sidebar-row-menu!
         nil
         (assoc entry :session (assoc (:session entry) "archived_at" "now"))
         nil)
        (expect (= [[:archive "saved" false] [:refresh]] @calls))
        (reset! calls [])
        (reset! pick :delete-session)
        (#'screen/sidebar-row-menu! nil entry nil)
        (expect (empty? @calls) "Canceling deletion leaves the gateway untouched")
        (reset! confirmed? true)
        (#'screen/sidebar-row-menu! nil entry nil)
        (expect (= [[:delete "saved"] [:refresh]] @calls))
        (expect (= :tab-1 (:active-tab-id @state/app-db))
                "Deleting an unopened row does not change focus")))))

(defdescribe
  set-creation-shortcuts-and-buttons-test
  (it
    "set creation shortcuts and buttons"
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

            group-hit
            (first (filter #(= :project-group-add (:kind %)) hits))

            group-menu
            (first (filter #(and (= :project-set-menu (:kind %)) (= :groups (:set %))) hits))

            session-hit
            (first (filter #(= :project-session-add (:kind %)) hits))]

        (expect (some? group-hit))
        (expect (some? group-menu))
        (expect (some? session-hit))
        (doseq [[hit expected]
                [[group-hit [:menu (assoc group-entry :initial-action :new)]]
                 [group-menu [:menu group-entry]]
                 [session-hit [:menu (assoc session-entry :initial-action :new-session)]]]

                :when hit]

          (let [{:keys [col row]}
                (:bounds hit)

                click
                (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. (int col) (int row)))]

            (expect (= expected (projects/key-action db click)))))
        (expect (some #(and (= :project-set-menu (:kind %)) (= :sessions (:set %))) hits)))
      (doseq [[entry choice] [[group-entry :new] [session-entry :new-session]]]
        (expect (= [:menu (assoc entry :initial-action choice)]
                   (projects/key-action (assoc-in db [:project-sidebar :index] (:index entry))
                                        (cap/key-stroke \+)))))
      (expect (= [:menu group-entry]
                 (projects/key-action (assoc-in db [:project-sidebar :index] (:index group-entry))
                                      (cap/key-stroke \g)))))))

(defdescribe
  sidebar-overflow-buttons-align-and-use-portable-marker-test
  (it "sidebar overflow buttons align and use portable marker"
      (doseq [cols [32 120]]
        (let [db (-> (fixture-db)
                     (assoc-in [:project-sidebar :expanded] #{"a"})
                     (assoc-in [:project-sidebar :pages "a"]
                               {:sessions [{"id" "saved" "title" "Saved session"}] :grouped []}))
              capture (cap/capture! {:cols cols
                                     :rows 24
                                     :paint!
                                     (fn [{:keys [screen]}]
                                       (projects/paint! (.newTextGraphics screen) db cols 24))})
              lines (str/split-lines (cap/frame-text capture))
              buttons (filter #(contains? #{:project-set-menu :project-details} (:kind %))
                              (.current projects/hit-map))]

          (expect (nil? (:error capture)))
          (expect (= 3 (count buttons)))
          (expect (= 1 (count (distinct (map #(get-in % [:bounds :col]) buttons))))
                  "Group, Sessions, and saved-session menus share a right-aligned column")
          (doseq [{:keys [bounds]} buttons
                  :let [{:keys [col row width]} bounds]]

            (expect (= 3 width))
            (expect (= " ⋮ " (subs (nth lines row) col (+ col width)))))))))

(defdescribe
  set-buttons-use-web-bands-and-direct-actions-test
  (it
    "set buttons use web bands and direct actions"
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

      (expect (= (palette theme/text-fg 0.04) (bg project-row)))
      (expect (= (palette theme/header-active-tab-bg 0.08) (bg groups-row)))
      (expect (= (palette theme/text-fg 0.06) (bg sessions-row)))
      (with-redefs [state/app-db
                    (atom (assoc-in db [:project-sidebar :focused?] true))

                    screen/create-group!
                    (fn [_ pid]
                      (swap! calls conj [:group pid]))

                    dlg/select-dialog!
                    (fn [& _]
                      (throw (ex-info "Unexpected menu" {})))]

        (#'screen/sidebar-row-menu!
         nil
         (assoc (first (filter #(= :groups (:set %)) entries)) :initial-action :new)
         nil)
        (expect (true? (get-in @state/app-db [:project-sidebar :focused?]))
                "Naming a new group keeps the rail focused")
        (#'screen/sidebar-row-menu!
         nil
         (assoc (first (filter #(= :sessions (:set %)) entries)) :initial-action :new-session)
         (fn [gid root]
           (swap! calls conj [:session gid root])))
        (expect (false? (get-in @state/app-db [:project-sidebar :focused?]))
                "A new session hands the keys to the chat, so typing never runs rail commands"))
      (expect (= [[:group "a"] [:session nil "/work/vis"]] @calls)))))

(defdescribe
  project-sections-indent-rows-and-metadata-test
  (it
    "project sections indent rows and metadata"
    (let [db
          (-> (fixture-db)
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :focused?] false)
              (assoc-in [:project-sidebar :groups "a"] [group-release])
              (assoc-in
                [:project-sidebar :pages "a"]
                {:sessions [{"id" "loose-id" "title" "Loose session" "turn_count" 2}]
                 :grouped
                 [{"id" "group-id" "title" "Grouped session" "group_id" "g1" "turn_count" 5}]}))

          capture
          (cap/capture! {:cols 168
                         :rows 36
                         :paint! (fn [{:keys [screen]}]
                                   (projects/paint! (.newTextGraphics screen) db 168 36))})

          hits
          (.current projects/hit-map)

          lines
          (str/split-lines (cap/frame-text capture))

          row-of
          (fn [match]
            (get-in (first (filter match hits)) [:bounds :row]))

          project-row
          (row-of #(= :project-select (:kind %)))

          groups-row
          (row-of #(and (= :project-set (:kind %)) (= :groups (:set %))))

          group-row
          (row-of #(= :project-group (:kind %)))

          grouped-row
          (row-of #(= "group-id" (get-in % [:session "id"])))

          sessions-row
          (row-of #(and (= :project-set (:kind %)) (= :sessions (:set %))))

          loose-row
          (row-of #(= "loose-id" (get-in % [:session "id"])))

          companion-row
          (row-of #(= "b" (get-in % [:project "id"])))

          column
          (fn [row text]
            (str/index-of (nth lines row) text))]

      (expect (nil? (:error capture)))
      (expect (= (inc (column project-row "Vis")) (column groups-row "Groups")))
      (expect (= (inc (column groups-row "Groups")) (column group-row "Release apps")))
      (expect (= (inc (column group-row "Release apps")) (column grouped-row "Grouped session")))
      (expect (= (inc (column project-row "Vis")) (column sessions-row "Sessions")))
      (expect (= (inc (column sessions-row "Sessions")) (column loose-row "Loose session")))
      (expect (= (column grouped-row "Grouped session") (column (inc grouped-row) "5 turns")))
      (expect (= (column loose-row "Loose session") (column (inc loose-row) "2 turns")))
      (expect (not-any? #(str/includes? (nth lines (inc %)) "-id") [grouped-row loose-row]))
      (expect (str/includes? (nth lines group-row) "▾ Release apps"))
      (doseq [[row label] [[1 "Projects"] [project-row "Vis"] [companion-row "Companion"]
                           [groups-row "Groups"] [sessions-row "Sessions"]]]
        (expect (true? (get-in capture [:frames 0 row (column row label) :bold])))))))

(defdescribe
  project-sidebar-section-headings-are-single-line-test
  (it
    "project sidebar section headings are single line"
    (let [db
          (-> (fixture-db)
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :groups "a"] [group-release])
              (assoc-in [:project-sidebar :pages "a"]
                        {:sessions [{"id" "loose" "title" "Loose session"}]
                         :grouped [{"id" "filed" "title" "Filed session" "group_id" "g1"}]}))

          cols
          180

          rows
          36

          capture
          (cap/capture! {:cols cols
                         :rows rows
                         :paint! (fn [{:keys [screen]}]
                                   (projects/paint! (.newTextGraphics screen) db cols rows))})

          hits
          (.current projects/hit-map)

          lines
          (str/split-lines (cap/frame-text capture))

          project
          (first (filter #(and (= :project-select (:kind %)) (= "a" (get-in % [:project "id"])))
                         hits))

          groups
          (first (filter #(and (= :project-set (:kind %)) (= :groups (:set %))) hits))

          sessions
          (first (filter #(and (= :project-set (:kind %)) (= :sessions (:set %))) hits))]

      (expect (nil? (:error capture)))
      (doseq [hit [project groups sessions]]
        (expect (= 1 (get-in hit [:bounds :height]))))
      (expect (str/includes? (nth lines (get-in project [:bounds :row])) "Vis"))
      (expect (str/includes? (nth lines (get-in project [:bounds :row])) "2 sessions"))
      (expect (str/includes? (nth lines (get-in groups [:bounds :row])) "▾ Groups"))
      (expect (str/includes? (nth lines (get-in sessions [:bounds :row])) "▾ Sessions"))
      (expect (= [:toggle-groups "a"] (:action groups)))
      (expect (= [:toggle-sessions "a"] (:action sessions)))
      (doseq [hit [groups sessions]]
        (expect (= (:action hit)
                   (projects/key-action db
                                        (MouseAction.
                                          MouseActionType/CLICK_DOWN
                                          1
                                          (TerminalPosition. 6 (get-in hit [:bounds :row])))))))
      (let [folded
            (assoc-in db [:project-sidebar :groups-folded? "a"] true)

            entries
            (projects/sidebar-entries folded)]

        (expect (not-any? #(= :project-group (:kind %)) entries))
        (expect (some #(and (= :project-set (:kind %)) (= :groups (:set %))) entries))
        (expect (some #(= "loose" (get-in % [:session "id"])) entries))))))

(defdescribe
  project-sidebar-section-toggle-test
  (it
    "project sidebar section toggle"
    (let [db
          (-> (fixture-db)
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :groups "a"] [group-release])
              (assoc-in [:project-sidebar :pages "a"]
                        {:sessions [{"id" "loose" "title" "Loose session"}]
                         :grouped [{"id" "filed" "title" "Filed session" "group_id" "g1"}]}))

          paint!
          (fn []
            (cap/capture! {:cols 180
                           :rows 36
                           :paint!
                           (fn [{:keys [screen]}]
                             (projects/paint! (.newTextGraphics screen) @state/app-db 180 36))}))

          handle!
          (fn [key]
            (#'screen/project-sidebar-key!
             key
             (constantly nil)
             (constantly nil)
             (constantly nil)
             (constantly nil)
             (constantly nil)))]

      (with-redefs [state/app-db (atom db)]
        (paint!)
        (let [groups (first (filter #(and (= :project-set (:kind %)) (= :groups (:set %)))
                                    (.current projects/hit-map)))]
          (state/dispatch [:project-sidebar {:index (:index groups)}])
          (handle! (cap/key-stroke :enter))
          (expect (true? (get-in @state/app-db [:project-sidebar :groups-folded? "a"])))
          (expect (not-any? #(= :project-group (:kind %)) (projects/sidebar-entries @state/app-db)))
          (expect (some #(= "loose" (get-in % [:session "id"]))
                        (projects/sidebar-entries @state/app-db)))
          (let [capture (paint!)]
            (expect (str/includes? (nth (str/split-lines (cap/frame-text capture))
                                        (get-in groups [:bounds :row]))
                                   "▸ Groups")))
          (handle! (MouseAction. MouseActionType/CLICK_DOWN
                                 1
                                 (TerminalPosition. 6 (get-in groups [:bounds :row]))))
          (expect (false? (get-in @state/app-db [:project-sidebar :groups-folded? "a"]))))
        (paint!)
        (let [sessions (first (filter #(and (= :project-set (:kind %)) (= :sessions (:set %)))
                                      (.current projects/hit-map)))]
          (handle! (MouseAction. MouseActionType/CLICK_DOWN
                                 1
                                 (TerminalPosition. 6 (get-in sessions [:bounds :row]))))
          (expect (true? (get-in @state/app-db [:project-sidebar :sessions-folded? "a"])))
          (expect (some #(= :project-group (:kind %)) (projects/sidebar-entries @state/app-db)))
          (expect (not-any? #(= "loose" (get-in % [:session "id"]))
                            (projects/sidebar-entries @state/app-db)))
          (let [capture (paint!)]
            (expect (str/includes? (nth (str/split-lines (cap/frame-text capture))
                                        (get-in sessions [:bounds :row]))
                                   "▸ Sessions"))))))))

(defdescribe folded-group-uses-project-disclosure-icon-test
             (it "folded group uses project disclosure icon"
                 (let [db
                       (-> (fixture-db)
                           (assoc-in [:project-sidebar :expanded] #{"a"})
                           (assoc-in [:project-sidebar :groups "a"] [group-release])
                           (assoc-in [:project-sidebar :group-folds "a"] #{"g1"})
                           (assoc-in [:project-sidebar :pages "a"] {:sessions [] :grouped []}))

                       capture
                       (cap/capture! {:cols 120
                                      :rows 24
                                      :paint!
                                      (fn [{:keys [screen]}]
                                        (projects/paint! (.newTextGraphics screen) db 120 24))})

                       row
                       (->> (.current projects/hit-map)
                            (filter #(= :project-group (:kind %)))
                            first
                            :bounds
                            :row)]

                   (expect (nil? (:error capture)))
                   (expect (str/includes? (nth (str/split-lines (cap/frame-text capture)) row)
                                          "▸ Release apps")))))

(defdescribe
  project-rail-focus-highlights-instead-of-leading-dot-test
  (it
    "project rail focus highlights instead of leading dot"
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
              capture (cap/capture!
                        {:cols 120
                         :rows 24
                         :paint! (fn [{:keys [screen]}]
                                   (projects/paint! (.newTextGraphics screen) selected 120 24))})
              hit (first (filter #(= index (:index %)) (.current projects/hit-map)))
              row (get-in hit [:bounds :row])
              height (get-in hit [:bounds :height])
              lines (str/split-lines (cap/frame-text capture))]

          (expect (nil? (:error capture)))
          (expect (not (str/includes? (nth lines row) "•")))
          (expect (every? #(= highlight (get-in capture [:frames 0 % 1 :bg]))
                          (range row (+ row height))))
          (when (= :project-group (:kind hit))
            (expect (= (#'theme-test/rgb-tuple theme/dialog-fg)
                       (get-in capture [:frames 0 row 6 :fg])))
            (expect (= (#'theme-test/rgb-tuple (theme/group-ink "violet"))
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

        (expect (str/includes? (nth lines 4) "Vis"))
        (expect (not (str/includes? (nth lines 4) "● Vis")))
        (expect (not (str/includes? (nth lines 4) "▸ Vis")))
        (expect (= (palette theme/header-active-tab-bg 0.10) (get-in capture [:frames 0 4 1 :bg])))
        (expect (= (palette theme/text-fg 0.04) (get-in capture [:frames 0 5 1 :bg])))))))

(defdescribe
  saved-session-grid-separates-title-and-status-test
  (it
    "saved session grid separates title and status"
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
          (get-in capture [:frames 0 live-row (- (:width (projects/geometry db 168 24)) 7)])]

      (expect (nil? (:error capture)))
      (expect (= 3 (get-in live [:bounds :height])))
      (expect (= (+ live-row 3) idle-row))
      (expect (str/includes? (nth lines live-row) "Writing the tests"))
      (expect (str/includes? (nth lines live-row) "* Live"))
      (expect (str/includes? (nth lines idle-row) "Idle"))
      (expect (not (str/includes? (nth lines (inc live-row)) "live-sess")))
      (expect (str/includes? (nth lines (inc live-row)) "12 turns"))
      (expect (str/includes? (nth lines (inc idle-row)) "7 turns"))
      (expect (true? (:bold status-cell)))
      (expect (= (#'theme-test/rgb-tuple
                  (theme/legible-ink theme/status-ok (tuple-rgb (:bg status-cell))))
                 (:fg status-cell)))
      (expect (= [:session "live-session"]
                 (projects/key-action db
                                      (MouseAction. MouseActionType/CLICK_DOWN
                                                    1
                                                    (TerminalPosition. 18
                                                                       (int (inc live-row))))))))))

(defdescribe
  saved-session-grid-narrow-status-and-metadata-test
  (it
    "saved session grid narrow status and metadata"
    (doseq [cols [32 100]]
      (let [db (-> (fixture-db)
                   (assoc-in [:project-sidebar :expanded] #{"a"})
                   (assoc-in [:project-sidebar :pages "a"]
                             {:sessions [{"id" "live-session"
                                          "title" "Writing the tests"
                                          "live" true
                                          "favorite_rank" 1
                                          "turn_count" 12}]
                              :grouped []}))
            capture (cap/capture! {:cols cols
                                   :rows 24
                                   :paint!
                                   (fn [{:keys [screen]}]
                                     (projects/paint! (.newTextGraphics screen) db cols 24))})
            live (first (filter #(= :project-session (:kind %)) (.current projects/hit-map)))
            row (get-in live [:bounds :row])
            lines (str/split-lines (cap/frame-text capture))
            status-cell (get-in capture [:frames 0 (inc row) 8])]

        (expect (nil? (:error capture)))
        (expect (str/includes? (nth lines row)
                               (if (= cols 32) "Writing the te" "Writing the tests")))
        (expect (not (str/includes? (nth lines row) "Live")))
        (expect (str/includes? (nth lines (inc row)) "* Live"))
        (expect (= (str/index-of (nth lines row) "Writing")
                   (str/index-of (nth lines (inc row)) "* Live")))
        (expect (str/includes? (nth lines (inc row)) "* Live · 12 turns"))
        (expect (true? (:bold status-cell)))
        (expect (= (#'theme-test/rgb-tuple
                    (theme/legible-ink theme/status-ok (tuple-rgb (:bg status-cell))))
                   (:fg status-cell)))))))

(defdescribe session-time-label-test
             (it "words recent changes relative and older ones as a date"
                 (let [zone
                       (java.time.ZoneId/of "UTC")

                       now
                       (.toEpochMilli (java.time.Instant/parse "2026-10-02T12:00:00Z"))

                       label
                       #(projects/session-time-label (java.time.Instant/parse %) now zone)]

                   (expect (= "now" (label "2026-10-02T11:59:30Z")))
                   (expect (= "5 min ago" (label "2026-10-02T11:55:00Z")))
                   (expect (= "1 hour ago" (label "2026-10-02T10:30:00Z")))
                   (expect (= "23 hours ago" (label "2026-10-01T12:30:00Z")))
                   (expect (= "Sep 28 09:15" (label "2026-09-28T09:15:00Z")))
                   (expect (= "Dec 31, 2025" (label "2025-12-31T18:00:00Z")))
                   (expect (nil? (projects/session-time-label nil now zone))))))

(defdescribe
  saved-session-rows-fit-every-width-test
  (it
    "shows the date instead of the ID and keeps title, status and details apart"
    (let [before
          @theme/active-theme-id

          now
          (System/currentTimeMillis)

          hitl-at
          (java.time.Instant/parse "2024-06-15T12:00:00Z")

          stopped-at
          (java.time.Instant/parse "2025-03-14T12:00:00Z")

          long-title
          (str/join " " (repeat 6 "Rework the sidebar layout"))

          sessions
          [{"id" "session-0001"
            "title" long-title
            "live" true
            "favorite_rank" 1
            "turn_count" 12
            "modified_at" (java.time.Instant/ofEpochMilli (- now (* 3 3600000)))}
           {"id" "session-0002"
            "title" long-title
            "is_awaiting_input" true
            "awaiting_input_count" 12
            "turn_count" 3
            "modified_at" hitl-at}
           {"id" "session-0003"
            "title" long-title
            "was_failed" true
            "is_unread" true
            "unread_answers" 1
            "turn_count" 1
            "modified_at" stopped-at}
           {"id" "session-0004"
            "title" "Fresh answers"
            "is_unread" true
            "unread_answers" 3
            "turn_count" 9} {"id" "session-0005" "title" "Paused" "status" "suspended"}
           {"id" "session-0006"
            "title" "Old work"
            "archived_at" "2025-01-01T00:00:00Z"
            "turn_count" 2}
           {"id" "session-0007"
            "title" "Finished review"
            "turn_count" 7
            "modified_at" (java.time.Instant/ofEpochMilli (- now 60000))}]

          labels
          ["* Live" "HITL ×12" "Stopped" "New ×3" "Waiting" "Archived" "Idle"]

          narrow-details
          ["* Live · 3 hours ago · 12 turns"
           (str "HITL ×12 · " (projects/session-time-label hitl-at))
           (str "Stopped · " (projects/session-time-label stopped-at) " · 1 turn")
           "New ×3 · 9 turns" "Waiting" "Archived · 2 turns" "Idle · 1 min ago · 7 turns"]

          wide-details
          ["3 hours ago · 12 turns" (str (projects/session-time-label hitl-at) " · 3 turns")
           (str (projects/session-time-label stopped-at) " · 1 turn") "9 turns" "" "2 turns"
           "1 min ago · 7 turns"]

          index
          (zipmap (map #(get % "id") sessions) (range))

          db
          (-> (fixture-db)
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :pages "a"] {:sessions sessions :grouped []}))]

      (try
        (doseq [id
                (shared-theme/available-theme-ids)

                cols
                [100 120 168]]

          (theme/apply-theme! (keyword id))
          (let [capture
                (cap/capture! {:cols cols
                               :rows 40
                               :paint! (fn [{:keys [screen]}]
                                         (projects/paint! (.newTextGraphics screen) db cols 40))})

                width
                (long (:width (projects/geometry db cols 40)))

                wide?
                (>= width 48)

                hits
                (filterv #(= :project-session (:kind %)) (.current projects/hit-map))]

            (expect (nil? (:error capture)))
            (expect (= (count sessions) (count hits)))
            (doseq [hit
                    hits

                    :let [i
                          (index (get-in hit [:session "id"]))

                          label
                          (nth labels i)

                          row
                          (long (get-in hit [:bounds :row]))

                          n
                          (count label)

                          status-row
                          (if wide? row (inc row))

                          status-col
                          (if wide? (- width 6 n) 6)]]

              (expect (not (str/includes? (str (cell-text capture row 0 width)
                                               (cell-text capture (inc row) 0 width))
                                          "session-")))
              (expect (= label (cell-text capture status-row status-col (+ status-col n))))
              (expect (= (nth (if wide? wide-details narrow-details) i)
                         (str/trim (cell-text capture (inc row) 6 (dec width)))))
              (expect (str/blank? (if wide?
                                    (cell-text capture row (- status-col 2) status-col)
                                    (cell-text capture row (- width 9) (- width 5)))))
              (expect (str/blank? (cell-text capture (inc row) (- width 2) (dec width))))
              (doseq [col (range status-col (+ status-col n))]
                (expect (true? (get-in capture [:frames 0 status-row col :bold]))))
              (doseq [[r cols*]
                      [[row (range 1 (- width 5))] [(inc row) (range 1 (dec width))]]

                      col
                      cols*

                      :let [cell
                            (get-in capture [:frames 0 r col])]
                      :when (not (str/blank? (str (:ch cell))))]

                (expect (<= theme/legible-contrast (cell-contrast cell)))))))
        (finally (theme/apply-theme! before))))))

(defdescribe
  saved-session-grid-scroll-keeps-focused-card-and-parent-test
  (it "saved session grid scroll keeps focused card and parent"
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

        (expect (= :project-select (:kind (first visible))))
        (expect (= "s29" (get-in (last visible) [:session "id"])))
        (expect (<= (reduce + (map #'projects/row-height visible)) 9))
        (expect (every? #(<= (+ (get-in % [:bounds :row]) (get-in % [:bounds :height] 1)) 13)
                        (filter #(= :project-session (:kind %)) hits)))
        (expect (nil? (:error capture))))))

(defdescribe project-sidebar-scroll-does-not-orphan-a-card-test
             (it "project sidebar scroll does not orphan a card"
                 (let [db
                       (-> (fixture-db)
                           (assoc-in [:project-sidebar :expanded] #{"a"})
                           (assoc-in [:project-sidebar :pages "a"]
                                     {:sessions [{"id" "s1" "title" "Last saved session"}]
                                      :grouped []}))

                       db
                       (assoc-in db [:project-sidebar :index] (count (projects/sidebar-entries db)))

                       visible
                       (projects/visible-entries db 11)]

                   (expect (= [:project-select] (mapv :kind visible)))
                   (expect (= ["b"] (mapv #(get-in % [:project "id"]) visible))))))

(defdescribe
  saved-session-archive-set-menu-has-keyboard-and-pointer-test
  (it
    "saved session archive set menu has keyboard and pointer"
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

      (expect (= [:menu entry] (projects/key-action selected (cap/key-stroke \g))))
      (cap/capture! {:cols 120
                     :rows 24
                     :paint! (fn [{:keys [screen]}]
                               (projects/paint! (.newTextGraphics screen) selected 120 24))})
      (let [hit
            (first (filter #(and (= :project-set (:kind %)) (= "Sessions" (:label %)))
                           (.current projects/hit-map)))

            {:keys [col row]}
            (:bounds hit)]

        (expect (= :menu
                   (first (projects/key-action selected
                                               (MouseAction. MouseActionType/CLICK_DOWN
                                                             3
                                                             (TerminalPosition. (int col)
                                                                                (int row))))))))
      (with-redefs-fn {#'state/app-db (atom selected)
                       #'screen/with-dialog-lock (fn [f]
                                                   (f))
                       #'screen/load-project-page! #(swap! loaded conj %)
                       #'dlg/select-dialog! (fn [_ _ items]
                                              (reset! choice items)
                                              {:id :toggle-session-archive})}
        (fn []
          (#'screen/sidebar-row-menu! nil entry nil)
          (expect (= "Show archived sessions" (:label (first @choice))))
          (expect (true? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
          (expect (= ["a"] @loaded))
          (#'screen/sidebar-row-menu! nil (assoc entry :archived? true) nil)
          (expect (= "Hide archived sessions" (:label (first @choice))))
          (expect (false? (get-in @state/app-db [:project-sidebar :session-archived? "a"]))))))))

(defdescribe
  saved-session-deleting-the-active-row-reconciles-its-id-test
  (it
    "saved session deleting the active row reconciles its id"
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
        (expect (= "a1" (get-in @state/app-db [:session :id]))
                "A failed DELETE keeps the active view")
        (expect (= #{"a1"} (get-in @state/app-db [:project-sidebar :selected "a"])))
        (reset! fail? false)
        (#'screen/sidebar-row-menu! nil entry nil)
        (expect (= ["a1" "a1"] @attempts))
        (expect (= "a2" (get-in @state/app-db [:session :id])))
        (expect (= [:tab-2 :tab-3] (mapv :id (:tabs @state/app-db))))
        (expect (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))))))

(defdescribe
  saved-session-selection-has-keyboard-and-menu-test
  (it
    "saved session selection has keyboard and menu"
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

      (expect (= [:toggle-session "a" "s1"] (projects/key-action selected (cap/key-stroke \space))))
      (with-redefs [state/app-db (atom selected)]
        (#'screen/project-sidebar-key!
         (cap/key-stroke \space)
         identity
         identity
         identity
         identity
         nil)
        (expect (= #{"s1"} (get-in @state/app-db [:project-sidebar :selected "a"])))
        (let [unfocused (assoc-in @state/app-db [:project-sidebar :index] 1)
              capture (cap/capture!
                        {:cols 120
                         :rows 24
                         :paint! (fn [{:keys [screen]}]
                                   (projects/paint! (.newTextGraphics screen) unfocused 120 24))})
              hit (first (filter #(and (= :project-session (:kind %))
                                       (= "s1" (get-in % [:session "id"])))
                                 (.current projects/hit-map)))
              {:keys [col row]} (:bounds hit)
              highlight (#'theme-test/rgb-tuple
                         (theme/mix-color theme/terminal-bg theme/header-active-tab-bg 0.16))]

          (expect (nil? (:error capture)))
          (expect (str/includes? (cap/frame-text capture) "First"))
          (expect (not (re-find #"[☑◻□]" (nth (str/split-lines (cap/frame-text capture)) row))))
          (expect (= highlight (get-in capture [:frames 0 row 1 :bg])))
          (expect (not-any? #(= :project-selection (:kind %)) (.current projects/hit-map)))
          (expect (= [:menu hit]
                     (projects/key-action unfocused
                                          (MouseAction. MouseActionType/CLICK_DOWN
                                                        3
                                                        (TerminalPosition. (int col) (int row))))))
          (expect (= [:session "s1"]
                     (projects/key-action unfocused
                                          (MouseAction. MouseActionType/CLICK_DOWN
                                                        1
                                                        (TerminalPosition. (int (inc col))
                                                                           (int row)))))))
        (expect (= #{"s1"} (get-in @state/app-db [:project-sidebar :selected "a"])))
        (#'screen/project-sidebar-key!
         (cap/key-stroke \space)
         identity
         identity
         identity
         identity
         nil)
        (expect (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
        (state/dispatch [:project-session-select-toggle "a" "s1"])
        (state/dispatch [:project-session-archive-toggle "a"])
        (expect (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))))))

(defdescribe
  selected-session-move-keeps-failed-rows-and-reports-partial-error-test
  (it
    "selected session move keeps failed rows and reports partial error"
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
        (expect (= [["s1" "g1"] ["s2" "g1"] [:refresh]] @calls))
        (expect (= #{"s2"} (get-in @state/app-db [:project-sidebar :selected "a"])))
        (expect (= ["s1"]
                   (mapv #(get-in % [:session "id"])
                         (filter (fn [row]
                                   (= "g1" (get-in row [:session "group_id"])))
                                 (projects/sidebar-entries @state/app-db)))))
        (expect (= ["s2"]
                   (mapv #(get % "id")
                         (get-in @state/app-db [:project-sidebar :pages "a" :sessions]))))
        (expect (some #(str/includes? % "1 of 2") @notices))
        (reset! calls [])
        (reset! notices [])
        (with-redefs [dlg/select-dialog! (fn [_ _ _]
                                           {:id :ungroup-selected})]
          (#'screen/sidebar-row-menu! nil loose-entry nil))
        (expect (= [["s2" nil] [:refresh]] @calls))
        (expect (= #{"s2"} (get-in @state/app-db [:project-sidebar :selected "a"])))
        (expect (some #(str/includes? % "1 of 1") @notices))))))

(defdescribe
  single-and-selected-session-move-uses-project-groups-test
  (it
    "single and selected session move uses project groups"
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
                      (expect (= {:project-id "a"} opts))
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
                      (expect (some #(= :move-session (:id %)) items))
                      {:id :move-session})

                    dlg/searchable-select!
                    (fn [_ _ items _]
                      (reset! options items)
                      {:id @pick})]

        (#'screen/sidebar-row-menu! nil entry nil)
        (expect (= [["s1" "g2"] [:refresh]] @calls))
        (expect (= ["g1" "g2" ::screen/new-group ::screen/remove-group] (mapv :id @options)))
        (reset! calls [])
        (state/dispatch [:project-session-select-toggle "a" "s1"])
        (state/dispatch [:project-session-select-toggle "a" "s2"])
        (state/dispatch [:project-session-select-toggle "b" "other"])
        (reset! pick ::screen/remove-group)
        (#'screen/sidebar-row-menu! nil entry nil)
        (expect (= [["s1" nil] ["s2" nil] [:refresh]] @calls))
        (expect (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
        (expect (= #{"other"} (get-in @state/app-db [:project-sidebar :selected "b"])))))))

(defdescribe
  group-creation-edit-and-new-session-refresh-test
  (it
    "group creation edit and new session refresh"
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
                        (do (expect (some #(= :new-session (:id %)) items)) {:id @choice})))

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
        (expect (= [[:create {:name "New group" :project-id "a" :color "cyan"}] [:refresh]] @calls))
        (reset! choice :rename)
        (reset! typed "  Renamed  ")
        (#'screen/sidebar-row-menu! nil group-entry nil)
        (reset! choice :recolour)
        (#'screen/sidebar-row-menu! nil group-entry nil)
        (expect (= [[:update "g1" {:name "Renamed"}] [:refresh] [:update "g1" {:color "cyan"}]
                    [:refresh]]
                   (subvec @calls 2)))
        (reset! fail? true)
        (reset! notices [])
        (reset! choice :rename)
        (#'screen/sidebar-row-menu! nil group-entry nil)
        (expect (= [:update "g1" {:name "Renamed"}] (last @calls))
                "A failed rename does not refresh as though it succeeded")
        (expect (some #(str/includes? % "Could not rename group") @notices))
        (reset! choice :new-session)
        (#'screen/sidebar-row-menu! nil loose-entry #(swap! calls conj [:start %1 %2]))
        (#'screen/sidebar-row-menu! nil group-entry #(swap! calls conj [:start %1 %2]))
        (expect (= [[:start nil "/work/vis"] [:start "g1" "/work/vis"]]
                   (subvec @calls (- (count @calls) 2))))))))

(defdescribe
  saved-project-pagers-and-pointer-test
  (it
    "saved project pagers and pointer"
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

      (expect (nil? (:error paint)))
      (expect (= [:session "saved-1"]
                 (projects/key-action db
                                      (MouseAction. MouseActionType/CLICK_DOWN
                                                    1
                                                    (TerminalPosition. (int col) (int row))))))
      (let [detail
            (first (filter #(= :project-details (:kind %)) (.current projects/hit-map)))

            bounds
            (:bounds detail)

            selected
            (assoc-in db
              [:project-sidebar :index]
              (:index (first (filter #(= :project-session (:kind %)) entries))))]

        (expect (= [:details "saved-1"]
                   (projects/key-action db
                                        (MouseAction. MouseActionType/CLICK_DOWN
                                                      1
                                                      (TerminalPosition. (int (:col bounds))
                                                                         (int (:row bounds)))))))
        (expect (= [:details "saved-1"] (projects/key-action selected (cap/key-stroke \d))))
        (expect (= :menu
                   (first (projects/key-action db
                                               (MouseAction. MouseActionType/CLICK_DOWN
                                                             3
                                                             (TerminalPosition. (int col)
                                                                                (int row))))))))
      ;; Live groups are never paged, whatever total the gateway reports beside them.
      (expect (not-any? #(= :project-group-page (:kind %)) entries))
      (with-redefs [state/app-db (atom db)]
        (state/dispatch [:project-group-turn "a" :next])
        (expect (= 0 (get-in @state/app-db [:project-sidebar :pages "a" :group-offset])))
        (state/dispatch [:project-page-turn "a" :next])
        (expect (= "next" (get-in @state/app-db [:project-sidebar :pages "a" :after])))
        (state/dispatch [:project-page-request "a" "new"])
        (state/dispatch [:project-page-loaded "a" "stale" {:sessions [{"id" "stale"}]}
                         {:groups [] :total 0}])
        (expect (= "saved-1" (get-in @state/app-db [:project-sidebar :pages "a" :sessions 0 "id"])))
        (state/dispatch [:project-page-turn "a" :previous])
        (expect (nil? (get-in @state/app-db [:project-sidebar :pages "a" :after])))))))

(defdescribe
  group-archive-view-is-independent-of-loose-sessions-test
  (it
    "group archive view is independent of loose sessions"
    (let [requests
          (atom [])

          db
          (-> (fixture-db)
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :pages "a"]
                        {:sessions [{"id" "loose" "title" "Loose"}]
                         :grouped [{"id" "filed-active" "group_id" "g1"}]
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
        (expect (true? (get-in @state/app-db [:project-sidebar :group-archived? "a"])))
        (expect (nil? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
        (expect (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
        (expect (= 0 (get-in @state/app-db [:project-sidebar :pages "a" :group-offset])))
        (state/dispatch [:project-page-loaded "a" "old" {:sessions [{"id" "stale"}]} {:groups []}])
        (#'screen/load-project-page! "a")
        (let [entries (projects/sidebar-entries @state/app-db)]
          (expect (= ["Groups · Archived" "Sessions"]
                     (mapv :label (filter #(= :project-set (:kind %)) entries))))
          (expect (= ["Gateway"] (mapv :label (filter #(= :project-group (:kind %)) entries))))
          (expect (= "Archived" (:status (first (filter #(= :project-session (:kind %)) entries)))))
          (expect (= ["filed-archived" "loose"]
                     (mapv #(get-in % [:session "id"])
                           (filter #(= :project-session (:kind %)) entries)))))
        (expect (= :only (:archived (second (first @requests)))))
        (expect (= :exclude (:archived (second (second @requests)))))
        (expect (some #(= :only (:archived (second %)))
                      (filter #(= :sessions (first %)) @requests)))
        (state/dispatch [:project-session-archive-toggle "a"])
        (#'screen/load-project-page! "a")
        (expect (= ["filed-archived"]
                   (mapv #(get-in % [:session "id"])
                         (filter #(= :project-session (:kind %))
                                 (projects/sidebar-entries @state/app-db)))))
        (state/dispatch [:project-group-archive-toggle "a"])
        (#'screen/load-project-page! "a")
        (expect (= ["Release apps"]
                   (mapv :label
                         (filter #(= :project-group (:kind %))
                                 (projects/sidebar-entries @state/app-db)))))
        (state/dispatch [:project-group-archive-toggle "a"])
        (state/dispatch [:project-page-request "a" "empty"])
        (state/dispatch [:project-page-loaded "a" "empty"
                         {:sessions [] :grouped [] :current nil :loading? false}
                         {:groups [] :total 0}])
        (expect (some #(= "No archived groups" (:label %))
                      (projects/sidebar-entries @state/app-db)))))))

;; Regression, user report (paraphrased: "groups gather sessions, so every live group must
;; always show; only archived groups, which pile up, may be paged, the same in the app and
;; the TUI").
(defdescribe
  live-groups-are-never-paged-test
  (it
    "reads every live group whole and pages only the archive"
    (let [requests
          (atom [])

          wall
          (mapv #(hash-map "id" (str "g" %) "name" (str "Group " %) "session_count" 0) (range 40))

          windows
          (fn [kind]
            (->> @requests
                 (keep (fn [[asked opts]]
                         (case asked
                           :groups
                           (when (= :groups kind) [(:limit opts) (:offset opts)])

                           :sessions
                           (when (and (= :sessions kind) (= :aside (:grouped opts)))
                             [(:group-limit opts) (:group-offset opts)]))))
                 vec))

          entries
          (fn [kind]
            (filter #(= kind (:kind %)) (projects/sidebar-entries @state/app-db)))

          db
          (-> (fixture-db)
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :pages "a"] {:sessions [] :grouped [] :group-offset 0}))]

      (with-redefs [state/app-db
                    (atom db)

                    vis/worker-future
                    (fn [_ f]
                      (f))

                    vis/gateway-list-session-groups-page
                    (fn [{:keys [archived limit offset] :as opts}]
                      (swap! requests conj [:groups opts])
                      (let [from
                            (long (or offset 0))

                            to
                            (if limit (min (count wall) (+ from (long limit))) (count wall))

                            shown
                            (subvec wall from to)]

                        {:groups
                         (if (= :only archived) (mapv #(assoc % "archived_at" "today") shown) shown)
                         :total (count wall)}))

                    vis/gateway-list-sessions-page
                    (fn [opts]
                      (swap! requests conj [:sessions opts])
                      {:sessions [] :grouped []})]

        (#'screen/load-project-page! "a")
        (expect (= 40 (count (entries :project-group))))
        (expect (empty? (entries :project-group-page)))
        (expect (= [[nil nil]] (windows :groups)))
        (expect (= [[nil nil]] (windows :sessions)))
        (state/dispatch [:project-group-turn "a" :next])
        (expect (= 0 (get-in @state/app-db [:project-sidebar :pages "a" :group-offset])))
        (reset! requests [])
        (state/dispatch [:project-group-archive-toggle "a"])
        (#'screen/load-project-page! "a")
        (expect (= state/archived-groups-page-size (count (entries :project-group))))
        (expect (= [[:group-page "a" :next]] (mapv :action (entries :project-group-page))))
        (expect (= [[15 0]] (windows :groups)))
        (expect (= [[15 0] [15 0]] (windows :sessions)))
        (reset! requests [])
        (state/dispatch [:project-group-turn "a" :next])
        (#'screen/load-project-page! "a")
        (expect (= [[15 15]] (windows :groups)))
        (expect (= [[15 15] [15 15]] (windows :sessions)))
        (expect (= [[:group-page "a" :previous] [:group-page "a" :next]]
                   (mapv :action (entries :project-group-page))))
        (state/dispatch [:project-group-turn "a" :next])
        (#'screen/load-project-page! "a")
        (expect (= 10 (count (entries :project-group))))
        (expect (= [[:group-page "a" :previous]] (mapv :action (entries :project-group-page))))))))

(defdescribe
  group-archive-and-delete-actions-keep-failures-visible-test
  (it
    "group archive and delete actions keep failures visible"
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
                              (expect (not-any? #(= :new-session (:id %)) items)))
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
        (expect (= [[:archive "g1" {:archived true}] [:refresh]] @calls))
        (reset! fail? true)
        (reset! choice :unarchive-group)
        (#'screen/sidebar-row-menu!
         nil
         (assoc group-entry :group (assoc group-release "archived_at" "today"))
         nil)
        (expect (= [:archive "g1" {:archived false}] (last @calls)))
        (expect (some #(str/includes? % "Could not unarchive group") @notices))
        (reset! fail? false)
        (reset! choice :delete)
        (#'screen/sidebar-row-menu! nil group-entry nil)
        (expect (= 3 (count @calls)) "Cancel at the choice does not touch the gateway")
        (reset! mode :detach)
        (#'screen/sidebar-row-menu! nil group-entry nil)
        (expect (= [[:delete "g1" :detach] [:refresh]] (take-last 2 @calls)))
        (expect (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
        (expect (some #(= :tab-2 (:id %)) (:tabs @state/app-db)) "Detach keeps the open session")
        (reset! mode :with-sessions)
        (#'screen/sidebar-row-menu! nil group-entry nil)
        (expect (= [[:delete "g1" :with-sessions] [:refresh]] (take-last 2 @calls)))
        (expect (not-any? #(= :tab-2 (:id %)) (:tabs @state/app-db))
                "Delete prunes only its returned IDs")
        (reset! fail? true)
        (#'screen/sidebar-row-menu! nil group-entry nil)
        (expect (= [:delete "g1" :with-sessions] (last @calls)))
        (expect (some #(str/includes? % "Could not delete group") @notices))))))

(defdescribe
  groups-set-menu-reveals-its-own-archive-test
  (it "groups set menu reveals its own archive"
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
                        (expect (some #(= :toggle-group-archive (:id %)) items))
                        {:id :toggle-group-archive})]

          (with-redefs-fn {#'screen/load-project-page! #(swap! reads conj %)}
            (fn []
              (#'screen/sidebar-row-menu!
               nil
               (first (filter #(= :groups (:set %)) (projects/sidebar-entries @state/app-db)))
               nil)
              (expect (= ["a"] @reads))
              (expect (true? (get-in @state/app-db [:project-sidebar :group-archived? "a"])))
              (expect (nil? (get-in @state/app-db [:project-sidebar :session-archived? "a"])))
              (expect (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))))))))

(defdescribe
  project-inventory-refresh-keeps-its-row-test
  (it
    "project inventory refresh keeps its row"
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
        (expect (= "a1"
                   (get-in (nth (projects/sidebar-entries @state/app-db)
                                (dec (get-in @state/app-db [:project-sidebar :index])))
                           [:session "id"])))
        (expect (true? (get-in @state/app-db [:project-sidebar :focused?])))))))

(defdescribe
  project-chooser-creates-a-gateway-folder-test
  (it
    "project chooser creates a gateway folder"
    (let [field
          (assoc (projects/add-field-listing {:text "/work/" :cursor 6}
                                             "/work/"
                                             (get browse-listing "entries"))
            :listing-path "/work")

          calls
          (atom [])

          db
          (assoc-in (fixture-db) [:project-sidebar :adding] field)]

      (expect (= [:add-folder] (projects/key-action db (KeyStroke. \n true false))))
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
        (expect (= [[:mkdir "/work" "new"] [:add "/work/new"]] @calls))))))

(defdescribe
  project-chooser-folder-cancel-and-failure-test
  (it
    "project chooser folder cancel and failure"
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
        (expect (empty? @calls))
        (expect (not (get-in @state/app-db [:project-sidebar :saving?]))))
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
        (expect (empty? @calls))
        (expect (not (get-in @state/app-db [:project-sidebar :saving?])))
        (expect (re-find #"No permission" (get-in @state/app-db [:project-sidebar :error])))))))

(defdescribe
  project-chooser-opens-existing-or-saved-session-test
  (it
    "project chooser opens existing or saved session"
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
        (expect (= 0 @pages) "An open view switches without a gateway read")
        (expect (= "b" (:active-project-id @state/app-db)))
        (expect (empty? @opened))
        (swap! state/app-db update
          :tabs
          #(filterv (fn [tab]
                      (= "a" (:project-id tab)))
             %))
        (#'screen/choose-project!
         project-b
         #(swap! opened conj [:open %])
         #(swap! opened conj [:new %]))
        (expect (= [[:open "saved"]] @opened))
        (expect (= 1 @pages))))))

(defdescribe
  project-removal-is-confirmed-and-failure-preserves-sessions-test
  (it
    "project removal is confirmed and failure preserves sessions"
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
                      (expect (= "Remove project" title))
                      (expect (str/includes? (str message) "cannot be undone"))
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
        (expect (empty? @calls) "Cancel sends no destructive request")
        (reset! answer true)
        (reset! failure true)
        (#'screen/remove-project! nil project-a)
        (expect (= [[:delete "a" {:is-recursive? true}]] @calls))
        (expect (= "a2" (get-in @state/app-db [:project-sidebar :pages "a" :sessions 0 "id"])))
        (expect (some #(= :tab-2 (:id %)) (:tabs @state/app-db)))
        (expect (str/includes? (get-in @state/app-db [:project-sidebar :error]) "Remove failed"))
        (reset! failure false)
        (#'screen/remove-project! nil project-a)
        (expect (= [:refresh] (last @calls)))
        (expect (not-any? #(= :tab-2 (:id %)) (:tabs @state/app-db)))
        (expect (= "b" (:active-project-id @state/app-db))
                "Removing the active project focuses a surviving one")
        (expect (not-any? #(= "a" (:project-id %)) (:tabs @state/app-db)))
        (expect (empty? (get-in @state/app-db [:project-sidebar :selected "a"])))
        (expect (nil? (get-in @state/app-db [:project-sidebar :removing])))))))

(defdescribe
  project-menu-chooses-or-removes-only-the-project-row-test
  (it
    "project menu chooses or removes only the project row"
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
                      (expect (= #{:use-project :delete-project}
                                 (set (map :id
                                           (filter #(#{:use-project :delete-project} (:id %))
                                                   items)))))
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
        (expect (= [[:choose "a"] [:remove "b"]] @calls))))))

(defdescribe empty-project-chooses-its-own-root-test
             (it "empty project chooses its own root"
                 (let [started (atom nil)]
                   (with-redefs [state/app-db (atom (update (fixture-db)
                                                            :tabs
                                                            #(filterv (fn [tab]
                                                                        (= "a" (:project-id tab)))
                                                               %)))
                                 vis/worker-future (fn [_ f]
                                                     (f))
                                 vis/gateway-list-sessions-page (constantly {:sessions []
                                                                             :grouped []})]

                     (#'screen/choose-project!
                      project-b
                      (fn [_]
                        (throw (ex-info "No saved row" {})))
                      (fn [root build-id]
                        (reset! started [root build-id])))
                     (expect (= "/work/companion" (first @started)))
                     (expect (= "b" (:active-project-id @state/app-db)))
                     (expect (= (second @started) (:build-id (last (:tabs @state/app-db)))))))))

(defdescribe project-removal-shows-progress-before-request-test
             (it "project removal shows progress before request"
                 (let [pending (atom nil)]
                   (with-redefs [state/app-db (atom (fixture-db))
                                 screen/with-dialog-lock (fn [f]
                                                           (f))
                                 dlg/confirm-dialog! (fn [& _]
                                                       true)
                                 vis/worker-future (fn [_ f]
                                                     (reset! pending f))]

                     (#'screen/remove-project! nil project-a)
                     (expect (= "a" (get-in @state/app-db [:project-sidebar :removing])))
                     (expect (fn? @pending))
                     (let [capture
                           (cap/capture!
                             {:cols 100
                              :rows 18
                              :paint!
                              (fn [{:keys [screen]}]
                                (projects/paint! (.newTextGraphics screen) @state/app-db 100 18))})]
                       (expect (str/includes? (cap/frame-text capture) "Removing…")))))))

(defdescribe
  project-page-refresh-keeps-focus-on-the-same-project-test
  (it
    "project page refresh keeps focus on the same project"
    (let [base
          (-> (fixture-db)
              (assoc :active-project-id "b"
                     :session {:id "b1"})
              (assoc-in [:project-sidebar :pages]
                        {"a" {:sessions [{"id" "old"}]} "b" {:sessions []}})
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
        (expect (= "b"
                   (get-in (nth (projects/sidebar-entries @state/app-db)
                                (dec (get-in @state/app-db [:project-sidebar :index])))
                           [:project "id"])))))))

(defdescribe project-search-finds-unloaded-and-archived-results-test
             (it "project search finds unloaded and archived results"
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
                   (expect (= [:search]
                              (projects/key-action (fixture-db) (KeyStroke. \/ false false))))
                   (expect (= ["b9"]
                              (->> (projects/sidebar-entries db)
                                   (keep #(get-in % [:session "id"]))
                                   vec)))
                   (expect (= "b"
                              (get-in (first (filter :session (projects/sidebar-entries db)))
                                      [:project "id"]))))))

(defdescribe project-automatic-refresh-shows-new-rows-test
             (it "shows arriving rows and adopts the current page cursor automatically"
                 (let [db (-> (fixture-db)
                              (assoc-in [:project-sidebar :expanded] #{"a"})
                              (assoc-in [:project-sidebar :pages]
                                        {"a" {:sessions [{"id" "a1" "title" "First"}]
                                              :grouped []
                                              :after nil
                                              :history []
                                              :next-cursor "old-cursor"
                                              :has-more false
                                              :request-id "first"}}))]
                   (with-redefs [state/app-db (atom db)]
                     (state/dispatch [:project-page-loaded "a" "first"
                                      {:sessions [{"id" "a-new" "title" "New"}
                                                  {"id" "a1" "title" "Updated"}]
                                       :grouped []
                                       :next-cursor "new-cursor"
                                       :has-more true
                                       :total 3} {:groups [] :total 0} nil true])
                     (let [page (get-in @state/app-db [:project-sidebar :pages "a"])]
                       (expect (= ["a-new" "a1"] (mapv #(get % "id") (:sessions page))))
                       (expect (= "Updated" (get-in page [:sessions 1 "title"])))
                       (expect (= "new-cursor" (:next-cursor page)))
                       (expect (true? (:has-more page)))
                       (expect (= [] (:history page))))
                     (expect (not-any? #(= :project-updates (:kind %))
                                       (projects/sidebar-entries @state/app-db)))))))

(defdescribe
  project-automatic-refresh-shows-arrivals-after-empty-page-test
  (it "shows arrivals on a previously empty page without an adoption action"
      (let [db (-> (fixture-db)
                   (assoc-in [:project-sidebar :expanded] #{"a"})
                   (assoc-in [:project-sidebar :pages]
                             {"a" {:sessions [] :grouped [] :request-id "empty" :has-more false}}))]
        (with-redefs [state/app-db (atom db)]
          (state/dispatch
            [:project-page-loaded "a" "empty"
             {:sessions [{"id" "new" "title" "Arrived"}] :grouped [] :has-more false :total 1}
             {:groups [] :total 0} nil true])
          (expect (= ["new"]
                     (mapv #(get % "id")
                           (get-in @state/app-db [:project-sidebar :pages "a" :sessions]))))
          (expect (some #(= "new" (get-in % [:session "id"]))
                        (projects/sidebar-entries @state/app-db)))
          (expect (not-any? #(= :project-updates (:kind %))
                            (projects/sidebar-entries @state/app-db)))))))

(defdescribe
  project-search-paints-ranked-unloaded-group-and-archive-test
  (it
    "project search paints unloaded, grouped and archived hits from one answer"
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

                    vis/gateway-search-sessions
                    (fn [q opts]
                      (swap! calls conj [:search q opts])
                      ;; Every hit arrives WITH its row, in the gateway's order.
                      {:sessions [{"id" "b9"
                                   "project_id" "b"
                                   "archived_at" "yesterday"
                                   "title" "Archived reply"}
                                  {"id" "a5" "project_id" "a" "group_id" "g" "title" "Grouped"}]
                       :matches matches})]

        (let [press! (fn [key]
                       (#'screen/project-sidebar-key!
                        (cap/key-stroke key)
                        identity
                        identity
                        identity
                        identity
                        nil))]
          (press! \/)
          (expect (= "" (get-in @state/app-db [:project-sidebar :search :text])))
          (press! \q)
          (expect (= [[:search "q" {:limit 200 :archived :include}]] @calls))
          (let [entries (projects/sidebar-entries @state/app-db)]
            (expect (= ["b9" "a5"] (mapv #(get-in % [:session "id"]) (filter :session entries))))
            (expect (= ["Companion / Archived reply" "Vis / Grouped"]
                       (mapv :label (filter :session entries))))
            (expect (some #(str/includes? (:label %) "reply: answer found") entries))
            (expect (some #(str/includes? (:label %) "request: request found") entries)))
          (press! :esc)
          (expect (nil? (get-in @state/app-db [:project-sidebar :search])))
          (expect (= (select-keys original [:pages :expanded :group-folds :index])
                     (select-keys (:project-sidebar @state/app-db)
                                  [:pages :expanded :group-folds :index]))))))))

(defdescribe
  project-search-stale-requests-pagination-and-recovery-test
  (it
    "project search stale requests pagination and recovery"
    (let [jobs
          (atom [])

          calls
          (atom [])

          failure?
          (atom false)

          matches
          (mapv (fn [i]
                  {:id (str "s" i) :in-title? true})
                (range 7))

          rows
          (mapv (fn [{:keys [id]}]
                  {"id" id "project_id" "a" "title" id})
                matches)]

      (with-redefs [state/app-db
                    (atom (assoc (fixture-db) :layout {:rows 18}))

                    vis/worker-future
                    (fn [_ f]
                      (swap! jobs conj f))

                    vis/gateway-search-sessions
                    (fn [q opts]
                      (swap! calls conj [:search q opts])
                      (when @failure? (throw (ex-info "offline" {})))
                      (if (= q "none")
                        {:sessions [] :matches []}
                        {:sessions rows :matches matches}))]

        (state/dispatch [:project-search-open])
        (#'screen/search-projects! {:text "old" :cursor 3})
        (#'screen/search-projects! {:text "new" :cursor 3})
        (expect (some #(= "Searching sessions…" (:label %))
                      (projects/sidebar-entries @state/app-db)))
        ((second @jobs))
        ((first @jobs))
        (expect (= [[:search "new" {:limit 200 :archived :include}]] @calls))
        (expect (= ["s0" "s1" "s2" "s3" "s4"]
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
        (expect (= 5 (get-in @state/app-db [:project-sidebar :search :offset])))
        (expect (= ["s5" "s6"]
                   (mapv #(get-in % [:session "id"])
                         (get-in @state/app-db [:project-sidebar :search :rows]))))
        (expect (false? (get-in @state/app-db [:project-sidebar :search :has-more?])))
        (let [previous (first (filter #(= [:search-page :previous] (:action %))
                                      (projects/sidebar-entries @state/app-db)))]
          (state/dispatch [:project-sidebar {:index (:index previous)}])
          (#'screen/project-sidebar-key!
           (cap/key-stroke :enter)
           identity
           identity
           identity
           identity
           nil))
        (expect (zero? (get-in @state/app-db [:project-sidebar :search :offset])))
        (let [stale-id (get-in @state/app-db [:project-sidebar :search :request-id])]
          (#'screen/search-projects! {:text "none" :cursor 4})
          (state/dispatch [:project-search-loaded stale-id matches [] 5 5 false])
          ((last @jobs)))
        (expect (some #(= "No saved sessions match" (:label %))
                      (projects/sidebar-entries @state/app-db)))
        (reset! failure? true)
        (#'screen/search-projects! {:text "error" :cursor 5})
        ((last @jobs))
        (expect (some #(str/includes? (:label %) "Search failed · gateway unavailable")
                      (projects/sidebar-entries @state/app-db)))
        (reset! failure? false)
        (#'screen/search-projects! {:text "retry" :cursor 5})
        ((last @jobs))
        (expect (nil? (get-in @state/app-db [:project-sidebar :search :error])))
        (expect (= 5 (count (get-in @state/app-db [:project-sidebar :search :rows]))))))))

(defdescribe
  project-automatic-refresh-preserves-focused-session-test
  (it
    "shows arriving rows while preserving the focused session"
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
        (expect (= ["new" "a1"]
                   (mapv #(get % "id")
                         (get-in @state/app-db [:project-sidebar :pages "a" :sessions]))))
        (expect (= "a1"
                   (get-in (nth (projects/sidebar-entries @state/app-db)
                                (dec (get-in @state/app-db [:project-sidebar :index])))
                           [:session "id"])))
        (let [capture (cap/capture!
                        {:cols 144
                         :rows 24
                         :paint!
                         (fn [{:keys [screen]}]
                           (projects/paint! (.newTextGraphics screen) @state/app-db 144 24))})
              hits (.current projects/hit-map)]

          (expect (nil? (:error capture)))
          (expect (some #(= [:session "new"] (:action %)) hits))
          (expect (not-any? #(= :project-updates (:kind %)) hits)))))))

(defdescribe
  project-automatic-refresh-shows-groups-and-grouped-arrivals-test
  (it
    "adopts group order, names and members while preserving focus and selection"
    (let [base
          (-> (fixture-db)
              (assoc-in [:project-sidebar :expanded] #{"a"})
              (assoc-in [:project-sidebar :selected "a"] #{"a1"})
              (assoc-in [:project-sidebar :groups "a"] [{"id" "g1" "name" "Original group"}])
              (assoc-in [:project-sidebar :pages "a"]
                        {:sessions []
                         :grouped [{"id" "a1" "title" "Current" "group_id" "g1"}]
                         :request-id "groups"
                         :has-more false}))

          focused
          (->> (projects/sidebar-entries base)
               (filter #(= "a1" (get-in % [:session "id"])))
               first
               :index)

          db
          (assoc-in base [:project-sidebar :index] focused)]

      (with-redefs [state/app-db (atom db)]
        (state/dispatch
          [:project-page-loaded "a" "groups"
           {:sessions []
            :grouped [{"id" "new-group-row" "title" "New group member" "group_id" "g-new"}
                      {"id" "new-member" "title" "Arriving member" "group_id" "g1"}
                      {"id" "a1" "title" "Current revised" "group_id" "g1"}]
            :has-more false
            :total 0}
           {:groups [{"id" "g-new" "name" "Incoming group"} {"id" "g1" "name" "Renamed group"}]
            :total 2} nil true])
        (expect (= ["g-new" "g1"]
                   (mapv #(get % "id") (get-in @state/app-db [:project-sidebar :groups "a"]))))
        (expect (= 2 (get-in @state/app-db [:project-sidebar :group-total "a"])))
        (let [entries (projects/sidebar-entries @state/app-db)
              focused-row (nth entries (dec (get-in @state/app-db [:project-sidebar :index])))]

          (expect (= ["new-group-row" "new-member" "a1"]
                     (vec (keep #(get-in % [:session "id"]) entries))))
          (expect (some #(= "Renamed group" (:label %)) entries))
          (expect (= "a1" (get-in focused-row [:session "id"])))
          (expect (:selected? focused-row))
          (expect (not-any? #(= :project-updates (:kind %)) entries)))))))

(defdescribe
  project-search-shortcut-and-empty-state-test
  (it
    "project search shortcut and empty state"
    (with-redefs [state/app-db (atom (fixture-db))]
      (let [paint! (fn []
                     (cap/capture!
                       {:cols 144
                        :rows 24
                        :paint!
                        (fn [{:keys [screen]}]
                          (projects/paint! (.newTextGraphics screen) @state/app-db 144 24))}))
            press! (fn [key]
                     (#'screen/project-sidebar-key! key identity identity identity identity nil))]

        (expect (nil? (:error (paint!))))
        (expect (not-any? #(= :project-search (:kind %)) (.current projects/hit-map)))
        (press! (cap/key-stroke \/))
        (expect (some? (get-in @state/app-db [:project-sidebar :search])))
        (expect (some #(= "Type to search saved sessions" (:label %))
                      (projects/sidebar-entries @state/app-db)))
        (expect (nil? (:error (paint!))))
        (expect (some #(= :project-search-field (:kind %)) (.current projects/hit-map)))
        (press! (cap/key-stroke :esc))
        (expect (nil? (get-in @state/app-db [:project-sidebar :search])))))))

(defdescribe
  project-refresh-subscription-lifecycle-test
  (it
    "project refresh subscription lifecycle"
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
          (try
            (expect (fn? @sink))
            (@sink {"type" "session.title_updated"})
            (Thread/sleep 100)
            (expect (empty? @reads) "Closed panes do not refresh")
            (state/dispatch [:project-sidebar {:open? true}])
            (loop [attempt 0]
              (when (and (empty? @reads) (< attempt 40)) (Thread/sleep 50) (recur (inc attempt))))
            (expect (= [true] @reads))
            (finally (stop)))
          (expect (= 1 @stops)))))))

(defn- every-kind-db
  "A rail with every row kind: projects, both sets, two groups and two saved sessions."
  []
  (-> (grouped-db)
      (assoc-in [:project-sidebar :expanded] #{"a"})
      (assoc-in [:project-sidebar :pages "a"]
                {:sessions [{"id" "s1" "title" "Loose session"}]
                 :grouped [{"id" "s2" "title" "Grouped session" "group_id" "g1"}]})))

(defn- paint-rail
  "Capture `db` painted as the project rail."
  [db cols rows]
  (cap/capture! {:cols cols
                 :rows rows
                 :paint! (fn [{:keys [screen]}]
                           (projects/paint! (.newTextGraphics screen) db cols rows))}))

(defdescribe sidebar-row-keys-test
             (it "runs every keyed menu item from its key on the focused row"
                 (let [db (every-kind-db)]
                   (doseq [entry (projects/sidebar-entries db)
                           :let [focused (assoc-in db [:project-sidebar :index] (:index entry))
                                 sid (get-in entry [:session "id"])]
                           {:keys [id key hint]} (projects/row-menu-items db entry)
                           :when key]

                     (expect (= (if (= \space key) "Space" (str key)) hint))
                     (expect (= (case id
                                  :details
                                  [:details sid]

                                  :toggle-session
                                  [:toggle-session "a" sid]

                                  :refresh
                                  [:refresh]

                                  [:menu (assoc entry :initial-action id)])
                                (projects/key-action focused (cap/key-stroke key)))))))
             (it "gives every sidebar command its own key and an action on some row"
                 (let [db
                       (every-kind-db)

                       keyed
                       (filter :key keymap/sidebar-commands)]

                   (expect (apply distinct? (map :key keyed)))
                   (doseq [{:keys [key]} keyed]
                     (expect (some (fn [entry]
                                     (not= [:noop]
                                           (projects/key-action
                                             (assoc-in db [:project-sidebar :index] (:index entry))
                                             (cap/key-stroke key))))
                                   (projects/sidebar-entries db))
                             (str "No row acts on " key)))))
             (it "opens the main settings and adds a project above the first row"
                 (let [header (assoc-in (every-kind-db) [:project-sidebar :index] 0)]
                   (expect (= [:menu {:initial-action :settings}]
                              (projects/key-action header (cap/key-stroke \s))))
                   (expect (= [:add] (projects/key-action header (cap/key-stroke \a))))))
             (it "walks rows with C-n, C-p, PgUp, PgDn, Home and End"
                 (let [db
                       (-> (every-kind-db)
                           (assoc-in [:project-sidebar :index] 3)
                           (assoc-in [:layout :rows] 12))

                       page
                       (dec (count (projects/visible-entries db 12)))]

                   (expect (= [:move 1] (projects/key-action db (KeyStroke. \n true false))))
                   (expect (= [:move -1] (projects/key-action db (KeyStroke. \p true false))))
                   (expect (= [:move page] (projects/key-action db (KeyStroke. KeyType/PageDown))))
                   (expect (= [:move (- page)]
                              (projects/key-action db (KeyStroke. KeyType/PageUp))))
                   (expect (= [:move -2] (projects/key-action db (KeyStroke. KeyType/Home))))
                   (expect (= [:move 5] (projects/key-action db (KeyStroke. KeyType/End))))
                   ;; In the add field, C-n still creates a folder.
                   (expect (= [:add-folder]
                              (projects/key-action
                                (assoc-in db [:project-sidebar :adding] {:text "/work/" :cursor 6})
                                (KeyStroke. \n true false)))))))

(defdescribe
  project-sidebar-footer-buttons-test
  (it "runs each footer pair as its key"
      (let [db
            (every-kind-db)

            entry
            (first (projects/sidebar-entries db))

            capture
            (paint-rail db 240 24)

            buttons
            (sort-by (comp :col :bounds)
                     (filter #(= :project-footer (:kind %)) (.current projects/hit-map)))

            click
            (fn [{{:keys [col row]} :bounds}]
              (projects/key-action db
                                   (MouseAction. MouseActionType/CLICK_DOWN
                                                 1
                                                 (TerminalPosition. (int col) (int row)))))]

        (expect (nil? (:error capture)))
        (expect (= [:help :menu :settings :add-project :back] (mapv :command buttons)))
        (expect (every? #(= 22 (get-in % [:bounds :row])) buttons))
        (expect (= [[:help] [:menu entry] [:menu (assoc entry :initial-action :settings)] [:add]
                    [:blur]]
                   (mapv click buttons)))))
  (it "keeps Esc in a narrow add field footer, which has no buttons"
      (let [db
            (assoc-in (every-kind-db) [:project-sidebar :adding] {:text "/work/" :cursor 6})

            capture
            (paint-rail db 40 18)

            footer
            (nth (str/split-lines (cap/frame-text capture)) 16)]

        (expect (nil? (:error capture)))
        (expect (str/includes? footer "Enter add"))
        (expect (str/includes? footer "Esc cancel"))
        (expect (not-any? #(= :project-footer (:kind %)) (.current projects/hit-map))))))

(defdescribe
  sidebar-dialog-anchor-test
  (it "anchors on the last painted row of each entry"
      (let [db
            (every-kind-db)

            capture
            (paint-rail db 144 24)

            rows
            (keep (fn [{:keys [kind index bounds]}]
                    (when (#{:project-select :project-group :project-set :project-session} kind)
                      [index (+ (long (:row bounds)) (long (:height bounds)) -1)]))
                  (.current projects/hit-map))]

        (expect (nil? (:error capture)))
        (expect (= 8 (count rows)))
        (doseq [[index row] rows]
          (expect (= row (projects/entry-row db 24 index))))
        ;; The add field hides the list, so no row anchors a dialog.
        (expect (nil? (projects/entry-row
                        (assoc-in db [:project-sidebar :adding] {:text "" :cursor 0})
                        24
                        1)))))
  (it "scopes sidebar dialogs to the rail, under their row"
      (let [db
            (assoc-in (every-kind-db) [:layout :rows] 24)

            session-entry
            (nth (projects/sidebar-entries db) 3)]

        (with-redefs [state/app-db (atom db)]
          (let [{:keys [width anchor-row]} (#'screen/sidebar-dialog-region session-entry)]
            (expect (= 9 anchor-row))
            (expect (= (:width (projects/geometry db 144 24)) (width 144 24)))
            ;; A row known only by its painted bounds anchors on its last row.
            (expect (= 12
                       (:anchor-row (#'screen/sidebar-dialog-region
                                     {:bounds {:row 10 :height 3}})))))))))

(defdescribe sidebar-row-menu-settings-and-ungroup-test
             (it "opens the settings of the row's session, group or project"
                 (let [db
                       (dissoc (every-kind-db) :session)

                       entries
                       (projects/sidebar-entries db)

                       opened
                       (atom [])]

                   (with-redefs-fn {#'state/app-db (atom db)
                                    #'screen/with-dialog-lock (fn [f]
                                                                (f))
                                    #'screen/open-settings-target! (fn [_ scope id label context]
                                                                     (swap! opened conj
                                                                       [scope id label context]))
                                    #'screen/open-settings-modal! (fn [& _]
                                                                    (swap! opened conj [:main]))}
                     (fn []
                       (doseq [index [1 3 4]]
                         (#'screen/sidebar-row-menu!
                          nil
                          (assoc (nth entries (dec index)) :initial-action :settings)
                          nil))
                       (#'screen/sidebar-row-menu! nil {:initial-action :settings} nil)))
                   (expect (= [["project" "a" "Vis" nil] ["group" "g1" "Release apps" nil]
                               ["session" "s2" "Grouped session" "s2"] [:main]]
                              @opened))))
             (it
               "takes a grouped session out of its group"
               (let [db
                     (every-kind-db)

                     moved
                     (atom [])]

                 (with-redefs-fn {#'state/app-db (atom db)
                                  #'screen/with-dialog-lock (fn [f]
                                                              (f))
                                  #'screen/move-project-sessions! (fn [& args]
                                                                    (swap! moved conj (vec args)))}
                   (fn []
                     (#'screen/sidebar-row-menu!
                      nil
                      (assoc (nth (projects/sidebar-entries db) 3) :initial-action :ungroup-session)
                      nil)))
                 (expect (= [["a" ["s2"] nil]] @moved)))))
