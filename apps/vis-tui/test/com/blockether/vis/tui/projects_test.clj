(ns com.blockether.vis.tui.projects-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.header-model :as model]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.projects :as projects]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlTerminal]))

(def project-a {"id" "a" "name" "Vis" "workspace_root" "/work/vis" "session_count" 2})

(def project-b {"id" "b" "name" "Companion" "workspace_root" "/work/companion" "session_count" 1})

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

        opened
        (atom [])

        requests
        (atom [])]

    (with-redefs [state/app-db
                  (atom (fixture-db))

                  vis/worker-future
                  (fn [_ f]
                    (swap! workers conj f))

                  vis/gateway-list-sessions
                  (fn [opts]
                    (swap! requests conj opts)
                    [{"id" "late" "project_position" 1} {"id" "first" "project_position" 0}])]

      (#'screen/request-project! {"id" "c" "workspace_root" "/work/c"} #(swap! opened conj %))
      (is (empty? @requests) "The input thread does not fetch sessions")
      ((first @workers))
      (is (= [{:project-id "c"}] @requests))
      (is (= ["first" "late"] (mapv :session-id (first @opened))))
      (is (every? #(= "/work/c" (:root %)) (first @opened)))
      (reset! opened [])
      (#'screen/request-project! {"id" "d"} #(swap! opened conj %))
      (#'screen/request-project! project-a #(swap! opened conj %))
      ((last @workers))
      (is (= [[]] @opened) "Late results cannot override a cached project choice"))))

(deftest project-errors-and-add-test
  (with-redefs [state/app-db
                (atom (fixture-db))

                vis/worker-future
                (fn [_ f]
                  (f))

                vis/gateway-list-sessions
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
    (is (str/includes? (get-in @state/app-db [:project-sidebar :error]) "Open failed"))
    (#'screen/refresh-projects!)
    (is (= [project-a project-b] (get-in @state/app-db [:project-sidebar :items])))
    (is (false? (get-in @state/app-db [:project-sidebar :loading?]))))
  (let [calls (atom [])]
    (with-redefs-fn {#'state/app-db (atom (fixture-db))
                     #'screen/with-dialog-lock (fn [f]
                                                 (f))
                     #'dlg/text-input-dialog! (constantly " /work/new ")
                     #'vis/worker-future (fn [_ f]
                                           (f))
                     #'vis/gateway-list-projects (constantly [project-a project-b])
                     #'vis/gateway-ensure-project-for-root! (fn [path]
                                                              (swap! calls conj path)
                                                              project-b)}
      #(do (#'screen/add-project!
            nil
            (fn [project]
              (swap! calls conj project)))
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
                                   (cap/key-stroke \+))))))

(deftest project-sidebar-grid-test
  (doseq [cols [24 40 80 120]]
    (let [db (fixture-db)
          capture (cap/capture! {:cols cols
                                 :rows 18
                                 :paint! (fn [{:keys [screen]}]
                                           (.beginFrame interactions/hit-map)
                                           (projects/paint! (.newTextGraphics screen) db cols 18)
                                           (.commitFrame interactions/hit-map))})
          text (cap/frame-text capture)
          left (:left (projects/geometry db cols 18))]

      (is (nil? (:error capture)))
      (is (str/includes? text "Projects"))
      (is (str/includes? text "Companion"))
      (is (str/includes? text "1 running"))
      (is (= :project-add (:kind (.lookup interactions/hit-map (- cols 7) 0))))
      (is (= :project-hide (:kind (.lookup interactions/hit-map (- cols 3) 0))))
      (is (= [:select project-b]
             (projects/key-action db
                                  (MouseAction. MouseActionType/CLICK_DOWN
                                                1
                                                (TerminalPosition. (+ (int left) 4) 4))))))))

(deftest project-sidebar-overflow-test
  (let [sidebar
        {:items (vec (repeat 50 project-a)) :index 50}

        visible
        (projects/visible-projects sidebar 16)]

    (is (= 5 (count visible)))
    (is (= 50 (first (last visible))))))

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

    (doseq [cols [40 80 120]]
      (let [capture (cap/capture! {:cols cols
                                   :rows 24
                                   :paint!
                                   (fn [{:keys [screen]}]
                                     (let [db (fixture-db)
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
        (is (not-any? #(= :project-select (:kind %)) (.current interactions/hit-map)))))))

(deftest project-screen-backend-parity-test
  (with-redefs [timg/images-protocol
                (constantly nil)

                vis/get-router
                (constantly nil)]

    (doseq [cols [24 40 80 120]]
      (with-open [html (review-terminal cols 24)
                  html-screen (doto (TerminalScreen. html) (.startScreen))]

        (let [capture (cap/capture!
                        {:cols cols
                         :rows 24
                         :paint! (fn [{:keys [^TerminalScreen screen]}]
                                   (#'screen/render-frame! screen cols 24 (fixture-db) 1000)
                                   (#'screen/render-frame! html-screen cols 24 (fixture-db) 1000)
                                   (is (= (for [y (range 24)
                                                x (range cols)]

                                            (.getFrontCharacter screen x y))
                                          (for [y (range 24)
                                                x (range cols)]

                                            (.getFrontCharacter html-screen x y)))))})]
          (is (nil? (:error capture)))
          (is (str/includes? (.renderHtml html) "Projects")))))))

(deftest project-sidebar-dispatch-test
  (let [added (atom 0)]
    (with-redefs [state/app-db (atom (fixture-db))
                  vis/gateway-list-projects (constantly [project-a project-b])
                  vis/worker-future (fn [_ f]
                                      (f))
                  timg/images-protocol (constantly nil)]

      (let [select! #(state/dispatch [:select-project (get % "id") [] "unused"])
            add! #(swap! added inc)]

        (is (true? (#'screen/project-sidebar-key! (cap/key-stroke :down) select! add!)))
        (#'screen/project-sidebar-key! (cap/key-stroke :enter) select! add!)
        (is (= "b" (:active-project-id @state/app-db)))
        (is (= "background-turn" (:gateway-turn-id @state/app-db)))
        (#'screen/project-sidebar-key! (cap/key-stroke \+) select! add!)
        (is (= 1 @added))
        (#'screen/project-sidebar-key! (cap/key-stroke :esc) select! add!)
        (is (false? (get-in @state/app-db [:project-sidebar :focused?])))
        (is (nil? (#'screen/project-sidebar-key! (cap/key-stroke \a) select! add!)))
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
