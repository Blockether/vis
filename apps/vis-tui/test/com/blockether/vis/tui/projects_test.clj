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
      (when (>= cols 26) (is (str/includes? text "↑↓ select · Enter open")))
      (is (str/includes? text (if (>= cols 26) "C-x w hide · Esc chat" "C-x w hide")))
      (is (= :project-add (:kind (.lookup projects/hit-map (- width 8) 1))))
      (is (= :project-hide (:kind (.lookup projects/hit-map (- width 4) 1))))
      (is (= "┌" (get-in capture [:frames 0 0 0 :ch])))
      (is (= "┤" (get-in capture [:frames 0 2 (dec width) :ch])))
      (is (= "┘" (get-in capture [:frames 0 17 (dec width) :ch])))
      (is (= [:select project-b]
             (projects/key-action
               db
               (MouseAction. MouseActionType/CLICK_DOWN 1 (TerminalPosition. 4 5))))))))

(deftest project-sidebar-label-width-test
  (doseq [[cols index label fits?] [[40 0 "vis-python-runtime" true]
                                    [40 1 "tree-sitter-language-pack" false]
                                    [168 1 "tree-sitter-language-pack" true]]]
    (let [db (assoc-in (fixture-db) [:project-sidebar :items index "name"] label)
          capture (cap/capture! {:cols cols
                                 :rows 18
                                 :paint! (fn [{:keys [screen]}]
                                           (projects/paint! (.newTextGraphics screen) db cols 18))})
          row (nth (str/split-lines (cap/frame-text capture)) (+ 4 index))]

      (is (nil? (:error capture)))
      (is (= fits? (str/includes? row label)))
      (is (= (not fits?) (str/includes? row "…"))))))

(deftest project-sidebar-overflow-test
  (let [sidebar
        {:items (vec (repeat 50 project-a)) :index 50}

        visible
        (projects/visible-entries {:project-sidebar sidebar} 16)]

    (is (= 8 (count visible)))
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
            [(fixture-db) (attention-fixture-db)]

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
  (let [added (atom 0)]
    (with-redefs [state/app-db (atom (fixture-db))
                  vis/gateway-list-projects (constantly [project-a project-b])
                  vis/worker-future (fn [_ f]
                                      (f))
                  timg/images-protocol (constantly nil)]

      (let [select! #(state/dispatch [:select-project (get % "id") [] "unused"])
            add! #(swap! added inc)
            refresh! (fn [_])]

        (is (true? (#'screen/project-sidebar-key! (cap/key-stroke :down) select! add! refresh!)))
        (#'screen/project-sidebar-key! (cap/key-stroke :enter) select! add! refresh!)
        (is (= "b" (:active-project-id @state/app-db)))
        (is (= "background-turn" (:gateway-turn-id @state/app-db)))
        (#'screen/project-sidebar-key! (cap/key-stroke \+) select! add! refresh!)
        (is (= 1 @added))
        (#'screen/project-sidebar-key! (cap/key-stroke :esc) select! add! refresh!)
        (is (false? (get-in @state/app-db [:project-sidebar :focused?])))
        (is (nil? (#'screen/project-sidebar-key! (cap/key-stroke \a) select! add! refresh!)))
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
                       (let [hit (first (filter #(= :workspace-entry (:kind %))
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
      (is (= [:select :select :input]
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

(deftest project-input-grid-and-navigation-test
  (doseq [pointer? [true false]]
    (let [refreshes (atom [])
          workers (atom [])
          opened (atom false)]

      (with-redefs [state/app-db (atom (assoc (attention-fixture-db)
                                         :project-active-tabs {"b" :tab-4}))
                    vis/worker-future (fn [_ f]
                                        (swap! workers conj f))
                    vis/gateway-list-sessions (constantly [])]

        (#'screen/request-project!
         {"id" "uncached"}
         (fn [_]
           (reset! opened true)))
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
                           (swap! refreshes conj notify?)))]

          (is (nil? (:error capture)))
          (is (re-find #"Companion +2 tabs · 1 running · 1 needs input" text))
          (is (re-find #"! Mobile navigation +needs input" text))
          (is (= 6 row) "The alert appears immediately below its project")
          (is (= [(.getRed ^com.googlecode.lanterna.TextColor theme/warning-fg)
                  (.getGreen ^com.googlecode.lanterna.TextColor theme/warning-fg)
                  (.getBlue ^com.googlecode.lanterna.TextColor theme/warning-fg)]
                 (get-in capture [:frames 0 row 43 :fg])))
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
          (is (false? (get-in @state/app-db [:project-sidebar :focused?])))
          ((first @workers))
          (is (false? @opened) "A late project lookup cannot replace the selected request"))))))

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

    (is (= 8 (count visible)))
    (is (= :project-select (:kind (first visible)))
        "Keep the parent visible above a long waiting group")
    (is (= 31 (:index (last visible))))
    (is (= [:input "29"] (projects/key-action db (cap/key-stroke :enter))))
    (is (empty? (projects/visible-entries db 8)))))

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
           (is (>= (#'theme-test/contrast-ratio theme/dialog-hint theme/terminal-bg) 4.5)
               (str id " sidebar hints and borders")))
         (finally (theme/apply-theme! before)))))
