(ns com.blockether.vis.tui.app-surface-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.tui.app-model :as model]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.app-surface :as surface]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.view-materializer :as live]
            [com.blockether.vis.tui.view-model :as view-model]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TextColor$RGB]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]))

(def ^:private view
  {:id "app-1"
   :title "Review"
   :seq 4
   :placement :attachment
   :nodes [{:id "diff"
            :type :diff
            :new-revision "b2"
            :patch "--- a/x.clj\n+++ b/x.clj\n@@ -1,2 +1,2 @@\n a\n-b\n+c\n"
            :threads [{:id "t1"
                       :anchor {:path "x.clj" :side "new" :line 2}
                       :messages [{:id "m1" :author {:kind "agent"} :body "Why?" :created-at 1}]}]}
           {:id "notes" :type :thread :label "Notes" :messages []}]
   :submissions [{:id "sub-1" :status "delivered"}]})

(def ^:private own-keys
  "The shortcuts of an app that declares its keys. Such an app keeps a row for each
   button without a key. An app with no shortcut gets a key for each button."
  [{:key "!" :label "Ping" :action "ping"}])

(defn- harness
  "Key driver that records every effect instead of calling the gateway."
  [v]
  (let [calls
        (atom [])

        effects
        {:save! (fn [action _]
                  (swap! calls conj [:save action]))
         :send! (fn [action node-id]
                  (swap! calls conj [:send action node-id]))
         :comment! (fn []
                     (swap! calls conj [:comment]))
         :activate! (fn [action]
                      (swap! calls conj [:activate action]))
         :reload! (fn []
                    (swap! calls conj [:reload]))
         :select! (fn [action]
                    (swap! calls conj [:select action]))
         :paste identity}

        press
        (fn [st key]
          (let [geom (surface/measure st 100 40 v)]
            (surface/reconcile (surface/handle-key st key geom effects) geom)))]

    {:calls calls
     :press press
     :start (surface/reconcile (surface/initial-state)
                               (surface/measure (surface/initial-state) 100 40 v))}))

(defn- k ([kt] (KeyStroke. ^KeyType kt)) ([ch ctrl?] (KeyStroke. (char ch) (boolean ctrl?) false)))

(defn- row-at [st v] (nth (:rows (surface/measure st 100 40 v)) (:selected st)))

(defn- down
  [press st n]
  (reduce (fn [s _]
            (press s (k KeyType/ArrowDown)))
          st
          (range n)))

(defn- canvas-view [opts]
  (assoc view :nodes [(merge {:type :canvas :id "board"
                             :cards [{:id "a" :title "First"} {:id "b" :title "Second"}]}
                            opts)]))

(defn- app-frame [v st cols rows]
  (let [geom (surface/measure st cols rows v)
        st (surface/reconcile st geom)
        cursor (volatile! nil)
        captured (cap/capture! {:cols cols :rows rows
                                :paint! (fn [{:keys [g]}]
                                          (vreset! cursor (#'surface/paint! g st geom)))})]
    (expect (nil? (:error captured)))
    {:geom geom :cells (last (:frames captured)) :cursor @cursor}))

(defdescribe canvas-reading
  (it "paints a static canvas once while idle and again when a patch arrives"
      (let [current (atom (canvas-view {}))
            polls (atom 0)
            captured (cap/capture!
                       {:cols 80 :rows 24
                        :paint! (fn [{:keys [screen]}]
                                  (with-redefs-fn {#'surface/read-view! (constantly nil)}
                                    #(dlg/run-modal!
                                       screen
                                       (assoc (surface/component nil "app-1" @current screen)
                                         :measure (fn [st cols rows]
                                                    (surface/measure st cols rows @current))
                                         :read-key (fn [_]
                                                     (case (swap! polls inc)
                                                       1 nil
                                                       2 (do (swap! current assoc-in [:nodes 0 :cards 0 :title] "Patched") nil)
                                                       3 (k KeyType/Escape)))))))})]
        (expect (nil? (:error captured)))
        (expect (= 2 (count (:frames captured))))
        (expect (str/includes? (cap/frame-text captured 0) "First"))
        (expect (str/includes? (cap/frame-text captured) "Patched"))))
  (it "reads all rows of a long canvas card without moving to a control"
      (let [v (dissoc (canvas-view {:cards [{:id "a" :title "One"
                                            :text (str/join "\n\n" (map #(str "Line " %) (range 18)))}]})
                      :submissions)
            initial (surface/initial-state)
            geom (surface/measure initial 32 10 v)
            start (surface/reconcile initial geom)
            press (fn [st key]
                    (surface/reconcile (surface/handle-key st key geom {}) geom))
            scroll-row (fn [st] (nth (:rows geom) (:scroll st)))
            page (press start (k KeyType/PageDown))
            end (press page (k KeyType/End))
            down (press start (MouseAction. MouseActionType/SCROLL_DOWN 5 (TerminalPosition. 5 4)))]
        (expect (< (:scroll start) (:scroll page)))
        (expect (= (:selected start) (:selected page)))
        (expect (= "  Line 17" (:text (scroll-row end))))
        (expect (< (:scroll start) (:scroll down)))
        (expect (= (:scroll start) (:scroll (press page (k KeyType/PageUp)))))))
  (it "does not offer Enter for an item outside the reading window"
      (let [v
            (canvas-view {:is-selectable true
                          :cards [{:id "a"
                                   :title "One"
                                   :text (str/join "\n\n" (repeat 22 "Read this line."))}]})

            initial
            (surface/initial-state)

            geom
            (surface/measure initial 45 18 v)

            start
            (surface/reconcile initial geom)

            end
            (surface/handle-key start (k KeyType/End) geom {})

            hint
            (fn [st]
              (let [{:keys [geom cells]} (app-frame v st 45 18)]
                (apply str (map :ch (nth cells (:hint-row geom))))))]

        (expect (str/includes? (hint start) "Enter"))
        (expect (not (str/includes? (hint end) "Enter")))))
  (it "keeps canvas text inside its right rail in a narrow terminal"
      (let [v (canvas-view {:cards [{:id "a" :title "One" :text "From the request to the release."}]})
            {:keys [geom cells]} (app-frame v (surface/initial-state) 20 24)
            {:keys [right]} (:bounds geom)
            top (:content-top geom)]
        (expect (every? #(= "│" (get-in cells [% right :ch]))
                        (range top (+ top (:content-h geom)))))))
  (it "keeps an open canvas draft visible and editable after a resize"
      (let [v (canvas-view {})
            {:keys [start press calls]} (harness v)
            editor (-> start (press (k KeyType/Enter)) (press {:paste "Keep this draft"}))
            {:keys [geom cells cursor]} (app-frame v editor 20 10)
            {:keys [left bottom]} (:bounds geom)
            row (some-> ^TerminalPosition cursor .getRow)
            line (fn [y] (apply str (map :ch (nth cells y))))]
        (expect (some? cursor))
        (when row
          (expect (< row (- bottom 2)))
          (expect (re-find #"Keep" (line row))))
        (expect (= "──" (subs (line bottom) (inc left) (+ left 3))))
        (expect (re-find #"save" (line (:hint-row geom))))
        (press editor (k \s true))
        (expect (= "Keep this draft" (get-in @calls [0 1 :body]))))))

(defdescribe canvas-navigation
  (it "shows a refused canvas save and keeps the draft open"
      (doseq [[body reason] [["{\"reason\":\"invalid\",\"message\":\"The thread is full\"}" "The thread is full"]
                             ["{\"reason\":\"Canvas item not found\"}" "Canvas item not found"]]]
        (let [v (canvas-view {})
              events (atom [])]
          (with-redefs-fn {#'surface/read-view! (constantly nil)
                           #'vis/worker-future (fn [_ f] (f))
                           #'vis/request! (fn [method _ request]
                                            (swap! events conj [method (:body request)])
                                            {:status 400 :body body})}
              (fn []
                (let [{:keys [init measure reconcile on-key]} (surface/component "session" (:id v) v)
                      geom (measure init 80 24)
                      start (reconcile init geom)
                      press #(on-key %1 %2 geom)
                      editor (-> start (press (k KeyType/Enter)) (press {:paste "Keep my reply"}))
                      saving (press editor (k \s true))
                      refused (reconcile saving (measure saving 80 24))
                      {:keys [cells]} (app-frame v refused 80 24)
                      frame (str/join "\n" (map #(apply str (map :ch %)) cells))
                      compact (app-frame v refused 40 10)
                      compact-text (str/join "\n" (map #(apply str (map :ch %)) (:cells compact)))]
                  (expect (= [[:post {:action "draft" :node-id "board" :item-id "a"
                                      :body "Keep my reply" :base-seq 4}]] @events))
                  (expect (= (:editor editor) (:editor refused)))
                  (expect (str/includes? frame reason))
                  (expect (str/includes? compact-text reason))
                  (expect (some? (:cursor compact)))))))))
  (it
    "keeps the open draft base until a conflict is read and the person saves again"
    (let [view
          (canvas-view {:threads [{:id "t" :item-id "a" :draft "Prior" :messages []}]})

          competing
          (-> view
              (assoc :seq 5)
              (assoc-in [:nodes 0 :threads 0 :draft] "Competing"))

          calls
          (atom [])

          remote
          (atom view)

          press-save
          (k \s true)]

      (with-redefs [vis/worker-future
                    (fn [_ f]
                      (f))

                    vis/gateway-app-read
                    (fn [_ _ since]
                      (swap! calls conj [:read since])
                      {"view" @remote})

                    view-model/live-view<-wire
                    identity

                    vis/gateway-app-action!
                    (fn [_ _ action]
                      (swap! calls conj [:save (:base-seq action) (:body action)])
                      (if (= 5 (:base-seq action))
                        {:is-accepted true :seq 6 :thread-id "t"}
                        (do (reset! remote competing)
                            {:reason "conflict" :seq 5 :http-status 409})))]

        (let [{:keys [init measure reconcile on-key]}
              (surface/component "conflict-session" (:id view) view)

              key!
              (fn [st key]
                (on-key st key (measure st 80 24)))

              sync!
              (fn [st]
                (reconcile st (measure st 80 24)))

              start
              (sync! init)

              rows
              (:rows (measure start 80 24))

              thread-row
              (first (keep-indexed #(when (= "t" (:thread-id %2)) %1) rows))

              editor
              (-> (assoc start :selected thread-row)
                  (key! (k KeyType/Enter))
                  (key! {:paste " mine"}))

              newer-geom
              (assoc (measure editor 80 24) :view competing)

              refused
              (sync! (on-key editor press-save newer-geom))]

          (expect (= [[:read nil] [:save 4 "Prior mine"] [:read nil]] @calls))
          (expect (= :conflict (get-in refused [:save :state])))
          (expect (= (:input (:editor editor)) (get-in refused [:editor :input])))
          (key! refused press-save)
          (expect (= [:save 5 "Prior mine"] (last (filter #(= :save (first %)) @calls))))))))
  (it "opens each canvas thread and can start a new one"
      (let [v (canvas-view {:threads [{:id "one" :item-id "a" :messages [] :is-resolved true}
                                      {:id "two" :item-id "a" :messages [] :draft "unsent"}]})
            {:keys [press start]} (harness v)
            target (fn [n] (get-in (press (down press start n) (k KeyType/Enter)) [:editor :target]))]
        (expect (= [nil "one" "two"] (mapv (comp :thread-id target) (range 3))))
        (expect (= "a" (:item-id (target 0))))
        (expect (= "unsent" (:draft (target 2))))))
  (it "selects a canvas item and offers a separate stop for a new thread"
      (let [v (canvas-view {:is-selectable true})
            {:keys [press start calls]} (harness v)
            selected (press start (k KeyType/Enter))
            comment (press (down press start 1) (k KeyType/Enter))]
        (expect (= [[:select {:action :select :node-id "board" :item-ids ["a"]}]] @calls))
        (expect (nil? (:editor selected)))
        (expect (= "a" (get-in comment [:editor :target :item-id])))))
  (it "attaches existing canvas messages without a note and can remove the draft"
      (let [thread {:id "discussion" :item-id "a"
                    :messages [{:id "message" :author {:kind "agent"} :body "Review this" :created-at 1}]}
            v (canvas-view {:threads [thread]})
            {:keys [press start calls]} (harness v)
            thread-editor #(press (down press start 1) (k KeyType/Enter))
            expected {:action :draft :node-id "board" :thread-id "discussion" :base-seq 4}]
        (press (thread-editor) (KeyStroke. KeyType/Enter true false))
        (expect (= [[:save (assoc expected :body "")]] @calls))
        (let [saved (assoc-in v [:nodes 0 :threads 0 :draft] "")
              {:keys [press start calls]} (harness saved)]
          (expect (some #(re-find #"without a note" (:text %))
                        (surface/surface-rows saved 100 #{})))
          (-> (down press start 1) (press (k KeyType/Enter)) (press (k \d true)))
          (expect (= [[:save (assoc expected :body nil)]] @calls)))
        (let [{:keys [press start calls]} (harness (assoc-in v [:nodes 0 :threads 0 :messages] []))]
          (-> (down press start 1) (press (k KeyType/Enter)) (press (k \s true)))
          (expect (empty? @calls)))))
  (it "keeps a local comment when a patch removes its item"
      (let [v (canvas-view {})
            {:keys [press start calls]} (harness v)
            editor (-> start (press (k KeyType/Enter)) (press {:paste "keep the text"}))
            next-view (update-in v [:nodes 0 :cards] subvec 1)
            geom (surface/measure editor 100 40 next-view)
            removed (surface/reconcile editor geom)
            attempted (surface/handle-key removed (k \s true) geom
                                          {:save! (fn [action _] (swap! calls conj action))})]
        (expect (empty? @calls))
        (expect (= "keep the text" (input/input->text (get-in attempted [:editor :input]))))
        (expect (re-find #"item .* gone" (surface/status-line removed)))))
  (it "keeps the canvas cursor on the same thread when rows arrive above it"
      (let [v (canvas-view {:threads [{:id "second-thread" :item-id "b" :messages []}]})
            {:keys [press start]} (harness v)
            focused (down press start 2)
            next-view (update-in v [:nodes 0 :cards] #(into [{:id "new" :title "New"}] %))
            refreshed (surface/reconcile focused (surface/measure focused 100 40 next-view))]
        (expect (= "second-thread" (:thread-id (row-at refreshed next-view))))))
  (it "opens a collapsed group to reach its canvas and can close it again"
      (let [v (update (canvas-view {}) :nodes
                      (fn [nodes] [{:id "group" :type :collapsible :label "Review queue" :children nodes}]))
            {:keys [press start]} (harness v)
            opened (press start (k KeyType/Enter))
            closed (press opened (k KeyType/Enter))
            items (fn [st] (set (keep :item-id (:rows (surface/measure st 100 40 v)))))]
        (expect (= "group" (:node-id (row-at start v))))
        (expect (empty? (items start)))
        (expect (= #{"a" "b"} (items opened)))
        (expect (empty? (items closed)))))
  (it "does not send an oversized canvas draft and lets the person shorten it"
      (let [v (canvas-view {})
            {:keys [press start calls]} (harness v)
            limit (:canvas-draft app/bounds)
            text (apply str (repeat limit "😀"))
            oversized (-> start (press (k KeyType/Enter)) (press {:paste (str text "!")}))
            refused (press oversized (k \s true))]
        (expect (zero? (count @calls)))
        (expect (re-find (re-pattern (str limit)) (surface/status-line refused)))
        (expect (= (str text "!") (input/input->text (get-in refused [:editor :input]))))
        (-> refused (press (k KeyType/Backspace)) (press (k \s true)))
        (expect (= text (get-in @calls [0 1 :body]))))))

(defdescribe
  diff-navigation
  (it "stops on the file row first and then on each line"
      (let [{:keys [press start]} (harness view)]
        (expect (= :file (:kind (row-at start view))))
        (expect (= [:ctx :del :add] (mapv #(:kind (row-at (down press start %) view)) [1 2 3])))))
  (it "folds the file with ← from a line inside it and unfolds with →"
      (let [{:keys [press start]}
            (harness view)

            folded
            (press (down press start 2) (k KeyType/ArrowLeft))]

        (expect (= #{"x.clj"} (:folded folded)))
        (expect (= :file (:kind (row-at folded view))))
        (expect (= #{} (:folded (press folded (k KeyType/ArrowRight)))))))
  (it "scrolls code sideways but keeps the gutter"
      (let [{:keys [press start]}
            (harness view)

            st
            (press (assoc start :wrap? false) (k \> false))]

        (expect (= 8 (:hscroll st)))
        (expect (= 0 (:hscroll (press (press st (k \< false)) (k \< false)))))
        (expect (= "    1     1   9abc"
                   (surface/row-text {:kind :ctx :old 1 :new 1 :text "123456789abc"} 8 40))))))

(defdescribe
  thread-editor
  (it "saves a new canvas draft on the selected item"
      (let [canvas
            (assoc view :nodes [{:type :canvas :id "board" :cards [{:id "item" :title "Review"}]}])

            {:keys [press start calls]}
            (harness canvas)

            st
            (-> start
                (press (k KeyType/Enter))
                (press (k \o false))
                (press (k \k false))
                (press (k \s true)))]

        (expect (= [[:save
                     {:action :draft :node-id "board" :item-id "item" :base-seq 4 :body "ok"}]]
                   @calls))
        (expect (= :saving (get-in st [:save :state])))))
  (it "opens on a line with no thread at that anchor"
      (let [{:keys [press start]}
            (harness view)

            st
            (press (down press start 2) (k KeyType/Enter))]

        (expect (= {:node-id "diff" :anchor {:path "x.clj" :side "old" :line 2}}
                   (get-in st [:editor :target])))))
  (it "opens the existing thread of an anchored line"
      (let [{:keys [press start]}
            (harness view)

            st
            (press (down press start 3) (k KeyType/Enter))]

        (expect (= "t1" (get-in st [:editor :target :thread-id])))))
  (it
    "saves through a draft action and shows Saving until the ack"
    (let [{:keys [press start calls]}
          (harness view)

          st
          (-> (down press start 3)
              (press (k KeyType/Enter))
              (press (k \o false))
              (press (k \k false))
              (press (k \s true)))]

      (expect
        (=
          [[:save
            {:action :draft :node-id "diff" :body "ok" :base-seq 4 :thread-id "t1" :revision "b2"}]]
          @calls))
      (expect (= :saving (get-in st [:save :state])))))
  (it "creates nothing for a blank new comment"
      (let [{:keys [press start calls]}
            (harness view)

            st
            (-> (down press start 2)
                (press (k KeyType/Enter))
                (press (k \space false))
                (press (k \s true)))]

        (expect (empty? @calls))
        (expect (nil? (:editor st)))
        (expect (= :skipped (get-in st [:save :state])))))
  (it "cancels with Escape and keeps the saved draft for the next open"
      (let [v
            (assoc-in view [:nodes 0 :threads 0 :draft] "kept")

            {:keys [press start calls]}
            (harness v)

            st
            (-> (down press start 3)
                (press (k KeyType/Enter)))]

        (expect (= "kept" (input/input->text (get-in st [:editor :input]))))
        (expect (nil? (:editor (press st (k KeyType/Escape)))))
        (expect (empty? @calls))))
   (it "edits the thread node from its head row"
       (let [{:keys [press start]} (harness view)
             rows (:rows (surface/measure start 100 40 view))
             stops (surface/selectable-indexes rows)
             steps (count (take-while #(not= :thread-node (:kind (nth rows %))) stops))
             st (press (down press start steps) (k KeyType/Enter))]
         (expect (= "notes" (get-in st [:editor :target :thread-id]))))))

(defdescribe
  save-and-send-states
  (describe "Save"
            (it "says Saved only with the ack seq"
                (expect (= :saved (:state (surface/saved {} {:is-accepted true :seq 5}))))
                (expect (= :failed (:state (surface/saved {} {:is-accepted true}))))
                (expect (= :offline (:state (surface/saved {} {:reason "offline"}))))
                (expect (= :conflict (:state (surface/saved {} {:reason "conflict"}))))))
  (describe
    "Send"
    (it "reuses the key of a Send without a receipt and makes a new key after one"
        (let [{:keys [press start calls]}
              (harness view)

              first-try
              (press start (k \s false))

              offline
              (update first-try :send surface/sent {:reason "offline"})

              retry
              (press offline (k \s false))

              receipt
              (update retry :send surface/sent {:is-accepted true :seq 6 :submission {:id "sub-2"}})

              again
              (press receipt (k \s false))

              keys
              (mapv #(get-in % [1 :idempotency-key]) @calls)]

          (expect (= 3 (count keys)))
          (expect (= (first keys) (second keys)))
          (expect (not= (second keys) (nth keys 2)))
          (expect (= (nth keys 2) (get-in again [:send :key])))))
    ;; The next turn of the session carries the saved comments, not an app Send.
    (it "sends the saved comments with a turn of the session"
        (let [commented
              (assoc-in view [:nodes 0 :threads 0 :draft] "")

              {:keys [press start calls]}
              (harness commented)]

          (press start (k \s false))
          (expect (= [[:comment]] @calls))))
    ;; Issue #421: a wire view can keep the button action as a string.
    (it "sends only from a submit button, also when its action is a string"
        (doseq [[action expected] [[:submit [:send :submit "go"]] ["submit" [:send :submit "go"]]
                                   [nil [:activate :activate "go"]]]]
          (let [v (update (assoc view :shortcuts own-keys)
                          :nodes
                          conj
                          (cond-> {:id "go" :type :button :label "Go"}
                            action
                            (assoc :action action)))
                {:keys [press start calls]} (harness v)
                at (first (keep-indexed (fn [i row]
                                          (when (and (= "go" (:node-id row)) (:is-selectable row))
                                            i))
                                        (:rows (surface/measure start 100 40 v))))]

            (expect (some? at))
            (press (assoc start :selected at) (k KeyType/Enter))
            (expect (= [expected]
                       (mapv (fn [[kind payload node-id]]
                               [kind (:action payload) (or node-id (:node-id payload))])
                             @calls))))))))

(defdescribe read-only-app
             (it "opens no editor and sends nothing once the app is closed"
                 (let [v
                       (assoc view :is-closed true)

                       {:keys [press start calls]}
                       (harness v)

                       st
                       (-> (down press start 3)
                           (press (k KeyType/Enter))
                           (press (k \s false)))]

                   (expect (nil? (:editor st)))
                   (expect (empty? @calls)))))

(defdescribe
  session-state
  (it "keeps app views out of the live band and applies their patches in order"
      (reset! state/app-db
        {:session {:id "s1"} :tabs [] :live-views [] :app-views {} :render-version 0})
      (state/dispatch [:live-view-open (assoc view :session-id "s1")])
      (expect (empty? (:live-views @state/app-db)))
      (expect (= 4 (get-in @state/app-db [:app-views "app-1" :view :seq])))
      (state/dispatch [:live-view-patch
                       {:view-id "app-1"
                        :seq 5
                        :ops [{:op :submission
                               :submission {:id "sub-3"
                                            :idempotency-key "tui-12345678"
                                            :status "accepted"
                                            :created-at 1
                                            :updated-at 1}}]}])
      (expect (= ["sub-1" "sub-3"]
                 (mapv :id (get-in @state/app-db [:app-views "app-1" :view :submissions]))))
      ;; A frame that coalesces patches 6 and 7 continues seq 5. It applies at once:
      ;; a game sends such frames all the time, and each one cost a full read.
      (state/dispatch [:live-view-patch
                       {:view-id "app-1"
                        :seq 7
                        :ops [{:op :submission
                               :submission {:id "sub-4"
                                            :idempotency-key "tui-87654321"
                                            :status "accepted"
                                            :created-at 2
                                            :updated-at 2}}]} 6])
      (expect (= 7 (get-in @state/app-db [:app-views "app-1" :view :seq])))
      (expect (not (get-in @state/app-db [:app-views "app-1" :is-gap])))
      (state/dispatch [:live-view-patch
                       {:view-id "app-1" :seq 9 :ops [{:op :clear :node-id "diff"}]}])
      (expect (true? (get-in @state/app-db [:app-views "app-1" :is-gap])))
      (state/dispatch [:app-view-loaded
                       (assoc view
                         :session-id "s1"
                         :seq 9)])
      (expect (false? (get-in @state/app-db [:app-views "app-1" :is-gap])))
      (state/dispatch [:live-view-close "app-1" {:reason :completed}])
      (expect (= :closed (model/status (get-in @state/app-db [:app-views "app-1" :view])))))
  ;; Report: the terminal got each large patch of an app whole on its stream. The
  ;; stream now leaves such a patch out and names the seq of the view.
  (it "marks an app behind an elided frame, also for a frame that came during the read"
      (let [gap?
            #(get-in @state/app-db [:app-views "app-1" :is-gap])

            loaded
            #(state/dispatch [:app-view-loaded
                              (assoc view
                                :session-id "s1"
                                :seq %)])]

        (reset! state/app-db
          {:session {:id "s1"} :tabs [] :live-views [] :app-views {} :render-version 0})
        (state/dispatch [:live-view-open (assoc view :session-id "s1")])
        ;; A replayed frame is not news.
        (state/dispatch [:app-view-behind "app-1" 4])
        (expect (false? (gap?)))
        (state/dispatch [:app-view-behind "app-1" 6])
        (expect (true? (gap?)))
        (state/dispatch [:app-view-behind "app-1" 8])
        (loaded 6)
        (expect (true? (gap?)))
        (loaded 8)
        (expect (false? (gap?)))
        (state/dispatch [:app-view-behind "other" 3])
        (expect (= ["app-1"] (keys (:app-views @state/app-db))))))
  ;; Report: a gateway restart sent the whole view of a large app two times, only to
  ;; say that its owner left and came back. The stream now sends the summary of the app.
  (it
    "keeps the body of an app when its summary comes, and reads an app without a body"
    (let [held
          #(get-in @state/app-db [:app-views "app-1"])

          whole
          (assoc view :session-id "s1")

          summary
          (fn [seq & {:as more}]
            (merge (assoc (dissoc whole :nodes :submissions)
                     :seq seq
                     :is-summary true)
                   more))]

      (reset! state/app-db
        {:session {:id "s1"} :tabs [] :live-views [] :app-views {} :render-version 0})
      ;; No copy: the summary is all that the terminal has, so it reads the app.
      (state/dispatch [:live-view-open (summary 3)])
      (expect (true? (:is-gap (held))))
      ;; A patch has no body to change.
      (state/dispatch [:live-view-patch
                       {:view-id "app-1" :seq 4 :ops [{:op :clear :node-id "diff"}]}])
      (expect (= (summary 3) (:view (held))))
      (expect (true? (:is-gap (held))))
      (state/dispatch [:app-view-loaded whole])
      (expect (= {:view whole :is-gap false} (select-keys (held) [:view :is-gap])))
      ;; The summary at the seq of the copy carries the newer state of the app.
      (state/dispatch [:live-view-open (summary 4 :is-stale true :title "Renamed")])
      (expect (= (assoc whole
                   :is-stale true
                   :title "Renamed")
                 (:view (held))))
      (expect (false? (:is-gap (held))))
      (state/dispatch [:live-view-open (summary 4)])
      (expect (= whole (:view (held))))
      ;; A newer summary keeps the body as the base of the read.
      (state/dispatch [:live-view-open (summary 6 :is-stale true)])
      (expect (= (assoc whole :is-stale true) (:view (held))))
      (expect (= [true 6] ((juxt :is-gap :behind-seq) (held))))
      (state/dispatch [:app-view-loaded (assoc whole :seq 6)])
      (expect (false? (:is-gap (held))))
      ;; An older summary changes only the owner state.
      (state/dispatch [:live-view-open (summary 4 :is-stale true :title "Old")])
      (expect (= (assoc whole
                   :seq 6
                   :is-stale true)
                 (:view (held)))))))

;; Report: an open review read its whole view again after each change of one file.
(defdescribe
  app-read
  (let [{:strs [view frame patch]}
        (get (json/read-json (slurp (io/resource "vis-contract/fixtures/view.json"))) "delta_fold")

        held
        (view-model/live-view<-wire (assoc view
                                      "title" "Review"
                                      "placement" "attachment"))

        read-app
        @#'surface/read-app

        asked
        (fn [answer f]
          (let [calls (atom [])]
            (with-redefs [vis/gateway-app-read (fn [sid view-id since]
                                                 (swap! calls conj [sid view-id since])
                                                 (if since answer {"view" (assoc view "seq" 9)}))]
              [(f) @calls])))]

    (it "asks only for the frames after the held copy and folds their string edits"
        (let [[read calls] (asked {"patches" [frame]} #(read-app "s1" "live-one" held))]
          (expect (= (live/apply-patch held (view-model/live-patch<-wire patch)) read))
          (expect (= [["s1" "live-one" 1]] calls)))
        (let [[read calls] (asked {"patches" []} #(read-app nil "live-one" held))]
          (expect (= held read))
          (expect (= [[nil "live-one" 1]] calls)))
        ;; An app that did not change after it opened has seq 0.
        (let [[_ calls] (asked {"patches" []} #(read-app "s1" "live-one" (assoc held :seq 0)))]
          (expect (= [["s1" "live-one" 0]] calls))))
    (it "takes the whole view when a frame does not fit, or when no copy is held"
        (let [other
              (assoc-in held [:nodes 0 :patch] "other")

              [read calls]
              (asked {"patches" [frame]} #(read-app "s1" "live-one" other))]

          (expect (= 9 (:seq read)))
          (expect (= [["s1" "live-one" 1] ["s1" "live-one" nil]] calls)))
        (let [[read calls] (asked {"patches" [(assoc frame "first_seq" 4)]}
                                  #(read-app "s1" "live-one" held))]
          (expect (= 9 (:seq read)))
          (expect (= 2 (count calls))))
        (let [[read calls] (asked {"view" (assoc view "seq" 7)} #(read-app "s1" "live-one" held))]
          (expect (= 7 (:seq read)))
          (expect (= [["s1" "live-one" 1]] calls)))
        (let [[read calls] (asked nil #(read-app "s1" "live-one" nil))]
          (expect (= 9 (:seq read)))
          (expect (= [["s1" "live-one" nil]] calls)))
        ;; A summary has no body that the frames can change.
        (let [[read calls]
              (asked {"patches" []}
                     #(read-app "s1" "live-one" (assoc (dissoc held :nodes) :is-summary true)))]
          (expect (seq (:nodes read)))
          (expect (= [["s1" "live-one" nil]] calls))))
    ;; Network rig, run 3a: the gateway stores no `view.patch`. A change that came
    ;; while the event stream was away showed only after the next change.
    (it
      "reads the states of the session apps again when the event stream is ready"
      (let [sink
            (atom nil)

            dispatch
            state/dispatch

            newest
            (get patch "seq")]

        (reset! state/app-db
          {:session {:id "s1"} :tabs [] :live-views [] :app-views {} :render-version 0})
        (state/dispatch [:app-view-loaded held])
        (with-redefs [vis/add-title-listener!
                      (fn [& _])

                      vis/add-title-pending-listener!
                      (fn [& _])

                      vis/remove-title-listener!
                      (fn [& _])

                      vis/remove-title-pending-listener!
                      (fn [& _])

                      vis/refresh-session-model!
                      (fn [& _])

                      vis/gateway-current-seq
                      (constantly 0)

                      vis/gateway-input-views
                      (constantly [])

                      vis/gateway-live-views
                      (constantly [])

                      vis/gateway-mux-subscribe!
                      (fn [_ s _]
                        (reset! sink s)
                        (fn []))

                      vis/gateway-app-summaries
                      (fn [sid]
                        (when (= "s1" sid)
                          [(-> view
                               (dissoc "nodes")
                               (assoc "title" "Review"
                                      "placement" "attachment"
                                      "seq" newest
                                      "is_summary" true))]))

                      vis/worker-future
                      (fn [_ f]
                        (f))

                      state/tab-id-for-session
                      (constantly "tab")

                      state/dispatch
                      (fn [[id :as event]]
                        (when (= :app-view-loaded id) (dispatch event)))]

          (let [cleanup (#'screen/subscribe-session-live! "s1")]
            (try (@sink {"type" "subscription.ready" "session_id" "s1" "cursor" 0})
                 (finally (cleanup)))))
        (expect (= [true newest held]
                   ((juxt :is-gap :behind-seq :view)
                     (get-in @state/app-db [:app-views "live-one"]))))))
    ;; Report: the transcript shows an app, but only an open surface read its gap.
    (it "reads an app of the tab that is behind, without an open surface and one time"
        (let [held-in-tab
              #(get-in @state/app-db [:app-views "live-one"])

              behind!
              (fn []
                (reset! @#'surface/reads* {})
                (reset! state/app-db
                  {:session {:id "s1"} :tabs [] :live-views [] :app-views {} :render-version 0})
                (state/dispatch [:app-view-loaded held])
                (state/dispatch [:app-view-behind "live-one" (get patch "seq")]))]

          (with-redefs [vis/worker-future (fn [_ f]
                                            (f))]
            (behind!)
            (let [[_ calls] (asked {"patches" [frame]}
                                   #(do (surface/refresh-gaps! "s1") (surface/refresh-gaps! "s1")))]
              (expect (= [["s1" "live-one" 1]] calls))
              (expect (= {:view (live/apply-patch held (view-model/live-patch<-wire patch))
                          :is-gap false}
                         (held-in-tab))))
            ;; A read that fails does not run again at each tick.
            (behind!)
            (let [calls (atom 0)]
              (with-redefs [vis/gateway-app-read (fn [& _]
                                                   (swap! calls inc)
                                                   (throw (ex-info "gateway down" {})))]
                (surface/refresh-gaps! "s1")
                (surface/refresh-gaps! "s1"))
              (expect (= 1 @calls))
              (expect (true? (:is-gap (held-in-tab))))))))))

(def ^:private game
  "A small app with controls, a selectable table, an input and shortcuts."
  {:id "app-2"
   :title "Board"
   :seq 2
   :placement :attachment
   :nodes [{:id "go" :type :button :label "Go"}
           {:id "off" :type :button :label "Off" :is-disabled true}
           {:id "pick" :type :card :title "Pick me" :text "A card" :is-selectable true}
           {:id "jobs"
            :type :table
            :is-selectable true
            :columns [{:id "job" :label "Job"}]
            :rows [{:id "r1" :cells ["One"]} {:id "r2" :cells ["Two"]}]
            :selected-ids []
            :max-rows 500} {:id "ok" :type :checkbox :label "Confirm"}
           {:id "done" :type :button :label "Done" :action :submit}]
   :shortcuts [{:key "n" :label "New game" :action "new"} {:key "g" :label "Go" :node-id "go"}
               {:key "s" :label "Shuffle" :action "shuffle"}
               {:key "ArrowLeft" :label "Left" :action "left"}]})

(defn- game-harness
  [v]
  (let [{:keys [calls] :as h}
        (harness v)

        effects
        {:save! (fn [action _]
                  (swap! calls conj [:save action]))
         :send! (fn [action node-id]
                  (swap! calls conj [:send action node-id]))
         :activate! (fn [action]
                      (swap! calls conj [:activate action]))
         :select! (fn [action]
                    (swap! calls conj [:select action]))
          :change! (fn [action] (swap! calls conj [:change action]))
          :edit! (fn [node] (swap! calls conj [:edit (:id node)]))
         :shortcut! (fn [action s]
                      (swap! calls conj [:shortcut action (:action s)]))
         :reload! (fn []
                    (swap! calls conj [:reload]))
         :paste identity}]

    (assoc h
      :press (fn [st key]
               (let [geom (surface/measure st 100 40 v)]
                 (surface/reconcile (surface/handle-key st key geom effects) geom))))))

(defn- ch [c] (KeyStroke. (char c) false false))

(defn- select-row
  "Move the cursor down until `pred` holds for the selected row."
  [press st v pred]
  (loop [st
         st

         n
         0]

    (if (or (pred (row-at st v)) (> n 40)) st (recur (press st (k KeyType/ArrowDown)) (inc n)))))

(defdescribe
  app-shortcuts
  (it "posts an action shortcut and shows its label"
      (let [{:keys [press start calls]} (game-harness game)]
        (press start (ch \n))
        (expect (= [[:shortcut {:action :shortcut :key "n"} "new"]] @calls))))
  (it "presses the node of a node shortcut like Enter"
      (let [{:keys [press start calls]} (game-harness game)]
        (press start (ch \g))
        (expect (= [[:activate {:action :activate :node-id "go"}]] @calls))))
  (it "lets a shortcut take a surface key and keeps the verb behind C-x"
      (let [{:keys [press start calls]}
            (game-harness game)

            st
            (press start (ch \s))]

        (expect (= [[:shortcut {:action :shortcut :key "s"} "shuffle"]] @calls))
        (let [st (press st (k \x true))]
          (expect (true? (:prefix st)))
          (let [st (press st (ch \s))]
            (expect (nil? (:prefix st)))
            (expect (= :send (first (last @calls))))
            (expect (= :sending (get-in st [:send :state])))))))
  (it "takes a named key such as ArrowLeft"
      (let [{:keys [press start calls]} (game-harness game)]
        (press start (k KeyType/ArrowLeft))
        (expect (= [[:shortcut {:action :shortcut :key "ArrowLeft"} "left"]] @calls))))
  (it "never binds Escape, and C-g leaves the C-x prefix"
      (let [{:keys [press start calls]} (game-harness game)]
        (expect
          (= {::dlg/done nil}
             (surface/handle-key start (k KeyType/Escape) (surface/measure start 100 40 game) {})))
        (let [st (press (press start (k \x true)) (k \g true))]
          (expect (nil? (:prefix st)))
          (expect (empty? @calls)))))
  (it "ignores shortcuts while the editor has the focus"
      (let [{:keys [press start calls]}
            (game-harness (assoc view :shortcuts [{:key "a" :action "a"}]))

            st
            (press (down press start 1) (k KeyType/Enter))

            st
            (press st (ch \a))]

        (expect (some? (:editor st)))
        (expect (= "a" (input/input->text (get-in st [:editor :input]))))
        (expect (empty? @calls))))
  (it "has no shortcuts once the app is closed"
      (let [closed
            (assoc game :is-closed true)

            {:keys [press start calls]}
            (game-harness closed)]

        (press start (ch \n))
        (expect (empty? @calls))
        (expect (nil? (surface/shortcut-legend closed)))))
  (it "shows the legend and marks hidden verbs with C-x"
      (let [geom (surface/measure (surface/initial-state) 100 40 game)]
        (expect (= [["n" "New game"] ["g" "Go"] ["s" "Shuffle"] ["←" "Left"]] (:legend geom)))
        (expect (nil? (:legend (surface/measure (surface/initial-state) 100 40 view)))))))

(defdescribe
  stale-app
  (it "answers app keys and owner controls with one notice and calls nothing"
      (let [stale
            (assoc game :is-stale true)

            {:keys [press start calls]}
            (game-harness stale)]

        (expect (= surface/disconnected-notice (surface/status-line (press start (ch \n)))))
        (expect (= surface/disconnected-notice (surface/status-line (press start (ch \g)))))
        (expect (= surface/disconnected-notice
                   (surface/status-line (press start (k KeyType/ArrowLeft)))))
        (expect (= surface/disconnected-notice
                   (surface/status-line (press (down press start 0) (k KeyType/Enter)))))
        (expect (empty? @calls))))
  (it "keeps Send, reload and the C-x surface keys"
      (let [{:keys [press start calls]}
            (game-harness (assoc game :is-stale true))

            st
            (press (press start (k \x true)) (ch \s))]

        (expect (= :sending (get-in st [:send :state])))
        (expect (= :send (first (last @calls))))
        (press (press start (k \x true)) (ch \r))
        (expect (= [:reload] (last @calls)))))
  (it "says Disconnected once when the gateway refuses an action with 409"
      (expect (= surface/disconnected-notice
                 (surface/refusal-notice
                   {:is-accepted false :reason "disconnected" :http-status 409}))))
  (it "keeps one app for each view id and follows the owner state of each open"
      (reset! state/app-db
        {:session {:id "s1"} :tabs [] :live-views [] :app-views {} :render-version 0})
      (state/dispatch [:live-view-open (assoc game :session-id "s1")])
      (state/dispatch [:live-view-patch
                       {:view-id "app-2" :seq 3 :ops [{:op :set :node-id "pick" :text "Moved"}]}])
      (state/dispatch [:live-view-open
                       (assoc game
                         :session-id "s1"
                         :is-stale true)])
      (expect (= ["app-2"] (keys (:app-views @state/app-db))))
      (expect (= 3 (get-in @state/app-db [:app-views "app-2" :view :seq])))
      (expect (= :stale (model/status (get-in @state/app-db [:app-views "app-2" :view]))))
      (state/dispatch [:live-view-open
                       (assoc game
                         :session-id "s1"
                         :seq 3)])
      (expect (= :live (model/status (get-in @state/app-db [:app-views "app-2" :view]))))
      (state/dispatch [:live-view-open
                       (assoc game
                         :session-id "s1"
                         :seq 4
                         :is-stale true)])
      (expect (= 4 (get-in @state/app-db [:app-views "app-2" :view :seq])))
      (expect (= :stale (model/status (get-in @state/app-db [:app-views "app-2" :view]))))))

(defdescribe
  app-controls
  (it "presses buttons, cards and table rows and edits inputs with Enter"
      (let [unkeyed
            (assoc game :shortcuts own-keys)

            {:keys [press start calls]}
            (game-harness unkeyed)

            on
            (fn [pred]
              (let [st (select-row press start unkeyed pred)]
                (press st (k KeyType/Enter))))]

        (on #(= "go" (:node-id %)))
        (on #(= "pick" (:node-id %)))
        (on #(= "r2" (:item-id %)))
        (on #(= "ok" (:node-id %)))
        (expect (= [[:activate {:action :activate :node-id "go"}]
                    [:activate {:action :activate :node-id "pick"}]
                    [:select {:action :select :node-id "jobs" :item-ids ["r2"]}] [:edit "ok"]]
                   @calls))))
  (it "never stops on a disabled button"
      (let [{:keys [rows]} (surface/measure (surface/initial-state) 100 40 game)]
        (expect (not-any? #(and (= "off" (:node-id %)) (:is-selectable %)) rows))
        (expect (= 1 (count (filter #(and (= "ok" (:node-id %)) (:is-selectable %)) rows))))))
  (it "sends the input values with a submit button"
      (let [v
            (update game
                    :nodes
                    (fn [nodes]
                      (mapv #(if (= "ok" (:id %)) (assoc % :value true) %) nodes)))

            {:keys [press start calls]}
            (game-harness v)

            st
            (select-row press start v #(= "done" (:node-id %)))]

        (press st (k KeyType/Enter))
        (let [[kind action node-id] (last @calls)]
          (expect (= :send kind))
          (expect (= "done" node-id))
          (expect (= {"ok" true} (:values action)))))))

(defdescribe app-height
             (it "uses most of a small terminal, so the controls fit"
                 (let [g (surface/measure (surface/initial-state) 80 24 game)]
                   (expect (<= 12 (long (:list-h g))))
                   (expect (<= (long (:hint-row g)) 22))))
             (it "keeps a margin around the box in a large terminal"
                 (let [{:keys [bounds]} (surface/measure (surface/initial-state) 160 48 game)]
                   (expect (<= 2 (long (:top bounds))))
                   (expect (<= (long (:bottom bounds)) 45)))))

(def ^:private entry-row
  "An app with a field and buttons side by side, as a row group lays them out."
  {:id "app-3"
   :title "Todo"
   :seq 1
   :placement :attachment
   :shortcuts own-keys
   :nodes [{:id "entry"
            :type :flex
            :direction :row
            :children [{:id "new" :type :plaintext :name "new" :label "New task" :value ""}
                       {:id "add" :type :button :label "Add" :action :activate}]}
           {:id "acts"
            :type :flex
            :direction :row
            :children [{:id "undo" :type :button :label "Undo" :action :activate}
                       {:id "off" :type :button :label "Off" :is-disabled true}
                       {:id "clear" :type :button :label "Clear" :action :activate}]}]})

(def ^:private composed-app
  {:id "composed" :title "Work" :seq 1 :placement :attachment :header ["search" "new"]
   :nodes [{:id "content" :type :paragraph :text "App body"}
           {:id "search" :type :plaintext :label "Find an issue" :placeholder "Search issues"}
           {:id "new" :type :button :label "Create" :icon :plus}
           {:id "form" :type :dialog :label "New issue" :description "Enter the issue details" :is-open false
            :children [{:id "name" :type :plaintext :label "Summary" :value "Next task"}
                       {:id "save" :type :button :label "Save issue" :action :submit}]}]})

(defn- screen-lines [frame]
  (mapv #(apply str (map :ch %)) (:cells frame)))

(defdescribe composed-header
  (it "puts the compact search and icon button beside each other above the body"
      (doseq [width [80 120]]
        (let [rows (screen-lines (app-frame composed-app (surface/initial-state) width 30))]
          (expect (some #(re-find #"Search issues.*\[ \+ \]" %) rows))
          (expect (not-any? #(re-find #"Find an issue|Create" %) rows))))))

(defdescribe composed-dialog
  (it "shows the form only when it is open, above the app"
      (let [st (surface/initial-state)
            shut (screen-lines (app-frame composed-app st 100 30))
            open (screen-lines (app-frame (assoc-in composed-app [:nodes 3 :is-open] true) st 100 30))]
        (expect (not-any? #(re-find #"New issue|Summary|Save issue" %) shut))
        (doseq [text ["New issue" "Enter the issue details" "Summary" "Save issue"]]
          (expect (some #(str/includes? % text) open)))))
  (it "edits and submits the form, then sends its close action on Escape"
      (let [v (assoc-in composed-app [:nodes 3 :is-open] true)
            {:keys [calls press start]} (game-harness v)
            save (press start (k KeyType/ArrowDown))]
        (press start (k KeyType/Enter))
        (press save (k KeyType/Enter))
         (let [closed (press start (k KeyType/Escape))]
           (expect (not (contains? closed ::dlg/done)))
           (expect (not-any? #(str/includes? % "New issue")
                             (screen-lines (app-frame v closed 100 30)))))
        (expect (= [[:edit "name"] [:send :submit "save"]
                    [:activate {:action :activate :node-id "form"}]]
                   (mapv (fn [[kind action node-id :as call]]
                           (if (= :send kind) [kind (:action action) node-id] call)) @calls))))))

(defdescribe grouped-board
  (it "shows the columns across each lane and selects an issue in the second column"
      (let [v (view-model/live-view<-wire
                {:id "work" :title "Work" :placement "attachment"
                 :nodes [{:id "board" :type "board" :group_by "assignee" :is_selectable true
                          :columns [{:id "todo" :title "Todo"} {:id "done" :title "Done"}]
                          :people [{:id "ada" :name "Ada"}]
                          :issues [{:id "a" :column "todo" :title "Write a test" :assignee "ada"}
                                   {:id "b" :column "done" :title "Read a test" :assignee "ada"}
                                   {:id "c" :column "todo" :title "Review a test"}]}]})
            {:keys [press start calls]} (game-harness v)
            rows (screen-lines (app-frame v start 100 30))]
        (expect (some #(re-find #"Todo.*Done" %) rows))
        (expect (some #(re-find #"Write a test.*Read a test" %) rows))
        (expect (some #(str/includes? % "Unassigned") rows))
        (press (press start (k KeyType/ArrowDown)) (k KeyType/Enter))
        (expect (= [[:select {:action :select :node-id "board" :item-ids ["b"]}]] @calls)))))

(defdescribe side-by-side-controls
             (it "stops on each control of a row and skips a disabled one"
                 (let [{:keys [press start]}
                       (game-harness entry-row)

                       ids
                       (fn [st]
                         (let [rows (:rows (surface/measure st 100 40 entry-row))]
                           (some-> (surface/focused-entry (nth rows (:selected st)) (:cell st))
                                   :node-id)))]

                   (expect (= ["new" "add" "undo" "clear"]
                              (mapv #(ids (down press start %)) [0 1 2 3])))))
             (it "Enter edits the focused field and presses the focused button"
                 (let [{:keys [calls press start]} (game-harness entry-row)]
                   (press start (k KeyType/Enter))
                   (press (down press start 1) (k KeyType/Enter))
                   (expect (= [:edit "new"] (first @calls)))
                   (expect (= :activate (first (second @calls)))))))

(defdescribe
  shortcut-legend-merge
  ;; User report 2026-11-09: the review scope menu marks the choice that
  ;; holds, as T3 Code does, instead of a dead row.
  (it "marks the button of the choice that holds and keeps its key"
      (expect
        (= [["1" "This session"] ["2" "• Uncommitted"]]
           (surface/shortcut-legend
             {:id "r"
              :placement :attachment
              :nodes [{:id "session" :type :button :label "This session" :is-selected false}
                      {:id "uncommitted" :type :button :label "Uncommitted" :is-selected true}]
              :shortcuts [{:key "1" :node-id "session"} {:key "2" :node-id "uncommitted"}]}))))
  (it "joins the keys of one label, so a narrow legend keeps every action"
      (expect (= [["←/h" "Left"] ["→/l" "Right"] ["Del" "Delete"]]
                 (surface/shortcut-legend {:id "k"
                                           :placement :attachment
                                           :nodes []
                                           :shortcuts
                                           [{:key "ArrowLeft" :label "Left" :action "left"}
                                            {:key "ArrowRight" :label "Right" :action "right"}
                                            {:key "h" :label "Left" :action "left"}
                                            {:key "l" :label "Right" :action "right"}
                                            {:key "Delete" :label "Delete" :action "delete"}]})))))

;; Review scenarios S06/S07/S18/S22, Council #9489.
(defdescribe
  review-panel-controls
  (it "toggles the file list and test filter without losing the selected row"
      (let [st
            (surface/initial-state)

            geom
            {:view {} :rows [{:kind :file :path "a.py"}] :list-h 10}

            toggle
            #(surface/handle-key %1 (k %2 false) geom {})]

        (expect (:files? (toggle st \f)))
        (expect (false? (:files? (toggle (toggle st \f) \f))))
        (expect (:hide-tests? (toggle st \t)))))
  (it "collapses and expands all file headers on repeated presses"
      (let [geom
            {:view {} :rows [{:kind :file :path "a"} {:kind :file :path "b"}] :list-h 10}

            fold-all
            #(surface/handle-key % (k \a false) geom {})

            folded
            (fold-all (surface/initial-state))]

        (expect (= #{"a" "b"} (:folded folded)))
        (expect (empty? (:folded (fold-all folded))))))
  (it "copies the current code row path, not its shortened header"
      (let [copied
            (atom nil)

            geom
            {:view {} :rows [{:kind :add :path "src/a.py"}] :list-h 10}]

        (surface/handle-key (surface/initial-state) (k \c false) geom {:copy! #(reset! copied %)})
        (expect (= "src/a.py" @copied))))
  (it "saves a nonblank comment with Ctrl Enter once and keeps blank comments open"
      (let [calls
            (atom [])

            base
            (surface/open-editor (surface/initial-state)
                                 {:node-id "diff" :anchor {:path "x.clj" :side "new" :line 2}})

            geom
            {:view view :rows [] :list-h 10}

            enter
            (KeyStroke. KeyType/Enter true false)

            effects
            {:save! (fn [action _]
                      (swap! calls conj action))}

            blank
            (surface/handle-key base enter geom effects)

            typed
            (assoc-in base [:editor :input] (input/paste-text (input/empty-input) "Comment"))

            saving
            (surface/handle-key typed enter geom effects)]

        (expect (= base blank))
        (expect (= :saving (get-in saving [:save :state])))
        (surface/handle-key saving enter geom effects)
        (expect (= 1 (count @calls))))))

;; Review scenarios S11/S12, Council #9489: split comments must use the selected side.
(defdescribe split-comment-side
             (it "opens the editor on the old or new anchor chosen by the person"
                 (doseq [[side line] [[:left {:path "x.clj" :side "old" :line 2}]
                                      [:right {:path "x.clj" :side "new" :line 2}]]]
                   (let [row {:kind :split
                              :node-id "diff"
                              :anchor {:path "x.clj" :side "new" :line 2}
                              :left {:anchor {:path "x.clj" :side "old" :line 2}}
                              :right {:anchor {:path "x.clj" :side "new" :line 2}}}
                         geom {:view view :rows [row] :list-h 10}
                         state (assoc (surface/initial-state) :diff-side side)
                         edited (surface/handle-key state (k KeyType/Enter) geom {})]

                     (expect (= line (get-in edited [:editor :target :anchor])))))))

;; Review scenario S11, Council #9489: row selection must not capture clicks on chrome.
(defdescribe diff-mouse-target
             (it "maps only clicks inside the visible row area to a selectable row"
                 (let [geom {:bounds {:left 2 :inner-w 30}
                             :content-top 5
                             :list-h 3
                             :rows [{:kind :diff-head} {:kind :file :is-selectable true}
                                    {:kind :add :is-selectable true}
                                    {:kind :ctx :is-selectable true}]}]
                   (doseq [[scroll col row expected] [[0 4 5 nil] [0 4 6 1] [1 4 5 1] [0 2 6 nil]
                                                      [0 33 6 nil] [0 4 8 nil]]]
                     (expect (= expected (surface/mouse-target geom scroll col row)))))))

;; Review scenario S22, Council #9489: search must navigate without posting an app action.
(defdescribe
  code-search-keys
  (it
    "finds and cycles code matches, edits the query, and closes only the search"
    (let [geom
          {:view {}
           :rows [{:kind :file :text "Needle"} {:kind :add :text "needle one" :is-selectable true}
                  {:kind :ctx :text "plain" :is-selectable true}
                  {:kind :add :text "needle two" :is-selectable true}]
           :list-h 10}

          press
          #(surface/handle-key %1 %2 geom {})

          start
          (press (surface/initial-state) (k \/ false))

          found
          (reduce #(press %1 (k %2 false)) start "needle")

          next
          (press found (k KeyType/Enter))

          wrapped
          (press next (k KeyType/Enter))

          back
          (press found (k KeyType/Backspace))]

      (expect (= 1 (:selected found)))
      (expect (= 3 (:selected next)))
      (expect (= 1 (:selected wrapped)))
      (expect (= "needl" (get-in back [:search :text])))
      (expect (= "needle" (get-in (press found (k \a true)) [:search :text])))
      (expect (nil? (:search (press found (k KeyType/Escape)))))
      (expect (not (contains? (press found (k KeyType/Escape)) ::dlg/done))))))

;; Review scenarios S11/S22, Council #9489: clicks and Tab must select the correct diff side.
(defdescribe
  diff-mouse-keys
  (it
    "selects code on release, changes split sides with Tab, and folds a clicked header"
    (let [geom
          {:view view
           :bounds {:left 2 :inner-w 30}
           :content-top 5
           :list-h 3
           :rows [{:kind :file :path "x.clj" :is-selectable true}
                  {:kind :split
                   :node-id "diff"
                   :path "x.clj"
                   :is-selectable true
                   :left {:anchor {:path "x.clj" :side "old" :line 2}}
                   :right {:anchor {:path "x.clj" :side "new" :line 2}}}]}

          click
          (fn [type col row]
            (MouseAction. type 1 (TerminalPosition. col row)))

          press
          #(surface/handle-key %1 %2 geom {})

          start
          (surface/initial-state)

          left
          (press start (click MouseActionType/CLICK_RELEASE 4 6))

          right
          (press left (k KeyType/Tab))]

      (expect (= start (press start (click MouseActionType/CLICK_DOWN 4 6))))
      (expect (= 1 (:selected left)))
      (expect (= :left (:diff-side left)))
      (expect (= :right (:diff-side right)))
      (expect (= "new" (get-in (press right (k KeyType/Enter)) [:editor :target :anchor :side])))
      (expect (= #{"x.clj"} (:folded (press start (click MouseActionType/CLICK_RELEASE 4 5))))))))

;; User report: review search must support its terminal-safe shortcuts.
(defdescribe review-search-shortcuts-test
             (it "opens find with Ctrl F and returns to the review on Escape"
                 (let [geom
                       {:view {} :rows [] :list-h 10}

                       opened
                       (surface/handle-key (surface/initial-state) (k \f true) geom {})]

                   (expect (= "" (get-in opened [:search :text])))
                   (expect (nil? (:search
                                   (surface/handle-key opened (k KeyType/Escape) geom {}))))))
             (it "moves backward with Shift Enter and wraps to the last matching row"
                 (let [geom
                       {:view {}
                        :rows [{:kind :add :text "match"} {:kind :add :text "match"}
                               {:kind :add :text "other"} {:kind :add :text "match"}]
                        :list-h 10}

                       st
                       (assoc (surface/initial-state)
                         :selected 1
                         :search {:text "match"})

                       previous
                       #(surface/handle-key % (KeyStroke. KeyType/Enter false false true) geom {})

                       first-hit
                       (previous st)]

                   (expect (= 0 (:selected first-hit)))
                   (expect (= 3 (:selected (previous first-hit)))))))

(defdescribe
  shortcut-only-controls
  ;; User report: review buttons consume the TUI pane instead of using keys.
  (it "removes covered button rows, including nested buttons, but keeps other controls"
      (let [v
            {:id "keys"
             :nodes [{:id "group"
                      :type :flex
                      :children [{:id "toolbar"
                                  :type :flex
                                  :children [{:id "covered" :type :button :label "Covered"}]}
                                 {:id "visible" :type :button :label "Visible"}]}]
             :shortcuts [{:key "F" :node-id "covered"}]}

            rows
            (surface/surface-rows v 50 #{})]

        (expect (seq rows))
        (expect (not-any? #(= "covered" (:node-id %)) rows))
        (expect (some #(= "visible" (:node-id %)) rows))))
  ;; The review app moved its controls into the diff toolbar: covered keys must not
  ;; take the diff with them, and a choice with no key still needs a row.
  (it
    "keeps a diff whose toolbar keys cover every button and shows unkeyed toolbar buttons"
    (let [toolbar
          (fn [& extra]
            (assoc-in view
              [:nodes 0 :children]
              (into [{:id "refresh" :type :button :label "Refresh"}] extra)))

          covered
          (surface/surface-rows (assoc (toolbar) :shortcuts [{:key "r" :node-id "refresh"}]) 50 #{})

          menu
          (surface/surface-rows
            (assoc (toolbar {:id "scope"
                             :type :menu
                             :label "This session"
                             :children [{:id "checkpoint-2" :type :button :label "#2 edit"}]})
              :shortcuts [{:key "r" :node-id "refresh"}])
            50
            #{})]

      (expect (some #(= :diff-head (:kind %)) covered))
      (expect (not-any? #(= "refresh" (:node-id %)) covered))
      (expect (some #(= :diff-head (:kind %)) menu))
      (expect (some #(= "scope" (get-in % [:entry :node-id] (:node-id %))) menu))))
  (it "shows the app header before its body"
      (let [v {:id "v" :nodes [{:id "notes" :type :paragraph :text "Body"}
                               {:id "live" :type :status :text "Live"}
                               {:id "go" :type :button :label "Go"}]
               :header ["go" "live"]}
            rows (surface/surface-rows v 80 #{})]
        (expect (= ["go" "live"] (mapv :node-id (get-in (first rows) [:entry :cells]))))
        (expect (some #(= "notes" (:node-id %)) (rest rows)))))

  ;; User request: the state shows as one dot, green or red, as in a chat app.
  (it "shows the connection as a green or red dot in the title, without a word"
      (let [v {:id "v" :title "T" :nodes [{:id "go" :type :button :label "Go"}]}
            rgb (fn [^com.googlecode.lanterna.TextColor c] [(.getRed c) (.getGreen c) (.getBlue c)])
            dots (fn [v]
                   (let [cells (:cells (app-frame v (surface/initial-state) 80 24))]
                     {:text (apply str (map :ch (apply concat cells)))
                      :fg (keep #(when (= "●" (str (:ch %))) (:fg %)) (apply concat cells))}))]
        (expect (= [(rgb t/status-ok)] (:fg (dots v))))
        (expect (= [(rgb t/status-bad)] (:fg (dots (assoc v :is-stale true)))))
        (expect (not (str/includes? (:text (dots (assoc v :is-stale true))) "Disconnected")))))
  (it "uses the current button label and leaves disabled keys out of the legend"
      (let [v (assoc game :shortcuts [{:key "g" :node-id "go"} {:key "o" :node-id "off"}])]
        (expect (= [["g" "Go"]] (surface/shortcut-legend v)))
        (expect (= [["g" "Continue"]]
                   (surface/shortcut-legend (assoc-in v [:nodes 0 :label] "Continue"))))
        (let [{:keys [press start calls]} (game-harness v)
              st (press start (ch \o))]

          (expect (empty? @calls))
          (expect (seq (:notice st))))))
  (it "keeps every action reachable when the app has more keys than the pane has rows"
      (let [v
            (assoc game
              :shortcuts (mapv (fn [c]
                                 {:key (str c) :action (str c) :label (str "Long action label " c)})
                               "abcdefghijkl"))

            {:keys [press start calls]}
            (game-harness v)

            opened
            (press start (ch \?))

            selected
            (assoc-in opened [:key-menu :menu-state :selected] 11)

            result
            (press selected (k KeyType/Enter))]

        (expect (nil? (:key-menu result)))
        (expect (= [[:shortcut {:action :shortcut :key "l"} "l"]] @calls)))))

;; User report, Council #9635: table focus painted over the rails and past the table.
(defdescribe
  table-focus-bounds
  (it
    "keeps all rails and outside cells unchanged while highlighting the choice mark"
    (doseq [nested? [false true]]
      (let [table (nth (:nodes game) 3)
            node (if nested? {:id "layout" :type :flex :direction :row :children [table]} table)
            v (assoc game :nodes [node])
            row (first (filter #(or (= :trow (get-in % [:entry :kind]))
                                    (= :trow (get-in % [:parts 0 :entry :kind])))
                               (surface/surface-rows v 39 #{})))
            paint
            (fn [selected?]
              (binding [interactions/hit-map (interactions/create-hit-map)]
                (let [capture
                      (cap/capture!
                        {:cols 60
                         :rows 5
                         :paint!
                         (fn [{:keys [g]}]
                           (.beginFrame interactions/hit-map)
                           (#'surface/paint-plan-row! g 4 2 42 "app" selected? (when nested? 0) row)
                           (.commitFrame interactions/hit-map))})
                      targets (.current interactions/hit-map)]

                  (expect (nil? (:error capture)))
                  (expect (seq targets))
                  (expect (every? #(= "app" (:view-id %)) targets))
                  (get-in capture [:frames 0 2]))))
            plain (paint false)
            focused (paint true)
            rails (keep-indexed #(when (= "│" (:ch %2)) %1) plain)
            mark (first (keep-indexed #(when (= "○" (:ch %2)) %1) plain))]

        (expect (<= 2 (count rails)))
        (expect (some? mark))
        (doseq [x (concat rails (range 0 (first rails)) (range (inc (last rails)) 60))]
          (expect (= (nth plain x) (nth focused x))))
        (expect (not= (:bg (nth plain mark)) (:bg (nth focused mark))))))))

;; User report, Council #9635: review sections ran together and legends consumed the pane.
(defdescribe app-section-spacing
             (it "puts one gap between top-level nodes but none inside a table or diff"
                 (let [v
                       (assoc view
                         :submissions []
                         :nodes [{:id "title" :type :heading :level 1 :text "Review"}
                                 (nth (:nodes game) 3) (first (:nodes view))])

                       rows
                       (surface/surface-rows v 60 #{})

                       gaps
                       (keep-indexed #(when (= :section-gap (:kind %2)) %1) rows)]

                   (expect (= 2 (count gaps)))
                   (expect (not= :section-gap (:kind (first rows))))
                   (expect (not= :section-gap (:kind (last rows))))
                   (expect (not-any? #(= :diff-tools (:kind %)) rows)))))

(defdescribe app-key-menu
             (it "opens from ? or C-h, lists app actions first, and runs a menu key"
                 (let [{:keys [press start calls]}
                       (game-harness game)

                       opened
                       (press start (ch \?))

                       ctrl
                       (press start (k \h true))

                       chosen
                       (press opened (ch \g))]

                   (expect (seq (get-in opened [:key-menu :items])))
                   (expect (= (get-in opened [:key-menu :items]) (get-in ctrl [:key-menu :items])))
                   (expect (= "New game" (get-in opened [:key-menu :items 0 :label])))
                   (expect (nil? (:key-menu chosen)))
                   (expect (= [[:activate {:action :activate :node-id "go"}]] @calls))))
             (it "uses Enter on the selected action and closes on ? without running anything"
                 (let [{:keys [press start calls]}
                       (game-harness game)

                       opened
                       (press start (ch \?))

                       closed
                       (press opened (ch \?))

                       selected
                       (press opened (k KeyType/Enter))]

                   (expect (nil? (:key-menu closed)))
                   (expect (nil? (:key-menu selected)))
                   (expect (= [[:shortcut {:action :shortcut :key "n"} "new"]] @calls))))
             (it "keeps app ? and hidden surface keys available with C-x"
                 (let [v
                       (assoc game
                         :nodes (into (:nodes game) (:nodes view))
                         :shortcuts [{:key "?" :node-id "go"} {:key "w" :node-id "go"}])

                       {:keys [press start calls]}
                       (game-harness v)

                       app
                       (press start (ch \?))

                       opened
                       (press (press start (k \x true)) (ch \?))

                       hidden
                       (press (press opened (k \x true)) (ch \w))]

                   (expect (= 1 (count @calls)))
                   (expect (nil? (:key-menu app)))
                   (expect (seq (get-in opened [:key-menu :items])))
                   (expect (nil? (:key-menu hidden)))
                   (expect (false? (:wrap? hidden)))))
             (it "runs diff toggles from the menu and exposes their current state"
                 (let [{:keys [press start]}
                       (harness view)

                       opened
                       (press start (ch \?))

                       wrapped
                       (press opened (ch \w))

                       again
                       (press wrapped (ch \?))]

                   (expect (false? (:wrap? wrapped)))
                   (expect (some #(= "Wrap: off" (:label %)) (get-in again [:key-menu :items])))
                   (expect (empty? (:legend-lines (surface/measure start 100 40 game)))))))

;; Council #9635 CUA: macOS reports uppercase app keys with the Shift modifier.
(defdescribe app-menu-shifted-shortcut
             (it "runs an uppercase producer key from a real shifted keystroke"
                 (let [v
                       (assoc game :shortcuts [{:key "F" :node-id "go"}])

                       {:keys [press start calls]}
                       (game-harness v)

                       opened
                       (press start (ch \?))

                       result
                       (press opened (KeyStroke. (Character/valueOf \F) false false true))]

                   (expect (nil? (:key-menu result)))
                   (expect (= [[:activate {:action :activate :node-id "go"}]] @calls)))))

(defdescribe app-menu-navigation
             (it "moves the menu cursor without moving the review cursor, then runs that row"
                 (let [{:keys [press start calls]}
                       (game-harness game)

                       opened
                       (press start (ch \?))

                       next
                       (press opened (k KeyType/ArrowDown))

                       done
                       (press next (k KeyType/Enter))]

                   (expect (= (:selected start) (:selected next)))
                   (expect (= 1 (get-in next [:key-menu :menu-state :selected])))
                   (expect (nil? (:key-menu done)))
                   (expect (= [[:activate {:action :activate :node-id "go"}]] @calls)))))

(defdescribe app-menu-reserved-keys
             (it "keeps Enter for the selected menu row when the app also binds Enter"
                 (let [v
                       (assoc game
                         :shortcuts [{:key "n" :action "new" :label "New"}
                                     {:key "Enter" :node-id "go"}])

                       {:keys [press start calls]}
                       (game-harness v)

                       done
                       (press (press start (ch \?)) (k KeyType/Enter))]

                   (expect (nil? (:key-menu done)))
                   (expect (= [[:shortcut {:action :shortcut :key "n"} "new"]] @calls))))
             (it "opens search with C-f from the key menu"
                 (let [{:keys [press start]}
                       (harness view)

                       done
                       (press (press start (ch \?)) (k \f true))]

                   (expect (nil? (:key-menu done)))
                   (expect (= "" (get-in done [:search :text])))))
             (it "hides the pane with its advertised C-x b p chord while the menu is open"
                 (let [state
                       (surface/initial-state)

                       geom
                       (surface/pane-measure state 60 0 60 40 view)

                       hidden
                       (atom false)

                       effects
                       {:hide! #(reset! hidden true)}

                       step
                       (fn [st key]
                         (surface/handle-key st key geom effects))

                       done
                       (-> state
                           (step (ch \?))
                           (step (k \x true))
                           (step (ch \b))
                           (step (ch \p)))]

                   (expect @hidden)
                   (expect (nil? (:key-menu done))))))

;; Council #9635 CUA: the live component lacked Hide even though the key driver had it.
(defdescribe app-component-menu-hide
             (it "hides the actual retained pane when its component runs the menu chord"
                 (let [saved @state/app-db]
                   (try (reset! state/app-db (assoc saved
                                               :live-panel {:open? true
                                                            :focused? true
                                                            :app {:session-id "A" :view-id "v"}}))
                        (with-redefs-fn {#'surface/read-view! (fn [& _])}
                          (fn []
                            (let [component (surface/component "A" "v" view)
                                  initial (:init component)
                                  geom ((:measure-pane component) initial 60 0 60 40)
                                  step (fn [st key]
                                         ((:on-key component) st key geom))
                                  result (-> initial
                                             (step (ch \?))
                                             (step (k \x true))
                                             (step (ch \b))
                                             (step (ch \p)))]

                              (expect (nil? (:key-menu result)))
                              (expect (false? (get-in @state/app-db [:live-panel :open?])))
                              (expect (= {:session-id "A" :view-id "v"}
                                         (get-in @state/app-db [:live-panel :app]))))))
                        (finally (reset! state/app-db saved))))))

;; User report 2026-11-29: the review app said Disconnected after a gateway restart, until
;; a turn ran. The open of such an app must start its owner.
(defdescribe
  away-app
  (it
    "sends one connect when a person opens the app, and reads the app that came back"
    (let [saved
          @state/app-db

          away
          (assoc game
            :is-stale true
            :is-reconnectable true)

          calls
          (atom [])

          reads
          (atom [])

          answer
          (atom {:is-accepted false :reason "disconnected"})

          open!
          (fn []
            (let [component
                  (surface/component "s1" (:id away) away)

                  geom
                  #((:measure-pane component) (:init component) 60 0 60 40)]

              (geom)
              (model/status (:view (geom)))))]

      (try (reset! state/app-db (assoc saved :app-views {(:id away) {:view away}}))
           (with-redefs [vis/worker-future
                         (fn [_ f]
                           (f))

                         vis/gateway-app-action!
                         (fn [sid view-id action]
                           (swap! calls conj [sid view-id action])
                           @answer)]

             (with-redefs-fn {#'surface/read-view! (fn [_ _ _ & [whole?]]
                                                     (swap! reads conj whole?))}
               (fn []
                 ;; A refusal leaves the app disconnected, and nothing sends again.
                 (expect (= :stale (open!)))
                 (expect (= [["s1" (:id away) {:action :connect}]] @calls))
                 (reset! answer {:is-accepted true})
                 (reset! reads [])
                 (expect (= :away (open!)))
                 (expect (= 2 (count @calls)))
                 (expect (= [nil true] @reads)))))
           (finally (reset! state/app-db saved))))))

;; User report, Council #9648: the app menu crossed the pane and inherited focus ink.
(defdescribe
  app-menu-paint-bounds
  (it
    "keeps pane menus inside their area with uniform focus and unchanged outside cells"
    (doseq [theme
            ["blockether-light" "blockether-dark"]

            width
            [60 32]]

      (t/apply-theme! theme)
      (let [left
            (- 120 width)

            initial
            (surface/initial-state)

            geom
            (surface/pane-measure initial left 3 width 40 game)

            opened
            (surface/handle-key initial (ch \?) geom {})

            layout
            (#'surface/menu-layout opened geom)

            [ox oy]
            (get layout :origin [0 0])

            bounds
            (get-in layout [:geom :bounds])

            l
            (+ ox (:left bounds))

            r
            (+ ox (:right bounds))

            top
            (+ oy (:top bounds))

            bottom
            (+ oy (:bottom bounds))

            paint
            (fn [st]
              (cap/capture!
                {:cols 120
                 :rows 46
                 :paint! (fn [{:keys [g screen]}]
                           (p/set-colors! g t/dialog-fg t/dialog-bg)
                           (p/fill-rect! g 0 0 120 46)
                           (p/enable! g p/REVERSE)
                           (#'surface/paint! g st geom)
                           (with-redefs-fn {#'screen/focused-area (constantly :panel)
                                            #'screen/focus-areas (constantly [:chat :panel])}
                             #(#'screen/fade-unfocused-areas!
                                screen
                                {}
                                120
                                46
                                0
                                {:col left :row 3 :width width :height 40})))}))

            plain
            (paint initial)

            menu
            (paint opened)

            before
            (first (:frames plain))

            after
            (first (:frames menu))

            title
            (inc top)

            selected
            (+ oy (get-in layout [:geom :list-top]))]

        (expect (nil? (:error plain)))
        (expect (nil? (:error menu)))
        (expect (<= left l r 119))
        (expect (<= 3 top bottom 42))
        (expect (< l r))
        (expect (re-find #"App keys" (apply str (map :ch (nth after title)))))
        (expect (some #(not= " " (:ch %)) (subvec (nth after selected) (inc l) r)))
        (doseq [[y end] [[title (- r 3)] [selected r]]]
          (expect (= 1 (count (set (map :bg (subvec (nth after y) (inc l) end)))))))
        (expect (= [(.getRed ^TextColor$RGB t/dialog-shadow)
                    (.getGreen ^TextColor$RGB t/dialog-shadow)
                    (.getBlue ^TextColor$RGB t/dialog-shadow)]
                   (:bg (get-in after [(inc bottom) r]))))
        (doseq [y
                (range 46)

                x
                (range 120)

                :when (not (and (<= l x (min 119 (+ r 2))) (<= top y (min 45 (inc bottom)))))]

          (expect (= (get-in before [y x]) (get-in after [y x]))))))))

(def ^:private workflows
  "An app that declares no shortcut: a strip of tabs, a field and three actions."
  {:id "wf"
   :title "Workflows"
   :seq 1
   :placement :attachment
   :nodes [{:id "tabs"
            :type :flex
            :direction :row :is-wrapping true
            :children [{:id "tab-run" :type :button :label "Run" :is-selected true}
                       {:id "tab-history" :type :button :label "History" :is-selected false}]}
           {:id "name" :type :plaintext :label "Name"}
           {:id "actions"
            :type :flex
            :direction :row :is-wrapping true
            :children [{:id "new" :type :button :label "New run"}
                       {:id "reset" :type :button :label "Reset"}
                       {:id "delete" :type :button :label "Delete" :is-disabled true}]}]})

(defn- button-ids
  "The node id of each button that `rows` show, also in a row of side-by-side cells."
  [rows]
  (for [row
        rows

        entry
        (or (get-in row [:entry :cells]) [(:entry row)])

        :when (= :button (:kind entry))]

    (:node-id entry)))

(defdescribe
  automatic-keys
  ;; User report 2026-11-14: in the terminal, keys are better than buttons. An app
  ;; with no shortcut of its own showed each button as a row.
  (it "gives each button of an app without shortcuts a key"
      (let [geom
            (surface/measure (surface/initial-state) 100 40 workflows)

            {:keys [press start calls]}
            (game-harness workflows)]

        ;; `r` is the surface key of Reload, so Reset takes the next free letter.
        (expect (= [["R" "• Run"] ["h" "History"] ["n" "New run"] ["e" "Reset"]] (:legend geom)))
        ;; A disabled button has no key: it keeps its row and shows that it exists.
        (expect (= ["delete"] (button-ids (:rows geom))))
        (press start (ch \h))
        (press start (ch \n))
        (expect (= [[:activate {:action :activate :node-id "tab-history"}]
                    [:activate {:action :activate :node-id "new"}]]
                   @calls))))
  (it "names the keys in the hint bar, with the tab that holds"
      (let [st
            (surface/initial-state)

            geom
            (surface/measure st 100 30 workflows)

            shot
            (cap/capture! {:cols 100
                           :rows 30
                           :paint! (fn [{:keys [g]}]
                                     (p/set-colors! g t/dialog-fg t/dialog-bg)
                                     (#'surface/paint! g st geom))})

            lines
            (map #(apply str (map :ch %)) (first (:frames shot)))]

        (expect (nil? (:error shot)))
        ;; The buttons have no row, so only the bar names their keys.
        (expect (some #(re-find #"R • Run  ·  h History  ·  n New run  ·  e Reset" %) lines))))
  (it "keeps the keys of the surface: Reload still answers r"
      (let [{:keys [press start calls]} (game-harness workflows)]
        (press start (ch \r))
        (expect (= [[:reload]] @calls))))
  (it "gives no key to an app that is closed"
      (let [geom (surface/measure (surface/initial-state) 100 40 (assoc workflows :is-closed true))]
        (expect (nil? (:legend geom)))
        (expect (= 5 (count (button-ids (:rows geom))))))))

(defdescribe composed-focus-and-section
  (it "paints aligned and nested controls once, in their layout slots"
      (doseq [w [80 120]
              props [{:direction :row :justify :center}
                     {:direction :row :justify :end}
                     {:direction :column :align :center}
                     {:direction :column :align :end}]]
        (let [view {:id "layout" :placement :attachment
                    :nodes [(merge {:id "layout" :type :flex
                                    :children [{:id "outer" :type :button :label "Outside"}
                                               {:id "section" :type :section :label "Verification"
                                                :children [{:id "inner" :type :button :label "Inside"}]}]}
                                   props)]}
              rows (screen-lines (app-frame view (surface/initial-state) w 26))
              text (str/join "\n" rows)]
          (expect (= 1 (count (re-seq #"Outside" text))))
          (expect (= 1 (count (re-seq #"Inside" text))))
          (expect (some #(re-find #"┌.*Verification.*┐" %) rows))))))

(defdescribe composed-thread
  (it "paints the thread heading once, beside its sibling in a row"
      (doseq [w [80 120]]
        (let [view {:id "v" :placement :attachment
                    :shortcuts [{:key "z" :label "Other" :action "other"}]
                    :nodes [{:id "layout" :type :flex :direction :row :justify :end :gap :lg
                             :children [{:id "before" :type :button :label "Before thread"}
                                        {:id "notes" :type :thread :label "Composition thread"}]}]}
              rows (screen-lines (app-frame view (surface/initial-state) w 28))]
          (expect (some #(re-find #"Before thread.*Composition thread" %) rows))
          (expect (= 1 (count (filter #(str/includes? % "Composition thread") rows))))))))

(defdescribe stretched-section
  (it "puts the bottoms of short and tall section frames on the same row"
      (doseq [w [80 120]]
        (let [view {:id "v" :placement :attachment
                    :nodes [{:id "row" :type :flex :direction :row :align :stretch
                             :children [{:id "short" :type :section :label "Short"
                                         :children [{:id "one" :type :paragraph :text "One"}]}
                                        {:id "tall" :type :section :label "Tall"
                                         :children [{:id "two" :type :paragraph :text "Two"}
                                                    {:id "three" :type :paragraph :text "Three"}]}]}]}
              rows (screen-lines (app-frame view (surface/initial-state) w 28))]
          (expect (some #(re-find #"└.*┘.*└.*┘" %) rows))))))

(defdescribe wide-toggle
  (it "keeps the focused segment visible and selects the clicked option"
      (let [options (mapv (fn [n] {:value (str n) :label (str "Option " n " long label")}) (range 1 9))
            view {:id "v" :placement :attachment
                  :nodes [{:id "scope" :type :toggle :name "scope" :label "Scope" :options options}]}
            {:keys [press start calls]} (game-harness view)
            last-option (nth (iterate #(press % (k KeyType/ArrowRight)) start) 7)
            rendered (fn [w] (screen-lines (app-frame view last-option w 40)))
            frame (app-frame view last-option 100 40)
            line (first (keep-indexed #(when (str/includes? %2 "Option 8 long label") %1) (screen-lines frame)))
            row (when line (nth (screen-lines frame) line))]
        (doseq [w [80 120]]
          (expect (some #(str/includes? % "Option 8 long label") (rendered w))))
        (expect (some? row))
        (when row
          (press last-option (MouseAction. MouseActionType/CLICK_RELEASE 1
                                           (TerminalPosition. (str/index-of row "Option 8") line))))
        (expect (= [[:change {:action :change :node-id "scope" :value "8"}]] @calls)))))

(defdescribe dense-composition
  (it "keeps every control reachable when a terminal row is overfull"
      (doseq [w [80 120]]
        (let [buttons (mapv (fn [n] {:id (str "b" n) :type :button :label (str "Button " n)}) (range 24))
              view {:id "dense" :placement :attachment :shortcuts [{:key "z" :label "Other" :action "other"}]
                    :nodes [{:id "actions" :type :flex :direction :row :gap :lg :children buttons}]}
              rows (:rows (surface/measure (surface/initial-state) w 45 view))]
          (expect (= (set (map :id buttons))
                     (into #{} (mapcat (fn [row] (map #(get-in % [:entry :node-id]) (:parts row)))) rows)))
          (expect (some #(= :row-overflow (get-in % [:entry :kind])) rows))))))

(defdescribe composed-controls
  (it "opens a submenu, skips disabled items, and sends the chosen action"
      (let [v {:id "v" :placement :attachment :header ["tools"]
               :nodes [{:id "tools" :type :menu :label "Tools"
                        :children [{:id "off" :type :button :label "Disabled" :is-disabled true}
                                   {:id "sep" :type :divider}
                                   {:id "more" :type :menu :label "More"
                                    :children [{:id "go" :type :button :label "Run"}]}]}]}
             {:keys [press start calls]} (game-harness v)
             opened (press start (k KeyType/Enter))
             submenu (press opened (k KeyType/ArrowRight))
             chosen (press submenu (k KeyType/Enter))]
         (expect (nil? (:node-menu chosen)))
         (expect (= [[:activate {:action :activate :node-id "go"}]] @calls))))
  (it "activates the item that the person clicks in the popup"
      (let [v {:id "v" :placement :attachment
               :nodes [{:id "tools" :type :menu :label "Tools"
                        :children [{:id "first" :type :button :label "First"}
                                   {:id "second" :type :button :label "Second"}]}]}
            {:keys [press start calls]} (game-harness v)
            opened (press start (k KeyType/Enter))
            click (MouseAction. MouseActionType/CLICK_RELEASE 1 (TerminalPosition. 50 20))
            done (press opened click)]
        (expect (= [[:activate {:action :activate :node-id "second"}]] @calls))
        (expect (nil? (:node-menu done)))))
  (it "moves the segmented cursor without a change and selects with Space"
      (let [v {:id "v" :placement :attachment
               :nodes [{:id "scope" :type :toggle :name "scope" :label "Scope"
                        :options [{:label "Work" :value "work"} {:label "Stage" :value "stage"}]
                        :is-multiple true :value ["work"]}]}
            {:keys [press start calls]} (game-harness v)
            st (press start (k KeyType/ArrowRight))]
        (expect (empty? @calls))
        (let [both (press st (ch \space))]
          (press (press both (k KeyType/ArrowLeft)) (ch \space)))
        (expect (= [[:change {:action :change :node-id "scope" :value ["work" "stage"]}]
                    [:change {:action :change :node-id "scope" :value ["stage"]}]] @calls)))))
