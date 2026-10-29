(ns com.blockether.vis.tui.app-surface-test
  (:require [com.blockether.vis.tui.app-model :as model]
            [com.blockether.vis.tui.app-surface :as surface]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]))

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
         :activate! (fn [action]
                      (swap! calls conj [:activate action]))
         :reload! (fn []
                    (swap! calls conj [:reload]))
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
            (press start (k \> false))]

        (expect (= 8 (:hscroll st)))
        (expect (= 0 (:hscroll (press (press st (k \< false)) (k \< false)))))
        (expect (= "    1     1   9abc"
                   (surface/row-text {:kind :ctx :old 1 :new 1 :text "123456789abc"} 8 40))))))

(defdescribe
  thread-editor
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
      (let [{:keys [press start]}
            (harness view)

            stops
            (count (surface/selectable-indexes (:rows (surface/measure start 100 40 view))))

            st
            (press (down press start (dec stops)) (k KeyType/Enter))]

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
    ;; Issue #421: a wire view can keep the button action as a string.
    (it "sends only from a submit button, also when its action is a string"
        (doseq [[action expected] [[:submit [:send :submit "go"]] ["submit" [:send :submit "go"]]
                                   [nil [:activate :activate "go"]]]]
          (let [v (update view
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
      (expect (= :closed (model/status (get-in @state/app-db [:app-views "app-1" :view]))))))

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
         :edit! (fn [node]
                  (swap! calls conj [:edit (:id node)]))
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
      (let [{:keys [press start calls]}
            (game-harness game)

            on
            (fn [pred]
              (let [st (select-row press start game pred)]
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
   :nodes [{:id "entry"
            :type :group
            :direction :row
            :children [{:id "new" :type :plaintext :name "new" :label "New task" :value ""}
                       {:id "add" :type :button :label "Add" :action :activate}]}
           {:id "acts"
            :type :group
            :direction :row
            :children [{:id "undo" :type :button :label "Undo" :action :activate}
                       {:id "off" :type :button :label "Off" :is-disabled true}
                       {:id "clear" :type :button :label "Clear" :action :activate}]}]})

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

(defdescribe shortcut-legend-merge
             (it "joins the keys of one label, so a narrow legend keeps every action"
                 (expect (= [["←/h" "Left"] ["→/l" "Right"] ["Del" "Delete"]]
                            (surface/shortcut-legend
                              {:id "k"
                               :placement :attachment
                               :nodes []
                               :shortcuts [{:key "ArrowLeft" :label "Left" :action "left"}
                                           {:key "ArrowRight" :label "Right" :action "right"}
                                           {:key "h" :label "Left" :action "left"}
                                           {:key "l" :label "Right" :action "right"}
                                           {:key "Delete" :label "Delete" :action "delete"}]})))))
