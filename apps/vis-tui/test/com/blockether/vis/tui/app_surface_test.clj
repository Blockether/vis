(ns com.blockether.vis.tui.app-surface-test
  (:require [com.blockether.vis.tui.app-model :as model]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.app-surface :as surface]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]
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
            (press (assoc start :wrap? false) (k \> false))]

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
      (let [unkeyed
            (dissoc game :shortcuts)

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

;; Real T3 S06/S07/S18/S22, Council #9489.
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

;; T3 S11/S12, Council #9489: split comments must use the selected side.
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

;; T3 S11, Council #9489: row selection must not capture clicks on chrome.
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

;; Real T3 S22, Council #9489: search must navigate without posting an app action.
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

;; Real T3 S11/S22, Council #9489: clicks and Tab must select the correct diff side.
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

;; User report: review search must support the terminal-safe T3 shortcuts.
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
                      :type :group
                      :children [{:id "toolbar"
                                  :type :group
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
                             :type :group
                             :label "This session"
                             :children [{:id "checkpoint-2" :type :button :label "#2 edit"}]})
              :shortcuts [{:key "r" :node-id "refresh"}])
            50
            #{})]

      (expect (some #(= :diff-head (:kind %)) covered))
      (expect (not-any? #(= "refresh" (:node-id %)) covered))
      (expect (some #(= :diff-head (:kind %)) menu))
      (expect (some #(= "checkpoint-2" (get-in % [:entry :node-id] (:node-id %))) menu))))
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
      (let [v (assoc game :nodes [(nth (:nodes game) 3)])
            table-row (first (filter #(= :trow (get-in % [:entry :kind]))
                                     (surface/surface-rows v 38 #{})))
            row (if nested?
                  (assoc table-row
                    :cells [0]
                    :entry {:kind :columns :cells [(:entry table-row)]})
                  table-row)
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
