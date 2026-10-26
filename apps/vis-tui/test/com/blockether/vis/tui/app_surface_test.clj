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
      (expect (= :saving (get-in st [:save :state])))
      (expect (= "Saving…" (surface/status-line st)))))
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
                (expect (= :conflict (:state (surface/saved {} {:reason "conflict"}))))
                (expect (= "Saved · state 5"
                           (surface/status-line {:save {:state :saved :seq 5}})))))
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
          (expect (= (nth keys 2) (get-in again [:send :key])))
          (expect (= "Offline: not sent, retry keeps the same key" (surface/status-line offline)))
          (expect (= "Sent · sub-2" (surface/status-line receipt)))))
    (it "lists the receipts of the view"
        (let [rows (surface/surface-rows view 80 #{})]
          (expect (= ["Send receipts" "Sent · delivered · sub-1"]
                     (mapv :text (filter #(#{:receipts-head :receipt} (:kind %)) rows))))))
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
                   (expect (empty? @calls))
                   (expect (= "Closed: read only" (surface/status-line st)))))
             (it "closes on Escape"
                 (let [{:keys [start]} (harness view)]
                   (expect (= {::dlg/done nil}
                              (surface/handle-key start
                                                  (k KeyType/Escape)
                                                  (surface/measure start 100 40 view)
                                                  {}))))))

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
