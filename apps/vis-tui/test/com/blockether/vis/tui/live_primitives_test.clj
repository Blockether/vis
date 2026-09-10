(ns com.blockether.vis.tui.live-primitives-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.view :as spec]
            [com.blockether.vis.tui.capture :as capture]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.dialogs :as dialogs]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.view-materializer :as materializer]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.html HtmlTerminal]))

(defn fixture
  []
  (hi/live-view<-wire (json/read-json (slurp (io/resource
                                               "vis-contract/fixtures/live-primitives.json")))))

(defn- entries [pane] (tree-seq #(seq (:cells %)) :cells {:cells (lv/plan pane 80)}))

(deftest all-primitives-paint-plan-test
  (let [view
        (fixture)

        pane
        (lv/opened view)

        rows
        (entries pane)]

    (is (nil? (spec/live-view-error view)))
    (is (= 6 (count (set (map :node-id (filter #(= :heading (:kind %)) rows))))))
    (is (= 4 (count (filter #(= :spinner (:kind %)) rows))))
    (is (str/includes? (str/join "\n" (map :text rows)) "# <button> is literal text"))
    (is (lv/animating? [pane]))
    (is (= #{"refresh" "details" "table"} (set (map :node-id (lv/controls [pane])))))
    (is (not (lv/animating? [(lv/settled pane {:reason :completed})])))
    (is (not (lv/animating? [(lv/minimized pane)])))
    (is (str/includes? (materializer/->markdown view) "Heading level 6"))
    (let [markdown (materializer/->markdown view)]
      (is (= markdown (materializer/->markdown (:view (materializer/parse-markdown markdown))))))))

(deftest disclosures-retain-state-and-output-test
  ;; Regression #189: independent folds survive patches and nested parent folds.
  (let [view
        (fixture)

        pane
        (-> (lv/opened view)
            (lv/expanded "details")
            (lv/expanded "a"))

        pane
        (lv/patched
          pane
          {:view-id (:id view) :seq 1 :ops [{:op :append :node-id "a" :lines ["A updated"]}]})]

    (is (some #(= "A updated" (:text %)) (entries pane)))
    (is (not-any? #(= "B started" (:text %)) (entries pane)))
    (is (some #(= "A updated" (:text %))
              (entries (-> pane
                           (lv/expanded "details")
                           (lv/expanded "details")))))
    (let [closed
          (lv/expanded pane "a")

          updated
          (lv/patched
            closed
            {:view-id (:id view) :seq 2 :ops [{:op :append :node-id "a" :lines ["later"]}]})]

      (is (not-any? #(= :log (:kind %)) (entries updated)))
      (is (some #(= "later" (:text %)) (entries (lv/expanded updated "a")))))
    (let [receipt (lv/settled pane {:reason :completed})]
      (is (empty? (:disclosures receipt)))
      (is (not-any? #(= :log (:kind %)) (entries receipt)))
      (is (every? :is-disabled (filter #(= :button (:kind %)) (entries receipt)))))))

(deftest primitives-paint-real-terminal-test
  (doseq [cols [40 80 120]]
    (let [view (update (fixture)
                       :nodes
                       #(filterv (fn [n]
                                   (contains? #{"h1" "code" "spinners" "actions" "details" "links"}
                                              (:id n)))
                          %))
          pane (assoc (lv/opened view) :is-following false)
          captured (capture/capture!
                     {:cols cols
                      :rows 48
                      :paint!
                      (fn [{:keys [screen]}]
                        (.beginFrame interactions/hit-map)
                        (lv/paint! (.newTextGraphics ^TerminalScreen screen) cols 48 [pane] 1 3)
                        (.commitFrame interactions/hit-map)
                        (.refresh ^TerminalScreen screen)
                        (vec (.current interactions/hit-map)))})
          text (capture/frame-text (last (:frames captured)))]

      (is (str/includes? text "Heading level 1"))
      (is (str/includes? text "Refresh results"))
      (is (str/includes? text "Details"))
      (is (str/includes? text "F3"))
      (is (str/includes? text "Braille"))
      (is (str/includes? text "Documentation"))
      (is (some #(= :url (:kind %)) (:ret captured)))
      (is (some #(= :live-activate (:kind %)) (:ret captured)))
      (is (not-any? #(= "disabled" (:node-id %)) (:ret captured))))))

(defn write-review!
  "Render the shared fixture with the production terminal painter for browser review."
  [path cols]
  (let [rows
        100

        pane
        (assoc (lv/opened (fixture)) :is-following false)]

    (with-open [terminal
                (-> (HtmlTerminal/builder)
                    (.initialSize (TerminalSize. cols rows))
                    (.defaultForeground theme/text-fg)
                    (.defaultBackground theme/terminal-bg)
                    (.title "Live view primitives")
                    (.build))

                screen
                (TerminalScreen. terminal)]

      (.startScreen screen)
      (.setCursorPosition screen nil)
      (doto (.newTextGraphics screen)
        (.setBackgroundColor theme/terminal-bg)
        (.setForegroundColor theme/text-fg)
        (.fill \space))
      (.beginFrame interactions/hit-map)
      (lv/paint! (.newTextGraphics screen) cols rows [pane] 1 3)
      (.commitFrame interactions/hit-map)
      (.refresh screen)
      (.writeHtml terminal (.toPath (io/file path)) (int rows))
      path)))

(deftest button-control-routes-through-the-shared-gateway-action-test
  (let [pane
        (lv/opened (fixture))

        control
        (first (filter #(= "refresh" (:node-id %)) (lv/controls [pane])))

        called
        (promise)]

    (with-redefs [client/gateway-view-action! (fn [& args]
                                                (deliver called args)
                                                {:is-accepted true})]
      (is (true? (#'screen/activate-live-region! {:live-views [pane]} control)))
      (is (= [(get-in pane [:view :session-id]) (lv/view-id pane)
              {:action :activate :node-id "refresh"}]
             (deref called 3000 ::timeout))))))

(deftest human-record-retains-independent-disclosures-test
  ;; Regression #189: restore the full human snapshot through the archive reader.
  (let [raw
        (json/read-json (slurp (io/resource "vis-contract/fixtures/live-primitives.json")))

        source
        (str/join "\n"
                  (map json/write-json-str
                       [{:kind "open" :view raw}
                        {:kind "close"
                         :at 20
                         :result {:reason "completed"
                                  :is_completed true
                                  :view (select-keys raw ["title" "nodes"])}}]))

        pane
        (lv/recorded-pane source "fixture")

        open
        (-> pane
            (lv/expanded "details")
            (lv/expanded "a"))]

    (is (= #{"details"} (set (map :node-id (lv/controls [pane])))))
    (is (not-any? #(= :log (:kind %)) (entries pane)))
    (is (some #(= "A started" (:text %)) (entries open)))
    (is (not-any? #(= "B started" (:text %)) (entries open)))
    (is (every? :is-disabled (filter #(= :button (:kind %)) (entries open))))
    (is (not (lv/animating? [open])))))

(deftest logs-offer-read-only-search-controls-test
  (doseq [pane [(lv/opened (fixture))
                (lv/reopened (lv/settled (lv/opened (fixture)) {:reason :completed}))]]
    (let [opened (-> pane
                     (lv/expanded "details")
                     (lv/expanded "a"))
          search (filter #(= :live-log-search (:kind %)) (lv/controls [opened]))]

      (is (= ["a"] (mapv :node-id search)))
      (is (= "Search Build A logs" (:label (first search)))))))

(deftest log-search-renders-real-terminal-and-receipt-controls-test
  ;; Regression #189: search remains reachable in live output and reopened receipts.
  (doseq [cols
          [40 80 120]

          settled?
          [false true]]

    (let [view
          (update (fixture)
                  :nodes
                  #(filterv (fn [n]
                              (= "details" (:id n)))
                     %))

          pane
          (cond-> (lv/opened view)
            settled?
            (lv/settled {:reason :completed})

            settled?
            lv/reopened)

          pane
          (-> pane
              (lv/expanded "details")
              (lv/expanded "a")
              (assoc :is-following false))

          captured
          (capture/capture!
            {:cols cols
             :rows 24
             :paint! (fn [{:keys [screen]}]
                       (.beginFrame interactions/hit-map)
                       (lv/paint! (.newTextGraphics ^TerminalScreen screen) cols 24 [pane] 1 3)
                       (.commitFrame interactions/hit-map)
                       (.refresh ^TerminalScreen screen)
                       (vec (.current interactions/hit-map)))})]

      (is (nil? (:error captured)))
      (is (str/includes? (capture/frame-text captured) "Search Build A logs"))
      (is (some #(and (= :live-log-search (:kind %)) (= "a" (:node-id %))) (:ret captured))))))

(deftest log-search-dialog-renders-results-empty-error-and-full-lines-test
  ;; Exercise the real input/list/text dialogs, replacing only the retained-log read.
  (doseq [cols
          [40 80 120]

          [response keys expected]
          [[{:page {"from" 0
                    "total" 503
                    "matched" 2
                    "lines" ["ERROR [disk] cache write failed" "error retry completed"]
                    "line_numbers" [7 9]}} [:down :down :enter :esc :esc]
            ["7: ERROR" "Line 7" "failed"]]
           [{:page {"from" 0 "total" 503 "matched" 0 "lines" [] "line_numbers" []}} [:esc]
            ["0 matches / 503" "No matching lines."]]
           [{:error "Log unavailable"} [:esc] ["Could not read log."]]]]

    (let [requests
          (atom [])

          captured
          (with-redefs-fn {#'lv/read-log-page! (fn [_screen _sid _view _node from query]
                                                 (swap! requests conj [from query])
                                                 response)}
            #(capture/capture! {:cols cols
                                :rows 24
                                :keys (into [\e \r \r \o \r :enter] keys)
                                :paint! (fn [{:keys [screen]}]
                                          (lv/search-log! screen (lv/opened (fixture)) "a"))}))

          text
          (str/join "\n" (map capture/frame-text (:frames captured)))]

      (is (nil? (:error captured)))
      (is (= [[0 "error"]] @requests))
      (doseq [copy expected]
        (is (str/includes? text copy)))
      (is (every? #(and (= 24 (count %))
                        (every? (fn [row]
                                  (= cols (count row)))
                                %))
                  (:frames captured))))))

(deftest log-search-dialog-pages-refreshes-and-shows-full-lines-test
  (let [calls
        (atom [])

        choices
        (atom [:next :refresh :query :line nil])

        prompts
        (atom ["error" "[disk]"])

        read-line
        (atom nil)]

    (with-redefs-fn {(ns-resolve 'com.blockether.vis.tui.live-view 'read-log-page!)
                     (fn [_screen sid view-id node-id from query]
                       (swap! calls conj [sid view-id node-id from query])
                       {:page {"from" from
                               "total" 1000
                               "matched" 201
                               "lines" ["ERROR [disk] full line"]
                               "line_numbers" [42]}})
                     #'dialogs/text-input-dialog! (fn [& _]
                                                    (let [q (first @prompts)]
                                                      (swap! prompts rest)
                                                      q))
                     #'dialogs/list-dialog! (fn [_screen _title items _opts]
                                              (let [kind (first @choices)]
                                                (swap! choices rest)
                                                (first (filter #(= kind (:action %)) items))))
                     #'dialogs/text-view-dialog! (fn [_screen _title lines]
                                                   (reset! read-line lines))}
      #((ns-resolve 'com.blockether.vis.tui.live-view 'search-log!) nil (lv/opened (fixture)) "a"))
    (is (= [0 200 0 0] (mapv #(nth % 3) @calls)))
    (is (= ["error" "error" "error" "[disk]"] (mapv last @calls)))
    (is (= ["ERROR [disk] full line"] @read-line))))

(deftest log-search-loading-can-be-cancelled-test
  (let [started
        (promise)

        cancelled
        (promise)]

    (with-redefs [client/live-view-log (fn [& _]
                                         (deliver started true)
                                         (try (Thread/sleep 30000)
                                              (catch InterruptedException _
                                                (deliver cancelled true))))]
      (let [captured (capture/capture!
                       {:cols 40
                        :rows 16
                        :paint!
                        (fn [{:keys [screen terminal]}]
                          (let [escape
                                (future
                                  @started
                                  (.addInput
                                    ^com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
                                    terminal
                                    (capture/key-stroke :esc)))]
                            (try (#'lv/read-log-page! screen "sid" "view" "log" 0 "error")
                                 (finally (future-cancel escape)))))})]
        (is (nil? (:ret captured)))
        (is (str/includes? (capture/frame-text (last (:frames captured))) "Searching"))
        (is (= true (deref cancelled 1000 false)))))))

(deftest log-search-client-encodes-literal-query-test
  (let [called (atom nil)]
    (with-redefs-fn {#'client/send-json! (fn [& args]
                                           (reset! called args)
                                           {"matched" 0})}
      #(client/live-view-log "sid" "view" "a" 200 200 "[disk]&Ł"))
    (is (= ["GET"
            "/v1/sessions/sid/views/live/view/log/a?from=200&limit=200&query=%5Bdisk%5D%26%C5%81"]
           @called))))
