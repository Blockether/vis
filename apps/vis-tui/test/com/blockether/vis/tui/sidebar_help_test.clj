(ns com.blockether.vis.tui.sidebar-help-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.projects :as projects]
            [com.blockether.vis.tui.projects-test :as projects-test]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminal-image :as timg]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna.input DefaultKeyDecodingProfile InputDecoder KeyStroke]
           [java.io StringReader]))

(defn- help-frame
  "Capture the production help overlay with the requested sidebar focus."
  [db]
  (with-redefs [state/app-db
                (atom db)

                timg/images-protocol
                (constantly nil)]

    (cap/capture! {:cols 144
                   :rows 60
                   :paint! (fn [{:keys [screen]}]
                             (#'screen/render-frame! screen 144 60 db 1000))})))

(defdescribe
  sidebar-help-test
  (describe
    "sidebar help keys"
    (it "routes the terminal Ctrl-H byte to sidebar help, not Backspace"
        (with-open [reader (StringReader. "\b")]
          (let [decoder (doto (InputDecoder. reader) (.addProfile (DefaultKeyDecodingProfile.)))
                key (.getNextCharacter decoder true)]

            (expect (= (KeyStroke. \h true false) key))
            (expect (= [:help] (projects/key-action (projects-test/fixture-db) key))))))
    (it "opens help for both question-mark key forms"
        (doseq [key [(cap/key-stroke \?) (KeyStroke. \? false false true)]]
          (expect (= [:help] (projects/key-action (projects-test/fixture-db) key)))))
    (it "leaves chat focus, Backspace and unrelated modifiers unchanged"
        (let [db (projects-test/fixture-db)]
          (doseq [key [(KeyStroke. \h true false) (cap/key-stroke \?)]]
            (expect (nil? (projects/key-action (assoc-in db [:project-sidebar :focused?] false)
                                               key)))
            (expect (nil? (projects/key-action (assoc-in db [:project-sidebar :open?] false) key))))
          (expect (= [:noop] (projects/key-action db (cap/key-stroke :backspace))))
          (expect (= [:noop] (projects/key-action db (cap/key-stroke \h))))
          (expect (nil? (projects/key-action db (KeyStroke. \h true true))))
          (expect (nil? (projects/key-action db (KeyStroke. \? true false))))
          (expect (nil? (projects/key-action db (KeyStroke. \? false true))))))
    (it "keeps question marks editable in sidebar fields and Ctrl-H available"
        (doseq [[field action] [[:adding :adding] [:search :search-change]]]
          (let [db (assoc-in (projects-test/fixture-db)
                     [:project-sidebar field]
                     {:text "path" :cursor 4})
                [actual value] (projects/key-action db (cap/key-stroke \?))]

            (expect (= action actual))
            (expect (= "path?" (:text value)))
            (expect (= [:help] (projects/key-action db (KeyStroke. \h true false)))))))
    (it "allows either help key to dismiss help even over an inline field"
        (doseq [field
                [:adding :search]

                key
                [(KeyStroke. \h true false) (cap/key-stroke \?)]]

          (let [db (-> (projects-test/fixture-db)
                       (assoc :help-open? true)
                       (assoc-in [:project-sidebar field] {:text "path" :cursor 4}))]
            (expect (= [:help] (projects/key-action db key)))))))
  (describe
    "sidebar help dispatch and presentation"
    (it "allows help keys through the production overlay lock but blocks navigation"
        (let [db (assoc (projects-test/fixture-db) :help-open? true)]
          (expect (true? (#'screen/project-sidebar-locked? db 144)))
          (doseq [key [(KeyStroke. \h true false) (cap/key-stroke \?)]]
            (expect (true? (#'screen/project-sidebar-key-allowed? db 144 key)))
            (expect (false? (#'screen/project-sidebar-key-allowed?
                             (assoc-in db [:project-sidebar :focused?] false)
                             144
                             key))))
          (expect (false? (#'screen/project-sidebar-key-allowed? db 144 (cap/key-stroke :down))))))
    (it "toggles help without changing the draft, cursor or sidebar selection"
        (doseq [key [(KeyStroke. \h true false) (cap/key-stroke \?)]]
          (let [db (assoc (projects-test/fixture-db) :help-scroll 7)]
            (with-redefs [state/app-db (atom db)]
              (expect (true? (#'screen/project-sidebar-key! key nil nil nil nil nil)))
              (expect (true? (:help-open? @state/app-db)))
              (expect (= 0 (:help-scroll @state/app-db)))
              (expect (= (:input db) (:input @state/app-db)))
              (expect (= (:project-sidebar db) (:project-sidebar @state/app-db)))
              (expect (true? (#'screen/project-sidebar-key! key nil nil nil nil nil)))
              (expect (false? (:help-open? @state/app-db)))))))
    (it "opens the sidebar key list and runs the chosen command"
        (doseq [key [(KeyStroke. \h true false) (cap/key-stroke \?)]]
          (let [db (projects-test/fixture-db)
                menus (atom [])
                adds (atom 0)
                press! (fn [command]
                         (#'screen/project-sidebar-key!
                          key
                          nil
                          #(swap! adds inc)
                          nil
                          #(swap! menus conj %)
                          nil
                          nil
                          (constantly command)))]

            (with-redefs [state/app-db (atom db)]
              ;; Esc and the list's own help item close the list and change nothing.
              (doseq [command [nil {:id :help} {:id :rows}]]
                (expect (true? (press! command)))
                (expect (= db @state/app-db)))
              (expect (true? (press! {:id :menu :key \g})))
              (expect (= [:project-select] (mapv :kind @menus)))
              (expect (true? (press! {:id :back})))
              (expect (false? (get-in @state/app-db [:project-sidebar :focused?])))
              ;; The help keys act only while the rail has focus.
              (reset! state/app-db db)
              (expect (true? (press! {:id :hide})))
              (expect (false? (get-in @state/app-db [:project-sidebar :open?])))
              (expect (false? (boolean (:help-open? @state/app-db))))))))
    (it "keeps one help card and lists the sidebar section in it"
        (let [capture
              (help-frame (assoc (projects-test/fixture-db)
                            :help-open? true
                            :help-scroll 1000))

              text
              (cap/frame-text capture)]

          (expect (nil? (:error capture)))
          (expect (str/includes? text "Keyboard shortcuts"))
          (expect (str/includes? text "Project sidebar"))
          (expect (str/includes? text "List the sidebar keys"))
          (expect (not (str/includes? text "Sidebar help")))))
    (it "keeps the normal help card when the sidebar is not focused"
        (let [capture
              (help-frame (-> (projects-test/fixture-db)
                              (assoc :help-open? true)
                              (assoc-in [:project-sidebar :focused?] false)))

              text
              (cap/frame-text capture)]

          (expect (nil? (:error capture)))
          (expect (str/includes? text "Keyboard shortcuts"))
          (expect (str/includes? text "Cycle model"))))))
