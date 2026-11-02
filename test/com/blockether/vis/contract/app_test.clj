(ns com.blockether.vis.contract.app-test
  "Where a live app shows. The engine resolves it once, and the TUI and the
   Companion read the answer, so these rules decide what both clients show."
  (:require [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.contract.document :as document]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private setting-id-pattern
  (re-pattern (get-in (document/schema-document "toggle")
                      ["$defs" "contribution" "properties" "id" "pattern"])))

(defdescribe
  app-surfaces-test
  (describe
    "effective-surfaces"
    (it "gives the person's choice for one app over the app's declaration"
        (expect (= {:inline false :session "chat"}
                   (app/effective-surfaces {:extension "demo"
                                            :key "board"
                                            :surfaces {:inline true :session "sidebar"}}
                                           {"app_demo_board_inline" false
                                            "app_demo_board_session" "chat"}))))
    (it "turns a surface off for every app when its main setting is off"
        (expect (= {:inline false :session "off"}
                   (app/effective-surfaces {:extension "demo" :key "board"}
                                           {"apps_inline" false
                                            "apps_session" false
                                            "app_demo_board_inline" true
                                            "app_demo_board_session" "chat"}))))
    (it "keeps the declaration of an app that has no settings"
        (expect (= {:inline false :session "chat"}
                   (app/effective-surfaces {:surfaces {:inline false :session "chat"}} {})))))
  (describe
    "settings-key"
    ;; The engine registers `<settings-key>_inline` and `<settings-key>_session` when
    ;; an app opens. An invalid id would make that registration, and the open, fail.
    (it "names valid setting ids for an odd or a long key, and long keys stay apart"
        (let [long-a (apply str "Board " (repeat 80 "a"))
              long-b (str long-a "b")
              keys (map #(app/settings-key {:extension "sdk/My.Owner" :key %})
                        ["Board 1" long-a long-b])]
          (expect (every? #(re-matches setting-id-pattern (str % "_session")) keys))
          (expect (every? #(<= (count %) 64) keys))
          (expect (= 3 (count (distinct keys))))))
    (it "gives no settings to an app without a key"
        (expect (nil? (app/settings-key {:extension "demo"}))))))
