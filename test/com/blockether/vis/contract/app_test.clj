(ns com.blockether.vis.contract.app-test
  "Where a live app shows. The engine resolves it once, and the TUI and the
   Companion read the answer, so these rules decide what both clients show."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.contract.document :as document]
            [lazytest.core :refer [defdescribe describe expect it throws?]]))

(def ^:private setting-id-pattern
  (re-pattern (get-in (document/schema-document "toggle")
                      ["$defs" "contribution" "properties" "id" "pattern"])))

(defdescribe
  app-surfaces-test
  (describe
    "effective-surfaces"
    (it "gives the person's choice for one app over the app's declaration"
        (expect (= {:inline false :session "chat"}
                   (app/effective-surfaces
                     {:extension "demo" :key "board" :surfaces {:inline true :session "sidebar"}}
                     {"app_demo_board_inline" false "app_demo_board_session" "chat"}))))
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
  (describe "settings-key"
            ;; The engine registers `<settings-key>_inline` and `<settings-key>_session` when
            ;; an app opens. An invalid id would make that registration, and the open, fail.
            (it "names valid setting ids for an odd or a long key, and long keys stay apart"
                (let [long-a
                      (apply str "Board " (repeat 80 "a"))

                      long-b
                      (str long-a "b")

                      keys
                      (map #(app/settings-key {:extension "sdk/My.Owner" :key %})
                           ["Board 1" long-a long-b])]

                  (expect (every? #(re-matches setting-id-pattern (str % "_session")) keys))
                  (expect (every? #(<= (count %) 64) keys))
                  (expect (= 3 (count (distinct keys))))))
            (it "gives no settings to an app without a key"
                (expect (nil? (app/settings-key {:extension "demo"}))))))

(defn- fixture-text
  "The text that a fixture value spells: text, a repeat or a list of both."
  [value]
  (cond (string? value) value
        (sequential? value) (apply str (map fixture-text value))
        :else (apply str (repeat (long (get value "times")) (get value "repeat")))))

(defn- text-matches?
  "Whether `actual` is the fixture value `wanted`: exact, or the checks of a map."
  [actual wanted]
  (if (map? wanted)
    (let [{:strs [length starts ends contains]}
          wanted

          ^String text
          actual]

      (and (string? actual)
           (or (nil? length) (= length (.codePointCount text 0 (.length text))))
           (or (nil? starts) (str/starts-with? text starts))
           (or (nil? ends) (str/ends-with? text ends))
           (or (nil? contains) (str/includes? text contains))))
    (= wanted actual)))

(defdescribe
  app-log-test
  ;; Review report (goal 4b41c770): a long stack trace must not lose the entry.
  (it "follows the shared log rules of fixtures/app-log.json"
      (doseq [case
              (get (json/read-json (slurp (io/resource "vis-contract/fixtures/app-log.json")))
                   "cases")

              :let [named
                    (get case "name")

                    prefill
                    (mapv (fn [i]
                            {:seq i :at 0 :level "info" :source "app" :message (str "e" i)})
                          (range 1 (inc (long (get case "prefill")))))

                    entry
                    (into {}
                          (map (fn [[k v]]
                                 [(keyword k) (if (#{"message" "detail"} k) (fixture-text v) v)]))
                          (get case "entry"))

                    log
                    (:log (app/append-log {:placement :attachment :log prefill} entry))

                    {:strs [count first_seq last absent]}
                    (get case "expect")]]

        (expect (= count (clojure.core/count log)) named)
        (expect (= first_seq (:seq (first log))) named)
        (doseq [[k wanted] last]
          (expect (text-matches? (get (peek log) (keyword k)) wanted) (str named ": " k)))
        (doseq [k absent]
          (expect (not (contains? (peek log) (keyword k))) (str named ": no " k)))))
  (it "refuses an entry without a source"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(app/append-log {:placement :attachment}
                                        {:level "info" :message "x" :at 1})))))

(defdescribe canvas-thread-id-test
             (it "does not replace a card or thread when a new draft picks its id"
                 (let [node
                       {:id "board"
                        :type :canvas
                        :cards [{:id "a"}]
                        :threads [{:id "a-1" :item-id "a" :messages []}]}

                       draft
                       (app/draft-op node {:item-id "a" :body "Check it"} "a")

                       reply
                       (app/reply-op node {:item-id "a" :new-thread-id "a-1"} {:body "Answer"})]

                   (expect (= "a-2" (get-in draft [:thread :id])))
                   (expect (not (#{"a" "a-1"} (get-in reply [:thread :id])))))))

(defdescribe
  canvas-draft-test
  (it "refuses a new draft on a missing card"
      (let [node {:id "board" :type :canvas :cards []}]
        (expect (throws? clojure.lang.ExceptionInfo
                         #(app/draft-op node {:item-id "missing" :body "Check it"} "draft-1")))))
  (it "refuses a reply on a full thread but lets the human attach or remove a draft"
      (let [limit (:canvas-messages app/bounds)
            thread {:id "t" :item-id "a" :draft "Old note"
                    :messages (mapv #(hash-map :id (str %) :body "Note") (range limit))}
            node {:id "board" :type :canvas :cards [{:id "a"}] :threads [thread]}
            save #(app/draft-op node {:thread-id "t" :body %} "unused")]
        (expect (throws? clojure.lang.ExceptionInfo #(save "New note")))
        (expect (= "" (get-in (save "") [:thread :draft])))
        (expect (= (dissoc thread :draft) (:thread (save nil)))))))
