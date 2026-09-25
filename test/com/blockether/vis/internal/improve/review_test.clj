(ns com.blockether.vis.internal.improve.review-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config.improve :as settings]
            [com.blockether.vis.internal.improve.core :as improve]
            [com.blockether.vis.internal.improve.review :as review]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(h/use-mem-store!)

(defn- status [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:status (ex-data e)))))

(defn- automatic
  [f]
  (with-redefs [settings/snapshot
                (constantly {:settings {:mode "automatic" :provider "p" :model "m"}})

                settings/current?
                (constantly true)]

    (reset! @#'review/cursors {})
    (f)))

(deftest real-adapter-pins-routing-and-never-falls-back
  (let [calls
        (atom [])

        chosen
        {:id :p :models [{:name "m"} {:name "other"}]}

        result
        {:analyses [] :groups []}]

    (with-redefs [loop-router/get-router
                  (constantly {:providers [chosen {:id :other :models [{:name "m"}]}]})

                  svar/ask!
                  (fn [router opts]
                    (swap! calls conj [router opts])
                    {:result result})]

      (is (= result (review/analyze! {:provider "p" :model "m"} [])))
      (let [[router opts] (first @calls)]
        (is (= [(assoc chosen :models [{:name "m"}])] (:providers router)))
        (is (= {:provider :p :model "m" :on-transient-error :fail :on-auth-error :fail}
               (:routing opts)))
        (is (false? (get-in router [:rate-limit :fallback-provider?])))
        (is (= [] (:refusal-fallbacks opts)))
        (is (= 0 (:format-retries opts)))
        (is (nil? (:tools opts))))
      (is (= 409 (status #(review/analyze! {:provider "missing" :model "m"} []))))
      (is (= 409 (status #(review/analyze! {:provider "p" :model "missing"} []))))
      ;; Deliberately invalid route: the adapter must reject before keyword routing.
      (is (= 409 (status #(review/analyze! {:provider nil :model nil} []))))
      (is (= 1 (count @calls))))))

(deftest human-and-off-never-call-model
  (doseq [mode ["off" "human"]]
    (with-redefs [settings/snapshot (constantly {:settings {:mode mode}})
                  review/analyze! (fn [& _]
                                    (throw (AssertionError. "Must not call model")))]

      (is (= 409 (status #(review/run! (h/store))))))))

(deftest review-appends-analysis-and-groups-without-closing
  (automatic
    (fn []
      (let [db
            (h/store)

            a
            (improve/create! db {:title "First" :content "Human Markdown"})

            b
            (improve/create! db {:title "Second"})]

        (with-redefs [review/analyze! (fn [route records]
                                        (is (= "p" (:provider route)))
                                        (is (= 2 (count records)))
                                        {:analyses [{:id (:id a)
                                                     :markdown "Plan: use a safe fixture."}]
                                         :groups [{:title "Shared cause"
                                                   :markdown "Related reports"
                                                   :children [(:id a) (:id b)]}]})]
          (let [result (review/run! db)
                a* (improve/get-record db (:id a))]

            (is (= "reviewed" (get-in result [:projects 0 :status])))
            (is (= "not_attempted" (:reproduction result)))
            (is (str/starts-with? (:content a*) "Human Markdown"))
            (is (str/includes? (:content a*) "not attempted"))
            (is (:parent_id a*))
            (is (= (:parent_id a*) (:parent_id (improve/get-record db (:id b)))))
            (is (every? #(= "open" (:status %)) (improve/list-records db {})))))))))

(deftest stale-human-edit-is-never-overwritten
  (automatic (fn []
               (let [db
                     (h/store)

                     a
                     (improve/create! db {:title "First"})]

                 (with-redefs [review/analyze!
                               (fn [& _]
                                 (improve/update! db (:id a) {:content "New human edit"})
                                 {:analyses [{:id (:id a) :markdown "Old suggestion"}] :groups []})]
                   (is (= "failed" (get-in (review/run! db) [:projects 0 :status])))
                   (is (= "New human edit" (:content (improve/get-record db (:id a))))))))))

(deftest mode-change-during-model-call-invalidates-result
  (automatic
    (fn []
      (let [db
            (h/store)

            a
            (improve/create! db {:title "First"})

            current
            (atom true)]

        (with-redefs [settings/current?
                      (fn [_]
                        @current)

                      review/analyze!
                      (fn [& _]
                        (reset! current false)
                        {:analyses [{:id (:id a) :markdown "Suggestion"}] :groups []})]

          (is (= "failed" (get-in (review/run! db) [:projects 0 :status])))
          (is (= "" (:content (improve/get-record db (:id a))))))))))

(deftest malformed-or-foreign-output-is-rejected
  (automatic
    (fn []
      (let [db
            (h/store)

            a
            (improve/create! db {:title "First"})]

        (doseq [output [{:analyses [{:id 999999 :markdown "Foreign"}] :groups []}
                        {:analyses [{:id (:id a) :markdown "Text" :status "closed"}] :groups []}
                        {:analyses []
                         :groups [{:title "Bad" :markdown "Bad" :children [(:id a) (:id a)]}]}
                        {:analyses [{:id (:id a) :markdown (apply str (repeat 6001 "x"))}]
                         :groups []} {:analyses [] :groups [] :commands ["do not execute"]}]]
          (with-redefs [review/analyze! (fn [& _]
                                          output)]
            (is (= "failed" (get-in (review/run! db) [:projects 0 :status])))
            (is (= 1 (:version (improve/get-record db (:id a)))))))))))

(deftest batches-are-bounded-and-advance
  (automatic (fn []
               (let [db
                     (h/store)

                     calls
                     (atom [])]

                 (dotimes [i 12]
                   (improve/create! db {:title (str "Issue " i)}))
                 (with-redefs [review/analyze! (fn [_ records]
                                                 (swap! calls conj (mapv :id records))
                                                 {:analyses [] :groups []})]
                   (review/run! db)
                   (review/run! db)
                   (is (= [10 2] (mapv count @calls)))
                   (is (= 12 (count (distinct (mapcat identity @calls))))))))))

(deftest empty-projects-do-not-call-model
  (automatic (fn []
               (with-redefs [review/analyze! (fn [& _]
                                               (throw (AssertionError. "Must not call model")))]
                 (is (= [] (:projects (review/run! (h/store)))))))))

(deftest periodic-mode-and-interval-changes
  (let [mode
        (atom {:settings {:mode "human" :interval_minutes 1}})

        schedule
        (atom nil)

        calls
        (atom 0)]

    (with-redefs [settings/snapshot
                  (fn []
                    @mode)

                  review/run!
                  (fn [_]
                    (swap! calls inc))]

      (#'review/tick! :db schedule 0)
      (#'review/tick! :db schedule 60000)
      (is (zero? @calls))
      (reset! mode {:settings {:mode "automatic" :provider "p" :model "m" :interval_minutes 1}})
      (#'review/tick! :db schedule 60001)
      (#'review/tick! :db schedule 120000)
      (is (zero? @calls))
      (#'review/tick! :db schedule 120001)
      (is (= 1 @calls))
      (swap! mode assoc-in [:settings :interval_minutes] 2)
      (#'review/tick! :db schedule 130000)
      (#'review/tick! :db schedule 249999)
      (is (= 1 @calls))
      (#'review/tick! :db schedule 250000)
      (is (= 2 @calls))
      (swap! mode assoc-in [:settings :mode] "off")
      (#'review/tick! :db schedule 251000)
      (#'review/tick! :db schedule 400000)
      (is (= 2 @calls)))))

(deftest incomplete-automatic-route-does-not-start
  (with-redefs [settings/snapshot
                (constantly {:settings {:mode "automatic"}})

                review/analyze!
                (fn [& _]
                  (throw (AssertionError. "Must not call model")))]

    (is (= 409 (status #(review/run! (h/store)))))))

(deftest concurrent-review-and-shutdown-invalidate-writes
  (automatic
    (fn []
      (let [db
            (h/store)

            record
            (improve/create! db {:title "First"})

            entered
            (promise)

            release
            (promise)]

        (with-redefs [review/analyze! (fn [& _]
                                        (deliver entered true)
                                        (deref release 5000 nil)
                                        {:analyses [{:id (:id record) :markdown "Late suggestion"}]
                                         :groups []})]
          (let [stop! (review/start! db)
                run (future (review/run! db))]

            (try (is (= true (deref entered 5000 false)))
                 (is (= 409 (status #(review/run! db))))
                 (stop!)
                 (stop!)
                 (deliver release true)
                 (is (= "failed" (get-in (deref run 5000 {}) [:projects 0 :status])))
                 (is (= "" (:content (improve/get-record db (:id record)))))
                 (finally (stop!) (deliver release true) (future-cancel run)))))))))

(deftest provider-failure-and-timeout-leave-records-untouched
  (automatic
    (fn []
      (let [db
            (h/store)

            record
            (improve/create! db {:title "First"})]

        (with-redefs [review/analyze! (fn [& _]
                                        (throw (ex-info "Private provider detail" {})))]
          (let [result (review/run! db)]
            (is (= "failed" (get-in result [:projects 0 :status])))
            (is (not (str/includes? (pr-str result) "Private provider detail")))))
        (let [result (with-redefs-fn {#'review/call-timeout-ms 5
                                      #'review/analyze! (fn [& _]
                                                          (Thread/sleep 1000)
                                                          {:analyses [] :groups []})}
                       #(review/run! db))]
          (is (= "failed" (get-in result [:projects 0 :status]))))
        (is (= 1 (:version (improve/get-record db (:id record)))))))))

(deftest multiple-projects-and-records-eventually-visit-every-batch
  (automatic
    (fn []
      (let [db
            (h/store)

            calls
            (atom [])]

        (dotimes [p 5]
          (let [id (str (random-uuid))]
            (h/raw-query db
                         {:insert-into :project
                          :values
                          [{:id id :owner_id "local" :name (str "Project " p) :created_at 0}]})
            (dotimes [i 12]
              (improve/create! db {:project_id id :title (str "Issue " i)}))))
        (with-redefs [review/analyze! (fn [_ records]
                                        (swap! calls conj (mapv :id records))
                                        {:analyses [] :groups []})]
          (dotimes [_ 5]
            (is (= 2 (count (:projects (review/run! db)))))))
        (is (= 10 (count @calls)))
        (is (every? #(<= (count %) 10) @calls))
        (is (= 60 (count (distinct (mapcat identity @calls)))))))))

(deftest failed-batches-do-not-starve-later-records
  (automatic (fn []
               (let [db
                     (h/store)

                     calls
                     (atom [])]

                 (dotimes [i 12]
                   (improve/create! db {:title (str "Issue " i)}))
                 (with-redefs [review/analyze! (fn [_ records]
                                                 (swap! calls conj (mapv :id records))
                                                 (throw (ex-info "Provider unavailable" {})))]
                   (review/run! db)
                   (review/run! db))
                 (is (= [10 2] (mapv count @calls)))
                 (is (= 12 (count (distinct (mapcat identity @calls)))))))))
