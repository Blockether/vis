(ns com.blockether.vis.internal.workspace.fff-worker-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.fff :as fff]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis.internal.workspace.fff-index :as index]
            [com.blockether.vis.internal.workspace.fff-index-test :as index-test]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(defn- with-worker
  [roots-fn search f]
  (let [made
        (env/create-python-context {'shared-search search}
                                   roots-fn
                                   {:worker? true
                                    :worker-policy-fn (fn []
                                                        {:roots-fn roots-fn :net-enabled? false})
                                    :jail-enabled? true
                                    :enabled? false}
                                   nil)

        session
        (:python-context made)]

    (try (f session)
         (finally (env/dispose-python-context! session) (is (not (worker/worker-live? session)))))))

(defn- search-block
  [mode]
  (str "import json, os, sys\n"
       "print(json.dumps({'pid': os.getpid(), 'platform': sys.platform, "
       "'confined': os.environ.get('VIS_SEATBELT_ACTIVE'), "
       "'matches': await shared_search('"
       mode
       "')}))"))

(defn- result-data
  [result]
  (is (nil? (:error result)) (pr-str result))
  (some-> (:stdout result)
          (json/read-json :key-fn keyword)))

(deftest native-workers-share-host-fff-index-test
  ;; Worker processes call back into one host pool, not one native index per worker.
  (let [fixture
        (fs/create-temp-dir {:prefix "vis-fff-workers-"})

        root
        (io/file (str fixture) "shared")

        other-root
        (io/file (str fixture) "other")

        roots-fn
        (constantly [(.getCanonicalPath (io/file ".")) (.getCanonicalPath (io/file (str fixture)))])

        create
        fff/create

        creates
        (atom 0)

        handles
        (atom {})]

    (try
      (.mkdirs root)
      (.mkdirs other-root)
      (spit (io/file root "marker.txt") "shared fixture\n")
      (spit (io/file root "private.txt") "excluded by another policy\n")
      (spit (io/file other-root "other.txt") "separate root\n")
      (#'index-test/with-pool
       (fn [_ _]
         (with-redefs [fff/create (fn [opts]
                                    (swap! creates inc)
                                    (create opts))]
           (let [search (fn [mode]
                          (let [lease (case mode
                                        "shared"
                                        (index/lease root true)

                                        "alias"
                                        (index/lease (io/file root ".") true)

                                        "filtered"
                                        (index/lease root true {:exclude-globs ["private.txt"]})

                                        "other"
                                        (index/lease other-root true))]
                            (index/with-index* lease
                                               (fn [idx]
                                                 (swap! handles update mode (fnil conj []) idx)
                                                 (:total-matched
                                                   (fff/search idx {:query "" :page-size 20}))))))]
             (with-worker
               roots-fn
               search
               (fn [first-worker]
                 (with-worker
                   roots-fn
                   search
                   (fn [second-worker]
                     (is (not= first-worker second-worker))
                     (is (worker/worker-live? first-worker))
                     (is (worker/worker-live? second-worker))
                     (let [runs (mapv (fn [session]
                                        (future (env/run-python-block session
                                                                      (search-block "shared"))))
                                      [first-worker second-worker])]
                       (try
                         (let [results (mapv #(result-data (deref % 15000 {:error :timeout})) runs)
                               pids (mapv :pid results)]

                           (is (every? pos-int? pids))
                           (is (apply not= pids))
                           (doseq [result results]
                             (is (= 2 (:matches result)))
                             (when (= "darwin" (:platform result)) (is (= "1" (:confined result)))))
                           (is (= 1 @creates))
                           (is (= 2 (count (get @handles "shared"))))
                           (is (apply identical? (get @handles "shared")))
                           (let [alias (result-data (env/run-python-block second-worker
                                                                          (search-block "alias")))
                                 _ (is (= 2 (:matches alias)))
                                 _ (is (= 1 @creates))
                                 _ (is (identical? (first (get @handles "shared"))
                                                   (first (get @handles "alias"))))
                                 filtered (result-data (env/run-python-block first-worker
                                                                             (search-block
                                                                               "filtered")))
                                 other (result-data (env/run-python-block second-worker
                                                                          (search-block "other")))]

                             (is (= 1 (:matches filtered)))
                             (is (= 1 (:matches other)))
                             (is (= 3 @creates))
                             (is (not (identical? (first (get @handles "shared"))
                                                  (first (get @handles "filtered")))))
                             (is (not (identical? (first (get @handles "shared"))
                                                  (first (get @handles "other")))))
                             (println "FFF worker sharing experiment"
                                      {:worker-pids pids
                                       :same-root-builds 1
                                       :builds-including-distinct-root-and-policy @creates})))
                         (finally (doseq [run runs]
                                    (future-cancel run)))))))))))))
      (finally (fs/delete-tree fixture)))))
