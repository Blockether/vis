(ns com.blockether.vis.internal.commandline-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.commandline :as commandline]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- sample-root
  []
  {:cmd/name "vis"
   :cmd/doc  "root"
   :cmd/subcommands
   [{:cmd/name "providers"
     :cmd/doc  "providers"
     :cmd/subcommands
     [{:cmd/name "list"
       :cmd/doc  "list providers"
       :cmd/run-fn (fn [_ _] :listed)}]}]})

(defdescribe dispatch-help-test
  (it "reports unknown nested subcommands before rendering parent help"
    (let [printed (atom nil)
          result  (commandline/dispatch! (sample-root)
                    ["vis" "providers" "missing" "--help"]
                    {:print-fn #(reset! printed %)})]
      (expect (= :error (:status result)))
      (expect (str/includes? @printed "Unknown command: vis providers missing"))
      (expect (str/includes? @printed "USAGE"))))

  (it "keeps known leaf help working"
    (let [printed (atom nil)
          result  (commandline/dispatch! (sample-root)
                    ["vis" "providers" "list" "--help"]
                    {:print-fn #(reset! printed %)})]
      (expect (= :help (:status result)))
      (expect (str/includes? @printed "vis providers list")))))
