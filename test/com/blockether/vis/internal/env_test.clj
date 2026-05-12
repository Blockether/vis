(ns com.blockether.vis.internal.env-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]))

(defn- eval-sci
  [{:keys [sci-ctx sandbox-ns]} code]
  (:val (sci/eval-string+ sci-ctx code {:ns sandbox-ns})))

(defdescribe repl-helper-test
  (it "does not expose the retired repl alias in the sandbox"
    (let [ctx (env/create-sci-context nil)]
      (expect (nil? (eval-sci ctx "(resolve 'repl/apropos)")))
      (expect (nil? (eval-sci ctx "(resolve 'clojure.repl/apropos)")))
      (expect (nil? (eval-sci ctx "(resolve 'clojure.java.io/file)")))
      (expect (some? (eval-sci ctx "IOException"))))))

(defdescribe build-bindings-test
  (it "renders persisted def source with version, scope, and Malli shape comment"
    (let [{:keys [sci-ctx initial-ns-keys]}
          (env/create-sci-context {})
          source "(def file-data (v/cat \"deps.edn\"))"
          value  {:path "deps.edn"
                  :lines ["a" "b"]}]
      (env/sci-update-binding! sci-ctx 'file-data value)
      (with-redefs [persistance/db-latest-var-registry
                    (fn [_ _]
                      {'file-data {:code source
                                   :version 2
                                   :created-at #inst "2026-05-12T00:00:00.000-00:00"}})]
        (let [bindings (env/build-bindings sci-ctx initial-ns-keys nil ::db ::conversation)]
          (expect (str/includes? bindings ";; version=2 scope=live shape=[:map"))
          (expect (str/includes? bindings "[:path :string]"))
          (expect (str/includes? bindings "[:lines [:vector :string]]"))
          (expect (str/includes? bindings source))
          (expect (not (str/includes? bindings "{:keys")))))))

  (it "renders persisted defn source instead of a synthetic signature"
    (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]}
          (env/create-sci-context {})
          source "(defn twice [x] (* 2 x))"]
      (sci/eval-string+ sci-ctx source {:ns sandbox-ns})
      (with-redefs [persistance/db-latest-var-registry
                    (fn [_ _]
                      {'twice {:code source
                               :version 1
                               :created-at #inst "2026-05-12T00:00:00.000-00:00"}})]
        (let [bindings (env/build-bindings sci-ctx initial-ns-keys nil ::db ::conversation)]
          (expect (str/includes? bindings ";; version=1 scope=live shape="))
          (expect (str/includes? bindings source))
          (expect (not (str/includes? bindings "..."))))))))
