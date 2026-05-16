(ns com.blockether.vis.internal.ctx-test
  (:require
   [com.blockether.vis.internal.ctx :as ctx]
   [com.blockether.vis.internal.env :as env]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]))

(defn- fresh []
  (env/create-sci-context nil))

(defn- ns-obj [sci-ctx]
  (sci/find-ns sci-ctx 'sandbox))

(defdescribe build-test
  (it "returns :conversation passthrough + :defs from sandbox state"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)
          _ (sci/eval-string+ sci-ctx
              "(def hits \"rg results\" {:hit-count 7 :hits []})
               (def hit-count 7)
               (defn hits-fn \"filter\" [pred] (filter pred [1 2 3]))"
              {:ns (ns-obj sci-ctx)})
          conv {:id :c :turn-id :t :iteration-id :i :user-request "q"}
          out  (ctx/build {:environment {:sci-ctx sci-ctx :initial-ns-keys initial-ns-keys}
                           :conversation conv})]
      (expect (= conv (:conversation out)))
      (expect (some? (get-in out [:defs 'hits :doc])))
      (expect (= "rg results" (get-in out [:defs 'hits :doc])))
      (expect (some? (get-in out [:defs 'hits :shape])))
      (expect (= :fn (get-in out [:defs 'hits-fn :shape :type])))
      (expect (some? (get-in out [:defs 'hits-fn :shape :arglists])))))

  (it "hides engine-owned symbols from :defs"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)
          _ (sci/eval-string+ sci-ctx
              "(def ctx {:user {:request \"hi\"}}) (def my-var 1)"
              {:ns (ns-obj sci-ctx)})
          out (ctx/build {:environment {:sci-ctx sci-ctx :initial-ns-keys initial-ns-keys}
                          :conversation {}})]
      (expect (contains? (:defs out) 'my-var))
      (expect (not (contains? (:defs out) 'ctx)))
      (expect (not (contains? (:defs out) '*1)))))

  (it "shape cache reuses schema when value identity is unchanged"
    (let [{:keys [sci-ctx initial-ns-keys]} (fresh)
          _ (sci/eval-string+ sci-ctx "(def hits {:hit-count 1})" {:ns (ns-obj sci-ctx)})
          env {:sci-ctx sci-ctx :initial-ns-keys initial-ns-keys}
          a (ctx/build {:environment env :conversation {}})
          b (ctx/build {:environment env :conversation {}})]
      (expect (= (get-in a [:defs 'hits :shape])
                (get-in b [:defs 'hits :shape]))))))
