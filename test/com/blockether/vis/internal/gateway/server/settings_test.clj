(ns com.blockether.vis.internal.gateway.server.settings-test
  (:require [com.blockether.vis.internal.gateway.server.settings :as api]
            [lazytest.core :refer [defdescribe describe it expect]]))

(def ^:private nest-rows #'api/nest-rows)

(defn- ids
  [rows]
  (mapv (fn [row]
          (cond-> [(:id row)]
            (:children row)
            (conj (ids (:children row)))))
        rows))

(defdescribe nest-rows-test
             (describe "settings rows"
                       (it "nests rows under their parent at any depth and keeps their order"
                           (expect (= [["a" [["b" [["c"]]] ["d"]]] ["e"]]
                                      (ids (nest-rows [{:id "a"} {:id "b" :parent "a"} {:id "e"}
                                                       {:id "c" :parent "b"}
                                                       {:id "d" :parent "a"}])))))
                       (it "keeps a row at the top when its parent is not in the list"
                           (expect (= [["a"] ["b"]]
                                      (ids (nest-rows [{:id "a"} {:id "b" :parent "missing"}])))))
                       (it "shows each row of a parent cycle once"
                           (expect (= [["a" [["b"]]] ["c"]]
                                      (ids (nest-rows [{:id "a" :parent "b"} {:id "b" :parent "a"}
                                                       {:id "c" :parent "c"}])))))
                       (it "keeps a flat list flat"
                           (expect (= [{:id "a"} {:id "b"}] (nest-rows [{:id "a"} {:id "b"}]))))))
