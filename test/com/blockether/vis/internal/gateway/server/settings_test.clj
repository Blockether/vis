(ns com.blockether.vis.internal.gateway.server.settings-test
  (:require [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.gateway.server.settings :as api]
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

(def ^:private extension-rows #'api/extension-rows)

;; Regression for #337: the built-in foundation-mcp showed as its own extension section.
(defdescribe extension-rows-test
             (describe "extension sections"
                       (it "gives foundation extensions no section and no engine row"
                           (with-redefs [extension/registered-extensions
                                         (fn [_]
                                           [{:ext/name "foundation-mcp" :ext/kind "foundation"}
                                            {:ext/name "language-clojure"
                                             :ext/kind "language"
                                             :ext/toggles [{:id "clojure_repl"}]}])]
                             (expect (= {(scoped/resource-id :engines "language-clojure")
                                         ["language-clojure" 0]
                                         "clojure_repl" ["language-clojure" 1]}
                                        (extension-rows {:root "/tmp/project"})))))))
