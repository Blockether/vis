(ns com.blockether.vis.loop.knowledge.pageindex.core
  "PageIndex orchestration facade.
   Owns the public index! entrypoint and explicit dependency injection for tests."
  (:require [com.blockether.anomaly.core :as anomaly]))

(defn index!
  "Runs incremental indexing through an injected implementation.

   Required:
   - opts :deps {:index-impl-fn (fn [file-path opts] ...)}

   This allows tests to stub behavior without with-redefs on another namespace."
  ([file-path] (index! file-path {}))
  ([file-path {:keys [deps] :as opts}]
   (let [impl-fn (:index-impl-fn deps)]
     (when-not (fn? impl-fn)
       (anomaly/incorrect! "index! missing dependency :index-impl-fn"
         {:type :svar.pageindex/missing-dependency
          :dependency :index-impl-fn}))
     (impl-fn file-path opts))))
