(ns com.blockether.vis-loop.loop.auto-forget-sqlite-test
  "SQLite-backed integration coverage for auto-forget stale vars."
  (:require
   [com.blockether.vis-loop.loop.core :as loop]
   [com.blockether.vis-persistance.core :as db]
   [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h :refer [store]]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

(h/use-mem-store!)

(defn- sci-var
  "Create a SCI var with optional :doc metadata."
  ([sym value]
   (sci/new-var sym value))
  ([sym value doc]
   (sci/new-var sym value {:doc doc})))

(defn- make-sandbox
  "Build a sandbox-map {symbol -> sci-var} from a seq of
   [sym value] or [sym value doc] triples."
  [entries]
  (into {}
    (map (fn [[sym value & [doc]]]
           [sym (if doc (sci-var sym value doc) (sci-var sym value))]))
    entries))

(defn- make-sci-ctx
  "Create a minimal SCI context with a sandbox namespace containing `entries`.
   Returns the sci-ctx map (with :env atom)."
  [entries]
  (let [sandbox-map (make-sandbox entries)
        env-atom    (atom {:namespaces {'sandbox sandbox-map}})]
    {:env env-atom}))

(defn- sandbox-syms
  "Return the set of symbols currently in the SCI sandbox."
  [sci-ctx]
  (set (keys (get-in @(:env sci-ctx) [:namespaces 'sandbox]))))

(defdescribe auto-forget-stale-vars-test

  (it "🔥 stale var evicted from live SCI sandbox, fresh var untouched, revision bumped"
    (let [s       (store)
          cid     (db/store-conversation! s {:channel :vis})
          ;; Create 4 queries — only last 3 are "recent" (AUTO_FORGET_STALE_QUERIES=3)
          old-qid (db/store-query! s {:parent-conversation-id cid :query "old" :status :done})
          _       (Thread/sleep 5)
          q2id    (db/store-query! s {:parent-conversation-id cid :query "q2" :status :done})
          _       (Thread/sleep 5)
          q3id    (db/store-query! s {:parent-conversation-id cid :query "q3" :status :done})
          _       (Thread/sleep 5)
          q4id    (db/store-query! s {:parent-conversation-id cid :query "q4" :status :done})
          ;; Persist vars: `stale` was defined in old query, `fresh` in q4
          _       (db/store-iteration! s {:query-id old-qid :expressions [] :duration-ms 0
                                          :vars [{:name "stale" :value 1 :code "(def stale 1)"}]})
          _       (db/store-iteration! s {:query-id q4id :expressions [] :duration-ms 0
                                          :vars [{:name "fresh" :value 2 :code "(def fresh 2)"}]})
          ;; Build SCI sandbox with both vars (no docstrings)
          sci-ctx (make-sci-ctx [['stale 1] ['fresh 2]])
          via     (atom {:current-revision 0})
          rlm-env {:db-info          s
                   :conversation-id  cid
                   :sci-ctx          sci-ctx
                   :initial-ns-keys  #{}
                   :var-index-atom   via}]
      (#'loop/auto-forget-stale-vars! rlm-env)
      ;; `stale` should be gone, `fresh` should remain
      (expect (not (contains? (sandbox-syms sci-ctx) 'stale)))
      (expect (contains? (sandbox-syms sci-ctx) 'fresh))
      ;; var-index revision bumped
      (expect (= 1 (:current-revision @via)))))

  (it "😴 all vars recent → janitor naps, sandbox untouched, no revision bump"
    (let [s       (store)
          cid     (db/store-conversation! s {:channel :vis})
          qid     (db/store-query! s {:parent-conversation-id cid :query "q1" :status :done})
          _       (db/store-iteration! s {:query-id qid :expressions [] :duration-ms 0
                                          :vars [{:name "x" :value 1 :code "(def x 1)"}]})
          sci-ctx (make-sci-ctx [['x 1]])
          via     (atom {:current-revision 0})
          rlm-env {:db-info          s
                   :conversation-id  cid
                   :sci-ctx          sci-ctx
                   :initial-ns-keys  #{}
                   :var-index-atom   via}]
      (#'loop/auto-forget-stale-vars! rlm-env)
      (expect (contains? (sandbox-syms sci-ctx) 'x))
      ;; No bump — nothing was forgotten
      (expect (= 0 (:current-revision @via)))))

  (it "📖 docstring = immortality shield — stale but documented vars survive the purge"
    (let [s       (store)
          cid     (db/store-conversation! s {:channel :vis})
          old-qid (db/store-query! s {:parent-conversation-id cid :query "old" :status :done})
          _       (Thread/sleep 5)
          _       (db/store-query! s {:parent-conversation-id cid :query "q2" :status :done})
          _       (Thread/sleep 5)
          _       (db/store-query! s {:parent-conversation-id cid :query "q3" :status :done})
          _       (Thread/sleep 5)
          _       (db/store-query! s {:parent-conversation-id cid :query "q4" :status :done})
          _       (db/store-iteration! s {:query-id old-qid :expressions [] :duration-ms 0
                                          :vars [{:name "keeper" :value 42 :code "(def keeper 42)"}]})
          ;; `keeper` has a docstring in the live SCI sandbox
          sci-ctx (make-sci-ctx [['keeper 42 "I'm documented, keep me"]])
          via     (atom {:current-revision 0})
          rlm-env {:db-info          s
                   :conversation-id  cid
                   :sci-ctx          sci-ctx
                   :initial-ns-keys  #{}
                   :var-index-atom   via}]
      (#'loop/auto-forget-stale-vars! rlm-env)
      (expect (contains? (sandbox-syms sci-ctx) 'keeper))
      (expect (= 0 (:current-revision @via)))))

  (it "🚫 nil db-info → graceful no-op, nothing explodes, vars stay put"
    (let [sci-ctx (make-sci-ctx [['x 1]])
          rlm-env {:db-info nil :conversation-id nil :sci-ctx sci-ctx
                   :initial-ns-keys #{} :var-index-atom (atom {:current-revision 0})}]
      (#'loop/auto-forget-stale-vars! rlm-env)
      ;; Nothing exploded, var still there
      (expect (contains? (sandbox-syms sci-ctx) 'x))))

  (it "🏰 REASONING and friends are fortress vars — stale or not, the janitor can't touch them"
    (let [s       (store)
          cid     (db/store-conversation! s {:channel :vis})
          old-qid (db/store-query! s {:parent-conversation-id cid :query "old" :status :done})
          _       (Thread/sleep 5)
          _       (db/store-query! s {:parent-conversation-id cid :query "q2" :status :done})
          _       (Thread/sleep 5)
          _       (db/store-query! s {:parent-conversation-id cid :query "q3" :status :done})
          _       (Thread/sleep 5)
          _       (db/store-query! s {:parent-conversation-id cid :query "q4" :status :done})
          _       (db/store-iteration! s {:query-id old-qid :expressions [] :duration-ms 0
                                          :vars [{:name "REASONING" :value "think" :code "(def REASONING \"think\")"}]})
          sci-ctx (make-sci-ctx [['REASONING "think"]])
          via     (atom {:current-revision 0})
          rlm-env {:db-info          s
                   :conversation-id  cid
                   :sci-ctx          sci-ctx
                   :initial-ns-keys  #{}
                   :var-index-atom   via}]
      (#'loop/auto-forget-stale-vars! rlm-env)
      (expect (contains? (sandbox-syms sci-ctx) 'REASONING))
      (expect (= 0 (:current-revision @via))))))
