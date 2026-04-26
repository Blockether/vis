(ns com.blockether.vis.loop.auto-forget-test
  "Auto-forget 🧹 — the janitor that keeps the sandbox from turning into
   a hoarder's attic. Pure candidate selection + full DB→SCI integration."
  (:require
   [com.blockether.vis.loop.core :as loop]
   [com.blockether.vis.persistance.core :as db]
   [com.blockether.vis.test-helpers :as h :refer [store]]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

(h/use-mem-store!)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- sci-var
  "Create a SCI var with optional :doc metadata."
  ([sym val]
   (sci/new-var sym val))
  ([sym val doc]
   (sci/new-var sym val {:doc doc})))

(defn- make-sandbox
  "Build a sandbox-map {symbol -> sci-var} from a seq of
   [sym val] or [sym val doc] triples."
  [entries]
  (into {}
    (map (fn [[sym val & [doc]]]
           [sym (if doc (sci-var sym val doc) (sci-var sym val))]))
    entries))

(defn- make-registry
  "Build a var-registry {symbol -> {:query-id uuid ...}} from a seq of
   [sym query-id] pairs."
  [entries]
  (into {}
    (map (fn [[sym qid]]
           [sym {:query-id qid :value nil :code "" :version 0}]))
    entries))

;; ---------------------------------------------------------------------------
;; auto-forget-candidates (pure)
;; ---------------------------------------------------------------------------

(def q1 (random-uuid))
(def q2 (random-uuid))
(def q3 (random-uuid))
(def q4 (random-uuid))

(defdescribe auto-forget-candidates-test

  (it "🫙 empty sandbox → nothing to forget, move along"
    (expect (= #{} (loop/auto-forget-candidates {} #{} {} #{q1}))))

  (it "🛡️ built-ins are untouchable — hands off initial-ns-keys"
    (let [sandbox   (make-sandbox [['fetch 42]])
          initials  #{'fetch}
          registry  (make-registry [['fetch q1]])
          recent    #{q1}]
      (expect (= #{} (loop/auto-forget-candidates sandbox initials registry recent)))))

  (it "🎧 SYSTEM vars (QUERY/ANSWER/REASONING) are sacred — never forgotten"
    (let [sandbox   (make-sandbox [['QUERY "hello"]])
          registry  (make-registry [['QUERY q1]])
          recent    #{q2}]  ;; q1 is NOT recent — would be forgotten if not in SYSTEM_VAR_NAMES
      (expect (= #{} (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "📝 documented vars survive any purge — docstrings are armor"
    (let [sandbox   (make-sandbox [['important 99 "This var is documented"]])
          registry  (make-registry [['important q1]])
          recent    #{q2}]  ;; q1 is NOT recent
      (expect (= #{} (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "🕐 recently-touched vars stay alive within the recency window"
    (let [sandbox   (make-sandbox [['scratch 1]])
          registry  (make-registry [['scratch q2]])
          recent    #{q1 q2 q3}]
      (expect (= #{} (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "🗑️ stale undocumented scratch vars get swept without mercy"
    (let [sandbox   (make-sandbox [['scratch 1] ['tmp 2]])
          registry  (make-registry [['scratch q1] ['tmp q1]])
          recent    #{q3 q4}]
      (expect (= #{'scratch 'tmp}
                (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "🎯 full gauntlet: stale→gone, documented→safe, recent→safe, system→safe, builtin→safe"
    (let [sandbox   (make-sandbox [['stale-a 1]
                                   ['stale-b 2]
                                   ['documented 3 "keep me"]
                                   ['recent-var 4]
                                   ['REASONING 5]            ;; SYSTEM_VAR_NAMES — protected
                                   ['builtin 6]])
          initials  #{'builtin}
          registry  (make-registry [['stale-a q1]
                                    ['stale-b q1]
                                    ['documented q1]
                                    ['recent-var q3]
                                    ['REASONING q1]
                                    ['builtin q1]])
          recent    #{q3 q4}]
      (expect (= #{'stale-a 'stale-b}
                (loop/auto-forget-candidates sandbox initials registry recent)))))

  (it "👻 ephemeral vars with no DB footprint are invisible to the janitor"
    (let [sandbox   (make-sandbox [['ephemeral 99]])
          registry  {}
          recent    #{q1}]
      (expect (= #{} (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "⚡ a non-registered uppercase var (e.g. CONFIG) gets forgotten like any mortal var"
    ;; SYSTEM_VAR_NAMES is a fixed set #{QUERY ANSWER REASONING};
    ;; user-defined uppercase names (CONFIG, MAX_FOO, ...) are NOT system
    ;; vars and get the normal stale-sweep treatment.
    (let [sandbox   (make-sandbox [['CONFIG 42]])
          registry  (make-registry [['CONFIG q1]])
          recent    #{q2}]
      (expect (= #{'CONFIG} (loop/auto-forget-candidates sandbox #{} registry recent))))))

;; ---------------------------------------------------------------------------
;; auto-forget-stale-vars! (effectful, DB + SCI)
;; ---------------------------------------------------------------------------

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
