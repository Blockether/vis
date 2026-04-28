(ns com.blockether.vis-loop.loop.auto-forget-test
  "Auto-forget 🧹 — the janitor that keeps the sandbox from turning into
   a hoarder's attic. Pure candidate selection + full DB→SCI integration."
  (:require
   [com.blockether.vis-loop.loop.core :as loop]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

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
