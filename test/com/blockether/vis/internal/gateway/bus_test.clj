(ns com.blockether.vis.internal.gateway.bus-test
  "Cross-process journal hydration: a watcher joining a session mid-turn must
   MIRROR a turn genuinely streaming in a live sibling process, but must never
   RESURRECT one orphaned by a crashed/restarted daemon. Resurrecting an orphan
   pins the reader's `:current-turn` to a turn that will never emit a terminal —
   wedging the session queue (new sends stall `queued`) and spinning every
   watcher forever. The pid-liveness gate is what tells the two apart."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-temp-journal
  "Run `f` with the bus journal dir redirected to a throwaway temp dir and the
   deliver-fn capturing every ingested event into the returned atom. Restores
   the previous deliver-fn afterwards. `f` receives `[capture write-journal!]`
   where `write-journal!` spits ndjson lines for a sid."
  [f]
  (let
    [tmp
     (java.nio.file.Files/createTempDirectory "bus-test"
                                              (make-array java.nio.file.attribute.FileAttribute 0))

     capture
     (atom [])

     prev
     @(var-get #'bus/deliver-fn)]

    (with-redefs
      [bus/events-dir (fn []
                        tmp)]
      ;; a fresh journal dir is a fresh world: drop this process's orphan-reap
      ;; markers so re-running in one JVM doesn't suppress a reap
      (reset! (var-get #'bus/reaped-turns) {})
      (bus/set-deliver-fn! (fn [_sid _store? ev]
                             (swap! capture conj ev)))
      (try (f capture
              (fn write-journal! [sid events]
                (let [file (#'bus/session-file sid)]
                  (spit file (str/join (map #(str (wire/json-str %) "\n") events))))))
           (finally (bus/set-deliver-fn! prev))))))

(defn- turn-started
  [prod pid sid tid]
  {:_producer prod
   :_pid pid
   :_store true
   :schema 1
   :seq 5
   :type "turn.started"
   :turn_id tid
   :session_id sid})

(defn- delta
  [prod pid tid]
  {:_producer prod :_pid pid :_store true :schema 1 :seq 6 :type "content.delta" :turn_id tid})

(def ^:private dead-pid
  "A pid no live process owns, so `ProcessHandle/of` reports it absent."
  2147483646)

(defdescribe
  hydrate-liveness-test
  (it "mirrors a non-terminal turn whose producer process is still ALIVE"
      (with-temp-journal (fn [capture write!]
                           (let
                             [prod
                              (str (java.util.UUID/randomUUID))

                              live-pid
                              (var-get #'bus/producer-pid)]

                             (write! "sid-live"
                                     [(turn-started prod live-pid "sid-live" "T-live")
                                      (delta prod live-pid "T-live")])
                             (bus/hydrate! "sid-live")
                             (expect (= ["turn.started" "content.delta"]
                                        (mapv #(get % "type") @capture)))))))
  (it "REAPS an orphan (dead producer) with a synthetic terminal, never resurrects it"
      (with-temp-journal (fn [capture write!]
                           (let [prod (str (java.util.UUID/randomUUID))]
                             (write! "sid-orphan"
                                     [(turn-started prod dead-pid "sid-orphan" "T-orphan")
                                      (delta prod dead-pid "T-orphan")])
                             (bus/hydrate! "sid-orphan")
                             (let [ev (first @capture)]
                               ;; the ONLY thing delivered is a terminal for the orphaned turn —
                               ;; the live turn.started/deltas are NOT replayed
                               (expect (= 1 (count @capture)))
                               (expect (= "turn.failed" (get ev "type")))
                               (expect (= "interrupted" (get ev "status")))
                               (expect (= "T-orphan" (get ev "turn_id"))))))))
  (it "treats a cancelled turn as terminal, not an orphan to fail again"
      (with-temp-journal
        (fn [capture write!]
          (let
            [prod
             (str (java.util.UUID/randomUUID))

             sid
             "sid-cancel"

             tid
             "T-cancel"]

            (write! sid
                    [(turn-started prod dead-pid sid tid)
                     (assoc (delta prod dead-pid tid)
                       :seq 7
                       :type "turn.cancelled"
                       :status "cancelled"
                       :session_id sid)])
            (bus/hydrate! sid)
            (expect (empty? @capture))))))
  (it "is idempotent: a second hydrate of a reaped journal delivers nothing"
      (with-temp-journal (fn [capture write!]
                           (let [prod (str (java.util.UUID/randomUUID))]
                             (write! "sid-idem" [(turn-started prod dead-pid "sid-idem" "T-idem")])
                             (bus/hydrate! "sid-idem") ; reaps, appends terminal to journal
                             (reset! capture [])
                             (bus/hydrate! "sid-idem") ; terminal present now -> no-op
                             (expect (empty? @capture))))))
  (it "reaps ONCE even before the terminal lands in the journal (async publish! race)"
      (with-temp-journal (fn [capture write!]
                           (let [prod (str (java.util.UUID/randomUUID))]
                             (write! "sid-race" [(turn-started prod dead-pid "sid-race" "T-race")])
                             ;; `publish!` is ASYNC: hold the terminal out of the journal to stand
                             ;; in for its write window. Without the CAS marker both hydrates read
                             ;; `terminal? = false` and each emit their own `turn.failed`.
                             (with-redefs
                               [bus/publish! (fn [& _]
                                               nil)]
                               (bus/hydrate! "sid-race")
                               (bus/hydrate! "sid-race"))
                             (expect (= 1 (count @capture)))
                             (expect (= "turn.failed" (get (first @capture) "type"))))))))

(defdescribe
  journal-high-water-seq-test
  (it
    "returns the max :seq in the journal so a restarted daemon numbers ABOVE a client's stale cursor"
    (with-temp-journal (fn [_capture write!]
                         (let [prod (str (java.util.UUID/randomUUID))]
                           (write! "sid-hw"
                                   [(turn-started prod dead-pid "sid-hw" "T-hw")
                                    (delta prod dead-pid "T-hw")])
                           ;; turn-started :seq 5, delta :seq 6 -> high-water 6
                           (expect (= 6 (bus/journal-high-water-seq "sid-hw")))))))
  (it "is 0 when the session has no journal file yet"
      (with-temp-journal (fn [_capture _write!]
                           (expect (= 0 (bus/journal-high-water-seq "sid-none")))))))

(defdescribe
  tail-cursor-race-test
  "`hydrate!` (HTTP thread) and `drain-file!` (tailer thread) own the SAME tail
   cursor. Unsynchronized, a drain that read a PREFIX writes its stale, smaller
   offset back over hydrate's EOF claim — and the next poll re-delivers events
   the watcher already rendered."
  (it
    "a concurrent drain never rewinds hydrate!'s EOF claim"
    (with-temp-journal
      (fn [capture write!]
        (let
          [prod
           (str (java.util.UUID/randomUUID))

           sid
           "sid-cursor"

           f
           (#'bus/session-file sid)]

          (write! sid [(turn-started prod (var-get #'bus/producer-pid) sid "T-cursor")])
          (let
            [gate
             (promise)

             orig
             @#'bus/deliver-line!

             ;; park the drain mid-file, holding the cursor it read at entry
             drain
             (future (with-redefs
                       [bus/deliver-line! (fn [s l]
                                            @gate
                                            (orig s l))]
                       (#'bus/drain-file! f)))]

            (Thread/sleep 100)
            ;; producer appends while the drain is parked, then a subscriber hydrates
            (spit f
                  (str (wire/json-str (delta prod (var-get #'bus/producer-pid) "T-cursor")) "\n")
                  :append
                  true)
            (future (bus/hydrate! sid))
            (Thread/sleep 100)
            (deliver gate true)
            @drain
            (Thread/sleep 200)
            (expect (= (.length f) (long (get @(var-get #'bus/offsets) sid))))
            (reset! capture [])
            (#'bus/drain-file! f)
            (expect (empty? @capture))))))))

(defdescribe
  writer-liveness-test
  "The journal writer loop EXITS on interrupt. A `writer` atom still holding that
   corpse used to make every later durable `publish!` block the full timeout and
   then vanish — the process silently stops journalling (no cross-process mirror,
   no orphan-reap terminal) for the rest of its life."
  (it "revives a dead writer instead of dropping every later durable event"
      (with-temp-journal
        (fn [_capture _write!]
          (bus/publish! "sid-writer" {"type" "turn.started" "seq" 1} {:store? true})
          (let [dead @(var-get #'bus/writer)]
            (.interrupt ^Thread dead)
            (Thread/sleep 200)
            (expect (not (.isAlive ^Thread dead)))
            (let
              [t0 (System/currentTimeMillis)
               _ (bus/publish! "sid-writer" {"type" "turn.completed" "seq" 2} {:store? true})
               ms (- (System/currentTimeMillis) t0)]

              (expect (< ms 2000))
              (expect (not (identical? dead @(var-get #'bus/writer))))
              (Thread/sleep 200)
              (expect (= ["turn.started" "turn.completed"]
                         (mapv #(get (wire/parse-json %) "type")
                               (remove str/blank?
                                 (str/split-lines (slurp (#'bus/session-file
                                                          "sid-writer")))))))))))))
