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
      ;; markers and tail cursors so re-running in one JVM starts clean
      (reset! (var-get #'bus/reaped-turns) {})
      (reset! (var-get #'bus/tails) {})
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
            (expect (= (.length f) (long (get-in @(var-get #'bus/tails) [sid :off]))))
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

(defdescribe
  writer-start-failure-test
  "Starting the writer is gated by a `::starting` sentinel in the `writer` atom.
   If the thread cannot be created (thread limit / OOME) AFTER the gate is taken,
   leaving that sentinel behind wedges the gate FOREVER: no later `start-writer!`
   ever passes it, so every durable `publish!` for the rest of the process burns
   the full 5 s queue timeout and then vanishes — journalling silently dead."
  (it "recovers when the writer thread cannot be created"
      (with-temp-journal
        (fn [_capture _write!]
          (let [cur @(var-get #'bus/writer)]
            (when (instance? Thread cur) (.interrupt ^Thread cur))
            (Thread/sleep 100)
            (reset! (var-get #'bus/writer) nil))
          (with-redefs
            [bus/spawn-writer-thread! (fn []
                                        (throw (OutOfMemoryError.
                                                 "unable to create native thread")))]
            (bus/publish! "sid-start-fail" {"type" "turn.started" "seq" 1} {:store? true}))
          ;; the gate must be released, not stranded
          (expect (not= ::bus/starting @(var-get #'bus/writer)))
          (let
            [t0
             (System/currentTimeMillis)

             _
             (bus/publish! "sid-start-fail" {"type" "turn.completed" "seq" 2} {:store? true})

             ms
             (- (System/currentTimeMillis) t0)]

            (expect (< ms 2000))
            (expect (.isAlive ^Thread @(var-get #'bus/writer)))
            (Thread/sleep 200)
            (expect (= ["turn.started" "turn.completed"]
                       (mapv #(get (wire/parse-json %) "type")
                             (remove str/blank?
                               (str/split-lines (slurp (#'bus/session-file
                                                        "sid-start-fail"))))))))))))

(defdescribe
  journal-generation-test
  "The producer TRUNCATES the journal at every `turn.started`. A rewrite that
   regrows PAST the tail cursor inside one poll interval looks like an append by
   LENGTH alone, so the new turn's head — `turn.started` included — would be
   skipped forever and the tab would sit frozen-idle through a live sibling turn."
  (it
    "replays from byte 0 when the journal's first bytes change"
    (with-temp-journal
      (fn [capture write!]
        (let
          [prod
           (str (java.util.UUID/randomUUID))

           pid
           (var-get #'bus/producer-pid)

           sid
           "sid-generation"

           f
           (#'bus/session-file sid)]

          ;; Turn 1: drained normally, cursor lands at EOF.
          (write! sid
                  [(turn-started prod pid sid "T-old") (delta prod pid "T-old")
                   (delta prod pid "T-old")])
          (#'bus/drain-file! f)
          (expect (= 3 (count @capture)))
          (let [off1 (long (get-in @(var-get #'bus/tails) [sid :off]))]
            (expect (= (.length f) off1))
            (reset! capture [])
            ;; Turn 2: the producer truncates and rewrites LONGER than turn 1,
            ;; so `len > off` — indistinguishable from an append by length.
            (write! sid
                    (into [(assoc (turn-started prod pid sid "T-new") :seq 99)]
                          (repeat 4 (delta prod pid "T-new"))))
            (expect (> (.length f) off1))
            (#'bus/drain-file! f)
            ;; The whole new generation is delivered, head first.
            (expect (= 5 (count @capture)))
            (expect (= "turn.started" (get (first @capture) "type")))
            (expect (= "T-new" (get (first @capture) "turn_id")))
            (expect (= (.length f) (long (get-in @(var-get #'bus/tails) [sid :off]))))
            ;; …and exactly once: a same-generation re-drain delivers nothing.
            (reset! capture [])
            (#'bus/drain-file! f)
            (expect (empty? @capture))))))))


(defdescribe
  partial-line-test
  "A sibling caught MID-WRITE leaves a half-written trailing line. `hydrate!` must
   claim only COMPLETE lines: claiming to EOF made the tailer resume INSIDE that
   line, so the event was lost outright the moment it finished landing."
  (it "delivers the event that was half-written when the reader joined"
      (with-temp-journal
        (fn [capture _write!]
          (let
            [sid
             "partial"

             f
             (#'bus/session-file sid)

             prod
             (str (random-uuid))

             pid
             (.pid (java.lang.ProcessHandle/current))

             whole
             (str (wire/json-str (assoc (delta prod pid "T") :text "IMPORTANT")) "\n")

             cut
             (quot (count whole) 2)]

            (spit f (str (wire/json-str (turn-started prod pid sid "T")) "\n" (subs whole 0 cut)))
            (bus/hydrate! sid)
            (expect (= ["turn.started"] (mapv #(get % "type") @capture)))
            (expect (< (long (get-in @(var-get #'bus/tails) [sid :off])) (.length f)))
            ;; the sibling finishes the line
            (spit f (subs whole cut) :append true)
            (reset! capture [])
            (#'bus/drain-file! f)
            (expect (= ["IMPORTANT"] (mapv #(get % "text") @capture))))))))

(defdescribe
  closed-session-test
  "`forget!` drops a session's tail. A drain that still had the closed sid in
   flight must NOT re-create it: `sweep!` only forgets sids whose FILE it deletes,
   so a resurrected entry would sit in the map for the daemon's whole life."
  (it "leaves no tail entry behind after the journal is gone"
      (with-temp-journal (fn [_capture write-journal!]
                           (let
                             [sid
                              "closed"

                              prod
                              (str (random-uuid))

                              pid
                              (.pid (java.lang.ProcessHandle/current))]

                             (write-journal! sid [(turn-started prod pid sid "T")])
                             (#'bus/drain-file! (#'bus/session-file sid))
                             (expect (contains? @(var-get #'bus/tails) sid))
                             (bus/forget! sid)
                             (#'bus/drain-file! (#'bus/session-file sid))
                             (expect (not (contains? @(var-get #'bus/tails) sid))))))))

(defdescribe
  size-cap-test
  "The journal is capped, and the cap can fire MID-turn. The first line is what
   tells every reader WHICH turn is in flight, so the cap must keep it: without
   it a >16MB turn had no `turn.started` to mirror (so `:current-turn` never got
   set and every new subscriber re-mirrored the same deltas) and no `turn_id` to
   reap (so a crashed producer wedged the queue forever)."
  (it "keeps `turn.started` as the first line after the cap truncates"
      (with-temp-journal
        (fn [_capture _write!]
          (let
            [sid
             "capped"

             f
             (#'bus/session-file sid)

             blob
             (apply str (repeat (* 6 1024 1024) "x"))]

            (#'bus/write-event!
             sid
             {"schema" 1 "type" "turn.started" "turn_id" "T7" "session_id" sid "seq" 1}
             {:store? true :truncate? true})
            (dotimes [i 4]
              (#'bus/write-event!
               sid
               {"schema" 1 "type" "content.delta" "turn_id" "T7" "seq" (+ 2 i) "text" blob}
               {:store? true}))
            (let [head (wire/parse-json (first (str/split-lines (slurp f))))]
              (expect (= "turn.started" (get head "type")))
              (expect (= "T7" (get head "turn_id"))))))))
  (it "reaps an orphan whose `turn.started` the cap already dropped"
      (with-temp-journal
        (fn [capture write-journal!]
          (let
            [sid
             "capped-orphan"

             prod
             (str (random-uuid))]

            (write-journal! sid [(assoc (delta prod dead-pid "T9") :session_id sid)])
            (bus/hydrate! sid)
            (expect (= [["turn.failed" "T9"]]
                       (mapv #(vector (get % "type") (get % "turn_id")) @capture))))))))
