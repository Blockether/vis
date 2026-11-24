(ns com.blockether.vis.internal.gateway.journal-test
  "Cross-process journal hydration: a watcher joining a session mid-turn must
   MIRROR a turn genuinely streaming in a live sibling process, but must never
   RESURRECT one orphaned by a crashed/restarted daemon. Resurrecting an orphan
   pins the reader's `:current-turn` to a turn that will never emit a terminal —
   wedging the session queue (new sends stall `queued`) and spinning every
   watcher forever. The pid-liveness gate is what tells the two apart."
  (:require [clojure.string :as str]
            [com.blockether.vis.test-temp :as test-temp]
            [com.blockether.vis.internal.gateway.journal :as journal]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-temp-journal
  "Run `f` with the bus journal dir redirected to a throwaway temp dir and the
   deliver-fn capturing every ingested event into the returned atom. Restores
   the previous deliver-fn afterwards. `f` receives `[capture write-journal!]`
   where `write-journal!` spits ndjson lines for a sid."
  [f]
  (let [tmp
        (.toPath (test-temp/temp-dir "bus-test"))

        capture
        (atom [])

        prev
        @(var-get #'journal/deliver-fn)]

    (with-redefs [journal/events-dir
                  (fn []
                    tmp)

                  ;; In production the liveness index is a SIBLING of the journal dir; give
                  ;; it a home inside the throwaway one so no test ever marks a session live
                  ;; in the developer's own ~/.vis.
                  journal/live-dir
                  (fn []
                    (.resolve ^java.nio.file.Path tmp "live"))

                  journal/waiting-dir
                  (fn []
                    (.resolve ^java.nio.file.Path tmp "waiting"))]

      ;; a fresh journal dir is a fresh world: drop this process's orphan-reap
      ;; markers, tail cursors and cached liveness so re-running in one JVM starts
      ;; clean
      (reset! (var-get #'journal/reaped-turns) {})
      (reset! (var-get #'journal/tails) {})
      (reset! (var-get #'journal/live-cache) {})
      (reset! (var-get #'journal/waiting-cache) {})
      (journal/set-deliver-fn! (fn [_sid _store? ev]
                             (swap! capture conj ev)))
      (try (f capture
              (fn write-journal! [sid events]
                (let [file (#'journal/session-file sid)]
                  (spit file (str/join (map #(str (wire/json-str %) "\n") events))))))
           (finally (journal/set-deliver-fn! prev))))))

(defdescribe canonical-journal-encoding-test
             (it "writes a canonical event without normalizing it again"
                 (with-temp-journal
                   (fn [_capture _write!]
                     (let [sid
                           "canonical-journal"

                           event
                           (wire/canonical {:schema 1
                                            :seq 1
                                            :type "content.delta"
                                            :nested {:is-live true :ratio (/ 0.0 0.0)}})]

                       (with-redefs [wire/->wire (fn [_]
                                                   (throw (ex-info "reconverted" {})))]
                         (#'journal/write-event! sid event {:store? true}))
                       (let [row (wire/parse-json (first (str/split-lines (slurp (#'journal/session-file
                                                                                  sid)))))]
                         (expect (= event (select-keys row (keys event))))
                         (expect (true? (get row "_store")))))))))

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
                           (let [prod
                                 (str (java.util.UUID/randomUUID))

                                 live-pid
                                 (journal/producer-pid)]

                             (write! "sid-live"
                                     [(turn-started prod live-pid "sid-live" "T-live")
                                      (delta prod live-pid "T-live")])
                             (journal/hydrate! "sid-live")
                             (expect (= ["turn.started" "content.delta"]
                                        (mapv #(get % "type") @capture)))))))
  (it "REAPS an orphan (dead producer) with a synthetic terminal, never resurrects it"
      (with-temp-journal (fn [capture write!]
                           (let [prod (str (java.util.UUID/randomUUID))]
                             (write! "sid-orphan"
                                     [(turn-started prod dead-pid "sid-orphan" "T-orphan")
                                      (delta prod dead-pid "T-orphan")])
                             (journal/hydrate! "sid-orphan")
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
          (let [prod
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
            (journal/hydrate! sid)
            (expect (empty? @capture))))))
  (it "is idempotent: a second hydrate of a reaped journal delivers nothing"
      (with-temp-journal (fn [capture write!]
                           (let [prod (str (java.util.UUID/randomUUID))]
                             (write! "sid-idem" [(turn-started prod dead-pid "sid-idem" "T-idem")])
                             (journal/hydrate! "sid-idem") ; reaps, appends terminal to journal
                             (reset! capture [])
                             (journal/hydrate! "sid-idem") ; terminal present now -> no-op
                             (expect (empty? @capture))))))
  (it "reaps ONCE even before the terminal lands in the journal (async publish! race)"
      (with-temp-journal (fn [capture write!]
                           (let [prod (str (java.util.UUID/randomUUID))]
                             (write! "sid-race" [(turn-started prod dead-pid "sid-race" "T-race")])
                             ;; `publish!` is ASYNC: hold the terminal out of the journal to stand
                             ;; in for its write window. Without the CAS marker both hydrates read
                             ;; `terminal? = false` and each emit their own `turn.failed`.
                             (with-redefs [journal/publish! (fn [& _]
                                                          nil)]
                               (journal/hydrate! "sid-race")
                               (journal/hydrate! "sid-race"))
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
                           (expect (= 6 (journal/journal-high-water-seq "sid-hw")))))))
  (it "is 0 when the session has no journal file yet"
      (with-temp-journal (fn [_capture _write!]
                           (expect (= 0 (journal/journal-high-water-seq "sid-none")))))))

;; An app frame skips the journal. A restart that seeded only from the journal
;; numbered below a client's cursor, and the client dropped the next turn.
(defdescribe
  unjournaled-seq-floor-test
  (it "keeps a restart above the seq of an event that skipped the journal"
      (with-temp-journal (fn [_capture write!]
                           (reset! (var-get #'journal/floors) {})
                           (write! "sid-floor" [(turn-started "prod-A" 1 "sid-floor" "T-f")])
                           (journal/note-unjournaled! "sid-floor" 900)
                           ;; A durable event waits for the FIFO writer, so the floor is on disk.
                           (journal/publish! "sid-floor" {"type" "session.title_updated" "seq" 901})
                           ;; A restart forgets the floors of this process; the file keeps them.
                           (reset! (var-get #'journal/floors) {})
                           (expect (< 901 (journal/journal-high-water-seq "sid-floor"))))))
  (it "writes no floor for a frame that the last floor already covers"
      (with-temp-journal (fn [_capture _write!]
                           (reset! (var-get #'journal/floors) {})
                           (journal/note-unjournaled! "sid-floor-2" 10)
                           (journal/publish! "sid-floor-2" {"type" "session.title_updated" "seq" 11})
                           (let [f
                                 (#'journal/floor-file "sid-floor-2")

                                 floor
                                 (slurp f)]

                             (.delete ^java.io.File f)
                             (journal/note-unjournaled! "sid-floor-2" 12)
                             (journal/publish! "sid-floor-2" {"type" "session.title_updated" "seq" 13})
                             (expect (not (.exists ^java.io.File f)))
                             (expect (< 13 (parse-long floor))))))))

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
        (let [prod
              (str (java.util.UUID/randomUUID))

              sid
              "sid-cursor"

              f
              (#'journal/session-file sid)]

          (write! sid [(turn-started prod (journal/producer-pid) sid "T-cursor")])
          (let [gate
                (promise)

                orig
                @#'journal/deliver-line!

                ;; park the drain mid-file, holding the cursor it read at entry
                drain
                (future (with-redefs [journal/deliver-line! (fn [s l]
                                                          @gate
                                                          (orig s l))]
                          (#'journal/drain-file! sid f)))]

            (Thread/sleep 100)
            ;; producer appends while the drain is parked, then a subscriber hydrates
            (spit f
                  (str (wire/json-str (delta prod (journal/producer-pid) "T-cursor")) "\n")
                  :append
                  true)
            (future (journal/hydrate! sid))
            (Thread/sleep 100)
            (deliver gate true)
            @drain
            (Thread/sleep 200)
            (expect (= (.length f) (long (get-in @(var-get #'journal/tails) [sid :off]))))
            (reset! capture [])
            (#'journal/drain-file! sid f)
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
          (journal/publish! "sid-writer" {"type" "turn.started" "seq" 1} {:store? true})
          (let [dead @(var-get #'journal/writer)]
            (.interrupt ^Thread dead)
            (Thread/sleep 200)
            (expect (not (.isAlive ^Thread dead)))
            (let [t0 (System/currentTimeMillis)
                  _ (journal/publish! "sid-writer" {"type" "turn.completed" "seq" 2} {:store? true})
                  ms (- (System/currentTimeMillis) t0)]

              (expect (< ms 2000))
              (expect (not (identical? dead @(var-get #'journal/writer))))
              (Thread/sleep 200)
              (expect (= ["turn.started" "turn.completed"]
                         (mapv #(get (wire/parse-json %) "type")
                               (remove str/blank?
                                 (str/split-lines (slurp (#'journal/session-file
                                                          "sid-writer")))))))))))))

(defdescribe
  writer-start-failure-test
  "Starting the writer is gated by a `::starting` sentinel in the `writer` atom.
   If the thread cannot be created (thread limit / OOME) AFTER the gate is taken,
   leaving that sentinel behind wedges the gate FOREVER: no later `start-writer!`
   ever passes it, so every durable `publish!` for the rest of the process burns
   the full 5 s queue timeout and then vanishes — journalling silently dead."
  (it
    "recovers when the writer thread cannot be created"
    (with-temp-journal
      (fn [_capture _write!]
        (let [cur @(var-get #'journal/writer)]
          (when (instance? Thread cur) (.interrupt ^Thread cur))
          (Thread/sleep 100)
          (reset! (var-get #'journal/writer) nil))
        (with-redefs [journal/durable-write-timeout-ms
                      400

                      journal/spawn-writer-thread!
                      (fn []
                        (throw (OutOfMemoryError. "unable to create native thread")))]

          (journal/publish! "sid-start-fail" {"type" "turn.started" "seq" 1} {:store? true}))
        ;; the gate must be released, not stranded
        (expect (not= ::journal/starting @(var-get #'journal/writer)))
        (let [t0
              (System/currentTimeMillis)

              _
              (journal/publish! "sid-start-fail" {"type" "turn.completed" "seq" 2} {:store? true})

              ms
              (- (System/currentTimeMillis) t0)]

          (expect (< ms 2000))
          (expect (.isAlive ^Thread @(var-get #'journal/writer)))
          (Thread/sleep 200)
          (expect (= ["turn.started" "turn.completed"]
                     (mapv #(get (wire/parse-json %) "type")
                           (remove str/blank?
                             (str/split-lines (slurp (#'journal/session-file
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
        (let [prod
              (str (java.util.UUID/randomUUID))

              pid
              (journal/producer-pid)

              sid
              "sid-generation"

              f
              (#'journal/session-file sid)]

          ;; Turn 1: drained normally, cursor lands at EOF.
          (write! sid
                  [(turn-started prod pid sid "T-old") (delta prod pid "T-old")
                   (delta prod pid "T-old")])
          (#'journal/drain-file! sid f)
          (expect (= 3 (count @capture)))
          (let [off1 (long (get-in @(var-get #'journal/tails) [sid :off]))]
            (expect (= (.length f) off1))
            (reset! capture [])
            ;; Turn 2: the producer truncates and rewrites LONGER than turn 1,
            ;; so `len > off` — indistinguishable from an append by length.
            (write! sid
                    (into [(assoc (turn-started prod pid sid "T-new") :seq 99)]
                          (repeat 4 (delta prod pid "T-new"))))
            (expect (> (.length f) off1))
            (#'journal/drain-file! sid f)
            ;; The whole new generation is delivered, head first.
            (expect (= 5 (count @capture)))
            (expect (= "turn.started" (get (first @capture) "type")))
            (expect (= "T-new" (get (first @capture) "turn_id")))
            (expect (= (.length f) (long (get-in @(var-get #'journal/tails) [sid :off]))))
            ;; …and exactly once: a same-generation re-drain delivers nothing.
            (reset! capture [])
            (#'journal/drain-file! sid f)
            (expect (empty? @capture))))))))

(defdescribe
  partial-line-test
  "A sibling caught MID-WRITE leaves a half-written trailing line. `hydrate!` must
   claim only COMPLETE lines: claiming to EOF made the tailer resume INSIDE that
   line, so the event was lost outright the moment it finished landing."
  (it "delivers the event that was half-written when the reader joined"
      (with-temp-journal
        (fn [capture _write!]
          (let [sid
                "partial"

                f
                (#'journal/session-file sid)

                prod
                (str (random-uuid))

                pid
                (.pid (java.lang.ProcessHandle/current))

                whole
                (str (wire/json-str (assoc (delta prod pid "T") :text "IMPORTANT")) "\n")

                cut
                (quot (count whole) 2)]

            (spit f (str (wire/json-str (turn-started prod pid sid "T")) "\n" (subs whole 0 cut)))
            (journal/hydrate! sid)
            (expect (= ["turn.started"] (mapv #(get % "type") @capture)))
            (expect (< (long (get-in @(var-get #'journal/tails) [sid :off])) (.length f)))
            ;; the sibling finishes the line
            (spit f (subs whole cut) :append true)
            (reset! capture [])
            (#'journal/drain-file! sid f)
            (expect (= ["IMPORTANT"] (mapv #(get % "text") @capture))))))))

(defdescribe
  closed-session-test
  "`forget!` drops a session's tail. A drain that still had the closed sid in
   flight must NOT re-create it: `sweep!` only forgets sids whose FILE it deletes,
   so a resurrected entry would sit in the map for the daemon's whole life."
  (it "leaves no tail entry behind after the journal is gone"
      (with-temp-journal (fn [_capture write-journal!]
                           (let [sid
                                 "closed"

                                 prod
                                 (str (random-uuid))

                                 pid
                                 (.pid (java.lang.ProcessHandle/current))]

                             (write-journal! sid [(turn-started prod pid sid "T")])
                             (#'journal/drain-file! sid (#'journal/session-file sid))
                             (expect (contains? @(var-get #'journal/tails) sid))
                             (journal/forget! sid)
                             (#'journal/drain-file! sid (#'journal/session-file sid))
                             (expect (not (contains? @(var-get #'journal/tails) sid))))))))

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
          (let [sid
                "capped"

                f
                (#'journal/session-file sid)

                blob
                (apply str (repeat (* 6 1024 1024) "x"))]

            (#'journal/write-event!
             sid
             {"schema" 1 "type" "turn.started" "turn_id" "T7" "session_id" sid "seq" 1}
             {:store? true :truncate? true})
            (dotimes [i 4]
              (#'journal/write-event!
               sid
               {"schema" 1 "type" "content.delta" "turn_id" "T7" "seq" (+ 2 i) "text" blob}
               {:store? true}))
            (let [head (wire/parse-json (first (str/split-lines (slurp f))))]
              (expect (= "turn.started" (get head "type")))
              (expect (= "T7" (get head "turn_id"))))))))
  (it "reaps an orphan whose `turn.started` the cap already dropped"
      (with-temp-journal
        (fn [capture write-journal!]
          (let [sid
                "capped-orphan"

                prod
                (str (random-uuid))]

            (write-journal! sid [(assoc (delta prod dead-pid "T9") :session_id sid)])
            (journal/hydrate! sid)
            (expect (= [["turn.failed" "T9"]]
                       (mapv #(vector (get % "type") (get % "turn_id")) @capture))))))))

;; Polling audit B2: the tailer counted the lines that its own process wrote as work, so each
;; local turn kept it at the 100 ms rate and it read back every byte that it wrote.
(defdescribe
  own-journal-lines-are-not-tail-work-test
  (it "a drain of only own lines is idle, and a caught-up tail skips the next own line"
      (with-temp-journal
        (fn [capture _]
          (let [sid
                (str (random-uuid))

                f
                (#'journal/session-file sid)

                own
                (fn [n]
                  (#'journal/write-event!
                   sid
                   {"type" "content.delta" "seq" n "session_id" sid}
                   {:store? true}))

                off
                #(:off (get @(var-get #'journal/tails) sid))]

            (own 1)
            (expect (not (#'journal/drain-file! sid f)) "Own lines are no work for the tailer")
            (expect (= (.length f) (off)))
            (own 2)
            (expect (= (.length f) (off)) "The writer moved the caught-up tail past its line")
            (spit f
                  (str (wire/json-str (assoc (delta "other-process" dead-pid "T1") :session_id sid))
                       "\n")
                  :append
                  true)
            (expect (true? (#'journal/drain-file! sid f)) "A line of another process is work")
            (expect (= ["content.delta"] (mapv #(get % "type") @capture))))))))

;; Regression: the companion sat on "thinking" forever while the turn ran in
;; another process. `hydrate!` mirrored the turn as running (it keeps the
;; CALLER's sid), then every live delta and the terminal event were dropped.
(defdescribe
  mirrored-sid-identity-test
  "The tailer must hand the deliver-fn the sid this process NAMED as relevant —
   its own registry key — not a lookalike re-derived from the journal FILE NAME.
   The gateway registry is UUID-keyed (`path-sid`), so a String sid made
   `state/ingest-mirrored-event!`'s `(contains? @registry sid)` false for every
   mirrored event and discarded a whole sibling turn without a word."
  (it "delivers a mirrored event under the very sid the process asked for"
      (with-temp-journal
        (fn [_capture write!]
          (let [sid
                (java.util.UUID/randomUUID)

                seen
                (atom [])

                prod
                (str (random-uuid))

                poll-thread
                (Thread/currentThread)

                pid
                (.pid (java.lang.ProcessHandle/current))

                prev-relevant-sids
                @(var-get #'journal/relevant-sids-fn)]

            (journal/set-deliver-fn! (fn [s _store? _ev]
                                   (swap! seen conj s)))
            (journal/set-relevant-sids-fn! (fn []
                                         (when (identical? poll-thread (Thread/currentThread))
                                           [sid])))
            (try (write! sid [(turn-started prod pid (str sid) "T-mirror")])
                 ;; A tailer started by an earlier suite test must not consume this fixture.
                 (expect (false? @(future (#'journal/poll-once!))))
                 (expect (true? (#'journal/poll-once!)))
                 (expect (= [sid] @seen))
                 (finally (journal/set-relevant-sids-fn! prev-relevant-sids))))))))

(defdescribe
  fleet-liveness-test
  "Liveness is a fact about the MACHINE, not about one process. `live-turns` is
   the cross-process index every producer writes at `turn.started` and retracts at
   the terminal, so a gateway can answer \"is this session running\" for a turn it
   is not running itself and has never subscribed to."
  ;; Regression: two Vis apps (desktop and phone) reading the SAME gateway showed
  ;; different sets of live sessions. `/v1/sessions` derived `live` from this
  ;; process's in-memory registry, which learns about a sibling process's turn only
  ;; when somebody SSE-subscribes to that session — so each app lit up exactly the
  ;; sessions it had happened to open, and both under-reported the fleet.
  (it "reports a published turn with no subscriber, no hydrate and no tailer delivery"
      (with-temp-journal
        (fn [_capture _write!]
          (let [sid (str (random-uuid))]
            (expect (nil? (journal/live-turn-id sid)))
            (journal/publish!
              sid
              {"schema" 1 "seq" 1 "type" "turn.started" "session_id" sid "turn_id" "T-1"}
              {:store? true :truncate? true})
            (expect (= "T-1" (journal/live-turn-id sid)))
            (expect (= {sid "T-1"} (journal/live-turns)))))))
  (it "retracts it on the terminal, so a finished session never stays lit"
      (with-temp-journal
        (fn [_capture _write!]
          (let [sid (str (random-uuid))]
            (journal/publish!
              sid
              {"schema" 1 "seq" 1 "type" "turn.started" "session_id" sid "turn_id" "T-2"}
              {:store? true :truncate? true})
            (expect (= "T-2" (journal/live-turn-id sid)))
            (journal/publish!
              sid
              {"schema" 1 "seq" 2 "type" "turn.completed" "session_id" sid "turn_id" "T-2"}
              {:store? true})
            (expect (nil? (journal/live-turn-id sid)))))))
  (it
    "retracts it on a terminal published from an INTERRUPTED producer thread"
    ;; Regression: cancelling a Codex (Responses WebSocket) turn from the companion
    ;; app showed "Cancelled by user" for a second, then the bubble ran again and a
    ;; second Stop was accepted as if nothing had happened. The gateway cancels a
    ;; turn by interrupting its worker; the WebSocket transport keeps that
    ;; interrupt armed on the worker, so the worker reached its `turn.cancelled`
    ;; publish with the flag set. The durable hand-off blocks on an interruptible
    ;; queue offer, which threw at once, `publish!` swallowed it, and the journal
    ;; line - the SAME write that retracts the liveness marker - never happened.
    ;; Every `/v1/sessions` row then said `live` with a `current_turn_id`, so each
    ;; client re-adopted a turn that had already ended.
    (with-temp-journal
      (fn [_capture _write!]
        (let [sid (str (random-uuid))]
          (journal/publish! sid
                        {"schema" 1 "seq" 1 "type" "turn.started" "session_id" sid "turn_id" "T-4"}
                        {:store? true :truncate? true})
          (expect (= "T-4" (journal/live-turn-id sid)))
          (.interrupt (Thread/currentThread))
          (let [still-interrupted?
                (try (journal/publish!
                       sid
                       {"schema" 1 "seq" 2 "type" "turn.cancelled" "session_id" sid "turn_id" "T-4"}
                       {:store? true})
                     (.isInterrupted (Thread/currentThread))
                     (finally (Thread/interrupted)))]
            (expect (nil? (journal/live-turn-id sid)))
            (expect (str/includes? (slurp (#'journal/session-file sid)) "\"turn.cancelled\""))
            ;; The producer's cancellation is not the bus's to consume: the
            ;; interrupt still stands for the next interruptible call.
            (expect still-interrupted?))))))
  (it "retracts it on a terminal whose journal write was LOST, so a failed turn never stays lit"
      ;; Regression: a stall-watchdog cancel published `turn.failed`; the registry took
      ;; it, but the durable journal line never reached disk (seq 4 missing from the
      ;; ndjson), and the marker retract rode ONLY that write on the writer thread. The
      ;; session header then answered `live` with the dead turn for as long as the
      ;; gateway process lived. The retract belongs to the PRODUCER's publish, not to
      ;; the line landing.
      (with-temp-journal
        (fn [_capture _write!]
          (let [sid (str (random-uuid))]
            (journal/publish!
              sid
              {"schema" 1 "seq" 1 "type" "turn.started" "session_id" sid "turn_id" "T-5"}
              {:store? true :truncate? true})
            (expect (= "T-5" (journal/live-turn-id sid)))
            (let [real-write! @#'journal/write-event!]
              (with-redefs [journal/write-event! (fn [sid event opts]
                                               (when-not (= "turn.failed" (get event "type"))
                                                 (real-write! sid event opts)))]
                (journal/publish!
                  sid
                  {"schema" 1 "seq" 2 "type" "turn.failed" "session_id" sid "turn_id" "T-5"}
                  {:store? true})))
            (expect (not (str/includes? (slurp (#'journal/session-file sid)) "\"turn.failed\"")))
            (expect (nil? (journal/live-turn-id sid)))))))
  (it "ignores AND deletes a marker whose producer process is gone"
      (with-temp-journal
        (fn [_capture _write!]
          (let [sid
                (str (random-uuid))

                dir
                ^java.nio.file.Path (#'journal/live-dir)

                f
                (.toFile (.resolve dir (str sid ".json")))]

            (java.nio.file.Files/createDirectories dir
                                                   (make-array java.nio.file.attribute.FileAttribute
                                                               0))
            (spit f
                  (wire/json-str {"schema" 1 "session_id" sid "turn_id" "T-orphan" "pid" dead-pid}))
            (reset! (var-get #'journal/live-cache) {})
            ;; A crashed producer owes a retract it can no longer make, so the
            ;; reader retracts for it — otherwise one kill -9 lights a session up
            ;; forever on every client of this machine.
            (expect (nil? (journal/live-turn-id sid)))
            (expect (not (.exists f)))))))
  (it "drops the marker when the session is forgotten"
      (with-temp-journal
        (fn [_capture _write!]
          (let [sid (str (random-uuid))]
            (journal/publish!
              sid
              {"schema" 1 "seq" 1 "type" "turn.started" "session_id" sid "turn_id" "T-3"}
              {:store? true :truncate? true})
            (expect (= "T-3" (journal/live-turn-id sid)))
            (journal/forget! sid)
            (expect (nil? (journal/live-turn-id sid))))))))

(defdescribe
  fleet-waiting-index-test
  "Which sessions are BLOCKED on a human, for every process on this machine.
   The engine's pending registry is process-local — only the process that raised
   a request can settle it — so a gateway answering from it lit up exactly the
   requests it happened to own, and a phone listing the fleet was never told
   that a run had been standing on the operator for an hour."
  (it "marks the session on the request event and retracts it on the close"
      (with-temp-journal
        (fn [_capture _write!]
          (let [sid (str (random-uuid))]
            (expect (not (journal/session-waiting? sid)))
            (journal/publish! sid
                          {"schema" 1
                           "seq" 1
                           "type" "view.open"
                           "kind" "input"
                           "session_id" sid
                           "view" {"id" "req-1" "title" "Which branch?"}}
                          {:store? true})
            (expect (journal/session-waiting? sid))
            (expect (= ["req-1"] (mapv #(get % "id") (get (journal/waiting-requests) sid))))
            (journal/publish! sid
                          {"schema" 1
                           "seq" 2
                           "type" "view.close"
                           "kind" "input"
                           "session_id" sid
                           "view_id" "req-1"}
                          {:store? true})
            (expect (not (journal/session-waiting? sid)))))))
  (it "keeps waiting while a SECOND request of the same run is still open"
      (with-temp-journal
        (fn [_capture _write!]
          (let [sid (str (random-uuid))]
            (doseq [rid ["req-a" "req-b"]]
              (journal/publish! sid
                            {"schema" 1
                             "seq" 1
                             "type" "view.open"
                             "kind" "input"
                             "session_id" sid
                             "view" {"id" rid}}
                            {:store? true}))
            (expect (= ["req-a" "req-b"] (mapv #(get % "id") (get (journal/waiting-requests) sid))))
            (expect (= 2 (journal/session-waiting-count sid)))
            (journal/publish! sid
                          {"schema" 1
                           "seq" 2
                           "type" "view.close"
                           "kind" "input"
                           "session_id" sid
                           "view_id" "req-a"}
                          {:store? true})
            (expect (journal/session-waiting? sid))
            ;; The flag alone cannot say that answering one of the two moved
            ;; anything, which is exactly what a lit badge looked like. The
            ;; COUNT is what a surface counts down.
            (expect (= 1 (journal/session-waiting-count sid)))
            (expect (= ["req-b"] (mapv #(get % "id") (get (journal/waiting-requests) sid))))))))
  (it "drops the demand at the turn's terminal, answered or not"
      (with-temp-journal
        (fn [_capture _write!]
          ;; A cancelled or failed turn takes its unanswered dialog with it. No
          ;; close event is coming for a request nobody will ever settle, so a
          ;; terminal is the last chance to retract the demand.
          (let [sid (str (random-uuid))]
            (journal/publish! sid
                          {"schema" 1
                           "seq" 1
                           "type" "view.open"
                           "kind" "input"
                           "session_id" sid
                           "view" {"id" "req-abandoned"}}
                          {:store? true})
            (expect (journal/session-waiting? sid))
            (journal/publish!
              sid
              {"schema" 1 "seq" 2 "type" "turn.cancelled" "session_id" sid "turn_id" "T-9"}
              {:store? true})
            (expect (not (journal/session-waiting? sid)))))))
  (it
    "keeps the form of a subagent through the terminal of the leader that shows it"
    (with-temp-journal
      (fn [_capture _write!]
        ;; Issue #494: the leader session shows the form of its subagent. The turn
        ;; of the leader can end while the subagent still waits for the person.
        (let [leader
              (str (random-uuid))

              child
              (str (random-uuid))

              open!
              (fn [seq-no rid raiser]
                (journal/publish! leader
                              {"schema" 1
                               "seq" seq-no
                               "type" "view.open"
                               "kind" "input"
                               "session_id" leader
                               "view" {"id" rid "session_id" raiser}}
                              {:store? true}))

              terminal!
              (fn [sid seq-no]
                (journal/publish!
                  sid
                  {"schema" 1 "seq" seq-no "type" "turn.cancelled" "session_id" sid "turn_id" "T"}
                  {:store? true}))]

          (open! 1 "req-own" leader)
          (open! 2 "req-child" child)
          (expect (= 2 (journal/session-waiting-count leader)))
          (expect (= [1 1] (mapv journal/raised-waiting-count [leader child])))
          (terminal! leader 3)
          (expect (= ["req-child"] (mapv #(get % "id") (get (journal/waiting-requests) leader))))
          (expect (= [0 1] (mapv journal/raised-waiting-count [leader child])))
          ;; The terminal of the subagent takes its form out of the leader marker.
          (terminal! child 1)
          (expect (not (journal/session-waiting? leader)))
          (expect (zero? (long (journal/raised-waiting-count child))))))))
  (it "ignores AND deletes a marker whose producer process is gone"
      (with-temp-journal
        (fn [_capture _write!]
          (let [sid
                (str (random-uuid))

                dir
                ^java.nio.file.Path (#'journal/waiting-dir)

                f
                (.toFile (.resolve dir (str sid ".json")))]

            (java.nio.file.Files/createDirectories dir
                                                   (make-array java.nio.file.attribute.FileAttribute
                                                               0))
            (spit f
                  (wire/json-str {"schema" 1
                                  "session_id" sid
                                  "pid" dead-pid
                                  "requests" [{"id" "req-orphan" "since" 1}]}))
            (reset! (var-get #'journal/waiting-cache) {})
            ;; A dead process cannot answer its own dialog, and cannot retract
            ;; it either — without this reap one crash leaves a session demanding
            ;; input forever on every client of this machine.
            (expect (not (journal/session-waiting? sid)))
            (expect (not (.exists f)))))))
  (it "drops the marker when the session is forgotten"
      (with-temp-journal (fn [_capture _write!]
                           (let [sid (str (random-uuid))]
                             (journal/publish! sid
                                           {"schema" 1
                                            "seq" 1
                                            "type" "view.open"
                                            "kind" "input"
                                            "session_id" sid
                                            "view" {"id" "req-forgotten"}}
                                           {:store? true})
                             (expect (journal/session-waiting? sid))
                             (journal/forget! sid)
                             (expect (not (journal/session-waiting? sid))))))))

(defdescribe
  terminal-tail-test
  ;; Regression, gateway CPU audit: `hydrate!` read and JSON-parsed a session's
  ;; WHOLE journal - 82 ms for a 7 MB one, under the sid's tail lock, once per
  ;; subscriber per session - to learn that the turn had already ended. The
  ;; answer sits in the last few lines. The tail read must be SOUND: yes only
  ;; when the full parse would also say yes, so it can never skip a live turn.
  (let [tail?
        (var-get #'journal/terminal-tail?)

        line
        (fn [type]
          (wire/json-str {:schema 1 :seq 1 :type type}))

        journal!
        (fn [sid text]
          (let [f (#'journal/session-file sid)]
            (spit f text)
            f))]

    (it "requires a newline even when the terminal JSON is already complete"
        ;; Regression: JSON validity is not the journal's complete-line boundary.
        (with-temp-journal (fn [_ _]
                             (doseq [type
                                     ["turn.completed" "turn.failed" "turn.cancelled"]

                                     prefix
                                     ["" (str (line "turn.started") "\n")
                                      (apply str (repeat 2000 (str (line "content.delta") "\n")))]]

                               (let [f (journal! "tail-no-newline" (str prefix (line type)))]
                                 (expect (false? (tail? f)))
                                 (spit f "\n" :append true)
                                 (expect (true? (tail? f))))))))
    (it "hydrates the live prefix until the terminal newline arrives"
        (with-temp-journal
          (fn [capture _]
            (let [sid
                  "tail-hydrate-no-newline"

                  started
                  (turn-started "sibling" (journal/producer-pid) sid "T")

                  f
                  (journal! sid (str (wire/json-str started) "\n" (line "turn.completed")))]

              (journal/hydrate! sid)
              (expect (= ["turn.started"] (mapv #(get % "type") @capture)))
              (expect (< (long (get-in @(var-get #'journal/tails) [sid :off] 0)) (.length f)))
              (spit f "\n" :append true)
              (reset! capture [])
              (journal/hydrate! sid)
              (expect (empty? @capture))))))
    (it "answers yes when the terminal is followed by what the producer appends after it"
        (with-temp-journal (fn [_ _]
                             (let [f (journal! "tail-title"
                                               (str (line "turn.started")
                                                    "\n"
                                                    (line "content.delta")
                                                    "\n"
                                                    (line "turn.completed")
                                                    "\n"
                                                    (line "session.title_updated")
                                                    "\n"))]
                               (expect (true? (tail? f)))))))
    (it "ignores a trailing partial line a producer is still writing"
        (with-temp-journal (fn [_ _]
                             (let [f (journal! "tail-partial"
                                               (str (line "turn.started")
                                                    "\n" (line "turn.failed")
                                                    "\n" "{\"schema\":1,\"type\":\"conte"))]
                               (expect (true? (tail? f)))))))
    (it "answers no for a turn still in flight, so the full path takes over"
        (with-temp-journal
          (fn [_ _]
            (let [f (journal! "tail-live"
                              (str (line "turn.started") "\n" (line "content.delta") "\n"))]
              (expect (false? (tail? f)))))))
    (it "finds a terminal inside the window of a journal larger than it"
        (with-temp-journal (fn [_ _]
                             (let [filler
                                   (apply str (repeat 2000 (str (line "content.delta") "\n")))

                                   f
                                   (journal! "tail-big"
                                             (str (line "turn.started")
                                                  "\n"
                                                  filler
                                                  (line "turn.cancelled")
                                                  "\n"
                                                  (line "session.title_updated")
                                                  "\n"))]

                               (expect (> (.length f) (long (var-get #'journal/TAIL_SCAN_BYTES))))
                               (expect (true? (tail? f)))))))
    (it "answers no - never a false yes - when the terminal sits beyond the window"
        (with-temp-journal
          (fn [_ _]
            (let [filler
                  (apply str (repeat 2000 (str (line "content.delta") "\n")))

                  f
                  (journal! "tail-buried"
                            (str (line "turn.started") "\n" (line "turn.completed") "\n" filler))]

              (expect (false? (tail? f)))))))
    (it "keeps a finished journal out of the full path entirely"
        (with-temp-journal (fn [capture _]
                             (let [f
                                   (journal!
                                     "tail-skip"
                                     (str (line "turn.started") "\n" (line "turn.completed") "\n"))

                                   reads
                                   (atom 0)]

                               (with-redefs [journal/set-tail! (fn [& _]
                                                             (swap! reads inc)
                                                             nil)]
                                 (journal/hydrate! "tail-skip"))
                               (expect (.isFile f))
                               (expect (zero? @reads))
                               (expect (empty? @capture))))))))

(defdescribe producer-identity-is-this-process-test
             ;; Regression: `producer-id` and `producer-pid` were top-level VALUES, and a
             ;; native image evaluates top-level forms at BUILD time — so every process
             ;; started from one binary published under the same producer id and the same
             ;; (build-time, long dead) pid. Siblings then read each other's journal lines
             ;; as their own, and every liveness marker looked orphaned to the next process
             ;; that scanned the directory: `live-turns` lost running sessions and a project
             ;; header counted fewer live sessions than the rows under it showed LIVE.
             ;; `native-producer-identity-test` guards the linked image; this guards the
             ;; contract.
             (it "stamps the RUNNING process's pid"
                 (expect (= (.pid (java.lang.ProcessHandle/current)) (journal/producer-pid))))
             (it "builds the self marker from this process's own producer id"
                 (expect (= (str "\"_producer\":\"" (journal/producer-id) "\"")
                            @(var-get #'journal/self-marker)))))
