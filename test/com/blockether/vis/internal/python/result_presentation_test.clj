(ns com.blockether.vis.internal.python.result-presentation-test
  "Model-facing representations across the host/embedded-Python boundary."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- check-result
  [data code]
  (tpc/with-own [ctx
                 {'fixture (fn []
                             data)}]
                (let [result (ep/run-python-block ctx (str "r = await fixture()\n" code))]
                  (expect (nil? (:error result)) (pr-str (:error result)))
                  (:stdout result))))

(defdescribe
  guest-source-version-isolation-test
  ;; A mixed-version suite reproduced a worker importing another engine's old discovery module.
  (it
    "keeps each engine's guest sources intact when another version starts"
    (let [root
          (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-guest-source-"
                     (make-array java.nio.file.attribute.FileAttribute 0)))

          sources
          {"vis_introspection.py" "VERSION = 1\n" "vis_results.py" "VERSION = 1\n"}]

      (try (let [first-dir
                 (#'worker/materialize-guest-sources! root sources)

                 second-dir
                 (#'worker/materialize-guest-sources!
                  root
                  (assoc sources "vis_introspection.py" "VERSION = 2\n"))]

             (expect (not= first-dir second-dir))
             ;; `<release>-<digest>`: a person reading `~/.vis/python/vis-guest`
             ;; sees which build left a tree there, and the digest keeps the NAME
             ;; the identity, so one build's edit never rewrites the modules a
             ;; live worker imports.
             (let [[_ first-release first-digest]
                   (re-matches #"(.+)-([0-9a-f]{12})" (.getName (io/file first-dir)))

                   [_ second-release second-digest]
                   (re-matches #"(.+)-([0-9a-f]{12})" (.getName (io/file second-dir)))]

               (expect (= first-release second-release))
               (expect (not= first-digest second-digest)))
             (expect (= "VERSION = 1\n" (slurp (io/file first-dir "vis_introspection.py"))))
             (expect (= "VERSION = 2\n" (slurp (io/file second-dir "vis_introspection.py"))))
             (expect (= first-dir (#'worker/materialize-guest-sources! root (reverse sources))))
             (expect (= "VERSION = 2\n" (slurp (io/file second-dir "vis_introspection.py")))))
           (finally (doseq [file (reverse (file-seq root))]
                      (io/delete-file file true)))))))

(defdescribe
  result-field-access-test
  ;; Session report fd553c3f-a123-4f63-afae-969e07c6f064: `h.transcript` raised
  ;; AttributeError on a read_session result whose `h['transcript']` answered, so the
  ;; field was fetched a second time instead of read where it already was.
  (it "reads a result field by dot as well as by key, at any depth"
      (let [out (check-result
                  {"op" "read_session" "session_id" "s-1" "transcript" {"turns" [{"index" 1}]}}
                  (str "print(r.transcript is r['transcript'], r.transcript.turns[0].index)\n"
                       "try:\n    r.transcripts\nexcept AttributeError as e:\n    print(e)"))]
        (expect (str/includes? out "True 1"))
        (expect (str/includes? out "'transcripts' is not a field of 'read_session' result"))
        (expect (str/includes? out "Keys: 'op', 'session_id', 'transcript'"))
        (expect (str/includes? out "Did you mean 'transcript'?")))))

(defdescribe
  compact-shell-result-test
  (it
    "prints the verdict and log without discarding any mapping data"
    (expect
      (=
        "shell demo: exited; exit=0; 12ms\nok\n"
        (check-result
          {"op" "shell"
           "id" "demo"
           "status" "exited"
           "exit" 0
           "duration_ms" 12
           "out" "ok\n"
           "cpu_ms" nil
           "note" nil}
          "assert isinstance(r, dict)\nassert r['cpu_ms'] is None\nassert json.loads(json.dumps(r)) == dict(r)\nassert callable(r.logs)\nprint(r)"))))
  (it "preserves wait-timeout, partial-page, refusal and stop diagnostics"
      (let [out (check-result {"op" "_shell_wait"
                               "id" "demo"
                               "status" "running"
                               "exit" nil
                               "duration_ms" 20
                               "timed_out" true
                               "out" "partial"
                               "out_omitted_chars" 120
                               "is_eof" false
                               "next_offset" 321
                               "note" "waiting for input"
                               "error" "fixture diagnostic"}
                              "q = r\ndel r\nprint([q])")]
        (doseq [text ["running" "wait timed out" "120" "logs(offset=0)" "321" "waiting for input"
                      "fixture diagnostic" "saved shell handle"]]
          (expect (str/includes? out text)))
        (expect (not (str/includes? out "r.logs")))))
  (it "bounds long output, retains both ends and tells the caller what to read"
      (let [out (check-result {"op" "shell"
                               "id" "demo"
                               "status" "exited"
                               "exit" 1
                               "out" (str "first diagnostic\n"
                                          (apply str (repeat 20000 "x"))
                                          "\nlast diagnostic")}
                              "q = r\ndel r\nassert len(q['out']) > 20000\nprint(q)")]
        (expect (< (count out) 5000))
        (doseq [text ["exit=1" "first diagnostic" "last diagnostic" "omitted" "['out']"]]
          (expect (str/includes? out text)))
        (expect (not (str/includes? out "r['out']"))))))

(defdescribe
  compact-session-result-test
  (it
    "summarizes a session without recursively printing its history"
    (let
      [out
       (check-result
         {"op" "read_session"
          "session_id" "demo"
          "session" {"id" "demo" "title" "Fixture" "turn_count" 2}
          "failures" [{"message" "failure"}]
          "transcript" {"turns" [{"iterations" [{"blocks" [{"stdout" "private history"}]}]}]}}
         "q = r\ndel r\nassert q['transcript']['turns'][0]['iterations'][0]['blocks'][0]['stdout'] == 'private history'\nprint(q)")]
      (expect (str/includes? out "read_session demo"))
      (expect (str/includes? out "Fixture"))
      (expect (str/includes? out "1 failures"))
      (expect (str/includes? out "['transcript']['turns']"))
      (expect (not (str/includes? out "r['transcript']")))
      (expect (not (str/includes? out "private history")))))
  (it "reports missing sessions and leaves unfamiliar result shapes unchanged"
      (expect (str/includes? (check-result {"op" "read_session" "session" nil "transcript" nil}
                                           "print(r)")
                             "not found"))
      (check-result {"op" "other" "optional" nil "value" [1 2]}
                    "assert repr(r) == repr(dict(r))\nassert r['optional'] is None")))

(defn- council-entry
  [entry-id]
  {"entry_id" entry-id
   "thread_id" 1
   "group_id" "fixture-group"
   "author_session_id" "fixture-author"
   "created_at" 0
   "source" "host"
   "ping" []
   "kind" "coordination"
   "title" "Review cancellation"
   "content" "Keep the existing cancellation boundary."})

(defdescribe
  compact-council-result-test
  (it "prints entry identity, message and reply state without changing raw data"
      (doseq [op ["council.publish" "council.get"]]
        (let [out (check-result (assoc (council-entry 7)
                                  "op" op
                                  "reply_required" true
                                  "reply_to" 3
                                  "ping" ["notified-peer"]
                                  "replies"
                                  [{"session_id" "peer-a" "state" "pending"}
                                   {"session_id" "peer-b" "state" "delivered"}
                                   {"session_id" "peer-c" "state" "replied" "reply_entry_id" 8}])
                                (str "q = r\ndel r\nbefore = json.dumps(q)\nprint([q])\n"
                                     "assert json.dumps(q) == before\n"
                                     "assert json.loads(before) == dict(q)\n"
                                     "assert q['replies'][2]['reply_entry_id'] == 8"))]
          (doseq [text [op "Entry #7" "thread #1" "coordination" "fixture-group" "fixture-author"
                        "Review cancellation" "Keep the existing cancellation boundary."
                        "reply_required=True" "reply_to=#3" "pending=1" "delivered=1" "peer-a"
                        "replied=1" "reply_entry_id=8" "Ping: 1 recipients" "notified-peer"
                        "field paths above start from this result"
                        "council.get with its displayed ID"]]
            (expect (str/includes? out text) text))
          (expect (not (str/includes? out "council.get(entry_id)"))))))
  (it
    "bounds long messages and recipient lists with explicit read-back paths"
    (let
      [out
       (check-result
         (assoc (council-entry 9)
           "op" "council.get"
           "ping" (mapv #(str "notified-peer-" %) (range 256))
           "content" (str "first diagnostic\n" (apply str (repeat 20000 "x")) "\nlast diagnostic")
           "replies" (vec (concat (for [n (range 255)]
                                    {"session_id" (str "peer-" n) "state" "pending"})
                                  [{"session_id" "last-peer" "state" "unavailable"}])))
         "q = r\ndel r\nassert len(q['content']) > 20000\nassert len(q['replies']) == 256\nprint(q)")]
      (expect (< (count out) 4000))
      (doseq [text ["Ping: 256 recipients" "['ping']" "first diagnostic" "last diagnostic" "omitted"
                    "['content']" "['replies']" "pending=255" "unavailable=1"
                    "await council.get(9)"]]
        (expect (str/includes? out text) text))
      (expect (not (str/includes? out "r['")))))
  (it "uses a stable entry id rather than a guessed variable for omitted content"
      (let [out (check-result (assoc (council-entry 7093)
                                "op" "council.get"
                                "content" (apply str (repeat 737 "x")))
                              "q = r\ndel r\nprint(q)")]
        (expect (str/includes? out "137 chars omitted"))
        (expect (str/includes? out "full field on this result: ['content']"))
        (expect (str/includes? out "await council.get(7093)"))
        (expect (not (str/includes? out "r['content']")))))
  (it "bounds pages while retaining cursors, omitted IDs and all reply-state counts"
      (let [entries
            (mapv #(assoc (council-entry %) "content" (apply str (repeat 20000 "x"))) (range 1 51))

            entries
            (update entries
                    49 assoc
                    "reply_required" true
                    "replies" [{"session_id" "last-peer" "state" "unavailable"}
                               {"session_id" "held-peer" "state" "interrupted"}])

            out
            (check-result {"op" "council.read" "entries" entries "after" 50 "has_more" true}
                          (str "q = r\ndel r\nassert len(q['entries']) == 50\n"
                               "assert len(q['entries'][49]['content']) == 20000\nprint(q)"))]

        (expect (< (count out) 9000))
        (doseq [text ["50 entries" "after=50" "has_more=True" "#50" "45 more entries" "['entries']"
                      "same filters" "unavailable=1" "interrupted=1" "reply_required=True: 1"]]
          (expect (str/includes? out text) text))
        (expect (not (str/includes? out "r['entries']")))))
  (it "distinguishes thread lists, empty pages and failures"
      (let [out (check-result {"op" "council.threads"
                               "entries" [{"thread_id" 7
                                           "title" "Recover retries"
                                           "kind" "complain"
                                           "author_session_id" "fixture-author"}]
                               "after" 7
                               "has_more" false}
                              "print(r)")]
        (doseq [text ["Thread #7" "Recover retries" "complain" "has_more=False"]]
          (expect (str/includes? out text) text)))
      (let [out (check-result {"op" "council.read" "entries" [] "after" 0 "has_more" false}
                              "print(r)")]
        (doseq [text ["0 entries" "after=0" "has_more=False"]]
          (expect (str/includes? out text) text)))
      (let [out (check-result {"op" "council.read" "error" {"message" "limit must be <= 50"}}
                              "print(r)")]
        (expect (str/includes? out "error"))
        (expect (str/includes? out "limit must be <= 50"))))
  (it "leaves unfamiliar or malformed Council shapes inspectable"
      (doseq [data [{"op" "council.unknown" "payload" [1 2]}
                    {"op" "council.read" "entries" [nil] "after" 1 "has_more" false}]]
        (check-result data "assert repr(r) == repr(dict(r))"))))

(defdescribe
  compact-discovery-result-test
  (it
    "preserves rows, doc lookup and JSON after printing and context refresh"
    (tpc/with-own
      [ctx {}]
      (dotimes [_ 2]
        (ep/bind-ctx! ctx {"workspace" {"root" (System/getProperty "user.dir")}})
        (let
          [result
           (ep/run-python-block
             ctx
             "rows = apropos(r'^doc$')\nassert isinstance(rows, list) and len(rows) == 1\nrow = rows[0]\nassert tuple(row) == (row.type, row.name, row.body)\nassert json.loads(json.dumps(rows))[0][1] == 'doc'\nassert doc(row) == doc('doc')\nassert str(rows) == str(row)\nassert str(row).startswith('tool doc — ')\nassert len(str(rows)) < len(repr([row._asdict()]))\nprint(rows)")]
          (expect (nil? (:error result)) (pr-str (:error result)))
          (expect (str/starts-with? (:stdout result) "tool doc — ")))))))

(defdescribe
  worker-result-presentation-test
  (it "installs presentation in the jailed worker, not only the in-process test interpreter"
      (tpc/with-own
        [ctx
         {'fixture (fn []
                     {"op" "shell" "id" "demo" "status" "exited" "exit" 0 "out" "ok\n"})
          'council-fixture (fn []
                             (assoc (council-entry 7) "op" "council.get"))}
         (constantly [(System/getProperty "user.dir")]) {:worker? true :jail-enabled? true}]
        (let [result (ep/run-python-block ctx
                                          (str "print(await fixture())\n"
                                               "print(apropos(r'^doc$'))\n"
                                               "print(await council_fixture())"))]
          (expect (nil? (:error result)) (pr-str (:error result)))
          (expect (str/includes? (:stdout result) "shell demo: exited; exit=0"))
          (expect (str/includes? (:stdout result) "tool doc — "))
          (expect (str/includes? (:stdout result) "Entry #7"))
          (expect (str/includes? (:stdout result) "Keep the existing cancellation boundary."))))))
