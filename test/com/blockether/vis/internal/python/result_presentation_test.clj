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
  (it "keeps each engine's guest sources intact when another version starts"
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
               (expect (= "VERSION = 1\n" (slurp (io/file first-dir "vis_introspection.py"))))
               (expect (= "VERSION = 2\n" (slurp (io/file second-dir "vis_introspection.py"))))
               (expect (= first-dir (#'worker/materialize-guest-sources! root (reverse sources))))
               (expect (= "VERSION = 2\n" (slurp (io/file second-dir "vis_introspection.py")))))
             (finally (doseq [file (reverse (file-seq root))]
                        (io/delete-file file true)))))))

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
                              "print([r])")]
        (doseq [text ["running" "wait timed out" "120" "logs(offset=0)" "321" "waiting for input"
                      "fixture diagnostic"]]
          (expect (str/includes? out text)))))
  (it "bounds long output, retains both ends and tells the caller what to read"
      (let [out (check-result {"op" "shell"
                               "id" "demo"
                               "status" "exited"
                               "exit" 1
                               "out" (str "first diagnostic\n"
                                          (apply str (repeat 20000 "x"))
                                          "\nlast diagnostic")}
                              "assert len(r['out']) > 20000\nprint(r)")]
        (expect (< (count out) 5000))
        (doseq [text ["exit=1" "first diagnostic" "last diagnostic" "omitted" "r['out']"]]
          (expect (str/includes? out text))))))

(defdescribe
  compact-test-result-test
  (it "summarizes a green run without runner boilerplate"
      (let [out (check-result {"op" "run_tests"
                               "is_pass" true
                               "total" 48
                               "fail" 0
                               "skipped" 2
                               "ms" 2100
                               "output" "runner boilerplate"
                               "failures" []
                               "hint" nil}
                              "assert r['output'] == 'runner boilerplate'\nprint(r)")]
        (expect (str/starts-with? out "run_tests: PASS; 48 tests; 0 failures; 2 skipped; 2.1s"))
        (expect (not (str/includes? out "runner boilerplate")))))
  (it "prints faults and diagnostics even when counts or the verdict are incomplete"
      (let [out (check-result {"op" "run_tests"
                               "is_pass" false
                               "total" 1
                               "fail" 1
                               "errored" 1
                               "exit" 1
                               "repl_unusable" true
                               "failures" [{"test" "test_value"
                                            "type" "error"
                                            "file" "test/value.py"
                                            "message" "expected a value"}]
                               "output" "runner stack trace"
                               "warning" "layout could not be read"
                               "hint" "start a fresh REPL"}
                              "print(r)")]
        (doseq [text ["FAIL" "test_value" "test/value.py" "expected a value" "runner stack trace"
                      "repl_unusable" "start a fresh REPL" "layout could not be read"]]
          (expect (str/includes? out text)))))
  (it "never invents a pass when the runner times out or returns no verdict"
      (doseq [[data expected] [[{"is_pass" true "timed_out" true} "TIMEOUT"]
                               [{"is_pass" true "exit" 1} "FAIL"] [{"is_pass" true "fail" 1} "FAIL"]
                               [{"is_pass" nil} "UNKNOWN"] [{"is_pass" true "total" 0} "NO TESTS"]]]
        (expect (str/includes? (check-result (assoc data "op" "run_tests") "print(r)") expected))))
  (it "bounds multiple faults explicitly, with every fault still available"
      (let [out (check-result {"op" "run_tests"
                               "is_pass" false
                               "failures" (mapv (fn [i]
                                                  {"test" (str "test_" i)
                                                   "type" "fail"
                                                   "message" (apply str (repeat 1000 "m"))})
                                                (range 20))}
                              "assert len(r['failures']) == 20\nprint(r)")]
        (expect (< (count out) 5000))
        (expect (str/includes? out "15 more"))
        (expect (str/includes? out "r['failures']")))))

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
         "assert r['transcript']['turns'][0]['iterations'][0]['blocks'][0]['stdout'] == 'private history'\nprint(r)")]
      (expect (str/includes? out "read_session demo"))
      (expect (str/includes? out "Fixture"))
      (expect (str/includes? out "1 failures"))
      (expect (str/includes? out "r['transcript']['turns']"))
      (expect (not (str/includes? out "private history")))))
  (it "reports missing sessions and leaves unfamiliar result shapes unchanged"
      (expect (str/includes? (check-result {"op" "read_session" "session" nil "transcript" nil}
                                           "print(r)")
                             "not found"))
      (check-result {"op" "other" "optional" nil "value" [1 2]}
                    "assert repr(r) == repr(dict(r))\nassert r['optional'] is None")))

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
                     {"op" "run_tests" "is_pass" true "total" 3 "fail" 0})}
         (constantly [(System/getProperty "user.dir")]) {:worker? true :jail-enabled? true}]
        (let [result (ep/run-python-block ctx "print(await fixture())\nprint(apropos(r'^doc$'))")]
          (expect (nil? (:error result)) (pr-str (:error result)))
          (expect (str/includes? (:stdout result) "run_tests: PASS; 3 tests; 0 failures"))
          (expect (str/includes? (:stdout result) "tool doc — "))))))
