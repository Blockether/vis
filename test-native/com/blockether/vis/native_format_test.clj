(ns com.blockether.vis.native-format-test
  "Language-tool reachability through an actual native agent call, without a paid model."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [charred.api :as json]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]]
            [nrepl.server :as nrepl-server])
  (:import (java.io File)
           (java.util.concurrent TimeUnit)))

(defdescribe
  native-formatters-remain-callable-test
  ;; JVM startup defers these dependencies. Their first use must still work after
  ;; native-image has discarded everything the builder did not load.
  (it
    "runs Python tests and REPLs, formats, lints and searches through the linked image"
    (doseq [model ["gpt-4o" "gpt-4"]]
      (let
        [^File dir (#'native/temp-dir "vis-native-format-")
         ^File bin (#'native/require-binary)
         source "(defn f [x]\n(+ x 1))"
         calls (atom 0)
         repl-server (nrepl-server/start-server :port 0 :bind "127.0.0.1")
         code
         (str
           ;; #197: typed namespace kwargs must survive the native agent/worker boundary.
           "assert await probe.echo(value='hello') == 'hello!'\n"
           "assert await probe.echo('hello', suffix='?') == 'hello?'\n"
           "assert await probe.echo(value='hello', suffix='.') == 'hello.'\n"
           "assert await probe.echo() == 'default!'\n"
           "assert (await probe.mapping({'payload': 'unchanged'}))['payload'] == 'unchanged'\n"
           "assert (await probe.mapping(payload={'x': 1}))['x'] == 1\n"
           "assert await gather(probe.echo(value='a'), probe.echo(value='b')) == ['a!', 'b!']\n"
           "assert await asyncio.to_thread(probe.echo, value='thread') == 'thread!'\n"
           "print('NATIVE_KEYWORDS_READY')\n"
           (str/join
             "\n"
             ["def check_tests(runner):"
              "    options = {'cwd': str(project_root_path), 'runner': runner}"
              "    good = run_tests('python', dict(options, paths=['test_good.py']))"
              "    assert good['is_pass'] and good['pass'] == 2 and good['total'] == 2, str(good)"
              "    bad = run_tests('python', dict(options, paths=['test_bad.py']))"
              "    assert not bad['is_pass'] and bad['fail'] > 0, str(bad)"
              "    error = run_tests('python', dict(options, paths=['test_error.py']))"
              "    assert not error['is_pass'] and error['errored'] > 0, str(error)"
              "    empty = run_tests('python', dict(options, paths=['test_empty.py']))"
              "    assert not empty['is_pass'], str(empty)"
              "for runner in ['vispython', 'project']:" "    check_tests(runner)"
              "print('NATIVE_PYTHON_TESTS_COLD_READY')"
              "started = repl_start('python', {'cwd': str(project_root_path)})" "try:"
              "    assert started['status'] == 'up', str(started)"
              "    result = repl_eval('python', {'code': 'native_marker = 21\\nprint(native_marker * 2)', 'cwd': str(project_root_path)})"
              "    assert result['out'].strip() == '42', str(result)"
              "    again = repl_start('python', {'cwd': str(project_root_path)})"
              "    assert again['pid'] == started['pid'], str(again)"
              "    for runner in ['vispython', 'project']:" "        check_tests(runner)"
              "    state = repl_eval('python', {'code': 'print(native_marker)', 'cwd': str(project_root_path)})"
              "    assert state['out'].strip() == '21', str(state)"
              "    print('NATIVE_PYTHON_REPL_READY')" "finally:"
              "    repl_stop('python', {'cwd': str(project_root_path)})"
              "restarted = repl_start('python', {'cwd': str(project_root_path)})" "try:"
              "    assert restarted['status'] == 'up' and restarted['pid'] != started['pid'], str(restarted)"
              "    clean = repl_eval('python', {'code': \"print('native_marker' in globals())\", 'cwd': str(project_root_path)})"
              "    assert clean['out'].strip() == 'False', str(clean)" "finally:"
              "    repl_stop('python', {'cwd': str(project_root_path)})" "check_tests('project')"
              "print('NATIVE_PYTHON_TESTS_RESTART_READY')" ""])
           "print(format_code('clojure', {'path': 'default.clj'}))\n"
           "print(format_code('clojure', {'path': 'configured/example.clj'}))\n"
           "try:\n"
           "    result = run_tests('clojure', {'path': 'missing_test.clj'})\n"
           "except Exception as error:\n"
           "    result = str(error)\n"
           "assert 'no such path' in str(result), str(result)\n"
           "print('NATIVE_TEST_HANDLER_READY')\n"
           ;; #207: execute the new owned-process/capture boundary in the image.
           "cli = run_tests('clojure', {'cwd': str(project_root_path / 'clj-runner')})\n"
           "assert cli['is_pass'] and cli['total'] == 1 and cli['mode'] == 'cli', str(cli)\n"
           "assert 'native runner stderr' in cli['output'], str(cli)\n"
           "print('NATIVE_TEST_SUBPROCESS_READY')\n"
           "child = await shell('sleep 30')\n"
           "stopped = await child.stop()\n"
           "assert stopped['status'] == 'stopped', str(stopped)\n"
           "print('NATIVE_PROCESS_CLEANUP_READY')\n"
           ;; #207: exercise the deadline transport and timed lock in the image.
           "clj_repl = {'host': '127.0.0.1', 'port': "
           (:port repl-server)
           "}\n"
           "warm = repl_eval('clojure', dict(clj_repl, code='(+ 1 2)'))\n"
           "assert warm['value'] == '3', str(warm)\n"
           "started = time.monotonic()\n"
           "expired = repl_eval('clojure', dict(clj_repl, timeout_ms=1500, code='(do (Thread/sleep 900) (println :partial) (flush) (Thread/sleep 5000))'))\n"
           "elapsed = time.monotonic() - started\n"
           "assert expired['timed_out'] and ':partial' in expired['out'], str(expired)\n"
           "assert elapsed < 2.3, str(elapsed)\n"
           "alive = repl_eval('clojure', dict(clj_repl, code='(+ 2 3)'))\n"
           "assert alive['value'] == '5', str(alive)\n"
           "print('NATIVE_CLOJURE_REPL_DEADLINE_READY')\n"
           "from pathlib import Path\n" "assert 'default.clj' in ls(Path('.'), depth=2)\n"
           "file_hit = grep({'query': ['defn f'], 'paths': [Path('default.clj')], 'context': 1})\n"
           "dir_hit = grep({'query': ['defn.*f'], 'paths': [Path('configured')], 'is_regex': True, 'context': 1})\n"
           "assert 'default.clj' in str(file_hit), str(file_hit)\n"
           "assert 'example.clj' in str(dir_hit), str(dir_hit)\n"
           "print('NATIVE_FFF_READY')\n"
           ;; Regression: native lint could not locate clj_kondo/core__init.class.
           "linted = lint_code('clojure', {'code': '(ns probe) (unknown-call)'})\n"
           "assert 'unresolved-symbol' in str(linted), str(linted)\n" "print('NATIVE_LINT_READY')\n"
           ;; The `general` provider COMPILES its target for the reflection and
           ;; boxed-math warnings, which only exist at compile time — so the image
           ;; has to carry a usable Clojure compiler, not just clj-kondo.
           "reflected = lint_code('clojure', {'code': '(ns probe)\\n(defn f [x] (.length x))\\n'})\n"
           "assert [f for f in reflected['findings'] if f['provider'] == 'general' and f['type'] == 'reflection'], str(reflected)\n"
           "print('NATIVE_REFLECTION_LINT_READY')\n"
           ;; #225: Ruff binds configured FFM calls before returning any lint result.
           "for options in [{'code': 'import os\\n'}, {'paths': ['lint_target.py'], 'cwd': str(project_root_path)}]:\n"
           "    linted = lint_code('python', options)\n"
           "    assert linted['providers'] == ['ruff'] and linted['files'] == 1, str(linted)\n"
           "    assert linted['warning'] == 1 and linted['error'] == 0, str(linted)\n"
           "    assert linted['findings'][0]['type'] == 'F401', str(linted)\n"
           "clean = lint_code('python', {'code': 'print(42)\\n'})\n"
           "assert clean['findings'] == [] and clean['error'] == 0, str(clean)\n"
           "formatted = format_code('python', {'code': 'x= 1\\n'})\n"
           "assert formatted['changed'], str(formatted)\n"
           "print('NATIVE_PYTHON_LINT_READY')\n"
           ;; The delimiter repair a patch may apply is the Python language surface's
           ;; now: the image has to reach it across the worker boundary.
           "repair_target = project_root_path / 'repair.clj'\n"
           "repair_target.write_text('(defn g [x]\\n  (+ x 1))\\n')\n"
           "anchor = re.match(r'\\d+:[0-9a-f]{3}', cat(str(repair_target)).splitlines()[1]).group(0)\n"
           "repaired = patch(str(repair_target), [{'from': anchor, 'replace': '  (+ x 2)'}])\n"
           "assert 'delimiters repaired' in str(repaired), str(repaired)\n"
           "assert repair_target.read_text() == '(defn g [x]\\n  (+ x 2))\\n', repair_target.read_text()\n"
           "print('NATIVE_DELIMITER_REPAIR_READY')")
         tool {:id "format-native"
               :type "function"
               :function {:name "python_execution" :arguments (json/write-json-str {:code code})}}
         whole @#'native/whole-body
         stream @#'native/stream-body
         reply (fn [stream? text]
                 (if (= 1 (swap! calls inc))
                   (if stream?
                     (str "data: "
                          (json/write-json-str {:id "stub"
                                                :object "chat.completion.chunk"
                                                :created 0
                                                :model "stub-model"
                                                :choices [{:index 0
                                                           :delta {:role "assistant"
                                                                   :tool_calls [(assoc tool
                                                                                  :index 0)]}
                                                           :finish_reason nil}]})
                          "\n\ndata: "
                          (json/write-json-str {:id "stub"
                                                :object "chat.completion.chunk"
                                                :created 0
                                                :model "stub-model"
                                                :choices
                                                [{:index 0 :delta {} :finish_reason "tool_calls"}]})
                          "\n\ndata: [DONE]\n\n")
                     (json/write-json-str
                       {:id "stub"
                        :object "chat.completion"
                        :created 0
                        :model "stub-model"
                        :choices [{:index 0
                                   :message {:role "assistant" :content nil :tool_calls [tool]}
                                   :finish_reason "tool_calls"}]
                        :usage {:prompt_tokens 1 :completion_tokens 2 :total_tokens 3}}))
                   ((if stream? stream whole) text)))]

        (try
          (let [entry (io/file dir ".vis/extensions/keyword_probe.py")]
            (io/make-parents entry)
            (spit entry
                  (str "from __future__ import annotations\n"
                       "import blockether.vis.extension as vis\n" "class Probe:\n"
                       "    @vis.method()\n"
                       "    def echo(self, value: str='default', *, suffix: str='!') -> str:\n"
                       "        \"Echo a string with a suffix.\"\n"
                       "        if not isinstance(value, str): raise TypeError('expected str')\n"
                       "        return value + suffix\n" "    @vis.method()\n"
                       "    def mapping(self, payload):\n"
                       "        \"Echo a positional mapping unchanged.\"\n"
                       "        return payload\n"
                       "vis.register_extension(vis.Extension(name='keyword-probe', alias='probe', "
                       "description='Native keyword transport fixture', "
                       "symbols=[vis.Symbol(Probe(), name='probe')]))\n")))
          (io/make-parents (io/file dir "configured/example.clj"))
          (spit (io/file dir "default.clj") source)
          (spit (io/file dir "configured/example.clj") source)
          (spit (io/file dir "configured/.zprint.edn") "{:width 80}")
          (io/make-parents (io/file dir "clj-runner/bb.edn"))
          (spit
            (io/file dir "clj-runner/bb.edn")
            (pr-str '{:tasks {:requires ([clojure.test :as t])
                              test (do
                                    (t/deftest native-pass (t/is (= 2 (+ 1 1))))
                                    (binding [*out* *err*] (println "native runner stderr"))
                                    (t/run-tests))}}))
          (spit (io/file dir "pyproject.toml")
                "[project]\nname = \"native-language-probe\"\nversion = \"0.0.0\"\n")
          ;; Install only in this disposable project, never the operator's interpreter.
          (doseq [argv [["uv" "venv" ".venv"]
                        ["uv" "pip" "install" "--python" ".venv/bin/python" "pytest==8.4.2"]]]
            (let [result (apply sh/sh (concat argv [:dir (.getAbsolutePath dir)]))]
              (expect (zero? (:exit result)) (pr-str result))))
          (spit (io/file dir "test_good.py")
                "def test_one():\n    assert 1 + 1 == 2\ndef test_two():\n    assert 3 * 7 == 21\n")
          (spit (io/file dir "test_bad.py") "def test_bad():\n    assert 1 == 2\n")
          (spit (io/file dir "test_error.py") "import native_missing_dependency\n")
          ;; Issue #70: zero discovered tests must not produce a successful 0/0.
          (spit (io/file dir "test_empty.py") "# No tests defined.\n")
          (spit (io/file dir "lint_target.py") "import os\n")
          (with-redefs-fn {#'native/whole-body #(reply false %)
                           #'native/stream-body #(reply true %)}
            (fn []
              (let [{:keys [server port asked]} (#'native/start-stub-provider! "FORMAT_COMPLETE")]
                (try
                  (#'native/overlay! dir port)
                  (let [config (io/file dir ".vis/config.yml")]
                    (spit config (str/replace (slurp config) "stub-model" model)))
                  (let [log (io/file dir "run.log")
                        builder (doto (ProcessBuilder. ^java.util.List
                                                       [(.getAbsolutePath bin)
                                                        (str "-Duser.home=" (.getAbsolutePath dir))
                                                        "--db" ":memory" "--raw"
                                                        "Format both Clojure files."])
                                  (.directory dir)
                                  (.redirectErrorStream true)
                                  (.redirectOutput log))
                        env (.environment builder)
                        kept (select-keys (into {} env)
                                          ["PATH" "JAVA_HOME" "SystemRoot" "TMPDIR" "TMP" "TEMP"])]

                    ;; Never inherit a gateway override or the operator's provider credentials.
                    (.clear env)
                    (.putAll env kept)
                    (.putAll env (#'native/native-environment))
                    (.put env "HOME" (.getAbsolutePath dir))
                    (let [process (.start builder)]
                      (try
                        (expect (.waitFor process 180 TimeUnit/SECONDS)
                                "native formatting timed out")
                        (let [tool-results (for [request @asked
                                                 message (:messages (json/read-json (:body request)
                                                                                    :key-fn
                                                                                    keyword))
                                                 :when (= "tool" (:role message))]

                                             (:content message))
                              output (str (slurp log)
                                          "\n" (pr-str tool-results)
                                          "\n" (str/join "\n"
                                                         (for [^File file
                                                               (file-seq (io/file dir ".vis/logs"))
                                                               :when (= "worker.log"
                                                                        (.getName file))]

                                                           (slurp file))))]

                          (expect (= 0 (.exitValue process)) output)
                          (expect (>= @calls 2) output)
                          (expect (str/includes? (pr-str tool-results) "NATIVE_TEST_HANDLER_READY")
                                  output)
                          (expect (str/includes? (pr-str tool-results) "NATIVE_FFF_READY") output)
                          (expect (str/includes? (pr-str tool-results) "NATIVE_LINT_READY") output)
                          (doseq [marker
                                  ["NATIVE_KEYWORDS_READY" "NATIVE_PYTHON_REPL_READY"
                                   "NATIVE_PYTHON_TESTS_COLD_READY"
                                   "NATIVE_PYTHON_TESTS_RESTART_READY"
                                   "NATIVE_TEST_SUBPROCESS_READY" "NATIVE_PROCESS_CLEANUP_READY"
                                   "NATIVE_CLOJURE_REPL_DEADLINE_READY" "NATIVE_PYTHON_LINT_READY"
                                   "NATIVE_DELIMITER_REPAIR_READY" "NATIVE_REFLECTION_LINT_READY"]]
                            (expect (str/includes? (pr-str tool-results) marker) output))
                          (expect (.isDirectory (io/file dir ".vis/native/sqlite")) output)
                          (expect (every? #(= model
                                              (:model (json/read-json (:body %) :key-fn keyword)))
                                          @asked)
                                  output)
                          (expect (= "(defn f [x]\n  (+ x 1))\n"
                                     (slurp (io/file dir "default.clj")))
                                  output)
                          (expect (= "(defn f [x] (+ x 1))\n"
                                     (slurp (io/file dir "configured/example.clj")))
                                  output))
                        (finally (when (.isAlive process) (#'native/kill-tree! process))))))
                  (finally (.stop ^com.sun.net.httpserver.HttpServer server 0))))))
          (finally (nrepl-server/stop-server repl-server) (#'native/delete-tree! dir)))))))
