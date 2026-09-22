(ns com.blockether.vis.native-sandbox-boundary-test
  "Sandbox reachability through an actual native agent call, without a paid model."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [charred.api :as json]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.util.concurrent TimeUnit)))

(defdescribe
  native-sandbox-calls-remain-callable-test
  ;; JVM startup defers these dependencies. Their first use must still work after
  ;; native-image has discarded everything the builder did not load.
  (it
    "calls an extension, searches, patches and stops a child process through the linked image"
    (doseq [model ["gpt-4o" "gpt-4"]]
      (let
        [^File dir (#'native/temp-dir "vis-native-sandbox-")
         ^File bin (#'native/require-binary)
         source "(defn f [x]\n(+ x 1))"
         calls (atom 0)
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
           ;; #207: execute the owned-process and capture boundary in the image.
           "child = await shell('sleep 30')\n" "stopped = await child.stop()\n"
           "assert stopped['status'] == 'stopped', str(stopped)\n"
           "print('NATIVE_PROCESS_CLEANUP_READY')\n"
           "from pathlib import Path\n" "assert 'default.clj' in ls(Path('.'), depth=2)\n"
           "file_hit = grep({'query': ['defn f'], 'paths': [Path('default.clj')], 'context': 1})\n"
           "dir_hit = grep({'query': ['defn.*f'], 'paths': [Path('configured')], 'is_regex': True, 'context': 1})\n"
           "assert 'default.clj' in str(file_hit), str(file_hit)\n"
           "assert 'example.clj' in str(dir_hit), str(dir_hit)\n"
           "print('NATIVE_FFF_READY')\n"
           ;; An anchored write is the editing path the agent depends on most.
           "edit_target = project_root_path / 'edit.txt'\n"
           "edit_target.write_text('alpha\\nbeta\\n')\n"
           "anchor = re.match(r'\\d+:[0-9a-f]{3}', cat(str(edit_target)).splitlines()[1]).group(0)\n"
           "patch(str(edit_target), [{'from': anchor, 'replace': 'gamma'}])\n"
           "assert edit_target.read_text() == 'alpha\\ngamma\\n', edit_target.read_text()\n"
           "print('NATIVE_PATCH_READY')\n"
           ;; #279: let the refusal escape this tool call; the next model step must
           ;; still see the same session, rather than an env_context_retired.
           "scan_kept = 279\nscan_root = project_root_path / 'scan-budget'\n"
           "scan_root.mkdir()\n(scan_root / 'entry').touch()\n"
           "for _ in range(10001):\n    list(scan_root.rglob('*.missing'))")
         tools [{:id "sandbox-native"
                 :type "function"
                 :function {:name "python_execution" :arguments (json/write-json-str {:code code})}}
                {:id "sandbox-recovery"
                 :type "function"
                 :function {:name "python_execution"
                            :arguments (json/write-json-str
                                         {:code (str "assert scan_kept == 279\n"
                                                     "assert len(list(scan_root.iterdir())) == 1\n"
                                                     "print('NATIVE_TRAVERSAL_RECOVERED')")})}}]
         whole @#'native/whole-body
         stream @#'native/stream-body
         reply (fn [stream? text]
                 (if-let [tool (get tools (dec (swap! calls inc)))]
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
          (with-redefs-fn {#'native/whole-body #(reply false %)
                           #'native/stream-body #(reply true %)}
            (fn []
              (let [{:keys [server port asked]} (#'native/start-stub-provider! "PROBE_COMPLETE")]
                (try
                  (#'native/overlay! dir port)
                  (let [config (io/file dir ".vis/config.yml")]
                    (spit config (str/replace (slurp config) "stub-model" model)))
                  (let [log (io/file dir "run.log")
                        builder (doto (ProcessBuilder. ^java.util.List
                                                       [(.getAbsolutePath bin)
                                                        (str "-Duser.home=" (.getAbsolutePath dir))
                                                        "--db" ":memory" "--raw"
                                                        "Run the sandbox probe."])
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
                      (try (expect (.waitFor process 180 TimeUnit/SECONDS)
                                   "native sandbox run timed out")
                           (let [tool-results
                                 (for [request @asked
                                       message (:messages
                                                 (json/read-json (:body request) :key-fn keyword))
                                       :when (= "tool" (:role message))]

                                   (:content message))
                                 output (str (slurp log)
                                             "\n" (pr-str tool-results)
                                             "\n" (str/join
                                                    "\n"
                                                    (for [^File file (file-seq
                                                                       (io/file dir ".vis/logs"))
                                                          :when (= "worker.log" (.getName file))]

                                                      (slurp file))))]

                             (expect (= 0 (.exitValue process)) output)
                             (expect (>= @calls 3) output)
                             (expect (str/includes? (pr-str tool-results) "10000") output)
                             (expect (str/includes? (pr-str tool-results) "grep/ls") output)
                             (doseq [marker ["NATIVE_KEYWORDS_READY" "NATIVE_PROCESS_CLEANUP_READY"
                                             "NATIVE_FFF_READY" "NATIVE_PATCH_READY"
                                             "NATIVE_TRAVERSAL_RECOVERED"]]
                               (expect (str/includes? (pr-str tool-results) marker) output))
                             (expect (.isDirectory (io/file dir ".vis/native/sqlite")) output)
                             (expect (every? #(= model
                                                 (:model
                                                   (json/read-json (:body %) :key-fn keyword)))
                                             @asked)
                                     output))
                           (finally (when (.isAlive process) (#'native/kill-tree! process))))))
                  (finally (.stop ^com.sun.net.httpserver.HttpServer server 0))))))
          (finally (#'native/delete-tree! dir)))))))
