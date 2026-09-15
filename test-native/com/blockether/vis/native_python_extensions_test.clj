(ns com.blockether.vis.native-python-extensions-test
  "Python extension registration during isolated native gateway startup."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as native]
            [com.blockether.vis.native-speech-startup-test :as startup]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.net.httpserver HttpServer]
           [java.io File]
           [java.net ServerSocket]
           [java.util.concurrent TimeUnit]))

(defdescribe
  native-python-reexecution-test
  ;; #199: JVM coverage cannot prove the native worker's executable identity.
  (it "re-executes bundled CPython from the built native CLI"
      (let [home
            (#'native/temp-dir "vis-native-reexecution-")

            binary
            (#'native/require-binary)]

        (try (let [[^Process child log] (#'startup/start-native!
                                         home
                                         binary
                                         "reexecution"
                                         ["python" "--no-env" "--no-network" "-c"
                                          (slurp "test/resources/python_reexecution.py")])]
               (try (expect (.waitFor child 60 TimeUnit/SECONDS) "native re-execution timed out")
                    (let [output (slurp log)]
                      (expect (= 0 (.exitValue child)) output)
                      (expect (str/includes? output "python-reexecution-ok") output))
                    (finally (when (.isAlive child) (#'native/kill-tree! child)))))
             (finally (#'native/delete-tree! home))))))

(defdescribe
  native-runtime-source-selection-test
  ;; #194: the prebuilt worker must not override the host's pinned guest sources.
  (it
    "loads the package helper from the pinned runtime revision"
    (let [home
          (#'native/temp-dir "vis-native-runtime-source-")

          binary
          (#'native/require-binary)

          expected
          (util/sha256-hex (slurp (io/resource "vis-python/package_paths.py")))]

      (try
        (let
          [[^Process child log]
           (#'startup/start-native!
            home
            binary
            "source"
            ["python" "--no-env" "--no-network" "-c"
             (str
               "import hashlib, package_paths, re\nfrom pathlib import Path\n"
               "assert VIS_VERSION == VIS_PYTHON_SDK_VERSION != 'dev'\n"
               "assert re.fullmatch(r'[0-9a-f]{40}(-dirty)?', VIS_SHA_RELEASE)\n"
               "assert VIS_PYTHON_RUNTIME_VERSION == "
               (pr-str (str/trim (slurp (io/resource "vis-python-runtime/VERSION"))))
               "\n"
               "print(hashlib.sha256(Path(package_paths.__file__).read_bytes()).hexdigest())\n")])]
          (try (expect (.waitFor child 60 TimeUnit/SECONDS) "native source probe timed out")
               (let [output (slurp log)]
                 (expect (= 0 (.exitValue child)) output)
                 (expect (str/includes? output expected) output))
               (finally (when (.isAlive child) (#'native/kill-tree! child)))))
        (finally (#'native/delete-tree! home))))))

(defdescribe
  native-record-subscription-test
  ;; #240: exercise generated records across the native extension worker boundary.
  (it
    "supports immutable field-name access on top-level and nested tool results"
    (let [dir
          (#'native/temp-dir "vis-native-record-subscription-")

          original-stream
          @#'native/stream-body

          original-whole
          @#'native/whole-body

          calls
          (atom 0)

          code
          (str "from dataclasses import FrozenInstanceError\n" "result = await records()\n"
               "assert result['items'] is result.items\n" "item = result['items'][0]\n"
               "assert item['url'] == item.url == 'https://gateway.example.com/240'\n"
               "try:\n    item['missing']\n"
               "except KeyError as exc:\n    assert 'url' in str(exc) and 'missing' in str(exc)\n"
               "else:\n    raise AssertionError('unknown field accepted')\n"
               "try:\n    item[0]\n" "except TypeError:\n    pass\n"
               "else:\n    raise AssertionError('positional field accepted')\n"
               "try:\n    item['url'] = 'changed'\n"
               "except TypeError:\n    pass\n"
               "else:\n    raise AssertionError('mutable subscription')\n"
               "try:\n    item.url = 'changed'\n" "except FrozenInstanceError:\n    pass\n"
               "else:\n    raise AssertionError('mutable attribute')\n"
               "print('Native record subscription ' + 'verified')")

          reply
          (fn [stream? text]
            (if (= 1 (swap! calls inc))
              (#'native/python-tool-body code 1 stream?)
              ((if stream? original-stream original-whole) text)))]

      (try
        (let [entry (io/file dir ".vis/extensions/records.py")]
          (io/make-parents entry)
          (spit
            entry
            (str
              "from dataclasses import dataclass\n"
              "import blockether.vis.extension as vis\n"
              "@dataclass(frozen=True)\nclass CreatedIssue:\n    url: str\n"
              "@dataclass(frozen=True)\nclass Results:\n    items: tuple[CreatedIssue, ...]\n"
              "def find() -> Results:\n"
              "    \"Return the fixture records.\"\n"
              "    return Results((CreatedIssue('https://gateway.example.com/240'),))\n"
              "def _render(*, result, error, **_):\n"
              "    summary = str(error) if error else f'{len(result.items)} records'\n"
              "    return vis.ActivityPresentation('Read records', summary)\n"
              "vis.register_extension(vis.Extension(name='record-subscription', alias='records', "
              "description='Native record fixture', symbols=[vis.Symbol(find, name='records', "
              "activity=vis.Activity(label='Read records', show_start=False, render=_render))]))\n")))
        (with-redefs-fn {#'native/stream-body #(reply true %) #'native/whole-body #(reply false %)}
          (fn []
            (let [{:keys [server asked port]} (#'native/start-stub-provider!
                                               "Record check complete")]
              (try (#'native/overlay! dir port)
                   (let [{:keys [finished? exit output]}
                         (#'native/run-binary
                          dir
                          [(.getAbsolutePath ^File (#'native/require-binary))
                           (str "-Duser.home=" (.getAbsolutePath ^File dir)) "--db"
                           (.getAbsolutePath (io/file dir "sessions")) "--raw"
                           "Run the supplied Python fixture and finish."]
                          180)
                         tools (->> @asked
                                    (mapcat #(get (json/read-json (:body %)) "messages"))
                                    (filter #(= "tool" (get % "role")))
                                    (map #(str (get % "content"))))]

                     (expect finished? output)
                     (expect (= 0 exit) output)
                     (expect (some #(str/includes? % "Native record subscription verified") tools)
                             (str output "\nTool results: " (pr-str tools))))
                   (finally (.stop ^HttpServer server 0))))))
        (finally (#'native/delete-tree! dir))))))

(defdescribe
  native-editable-sdk-startup-test
  ;; #194/#196: both the injected SDK and managed extension contexts must survive refresh.
  (it
    "registers extensions from an editable cwd without installing another SDK"
    (let [home
          (#'native/temp-dir "vis-native-editable-sdk-")

          binary
          (#'native/require-binary)

          entries
          (doto (io/file home ".vis/extensions") .mkdirs)

          packages
          (doto (io/file home ".vis/python/packages") .mkdirs)

          names
          ["editable-sdk-first" "editable-sdk-second"]

          markers
          (mapv #(io/file entries (str % ".loaded")) names)]

      (try (spit (io/file home "vis.yml") "{}\n")
           (spit (io/file packages "cwd.pth") (str (.getCanonicalPath home) "\n"))
           (let [metadata (io/file packages "editable-0.0.1.dist-info/direct_url.json")]
             (io/make-parents metadata)
             (spit metadata
                   (str "{\"dir_info\":{\"editable\":true},\"url\":"
                        (pr-str (str (.toURI (.getCanonicalFile home))))
                        "}")))
           (doseq [name names]
             (spit
               (io/file entries (str name ".py"))
               (str
                 "import hashlib, package_paths, sys\nfrom pathlib import Path\n"
                 "assert hashlib.sha256(Path(package_paths.__file__).read_bytes()).hexdigest() == "
                 (pr-str (util/sha256-hex (slurp (io/resource "vis-python/package_paths.py"))))
                 ", package_paths.__file__\n"
                 "import blockether.vis.extension as vis\n"
                 "assert VIS_VERSION == VIS_PYTHON_SDK_VERSION != 'dev'\n"
                 "assert len(VIS_SHA_RELEASE) >= 40\n"
                 "assert VIS_PYTHON_RUNTIME_VERSION != 'dev'\n"
                 "assert callable(__vis_registration__) and callable(__vis_host_live__)\n"
                 "contexts = getattr(package_paths, '_test_extension_contexts', [])\n"
                 "contexts.append(sys.modules[__name__])\n"
                 "package_paths._test_extension_contexts = contexts\n"
                 "assert all(sys.modules.get(ctx.__name__) is ctx for ctx in contexts)\n"
                 "assert all(callable(ctx.__vis_registration__) for ctx in contexts)\n"
                 "vis.register_extension(vis.Extension(name=" (pr-str name)
                 ", description='Native editable SDK fixture'))\n"
                 "Path(__file__).with_suffix('.loaded').write_text('registered')\n")))
           (let [port
                 (with-open [socket (ServerSocket. 0)]
                   (.getLocalPort socket))

                 [^Process gateway gateway-log]
                 (#'startup/start-native!
                  home
                  binary
                  "gateway"
                  ["gateway" "start" "--host" "127.0.0.1" "--port" (str port) "--require-token"])]

             (try (let [registered?
                        (loop [remaining 300]
                          (cond (not (.isAlive gateway)) false
                                (every? #(.isFile ^File %) markers) true
                                (zero? remaining) false
                                :else (do (Thread/sleep 50) (recur (dec remaining)))))

                        errors
                        (->> (cons gateway-log
                                   (filter #(and (.isFile ^File %)
                                                 (str/ends-with? (.getName ^File %) ".log"))
                                           (file-seq (io/file home ".vis/logs"))))
                             (mapcat #(str/split-lines (slurp %)))
                             (filter #(re-find #"(?i)error|exception|failed" %))
                             (take-last 20)
                             (str/join "\n"))]

                    (expect registered? (str "native SDK registration did not complete\n" errors))
                    (expect (#'startup/listening? port))
                    (doseq [marker markers]
                      (expect (= "registered" (slurp marker)))))
                  (finally (when (.isAlive gateway) (#'native/kill-tree! gateway)))))
           (finally (#'native/delete-tree! home))))))
