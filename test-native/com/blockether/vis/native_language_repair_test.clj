(ns com.blockether.vis.native-language-repair-test
  "Published language hooks through the built CLI and its extension workers."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]]
            [yamlstar.core :as yaml])
  (:import [com.sun.net.httpserver HttpServer]
           [java.io File]))

(defdescribe
  native-language-repair-test
  (it
    "repairs blocks in the host and file edits with published hooks and full reader validation"
    (let
      [home
       (#'native/temp-dir "vis-native-language-repair-")

       dir
       (doto (io/file home "workspace") .mkdirs)

       binary
       (#'native/require-binary)

       packages
       (doto (io/file dir "packages") .mkdirs)

       environment
       (assoc (#'native/native-environment) "VIS_PYTHON_PACKAGES" (.getCanonicalPath packages))

       pins
       (select-keys (get (yaml/load (slurp "vis.yml")) "extensions")
                    ["vis-lang-interface" "vis-lang-python" "vis-lang-clojure"])

       original-stream
       @#'native/stream-body

       original-whole
       @#'native/whole-body

       calls
       (atom 0)

       fixtures
       ["print('Native block repair verified'"
        (str "import re\n"
             "for name, replacement, expected in ["
             "('sample.py', 'value = [1, 2', 'value = [1, 2]'), "
             "('sample.clj', '(def value [1 2', '(def value [1 2])')]:\n"
             "    path = project_root_path / name\n"
             "    anchor = re.search(r'1:[0-9a-f]+', await cat(path)).group(0)\n"
             "    print(await patch(path, [{'from': anchor, 'replace': replacement}]))\n"
             "    assert path.read_text().strip() == expected, path.read_text()\n"
             "print('Native patch repair verified')")
        (str "(project_root_path / 'late.py').write_text('value = [3, 4\\n')\n"
             "(project_root_path / 'late.clj').write_text('(def late [3 4]\\n')\n"
             "raise ValueError('failure after writes')")
        (str
          "assert (project_root_path / 'late.py').read_text().strip() == 'value = [3, 4]'\n"
          "assert (project_root_path / 'late.clj').read_text().strip() == '(def late [3 4])'\n"
          "assert session.get('python_syntax_repairs')\n"
          "assert session.get('clojure_syntax_repairs')\n"
          "for name, replacement in [('sample.py', 'value = if'), ('sample.clj', '{:a 1 :a 2}')]:\n"
          "    path = project_root_path / name\n"
          "    before = path.read_text()\n"
          "    anchor = re.search(r'1:[0-9a-f]+', await cat(path)).group(0)\n"
          "    try:\n" "        await patch(path, [{'from': anchor, 'replace': replacement}])\n"
          "    except Exception as error:\n"
          "        assert 'unparseable, so nothing was written' in str(error), str(error)\n"
          "    else:\n" "        raise AssertionError('invalid patch was accepted')\n"
          "    assert path.read_text() == before\n"
          "formatted = project_root_path / 'formatted.clj'\n"
          "formatted.write_text('(def formatted [1 2])  \\n')\n"
          "layout = await clj.format_code(paths=[str(formatted)], cwd=str(project_root_path),"
          " is_written=True)\n" "assert layout.is_written and layout.changed, layout\n"
          "assert formatted.read_text() == '(def formatted [1 2])\\n', formatted.read_text()\n"
          "broken = project_root_path / 'format.clj'\n"
          "broken.write_text('(def broken [1\\n')\n" "before = broken.read_text()\n"
          "try:\n"
          "    await clj.format_code(paths=[str(broken)], cwd=str(project_root_path), is_written=True)\n"
          "except Exception:\n" "    pass\n"
          "assert broken.read_text() == before\n"
          "print('Native file repair and reader validation verified')")
        ;; Parinferish cannot repair this block, so the host diagnosis must name the cause.
        "x = (1 + 2\ny = 3 3"]

       reply
       (fn [stream? text]
         (let [n (swap! calls inc)]
           (if-let [code (get fixtures (dec n))]
             (#'native/python-tool-body code n stream?)
             ((if stream? original-stream original-whole) text))))]

      (try (expect (= 3 (count pins)))
           (spit (io/file dir "vis.yml") (json/write-json-str {"extensions" pins}))
           (spit (io/file dir "sample.py") "value = [1]\n")
           (spit (io/file dir "sample.clj") "(def value [1])\n")
           (with-redefs-fn {#'native/native-environment (constantly environment)
                            #'native/stream-body #(reply true %)
                            #'native/whole-body #(reply false %)}
             (fn []
               (let [synced (#'native/run-binary
                             dir
                             [(.getAbsolutePath ^File binary) (str "-Duser.home=" home) "extension"
                              "sync" "--project" "--trust"]
                             240)]
                 (expect (:finished? synced) (:output synced))
                 (expect (= 0 (:exit synced)) (:output synced))
                 (expect (str/includes? (:output synced) "Synced 3 package(s)") (:output synced)))
               (let [{:keys [server asked port]} (#'native/start-stub-provider!
                                                  "Language repair check complete")]
                 (try (#'native/overlay! dir port)
                      (let [{:keys [finished? exit output]}
                            (#'native/run-binary
                             dir
                             [(.getAbsolutePath ^File binary) (str "-Duser.home=" home) "--db"
                              (.getAbsolutePath (io/file dir "sessions")) "--raw"
                              "Run the supplied language repair fixtures and finish."]
                             360)
                            tools (#'native/provider-result-messages @asked)]

                        (expect finished? output)
                        (expect (= 0 exit)
                                (str output
                                     (when-not (= 0 exit)
                                       (str/join
                                         "\n"
                                         (for [^File file (file-seq (io/file dir ".vis/logs"))
                                               :when (and (.isFile file)
                                                          (str/ends-with? (.getName file) ".log"))]

                                           (str/join
                                             "\n"
                                             (take-last 30 (str/split-lines (slurp file)))))))))
                        (doseq [marker ["Vis repaired this block before running it."
                                        "Native block repair verified"
                                        "Native patch repair verified" "failure after writes"
                                        "Native file repair and reader validation verified"
                                        "line 1, column 5: '(' is never closed"]]
                          (expect (some #(str/includes? % marker) tools)
                                  (str marker "\n" output "\nTool results: " (pr-str tools)))))
                      (finally (.stop ^HttpServer server 0))))))
           (finally (#'native/delete-tree! home))))))
