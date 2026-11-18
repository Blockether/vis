(ns com.blockether.vis.internal.python.cli-support
  "Clean-process helpers of the Python CLI tests. Each source-launcher JVM loads the whole
   engine, so the tests start the children before their turn and run them together."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [com.blockether.vispython Interpreter]
           [java.io File]
           [java.nio.file Files]
           [java.util.concurrent TimeUnit]
           [java.util.zip ZipEntry ZipOutputStream]))

(set! *warn-on-reflection* true)

(defn delete-tree!
  [^File dir]
  (doseq [^File file (reverse (file-seq dir))]
    (.delete file)))

(defn- shared-uv-form
  "The form that `-main` runs for `python --shared uv ARGS`, without loading the engine.
   `args` is a form that gives the arguments."
  [args]
  `(do (require 'com.blockether.vis.internal.python.runtime)
       (System/exit (try ((resolve 'com.blockether.vis.internal.python.runtime/uv-command!)
                           ~args
                           {:shared? true})
                         (catch Throwable t#
                           (binding [*out* *err*]
                             (println (.getMessage t#)))
                           1)))))

(defn- gated-program
  "The program of a child that loads its code, waits for the file `gate` and then runs the
   arguments in that file: `python ARGS` through `-main`, or `--shared uv ARGS` without
   the engine when `runtime-only?`. The child ends when this test process is gone."
  [^File gate runtime-only?]
  (let [path
        (.getPath gate)

        parent
        (.pid (java.lang.ProcessHandle/current))]

    (pr-str
      `(do
         (require '~(if
                     runtime-only?
                     'com.blockether.vis.internal.python.runtime
                     'com.blockether.vis.core))
         (while (not (.isFile (File. ~path)))
           (when-not (.isPresent (java.lang.ProcessHandle/of ~parent)) (System/exit 1))
           (Thread/sleep 5))
         (let [~'args (read-string (slurp ~path))]
           ~(if runtime-only?
              (shared-uv-form '(vec (drop 2 args)))
              '(apply (resolve 'com.blockether.vis.core/-main) "python" args)))))))

(defn- start-cli!
  "Start `vis-agent python` from source like the launcher: the process runs in `dir`
   and an :invocation-dir travels in user.dir. :input is written to its stdin.
   :runtime-only? runs a `--shared uv` invocation without the engine load of `-main`.
   :gated? ignores `args`: the child loads its code and then waits for the :gate file."
  [^File dir environment args {:keys [invocation-dir jvm-opts ^String input runtime-only? gated?]}]
  (let [classpath
        (str/join File/pathSeparator
                  (map (fn [entry]
                         (let [file (io/file entry)]
                           (.getCanonicalPath file)))
                       (str/split (System/getProperty "java.class.path")
                                  (re-pattern (java.util.regex.Pattern/quote File/pathSeparator)))))

        log
        (str "cli-" (java.util.UUID/randomUUID))

        gate
        (io/file dir (str log ".gate"))

        command
        (into (cond-> (into [(str (io/file (System/getProperty "java.home") "bin/java"))
                             "--enable-native-access=ALL-UNNAMED" "--enable-preview"]
                            jvm-opts)
                invocation-dir
                (conj (str "-Duser.dir=" (.getCanonicalPath ^File invocation-dir)))

                gated?
                (into ["-cp" classpath "clojure.main" "-e" (gated-program gate runtime-only?)])

                (and runtime-only? (not gated?))
                (into ["-cp" classpath "clojure.main" "-e"
                       (pr-str (shared-uv-form (vec (drop 2 args))))])

                (not (or runtime-only? gated?))
                (into ["-cp" classpath "clojure.main" "-m" "com.blockether.vis.core" "python"]))
              (when-not (or runtime-only? gated?) args))

        output
        (io/file dir (str log ".log"))

        errors
        (io/file dir (str log ".err"))

        builder
        (doto (ProcessBuilder. ^java.util.List command)
          (.directory dir)
          (.redirectError errors)
          (.redirectOutput output))]

    (doseq [key ["PYTHONPATH" "UV_PROJECT_ENVIRONMENT" "VIRTUAL_ENV"]]
      (.remove (.environment builder) key))
    ;; Without packages of its own, a test must not get the shared packages of the person.
    (.put (.environment builder)
          "VIS_PYTHON_PACKAGES"
          (.getCanonicalPath (io/file dir "default-shared-packages")))
    (.putAll (.environment builder) environment)
    (let [process (.start builder)]
      (with-open [stdin (.getOutputStream process)]
        (when input (.write stdin (.getBytes input "UTF-8"))))
      {:process process :output output :errors errors :gate gate})))

(defn- await-cli!
  "Wait for a started CLI. :output joins stdout and stderr for messages; :stderr omits
   the JVM's JAVA_TOOL_OPTIONS notice (CI sets it), which is not the program's output."
  [{:keys [^Process process output errors]} timeout-seconds]
  (try (when-not (.waitFor process timeout-seconds TimeUnit/SECONDS)
         (throw (ex-info "Python CLI timed out" {:output (str (slurp output) (slurp errors))})))
       (let [stdout
             (slurp output)

             stderr
             (str/replace (slurp errors) #"(?m)^Picked up .*\n?" "")]

         {:exit (.exitValue process) :stdout stdout :stderr stderr :output (str stdout stderr)})
       (finally (when (.isAlive process)
                  (doseq [^java.lang.ProcessHandle child (-> process
                                                             .toHandle
                                                             .descendants
                                                             .toList)]
                    (.destroyForcibly child))
                  (.destroyForcibly process)
                  (.waitFor process 10 TimeUnit/SECONDS)))))

;; C1 and the serial collector roughly halve the CPU of each short-lived JVM.
(def ^:private child-jvm-opts ["-XX:TieredStopAtLevel=1" "-XX:+UseSerialGC"])

(defn run-uv-cli
  "Run a `--shared uv` invocation in a clean process without the engine load of `-main`.
   python-cli-shared-sync-test proves the real dispatch of such an invocation."
  [dir environment args]
  (await-cli! (start-cli! dir environment args {:runtime-only? true :jvm-opts child-jvm-opts}) 90))

(def ^:private child-limit
  "The number of child JVMs that live at once: half of the CPU cores, and three at the least."
  (max 3 (quot (.availableProcessors (Runtime/getRuntime)) 2)))

(defn cli-queue
  "Start the child of each slot before its turn, so that the code of a child loads while
   an earlier invocation runs. A slot is a map of :dir, :environment and the :options and
   :input of `start-cli!`. At most `child-limit` children live at once. `:invoke!` gives
   its arguments to the next child and waits for the result: the invocations still run
   one after another, each in a clean process. `:stop!` ends the children that did not run."
  [slots]
  (let [waiting
        (atom (seq slots))

        ready
        (atom clojure.lang.PersistentQueue/EMPTY)

        start-next!
        (fn []
          (when-let [{:keys [dir environment options input]} (first @waiting)]
            (swap! waiting next)
            (swap! ready conj
              (start-cli! dir
                          environment
                          nil
                          (assoc options
                            :input input
                            :jvm-opts child-jvm-opts
                            :gated? true)))))]

    (dotimes [_ child-limit]
      (start-next!))
    {:invoke! (fn [args]
                (let [child
                      (or (peek @ready) (throw (ex-info "No Python CLI child is left" {})))

                      ^File gate
                      (:gate child)

                      draft
                      (io/file (str gate ".new"))]

                  (swap! ready pop)
                  (spit draft (pr-str (vec args)))
                  ;; The child reads the gate when it exists, so it must appear complete.
                  (Files/move (.toPath draft)
                              (.toPath gate)
                              (into-array java.nio.file.CopyOption
                                          [java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
                  (try (await-cli! child 240) (finally (start-next!)))))
     :stop! (fn []
              (reset! waiting nil)
              (doseq [{:keys [^Process process]} @ready]
                (doseq [^java.lang.ProcessHandle child (-> process
                                                           .toHandle
                                                           .descendants
                                                           .toList)]
                  (.destroyForcibly child))
                (.destroyForcibly process)
                (.waitFor process 10 TimeUnit/SECONDS))
              (reset! ready clojure.lang.PersistentQueue/EMPTY))}))

(defn run-cli-together
  "Run independent invocations, at most `child-limit` at once, so that the test does not
   use all CPU cores. Answer the results in the order of `invocations`."
  [dir invocations]
  (into []
        (mapcat (fn [batch]
                  (let [started (mapv (fn [{:keys [environment args options input]}]
                                        (start-cli! dir
                                                    environment
                                                    args
                                                    (assoc options
                                                      :input input
                                                      :jvm-opts child-jvm-opts)))
                                      batch)]
                    (mapv #(await-cli! % 240) started))))
        (partition-all child-limit invocations)))

(defn shared-fixture-wheel!
  "Write a wheel of one module. With `script`, the wheel declares that console script, which
   prints its arguments and returns 5; the module run as `__main__` prints them too. It also
   installs the shell program `SCRIPT-program`, which prints its arguments and exits 4."
  (^File [^File project module] (shared-fixture-wheel! project module nil))
  (^File [^File project module script]
   (let [dist
         (str module "-1.0.0.dist-info/")

         wheel
         (io/file project (str module "-1.0.0-py3-none-any.whl"))]

     (with-open [zip (ZipOutputStream. (io/output-stream wheel))]
       (doseq [[path text]
               (cond-> {(str module ".py") "VALUE = 42\n"
                        (str dist "METADATA")
                        (str "Metadata-Version: 2.1\nName: " module "\nVersion: 1.0.0\n")
                        (str dist "WHEEL")
                        "Wheel-Version: 1.0\nRoot-Is-Purelib: true\nTag: py3-none-any\n"
                        (str dist "RECORD") ""}
                 script
                 (assoc (str module ".py")
                   (str "import sys\nVALUE = 42\ndef main():\n"
                        "    print('script', sys.argv[1:])\n    return 5\n"
                        "if __name__ == '__main__':\n    print('module', sys.argv[1:])\n")
                   (str dist "entry_points.txt")
                   (str "[console_scripts]\n" script " = " module ":main\n")
                   (str module "-1.0.0.data/scripts/" script "-program")
                   "#!/bin/sh\necho program \"$@\"\nexit 4\n"))]
         (.putNextEntry zip (ZipEntry. ^String path))
         (.write zip (.getBytes ^String text "UTF-8"))
         (.closeEntry zip)))
     wheel)))

(defn runtime-installs
  "The files of `module` in the tree of the Python runtime itself, where no install belongs."
  [module]
  (let [home (-> (Interpreter/pythonExecutable)
                 io/file
                 .getCanonicalFile
                 .getParentFile
                 .getParentFile)]
    (filterv #(str/starts-with? (.getName ^File %) module) (file-seq (io/file home "lib")))))
