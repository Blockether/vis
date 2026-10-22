(ns com.blockether.vis.test-parallel
  "Run the Lazytest suite in a pool of worker JVMs: `clojure -M:test:test-parallel`.

   Many tests change process-wide state, so the workers are separate processes,
   never threads. Each worker gets its own drafts, database, logs and image
   directories. The coordinator sends one namespace at a time to a free worker,
   the slowest first, from the times that the previous run saved. A worker reports
   each namespace on its real stdout, so a test that rebinds `*out*` cannot hide it.

   Arguments: `--workers N` (default: half the processors, at most 6) and optional
   namespace names. `VIS_TEST_WORKERS` also sets the worker count."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.tools.namespace.find :as ns-find]
            [lazytest.hooks :as hooks])
  (:import [java.io BufferedReader File FileDescriptor FileOutputStream PrintStream Writer]
           [java.lang.management ManagementFactory]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(set! *warn-on-reflection* true)

(def ^:private done-marker
  "The prefix of the line that ends one namespace in the worker output."
  "\u0001vis-test-done ")

(def ^:private run-dir
  "The directory for worker state, logs and the saved namespace times."
  (io/file "target" "test-parallel"))

(def ^:private timings-file (io/file run-dir "timings.edn"))

(def ^:private worker-properties
  "The path properties of the `:test` alias that each worker gets for itself."
  ["vis.drafts.dir" "vis.db.path" "vis.image-descriptions.dir" "vis.logs.dir"])

;;; Worker

(defn- initialize-builtins!
  "Register the built-in extensions first, as the engine does at startup. A test
   that triggers the first registration inside a temporary registry loses it for
   the whole process, because the manifest registers each entry only once."
  []
  ((requiring-resolve 'com.blockether.vis.internal.extension.manifest/initialize!)))

(hooks/defhook builtins
               "Register the built-in extensions before the serial `:test` run."
               (pre-test-run [_config m] (initialize-builtins!) m))

(defn- run-namespace
  "Load and run one test namespace. Return its summary."
  [ns-name]
  (let [config
        ((requiring-resolve 'lazytest.config/->config)
          {:hooks ['com.blockether.vis.test-network-guard/no-network]})

        started
        (System/nanoTime)

        summary
        (try (require (symbol ns-name))
             ((requiring-resolve 'lazytest.results/summarize)
               ((requiring-resolve 'lazytest.runner/run-tests) [(symbol ns-name)] config))
             (catch Throwable t
               (.printStackTrace t)
               {:total 0 :pass 0 :fail 1 :error (.getMessage t)}))]

    (flush)
    (assoc summary :ms (quot (- (System/nanoTime) started) 1000000))))

(defn- worker!
  "Read namespace names from stdin until it closes. Report each on the real stdout."
  []
  (initialize-builtins!)
  (let [report (PrintStream. (FileOutputStream. FileDescriptor/out) true "UTF-8")]
    (doseq [ns-name (line-seq (BufferedReader. *in*))
            :when (seq ns-name)]

      (let [summary (run-namespace ns-name)]
        (.flush System/out)
        (.println report (str done-marker (pr-str (assoc summary :ns ns-name))))))
    (shutdown-agents)
    (System/exit 0)))

;;; Coordinator

(defn- test-namespaces
  "The namespace names of the test files under `test`."
  []
  (->> (ns-find/find-sources-in-dir (io/file "test"))
       (filter #(str/ends-with? (.getName ^File %) "_test.clj"))
       (keep ns-file/read-file-ns-decl)
       (map (comp str second))
       sort))

(defn- saved-timings [] (try (edn/read-string (slurp timings-file)) (catch Exception _ {})))

(defn- by-cost
  "Sort `names` slowest first, by the saved time in ms. A namespace without one
   uses its file size in bytes instead, a rough estimate for a first run."
  [names timings]
  (letfn [(cost [ns-name]
            (or (get timings ns-name)
                (.length (io/file "test"
                                  (str (-> ^String ns-name
                                           (str/replace "." "/")
                                           (str/replace "-" "_"))
                                       ".clj")))))]
    (sort-by #(- (long (cost %))) names)))

(defn- worker-command
  "The java command line of worker `index`: this JVM's options with its own paths."
  [index]
  (let [dir
        (.getAbsoluteFile (io/file run-dir (str "worker-" index)))

        own
        {"vis.drafts.dir" (io/file dir "drafts")
         "vis.db.path" (io/file dir "vis-test.mdb")
         "vis.image-descriptions.dir" (io/file dir "image-descriptions")
         "vis.logs.dir" (io/file dir "logs")}

        inherited
        (remove (fn [^String arg]
                  (some #(str/starts-with? arg (str "-D" % "=")) worker-properties))
          (.getInputArguments (ManagementFactory/getRuntimeMXBean)))]

    (vec (concat [(str (io/file (System/getProperty "java.home") "bin" "java"))]
                 inherited
                 (for [[k v] own]
                   (str "-D" k "=" v))
                 ["-cp" (System/getProperty "java.class.path") "clojure.main" "-m"
                  "com.blockether.vis.test-parallel" "--worker"]))))

(defn- start-worker!
  [index]
  (.mkdirs (io/file run-dir (str "worker-" index)))
  (let [log
        (io/file run-dir (str "worker-" index ".log"))

        process
        (.start (doto (ProcessBuilder. ^java.util.List (worker-command index))
                  (.redirectErrorStream true)))]

    {:index index
     :process process
     :log log
     :input (io/writer (.getOutputStream process))
     :output (io/reader (.getInputStream process))}))

(defn- run-on-worker!
  "Send one namespace to `worker`. Return its summary and the output lines it printed."
  [{:keys [^Writer input ^BufferedReader output log]} ns-name]
  (.write input (str ns-name "\n"))
  (.flush input)
  (with-open [^Writer log-writer (io/writer log :append true)]
    (loop [lines []]
      (if-let [line (.readLine output)]
        (if (str/starts-with? line done-marker)
          {:summary (edn/read-string (subs line (count done-marker))) :lines lines}
          (do (.write log-writer (str line "\n")) (recur (conj lines line))))
        {:summary {:ns ns-name :total 0 :pass 0 :fail 1 :error "The worker JVM stopped."}
         :lines lines}))))

(defn- fmt
  "Format `s` with a dot as the decimal separator, whatever the system locale is."
  [^String s & args]
  (String/format java.util.Locale/ROOT s (to-array args)))

(defn- report!
  [{:keys [summary lines]} ^Object lock]
  (locking lock
    (let [{:keys [ns total fail ms error]} summary]
      (when (or (pos? (long fail)) error) (run! println lines))
      (println (fmt "%s %s: %d tests, %d failed, %.1f s%s"
                    (if (or (pos? (long fail)) error) "FAIL" "ok  ")
                    ns
                    total
                    fail
                    (/ (double ms) 1000.0)
                    (if error (str " (" error ")") "")))
      (flush))))

(defn- default-workers
  []
  (or (some-> (System/getenv "VIS_TEST_WORKERS")
              parse-long)
      (max 1 (min 6 (quot (.availableProcessors (Runtime/getRuntime)) 2)))))

(defn- coordinate!
  [{:keys [workers names]}]
  (.mkdirs ^File run-dir)
  (let [timings
        (saved-timings)

        queue
        (LinkedBlockingQueue. ^java.util.Collection (by-cost names timings))

        results
        (atom [])

        lock
        (Object.)

        started
        (System/nanoTime)

        threads
        (doall (for [index (range (min workers (count names)))]
                 (future (io/delete-file (io/file run-dir (str "worker-" index ".log")) true)
                         (loop [worker (start-worker! index)]
                           (when-let [ns-name (.poll queue 0 TimeUnit/SECONDS)]
                             (let [result (run-on-worker! worker ns-name)
                                   alive? (.isAlive ^Process (:process worker))]

                               (swap! results conj (:summary result))
                               (report! result lock)
                               (recur (if alive? worker (start-worker! index))))))
                         nil)))]

    (run! deref threads)
    (let [summaries
          @results

          total
          (reduce + (map :total summaries))

          failed
          (filter #(or (pos? (long (:fail %))) (:error %)) summaries)

          seconds
          (/ (double (- (System/nanoTime) started)) 1e9)]

      (spit timings-file (pr-str (into timings (map (juxt :ns :ms)) summaries)))
      (println (fmt "\nRan %d test cases in %d namespaces on %d workers in %.1f s."
                    total
                    (count summaries)
                    workers
                    seconds))
      (println
        (if (seq failed)
          (str (reduce + (map :fail failed)) " failures in: " (str/join ", " (map :ns failed)))
          "0 failures."))
      (if (or (seq failed) (zero? total)) 1 0))))

(defn- parse-args
  [args]
  (loop [[arg & more]
         args

         opts
         {:workers (default-workers) :names []}]

    (cond (nil? arg) opts
          (= "--workers" arg) (recur (rest more) (assoc opts :workers (parse-long (first more))))
          :else (recur more (update opts :names conj arg)))))

(defn -main
  [& args]
  (if (= ["--worker"] args)
    (worker!)
    (let [{:keys [names] :as opts}
          (parse-args args)

          exit
          (coordinate! (assoc opts :names (if (seq names) names (test-namespaces))))]

      (shutdown-agents)
      (System/exit exit))))
