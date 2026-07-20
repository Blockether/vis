(ns com.blockether.vis.internal.jfr
  "Opt-in Java Flight Recorder profiling — ONE recording per PROCESS.

   Turned on per-process by the `VIS_JFR` env var (set by `bin/vis --jfr`). The
   var is INHERITED by the detached gateway daemon that a client spawns, so both
   the client (TUI/web) process AND the gateway daemon each start their OWN
   recording into a role+pid-tagged file under `~/.vis/logs/`:

     vis-client-<pid>-<ts>.jfr     ← the TUI / web / one-shot process
     vis-gateway-<pid>-<ts>.jfr    ← the long-lived gateway daemon

   That gives you two SEPARATE readings to compare when a client seems to be
   waiting on the gateway (`jfr print --events jdk.ExecutionSample <file>`).

   Works on the JVM always; on the compiled native binary only when it was built
   with `--enable-monitoring=jfr` (see the app's native-image.properties). Never
   throws and never blocks startup — if JFR is unavailable it just no-ops."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)
           (java.lang ProcessHandle)
           (java.time LocalDateTime)
           (java.time.format DateTimeFormatter)
           (jdk.jfr Configuration Recording)))

(defonce ^:private started (atom false))

(def ^:private truthy #{"1" "true" "yes" "on"})

(defn enabled?
  "True when `VIS_JFR` requests profiling for this process."
  []
  (boolean (truthy (some-> (System/getenv "VIS_JFR")
                           str
                           str/trim
                           str/lower-case))))

(defn- jfr-dir ^File [] (io/file (System/getProperty "user.home") ".vis" "logs"))

(defn- recording-file
  ^File [role]
  (let
    [ts
     (.format (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss") (LocalDateTime/now))

     pid
     (.pid (ProcessHandle/current))]

    (io/file (jfr-dir) (format "vis-%s-%s-%s.jfr" role pid ts))))

(def ^:private MAX_RECORDINGS
  "Newest JFR dumps to keep under ~/.vis/logs; older ones are pruned on each start so
   opt-in profiling can't silently fill the disk. `.setDumpOnExit` leaves one
   file per run tagged by pid, so dead processes' recordings pile up otherwise."
  6)

(defn- role-max-bytes
  "Ring-buffer cap. A short-lived TUI / one-shot client needs little; the
   long-lived gateway daemon gets more headroom."
  ^long [role]
  (* 1024 1024 (if (= role "gateway") 256 128)))

(defn- prune-old-recordings!
  "Keep only the newest `MAX_RECORDINGS` `vis-*.jfr` dumps under ~/.vis/logs and delete
   the rest (dead processes' orphaned recordings). Never throws."
  []
  (try (let
         [files (->> (.listFiles (jfr-dir))
                     (filter (fn [^File f]
                               (let [n (.getName f)]
                                 (and (.isFile f)
                                      (str/starts-with? n "vis-")
                                      (str/ends-with? n ".jfr")))))
                     (sort-by (fn [^File f]
                                (- (.lastModified f)))))]
         (doseq [^File f (drop MAX_RECORDINGS files)]
           (.delete f)))
       (catch Throwable _ nil)))

(defn maybe-start!
  "Start a per-process JFR recording tagged with `role` (\"client\"/\"gateway\")
   when `VIS_JFR` is set. Idempotent (first call per process wins), never throws.
   Returns the destination `File` when a recording started, else nil."
  [role]
  (when (and (enabled?) (compare-and-set! started false true))
    (try (.mkdirs (jfr-dir))
         (prune-old-recordings!)
         (let
           [f
            (recording-file role)

            rec
            (Recording. (Configuration/getConfiguration "profile"))]

           (doto rec
             (.setName (str "vis-" role))
             (.setDumpOnExit true)
             (.setMaxSize (role-max-bytes role))
             (.setDestination (.toPath f))
             (.start))
           (binding [*out* *err*]
             (println (str "vis: JFR profiling [" role "] \u2192 " f " (dumped on exit)")))
           f)
         (catch Throwable t
           (binding [*out* *err*]
             (println (str "vis: JFR profiling unavailable for [" role "]: " (.getMessage t))))
           nil))))
