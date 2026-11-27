(ns com.blockether.vis.internal.gateway.turn-archive
  "Disk-backed gateway projections for terminal turns, including turns that never entered the engine."
  (:require [com.blockether.vis.internal.paths :as paths]
            [taoensso.nippy :as nippy]
            [taoensso.telemere :as tel])
  (:import [java.nio.file Files LinkOption NoSuchFileException OpenOption Path]
           [java.nio.file.attribute FileAttribute]))

(defn- create-directory!
  ^Path []
  (Files/createTempDirectory (.toPath (paths/temp-dir))
                             "vis-gateway-"
                             (make-array FileAttribute 0)))

(defonce ^:private directory
  (delay (let [current (atom (create-directory!))]
           ;; One process-owned directory, not deleteOnExit's ever-growing set of names.
           (.addShutdownHook (Runtime/getRuntime)
                             (Thread. ^Runnable
                                      (fn []
                                        (let [^Path path @current]
                                          (try (with-open [files (Files/newDirectoryStream path)]
                                                 (doseq [^Path file files]
                                                   (Files/deleteIfExists file)))
                                               (Files/deleteIfExists path)
                                               (catch Exception _ nil))))
                                      "gateway-archive-cleanup"))
           current)))

(defn- archive-file!
  "Create an empty archive file. When another program deleted the directory, make a new
   directory and try one more time. Without this, each write failed until the process ended."
  ^Path []
  (let [current
        @directory

        create
        #(Files/createTempFile ^Path @current "turn-" ".nippy" (make-array FileAttribute 0))]

    (try (create)
         (catch NoSuchFileException e
           (locking current
             (when-not (Files/isDirectory ^Path @current (make-array LinkOption 0))
               ;; Telemere evaluates `:data` only for a signal that it keeps: reset first.
               (let [directory (str (reset! current (create-directory!)))]
                 (tel/log! {:level :warn
                            :id ::directory-recreated
                            :data {:error-class (.getName (class e)) :directory directory}}))))
           (create)))))

(defn write-frozen!
  "Store bytes before publishing their registry descriptor."
  [^bytes frozen]
  (let [path (archive-file!)]
    ;; Resolve the byte-array overload statically; native reflection cannot call Files/write.
    (try (Files/write path
                      frozen
                      ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0))
         (str path)
         (catch Throwable t (Files/deleteIfExists path) (throw t)))))

(defn write!
  "Store a terminal projection before publishing its small registry descriptor."
  [turn]
  (write-frozen! (nippy/freeze turn)))

(defn read-turn
  "Read exactly one archived projection. Missing or damaged archives fail explicitly."
  [filename]
  (nippy/thaw (Files/readAllBytes (Path/of ^String filename (make-array String 0)))))

(defn delete!
  "Remove an archive when its registry session is forgotten."
  [filename]
  (try (Files/deleteIfExists (Path/of ^String filename (make-array String 0)))
       (catch Exception e
         (tel/log! {:level :warn :id ::cleanup-failed :data {:error-class (.getName (class e))}}))))
