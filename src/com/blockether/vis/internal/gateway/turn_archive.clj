(ns com.blockether.vis.internal.gateway.turn-archive
  "Disk-backed gateway projections for terminal turns, including turns that never entered the engine."
  (:require [taoensso.nippy :as nippy]
            [taoensso.telemere :as tel])
  (:import [java.nio.file Files Path OpenOption]
           [java.nio.file.attribute FileAttribute]))

(defonce ^:private directory
  (delay (let [path (Files/createTempDirectory "vis-gateway-" (make-array FileAttribute 0))]
           ;; One process-owned directory, not deleteOnExit's ever-growing set of names.
           (.addShutdownHook (Runtime/getRuntime)
                             (Thread. ^Runnable
                                      (fn []
                                        (try (with-open [files (Files/newDirectoryStream path)]
                                               (doseq [^Path file files]
                                                 (Files/deleteIfExists file)))
                                             (Files/deleteIfExists path)
                                             (catch Exception _ nil)))
                                      "gateway-archive-cleanup"))
           path)))

(defn write!
  "Store a terminal projection before publishing its small registry descriptor."
  [turn]
  (let [path (Files/createTempFile ^Path @directory "turn-" ".nippy" (make-array FileAttribute 0))]
    ;; Resolve the byte-array overload statically; native reflection cannot call Files/write.
    (try (Files/write path
                      ^bytes (nippy/freeze turn)
                      ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0))
         (str path)
         (catch Throwable t (Files/deleteIfExists path) (throw t)))))

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
