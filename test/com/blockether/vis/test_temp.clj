(ns com.blockether.vis.test-temp
  "Temp directories that the test JVM deletes with their content at exit.

   `File.deleteOnExit` removes only an empty directory. Helpers that wrote files
   into their temp directory left thousands of trees in the user's temp directory."
  (:require [babashka.fs :as fs])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent ConcurrentLinkedQueue]))

(defonce ^:private ^ConcurrentLinkedQueue pending (ConcurrentLinkedQueue.))

(defonce ^:private exit-hook
  (delay (.addShutdownHook (Runtime/getRuntime)
                           (Thread. ^Runnable
                                    (fn []
                                      (doseq [root pending]
                                        ;; delete-tree does not follow symbolic links.
                                        (try (fs/delete-tree root) (catch Exception _ nil))))))))

(defn delete-at-exit!
  "Delete `root` and its content when the JVM exits. Returns `root`."
  [root]
  @exit-hook
  (.add ^ConcurrentLinkedQueue pending (str root))
  root)

(defn temp-dir
  "A new temp directory with `prefix` that the JVM deletes with its content at exit."
  ^File [prefix]
  (delete-at-exit! (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0)))))
