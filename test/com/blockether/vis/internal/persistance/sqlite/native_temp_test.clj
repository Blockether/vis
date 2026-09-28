(ns com.blockether.vis.internal.persistance.sqlite.native-temp-test
  (:require [babashka.fs :as fs]
            [com.blockether.vis.internal.persistance.sqlite.core]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files FileSystems]
           [java.nio.file.attribute PosixFilePermissions]))

(defdescribe
  sqlite-native-temp-directory-test
  (it
    "sqlite native temp directory"
    (locking org.sqlite.SQLiteJDBCLoader
      (let [keys
            ["user.home" "org.sqlite.tmpdir" "java.io.tmpdir"]

            original
            (zipmap keys (map #(System/getProperty %) keys))

            home
            (fs/create-temp-dir {:prefix "vis-sqlite-temp-"})

            ensure-dir!
            (ns-resolve 'com.blockether.vis.internal.persistance.sqlite.core
                        'ensure-native-temp-dir!)]

        (try (System/setProperty "user.home" (str home))
             (System/clearProperty "org.sqlite.tmpdir")
             ;; default is isolated and leaves the global temp directory unchanged
             (let [dir (ensure-dir!)]
               (expect (= (str (fs/path home ".vis" "native" "sqlite")) dir))
               (expect (fs/directory? dir))
               (expect (= dir (ensure-dir!)))
               (expect (= (original "java.io.tmpdir") (System/getProperty "java.io.tmpdir")))
               (when (contains? (.supportedFileAttributeViews (FileSystems/getDefault)) "posix")
                 (expect (= (PosixFilePermissions/fromString "rwx------")
                            (Files/getPosixFilePermissions (fs/path dir)
                                                           (make-array java.nio.file.LinkOption
                                                                       0))))))
             ;; an explicit driver directory is honored without creating it
             (let [custom (str (fs/path home "custom"))]
               (System/setProperty "org.sqlite.tmpdir" custom)
               (expect (= custom (ensure-dir!)))
               (expect (not (fs/exists? custom))))
             ;; a non-directory default fails without changing the property
             (System/clearProperty "org.sqlite.tmpdir")
             (let [dir (fs/path home ".vis" "native" "sqlite")]
               (fs/delete dir)
               (spit (str dir) "not a directory")
               (expect (try (ensure-dir!) false (catch java.io.IOException _ true)))
               (expect (nil? (System/getProperty "org.sqlite.tmpdir"))))
             (when (contains? (.supportedFileAttributeViews (FileSystems/getDefault)) "posix")
               ;; a symlink default is rejected
               (let [dir (fs/path home ".vis" "native" "sqlite")]
                 (fs/delete dir)
                 (fs/create-sym-link dir home)
                 (expect (try (ensure-dir!) false (catch java.io.IOException _ true)))
                 (expect (nil? (System/getProperty "org.sqlite.tmpdir")))))
             (finally (doseq [[key value] original]
                        (if value (System/setProperty key value) (System/clearProperty key)))
                      (fs/delete-tree home)))))))
