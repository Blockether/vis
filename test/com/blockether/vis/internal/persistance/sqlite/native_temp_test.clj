(ns com.blockether.vis.internal.persistance.sqlite.native-temp-test
  (:require [babashka.fs :as fs]
            [com.blockether.vis.internal.persistance.sqlite.core]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import [java.nio.file Files FileSystems]
           [java.nio.file.attribute PosixFilePermissions]))

(deftest sqlite-native-temp-directory-test
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
           (testing "default is isolated and leaves the global temp directory unchanged"
             (let [dir (ensure-dir!)]
               (is (= (str (fs/path home ".vis" "native" "sqlite")) dir))
               (is (fs/directory? dir))
               (is (= dir (ensure-dir!)))
               (is (= (original "java.io.tmpdir") (System/getProperty "java.io.tmpdir")))
               (when (contains? (.supportedFileAttributeViews (FileSystems/getDefault)) "posix")
                 (is (= (PosixFilePermissions/fromString "rwx------")
                        (Files/getPosixFilePermissions (fs/path dir)
                                                       (make-array java.nio.file.LinkOption 0)))))))
           (testing "an explicit driver directory is honored without creating it"
             (let [custom (str (fs/path home "custom"))]
               (System/setProperty "org.sqlite.tmpdir" custom)
               (is (= custom (ensure-dir!)))
               (is (not (fs/exists? custom)))))
           (testing "a non-directory default fails without changing the property"
             (System/clearProperty "org.sqlite.tmpdir")
             (let [dir (fs/path home ".vis" "native" "sqlite")]
               (fs/delete dir)
               (spit (str dir) "not a directory")
               (is (try (ensure-dir!) false (catch java.io.IOException _ true)))
               (is (nil? (System/getProperty "org.sqlite.tmpdir")))))
           (when (contains? (.supportedFileAttributeViews (FileSystems/getDefault)) "posix")
             (testing "a symlink default is rejected"
               (let [dir (fs/path home ".vis" "native" "sqlite")]
                 (fs/delete dir)
                 (fs/create-sym-link dir home)
                 (is (try (ensure-dir!) false (catch java.io.IOException _ true)))
                 (is (nil? (System/getProperty "org.sqlite.tmpdir"))))))
           (finally (doseq [[key value] original]
                      (if value (System/setProperty key value) (System/clearProperty key)))
                    (fs/delete-tree home))))))
