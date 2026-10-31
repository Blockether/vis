;; User request 2026-10-31: each native start copied bundled dylibs to new temp files,
;; and macOS scanned each new file for about 1 s at its first dlopen.
(ns com.blockether.vis.internal.native-libs-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.native-libs :as native-libs]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- sqlite-resource
  "A native library that sits inside a jar on the test classpath."
  []
  (let [lib-name (org.sqlite.util.LibraryLoaderUtil/getNativeLibName)]
    [(str (subs (org.sqlite.util.LibraryLoaderUtil/getNativeLibResourcePath) 1) "/" lib-name)
     lib-name]))

(defn- with-home
  "Call `f` with a new empty `user.home`, then restore the old one."
  [f]
  (let [old
        (System/getProperty "user.home")

        home
        (str (Files/createTempDirectory "native-libs-test" (make-array FileAttribute 0)))]

    (try (System/setProperty "user.home" home)
         (f (io/file home))
         (finally (System/setProperty "user.home" old)))))

(defdescribe
  stage-test
  (it "reuses one complete copy in later calls instead of a new file"
      (with-home
        (fn [_]
          (let [[res lib-name]
                (sqlite-resource)

                ^File first-copy
                (native-libs/stage! "sqlite-jdbc" res lib-name)

                inode
                (Files/getAttribute (.toPath first-copy)
                                    "unix:ino"
                                    (make-array java.nio.file.LinkOption 0))

                ^File second-copy
                (native-libs/stage! "sqlite-jdbc" res lib-name)]

            (expect (= first-copy second-copy))
            (expect (= inode
                       (Files/getAttribute (.toPath second-copy)
                                           "unix:ino"
                                           (make-array java.nio.file.LinkOption 0))))
            (expect (= (with-open [in (io/input-stream (io/resource res))]
                         (alength (.readAllBytes in)))
                       (.length second-copy)))))))
  (it "deletes the copy of an old container that no process claims"
      (with-home (fn [^File home]
                   (let [[res lib-name]
                         (sqlite-resource)

                         old
                         (io/file home ".vis" "native" "sqlite-jdbc" "0000000000000000")]

                     (.mkdirs old)
                     (spit (io/file old ".vis-live") "")
                     (spit (io/file old lib-name) "old")
                     (native-libs/stage! "sqlite-jdbc" res lib-name)
                     (expect (not (.exists old)))))))
  (it "answers nil for a missing resource"
      (with-home (fn [_]
                   (expect (nil? (native-libs/stage! "none" "no/such/lib.dylib" "lib.dylib")))))))
