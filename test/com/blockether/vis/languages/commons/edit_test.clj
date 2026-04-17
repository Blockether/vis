(ns com.blockether.vis.languages.commons.edit-test
  (:require
   [clojure.java.io :as io]
   [lazytest.core :refer [defdescribe describe it expect]]
   [com.blockether.vis.languages.commons.edit :as sut]))

(defn- tmp-dir
  []
  (let [d (.toFile (java.nio.file.Files/createTempDirectory "vis-edit-patch-test-" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.deleteOnExit d)
    d))

(defn- file-in
  [^java.io.File dir rel]
  (io/file dir rel))

(defn- write-file!
  [^java.io.File f content]
  (when-let [p (.getParentFile f)]
    (.mkdirs p))
  (spit f content :encoding "UTF-8")
  f)

(defn- read-file
  [^java.io.File f]
  (slurp f :encoding "UTF-8"))

(defdescribe edit-file-openai-patch-test
  (describe "edit-file OpenAI-style patch"
    (it "applies multi-file add/update/delete in one patch"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo\nthree")
            b (write-file! (file-in dir "b.txt") "to delete")
            c (file-in dir "c.txt")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -1,3 +1,3 @@\n"
                    " one\n"
                    "-two\n"
                    "+TWO\n"
                    " three\n"
                    "*** Delete File: " (.getAbsolutePath b) "\n"
                    "*** Add File: " (.getAbsolutePath c) "\n"
                    "+hello\n"
                    "+world\n"
                    "*** End Patch")
            result (sut/edit-file patch)]
        (expect (= "one\nTWO\nthree" (read-file a)))
        (expect (false? (.exists b)))
        (expect (= "hello\nworld" (read-file c)))
        (expect (= 3 (:total-files result)))
        (expect (= 1 (:total-hunks result)))))

    (it "supports update + move"
      (let [dir (tmp-dir)
            src (write-file! (file-in dir "old/name.txt") "alpha\nbeta")
            dst (file-in dir "new/name.txt")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath src) "\n"
                    "*** Move to: " (.getAbsolutePath dst) "\n"
                    "@@ -1,2 +1,2 @@\n"
                    " alpha\n"
                    "-beta\n"
                    "+BETA\n"
                    "*** End Patch")
            result (sut/edit-file patch)]
        (expect (false? (.exists src)))
        (expect (= "alpha\nBETA" (read-file dst)))
        (expect (= :move (:action (first (:files result)))))))

    (it "is atomic when one section fails"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo")
            b (write-file! (file-in dir "b.txt") "exists")
            before-a (read-file a)
            before-b (read-file b)
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -1,2 +1,2 @@\n"
                    " one\n"
                    "-two\n"
                    "+TWO\n"
                    "*** Add File: " (.getAbsolutePath b) "\n"
                    "+should fail\n"
                    "*** End Patch")
            threw? (try
                     (sut/edit-file patch)
                     false
                     (catch Exception _ true))]
        (expect (true? threw?))
        (expect (= before-a (read-file a)))
        (expect (= before-b (read-file b)))))

    (it "returns structured error with file/hunk context for mismatch"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -1,2 +1,2 @@\n"
                    " one\n"
                    "-THAT-DOES-NOT-EXIST\n"
                    "+TWO\n"
                    "*** End Patch")
            ex (try
                 (sut/edit-file patch)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/edit-context-not-found (:type (ex-data ex))))
        (expect (= 0 (:hunk-index (ex-data ex))))
        (expect (= (.getAbsolutePath a) (:path (ex-data ex))))))

    (it "rejects non-patch legacy calls"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one")
            ex (try
                 (apply sut/edit-file [(.getAbsolutePath a) "old" "new"])
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (some? ex))))

    (it "rejects duplicate operations on same file path"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -1,2 +1,2 @@\n"
                    " one\n"
                    "-two\n"
                    "+TWO\n"
                    "*** Delete File: " (.getAbsolutePath a) "\n"
                    "*** End Patch")
            ex (try
                 (sut/edit-file patch)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/edit-conflict (:type (ex-data ex))))))

    (it "rejects move target collisions within same patch"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo")
            b (write-file! (file-in dir "b.txt") "three\nfour")
            dst (file-in dir "dst.txt")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "*** Move to: " (.getAbsolutePath dst) "\n"
                    "@@ -1,2 +1,2 @@\n"
                    " one\n"
                    "-two\n"
                    "+TWO\n"
                    "*** Update File: " (.getAbsolutePath b) "\n"
                    "*** Move to: " (.getAbsolutePath dst) "\n"
                    "@@ -1,2 +1,2 @@\n"
                    " three\n"
                    "-four\n"
                    "+FOUR\n"
                    "*** End Patch")
            ex (try
                 (sut/edit-file patch)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/edit-conflict (:type (ex-data ex))))))

    (it "rejects empty lines in patch envelope"
      (let [patch (str "*** Begin Patch\n"
                    "\n"
                    "*** End Patch")
            ex (try
                 (sut/edit-file patch)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/invalid-input (:type (ex-data ex))))))

    (it "rejects non-ranged hunk headers"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@\n"
                    " one\n"
                    "-two\n"
                    "+TWO\n"
                    "*** End Patch")
            ex (try
                 (sut/edit-file patch)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/invalid-input (:type (ex-data ex))))))))

(defdescribe hunk-range-validation-test
  (describe "hunk range validation"
    (it "rejects old-side count mismatch"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo\nthree")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -1,5 +1,3 @@\n"
                    " one\n"
                    "-two\n"
                    "+TWO\n"
                    " three\n"
                    "*** End Patch")
            ex (try
                 (sut/edit-file patch)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/invalid-input (:type (ex-data ex))))
        (expect (re-find #"old-side count mismatch" (ex-message ex)))))

    (it "rejects new-side count mismatch"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -1,2 +1,5 @@\n"
                    " one\n"
                    "-two\n"
                    "+TWO\n"
                    "*** End Patch")
            ex (try
                 (sut/edit-file patch)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/invalid-input (:type (ex-data ex))))
        (expect (re-find #"new-side count mismatch" (ex-message ex)))))

    (it "accepts correct ranges with omitted comma-count (defaults to 1)"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo\nthree")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -1,3 +1,3 @@\n"
                    " one\n"
                    "-two\n"
                    "+TWO\n"
                    " three\n"
                    "*** End Patch")
            result (sut/edit-file patch)]
        (expect (= "one\nTWO\nthree" (read-file a)))))

    (it "rejects old-start < 1"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -0,1 +1,1 @@\n"
                    " one\n"
                    "*** End Patch")
            ex (try
                 (sut/edit-file patch)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/invalid-input (:type (ex-data ex))))
        (expect (re-find #"old-start must be >= 1" (ex-message ex)))))

    (it "rejects new-start < 1"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -1,1 +0,1 @@\n"
                    " one\n"
                    "*** End Patch")
            ex (try
                 (sut/edit-file patch)
                 nil
                 (catch Exception e e))]
        (expect (some? ex))
        (expect (= :tool/invalid-input (:type (ex-data ex))))
        (expect (re-find #"new-start must be >= 1" (ex-message ex)))))

    (it "validates ranges across multiple hunks"
      (let [dir (tmp-dir)
            a (write-file! (file-in dir "a.txt") "one\ntwo\nthree\nfour\nfive")
            patch (str "*** Begin Patch\n"
                    "*** Update File: " (.getAbsolutePath a) "\n"
                    "@@ -1,2 +1,2 @@\n"
                    " one\n"
                    "-two\n"
                    "+TWO\n"
                    "@@ -4,2 +4,2 @@\n"
                    " four\n"
                    "-five\n"
                    "+FIVE\n"
                    "*** End Patch")
            result (sut/edit-file patch)]
        (expect (= "one\nTWO\nthree\nfour\nFIVE" (read-file a)))
        (expect (= 2 (:total-hunks result)))))))
