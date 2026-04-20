(ns com.blockether.vis.loop.runtime.read-file-freshness-test
  "Tool-owned freshness contract coverage for `read-file`.

   The iteration-loop seeds a var's `:metadata` by calling the
   producing tool's `:freshness` fn with `{:args result :metadata nil}`,
   then re-checks freshness later by calling the same fn with the
   stored snapshot. This suite pins both phases + the MISSING path
   (file deleted since bind) — the only signal the agent has that
   its cached read is no longer valid."
  (:require
    [clojure.java.io :as io]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.languages.commons.read :as sut]))

(defn- tmp-file [content]
  (let [f (java.io.File/createTempFile "vis-read-fresh-" ".txt")]
    (.deleteOnExit f)
    (spit f content)
    (.getAbsolutePath f)))

(defdescribe read-file-freshness-seed-test
  (describe "seed phase (metadata nil)"
    (it "returns baseline metadata + :freshness? true"
      (let [p (tmp-file "alpha")
            out (sut/freshness {:args [p] :result "alpha" :metadata nil})]
        (expect (= :file (get-in out [:metadata :kind])))
        (expect (= p     (get-in out [:metadata :path])))
        (expect (pos?    (get-in out [:metadata :mtime])))
        (expect (= 5     (get-in out [:metadata :size])))
        (expect (true?   (:freshness? out)))))))

(defdescribe read-file-freshness-recheck-test
  (describe "re-check phase (metadata present)"
    (it "reports :freshness? true when mtime+size are unchanged"
      (let [p (tmp-file "alpha")
            seed (sut/freshness {:args [p] :result "alpha" :metadata nil})
            out  (sut/freshness {:args nil :result nil :metadata (:metadata seed)})]
        (expect (true? (:freshness? out)))))

    (it "reports :freshness? false when content changes"
      (let [p (tmp-file "alpha")
            seed (sut/freshness {:args [p] :result "alpha" :metadata nil})]
        (Thread/sleep 1100)  ;; FAT-class mtime resolution safety
        (spit p "alpha-plus-extra")
        (let [out (sut/freshness {:args nil :result nil :metadata (:metadata seed)})]
          (expect (false? (:freshness? out)))
          ;; New metadata reflects current on-disk state so a caller
          ;; can distinguish "size changed" from "mtime changed".
          (expect (not= (get-in seed [:metadata :size])
                    (get-in out [:metadata :size]))))))

    (it "throws :rlm.freshness/missing when the file is deleted"
      (let [p (tmp-file "alpha")
            seed (sut/freshness {:args [p] :result "alpha" :metadata nil})]
        (io/delete-file p true)
        (let [ex (try (sut/freshness {:args nil :result nil :metadata (:metadata seed)})
                   nil
                   (catch Exception e e))]
          (expect (some? ex))
          (expect (= :rlm.freshness/missing (:type (ex-data ex)))))))))
