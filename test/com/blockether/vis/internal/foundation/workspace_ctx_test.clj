(ns com.blockether.vis.internal.foundation.workspace-ctx-test
  "`:session/workspace` CTX block render — git-free rift model."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.internal.foundation.workspace-ctx :as wctx]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath
    (.toFile
      (java.nio.file.Files/createTempDirectory
        prefix
        (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defdescribe render-block-test
  (it "workspace identity: root, sandbox? always true, :vcs/kind :rift"
    (let [base (temp-dir "vis-wctx-id")]
      (try
        (let [block (wctx/render-block {:workspace {:id "ws-1" :root base}})]
          (expect (= base (:workspace/root block)))
          (expect (true? (:workspace/sandbox? block)))
          (expect (= :rift (:vcs/kind block)))
          (expect (= "ws-1" (:workspace/id block))))
        (finally (delete-tree! base)))))

  (it "nil workspace falls back to the bound cwd"
    (let [base (temp-dir "vis-wctx-nil")]
      (try
        (binding [workspace/*workspace-root* base]
          (let [block (wctx/render-block {:workspace nil})]
            (expect (= base (:workspace/root block)))
            (expect (true? (:workspace/sandbox? block)))
            (expect (= :rift (:vcs/kind block)))))
        (finally (delete-tree! base)))))

  (it "surfaces since-fork changed paths (mtime newer than the fork ms)"
    (let [base (temp-dir "vis-wctx-changed")]
      (try
        (spit (io/file base "note.txt") "edited\n")
        ;; fork-ms 0 ⇒ every file counts as changed (mtime > 0).
        (let [block (wctx/render-block {:workspace {:id "ws-2" :root base :fork-ms 0}})]
          (expect (= 1 (:workspace/changed block)))
          (expect (= ["note.txt"] (:workspace/changed-paths block))))
        (finally (delete-tree! base)))))

  (it "omits change keys when the workspace has no fork timestamp"
    (let [base (temp-dir "vis-wctx-nochange")]
      (try
        (spit (io/file base "note.txt") "x\n")
        (let [block (wctx/render-block {:workspace {:id "ws-2b" :root base}})]
          (expect (nil? (:workspace/changed block)))
          (expect (nil? (:workspace/changed-paths block))))
        (finally (delete-tree! base)))))

  (it "surfaces the label"
    (let [base (temp-dir "vis-wctx-label")]
      (try
        (let [block (wctx/render-block {:workspace {:id    "ws-3"
                                                    :root  base
                                                    :label "frontend"}})]
          (expect (= "frontend" (:workspace/label block))))
        (finally (delete-tree! base)))))

  (it "session-state hydration adds :session/* identity + fork lineage"
    (let [base (temp-dir "vis-wctx-session")]
      (try
        (let [ws    {:id "ws-4" :root base}
              ss    {:id              "ss-1"
                     :session-soul-id "soul-1"
                     :title           "Auth refactor"
                     :parent-state-id "ss-0"}
              block (wctx/render-block {:workspace ws :session-state ss})]
          (expect (= "ss-1"          (:session/state-id block)))
          (expect (= "soul-1"        (:session/id block)))
          (expect (= "Auth refactor" (:session/title block)))
          (expect (= {:soul "soul-1" :parent-state "ss-0"}
                    (:session/fork-of block))))
        (finally (delete-tree! base))))))
