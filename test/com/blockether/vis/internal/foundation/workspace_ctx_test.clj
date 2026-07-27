(ns com.blockether.vis.internal.foundation.workspace-ctx-test
  "`\"session_workspace\"` CTX block render (STRING-KEYED). Sandbox-ness rides on
   `\"sandbox\"`; `\"vcs_kind\"` reports the real underlying repo VCS
   (`\"git\"` inside a repo, else `\"none\"`) — never `\"rift\"` (a non-VCS)."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.workspace-ctx :as wctx]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- temp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq (io/file root)))]
    (io/delete-file f true)))

(defdescribe
  render-block-test
  (it "reports live trunk as non-sandboxed and keeps sandbox separate from VCS"
      (let [base (temp-dir "vis-wctx-id")]
        (try (let [block (wctx/render-block {:workspace
                                             {:id "ws-1" :root base :workspace-backend :live}})]
               (expect (= base (get block "root")))
               (expect (false? (get block "sandbox")))
               ;; temp dir is not a git repo → "none"; sandbox-ness is on "sandbox"
               (expect (= "none" (get block "vcs_kind")))
               (expect (not= "rift" (get block "vcs_kind")))
               (expect (= "ws-1" (get block "id"))))
             (finally (delete-tree! base)))))
  (it "reports backend workspaces as sandboxed"
      (let [base (temp-dir "vis-wctx-sandbox")]
        (try (let [block (wctx/render-block {:workspace
                                             {:id "ws-iso" :root base :workspace-backend :rift}})]
               (expect (true? (get block "sandbox"))))
             (finally (delete-tree! base)))))
  (it "treats pre-migration fork rows without backend ids as sandboxed"
      (let [base (temp-dir "vis-wctx-legacy-isolated")]
        (try (let [block (wctx/render-block {:workspace {:id "ws-legacy" :root base :fork-ms 1}})]
               (expect (true? (get block "sandbox"))))
             (finally (delete-tree! base)))))
  (it "reports \"vcs_kind\" \"git\" when the workspace root is inside a git repo"
      ;; the project cwd is a git repo
      (let [block (wctx/render-block {:workspace {:id "ws-git" :root (workspace/cwd)}})]
        (expect (= "git" (get block "vcs_kind")))))
  (it "nil workspace falls back to the bound cwd"
      (let [base (temp-dir "vis-wctx-nil")]
        (try (binding [workspace/*workspace-root* base]
               (let [block (wctx/render-block {:workspace nil})]
                 (expect (= base (get block "root")))
                 (expect (false? (get block "sandbox")))
                 (expect (= "none" (get block "vcs_kind")))))
             (finally (delete-tree! base)))))
  (it "surfaces since-fork changed paths (mtime newer than the fork ms)"
      (let [base (temp-dir "vis-wctx-changed")]
        (try (spit (io/file base "note.txt") "edited\n")
             ;; fork-ms 0 ⇒ every file counts as changed (mtime > 0).
             (let [block (wctx/render-block {:workspace {:id "ws-2" :root base :fork-ms 0}})]
               (expect (= 1 (get block "changed")))
               (expect (= ["note.txt"] (get block "changed_paths"))))
             (finally (delete-tree! base)))))
  (it "omits change keys when the workspace has no fork timestamp"
      (let [base (temp-dir "vis-wctx-nochange")]
        (try (spit (io/file base "note.txt") "x\n")
             (let [block (wctx/render-block {:workspace {:id "ws-2b" :root base}})]
               (expect (nil? (get block "changed")))
               (expect (nil? (get block "changed_paths"))))
             (finally (delete-tree! base)))))
  (it "surfaces the label"
      (let [base (temp-dir "vis-wctx-label")]
        (try (let [block (wctx/render-block {:workspace {:id "ws-3" :root base :label "frontend"}})]
               (expect (= "frontend" (get block "label"))))
             (finally (delete-tree! base)))))
  (it "session-state hydration adds session_* identity + fork lineage"
      (let [base (temp-dir "vis-wctx-session")]
        (try (let [ws {:id "ws-4" :root base}
                   ss {:id "ss-1"
                       :session-soul-id "soul-1"
                       :title "Auth refactor"
                       :parent-state-id "ss-0"}
                   block (wctx/render-block {:workspace ws :session-state ss})]

               (expect (= "ss-1" (get block "session_state_id")))
               (expect (= "soul-1" (get block "session_id")))
               (expect (= "Auth refactor" (get block "session_title")))
               (expect (= {"soul" "soul-1" "parent_state" "ss-0"} (get block "session_fork_of"))))
             (finally (delete-tree! base))))))
