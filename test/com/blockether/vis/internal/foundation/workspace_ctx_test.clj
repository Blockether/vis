(ns com.blockether.vis.internal.foundation.workspace-ctx-test
  "`\"session_workspace\"` CTX block render (STRING-KEYED). Backend-workspace
   isolation rides on `\"isolated\"`; `\"vcs_kind\"` reports the real underlying repo VCS
   (`\"git\"` inside a repo, else `\"none\"`) — never `\"rift\"` (a non-VCS)."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.foundation.workspace-ctx :as wctx]
            [com.blockether.vis.internal.workspace.core :as workspace]
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
  project-filesystem-roots-test
  (it
    "merges registered names into filesystem roots and advertises working copies only once"
    (let [block
          (wctx/render-block
            {:workspace {:root "/draft/main" :repo-root "/projects/main"}
             :filesystem-roots
             [{:trunk "/projects/main" :clone "/draft/main" :primary? true}
              {:trunk "/projects" :clone "/draft/broad"}
              {:trunk "/projects/library" :clone "/draft/library" :draft :copy-only}
              {:trunk "/projects/private" :clone "/projects/private" :denied? true}
              {:trunk "/cache" :clone "/draft/cache" :no-search? true}]
             :project-paths {"main_path" "/projects/main"
                             "library_path" "/projects/library"
                             "nested_path" "/projects/library/nested"
                             "private_path" "/projects/private"
                             "private_nested_path" "/projects/private/nested"
                             "reference_path" "/projects/reference"
                             "readonly_path" "/reference/readonly"}})

          roots
          (get block "filesystem_roots")]

      (expect (= "/draft/main" (get block "root")))
      (expect (not (contains? block "path_globals")))
      (expect (= {"library_path" "/draft/library"
                  "nested_path" "/draft/library/nested"
                  "reference_path" "/draft/broad/reference"
                  "readonly_path" "/reference/readonly"}
                 (into {}
                       (keep #(when (get % "python_name") [(get % "python_name") (get % "cwd")]))
                       roots)))
      (expect (= [{"cwd" "/draft/library"
                   "python_name" "library_path"
                   "isolated" true
                   "draft" "copy-only"}]
                 (filter #(= "/draft/library" (get % "cwd")) roots)))
      (expect (= [{"cwd" "/projects/private" "isolated" false "draft" "shared" "is_denied" true}]
                 (filter #(get % "is_denied") roots)))
      (expect (some #(= {"cwd" "/draft/cache" "isolated" true "draft" "shared"} %) roots))
      (expect (not-any? #(#{"/projects/main" "/draft/main"} (get % "cwd")) roots))))
  (it "keeps the main root in workspace.root without an extra catalog or duplicate root row"
      (let [block (wctx/render-block
                    {:workspace {:root "/projects/main" :repo-root "/projects/main"}
                     :filesystem-roots [{:trunk "/projects/main" :clone "/projects/main"}]
                     :project-paths {"main_path" "/projects/main"}})]
        (expect (= "/projects/main" (get block "root")))
        (expect (not (contains? block "path_globals")))
        (expect (not (contains? block "filesystem_roots")))))
  (it "preserves explicitly registered distinct names for the same directory"
      (let [roots (get (wctx/render-block {:workspace {:root "/projects/main"}
                                           :filesystem-roots [{:trunk "/projects/library"
                                                               :clone "/projects/library"}]
                                           :project-paths {"library_path" "/projects/library"
                                                           "reference_path" "/projects/library"}})
                       "filesystem_roots")]
        (expect (= ["library_path" "reference_path"] (mapv #(get % "python_name") roots)))
        (expect (= ["/projects/library" "/projects/library"] (mapv #(get % "cwd") roots))))))

(defdescribe
  render-block-test
  (it "reports live trunk as non-isolated and keeps isolation separate from VCS"
      (let [base (temp-dir "vis-wctx-id")]
        (try (let [block (wctx/render-block {:workspace
                                             {:id "ws-1" :root base :workspace-backend :live}})]
               (expect (= base (get block "root")))
               (expect (false? (get block "isolated")))
               ;; temp dir is not a git repo → "none"; isolation is on "isolated"
               (expect (= "none" (get block "vcs_kind")))
               (expect (not= "rift" (get block "vcs_kind")))
               (expect (= "ws-1" (get block "id"))))
             (finally (delete-tree! base)))))
  (it "reports backend workspaces as isolated"
      (let [base (temp-dir "vis-wctx-isolated")]
        (try (let [block (wctx/render-block {:workspace
                                             {:id "ws-iso" :root base :workspace-backend :rift}})]
               (expect (true? (get block "isolated"))))
             (finally (delete-tree! base)))))
  (it "treats pre-migration fork rows without backend ids as isolated"
      (let [base (temp-dir "vis-wctx-legacy-isolated")]
        (try (let [block (wctx/render-block {:workspace {:id "ws-legacy" :root base :fork-ms 1}})]
               (expect (true? (get block "isolated"))))
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
                 (expect (false? (get block "isolated")))
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
