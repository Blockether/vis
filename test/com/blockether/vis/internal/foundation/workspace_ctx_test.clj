(ns com.blockether.vis.internal.foundation.workspace-ctx-test
  "`\"session_workspace\"` CTX block render (STRING-KEYED). `\"vcs_kind\"` reports
   the underlying repository VCS (`\"git\"` inside a repo, else `\"none\"`)."
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
  (it "merges registered names into filesystem roots and lists each root only once"
      (let [block
            (wctx/render-block {:workspace {:root "/projects/main"}
                                :filesystem-roots [{:root "/projects/main"}
                                                   {:root "/projects/library"}
                                                   {:root "/cache" :no-search? true}]
                                :project-paths {"main_path" "/projects/main"
                                                "library_path" "/projects/library"
                                                "nested_path" "/projects/library/nested"
                                                "readonly_path" "/reference/readonly"}})

          roots
          (get block "filesystem_roots")]

        (expect (= "/projects/main" (get block "root")))
        (expect (not (contains? block "path_globals")))
        (expect (= [{"cwd" "/projects/library" "python_name" "library_path"}
                    {"cwd" "/projects/library/nested" "python_name" "nested_path"}
                    {"cwd" "/reference/readonly" "python_name" "readonly_path"}
                    ;; Defaults (read/write, searched) are not written on each root.
                    {"cwd" "/cache" "no_search" true}]
                   roots))))
  (it "keeps the main root in workspace.root without an extra catalog or duplicate root row"
      (let [block (wctx/render-block {:workspace {:root "/projects/main"}
                                      :filesystem-roots [{:root "/projects/main"}]
                                      :project-paths {"main_path" "/projects/main"}})]
        (expect (= "/projects/main" (get block "root")))
        (expect (not (contains? block "path_globals")))
        (expect (not (contains? block "filesystem_roots")))))
  (it "shows paths under home as ~/ and keeps other paths absolute"
      ;; User review: the session block repeated `/Users/<name>/` for every root.
      (let [home
            (System/getProperty "user.home")

            block
            (wctx/render-block {:workspace {:root (str home "/vis-wctx-absent/main")}
                                :filesystem-roots [{:root (str home "/vis-wctx-absent/library")}
                                                   {:root "/opt/cache"}]})]

        (expect (= "~/vis-wctx-absent/main" (get block "root")))
        (expect (= ["~/vis-wctx-absent/library" "/opt/cache"]
                   (mapv #(get % "cwd") (get block "filesystem_roots"))))))
  (it "preserves explicitly registered distinct names for the same directory"
      (let [roots (get (wctx/render-block {:workspace {:root "/projects/main"}
                                           :filesystem-roots [{:root "/projects/library"}]
                                           :project-paths {"library_path" "/projects/library"
                                                           "reference_path" "/projects/library"}})
                       "filesystem_roots")]
        (expect (= ["library_path" "reference_path"] (mapv #(get % "python_name") roots)))
        (expect (= ["/projects/library" "/projects/library"] (mapv #(get % "cwd") roots))))))

(defdescribe render-block-test
             (it "reports the root, its VCS and the workspace id"
                 (let [base (temp-dir "vis-wctx-id")]
                   (try (let [block (wctx/render-block {:workspace {:id "ws-1" :root base}})]
                          (expect (= base (get block "root")))
                          ;; A temporary directory is not a Git repository.
                          (expect (= "none" (get block "vcs_kind")))
                          (expect (= "ws-1" (get block "id"))))
                        (finally (delete-tree! base)))))
             (it "nil workspace falls back to the bound cwd"
                 (let [base (temp-dir "vis-wctx-nil")]
                   (try (binding [workspace/*workspace-root* base]
                          (let [block (wctx/render-block {:workspace nil})]
                            (expect (= base (get block "root")))
                            (expect (= "none" (get block "vcs_kind")))))
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
               ;; The title is for people; the model has the conversation itself.
               (expect (not (contains? block "session_title")))
               (expect (= {"soul" "soul-1" "parent_state" "ss-0"} (get block "session_fork_of"))))
             (finally (delete-tree! base))))))
