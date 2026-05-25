(ns com.blockether.vis.ext.foundation-core.workspace-ctx-test
  "`:session/workspace` CTX block render."
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-core.workspace-ctx :as wctx]
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

(defn- git!
  [dir args]
  (let [argv   (into ["git" "-C" (.getCanonicalPath (io/file dir))] args)
        result (apply sh/sh argv)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "git failed: " (str/join " " argv))
               (assoc result :argv argv))))
    (str/trim (or (:out result) ""))))

(defn- init-repo! [root]
  (.mkdirs (io/file root))
  (git! root ["init"])
  (git! root ["config" "user.name"  "Vis Test"])
  (git! root ["config" "user.email" "vis-test@example.invalid"])
  (spit (io/file root "note.txt") "base\n")
  (git! root ["add" "note.txt"])
  (git! root ["commit" "-m" "base"]))

(defdescribe render-block-test
  (it "{:vcs/kind :none} when no workspace pinned"
    (expect (= {:vcs/kind :none}
              (wctx/render-block {:workspace nil}))))

  (it "trunk-kind: :vcs/* keys + no commits-ahead, :ff-possible? false"
    (let [base (temp-dir "vis-wctx-trunk")]
      (try
        (init-repo! base)
        (let [ws    {:id        "ws-trunk"
                     :kind      :trunk
                     :repo-root base
                     :root      base
                     :branch    (git! base ["rev-parse" "--abbrev-ref" "HEAD"])}
              block (wctx/render-block {:workspace ws})]
          (expect (= :git    (:vcs/kind block)))
          (expect (= :trunk  (:workspace/kind block)))
          (expect (= "ws-trunk" (:workspace/id block)))
          (expect (= [] (:vcs/commits-ahead block)))
          (expect (= false (:vcs/ff-possible? block)))
          (expect (false? (:vcs/dirty? block))))
        (finally (delete-tree! base)))))

  (it "session-state hydration adds :session/* identity keys"
    (let [base (temp-dir "vis-wctx-session")]
      (try
        (init-repo! base)
        (let [ws            {:id        "ws-1"
                             :kind      :trunk
                             :repo-root base
                             :root      base
                             :branch    "main"}
              session-state {:id              "ss-1"
                             :session-soul-id "soul-1"
                             :title           "Auth refactor"}
              block (wctx/render-block {:workspace ws :session-state session-state})]
          (expect (= "ss-1"          (:session/state-id block)))
          (expect (= "soul-1"        (:session/id block)))
          (expect (= "Auth refactor" (:session/title block))))
        (finally (delete-tree! base)))))

  (it "branch-kind workspace with a commit ahead surfaces :vcs/commits-ahead + :vcs/stats"
    (let [base (temp-dir "vis-wctx-ahead")]
      (try
        (init-repo! base)
        (let [trunk (git! base ["rev-parse" "--abbrev-ref" "HEAD"])
              wt    (str (io/file base ".vis-wt"))]
          (git! base ["worktree" "add" "-b" "vis/test-ahead" wt "HEAD"])
          (spit (io/file wt "note.txt") "edited\n")
          (git! wt ["add" "-A"])
          (git! wt ["commit" "-m" "branch commit"])
          (let [ws    {:id        "ws-ahead"
                       :kind      :branch
                       :repo-root base
                       :root      wt
                       :branch    "vis/test-ahead"}
                block (wctx/render-block {:workspace ws})]
            (expect (= :git (:vcs/kind block)))
            (expect (= "vis/test-ahead" (:vcs/branch block)))
            (expect (= trunk (:vcs/trunk block)))
            (expect (= 1 (count (:vcs/commits-ahead block))))
            (expect (= "branch commit"
                      (-> block :vcs/commits-ahead first :message)))
            (expect (some? (get-in block [:vcs/stats "note.txt"])))
            ;; trunk is ancestor of branch → FF possible
            (expect (true? (:vcs/ff-possible? block)))))
        (finally (delete-tree! base))))))
