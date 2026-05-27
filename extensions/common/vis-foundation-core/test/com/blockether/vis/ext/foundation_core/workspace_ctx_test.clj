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
  (it "non-VCS workspace keeps workspace identity and :vcs/kind :none"
    (let [base (temp-dir "vis-wctx-no-vcs")]
      (try
        (let [block (wctx/render-block {:workspace {:root base}})]
          (expect (= base (:workspace/root block)))
          (expect (= false (:workspace/sandbox? block)))
          (expect (= :none (:vcs/kind block))))
        (finally (delete-tree! base)))))

  (it "nil workspace falls back to cwd workspace identity"
    (let [base (temp-dir "vis-wctx-nil")]
      (try
        (binding [com.blockether.vis.internal.workspace/*workspace-root* base]
          (let [block (wctx/render-block {:workspace nil})]
            (expect (= base (:workspace/root block)))
            (expect (= false (:workspace/sandbox? block)))
            (expect (= :none (:vcs/kind block)))))
        (finally (delete-tree! base)))))

  (it "primary workspace: generic workspace + :vcs/* keys"
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
          (expect (= false   (:workspace/sandbox? block)))
          (expect (= base    (:workspace/root block)))
          (expect (= "ws-trunk" (:workspace/id block)))
          (expect (= [] (:vcs/unmerged-commits block)))
          (expect (= false (:vcs/integrable? block)))
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

  (it "sandbox workspace with a commit ahead surfaces :vcs/unmerged-commits + :vcs/stats"
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
            (expect (true? (:workspace/sandbox? block)))
            (expect (= "vis/test-ahead" (:vcs/ref block)))
            (expect (= trunk (:vcs/mainline block)))
            (expect (= 1 (count (:vcs/unmerged-commits block))))
            (expect (= "branch commit"
                      (-> block :vcs/unmerged-commits first :message)))
            (expect (some? (get-in block [:vcs/stats "note.txt"])))
            ;; mainline is ancestor of sandbox ref → integrable
            (expect (true? (:vcs/integrable? block)))))
        (finally (delete-tree! base))))))
