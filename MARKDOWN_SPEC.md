diff --git a/deps.edn b/deps.edn
index 1406511b..3e372b7d 100644
--- a/deps.edn
+++ b/deps.edn
@@ -103,6 +103,7 @@
                         "extensions/channels/vis-channel-tui/test"
                         "extensions/channels/vis-channel-telegram/test"
                         "extensions/common/vis-foundation/test"
+                        "extensions/common/vis-foundation-git/test"
                         "extensions/common/vis-exa/test"
                         "extensions/common/vis-voice/test"]
           :extra-deps  {io.github.noahtheduke/lazytest           {:mvn/version "2.0.0"}
@@ -135,6 +136,7 @@
                         "--dir" "extensions/channels/vis-channel-tui/test"
                         "--dir" "extensions/channels/vis-channel-telegram/test"
                         "--dir" "extensions/common/vis-foundation/test"
+                        "--dir" "extensions/common/vis-foundation-git/test"
                         "--dir" "extensions/common/vis-exa/test"
                         "--dir" "extensions/common/vis-voice/test"]}
 
diff --git a/extensions/common/vis-foundation-git/src/com/blockether/vis/ext/foundation_git/core.clj b/extensions/common/vis-foundation-git/src/com/blockether/vis/ext/foundation_git/core.clj
index 160252bb..5ebe7371 100644
--- a/extensions/common/vis-foundation-git/src/com/blockether/vis/ext/foundation_git/core.clj
+++ b/extensions/common/vis-foundation-git/src/com/blockether/vis/ext/foundation_git/core.clj
@@ -120,26 +120,32 @@
       :stat      {:files N :+ N :- N}
       :files     [{:file :+ :-} ...]
       :porcelain [{:status :file} ...]}"
-  [env]
-  (let [root      (env-root env)
-        ws-id     (:workspace/id env)
-        db-info   (:db-info env)
-        ws        (when (and db-info ws-id) (vis/workspace-get db-info ws-id))
-        base      (when (and (= :branch (:kind ws)) (:commit-id ws))
-                    (:commit-id ws))
-        diff-arg  (if base [base "HEAD"] ["HEAD"])
-        numstat   (parse-numstat (git-out root (into ["diff" "--numstat"] diff-arg)))
-        porc      (parse-porcelain (git-out root ["status" "--porcelain"]))
-        head      (git-out root ["rev-parse" "HEAD"])
-        +sum      (reduce + 0 (map :+ numstat))
-        -sum      (reduce + 0 (map :- numstat))]
-    (extension/success
-      {:result {:branch    (:branch ws)
-                :head      head
-                :kind      (:kind ws)
-                :stat      {:files (count numstat) :+ +sum :- -sum}
-                :files     numstat
-                :porcelain porc}})))
+  ([env]
+   (git-diff-fn env nil))
+  ([env opts]
+   (when (and (some? opts) (not (map? opts)))
+     (throw (ex-info "v/git-diff opts must be a map when provided"
+              {:type :foundation-git/invalid-opts
+               :opts opts})))
+   (let [root      (env-root env)
+         ws-id     (:workspace/id env)
+         db-info   (:db-info env)
+         ws        (when (and db-info ws-id) (vis/workspace-get db-info ws-id))
+         base      (when (and (= :branch (:kind ws)) (:commit-id ws))
+                     (:commit-id ws))
+         diff-arg  (if base [base "HEAD"] ["HEAD"])
+         numstat   (parse-numstat (git-out root (into ["diff" "--numstat"] diff-arg)))
+         porc      (parse-porcelain (git-out root ["status" "--porcelain"]))
+         head      (git-out root ["rev-parse" "HEAD"])
+         +sum      (reduce + 0 (map :+ numstat))
+         -sum      (reduce + 0 (map :- numstat))]
+     (extension/success
+       {:result {:branch    (:branch ws)
+                 :head      head
+                 :kind      (:kind ws)
+                 :stat      {:files (count numstat) :+ +sum :- -sum}
+                 :files     numstat
+                 :porcelain porc}}))))
 
 (defn git-status-fn
   "Working-tree status of the active workspace as parsed porcelain.
@@ -182,8 +188,8 @@
        {:result {:branch  branch
                  :commits (parse-log out)}}))))
 
-(def ^{:doc "Diff stat + porcelain for the currently bound workspace. Branch workspaces diff against their spawn commit; trunk workspaces diff against HEAD. Returns {:branch :head :kind :stat {:files :+ :-} :files [...] :porcelain [...]}."
-       :arglists '([])} git-diff git-diff-fn)
+(def ^{:doc "Diff stat + porcelain for the currently bound workspace. Branch workspaces diff against their spawn commit; trunk workspaces diff against HEAD. Optional opts map accepted for compatibility (e.g. {:stat? true}); result always includes stat, files, and porcelain. Returns {:branch :head :kind :stat {:files :+ :-} :files [...] :porcelain [...]}."
+       :arglists '([] [opts])} git-diff git-diff-fn)
 
 (def ^{:doc "Working-tree status of the currently bound workspace. Returns {:branch :head :clean? :entries [{:status :file} ...]}."
        :arglists '([])} git-status git-status-fn)
diff --git a/extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/introspection_test.clj b/extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/introspection_test.clj
index afd46ba6..3e356981 100644
--- a/extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/introspection_test.clj
+++ b/extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/introspection_test.clj
@@ -3,7 +3,8 @@
    [clojure.string]
    [com.blockether.vis.ext.foundation.introspection :as introspection]
    [com.blockether.vis.internal.extension :as extension]
-   [lazytest.core :refer [defdescribe expect it]]))
+   [lazytest.core :refer [defdescribe expect it]]
+   [sci.core :as sci]))
 
 (defdescribe introspection-public-surface-test
   (it "exposes session and Clojure symbol introspection symbols"
@@ -14,7 +15,23 @@
       (expect (contains? symbols 'engine-symbol-source-code))
       (expect (contains? symbols 'engine-symbol-metadata))
       (expect (contains? symbols 'engine-symbol-apropos))
-      (expect (= 6 (count symbols))))))
+      (expect (= 6 (count symbols)))))
+
+  (it "documents quoted aliased SCI symbols"
+    (let [v-ns    (sci/create-ns 'vis.ext.v)
+          sci-ctx (sci/init {:namespaces {'vis.ext.v {'git-diff (sci/new-var 'git-diff
+                                                                  (fn [] nil)
+                                                                  {:ns v-ns
+                                                                   :doc "diff docs"
+                                                                   :arglists '([])})}}
+                             :ns-aliases {'v 'vis.ext.v}})
+          tool    @#'introspection/engine-symbol-documentation-tool
+          result  (tool {:sci-ctx sci-ctx} 'v/git-diff)]
+      (expect (extension/tool-result? result))
+      (expect (get-in result [:result :found?]))
+      (expect (= 'v/git-diff (get-in result [:result :symbol])))
+      (expect (= 'vis.ext.v/git-diff (get-in result [:result :resolved-symbol])))
+      (expect (= "diff docs" (get-in result [:result :doc]))))))
 
 (defdescribe session-state-envelope-test
   (it "returns a canonical envelope so observed symbol wrapping can unwrap it"
diff --git a/src/com/blockether/vis/internal/extension.clj b/src/com/blockether/vis/internal/extension.clj
index 39f47132..9e844865 100644
--- a/src/com/blockether/vis/internal/extension.clj
+++ b/src/com/blockether/vis/internal/extension.clj
@@ -1580,7 +1580,8 @@
         (let [contribution
               (try
                 (binding [*current-extension* ext
-                          *current-symbol* nil]
+                          *current-symbol* nil
+                          workspace/*workspace-root* (workspace/workspace-root environment)]
                   (f environment))
                 (catch Throwable t
                   (tel/log! {:level :warn
diff --git a/test/com/blockether/vis/internal/extension_test.clj b/test/com/blockether/vis/internal/extension_test.clj
index d47eab6f..7c372ad3 100644
--- a/test/com/blockether/vis/internal/extension_test.clj
+++ b/test/com/blockether/vis/internal/extension_test.clj
@@ -1,6 +1,7 @@
 (ns com.blockether.vis.internal.extension-test
   (:require
    [com.blockether.vis.internal.extension :as extension]
+   [com.blockether.vis.internal.workspace :as workspace]
    [lazytest.core :refer [defdescribe expect it]]))
 
 (defn- sample-channel-fn
@@ -21,6 +22,17 @@
       (expect (= "First line\n\n  Nested line" ((:ext/prompt string-ext) {})))
       (expect (= "First line\n\n  Nested line" ((:ext/prompt fn-ext) {}))))))
 
+(defdescribe ctx-contributions-test
+  (it "binds active workspace root while building extension ctx"
+    (let [root (.getCanonicalPath (java.io.File. "target/test-workspace-ctx"))
+          ext  {:ext/name "test.ctx-workspace"
+                :ext/ctx  (fn [_]
+                            {:project {:ctx-root workspace/*workspace-root*
+                                       :cwd      (.getCanonicalPath (workspace/cwd))}})}
+          ctx  (extension/ctx-contributions {:workspace/root root} [ext])]
+      (expect (= root (get-in ctx [:project :ctx-root])))
+      (expect (= root (get-in ctx [:project :cwd]))))))
+
 (defdescribe channel-contributions-test
   (it "extension accepts channel contributions and derives channel kind"
     (let [ext (extension/extension
