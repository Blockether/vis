(ns com.blockether.vis.ext.foundation.editing.core-test
  "Tests for the editing extension.

   Smoke-checks the loaded extension surface (symbol vector, doc
   strings, prompt fragment) plus behavioral coverage of the
   structured preview/search helpers (`v/cat`, `v/rg`) and the new
   thin babashka.fs wrappers (`v/patch`, `v/glob`, ...).

   Tests reach private fns directly through the registry to avoid
   bringing up a full SCI sandbox. Temp files land under
   `target/editing-test/` (always inside the repo cwd, so
   `safe-path` accepts them)."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as string]
   [com.blockether.vis.ext.foundation.core :as foundation]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.workspace-context :as workspace-context]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" name))))

(defn- temp-root
  "Cwd-relative path string for the shared temp root, idempotently
   created. Stays relative on purpose: `safe-path` resolves against
   `(fs/cwd)` and `fs/relativize` chokes when one arg is absolute and
   the other isn't."
  []
  (let [rel "target/editing-test"]
    (fs/create-dirs rel)
    rel))

(defn- write-temp! [name content]
  (let [rel (str (temp-root) "/" name)]
    (fs/create-dirs (fs/parent rel))
    (spit (fs/file rel) content)
    rel))

(defn- temp-dir-path
  "Cwd-relative directory path under the shared temp root, created if
   missing. Used when a v/ tool takes a directory (vs. a file) and
   we must NOT spit into it."
  [name]
  (let [rel (str (temp-root) "/" name)]
    (fs/create-dirs rel)
    rel))

(defdescribe editing-extension-loads-test
  (it "exposes structured helpers plus the required thin babashka.fs wrappers"
    (expect (vector? editing/editing-symbols))
    (expect (= 15 (count editing/editing-symbols)))
    (expect (not-any? #{'edit 'write 'cwd 'parent 'file-name 'extension 'relativize}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (not-any? #{'read-all-lines}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (some #{'patch}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (some #{'patch-check}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (not-any? #{'write-lines 'update-file}
              (map :ext.symbol/sym editing/editing-symbols)))
    (do
      (expect (some #{'bash}
                (map :ext.symbol/sym editing/editing-symbols)))
      (expect (= 1 (count (filter #{'bash}
                            (map :ext.symbol/sym editing/editing-symbols))))))
    (expect (some #{'preview}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (not-any? #{'silent!}
              (map :ext.symbol/sym editing/editing-symbols))))

  (it "omits bash symbols and prompt examples when bash is disabled by config"
    (with-redefs [config/bash-disabled? (fn [] true)]
      (let [symbols (map :ext.symbol/sym (editing/available-editing-symbols))
            prompt (editing/available-editing-prompt)]
        (expect (not-any? #{'bash} symbols))
        (expect (string/includes? prompt "v/bash is disabled"))
        (expect (not (string/includes? prompt "(get-in (v/bash \"pwd\")"))))))

  (it "every editing symbol carries a non-blank :doc and an :arglists vector"
    (doseq [s editing/editing-symbols
            :let [doc      (:ext.symbol/doc s)
                  arglists (:ext.symbol/arglists s)]]
      (expect (string? doc))
      (expect (not (string/blank? doc)))
      (expect (or (vector? arglists) (seq? arglists)))))

  (it "exposes a non-blank prompt fragment"
    (expect (string? editing/editing-prompt))
    (expect (not (string/blank? editing/editing-prompt))))

  (it "teaches positive preview strategies without priming bad binding forms"
    (let [preview-symbol (some #(when (= 'preview (:ext.symbol/sym %)) %)
                           editing/editing-symbols)]
      (expect (string/includes? editing/editing-prompt
                "Correct strategies:"))
      (expect (string/includes? editing/editing-prompt
                "use it as a standalone display form"))
      (expect (string/includes? editing/editing-prompt
                "(def file (v/cat \"src/foo.clj\"))"))
      (expect (string/includes? editing/editing-prompt
                "(v/preview file {:result [[:lines {:from 100 :to 180}]]})"))
      (expect (string/includes? editing/editing-prompt
                "(do (v/preview focus {:result [[:lines {:from 100 :to 180}]]}) :done)"))
      (expect (string/includes? (:ext.symbol/doc preview-symbol)
                "Strategy: bind full reads/searches you need later, then call preview separately; never echo a var just to inspect it"))
      (expect (string/includes? (:ext.symbol/doc preview-symbol)
                "With no EQL, previews the whole payload"))
      (expect (not (string/includes? editing/editing-prompt "(def x (v/preview")))
      (expect (not (string/includes? editing/editing-prompt "(def focus-early")))
      (expect (not (string/includes? (:ext.symbol/doc preview-symbol) "(def x (v/preview")))))

  (it "pushes search/read/path discovery to the structured v tool surface"
    (expect (string/includes? editing/editing-prompt
              "Use structured tools for discovery and reads"))
    (expect (string/includes? editing/editing-prompt
              "`v/glob` returns cwd-relative path strings under `:result`"))
    (expect (string/includes? editing/editing-prompt
              "recursive patterns like `**/*.clj` walk descendants"))
    (expect (string/includes? editing/editing-prompt
              "immediate children"))
    nil)

  (it "registers journal + channel renderers on every fn-symbol"
    (doseq [sym-name '[cat ls rg patch patch-check create-dirs glob copy move delete delete-if-exists exists? bash]]
      (let [entry (some #(when (= sym-name (:ext.symbol/sym %)) %)
                    editing/editing-symbols)]
        (expect (ifn? (:ext.symbol/journal-render-fn entry)))
        (expect (ifn? (:ext.symbol/channel-render-fn entry))))))

  (it "teaches the model that file and shell payloads live under the tool envelope :result"
    (let [bash-symbol (some #(when (= 'bash (:ext.symbol/sym %)) %)
                        editing/editing-symbols)]
      (expect (string/includes? editing/editing-prompt "[:result :lines]"))
      (expect (string/includes? editing/editing-prompt "(-> (v/rg {:all [\"needle\"] :paths [\"src\" \"test\"] :include [\"*.clj\" \"*.cljc\"]}) :result :hits)"))
      (expect (string/includes? editing/editing-prompt "[:result :stdout]"))
      (expect (string/includes? (:ext.symbol/doc bash-symbol) ":result :stdout"))
      (expect (string/includes? (:ext.symbol/doc bash-symbol) "Refuses shell-driven Clojure/EDN source edits"))
      (expect (string/includes? editing/editing-prompt "Use `v/bash`"))
      (expect (string/includes? editing/editing-prompt "set -euo pipefail")))))

(it "classifies operations into stable classes and color roles"
  (doseq [[op class role] [[:v/cat :op/read :tool-color/read]
                           [:z/locators :op/read :tool-color/read]
                           [:v/rg :op/search :tool-color/search]
                           [:v/patch :op/edit :tool-color/edit]
                           [:v/create-dirs :op/create :tool-color/create]
                           [:v/delete :op/delete :tool-color/delete]
                           [:v/move :op/move :tool-color/move]
                           [:v/bash :op/shell :tool-color/shell]
                           [:v/extensions :op/meta :tool-color/meta]]]
    (expect (= class (editing/tool-op->class op)))
    (expect (= role (editing/tool-op->color-role op)))))

(defdescribe editing-prompt-read-policy-test
  (it "teaches full-read vars, canonical patching, and no duplicate rereads by default"
    (let [patch-symbol (some #(when (= 'patch (:ext.symbol/sym %)) %)
                         editing/editing-symbols)]
      (expect (string/includes? editing/editing-prompt
                "`v/cat` reads the whole file"))
      (expect (string/includes? editing/editing-prompt
                "[:result :lines]"))
      (expect (string/includes? editing/editing-prompt
                "Edit text with canonical (v/patch"))
      (expect (string/includes? editing/editing-prompt
                "every :search must match exactly once"))
      (expect (string/includes? (:ext.symbol/doc patch-symbol)
                "Canonical exact text patch"))
      (expect (string/includes? (:ext.symbol/doc patch-symbol)
                "all edits validate before any write"))
      (expect (string/includes? editing/editing-prompt
                "Read back after writes only when exact persisted bytes matter"))
      (expect (not (string/includes? editing/editing-prompt "read-all-lines")))
      (expect (not (string/includes? editing/editing-prompt "write-lines")))
      (expect (not (string/includes? editing/editing-prompt "update-file"))))))

(defdescribe vis-cat-structured-shape-test
  (it "reads the whole file as raw lines plus shape metadata"
    (let [path (write-temp! "small.txt" "alpha\nbeta\ngamma\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      (expect (= #{:path :offset :total-lines :truncated-by :lines}
                (set (keys out))))
      (expect (string? (:path out)))
      (expect (= 1 (:offset out)))
      ;; str/split-lines drops the trailing empty after final \n.
      (expect (= 3 (:total-lines out)))
      (expect (= ["alpha" "beta" "gamma"] (:lines out)))
      (expect (= :end-of-file (:truncated-by out)))))

  (it ":lines carries raw strings - no leading line-number prefix"
    (let [path (write-temp! "raw.txt" "   indented\nplain\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      ;; The contract preserves the file's exact bytes per line so
      ;; the model can compute on raw content without parsing display text.
      (expect (= ["   indented" "plain"] (:lines out)))))

  (it "cat reads everything; display ranges belong to v/preview"
    (let [long-line (apply str (repeat 7000 "x"))
          path (write-temp! "whole-cat.txt" (str long-line "\nlast\n"))
          read-file (private-fn "read-file")
          out (read-file path)]
      (expect (= :end-of-file (:truncated-by out)))
      (expect (= [long-line "last"] (:lines out)))))

  (it "rejects all v/cat opts because cat is full acquisition only"
    (let [path (write-temp! "validate.txt" "x\n")
          read-file (private-fn "read-file")]
      (doseq [bad-opts [(hash-map (keyword "offset") 1)
                        (hash-map (keyword "limit") 10)
                        (hash-map (keyword "char-limit") 20)
                        (hash-map (keyword "max-lines") 2)
                        2]]
        (expect (throws? clojure.lang.ExceptionInfo #(read-file path bad-opts)))))))

(defdescribe new-renderer-contract-test
  (it "v/cat journal renderer shows head/tail with read-more hint"
    (let [journal-render-cat (private-fn "journal-render-cat")
          result {:path "src/demo.clj" :offset 0 :total-lines 6 :truncated-by :end-of-file
                  :lines ["alpha" "beta" "gamma" "delta" "epsilon" "zeta"]}
          out (journal-render-cat result)]
      (expect (string/includes? out "v/cat src/demo.clj"))
      (expect (string/includes? out "6 line(s)"))
      (expect (string/includes? out "alpha"))
      (expect (string/includes? out "<your binding>"))))

  (it "v/cat channel renderer wraps lines in a code block"
    (let [channel-render-cat (private-fn "channel-render-cat")
          result {:path "src/demo.clj" :offset 0 :total-lines 1 :truncated-by :end-of-file
                  :lines ["only-line"]}
          out (channel-render-cat result :channel-tui)]
      (expect (string/includes? out "```text"))
      (expect (string/includes? out "1: only-line"))))

  (it "engine-default channel error formatter renders failures without symbol error-fn"
    (let [out (extension/default-channel-error-text
                {:success? false :info {:op :v/cat}
                 :error {:type "java.io.FileNotFoundException"
                         :message "src/missing.clj (No such file)"}}
                :channel-tui)]
      (expect (string/includes? out "**ERROR**"))
      (expect (string/includes? out "v/cat"))
      (expect (string/includes? out "FileNotFoundException")))))

(defdescribe vis-rg-structured-shape-test
  (it "returns a 2-key map: :hits + :truncated-by"
    (let [_    (write-temp! "rg/a.txt" "alpha needle gamma\nbeta\n")
          _    (write-temp! "rg/b.txt" "plain line\nanother needle here\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["needle"] :paths [(temp-dir-path "rg")]})]
      (expect (= #{:hits :truncated-by} (set (keys out))))
      (expect (vector? (:hits out)))
      ;; Every hit is a clean {:path :line :text} map, no sentinel.
      (expect (every? #(= #{:path :line :text} (set (keys %))) (:hits out)))
      (expect (= 2 (count (:hits out))))
      (expect (= :end-of-results (:truncated-by out)))))

  (it "query strings are literal, including pipe characters"
    (let [_    (write-temp! "rgliteral/a.clj" "foo|bar\nfoo only\nbar only\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["foo|bar"]
                      :paths [(temp-dir-path "rgliteral")]
                      :include ["*.clj"]})]
      (expect (= ["foo|bar"] (mapv :text (:hits out))))))

  (it "spec {:all [...]} requires all literals on the same line"
    (let [_    (write-temp! "rgall/a.clj" "(defn info-event [x] x)\n(defn other [x] x)\ninfo-event call\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["defn" "info-event"]
                      :paths [(temp-dir-path "rgall")]
                      :include ["*.clj"]})]
      (expect (= ["(defn info-event [x] x)"]
                (mapv :text (:hits out))))))

  (it "spec {:any [...]} is explicit OR"
    (let [_    (write-temp! "rgany/a.clj" "alpha\nbeta\ngamma\n")
          grep (private-fn "grep-files")
          out  (grep {:any ["alpha" "gamma"]
                      :paths [(temp-dir-path "rgany")]
                      :include ["*.clj"]})]
      (expect (= ["alpha" "gamma"] (mapv :text (:hits out))))))

  (it "accepts path vectors, include globs, and dedups overlapping roots"
    (let [root (temp-dir-path "rgpaths")
          _    (write-temp! "rgpaths/src/a.clj" "needle clj\n")
          _    (write-temp! "rgpaths/src/a.txt" "needle txt\n")
          _    (write-temp! "rgpaths/test/b.cljc" "needle cljc\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["needle"]
                      :paths [root (str root "/src")]
                      :include ["*.clj" "*.cljc"]})]
      (expect (= ["needle clj" "needle cljc"]
                (mapv :text (:hits out))))))

  (it "private grep and public rg use the same single spec-map grammar"
    (let [_ (write-temp! "rgsame/a.clj" "needle same\n")
          spec {:all ["needle"]
                :paths [(temp-dir-path "rgsame")]
                :include ["*.clj"]}
          grep (private-fn "grep-files")
          rg (private-fn "rg-tool")]
      (expect (= (grep spec) (:result (rg spec))))))

  (it "rejects shorthand and unknown keys instead of silently changing grammar"
    (let [grep (private-fn "grep-files")
          bad-spec (fn [k v] (assoc {:all ["needle"] :paths ["."]} k v))]
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep "needle")))
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep {:all ["needle"] :paths "."})))
      (doseq [[k v] [[(keyword "limit") 2]
                     [(keyword "type") :clj]
                     [(keyword "mode") :any]]]
        (expect (throws? clojure.lang.ExceptionInfo
                  #(grep (bad-spec k v)))))))

  (it ":truncated-by :internal-cap when results exceed the private acquisition cap"
    (let [_ (write-temp! "rgcap/a.txt"
              (string/join "\n" (map #(str "needle " %) (range 60))))
          grep (private-fn "grep-files")
          out  (grep {:all ["needle"] :paths [(temp-dir-path "rgcap")]})]
      (expect (= 50 (count (:hits out))))
      (expect (= :internal-cap (:truncated-by out)))))

  (it "empty result still has :truncated-by :end-of-results, never nil"
    (let [_ (write-temp! "rgmiss/a.txt" "nothing matches in here\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["definitely-not-present"] :paths [(temp-dir-path "rgmiss")]})]
      (expect (= [] (:hits out)))
      (expect (= :end-of-results (:truncated-by out))))))

(defdescribe thin-bbfs-wrapper-test
  (it "patch replaces exact text only when search is unique"
    (let [path  (write-temp! "bbfs/patch.txt" "alpha\nbeta\ngamma\n")
          patch (private-fn "patch-safe")]
      (expect (= [{:path path
                   :before "alpha\nbeta\ngamma\n"
                   :after "alpha\nBETA\ngamma\n"}]
                (patch [{:path path :search "beta" :replace "BETA"}])))
      (expect (= "alpha\nBETA\ngamma\n" (slurp path)))
      (let [err (try
                  (patch [{:path path :search "missing" :replace "x"}])
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? err))
        (expect (= 0 (-> err ex-data :failures first :matches))))
      (spit path "dup\ndup\n")
      (let [err (try
                  (patch [{:path path :search "dup" :replace "x"}])
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? err))
        (expect (= 2 (-> err ex-data :failures first :matches))))))

  (it "patch diagnostics report failing edit index, all failures, bounded previews, and write nothing"
    (let [path  (write-temp! "bbfs/patch-diagnostics.txt" "alpha\nbeta\ngamma\n")
          patch (private-fn "patch-safe")
          long-search (apply str (repeat 80 "missing "))
          err   (try
                  (patch [{:path path :search "alpha" :replace "ALPHA"}
                          {:path path :search long-search :replace "x"}
                          {:path path :search "other missing" :replace "y"}])
                  nil
                  (catch clojure.lang.ExceptionInfo e e))
          data  (ex-data err)
          failures (:failures data)]
      (expect (some? err))
      (expect (string/includes? (ex-message err) "first edit 1"))
      (expect (= [1 2] (mapv :edit-index failures)))
      (expect (= [0 0] (mapv :matches failures)))
      (expect (every? #(<= (count (:search-preview %)) 200) failures))
      (expect (= "alpha\nbeta\ngamma\n" (slurp path)))))

  (it "patch-check reports match counts without writing"
    (let [path        (write-temp! "bbfs/patch-check.txt" "alpha\nbeta\nbeta\n")
          patch-check (private-fn "patch-check")
          out         (patch-check [{:path path :search "alpha" :replace "ALPHA"}
                                    {:path path :search "beta" :replace "BETA"}
                                    {:path path :search "missing" :replace "x"}])]
      (expect (= [1 2 0] (mapv :matches (:checks out))))
      (expect (= [1 2] (mapv :edit-index (:failures out))))
      (expect (false? (:valid? out)))
      (expect (= "alpha\nbeta\nbeta\n" (slurp path)))))

  (it "glob returns cwd-relative path strings for immediate-child patterns"
    (let [_    (write-temp! "bbfs/tree/a.clj" "(ns a)")
          _    (write-temp! "bbfs/tree/b.txt" "b")
          glob (private-fn "glob-safe")
          root (str (temp-root) "/bbfs/tree")]
      (expect (= #{"target/editing-test/bbfs/tree/a.clj"
                   "target/editing-test/bbfs/tree/b.txt"}
                (set (:paths (glob root "*")))))
      (expect (= ["target/editing-test/bbfs/tree/a.clj"]
                (:paths (glob root "*.clj"))))))

  (it "glob walks descendants for recursive patterns and supports explicit :scope"
    (let [_    (write-temp! "bbfs/deep/a.clj" "(ns a)")
          _    (write-temp! "bbfs/deep/nested/b.clj" "(ns b)")
          glob (private-fn "glob-safe")
          root (str (temp-root) "/bbfs/deep")]
      (expect (= #{"target/editing-test/bbfs/deep/a.clj"
                   "target/editing-test/bbfs/deep/nested/b.clj"}
                (set (:paths (glob root "**/*.clj")))))
      (expect (= ["target/editing-test/bbfs/deep/a.clj"]
                (:paths (glob root "*.clj" {:scope :children}))))))

  (it "exists? and delete-if-exists work on cwd-relative paths"
    (let [path             (write-temp! "bbfs/meta/x.txt" "x")
          exists?          (private-fn "exists-safe?")
          delete-if-exists (private-fn "delete-if-exists-safe")]
      (expect (true? (exists? path)))
      (expect (true? (delete-if-exists path)))
      (expect (false? (exists? path)))))

  (it "bash runs bounded commands inside the working tree"
    (let [run-bash (private-fn "run-bash-safe")
          out      (run-bash "printf '%s' hello && printf '%s' err >&2" {:timeout-ms 5000})]
      (expect (= 0 (:exit out)))
      (expect (= "hello" (:stdout out)))
      (expect (= "err" (:stderr out)))
      (expect (= "." (:cwd out)))
      (expect (false? (:timed-out? out)))))

  (it "bash default cwd follows the active workspace root binding"
    (let [run-bash (private-fn "run-bash-safe")
          root     (.getCanonicalPath (fs/file (temp-dir-path "workspace-root")))]
      (binding [workspace-context/*workspace-root* root]
        (let [out (run-bash "pwd" {:timeout-ms 5000})]
          (expect (= root (string/trim (:stdout out))))
          (expect (= "." (:cwd out)))))))

  (it "bash warns when Traceback appears on stderr despite exit 0"
    (let [bash-tool   (private-fn "bash-tool")
          render-bash (private-fn "render-bash")
          out         (bash-tool "printf '%s\n' 'Traceback (most recent call last):' >&2" {:timeout-ms 5000})
          rendered    (render-bash {:tool-result out})]
      (expect (true? (:success? out)))
      (expect (= 0 (get-in out [:result :exit])))
      (expect (= :stderr-traceback-with-zero-exit
                (get-in out [:result :warnings 0 :type])))
      (expect (string/includes? rendered "warning(s)"))
      (expect (string/includes? rendered "swallowed"))))

  (it "bash prepends strict mode and keeps original command in result"
    (let [strict-command (private-fn "strict-bash-command")
          bash-tool      (private-fn "bash-tool")
          out            (bash-tool "false\necho SHOULD_NOT_RUN" {:timeout-ms 5000})
          result         (:result out)]
      (expect (= "set -euo pipefail\nfalse\necho SHOULD_NOT_RUN"
                (strict-command "false\necho SHOULD_NOT_RUN")))
      (expect (true? (:success? out)))
      (expect (= 1 (:exit result)))
      (expect (= "" (:stdout result)))
      (expect (true? (:strict? result)))
      (expect (= "false\necho SHOULD_NOT_RUN" (:original-command result)))
      (expect (string/starts-with? (:command result) "set -euo pipefail\n"))))

  (it "bash interruption returns concise cancelled failure instead of timeout or stack dump"
    (let [entry       (some #(when (= 'bash (:ext.symbol/sym %)) %)
                        (:ext/symbols foundation/vis-extension))
          render-bash (private-fn "render-bash")
          out         (promise)
          worker      (Thread.
                        (fn []
                          (try
                            (deliver out
                              (extension/invoke-symbol-wrapper
                                foundation/vis-extension entry
                                ["sleep 5" {:timeout-ms 30000}]
                                {}))
                            (catch Throwable e
                              (deliver out e)))))]
      (.start worker)
      (Thread/sleep 100)
      (.interrupt worker)
      (let [v        (deref out 2000 ::timeout)
            rendered (when (map? v) (render-bash {:tool-result v}))]
        (expect (not= ::timeout v))
        (expect (map? v))
        (expect (false? (:success? v)))
        (expect (= "java.lang.InterruptedException" (get-in v [:error :type])))
        (expect (= [] (get-in v [:error :trace])))
        (expect (string/includes? (get-in v [:error :message]) "cancelled"))
        (expect (not (string/includes? (get-in v [:error :message]) "timed out")))
        (expect (= :interrupted (get-in v [:info :status])))
        (expect (= "sleep 5" (get-in v [:info :command])))
        (expect (= :v/bash (get-in v [:info :op])))
        (expect (= "." (get-in v [:info :target :requested])))
        (expect (string/includes? rendered "cancelled"))
        (expect (not (string/includes? rendered "timed out")))
        (expect (not (string/includes? (pr-str (:error v)) "core.clj"))))))

  (it "bash validates cwd through the same safe path guard and blocks shell source edits"
    (let [run-bash (private-fn "run-bash-safe")]
      (expect (throws? clojure.lang.ExceptionInfo #(run-bash "pwd" {:cwd ".."})))
      (let [command "python3 - <<'PY'\nfrom pathlib import Path\nPath('src/demo.clj').write_text('(ns demo)')\nPY"]
        (expect (throws? clojure.lang.ExceptionInfo #(run-bash command)))
        (try
          (run-bash command)
          (expect false)
          (catch clojure.lang.ExceptionInfo e
            (expect (= :ext.foundation.editing/bash-clojure-source-edit-blocked (:type (ex-data e))))
            (expect (= :use-z-patch (:reason (ex-data e))))))))))

(defdescribe editing-renderer-guidance-test
  (it "patch renderer avoids mandatory duplicate read-back and shows fenced diffs"
    (let [render-patch (private-fn "render-patch")
          rendered (render-patch
                     {:tool-result {:success? true
                                    :result [{:path "target/editing-test/out.txt"}]
                                    :info {:files [{:path "target/editing-test/out.txt"
                                                    :changed? true
                                                    :before "alpha\nbeta\n"
                                                    :after "alpha\ngamma\n"}]}}})]
      (expect (string/includes? rendered "Read back only when exact persisted bytes matter"))
      (expect (string/includes? rendered "```diff"))
      (expect (string/includes? rendered "--- a/target/editing-test/out.txt"))
      (expect (string/includes? rendered "-beta"))
      (expect (string/includes? rendered "+gamma"))))
  (it "search-hits renderer formats partial-projection hits without raw EDN fallback"
    (let [render-hits (private-fn "render-search-hits-kind")
          rendered (render-hits
                     {:surface :markdown
                      :value {:result {:hits [{:line 462
                                               :text "  (inc (reduce max 0 ...))"}
                                              {:line 472
                                               :text "(defn- workspace-tabs-or-base"}]}}})]
      (expect (string? rendered))
      (expect (not (string/includes? rendered "{:line 462")))
      (expect (not (string/includes? rendered ":text \"")))
      (expect (string/includes? rendered "line 462"))
      (expect (string/includes? rendered "(inc (reduce max 0"))
      (expect (string/includes? rendered "line 472"))
      (expect (string/includes? rendered "workspace-tabs-or-base"))))
  (it "search-hits renderer keeps full path:line backtick prefix when path present"
    (let [render-hits (private-fn "render-search-hits-kind")
          rendered (render-hits
                     {:surface :markdown
                      :value {:result {:hits [{:path "src/foo.clj"
                                               :line 10
                                               :text "(def x 1)"}]}}})]
      (expect (string/includes? rendered "`src/foo.clj:10`"))
      (expect (string/includes? rendered "(def x 1)")))))

(defdescribe tool-envelope-test
  (it "tool wrappers return the required contract keys"
    (let [path (write-temp! "contract/read.txt" "alpha\nbeta\n")
          cat-tool (private-fn "cat-tool")
          out (cat-tool path)]
      (expect (= #{:success? :result :info :error :presentation}
                (set (keys out))))
      (expect (true? (:success? out)))
      (expect (= ["alpha" "beta"] (get-in out [:result :lines])))
      (expect (not (contains? out :markdown)))
      (expect (nil? (:error out)))
      (expect (= :source (get-in out [:presentation :kind])))))

  (it "tool failure contract includes structured :error with normalized trace"
    (let [cat-symbol (private-fn "cat-symbol")
          on-error   (:ext.symbol/on-error-fn cat-symbol)
          out        (:result (on-error (ex-info "boom" {}) nil nil ["missing.txt"]))]
      (expect (false? (:success? out)))
      (expect (= nil (:result out)))
      (expect (= "clojure.lang.ExceptionInfo" (get-in out [:error :type])))
      (expect (vector? (get-in out [:error :trace])))
      (expect (not (contains? out :markdown))))))
