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
   [com.blockether.vis.internal.extension :as extension]
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
    (expect (= 13 (count editing/editing-symbols)))
    (expect (not-any? #{'edit 'write 'cwd 'parent 'file-name 'extension 'relativize}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (not-any? #{'read-all-lines}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (some #{'patch}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (not-any? #{'write-lines 'update-file}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (some #{'bash}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (some #{'preview}
              (map :ext.symbol/sym editing/editing-symbols)))
    (expect (not-any? #{'silent!}
              (map :ext.symbol/sym editing/editing-symbols))))

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
    (let [bash-symbol (some #(when (= 'bash (:ext.symbol/sym %)) %)
                        editing/editing-symbols)]
      (expect (not-any? #(string/includes? % "grep")
                (:ext.symbol/examples bash-symbol)))))

  (it "registers custom structured renderers for rich tool outputs"
    (doseq [sym-name '[cat preview ls rg patch create-dirs glob copy move delete delete-if-exists exists? bash]]
      (let [entry (some #(when (= sym-name (:ext.symbol/sym %)) %)
                    editing/editing-symbols)]
        (expect (ifn? (:ext.symbol/render-fn entry))))))

  (it "teaches the model that file and shell payloads live under the tool envelope :result"
    (let [bash-symbol (some #(when (= 'bash (:ext.symbol/sym %)) %)
                        editing/editing-symbols)]
      (expect (string/includes? editing/editing-prompt "[:result :lines]"))
      (expect (string/includes? editing/editing-prompt "(-> (v/rg {:all [\"needle\"] :paths [\"src\" \"test\"] :include [\"*.clj\" \"*.cljc\"]}) :result :hits)"))
      (expect (string/includes? editing/editing-prompt "[:result :stdout]"))
      (expect (string/includes? (:ext.symbol/doc bash-symbol) ":result :stdout"))
      (expect (some #(string/includes? % "[:result :exit]")
                (:ext.symbol/examples bash-symbol))))))

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

  (it ":lines carries raw strings — no leading line-number prefix"
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

(defdescribe vis-preview-tool-test
  (it "projects nested EQL ranges without source/projection shapes"
    (let [preview-tool (private-fn "preview-tool")
          value {:result {:lines ["a" "b" "c" "d"]
                          :from "literal-from"
                          :to "literal-to"}}
          out (preview-tool value {:result [[:lines {:from 1 :to 3}]]})]
      (expect (extension/tool-result? out))
      (expect (= {:result {:lines ["b" "c"]}} (:result out)))
      (expect (= {:result [[:lines {:from 1 :to 3}]]} (:preview-eql out)))
      (expect (= {:rendering-kind :source} (:preview out)))
      (expect (not (contains? out :source-shape)))
      (expect (not (contains? out :projection-shape)))
      (expect (= :v/preview (get-in out [:provenance :op])))))

  (it "records enriched previews through the extension invocation wrapper"
    (let [entry (some #(when (= 'preview (:ext.symbol/sym %)) %)
                  (:ext/symbols foundation/vis-extension))
          sink  (atom [])
          out   (binding [extension/*preview-sink* sink]
                  (extension/invoke-symbol-wrapper
                    foundation/vis-extension entry
                    [{:result {:x 1}} {:result [:x]}]
                    {}))]
      (expect (= {:result {:x 1}} (:result out)))
      (expect (= [out] @sink))
      (expect (= 'preview (get-in out [:provenance :tool :sym])))
      (expect (= 'com.blockether.vis.ext.foundation.core
                (get-in out [:provenance :extension :namespace])))))

  (it "previews the whole payload when called without EQL"
    (let [preview-tool (private-fn "preview-tool")
          value {:result {:lines ["a" "b" "c"]}
                 :stdout "out"
                 :stderr "err"
                 :provenance {:op :ignored-by-payload}}
          out (preview-tool value)]
      (expect (= {:result {:lines ["a" "b" "c"]}
                  :stdout "out"
                  :stderr "err"}
                (:result out)))
      (expect (nil? (:preview-eql out)))))

  (it "supports literal :from/:to keys, wildcard, and ranged item projection"
    (let [preview-tool (private-fn "preview-tool")
          value {:result {:from "literal-from"
                          :to "literal-to"
                          :hits [{:path "a" :line 1 :text "x" :extra true}
                                 {:path "b" :line 2 :text "y" :extra false}]}}
          literal (preview-tool value {:result [:from :to]})
          wildcard (preview-tool value {:result [:*]})
          hits (preview-tool value {:result [[:hits {:from 0 :to 1} [:path :line :text]]]})]
      (expect (= {:result {:from "literal-from" :to "literal-to"}}
                (:result literal)))
      (expect (= (:result value) (get-in wildcard [:result :result])))
      (expect (= {:result {:hits [{:path "a" :line 1 :text "x"}]}}
                (:result hits))))))

(defdescribe rendering-kind-strategies-test
  (it "renders source, search hits, table, tree, diagnostic, markdown, text, diff, and data kinds"
    (let [source (extension/render-rendering-kind
                   :journal :source {:lines ["alpha" "beta"]}
                   {:tool-result {:preview-eql {:result [[:lines {:from 9 :to 11}]]}}})
          hits (extension/render-rendering-kind
                 :journal :search-hits [{:path "src/a.clj" :line 7 :text "needle"}])
          table (extension/render-rendering-kind
                  :journal :table [{:path "a" :line 1} {:path "b" :line 2}])
          tree (extension/render-rendering-kind
                 :journal :tree {:name "root" :type :dir
                                 :children [{:name "a.clj" :type :file :size 3}]})
          diagnostic (extension/render-rendering-kind
                       :journal :diagnostic {:exit 1 :stdout "out" :stderr "err"})
          markdown (extension/render-rendering-kind :journal :markdown "**ok**")
          text (extension/render-rendering-kind :journal :text "plain")
          diff (extension/render-rendering-kind :journal :diff "-a\n+b")
          data (extension/render-rendering-kind :journal :data {:a 1})]
      (expect (string/includes? source "10: alpha"))
      (expect (string/includes? hits "`src/a.clj:7` needle"))
      (expect (string/includes? table "| path | line |"))
      (expect (string/includes? tree "- root (dir)"))
      (expect (string/includes? tree "- a.clj (file) 3B"))
      (expect (string/includes? diagnostic "Exit `1`"))
      (expect (string/includes? diagnostic "stdout:"))
      (expect (= "**ok**" markdown))
      (expect (string/includes? text "```text"))
      (expect (string/includes? diff "```diff"))
      (expect (string/includes? data "{:a 1}"))))

  (it "renders TUI previews as direct display text without markdown fences or shape dump"
    (let [preview-tool (private-fn "preview-tool")
          render-preview (private-fn "render-preview")
          out (preview-tool {:result {:lines ["alpha" "beta" "gamma"]}}
                {:result [[:lines {:from 1 :to 3}]]})
          rendered (render-preview {:surface :tui :tool-result out})]
      (expect (not (string/includes? rendered "Preview.")))
      (expect (string/includes? rendered "2: beta"))
      (expect (string/includes? rendered "3: gamma"))
      (expect (not (string/includes? rendered "```")))
      (expect (not (string/includes? rendered "Shape:"))))))

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
    (let [_    (write-temp! "rgall/a.clj" "(defn provenance-event [x] x)\n(defn other [x] x)\nprovenance-event call\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["defn" "provenance-event"]
                      :paths [(temp-dir-path "rgall")]
                      :include ["*.clj"]})]
      (expect (= ["(defn provenance-event [x] x)"]
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
      (expect (throws? clojure.lang.ExceptionInfo
                #(patch [{:path path :search "missing" :replace "x"}])))
      (spit path "dup\ndup\n")
      (expect (throws? clojure.lang.ExceptionInfo
                #(patch [{:path path :search "dup" :replace "x"}])))))

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

  (it "bash validates cwd through the same safe path guard"
    (let [run-bash (private-fn "run-bash-safe")]
      (expect (throws? clojure.lang.ExceptionInfo #(run-bash "pwd" {:cwd ".."}))))))

(defdescribe editing-renderer-guidance-test
  (it "patch renderer avoids mandatory duplicate read-back and shows fenced diffs"
    (let [render-patch (private-fn "render-patch")
          rendered (render-patch
                     {:tool-result {:success? true
                                    :result [{:path "target/editing-test/out.txt"}]
                                    :provenance {:files [{:path "target/editing-test/out.txt"
                                                          :changed? true
                                                          :before "alpha\nbeta\n"
                                                          :after "alpha\ngamma\n"}]}}})]
      (expect (string/includes? rendered "Read back only when exact persisted bytes matter"))
      (expect (string/includes? rendered "```diff"))
      (expect (string/includes? rendered "--- a/target/editing-test/out.txt"))
      (expect (string/includes? rendered "-beta"))
      (expect (string/includes? rendered "+gamma")))))

(defdescribe tool-envelope-test
  (it "tool wrappers return the required contract keys"
    (let [path (write-temp! "contract/read.txt" "alpha\nbeta\n")
          cat-tool (private-fn "cat-tool")
          out (cat-tool path)]
      (expect (= #{:success? :result :provenance :error :presentation}
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
