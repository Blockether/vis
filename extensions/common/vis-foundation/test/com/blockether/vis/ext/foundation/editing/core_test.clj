(ns com.blockether.vis.ext.foundation.editing.core-test
  "Tests for the editing extension.

   Smoke-checks the loaded extension surface (symbol vector, doc
   strings, prompt fragment) plus behavioral coverage of the
   structured preview/search helpers (`v/cat`, `v/rg`) and the new
   thin babashka.fs wrappers (`v/patch`, `v/copy`, ...).

   Tests reach private fns directly through the registry to avoid
   bringing up a full SCI sandbox. Temp files land under
   `target/editing-test/` (always inside the repo cwd, so
   `safe-path` accepts them)."
  (:require
   [babashka.fs :as fs]
   [clojure.set]
   [clojure.string :as string]
   [com.blockether.vis.ext.foundation.editing.core :as editing]
   [com.blockether.vis.internal.env.handle :as handle]
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
    (expect (= 11 (count editing/editing-symbols)))
    (expect (not-any? #{'edit 'write 'cwd 'parent 'file-name 'extension 'relativize 'bash}
              (map :ext.symbol/symbol editing/editing-symbols)))
    (expect (not-any? #{'read-all-lines}
              (map :ext.symbol/symbol editing/editing-symbols)))
    (expect (some #{'patch}
              (map :ext.symbol/symbol editing/editing-symbols)))
    (expect (some #{'patch-check}
              (map :ext.symbol/symbol editing/editing-symbols)))
    (expect (not-any? #{'write-lines 'update-file}
              (map :ext.symbol/symbol editing/editing-symbols)))
    (expect (not-any? #{'preview 'silent!}
              (map :ext.symbol/symbol editing/editing-symbols))))

  (it "bash tool fully removed: no symbol, no helpers, no prompt mention"
    (let [symbols (map :ext.symbol/symbol (editing/available-editing-symbols))
          prompt (editing/available-editing-prompt)]
      (expect (not-any? #{'bash} symbols))
      (expect (not (string/includes? prompt "v/bash")))
      (expect (not (string/includes? prompt "bash")))
      (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "bash-tool"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "bash-symbol"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "run-bash-safe"))))))

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

  (it "editing prompt has no v/preview references (tool retired)"
    (expect (not (string/includes? editing/editing-prompt "v/preview")))
    (expect (nil? (some #(when (= 'preview (:ext.symbol/symbol %)) %)
                    editing/editing-symbols))))

  (it "pushes search/read/path discovery to the structured v tool surface"
    (expect (string/includes? editing/editing-prompt "v/rg"))
    (expect (string/includes? editing/editing-prompt "v/ls"))
    (expect (string/includes? editing/editing-prompt "v/cat"))
    nil)

  (it "documents v/rg's exact single spec-map grammar, not the old shorthand"
    (expect (string/includes? editing/editing-prompt
              "{:any [\"a\" \"b\"] :paths [\"src\"] :include [\"**/*.clj\"]}"))
    (expect (string/includes? editing/editing-prompt "no regex/shorthand"))
    (expect (not (string/includes? editing/editing-prompt "(v/rg :include/:exclude"))))

  (it "registers journal + channel renderers on every fn-symbol"
    (doseq [sym-name '[cat ls rg patch patch-check create-dirs copy move delete delete-if-exists exists?]]
      (let [entry (some #(when (= sym-name (:ext.symbol/symbol %)) %)
                    editing/editing-symbols)]
        (expect (ifn? (:ext.symbol/journal-render-fn entry)))
        (expect (ifn? (:ext.symbol/channel-render-fn entry)))))))

(it "defers op classification to the engine contract (no editing-local copy)"
  ;; The classification table + presentation map live in
  ;; `com.blockether.vis.internal.extension` (`op-tag`,
  ;; `op-presentation`). Editing used to keep a thin shim; that
  ;; shim is gone and callers go straight to the engine. Tags
  ;; collapsed to observation/mutation values; ops not in the
  ;; registration table fail closed instead of defaulting to observation.
  (doseq [[op tag] [[:v/cat         :op.tag/observation]
                    [:z/locators    :op.tag/observation]
                    [:v/rg          :op.tag/observation]
                    [:v/patch       :op.tag/mutation]
                    [:v/create-dirs :op.tag/mutation]
                    [:v/delete      :op.tag/mutation]
                    [:v/move        :op.tag/mutation]]]
    (expect (= tag (extension/op-tag op)))
    (expect (= {:tag tag} (extension/op-presentation op))))
  (let [thrown (try (extension/op-tag :v/extensions)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (expect (= :extension/unregistered-op (:type (ex-data thrown))))))

(defdescribe editing-prompt-read-policy-test
  (it "teaches full-read vars, canonical patching, and no duplicate rereads by default"
    (let [patch-symbol (some #(when (= 'patch (:ext.symbol/symbol %)) %)
                         editing/editing-symbols)]
      (expect (string/includes? editing/editing-prompt
                "v/cat reads one window"))
      (expect (string/includes? editing/editing-prompt
                "(:next-offset prev)"))
      (expect (string/includes? editing/editing-prompt
                "(v/patch [{:path :search :replace}])"))
      (expect (string/includes? editing/editing-prompt
                "each :search must match exactly once"))
      (expect (string/includes? editing/editing-prompt
                "Codex apply_patch envelope"))
      (expect (string/includes? (:ext.symbol/doc patch-symbol)
                "Codex `apply_patch` envelope"))
      (expect (string/includes? (:ext.symbol/doc patch-symbol)
                "validate the full plan against the live filesystem\n   before any write"))
      (expect (string/includes? editing/editing-prompt
                "Do NOT v/cat to verify"))
      (expect (not (string/includes? editing/editing-prompt "read-all-lines")))
      (expect (not (string/includes? editing/editing-prompt "write-lines")))
      (expect (not (string/includes? editing/editing-prompt "update-file"))))))

(defdescribe vis-ls-structured-shape-test
  (it "represents the workspace root as dot plus absolute path"
    (let [list-files (private-fn "list-files")
          ls-tool    (private-fn "ls-tool")
          out        (list-files ".")
          summary    (handle/summary (:result (ls-tool ".")))]
      (expect (= "." (:path out)))
      (expect (= (str (.toAbsolutePath (fs/path (fs/cwd))))
                (:absolute-path out)))
      (expect (= (:absolute-path out) (:absolute-path summary)))
      (expect (= :dir (:type out))))))

(defdescribe vis-cat-structured-shape-test
  (it "returns the paginated shape (small file, single window, eof)"
    (let [path (write-temp! "small.txt" "alpha\nbeta\ngamma\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      (expect (= #{:path :offset :returned :limit :next-offset :eof? :truncated-by :lines}
                (set (keys out))))
      (expect (string? (:path out)))
      (expect (= 1 (:offset out)))
      (expect (= 3 (:returned out)))
      (expect (= 200 (:limit out)))
      (expect (nil? (:next-offset out)))
      (expect (true? (:eof? out)))
      (expect (= :eof (:truncated-by out)))
      (expect (= ["alpha" "beta" "gamma"] (:lines out)))))

  (it ":lines carries raw strings - no leading line-number prefix"
    (let [path (write-temp! "raw.txt" "   indented\nplain\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      (expect (= ["   indented" "plain"] (:lines out)))))

  (it "(v/cat path n) reads first n lines and reports :limit truncation"
    (let [body (string/join "\n" (map #(str "line-" %) (range 1 11)))
          path (write-temp! "ten.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 4)]
      (expect (= 1 (:offset out)))
      (expect (= 4 (:returned out)))
      (expect (= 4 (:limit out)))
      (expect (= 5 (:next-offset out)))
      (expect (false? (:eof? out)))
      (expect (= :limit (:truncated-by out)))
      (expect (= ["line-1" "line-2" "line-3" "line-4"] (:lines out)))))

  (it "(v/cat path offset n) reads a mid-file window and advances :next-offset"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 21)))
          path (write-temp! "twenty.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 7 3)]
      (expect (= 7 (:offset out)))
      (expect (= 3 (:returned out)))
      (expect (= 10 (:next-offset out)))
      (expect (= ["L7" "L8" "L9"] (:lines out)))
      (expect (= :limit (:truncated-by out)))))

  (it "paging via :next-offset reaches eof cleanly"
    (let [body (string/join "\n" (map #(str "line-" %) (range 1 11)))
          path (write-temp! "page.txt" (str body "\n"))
          read-file (private-fn "read-file")
          page-1 (read-file path 1 4)
          page-2 (read-file path (:next-offset page-1) 4)
          page-3 (read-file path (:next-offset page-2) 4)]
      (expect (= ["line-1" "line-2" "line-3" "line-4"] (:lines page-1)))
      (expect (= ["line-5" "line-6" "line-7" "line-8"] (:lines page-2)))
      (expect (= ["line-9" "line-10"] (:lines page-3)))
      (expect (true? (:eof? page-3)))
      (expect (nil? (:next-offset page-3)))
      (expect (= :eof (:truncated-by page-3)))))

  (it "offset past EOF returns an empty window, eof?, no next-offset"
    (let [path (write-temp! "two.txt" "a\nb\n")
          read-file (private-fn "read-file")
          out  (read-file path 99 10)]
      (expect (= 0 (:returned out)))
      (expect (= [] (:lines out)))
      (expect (true? (:eof? out)))
      (expect (nil? (:next-offset out)))
      (expect (= :eof (:truncated-by out)))))

  (it ":truncated-by :bytes when a window would exceed max-cat-window-bytes"
    ;; Each line is ~70KB; the byte cap is 64KB. First line is always
    ;; included (guarantees one-line forward progress), second line
    ;; would push past the cap -> stop with :bytes.
    (let [huge (apply str (repeat 70000 "x"))
          path (write-temp! "huge.txt" (str huge "\n" huge "\n" huge "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 1 10)]
      (expect (= :bytes (:truncated-by out)))
      (expect (= 1 (:returned out)))
      (expect (false? (:eof? out)))
      (expect (= 2 (:next-offset out)))))

  (it "persistence-blob contract: :lines bytes are bounded by max-cat-window-bytes"
    ;; This is the storage claim: a single v/cat call cannot persist
    ;; more than max-cat-window-bytes of line bytes regardless of file size.
    (let [line (apply str (repeat 200 "x"))    ; 200 bytes
          body (string/join "\n" (repeat 5000 line))  ; ~1MB total
          path (write-temp! "persist.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 1 100000)
          line-bytes (reduce + 0 (map #(inc (count (.getBytes ^String % "UTF-8")))
                                   (:lines out)))]
      (expect (<= line-bytes 65536))))

  (it "rejects bad positional args (non-positive ints, non-int types)"
    (let [path (write-temp! "validate.txt" "x\n")
          read-file (private-fn "read-file")]
      (doseq [bad [[0 10]    ; offset 0 (must be >= 1)
                   [-1 10]   ; negative offset
                   [1 0]     ; zero limit
                   [1 -5]    ; negative limit
                   ["a" 10]  ; non-int offset
                   [1 :hi]]] ; non-int limit
        (expect (throws? clojure.lang.ExceptionInfo
                  #(apply read-file path bad)))))))

(defn- cat-handle
  "Construct a CatHandle directly for renderer-contract tests, mirroring
   what `cat-tool` does internally."
  [path offset lines next-offset eof? truncated-by]
  (handle/clear-store!)
  (handle/make-cat
    {:path path
     :lines lines
     :offset offset
     :next-offset next-offset
     :eof? eof?
     :truncated-by truncated-by}))

(defdescribe new-renderer-contract-test
  (it "v/cat journal renderer collapses to a one-line handle summary plus the read-more hint"
    (let [journal-render-cat (private-fn "journal-render-cat")
          h    (cat-handle "src/demo.clj" 1
                 ["alpha" "beta" "gamma" "delta" "epsilon" "zeta"]
                 nil true :eof)
          out  (journal-render-cat h)]
      ;; New shape: handle's print-method emits `#vis/handle {...}` on
      ;; one line. The journal must not echo file content (that path is
      ;; closed.
      (expect (string/starts-with? out "#vis/handle "))
      (expect (string/includes? out ":op :v/cat"))
      (expect (string/includes? out ":path \"src/demo.clj\""))
      (expect (string/includes? out ":line-count 6"))
      (expect (string/includes? out "<your binding>"))
      ;; The handle's :first-line / :last-line summary intentionally
      ;; previews bookends, so a 6-line file's "alpha" / "zeta" can
      ;; appear in the summary. The leak guarantee is only about
      ;; numbered-line content blocks (`1: alpha` etc.), which the new
      ;; renderer no longer emits.
      (expect (not (string/includes? out "1: alpha")))))

  (it "v/cat handle summary surfaces pagination metadata"
    (let [h    (cat-handle "big.log" 1 ["a" "b" "c" "d"] 5 false :limit)
          summ (handle/summary h)]
      ;; Pagination state moves from the journal header into the
      ;; handle's summary; the model reads :next-offset / :eof? off the
      ;; summary to decide whether to page.
      (expect (= 5 (:next-offset summ)))
      (expect (false? (:eof? summ)))
      (expect (= :limit (:truncated-by summ))))))

(defdescribe channel-renderer-contract-test
  (it "v/cat channel renderer returns canonical [:ir ...] with a :code block, line-numbered from :offset"
    (let [channel-render-cat (private-fn "channel-render-cat")
          h   (cat-handle "src/demo.clj" 1 ["only-line"] nil true :eof)
          out (channel-render-cat h)]
      (expect (vector? out))
      (expect (= :ir (first out)))
      (let [code-blocks (filter #(and (vector? %) (= :code (first %))) (tree-seq sequential? seq out))
            body (last (first code-blocks))]
        (expect (= 1 (count code-blocks)))
        (expect (string/includes? body "1: only-line")))))

  (it "v/cat channel renderer separates inline text/code tokens with spaces"
    (let [channel-render-cat (private-fn "channel-render-cat")
          h   (cat-handle "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj"
                1898 ["x"] 1928 false :limit)
          out (channel-render-cat h)
          paragraph (nth out 2)
          text (apply str (filter string? (tree-seq sequential? seq paragraph)))]
      (expect (string/includes? text
                "Read extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj — 1 line(s) from line 1898 (next-offset 1928)."))))

  (it "v/cat channel renderer respects :offset for mid-file windows"
    (let [channel-render-cat (private-fn "channel-render-cat")
          h   (cat-handle "f.txt" 100 ["hundred" "hundred-one"] 102 false :limit)
          out (channel-render-cat h)
          body (last (first (filter #(and (vector? %) (= :code (first %)))
                              (tree-seq sequential? seq out))))]
      (expect (= :ir (first out)))
      (expect (string/includes? body "100: hundred"))
      (expect (string/includes? body "101: hundred-one")))))

(defdescribe error-formatter-contract-test
  (it "engine-default channel error formatter renders failures as canonical [:ir ...]"
    (let [out (extension/default-channel-error-ir
                {:success? false
                 :symbol :v/cat
                 :error {:message "src/missing.clj (No such file)"
                         :trace "java.io.FileNotFoundException: src/missing.clj (No such file)"}})
          text-leaves (filter string? (tree-seq sequential? seq out))
          joined (string/join " " text-leaves)]
      (expect (vector? out))
      (expect (= :ir (first out)))
      (expect (string/includes? joined "ERROR"))
      (expect (string/includes? joined "v/cat"))
      (expect (string/includes? joined "FileNotFoundException")))))

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
          rg (private-fn "rg-tool")
          ;; rg-tool now returns an RgHandle as :result; deref to compare
          ;; against the raw grep payload (vec of hits).
          rg-handle (:result (rg spec))]
      (expect (= (:hits (grep spec)) (deref rg-handle)))))

  (it "rejects shorthand and unknown keys instead of silently changing grammar"
    (let [grep (private-fn "grep-files")
          rg (private-fn "rg-tool")
          bad-spec (fn [k v] (assoc {:all ["needle"] :paths ["."]} k v))]
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep "needle")))
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep {:all ["needle"] :paths "."})))
      (let [err (try
                  (rg "needle" {:include "**/*.clj"})
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? err))
        (expect (= :ext.foundation.editing/invalid-rg-arity (:type (ex-data err))))
        (expect (clojure.string/includes? (ex-message err) "exactly one spec map")))
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

  (it "exists? and delete-if-exists work on cwd-relative paths"
    (let [path             (write-temp! "bbfs/meta/x.txt" "x")
          exists?          (private-fn "exists-safe?")
          delete-if-exists (private-fn "delete-if-exists-safe")]
      (expect (true? (exists? path)))
      (expect (true? (delete-if-exists path)))
      (expect (false? (exists? path)))))

  (it "bash helpers fully removed from the editing core"
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "run-bash-safe"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "bash-tool"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "strict-bash-command"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "coerce-bash-opts"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "bash-warnings"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "channel-render-bash"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation.editing.core" "journal-render-bash"))))))

(defdescribe editing-renderer-guidance-test
  (it "patch renderer reports patched paths"
    (let [render-patch (private-fn "channel-render-patch")
          rendered (render-patch [{:path "target/editing-test/out.txt"}])]
      (expect (string/includes? rendered "Patched"))
      (expect (string/includes? rendered "target/editing-test/out.txt"))))
  (it "search-hits renderer formats partial-projection hits without raw EDN fallback"
    (let [render-hits (private-fn "channel-render-rg")
          _ (handle/clear-store!)
          rg-handle (handle/make-rg
                      {:hits [{:line 462
                               :text "  (inc (reduce max 0 ...))"}
                              {:line 472
                               :text "(defn- workspace-tabs-or-base"}]
                       :truncated-by :end-of-results}
                      {:spec nil :paths nil})
          rendered (render-hits rg-handle)
          text-leaves (filter string? (tree-seq sequential? seq rendered))
          joined (string/join "\n" text-leaves)]
      ;; Channel renderer must return canonical IR, not raw text/EDN.
      (expect (vector? rendered))
      (expect (= :ir (first rendered)))
      (expect (not (string/includes? joined "{:line 462")))
      (expect (not (string/includes? joined ":text \"")))
      (expect (string/includes? joined ":462"))
      (expect (string/includes? joined "(inc (reduce max 0"))
      (expect (string/includes? joined ":472"))
      (expect (string/includes? joined "workspace-tabs-or-base"))))
  (it "search-hits renderer keeps full path:line prefix when path present"
    (let [render-hits (private-fn "channel-render-rg")
          _ (handle/clear-store!)
          rg-handle (handle/make-rg
                      {:hits [{:path "src/foo.clj"
                               :line 10
                               :text "(def x 1)"}]
                       :truncated-by :end-of-results}
                      {:spec nil :paths ["src/foo.clj"]})
          rendered (render-hits rg-handle)
          text-leaves (filter string? (tree-seq sequential? seq rendered))
          joined (string/join " " text-leaves)]
      (expect (string/includes? joined "src/foo.clj:10"))
      (expect (string/includes? joined "(def x 1)")))))

(defdescribe tool-envelope-test
  (it "tool wrappers return the required contract keys (PLAN §2.1 envelope)"
    (let [path (write-temp! "contract/read.txt" "alpha\nbeta\n")
          cat-tool (private-fn "cat-tool")
          out (cat-tool path)
          required #{:success? :result :error :symbol :tag :metadata}]
      ;; Envelope keys MUST include the canonical op/* set; extra keys
      ;; (e.g. :presentation, :stdout when set) may also appear.
      (expect (= required (clojure.set/intersection required (set (keys out)))))
      (expect (true? (:success? out)))
      ;; v/cat now returns a CatHandle as :result. Lines are reachable
      ;; via deref; the envelope shape (:success? :result ...) is unchanged.
      (let [h (:result out)]
        (expect (= ["alpha" "beta"] (deref h)))
        (expect (= "alpha" (-> h :info :first-line)))
        (expect (= "beta" (-> h :info :last-line))))
      (expect (not (contains? out :markdown)))
      (expect (nil? (:error out)))))

  (it "tool failure envelope carries structured :error per PLAN §2.1"
    (let [cat-symbol (private-fn "cat-symbol")
          on-error   (:ext.symbol/on-error-fn cat-symbol)
          out        (:result (on-error (ex-info "boom" {}) nil nil ["missing.txt"]))]
      (expect (false? (:success? out)))
      (expect (nil? (:result out)))
      ;; Per PLAN §2.7 / §7.3.4: :trace is a preformatted string,
      ;; first line carries the underlying class name.
      (expect (string? (get-in out [:error :trace])))
      (expect (string/includes? (get-in out [:error :trace]) "ExceptionInfo"))
      (expect (not (contains? out :markdown))))))
