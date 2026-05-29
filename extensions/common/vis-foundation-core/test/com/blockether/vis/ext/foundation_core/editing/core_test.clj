(ns com.blockether.vis.ext.foundation-core.editing.core-test
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
   [com.blockether.vis.ext.foundation-core.editing.core :as editing]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" name))))

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

(defdescribe cwd-safety-test
  ;; THE non-negotiable invariant: every v/* tool that touches the
  ;; filesystem must refuse any path that escapes (workspace/cwd).
  ;; safe-path is the single gate; this suite proves every mutation
  ;; tool actually routes through it.
  (let [escape-paths ["../escape.txt"
                      "../../etc/passwd"
                      "/etc/passwd"
                      "target/../../escape.txt"]]

    (it "v/patch (exact-replace) refuses to write outside cwd"
      (let [patch (private-fn "patch-safe")]
        (doseq [p escape-paths]
          (let [r (patch [{:path p :search "x" :replace "y"}])]
            (expect (false? (:success? r)))
            (expect (= :path-escape (-> r :failures first :reason)))))))

    (it "v/write refuses to create files outside cwd"
      ;; Note: we deliberately do NOT (.exists) the escape path here; the
      ;; check is whether `write-safe` REFUSED to act. /etc/passwd exists
      ;; on macOS regardless of our actions; what matters is :reason :path-escape
      ;; and the cwd guard kicking in before any IO.
      (let [write (private-fn "write-safe")]
        (doseq [p escape-paths]
          (let [r (write {:path p :content "hi"})]
            (expect (false? (:success? r)))
            (expect (= :path-escape (-> r :failures first :reason)))))))

    (it "v/create-dirs refuses to mkdir outside cwd"
      (let [create (private-fn "create-dirs-safe")]
        (doseq [p escape-paths]
          (let [err (try (create p) nil
                      (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err))
            (expect (= :ext.foundation.editing/path-escape
                      (:type (ex-data err))))))))

    (it "v/copy refuses src OR dest outside cwd"
      (let [copy (private-fn "copy-safe")
            inside (write-temp! "cwd-safety/copy-src.txt" "x")]
        (doseq [p escape-paths]
          (let [err1 (try (copy p inside) nil (catch clojure.lang.ExceptionInfo e e))
                err2 (try (copy inside p) nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err1))
            (expect (some? err2))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err1))))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err2))))))))

    (it "v/move refuses src OR dest outside cwd"
      (let [move (private-fn "move-safe")
            inside (write-temp! "cwd-safety/move-src.txt" "x")]
        (doseq [p escape-paths]
          (let [err1 (try (move p inside) nil (catch clojure.lang.ExceptionInfo e e))
                err2 (try (move inside p) nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err1))
            (expect (some? err2))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err1))))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err2))))))))

    (it "v/delete and v/delete-if-exists refuse paths outside cwd"
      (let [del   (private-fn "delete-safe")
            del-if (private-fn "delete-if-exists-safe")]
        (doseq [p escape-paths]
          (let [err1 (try (del p) nil (catch clojure.lang.ExceptionInfo e e))
                err2 (try (del-if p) nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err1))
            (expect (some? err2))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err1))))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err2))))))))

    (it "v/cat (read) ALSO refuses paths outside cwd"
      ;; Defense in depth: even reads can't leak through path traversal.
      (let [cat (private-fn "read-file")]
        (doseq [p escape-paths]
          (let [err (try (cat p) nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err))
            (expect (= :ext.foundation.editing/path-escape
                      (:type (ex-data err))))))))))

(defdescribe editing-extension-loads-test
  (it "exposes structured helpers plus the required thin babashka.fs wrappers"
    (expect (vector? editing/editing-symbols))
    ;; cat, ls, rg, patch, write, create-dirs, copy, move, delete,
    ;; delete-if-exists, exists?
    (expect (= 11 (count editing/editing-symbols)))
    ;; `write` IS exposed (T9 added it as the whole-file primitive).
    ;; `edit` / `cwd` / `parent` / etc. remain banned.
    (expect (not-any? #{'edit 'cwd 'parent 'file-name 'extension 'relativize 'bash}
              (map :ext.symbol/symbol editing/editing-symbols)))
    (expect (not-any? #{'read-all-lines}
              (map :ext.symbol/symbol editing/editing-symbols)))
    (expect (some #{'patch} (map :ext.symbol/symbol editing/editing-symbols)))
    (expect (some #{'write} (map :ext.symbol/symbol editing/editing-symbols)))
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
      (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "bash-tool"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "bash-symbol"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "run-bash-safe"))))))

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

  (it "documents only the canonical root-scoped v/rg map shape"
    (expect (string/includes? editing/editing-prompt
              "(v/rg {:any [P] :files-only? true})"))
    (expect (string/includes? editing/editing-prompt "Do not assume `src`"))
    (expect (not (string/includes? editing/editing-prompt ":paths [\"src\"]")))
    (expect (not (string/includes? editing/editing-prompt "kwargs")))
    (expect (not (string/includes? editing/editing-prompt "BOTH"))))

  (it "registers observed fn-symbols with tool-specific renderers"
    (doseq [sym-name '[cat ls rg patch write create-dirs copy move delete delete-if-exists exists?]]
      (let [entry (some #(when (= sym-name (:ext.symbol/symbol %)) %)
                    editing/editing-symbols)]
        (expect (some? entry))
        (expect (fn? (:ext.symbol/render-fn entry)))))))

(it "defers op classification to the engine contract (no editing-local copy)"
  ;; The classification table + presentation map live in
  ;; `com.blockether.vis.internal.extension` (`op-tag`,
  ;; `op-presentation`). Editing used to keep a thin shim; that
  ;; shim is gone and callers go straight to the engine. Tags
  ;; collapsed to observation/mutation values; ops not in the
  ;; registration table fail closed instead of defaulting to observation.
  (doseq [[op tag] [[:v/cat         :observation]
                    [:z/locators    :observation]
                    [:v/rg          :observation]
                    [:v/patch       :mutation]
                    [:v/create-dirs :mutation]
                    [:v/delete      :mutation]
                    [:v/move        :mutation]]]
    (expect (= tag (extension/op-tag op)))
    (expect (= {:tag tag} (extension/op-presentation op))))
  (let [thrown (try (extension/op-tag :v/extensions)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (expect (= :extension/unregistered-op (:type (ex-data thrown))))))

(defn- protected-env
  [rules]
  {:extensions (atom [(extension/extension
                        {:ext/name "test.protected-paths"
                         :ext/description "Test protected paths."
                         :ext/protected-paths (constantly (vec rules))})])})

(defdescribe protected-path-before-fn-test
  (it "v/cat blocks :none protected paths and returns the extension hint"
    (let [hint "Use (br/policy) instead of reading this file directly."
          path "target/editing-test/protected/secret.edn"
          before (:ext.symbol/before-fn (private-fn "cat-symbol"))
          out (before (protected-env [{:glob "target/editing-test/protected/*.edn"
                                       :access :none
                                       :hint hint}])
                (constantly :ok)
                [path])
          failure (:result out)]
      (expect (some? failure))
      (expect (false? (:success? failure)))
      (expect (= :ext.foundation.editing/path-protected
                (-> failure :error :type)))
      (expect (= hint (-> failure :error :hint)))
      (expect (= hint (-> failure :error :loop-hint)))
      (expect (= :none (-> failure :error :failures first :access)))
      (expect (= :read (-> failure :error :failures first :intent)))))

  (it "v/patch blocks writes to :read-only protected paths and returns the extension hint"
    (let [hint "Use (br/update-policy!) instead of patching policy files."
          path "target/editing-test/protected/policy.txt"
          before (:ext.symbol/before-fn (private-fn "patch-symbol"))
          out (before (protected-env [{:glob "target/editing-test/protected/*.txt"
                                       :access :read-only
                                       :hint hint}])
                (constantly :ok)
                [[{:path path :search "old" :replace "new"}]])
          failure (:result out)]
      (expect (some? failure))
      (expect (false? (:success? failure)))
      (expect (= :ext.foundation.editing/path-protected
                (-> failure :error :type)))
      (expect (= hint (-> failure :error :hint)))
      (expect (= :read-only (-> failure :error :failures first :access)))
      (expect (= :write (-> failure :error :failures first :intent)))))

  (it "v/ls allows current directory even when descendants are protected"
    (let [before (:ext.symbol/before-fn (private-fn "ls-symbol"))
          out (before (protected-env [{:glob "target/editing-test/protected/*.edn"
                                       :access :none
                                       :hint "Use owner API."}])
                (constantly :ok)
                ["."])]
      (expect (not (contains? out :result)))
      (expect (= ["."] (:args out)))))

  (it "v/rg allows current directory even when descendants are protected (regression: .bridge/ blocks rg on `.`)"
    ;; Repro for transcript ccee2e1f-16ee-4acf-8d93-b4505034c0de iter 1:
    ;;   (v/rg {:any ["scrollbar"] :paths ["."] :counts? true})
    ;;   -> ERROR ":v/rg blocked: . is protected; use the owning extension API instead."
    ;; The bridge extension registers `.bridge/` with :access :none. Because
    ;; "." is an ancestor of every protected descendant, the composite-dir
    ;; branch in `protected-rule-matches?` reported a match and rg failed
    ;; closed even though it's a recursive read that can skip protected
    ;; subtrees during its own walk. The cwd-ancestor bypass must apply to
    ;; rg the same way it applies to ls.
    (let [before (:ext.symbol/before-fn (private-fn "rg-symbol"))
          out (before (protected-env [{:glob ".bridge/"
                                       :access :none
                                       :hint "Use (br/policy) instead."}])
                (constantly :ok)
                [{:any ["scrollbar"] :paths ["."] :counts? true}])]
      (expect (not (contains? out :result)))
      (expect (= [{:any ["scrollbar"] :paths ["."] :counts? true}] (:args out)))))

  (it "v/rg with no :paths (default `.`) is allowed when only descendants are protected"
    ;; rg-arg-paths returns ["."] when :paths is omitted; same bypass
    ;; must apply so model can call `(v/rg {:any ["x"]})` without paths.
    (let [before (:ext.symbol/before-fn (private-fn "rg-symbol"))
          out (before (protected-env [{:glob ".bridge/"
                                       :access :none
                                       :hint "Use (br/policy) instead."}])
                (constantly :ok)
                [{:any ["scrollbar"]}])]
      (expect (not (contains? out :result)))))

  (it "v/exists? on `.` is allowed when only descendants are protected"
    (let [before (:ext.symbol/before-fn (private-fn "exists?-symbol"))
          out (before (protected-env [{:glob ".bridge/"
                                       :access :none
                                       :hint "Use (br/policy) instead."}])
                (constantly :ok)
                ["."])]
      (expect (not (contains? out :result)))
      (expect (= ["."] (:args out)))))

  (it "v/rg still respects direct rules whose glob matches `.` itself"
    ;; The bypass is descendant-only. If an extension explicitly says
    ;; `:glob "." :access :none` (\"do not read cwd at all\") that's still
    ;; honored — we don't want the bypass to be a back door.
    (let [before (:ext.symbol/before-fn (private-fn "rg-symbol"))
          out (before (protected-env [{:glob "."
                                       :access :none
                                       :hint "cwd is sealed."}])
                (constantly :ok)
                [{:any ["x"] :paths ["."]}])
          failure (:result out)]
      (expect (some? failure))
      (expect (false? (:success? failure)))))

  (it "writes on `.` are still blocked even when only descendants are protected"
    ;; The bypass is INTENTIONALLY read-only — a recursive write on cwd
    ;; cannot filter protected descendants safely, so we keep failing
    ;; closed. (v/patch / v/write don't target cwd in practice, but
    ;; v/delete on \".\" must stay blocked.)
    (let [before (:ext.symbol/before-fn (private-fn "delete-symbol"))
          out (before (protected-env [{:glob ".bridge/"
                                       :access :none
                                       :hint "Use (br/policy) instead."}])
                (constantly :ok)
                ["."])
          failure (:result out)]
      (expect (some? failure))
      (expect (false? (:success? failure)))))

  (it "v/ls still blocks non-root ancestor directories that would reveal :none protected children"
    (let [hint "Use (br/files) instead of listing Bridge-owned files."
          before (:ext.symbol/before-fn (private-fn "ls-symbol"))
          out (before (protected-env [{:glob "target/editing-test/protected/*.edn"
                                       :access :none
                                       :hint hint}])
                (constantly :ok)
                ["target/editing-test"])
          failure (:result out)]
      (expect (some? failure))
      (expect (false? (:success? failure)))
      (expect (= :ext.foundation.editing/path-protected
                (-> failure :error :type)))
      (expect (= hint (-> failure :error :hint)))
      (expect (= :read (-> failure :error :failures first :intent)))))

  (it "allows first-match :read-write exceptions for the exact file without unblocking parents"
    (let [hint "Use (br/files) instead of listing Bridge-owned files."
          path "target/editing-test/protected/public.edn"
          rules [{:glob path
                  :access :read-write
                  :hint "Direct edits are allowed for this file."}
                 {:glob "target/editing-test/protected/*.edn"
                  :access :none
                  :hint hint}]
          patch-before (:ext.symbol/before-fn (private-fn "patch-symbol"))
          ls-before (:ext.symbol/before-fn (private-fn "ls-symbol"))
          patch-out (patch-before (protected-env rules)
                      (constantly :ok)
                      [[{:path path :search "old" :replace "new"}]])
          ls-out (ls-before (protected-env rules)
                   (constantly :ok)
                   ["target/editing-test/protected"])
          failure (:result ls-out)]
      (expect (not (contains? patch-out :result)))
      (expect (= [[{:path path :search "old" :replace "new"}]] (:args patch-out)))
      (expect (some? failure))
      (expect (false? (:success? failure)))
      (expect (= hint (-> failure :error :hint)))
      (expect (= :none (-> failure :error :failures first :access))))))

(defdescribe editing-prompt-read-policy-test
  (it "keeps the prompt small and canonical"
    (let [patch-symbol (some #(when (= 'patch (:ext.symbol/symbol %)) %)
                         editing/editing-symbols)
          prompt editing/editing-prompt]
      (expect (< (count prompt) 1600))
      (expect (string/includes? prompt "Canonical path only"))
      (expect (not (string/includes? prompt "v/strategy")))
      (expect (string/includes? prompt "(v/ls \".\" {:depth 2})"))
      (expect (string/includes? prompt "(v/rg {:any [P] :files-only? true})"))
      (expect (string/includes? prompt "400-500 line range"))
      (expect (string/includes? prompt "(v/patch [{:path P :search S :replace R}])"))
      (expect (string/includes? prompt "diff is evidence"))
      (expect (not (string/includes? prompt "kwargs")))
      (expect (not (string/includes? prompt "BOTH")))
      (expect (not (string/includes? prompt "STRATEGIES — decision table")))
      (expect (not (string/includes? prompt "Bulk rename idiom")))
      (expect (not (string/includes? prompt "Codex envelope")))
      (expect (not (string/includes? prompt "*** Begin Patch")))
      (expect (not (string/includes? (:ext.symbol/doc patch-symbol)
                     "Codex `apply_patch` envelope")))
      (expect (not (string/includes? prompt "read-all-lines")))
      (expect (not (string/includes? prompt "write-lines")))
      (expect (not (string/includes? prompt "update-file"))))))

(defdescribe vis-ls-flat-shape-test
  ;; v/ls now returns a FLAT entry list (no nested :children). The shape
  ;; mirrors what other harnesses surface so the model can `(filter ...)`
  ;; directly on (:entries r) instead of a tree-seq walk.
  (it "returns a flat entry list with workspace-relative paths"
    (let [list-files (private-fn "list-files")
          ls-tool    (private-fn "ls-tool")
          out        (list-files "." {:depth 1})
          result     (:result (ls-tool "." {:depth 1}))]
      (expect (= "." (:path out)))
      (expect (= (str (.toAbsolutePath (fs/path (fs/cwd))))
                (:absolute-path out)))
      (expect (= :v/ls (:vis.op result)))
      (expect (= (:absolute-path out) (:absolute-path result)))
      (expect (vector? (:entries out)))
      (expect (every? #(= #{:path :type :size} (set (keys %))) (:entries out)))
      (expect (every? #{:dir :file} (map :type (:entries out))))
      ;; Counts add up.
      (expect (= (count (:entries out))
                (+ (:file-count out) (:dir-count out))))))

  (it "recurses up to :depth (default 10) and reports entry counts"
    (let [list-files (private-fn "list-files")
          ;; target/probe/rg-corpus is a small known fixture
          path "target/probe/rg-corpus"
          out (list-files path)]
      (expect (pos? (:entry-count out)))
      (expect (pos? (:file-count out)))
      (expect (pos? (:dir-count out)))
      (expect (false? (:truncated? out)))
      (expect (= 10 (:depth out)))))

  (it ":files-only? excludes directory entries; :dirs-only? excludes files"
    (let [list-files (private-fn "list-files")
          path "target/probe/rg-corpus"
          files-only (list-files path {:files-only? true})
          dirs-only  (list-files path {:dirs-only?  true})]
      (expect (every? #(= :file (:type %)) (:entries files-only)))
      (expect (every? #(= :dir  (:type %)) (:entries dirs-only)))
      (expect (zero? (:dir-count files-only)))
      (expect (zero? (:file-count dirs-only)))))

  (it ":limit caps entries and surfaces :truncated? true"
    (let [list-files (private-fn "list-files")
          path "target/probe/rg-corpus"
          out (list-files path {:limit 2})]
      (expect (= 2 (count (:entries out))))
      (expect (true? (:truncated? out)))))

  (it ":files-only? and :dirs-only? are mutually exclusive"
    (let [list-files (private-fn "list-files")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(list-files "." {:files-only? true :dirs-only? true})))))

  (it ":depth 0 emits no entries (root not included)"
    (let [list-files (private-fn "list-files")
          out (list-files "target/probe/rg-corpus" {:depth 0})]
      (expect (= 0 (count (:entries out))))
      (expect (= 0 (:entry-count out))))))

(defn- numbered-tuples
  "[[start str0] [start+1 str1] …] helper for assembling expected
   `:lines` payloads in shape tests."
  [start xs]
  (mapv vector (iterate inc start) xs))

(defdescribe vis-cat-structured-shape-test
  (it "returns the paginated shape (small file, single window, eof) plus staleness metadata"
    (let [path (write-temp! "small.txt" "alpha\nbeta\ngamma\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      (expect (= #{:path :lines :next-offset :eof? :truncated? :mtime :size}
                (set (keys out))))
      (expect (string? (:path out)))
      (expect (nil? (:next-offset out)))
      (expect (true? (:eof? out)))
      (expect (false? (:truncated? out)))
      (expect (= (numbered-tuples 1 ["alpha" "beta" "gamma"]) (:lines out)))
      ;; staleness metadata mirrors File.lastModified / File.length and can
      ;; be threaded into a later v/patch as :expected-mtime / :expected-size.
      (expect (pos-int? (:mtime out)))
      (expect (= (.length (fs/file path)) (:size out)))))

  (it ":eof? false (with :next-offset) when window stops short of file end"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 21)))
          path (write-temp! "eof-false.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 1 3)]
      (expect (false? (:eof? out)))
      (expect (= 4 (:next-offset out)))))

  (it ":mtime / :size from v/cat round-trip into v/patch :expected-mtime guard"
    ;; This is the canonical staleness recipe: cat -> patch :expected-mtime
    ;; matches -> succeeds. If something rewrites the file in between, the
    ;; patch fails closed with :reason :stale.
    (let [path  (write-temp! "cat-stale.txt" "alpha\n")
          read-file (private-fn "read-file")
          patch (private-fn "patch-safe")
          first-read (read-file path)
          mtime0 (:mtime first-read)]
      ;; Same mtime -> patch goes through cleanly.
      (patch [{:path path :search "alpha" :replace "BETA" :expected-mtime mtime0}])
      (expect (= "BETA\n" (slurp path)))
      ;; Force-clock the file backwards so the next read sees a fresh mtime
      ;; distinct from `mtime0` regardless of filesystem millis precision.
      (.setLastModified (fs/file path) (- (long mtime0) 60000))
      (let [r (patch [{:path path :search "BETA" :replace "GAMMA"
                       :expected-mtime mtime0}])]
        (expect (false? (:success? r)))
        (expect (= :stale (-> r :failures first :reason)))
        (expect (= "BETA\n" (slurp path))))))

  (it ":lines tuples carry raw strings - no embedded line-number prefix in the text"
    (let [path (write-temp! "raw.txt" "   indented\nplain\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      (expect (= [[1 "   indented"] [2 "plain"]] (:lines out)))))

  (it "(v/cat path n) reads first n lines and sets :next-offset when more remain"
    (let [body (string/join "\n" (map #(str "line-" %) (range 1 11)))
          path (write-temp! "ten.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 4)]
      (expect (= 5 (:next-offset out)))
      (expect (false? (:truncated? out)))
      (expect (= (numbered-tuples 1 ["line-1" "line-2" "line-3" "line-4"]) (:lines out)))))

  (it "(v/cat path offset n) reads a mid-file window and advances :next-offset"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 21)))
          path (write-temp! "twenty.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 7 3)]
      (expect (= 10 (:next-offset out)))
      (expect (= (numbered-tuples 7 ["L7" "L8" "L9"]) (:lines out)))
      (expect (false? (:truncated? out)))))

  (it "paging via :next-offset reaches eof cleanly"
    (let [body (string/join "\n" (map #(str "line-" %) (range 1 11)))
          path (write-temp! "page.txt" (str body "\n"))
          read-file (private-fn "read-file")
          page-1 (read-file path 1 4)
          page-2 (read-file path (:next-offset page-1) 4)
          page-3 (read-file path (:next-offset page-2) 4)]
      (expect (= (numbered-tuples 1 ["line-1" "line-2" "line-3" "line-4"]) (:lines page-1)))
      (expect (= (numbered-tuples 5 ["line-5" "line-6" "line-7" "line-8"]) (:lines page-2)))
      (expect (= (numbered-tuples 9 ["line-9" "line-10"]) (:lines page-3)))
      (expect (nil? (:next-offset page-3)))))

  (it "offset past EOF returns an empty window and no :next-offset"
    (let [path (write-temp! "two.txt" "a\nb\n")
          read-file (private-fn "read-file")
          out  (read-file path 99 10)]
      (expect (= [] (:lines out)))
      (expect (nil? (:next-offset out)))
      (expect (false? (:truncated? out)))))

  (it ":truncated? true when a window would exceed max-cat-window-bytes"
    ;; Window byte cap is 256KB. Use 200 lines of ~1500 chars so the
    ;; byte-cap fires on cumulative volume. First line always included
    ;; for forward progress (even a single pathological 1MB line gets
    ;; emitted whole — the per-line cap was retired; see source note).
    (let [chunky (apply str (repeat 1500 "x"))
          body (string/join "\n" (repeat 200 chunky))
          path (write-temp! "huge.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 1 500)]
      (expect (true? (:truncated? out)))
      (expect (pos? (count (:lines out))))
      (expect (< (count (:lines out)) 200))
      (expect (some? (:next-offset out)))))

  (it "persistence-blob contract: :lines bytes are bounded by max-cat-window-bytes"
    ;; This is the storage claim: a single v/cat call cannot persist
    ;; more than max-cat-window-bytes of line bytes regardless of file size.
    (let [line (apply str (repeat 200 "x"))
          body (string/join "\n" (repeat 5000 line))
          path (write-temp! "persist.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 1 100000)
          line-bytes (reduce + 0 (map (fn [[_ ^String s]]
                                        (inc (count (.getBytes s "UTF-8"))))
                                   (:lines out)))]
      ;; 256KB window cap (bumped from 64KB).
      (expect (<= line-bytes (* 256 1024)))))

  (it "rejects bad positional args (non-positive ints, non-int types)"
    (let [path (write-temp! "validate.txt" "x\n")
          read-file (private-fn "read-file")]
      (doseq [bad [[0 10]
                   [-1 10]
                   [1 0]
                   [1 -5]
                   ["a" 10]
                   [1 :hi]]]
        (expect (throws? clojure.lang.ExceptionInfo
                  #(apply read-file path bad)))))))

(defdescribe vis-cat-tail-shape-test
  (it "(v/cat path :tail n) reads the last n lines and reports correct line numbers"
    (let [body (string/join "\n" (map #(str "line-" %) (range 1 21)))
          path (write-temp! "tail.txt" (str body "\n"))
          tail-file (private-fn "tail-file")
          out  (tail-file path 5)]
      (expect (nil? (:next-offset out)))
      (expect (false? (:truncated? out)))
      (expect (= (numbered-tuples 16 ["line-16" "line-17" "line-18" "line-19" "line-20"])
                (:lines out)))))

  (it "tail of a file shorter than n returns the whole file with :eof? true"
    (let [path (write-temp! "short.txt" "alpha\nbeta\n")
          tail-file (private-fn "tail-file")
          out  (tail-file path 50)]
      (expect (= [[1 "alpha"] [2 "beta"]] (:lines out)))
      (expect (nil? (:next-offset out)))
      (expect (true? (:eof? out)))
      (expect (pos-int? (:mtime out)))
      (expect (pos-int? (:size out)))))

  (it ":truncated? true when byte cap drops older lines from the tail window"
    ;; Same trick as the read-file byte-cap test: use 200 × 1500-char
    ;; lines so cumulative volume blows the 256KB window cap, not the
    ;; per-line 2000-char cap. Most-recent line is the LAST one included.
    (let [chunky (apply str (repeat 1500 "x"))
          body (string/join "\n" (repeat 200 chunky))
          path (write-temp! "htail.txt" (str body "\n"))
          tail-file (private-fn "tail-file")
          out  (tail-file path 500)]
      (expect (true? (:truncated? out)))
      (expect (pos? (count (:lines out))))
      (expect (< (count (:lines out)) 200))
      ;; Last kept line should be line 200 (most-recent wins on tail).
      (expect (= 200 (first (peek (:lines out))))))))

(defdescribe vis-cat-tool-arities-test
  (it "(v/cat path :tail) defaults to default-cat-limit (2000) lines from the end"
    ;; Bumped from 400 → 2000 for industry parity with Claude Code / Roo Code.
    ;; Use a file with >2000 lines so the tail default actually clamps.
    (let [body (string/join "\n" (map #(str "L" %) (range 1 2401)))
          path (write-temp! "big-tail.txt" (str body "\n"))
          cat-tool (private-fn "cat-tool")
          out (-> (cat-tool path :tail) :result)]
      (expect (= 2000 (count (:lines out))))
      (expect (= 401 (ffirst (:lines out))))
      (expect (= 2400 (first (peek (:lines out)))))
      (expect (nil? (:next-offset out)))))

  (it "(v/cat path :tail n) honours an explicit count"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 21)))
          path (write-temp! "explicit-tail.txt" (str body "\n"))
          cat-tool (private-fn "cat-tool")
          out (-> (cat-tool path :tail 3) :result)]
      (expect (= (numbered-tuples 18 ["L18" "L19" "L20"]) (:lines out))))))

(defdescribe vis-cat-range-arity-test
  ;; G1 from the v/cat probe (C9): the offset+count arity feels awkward
  ;; when the model already knows both endpoints ("convert end=100 to
  ;; n=51 mentally"). The :range arity takes inclusive start..end.
  (it "(v/cat path :range start end) reads inclusive 1-based start..end"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 21)))
          path (write-temp! "range/inclusive.txt" (str body "\n"))
          cat-tool (private-fn "cat-tool")
          out (-> (cat-tool path :range 5 10) :result)]
      ;; 5..10 inclusive = 6 lines (L5, L6, L7, L8, L9, L10).
      (expect (= 6 (count (:lines out))))
      (expect (= [[5 "L5"] [6 "L6"] [7 "L7"] [8 "L8"] [9 "L9"] [10 "L10"]]
                (:lines out)))))

  (it ":range with start == end reads exactly one line"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 11)))
          path (write-temp! "range/single.txt" (str body "\n"))
          cat-tool (private-fn "cat-tool")
          out (-> (cat-tool path :range 7 7) :result)]
      (expect (= [[7 "L7"]] (:lines out)))))

  (it ":range rejects start > end, non-positive ints, and the wrong kw"
    (let [path (write-temp! "range/invalid.txt" "a\nb\nc\n")
          cat-tool (private-fn "cat-tool")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :range 10 5)))
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :range 0 5)))
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :range -1 5)))
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :not-range 1 5))))))

(defdescribe vis-cat-line-passthrough-test
  ;; Regression: an earlier `max-line-length` (2000) cap rewrote every
  ;; long line into `…<+N chars truncated>` and surfaced a
  ;; `:long-line-truncations` count. Same failure pattern as the rg /
  ;; trailer caps removed alongside (see ctx_renderer.clj header note
  ;; + conversation ccee2e1f-16ee-4acf-8d93-b4505034c0de). The structural
  ;; defense is the 256KB per-window byte cap: a pathological single
  ;; line is included whole, the model sees real data, and the next
  ;; window stops with `:truncated? true :next-offset N`.
  (it "long lines pass through verbatim (no per-line cap, no `…<+N chars truncated>` marker)"
    (let [long-line (apply str (repeat 5000 "x"))
          path (write-temp! "long-line.txt" (str long-line "\nshort line\n"))
          read-file (private-fn "read-file")
          out (read-file path)
          [_ first-text] (first (:lines out))]
      (expect (= long-line first-text))
      (expect (= 5000 (count first-text)))
      (expect (not (string/includes? first-text "…<+")))
      (expect (not (string/includes? first-text "chars truncated")))
      (expect (= [2 "short line"] (nth (:lines out) 1)))
      (expect (not (contains? out :long-line-truncations)))))

  (it ":long-line-truncations key is gone from the result map shape entirely"
    (let [path (write-temp! "short-lines.txt" "a\nb\nc\n")
          read-file (private-fn "read-file")
          out (read-file path)]
      (expect (not (contains? out :long-line-truncations))))))

(defn- cat-result
  "Construct the plain-map shape `cat-tool` produces, for renderer-contract
   tests. Tuples for `:lines`, no `:offset` / `:eof?` / `:truncated-by`."
  [path lines next-offset truncated?]
  {:vis.op :v/cat
   :path path
   :lines (vec lines)
   :next-offset next-offset
   :truncated? truncated?})

(defdescribe channel-renderer-contract-test
  (it "v/cat channel renderer returns canonical [:ir ...] with a :code block, line-numbered from the first tuple"
    (let [channel-render-cat (private-fn "channel-render-cat")
          r   (cat-result "src/demo.clj" [[1 "only-line"]] nil false)
          out (channel-render-cat r)]
      (expect (vector? out))
      (expect (= :ir (first out)))
      (let [form-sources (filter #(and (vector? %) (= :code (first %))) (tree-seq sequential? seq out))
            body (last (first form-sources))]
        (expect (= 1 (count form-sources)))
        (expect (string/includes? body "1: only-line")))))

  (it "v/cat channel renderer wraps a CAT badge around the header"
    (let [channel-render-cat (private-fn "channel-render-cat")
          r   (cat-result "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj"
                [[1898 "x"]] 1928 false)
          out (channel-render-cat r)
          paragraph (nth out 2)
          text (apply str (filter string? (tree-seq sequential? seq paragraph)))
          has-strong-cat? (some #(and (vector? %)
                                   (= :strong (first %))
                                   (= "CAT" (last %)))
                            (tree-seq sequential? seq paragraph))]
      (expect has-strong-cat?)
      (expect (string/includes? text "render.clj"))
      (expect (string/includes? text "1 line"))
      (expect (string/includes? text "from=1898"))
      (expect (string/includes? text "next-offset=1928"))))

  (it "v/cat channel renderer uses the absolute line number on each tuple"
    (let [channel-render-cat (private-fn "channel-render-cat")
          r   (cat-result "f.txt" [[100 "hundred"] [101 "hundred-one"]] 102 false)
          out (channel-render-cat r)
          body (last (first (filter #(and (vector? %) (= :code (first %)))
                              (tree-seq sequential? seq out))))]
      (expect (= :ir (first out)))
      (expect (string/includes? body "100: hundred"))
      (expect (string/includes? body "101: hundred-one")))))

(defdescribe error-formatter-contract-test
  (it "engine-default channel error formatter renders failures as canonical [:ir ...]"
    (let [out (extension/default-error-ir
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
          ;; rg-tool now returns a plain map as :result; :hits IS the
          ;; grep payload — no protocol indirection.
          rg-result (:result (rg spec))]
      (expect (= :v/rg (:vis.op rg-result)))
      (expect (= (:hits (grep spec)) (:hits rg-result)))))

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
        (expect (clojure.string/includes? (ex-message err) "single spec map or inline kwargs")))
      ;; :limit, :regex?, :files-only?, :counts? are NOW valid keys.
      (doseq [[k v] [[(keyword "type") :clj]
                     [(keyword "mode") :any]]]
        (expect (throws? clojure.lang.ExceptionInfo
                  #(grep (bad-spec k v)))))))

  (it ":truncated-by :limit when results exceed the configured limit (default 250)"
    ;; Limit bumped 50 -> 250 in the v/rg sweep. Use 300 hits to force the cap.
    (let [_ (write-temp! "rgcap/a.txt"
              (string/join "\n" (map #(str "needle " %) (range 300))))
          grep (private-fn "grep-files")
          out  (grep {:all ["needle"] :paths [(temp-dir-path "rgcap")]})]
      (expect (= 250 (count (:hits out))))
      (expect (= :limit (:truncated-by out)))))

  (it ":limit override caps results below default"
    (let [_ (write-temp! "rglim/a.txt"
              (string/join "\n" (map #(str "needle " %) (range 50))))
          grep (private-fn "grep-files")
          out  (grep {:all ["needle"] :paths [(temp-dir-path "rglim")] :limit 5})]
      (expect (= 5 (count (:hits out))))
      (expect (= :limit (:truncated-by out)))))

  (it "empty result still has :truncated-by :end-of-results, never nil"
    (let [_ (write-temp! "rgmiss/a.txt" "nothing matches in here\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["definitely-not-present"] :paths [(temp-dir-path "rgmiss")]})]
      (expect (= [] (:hits out)))
      (expect (= :end-of-results (:truncated-by out)))))

  ;; Q1+Q2+Q3+Q4 — new option coverage.

  (it ":before / :after add context lines around each hit"
    (let [_path (write-temp! "rgctx/file.txt"
                  "alpha\nbeta\nGAMMA\ndelta\nepsilon\nfoo\nGAMMA\nbar\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["GAMMA"]
                      :paths [(temp-dir-path "rgctx")]
                      :before 1 :after 1})
          hits (:hits out)]
      (expect (= 2 (count hits)))
      ;; First GAMMA: line 3. before should hold beta(2), after should hold delta(4).
      (let [h1 (first hits)]
        (expect (= [[2 "beta"]]  (:before h1)))
        (expect (= [[4 "delta"]] (:after  h1))))
      ;; Second GAMMA: line 7. before foo(6), after bar(8).
      (let [h2 (second hits)]
        (expect (= [[6 "foo"]] (:before h2)))
        (expect (= [[8 "bar"]] (:after  h2))))))

  (it ":context N is shorthand for :before N + :after N"
    (let [_path (write-temp! "rgctxa/a.txt" "L1\nL2\nMATCH\nL4\nL5\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["MATCH"]
                      :paths [(temp-dir-path "rgctxa")]
                      :context 2})
          h (first (:hits out))]
      (expect (= [[1 "L1"] [2 "L2"]] (:before h)))
      (expect (= [[4 "L4"] [5 "L5"]] (:after  h)))))

  (it ":files-only? returns distinct paths and never line-level hits"
    (let [_ (write-temp! "rgfo/src/a.py" "alpha\nalpha\nalpha\n")
          _ (write-temp! "rgfo/src/b.py" "alpha\n")
          _ (write-temp! "rgfo/src/c.py" "no match\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["alpha"]
                      :paths [(temp-dir-path "rgfo")]
                      :files-only? true})]
      (expect (= #{:files :truncated-by} (set (keys out))))
      (expect (= 2 (count (:files out))))
      (expect (every? string? (:files out)))))

  (it ":counts? returns per-file match counts (real, not hit-cap-truncated)"
    (let [_ (write-temp! "rgcnt/src/a.py"
              (string/join "\n" (repeat 300 "needle")))
          _ (write-temp! "rgcnt/src/b.py" "needle\nneedle\n")
          _ (write-temp! "rgcnt/src/c.py" "no\n")
          grep (private-fn "grep-files")
          out  (grep {:all ["needle"]
                      :paths [(temp-dir-path "rgcnt")]
                      :counts? true})
          a-count (some (fn [{:keys [path count]}]
                          (when (string/ends-with? path "a.py") count))
                    (:counts out))]
      (expect (= #{:counts :truncated-by} (set (keys out))))
      ;; Real per-file count is reported, NOT capped at 250 default.
      (expect (= 300 a-count))))

  (it ":regex? true treats needles as java.util.regex patterns"
    (let [_ (write-temp! "rgrgx/src/a.py"
              "def login(user): pass\ndef test_login(): pass\ndef logout(): pass\n")
          grep (private-fn "grep-files")
          ;; Word-boundary regex: matches `login` but not `test_login`.
          out  (grep {:any ["\\bdef login\\b"]
                      :paths [(temp-dir-path "rgrgx")]
                      :regex? true})
          texts (mapv :text (:hits out))]
      (expect (= 1 (count texts)))
      (expect (= "def login(user): pass" (first texts)))))

  (it ":regex? with malformed pattern raises an invalid-rg-spec error"
    (let [grep (private-fn "grep-files")
          err (try (grep {:any ["(unclosed"]
                          :paths ["."]
                          :regex? true})
                nil (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (= :ext.foundation.editing/invalid-rg-spec (:type (ex-data err))))))

  (it ":files-only? and :counts? are mutually exclusive"
    (let [grep (private-fn "grep-files")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep {:any ["x"] :files-only? true :counts? true})))))

  (it ":before / :after rejected in :files-only? or :counts? mode"
    (let [grep (private-fn "grep-files")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep {:any ["x"] :files-only? true :context 2})))
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep {:any ["x"] :counts? true :before 1})))))

  (it "long lines pass through verbatim in hit :text (no per-line cap, no `…<+N chars>` marker)"
    ;; Regression: an earlier `rg-line-preview-chars` (500 chars) cap
    ;; mutilated every long hit line into `…<+N chars>`. That marker
    ;; reproduced the same failure mode as the trailer cap removed in
    ;; ccee2e1f-16ee-4acf-8d93-b4505034c0de — the model perceived its
    ;; own search data as missing and chased phantom v/cat roundtrips
    ;; even on normal source lines that brushed the cap. The model
    ;; owns its data; no silent renderer-side ellipsis.
    (let [huge (apply str (repeat 1000 "x"))
          line (str "NEEDLE " huge)
          _ (write-temp! "rgtext/big.txt" (str line "\n"))
          grep (private-fn "grep-files")
          out  (grep {:all ["NEEDLE"] :paths [(temp-dir-path "rgtext")]})
          text (:text (first (:hits out)))]
      (expect (= line text))
      (expect (= (count line) (count text)))
      (expect (not (string/includes? text "…<+"))))))

(defdescribe thin-bbfs-wrapper-test
  ;; patch-safe and patch-envelope-safe both return a STRUCTURED MAP and
  ;; never throw on "normal" failure paths (no-match / anchor-not-found
  ;; / stale / file-not-found / path-escape / nth-out-of-range / etc.).
  ;; Throws are reserved for genuinely unexpected programming errors
  ;; (invalid coercion, blank :search, malformed :nth value).
  (it "patch replaces the first occurrence by default; no global uniqueness requirement"
    ;; New semantics (industry parity with Aider/Codex/Roo): a bare
    ;; {:search :replace} edit matches the FIRST occurrence. Models no
    ;; longer have to expand :search until it is globally unique — the
    ;; old behaviour drove the dreaded "matched 2 times" patch loop.
    (let [path  (write-temp! "bbfs/patch.txt" "alpha\nbeta\ngamma\n")
          patch (private-fn "patch-safe")
          ok    (patch [{:path path :search "beta" :replace "BETA"}])]
      (expect (true? (:success? ok)))
      (expect (= [{:path path
                   :before "alpha\nbeta\ngamma\n"
                   :after "alpha\nBETA\ngamma\n"}]
                (:plans ok)))
      (expect (= "alpha\nBETA\ngamma\n" (slurp path)))
      (let [r (patch [{:path path :search "missing" :replace "x"}])]
        (expect (false? (:success? r)))
        (expect (= 0 (-> r :failures first :matches)))
        (expect (= :no-match (-> r :failures first :reason))))
      ;; Duplicate matches now resolve to the first occurrence by default.
      (spit path "dup\ndup\n")
      (let [r (patch [{:path path :search "dup" :replace "x"}])]
        (expect (true? (:success? r)))
        (expect (= [{:path path :before "dup\ndup\n" :after "x\ndup\n"}]
                  (:plans r))))
      (expect (= "x\ndup\n" (slurp path)))))

  (it ":nth selects which occurrence to replace (:first | :last | :all | 1-based int)"
    (let [patch (private-fn "patch-safe")
          read! (fn [content]
                  (write-temp! "bbfs/patch-nth.txt" content))]
      ;; :first (default) hits the very first match
      (let [p (read! "a\na\na\n")]
        (patch [{:path p :search "a" :replace "X" :nth :first}])
        (expect (= "X\na\na\n" (slurp p))))
      ;; :last hits the final match
      (let [p (read! "a\na\na\n")]
        (patch [{:path p :search "a" :replace "X" :nth :last}])
        (expect (= "a\na\nX\n" (slurp p))))
      ;; :all replaces every occurrence
      (let [p (read! "a\na\na\n")]
        (patch [{:path p :search "a" :replace "X" :nth :all}])
        (expect (= "X\nX\nX\n" (slurp p))))
      ;; Positive 1-based integer addresses a specific occurrence
      (let [p (read! "a\na\na\n")]
        (patch [{:path p :search "a" :replace "X" :nth 2}])
        (expect (= "a\nX\na\n" (slurp p))))
      ;; Out-of-range :nth surfaces :nth-out-of-range and writes nothing
      (let [p (read! "a\na\n")
            r (patch [{:path p :search "a" :replace "X" :nth 5}])]
        (expect (false? (:success? r)))
        (expect (= :nth-out-of-range (-> r :failures first :reason)))
        (expect (= "a\na\n" (slurp p))))))

  (it ":after / :before anchors restrict which occurrences are eligible"
    (let [patch (private-fn "patch-safe")
          path  (write-temp! "bbfs/patch-anchors.txt"
                  "def foo():\n  pass\ndef bar():\n  pass\ndef baz():\n  pass\n")]
      ;; :after picks the FIRST `pass` after `def bar():`
      (patch [{:path path :search "  pass" :replace "  return 1"
               :after "def bar():\n"}])
      (expect (string/includes? (slurp path) "def bar():\n  return 1\ndef baz()"))
      ;; :before constrains to occurrences ending before an anchor
      (let [p2 (write-temp! "bbfs/patch-anchor-before.txt" "x\ny\nx\nz\n")]
        (patch [{:path p2 :search "x" :replace "X" :before "z"}])
        ;; The :before anchor allows BOTH x's (both end before "z"); :first wins.
        (expect (= "X\ny\nx\nz\n" (slurp p2))))
      ;; Missing anchor fails the edit cleanly
      (let [p3 (write-temp! "bbfs/patch-anchor-missing.txt" "x\n")
            r (patch [{:path p3 :search "x" :replace "X" :after "NOT_HERE"}])]
        (expect (false? (:success? r)))
        (expect (= :anchor-not-found (-> r :failures first :reason)))
        (expect (= "x\n" (slurp p3))))))

  (it "fuzzy fallback (line-based) recovers from whitespace and unicode drift"
    (let [patch (private-fn "patch-safe")]
      ;; rstrip pass: file has trailing whitespace on one line, SEARCH does not.
      (let [p (write-temp! "bbfs/patch-rstrip.txt" "def foo():   \n    return 1\n")]
        (patch [{:path p :search "def foo():\n    return 1" :replace "def foo():\n    return 2"}])
        (expect (string/includes? (slurp p) "return 2")))
      ;; trim pass: SEARCH authored with different leading indent.
      (let [p (write-temp! "bbfs/patch-trim.txt" "  def foo():\n    return 1\n")]
        (patch [{:path p :search "def foo():\n    return 1" :replace "def foo():\n    return 9"}])
        (expect (string/includes? (slurp p) "return 9")))
      ;; unicode pass: smart quote in file, ASCII apostrophe in SEARCH.
      (let [p (write-temp! "bbfs/patch-unicode.txt" "it’s late\nover\n")]
        (patch [{:path p :search "it's late\nover" :replace "it's done\nover"}])
        (expect (string/includes? (slurp p) "it's done")))))

  (it "relative-indent fuzzy pass re-indents the :replace payload to match the file"
    (let [patch (private-fn "patch-safe")
          ;; File has 4-space indent; SEARCH/REPLACE authored at 0-space.
          p (write-temp! "bbfs/patch-relindent.txt"
              "    def foo():\n        return 1\n        return 2\n")]
      (patch [{:path p
               :search "def foo():\n    return 1\n    return 2"
               :replace "def foo():\n    return 10\n    return 20"}])
      (expect (= "    def foo():\n        return 10\n        return 20\n"
                (slurp p)))))

  (it ":expected-mtime guards against editing a file that changed since it was read"
    (let [patch (private-fn "patch-safe")
          p (write-temp! "bbfs/patch-stale.txt" "alpha\n")
          stale-mtime (- (.lastModified (fs/file p)) 100000)
          r (patch [{:path p :search "alpha" :replace "BETA"
                     :expected-mtime stale-mtime}])]
      (expect (false? (:success? r)))
      (expect (= :stale (-> r :failures first :reason)))
      (expect (= "alpha\n" (slurp p)))))

  (it "unknown edit keys are rejected (typo guard)"
    (let [patch (private-fn "patch-safe")
          p (write-temp! "bbfs/patch-unknown.txt" "x\n")
          err (try (patch [{:path p :search "x" :replace "y" :occurence 1}])
                nil (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (string/includes? (ex-message err) "unknown keys"))))

  (it "loop detector: after N consecutive failures on a path, the message carries a hard hint"
    ;; Hits the per-path failure counter. Threshold is private but the
    ;; behaviour is observable on the structured result map.
    (let [patch (private-fn "patch-safe")
          clear (private-fn "clear-patch-fail-count!")
          p (write-temp! "bbfs/patch-loop.txt" "alpha\n")
          file (fs/file p)
          run! (fn [] (patch [{:path p :search "NOT_HERE" :replace "x"}]))]
      (clear file)
      (run!)
      (run!)
      (let [r (run!)]
        (expect (false? (:success? r)))
        (expect (some? (:loop-hint r)))
        (expect (string/includes? (:message r) "Consecutive v/patch failures"))
        (expect (= 3 (-> r :failures first :consecutive-failures))))
      (clear file)))

  (it "successful patch on a path clears the loop counter"
    (let [patch (private-fn "patch-safe")
          clear (private-fn "clear-patch-fail-count!")
          p (write-temp! "bbfs/patch-clear.txt" "alpha\n")
          file (fs/file p)]
      (clear file)
      (patch [{:path p :search "NOT_HERE" :replace "x"}])
      (patch [{:path p :search "alpha" :replace "BETA"}])
      (let [counts2 @(deref (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "patch-fail-counts")))]
        (expect (nil? (get counts2 (.getAbsolutePath file)))))))

  (it "all-or-nothing: a single failing edit aborts every prior edit in the batch"
    ;; This guards the core safety invariant. Earlier edits that
    ;; "would have" succeeded against the in-memory plan must NOT
    ;; touch disk when any later edit fails.
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-aon.txt" "alpha\nbeta\n")
          r     (patch [{:path p :search "alpha" :replace "ALPHA"}
                        {:path p :search "NEVER_MATCHES" :replace "x"}])]
      (expect (false? (:success? r)))
      (expect (= "alpha\nbeta\n" (slurp p)))))

  (it "sequential edits on the same file operate against the post-edit state"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-seq.txt" "alpha\nbeta\n")
          r     (patch [{:path p :search "alpha" :replace "first"}
                        {:path p :search "first" :replace "FIRST"}])]
      (expect (true? (:success? r)))
      (expect (= "FIRST\nbeta\n" (slurp p)))
      ;; Both edits collapse into one per-file plan (one before/after pair).
      (expect (= 1 (count (:plans r))))
      (expect (= "alpha\nbeta\n" (-> r :plans first :before)))
      (expect (= "FIRST\nbeta\n" (-> r :plans first :after)))))

  (it "long :search is bounded in the previewed failure trailer"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-bigpreview.txt" "a\n")
          long-search (apply str (repeat 4000 "x"))
          r     (patch [{:path p :search long-search :replace "y"}])
          preview (-> r :failures first :search-preview)]
      (expect (false? (:success? r)))
      ;; Preview must NOT carry the full 4000-char :search into the trailer.
      (expect (< (count preview) 250))
      (expect (string/includes? preview "...<+"))))

  (it "blank :search is rejected with a structured error (would otherwise match everywhere)"
    ;; Still a throw — :blank-search is a coercion-time programming
    ;; error, not a normal failure path.
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-blank.txt" "alpha\n")
          err   (try (patch [{:path p :search "" :replace "x"}])
                  nil (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (= :ext.foundation.editing/invalid-patch-search
                (-> err ex-data :type)))))

  (it "invalid :nth value (negative / wrong type) is rejected at coercion"
    ;; Still a throw — coercion-time programming error.
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-bad-nth.txt" "a\n")
          err   (try (patch [{:path p :search "a" :replace "X" :nth -1}])
                  nil (catch clojure.lang.ExceptionInfo e e))
          err2  (try (patch [{:path p :search "a" :replace "X" :nth :weird}])
                  nil (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (some? err2))
      (expect (string/includes? (ex-message err)  ":nth must be"))
      (expect (string/includes? (ex-message err2) ":nth must be"))
      (expect (= "a\n" (slurp p)))))

  (it "editing an unknown path surfaces a structured :file-not-found failure"
    (let [patch (private-fn "patch-safe")
          fake-path "target/editing-test/bbfs/does-not-exist.txt"
          r (patch [{:path fake-path :search "x" :replace "y"}])]
      (expect (false? (:success? r)))
      (expect (= :file-not-found (-> r :failures first :reason)))))

  (it ":expected-size guards independent of :expected-mtime"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-size.txt" "hello\n")
          r     (patch [{:path p :search "hello" :replace "x"
                         :expected-size 1}])]
      (expect (false? (:success? r)))
      (expect (= :stale (-> r :failures first :reason)))
      (expect (= :stale-size (-> r :failures first :stale :reason)))
      (expect (= "hello\n" (slurp p)))))

  (it "failed fuzzy search includes a :nearest candidate with context window"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-nearest.txt"
                  "line A\nline B\nline C\nline D\nline E\n")
          ;; multi-line search that does NOT match exactly (or fuzzily)
          r     (patch [{:path p
                         :search "COMPLETELY MISSING\nALSO MISSING"
                         :replace "x"}])
          failure (-> r :failures first)]
      (expect (false? (:success? r)))
      ;; :nearest is best-effort — the bare minimum is that the failure
      ;; reason is observable and writes are zero.
      (expect (#{:no-match} (:reason failure)))
      (expect (= "line A\nline B\nline C\nline D\nline E\n" (slurp p)))))

  (it "empty edit vector is a no-op success (no failures, no writes)"
    (let [patch (private-fn "patch-safe")
          r (patch [])]
      (expect (true? (:success? r)))
      (expect (= [] (:plans r)))))

  (it ":nth :all replaces every occurrence in a single edit"
    (let [patch (private-fn "patch-safe")
          p (write-temp! "bbfs/nth-all.txt"
              "foo 1\nbar foo 2\nfoo 3\n")]
      (patch [{:path p :search "foo" :replace "BAZ" :nth :all}])
      (expect (= "BAZ 1\nbar BAZ 2\nBAZ 3\n" (slurp p)))))

  (it ":replace may legally contain :search (no recursive blow-up)"
    ;; Edge: `:replace` contains the same substring we just removed.
    ;; With multi-edit chaining the next edit operates on POST-state,
    ;; so the second `foo → foo + xtra` will re-match if not careful.
    (let [patch (private-fn "patch-safe")
          p (write-temp! "bbfs/replace-contains-search.txt" "foo bar\n")]
      (patch [{:path p :search "foo" :replace "foo xtra"}])
      (expect (= "foo xtra bar\n" (slurp p)))))

  (it "a single v/patch invocation cannot move the loop counter past +1 per path"
    ;; Loop counter must be PER INVOCATION, not per failed edit. Two
    ;; failed edits in one call against the same path bump the counter
    ;; once, not twice.
    (let [patch (private-fn "patch-safe")
          clear (private-fn "clear-patch-fail-count!")
          p (write-temp! "bbfs/loop-once.txt" "alpha\n")
          file (fs/file p)]
      (clear file)
      (patch [{:path p :search "NOPE1" :replace "x"}
              {:path p :search "NOPE2" :replace "y"}])
      (let [r (patch [{:path p :search "NOPE3" :replace "z"}])]
        ;; Failures came from two invocations -> counter is 2.
        (expect (= 2 (-> r :failures first :consecutive-failures)))
        (expect (nil? (:loop-hint r))))
      (clear file)))

  (it "v/patch reports :exact-replace as its only mode (envelope retired)"
    (let [patch-tool (private-fn "patch-tool")
          p (write-temp! "bbfs/dispatch-mode.txt" "alpha\nbeta\n")
          vec-out (-> (patch-tool [{:path p :search "alpha" :replace "X"}])
                    :metadata :mode)]
      (expect (= :exact-replace vec-out))
      (expect (= "X\nbeta\n" (slurp p)))))

  (it "patch diagnostics report per-edit reasons, all match counts, bounded previews, and write nothing"
    (let [path  (write-temp! "bbfs/patch-diagnostics.txt" "alpha\nbeta\nbeta\n")
          patch (private-fn "patch-safe")
          long-search (apply str (repeat 80 "missing "))
          r     (patch [{:path path :search "alpha" :replace "ALPHA"}
                        {:path path :search "beta" :replace "BETA"}
                        {:path path :search long-search :replace "x"}
                        {:path path :search "other missing" :replace "y"}])
          checks (:checks r)
          failures (:failures r)]
      (expect (false? (:success? r)))
      ;; New semantics: first 2 edits succeed (beta hits first occurrence), 2 fail.
      (expect (= [0 1 2 3] (mapv :edit-index checks)))
      (expect (= [1 2 0 0] (mapv :matches checks)))
      (expect (= [2 3] (mapv :edit-index failures)))
      (expect (= [0 0] (mapv :matches failures)))
      (expect (every? #{:no-match} (map :reason failures)))
      (expect (every? #(<= (count (:search-preview %)) 200) failures))
      ;; All-or-nothing still holds: zero writes when any edit fails.
      (expect (= "alpha\nbeta\nbeta\n" (slurp path)))))

  (it "exists? and delete-if-exists work on cwd-relative paths"
    (let [path             (write-temp! "bbfs/meta/x.txt" "x")
          exists?          (private-fn "exists-safe?")
          delete-if-exists (private-fn "delete-if-exists-safe")]
      (expect (true? (exists? path)))
      (expect (true? (delete-if-exists path)))
      (expect (false? (exists? path)))))

  (it "v/exists? tool returns the canonical {:vis.op :v/exists? :path :exists?} map (envelope-shape consistency)"
    ;; Regression for conversation 11d4f817-fbd1-43ab-a6b4-052c8557af0a
    ;; turn 4 iter 1→2:
    ;;   model wrote `(def ports (v/exists? \".nrepl-port\"))` then
    ;;   `(:exists? ports)`, expecting the map shape every other `v/*`
    ;;   tool returns. The old `exists-tool` returned a bare boolean
    ;;   so `(:exists? true)` evaluated to `nil` and the model burned
    ;;   an iter realizing \"v/exists? is returning true directly rather
    ;;   than a map containing an :exists? key\".
    ;; Fix: every v/* tool returns a map keyed on :vis.op for shape
    ;; consistency; bare booleans are no longer a supported result.
    (let [present-path (write-temp! "exists-shape/yes.txt" "x")
          missing-path "exists-shape/no.txt"
          exists-tool  (private-fn "exists-tool")
          present (:result (exists-tool present-path))
          missing (:result (exists-tool missing-path))]
      (expect (map? present))
      (expect (= :v/exists? (:vis.op present)))
      (expect (true? (:exists? present)))
      (expect (= present-path (:path present)))
      (expect (map? missing))
      (expect (= :v/exists? (:vis.op missing)))
      (expect (false? (:exists? missing)))
      (expect (= missing-path (:path missing)))))

  (it "keeps v/exists? shape details out of the compact prompt and in symbol docs"
    (let [exists-symbol (some #(when (= 'exists? (:ext.symbol/symbol %)) %)
                          editing/editing-symbols)]
      (expect (not (string/includes? editing/editing-prompt ":vis.op :v/exists?")))
      (expect (string/includes? (:ext.symbol/doc exists-symbol) ":vis.op :v/exists?"))
      (expect (string/includes? (:ext.symbol/doc exists-symbol) ":exists?"))))

  (it "bash helpers fully removed from the editing core"
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "run-bash-safe"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "bash-tool"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "strict-bash-command"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "coerce-bash-opts"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "bash-warnings"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "channel-render-bash"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.ext.foundation-core.editing.core" "journal-render-bash"))))))

(defdescribe delete-tool-shape-test
  ;; Regression: `delete-tool` used to set `:result nil`. The channel
  ;; preview then painted `DELETE nil` and `(def r (v/delete p))`
  ;; consumers couldn't read `(:path r)` (same parity bug `v/exists?`
  ;; already fixed). All `v/*` tools now return a map shape.
  (it "v/delete returns {:vis.op :v/delete :path P :deleted? true} (no bare nil)"
    (let [delete-tool (private-fn "delete-tool")
          p           (write-temp! "delete-shape/x.txt" "goodbye\n")
          envelope    (delete-tool p)]
      (expect (true? (:success? envelope)))
      (expect (= :v/delete (:symbol envelope)))
      (let [r (:result envelope)]
        (expect (map? r))
        (expect (= :v/delete (:vis.op r)))
        (expect (= p (:path r)))
        (expect (true? (:deleted? r))))))

  (it "v/delete-if-exists returns the same map shape with :deleted? reflecting the actual outcome"
    (let [delete-if (private-fn "delete-if-exists-tool")
          p         (write-temp! "delete-shape/here.txt" "x\n")
          present   (:result (delete-if p))
          absent    (:result (delete-if p))]
      ;; First call deletes the file; the result map carries :deleted? true.
      (expect (map? present))
      (expect (= p (:path present)))
      (expect (true? (:deleted? present)))
      ;; Second call hits an already-absent path; the map stays the same shape.
      (expect (map? absent))
      (expect (= p (:path absent)))
      (expect (false? (:deleted? absent))))))

(defdescribe mutation-tool-renderer-shapes-test
  ;; Every mutation channel renderer destructures the canonical
  ;; `v/*` map shape directly. No nil tolerance, no string fallback,
  ;; no bare-boolean compatibility — if a tool changes its shape it
  ;; surfaces here, not at paint time.
  (it "MKDIR renderer reads :path off the result map"
    (let [ir ((private-fn "channel-render-create-dirs")
              {:vis.op :v/create-dirs :path "target/x" :created? true})]
      (expect (string/includes? (pr-str ir) "target/x"))
      (expect (string/includes? (pr-str ir) "MKDIR"))))

  (it "COPY renderer reads :src/:dest off the result map"
    (let [ir ((private-fn "channel-render-copy")
              {:vis.op :v/copy :src "a.txt" :dest "b.txt" :path "b.txt"})
          s  (pr-str ir)]
      (expect (string/includes? s "COPY"))
      (expect (string/includes? s "a.txt"))
      (expect (string/includes? s "b.txt"))))

  (it "MOVE renderer reads :src/:dest off the result map"
    (let [ir ((private-fn "channel-render-move")
              {:vis.op :v/move :src "a.txt" :dest "b.txt" :path "b.txt"})
          s  (pr-str ir)]
      (expect (string/includes? s "MOVE"))
      (expect (string/includes? s "a.txt"))
      (expect (string/includes? s "b.txt"))))

  (it "DELETE renderer reads :path off the result map (NO nil token in output)"
    (let [ir ((private-fn "channel-render-delete")
              {:vis.op :v/delete :path "src/foo.txt" :deleted? true})
          s  (pr-str ir)]
      (expect (string/includes? s "src/foo.txt"))
      (expect (string/includes? s "DELETE"))
      (expect (not (string/includes? s "nil")))))

  (it "DELETE-IF-EXISTS renderer flips badge between DELETE / ABSENT on :deleted?"
    (let [render (private-fn "channel-render-delete-if-exists")
          gone   (render {:vis.op :v/delete-if-exists :path "a.txt" :deleted? true})
          absent (render {:vis.op :v/delete-if-exists :path "a.txt" :deleted? false})]
      (expect (string/includes? (pr-str gone)   "DELETE"))
      (expect (string/includes? (pr-str absent) "ABSENT"))))

  (it "EXISTS? renderer flips badge between EXISTS / MISSING on :exists?"
    (let [render  (private-fn "channel-render-exists?")
          present (render {:vis.op :v/exists? :path "a.txt" :exists? true})
          absent  (render {:vis.op :v/exists? :path "a.txt" :exists? false})]
      (expect (string/includes? (pr-str present) "EXISTS"))
      (expect (string/includes? (pr-str absent)  "MISSING")))))

(defdescribe patch-summary-shape-test
  ;; The summary IS what the model reads back as the v/patch result
  ;; AND what the channel renderer projects. Every key counts; redundant
  ;; signal pollutes the iteration trailer.
  (it "byte-exact match: no :passes key, no :indent-delta, no line counters"
    (let [patch (private-fn "patch-safe")
          summary (private-fn "patch-result-file-summary")
          p (write-temp! "summary/exact.txt" "alpha\nbeta\n")
          r (patch [{:path p :search "alpha" :replace "ALPHA"}])
          s (summary (first (:plans r)))]
      (expect (true? (:success? r)))
      (expect (= #{:path :op :changed? :diff} (set (keys s))))
      (expect (not (contains? s :passes)))
      (expect (not (contains? s :indent-delta)))
      (expect (not (contains? s :lines-before)))
      (expect (not (contains? s :lines-after)))
      (expect (not (contains? s :delta-lines)))))

  (it "fuzzy :rstrip pass surfaces as :passes [:rstrip], in edit order"
    (let [patch (private-fn "patch-safe")
          summary (private-fn "patch-result-file-summary")
          ;; File has trailing whitespace the SEARCH lacks → :rstrip pass
          p (write-temp! "summary/rstrip.txt" "def hi():   \n    return 1\n")
          r (patch [{:path p :search "def hi():\n    return 1" :replace "def hi():\n    return 2"}])
          plan (first (:plans r))
          s (summary plan)]
      (expect (true? (:success? r)))
      (expect (= [:rstrip] (:passes s)))
      (expect (not (contains? s :indent-delta)))))

  (it "fuzzy :relative-indent surfaces both :passes AND :indent-delta"
    (let [patch (private-fn "patch-safe")
          summary (private-fn "patch-result-file-summary")
          ;; File at 4-space indent, SEARCH authored at 0-space
          p (write-temp! "summary/relindent.txt"
              "    def f():\n        return 1\n        return 2\n")
          r (patch [{:path p
                     :search "def f():\n    return 1\n    return 2"
                     :replace "def f():\n    return 10\n    return 20"}])
          s (summary (first (:plans r)))]
      (expect (true? (:success? r)))
      (expect (or (= [:relative-indent] (:passes s))
                ;; :trim may catch first depending on pass ordering; either
                ;; way :indent-delta should be present so the model knows
                ;; Vis auto-shifted the replace payload.
                (= [:trim] (:passes s))))
      (expect (= 4 (:indent-delta s)))))

  (it "passes for several non-exact edits on the same path appear in edit order"
    ;; Fuzzy only fires for multi-line searches, so both edits below are
    ;; multi-line. First edit hits :rstrip (file has trailing space the
    ;; SEARCH lacks). Second hits :unicode (file has ’ smart quote, SEARCH
    ;; uses ASCII '). The result projects both passes in edit order.
    (let [patch (private-fn "patch-safe")
          summary (private-fn "patch-result-file-summary")
          p (write-temp! "summary/mixed.txt"
              "def alpha():   \n    return 1\nit’s late\nfor now\n")
          r (patch [{:path p :search "def alpha():\n    return 1" :replace "def alpha():\n    return 2"}
                    {:path p :search "it's late\nfor now" :replace "it's done\nfor real"}])
          s (summary (first (:plans r)))]
      (expect (true? (:success? r)))
      (expect (= [:rstrip :unicode] (:passes s)))))

  (it "successful v/patch with byte-exact match carries no :passes / :indent-delta"
    ;; This used to be tested against envelope mode; envelope is retired.
    ;; Byte-exact single-edit success on exact-replace still must omit the
    ;; fuzzy alarm keys.
    (let [patch-tool (private-fn "patch-tool")
          p (write-temp! "summary/byte-exact.txt" "line1\nline2\n")
          out (patch-tool [{:path p :search "line1" :replace "LINE1"}])
          first-file (first (:result out))]
      (expect (true? (:success? out)))
      (expect (not (contains? first-file :passes)))
      (expect (not (contains? first-file :indent-delta))))))

(defdescribe editing-renderer-guidance-test
  (it "patch renderer wraps a PATCH badge around the header and lists paths"
    (let [render-patch (private-fn "channel-render-patch")
          rendered (render-patch [{:path "target/editing-test/out.txt"}])
          text-leaves (filter string? (tree-seq sequential? seq rendered))
          joined (string/join " " text-leaves)
          has-strong-patch? (some #(and (vector? %)
                                     (= :strong (first %))
                                     (= "PATCH" (last %)))
                              (tree-seq sequential? seq rendered))]
      (expect (vector? rendered))
      (expect (= :ir (first rendered)))
      (expect has-strong-patch?)
      (expect (string/includes? joined "target/editing-test/out.txt"))))

  (it "patch renderer surfaces :passes as a fuzzy alarm in the header"
    (let [render-patch (private-fn "channel-render-patch")
          rendered (render-patch [{:path "src/foo.py"
                                   :op :update
                                   :changed? true
                                   :diff ""
                                   :passes [:rstrip :trim]}])
          joined (string/join " " (filter string? (tree-seq sequential? seq rendered)))]
      (expect (string/includes? joined "[fuzzy: rstrip,trim]"))))

  (it "patch renderer surfaces :indent-delta when relative-indent fired"
    (let [render-patch (private-fn "channel-render-patch")
          rendered (render-patch [{:path "src/foo.py"
                                   :op :update
                                   :changed? true
                                   :diff ""
                                   :passes [:relative-indent]
                                   :indent-delta 4}])
          joined (string/join " " (filter string? (tree-seq sequential? seq rendered)))]
      (expect (string/includes? joined "[indentΔ +4]"))))

  (it "patch renderer says nothing about fuzzy when only exact matches fired"
    (let [render-patch (private-fn "channel-render-patch")
          rendered (render-patch [{:path "src/foo.py"
                                   :op :update
                                   :changed? true
                                   :diff ""}])
          joined (string/join " " (filter string? (tree-seq sequential? seq rendered)))]
      (expect (not (string/includes? joined "fuzzy")))
      (expect (not (string/includes? joined "indentΔ")))))
  (it "patch diff stays compact for large files"
    (let [diff-fn (private-fn "unified-diff-text")
          before  (string/join "\n" (map #(str "line-" %) (range 1500)))
          after   (string/replace before "line-750" "LINE-750")
          out     (diff-fn before after)
          lines   (string/split-lines out)]
      (expect (< (count lines) 50))
      (expect (string/includes? out "@@"))
      (expect (string/includes? out "-line-750"))
      (expect (string/includes? out "+LINE-750"))))
  (it "patch diff handles insert, delete, and all-different cases as bounded previews"
    (let [diff-fn (private-fn "unified-diff-text")
          inserted (diff-fn "a\nb\nc" "a\nX\nb\nc")
          deleted  (diff-fn "a\nb\nc" "a\nb")
          before   (string/join "\n" (map #(str "line-" %) (range 300)))
          after    (string/join "\n" (map #(str "other-" %) (range 300)))
          changed  (diff-fn before after)]
      (expect (string/includes? inserted "+X"))
      (expect (not (string/includes? inserted "-a")))
      (expect (string/includes? deleted "-c"))
      (expect (< (count (string/split-lines changed)) 260))
      (expect (string/includes? changed "diff truncated"))))
  (it "search-hits renderer formats partial-projection hits without raw EDN fallback"
    (let [render-hits (private-fn "channel-render-rg")
          rg-result {:vis.op :v/rg
                     :hit-count 2
                     :truncated-by :end-of-results
                     :hits [{:line 462
                             :text "  (inc (reduce max 0 ...))"}
                            {:line 472
                             :text "(defn- workspace-tabs-or-base"}]}
          rendered (render-hits rg-result)
          text-leaves (filter string? (tree-seq sequential? seq rendered))
          joined (string/join "\n" text-leaves)]
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
          rg-result {:vis.op :v/rg
                     :hit-count 1
                     :truncated-by :end-of-results
                     :hits [{:path "src/foo.clj"
                             :line 10
                             :text "(def x 1)"}]}
          rendered (render-hits rg-result)
          text-leaves (filter string? (tree-seq sequential? seq rendered))
          joined (string/join " " text-leaves)]
      (expect (string/includes? joined "src/foo.clj:10"))
      (expect (string/includes? joined "(def x 1)")))))

(defdescribe tool-envelope-test
  (it "tool wrappers return the required contract keys"
    (let [path (write-temp! "contract/read.txt" "alpha\nbeta\n")
          cat-tool (private-fn "cat-tool")
          out (cat-tool path)
          required #{:success? :result :error :symbol :tag :metadata}]
      ;; Envelope keys MUST include the canonical op/* set; extra keys
      ;; (e.g. :presentation) may also appear.
      (expect (= required (clojure.set/intersection required (set (keys out)))))
      (expect (true? (:success? out)))
      ;; v/cat returns a plain map as :result. :lines is a vec of
      ;; `[line-number text]` tuples; no deref, no handle, no offset key.
      (let [r (:result out)]
        (expect (= :v/cat (:vis.op r)))
        (expect (= [[1 "alpha"] [2 "beta"]] (:lines r)))
        (expect (nil? (:next-offset r)))
        (expect (false? (:truncated? r))))
      (expect (not (contains? out :markdown)))
      (expect (nil? (:error out)))))

  (it "tool failure envelope carries structured :error"
    (let [cat-symbol (private-fn "cat-symbol")
          on-error   (:ext.symbol/on-error-fn cat-symbol)
          out        (:result (on-error (ex-info "boom" {}) nil nil ["missing.txt"]))]
      (expect (false? (:success? out)))
      (expect (nil? (:result out)))
      ;; :trace is a preformatted string; first line carries the
      ;; underlying class name.
      (expect (string? (get-in out [:error :trace])))
      (expect (string/includes? (get-in out [:error :trace]) "ExceptionInfo"))
      (expect (not (contains? out :markdown))))))
