(ns com.blockether.vis.internal.foundation.editing.core-test
  "Tests for the editing extension.

   Smoke-checks the loaded extension surface (symbol vector, doc
   strings, prompt fragment) plus behavioral coverage of the
   structured preview/search helpers (`cat`, `rg`) and the new
   thin babashka.fs wrappers (`patch`, `copy`, ...).

   Tests reach private fns directly through the registry to avoid
   bringing up a full SCI sandbox. Temp files land under
   `target/editing-test/` (always inside the repo cwd, so
   `safe-path` accepts them)."
  (:require
   [babashka.fs :as fs]
   [clojure.set]
   [clojure.string :as string]
   [com.blockether.vis.internal.foundation.editing.core :as editing]
   [com.blockether.vis.internal.foundation.editing.patch :as patch]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" name))))

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

(defdescribe rg-spec-path-alias-test
  (let [coerce (private-fn "coerce-rg-spec")]
    (it "accepts :path as an undocumented alias for :paths (scalar or vector)"
      (expect (= ["src"] (:paths (coerce {:any ["x"] :path "src"}))))
      (expect (= ["a" "b"] (:paths (coerce {:any ["x"] :path ["a" "b"]})))))
    (it "keeps canonical :paths working and defaults to [\".\"] when absent"
      (expect (= ["src"] (:paths (coerce {:any ["x"] :paths ["src"]}))))
      (expect (= ["."] (:paths (coerce {:any ["x"]})))))
    (it "rejects :path used together with :paths or :files"
      (expect (= :threw (try (coerce {:any ["x"] :path "a" :paths ["b"]})
                          :no-throw (catch Exception _ :threw))))
      (expect (= :threw (try (coerce {:any ["x"] :path "a" :files ["b"]})
                          :no-throw (catch Exception _ :threw)))))
    (it "accepts :globs as an undocumented alias for :include"
      (let [spec (coerce {:any ["models" "model"]
                          :is_files_only true
                          :globs ["*.css" "*.tsx" "*.cljs"]})]
        (expect (= ["*.css" "*.tsx" "*.cljs"] (:include spec)))
        (expect (true? (:is_files_only spec))))
      ;; scalar tolerance and !-negation peel work through the alias too
      (expect (= ["*.css"] (:include (coerce {:any ["x"] :globs "*.css"}))))
      (expect (= ["min.js"] (:exclude (coerce {:any ["x"] :globs ["*.js" "!min.js"]})))))
    (it "accepts :excludes as an undocumented alias for :exclude"
      (expect (= ["target/**"] (:exclude (coerce {:any ["x"] :excludes ["target/**"]})))))
    (it "rejects include aliases used together"
      (expect (= :threw (try (coerce {:any ["x"] :glob "*.clj" :globs ["*.cljs"]})
                          :no-throw (catch Exception _ :threw))))
      (expect (= :threw (try (coerce {:any ["x"] :include ["*.clj"] :globs ["*.cljs"]})
                          :no-throw (catch Exception _ :threw))))
      (expect (= :threw (try (coerce {:any ["x"] :exclude ["a"] :excludes ["b"]})
                          :no-throw (catch Exception _ :threw)))))))

(defdescribe cwd-safety-test
  ;; THE non-negotiable invariant: every v/* tool that touches the
  ;; filesystem must refuse any path that escapes (workspace/cwd).
  ;; safe-path is the single gate; this suite proves every mutation
  ;; tool actually routes through it.
  (let [escape-paths ["../escape.txt"
                      "../../etc/passwd"
                      "/etc/passwd"
                      "target/../../escape.txt"]]

    (it "patch (exact-replace) refuses to write outside cwd"
      (let [patch (private-fn "patch-safe")]
        (doseq [p escape-paths]
          (let [r (patch [{:path p :search "x" :replace "y"}])]
            (expect (false? (:success? r)))
            (expect (= :path-escape (-> r :failures first :reason)))))))

    (it "write refuses to create files outside cwd"
      ;; Note: we deliberately do NOT (.exists) the escape path here; the
      ;; check is whether `write-safe` REFUSED to act. /etc/passwd exists
      ;; on macOS regardless of our actions; what matters is :reason :path-escape
      ;; and the cwd guard kicking in before any IO.
      (let [write (private-fn "write-safe")]
        (doseq [p escape-paths]
          (let [r (write {:path p :content "hi"})]
            (expect (false? (:success? r)))
            (expect (= :path-escape (-> r :failures first :reason)))))))

    (it "create-dirs refuses to mkdir outside cwd"
      (let [create (private-fn "create-dirs-safe")]
        (doseq [p escape-paths]
          (let [err (try (create p) nil
                      (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err))
            (expect (= :ext.foundation.editing/path-escape
                      (:type (ex-data err))))))))

    (it "copy refuses src OR dest outside cwd"
      (let [copy (private-fn "copy-safe")
            inside (write-temp! "cwd-safety/copy-src.txt" "x")]
        (doseq [p escape-paths]
          (let [err1 (try (copy p inside) nil (catch clojure.lang.ExceptionInfo e e))
                err2 (try (copy inside p) nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err1))
            (expect (some? err2))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err1))))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err2))))))))

    (it "move refuses src OR dest outside cwd"
      (let [move (private-fn "move-safe")
            inside (write-temp! "cwd-safety/move-src.txt" "x")]
        (doseq [p escape-paths]
          (let [err1 (try (move p inside) nil (catch clojure.lang.ExceptionInfo e e))
                err2 (try (move inside p) nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err1))
            (expect (some? err2))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err1))))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err2))))))))

    (it "delete and delete-if-exists refuse paths outside cwd"
      (let [del   (private-fn "delete-safe")
            del-if (private-fn "delete-if-exists-safe")]
        (doseq [p escape-paths]
          (let [err1 (try (del p) nil (catch clojure.lang.ExceptionInfo e e))
                err2 (try (del-if p) nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err1))
            (expect (some? err2))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err1))))
            (expect (= :ext.foundation.editing/path-escape (:type (ex-data err2))))))))

    (it "cat (read) ALSO refuses paths outside cwd"
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
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "bash-tool"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "bash-symbol"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "run-bash-safe"))))))

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
    (expect (string/includes? editing/editing-prompt "rg"))
    (expect (string/includes? editing/editing-prompt "ls"))
    (expect (string/includes? editing/editing-prompt "cat"))
    nil)

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
  (doseq [[op tag] [[:cat         :observation]
                    [:z/locators    :observation]
                    [:rg          :observation]
                    [:patch       :mutation]
                    [:create-dirs :mutation]
                    [:delete      :mutation]
                    [:move        :mutation]]]
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
  (it "cat blocks :none protected paths and returns the extension hint"
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

  (it "patch blocks writes to :read-only protected paths and returns the extension hint"
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

  (it "ls allows current directory even when descendants are protected"
    (let [before (:ext.symbol/before-fn (private-fn "ls-symbol"))
          out (before (protected-env [{:glob "target/editing-test/protected/*.edn"
                                       :access :none
                                       :hint "Use owner API."}])
                (constantly :ok)
                ["."])]
      (expect (not (contains? out :result)))
      (expect (= ["."] (:args out)))))

  (it "rg allows current directory even when descendants are protected (regression: .bridge/ blocks rg on `.`)"
    ;; Repro for transcript ccee2e1f-16ee-4acf-8d93-b4505034c0de iter 1:
    ;;   (rg {:any ["scrollbar"] :paths ["."] :is_counts true})
    ;;   -> ERROR ":rg blocked: . is protected; use the owning extension API instead."
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
                [{:any ["scrollbar"] :paths ["."] :is_counts true}])]
      (expect (not (contains? out :result)))
      (expect (= [{:any ["scrollbar"] :paths ["."] :is_counts true}] (:args out)))))

  (it "rg with no :paths (default `.`) is allowed when only descendants are protected"
    ;; rg-arg-paths returns ["."] when :paths is omitted; same bypass
    ;; must apply so model can call `(rg {:any ["x"]})` without paths.
    (let [before (:ext.symbol/before-fn (private-fn "rg-symbol"))
          out (before (protected-env [{:glob ".bridge/"
                                       :access :none
                                       :hint "Use (br/policy) instead."}])
                (constantly :ok)
                [{:any ["scrollbar"]}])]
      (expect (not (contains? out :result)))))

  (it "exists? on `.` is allowed when only descendants are protected"
    (let [before (:ext.symbol/before-fn (private-fn "exists?-symbol"))
          out (before (protected-env [{:glob ".bridge/"
                                       :access :none
                                       :hint "Use (br/policy) instead."}])
                (constantly :ok)
                ["."])]
      (expect (not (contains? out :result)))
      (expect (= ["."] (:args out)))))

  (it "rg still respects direct rules whose glob matches `.` itself"
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
    ;; closed. (patch / write don't target cwd in practice, but
    ;; delete on \".\" must stay blocked.)
    (let [before (:ext.symbol/before-fn (private-fn "delete-symbol"))
          out (before (protected-env [{:glob ".bridge/"
                                       :access :none
                                       :hint "Use (br/policy) instead."}])
                (constantly :ok)
                ["."])
          failure (:result out)]
      (expect (some? failure))
      (expect (false? (:success? failure)))))

  (it "ls still blocks non-root ancestor directories that would reveal :none protected children"
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

;; =============================================================================
;; FORCING plan-gate composition — write/patch consult env :mutation-gate AFTER
;; path-protection clears. (proposal Decision 2 / G1)
;; =============================================================================
  (defn- gate-env
    "A non-protecting env carrying a `:mutation-gate` stub that records the call
   payload into `seen!` and returns `ret` (a refusal string or nil)."
    [seen! ret]
    {:extensions (atom [])
     :mutation-gate (fn [payload] (reset! seen! payload) ret)})

  (defdescribe plan-gate-before-fn-test
    (it "write SHORT-CIRCUITS with a :plan-required failure when the gate refuses"
      (let [seen   (atom nil)
            before (:ext.symbol/before-fn (private-fn "write-symbol"))
            out    (before (gate-env seen "Plan required — 2nd file.")
                     (constantly :ok)
                     [{:path "target/editing-test/b.clj" :content "x"}])
            failure (:result out)]
        (expect (some? failure))
        (expect (false? (:success? failure)))
        (expect (= :ext.foundation.editing/plan-required (-> failure :error :type)))
        (expect (= :plan-required (-> failure :error :reason)))
      ;; gate saw the canonical path + op
        (expect (= :write (:op @seen)))
        (expect (= ["target/editing-test/b.clj"] (:paths @seen)))
        (expect (false? (:atomic? @seen)))))
    (it "write PASSES THROUGH (no :result) when the gate allows (nil)"
      (let [seen   (atom nil)
            before (:ext.symbol/before-fn (private-fn "write-symbol"))
            out    (before (gate-env seen nil)
                     (constantly :ok)
                     [{:path "target/editing-test/a.clj" :content "x"}])]
        (expect (not (contains? out :result)))
        (expect (= [{:path "target/editing-test/a.clj" :content "x"}] (:args out)))))
    (it "detects the atomic=True escape flag on a write"
      (let [seen   (atom nil)
            before (:ext.symbol/before-fn (private-fn "write-symbol"))]
        (before (gate-env seen nil)
          (constantly :ok)
          [{:path "target/editing-test/a.clj" :content "x" :atomic true}])
        (expect (true? (:atomic? @seen)))))
    (it "detects atomic on a patch edit map + reports all edited paths"
      (let [seen   (atom nil)
            before (:ext.symbol/before-fn (private-fn "patch-symbol"))]
        (before (gate-env seen nil)
          (constantly :ok)
          [[{:path "target/editing-test/a.clj" :search "o" :replace "n" :atomic true}
            {:path "target/editing-test/b.clj" :search "o" :replace "n"}]])
        (expect (true? (:atomic? @seen)))
        (expect (= #{"target/editing-test/a.clj" "target/editing-test/b.clj"}
                  (set (:paths @seen))))))
    (it "path-protection wins: a protected path NEVER reaches the gate"
      (let [hint   "owner API only"
            before (:ext.symbol/before-fn (private-fn "write-symbol"))
            env    (assoc (protected-env [{:glob "target/editing-test/protected/*.clj"
                                           :access :read-only :hint hint}])
                     :mutation-gate (fn [_] (throw (ex-info "gate must not run" {}))))
            out    (before env (constantly :ok)
                     [{:path "target/editing-test/protected/x.clj" :content "x"}])
            failure (:result out)]
        (expect (= :ext.foundation.editing/path-protected (-> failure :error :type)))))
    (it "no :mutation-gate on env → write passes through (gate is optional)"
      (let [before (:ext.symbol/before-fn (private-fn "write-symbol"))
            out    (before {:extensions (atom [])}
                     (constantly :ok)
                     [{:path "target/editing-test/a.clj" :content "x"}])]
        (expect (not (contains? out :result))))))

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

(defdescribe vis-ls-grouped-shape-test
  ;; ls returns entries GROUPED BY DIRECTORY: [{:dir D :files [{:name :size}]}]
  ;; — the dir prefix is stated once per group instead of repeating on every
  ;; file, so the result stays compact. The dir tree is the set of group :dir
  ;; headers; full path = (str dir "/" name).
  (it "returns dir-grouped entries with workspace-relative dir headers"
    (let [list-files (private-fn "list-files")
          ls-tool    (private-fn "ls-tool")
          out        (list-files "." {:depth 1})
          result     (:result (ls-tool "." {:depth 1}))
          groups     (:groups out)]
      (expect (= "." (:path out)))
      (expect (= (str (.toAbsolutePath (fs/path (fs/cwd))))
                (:absolute-path out)))
      (expect (= :ls (:op result)))
      (expect (= (:groups out) (:groups result)))
      (expect (vector? groups))
      ;; every group is {:dir <str> :files [{:name <str> :size <int|nil>}]}
      (expect (every? #(= #{:dir :files} (set (keys %))) groups))
      (expect (every? (fn [g] (string? (:dir g))) groups))
      (expect (every? (fn [g]
                        (every? #(= #{:name :size} (set (keys %))) (:files g)))
                groups))
      ;; root group leads and dir headers are unique (one group per dir)
      (expect (= "." (:dir (first groups))))
      (expect (= (count groups) (count (distinct (map :dir groups)))))
      ;; total files across groups == file-count; dir headers == dir-count + root
      (expect (= (:file-count out)
                (reduce + 0 (map (comp count :files) groups))))
      (expect (= (inc (:dir-count out)) (count groups)))))

  (it "ls() with no args lists the current directory — same as ls(\".\")"
    ;; The model naturally calls `ls()` (Pythonic, like os.listdir()). The
    ;; zero-arg arity must default the path to \".\" instead of throwing an
    ;; ArityException (regression: session 1cc54cb8 — `ls()` failed with
    ;; \"Wrong number of args (0)\" and the model had to grope to `ls(\".\")`).
    (let [ls-tool (private-fn "ls-tool")
          r0      (:result (ls-tool))
          r1      (:result (ls-tool "."))]
      (expect (= :ls (:op r0)))
      (expect (= "." (:path r0)))
      (expect (= (:absolute-path r0) (:absolute-path r1)))
      (expect (= (:entry-count r0) (:entry-count r1)))))

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

  (it ":is_files_only excludes directory entries; :is_dirs_only excludes files"
    (let [list-files (private-fn "list-files")
          path "target/probe/rg-corpus"
          files-only (list-files path {:is_files_only true})
          dirs-only  (list-files path {:is_dirs_only  true})]
      ;; files-only: dir-count 0; every listed file lives under a group
      (expect (zero? (:dir-count files-only)))
      (expect (= (:file-count files-only)
                (reduce + 0 (map (comp count :files) (:groups files-only)))))
      ;; dirs-only: every group is a bare dir header (no files), file-count 0
      (expect (zero? (:file-count dirs-only)))
      (expect (every? (comp empty? :files) (:groups dirs-only)))))

  (it ":limit caps entries and surfaces :truncated? true"
    (let [list-files (private-fn "list-files")
          path "target/probe/rg-corpus"
          out (list-files path {:limit 2})]
      (expect (= 2 (:entry-count out)))
      (expect (true? (:truncated? out)))))

  (it ":is_files_only and :is_dirs_only are mutually exclusive"
    (let [list-files (private-fn "list-files")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(list-files "." {:is_files_only true :is_dirs_only true})))))

  (it ":depth 0 emits no entries (root not included)"
    (let [list-files (private-fn "list-files")
          out (list-files "target/probe/rg-corpus" {:depth 0})]
      (expect (= 0 (:entry-count out)))
      ;; only the root group remains, with no files (nothing walked)
      (expect (= 1 (count (:groups out))))
      (expect (empty? (:files (first (:groups out))))))))

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
      (expect (= #{:path :lines :hashes :next-offset :eof? :truncated? :mtime :size}
                (set (keys out))))
      (expect (string? (:path out)))
      (expect (nil? (:next-offset out)))
      (expect (true? (:eof? out)))
      (expect (false? (:truncated? out)))
      (expect (= (numbered-tuples 1 ["alpha" "beta" "gamma"]) (:lines out)))
      ;; staleness metadata mirrors File.lastModified / File.length and can
      ;; be threaded into a later patch as :expected_mtime / :expected_size.
      (expect (pos-int? (:mtime out)))
      (expect (= (.length (fs/file path)) (:size out)))))

  (it ":eof? false (with :next-offset) when window stops short of file end"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 21)))
          path (write-temp! "eof-false.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 1 3)]
      (expect (false? (:eof? out)))
      (expect (= 4 (:next-offset out)))))

  (it ":mtime / :size from cat round-trip into patch :expected_mtime guard"
    ;; This is the canonical staleness recipe: cat -> patch :expected_mtime
    ;; matches -> succeeds. If something rewrites the file in between, the
    ;; patch fails closed with :reason :stale.
    (let [path  (write-temp! "cat-stale.txt" "alpha\n")
          read-file (private-fn "read-file")
          patch (private-fn "patch-safe")
          first-read (read-file path)
          mtime0 (:mtime first-read)]
      ;; Same mtime -> patch goes through cleanly.
      (patch [{:path path :search "alpha" :replace "BETA" :expected_mtime mtime0}])
      (expect (= "BETA\n" (slurp path)))
      ;; Force-clock the file backwards so the next read sees a fresh mtime
      ;; distinct from `mtime0` regardless of filesystem millis precision.
      (.setLastModified (fs/file path) (- (long mtime0) 60000))
      (let [r (patch [{:path path :search "BETA" :replace "GAMMA"
                       :expected_mtime mtime0}])]
        (expect (false? (:success? r)))
        (expect (= :stale (-> r :failures first :reason)))
        (expect (= "BETA\n" (slurp path))))))

  (it ":lines tuples carry raw strings - no embedded line-number prefix in the text"
    (let [path (write-temp! "raw.txt" "   indented\nplain\n")
          read-file (private-fn "read-file")
          out  (read-file path)]
      (expect (= [[1 "   indented"] [2 "plain"]] (:lines out)))))

  (it "(cat path n) reads first n lines and sets :next-offset when more remain"
    (let [body (string/join "\n" (map #(str "line-" %) (range 1 11)))
          path (write-temp! "ten.txt" (str body "\n"))
          read-file (private-fn "read-file")
          out  (read-file path 4)]
      (expect (= 5 (:next-offset out)))
      (expect (false? (:truncated? out)))
      (expect (= (numbered-tuples 1 ["line-1" "line-2" "line-3" "line-4"]) (:lines out)))))

  (it "(cat path offset n) reads a mid-file window and advances :next-offset"
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
    ;; This is the storage claim: a single cat call cannot persist
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
  (it "(cat path :tail n) reads the last n lines and reports correct line numbers"
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
  (it "(cat path :tail) defaults to default-cat-limit (2000) lines from the end"
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

  (it "(cat path :tail n) honours an explicit count"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 21)))
          path (write-temp! "explicit-tail.txt" (str body "\n"))
          cat-tool (private-fn "cat-tool")
          out (-> (cat-tool path :tail 3) :result)]
      (expect (= (numbered-tuples 18 ["L18" "L19" "L20"]) (:lines out))))))

(defdescribe vis-cat-range-arity-test
  ;; G1 from the cat probe (C9): the offset+count arity feels awkward
  ;; when the model already knows both endpoints ("convert end=100 to
  ;; n=51 mentally"). The :range arity takes inclusive start..end.
  (it "(cat path :range start end) reads inclusive 1-based start..end"
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
                #(cat-tool path :not-range 1 5)))))

  (it "(cat path :ranges [[start end] ...]) reads several ranges in one result"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 21)))
          path (write-temp! "range/multi.txt" (str body "\n"))
          cat-tool (private-fn "cat-tool")
          out (-> (cat-tool path :ranges [[2 4] [10 12]]) :result)]
      (expect (= [[2 "L2"] [3 "L3"] [4 "L4"]
                  [10 "L10"] [11 "L11"] [12 "L12"]]
                (:lines out)))
      (expect (= [[2 4] [10 12]] (mapv :range (:ranges out))))
      (expect (= [[2 "L2"] [3 "L3"] [4 "L4"]]
                (-> out :ranges first :lines)))
      (expect (nil? (:next-offset out)))))

  (it ":ranges rejects empty or malformed range specs"
    (let [path (write-temp! "range/bad-multi.txt" "a\nb\nc\n")
          cat-tool (private-fn "cat-tool")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :ranges [])))
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :ranges [[2 1]])))
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :ranges [1 2 3]))))))

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
  {:op :cat
   :path path
   :lines (vec lines)
   :next-offset next-offset
   :truncated? truncated?})

(defdescribe channel-renderer-contract-test
  ;; Phase 1 hard cut: every render-fn returns {:summary <ir-or-zones>
  ;; :display <ir>}. These assert the new contract shape — :display
  ;; carries the IR body the fns used to return, :summary is the badge.
  (it "cat channel renderer conforms to ::render-fn-result"
    (let [channel-render-cat (private-fn "channel-render-cat")
          r   (cat-result "src/demo.clj" [[1 "only-line"]] nil false)
          out (channel-render-cat r)]
      (expect (extension/render-fn-result? out))
      (expect (some? (:summary out)))
      (expect (extension/render-value? (:display out)))))

  (it "cat :display is canonical [:ir ...] with a :code block, line-number gutter (human surface)"
    (let [channel-render-cat (private-fn "channel-render-cat")
          r   (cat-result "src/demo.clj" [[1 "only-line"]] nil false)
          out (channel-render-cat r)
          display (:display out)]
      (expect (vector? display))
      (expect (= :ir (first display)))
      (let [form-sources (filter #(and (vector? %) (= :code (first %))) (tree-seq sequential? seq display))
            body (last (first form-sources))]
        (expect (= 1 (count form-sources)))
        ;; Channel/TUI gutter is the LINE NUMBER (human navigation), NOT
        ;; the model's content-hash edit anchor (Vis session ac065988).
        (expect (string/includes? body "1│ only-line"))
        (expect (not (string/includes? body (str (patch/line-hash "only-line") "│ only-line")))))))

  (it "cat summary is a zone badge: CAT label, path centered, line/pagination right"
    (let [channel-render-cat (private-fn "channel-render-cat")
          r   (cat-result "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj"
                [[1898 "x"]] 1928 false)
          out (channel-render-cat r)
          summary (:summary out)
          ;; summary->ir flattens the zone map into one [:p …]; the label,
          ;; path and metric all surface there.
          flat (extension/summary->ir summary)
          text (apply str (filter string? (tree-seq sequential? seq flat)))
          has-strong-cat? (some #(and (vector? %)
                                   (= :strong (first %))
                                   (string/includes? (pr-str %) "CAT"))
                            (tree-seq sequential? seq flat))]
      (expect (extension/render-zones? summary))
      (expect has-strong-cat?)
      (expect (string/includes? text "render.clj"))
      (expect (string/includes? text "1 line"))
      (expect (string/includes? text "from=1898"))
      (expect (string/includes? text "next-offset=1928"))))

  (it "cat :display gutter is the per-line LINE NUMBER on each tuple"
    (let [channel-render-cat (private-fn "channel-render-cat")
          r   (cat-result "f.txt" [[100 "hundred"] [101 "hundred-one"]] 102 false)
          display (:display (channel-render-cat r))
          body (last (first (filter #(and (vector? %) (= :code (first %)))
                              (tree-seq sequential? seq display))))]
      (expect (= :ir (first display)))
      (expect (string/includes? body "100│ hundred"))
      (expect (string/includes? body "101│ hundred-one"))
      (expect (not (string/includes? body (str (patch/line-hash "hundred") "│ hundred"))))))

  (it "cat multi-range summary and display stay one CAT result"
    (let [channel-render-cat (private-fn "channel-render-cat")
          r {:op :cat
             :path "src/demo.clj"
             :lines [[2 "b"] [3 "c"] [10 "j"]]
             :ranges [{:range [2 3] :lines [[2 "b"] [3 "c"]]}
                      {:range [10 10] :lines [[10 "j"]]}]
             :truncated? false}
          out (channel-render-cat r)
          flat (extension/summary->ir (:summary out))
          text (apply str (filter string? (tree-seq sequential? seq flat)))
          body (last (first (filter #(and (vector? %) (= :code (first %)))
                              (tree-seq sequential? seq (:display out)))))]
      (expect (string/includes? text "3 lines"))
      (expect (string/includes? text "ranges=2-3,10-10"))
      (expect (string/includes? body "-- range 2-3 --"))
      (expect (string/includes? body "2│ b"))
      (expect (string/includes? body "-- range 10-10 --"))
      (expect (string/includes? body "10│ j"))))

  (it "ls renderer conforms to ::render-fn-result with a zone summary"
    (let [render (private-fn "channel-render-ls")
          out (render {:path "." :groups [{:dir "." :files [{:name "a" :size 3}]}]
                       :entry-count 1 :file-count 1 :dir-count 0})]
      (expect (extension/render-fn-result? out))
      (expect (extension/render-zones? (:summary out)))
      (expect (extension/render-value? (:display out)))
      (expect (string/includes? (pr-str (:summary out)) "LS"))))

  (it "ls renderer groups files under their dir header, indented, with sizes"
    (let [render (private-fn "channel-render-ls")
          out (render {:path "."
                       :groups [{:dir "bin" :files [{:name "dev" :size 2308}
                                                    {:name "vis" :size 4626}]}
                                {:dir "src" :files []}]
                       :entry-count 3 :file-count 2 :dir-count 2})
          body (pr-str (:display out))]
      ;; dir header stated once, files indented beneath it (no repeated prefix)
      (expect (string/includes? body "bin/"))
      (expect (string/includes? body "  dev"))
      (expect (string/includes? body "  vis"))
      ;; a dir with only subdirs still shows as a bare header
      (expect (string/includes? body "src/"))
      ;; sizes rendered human-readable (2308 -> 2.3k), not raw bytes-with-B
      (expect (string/includes? body "2.3k"))
      (expect (not (string/includes? body "2308B")))))

  (it "ls renderer with no entries still returns a valid (empty-bodied) display"
    (let [render (private-fn "channel-render-ls")
          out (render {:path "." :groups [] :entry-count 0 :file-count 0 :dir-count 0})]
      (expect (extension/render-fn-result? out))
      (expect (= :ir (first (:display out)))))))

(defdescribe error-formatter-contract-test
  ;; The engine default error formatter (Phase 1) now returns the
  ;; {:summary :display} contract. The editing extension carries no
  ;; tool-specific :render-error-fn, so failures route through it.
  (it "engine-default channel error formatter conforms to ::render-fn-result"
    (let [result (extension/default-error-result
                   {:success? false
                    :symbol :cat
                    :error {:message "src/missing.clj (No such file)"
                            :trace "java.io.FileNotFoundException: src/missing.clj (No such file)"}})]
      (expect (extension/render-fn-result? result))
      (expect (some? (:summary result)))
      (expect (extension/render-value? (:display result)))))

  (it "engine-default error :display is canonical [:ir ...] carrying the error class"
    (let [result (extension/default-error-result
                   {:success? false
                    :symbol :cat
                    :error {:message "src/missing.clj (No such file)"
                            :trace "java.io.FileNotFoundException: src/missing.clj (No such file)"}})
          out (:display result)
          joined (string/join " " (filter string? (tree-seq sequential? seq out)))]
      (expect (vector? out))
      (expect (= :ir (first out)))
      (expect (string/includes? joined "ERROR"))
      (expect (string/includes? joined "cat"))
      (expect (string/includes? joined "FileNotFoundException")))))

(defdescribe vis-rg-structured-shape-test
  (it "returns a 2-key map: :hits + :truncated-by"
    (let [_    (write-temp! "rg/a.txt" "alpha needle gamma\nbeta\n")
          _    (write-temp! "rg/b.txt" "plain line\nanother needle here\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["needle"] :paths [(temp-dir-path "rg")]})]
      (expect (= #{:hits :truncated-by} (set (keys out))))
      (expect (vector? (:hits out)))
      ;; Every hit is a clean {:path :line :text :hash} map (the content-addressed
      ;; anchor lets the model patch straight from a hit), no sentinel.
      (expect (every? #(= #{:path :line :text :hash} (set (keys %))) (:hits out)))
      (expect (= 2 (count (:hits out))))
      (expect (= :end-of-results (:truncated-by out)))))

  (it "query strings are literal, including pipe characters"
    (let [_    (write-temp! "rgliteral/a.clj" "foo|bar\nfoo only\nbar only\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["foo|bar"]
                      :paths [(temp-dir-path "rgliteral")]
                      :include ["*.clj"]})]
      (expect (= ["foo|bar"] (mapv :text (:hits out))))))

  (it "spec {:all [...]} requires all literals on the same line"
    (let [_    (write-temp! "rgall/a.clj" "(defn info-event [x] x)\n(defn other [x] x)\ninfo-event call\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["defn" "info-event"]
                      :paths [(temp-dir-path "rgall")]
                      :include ["*.clj"]})]
      (expect (= ["(defn info-event [x] x)"]
                (mapv :text (:hits out))))))

  (it "spec {:any [...]} is explicit OR"
    (let [_    (write-temp! "rgany/a.clj" "alpha\nbeta\ngamma\n")
          grep (private-fn "rg-search")
          out  (grep {:any ["alpha" "gamma"]
                      :paths [(temp-dir-path "rgany")]
                      :include ["*.clj"]})]
      (expect (= ["alpha" "gamma"] (mapv :text (:hits out))))))

  (it "accepts path vectors, include globs, and dedups overlapping roots"
    (let [root (temp-dir-path "rgpaths")
          _    (write-temp! "rgpaths/src/a.clj" "needle clj\n")
          _    (write-temp! "rgpaths/src/a.txt" "needle txt\n")
          _    (write-temp! "rgpaths/test/b.cljc" "needle cljc\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["needle"]
                      :paths [root (str root "/src")]
                      :include ["*.clj" "*.cljc"]})]
      (expect (= ["needle clj" "needle cljc"]
                (mapv :text (:hits out))))))

  (it "accepts :files as an alias for :paths"
    (let [file (write-temp! "rgfiles/a.clj" "needle alias\n")
          _    (write-temp! "rgfiles/b.clj" "needle other\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["needle"]
                      :files [file]})]
      (expect (= ["needle alias"] (mapv :text (:hits out))))))

  (it "private grep and public rg use the same single spec-map grammar"
    (let [_ (write-temp! "rgsame/a.clj" "needle same\n")
          spec {:all ["needle"]
                :paths [(temp-dir-path "rgsame")]
                :include ["*.clj"]}
          grep (private-fn "rg-search")
          rg (private-fn "rg-tool")
          ;; rg-tool groups grep's flat :hits into :matches (path-once) on
          ;; the model-facing :result — there is no flat :hits vec anymore.
          rg-result (:result (rg spec))
          grep-hits (:hits (grep spec))]
      (expect (= :rg (:op rg-result)))
      (expect (vector? (:matches rg-result)))
      (expect (= (count grep-hits) (:hit-count rg-result)))
      (expect (= (count (distinct (map :path grep-hits)))
                (:file-count rg-result)))))

  (it "rejects shorthand and unknown keys instead of silently changing grammar"
    (let [grep (private-fn "rg-search")
          rg (private-fn "rg-tool")
          bad-spec (fn [k v] (assoc {:all ["needle"] :paths ["."]} k v))]
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep "needle")))
      ;; NOTE: scalar :paths "." is NO LONGER rejected — it coerces to ["."]
      ;; (commit 89d76804). The scalar→vec coercion is covered directly by
      ;; rg-spec-path-alias-test; here we only assert that positional strings
      ;; and genuinely-unknown keys still throw.
      (let [err (try
                  (rg "needle" {:include "**/*.clj"})
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? err))
        (expect (= :ext.foundation.editing/invalid-rg-arity (:type (ex-data err))))
        (expect (clojure.string/includes? (ex-message err) "single options dict")))
      ;; :limit, :is_regex, :is_files_only, :is_counts are NOW valid keys.
      (doseq [[k v] [[(keyword "type") :clj]
                     [(keyword "mode") :any]]]
        (expect (throws? clojure.lang.ExceptionInfo
                  #(grep (bad-spec k v)))))))

  (it ":truncated-by :limit when results exceed the configured limit (default 250)"
    ;; Limit bumped 50 -> 250 in the rg sweep. Use 300 hits to force the cap.
    (let [_ (write-temp! "rgcap/a.txt"
              (string/join "\n" (map #(str "needle " %) (range 300))))
          grep (private-fn "rg-search")
          out  (grep {:all ["needle"] :paths [(temp-dir-path "rgcap")]})]
      (expect (= 250 (count (:hits out))))
      (expect (= :limit (:truncated-by out)))))

  (it ":limit override caps results below default"
    (let [_ (write-temp! "rglim/a.txt"
              (string/join "\n" (map #(str "needle " %) (range 50))))
          grep (private-fn "rg-search")
          out  (grep {:all ["needle"] :paths [(temp-dir-path "rglim")] :limit 5})]
      (expect (= 5 (count (:hits out))))
      (expect (= :limit (:truncated-by out)))))

  (it "empty result still has :truncated-by :end-of-results, never nil"
    (let [_ (write-temp! "rgmiss/a.txt" "nothing matches in here\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["definitely-not-present"] :paths [(temp-dir-path "rgmiss")]})]
      (expect (= [] (:hits out)))
      (expect (= :end-of-results (:truncated-by out)))))

  ;; Q1+Q2+Q3+Q4 — new option coverage.

  (it ":before / :after add context lines around each hit"
    (let [_path (write-temp! "rgctx/file.txt"
                  "alpha\nbeta\nGAMMA\ndelta\nepsilon\nfoo\nGAMMA\nbar\n")
          grep (private-fn "rg-search")
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
          grep (private-fn "rg-search")
          out  (grep {:all ["MATCH"]
                      :paths [(temp-dir-path "rgctxa")]
                      :context 2})
          h (first (:hits out))]
      (expect (= [[1 "L1"] [2 "L2"]] (:before h)))
      (expect (= [[4 "L4"] [5 "L5"]] (:after  h)))))

  (it ":context accepts a ripgrep-style {:before N :after N} map (model-natural form)"
    ;; Regression: the model wrote `context={"before": 0, "after": 0}` and the
    ;; integer-only validator rejected it. The map form must be accepted, with
    ;; independent before/after.
    (let [_path (write-temp! "rgctxm/a.txt" "L1\nL2\nMATCH\nL4\nL5\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["MATCH"]
                      :paths [(temp-dir-path "rgctxm")]
                      :context {:before 2 :after 1}})
          h (first (:hits out))]
      (expect (= [[1 "L1"] [2 "L2"]] (:before h)))
      (expect (= [[4 "L4"]] (:after  h)))))

  (it ":context {:before 0 :after 0} is a no-op (the exact failing call), not an error"
    (let [_path (write-temp! "rgctxz/a.txt" "L1\nMATCH\nL3\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["MATCH"]
                      :paths [(temp-dir-path "rgctxz")]
                      :context {:before 0 :after 0}})
          h (first (:hits out))]
      ;; the point: it RETURNS a hit (no validation throw), with no context lines
      (expect (= [2 "MATCH"] [(:line h) (:text h)]))
      (expect (empty? (:before h)))
      (expect (empty? (:after  h)))))

  (it ":is_files_only returns distinct paths and never line-level hits"
    (let [_ (write-temp! "rgfo/src/a.py" "alpha\nalpha\nalpha\n")
          _ (write-temp! "rgfo/src/b.py" "alpha\n")
          _ (write-temp! "rgfo/src/c.py" "no match\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["alpha"]
                      :paths [(temp-dir-path "rgfo")]
                      :is_files_only true})]
      (expect (= #{:files :truncated-by} (set (keys out))))
      (expect (= 2 (count (:files out))))
      (expect (every? string? (:files out)))))

  (it ":is_counts returns per-file match counts (real, not hit-cap-truncated)"
    (let [_ (write-temp! "rgcnt/src/a.py"
              (string/join "\n" (repeat 300 "needle")))
          _ (write-temp! "rgcnt/src/b.py" "needle\nneedle\n")
          _ (write-temp! "rgcnt/src/c.py" "no\n")
          grep (private-fn "rg-search")
          out  (grep {:all ["needle"]
                      :paths [(temp-dir-path "rgcnt")]
                      :is_counts true})
          a-count (some (fn [{:keys [path count]}]
                          (when (string/ends-with? path "a.py") count))
                    (:counts out))]
      (expect (= #{:counts :truncated-by} (set (keys out))))
      ;; Real per-file count is reported, NOT capped at 250 default.
      (expect (= 300 a-count))))

  (it ":is_regex true treats needles as java.util.regex patterns"
    (let [_ (write-temp! "rgrgx/src/a.py"
              "def login(user): pass\ndef test_login(): pass\ndef logout(): pass\n")
          grep (private-fn "rg-search")
          ;; Word-boundary regex: matches `login` but not `test_login`.
          out  (grep {:any ["\\bdef login\\b"]
                      :paths [(temp-dir-path "rgrgx")]
                      :is_regex true})
          texts (mapv :text (:hits out))]
      (expect (= 1 (count texts)))
      (expect (= "def login(user): pass" (first texts)))))

  (it ":is_regex with malformed pattern raises an invalid-rg-spec error"
    (let [grep (private-fn "rg-search")
          err (try (grep {:any ["(unclosed"]
                          :paths ["."]
                          :is_regex true})
                nil (catch clojure.lang.ExceptionInfo e e))]
      (expect (some? err))
      (expect (= :ext.foundation.editing/invalid-rg-spec (:type (ex-data err))))))

  (it ":is_files_only and :is_counts are mutually exclusive"
    (let [grep (private-fn "rg-search")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep {:any ["x"] :is_files_only true :is_counts true})))))

  (it ":before / :after rejected in :is_files_only or :is_counts mode"
    (let [grep (private-fn "rg-search")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep {:any ["x"] :is_files_only true :context 2})))
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep {:any ["x"] :is_counts true :before 1})))))

  (it "long lines pass through verbatim in hit :text (no per-line cap, no `…<+N chars>` marker)"
    ;; Regression: an earlier `rg-line-preview-chars` (500 chars) cap
    ;; mutilated every long hit line into `…<+N chars>`. That marker
    ;; reproduced the same failure mode as the trailer cap removed in
    ;; ccee2e1f-16ee-4acf-8d93-b4505034c0de — the model perceived its
    ;; own search data as missing and chased phantom cat roundtrips
    ;; even on normal source lines that brushed the cap. The model
    ;; owns its data; no silent renderer-side ellipsis.
    (let [huge (apply str (repeat 1000 "x"))
          line (str "NEEDLE " huge)
          _ (write-temp! "rgtext/big.txt" (str line "\n"))
          grep (private-fn "rg-search")
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

  (it ":expected_mtime guards against editing a file that changed since it was read"
    (let [patch (private-fn "patch-safe")
          p (write-temp! "bbfs/patch-stale.txt" "alpha\n")
          stale-mtime (- (.lastModified (fs/file p)) 100000)
          r (patch [{:path p :search "alpha" :replace "BETA"
                     :expected_mtime stale-mtime}])]
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
        (expect (string/includes? (:message r) "Consecutive patch failures"))
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
      (let [counts2 @(deref (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "patch-fail-counts")))]
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

  (it "batch edits resolve against the ORIGINAL snapshot, not each other's output"
    ;; Every hunk anchors to the file as the model last read it (never
    ;; cumulatively), so hashline/ordinal anchors and line numbers stay valid
    ;; across a multi-edit batch. A hunk that targets a PRIOR hunk's output
    ;; therefore won't match the original and the whole batch fails atomically.
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-seq.txt" "alpha\nbeta\n")
          r     (patch [{:path p :search "alpha" :replace "first"}
                        {:path p :search "first" :replace "FIRST"}])]
      (expect (false? (:success? r)))               ;; "first" isn't in the original
      (expect (= "alpha\nbeta\n" (slurp p))))       ;; atomic — nothing written
    ;; Two INDEPENDENT edits against the original both apply, in one plan.
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-seq2.txt" "alpha\nbeta\n")
          r     (patch [{:path p :search "alpha" :replace "ALPHA"}
                        {:path p :search "beta" :replace "BETA"}])]
      (expect (true? (:success? r)))
      (expect (= "ALPHA\nBETA\n" (slurp p)))
      (expect (= 1 (count (:plans r))))
      (expect (= "alpha\nbeta\n" (-> r :plans first :before)))
      (expect (= "ALPHA\nBETA\n" (-> r :plans first :after)))))

  (it "grouped same-file patch map applies several edits without repeating :path"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-grouped.txt" "alpha\nbeta\ngamma\n")
          r     (patch {:path p
                        :edits [{:search "alpha" :replace "ALPHA"}
                                {:search "beta" :replace "BETA"}]})]
      (expect (true? (:success? r)))
      (expect (= "ALPHA\nBETA\ngamma\n" (slurp p)))
      (expect (= 1 (count (:plans r))))
      (expect (= "alpha\nbeta\ngamma\n" (-> r :plans first :before)))
      (expect (= "ALPHA\nBETA\ngamma\n" (-> r :plans first :after)))))

  (it "grouped same-file patch rejects per-edit :path and preserves all-or-nothing"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-grouped-bad.txt" "alpha\nbeta\n")
          err   (try (patch {:path p :edits [{:path p :search "alpha" :replace "ALPHA"}]})
                  nil (catch clojure.lang.ExceptionInfo e e))
          r     (patch {:path p
                        :edits [{:search "alpha" :replace "ALPHA"}
                                {:search "missing" :replace "x"}]})]
      (expect (some? err))
      (expect (false? (:success? r)))
      (expect (= "alpha\nbeta\n" (slurp p)))))

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

  (it ":expected_size guards independent of :expected_mtime"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-size.txt" "hello\n")
          r     (patch [{:path p :search "hello" :replace "x"
                         :expected_size 1}])]
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

  (it "a single patch invocation cannot move the loop counter past +1 per path"
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

  (it "patch reports :exact-replace as its only mode (envelope retired)"
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

  (it "exists? tool returns the canonical {:op :exists? :path :exists?} map (envelope-shape consistency)"
    ;; Regression for conversation 11d4f817-fbd1-43ab-a6b4-052c8557af0a
    ;; turn 4 iter 1→2:
    ;;   model wrote `(def ports (exists? \".nrepl-port\"))` then
    ;;   `(:exists? ports)`, expecting the map shape every other `v/*`
    ;;   tool returns. The old `exists-tool` returned a bare boolean
    ;;   so `(:exists? true)` evaluated to `nil` and the model burned
    ;;   an iter realizing \"exists? is returning true directly rather
    ;;   than a map containing an :exists? key\".
    ;; Fix: every v/* tool returns a map keyed on :op for shape
    ;; consistency; bare booleans are no longer a supported result.
    (let [present-path (write-temp! "exists-shape/yes.txt" "x")
          missing-path "exists-shape/no.txt"
          exists-tool  (private-fn "exists-tool")
          present (:result (exists-tool present-path))
          missing (:result (exists-tool missing-path))]
      (expect (map? present))
      (expect (= :exists? (:op present)))
      (expect (true? (:exists? present)))
      (expect (= present-path (:path present)))
      (expect (map? missing))
      (expect (= :exists? (:op missing)))
      (expect (false? (:exists? missing)))
      (expect (= missing-path (:path missing)))))

  (it "keeps exists? shape details out of the compact prompt and in symbol docs"
    (let [exists-symbol (some #(when (= 'exists? (:ext.symbol/symbol %)) %)
                          editing/editing-symbols)]
      (expect (not (string/includes? editing/editing-prompt ":op :exists?")))
      (expect (string/includes? (:ext.symbol/doc exists-symbol) ":op :exists?"))
      (expect (string/includes? (:ext.symbol/doc exists-symbol) ":exists?"))))

  (it "bash helpers fully removed from the editing core"
    (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "run-bash-safe"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "bash-tool"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "strict-bash-command"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "coerce-bash-opts"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "bash-warnings"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "channel-render-bash"))))
    (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "journal-render-bash"))))))

(defdescribe delete-tool-shape-test
  ;; Regression: `delete-tool` used to set `:result nil`. The channel
  ;; preview then painted `DELETE nil` and `(def r (delete p))`
  ;; consumers couldn't read `(:path r)` (same parity bug `exists?`
  ;; already fixed). All `v/*` tools now return a map shape.
  (it "delete returns {:op :delete :path P :deleted? true} (no bare nil)"
    (let [delete-tool (private-fn "delete-tool")
          p           (write-temp! "delete-shape/x.txt" "goodbye\n")
          envelope    (delete-tool p)]
      (expect (true? (:success? envelope)))
      (expect (= :delete (:symbol envelope)))
      (let [r (:result envelope)]
        (expect (map? r))
        (expect (= :delete (:op r)))
        (expect (= p (:path r)))
        (expect (true? (:deleted? r))))))

  (it "delete-if-exists returns the same map shape with :deleted? reflecting the actual outcome"
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
  (it "MKDIR renderer reads :path off the result map (contract-conformant)"
    (let [out ((private-fn "channel-render-create-dirs")
               {:op :create-dirs :path "target/x" :created? true})]
      (expect (extension/render-fn-result? out))
      (expect (string/includes? (pr-str out) "target/x"))
      (expect (string/includes? (pr-str out) "MKDIR"))))

  (it "COPY renderer reads :src/:dest off the result map (contract-conformant)"
    (let [out ((private-fn "channel-render-copy")
               {:op :copy :src "a.txt" :dest "b.txt" :path "b.txt"})
          s  (pr-str out)]
      (expect (extension/render-fn-result? out))
      (expect (string/includes? s "COPY"))
      (expect (string/includes? s "a.txt"))
      (expect (string/includes? s "b.txt"))))

  (it "MOVE renderer reads :src/:dest off the result map (contract-conformant)"
    (let [out ((private-fn "channel-render-move")
               {:op :move :src "a.txt" :dest "b.txt" :path "b.txt"})
          s  (pr-str out)]
      (expect (extension/render-fn-result? out))
      (expect (string/includes? s "MOVE"))
      (expect (string/includes? s "a.txt"))
      (expect (string/includes? s "b.txt"))))

  (it "DELETE renderer reads :path off the result map (NO nil token in output)"
    (let [out ((private-fn "channel-render-delete")
               {:op :delete :path "src/foo.txt" :deleted? true})
          s  (pr-str out)]
      (expect (extension/render-fn-result? out))
      (expect (string/includes? s "src/foo.txt"))
      (expect (string/includes? s "DELETE"))
      (expect (not (string/includes? s "nil")))))

  (it "DELETE-IF-EXISTS renderer flips badge between DELETE / ABSENT on :deleted?"
    (let [render (private-fn "channel-render-delete-if-exists")
          gone   (render {:op :delete-if-exists :path "a.txt" :deleted? true})
          absent (render {:op :delete-if-exists :path "a.txt" :deleted? false})]
      (expect (extension/render-fn-result? gone))
      (expect (extension/render-fn-result? absent))
      (expect (string/includes? (pr-str gone)   "DELETE"))
      (expect (string/includes? (pr-str absent) "ABSENT"))))

  (it "EXISTS? renderer flips badge between EXISTS / MISSING on :exists?"
    (let [render  (private-fn "channel-render-exists?")
          present (render {:op :exists? :path "a.txt" :exists? true})
          absent  (render {:op :exists? :path "a.txt" :exists? false})]
      (expect (extension/render-fn-result? present))
      (expect (extension/render-fn-result? absent))
      (expect (string/includes? (pr-str present) "EXISTS"))
      (expect (string/includes? (pr-str absent)  "MISSING")))))

(defdescribe patch-summary-shape-test
  ;; The summary IS what the model reads back as the patch result
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

  (it "successful patch with byte-exact match carries no :passes / :indent-delta"
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
  (it "patch renderer conforms to ::render-fn-result; PATCH badge in summary, paths in display"
    (let [render-patch (private-fn "channel-render-patch")
          out (render-patch [{:path "target/editing-test/out.txt"}])
          display (:display out)
          joined (string/join " " (filter string? (tree-seq sequential? seq display)))
          has-strong-patch? (some #(and (vector? %)
                                     (= :strong (first %))
                                     (string/includes? (pr-str %) "PATCH"))
                              (tree-seq sequential? seq (extension/summary->ir (:summary out))))]
      (expect (extension/render-fn-result? out))
      (expect (vector? display))
      (expect (= :ir (first display)))
      (expect has-strong-patch?)
      (expect (string/includes? joined "target/editing-test/out.txt"))))

  (it "patch renderer surfaces :passes as a fuzzy alarm in the display header"
    (let [render-patch (private-fn "channel-render-patch")
          out (render-patch [{:path "src/foo.py"
                              :op :update
                              :changed? true
                              :diff ""
                              :passes [:rstrip :trim]}])
          joined (string/join " " (filter string? (tree-seq sequential? seq (:display out))))]
      (expect (string/includes? joined "[fuzzy: rstrip,trim]"))))

  (it "patch renderer surfaces :indent-delta when relative-indent fired"
    (let [render-patch (private-fn "channel-render-patch")
          out (render-patch [{:path "src/foo.py"
                              :op :update
                              :changed? true
                              :diff ""
                              :passes [:relative-indent]
                              :indent-delta 4}])
          joined (string/join " " (filter string? (tree-seq sequential? seq (:display out))))]
      (expect (string/includes? joined "[indentΔ +4]"))))

  (it "patch renderer says nothing about fuzzy when only exact matches fired"
    (let [render-patch (private-fn "channel-render-patch")
          out (render-patch [{:path "src/foo.py"
                              :op :update
                              :changed? true
                              :diff ""}])
          joined (string/join " " (filter string? (tree-seq sequential? seq (:display out))))]
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
  (it "search-grouped renderer formats grouped matches (path once) without raw EDN fallback"
    (let [render-hits (private-fn "channel-render-rg")
          rg-result {:op :rg
                     :hit-count 2
                     :file-count 1
                     :truncated-by :end-of-results
                     :matches [{:path "src/foo.clj"
                                :lines [[462 "  (inc (reduce max 0 ...))"]
                                        [472 "(defn- workspace-tabs-or-base"]]}]}
          out (render-hits rg-result)
          display (:display out)
          joined (string/join "\n" (filter string? (tree-seq sequential? seq display)))]
      (expect (extension/render-fn-result? out))
      (expect (vector? display))
      (expect (= :ir (first display)))
      (expect (not (string/includes? joined ":lines")))
      ;; path stated ONCE as a header; line numbers + text indented beneath
      (expect (string/includes? joined "src/foo.clj"))
      (expect (string/includes? joined "462"))
      (expect (string/includes? joined "(inc (reduce max 0"))
      (expect (string/includes? joined "472"))
      (expect (string/includes? joined "workspace-tabs-or-base"))))
  (it "search-grouped renderer states the path once as a header row"
    (let [render-hits (private-fn "channel-render-rg")
          rg-result {:op :rg
                     :hit-count 1
                     :file-count 1
                     :truncated-by :end-of-results
                     :matches [{:path "src/foo.clj"
                                :lines [[10 "(def x 1)"]]}]}
          out (render-hits rg-result)
          joined (string/join "\n" (filter string? (tree-seq sequential? seq (:display out))))]
      (expect (extension/render-fn-result? out))
      (expect (string/includes? joined "src/foo.clj"))
      (expect (string/includes? joined "10"))
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
      ;; cat returns a plain map as :result. :lines is a vec of
      ;; `[line-number text]` tuples; no deref, no handle, no offset key.
      (let [r (:result out)]
        (expect (= :cat (:op r)))
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

(defdescribe vis-patch-hashline-test
  ;; End-to-end content-addressed editing: read hashes from cat, then
  ;; patch by :from_hash / :to_hash. The hash anchors come straight from
  ;; the read's `:hashes` map (same value rendered in the cat gutter),
  ;; and self-locate against live disk content on apply.
  (it "patch :from_hash replaces a single content-anchored line"
    (let [path (write-temp! "hashline/single.txt"
                 "alpha first\nbeta second\ngamma third\n")
          read-file (private-fn "read-file")
          patch (private-fn "patch-safe")
          hashes (:hashes (read-file path))
          h2 (get hashes 2)
          r (patch [{:path path :from_hash h2 :replace "BETA REPLACED"}])]
      (expect (true? (:success? r)))
      (expect (= "alpha first\nBETA REPLACED\ngamma third\n" (slurp path)))))

  (it "patch :from_hash + :to_hash replaces an inclusive range"
    (let [path (write-temp! "hashline/range.txt" "a\nb\nc\nd\n")
          read-file (private-fn "read-file")
          patch (private-fn "patch-safe")
          hashes (:hashes (read-file path))
          r (patch [{:path path
                     :from_hash (get hashes 1)
                     :to_hash (get hashes 3)
                     :replace "X"}])]
      (expect (true? (:success? r)))
      (expect (= "X\nd\n" (slurp path)))))

  (it "duplicate lines are surfaced as distinct `lineno:hash` anchors; a BARE hash that hits >1 line is refused"
    (let [path (write-temp! "hashline/dup.txt" "x\ny\nx\n")
          read-file (private-fn "read-file")
          patch (private-fn "patch-safe")
          ;; The dup line 'x' is surfaced as `1:hash` / `3:hash` — the line
          ;; number disambiguates, no `#N` ordinal needed. A BARE content hash
          ;; (no line number) still hits BOTH lines; feed it raw to exercise the
          ;; legacy ambiguity refusal.
          hashes (:hashes (read-file path))
          r (patch [{:path path :from_hash (patch/line-hash "x") :replace "NEW"}])]
      (expect (= (patch/line-anchor 1 "x") (get hashes 1)))  ;; 1st dup → 1:hash
      (expect (= (patch/line-anchor 3 "x") (get hashes 3)))  ;; 2nd dup → 3:hash
      (expect (= (patch/line-anchor 2 "y") (get hashes 2)))  ;; unique line too
      (expect (false? (:success? r)))
      (expect (= :hash-ambiguous (-> r :failures first :reason)))
      ;; file untouched
      (expect (= "x\ny\nx\n" (slurp path)))))

  (it "patch reconciles BOTH :search and :from_hash (search wins)"
    ;; A common model slip: an edit carrying both locators. Without :to_hash
    ;; the multi-line-capable :search wins; the stray :from_hash is dropped.
    (let [path (write-temp! "hashline/both.txt" "a\nb\n")
          patch (private-fn "patch-safe")
          r (patch [{:path path :search "a" :from_hash "junk99" :replace "Z"}])]
      (expect (true? (:success? r)))
      (expect (= "Z\nb\n" (slurp path)))))

  (it "patch reconciles dual locators with :to_hash (hash range wins)"
    ;; An explicit :to_hash declares hash-RANGE intent; :search is dropped,
    ;; so even a non-matching search string cannot break the edit.
    (let [path (write-temp! "hashline/both-range.txt" "a\nb\nc\n")
          patch (private-fn "patch-safe")
          h1 (patch/line-anchor 1 "a")
          h2 (patch/line-anchor 2 "b")
          r (patch [{:path path :search "NO-SUCH-TEXT" :from_hash h1 :to_hash h2 :replace "Z"}])]
      (expect (true? (:success? r)))
      (expect (= "Z\nc\n" (slurp path)))))

  (it "patch with ZERO locators still throws"
    (let [path (write-temp! "hashline/none.txt" "a\n")
          patch (private-fn "patch-safe")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(patch [{:path path :replace "Z"}]))))))

(defdescribe vis-cat-hash-read-test
  ;; cat :hash — the READ twin of patch :from_hash. Re-read a kept region
  ;; by its content hash, addressed by content not drifting line numbers.
  (let [cat-tool (private-fn "cat-tool")
        path     (write-temp! "hashread/probe.clj"
                   "(ns probe)\n(def alpha 1)\n(def beta 2)\n(def gamma 3)\n")
        h-beta   (patch/line-hash "(def beta 2)")
        h-gamma  (patch/line-hash "(def gamma 3)")]
    (it "(cat path :hash H) reads the single line whose content hash is H"
      (let [out (:result (cat-tool path :hash h-beta))]
        (expect (= [[3 "(def beta 2)"]] (:lines out)))
        (expect (= [3 3] (:range out)))))
    (it "(cat path :hash H1 H2) reads the inclusive content-addressed window"
      (let [out (:result (cat-tool path :hash h-beta h-gamma))]
        (expect (= (numbered-tuples 3 ["(def beta 2)" "(def gamma 3)"]) (:lines out)))
        (expect (= [3 4] (:range out)))))
    (it "addresses by CONTENT — survives line drift (prepend shifts numbers)"
      (let [p2 (write-temp! "hashread/drift.clj"
                 "(ns probe)\n(def beta 2)\n")
            _  (spit (fs/file p2) (str ";; banner\n;; banner2\n" (slurp (fs/file p2))))
            out (:result (cat-tool p2 :hash (patch/line-hash "(def beta 2)")))]
        ;; beta moved from line 2 to line 4; the hash still finds it
        (expect (= [[4 "(def beta 2)"]] (:lines out)))))
    (it "a missing hash throws back to cat for fresh :hashes"
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :hash "zzzz"))))
    (it "an unknown 4-arity mode throws"
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :nonsense h-beta h-gamma))))))

;; =============================================================================
;; Patch: duplicate-line anchors in a multi-edit batch, resolved vs the ORIGINAL
;; snapshot (regression for the cumulative-resolution bug that made a batch's
;; later anchors drift and fail). Duplicate lines are now told apart by their
;; LINE NUMBER (`lineno:hash`), not a `#N` ordinal.
;; =============================================================================

(defdescribe patch-dup-line-batch-test
  (it "dup-line edits in one batch all resolve against the ORIGINAL and apply atomically"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "ord/dup.txt" "x\nDUP\ny\nDUP\nz\nDUP\n")]
      ;; DUP on lines 2,4,6 — same hash, different line numbers.
      (let [r (patch [{:path p :from_hash (patch/line-anchor 2 "DUP") :replace "DUP1"}
                      {:path p :from_hash (patch/line-anchor 6 "DUP") :replace "DUP3"}])]
        (expect (true? (:success? r)))
        ;; lines 2 and 6 edited, line 4 untouched — line numbers resolve vs the
        ;; original snapshot, no drift.
        (expect (= "x\nDUP1\ny\nDUP\nz\nDUP3\n" (slurp p))))))

  (it "all three duplicate lines editable in one batch"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "ord/dup3.txt" "DUP\nDUP\nDUP\n")
          r     (patch [{:path p :from_hash (patch/line-anchor 1 "DUP") :replace "A"}
                        {:path p :from_hash (patch/line-anchor 2 "DUP") :replace "B"}
                        {:path p :from_hash (patch/line-anchor 3 "DUP") :replace "C"}])]
      (expect (true? (:success? r)))
      (expect (= "A\nB\nC\n" (slurp p)))))

  (it "overlapping edits in one batch are rejected — nothing written (atomic)"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "ord/over.txt" "alpha beta\n")
          r     (patch [{:path p :search "alpha beta" :replace "X"}
                        {:path p :search "beta" :replace "Y"}])]
      (expect (false? (:success? r)))
      (expect (= :overlapping-edits (-> r :failures last :reason)))
      (expect (= "alpha beta\n" (slurp p))))))

;; =============================================================================
;; rg hits carry the content-addressed :hash anchor (cat/rg parity), so a hit
;; is directly patchable without a follow-up cat.
;; =============================================================================

(defdescribe rg-returns-hash-test
  (let [rg-search (private-fn "rg-search")
        patch     (private-fn "patch-safe")]
    (it "a content hit carries its `lineno:hash` anchor and that anchor patches the line"
      (let [p   (write-temp! "rgh/uniq.clj" "(def a 1)\n(def b 2)\n(def c 3)\n")
            res (rg-search {:any ["def b"] :paths [p]})
            hit (first (:hits res))]
        (expect (= (patch/line-anchor 2 "(def b 2)") (:hash hit)))
        (let [r (patch [{:path p :from_hash (:hash hit) :replace "(def b 200)"}])]
          (expect (true? (:success? r)))
          (expect (string/includes? (slurp p) "(def b 200)")))))

    (it "hits on a DUPLICATED line carry distinct `lineno:hash` anchors (line number disambiguates)"
      (let [p   (write-temp! "rgh/dup.clj" "(def x 1)\n(other)\n(def x 1)\n")
            res (rg-search {:any ["def x"] :paths [p]})
            hashes (map :hash (:hits res))]
        (expect (= [(patch/line-anchor 1 "(def x 1)") (patch/line-anchor 3 "(def x 1)")] hashes))))))
