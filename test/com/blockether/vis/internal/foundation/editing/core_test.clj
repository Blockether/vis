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
   [com.blockether.vis.internal.foundation.environment.core :as environment]
   [com.blockether.vis.internal.workspace :as workspace]
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

(defn- ensure-rg-corpus!
  "Idempotently build the small known directory fixture the ls/list-files tests
   list. Shape: two files at the root + a nested dir with one file, so the tree
   has files AND a subdir at depth 1 (exercises depth/files-only/dirs-only/limit).
   Returns the cwd-relative path."
  []
  (let [rel "target/probe/rg-corpus"]
    (fs/create-dirs (str rel "/nested"))
    (spit (fs/file (str rel "/alpha.txt")) "alpha\n")
    (spit (fs/file (str rel "/beta.txt")) "beta\n")
    (spit (fs/file (str rel "/nested/gamma.txt")) "gamma\n")
    rel))

(defdescribe native-tools-flat-spec-guard
  ;; Cross-check the STRONG flat spec across EVERY editing native tool: schema is
  ;; required + tightly attached, render present, description non-blank, and the
  ;; legacy :native-tool map is gone. Build-time enforcement already throws on a
  ;; schema-less native tool; this locks render/description/no-legacy too.
  (it "every editing native tool is flat: :native-tool? + required :schema + :render + non-blank description, no legacy map"
    (let [ext   {:ext/engine {:ext.engine/symbols (editing/available-editing-symbols)}}
          ents  (filter :ext.symbol/native-tool? (extension/ext-symbols ext))
          tools (extension/native-tools-for [ext])
          names (set (map :name tools))]
      (expect (<= 8 (count ents)))                                  ;; cat ls find rg patch move delete file_exists
      (expect (contains? names "cat"))
      (expect (contains? names "rg"))
      (expect (contains? names "file_exists"))                      ;; file-exists → file_exists
      (expect (every? (comp map? :ext.symbol/schema) ents))         ;; schema tight on the symbol
      (expect (every? :schema tools))                               ;; and surfaced
      (expect (every? :render tools))                               ;; renderer present
      (expect (every? (comp seq str :description) tools))           ;; non-blank model-facing description
      (expect (not-any? :ext.symbol/native-tool ents))))            ;; legacy map removed
  (it "native-tool-renderers-by-op keys by the result :op string (cat→\"cat\", file-exists→\"file_exists\")"
    (let [ext   {:ext/engine {:ext.engine/symbols (editing/available-editing-symbols)}}
          by-op (extension/native-tool-renderers-by-op [ext])]
      (expect (fn? (:render (get by-op "cat"))))
      (expect (fn? (:render (get by-op "rg"))))
      (expect (contains? by-op "file_exists"))                       ;; file-exists → "file_exists"
      (expect (= :tool-color/search (:color-role (get by-op "rg")))))))

(defdescribe rg-simplified-api-test
  ;; NEW simplified rg grammar: `query` canonical, `any`/`all` accepted aliases
  ;; that BOTH mean OR, smart-case literal substring, `paths`/`include`/`context`
  ;; (int only)/`is_files_only`. Unknown keys ignored; missing query throws.
  (let [coerce (private-fn "coerce-rg-spec")
        matcher @#'editing/make-line-matcher
        grep    (private-fn "rg-search")]

    (it ":query is canonical; :any and :all are accepted aliases (all OR)"
      (expect (= ["a" "b"] (:needles (coerce {"query" ["a" "b"]}))))
      (expect (= ["a" "b"] (:needles (coerce {"any" ["a" "b"]}))))
      (expect (= ["a" "b"] (:needles (coerce {"all" ["a" "b"]})))))

    (it "a comma-joined query string is split into OR terms (session 71a69809 root cause)"
        ;; The model writes the OR list as ONE comma string (`\"model, cycle\"`),
        ;; which matched nothing as a literal → 0 hits. Split it into needles.
      (expect (= ["model" "cycle"] (:needles (coerce {"query" "model, cycle"}))))
      (expect (= ["a" "b" "c"] (:needles (coerce {"query" ["a, b" "c"]}))))
      (expect (= ["Cycle" "cycle"] (:needles (coerce {"query" "Cycle, cycle"}))))
      (expect (= ["foo"] (:needles (coerce {"query" "foo"})))))    ;; single term untouched

    (it "defaults :paths to [\".\"] and keeps canonical :paths/:include"
      (expect (= ["."] (:paths (coerce {"query" ["x"]}))))
      (expect (= ["src"] (:paths (coerce {"query" ["x"] "paths" ["src"]}))))
      (expect (= ["*.clj"] (:include (coerce {"query" ["x"] "include" ["*.clj"]})))))

    (it "ignores unknown keys (removed aliases are just dropped, never fatal)"
      (let [spec (coerce {"query" ["x"] "path" "src" "glob" "*.clj"
                          "excludes" ["t/**"] "is_regex" true "is_counts" true
                          "limit" 5})]
        ;; removed aliases don't set :paths/:include; canonical defaults win
        (expect (= ["."] (:paths spec)))
        (expect (= [] (:include spec)))))

    (it "missing query throws `rg needs query`"
      (let [err (try (coerce {"paths" ["."]}) nil
                  (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? err))
        (expect (clojure.string/includes? (ex-message err) "rg needs"))))

    (it ":context must be a non-negative integer (the map form is gone)"
      (expect (= 2 (:context (coerce {"query" ["x"] "context" 2}))))
      (expect (throws? clojure.lang.ExceptionInfo
                #(coerce {"query" ["x"] "context" {"before" 1 "after" 1}}))))

    (it "smart-case: a lowercase needle matches any case (make-line-matcher)"
      (let [m (matcher ["key"])]
        (expect (m "key"))
        (expect (m "Key"))
        (expect (m "KEY"))
        (expect (m "keymap"))
        (expect (not (m "nope")))))

    (it "smart-case: an uppercase-containing needle is case-sensitive"
      (let [m (matcher ["Key"])]
        (expect (m "Key"))
        (expect (m "a Keyword"))
        (expect (not (m "key")))
        (expect (not (m "KEY")))))

    (it "make-line-matcher ORs across needles"
      (let [m (matcher ["alpha" "gamma"])]
        (expect (m "alpha here"))
        (expect (m "gamma here"))
        (expect (not (m "beta here")))))

    (it "rg-search runs with a positional-equivalent list query and ORs"
      (let [_   (write-temp! "rgsimple/a.txt" "alpha\nbeta\ngamma\n")
            out (grep {"query" ["alpha" "gamma"] "paths" [(temp-dir-path "rgsimple")]})]
        (expect (= ["alpha" "gamma"] (mapv :text (:hits out))))))

    (it ":is_files_only returns distinct :files, never :hits"
      (let [_   (write-temp! "rgsimplefo/a.py" "alpha\nalpha\n")
            _   (write-temp! "rgsimplefo/b.py" "alpha\n")
            _   (write-temp! "rgsimplefo/c.py" "no match\n")
            out (grep {"query" ["alpha"]
                       "paths" [(temp-dir-path "rgsimplefo")]
                       "is_files_only" true})]
        (expect (contains? out :files))
        (expect (not (contains? out :hits)))
        (expect (= 2 (count (:files out))))))))

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
          (let [r (patch [{"path" p "from_anchor" (patch/line-anchor 1 "x") "replace" "y"}])]
            (expect (false? (:success? r)))
            (expect (= :path-escape (-> r :failures first :reason)))))))

    (it "write refuses to create files outside cwd"
      ;; Note: we deliberately do NOT (.exists) the escape path here; the
      ;; check is whether `write-safe` REFUSED to act. /etc/passwd exists
      ;; on macOS regardless of our actions; what matters is :reason :path-escape
      ;; and the cwd guard kicking in before any IO.
      (let [write (private-fn "write-safe")]
        (doseq [p escape-paths]
          (let [r (write {"path" p "content" "hi"})]
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
    ;; cat, find, ls, rg, patch, write, create-dirs, copy, move, delete,
    ;; delete-if-exists, exists?, plus the tree-sitter structural tools:
    ;; outline, occurrences (defs+uses, folds the old references/project_references),
    ;; symbol_rename (cross-file), struct_patch (locate by NAME or zipper PATH),
    ;; and the read-only zipper navigator sexpr.
    (expect (= 17 (count editing/editing-symbols)))
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
    (expect (string/includes? editing/editing-prompt "find"))
    (expect (string/includes? editing/editing-prompt "rg"))
    (expect (string/includes? editing/editing-prompt "ls"))
    (expect (string/includes? editing/editing-prompt "cat"))
    nil))

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
                [[{"path" path "search" "old" "replace" "new"}]])
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
                [{"any" ["scrollbar"] "paths" ["."] "is_counts" true}])]
      (expect (not (contains? out :result)))
      (expect (= [{"any" ["scrollbar"] "paths" ["."] "is_counts" true}] (:args out)))))

  (it "rg with no :paths (default `.`) is allowed when only descendants are protected"
    ;; rg-arg-paths returns ["."] when :paths is omitted; same bypass
    ;; must apply so model can call `(rg {:any ["x"]})` without paths.
    (let [before (:ext.symbol/before-fn (private-fn "rg-symbol"))
          out (before (protected-env [{:glob ".bridge/"
                                       :access :none
                                       :hint "Use (br/policy) instead."}])
                (constantly :ok)
                [{"any" ["scrollbar"]}])]
      (expect (not (contains? out :result)))))

  (it "exists? on `.` is allowed when only descendants are protected"
    (let [before (:ext.symbol/before-fn (private-fn "file-exists-symbol"))
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
                [{"any" ["x"] "paths" ["."]}])
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
                     [{"path" "target/editing-test/b.clj" "content" "x"}])
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
                     [{"path" "target/editing-test/a.clj" "content" "x"}])]
        (expect (not (contains? out :result)))
        (expect (= [{"path" "target/editing-test/a.clj" "content" "x"}] (:args out)))))
    (it "detects the atomic=True escape flag on a write"
      (let [seen   (atom nil)
            before (:ext.symbol/before-fn (private-fn "write-symbol"))]
        (before (gate-env seen nil)
          (constantly :ok)
          [{"path" "target/editing-test/a.clj" "content" "x" "atomic" true}])
        (expect (true? (:atomic? @seen)))))
    (it "detects atomic on a patch edit map + reports all edited paths"
      (let [seen   (atom nil)
            before (:ext.symbol/before-fn (private-fn "patch-symbol"))]
        (before (gate-env seen nil)
          (constantly :ok)
          [[{"path" "target/editing-test/a.clj" "search" "o" "replace" "n" "atomic" true}
            {"path" "target/editing-test/b.clj" "search" "o" "replace" "n"}]])
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
                     [{"path" "target/editing-test/protected/x.clj" "content" "x"}])
            failure (:result out)]
        (expect (= :ext.foundation.editing/path-protected (-> failure :error :type)))))
    (it "no :mutation-gate on env → write passes through (gate is optional)"
      (let [before (:ext.symbol/before-fn (private-fn "write-symbol"))
            out    (before {:extensions (atom [])}
                     (constantly :ok)
                     [{"path" "target/editing-test/a.clj" "content" "x"}])]
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
                      [[{"path" path "search" "old" "replace" "new"}]])
          ls-out (ls-before (protected-env rules)
                   (constantly :ok)
                   ["target/editing-test/protected"])
          failure (:result ls-out)]
      (expect (not (contains? patch-out :result)))
      (expect (= [[{"path" path "search" "old" "replace" "new"}]] (:args patch-out)))
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
          out        (list-files "." {"depth" 1})
          envelope   (ls-tool "." {"depth" 1})
          result     (:result envelope)
          groups     (get out "groups")]
      (expect (= "." (get out "path")))
      (expect (= (str (.toAbsolutePath (fs/path (fs/cwd))))
                (get out "absolute_path")))
      (expect (= :ls (:symbol envelope)))
      (expect (= (get out "groups") (get result "groups")))
      (expect (vector? groups))
      ;; every group is {"dir" <str> "files" [{"name" <str> "size" <int|nil>}]}
      (expect (every? #(= #{"dir" "files"} (set (keys %))) groups))
      (expect (every? (fn [g] (string? (get g "dir"))) groups))
      (expect (every? (fn [g]
                        (every? #(= #{"name" "size"} (set (keys %))) (get g "files")))
                groups))
      ;; root group leads and dir headers are unique (one group per dir)
      (expect (= "." (get (first groups) "dir")))
      (expect (= (count groups) (count (distinct (map #(get % "dir") groups)))))
      ;; total files across groups == file-count; dir headers == dir-count + root
      (expect (= (get out "file_count")
                (reduce + 0 (map (comp count #(get % "files")) groups))))
      (expect (= (inc (get out "dir_count")) (count groups)))))

  (it "ls() with no args lists the current directory — same as ls(\".\")"
    ;; The model naturally calls `ls()` (Pythonic, like os.listdir()). The
    ;; zero-arg arity must default the path to \".\" instead of throwing an
    ;; ArityException (regression: `ls()` failed with
    ;; \"Wrong number of args (0)\" and the model had to grope to `ls(\".\")`).
    (let [ls-tool (private-fn "ls-tool")
          e0      (ls-tool)
          r0      (:result e0)
          r1      (:result (ls-tool "."))]
      (expect (= :ls (:symbol e0)))
      (expect (= "." (get r0 "path")))
      (expect (= (get r0 "absolute_path") (get r1 "absolute_path")))
      (expect (= (get r0 "entry_count") (get r1 "entry_count")))))

  (it "recurses up to :depth (default 10) and reports entry counts"
    (let [list-files (private-fn "list-files")
          ;; target/probe/rg-corpus is a small known fixture
          path (ensure-rg-corpus!)
          out (list-files path)]
      (expect (pos? (get out "entry_count")))
      (expect (pos? (get out "file_count")))
      (expect (pos? (get out "dir_count")))
      (expect (false? (get out "truncated")))
      (expect (= 10 (get out "depth")))))

  (it ":is_files_only excludes directory entries; :is_dirs_only excludes files"
    (let [list-files (private-fn "list-files")
          path (ensure-rg-corpus!)
          files-only (list-files path {"is_files_only" true})
          dirs-only  (list-files path {"is_dirs_only"  true})]
      ;; files-only: dir-count 0; every listed file lives under a group
      (expect (zero? (get files-only "dir_count")))
      (expect (= (get files-only "file_count")
                (reduce + 0 (map (comp count #(get % "files")) (get files-only "groups")))))
      ;; dirs-only: every group is a bare dir header (no files), file-count 0
      (expect (zero? (get dirs-only "file_count")))
      (expect (every? (comp empty? #(get % "files")) (get dirs-only "groups")))))

  (it ":limit caps entries and surfaces :truncated? true"
    (let [list-files (private-fn "list-files")
          path (ensure-rg-corpus!)
          out (list-files path {"limit" 2})]
      (expect (= 2 (get out "entry_count")))
      (expect (true? (get out "truncated")))))

  (it ":is_files_only and :is_dirs_only are mutually exclusive"
    (let [list-files (private-fn "list-files")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(list-files "." {"is_files_only" true "is_dirs_only" true})))))

  (it ":depth 0 emits no entries (root not included)"
    (let [list-files (private-fn "list-files")
          out (list-files (ensure-rg-corpus!) {"depth" 0})]
      (expect (= 0 (get out "entry_count")))
      ;; only the root group remains, with no files (nothing walked)
      (expect (= 1 (count (get out "groups"))))
      (expect (empty? (get (first (get out "groups")) "files"))))))

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
      (expect (= #{:path :lines :anchors :next-offset :eof? :truncated? :mtime :size}
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
      (patch [{"path" path "from_anchor" (patch/line-anchor 1 "alpha") "replace" "BETA" "expected_mtime" mtime0}])
      (expect (= "BETA\n" (slurp path)))
      ;; Force-clock the file backwards so the next read sees a fresh mtime
      ;; distinct from `mtime0` regardless of filesystem millis precision.
      (.setLastModified (fs/file path) (- (long mtime0) 60000))
      (let [r (patch [{"path" path "from_anchor" (patch/line-anchor 1 "BETA") "replace" "GAMMA"
                       "expected_mtime" mtime0}])]
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
      (expect (= 2000 (count (get out "anchors"))))
      (expect (= 401 (ffirst (patch/anchor-map->tuples (get out "anchors")))))
      (expect (= 2400 (first (peek (patch/anchor-map->tuples (get out "anchors"))))))
      (expect (nil? (get out "next_offset")))))

  (it "(cat path :tail n) honours an explicit count"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 21)))
          path (write-temp! "explicit-tail.txt" (str body "\n"))
          cat-tool (private-fn "cat-tool")
          out (-> (cat-tool path :tail 3) :result)]
      (expect (= (numbered-tuples 18 ["L18" "L19" "L20"]) (patch/anchor-map->tuples (get out "anchors")))))))

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
      (expect (= 6 (count (get out "anchors"))))
      (expect (= [[5 "L5"] [6 "L6"] [7 "L7"] [8 "L8"] [9 "L9"] [10 "L10"]]
                (patch/anchor-map->tuples (get out "anchors"))))))

  (it ":range with start == end reads exactly one line"
    (let [body (string/join "\n" (map #(str "L" %) (range 1 11)))
          path (write-temp! "range/single.txt" (str body "\n"))
          cat-tool (private-fn "cat-tool")
          out (-> (cat-tool path :range 7 7) :result)]
      (expect (= [[7 "L7"]] (patch/anchor-map->tuples (get out "anchors"))))))

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
                (patch/anchor-map->tuples (get out "anchors"))))
      (expect (= [[2 4] [10 12]] (mapv #(get % "range") (get out "ranges"))))
      (expect (= [[2 "L2"] [3 "L3"] [4 "L4"]]
                (patch/anchor-map->tuples (get (first (get out "ranges")) "anchors"))))
      (expect (nil? (get out "next_offset")))))

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
  ;; trailer caps removed alongside (see ctx_renderer.clj header note). The structural
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

(defdescribe vis-rg-structured-shape-test
  (it "returns a 2-key map: :hits + :truncated-by"
    (let [_    (write-temp! "rg/a.txt" "alpha needle gamma\nbeta\n")
          _    (write-temp! "rg/b.txt" "plain line\nanother needle here\n")
          grep (private-fn "rg-search")
          out  (grep {"all" ["needle"] "paths" [(temp-dir-path "rg")]})]
      (expect (= #{:hits :truncated-by} (set (keys out))))
      (expect (vector? (:hits out)))
      ;; Every hit is a clean {:path :line :text :anchor} map (the content-addressed
      ;; anchor lets the model patch straight from a hit), no sentinel.
      (expect (every? #(= #{:path :line :text :anchor} (set (keys %))) (:hits out)))
      (expect (= 2 (count (:hits out))))
      (expect (= :end-of-results (:truncated-by out)))))

  (it "query strings are literal, including pipe characters"
    (let [_    (write-temp! "rgliteral/a.clj" "foo|bar\nfoo only\nbar only\n")
          grep (private-fn "rg-search")
          out  (grep {"all" ["foo|bar"]
                      "paths" [(temp-dir-path "rgliteral")]
                      "include" ["*.clj"]})]
      (expect (= ["foo|bar"] (mapv :text (:hits out))))))

  (it "spec {:all [...]} is an OR alias for :query (same-line AND was removed)"
    (let [_    (write-temp! "rgall/a.clj" "(defn info-event [x] x)\n(defn other [x] x)\ninfo-event call\n")
          grep (private-fn "rg-search")
          out  (grep {"all" ["defn" "info-event"]
                      "paths" [(temp-dir-path "rgall")]
                      "include" ["*.clj"]})]
      ;; OR: every line mentioning EITHER term is a hit.
      (expect (= ["(defn info-event [x] x)" "(defn other [x] x)" "info-event call"]
                (mapv :text (:hits out))))))

  (it "spec {:any [...]} is explicit OR"
    (let [_    (write-temp! "rgany/a.clj" "alpha\nbeta\ngamma\n")
          grep (private-fn "rg-search")
          out  (grep {"any" ["alpha" "gamma"]
                      "paths" [(temp-dir-path "rgany")]
                      "include" ["*.clj"]})]
      (expect (= ["alpha" "gamma"] (mapv :text (:hits out))))))

  (it "accepts path vectors, include globs, and dedups overlapping roots"
    (let [root (temp-dir-path "rgpaths")
          _    (write-temp! "rgpaths/src/a.clj" "needle clj\n")
          _    (write-temp! "rgpaths/src/a.txt" "needle txt\n")
          _    (write-temp! "rgpaths/test/b.cljc" "needle cljc\n")
          grep (private-fn "rg-search")
          out  (grep {"all" ["needle"]
                      "paths" [root (str root "/src")]
                      "include" ["*.clj" "*.cljc"]})]
      (expect (= ["needle clj" "needle cljc"]
                (mapv :text (:hits out))))))

  (it "private grep and public rg use the same single spec-map grammar"
    (let [_ (write-temp! "rgsame/a.clj" "needle same\n")
          spec {"all" ["needle"]
                "paths" [(temp-dir-path "rgsame")]
                "include" ["*.clj"]}
          grep (private-fn "rg-search")
          rg (private-fn "rg-tool")
          ;; rg-tool groups grep's flat :hits into :matches — an ordered
          ;; {path -> {anchor -> text}} map (LinkedHashMap) on the
          ;; model-facing :result; there is no flat :hits vec anymore.
          rg-env    (rg spec)
          rg-result (:result rg-env)
          grep-hits (:hits (grep spec))]
      (expect (= :rg (:symbol rg-env)))
      (expect (instance? java.util.Map (get rg-result "matches")))
      (expect (= (count grep-hits) (get rg-result "hit_count")))
      (expect (= (count (distinct (map :path grep-hits)))
                (get rg-result "file_count")))
      ;; NO `"spec"` echo in the model-facing payload: echoing the input map
      ;; back taught models a phantom "spec" INPUT key (`rg({..., "spec": {}})`).
      (expect (not (contains? rg-result "spec")))))

  (it "IGNORES unknown spec keys (forgiving) but still requires a query"
    (let [grep (private-fn "rg-search")
          rg (private-fn "rg-tool")]
      ;; The private ENGINE (`rg-search`) still takes ONE spec map — a bare
      ;; positional string is not a map, so it throws :invalid-rg-spec.
      (expect (throws? clojure.lang.ExceptionInfo
                #(grep "needle")))
      ;; The public rg now ACCEPTS a positional query + an options map:
      ;; rg("x", {opts}) folds :query in and runs (no arity error).
      (let [_   (write-temp! "rgposopts/a.clj" "needle here\n")
            env (rg "needle" {"paths" [(temp-dir-path "rgposopts")] "include" ["*.clj"]})]
        (expect (= :rg (:symbol env)))
        (expect (= 1 (get (:result env) "hit_count"))))
      ;; UNKNOWN keys are now IGNORED, not fatal — a model that tosses in a stray
      ;; annotation (e.g. `all_note: "defs"`, or an invented `type`/`spec`) still
      ;; gets its search instead of wasting the whole turn. Only recognised keys
      ;; are read; the rest are dropped.
      (let [_   (write-temp! "rglenient/a.txt" "needle here\nsecond needle")
            out (grep {"any" ["needle"]
                       "paths" [(temp-dir-path "rglenient")]
                       "all_note" "defs"
                       "type" :clj
                       "spec" {}})]
        (expect (map? out))
        (expect (contains? out :hits))
        (expect (pos? (count (:hits out)))))
      ;; ...but the all/any exactly-one grammar IS still enforced: a TYPO'd needle
      ;; key (so neither :all nor :any is present) is caught, not silently run.
      (let [err (try
                  (grep {"anyy" ["needle"] "paths" ["."]})
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? err))
        (expect (= :ext.foundation.editing/invalid-rg-spec (:type (ex-data err)))))))

  (it ":truncated-by :limit when results exceed the configured limit (default 250)"
    ;; Limit bumped 50 -> 250 in the rg sweep. Use 300 hits to force the cap.
    (let [_ (write-temp! "rgcap/a.txt"
              (string/join "\n" (map #(str "needle " %) (range 300))))
          grep (private-fn "rg-search")
          out  (grep {"all" ["needle"] "paths" [(temp-dir-path "rgcap")]})]
      (expect (= 250 (count (:hits out))))
      (expect (= :limit (:truncated-by out)))))

  (it "empty result still has :truncated-by :end-of-results, never nil"
    (let [_ (write-temp! "rgmiss/a.txt" "nothing matches in here\n")
          grep (private-fn "rg-search")
          out  (grep {"all" ["definitely-not-present"] "paths" [(temp-dir-path "rgmiss")]})]
      (expect (= [] (:hits out)))
      (expect (= :end-of-results (:truncated-by out)))))

  ;; Q1+Q2+Q3+Q4 — new option coverage.

  (it ":context N adds N symmetric context lines around each hit"
    (let [_path (write-temp! "rgctxa/a.txt" "L1\nL2\nMATCH\nL4\nL5\n")
          grep (private-fn "rg-search")
          out  (grep {"all" ["MATCH"]
                      "paths" [(temp-dir-path "rgctxa")]
                      "context" 2})
          h (first (:hits out))]
      (expect (= [[1 "L1"] [2 "L2"]] (:before h)))
      (expect (= [[4 "L4"] [5 "L5"]] (:after  h)))))

  (it ":is_files_only returns distinct paths and never line-level hits"
    (let [_ (write-temp! "rgfo/src/a.py" "alpha\nalpha\nalpha\n")
          _ (write-temp! "rgfo/src/b.py" "alpha\n")
          _ (write-temp! "rgfo/src/c.py" "no match\n")
          grep (private-fn "rg-search")
          out  (grep {"all" ["alpha"]
                      "paths" [(temp-dir-path "rgfo")]
                      "is_files_only" true})]
      (expect (= #{:files :truncated-by} (set (keys out))))
      (expect (= 2 (count (:files out))))
      (expect (every? string? (:files out)))))

  (it ":context is IGNORED (not rejected) in :is_files_only mode"
      ;; A stray `context` alongside `is_files_only` is harmless — content-mode
      ;; context has no meaning when returning bare file paths, so honor files-only
      ;; instead of hard-failing the whole call.
    (let [grep (private-fn "rg-search")
          out  (grep {"any" ["alpha"]
                      "paths" [(temp-dir-path "rgfo")]
                      "is_files_only" true
                      "context" 2})]
      (expect (= #{:files :truncated-by} (set (keys out))))
      (expect (every? string? (:files out)))))

  (it "keeps a long hit line FULL in the result value (no per-line mutilation)"
    ;; rg never mutilates a hit line. The full :text lives in the result value —
    ;; pickled into `r[\"tN/iN/fN\"]` and rebound into the sandbox — so the model
    ;; recovers the tail with `r[...][\"hits\"][i][\"text\"][N:]` in Python, no `cat`
    ;; roundtrip. Only the WIRE view is bounded (64KB per-observation clip), and
    ;; that clip is non-destructive (it points back to r[...]).
    (let [huge (apply str (repeat 1000 "x"))
          line (str "NEEDLE " huge)             ; 1007 chars
          _ (write-temp! "rgfull/big.txt" (str line "\n"))
          grep (private-fn "rg-search")
          out  (grep {"all" ["NEEDLE"] "paths" [(temp-dir-path "rgfull")]})
          text (:text (first (:hits out)))]
      (expect (= line text))                                   ; verbatim, full length
      (expect (= (count line) (count text)))
      (expect (not (string/includes? text "clipped"))))))

(defdescribe thin-bbfs-wrapper-test
  ;; patch-safe returns a STRUCTURED MAP and never throws on "normal"
  ;; failure paths (anchor-not-found / hashline-out-of-range / stale /
  ;; file-not-found / path-escape / etc.). Throws are reserved for genuinely
  ;; unexpected programming errors (a missing :from_anchor, an unknown key).
  (it ":expected_mtime guards against editing a file that changed since it was read"
    (let [patch (private-fn "patch-safe")
          p (write-temp! "bbfs/patch-stale.txt" "alpha\n")
          stale-mtime (- (.lastModified (fs/file p)) 100000)
          r (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "BETA"
                     "expected_mtime" stale-mtime}])]
      (expect (false? (:success? r)))
      (expect (= :stale (-> r :failures first :reason)))
      (expect (= "alpha\n" (slurp p)))))

  (it "unknown edit keys are rejected (typo guard)"
    (let [patch (private-fn "patch-safe")
          p (write-temp! "bbfs/patch-unknown.txt" "x\n")
          err (try (patch [{"path" p "from_anchor" (patch/line-anchor 1 "x") "replace" "y" "occurence" 1}])
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
          run! (fn [] (patch [{"path" p "from_anchor" (patch/line-anchor 9 "NOT_HERE") "replace" "x"}]))]
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
      (patch [{"path" p "from_anchor" (patch/line-anchor 9 "NOT_HERE") "replace" "x"}])
      (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "BETA"}])
      (let [counts2 @(deref (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "patch-fail-counts")))]
        (expect (nil? (get counts2 (.getAbsolutePath file)))))))

  (it "all-or-nothing: a single failing edit aborts every prior edit in the batch"
    ;; This guards the core safety invariant. Earlier edits that
    ;; "would have" succeeded against the in-memory plan must NOT
    ;; touch disk when any later edit fails.
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-aon.txt" "alpha\nbeta\n")
          r     (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}
                        {"path" p "from_anchor" (patch/line-anchor 9 "NEVER_MATCHES") "replace" "x"}])]
      (expect (false? (:success? r)))
      (expect (= "alpha\nbeta\n" (slurp p)))))

  (it "batch edits resolve against the ORIGINAL snapshot, not each other's output"
    ;; Every hunk anchors to the file as the model last read it (never
    ;; cumulatively), so hashline/ordinal anchors and line numbers stay valid
    ;; across a multi-edit batch. A hunk that targets a PRIOR hunk's output
    ;; therefore won't match the original and the whole batch fails atomically.
    ;; Two INDEPENDENT edits against the original both apply, in one plan.
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-seq2.txt" "alpha\nbeta\n")
          r     (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}
                        {"path" p "from_anchor" (patch/line-anchor 2 "beta") "replace" "BETA"}])]
      (expect (true? (:success? r)))
      (expect (= "ALPHA\nBETA\n" (slurp p)))
      (expect (= 1 (count (:plans r))))
      (expect (= "alpha\nbeta\n" (-> r :plans first :before)))
      (expect (= "ALPHA\nBETA\n" (-> r :plans first :after)))))

  (it "grouped same-file patch map applies several edits without repeating :path"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-grouped.txt" "alpha\nbeta\ngamma\n")
          r     (patch {"path" p
                        "edits" [{"from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}
                                 {"from_anchor" (patch/line-anchor 2 "beta") "replace" "BETA"}]})]
      (expect (true? (:success? r)))
      (expect (= "ALPHA\nBETA\ngamma\n" (slurp p)))
      (expect (= 1 (count (:plans r))))
      (expect (= "alpha\nbeta\ngamma\n" (-> r :plans first :before)))
      (expect (= "ALPHA\nBETA\ngamma\n" (-> r :plans first :after)))))

  (it "grouped same-file patch rejects per-edit :path and preserves all-or-nothing"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-grouped-bad.txt" "alpha\nbeta\n")
          err   (try (patch {"path" p "edits" [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}]})
                  nil (catch clojure.lang.ExceptionInfo e e))
          r     (patch {"path" p
                        "edits" [{"from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}
                                 {"from_anchor" (patch/line-anchor 9 "missing") "replace" "x"}]})]
      (expect (some? err))
      (expect (false? (:success? r)))
      (expect (= "alpha\nbeta\n" (slurp p)))))

  (it "editing an unknown path surfaces a structured :file-not-found failure"
    (let [patch (private-fn "patch-safe")
          fake-path "target/editing-test/bbfs/does-not-exist.txt"
          r (patch [{"path" fake-path "from_anchor" (patch/line-anchor 1 "x") "replace" "y"}])]
      (expect (false? (:success? r)))
      (expect (= :file-not-found (-> r :failures first :reason)))))

  (it ":expected_size guards independent of :expected_mtime"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "bbfs/patch-size.txt" "hello\n")
          r     (patch [{"path" p "from_anchor" (patch/line-anchor 1 "hello") "replace" "x"
                         "expected_size" 1}])]
      (expect (false? (:success? r)))
      (expect (= :stale (-> r :failures first :reason)))
      (expect (= :stale-size (-> r :failures first :stale :reason)))
      (expect (= "hello\n" (slurp p)))))

  (it "empty edit vector is a no-op success (no failures, no writes)"
    (let [patch (private-fn "patch-safe")
          r (patch [])]
      (expect (true? (:success? r)))
      (expect (= [] (:plans r)))))

  (it "a single patch invocation cannot move the loop counter past +1 per path"
    ;; Loop counter must be PER INVOCATION, not per failed edit. Two
    ;; failed edits in one call against the same path bump the counter
    ;; once, not twice.
    (let [patch (private-fn "patch-safe")
          clear (private-fn "clear-patch-fail-count!")
          p (write-temp! "bbfs/loop-once.txt" "alpha\n")
          file (fs/file p)]
      (clear file)
      (patch [{"path" p "from_anchor" (patch/line-anchor 7 "NOPE1") "replace" "x"}
              {"path" p "from_anchor" (patch/line-anchor 8 "NOPE2") "replace" "y"}])
      (let [r (patch [{"path" p "from_anchor" (patch/line-anchor 9 "NOPE3") "replace" "z"}])]
        ;; Failures came from two invocations -> counter is 2.
        (expect (= 2 (-> r :failures first :consecutive-failures)))
        (expect (nil? (:loop-hint r))))
      (clear file)))

  (it "patch reports :exact-replace as its only mode (envelope retired)"
    (let [patch-tool (private-fn "patch-tool")
          p (write-temp! "bbfs/dispatch-mode.txt" "alpha\nbeta\n")
          vec-out (-> (patch-tool [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "X"}])
                    :metadata :mode)]
      (expect (= :exact-replace vec-out))
      (expect (= "X\nbeta\n" (slurp p)))))

  (it "patch diagnostics report per-edit reasons in edit order and write nothing"
    (let [path  (write-temp! "bbfs/patch-diagnostics.txt" "alpha\nbeta\nbeta\n")
          patch (private-fn "patch-safe")
          ;; First 2 edits resolve cleanly against anchors; last 2 carry
          ;; out-of-range line anchors that cannot locate.
          r     (patch [{"path" path "from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}
                        {"path" path "from_anchor" (patch/line-anchor 2 "beta") "replace" "BETA"}
                        {"path" path "from_anchor" (patch/line-anchor 8 "missing") "replace" "x"}
                        {"path" path "from_anchor" (patch/line-anchor 9 "other") "replace" "y"}])
          checks (:checks r)
          failures (:failures r)]
      (expect (false? (:success? r)))
      ;; Every edit yields a check, in edit order.
      (expect (= [0 1 2 3] (mapv :edit-index checks)))
      ;; The last 2 anchors fail to locate.
      (expect (= [2 3] (mapv :edit-index failures)))
      ;; Each failure carries an observable :reason.
      (expect (every? (comp some? :reason) failures))
      ;; All-or-nothing still holds: zero writes when any edit fails.
      (expect (= "alpha\nbeta\nbeta\n" (slurp path)))))

  (it "exists? and delete-if-exists work on cwd-relative paths"
    (let [path             (write-temp! "bbfs/meta/x.txt" "x")
          exists?          (private-fn "exists-safe?")
          delete-if-exists (private-fn "delete-if-exists-safe")]
      (expect (true? (exists? path)))
      (expect (true? (delete-if-exists path)))
      (expect (false? (exists? path)))))

  (it "exists? tool returns a {:path :exists?} MAP, not a bare boolean (shape consistency)"
    ;; Regression (turn 4, iter 1→2):
    ;;   model wrote `(def ports (exists? \".nrepl-port\"))` then
    ;;   `(:exists? ports)`, expecting the map shape every other `v/*`
    ;;   tool returns. The old `exists-tool` returned a bare boolean
    ;;   so `(:exists? true)` evaluated to `nil` and the model burned
    ;;   an iter realizing \"exists? is returning true directly rather
    ;;   than a map containing an :exists? key\".
    ;; Fix: every v/* tool returns a MAP (not a bare boolean) so
    ;; `(:exists? r)` works; the result is self-describing by its fields.
    (let [present-path (write-temp! "exists-shape/yes.txt" "x")
          missing-path "exists-shape/no.txt"
          exists-tool  (private-fn "exists-tool")
          present (:result (exists-tool present-path))
          missing (:result (exists-tool missing-path))]
      (expect (map? present))
      (expect (true? (get present "exists")))
      (expect (= present-path (get present "path")))
      (expect (map? missing))
      (expect (false? (get missing "exists")))
      (expect (= missing-path (get missing "path")))))

  (it "keeps exists shape details out of the compact prompt and PYTHON in symbol docs"
    (let [exists-symbol (some #(when (= 'file-exists (:ext.symbol/symbol %)) %)
                          editing/editing-symbols)
          d (:ext.symbol/doc exists-symbol)]
      ;; the result shape lives in the symbol doc, not the compact prompt
      (expect (not (string/includes? editing/editing-prompt "\"exists\": bool")))
      ;; doc states the Python result shape — and NOT the old Clojure `:exists?`
      (expect (string/includes? d "\"exists\""))
      (expect (not (string/includes? d ":exists?")))))

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
      (expect (= :delete (:symbol envelope)))
      (let [r (:result envelope)]
        (expect (map? r))
        (expect (= p (get r "path")))
        (expect (true? (get r "deleted"))))))

  (it "delete-if-exists returns the same map shape with :deleted? reflecting the actual outcome"
    (let [delete-if (private-fn "delete-if-exists-tool")
          p         (write-temp! "delete-shape/here.txt" "x\n")
          present   (:result (delete-if p))
          absent    (:result (delete-if p))]
      ;; First call deletes the file; the result map carries "deleted" true.
      (expect (map? present))
      (expect (= p (get present "path")))
      (expect (true? (get present "deleted")))
      ;; Second call hits an already-absent path; the map stays the same shape.
      (expect (map? absent))
      (expect (= p (get absent "path")))
      (expect (false? (get absent "deleted"))))))

(defdescribe patch-summary-shape-test
  ;; The summary IS what the model reads back as the patch result
  ;; AND what the channel renderer projects. Every key counts; redundant
  ;; signal pollutes the iteration trailer.
  (it "anchor-located edit omits routine hashline pass, indent delta, and line counters"
    (let [patch (private-fn "patch-safe")
          summary (private-fn "patch-result-file-summary")
          p (write-temp! "summary/exact.txt" "alpha\nbeta\n")
          r (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}])
          s (summary (first (:plans r)))]
      (expect (true? (:success? r)))
      (expect (= #{"path" "op" "changed" "diff"} (set (keys s))))
      (expect (not (contains? s "passes")))
      (expect (not (contains? s "indent_delta")))
      (expect (not (contains? s "lines_before")))
      (expect (not (contains? s "lines_after")))
      (expect (not (contains? s "delta_lines"))))))

(defdescribe patch-diff-text-test
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
      (expect (string/includes? changed "diff truncated")))))

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
      ;; cat returns a plain map as :result. :lines is an ordered
      ;; anchor-map; convert back to tuples to assert; no deref, no
      ;; handle, no offset key.
      (expect (= :cat (:symbol out)))
      (let [r (:result out)]
        (expect (= [[1 "alpha"] [2 "beta"]] (patch/anchor-map->tuples (get r "anchors"))))
        (expect (nil? (get r "next_offset")))
        (expect (false? (get r "truncated"))))
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
  ;; patch by :from_anchor / :to_anchor. The hash anchors come straight from
  ;; the read's `:anchors` map (same value rendered in the cat gutter),
  ;; and self-locate against live disk content on apply.
  (it "patch :from_anchor replaces a single content-anchored line"
    (let [path (write-temp! "hashline/single.txt"
                 "alpha first\nbeta second\ngamma third\n")
          read-file (private-fn "read-file")
          patch (private-fn "patch-safe")
          hashes (:anchors (read-file path))
          h2 (get hashes 2)
          r (patch [{"path" path "from_anchor" h2 "replace" "BETA REPLACED"}])]
      (expect (true? (:success? r)))
      (expect (= "alpha first\nBETA REPLACED\ngamma third\n" (slurp path)))))

  (it "patch :from_anchor uses exact line+hash even when the hash is duplicated nearby"
    (let [path (write-temp! "hashline/duplicate-hash-exact-line.txt"
                 (str "keep\n"
                   "}\n"
                   "}\n"
                   "}\n"
                   "tail\n"))
          patch (private-fn "patch-safe")
          r (patch [{"path" path
                     "from_anchor" (patch/line-anchor 3 "}")
                     "replace" "TARGET"}])]
      (expect (true? (:success? r)))
      (expect (= "keep\n}\nTARGET\n}\ntail\n" (slurp path)))))

  (it "patch :from_anchor + :to_anchor replaces an inclusive range"
    (let [path (write-temp! "hashline/range.txt" "a\nb\nc\nd\n")
          read-file (private-fn "read-file")
          patch (private-fn "patch-safe")
          hashes (:anchors (read-file path))
          r (patch [{"path" path
                     "from_anchor" (get hashes 1)
                     "to_anchor" (get hashes 3)
                     "replace" "X"}])]
      (expect (true? (:success? r)))
      (expect (= "X\nd\n" (slurp path)))))

  (it "duplicate lines are surfaced as distinct `lineno:hash` anchors; a BARE hash that hits >1 line is refused"
    (let [path (write-temp! "hashline/dup.txt" "x\ny\nx\n")
          read-file (private-fn "read-file")
          patch (private-fn "patch-safe")
          ;; The dup line 'x' is surfaced as `1:hash` / `3:hash` — the line
          ;; number disambiguates, no `#N` ordinal needed. A BARE content hash
          ;; (no line number) carries only one coordinate, so it is refused
          ;; outright (`:hashline-malformed`) — the old bare-hash content
          ;; -uniqueness fallback is gone (it could land on the wrong dup line).
          hashes (:anchors (read-file path))
          r (patch [{"path" path "from_anchor" (patch/line-hash "x") "replace" "NEW"}])]
      (expect (= (patch/line-anchor 1 "x") (get hashes 1)))  ;; 1st dup → 1:hash
      (expect (= (patch/line-anchor 3 "x") (get hashes 3)))  ;; 2nd dup → 3:hash
      (expect (= (patch/line-anchor 2 "y") (get hashes 2)))  ;; unique line too
      (expect (false? (:success? r)))
      (expect (= :hashline-malformed (-> r :failures first :reason)))
      ;; file untouched
      (expect (= "x\ny\nx\n" (slurp path)))))

  (it "patch with ZERO locators still throws"
    (let [path (write-temp! "hashline/none.txt" "a\n")
          patch (private-fn "patch-safe")]
      (expect (throws? clojure.lang.ExceptionInfo
                #(patch [{"path" path "replace" "Z"}]))))))

(defdescribe vis-cat-anchor-read-test
  ;; cat :anchor — the READ twin of patch :from_anchor. Re-read a kept region
  ;; by its content hash, addressed by content not drifting line numbers.
  (let [cat-tool (private-fn "cat-tool")
        path     (write-temp! "hashread/probe.clj"
                   "(ns probe)\n(def alpha 1)\n(def beta 2)\n(def gamma 3)\n")
        h-beta   (patch/line-anchor 3 "(def beta 2)")
        h-gamma  (patch/line-anchor 4 "(def gamma 3)")]
    (it "(cat path :anchor H) reads the single line whose content hash is H"
      (let [out (:result (cat-tool path :anchor h-beta))]
        (expect (= [[3 "(def beta 2)"]] (patch/anchor-map->tuples (get out "anchors"))))
        (expect (= [3 3] (get out "range")))))
    (it "(cat path :anchor H1 H2) reads the inclusive content-addressed window"
      (let [out (:result (cat-tool path :anchor h-beta h-gamma))]
        (expect (= (numbered-tuples 3 ["(def beta 2)" "(def gamma 3)"]) (patch/anchor-map->tuples (get out "anchors"))))
        (expect (= [3 4] (get out "range")))))
    (it "addresses by CONTENT — survives line drift (prepend shifts numbers)"
      (let [p2 (write-temp! "hashread/drift.clj"
                 "(ns probe)\n(def beta 2)\n")
            _  (spit (fs/file p2) (str ";; banner\n;; banner2\n" (slurp (fs/file p2))))
            out (:result (cat-tool p2 :anchor (patch/line-anchor 2 "(def beta 2)")))]
        ;; beta moved from line 2 to line 4; within the drift tolerance the
        ;; `2:hash` anchor still resolves it by content
        (expect (= [[4 "(def beta 2)"]] (patch/anchor-map->tuples (get out "anchors"))))))
    (it "a missing hash throws back to cat for fresh :anchors"
      (expect (throws? clojure.lang.ExceptionInfo
                #(cat-tool path :anchor "zzzz"))))
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
          p     (write-temp! "ord/dup.txt" "x\nDUP\ny\nDUP\nz\nDUP\n")
          r     (patch [{"path" p "from_anchor" (patch/line-anchor 2 "DUP") "replace" "DUP1"}
                        {"path" p "from_anchor" (patch/line-anchor 6 "DUP") "replace" "DUP3"}])]
      (expect (true? (:success? r)))
      ;; lines 2 and 6 edited, line 4 untouched — line numbers resolve vs the
      ;; original snapshot, no drift.
      (expect (= "x\nDUP1\ny\nDUP\nz\nDUP3\n" (slurp p)))))

  (it "all three duplicate lines editable in one batch"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "ord/dup3.txt" "DUP\nDUP\nDUP\n")
          r     (patch [{"path" p "from_anchor" (patch/line-anchor 1 "DUP") "replace" "A"}
                        {"path" p "from_anchor" (patch/line-anchor 2 "DUP") "replace" "B"}
                        {"path" p "from_anchor" (patch/line-anchor 3 "DUP") "replace" "C"}])]
      (expect (true? (:success? r)))
      (expect (= "A\nB\nC\n" (slurp p)))))

  (it "overlapping edits in one batch are rejected — nothing written (atomic)"
    (let [patch (private-fn "patch-safe")
          p     (write-temp! "ord/over.txt" "alpha beta\n")
          r     (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha beta") "replace" "X"}
                        {"path" p "from_anchor" (patch/line-anchor 1 "alpha beta") "replace" "Y"}])]
      (expect (false? (:success? r)))
      (expect (= :overlapping-edits (-> r :failures last :reason)))
      (expect (= "alpha beta\n" (slurp p))))))

;; =============================================================================
;; rg hits carry the content-addressed :anchor anchor (cat/rg parity), so a hit
;; is directly patchable without a follow-up cat.
;; =============================================================================

(defdescribe rg-returns-anchor-test
  (let [rg-search (private-fn "rg-search")
        patch     (private-fn "patch-safe")]
    (it "a content hit carries its `lineno:hash` anchor and that anchor patches the line"
      (let [p   (write-temp! "rgh/uniq.clj" "(def a 1)\n(def b 2)\n(def c 3)\n")
            res (rg-search {"any" ["def b"] "paths" [p]})
            hit (first (:hits res))]
        (expect (= (patch/line-anchor 2 "(def b 2)") (:anchor hit)))
        (let [r (patch [{"path" p "from_anchor" (:anchor hit) "replace" "(def b 200)"}])]
          (expect (true? (:success? r)))
          (expect (string/includes? (slurp p) "(def b 200)")))))

    (it "hits on a DUPLICATED line carry distinct `lineno:hash` anchors (line number disambiguates)"
      (let [p   (write-temp! "rgh/dup.clj" "(def x 1)\n(other)\n(def x 1)\n")
            res (rg-search {"any" ["def x"] "paths" [p]})
            hashes (map :anchor (:hits res))]
        (expect (= [(patch/line-anchor 1 "(def x 1)") (patch/line-anchor 3 "(def x 1)")] hashes))))))

(defn- mk-tmp-dir [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix (make-array java.nio.file.attribute.FileAttribute 0)))))

(defdescribe multi-root-safe-path-test
  (it "accepts paths under a LIVE filesystem root (trunk==clone), rejects paths outside every root"
    (let [safe-path (private-fn "safe-path")
          primary   (.getCanonicalPath (java.io.File. (System/getProperty "user.dir")))
          ctx-root  (mk-tmp-dir "vis-ctxroot")]
      (binding [workspace/*workspace-root* primary
                workspace/*filesystem-roots*  [{:trunk ctx-root :clone ctx-root}]]
        (expect (string/starts-with? (.getPath ^java.io.File (safe-path "deps.edn")) primary))
        (expect (string/starts-with?
                  (.getPath ^java.io.File (safe-path (str ctx-root "/sub/file.clj")))
                  ctx-root))
        (expect (throws? clojure.lang.ExceptionInfo #(safe-path "/etc/hosts")))
        (expect (throws? clojure.lang.ExceptionInfo
                  #(safe-path (str ctx-root "/../../../../etc/hosts"))))
        (binding [workspace/*filesystem-roots* nil]
          (expect (throws? clojure.lang.ExceptionInfo
                    #(safe-path (str ctx-root "/x"))))))))

  (it "ISOLATED filesystem root: address by trunk → edits land in clone, display shows trunk"
    (let [safe-path (private-fn "safe-path")
          rel-path  (private-fn "rel-path")
          primary   (mk-tmp-dir "vis-prim")
          trunk     (mk-tmp-dir "vis-trunk")
          clone     (mk-tmp-dir "vis-clone")]
      (spit (java.io.File. clone "x.txt") "in-clone")
      (spit (java.io.File. trunk "x.txt") "in-trunk")
      (binding [workspace/*workspace-root* primary
                workspace/*filesystem-roots*  [{:trunk trunk :clone clone}]]
        (let [f (safe-path (str trunk "/x.txt"))]
          (expect (string/starts-with? (.getCanonicalPath ^java.io.File f) clone)) ;; lands in clone
          (expect (= "in-clone" (slurp f)))                                        ;; reads clone, NOT trunk
          (expect (= (.replace (str trunk "/x.txt") "\\" "/") (rel-path f)))) ;; display shows real trunk path, `/`-normalized
        (expect (throws? clojure.lang.ExceptionInfo #(safe-path "/etc/hosts")))))))

;; ---------------------------------------------------------------------------
;; patch is ANCHOR-ONLY — the removed `search`/`replace`/`nth` text-matcher API
;; keeps getting re-hallucinated by models (the prompt used to teach it). These
;; guard BOTH the runtime rejection AND the prompt that seeded the mistake.
;; ---------------------------------------------------------------------------
(defdescribe patch-anchor-only-test
  (let [coerce (private-fn "coerce-patch-edits")]
    (it "accepts a valid anchor edit (from_anchor + replace)"
      (let [out (coerce [{"path" "p.txt" "from_anchor" "12:abc" "replace" "new"}])]
        (expect (= 1 (count out)))
        (expect (= "12:abc" (get (first out) "from_anchor")))))
    (it "rejects the removed `search` key with an ANCHOR-ONLY message naming it"
      (let [ex (try (coerce [{"path" "p.txt" "search" "old" "replace" "new"}])
                 nil (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? ex))
        (expect (string/includes? (ex-message ex) "ANCHOR-ONLY"))
        (expect (string/includes? (ex-message ex) "from_anchor"))
        (expect (= ["search"] (:removed (ex-data ex))))))
    (it "rejects the removed `nth` key too"
      (expect (= :threw (try (coerce [{"path" "p.txt" "from_anchor" "1:a" "replace" "x" "nth" "all"}])
                          :no-throw (catch clojure.lang.ExceptionInfo _ :threw)))))
    (it "still rejects an edit with no locator (generic from_anchor error)"
      (expect (throws? clojure.lang.ExceptionInfo
                #(coerce [{"path" "p.txt" "replace" "x"}]))))))

(defdescribe editing-prompt-no-stale-api-test
  (let [prompt (editing/available-editing-prompt)]
    (it "teaches patch as ANCHOR-ONLY via from_anchor"
      (expect (string/includes? prompt "ANCHOR-ONLY"))
      (expect (string/includes? prompt "from_anchor")))

    (it "does NOT present the removed search/replace patch examples"
      (expect (not (string/includes? prompt "edits\": [{\"search")))
      (expect (not (string/includes? prompt "\"nth\": \"all"))))
    (it "does NOT teach reusing stale anchors as facts"
      (expect (not (string/includes? prompt "Re-patch by from_anchor")))
      (expect (string/includes? prompt "STALE after ANY write")))
    (it "describes cat's result as `anchors`, not a `lines` key"
      (expect (string/includes? prompt "anchors"))
      (expect (not (string/includes? prompt "cat(P)[\"lines\"]")))
      (expect (not (string/includes? prompt "[[lineno, text]"))))
    (it "teaches anchored edits via cat's lineno:hash → from_anchor"
      (expect (string/includes? prompt "lineno:hash"))
      (expect (string/includes? prompt "from_anchor")))))

(defdescribe outline-path-resolution-test
  "Regression: outline must route through safe-path like every other file tool —
   it used the RAW path (slurp resolves against the JVM user.dir, not the
   workspace cwd), so a nested `src/foo.clj` 404'd under `vis --source` while cat
   found it. The proof is that safe-path confinement now applies to outline."
  (let [outline-tool (private-fn "outline-tool")]
    (it "resolves a NESTED workspace-relative path"
      (let [dir (temp-dir-path "outline-nested/src")
            _   (spit (fs/file (str dir "/foo.clj")) "(ns foo)\n(defn bar [x] (+ x 1))\n")
            r   (outline-tool (str (temp-root) "/outline-nested/src/foo.clj"))]
        (expect (:success? r))
        (expect (clojure.string/includes? (str (get-in r [:result "skeleton"])) "bar"))))
    (it "REFUSES a path that escapes the workspace (proves safe-path confinement)"
      (expect (true? (try (outline-tool "/etc/hosts") false
                       (catch clojure.lang.ExceptionInfo _ true)))))))

(defdescribe project-rename-test
  "Cross-file rename via tree-sitter. For a Clojure ns it rewrites the ns form +
   :require targets + qualified usages, keeps local :as aliases. (rg prefilter is
   redef'd to a known file set so the test doesn't depend on gitignore.)"
  (let [rename-tool (private-fn "symbol-rename-tool")]
    (it "renames a Clojure namespace across files"
      (let [_  (temp-dir-path "nsrename")
            f1 (str (temp-root) "/nsrename/bar.clj")
            f2 (str (temp-root) "/nsrename/app.clj")]
        (spit (fs/file f1) "(ns foo.bar)\n(defn h [x] (inc x))\n")
        (spit (fs/file f2) "(ns app\n  (:require [foo.bar :as fb]))\n(defn run [] (+ (foo.bar/h 1) (fb/h 2)))\n")
        (with-redefs [editing/rg-search (constantly {:files [f1 f2]})]
          (let [r (rename-tool "foo.bar" "foo.baz")]
            (expect (:success? r))
            (expect (= 2 (get-in r [:result "file_count"])))))
        (let [a (slurp (fs/file f1)) b (slurp (fs/file f2))]
          (expect (clojure.string/includes? a "(ns foo.baz)"))
          (expect (clojure.string/includes? b "[foo.baz :as fb]"))    ; require target renamed
          (expect (clojure.string/includes? b "foo.baz/h 1"))         ; qualified usage renamed
          (expect (clojure.string/includes? b "fb/h 2"))              ; local alias UNCHANGED
          (expect (not (clojure.string/includes? b "foo.bar"))))))    ; nothing left
    (it "skips files that don't mention the name (file_count reflects only changes)"
      (let [_  (temp-dir-path "nsrename2")
            f1 (str (temp-root) "/nsrename2/has.clj")
            f2 (str (temp-root) "/nsrename2/none.clj")]
        (spit (fs/file f1) "(ns has)\n(zz/q 1)\n")
        (spit (fs/file f2) "(ns none)\n(defn k [] 1)\n")
        (with-redefs [editing/rg-search (constantly {:files [f1 f2]})]
          (let [r (rename-tool "zz" "ww")]
            (expect (= 1 (get-in r [:result "file_count"])))))
        (expect (clojure.string/includes? (slurp (fs/file f1)) "ww/q"))
        (expect (= "(ns none)\n(defn k [] 1)\n" (slurp (fs/file f2))))))))

(defdescribe render-rg-result-modes-test
  ;; Regression: a files-only / counts rg used the content-mode renderer, which
  ;; reads `:hit_count`/`:matches` → "0 hits in 193 files" with NO body, even
  ;; though the matching FILES were right there in `:files`. Each mode now renders
  ;; its OWN shape. `r` arrives with snake_case wire keys.
  (let [render @#'editing/render-rg-result]
    (it "files-only: summary counts FILES (not 0 hits) and lists the matching paths"
      (let [card (render {"files" ["src/a.clj" "src/b.clj" "src/c.clj"] "file_count" 3})]
        (expect (= "3 files" (:summary card)))
        (expect (clojure.string/includes? (:body card) "src/a.clj"))
        (expect (clojure.string/includes? (:body card) "src/c.clj"))
        (expect (not (clojure.string/includes? (:summary card) "hit")))))
    (it "files-only: a single matching file reads `1 file`"
      (expect (= "1 file" (:summary (render {"files" ["only.clj"] "file_count" 1})))))
    (it "content mode is unchanged: `N hits in M files`"
      (let [card (render {"matches" {"x.clj" {"1:abc" "line one"}} "hit_count" 1 "file_count" 1})]
        (expect (= "1 hit in 1 file" (:summary card)))))))

(defdescribe render-cat-result-spans-test
  ;; Regression (session 128cefd8): two adjacent ranged reads of the SAME file
  ;; rendered near-identical cards — `app.css · 60 lines` then `app.css ·
  ;; 266 lines` — because the summary carried no line-span info. The headline
  ;; now says WHICH lines were read.
  (let [render @#'editing/render-cat-result]
    (it "single contiguous range: `L<a>-<b>`, count implied"
      (let [card (render {"path" "app.css"
                          "anchors" {"1:aa" "x" "2:bb" "y" "3:cc" "z"}})]
        (expect (= "`app.css` · L1-3" (:summary card)))
        (expect (clojure.string/includes? (:body card) "    1  x"))))
    (it "single line: bare `L<n>`"
      (expect (= "`app.css` · L42"
                (:summary (render {"path" "app.css" "anchors" {"42:ff" "q"}})))))
    (it "multiple ranges: overall extent + run count + line total"
      (expect (= "`app.css` · L1-371 (2 ranges) · 4 lines"
                (:summary (render {"path" "app.css"
                                   "anchors" {"1:aa" "a" "2:bb" "b"
                                              "370:cc" "c" "371:dd" "d"}})))))
    (it "spans derive from SORTED line numbers, not map iteration order"
      (expect (= "`app.css` · L5-7"
                (:summary (render {"path" "app.css"
                                   "anchors" {"7:cc" "c" "5:aa" "a" "6:bb" "b"}})))))
    (it "unparseable anchor key degrades to the count-only summary (total, never throws)"
      (expect (= "`app.css` · 1 line"
                (:summary (render {"path" "app.css" "anchors" {"garbage" "g"}})))))
    (it "empty anchors: `0 lines`, no body"
      (let [card (render {"path" "app.css" "anchors" {}})]
        (expect (= "`app.css` · 0 lines" (:summary card)))
        (expect (nil? (:body card)))))))

;; ── e2e: REAL tool invocations against REAL temp files ───────────────────────

(defdescribe occurrences-tool-e2e-test
  "The `occurrences` TOOL (not just the structural fn): rg prefilter → per-file
   parse → def-marked result envelope, over real files on disk."
  (let [occ (private-fn "occurrences-tool")]
    (it "traces a symbol across real files: marks the def (kind/signature), lists uses"
      (let [_  (temp-dir-path "occ")
            f1 (str (temp-root) "/occ/lib.clj")
            f2 (str (temp-root) "/occ/use.clj")]
        (spit (fs/file f1) "(defn widget [x] (inc x))\n")
        (spit (fs/file f2) "(ns u)\n(println (widget 1))\n(println (widget 2))\n")
        (with-redefs [editing/rg-search (constantly {:files [f1 f2]})]
          (let [r   (occ "widget")
                res (:result r)
                all (mapcat #(get % "occurrences") (get res "files"))
                defs (filter #(get % "is_definition") all)]
            (expect (:success? r))
            (expect (= 3 (get res "count")))            ;; 1 def + 2 uses
            (expect (= 1 (get res "definition_count")))
            (expect (= 1 (count defs)))
            (expect (= "function" (get (first defs) "kind")))
            (expect (= "[x]" (get (first defs) "signature")))
              ;; every non-def occurrence still carries a patch anchor
            (expect (every? #(get % "anchor") all))
              ;; a plain USE is ANCHORS-ONLY — the `lineno:hash` anchor IS the sole
              ;; position; no redundant line/column/byte (unbounded, they'd bloat the
              ;; wire until it clips mid-object)
            (let [use (first (remove #(get % "is_definition") all))]
              (expect (= #{"anchor"} (set (keys use)))))
            (expect (not-any? #(or (contains? % "line")
                                 (contains? % "column")
                                 (contains? % "start_byte")
                                 (contains? % "end_byte"))
                      all))))))
    (it "a name with no definition on disk yields definition_count 0"
      (let [_  (temp-dir-path "occ2")
            f  (str (temp-root) "/occ2/u.clj")]
        (spit (fs/file f) "(ns u)\n(println (widget 1))\n")   ;; used, never defined here
        (with-redefs [editing/rg-search (constantly {:files [f]})]
          (let [res (:result (occ "widget"))]
            (expect (= 0 (get res "definition_count")))
            (expect (pos? (get res "count")))))))))

(defdescribe outline-tool-e2e-test
  "The `outline` TOOL over a real file — positional AND the dict form the native
   tool-call path synthesizes (`outline({\"path\": …})`)."
  (let [outline (private-fn "outline-tool")]
    (it "positional and dict forms both return the same skeleton"
      (let [_ (temp-dir-path "outl")
            f (str (temp-root) "/outl/m.clj")]
        (spit (fs/file f) "(defn add [a b] (+ a b))\n(defn sub [a b] (- a b))\n")
        (let [r1 (outline f)             ;; outline("m.clj")
              r2 (outline {"path" f})]   ;; outline({"path": "m.clj"}) — native shape
          (expect (:success? r1))
          (expect (:success? r2))
          (expect (clojure.string/includes? (get-in r1 [:result "skeleton"]) "add"))
          (expect (clojure.string/includes? (get-in r1 [:result "skeleton"]) "sub"))
          (expect (= (get-in r1 [:result "skeleton"])
                    (get-in r2 [:result "skeleton"]))))))))

(defdescribe rg-tool-e2e-test
  "The `rg` TOOL over real files: the comma-split + smart-case fixes end-to-end."
  (let [rg (private-fn "rg-tool")]
    (it "a comma query matches EITHER term (the session 71a69809 fix, real files)"
      (let [d (temp-dir-path "rge")
            f (str (temp-root) "/rge/a.clj")]
        (spit (fs/file f) "the model line\nthe cycle line\nunrelated\n")
        (let [r (rg "model, cycle" {"paths" [d]})]
          (expect (:success? r))
          (expect (= 2 (get-in r [:result "hit_count"]))))))   ;; both lines, not 0
    (it "smart-case: a lowercase query matches any case, on disk"
      (let [d (temp-dir-path "rgc")
            f (str (temp-root) "/rgc/a.clj")]
        (spit (fs/file f) "Keymap here\nkeystroke too\nnope\n")
        (let [r (rg "key" {"paths" [d]})]
          (expect (= 2 (get-in r [:result "hit_count"]))))))  ;; Keymap + keystroke
    (it "a MISSING path in the list is SKIPPED, not a hard error (vis.edn case)"
      (let [d (temp-dir-path "rgp")
            f (str (temp-root) "/rgp/a.clj")]
        (spit (fs/file f) "needle here\n")
          ;; one real dir + one path that does not exist → search the real one
        (let [r (rg "needle" {"paths" [d (str (temp-root) "/rgp/nope.edn")]})]
          (expect (:success? r))
          (expect (= 1 (get-in r [:result "hit_count"]))))))
    (it "when NONE of the paths exist, it errors clearly"
      (expect (throws? clojure.lang.ExceptionInfo
                #(rg "x" {"paths" [(str (temp-root) "/none1.edn")
                                   (str (temp-root) "/none2.edn")]}))))))

(defdescribe struct-patch-tool-e2e-test
  "struct_patch LENIENCY over real files: `delete` a def by name, and `replace_node`
   given a `target` but no `match` falling back to the name-based `replace` the model
   meant (instead of failing with 'replaceNode requires both match and code')."
  (let [sp (private-fn "struct-patch-tool")]
    (it "op delete drops the named def; the sibling survives"
      (let [_ (temp-dir-path "spd")
            f (str (temp-root) "/spd/m.clj")]
        (spit (fs/file f) "(defn keep-me [x] (inc x))\n(defn drop-me [y] (dec y))\n")
        (let [r (sp {"path" f "op" "delete" "target" "drop-me"})]
          (expect (:success? r))
          (let [src (slurp (fs/file f))]
            (expect (clojure.string/includes? src "keep-me"))
            (expect (not (clojure.string/includes? src "drop-me")))))))
    (it "replace_node with a target but no match = a name-based replace (not an error)"
      (let [_ (temp-dir-path "spr")
            f (str (temp-root) "/spr/m.clj")]
        (spit (fs/file f) "(defn foo [x] (inc x))\n")
        (let [r (sp {"path" f "op" "replace_node" "target" "foo"
                     "code" "(defn foo [x] (* 2 x))"})]
          (expect (:success? r))
          (expect (clojure.string/includes? (slurp (fs/file f)) "(* 2 x)")))))))

(defdescribe patch-syntax-guard-test
  "patch RE-PARSES the result and REFUSES an edit that turns CLEANLY-parsing code
   into broken code (parity with struct_patch). The guard compares before→after, so
   prose/markup that parses WITH error nodes under its grammar (`.txt` → tree-sitter
   `vimdoc`) is never blocked."
  (let [patch (private-fn "patch-safe")]
    (it "an edit that breaks Clojure syntax is refused — nothing written"
      (let [p (write-temp! "guard/ok.clj" "(defn add [a b] (+ a b))\n")
            r (patch [{"path" p
                       "from_anchor" (patch/line-anchor 1 "(defn add [a b] (+ a b))")
                       "replace" "(defn add [a b] (+ a b"}])]      ;; unbalanced → broken
        (expect (false? (:success? r)))
        (expect (= :syntax-error (:reason (first (:failures r)))))
          ;; The syntax-error failure carries a precomputed :message — the surfaced
          ;; summary must SHOW it, not flatten it to the generic "edit N in P failed."
          ;; (explain-failure used to drop :message because :syntax-error is not one
          ;; of the anchor-resolution `reason`s it case-matches on).
        (expect (string/includes? (:message r) "SYNTAX ERROR"))
        (expect (not (string/includes? (:message r) "failed.")))
          ;; The refusal carries the WHOLE-BATCH candidates so a language
          ;; pack's :around op-hook (e.g. the Clojure pack's parinfer rescue)
          ;; can whole-source-repair the broken files and commit the batch —
          ;; fragment repair can't fix contextual imbalance.
        (expect (= [p] (:broken-paths r)))
        (expect (= 1 (count (:candidate-plans r))))
        (expect (string/includes? (:after (first (:candidate-plans r)))
                  "(+ a b"))
        (expect (= "(defn add [a b] (+ a b))\n" (slurp p)))))    ;; untouched
    (it "a valid Clojure edit still applies"
      (let [p (write-temp! "guard/ok2.clj" "(defn add [a b] (+ a b))\n")
            r (patch [{"path" p
                       "from_anchor" (patch/line-anchor 1 "(defn add [a b] (+ a b))")
                       "replace" "(defn add [a b] (* a b))"}])]
        (expect (true? (:success? r)))
        (expect (= "(defn add [a b] (* a b))\n" (slurp p)))))
    (it "prose (.txt → vimdoc parses WITH error nodes) is NEVER blocked"
      (let [p (write-temp! "guard/notes.txt" "hello world\nsome notes\n")
            r (patch [{"path" p
                       "from_anchor" (patch/line-anchor 1 "hello world")
                       "replace" "hello there"}])]
        (expect (true? (:success? r)))
        (expect (= "hello there\nsome notes\n" (slurp p)))))
    (it "a strict config (JSON) is guarded too — breaking its syntax is refused"
      (let [p (write-temp! "guard/conf.json" "{\"a\": 1}\n")
            r (patch [{"path" p
                       "from_anchor" (patch/line-anchor 1 "{\"a\": 1}")
                       "replace" "{\"a\": 1"}])]                    ;; missing close → broken
        (expect (false? (:success? r)))
        (expect (= :syntax-error (:reason (first (:failures r)))))
        (expect (= "{\"a\": 1}\n" (slurp p)))))))

(defdescribe patch-multi-failure-message-test
  "A multi-edit patch that fails reports EVERY failing edit — not just `first:` —
   so the model sees the LATER edit that's the real problem, not only edit 0."
  (let [patch (private-fn "patch-safe")]
    (it "lists all failing edits (edit 0 AND edit 1), not just the first"
      (let [p   (write-temp! "pmf/a.txt" "alpha\nbeta\ngamma\n")
            r   (patch [{"path" p "from_anchor" (patch/line-anchor 1 "WRONGLINE") "replace" "x"}
                        {"path" p "from_anchor" (patch/line-anchor 3 "ALSOWRONG") "replace" "y"}])
            msg (:message r)]
        (expect (false? (:success? r)))
        (expect (= 2 (count (:failures r))))
        (expect (string/includes? msg "2 edits failed"))
        (expect (string/includes? msg "edit 0"))
        (expect (string/includes? msg "edit 1"))))))   ;; the later edit is visible now

(defdescribe find-files-op-name-test
  "Regression: renaming find→find_files means the result `:op` must stay in lockstep
   with the symbol name — `op-tag` keys the observation/mutation registry by the wire
   name, so a mismatch throws `Unregistered extension op :find`."
  (it "the find_files symbol IS named find_files"
    (expect (= 'find_files (:ext.symbol/symbol editing/find-symbol))))
  (it "find_files carries an observation tag (registry-resolvable)"
    (expect (= :observation (:ext.symbol/tag editing/find-symbol)))))

(defdescribe find-relevance-filter-test
  "Regression: fff's native matcher returns a full page of loose subsequence
   matches with no score (query \"lmstudio\" alone hit 108/489 unrelated paths).
   find-search must post-filter fff's candidates by per-token relevance so only
   genuine hits survive — while staying typo-tolerant and word-order-insensitive."
  (let [relevance (private-fn "find-relevance")
        min-score (private-fn "find-min-score")
        find-search (private-fn "find-search")]
    (it "scores a genuine filename hit far above scattered subsequence noise"
      (let [genuine (relevance "lmstudio" "a/b/provider_lmstudio.clj")
            noise   (relevance "lmstudio" "extensions/common/foundation_git/src/merge_ops.clj")]
        (expect (>= genuine min-score))
        (expect (< noise min-score))
        (expect (> genuine noise))))
    (it "is word-order-INSENSITIVE across tokens (matches fff's multi-token intent)"
      (doseq [q ["core editing" "editing core"]]
        (expect (>= (relevance q "src/foundation/editing/core.clj") min-score)))
        ;; blank / all-separator queries never score
      (expect (= 0.0 (relevance "" "anything/at/all.clj")))
      (expect (= 0.0 (relevance "   " "anything/at/all.clj"))))
    (it "tolerates a typo (dropped char) in the query"
        ;; "wrkspace" is a subsequence of "workspace" — tight window, kept.
      (expect (>= (relevance "wrkspace" "src/internal/workspace.clj") min-score)))
    (it "find-search returns only genuine hits and drops the fuzzy padding"
      (let [_   (write-temp! "findrel/provider_lmstudio.clj" ";; genuine\n")
            _   (write-temp! "findrel/provider_openai.clj" ";; noise\n")
            _   (write-temp! "findrel/foundation_voice_asr.clj" ";; noise\n")
            _   (write-temp! "findrel/foundation_git_merge_ops.clj" ";; noise\n")
            dir (temp-dir-path "findrel")
            out (find-search [{"query" "lmstudio" "paths" [dir]}])
            names (set (map #(last (string/split % #"/")) (get out "paths")))]
          ;; the genuine file is found
        (expect (contains? names "provider_lmstudio.clj"))
          ;; every returned item clears the relevance floor (no fff padding)
        (expect (every? #(>= (get % "score") min-score) (get out "items")))
          ;; scattered-subsequence noise is excluded
        (expect (not (contains? names "foundation_git_merge_ops.clj")))
        (expect (not (contains? names "foundation_voice_asr.clj")))))))

(defdescribe find-fuzzy-fallback-test
  "find-relevance takes the MIN across query tokens, so a multi-word CONCEPT
   query drops the moment any word is absent — the reason natural-language
   phrases returned nothing. When the strict pass is empty and the query has
   >=2 usable tokens, find-search falls back to per-token search and surfaces
   files by exact-name bullseye then coverage."
  (let [find-search (private-fn "find-search")]
    (it "a conceptual phrase surfaces the exact-name file the strict MIN pass dropped"
      (let [_   (write-temp! "findfuzz/render.clj" ";; the visualization renderer\n")
            _   (write-temp! "findfuzz/native_tool_handlers.md" "# native tool docs\n")
            _   (write-temp! "findfuzz/unrelated_widget.clj" ";; nope\n")
            dir (temp-dir-path "findfuzz")
            out (find-search [{"query" "native tool call visualization render" "paths" [dir]}])
            names (mapv #(last (string/split % #"/")) (get out "paths"))]
        ;; strict MIN would need ALL five words in one path → nothing; fuzzy saves it
        (expect (true? (get out "fuzzy")))
        (expect (some #{"render.clj"} names))
        ;; the exact-name bullseye (`render` → render.clj) ranks FIRST, above the
        ;; two-common-word loose hit (native+tool → native_tool_handlers.md)
        (expect (= "render.clj" (first names)))
        ;; the terms that actually landed are reported
        (expect (some #{"render"} (get out "matched_terms")))
        ;; a file matching NONE of the terms is not dragged in
        (expect (not (some #{"unrelated_widget.clj"} names)))))
    (it "a precise query that the strict MIN pass satisfies stays NON-fuzzy"
      (let [_   (write-temp! "findprecise/channel_tui_footer.clj" ";; footer\n")
            dir (temp-dir-path "findprecise")
            out (find-search [{"query" "channel tui footer" "paths" [dir]}])]
        (expect (nil? (get out "fuzzy")))
        (expect (some #{"channel_tui_footer.clj"}
                  (map #(last (string/split % #"/")) (get out "paths"))))))
    (it "a genuinely-unmatchable query still returns nothing (fuzzy can't invent hits)"
      (let [_   (write-temp! "findnone/alpha.clj" ";; x\n")
            dir (temp-dir-path "findnone")
            out (find-search [{"query" "zzzqqq wwwvvv" "paths" [dir]}])]
        (expect (zero? (get out "item_count")))))))

(defdescribe render-find-single-match-test
  "A SINGLE find match has nothing worth folding — the one path IS the result —
   so `render-find-result` rides it on the summary chip and emits NO body. With
   no body the shared `result-card` marks the card non-collapsible, so neither
   channel paints a pointless one-line disclosure. 2+ matches (and the 0-match
   steer) keep their body."
  (let [render-find-result (private-fn "render-find-result")]
    (it "one match: path rides the summary, NO body (→ non-collapsible)"
      (let [{:keys [summary body]}
            (render-find-result {"item_count" 1 "query" "PERSONA.md"
                                 "paths" ["docs/PERSONA.md"]})]
        (expect (nil? body))
        (expect (string/includes? summary "1 match"))
        (expect (string/includes? summary "`docs/PERSONA.md`"))))
    (it "two matches: summary stays plural and the ranked paths ride the body"
      (let [{:keys [summary body]}
            (render-find-result {"item_count" 2 "query" "render"
                                 "paths" ["a/render.clj" "b/render.clj"]})]
        (expect (string/includes? summary "2 matches"))
        (expect (string/includes? (str body) "a/render.clj"))
        (expect (string/includes? (str body) "b/render.clj"))))
    (it "zero matches: the filename-vs-content steer still rides the body"
      (let [{:keys [summary body]}
            (render-find-result {"item_count" 0 "query" "zzz" "paths" []
                                 "hint" "No FILENAME matched"})]
        (expect (string/includes? summary "0 matches"))
        (expect (string/includes? (str body) "No FILENAME matched"))))))

(defdescribe structural-tool-gating-test
  "The tree-sitter STRUCTURAL editors are advertised ONLY when the project has
   structurally-supported code; a docs/config repo hides them, and it FAILS OPEN."
  (let [active? (fn [sym langs]
                  (with-redefs [environment/snapshot
                                (fn [] {:languages {:languages (mapv (fn [l] {:language l}) langs)}})]
                    (extension/symbol-active? sym nil)))
        struct-syms [editing/struct-patch-symbol editing/outline-symbol editing/occurrences-symbol
                     editing/symbol-rename-symbol editing/sexpr-symbol]]
    (it "a Clojure project advertises every structural editor"
      (doseq [s struct-syms] (expect (true? (active? s ["clojure"])))))
    (it "a docs-only (markdown/text) project HIDES them; cat/rg/find_files stay"
      (doseq [s struct-syms] (expect (false? (active? s ["markdown" "text"]))))
      (doseq [s [editing/cat-symbol editing/rg-symbol editing/find-symbol]]
        (expect (true? (active? s ["markdown" "text"])))))
    (it "a mixed repo with ANY supported language keeps them (markdown + json)"
      (expect (true? (active? editing/struct-patch-symbol ["markdown" "json"]))))
    (it "shell reconciles to bash (scan says `shell`, tree-sitter says `bash`)"
      (expect (true? (active? editing/struct-patch-symbol ["shell"]))))
    (it "FAILS OPEN on an empty/unknown scan or a scan error"
      (expect (true? (active? editing/struct-patch-symbol [])))
      (with-redefs [environment/snapshot (fn [] (throw (ex-info "boom" {})))]
        (expect (true? (extension/symbol-active? editing/struct-patch-symbol nil)))))))
