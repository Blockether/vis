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
  (:require [babashka.fs :as fs]
            [clojure.set]
            [clojure.string :as string]
            ;; Loads/registers the built-in foundation extension so direct private
            ;; tool calls below see the same op-tag registry as production.
            [com.blockether.vis.internal.foundation.core]
            [com.blockether.vis.internal.foundation.editing.core :as editing]
            [com.blockether.fff :as fff]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.foundation.environment.core :as environment]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.foundation.editing.patch :as patch]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe describe expect it throws?]]))

(defn- private-fn
  [name]
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

(defn- write-temp!
  [name content]
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

(defdescribe
  native-tools-flat-spec-guard
  ;; Cross-check the STRONG flat spec across EVERY editing native tool: schema is
  ;; required + tightly attached, render present, description non-blank, and the
  ;; legacy :native-tool map is gone. Build-time enforcement already throws on a
  ;; schema-less native tool; this locks render/description/no-legacy too.
  (it
    "every editing native tool is flat: :native-tool? + required :schema + :render + non-blank description, no legacy map"
    (let
      [ext
       {:ext/engine {:ext.engine/symbols (editing/available-editing-symbols)}}

       ents
       (filter :ext.symbol/native-tool? (extension/ext-symbols ext))

       tools
       (extension/native-tools-for [ext])

       names
       (set (map :name tools))

       find-files-tool
       (first (filter #(= "find_files" (:name %)) tools))

       include-schema
       (get-in find-files-tool [:schema :properties "include"])]

      (expect (<= 8 (count ents)))                          ;; cat ls find_files patch move delete file_exists
      (expect (contains? names "cat"))
      (expect (not (contains? names "rg")))                 ;; rg folded into find_files
      (expect (contains? names "file_exists"))              ;; file-exists → file_exists
      (expect (every? (comp map? :ext.symbol/schema) ents)) ;; schema tight on the symbol
      (expect (every? :schema tools))                       ;; and surfaced
      (expect (some #(= {:type "string"} %) (:oneOf include-schema)))
      (expect (some #(= {:type "array" :items {:type "string"}} %) (:oneOf include-schema)))
      (expect (every? :render tools))                       ;; renderer present
      (expect (every? (comp seq str :description) tools))   ;; non-blank model-facing description
      (expect (not-any? :ext.symbol/native-tool ents))))    ;; legacy map removed
  (it
    "native-tool-renderers-by-op keys by the result :op string (cat→\"cat\", file-exists→\"file_exists\")"
    (let
      [ext
       {:ext/engine {:ext.engine/symbols (editing/available-editing-symbols)}}

       by-op
       (extension/native-tool-renderers-by-op [ext])]

      (expect (fn? (:render (get by-op "cat"))))
      (expect (fn? (:render (get by-op "rg"))))
      (expect (contains? by-op "file_exists")) ;; file-exists → "file_exists"
      (expect (= :tool-color/search (:color-role (get by-op "rg")))))))

(defdescribe
  rg-simplified-api-test
  ;; NEW simplified rg grammar: `query` canonical, `any`/`all` accepted aliases
  ;; that BOTH mean OR, smart-case literal substring, `paths`/`include`/`context`
  ;; (int only)/`is_files_only`. Unknown keys ignored; missing query throws.
  (let
    [coerce
     (private-fn "coerce-rg-spec")

     matcher
     @#'editing/make-line-matcher

     grep
     (private-fn "rg-search")]

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
        (expect (= ["foo"] (:needles (coerce {"query" "foo"}))))) ;; single term untouched
    (it "defaults :paths/:include and keeps canonical :paths/:include"
        (expect (= ["."] (:paths (coerce {"query" ["x"]}))))
        (expect (= [] (:include (coerce {"query" ["x"]}))))
        (expect (= [] (:include (coerce {"query" ["x"] "include" []}))))
        (expect (= ["src"] (:paths (coerce {"query" ["x"] "paths" ["src"]}))))
        (expect (= ["*.clj"] (:include (coerce {"query" ["x"] "include" ["*.clj"]}))))
        (expect (= ["*.clj"] (:include (coerce {"query" ["x"] "include" "*.clj"}))))
        (expect (= [] (:include (coerce {"query" ["x"] "include" []})))))
    (it "ignores unknown keys (removed aliases are just dropped, never fatal)"
        (let
          [spec (coerce {"query" ["x"]
                         "path" "src"
                         "glob" "*.clj"
                         "excludes" ["t/**"]
                         "is_regex" true
                         "is_counts" true
                         "limit" 5})]
          ;; removed aliases don't set :paths/:include; canonical defaults win
          (expect (= ["."] (:paths spec)))
          (expect (= [] (:include spec)))))
    (it "missing query throws `rg needs query`"
        (let [err (try (coerce {"paths" ["."]}) nil (catch clojure.lang.ExceptionInfo e e))]
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
        (let
          [_
           (write-temp! "rgsimple/a.txt" "alpha\nbeta\ngamma\n")

           out
           (grep {"query" ["alpha" "gamma"] "paths" [(temp-dir-path "rgsimple")]})]

          (expect (= ["alpha" "gamma"] (mapv :text (:hits out))))))
    (it "rg-needle-hostile-to-fff? flags quantifier/bracket needles (fff fast-path gate)"
        (let [hostile? (private-fn "rg-needle-hostile-to-fff?")]
          ;; These make fff match NOTHING/error → zero candidate files → the
          ;; literal matcher never ran → 0 hits (the regression).
          (expect (hostile? "*workspace-root*"))
          (expect (hostile? "(defn foo"))
          (expect (hostile? "arr[0]"))
          (expect (hostile? "foo{bar"))
          ;; No quantifier/bracket char → stays on the fff fast path.
          (expect (not (hostile? "workspace-root")))
          (expect (not (hostile? "config.json")))))
    (it "rg-search finds an ear-muffed *var* (fff pre-filter bypassed, literal match)"
        ;; fff honors `*workspace-root*` as a regex/glob → 0 candidate files →
        ;; the literal `make-line-matcher` never ran → 0 hits. Bypass fff for
        ;; such needles so the literal-substring contract holds.
        (let
          [_
           (write-temp! "rgstar/a.clj" "(def ^:dynamic *workspace-root* \"/x\")\n")

           out
           (grep {"query" ["*workspace-root*"] "paths" [(temp-dir-path "rgstar")]})]

          (expect (= 1 (count (:hits out))))
          (expect (= "(def ^:dynamic *workspace-root* \"/x\")" (:text (first (:hits out)))))))
    (it ":is_files_only returns distinct :files, never :hits"
        (let
          [_
           (write-temp! "rgsimplefo/a.py" "alpha\nalpha\n")

           _
           (write-temp! "rgsimplefo/b.py" "alpha\n")

           _
           (write-temp! "rgsimplefo/c.py" "no match\n")

           out
           (grep {"query" ["alpha"] "paths" [(temp-dir-path "rgsimplefo")] "is_files_only" true})]

          (expect (contains? out :files))
          (expect (not (contains? out :hits)))
          (expect (= 2 (count (:files out))))))))

(defdescribe
  cwd-safety-test
  ;; THE non-negotiable invariant: every v/* tool that touches the
  ;; filesystem must refuse any path that escapes (workspace/cwd).
  ;; safe-path is the single gate; this suite proves every mutation
  ;; tool actually routes through it.
  (let [escape-paths ["../escape.txt" "../../etc/passwd" "/etc/passwd" "target/../../escape.txt"]]
    (it "patch refuses to write outside cwd"
        (let [patch (private-fn "patch-safe")]
          (doseq [p escape-paths]
            (let [r (patch [{"path" p "from_anchor" (patch/line-anchor 1 "x") "replace" "y"}])]
              (expect (false? (:success? r)))
              (expect (= :path-escape
                         (-> r
                             :failures
                             first
                             :reason)))))))
    (it "write refuses to create files outside cwd"
        ;; Note: we deliberately do NOT (.exists) the escape path here; the
        ;; check is whether `write-safe` REFUSED to act. /etc/passwd exists
        ;; on macOS regardless of our actions; what matters is :reason :path-escape
        ;; and the cwd guard kicking in before any IO.
        (let [write (private-fn "write-safe")]
          (doseq [p escape-paths]
            (let [r (write {"path" p "content" "hi"})]
              (expect (false? (:success? r)))
              (expect (= :path-escape
                         (-> r
                             :failures
                             first
                             :reason)))))))
    (it "create-dirs refuses to mkdir outside cwd"
        (let [create (private-fn "create-dirs-safe")]
          (doseq [p escape-paths]
            (let [err (try (create p) nil (catch clojure.lang.ExceptionInfo e e))]
              (expect (some? err))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err))))))))
    (it "copy refuses src OR dest outside cwd"
        (let
          [copy (private-fn "copy-safe")
           inside (write-temp! "cwd-safety/copy-src.txt" "x")]

          (doseq [p escape-paths]
            (let
              [err1 (try (copy p inside) nil (catch clojure.lang.ExceptionInfo e e))
               err2 (try (copy inside p) nil (catch clojure.lang.ExceptionInfo e e))]

              (expect (some? err1))
              (expect (some? err2))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err1))))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err2))))))))
    (it "move refuses src OR dest outside cwd"
        (let
          [move (private-fn "move-safe")
           inside (write-temp! "cwd-safety/move-src.txt" "x")]

          (doseq [p escape-paths]
            (let
              [err1 (try (move p inside) nil (catch clojure.lang.ExceptionInfo e e))
               err2 (try (move inside p) nil (catch clojure.lang.ExceptionInfo e e))]

              (expect (some? err1))
              (expect (some? err2))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err1))))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err2))))))))
    (it "delete and delete-if-exists refuse paths outside cwd"
        (let
          [del (private-fn "delete-safe")
           del-if (private-fn "delete-if-exists-safe")]

          (doseq [p escape-paths]
            (let
              [err1 (try (del p) nil (catch clojure.lang.ExceptionInfo e e))
               err2 (try (del-if p) nil (catch clojure.lang.ExceptionInfo e e))]

              (expect (some? err1))
              (expect (some? err2))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err1))))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err2))))))))
    (it "cat (read) ALSO refuses paths outside cwd"
        ;; Defense in depth: even reads can't leak through path traversal.
        (let [cat (private-fn "read-file")]
          (doseq [p escape-paths]
            (let
              [err (try (cat p)
                        nil
                        (catch clojure.lang.ExceptionInfo e e))]
              (expect (some? err))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err))))))))))

(defdescribe
  editing-extension-loads-test
  (it "bash tool fully removed: no symbol, no helpers, no prompt mention"
      (let
        [symbols
         (map :ext.symbol/symbol (editing/available-editing-symbols))

         prompt
         (editing/available-editing-prompt)]

        (expect (not-any? #{'bash} symbols))
        (expect (not (string/includes? prompt "v/bash")))
        (expect (not (string/includes? prompt "bash")))
        (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                       "bash-tool"))))
        (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                       "bash-symbol"))))
        (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                       "run-bash-safe"))))))
  (it "every editing symbol carries a non-blank :doc and an :arglists vector"
      (doseq
        [s
         @editing/editing-symbols

         :let [doc
               (:ext.symbol/doc s)

               arglists
               (:ext.symbol/arglists s)]]

        (expect (string? doc))
        (expect (not (string/blank? doc)))
        (expect (or (vector? arglists) (seq? arglists)))))
  (it "emits no duplicated editing prompt fragment"
      (expect (string? @editing/editing-prompt))
      (expect (string/blank? @editing/editing-prompt)))
  (it "editing prompt has no v/preview references (tool retired)"
      (expect (not (string/includes? @editing/editing-prompt "v/preview")))
      (expect (nil? (some #(when (= 'preview (:ext.symbol/symbol %)) %) @editing/editing-symbols))))
  (it "keeps routing in compact native descriptions and inputs in schemas"
      (doseq
        [s
         @editing/editing-symbols

         :when (:ext.symbol/native-tool? s)]

        (let
          [description
           (:ext.symbol/description s)

           schema
           (:ext.symbol/schema s)]

          (expect (not (string/blank? description)))
          (expect (< (count description) 500))
          (expect (= "object" (:type schema)))
          (expect (false? (:additionalProperties schema)))))))

(it "defers op classification to the engine contract (no editing-local copy)"
    ;; The classification table + presentation map live in
    ;; `com.blockether.vis.internal.extension` (`op-tag`,
    ;; `op-presentation`). Editing used to keep a thin shim; that
    ;; shim is gone and callers go straight to the engine. Tags
    ;; collapsed to observation/mutation values; ops not in the
    ;; registration table fail closed instead of defaulting to observation.
    (doseq
      [[op tag] [[:cat :observation] [:z/locators :observation] [:find_files :observation]
                 [:patch :mutation] [:create-dirs :mutation] [:delete :mutation] [:move :mutation]]]
      (expect (= tag (extension/op-tag op)))
      (expect (= {:tag tag} (extension/op-presentation op))))
    (let [thrown (try (extension/op-tag :v/extensions) nil (catch clojure.lang.ExceptionInfo e e))]
      (expect (= :extension/unregistered-op (:type (ex-data thrown))))))

(defn- protected-env
  [rules]
  {:extensions (atom [(extension/extension {:ext/name "test.protected-paths"
                                            :ext/description "Test protected paths."
                                            :ext/protected-paths (constantly (vec rules))})])})

(defn- gate-env
  "A non-protecting env carrying a `:mutation-gate` stub that records the call
   payload into `seen!` and returns `ret` (a refusal string or nil)."
  [seen! ret]
  {:extensions (atom [])
   :mutation-gate (fn [payload]
                    (reset! seen! payload)
                    ret)})

(defdescribe
  protected-path-before-fn-test
  (it
    "cat blocks :none protected paths and returns the extension hint"
    (let
      [hint
       "Use (br/policy) instead of reading this file directly."

       path
       "target/editing-test/protected/secret.edn"

       before
       (:ext.symbol/before-fn (private-fn "cat-symbol"))

       out
       (before (protected-env
                 [{:glob "target/editing-test/protected/*.edn" :access :none :hint hint}])
               (constantly :ok)
               [path])

       failure
       (:result out)]

      (expect (some? failure))
      (expect (false? (:success? failure)))
      (expect (= :ext.foundation.editing/path-protected
                 (-> failure
                     :error
                     :type)))
      (expect (= hint
                 (-> failure
                     :error
                     :hint)))
      (expect (= hint
                 (-> failure
                     :error
                     :loop-hint)))
      (expect (= :none
                 (-> failure
                     :error
                     :failures
                     first
                     :access)))
      (expect (= :read
                 (-> failure
                     :error
                     :failures
                     first
                     :intent)))))
  (it
    "patch blocks writes to :read-only protected paths and returns the extension hint"
    (let
      [hint
       "Use (br/update-policy!) instead of patching policy files."

       path
       "target/editing-test/protected/policy.txt"

       before
       (:ext.symbol/before-fn (private-fn "patch-symbol"))

       out
       (before (protected-env
                 [{:glob "target/editing-test/protected/*.txt" :access :read-only :hint hint}])
               (constantly :ok)
               [[{"path" path "from_anchor" "1:abc" "replace" "new"}]])

       failure
       (:result out)]

      (expect (some? failure))
      (expect (false? (:success? failure)))
      (expect (= :ext.foundation.editing/path-protected
                 (-> failure
                     :error
                     :type)))
      (expect (= hint
                 (-> failure
                     :error
                     :hint)))
      (expect (= :read-only
                 (-> failure
                     :error
                     :failures
                     first
                     :access)))
      (expect (= :write
                 (-> failure
                     :error
                     :failures
                     first
                     :intent)))))
  (it
    "rg allows current directory even when descendants are protected (regression: .bridge/ blocks rg on `.`)"
    ;; Repro for transcript ccee2e1f-16ee-4acf-8d93-b4505034c0de iter 1:
    ;;   (rg {:any ["scrollbar"] :paths ["."] :is_counts true})
    ;;   -> ERROR ":rg blocked: . is protected; use the owning extension API instead."
    ;; The bridge extension registers `.bridge/` with :access :none. Because
    ;; "." is an ancestor of every protected descendant, the composite-dir
    ;; branch in `protected-rule-matches?` reported a match and rg failed
    ;; closed even though it's a recursive read that can skip protected
    ;; subtrees during its own walk. The cwd-ancestor bypass must apply to
    ;; rg the same way it applies to ls.
    (let
      [before
       (:ext.symbol/before-fn (private-fn "find-symbol"))

       out
       (before (protected-env [{:glob ".bridge/" :access :none :hint "Use (br/policy) instead."}])
               (constantly :ok)
               [{"any" ["scrollbar"] "paths" ["."] "is_counts" true}])]

      (expect (not (contains? out :result)))
      (expect (= [{"any" ["scrollbar"] "paths" ["."] "is_counts" true}] (:args out)))))
  (it "rg with no :paths (default `.`) is allowed when only descendants are protected"
      ;; rg-arg-paths returns ["."] when :paths is omitted; same bypass
      ;; must apply so model can call `(rg {:any ["x"]})` without paths.
      (let
        [before
         (:ext.symbol/before-fn (private-fn "find-symbol"))

         out
         (before (protected-env [{:glob ".bridge/" :access :none :hint "Use (br/policy) instead."}])
                 (constantly :ok)
                 [{"any" ["scrollbar"]}])]

        (expect (not (contains? out :result)))))
  (it "exists? on `.` is allowed when only descendants are protected"
      (let
        [before
         (:ext.symbol/before-fn (private-fn "file-exists-symbol"))

         out
         (before (protected-env [{:glob ".bridge/" :access :none :hint "Use (br/policy) instead."}])
                 (constantly :ok)
                 ["."])]

        (expect (not (contains? out :result)))
        (expect (= ["."] (:args out)))))
  (it "rg still respects direct rules whose glob matches `.` itself"
      ;; The bypass is descendant-only. If an extension explicitly says
      ;; `:glob "." :access :none` (\"do not read cwd at all\") that's still
      ;; honored — we don't want the bypass to be a back door.
      (let
        [before
         (:ext.symbol/before-fn (private-fn "find-symbol"))

         out
         (before (protected-env [{:glob "." :access :none :hint "cwd is sealed."}])
                 (constantly :ok)
                 [{"any" ["x"] "paths" ["."]}])

         failure
         (:result out)]

        (expect (some? failure))
        (expect (false? (:success? failure)))))
  (it "writes on `.` are still blocked even when only descendants are protected"
      ;; The bypass is INTENTIONALLY read-only — a recursive write on cwd
      ;; cannot filter protected descendants safely, so we keep failing
      ;; closed. (patch / write don't target cwd in practice, but
      ;; delete on \".\" must stay blocked.)
      (let
        [before
         (:ext.symbol/before-fn (private-fn "delete-symbol"))

         out
         (before (protected-env [{:glob ".bridge/" :access :none :hint "Use (br/policy) instead."}])
                 (constantly :ok)
                 ["."])

         failure
         (:result out)]

        (expect (some? failure))
        (expect (false? (:success? failure)))))
  ;; =============================================================================
  ;; FORCING plan-gate composition — write/patch consult env :mutation-gate AFTER
  ;; path-protection clears. (proposal Decision 2 / G1)
  ;; =============================================================================
  (describe
    "plan-gate-before-fn-test"
    (it "write SHORT-CIRCUITS with a :plan-required failure when the gate refuses"
        (let
          [seen
           (atom nil)

           before
           (:ext.symbol/before-fn (private-fn "write-symbol"))

           out
           (before (gate-env seen "Plan required — 2nd file.")
                   (constantly :ok)
                   [{"path" "target/editing-test/b.clj" "content" "x"}])

           failure
           (:result out)]

          (expect (some? failure))
          (expect (false? (:success? failure)))
          (expect (= :ext.foundation.editing/plan-required
                     (-> failure
                         :error
                         :type)))
          (expect (= :plan-required
                     (-> failure
                         :error
                         :reason)))
          ;; gate saw the canonical path + op
          (expect (= :write (:op @seen)))
          (expect (= ["target/editing-test/b.clj"] (:paths @seen)))
          (expect (false? (:atomic? @seen)))))
    (it "write PASSES THROUGH (no :result) when the gate allows (nil)"
        (let
          [seen
           (atom nil)

           before
           (:ext.symbol/before-fn (private-fn "write-symbol"))

           out
           (before (gate-env seen nil)
                   (constantly :ok)
                   [{"path" "target/editing-test/a.clj" "content" "x"}])]

          (expect (not (contains? out :result)))
          (expect (= [{"path" "target/editing-test/a.clj" "content" "x"}] (:args out)))))
    (it "detects the atomic=True escape flag on a write"
        (let
          [seen
           (atom nil)

           before
           (:ext.symbol/before-fn (private-fn "write-symbol"))]

          (before (gate-env seen nil)
                  (constantly :ok)
                  [{"path" "target/editing-test/a.clj" "content" "x" "atomic" true}])
          (expect (true? (:atomic? @seen)))))
    (it "detects atomic on a patch edit map + reports all edited paths"
        (let
          [seen
           (atom nil)

           before
           (:ext.symbol/before-fn (private-fn "patch-symbol"))]

          (before
            (gate-env seen nil)
            (constantly :ok)
            [[{"path" "target/editing-test/a.clj" "from_anchor" "1:abc" "replace" "n" "atomic" true}
              {"path" "target/editing-test/b.clj" "from_anchor" "1:def" "replace" "n"}]])
          (expect (true? (:atomic? @seen)))
          (expect (= #{"target/editing-test/a.clj" "target/editing-test/b.clj"}
                     (set (:paths @seen))))))
    (it "path-protection wins: a protected path NEVER reaches the gate"
        (let
          [hint
           "owner API only"

           before
           (:ext.symbol/before-fn (private-fn "write-symbol"))

           env
           (assoc (protected-env
                    [{:glob "target/editing-test/protected/*.clj" :access :read-only :hint hint}])
             :mutation-gate (fn [_]
                              (throw (ex-info "gate must not run" {}))))

           out
           (before env
                   (constantly :ok)
                   [{"path" "target/editing-test/protected/x.clj" "content" "x"}])

           failure
           (:result out)]

          (expect (= :ext.foundation.editing/path-protected
                     (-> failure
                         :error
                         :type)))))
    (it "no :mutation-gate on env → write passes through (gate is optional)"
        (let
          [before
           (:ext.symbol/before-fn (private-fn "write-symbol"))

           out
           (before {:extensions (atom [])}
                   (constantly :ok)
                   [{"path" "target/editing-test/a.clj" "content" "x"}])]

          (expect (not (contains? out :result))))))
  (it
    "allows first-match :read-write exceptions for the exact file without unblocking siblings"
    (let
      [hint
       "Use (br/files) instead of listing Bridge-owned files."

       path
       "target/editing-test/protected/public.edn"

       rules
       [{:glob path :access :read-write :hint "Direct edits are allowed for this file."}
        {:glob "target/editing-test/protected/*.edn" :access :none :hint hint}]

       patch-before
       (:ext.symbol/before-fn (private-fn "patch-symbol"))

       cat-before
       (:ext.symbol/before-fn (private-fn "cat-symbol"))

       patch-out
       (patch-before (protected-env rules)
                     (constantly :ok)
                     [[{"path" path "from_anchor" "1:abc" "replace" "new"}]])

       cat-out
       (cat-before (protected-env rules)
                   (constantly :ok)
                   ["target/editing-test/protected/secret.edn"])

       failure
       (:result cat-out)]

      (expect (not (contains? patch-out :result)))
      (expect (= [[{"path" path "from_anchor" "1:abc" "replace" "new"}]] (:args patch-out)))
      (expect (some? failure))
      (expect (false? (:success? failure)))
      (expect (= hint
                 (-> failure
                     :error
                     :hint)))
      (expect (= :none
                 (-> failure
                     :error
                     :failures
                     first
                     :access))))))

(defn- numbered-tuples
  "[[start str0] [start+1 str1] …] helper for assembling expected
   `:lines` payloads in shape tests."
  [start xs]
  (mapv vector (iterate inc start) xs))

(defdescribe
  vis-cat-structured-shape-test
  (it "returns the paginated shape (small file, single window, eof) plus staleness metadata"
      (let
        [path
         (write-temp! "small.txt" "alpha\nbeta\ngamma\n")

         read-file
         (private-fn "read-file")

         out
         (read-file path)]

        (expect (= #{:path :lines :anchors :next-offset :eof? :truncated? :mtime :size}
                   (set (keys out))))
        (expect (string? (:path out)))
        (expect (nil? (:next-offset out)))
        (expect (true? (:eof? out)))
        (expect (false? (:truncated? out)))
        (expect (= (numbered-tuples 1 ["alpha" "beta" "gamma"]) (:lines out)))
        ;; Staleness metadata mirrors File.lastModified / File.length and can
        ;; guard a later whole-file write. Anchored patch edits deliberately do
        ;; not accept file-wide mtime/size guards.
        (expect (pos-int? (:mtime out)))
        (expect (= (.length (fs/file path)) (:size out)))))
  (it ":eof? false (with :next-offset) when window stops short of file end"
      (let
        [body
         (string/join "\n" (map #(str "L" %) (range 1 21)))

         path
         (write-temp! "eof-false.txt" (str body "\n"))

         read-file
         (private-fn "read-file")

         out
         (read-file path 1 3)]

        (expect (false? (:eof? out)))
        (expect (= 4 (:next-offset out)))))
  (it ":mtime from cat round-trips into write's :expected_mtime guard"
      ;; Whole-file write needs a file-wide concurrency guard. Patch does not:
      ;; its fresh line anchors verify the target content instead.
      (let
        [path
         (write-temp! "cat-stale.txt" "alpha\n")

         read-file
         (private-fn "read-file")

         write
         (private-fn "write-safe")

         first-read
         (read-file path)

         mtime0
         (:mtime first-read)]

        ;; Same mtime -> whole-file write goes through cleanly.
        (write {"path" path "content" "BETA\n" "expected_mtime" mtime0})
        (expect (= "BETA\n" (slurp path)))
        ;; Force-clock the file backwards so the next read sees a fresh mtime
        ;; distinct from `mtime0` regardless of filesystem millis precision.
        (.setLastModified (fs/file path) (- (long mtime0) 60000))
        (let [r (write {"path" path "content" "GAMMA\n" "expected_mtime" mtime0})]
          (expect (false? (:success? r)))
          (expect (= :stale
                     (-> r
                         :failures
                         first
                         :reason)))
          (expect (= "BETA\n" (slurp path))))))
  (it ":lines tuples carry raw strings - no embedded line-number prefix in the text"
      (let
        [path
         (write-temp! "raw.txt" "   indented\nplain\n")

         read-file
         (private-fn "read-file")

         out
         (read-file path)]

        (expect (= [[1 "   indented"] [2 "plain"]] (:lines out)))))
  (it "(cat path n) reads first n lines and sets :next-offset when more remain"
      (let
        [body
         (string/join "\n" (map #(str "line-" %) (range 1 11)))

         path
         (write-temp! "ten.txt" (str body "\n"))

         read-file
         (private-fn "read-file")

         out
         (read-file path 4)]

        (expect (= 5 (:next-offset out)))
        (expect (false? (:truncated? out)))
        (expect (= (numbered-tuples 1 ["line-1" "line-2" "line-3" "line-4"]) (:lines out)))))
  (it "(cat path offset n) reads a mid-file window and advances :next-offset"
      (let
        [body
         (string/join "\n" (map #(str "L" %) (range 1 21)))

         path
         (write-temp! "twenty.txt" (str body "\n"))

         read-file
         (private-fn "read-file")

         out
         (read-file path 7 3)]

        (expect (= 10 (:next-offset out)))
        (expect (= (numbered-tuples 7 ["L7" "L8" "L9"]) (:lines out)))
        (expect (false? (:truncated? out)))))
  (it "paging via :next-offset reaches eof cleanly"
      (let
        [body
         (string/join "\n" (map #(str "line-" %) (range 1 11)))

         path
         (write-temp! "page.txt" (str body "\n"))

         read-file
         (private-fn "read-file")

         page-1
         (read-file path 1 4)

         page-2
         (read-file path (:next-offset page-1) 4)

         page-3
         (read-file path (:next-offset page-2) 4)]

        (expect (= (numbered-tuples 1 ["line-1" "line-2" "line-3" "line-4"]) (:lines page-1)))
        (expect (= (numbered-tuples 5 ["line-5" "line-6" "line-7" "line-8"]) (:lines page-2)))
        (expect (= (numbered-tuples 9 ["line-9" "line-10"]) (:lines page-3)))
        (expect (nil? (:next-offset page-3)))))
  (it "offset past EOF returns an empty window and no :next-offset"
      (let
        [path
         (write-temp! "two.txt" "a\nb\n")

         read-file
         (private-fn "read-file")

         out
         (read-file path 99 10)]

        (expect (= [] (:lines out)))
        (expect (nil? (:next-offset out)))
        (expect (false? (:truncated? out)))))
  (it ":truncated? true when a window would exceed max-cat-window-bytes"
      ;; Window byte cap is 256KB. Use 200 lines of ~1500 chars so the
      ;; byte-cap fires on cumulative volume. First line always included
      ;; for forward progress (even a single pathological 1MB line gets
      ;; emitted whole — the per-line cap was retired; see source note).
      (let
        [chunky
         (apply str (repeat 1500 "x"))

         body
         (string/join "\n" (repeat 200 chunky))

         path
         (write-temp! "huge.txt" (str body "\n"))

         read-file
         (private-fn "read-file")

         out
         (read-file path 1 500)]

        (expect (true? (:truncated? out)))
        (expect (pos? (count (:lines out))))
        (expect (< (count (:lines out)) 200))
        (expect (some? (:next-offset out)))))
  (it "persistence-blob contract: :lines bytes are bounded by max-cat-window-bytes"
      ;; This is the storage claim: a single cat call cannot persist
      ;; more than max-cat-window-bytes of line bytes regardless of file size.
      (let
        [line
         (apply str (repeat 200 "x"))

         body
         (string/join "\n" (repeat 5000 line))

         path
         (write-temp! "persist.txt" (str body "\n"))

         read-file
         (private-fn "read-file")

         out
         (read-file path 1 100000)

         line-bytes
         (reduce +
                 0
                 (map (fn [[_ ^String s]]
                        (inc (count (.getBytes s "UTF-8"))))
                      (:lines out)))]

        ;; 256KB window cap (bumped from 64KB).
        (expect (<= line-bytes (* 256 1024)))))
  (it "rejects bad positional args (non-positive ints, non-int types)"
      (let
        [path
         (write-temp! "validate.txt" "x\n")

         read-file
         (private-fn "read-file")]

        (doseq [bad [[0 10] [-1 10] [1 0] [1 -5] ["a" 10] [1 :hi]]]
          (expect (throws? clojure.lang.ExceptionInfo #(apply read-file path bad)))))))

(defdescribe vis-cat-tail-shape-test
             (it "(cat path :tail n) reads the last n lines and reports correct line numbers"
                 (let
                   [body
                    (string/join "\n" (map #(str "line-" %) (range 1 21)))

                    path
                    (write-temp! "tail.txt" (str body "\n"))

                    tail-file
                    (private-fn "tail-file")

                    out
                    (tail-file path 5)]

                   (expect (nil? (:next-offset out)))
                   (expect (false? (:truncated? out)))
                   (expect (= (numbered-tuples 16
                                               ["line-16" "line-17" "line-18" "line-19" "line-20"])
                              (:lines out)))))
             (it "tail of a file shorter than n returns the whole file with :eof? true"
                 (let
                   [path
                    (write-temp! "short.txt" "alpha\nbeta\n")

                    tail-file
                    (private-fn "tail-file")

                    out
                    (tail-file path 50)]

                   (expect (= [[1 "alpha"] [2 "beta"]] (:lines out)))
                   (expect (nil? (:next-offset out)))
                   (expect (true? (:eof? out)))
                   (expect (pos-int? (:mtime out)))
                   (expect (pos-int? (:size out)))))
             (it ":truncated? true when byte cap drops older lines from the tail window"
                 ;; Same trick as the read-file byte-cap test: use 200 × 1500-char
                 ;; lines so cumulative volume blows the 256KB window cap, not the
                 ;; per-line 2000-char cap. Most-recent line is the LAST one included.
                 (let
                   [chunky
                    (apply str (repeat 1500 "x"))

                    body
                    (string/join "\n" (repeat 200 chunky))

                    path
                    (write-temp! "htail.txt" (str body "\n"))

                    tail-file
                    (private-fn "tail-file")

                    out
                    (tail-file path 500)]

                   (expect (true? (:truncated? out)))
                   (expect (pos? (count (:lines out))))
                   (expect (< (count (:lines out)) 200))
                   ;; Last kept line should be line 200 (most-recent wins on tail).
                   (expect (= 200 (first (peek (:lines out))))))))

(defdescribe vis-cat-tool-arities-test
             (it "(cat path :tail) defaults to default-cat-limit (2000) lines from the end"
                 ;; Bumped from 400 → 2000 for industry parity with Claude Code / Roo Code.
                 ;; Use a file with >2000 lines so the tail default actually clamps.
                 (let
                   [body
                    (string/join "\n" (map #(str "L" %) (range 1 2401)))

                    path
                    (write-temp! "big-tail.txt" (str body "\n"))

                    cat-tool
                    (private-fn "cat-tool")

                    out
                    (-> (cat-tool path :tail)
                        :result)]

                   (expect (= 2000 (count (get out "anchors"))))
                   (expect (= 401 (ffirst (patch/anchor-map->tuples (get out "anchors")))))
                   (expect (= 2400 (first (peek (patch/anchor-map->tuples (get out "anchors"))))))
                   (expect (nil? (get out "next_offset")))))
             (it "(cat path :tail n) honours an explicit count"
                 (let
                   [body
                    (string/join "\n" (map #(str "L" %) (range 1 21)))

                    path
                    (write-temp! "explicit-tail.txt" (str body "\n"))

                    cat-tool
                    (private-fn "cat-tool")

                    out
                    (-> (cat-tool path :tail 3)
                        :result)]

                   (expect (= (numbered-tuples 18 ["L18" "L19" "L20"])
                              (patch/anchor-map->tuples (get out "anchors")))))))

(defdescribe
  vis-cat-range-arity-test
  ;; G1 from the cat probe (C9): the offset+count arity feels awkward
  ;; when the model already knows both endpoints ("convert end=100 to
  ;; n=51 mentally"). The :range arity takes inclusive start..end.
  (it "(cat path :range start end) reads inclusive 1-based start..end"
      (let
        [body
         (string/join "\n" (map #(str "L" %) (range 1 21)))

         path
         (write-temp! "range/inclusive.txt" (str body "\n"))

         cat-tool
         (private-fn "cat-tool")

         out
         (-> (cat-tool path :range 5 10)
             :result)]

        ;; 5..10 inclusive = 6 lines (L5, L6, L7, L8, L9, L10).
        (expect (= 6 (count (get out "anchors"))))
        ;; Each anchor VALUE is a {"text" line} map — mirrors rg's hit
        ;; value, so `v["text"]` reads the line uniformly across tools.
        (expect (every? #(and (map? %) (contains? % "text")) (vals (get out "anchors"))))
        (expect (= #{"L5" "L6" "L7" "L8" "L9" "L10"}
                   (set (map #(get % "text") (vals (get out "anchors"))))))
        (expect (= [[5 "L5"] [6 "L6"] [7 "L7"] [8 "L8"] [9 "L9"] [10 "L10"]]
                   (patch/anchor-map->tuples (get out "anchors"))))))
  (it ":range with start == end reads exactly one line"
      (let
        [body
         (string/join "\n" (map #(str "L" %) (range 1 11)))

         path
         (write-temp! "range/single.txt" (str body "\n"))

         cat-tool
         (private-fn "cat-tool")

         out
         (-> (cat-tool path :range 7 7)
             :result)]

        (expect (= [[7 "L7"]] (patch/anchor-map->tuples (get out "anchors"))))))
  (it ":range rejects start > end, non-positive ints, and the wrong kw"
      (let
        [path
         (write-temp! "range/invalid.txt" "a\nb\nc\n")

         cat-tool
         (private-fn "cat-tool")]

        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool path :range 10 5)))
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool path :range 0 5)))
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool path :range -1 5)))
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool path :not-range 1 5)))))
  (it "(cat path :ranges [[start end] ...]) reads several ranges in one result"
      (let
        [body
         (string/join "\n" (map #(str "L" %) (range 1 21)))

         path
         (write-temp! "range/multi.txt" (str body "\n"))

         cat-tool
         (private-fn "cat-tool")

         out
         (-> (cat-tool path :ranges [[2 4] [10 12]])
             :result)]

        (expect (= [[2 "L2"] [3 "L3"] [4 "L4"] [10 "L10"] [11 "L11"] [12 "L12"]]
                   (patch/anchor-map->tuples (get out "anchors"))))
        (expect (= [[2 4] [10 12]] (mapv #(get % "range") (get out "ranges"))))
        (expect (= [[2 "L2"] [3 "L3"] [4 "L4"]]
                   (patch/anchor-map->tuples (get (first (get out "ranges")) "anchors"))))
        (expect (nil? (get out "next_offset")))))
  (it "(cat {\"path\" p, ...}) accepts the collapsed all-kwargs spec map"
      (let
        [body
         (string/join "\n" (map #(str "L" %) (range 1 21)))

         path
         (write-temp! "range/spec-map.txt" (str body "\n"))

         cat-tool
         (private-fn "cat-tool")

         ;; `cat(path=p, ranges=[[2 4]])` collapses to ONE spec map at
         ;; the Python boundary; it must equal the positional form.
         spec
         (-> (cat-tool {"path" path "ranges" [[2 4]]})
             :result)

         positional
         (-> (cat-tool path {"ranges" [[2 4]]})
             :result)]

        (expect (= [[2 "L2"] [3 "L3"] [4 "L4"]] (patch/anchor-map->tuples (get spec "anchors"))))
        (expect (= spec positional))
        ;; a path-only spec map == whole-file read
        (expect (= (-> (cat-tool {"path" path})
                       :result)
                   (-> (cat-tool path)
                       :result)))))
  (it
    "cat coerces stringy/flat range + ranges shapes models mis-pass"
    (let
      [body
       (string/join "\n" (map #(str "L" %) (range 1 21)))

       path
       (write-temp! "range/coerce.txt" (str body "\n"))

       cat-tool
       (private-fn "cat-tool")

       canonical
       (-> (cat-tool path {"ranges" [[2 4]]})
           :result)]

      ;; a single flat pair of NUMERIC STRINGS (the reported failure) now reads
      (expect (= canonical
                 (-> (cat-tool path {"ranges" ["2" "4"]})
                     :result)))
      ;; nested string pair + comma-joined string coerce identically
      (expect (= canonical
                 (-> (cat-tool path {"ranges" [["2" "4"]]})
                     :result)))
      (expect (= canonical
                 (-> (cat-tool path {"ranges" "2, 4"})
                     :result)))
      ;; `range` with numeric strings / comma string coerce too
      (expect (= (-> (cat-tool path {"range" [2 4]})
                     :result)
                 (-> (cat-tool path {"range" ["2" "4"]})
                     :result)))
      (expect (= (-> (cat-tool path {"range" [2 4]})
                     :result)
                 (-> (cat-tool path {"range" "2, 4"})
                     :result)))
      ;; a WHOLE stringified nested list (the reported failure shape) parses its
      ;; digit runs into windows, tolerating extra/mismatched brackets
      (expect (= [[2 4] [6 8]]
                 (mapv #(get % "range")
                       (get (-> (cat-tool path {"ranges" "[[2, 4]], [[6, 8]]"})
                                :result)
                            "ranges"))))
      ;; multi-window string ranges preserve each window
      ;; a VECTOR whose entries are each a stringified pair (bracketed or
      ;; comma-joined) — `flat` can't read it as one pair, but each entry coerces
      (expect (= [[2 4] [6 8]]
                 (mapv #(get % "range")
                       (get (-> (cat-tool path {"ranges" ["[2, 4]" "[6, 8]"]})
                                :result)
                            "ranges"))))
      (expect (= [[2 4] [6 8]]
                 (mapv #(get % "range")
                       (get (-> (cat-tool path {"ranges" ["2,4" "6,8"]})
                                :result)
                            "ranges"))))
      ;; multi-window string ranges preserve each window
      (expect (= [[2 4] [6 8]]
                 (mapv #(get % "range")
                       (get (-> (cat-tool path {"ranges" [["2" "4"] ["6" "8"]]})
                                :result)
                            "ranges"))))))
  (it "cat names the non-numeric component when a range/ranges value is not a line number"
      (let
        [path
         (write-temp! "range/explain.txt" "L1\nL2\nL3\n")

         cat-tool
         (private-fn "cat-tool")

         msg
         (fn [arg]
           (try (cat-tool path arg) nil (catch clojure.lang.ExceptionInfo e (.getMessage e))))]

        ;; the `"1, x"` shape the user hit — explicit, names `x`, not a generic pair error
        (expect (string/includes? (msg {"ranges" "1, x"}) "non-numeric"))
        (expect (string/includes? (msg {"ranges" "1, x"}) "\"x\""))
        (expect (string/includes? (msg {"ranges" ["1" "x"]}) "\"x\""))
        (expect (string/includes? (msg {"ranges" [[10 "foo"]]}) "\"foo\""))
        (expect (string/includes? (msg {"range" "1, x"}) "non-numeric"))
        (expect (string/includes? (msg {"range" ["1" "x"]}) "\"x\""))
        ;; wrong arity is called out distinctly
        (expect (string/includes? (msg {"ranges" [[1 2 3]]}) "exactly 2 components"))))
  (it
    "path/src/edits tools unwrap the collapsed all-kwargs spec map, never stringify it"
    (let
      [txt
       (write-temp! "kwargs-collapse/a.txt" "hi\n")

       clj
       (write-temp! "kwargs-collapse/n.clj" "(def x 1)\n")

       create-dirs
       (private-fn "create-dirs-tool")

       exists
       (private-fn "exists-tool")

       delete
       (private-fn "delete-tool")

       delete-if
       (private-fn "delete-if-exists-tool")

       copy
       (private-fn "copy-tool")

       move
       (private-fn "move-tool")

       sexpr
       (private-fn "sexpr-tool")

       patch-tool
       (private-fn "patch-tool")]

      ;; Idempotency: copy/move below leave `.bak`/`.bak2` artifacts under the
      ;; shared temp root; a prior run's leftovers make copy/move throw
      ;; FileAlreadyExists. Clear the derived paths before asserting.
      (run! #(fs/delete-if-exists (fs/file %)) [(str txt ".bak") (str txt ".bak2")])
      ;; Each `tool(path=p, ...)` collapses at the Python boundary to ONE map
      ;; `{"path" p, ...}`; a scalar-first arity used to treat that whole map as the
      ;; path/src and stringify it (the `cat` bug). Assert the unwrap for each.
      (expect (true? (get (:result (exists {"path" txt})) "exists")))
      (expect (= txt (get (:result (exists {"path" txt})) "path")))
      (expect (= (str txt "-dir") (get (:result (create-dirs {"path" (str txt "-dir")})) "path")))
      (expect (false? (get (:result (delete-if {"path" (str txt "-absent")})) "deleted")))
      ;; `delete(path=p, is_missing_ok=True)` splits the lone map into path + opts.
      (expect (false? (get (:result (delete {"path" (str txt "-absent") "is_missing_ok" true}))
                           "deleted")))
      ;; `copy/move(src=a, dest=b)` previously threw wrong-arity (1).
      (expect (= txt (get (:result (copy {"src" txt "dest" (str txt ".bak")})) "src")))
      (expect (= (str txt ".bak")
                 (get (:result (move {"src" (str txt ".bak") "dest" (str txt ".bak2")})) "src")))
      ;; `struct_node(path=p)` previously threw a map→String ClassCastException.
      (expect (map? (sexpr {"path" clj})))
      ;; `patch(edits=[…])` unwraps to the edits vector — empty is a CLEAN rejection,
      ;; not a map cast error.
      (expect (throws? clojure.lang.ExceptionInfo #(patch-tool {"edits" []})))))
  (it ":ranges rejects empty or malformed range specs"
      (let
        [path
         (write-temp! "range/bad-multi.txt" "a\nb\nc\n")

         cat-tool
         (private-fn "cat-tool")]

        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool path :ranges [])))
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool path :ranges [[2 1]])))
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool path :ranges [1 2 3]))))))

(defdescribe
  vis-cat-line-passthrough-test
  ;; Regression: an earlier `max-line-length` (2000) cap rewrote every
  ;; long line into `…<+N chars truncated>` and surfaced a
  ;; `:long-line-truncations` count. Same failure pattern as the rg /
  ;; trailer caps removed alongside (see ctx_renderer.clj header note). The structural
  ;; defense is the 256KB per-window byte cap: a pathological single
  ;; line is included whole, the model sees real data, and the next
  ;; window stops with `:truncated? true :next-offset N`.
  (it "long lines pass through verbatim (no per-line cap, no `…<+N chars truncated>` marker)"
      (let
        [long-line
         (apply str (repeat 5000 "x"))

         path
         (write-temp! "long-line.txt" (str long-line "\nshort line\n"))

         read-file
         (private-fn "read-file")

         out
         (read-file path)

         [_ first-text]
         (first (:lines out))]

        (expect (= long-line first-text))
        (expect (= 5000 (count first-text)))
        (expect (not (string/includes? first-text "…<+")))
        (expect (not (string/includes? first-text "chars truncated")))
        (expect (= [2 "short line"] (nth (:lines out) 1)))
        (expect (not (contains? out :long-line-truncations)))))
  (it ":long-line-truncations key is gone from the result map shape entirely"
      (let
        [path
         (write-temp! "short-lines.txt" "a\nb\nc\n")

         read-file
         (private-fn "read-file")

         out
         (read-file path)]

        (expect (not (contains? out :long-line-truncations))))))

(defdescribe
  vis-rg-structured-shape-test
  (it "returns the content shape: :hits :truncated-by + breadth counts"
      (let
        [_
         (write-temp! "rg/a.txt" "alpha needle gamma\nbeta\n")

         _
         (write-temp! "rg/b.txt" "plain line\nanother needle here\n")

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["needle"] "paths" [(temp-dir-path "rg")]})]

        (expect (= #{:hits :truncated-by :total-file-count :total-file-count-exact? :missing}
                   (set (keys out))))
        ;; both files match — breadth == displayed file count, fully counted.
        (expect (= 2 (:total-file-count out)))
        (expect (true? (:total-file-count-exact? out)))
        (expect (vector? (:hits out)))
        ;; Every hit is a clean {:path :line :text :anchor} map (the content-addressed
        ;; anchor lets the model patch straight from a hit), no sentinel.
        (expect (every? #(= #{:path :line :text :anchor} (set (keys %))) (:hits out)))
        (expect (= 2 (count (:hits out))))
        (expect (= :end-of-results (:truncated-by out)))))
  (it "query strings are literal, including pipe characters"
      (let
        [_
         (write-temp! "rgliteral/a.clj" "foo|bar\nfoo only\nbar only\n")

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["foo|bar"] "paths" [(temp-dir-path "rgliteral")] "include" ["*.clj"]})]

        (expect (= ["foo|bar"] (mapv :text (:hits out))))))
  (it "spec {:all [...]} is an OR alias for :query (same-line AND was removed)"
      (let
        [_
         (write-temp! "rgall/a.clj"
                      "(defn info-event [x] x)\n(defn other [x] x)\ninfo-event call\n")

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["defn" "info-event"] "paths" [(temp-dir-path "rgall")] "include" ["*.clj"]})]

        ;; OR: every line mentioning EITHER term is a hit.
        (expect (= ["(defn info-event [x] x)" "(defn other [x] x)" "info-event call"]
                   (mapv :text (:hits out))))))
  (it "spec {:any [...]} is explicit OR"
      (let
        [_
         (write-temp! "rgany/a.clj" "alpha\nbeta\ngamma\n")

         grep
         (private-fn "rg-search")

         out
         (grep {"any" ["alpha" "gamma"] "paths" [(temp-dir-path "rgany")] "include" ["*.clj"]})]

        (expect (= ["alpha" "gamma"] (mapv :text (:hits out))))))
  (it "accepts path vectors, include globs, and dedups overlapping roots"
      (let
        [root
         (temp-dir-path "rgpaths")

         _
         (write-temp! "rgpaths/src/a.clj" "needle clj\n")

         _
         (write-temp! "rgpaths/src/a.txt" "needle txt\n")

         _
         (write-temp! "rgpaths/test/b.cljc" "needle cljc\n")

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["needle"] "paths" [root (str root "/src")] "include" ["*.clj" "*.cljc"]})]

        (expect (= ["needle clj" "needle cljc"] (mapv :text (:hits out))))))
  (it
    "private grep and public rg use the same single spec-map grammar"
    (let
      [_
       (write-temp! "rgsame/a.clj" "needle same\n")

       spec
       {"query" ["needle"] "paths" [(temp-dir-path "rgsame")] "include" ["*.clj"]}

       grep
       (private-fn "rg-search")

       find-tool
       (private-fn "find-tool")

       rg
       ;; find_files now folds the content grep in; flatten its :content block up
       ;; so these content-shape assertions read the same top-level keys rg-tool did.
       (fn [& a]
         (let [e (apply find-tool a)]
           (update e :result #(merge % (get % "content")))))

       ;; rg-tool groups grep's flat :hits into :matches — an ordered
       ;; {path -> {anchor -> text}} map (LinkedHashMap) on the
       ;; model-facing :result; there is no flat :hits vec anymore.
       rg-env
       (rg spec)

       rg-result
       (:result rg-env)

       grep-hits
       (:hits (grep spec))]

      (expect (= :find_files (:symbol rg-env)))
      (expect (instance? java.util.Map (get rg-result "matches")))
      (expect (= (count grep-hits) (get rg-result "hit_count")))
      (expect (= (count (distinct (map :path grep-hits))) (get rg-result "file_count")))
      ;; NO `"spec"` echo in the model-facing payload: echoing the input map
      ;; back taught models a phantom "spec" INPUT key (`rg({..., "spec": {}})`).
      (expect (not (contains? rg-result "spec")))))
  (it
    "IGNORES unknown spec keys (forgiving) but still requires a query"
    (let
      [grep
       (private-fn "rg-search")

       find-tool
       (private-fn "find-tool")

       rg
       (fn [& a]
         (let [e (apply find-tool a)]
           (update e :result #(merge % (get % "content")))))]

      ;; The private ENGINE (`rg-search`) still takes ONE spec map — a bare
      ;; positional string is not a map, so it throws :invalid-rg-spec.
      (expect (throws? clojure.lang.ExceptionInfo #(grep "needle")))
      ;; The public rg now ACCEPTS a positional query + an options map:
      ;; rg("x", {opts}) folds :query in and runs (no arity error).
      (let
        [_
         (write-temp! "rgposopts/a.clj" "needle here\n")

         env
         (rg "needle" {"paths" [(temp-dir-path "rgposopts")] "include" ["*.clj"]})]

        (expect (= :find_files (:symbol env)))
        (expect (= 1 (get (:result env) "hit_count"))))
      ;; UNKNOWN keys are now IGNORED, not fatal — a model that tosses in a stray
      ;; annotation (e.g. `all_note: "defs"`, or an invented `type`/`spec`) still
      ;; gets its search instead of wasting the whole turn. Only recognised keys
      ;; are read; the rest are dropped.
      (let
        [_
         (write-temp! "rglenient/a.txt" "needle here\nsecond needle")

         out
         (grep {"any" ["needle"]
                "paths" [(temp-dir-path "rglenient")]
                "all_note" "defs"
                "type" :clj
                "spec" {}})]

        (expect (map? out))
        (expect (contains? out :hits))
        (expect (pos? (count (:hits out)))))
      ;; ...but the all/any exactly-one grammar IS still enforced: a TYPO'd needle
      ;; key (so neither :all nor :any is present) is caught, not silently run.
      (let
        [err
         (try (grep {"anyy" ["needle"] "paths" ["."]}) nil (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? err))
        (expect (= :ext.foundation.editing/invalid-rg-spec (:type (ex-data err)))))))
  (it ":truncated-by :limit when results exceed the configured limit (default 250)"
      ;; Limit bumped 50 -> 250 in the rg sweep. Use 300 hits to force the cap.
      (let
        [_
         (write-temp! "rgcap/a.txt" (string/join "\n" (map #(str "needle " %) (range 300))))

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["needle"] "paths" [(temp-dir-path "rgcap")]})]

        (expect (= 250 (count (:hits out))))
        (expect (= :limit (:truncated-by out)))))
  (it "empty result still has :truncated-by :end-of-results, never nil"
      (let
        [_
         (write-temp! "rgmiss/a.txt" "nothing matches in here\n")

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["definitely-not-present"] "paths" [(temp-dir-path "rgmiss")]})]

        (expect (= [] (:hits out)))
        (expect (= :end-of-results (:truncated-by out)))))
  ;; Q1+Q2+Q3+Q4 — new option coverage.
  (it ":context N adds N symmetric context lines around each hit"
      (let
        [_path
         (write-temp! "rgctxa/a.txt" "L1\nL2\nMATCH\nL4\nL5\n")

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["MATCH"] "paths" [(temp-dir-path "rgctxa")] "context" 2})

         h
         (first (:hits out))]

        (expect (= [[1 "L1"] [2 "L2"]] (:before h)))
        (expect (= [[4 "L4"] [5 "L5"]] (:after h)))))
  (it ":is_files_only returns distinct paths and never line-level hits"
      (let
        [_
         (write-temp! "rgfo/src/a.py" "alpha\nalpha\nalpha\n")

         _
         (write-temp! "rgfo/src/b.py" "alpha\n")

         _
         (write-temp! "rgfo/src/c.py" "no match\n")

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["alpha"] "paths" [(temp-dir-path "rgfo")] "is_files_only" true})]

        (expect (= #{:files :truncated-by :total-file-count :total-file-count-exact? :missing}
                   (set (keys out))))
        (expect (= 2 (:total-file-count out)))
        (expect (true? (:total-file-count-exact? out)))
        (expect (= 2 (count (:files out))))
        (expect (every? string? (:files out)))))
  (it
    ":context is IGNORED (not rejected) in :is_files_only mode"
    ;; A stray `context` alongside `is_files_only` is harmless — content-mode
    ;; context has no meaning when returning bare file paths, so honor files-only
    ;; instead of hard-failing the whole call.
    (let
      [grep
       (private-fn "rg-search")

       out
       (grep {"any" ["alpha"] "paths" [(temp-dir-path "rgfo")] "is_files_only" true "context" 2})]

      (expect (= #{:files :truncated-by :total-file-count :total-file-count-exact? :missing}
                 (set (keys out))))
      (expect (every? string? (:files out)))))
  (it "keeps a long hit line FULL in the result value (no per-line mutilation)"
      ;; rg never mutilates a hit line. The full :text lives in the result value —
      ;; pickled into `r[\"tN/iN/fN\"]` and rebound into the sandbox — so the model
      ;; recovers the tail with `r[...][\"hits\"][i][\"text\"][N:]` in Python, no `cat`
      ;; roundtrip. Only the WIRE view is bounded (64KB per-observation clip), and
      ;; that clip is non-destructive (it points back to r[...]).
      (let
        [huge
         (apply str (repeat 1000 "x"))

         line
         (str "NEEDLE " huge)

         ; 1007 chars
         _
         (write-temp! "rgfull/big.txt" (str line "\n"))

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["NEEDLE"] "paths" [(temp-dir-path "rgfull")]})

         text
         (:text (first (:hits out)))]

        (expect (= line text)) ; verbatim, full length
        (expect (= (count line) (count text)))
        (expect (not (string/includes? text "clipped"))))))

(defdescribe
  thin-bbfs-wrapper-test
  ;; patch-safe returns a STRUCTURED MAP and never throws on "normal"
  ;; failure paths (anchor-not-found / hashline-out-of-range / stale anchor /
  ;; file-not-found / path-escape / etc.). Throws are reserved for genuinely
  ;; unexpected programming errors (a missing :from_anchor, an unknown key).
  (it "anchors preserve unrelated concurrent changes without an mtime guard"
      (let
        [patch
         (private-fn "patch-safe")

         p
         (write-temp! "bbfs/patch-concurrent.txt" "alpha\nbeta\n")

         anchor
         (patch/line-anchor 1 "alpha")

         _
         (spit p "alpha\nBETA-ELSEWHERE\n")

         r
         (patch [{"path" p "from_anchor" anchor "replace" "ALPHA"}])]

        (expect (true? (:success? r)))
        (expect (= "ALPHA\nBETA-ELSEWHERE\n" (slurp p)))))
  (it "unknown edit keys are rejected (typo guard)"
      (let
        [patch
         (private-fn "patch-safe")

         p
         (write-temp! "bbfs/patch-unknown.txt" "x\n")

         err
         (try (patch
                [{"path" p "from_anchor" (patch/line-anchor 1 "x") "replace" "y" "occurence" 1}])
              nil
              (catch clojure.lang.ExceptionInfo e e))]

        (expect (some? err))
        (expect (string/includes? (ex-message err) "unknown keys"))))
  (it "loop detector: after N consecutive failures on a path, the message carries a hard hint"
      ;; Hits the per-path failure counter. Threshold is private but the
      ;; behaviour is observable on the structured result map.
      (let
        [patch
         (private-fn "patch-safe")

         clear
         (private-fn "clear-patch-fail-count!")

         p
         (write-temp! "bbfs/patch-loop.txt" "alpha\n")

         file
         (fs/file p)

         run!
         (fn []
           (patch [{"path" p "from_anchor" (patch/line-anchor 9 "NOT_HERE") "replace" "x"}]))]

        (clear file)
        (run!)
        (run!)
        (let [r (run!)]
          (expect (false? (:success? r)))
          (expect (some? (:loop-hint r)))
          (expect (string/includes? (:message r) "Patch failed 3 times"))
          (expect (< (count (:message r)) 400))
          (expect (not (string/includes? (:message r) "nth selection")))
          (expect (= 3
                     (-> r
                         :failures
                         first
                         :consecutive-failures))))
        (clear file)))
  (it "successful patch on a path clears the loop counter"
      (let
        [patch
         (private-fn "patch-safe")

         clear
         (private-fn "clear-patch-fail-count!")

         p
         (write-temp! "bbfs/patch-clear.txt" "alpha\n")

         file
         (fs/file p)]

        (clear file)
        (patch [{"path" p "from_anchor" (patch/line-anchor 9 "NOT_HERE") "replace" "x"}])
        (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "BETA"}])
        (let
          [counts2 @(deref (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                            "patch-fail-counts")))]
          (expect (nil? (get counts2 (.getAbsolutePath file)))))))
  (it "all-or-nothing: a single failing edit aborts every prior edit in the batch"
      ;; This guards the core safety invariant. Earlier edits that
      ;; "would have" succeeded against the in-memory plan must NOT
      ;; touch disk when any later edit fails.
      (let
        [patch
         (private-fn "patch-safe")

         p
         (write-temp! "bbfs/patch-aon.txt" "alpha\nbeta\n")

         r
         (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}
                 {"path" p "from_anchor" (patch/line-anchor 9 "NEVER_MATCHES") "replace" "x"}])]

        (expect (false? (:success? r)))
        (expect (= "alpha\nbeta\n" (slurp p)))))
  (it "batch edits resolve against the ORIGINAL snapshot, not each other's output"
      ;; Every hunk anchors to the file as the model last read it (never
      ;; cumulatively), so hashline/ordinal anchors and line numbers stay valid
      ;; across a multi-edit batch. A hunk that targets a PRIOR hunk's output
      ;; therefore won't match the original and the whole batch fails atomically.
      ;; Two INDEPENDENT edits against the original both apply, in one plan.
      (let
        [patch
         (private-fn "patch-safe")

         p
         (write-temp! "bbfs/patch-seq2.txt" "alpha\nbeta\n")

         r
         (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}
                 {"path" p "from_anchor" (patch/line-anchor 2 "beta") "replace" "BETA"}])]

        (expect (true? (:success? r)))
        (expect (= "ALPHA\nBETA\n" (slurp p)))
        (expect (= 1 (count (:plans r))))
        (expect (= "alpha\nbeta\n"
                   (-> r
                       :plans
                       first
                       :before)))
        (expect (= "ALPHA\nBETA\n"
                   (-> r
                       :plans
                       first
                       :after)))))
  (it "editing an unknown path surfaces a structured :file-not-found failure"
      (let
        [patch
         (private-fn "patch-safe")

         fake-path
         "target/editing-test/bbfs/does-not-exist.txt"

         r
         (patch [{"path" fake-path "from_anchor" (patch/line-anchor 1 "x") "replace" "y"}])]

        (expect (false? (:success? r)))
        (expect (= :file-not-found
                   (-> r
                       :failures
                       first
                       :reason)))))
  (it "a changed anchor target fails closed without an mtime guard"
      (let
        [patch
         (private-fn "patch-safe")

         p
         (write-temp! "bbfs/patch-target-changed.txt" "hello\nworld\n")

         anchor
         (patch/line-anchor 1 "hello")

         _
         (spit p "HELLO-ELSEWHERE\nworld\n")

         r
         (patch [{"path" p "from_anchor" anchor "replace" "x"}])]

        (expect (false? (:success? r)))
        (expect (= "HELLO-ELSEWHERE\nworld\n" (slurp p)))))
  (it "empty edit vector is rejected"
      (let [patch (private-fn "patch-safe")]
        (expect (throws? clojure.lang.ExceptionInfo #(patch [])))))
  (it "a single patch invocation cannot move the loop counter past +1 per path"
      ;; Loop counter must be PER INVOCATION, not per failed edit. Two
      ;; failed edits in one call against the same path bump the counter
      ;; once, not twice.
      (let
        [patch
         (private-fn "patch-safe")

         clear
         (private-fn "clear-patch-fail-count!")

         p
         (write-temp! "bbfs/loop-once.txt" "alpha\n")

         file
         (fs/file p)]

        (clear file)
        (patch [{"path" p "from_anchor" (patch/line-anchor 7 "NOPE1") "replace" "x"}
                {"path" p "from_anchor" (patch/line-anchor 8 "NOPE2") "replace" "y"}])
        (let [r (patch [{"path" p "from_anchor" (patch/line-anchor 9 "NOPE3") "replace" "z"}])]
          ;; Failures came from two invocations -> counter is 2.
          (expect (= 2
                     (-> r
                         :failures
                         first
                         :consecutive-failures)))
          (expect (nil? (:loop-hint r))))
        (clear file)))
  (it "patch diagnostics report per-edit reasons in edit order and write nothing"
      (let
        [path
         (write-temp! "bbfs/patch-diagnostics.txt" "alpha\nbeta\nbeta\n")

         patch
         (private-fn "patch-safe")

         ;; First 2 edits resolve cleanly against anchors; last 2 carry
         ;; out-of-range line anchors that cannot locate.
         r
         (patch [{"path" path "from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}
                 {"path" path "from_anchor" (patch/line-anchor 2 "beta") "replace" "BETA"}
                 {"path" path "from_anchor" (patch/line-anchor 8 "missing") "replace" "x"}
                 {"path" path "from_anchor" (patch/line-anchor 9 "other") "replace" "y"}])

         checks
         (:checks r)

         failures
         (:failures r)]

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
      (let
        [path
         (write-temp! "bbfs/meta/x.txt" "x")

         exists?
         (private-fn "exists-safe?")

         delete-if-exists
         (private-fn "delete-if-exists-safe")]

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
      (let
        [present-path
         (write-temp! "exists-shape/yes.txt" "x")

         missing-path
         "exists-shape/no.txt"

         exists-tool
         (private-fn "exists-tool")

         present
         (:result (exists-tool present-path))

         missing
         (:result (exists-tool missing-path))]

        (expect (map? present))
        (expect (true? (get present "exists")))
        (expect (= present-path (get present "path")))
        (expect (map? missing))
        (expect (false? (get missing "exists")))
        (expect (= missing-path (get missing "path")))))
  (it "keeps exists shape details out of the compact prompt and PYTHON in symbol docs"
      (let
        [exists-symbol
         (some #(when (= 'file-exists (:ext.symbol/symbol %)) %) @editing/editing-symbols)

         d
         (:ext.symbol/doc exists-symbol)]

        ;; the result shape lives in the symbol doc, not the compact prompt
        (expect (not (string/includes? @editing/editing-prompt "\"exists\": bool")))
        ;; doc states the Python result shape — and NOT the old Clojure `:exists?`
        (expect (string/includes? d "\"exists\""))
        (expect (not (string/includes? d ":exists?")))))
  (it "bash helpers fully removed from the editing core"
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                     "run-bash-safe"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                     "bash-tool"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                     "strict-bash-command"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                     "coerce-bash-opts"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                     "bash-warnings"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                     "channel-render-bash"))))
      (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                     "journal-render-bash"))))))

(defdescribe
  delete-tool-shape-test
  ;; Regression: `delete-tool` used to set `:result nil`. The channel
  ;; preview then painted `DELETE nil` and `(def r (delete p))`
  ;; consumers couldn't read `(:path r)` (same parity bug `exists?`
  ;; already fixed). All `v/*` tools now return a map shape.
  (it "delete returns {:op :delete :path P :deleted? true} (no bare nil)"
      (let
        [delete-tool
         (private-fn "delete-tool")

         p
         (write-temp! "delete-shape/x.txt" "goodbye\n")

         envelope
         (delete-tool p)]

        (expect (true? (:success? envelope)))
        (expect (= :delete (:symbol envelope)))
        (expect (= :delete (:symbol envelope)))
        (let [r (:result envelope)]
          (expect (map? r))
          (expect (= p (get r "path")))
          (expect (true? (get r "deleted"))))))
  (it "delete-if-exists returns the same map shape with :deleted? reflecting the actual outcome"
      (let
        [delete-if
         (private-fn "delete-if-exists-tool")

         p
         (write-temp! "delete-shape/here.txt" "x\n")

         present
         (:result (delete-if p))

         absent
         (:result (delete-if p))]

        ;; First call deletes the file; the result map carries "deleted" true.
        (expect (map? present))
        (expect (= p (get present "path")))
        (expect (true? (get present "deleted")))
        ;; Second call hits an already-absent path; the map stays the same shape.
        (expect (map? absent))
        (expect (= p (get absent "path")))
        (expect (false? (get absent "deleted")))))
  (it "delete recursively removes non-empty directories"
      (let
        [delete-tool
         (private-fn "delete-tool")

         dir
         (temp-dir-path "delete-shape/tree")

         nested-file
         (fs/file dir "nested" "x.txt")]

        (fs/create-dirs (fs/parent nested-file))
        (spit nested-file "x\n")
        (let [r (:result (delete-tool dir))]
          (expect (true? (get r "deleted")))
          (expect (false? (fs/exists? dir))))))
  (it "delete-if-exists recursively removes non-empty directories"
      (let
        [delete-if
         (private-fn "delete-if-exists-tool")

         dir
         (temp-dir-path "delete-shape/tree-if-exists")

         nested-file
         (fs/file dir "nested" "x.txt")]

        (fs/create-dirs (fs/parent nested-file))
        (spit nested-file "x\n")
        (let [r (:result (delete-if dir))]
          (expect (true? (get r "deleted")))
          (expect (false? (fs/exists? dir)))))))

(defdescribe patch-summary-shape-test
             ;; The summary IS what the model reads back as the patch result
             ;; AND what the channel renderer projects. Every key counts; redundant
             ;; signal pollutes the iteration trailer.
             (it "anchor-located edit emits only path, operation, change flag, and diff"
                 (let
                   [patch
                    (private-fn "patch-safe")

                    summary
                    (private-fn "patch-result-file-summary")

                    p
                    (write-temp! "summary/exact.txt" "alpha\nbeta\n")

                    r
                    (patch
                      [{"path" p "from_anchor" (patch/line-anchor 1 "alpha") "replace" "ALPHA"}])

                    s
                    (summary (first (:plans r)))]

                   (expect (true? (:success? r)))
                   (expect (= #{"path" "op" "changed" "diff"} (set (keys s))))
                   (expect (not (contains? s "lines_before")))
                   (expect (not (contains? s "lines_after")))
                   (expect (not (contains? s "delta_lines"))))))

(defdescribe patch-diff-text-test
             (it "patch diff stays compact for large files"
                 (let
                   [diff-fn
                    (private-fn "unified-diff-text")

                    before
                    (string/join "\n" (map #(str "line-" %) (range 1500)))

                    after
                    (string/replace before "line-750" "LINE-750")

                    out
                    (diff-fn before after)

                    lines
                    (string/split-lines out)]

                   (expect (< (count lines) 50))
                   (expect (string/includes? out "@@"))
                   (expect (string/includes? out "-line-750"))
                   (expect (string/includes? out "+LINE-750"))))
             (it "patch diff handles insert, delete, and all-different cases as bounded previews"
                 (let
                   [diff-fn
                    (private-fn "unified-diff-text")

                    inserted
                    (diff-fn "a\nb\nc" "a\nX\nb\nc")

                    deleted
                    (diff-fn "a\nb\nc" "a\nb")

                    before
                    (string/join "\n" (map #(str "line-" %) (range 300)))

                    after
                    (string/join "\n" (map #(str "other-" %) (range 300)))

                    changed
                    (diff-fn before after)]

                   (expect (string/includes? inserted "+X"))
                   (expect (not (string/includes? inserted "-a")))
                   (expect (string/includes? deleted "-c"))
                   (expect (< (count (string/split-lines changed)) 260))
                   (expect (string/includes? changed "diff truncated")))))

(defdescribe
  tool-envelope-test
  (it "tool wrappers return the required contract keys"
      (let
        [path
         (write-temp! "contract/read.txt" "alpha\nbeta\n")

         cat-tool
         (private-fn "cat-tool")

         out
         (cat-tool path)

         required
         #{:success? :result :error :symbol :tag :metadata}]

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
      (let
        [cat-symbol
         (private-fn "cat-symbol")

         on-error
         (:ext.symbol/on-error-fn cat-symbol)

         out
         (:result (on-error (ex-info "boom" {}) nil nil ["missing.txt"]))]

        (expect (false? (:success? out)))
        (expect (nil? (:result out)))
        ;; :trace is a preformatted string; first line carries the
        ;; underlying class name.
        (expect (string? (get-in out [:error :trace])))
        (expect (string/includes? (get-in out [:error :trace]) "ExceptionInfo"))
        (expect (not (contains? out :markdown))))))

(defdescribe
  vis-patch-hashline-test
  ;; End-to-end content-addressed editing: read hashes from cat, then
  ;; patch by :from_anchor / :to_anchor. The hash anchors come straight from
  ;; the read's `:anchors` map (same value rendered in the cat gutter),
  ;; and self-locate against live disk content on apply.
  (it "patch :from_anchor replaces a single content-anchored line"
      (let
        [path
         (write-temp! "hashline/single.txt" "alpha first\nbeta second\ngamma third\n")

         read-file
         (private-fn "read-file")

         patch
         (private-fn "patch-safe")

         hashes
         (:anchors (read-file path))

         h2
         (get hashes 2)

         r
         (patch [{"path" path "from_anchor" h2 "replace" "BETA REPLACED"}])]

        (expect (true? (:success? r)))
        (expect (= "alpha first\nBETA REPLACED\ngamma third\n" (slurp path)))))
  (it "patch :from_anchor uses exact line+hash even when the hash is duplicated nearby"
      (let
        [path
         (write-temp! "hashline/duplicate-hash-exact-line.txt"
                      (str "keep\n" "}\n" "}\n" "}\n" "tail\n"))

         patch
         (private-fn "patch-safe")

         r
         (patch [{"path" path "from_anchor" (patch/line-anchor 3 "}") "replace" "TARGET"}])]

        (expect (true? (:success? r)))
        (expect (= "keep\n}\nTARGET\n}\ntail\n" (slurp path)))))
  (it "patch :from_anchor + :to_anchor replaces an inclusive range"
      (let
        [path
         (write-temp! "hashline/range.txt" "a\nb\nc\nd\n")

         read-file
         (private-fn "read-file")

         patch
         (private-fn "patch-safe")

         hashes
         (:anchors (read-file path))

         r
         (patch
           [{"path" path "from_anchor" (get hashes 1) "to_anchor" (get hashes 3) "replace" "X"}])]

        (expect (true? (:success? r)))
        (expect (= "X\nd\n" (slurp path)))))
  (it
    "duplicate lines are surfaced as distinct `lineno:hash` anchors; a BARE hash that hits >1 line is refused"
    (let
      [path
       (write-temp! "hashline/dup.txt" "x\ny\nx\n")

       read-file
       (private-fn "read-file")

       patch
       (private-fn "patch-safe")

       ;; The line coordinate disambiguates duplicate content. A hash without
       ;; a line coordinate is malformed.
       hashes
       (:anchors (read-file path))

       r
       (patch [{"path" path "from_anchor" (patch/line-hash "x") "replace" "NEW"}])]

      (expect (= (patch/line-anchor 1 "x") (get hashes 1))) ;; 1st dup → 1:hash
      (expect (= (patch/line-anchor 3 "x") (get hashes 3))) ;; 2nd dup → 3:hash
      (expect (= (patch/line-anchor 2 "y") (get hashes 2))) ;; unique line too
      (expect (false? (:success? r)))
      (expect (= :hashline-malformed
                 (-> r
                     :failures
                     first
                     :reason)))
      ;; file untouched
      (expect (= "x\ny\nx\n" (slurp path)))))
  (it "patch with ZERO locators still throws"
      (let
        [path
         (write-temp! "hashline/none.txt" "a\n")

         patch
         (private-fn "patch-safe")]

        (expect (throws? clojure.lang.ExceptionInfo #(patch [{"path" path "replace" "Z"}]))))))

(defdescribe
  vis-cat-anchor-read-test
  ;; cat :anchor — the READ twin of patch :from_anchor. Re-read a kept region
  ;; by its content hash, addressed by content not drifting line numbers.
  (let
    [cat-tool
     (private-fn "cat-tool")

     path
     (write-temp! "hashread/probe.clj" "(ns probe)\n(def alpha 1)\n(def beta 2)\n(def gamma 3)\n")

     h-beta
     (patch/line-anchor 3 "(def beta 2)")

     h-gamma
     (patch/line-anchor 4 "(def gamma 3)")]

    (it "(cat path :anchor H) reads the single line whose content hash is H"
        (let [out (:result (cat-tool path :anchor h-beta))]
          (expect (= [[3 "(def beta 2)"]] (patch/anchor-map->tuples (get out "anchors"))))
          (expect (= [3 3] (get out "range")))))
    (it "(cat path :anchor H1 H2) reads the inclusive content-addressed window"
        (let [out (:result (cat-tool path :anchor h-beta h-gamma))]
          (expect (= (numbered-tuples 3 ["(def beta 2)" "(def gamma 3)"])
                     (patch/anchor-map->tuples (get out "anchors"))))
          (expect (= [3 4] (get out "range")))))
    (it "addresses by CONTENT — survives line drift (prepend shifts numbers)"
        (let
          [p2
           (write-temp! "hashread/drift.clj" "(ns probe)\n(def beta 2)\n")

           _
           (spit (fs/file p2) (str ";; banner\n;; banner2\n" (slurp (fs/file p2))))

           out
           (:result (cat-tool p2 :anchor (patch/line-anchor 2 "(def beta 2)")))]

          ;; beta moved from line 2 to line 4; within the drift tolerance the
          ;; `2:hash` anchor still resolves it by content
          (expect (= [[4 "(def beta 2)"]] (patch/anchor-map->tuples (get out "anchors"))))))
    (it "a malformed anchor (no line number) throws back to cat for fresh :anchors"
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool path :anchor "zzzz"))))
    (it "a stale hash on a live line falls back to the line number — a READ is tolerant"
        ;; `3:<wrong-hash>`: line 3 exists but the hash matches no live line. Unlike
        ;; a WRITE, a non-destructive READ must not be blocked — it falls back to the
        ;; anchor's line number and flags `anchors_stale`, returning FRESH anchors.
        (let [out (:result (cat-tool path :anchor "3:dead"))]
          (expect (= [[3 "(def beta 2)"]] (patch/anchor-map->tuples (get out "anchors"))))
          (expect (= [3 3] (get out "range")))
          (expect (true? (get out "anchors_stale")))))
    (it "an unknown 4-arity mode throws"
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool path :nonsense h-beta h-gamma))))))

(defdescribe
  vis-cat-anchor-line-number-coercion-test
  ;; Models routinely send bare LINE NUMBERS where a `lineno:hash` anchor belongs
  ;; (session f81cd89b: `{"anchor": "9357, 9412"}` → :hashline-malformed). Coerce
  ;; every line-number shape into a line-RANGE read; real anchors fall through.
  (let
    [cat-tool
     (private-fn "cat-tool")

     norm
     (private-fn "normalize-cat-anchor-option")

     ->range
     (private-fn "cat-anchor->line-range")

     path
     (write-temp! "linenum/probe.clj"
                  (string/join "\n" (map #(str "(def x" % " " % ")") (range 1 20))))]

    (it "normalize splits a comma-joined anchor string into a real [from to] vector"
        (expect (= ["9357" "9412"] (norm "9357, 9412")))
        (expect (= ["3:abc" "5:def"] (norm "3:abc, 5:def")))
        (expect (= [9357 9412] (norm "[9357, 9412]")))
        (expect (= "325:0e3" (norm "325:0e3"))))
    (it "cat-anchor->line-range maps bare line numbers to a range, real anchors to nil"
        (expect (= [9357 9412] (->range ["9357" "9412"])))
        (expect (= [9 12] (->range [9 12])))
        (expect (= [9 9] (->range 9)))
        (expect (= [9 9] (->range "9")))
        (expect (nil? (->range ["3:abc" "5:def"])))
        (expect (nil? (->range "325:0e3"))))
    (it "a comma-joined line-number anchor reads the inclusive line RANGE (was :hashline-malformed)"
        (let [res (cat-tool path {"anchor" "3, 6"})]
          (expect (= [3 6] (get-in res [:metadata :range])))
          (expect (= [3 4 5 6]
                     (mapv first (patch/anchor-map->tuples (get (:result res) "anchors")))))))
    (it "an integer-vector anchor and a lone integer anchor both read lines directly"
        (expect (= [3 6] (get-in (cat-tool path {"anchor" [3 6]}) [:metadata :range])))
        (expect (= [5 5] (get-in (cat-tool path {"anchor" 5}) [:metadata :range])))
        (expect (= [5 5] (get-in (cat-tool path {"anchor" "5"}) [:metadata :range]))))))

;; =============================================================================
;; Patch: duplicate-line anchors in a multi-edit batch, resolved vs the ORIGINAL
;; snapshot (regression for the cumulative-resolution bug that made a batch's
;; later anchors drift and fail). Duplicate lines are now told apart by their
;; LINE NUMBER (`lineno:hash`), not a `#N` ordinal.
;; =============================================================================

(defdescribe
  patch-dup-line-batch-test
  (it "dup-line edits in one batch all resolve against the ORIGINAL and apply atomically"
      (let
        [patch
         (private-fn "patch-safe")

         p
         (write-temp! "ord/dup.txt" "x\nDUP\ny\nDUP\nz\nDUP\n")

         r
         (patch [{"path" p "from_anchor" (patch/line-anchor 2 "DUP") "replace" "DUP1"}
                 {"path" p "from_anchor" (patch/line-anchor 6 "DUP") "replace" "DUP3"}])]

        (expect (true? (:success? r)))
        ;; lines 2 and 6 edited, line 4 untouched — line numbers resolve vs the
        ;; original snapshot, no drift.
        (expect (= "x\nDUP1\ny\nDUP\nz\nDUP3\n" (slurp p)))))
  (it "all three duplicate lines editable in one batch"
      (let
        [patch
         (private-fn "patch-safe")

         p
         (write-temp! "ord/dup3.txt" "DUP\nDUP\nDUP\n")

         r
         (patch [{"path" p "from_anchor" (patch/line-anchor 1 "DUP") "replace" "A"}
                 {"path" p "from_anchor" (patch/line-anchor 2 "DUP") "replace" "B"}
                 {"path" p "from_anchor" (patch/line-anchor 3 "DUP") "replace" "C"}])]

        (expect (true? (:success? r)))
        (expect (= "A\nB\nC\n" (slurp p)))))
  (it "overlapping edits in one batch are rejected — nothing written (atomic)"
      (let
        [patch
         (private-fn "patch-safe")

         p
         (write-temp! "ord/over.txt" "alpha beta\n")

         r
         (patch [{"path" p "from_anchor" (patch/line-anchor 1 "alpha beta") "replace" "X"}
                 {"path" p "from_anchor" (patch/line-anchor 1 "alpha beta") "replace" "Y"}])]

        (expect (false? (:success? r)))
        (expect (= :overlapping-edits
                   (-> r
                       :failures
                       last
                       :reason)))
        (expect (= "alpha beta\n" (slurp p))))))

;; =============================================================================
;; rg hits carry the content-addressed :anchor anchor (cat/rg parity), so a hit
;; is directly patchable without a follow-up cat.
;; =============================================================================

(defdescribe
  rg-returns-anchor-test
  (let
    [rg-search
     (private-fn "rg-search")

     patch
     (private-fn "patch-safe")]

    (it "a content hit carries its `lineno:hash` anchor and that anchor patches the line"
        (let
          [p
           (write-temp! "rgh/uniq.clj" "(def a 1)\n(def b 2)\n(def c 3)\n")

           res
           (rg-search {"any" ["def b"] "paths" [p]})

           hit
           (first (:hits res))]

          (expect (= (patch/line-anchor 2 "(def b 2)") (:anchor hit)))
          (let [r (patch [{"path" p "from_anchor" (:anchor hit) "replace" "(def b 200)"}])]
            (expect (true? (:success? r)))
            (expect (string/includes? (slurp p) "(def b 200)")))))
    (it "hits on a DUPLICATED line carry distinct `lineno:hash` anchors (line number disambiguates)"
        (let
          [p
           (write-temp! "rgh/dup.clj" "(def x 1)\n(other)\n(def x 1)\n")

           res
           (rg-search {"any" ["def x"] "paths" [p]})

           hashes
           (map :anchor (:hits res))]

          (expect (= [(patch/line-anchor 1 "(def x 1)") (patch/line-anchor 3 "(def x 1)")]
                     hashes))))))

(defn- mk-tmp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defdescribe
  multi-root-safe-path-test
  (it "accepts paths under a LIVE filesystem root (trunk==clone), rejects paths outside every root"
      (let
        [safe-path
         (private-fn "safe-path")

         primary
         (.getCanonicalPath (java.io.File. (System/getProperty "user.dir")))

         ctx-root
         (mk-tmp-dir "vis-ctxroot")]

        (binding
          [workspace/*workspace-root*
           primary

           workspace/*filesystem-roots*
           [{:trunk ctx-root :clone ctx-root}]]

          (expect (string/starts-with? (.getPath ^java.io.File (safe-path "deps.edn")) primary))
          (expect (string/starts-with? (.getPath ^java.io.File
                                                 (safe-path (str ctx-root "/sub/file.clj")))
                                       ctx-root))
          (expect (throws? clojure.lang.ExceptionInfo #(safe-path "/etc/hosts")))
          (expect (throws? clojure.lang.ExceptionInfo
                           #(safe-path (str ctx-root "/../../../../etc/hosts"))))
          ;; /tmp (and $TMPDIR) are ALWAYS reachable, independent of the bound
          ;; roots — scratch under the system temp dir just works.
          (binding [workspace/*filesystem-roots* nil]
            (expect (some? (safe-path "/tmp/vis-safe-path-probe.txt")))
            (expect (some? (safe-path (str (System/getProperty "java.io.tmpdir")
                                           "/vis-safe-path-probe.txt"))))
            ;; ...but a NON-temp path outside every root is still rejected.
            (expect (throws? clojure.lang.ExceptionInfo #(safe-path "/etc/hosts")))))))
  (it
    "expands a leading ~ / ~/ so a home-relative path resolves to the real file (regression: was treated as a literal ~ segment under cwd)"
    (let
      [safe-path
       (private-fn "safe-path")

       home
       (.getCanonicalPath (java.io.File. (System/getProperty "user.home")))]

      (binding
        [workspace/*workspace-root*
         home

         workspace/*filesystem-roots*
         nil]

        ;; ~/x and <home>/x must resolve to the SAME real path, NOT <home>/~/x
        (expect (= (.getCanonicalPath ^java.io.File (safe-path (str home "/some-file.txt")))
                   (.getCanonicalPath ^java.io.File (safe-path "~/some-file.txt"))))
        (expect (not (string/includes? (.getPath ^java.io.File (safe-path "~/some-file.txt")) "~")))
        ;; bare ~ resolves to home itself
        (expect (= home (.getCanonicalPath ^java.io.File (safe-path "~")))))))
  (it
    "ISOLATED filesystem root: address by trunk → edits land in clone, display shows trunk"
    (let
      [safe-path
       (private-fn "safe-path")

       rel-path
       (private-fn "rel-path")

       primary
       (mk-tmp-dir "vis-prim")

       trunk
       (mk-tmp-dir "vis-trunk")

       clone
       (mk-tmp-dir "vis-clone")]

      (spit (java.io.File. ^String clone "x.txt") "in-clone")
      (spit (java.io.File. ^String trunk "x.txt") "in-trunk")
      (binding
        [workspace/*workspace-root*
         primary

         workspace/*filesystem-roots*
         [{:trunk trunk :clone clone}]]

        (let [f (safe-path (str trunk "/x.txt"))]
          (expect (string/starts-with? (.getCanonicalPath ^java.io.File f) clone)) ;; lands in clone
          (expect (= "in-clone" (slurp f)))                                        ;; reads clone, NOT trunk
          (expect (= (.replace (str trunk "/x.txt") "\\" "/") (rel-path f)))) ;; display shows real trunk path, `/`-normalized
        (expect (throws? clojure.lang.ExceptionInfo #(safe-path "/etc/hosts")))))))

(defdescribe
  native-temp-write-capture-test
  (it "a write to /tmp streams to the DB attachment sink; a workspace write does NOT"
      (let
        [write-safe
         (private-fn "write-safe")

         sink
         (atom [])

         seen
         (atom #{})

         tmp
         (str (System/getProperty "java.io.tmpdir") "/vis-native-tmpcap-" (System/nanoTime) ".txt")

         ws
         "target/editing-test/vis-native-nontmp.txt"]

        (fs/create-dirs "target/editing-test")
        (binding
          [mpl-capture/*attachment-sink*
           sink

           mpl-capture/*outbox-seen*
           seen]

          (expect (:success? (write-safe {"path" tmp "content" "captured tmp bytes"})))
          (expect (:success? (write-safe {"path" ws "content" "not captured"}))))
        ;; ONLY the /tmp write reached the sink — the workspace write is untouched.
        (expect (= 1 (count @sink)))
        (let [[att] @sink]
          (expect (string/ends-with? (:filename att) ".txt"))
          (expect (= "file" (:kind att)))))))

(defdescribe
  patch-input-contract-test
  (let [coerce (private-fn "coerce-patch-edits")]
    (it "accepts a valid anchor edit (from_anchor + replace)"
        (let [out (coerce [{"path" "p.txt" "from_anchor" "12:abc" "replace" "new"}])]
          (expect (= 1 (count out)))
          (expect (= "12:abc" (get (first out) "from_anchor")))))
    (it "accepts only a non-empty vector of edit maps"
        (expect (throws? clojure.lang.ExceptionInfo #(coerce [])))
        (expect (throws? clojure.lang.ExceptionInfo
                         #(coerce {"path" "p.txt" "from_anchor" "12:abc" "replace" "new"}))))
    (it "rejects unknown edit keys"
        (let
          [ex (try (coerce [{"path" "p.txt" "from_anchor" "1:abc" "replace" "new" "typo" true}])
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (expect (some? ex))
          (expect (string/includes? (ex-message ex) "unknown keys"))
          (expect (= ["typo"] (:unknown (ex-data ex))))))
    (it "rejects file-wide mtime/size guards"
        (doseq [field ["expected_mtime" "expected_size"]]
          (let
            [ex (try (coerce [{"path" "p.txt" "from_anchor" "1:abc" "replace" "new" field 1}])
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? ex))
            (expect (= [field] (:unknown (ex-data ex)))))))
    (it "rejects an edit with no locator"
        (expect (throws? clojure.lang.ExceptionInfo #(coerce [{"path" "p.txt" "replace" "x"}]))))))

(defdescribe
  patch-stale-anchor-diagnostic-test
  (let [explain (private-fn "explain-failure")]
    (it "shows the current line before suggesting reuse of its fresh anchor"
        (let
          [message (explain {:edit-index 0
                             :path "src/example.clj"
                             :reason :hashline-not-found
                             :hash-error {:which :from
                                          :stated-line 835
                                          :current-anchor "835:2b5"
                                          :current-text "return null;"}})]
          (expect (string/includes? message "line 835 changed — now `835:2b5`: \"return null;\"."))
          (expect (string/includes? message
                                    "Confirm this is your target before reusing the anchor."))))
    (it "bounds long current-line previews"
        (let
          [prefix (apply str (repeat 80 "x"))
           message (explain {:edit-index 0
                             :path "src/example.clj"
                             :reason :hashline-not-found
                             :hash-error {:which :from
                                          :stated-line 835
                                          :current-anchor "835:2b5"
                                          :current-text (str prefix "SHOULD-NOT-APPEAR")}})]

          (expect (string/includes? message (str "\"" prefix "…\"")))
          (expect (not (string/includes? message "SHOULD-NOT-APPEAR")))))))

(defdescribe editing-native-contract-test
             (let
               [patch-description
                (:ext.symbol/description editing/patch-symbol)

                patch-schema
                (:ext.symbol/schema editing/patch-symbol)

                cat-description
                (:ext.symbol/description editing/cat-symbol)]

               (it "keeps anchor lifecycle policy in the native descriptions"
                   (expect (string/includes? patch-description "fresh anchors"))
                   (expect (string/includes? patch-description "stales all anchors"))
                   (expect (string/includes? cat-description "invalidates returned anchors")))
               (it "keeps exact patch fields only in the JSON Schema"
                   (expect (not (string/includes? patch-description "from_anchor")))
                   (expect (contains? (get-in patch-schema [:properties "edits" :items :properties])
                                      "from_anchor")))
               (it "advertises exactly the canonical patch fields"
                   (let [fields (get-in patch-schema [:properties "edits" :items :properties])]
                     (expect (= #{"path" "from_anchor" "to_anchor" "replace"}
                                (set (keys fields))))))))

(defdescribe editing-native-schema-shape-test
             (it "find_files' query is a scalar-or-list contract (name + content OR)"
                 (let [query (get-in editing/find-symbol [:ext.symbol/schema :properties "query"])]
                   (expect (= #{"string" "array"} (set (map :type (:oneOf query)))))))
             (it "advertises directory-only paths and no context-lines argument"
                 (let
                   [properties
                    (get-in editing/find-symbol [:ext.symbol/schema :properties])

                    paths
                    (get properties "paths")]

                   (expect (contains? properties "paths"))
                   (expect (string/includes? (:description paths) "Directory scopes only"))
                   (expect (not (contains? properties "context")))))
             (it "rejects ambiguous cat selectors at the native boundary"
                 (let [schema (:ext.symbol/schema editing/cat-symbol)]
                   (expect (= 2 (:maxProperties schema)))
                   (expect (= 2 (get-in schema [:properties "range" :minItems])))
                   (expect (= 2 (get-in schema [:properties "range" :maxItems])))))
             (it "uses one portable patch shape with a path on every edit"
                 (let [schema (:ext.symbol/schema editing/patch-symbol)]
                   (expect (= "object" (:type schema)))
                   (expect (not-any? #(contains? schema %) [:oneOf :allOf :anyOf]))
                   (expect (= #{"edits"} (set (keys (:properties schema)))))
                   (expect (= ["path" "from_anchor" "replace"]
                              (get-in schema [:properties "edits" :items :required]))))))

(defdescribe
  outline-path-resolution-test
  "Regression: index must route through safe-path like every other file tool —
   it used the RAW path (slurp resolves against the JVM user.dir, not the
   workspace cwd), so a nested `src/foo.clj` 404'd under `vis --source` while cat
   found it. The proof is that safe-path confinement now applies to index."
  (let [index-tool (private-fn "index-tool")]
    (it "resolves a NESTED workspace-relative path"
        (let
          [dir (temp-dir-path "outline-nested/src")
           _ (spit (fs/file (str dir "/foo.clj")) "(ns foo)\n(defn bar [x] (+ x 1))\n")
           r (index-tool (str (temp-root) "/outline-nested/src/foo.clj"))]

          (expect (:success? r))
          (expect (clojure.string/includes? (str (get-in r [:result "skeleton"])) "bar"))
          ;; the STRUCTURED sibling: machine-addressable definitions (no skeleton parsing),
          ;; each row the same shape as an occurrences def — name/kind/anchor/end_anchor.
          (let
            [defs (get-in r [:result "definitions"])
             bar (first (filter #(= "bar" (get % "name")) defs))]

            (expect (vector? defs))
            (expect (= "fn" (get bar "kind")))
            (expect (= "[x]" (get bar "signature")))
            (expect (string? (get bar "anchor")))
            (expect (string? (get bar "end_anchor")))
            (expect (= 0 (get bar "depth"))))))
    (it "REFUSES a path that escapes the workspace (proves safe-path confinement)"
        (expect (true? (try (index-tool "/etc/hosts")
                            false
                            (catch clojure.lang.ExceptionInfo _ true)))))))

(defdescribe
  anchor-zipper-tool-test
  "A row anchor from outline/occurrences/cat is now a first-class zipper handle:
   sexpr can enter at it, and struct_patch can edit the corresponding node."
  (let
    [sexpr-tool
     (private-fn "sexpr-tool")

     struct-patch
     (private-fn "struct-patch-tool")]

    (it "sexpr enters the zipper at a lineno:hash anchor"
        (let
          [path
           (write-temp! "anchor-zipper/read.clj"
                        "(ns my.app)\n\n(defn foo [x]\n  (+ x 1))\n\n(defn bar [y]\n  (* y 2))\n")

           anchor
           (patch/line-anchor 6 "(defn bar [y]")

           r
           (sexpr-tool path {"anchor" anchor})]

          (expect (:success? r))
          (expect (= [2] (get-in r [:result "path"])))
          (expect (clojure.string/includes? (get-in r [:result "text"]) "defn bar"))))
    (it "struct_patch edits the node addressed by a lineno:hash anchor"
        (let
          [path
           (write-temp! "anchor-zipper/write.clj"
                        "(ns my.app)\n\n(defn foo [x]\n  (+ x 1))\n\n(defn bar [y]\n  (* y 2))\n")

           anchor
           (patch/line-anchor 6 "(defn bar [y]")

           r
           (struct-patch
             {"path" path "op" "replace" "anchor" anchor "code" "(defn bar [y]\n  (- y 2))"})]

          (expect (:success? r))
          (expect (clojure.string/includes? (slurp (fs/file path)) "(- y 2)"))))
    (it "replace_node reuses node-addressing semantics when an anchor locates the node"
        (let
          [path
           (write-temp! "anchor-zipper/replace-node.clj"
                        "(ns my.app)\n\n(defn bar [y]\n  (* y 2))\n")

           anchor
           (patch/line-anchor 3 "(defn bar [y]")

           r
           (struct-patch {"path" path
                          "op" "replace_node"
                          "anchor" anchor
                          "match" "(defn bar [y]\n  (* y 2))"
                          "code" "(defn bar [y]\n  (+ y 2))"})]

          (expect (:success? r))
          (expect (clojure.string/includes? (slurp (fs/file path)) "(+ y 2)"))))
    (it "stale anchors are refused before zipper navigation"
        (let
          [path
           (write-temp! "anchor-zipper/stale.clj" "(ns my.app)\n\n(defn bar [y]\n  (* y 2))\n")

           stale
           (patch/line-anchor 3 "(defn bar [y]")]

          (spit (fs/file path) "(ns my.app)\n\n(defn bar [z]\n  (* z 2))\n")
          (let [r (sexpr-tool path {"anchor" stale})]
            (expect (false? (:success? r)))
            (expect (= :hashline-not-found (get-in r [:error :reason]))))))))


(defdescribe
  project-rename-test
  "Cross-file rename via tree-sitter. For a Clojure ns it rewrites the ns form +
   :require targets + qualified usages, keeps local :as aliases. (rg prefilter is
   redef'd to a known file set so the test doesn't depend on gitignore.)"
  (let [rename-tool (private-fn "symbol-rename-tool")]
    (it "renames a Clojure namespace across files"
        (let
          [_ (temp-dir-path "nsrename")
           f1 (str (temp-root) "/nsrename/bar.clj")
           f2 (str (temp-root) "/nsrename/app.clj")]

          (spit (fs/file f1) "(ns foo.bar)\n(defn h [x] (inc x))\n")
          (spit
            (fs/file f2)
            "(ns app\n  (:require [foo.bar :as fb]))\n(defn run [] (+ (foo.bar/h 1) (fb/h 2)))\n")
          (with-redefs [editing/rg-search (constantly {:files [f1 f2]})]
            (let [r (rename-tool "foo.bar" "foo.baz")]
              (expect (:success? r))
              (expect (= 2 (get-in r [:result "file_count"])))))
          (let
            [a (slurp (fs/file f1))
             b (slurp (fs/file f2))]

            (expect (clojure.string/includes? a "(ns foo.baz)"))
            (expect (clojure.string/includes? b "[foo.baz :as fb]")) ; require target renamed
            (expect (clojure.string/includes? b "foo.baz/h 1"))      ; qualified usage renamed
            (expect (clojure.string/includes? b "fb/h 2"))           ; local alias UNCHANGED
            (expect (not (clojure.string/includes? b "foo.bar")))))) ; nothing left
    (it "skips files that don't mention the name (file_count reflects only changes)"
        (let
          [_ (temp-dir-path "nsrename2")
           f1 (str (temp-root) "/nsrename2/has.clj")
           f2 (str (temp-root) "/nsrename2/none.clj")]

          (spit (fs/file f1) "(ns has)\n(zz/q 1)\n")
          (spit (fs/file f2) "(ns none)\n(defn k [] 1)\n")
          (with-redefs [editing/rg-search (constantly {:files [f1 f2]})]
            (let [r (rename-tool "zz" "ww")]
              (expect (= 1 (get-in r [:result "file_count"])))))
          (expect (clojure.string/includes? (slurp (fs/file f1)) "ww/q"))
          (expect (= "(ns none)\n(defn k [] 1)\n" (slurp (fs/file f2))))))))

(defdescribe render-patch-result-compact-headline-test
             (let [render @#'editing/render-patch-result]
               (it "uses only path chips because the tool badge already names the operation"
                   (let
                     [card (render [{"path" "src/a.clj" "op" "update" "changed" true "diff" "+a"}
                                    {"path" "src/b.clj" "op" "add" "changed" true "diff" "+b"}])]
                     (expect (= "`src/a.clj`, `src/b.clj`" (:summary card)))
                     (expect (not (clojure.string/includes? (:body card) "update")))
                     (expect (not (clojure.string/includes? (:body card) "add `")))))
               (it "widens the diff fence past any backtick run in the diff so an inner ``` fence never closes it early"
                   ;; Editing a doc that shows ```diff examples produces a diff
                   ;; whose context lines carry a bare ``` — a fixed 3-backtick
                   ;; wrapper closed early and the rest rendered as prose.
                   (let [diff "@@ -1,3 +1,3 @@\n ```diff\n+ x\n ```"
                         card (render [{"path" "resources/vis-docs/configuration.md"
                                        "op" "update" "changed" true "diff" diff}])]
                     (expect (clojure.string/includes? (:body card) "````diff\n"))
                     (expect (clojure.string/ends-with? (clojure.string/trim (:body card)) "````"))))))

(defdescribe
  render-cat-result-spans-test
  ;; Regression (session 128cefd8): two adjacent ranged reads of the SAME file
  ;; rendered near-identical cards — `app.css · 60 lines` then `app.css ·
  ;; 266 lines` — because the summary carried no line-span info. The headline
  ;; now says WHICH lines were read.
  (let [render @#'editing/render-cat-result]
    (it "single contiguous range: `L<a>-<b>`, count implied (renders each value's \"text\")"
        ;; Canonical anchor values mirror rg hits: {"text" line}.
        (let
          [card (render {"path" "app.css"
                         "anchors" {"1:aa" {"text" "x"} "2:bb" {"text" "y"} "3:cc" {"text" "z"}}})]
          (expect (= "`app.css` · L1-3" (:summary card)))
          ;; Gutter is sized to the widest line number (1 digit here) — no
          ;; fixed-5 left-pad, so the row is flush `1  x`, not `    1  x`.
          (expect (clojure.string/includes? (:body card) "\n1  x\n"))))
    (it "single line: `L<n>`"
        (expect (= "`app.css` · L42"
                   (:summary (render {"path" "app.css" "anchors" {"42:ff" {"text" "q"}}})))))
    (it "multiple ranges: overall extent + run count + line total"
        (expect (= "`app.css` · L1-371 (2 ranges) · 4 lines"
                   (:summary (render {"path" "app.css"
                                      "anchors" {"1:aa" {"text" "a"}
                                                 "2:bb" {"text" "b"}
                                                 "370:cc" {"text" "c"}
                                                 "371:dd" {"text" "d"}}})))))
    (it "spans derive from SORTED line numbers, not map iteration order"
        (expect (= "`app.css` · L5-7"
                   (:summary (render {"path" "app.css"
                                      "anchors" {"7:cc" {"text" "c"}
                                                 "5:aa" {"text" "a"}
                                                 "6:bb" {"text" "b"}}})))))
    (it "unparseable anchor key degrades to the count-only summary (total, never throws)"
        (expect (= "`app.css` · 1 line"
                   (:summary (render {"path" "app.css" "anchors" {"garbage" {"text" "g"}}})))))
    (it "empty anchors: `0 lines`, no body"
        (let [card (render {"path" "app.css" "anchors" {}})]
          (expect (= "`app.css` · 0 lines" (:summary card)))
          (expect (nil? (:body card)))))))

(defdescribe
  render-cat-result-language-test
  (let [render @#'editing/render-cat-result]
    (it "tags code CAT bodies with the detected tree-sitter language"
        (let [body (:body (render {"path" "src/app.clj" "anchors" {"1:aa" {"text" "(def x 1)"}}}))]
          (expect (clojure.string/starts-with? body "\n```clojure\n"))))
    (it "leaves non-code CAT bodies as plain fences"
        (let [body (:body (render {"path" "notes.txt" "anchors" {"1:aa" {"text" "hello"}}}))]
          (expect (clojure.string/starts-with? body "\n```\n"))))
    (it "widens the fence past any backtick run in the file content so an inner ``` never closes it early"
        (let [body (:body (render {"path" "README.md"
                                   "anchors" {"1:aa" {"text" "```clojure"}
                                              "2:bb" {"text" "(def x 1)"}
                                              "3:cc" {"text" "```"}}}))]
          (expect (clojure.string/starts-with? body "\n````"))
          (expect (clojure.string/ends-with? (clojure.string/trim body) "````"))))))

;; ── e2e: REAL tool invocations against REAL temp files ───────────────────────

(defdescribe
  occurrences-tool-e2e-test
  "The `occurrences` TOOL (not just the structural fn): rg prefilter → per-file
   parse → def-marked result envelope, over real files on disk."
  (let [occ (private-fn "occurrences-tool")]
    (it
      "traces a symbol across real files: marks the def (kind/signature), lists uses"
      (let
        [_ (temp-dir-path "occ")
         f1 (str (temp-root) "/occ/lib.clj")
         f2 (str (temp-root) "/occ/use.clj")]

        (spit (fs/file f1) "(defn widget [x] (inc x))\n")
        (spit (fs/file f2) "(ns u)\n(println (widget 1))\n(println (widget 2))\n")
        (with-redefs [editing/rg-search (constantly {:files [f1 f2]})]
          (let
            [r (occ "widget")
             res (:result r)
             all (mapcat #(get % "occurrences") (get res "files"))
             defs (filter #(get % "is_definition") all)]

            (expect (:success? r))
            (expect (= 3 (get res "count"))) ;; 1 def + 2 uses
            (expect (= 1 (get res "definition_count")))
            (expect (= 1 (count defs)))
            (expect (= "widget" (get (first defs) "name")))
            (expect (= "fn" (get (first defs) "kind")))
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
        (let
          [_ (temp-dir-path "occ2")
           f (str (temp-root) "/occ2/u.clj")]

          (spit (fs/file f) "(ns u)\n(println (widget 1))\n") ;; used, never defined here
          (with-redefs [editing/rg-search (constantly {:files [f]})]
            (let [res (:result (occ "widget"))]
              (expect (= 0 (get res "definition_count")))
              (expect (pos? (get res "count")))))))))

(defdescribe
  index-tool-e2e-test
  "The `index` TOOL over a real file — positional AND the dict form the native
   tool-call path synthesizes (`index({\"path\": …})`)."
  (let [index (private-fn "index-tool")]
    (it "positional and dict forms both return the same skeleton"
        (let
          [_ (temp-dir-path "outl")
           f (str (temp-root) "/outl/m.clj")]

          (spit (fs/file f) "(defn add [a b] (+ a b))\n(defn sub [a b] (- a b))\n")
          (let
            [r1 (index f) ;; index("m.clj")
             r2 (index {"path" f})]

            ;; index({"path": "m.clj"}) — native shape
            (expect (:success? r1))
            (expect (:success? r2))
            (expect (clojure.string/includes? (get-in r1 [:result "skeleton"]) "add"))
            (expect (clojure.string/includes? (get-in r1 [:result "skeleton"]) "sub"))
            (expect (= (get-in r1 [:result "skeleton"]) (get-in r2 [:result "skeleton"]))))))))

(defdescribe
  index-tool-range-test
  "struct_index narrows to a single `range` OR several `ranges` windows; a def is
   kept when its span hits ANY window, and the chosen key is echoed back."
  (let
    [index
     (private-fn "index-tool")

     names
     (fn [r]
       (mapv #(get % "name") (get-in r [:result "definitions"])))]

    (it "range keeps defs in the single window; ranges unions disjoint windows"
        (let
          [_
           (temp-dir-path "idxrange")

           f
           (str (temp-root) "/idxrange/m.clj")]

          (spit (fs/file f) "(defn a [] 1)\n(defn b [] 2)\n(defn c [] 3)\n")
          (let
            [whole
             (index {"path" f})

             one
             (index {"path" f "range" [2 2]})

             multi
             (index {"path" f "ranges" [[1 1] [3 3]]})]

            (expect (= ["a" "b" "c"] (names whole)))
            ;; single range: only the def on line 2
            (expect (= ["b"] (names one)))
            (expect (= [2 2] (get-in one [:result "range"])))
            (expect (nil? (get-in one [:result "ranges"])))
            ;; two windows: a and c, not b
            (expect (= ["a" "c"] (names multi)))
            (expect (= [[1 1] [3 3]] (get-in multi [:result "ranges"])))
            (expect (nil? (get-in multi [:result "range"])))
            ;; line_count always the WHOLE file, never the window
            (expect (= 3 (get-in whole [:result "line_count"])))
            (expect (= 3 (get-in multi [:result "line_count"]))))))
    (it "ranges supersedes range and coerces numeric strings"
        (let
          [_
           (temp-dir-path "idxrange2")

           f
           (str (temp-root) "/idxrange2/m.clj")]

          (spit (fs/file f) "(defn a [] 1)\n(defn b [] 2)\n(defn c [] 3)\n")
          (let [r (index {"path" f "range" [1 3] "ranges" [["2" "2"]]})]
            (expect (= ["b"] (names r)))
            (expect (= [[2 2]] (get-in r [:result "ranges"])))
            (expect (nil? (get-in r [:result "range"]))))))
    (it "the render summary surfaces the narrowing window(s)"
        (let
          [render
           (private-fn "render-index-result")

           base
           {"path" "src/x.clj"
            "language" "clojure"
            "line_count" 3
            "definitions" [{"name" "b"
                            "kind" "function"
                            "visibility" "public"
                            "anchor" "2:aa"
                            "end_anchor" "2:bb"
                            "depth" 0}]}]

          ;; whole-file index: no window suffix
          (expect (not (clojure.string/includes? (:summary (render base)) "window")))
          (expect (not (clojure.string/includes? (:summary (render base)) "lines ")))
          ;; single range → "lines A–B"
          (expect (clojure.string/includes? (:summary (render (assoc base "range" [2 2])))
                                            "lines 2–2"))
          ;; several windows → "N windows"
          (expect (clojure.string/includes? (:summary (render (assoc base "ranges" [[1 1] [3 3]])))
                                            "2 windows"))))))

(defdescribe
  rg-tool-e2e-test
  "The `rg` TOOL over real files: the comma-split + smart-case fixes end-to-end."
  (let
    [find-tool
     (private-fn "find-tool")

     rg
     (fn [& a]
       (let [e (apply find-tool a)]
         (update e :result #(merge % (get % "content")))))]

    (it "a comma query matches EITHER term (the session 71a69809 fix, real files)"
        (let
          [d
           (temp-dir-path "rge")

           f
           (str (temp-root) "/rge/a.clj")]

          (spit (fs/file f) "the model line\nthe cycle line\nunrelated\n")
          (let [r (rg "model, cycle" {"paths" [d]})]
            (expect (:success? r))
            (expect (= 2 (get-in r [:result "hit_count"])))))) ;; both lines, not 0
    (it
      "content value is a UNIFORM `{\"text\" line}` map with AND without context"
      (let
        [d
         (temp-dir-path "rguni")

         f
         (str (temp-root) "/rguni/a.clj")]

        (spit (fs/file f) "L1\nMATCH\nL3\n")
        (let
          [plain
           (get-in (rg "MATCH" {"paths" [d]}) [:result "matches"])

           ctx
           (get-in (rg "MATCH" {"paths" [d] "context" 1}) [:result "matches"])

           plain-v
           (-> plain
               vals
               first
               vals
               first)

           ctx-v
           (-> ctx
               vals
               first
               vals
               first)]

          ;; ONE shape regardless of context: always a map carrying "text".
          (expect (map? plain-v))
          (expect (= "MATCH" (get plain-v "text")))
          ;; context:0 hit is JUST {"text"} — no before/after keys.
          (expect (= #{"text"} (set (keys plain-v))))
          ;; context hit is the SAME map, plus before/after.
          (expect (map? ctx-v))
          (expect (= "MATCH" (get ctx-v "text")))
          (expect (contains? ctx-v "before"))
          (expect (contains? ctx-v "after")))))
    (it "smart-case: a lowercase query matches any case, on disk"
        (let
          [d
           (temp-dir-path "rgc")

           f
           (str (temp-root) "/rgc/a.clj")]

          (spit (fs/file f) "Keymap here\nkeystroke too\nnope\n")
          (let [r (rg "key" {"paths" [d]})]
            (expect (= 2 (get-in r [:result "hit_count"])))))) ;; Keymap + keystroke
    (it
      "a MISSING path CLIMBS to its nearest existing ancestor dir and is REPORTED in missing_paths (never a hard error)"
      (let
        [d
         (temp-dir-path "rgp")

         f
         (str (temp-root) "/rgp/a.clj")

         ghost
         (str (temp-root) "/rgp/nope.edn")]

        (spit (fs/file f) "needle here\n")
        ;; one real dir + one path that does not exist. The ghost climbs to its
        ;; parent (the real dir), so the search still runs — and the ghost is
        ;; REPORTED, not silently absorbed.
        (let
          [r
           (rg "needle" {"paths" [d ghost]})

           missing
           (get-in r [:result "missing_paths"])]

          (expect (:success? r))
          (expect (= 1 (get-in r [:result "hit_count"])))
          (expect (= [ghost] (mapv #(get % "requested") missing)))
          (expect (contains? (first missing) "searched")))))
    (it
      "a BLANK/nil paths entry means \"everything\" — widens like \".\", never throws (`[\".github\" \"\"]` case)"
      (let
        [rsr
         @#'editing/resolve-search-roots

         sweep
         (rsr ["."])]

        ;; a lone blank / nil / whitespace resolves to the full allowed-roots sweep
        (expect (= sweep (rsr [""])))
        (expect (= sweep (rsr [nil])))
        (expect (= sweep (rsr ["   "])))
        ;; a blank mixed with a real path still means everything
        (expect (= sweep (rsr ["src" ""])))))
    (it
      "the DEFAULT sweep PRUNES vis's own ~/.vis home (its drafts/ repo mirrors + cache/ CPython are search noise) yet keeps the primary + sibling roots; the primary is NEVER pruned even when it IS ~/.vis"
      (let
        [rsr
         @#'editing/resolve-search-roots

         home
         (System/getProperty "user.home")

         vis-home
         (str home "/.vis")

         primary
         (str home "/proj")

         other
         (str home "/lib")]

        (with-redefs [workspace/allowed-roots (constantly [primary other vis-home])]
          (let [roots (mapv str (:roots (rsr ["."])))]
            ;; ~/.vis pruned from the default sweep …
            (expect (not (some #(clojure.string/starts-with? % vis-home) roots)))
            ;; … while the primary and sibling roots survive, in order
            (expect (= [primary other] roots))))
        ;; the primary is exempt: cwd == ~/.vis still scans
        (with-redefs [workspace/allowed-roots (constantly [vis-home primary])]
          (expect (= [vis-home primary] (mapv str (:roots (rsr ["."]))))))))
    (it
      "a real DRAFT clone under the drafts store (~/.vis/drafts) is KEPT in the default sweep even though it is under ~/.vis — so an in-draft session (its primary + /fs-add clones) stays searchable, while the raw ~/.vis grant is still pruned"
      (let
        [rsr
         @#'editing/resolve-search-roots

         home
         (System/getProperty "user.home")

         vis-home
         (str home "/.vis")

         draft-primary
         (str vis-home "/drafts/proj/feature-x")

         draft-clone
         (str vis-home "/drafts/proj/feature-x-lib")]

        (binding [workspace/*drafts-home* (java.io.File. (str vis-home "/drafts"))]
          (with-redefs [workspace/allowed-roots (constantly [draft-primary draft-clone vis-home])]
            (let [roots (mapv str (:roots (rsr ["."])))]
              ;; both draft clones kept (under the drafts store) …
              (expect (= [draft-primary draft-clone] roots))
              ;; … and the raw ~/.vis grant is still gone
              (expect (not (some #{vis-home} roots))))))))
    (it
      "an EXISTING file is searched as that ONE file (precise — never widened to its dir); a MISSING path CLIMBS to its nearest existing dir and is REPORTED in missing_paths"
      (let
        [dir
         (str (temp-root) "/rgd-precise")

         _
         (when (fs/exists? dir) (fs/delete-tree dir))

         _
         (fs/create-dirs dir)

         a
         (str dir "/a.clj")

         b
         (str dir "/b.clj")

         needle
         "zqUNIQUEneedle42"]

        (spit (fs/file a) (str needle " here\n"))
        (spit (fs/file b) (str needle " here\n"))
        ;; naming the DIR walks BOTH files under it
        (let [r (rg needle {"paths" [dir]})]
          (expect (:success? r))
          (expect (= 2 (get-in r [:result "file_count"]))))
        ;; naming ONE EXISTING file searches ONLY that file — NOT its sibling in the
        ;; same dir. An existing file is precise; it is NOT widened to its parent.
        (let [r (rg needle {"paths" [a]})]
          (expect (:success? r))
          (expect (= 1 (get-in r [:result "file_count"])))
          (expect (= 1 (get-in r [:result "hit_count"])))
          ;; an existing path is never reported missing
          (expect (nil? (get-in r [:result "missing_paths"]))))
        ;; a path that does NOT exist CLIMBS to its nearest existing ancestor dir
        ;; (here `dir`, holding a.clj + b.clj) so the search still runs — and the
        ;; ghost is REPORTED in missing_paths, never a hard error, never silent
        (let
          [ghost
           (str dir "/gone.clj")

           r
           (rg needle {"paths" [ghost]})]

          (expect (:success? r))
          (expect (= 2 (get-in r [:result "file_count"])))
          (expect (= [ghost] (mapv #(get % "requested") (get-in r [:result "missing_paths"])))))))))

(defdescribe
  struct-patch-tool-e2e-test
  "struct_patch LENIENCY over real files: `delete` a def by name, and `replace_node`
   given a `target` but no `match` falling back to the name-based `replace` the model
   meant (instead of failing with 'replaceNode requires both match and code')."
  (let [sp (private-fn "struct-patch-tool")]
    (it "op delete drops the named def; the sibling survives"
        (let
          [_ (temp-dir-path "spd")
           f (str (temp-root) "/spd/m.clj")]

          (spit (fs/file f) "(defn keep-me [x] (inc x))\n(defn drop-me [y] (dec y))\n")
          (let [r (sp {"path" f "op" "delete" "target" "drop-me"})]
            (expect (:success? r))
            (let [src (slurp (fs/file f))]
              (expect (clojure.string/includes? src "keep-me"))
              (expect (not (clojure.string/includes? src "drop-me")))))))
    (it "append_child by NAME inserts inside that definition, not at end-of-file"
        (let
          [_ (temp-dir-path "spac")
           f (str (temp-root) "/spac/m.clj")
           before (str "(defdescribe clipboard-copy-actions-test\n"
                       "  (it \"copies\" (expect true)))\n\n"
                       "(defdescribe later-test\n" "  (it \"stays later\" (expect true)))\n")]

          (spit (fs/file f) before)
          (let
            [r (sp {"path" f
                    "op" "append_child"
                    "target" "clipboard-copy-actions-test"
                    "kind" "fn"
                    "code" "(it \"reports failure\" (expect true))"})
             src (slurp (fs/file f))]

            (expect (:success? r))
            (expect (clojure.string/includes?
                      src
                      "(it \"copies\" (expect true))\n  (it \"reports failure\" (expect true)))"))
            (expect (< (.indexOf src "reports failure") (.indexOf src "later-test"))))))
    (it "append_child WITH a path locator (`at`) still edits the located node"
        (let
          [_ (temp-dir-path "spac2")
           f (str (temp-root) "/spac2/m.clj")]

          (spit (fs/file f) "(ns t)\n(defn f [] (do 1 2))\n")
          (let [r (sp {"path" f "op" "append_child" "at" [1] "code" "3"})]
            (expect (:success? r))
            (expect (clojure.string/includes? (slurp (fs/file f)) "(do 1 2)3")))))
    (it "replace_node with a target but no match = a name-based replace (not an error)"
        (let
          [_ (temp-dir-path "spr")
           f (str (temp-root) "/spr/m.clj")]

          (spit (fs/file f) "(defn foo [x] (inc x))\n")
          (let
            [r (sp {"path" f "op" "replace_node" "target" "foo" "code" "(defn foo [x] (* 2 x))"})]
            (expect (:success? r))
            (expect (clojure.string/includes? (slurp (fs/file f)) "(* 2 x)")))))))

(defdescribe
  patch-syntax-guard-test
  "patch RE-PARSES the result and REFUSES an edit that turns CLEANLY-parsing code
   into broken code (parity with struct_patch). The guard compares before→after, so
   prose/markup that parses WITH error nodes under its grammar (`.txt` → tree-sitter
   `vimdoc`) is never blocked."
  (let [patch (private-fn "patch-safe")]
    (it "an edit that breaks Clojure syntax is refused — nothing written"
        (let
          [p (write-temp! "guard/ok.clj" "(defn add [a b] (+ a b))\n")
           r (patch [{"path" p
                      "from_anchor" (patch/line-anchor 1 "(defn add [a b] (+ a b))")
                      "replace" "(defn add [a b] (+ a b"}])]

          ;; unbalanced → broken
          (expect (false? (:success? r)))
          (expect (= :syntax-error (:reason (first (:failures r)))))
          ;; The syntax-error failure carries a precomputed :message — the surfaced
          ;; summary must SHOW it, not flatten it to the generic "edit N in P failed."
          ;; (explain-failure used to drop :message because :syntax-error is not one
          ;; of the anchor-resolution `reason`s it case-matches on).
          (expect (= (str "No changes (atomic): edit would break syntax in "
                          p
                          ". Fix the replacement or use struct_patch.")
                     (:message r)))
          (expect (not (string/includes? (:message r) "failed.")))
          ;; The refusal carries the WHOLE-BATCH candidates so a language
          ;; pack's :around op-hook (e.g. the Clojure pack's parinfer rescue)
          ;; can whole-source-repair the broken files and commit the batch —
          ;; fragment repair can't fix contextual imbalance.
          (expect (= [p] (:broken-paths r)))
          (expect (= 1 (count (:candidate-plans r))))
          (expect (string/includes? (:after (first (:candidate-plans r))) "(+ a b"))
          (expect (= "(defn add [a b] (+ a b))\n" (slurp p))))) ;; untouched
    (it "a valid Clojure edit still applies"
        (let
          [p (write-temp! "guard/ok2.clj" "(defn add [a b] (+ a b))\n")
           r (patch [{"path" p
                      "from_anchor" (patch/line-anchor 1 "(defn add [a b] (+ a b))")
                      "replace" "(defn add [a b] (* a b))"}])]

          (expect (true? (:success? r)))
          (expect (= "(defn add [a b] (* a b))\n" (slurp p)))))
    (it "prose (.txt → vimdoc parses WITH error nodes) is NEVER blocked"
        (let
          [p (write-temp! "guard/notes.txt" "hello world\nsome notes\n")
           r (patch [{"path" p
                      "from_anchor" (patch/line-anchor 1 "hello world")
                      "replace" "hello there"}])]

          (expect (true? (:success? r)))
          (expect (= "hello there\nsome notes\n" (slurp p)))))
    (it "a strict config (JSON) is guarded too — breaking its syntax is refused"
        (let
          [p (write-temp! "guard/conf.json" "{\"a\": 1}\n")
           r (patch
               [{"path" p "from_anchor" (patch/line-anchor 1 "{\"a\": 1}") "replace" "{\"a\": 1"}])]

          ;; missing close → broken
          (expect (false? (:success? r)))
          (expect (= :syntax-error (:reason (first (:failures r)))))
          (expect (= "{\"a\": 1}\n" (slurp p)))))))

(defdescribe
  patch-multi-failure-message-test
  "A multi-edit patch that fails reports EVERY failing edit — not just `first:` —
   so the model sees the LATER edit that's the real problem, not only edit 0."
  (let [patch (private-fn "patch-safe")]
    (it "lists all failing edits (edit 0 AND edit 1), not just the first"
        (let
          [p (write-temp! "pmf/a.txt" "alpha\nbeta\ngamma\n")
           r (patch [{"path" p "from_anchor" (patch/line-anchor 1 "WRONGLINE") "replace" "x"}
                     {"path" p "from_anchor" (patch/line-anchor 3 "ALSOWRONG") "replace" "y"}])
           msg (:message r)]

          (expect (false? (:success? r)))
          (expect (= 2 (count (:failures r))))
          (expect (string/includes? msg "2 edits failed"))
          (expect (string/includes? msg "edit 0"))
          (expect (string/includes? msg "edit 1")))) ;; the later edit is visible now
    (it "GROUPS same-cause failures into ONE root-cause headline, not N paragraphs"
        (let
          [p (write-temp! "pmf/b.txt" "alpha\nbeta\ngamma\n")
           r (patch [{"path" p "from_anchor" (patch/line-anchor 1 "WRONGA") "replace" "x"}
                     {"path" p "from_anchor" (patch/line-anchor 3 "WRONGB") "replace" "y"}])
           msg (:message r)]

          ;; Both edits fail for the SAME reason (stale anchors), so the root-cause
          ;; sentence appears ONCE with both edits named in the compact list —
          ;; not two near-identical verbose paragraphs.
          (expect (false? (:success? r)))
          (expect (= 1 (count (re-seq #"no longer match the file" msg))))
          (expect (string/includes? msg "2 ×"))
          (expect (string/includes? msg "edit 0"))
          (expect (string/includes? msg "edit 1"))))))

(defdescribe
  find-files-op-name-test
  "Regression: renaming find→find_files means the result `:op` must stay in lockstep
   with the symbol name — `op-tag` keys the observation/mutation registry by the wire
   name, so a mismatch throws `Unregistered extension op :find`."
  (it "the find_files symbol IS named find_files"
      (expect (= 'find_files (:ext.symbol/symbol editing/find-symbol))))
  (it "find_files carries an observation tag (registry-resolvable)"
      (expect (= :observation (:ext.symbol/tag editing/find-symbol)))))

(defdescribe
  empty-search-paths-default-test
  "find_files scopes are directories; empty scope still means the workspace root."
  (let
    [coerce-find
     (private-fn "coerce-find-spec")

     coerce-rg
     (private-fn "coerce-rg-spec")

     find-paths
     (private-fn "find-arg-paths")]

    (it "find_files defaults empty paths to current directory in validation and path protection"
        (let [spec {"query" "resource-config" "paths" []}]
          (expect (= ["."] (:paths (coerce-find [spec]))))
          (expect (= ["."] (find-paths [spec])))))
    (it "normalizes an existing filename scope to its parent directory everywhere"
        (let
          [dir
           (temp-dir-path "find-dir-scope")

           file
           (str dir "/one.clj")

           expected
           ((private-fn "rel-path") (fs/file dir))

           spec
           {"query" "needle" "paths" [file]}]

          (spit (fs/file file) "needle\n")
          (expect (= [expected] (:paths (coerce-find [spec]))))
          (expect (= [expected] (find-paths [spec])))))
    (it "rejects the removed context-lines option"
        (expect (throws? clojure.lang.ExceptionInfo
                         #(coerce-find [{"query" "needle" "context" 2}]))))
    (it "rg keeps its own empty-path and file-path semantics"
        (let [spec {"query" ["FIND_FILES" "CAT"] "paths" []}]
          (expect (= ["."] (:paths (coerce-rg spec))))))))

(defdescribe
  find-files-directory-scope-test
  (let [find-files (private-fn "find-tool")]
    (it "widens a filename scope to its parent and returns matching lines without context"
        (let
          [dir (temp-dir-path "find-file-parent")
           scoped-file (str dir "/scope.clj")
           sibling-file (str dir "/sibling.clj")
           _ (spit (fs/file scoped-file) "before\nnot-it\nafter\n")
           _ (spit (fs/file sibling-file) "before\nsibling-only-needle\nafter\n")
           result (:result (find-files {"query" "sibling-only-needle" "paths" [scoped-file]}))
           expected-dir ((private-fn "rel-path") (fs/file dir))
           expected-file ((private-fn "rel-path") (fs/file sibling-file))
           hit (first (vals (get-in result ["content" "matches" expected-file])))]

          (expect (= [expected-dir] (get result "searched_paths")))
          (expect (= "sibling-only-needle" (get hit "text")))
          (expect (= #{"text"} (set (keys hit))))
          (expect (not (contains? (get result "content") "context")))))))

(defdescribe
  rg-stringified-list-coercion-test
  "Regression: LLMs frequently pass a real ARRAY quoted into ONE string —
   e.g. include=\"[\\\"**/oauth.clj\\\", \\\"**/prov.clj\\\"]\". That single string
   was fed straight to the Java NIO glob PathMatcher, where a leading `[`
   opens a character class and the `/` inside it throws
   `Explicit 'name separator' in class`. `parse-stringish-vector` must
   recognize a bracketed string literal and parse it back into the real vector
   for every string-list field (query / include / paths) — while leaving a
   PLAIN glob string and an already-real vector untouched."
  (let [coerce-rg (private-fn "coerce-rg-spec")]
    (it "include as a stringified JSON/EDN array parses back to the real vector"
        (expect (= ["**/oauth.clj" "**/prov.clj"]
                   (:include (coerce-rg {"query" ["x"]
                                         "include" "[\"**/oauth.clj\", \"**/prov.clj\"]"})))))
    (it "include as an already-real vector is passed through unchanged"
        (expect (= ["**/a.clj" "**/b.clj"]
                   (:include (coerce-rg {"query" ["x"] "include" ["**/a.clj" "**/b.clj"]})))))
    (it "include as a PLAIN glob string is scalar-wrapped, not glob-parsed"
        (expect (= ["**/*.clj"] (:include (coerce-rg {"query" ["x"] "include" "**/*.clj"})))))
    (it "query as a stringified array becomes the OR needles"
        (expect (= ["a" "b"] (:needles (coerce-rg {"query" "[\"a\", \"b\"]"})))))
    (it "paths as a stringified array parses back to the real vector"
        (expect (= ["src" "test"]
                   (:paths (coerce-rg {"query" ["x"] "paths" "[\"src\", \"test\"]"})))))
    (it "a bracketed string of NON-strings is left alone (falls to scalar path)"
        ;; `\"[1, 2]\"` parses to non-string elements → not a string vector, so the
        ;; scalar-tolerant path wraps the raw string and it stays one glob.
        (expect (= ["[1, 2]"] (:include (coerce-rg {"query" ["x"] "include" "[1, 2]"})))))))

(defdescribe
  find-relevance-filter-test
  "Regression: fff's native matcher returns a full page of loose subsequence
   matches with no score (query \"lmstudio\" alone hit 108/489 unrelated paths).
   find-search must post-filter fff's candidates by per-token relevance so only
   genuine hits survive — while staying typo-tolerant and word-order-insensitive."
  (let
    [relevance
     (private-fn "find-relevance")

     min-score
     (private-fn "find-min-score")

     find-search
     (private-fn "find-search")]

    (it "scores a genuine filename hit far above scattered subsequence noise"
        (let
          [genuine
           (relevance "lmstudio" "a/b/provider_lmstudio.clj")

           noise
           (relevance "lmstudio" "extensions/common/foundation_git/src/merge_ops.clj")]

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
        (let
          [_
           (write-temp! "findrel/provider_lmstudio.clj" ";; genuine\n")

           _
           (write-temp! "findrel/provider_openai.clj" ";; noise\n")

           _
           (write-temp! "findrel/foundation_voice_asr.clj" ";; noise\n")

           _
           (write-temp! "findrel/foundation_git_merge_ops.clj" ";; noise\n")

           dir
           (temp-dir-path "findrel")

           out
           (find-search [{"query" "lmstudio" "paths" [dir]}])

           names
           (set (map #(last (string/split % #"/")) (get out "paths")))]

          ;; the genuine file is found
          (expect (contains? names "provider_lmstudio.clj"))
          ;; every returned item clears the relevance floor (no fff padding)
          (expect (every? #(>= (get % "score") min-score) (get out "items")))
          ;; scattered-subsequence noise is excluded
          (expect (not (contains? names "foundation_git_merge_ops.clj")))
          (expect (not (contains? names "foundation_voice_asr.clj")))))))

(defdescribe
  find-fuzzy-fallback-test
  "find-relevance takes the MIN across query tokens, so a multi-word CONCEPT
   query drops the moment any word is absent — the reason natural-language
   phrases returned nothing. When the strict pass is empty and the query has
   >=2 usable tokens, find-search falls back to per-token search and surfaces
   files by exact-name bullseye then coverage."
  (let [find-search (private-fn "find-search")]
    (it "a conceptual phrase surfaces the exact-name file the strict MIN pass dropped"
        (let
          [_ (write-temp! "findfuzz/render.clj" ";; the visualization renderer\n")
           _ (write-temp! "findfuzz/native_tool_handlers.md" "# native tool docs\n")
           _ (write-temp! "findfuzz/unrelated_widget.clj" ";; nope\n")
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
        (let
          [_ (write-temp! "findprecise/channel_tui_footer.clj" ";; footer\n")
           dir (temp-dir-path "findprecise")
           out (find-search [{"query" "channel tui footer" "paths" [dir]}])]

          (expect (nil? (get out "fuzzy")))
          (expect (some #{"channel_tui_footer.clj"}
                        (map #(last (string/split % #"/")) (get out "paths"))))))
    (it "a genuinely-unmatchable query still returns nothing (fuzzy can't invent hits)"
        (let
          [_ (write-temp! "findnone/alpha.clj" ";; x\n")
           dir (temp-dir-path "findnone")
           out (find-search [{"query" "zzzqqq wwwvvv" "paths" [dir]}])]

          (expect (zero? (get out "item_count")))))))

(defdescribe
  render-find-single-match-test
  "A SINGLE find match can still carry a long path, so the summary stays compact
   and the ranked path rides the body. That body makes `result-card` collapsible,
   letting the TUI/Web hide long single-result FIND_FILES output by default.
   2+ matches (and the 0-match steer) keep their body too."
  (let [render-find-result (private-fn "render-find-result")]
    (it "one match: summary stays compact and the path rides the collapsible body"
        (let
          [{:keys [summary body]}
           (render-find-result
             {"item_count" 1
              "query" "resource config"
              "paths"
              ["resources/META-INF/native-image/com.blockether/spel/resource-config.json"]})]
          (expect (= "1 match for \"resource config\"" summary))
          (expect (string/includes?
                    (str body)
                    "resources/META-INF/native-image/com.blockether/spel/resource-config.json"))))
    (it "two matches: summary stays plural and the ranked paths ride the body"
        (let
          [{:keys [summary body]} (render-find-result {"item_count" 2
                                                       "query" "render"
                                                       "paths" ["a/render.clj" "b/render.clj"]})]
          (expect (string/includes? summary "2 matches"))
          (expect (string/includes? (str body) "a/render.clj"))
          (expect (string/includes? (str body) "b/render.clj"))))
    (it "zero matches: the filename-vs-content steer still rides the body"
        (let
          [{:keys [summary body]}
           (render-find-result
             {"item_count" 0 "query" "zzz" "paths" [] "hint" "No FILENAME matched"})]
          (expect (string/includes? summary "0 matches"))
          (expect (string/includes? (str body) "No FILENAME matched"))))
    (it "an explicit `searched_paths` scope is named on the headline; default `.` is not"
        (let
          [scoped
           (render-find-result
             {"item_count" 1 "query" "render" "paths" ["src/render.clj"] "searched_paths" ["src"]})
           default
           (render-find-result
             {"item_count" 1 "query" "render" "paths" ["render.clj"] "searched_paths" ["."]})]

          (expect (= "1 match for \"render\" · in `src`" (:summary scoped)))
          (expect (= "1 match for \"render\"" (:summary default)))))))

(defdescribe
  structural-tool-gating-test
  "The tree-sitter STRUCTURAL editors are advertised ONLY when the project has
   structurally-supported code; a docs/config repo hides them, and it FAILS OPEN."
  (let
    [active?
     (fn [sym langs]
       (with-redefs
         [environment/snapshot (fn []
                                 {:languages {:languages (mapv (fn [l]
                                                                 {:language l})
                                                               langs)}})]
         (extension/symbol-active? sym nil)))

     struct-syms
     [editing/struct-patch-symbol editing/index-symbol editing/occurrences-symbol
      editing/symbol-rename-symbol editing/sexpr-symbol]]

    (it "a Clojure project advertises every structural editor"
        (doseq [s struct-syms]
          (expect (true? (active? s ["clojure"])))))
    (it "a docs-only (markdown/text) project HIDES them; cat/rg/find_files stay"
        (doseq [s struct-syms]
          (expect (false? (active? s ["markdown" "text"]))))
        (doseq [s [editing/cat-symbol editing/find-symbol]]
          (expect (true? (active? s ["markdown" "text"]))))
        (with-redefs
          [environment/snapshot (fn []
                                  {:languages {:languages [{:language "markdown"}
                                                           {:language "text"}]}})]
          (let [prompt (editing/available-editing-prompt)]
            (doseq
              [name ["struct_index" "struct_patch" "struct_node" "struct_occurrences"
                     "struct_rename"]]
              (expect (not (string/includes? prompt name)))))))
    (it "a mixed repo with ANY supported language keeps them (markdown + json)"
        (expect (true? (active? editing/struct-patch-symbol ["markdown" "json"]))))
    (it "shell reconciles to bash (scan says `shell`, tree-sitter says `bash`)"
        (expect (true? (active? editing/struct-patch-symbol ["shell"]))))
    (it "FAILS OPEN on an empty/unknown scan or a scan error"
        (expect (true? (active? editing/struct-patch-symbol [])))
        (with-redefs
          [environment/snapshot (fn []
                                  (throw (ex-info "boom" {})))]
          (expect (true? (extension/symbol-active? editing/struct-patch-symbol nil)))))))

(defdescribe
  rg-sort-key-efficiency-test
  ;; `(sort-by rel-path)` used to run `rel-path` (canonicalize SYSCALLS) INSIDE
  ;; the comparator — O(n·log n) calls that pinned a full core for minutes on a
  ;; big tree and, with no interrupt checkpoint in the sort, kept burning long
  ;; AFTER cancellation (the 400%-CPU orphaned-gateway regression). The sort key
  ;; is now computed ONCE per walked file with a `check-interrupt!` poll.
  (let
    [grep
     (private-fn "rg-search")

     rel-path-var
     (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "rel-path"))

     corpus!
     (fn [dir n]
       (dotimes [i n]
         (write-temp! (format "%s/f%02d.txt" dir i) (if (zero? i) "alpha\n" "nothing here\n")))
       (temp-dir-path dir))]

    (it "computes the sort key O(n) — once per walked file, not once per comparison"
        (let
          [n
           40

           path
           (corpus! "rgsortcalls" n)

           orig
           @rel-path-var

           calls
           (atom 0)]

          (with-redefs-fn {rel-path-var (fn [f]
                                          (swap! calls inc)
                                          (orig f))}
            #(grep {"query" ["alpha"] "paths" [path]}))
          ;; decorate-sort-undecorate: ≈ n key calls + a few for hit rendering.
          ;; keyfn-in-comparator was ~n·log2 n ≈ 210+ for n=40.
          (expect (<= @calls (* 2 n)))))
    (it
      "the sort-key sweep polls check-interrupt! so a cancelled turn aborts instead of grinding on"
      (let
        [path
         (corpus! "rgsortint" 8)

         orig
         @rel-path-var

         first-call
         (atom true)]

        (try (let
               [thrown (try (with-redefs-fn {rel-path-var
                                             (fn [f]
                                               ;; simulate cancel! landing mid-sweep
                                               (when (compare-and-set! first-call true false)
                                                 (.interrupt (Thread/currentThread)))
                                               (orig f))}
                              #(grep {"query" ["alpha"] "paths" [path]}))
                            nil
                            (catch InterruptedException e e))]
               (expect (some? thrown)))
             (finally
               ;; never leak the interrupt flag into the test runner
               (Thread/interrupted)))))))

(defdescribe
  rg-scan-phase-interrupt-test
  ;; The post-sort SCAN phase reads every candidate file. It had no
  ;; check-interrupt! poll, so a cancelled turn kept scanning to the end
  ;; (same class as the sort-key burn, just usually shorter-lived). Both
  ;; output modes now poll per candidate file.
  (let
    [grep
     (private-fn "rg-search")

     core-var
     (fn [n]
       (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" n)))

     corpus!
     (fn [dir]
       (dotimes [i 4]
         (write-temp! (format "%s/f%d.txt" dir i) "alpha\n"))
       (temp-dir-path dir))

     interrupt-on-first-call
     ;; wrap a scan fn: first call interrupts the CURRENT thread (simulating
     ;; cancel! landing mid-scan), every call delegates to a cheap stub
     (fn [stub]
       (let [first-call (atom true)]
         (fn [& args]
           (when (compare-and-set! first-call true false) (.interrupt (Thread/currentThread)))
           (apply stub args))))]

    (it "files-only scan aborts on interrupt instead of scanning to the end"
        (let [path (corpus! "rgscanintfo")]
          (try (let
                 [thrown (try (with-redefs-fn {(core-var "file-has-any-hit?")
                                               (interrupt-on-first-call (fn [_ _]
                                                                          false))}
                                #(grep {"query" ["alpha"] "paths" [path] "is_files_only" true}))
                              nil
                              (catch InterruptedException e e))]
                 (expect (some? thrown)))
               (finally (Thread/interrupted)))))
    (it "content scan aborts on interrupt instead of scanning to the end"
        (let [path (corpus! "rgscanintc")]
          (try (let
                 [thrown (try (with-redefs-fn {(core-var "search-file-content")
                                               (interrupt-on-first-call (fn [_ _ _ _]
                                                                          []))}
                                #(grep {"query" ["alpha"] "paths" [path]}))
                              nil
                              (catch InterruptedException e e))]
                 (expect (some? thrown)))
               (finally (Thread/interrupted)))))))

(defdescribe
  is-respect-gitignore-override-test
  ;; `.gitignore`d files are hidden from rg/find_files by default, but a caller
  ;; must be able to reach vendored / corporate repos the project ignores by
  ;; passing is_respect_gitignore=false. rg walks the tree manually (skipping the
  ;; fff candidate-narrowing, whose index also honors .gitignore); find_files
  ;; swaps its fff scan for a direct filesystem walk.
  (let
    [grep
     (private-fn "rg-search")

     core-var
     (fn [n]
       (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" n)))

     find-search
     (private-fn "find-search")

     fixture!
     (fn [dir]
       (write-temp! (str dir "/.gitignore") "vendor/\n")
       (write-temp! (str dir "/vendor/corp/secret.txt") "NEEDLE_TOKEN here\n")
       (temp-dir-path dir))]

    (it "rg hides the ignored file by default and reveals it with the override"
        (let [path (fixture! "gitignore-override-rg")]
          (expect (zero? (:total-file-count (grep {"query" ["NEEDLE_TOKEN"] "paths" [path]}))))
          (let [r (grep {"query" ["NEEDLE_TOKEN"] "paths" [path] "is_respect_gitignore" false})]
            (expect (= 1 (:total-file-count r)))
            (expect (some #(string/includes? (:path %) "vendor/corp/secret.txt") (:hits r))))))
    (it
      "is_respect_gitignore=false walks each root's tree ONCE across the strict + fallback token scans"
      ;; "secret token" strict-matches nothing (no single path holds BOTH
      ;; words), so the relaxed fallback fires a per-token scan for "secret"
      ;; AND "token" — 3 find-scan passes total. The shared walk-cache must
      ;; collapse those to a SINGLE find-walk-files traversal, not one walk
      ;; per pass (the up-to-6x perf bug).
      (let
        [path
         (fixture! "gitignore-override-walkonce")

         orig
         (private-fn "find-walk-files")

         calls
         (atom 0)]

        (with-redefs-fn {(core-var "find-walk-files") (fn [& args]
                                                        (swap! calls inc)
                                                        (apply orig args))}
          #(let
             [r
              (find-search [{"query" "secret token" "paths" [path] "is_respect_gitignore" false}])]

             ;; the fallback still surfaces the file via the "secret" token
             (expect (some (fn [p]
                             (string/includes? p "vendor/corp/secret.txt"))
                           (get r "paths")))))
        (expect (= 1 @calls))))
    (it "the default (respect-gitignore) path never triggers a direct filesystem walk"
        ;; With the flag left on, find_files stays on the fff index — the direct
        ;; walk (find-walk-files) is the OPT-OUT branch only and must not run.
        (let
          [path
           (fixture! "gitignore-override-nowalk")

           orig
           (private-fn "find-walk-files")

           calls
           (atom 0)]

          (with-redefs-fn {(core-var "find-walk-files") (fn [& args]
                                                          (swap! calls inc)
                                                          (apply orig args))}
            #(find-search [{"query" "secret" "paths" [path]}]))
          (expect (zero? @calls))))
    (it "is_respect_gitignore=false still skips hidden .git metadata (is_hidden default false)"
        ;; Opting out of .gitignore must not drag VCS internals in: a file buried
        ;; in a dot-prefixed .git dir stays invisible because the is_hidden gate
        ;; (default false) prunes hidden dirs during the walk.
        (let
          [dir
           "gitignore-override-gitdir"

           path
           (fixture! dir)

           _
           (write-temp! (str dir "/vendor/corp/.git/config_secret.txt") "x\n")

           paths-of
           (fn [spec]
             (get (find-search [spec]) "paths"))]

          ;; the tracked (gitignored) file IS reachable with the override ...
          (expect (some #(string/includes? % "vendor/corp/secret.txt")
                        (paths-of {"query" "secret" "paths" [path] "is_respect_gitignore" false})))
          ;; ... but the .git-buried file stays hidden even with the override
          (expect (empty? (paths-of
                            {"query" "config" "paths" [path] "is_respect_gitignore" false})))))
    (it "find_files hides the ignored file by default and reveals it with the override"
        (let
          [path
           (fixture! "gitignore-override-find")

           paths-of
           (fn [spec]
             (get (find-search [spec]) "paths"))]

          (expect (empty? (paths-of {"query" "secret" "paths" [path]})))
          (let [hit (paths-of {"query" "secret" "paths" [path] "is_respect_gitignore" false})]
            (expect (some #(string/includes? % "vendor/corp/secret.txt") hit)))))))

(defdescribe
  tool-ignore-negation-layering-test
  ;; `.gitignore` still hides a path from git AND our tools by default, but a
  ;; `!`-negation in a TOOL-ONLY `.ignore`/`.rgignore` (files git never reads)
  ;; re-includes it for rg/find_files while is_respect_gitignore stays at its
  ;; DEFAULT true. Precedence (LOW→HIGH): .gitignore < .ignore < .rgignore, so a
  ;; higher-precedence rule (incl. a re-ignore) wins. fff's index only knows
  ;; .gitignore, so both tools must bypass it when a tool-only ignore file is
  ;; present or the `!` would never surface.
  (let
    [grep
     (private-fn "rg-search")

     find-search
     (private-fn "find-search")

     rg-files
     (fn [path]
       (:files (grep {"query" ["NEEDLE_TOKEN"] "paths" [path] "is_files_only" true})))

     find-paths
     (fn [path]
       (get (find-search [{"query" "secret" "paths" [path]}]) "paths"))

     has?
     (fn [coll frag]
       (boolean (some #(string/includes? % frag) coll)))]

    (it
      "a `!` in .ignore re-includes a .gitignore'd dir for rg AND find_files (default flag)"
      (let
        [dir
         "tool-ignore-neg-include"

         ;; This fixture is reused across test invocations. Remove the
         ;; prior run's tool-only ignore file before asserting the default
         ;; `.gitignore` behavior.
         _
         (fs/delete-if-exists (fs/file (str (temp-dir-path dir) "/.ignore")))

         _
         (write-temp! (str dir "/.gitignore") "vendor/\n")

         _
         (write-temp! (str dir "/vendor/corp/secret.txt") "NEEDLE_TOKEN here\n")

         _
         (write-temp! (str dir "/tracked.txt") "NEEDLE_TOKEN here\n")

         path
         (temp-dir-path dir)]

        ;; default: no tool-only ignore file yet, so .gitignore hides corp
        (expect (not (has? (rg-files path) "vendor/corp/secret.txt")))
        (expect (not (has? (find-paths path) "vendor/corp/secret.txt")))
        ;; drop a tool-only `.ignore` with a `!` — re-included WITHOUT any flag
        (write-temp! (str dir "/.ignore") "!vendor/\n")
        (expect (has? (rg-files path) "vendor/corp/secret.txt"))
        (expect (has? (find-paths path) "vendor/corp/secret.txt"))
        ;; the tracked, never-ignored file is reachable the whole time
        (expect (has? (rg-files path) "tracked.txt"))))
    (it ".rgignore outranks .ignore — a higher-precedence re-ignore wins"
        (let
          [dir
           "tool-ignore-neg-precedence"

           _
           (write-temp! (str dir "/.gitignore") "vendor/\n")

           _
           (write-temp! (str dir "/vendor/corp/secret.txt") "NEEDLE_TOKEN here\n")

           _
           (write-temp! (str dir "/.ignore") "!vendor/\n")

           _
           (write-temp! (str dir "/.rgignore") "vendor/\n")

           path
           (temp-dir-path dir)]

          (expect (not (has? (rg-files path) "vendor/corp/secret.txt")))
          (expect (not (has? (find-paths path) "vendor/corp/secret.txt")))))))

(defdescribe
  search-overlay-config-test
  ;; Issue #23: a `:search {:include-gitignored-paths [...]}` config overlay
  ;; re-includes chosen gitignored subtrees for rg AND find_files with
  ;; is_respect_gitignore left at its DEFAULT — the walker descends the
  ;; excluded dir (which a `.gitignore` `!` negation can never do: git never
  ;; descends an excluded directory, so a negation on a child is dead code),
  ;; while `:always-exclude` (defaults: `.git/`, `node_modules/`, `target/`, …)
  ;; keeps pruning INSIDE the rescued subtree. An EXPLICIT per-call
  ;; is_respect_gitignore — either value — wins over the overlay.
  (let
    [grep
     (private-fn "rg-search")

     find-search
     (private-fn "find-search")

     rg-files
     (fn [path & [spec-extra]]
       (:files (grep (merge {"query" ["NEEDLE_TOKEN"] "paths" [path] "is_files_only" true}
                            spec-extra))))

     find-paths
     (fn [path]
       (get (find-search [{"query" "secret" "paths" [path]}]) "paths"))

     has?
     (fn [coll frag]
       (boolean (some #(string/includes? % frag) coll)))

     overlay!
     (fn [search-block f]
       (with-redefs
         [config/load-config-raw (fn []
                                   {:search search-block})]
         (f)))

     fixture!
     (fn [dir]
       (write-temp! (str dir "/.gitignore") "repositories/\n")
       (write-temp! (str dir "/repositories/corp/secret.txt") "NEEDLE_TOKEN here\n")
       (write-temp! (str dir "/repositories/corp/node_modules/dep/secret_dep.txt")
                    "NEEDLE_TOKEN here\n")
       (temp-dir-path dir))]

    (it
      "re-includes the configured subtree for rg AND find_files; default :always-exclude still prunes"
      (let [path (fixture! "search-overlay-basic")]
        ;; unconfigured: .gitignore hides everything under repositories/
        (expect (not (has? (rg-files path) "repositories/corp/secret.txt")))
        (expect (not (has? (find-paths path) "repositories/corp/secret.txt")))
        (overlay! {:include-gitignored-paths ["repositories/"]}
                  (fn []
                    ;; rescued for both tools, no per-call flag needed…
                    (expect (has? (rg-files path) "repositories/corp/secret.txt"))
                    (expect (has? (find-paths path) "repositories/corp/secret.txt"))
                    ;; …but node_modules INSIDE the rescue stays pruned (default guard)
                    (expect (not (has? (rg-files path) "secret_dep.txt")))
                    (expect (not (has? (find-paths path) "secret_dep")))))))
    (it "the `repositories/**` glob spelling opens the ancestor dir too"
        (let [path (fixture! "search-overlay-glob")]
          (overlay! {:include-gitignored-paths ["repositories/**"]}
                    (fn []
                      (expect (has? (rg-files path) "repositories/corp/secret.txt"))))))
    (it "an explicit per-call is_respect_gitignore — either value — beats the overlay"
        (let [path (fixture! "search-overlay-explicit")]
          (overlay! {:include-gitignored-paths ["repositories/"]}
                    (fn []
                      ;; explicit true → pure gitignore: the rescued subtree stays hidden
                      (expect (not (has? (rg-files path {"is_respect_gitignore" true})
                                         "repositories/corp/secret.txt")))
                      ;; explicit false → full walk: even node_modules shows
                      (expect (has? (rg-files path {"is_respect_gitignore" false})
                                    "secret_dep.txt"))))))
    (it "an explicit :always-exclude REPLACES the defaults"
        (let [path (fixture! "search-overlay-replace")]
          (overlay! {:include-gitignored-paths ["repositories/"] :always-exclude ["*.md"]}
                    (fn []
                      ;; node_modules resurfaces — the default guard list is gone
                      (expect (has? (rg-files path) "secret_dep.txt"))))))))

(defdescribe
  fff-scan-concurrency-guard
  "The bounded-concurrency permit around FRESH fff index scans (rg /
   find_files / occurrences). Bounds the CPU-heavy scan fan-out without
   serializing it, and never leaks a permit."
  (let
    [guard
     (private-fn "with-fff-scan-permit*")

     semaphore
     (private-fn "fff-scan-semaphore")

     permits
     (private-fn "fff-scan-max-concurrency")]

    (describe
      "with-fff-scan-permit*"
      (it "caps concurrent scans at the permit count yet still overlaps them"
          ;; N > permits threads all pile into the guard at once; each records
          ;; the live in-flight count while inside. The peak must NEVER exceed
          ;; the permit count (bounded), and must REACH it (real overlap — the
          ;; guard isn't accidentally serializing everything down to 1).
          (let
            [n
             (+ permits 6)

             in-flight
             (atom 0)

             peak
             (atom 0)

             start
             (java.util.concurrent.CountDownLatch. 1)

             done
             (java.util.concurrent.CountDownLatch. n)

             workers
             (mapv (fn [_]
                     (future (.await start)
                             (guard (fn []
                                      (let [live (swap! in-flight inc)]
                                        (swap! peak max live)
                                        (Thread/sleep 60)
                                        (swap! in-flight dec)
                                        (.countDown done))))))
                   (range n))]

            (.countDown start)
            (let [finished? (.await done 15 java.util.concurrent.TimeUnit/SECONDS)]
              (run! deref workers)
              (expect finished?)
              ;; bounded above by the permit count
              (expect (<= @peak permits))
              ;; and reaches the cap — overlap is preserved, not serialized
              (expect (= permits @peak))
              ;; every permit handed back — nothing leaked
              (expect (= permits (.availablePermits semaphore))))))
      (it "releases the permit even when the thunk throws"
          (let [before (.availablePermits semaphore)]
            (expect (throws? clojure.lang.ExceptionInfo
                             #(guard (fn []
                                       (throw (ex-info "boom" {}))))))
            (expect (= before (.availablePermits semaphore))))))
    (describe
      "rg-fff-open wiring"
      (it "holds exactly one scan permit while fff builds its index, then releases it"
          ;; Prove the heavy op is actually GUARDED without a real fff scan:
          ;; stub create + wait-for-scan, capture the live permit count at the
          ;; moment the index build runs. One permit must be held during the
          ;; build, and all permits must be back afterward.
          (let
            [seen-during-build
             (atom nil)

             fake-idx
             (reify
               java.io.Closeable
                 (close [_] nil))

             rg-fff-open
             (private-fn "rg-fff-open")]

            (with-redefs
              [fff/create
               (fn [_opts]
                 (reset! seen-during-build (.availablePermits semaphore))
                 fake-idx)

               fff/wait-for-scan
               (fn [_idx _timeout]
                 true)]

              (rg-fff-open (java.io.File. ".")))
            ;; one permit taken while the (stubbed) scan ran
            (expect (= (dec permits) @seen-during-build))
            ;; released once the build returned
            (expect (= permits (.availablePermits semaphore)))))
      (it "releases the permit when fff's scan times out"
          ;; wait-for-scan false → rg-fff-open closes the idx and throws; the
          ;; permit must still come back (finally), or a timeout would slowly
          ;; drain the pool to deadlock.
          (let
            [before
             (.availablePermits semaphore)

             closed?
             (atom false)

             fake-idx
             (reify
               java.io.Closeable
                 (close [_] (reset! closed? true)))

             rg-fff-open
             (private-fn "rg-fff-open")]

            (with-redefs
              [fff/create
               (fn [_opts]
                 fake-idx)

               fff/wait-for-scan
               (fn [_idx _timeout]
                 false)]

              (expect (throws? clojure.lang.ExceptionInfo #(rg-fff-open (java.io.File. ".")))))
            (expect @closed?)
            (expect (= before (.availablePermits semaphore))))))))
