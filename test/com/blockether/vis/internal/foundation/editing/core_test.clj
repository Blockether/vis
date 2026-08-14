(ns com.blockether.vis.internal.foundation.editing.core-test
  "Tests for the editing extension.

   Smoke-checks the loaded extension surface (symbol vector, doc
   strings, prompt fragment) plus behavioral coverage of the
   anchored-text read/search verbs (`cat`, `grep`), the anchored writer
   (`patch`) and the thin babashka.fs wrappers (`copy`, `move`, ...).

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
            [com.blockether.vis.internal.foundation.editing.escapes :as escapes]
            [com.blockether.vis.internal.foundation.editing.hashline :as hashline]
            [com.blockether.vis.internal.foundation.editing.structural :as structural]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe describe expect it throws?]]))

(defn- private-fn
  [name]
  (deref (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" name))))

(defn- anchor-at
  "The anchor `cat`/`patch`/`grep` printed for line `n` in one anchored text
   block. Splitting on the gutter is exactly what the model does, so the tests
   address lines the same way the contract promises."
  [text n]
  (->> (string/split-lines text)
       (keep (fn [line]
               (let [[a _] (string/split line #"│ " 2)]
                 (when (= (str n) (first (string/split (string/trim (str a)) #":")))
                   (string/trim a)))))
       first))

(defn- grep-data-fn
  "grep answers in anchored TEXT now — `grep-tool` IS `render-grep-text` over
   `grep-data`'s map — so every assertion about counts, keys, scopes, hints and
   paging reads that pure core, wrapped in the same `{:result …}` envelope the
   tool used to hand back. The RENDERING has its own describes below."
  []
  (let [f (private-fn "grep-data")]
    (fn [& args]
      {:symbol :grep :success? true :result (apply f args)})))

(defn- ls-rows
  "Rows of the sandbox `ls` helper for one request map. `nil` environment is what
   a block sees when no `:fs/access` hook is registered."
  [args]
  (editing/list-directories nil args))

(defn- fff-index-fn
  "Same, for the canonical pooled-fff namespace the index lifecycle lives in."
  [name]
  (deref (resolve (symbol "com.blockether.vis.internal.fff-index" name))))

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
  "Writes a temp fixture file, mirroring the production invariant that EVERY
   in-process mutation announces itself via `note-fs-write!` so a pooled fff
   index resyncs before the next search (an edited `.gitignore`/`.ignore`
   changes the index's universe, not just one file's bytes)."
  [name content]
  (let [rel (str (temp-root) "/" name)]
    (fs/create-dirs (fs/parent rel))
    (spit (fs/file rel) content)
    ((fff-index-fn "note-fs-write!"))
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
        (let [m (matcher ["key"] false)]
          (expect (m "key"))
          (expect (m "Key"))
          (expect (m "KEY"))
          (expect (m "keymap"))
          (expect (not (m "nope")))))
    (it "smart-case: an uppercase-containing needle is case-sensitive"
        (let [m (matcher ["Key"] false)]
          (expect (m "Key"))
          (expect (m "a Keyword"))
          (expect (not (m "key")))
          (expect (not (m "KEY")))))
    (it "make-line-matcher ORs across needles"
        (let [m (matcher ["alpha" "gamma"] false)]
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
          ;; These make fff's FUZZY PATH search match NOTHING, so that side is
          ;; skipped for such a needle. Content discovery is unaffected: native
          ;; grep runs in `:mode :plain` (literal, smart-case) for every needle.
          (expect (hostile? "*workspace-root*"))
          (expect (hostile? "(defn foo"))
          (expect (hostile? "arr[0]"))
          (expect (hostile? "foo{bar"))
          ;; No quantifier/bracket char → the fuzzy path side runs too.
          (expect (not (hostile? "workspace-root")))
          (expect (not (hostile? "config.json")))))
    (it "a hostile needle still hits through fff's LITERAL native grep (no enumeration)"
        ;; Regression guard for the old fallback: a quantifier/bracket needle used
        ;; to force a full fff enumeration that rg then read file-by-file. fff's
        ;; plain-mode grep is literal, so the candidate set stays tiny AND exact.
        (let
          [_
           (write-temp! "rghostile/a.clj" "(get-in m [:a 0])\n(defn foo [x] x)\n")

           _
           (write-temp! "rghostile/b.clj" "nothing interesting here\n")

           out
           (grep {"query" ["[:a 0]"] "paths" [(temp-dir-path "rghostile")]})]

          (expect (= 1 (count (:hits out))))
          (expect (= "(get-in m [:a 0])" (:text (first (:hits out)))))))
    (it "rg-search finds an ear-muffed *var* (fff pre-filter bypassed, literal match)"
        ;; fff's fuzzy PATH search honors `*workspace-root*` as a regex/glob, so the
        ;; hit must come from the literal native grep + `make-line-matcher`.
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
  grep-is-regex-test
  ;; `is_regex` makes CONTENT matching a REGULAR EXPRESSION instead of a literal
  ;; smart-case substring: every term is a pattern, a list is still OR, the
  ;; comma-splitting and trimming are off, the fuzzy NAME axis is off, and a
  ;; pattern that cannot RUN is refused rather than answered as zero hits.
  (let
    [coerce-rg
     (private-fn "coerce-rg-spec")

     coerce-find
     (private-fn "coerce-find-spec")

     matcher
     (private-fn "make-line-matcher")

     rg
     (private-fn "rg-search")

     grep
     (grep-data-fn)]

    (it "coerce-rg-spec carries :is_regex and stops splitting/trimming the pattern"
        (expect (false? (:is_regex (coerce-rg {"query" ["a"]}))))
        (expect (true? (:is_regex (coerce-rg {"query" ["a"] "is_regex" true}))))
        ;; a LITERAL query is comma-split into OR terms; a PATTERN must not be —
        ;; that cuts `a{1,3}` in half and changes what it matches.
        (expect (= ["a{1,3}"] (:needles (coerce-rg {"query" "a{1,3}" "is_regex" true}))))
        (expect (= ["a{1" "3}"] (:needles (coerce-rg {"query" "a{1,3}"})))))
    (it "a pattern that does not COMPILE is refused, never answered as 0 hits"
        (let
          [err (try (coerce-rg {"query" "foo(" "is_regex" true})
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
          (expect (some? err))
          (expect (string/includes? (ex-message err) "does not compile"))
          ;; the same broken pattern is a perfectly good LITERAL search
          (expect (= ["foo("] (:needles (coerce-rg {"query" "foo("}))))))
    (it "coerce-find-spec takes is_regex as a first-class key, and still names it when misspelled"
        (expect (true? (:is_regex (coerce-find [{"query" "x" "is_regex" true}]))))
        (expect (false? (:is_regex (coerce-find [{"query" "x"}]))))
        (expect (string/includes? (ex-message (try (coerce-find [{"query" "x" "is_rgex" true}])
                                                   nil
                                                   (catch clojure.lang.ExceptionInfo e e)))
                                  "is_regex")))
    (it "make-line-matcher regex mode ORs patterns and keeps the smart-case rule"
        (let [m (matcher ["defn-? +grep" "^ns\\b"] true)]
          (expect (m "(defn grep [x] x)"))
          (expect (m "(defn- grep [x] x)"))
          (expect (m "ns foo"))
          (expect (not (m "(def grepish)"))))
        ;; no uppercase in the pattern → case-INSENSITIVE; an uppercase → case-sensitive
        (expect ((matcher ["key.*map"] true) "KEY to MAP"))
        (expect (not ((matcher ["Key.*map"] true) "key to map"))))
    (it "rg-search runs the pattern over content, where the literal read finds nothing"
        (let
          [_
           (write-temp! "rgregex/a.clj"
                        "(defn grep-tool [] :one)\n(defn- grep-data [] :two)\nnothing\n")

           _
           (write-temp! "rgregex/b.clj" "unrelated line\n")

           d
           (temp-dir-path "rgregex")

           out
           (rg {"query" "defn-? +grep-(tool|data)" "paths" [d] "is_regex" true})]

          (expect (= ["(defn grep-tool [] :one)" "(defn- grep-data [] :two)"]
                     (mapv :text (:hits out))))
          ;; the exact dead end `is_regex` exists to end
          (expect (empty? (:hits (rg {"query" "defn-? +grep-(tool|data)" "paths" [d]}))))))
    (it "a pattern the native scanner cannot run is refused, not silently narrowed"
        ;; Java compiles lookbehind; the native candidate scanner's Rust regex
        ;; does not, and would fall back to a LITERAL scan whose candidate set
        ;; misses every real hit — a false negative dressed up as an answer.
        (let
          [_
           (write-temp! "rgregexlook/a.txt" "foobar\n")

           err
           (try (rg {"query" "(?<=foo)bar" "paths" [(temp-dir-path "rgregexlook")] "is_regex" true})
                nil
                (catch clojure.lang.ExceptionInfo e e))]

          (expect (some? err))
          (expect (string/includes? (ex-message err) "native scanner"))))
    (it "grep answers regex CONTENT hits and turns the fuzzy NAME axis off"
        (let
          [d
           (temp-dir-path "grepregex")

           _
           (write-temp! "grepregex/alpha.txt" "one: alpha\ntwo:  beta\n")

           out
           (:result (grep {"query" "^two: +beta$" "paths" [d] "is_regex" true}))]

          (expect (= 1 (get out "hit_count")))
          (expect (= "two:  beta" (get-in out ["matches" (str d "/alpha.txt") "2" "text"])))
          ;; a PATTERN is not a filename: no fuzzy name matches, and no hint
          ;; telling the caller their dialect is wrong.
          (expect (= [] (get out "paths")))
          (expect (nil? (get out "hint")))))
    (it "a zero-hit regex says the pattern RAN instead of blaming the dialect"
        (let
          [d
           (temp-dir-path "grepregexzero")

           _
           (write-temp! "grepregexzero/only.txt" "nothing here\n")

           out
           (:result (grep {"query" "^ZZABSENT.*ZZ$" "paths" [d] "is_regex" true}))]

          (expect (zero? (long (get out "hit_count"))))
          (expect (string/includes? (get out "hint") "the pattern compiled and ran"))))
    (it "a regex-looking LITERAL query is told which flag would run it"
        ;; The old hint only said regex syntax was not interpreted, which left
        ;; the caller re-running the same pattern with cosmetic edits.
        (let
          [d
           (temp-dir-path "grepregexhint")

           _
           (write-temp! "grepregexhint/only.txt" "nothing here\n")

           out
           (:result (grep {"query" "ZZABSENT.*ZZ" "paths" [d]}))]

          (expect (zero? (long (get out "hit_count"))))
          (expect (string/includes? (get out "hint") "is_regex: True"))))))

(defdescribe
  cwd-safety-test
  ;; THE non-negotiable invariant: every v/* tool that touches the
  ;; filesystem must refuse any path that escapes (workspace/cwd).
  ;; safe-path is the single gate; this suite proves every mutation
  ;; tool actually routes through it.
  (let [escape-paths ["../escape.txt" "../../etc/passwd" "/etc/passwd" "target/../../escape.txt"]]
    (it "struct_patch refuses to write outside cwd"
        (let [struct-patch (private-fn "struct-patch-tool")]
          (doseq [p escape-paths]
            (let
              [err (try (struct-patch "path" p "op" "replace" "target" "f" "code" "x")
                        nil
                        (catch clojure.lang.ExceptionInfo e e))]
              (expect (some? err))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err))))))))
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
    (it "a READ path outside cwd is refused at the same gate"
        ;; Defense in depth: even reads can't leak through path traversal.
        (let [safe-path (private-fn "safe-path")]
          (doseq [p escape-paths]
            (let [err (try (safe-path p) nil (catch clojure.lang.ExceptionInfo e e))]
              (expect (some? err))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err))))))))))

(defdescribe
  editing-extension-loads-test
  (it "bash tool fully removed: no symbol, no helpers"
      (let [symbols (map :ext.symbol/symbol (editing/available-editing-symbols))]
        (expect (not-any? #{'bash} symbols))
        (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                       "bash-tool"))))
        (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                       "bash-symbol"))))
        (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                       "run-bash-safe"))))))
  (it "write tool fully removed: no symbol, no tool, no arg normalizer"
      ;; The whole-file write is Python's job now (`Path.write_text`, `open(p, "w")`),
      ;; which crosses the SAME `:fs/access` gate. `write-safe` survives only as the
      ;; internal primitive `struct_patch` uses to commit a whole-buffer rewrite.
      (let
        [symbols
         (map :ext.symbol/symbol (editing/available-editing-symbols))

         private-var
         (fn [n]
           (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" n)))]

        (expect (not-any? #{'write} symbols))
        (expect (nil? (private-var "write-symbol")))
        (expect (nil? (private-var "write-tool")))
        (expect (nil? (private-var "normalize-write-args")))
        ;; the primitive stays: struct_patch commits through it
        (expect (some? (private-var "write-safe")))))
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
  (it "preview tool retired: no symbol advertises it"
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
      [[op tag] [[:cat :observation] [:z/locators :observation] [:grep :observation]
                 [:patch :mutation] [:create-dirs :mutation] [:delete :mutation] [:move :mutation]]]
      (expect (= tag (extension/op-tag op)))
      (expect (= {:tag tag} (extension/op-presentation op))))
    (let [thrown (try (extension/op-tag :v/extensions) nil (catch clojure.lang.ExceptionInfo e e))]
      (expect (= :extension/unregistered-op (:type (ex-data thrown))))))

(defn- gate-env
  "A plain env carrying a `:mutation-gate` stub that records the call payload into
   `seen!` and returns `ret` (a refusal string or nil)."
  [seen! ret]
  {:extensions (atom [])
   :mutation-gate (fn [payload]
                    (reset! seen! payload)
                    ret)})

(defn- with-fs-gate!
  "Install `hook-fn` as the one `:fs/access` gate for `body`, then tear it down.
   A gate lives in the GLOBAL op-hook registry rather than on the env, and that
   IS the contract: an extension declares a boundary once and every surface — the
   editors here, the Python interpreter's own filesystem — asks that same one."
  [hook-fn body]
  (try (extension/register-op-hook! {:op :fs/access :owner :ext/test-fs-gate :fn hook-fn})
       (body)
       (finally (extension/unregister-op-hooks-for-owner! :ext/test-fs-gate))))

(defn- refusing-gate
  "A gate that records every ctx it is asked about and refuses any path spelling
   `marker`, with `hint` as its sentence."
  [seen! marker hint]
  (fn [_env _op ctx]
    (swap! seen! conj ctx)
    (when (clojure.string/includes? (str (:path ctx)) marker) hint)))

(defdescribe
  fs-access-gate-before-fn-test
  (it
    "struct_index asks the gate with file-read and refuses with the extension's own sentence"
    (let
      [seen!
       (atom [])

       hint
       "Use (br/policy) instead of reading this file directly."]

      (with-fs-gate!
        (refusing-gate seen! "protected/secret.edn" hint)
        (fn []
          (let
            [before
             (:ext.symbol/before-fn (private-fn "index-symbol"))

             failure
             (:result (before {:extensions (atom [])}
                              (constantly :ok)
                              [{"paths" ["target/editing-test/protected/secret.edn"]}]))]

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
            (expect (= "file-read"
                       (-> failure
                           :error
                           :operation)))
            (expect (= ["file-read"] (mapv :operation @seen!)))
            ;; The gate is asked about the ABSOLUTE path the op resolved to, so a
            ;; rule cannot be dodged by spelling the same file differently.
            (expect (clojure.string/starts-with? (:path (first @seen!)) "/")))))))
  (it
    "struct_patch asks with file-write and refuses the WHOLE batch when one path is protected"
    (let
      [seen!
       (atom [])

       hint
       "Use (br/update-policy!) instead of editing policy files."]

      (with-fs-gate! (refusing-gate seen! "protected/policy.txt" hint)
                     (fn []
                       (let
                         [before
                          (:ext.symbol/before-fn (private-fn "struct-patch-symbol"))

                          out
                          (before {:extensions (atom [])}
                                  (constantly :ok)
                                  [{"op" "replace"
                                    "target" "f"
                                    "code" "x"
                                    "edits" [{"path" "target/editing-test/plain.txt"}
                                             {"path"
                                              "target/editing-test/protected/policy.txt"}]}])]

                         (expect (= :ext.foundation.editing/path-protected
                                    (-> out
                                        :result
                                        :error
                                        :type)))
                         (expect (= hint
                                    (-> out
                                        :result
                                        :error
                                        :hint)))
                         (expect (= ["file-write" "file-write"] (mapv :operation @seen!))))))))
  (it
    "a gate hook that THROWS refuses: a boundary that fails open is not a boundary"
    (with-fs-gate!
      (fn [_env _op _ctx]
        (throw (ex-info "guard exploded" {})))
      (fn []
        (let
          [before
           (:ext.symbol/before-fn (private-fn "struct-patch-symbol"))

           out
           (before {:extensions (atom [])}
                   (constantly :ok)
                   [{"path" "target/editing-test/a.clj" "op" "replace" "target" "f" "code" "x"}])]

          (expect (= :ext.foundation.editing/path-protected
                     (-> out
                         :result
                         :error
                         :type)))
          (expect (clojure.string/includes? (-> out
                                                :result
                                                :error
                                                :hint)
                                            "fails closed"))))))
  (it "no gate registered: the op passes through with its args untouched"
      (let
        [before
         (:ext.symbol/before-fn (private-fn "struct-patch-symbol"))

         args
         [{"path" "target/editing-test/a.clj" "op" "replace" "target" "f" "code" "x"}]

         out
         (before {:extensions (atom [])} (constantly :ok) args)]

        (expect (not (contains? out :result)))
        (expect (= args (:args out)))))
  (it "a gate that reads a file does not recurse: the nested ask is skipped"
      (let [depth (atom 0)]
        (with-fs-gate! (fn [env op ctx]
                         (swap! depth inc)
                         ;; What a guard that reads a file in order to decide looks like from
                         ;; in here: the nested operation re-enters the gate.
                         (extension/run-gate-hooks op env ctx))
                       (fn []
                         (let
                           [before (:ext.symbol/before-fn (private-fn "index-symbol"))
                            out (before {:extensions (atom [])}
                                        (constantly :ok)
                                        [{"paths" ["target/editing-test/a.clj"]}])]

                           (expect (not (contains? out :result)))
                           (expect (= 1 @depth)))))))
  (it ":fs/access refuses BEFORE the env's :mutation-gate is consulted"
      (with-fs-gate! (fn [_env _op _ctx]
                       "owner API only")
                     (fn []
                       (let
                         [before
                          (:ext.symbol/before-fn (private-fn "struct-patch-symbol"))

                          out
                          (before {:extensions (atom [])
                                   :mutation-gate (fn [_]
                                                    (throw (ex-info "gate must not run" {})))}
                                  (constantly :ok)
                                  [{"path" "target/editing-test/protected/x.clj"
                                    "op" "replace"
                                    "target" "f"
                                    "code" "x"}])]

                         (expect (= :ext.foundation.editing/path-protected
                                    (-> out
                                        :result
                                        :error
                                        :type)))))))
  (it ":mutation-gate refusal becomes a :plan-required failure carrying its paths"
      (let
        [seen!
         (atom nil)

         before
         (:ext.symbol/before-fn (private-fn "struct-patch-symbol"))

         out
         (before (gate-env seen! "Write a PLAN.md first.")
                 (constantly :ok)
                 [{"path" "target/editing-test/a.clj" "op" "replace" "target" "f" "code" "x"}])]

        (expect (= :ext.foundation.editing/plan-required
                   (-> out
                       :result
                       :error
                       :type)))
        (expect (= "Write a PLAN.md first."
                   (-> out
                       :result
                       :error
                       :hint)))
        (expect (= :struct_patch (:op @seen!)))
        (expect (= ["target/editing-test/a.clj"] (:paths @seen!)))
        (expect (false? (:atomic? @seen!)))))
  (it "a nil :mutation-gate answer passes the op through"
      (let
        [seen!
         (atom nil)

         before
         (:ext.symbol/before-fn (private-fn "struct-patch-symbol"))

         out
         (before (gate-env seen! nil)
                 (constantly :ok)
                 [{"path" "target/editing-test/a.clj" "op" "replace" "target" "f" "code" "x"}])]

        (expect (not (contains? out :result)))
        (expect (some? @seen!)))))

(defdescribe
  vis-ls-test
  ;; `ls` is its OWN surface, never a hidden mode of `cat`: `ls` lists
  ;; directories, `cat` reads files, and each refuses the other's input while
  ;; naming the replacement call. Default hides dotfiles + gitignored paths;
  ;; opts widen the view; depth nests children.
  (it "(ls dir) returns a shallow directory listing envelope"
      (let
        [_
         (write-temp! "lsbasic/a.txt" "x")

         _
         (write-temp! "lsbasic/sub/b.txt" "y")

         dir
         (temp-dir-path "lsbasic")

         _
         (.mkdirs (java.io.File. dir "empty"))

         out
         (first (ls-rows {"paths" [dir]}))]

        (expect (= "dir" (get out "type")))
        (expect (= 1 (get out "depth")))
        ;; directories sort before files, each alphabetical; native mixed
        ;; search retains empty directories.
        (expect (= ["empty" "sub" "a.txt"] (mapv #(get % "name") (get out "entries"))))
        (expect (every? #(contains? % "size") (get out "entries")))
        ;; Preserve the original listing contract: directory size is its
        ;; filesystem metadata, not fff's intentionally-zero aggregate.
        (let [sub (some #(when (= "sub" (get % "name")) %) (get out "entries"))]
          (expect (= (.length (java.io.File. dir "sub")) (get sub "size"))))))
  (it "ls batches `paths` in request order, shared opts and per-entry overrides"
      (let
        [_
         (write-temp! "lsbatch/one/a.txt" "x")

         _
         (write-temp! "lsbatch/two/sub/b.txt" "y")

         one
         (temp-dir-path "lsbatch/one")

         two
         (temp-dir-path "lsbatch/two")

         out
         (ls-rows {"paths" [two {"path" one "depth" 1}] "depth" 2})]

        (expect (= 2 (count out)))
        ;; shared depth 2 nests `two`'s subdirectory ...
        (expect (= ["sub"] (mapv #(get % "name") (get (first out) "entries"))))
        (expect (= ["b.txt"]
                   (mapv #(get % "name") (get (first (get (first out) "entries")) "children"))))
        ;; ... while the per-entry override keeps `one` shallow.
        (expect (= 1 (get (second out) "depth")))
        (expect (= ["a.txt"] (mapv #(get % "name") (get (second out) "entries"))))))
  (it
    "(ls dir) hides dotfiles + gitignored entries by default; opts widen"
    (let
      [_
       (write-temp! "lsopts/.gitignore" "ignored.txt\n")

       _
       (write-temp! "lsopts/a.txt" "x")

       _
       (write-temp! "lsopts/.hidden" "x")

       _
       (write-temp! "lsopts/ignored.txt" "x")

       dir
       (temp-dir-path "lsopts")

       names
       (fn [arg]
         (->> (ls-rows (assoc arg "paths" [dir]))
              first
              (#(get % "entries"))
              (mapv (fn [e]
                      (get e "name")))
              set))]

      (expect (= #{"a.txt"} (names {})))
      (expect (contains? (names {"is_hidden" true}) ".hidden"))
      (expect (contains? (names {"is_hidden" true}) ".gitignore"))
      ;; hidden and gitignore are independent axes; gitignored entries are
      ;; ALWAYS skipped — there is no per-call opt-out any more
      (expect (not (contains? (names {"is_hidden" true}) "ignored.txt")))
      (expect (not (contains? (names {}) "ignored.txt")))))
  (it "(ls dir {\"depth\" 2}) nests a children vector under subdirs"
      (let
        [_
         (write-temp! "lsdepth/sub/b.txt" "y")

         dir
         (temp-dir-path "lsdepth")

         out
         (first (ls-rows {"paths" [dir] "depth" 2}))

         sub
         (some #(when (= "sub" (get % "name")) %) (get out "entries"))]

        (expect (= 2 (get out "depth")))
        (expect (= ["b.txt"] (mapv #(get % "name") (get sub "children"))))))
  (it "ls refuses a FILE and points at python_execution"
      (let
        [_
         (write-temp! "lsrefuse/b.txt" "x")

         file
         (str (temp-dir-path "lsrefuse") "/b.txt")

         err
         (try (ls-rows {"paths" [file]}) nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :ext.foundation.editing/ls-on-file (:type (ex-data err))))
        (expect (string/includes? (ex-message err) "python_execution"))))
  ;; Regression: `ls` on a path that does not exist answered with nothing but
  ;; "no such path", so an address INVENTED from a language namespace
  ;; (`com.blockether.vis.ext.channel-tui.human-input` →
  ;; `src/com/blockether/vis/channel_tui`) bounced with no way back and the next
  ;; call guessed again. The nearest EXISTING directory turns that bounce into a
  ;; recovery and names the wrong move.
  (it "ls names the nearest existing directory for a path that does not exist"
      (let
        [_
         (write-temp! "lsnear/real/keep.txt" "x")

         missing
         (str (temp-dir-path "lsnear") "/real/com/blockether/nope")

         err
         (try (ls-rows {"paths" [missing]}) nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :ext.foundation.editing/ls-missing-path (:type (ex-data err))))
        (expect (string/includes? (ex-message err) "no such path"))
        (expect (string/includes? (ex-message err) "nearest existing directory"))
        (expect (string/includes? (ex-message err) "namespace"))
        (expect (string/ends-with? (:nearest (ex-data err)) "lsnear/real")))))

(defdescribe
  vis-ensure-existing-file-home-homogenized-test
  ;; ensure-existing-file! reports paths through paths/abbreviate-home so a
  ;; workspace under $HOME reads "~/vis/…" instead of a leaked absolute home
  ;; path in both the not-found and is-a-directory messages.
  (it "file-not-found + path-is-dir messages collapse $HOME to ~"
      (let
        [ensure
         (private-fn "ensure-existing-file!")

         safe
         (private-fn "safe-path")

         home
         (System/getProperty "user.home")

         missing
         (str (fs/cwd) "/target/editing-test/homoge-missing.txt")

         dirp
         (temp-dir-path "homoge-dir")

         msg-of
         (fn [p]
           (try (ensure (safe p)) nil (catch clojure.lang.ExceptionInfo e (.getMessage e))))]

        (let [m (msg-of missing)]
          (expect (string/includes? m "File not found: ~/"))
          (expect (not (string/includes? m home))))
        (let [m (msg-of dirp)]
          (expect (string/includes? m "Path is a directory, not a file: ~/"))
          (expect (not (string/includes? m home)))))))



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
        ;; Every hit is a clean {:path :line :text} map, no sentinel.
        (expect (every? #(= #{:path :line :text} (set (keys %))) (:hits out)))
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
       (grep-data-fn)

       rg
       ;; grep returns ONE flat result — content hits already sit at the top
       ;; level next to the ranked name matches, so no unwrapping is needed.
       (fn [& a]
         (apply find-tool a))

       ;; rg-tool groups grep's flat :hits into :matches — an ordered
       ;; {path -> {lineno -> text}} map (LinkedHashMap) on the
       ;; model-facing :result; there is no flat :hits vec anymore.
       rg-env
       (rg spec)

       rg-result
       (:result rg-env)

       grep-hits
       (:hits (grep spec))]

      (expect (= :grep (:symbol rg-env)))
      (expect (instance? java.util.Map (get rg-result "matches")))
      (expect (= (count grep-hits) (get rg-result "hit_count")))
      (expect (= (count (distinct (map :path grep-hits))) (get rg-result "file_count")))
      ;; Rich score/frecency rows stay inside name-search assembly instead of
      ;; duplicating each ranked path in the public model payload.
      (expect (not (contains? rg-result "items")))
      ;; NO `"spec"` echo in the model-facing payload: echoing the input map
      ;; back taught models a phantom "spec" INPUT key (`rg({..., "spec": {}})`).
      (expect (not (contains? rg-result "spec")))))
  (it
    "IGNORES unknown spec keys (forgiving) but still requires a query"
    (let
      [grep
       (private-fn "rg-search")

       find-tool
       (grep-data-fn)

       rg
       (fn [& a]
         (apply find-tool a))]

      ;; The private ENGINE (`rg-search`) still takes ONE spec map — a bare
      ;; positional string is not a map, so it throws :invalid-rg-spec.
      (expect (throws? clojure.lang.ExceptionInfo #(grep "needle")))
      ;; The public grep takes THAT SAME one options map and nothing else — a
      ;; positional query, with or without a trailing options map, is refused.
      (expect (throws? clojure.lang.ExceptionInfo #(rg "needle")))
      (expect (throws? clojure.lang.ExceptionInfo #(rg "needle" {"include" ["*.clj"]})))
      (let
        [_
         (write-temp! "rgposopts/a.clj" "needle here\n")

         env
         (rg {"query" "needle" "paths" [(temp-dir-path "rgposopts")] "include" ["*.clj"]})]

        (expect (= :grep (:symbol env)))
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
  (it ":truncated-by :limit when results exceed the configured limit (default 50)"
      ;; The rg sweep ships 50 elements by default. Use 300 hits to force the cap.
      (let
        [_
         (write-temp! "rgcap/a.txt" (string/join "\n" (map #(str "needle " %) (range 300))))

         grep
         (private-fn "rg-search")

         out
         (grep {"all" ["needle"] "paths" [(temp-dir-path "rgcap")]})]

        (expect (= 50 (count (:hits out))))
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
  anchored-verbs-are-back-test
  ;; The inverse of the guard that used to keep these dead. `struct_patch`
  ;; addresses a NAMED definition in a parsed language; prose, config, a comment,
  ;; a docstring line and every unsupported language have no name and no node, so
  ;; the only address left was the old text quoted back. `cat` mints the address
  ;; and `patch` spends it — neither may quietly disappear again.
  (it "bash helpers stay fully removed from the editing core"
      (doseq
        [v ["run-bash-safe" "bash-tool" "strict-bash-command" "coerce-bash-opts" "bash-warnings"
            "channel-render-bash" "journal-render-bash"]]
        (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" v))))))
  (it "the anchored read/write verbs are in the namespace"
      (doseq [v ["cat-tool" "cat-symbol" "cat-one" "patch-tool" "patch-symbol" "patch-one"]]
        (expect (some? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core"
                                        v))))))
  ;; The multi-edit batch and its serializer-damage coercion layer stay dead: one
  ;; call is one file is one span, and several calls in one block are the batch.
  (it "no batch coercion layer came back with them"
      (doseq [v ["coerce-patch-edits" "patch-analysis" "patch-safe" "read-file-by-anchor"]]
        (expect (nil? (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" v))))))
  (it "the hashline namespace loads and owns the anchor"
      (expect (= :loaded
                 (try (require 'com.blockether.vis.internal.foundation.editing.hashline)
                      :loaded
                      (catch Exception _ :missing))))
      (expect (= "12:000" (hashline/line-anchor 12 "")))
      (expect (re-matches #"\d+:[0-9a-f]{3}" (hashline/line-anchor 7 "(defn f [] 1)")))
      (expect (some? (resolve (symbol "com.blockether.vis.internal.foundation.editing.escapes"
                                      "decode-unicode-escapes")))))
  (it "both verbs are advertised beside the structural ones"
      (let [names (set (map #(str (:ext.symbol/symbol %)) (editing/available-editing-symbols)))]
        (expect (contains? names "cat"))
        (expect (contains? names "patch"))
        (expect (contains? names "struct_patch"))
        (expect (contains? names "grep")))))


(defdescribe
  cat-returns-anchored-string-test
  (it "every line is addressable, blanks included"
      (let
        [rel
         (write-temp! "cat/anchored.clj" "(ns a)\n\n(defn one [] 1)\n(defn two [] 2)\n")

         cat-tool
         (comp :result (private-fn "cat-tool"))

         out
         (cat-tool rel)

         lines
         (string/split-lines out)]

        ;; A plain String, never a map: `print(cat(...))` IS the whole surface.
        (expect (string? out))
        (expect (= 4 (count lines)))
        (expect (every? #(re-matches #"\d+:[0-9a-f]{3}│ .*" %) lines))
        ;; The blank line carries an anchor too, so the read is gap-free.
        (expect (string/starts-with? (nth lines 1) "2:000│"))))
  ;; Regression: `cat` answered its anchored text as a BARE STRING, so a real
  ;; sandbox call died at the extension boundary with "Symbol 'cat' must
  ;; return a canonical :envelope map" while every direct call here passed.
  (it "the tool answers the canonical envelope with the text as :result"
      (let
        [rel
         (write-temp! "cat/envelope.txt" "alpha\nbeta\n")

         env
         ((private-fn "cat-tool") rel)]

        (expect (extension/tool-result? env))
        (expect (true? (:success? env)))
        (expect (string? (:result env)))
        (expect (string/starts-with? (:result env) "1:"))))
  (it "an anchor endpoint and a line number select the same window"
      (let
        [rel
         (write-temp! "cat/endpoints.txt" "alpha\nbeta\ngamma\ndelta\n")

         cat-tool
         (comp :result (private-fn "cat-tool"))

         by-number
         (cat-tool rel 2 3)

         anchor-from
         (first (string/split (first (string/split-lines by-number)) #"│"))

         by-anchor
         (cat-tool rel anchor-from 3)]

        (expect (= by-number by-anchor))
        (expect (= 2 (count (string/split-lines by-number))))))
  ;; Regression (session fbb1093f): only a BARE `<line>:<hash>` counted as an
  ;; anchor, so the line `cat` had just printed was not an endpoint `cat` took
  ;; back — the gutter it prints made its own output unusable as an address.
  (it "a whole printed line is an endpoint — the gutter travels with the address"
      (let
        [rel
         (write-temp! "cat/whole-line.txt" "alpha\nBETA\ngamma\ndelta\n")

         cat-tool
         (comp :result (private-fn "cat-tool"))

         printed
         (first (string/split-lines (cat-tool rel 2 3)))]

        (expect (string/includes? printed "│ BETA"))
        (expect (= (cat-tool rel 2 3) (cat-tool rel printed 3)))))
  (it "a window is capped and the clip names the call that continues it"
      (let
        [rel
         (write-temp! "cat/big.txt" (string/join "\n" (map #(str "line " %) (range 1 3001))))

         cat-tool
         (comp :result (private-fn "cat-tool"))

         out
         (cat-tool rel)

         lines
         (string/split-lines out)]

        (expect (= 2001 (count lines)))
        (expect (string/starts-with? (last lines) "… clipped at 2000 lines"))
        (expect (string/includes? (last lines) "continue with cat("))
        (expect (string/includes? (last lines) ", 2001, "))))
  (it "an unreadable path and an inverted window both refuse"
      (let [cat-tool (comp :result (private-fn "cat-tool"))]
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool "does/not/exist.txt")))
        (expect (throws? clojure.lang.ExceptionInfo
                         #(cat-tool (write-temp! "cat/inv.txt" "a\nb\nc\n") 3 1)))))
  ;; Regression: an `end` past the last line REFUSED, so `cat(path, 2172, 2212)`
  ;; on a 2210-line file threw the whole block away — every line it had already
  ;; printed included — instead of handing back the tail it asked for.
  (it "an `end` past the last line CLAMPS to it; a `start` past it still refuses"
      (let
        [cat-tool
         (comp :result (private-fn "cat-tool"))

         rel
         (write-temp! "cat/clamp.txt" "a\nb\nc\n")

         out
         (cat-tool rel 2 99)]

        (expect (= 2 (count (string/split-lines out))))
        (expect (string/includes? out "│ b"))
        (expect (string/includes? out "│ c"))
        (expect (= out (cat-tool rel 2 3)))
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool rel 9 99)))))
  ;; Regression: a negative endpoint was rejected as "outside this file's
  ;; 1..N lines", so the tail of a file could only be read by first
  ;; counting its lines — two calls where one is the natural one.
  (it "a NEGATIVE endpoint counts from the end, and clamps past the top"
      (let
        [cat-tool
         (comp :result (private-fn "cat-tool"))

         rel
         (write-temp! "cat/neg.txt" "a\nb\nc\nd\ne\nf\n")

         tail
         (cat-tool rel -2)]

        ;; -1 IS the last line, so -2 is the last TWO lines.
        (expect (= 2 (count (string/split-lines tail))))
        (expect (string/includes? tail "│ e"))
        (expect (string/includes? tail "│ f"))
        (expect (= tail (cat-tool rel 5 6)))
        ;; A negative window: -4 .. -2 is the closed range 3..5.
        (expect (= (cat-tool rel 3 5) (cat-tool rel -4 -2)))
        ;; Negative and positive endpoints mix freely.
        (expect (= (cat-tool rel 2 6) (cat-tool rel 2 -1)))
        ;; More tail than the file has is the whole file, not a refusal.
        (expect (= (cat-tool rel) (cat-tool rel -500)))
        ;; Zero is still no line at all.
        (expect (throws? clojure.lang.ExceptionInfo #(cat-tool rel 0))))))

(defdescribe
  patch-spends-the-anchor-test
  (it "returns a re-anchored window whose anchors resolve on the very next patch"
      (let
        [rel
         (write-temp! "patch/window.txt" "one\ntwo\nthree\nfour\nfive\nsix\nseven\n")

         cat-tool
         (comp :result (private-fn "cat-tool"))

         patch-one
         (private-fn "patch-one")

         a4
         (anchor-at (cat-tool rel) 4)

         first-out
         (:result (patch-one rel a4 a4 "FOUR"))

         ;; No `cat` between the two edits: the window patch just answered with
         ;; is the address the next edit spends.
         a5
         (anchor-at first-out 5)

         second-out
         (:result (patch-one rel a5 a5 "FIVE"))]

        (expect (string/starts-with? first-out "patched "))
        (expect (string/includes? first-out "4..4 → 1 line"))
        (expect (string/includes? first-out "4:"))
        (expect (string/includes? second-out "5..5 → 1 line"))
        (expect (= "one\ntwo\nthree\nFOUR\nFIVE\nsix\nseven\n" (slurp rel)))))
  (it "a span replace and an empty replacement both report the size they moved"
      (let
        [rel
         (write-temp! "patch/span.txt" "a\nb\nc\nd\ne\n")

         cat-tool
         (comp :result (private-fn "cat-tool"))

         patch-one
         (private-fn "patch-one")

         text
         (cat-tool rel)

         shrunk
         (:result (patch-one rel (anchor-at text 2) (anchor-at text 4) "B"))

         deleted
         (:result (patch-one rel (anchor-at (cat-tool rel) 1) nil ""))]

        (expect (string/includes? shrunk "2..4 → 1 line (-2)"))
        (expect (string/includes? deleted "→ 0 lines (-1)"))
        (expect (= "B\ne\n" (slurp rel)))))
  (it "a stale anchor is refused with the fresh one attached and nothing is written"
      (let
        [rel
         (write-temp! "patch/stale.txt" "alpha\nbeta\ngamma\n")

         cat-tool
         (comp :result (private-fn "cat-tool"))

         patch-one
         (private-fn "patch-one")

         stale
         (anchor-at (cat-tool rel) 2)

         _
         (patch-one rel stale stale "BETA")

         before
         (slurp rel)

         thrown
         (try (patch-one rel stale stale "again") nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (some? thrown))
        (expect (= :anchor-not-found (:reason (ex-data thrown))))
        (expect (string/starts-with? (ex-message thrown) "patch refused — nothing was written."))
        ;; The recovery is IN the refusal: one retry, not a re-read.
        (expect (string/includes? (ex-message thrown) "current anchor at 2 →"))
        (expect (string/includes? (ex-message thrown) (hashline/line-anchor 2 "BETA")))
        (expect (= before (slurp rel)))))
  (it "an anchor whose content moved far away is refused as misplaced"
      (let
        [rel
         (write-temp! "patch/misplaced.txt"
                      (string/join "\n" (concat ["needle"] (map #(str "filler " %) (range 1 200)))))

         patch-one
         (private-fn "patch-one")

         moved
         (str "160:" (hashline/line-hash "needle"))

         thrown
         (try (patch-one rel moved moved "x") nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (some? thrown))
        (expect (= :anchor-misplaced (:reason (ex-data thrown))))
        (expect (string/includes? (ex-message thrown) "drift window"))))
  (it "a bare line number is refused — patch verifies, it does not guess"
      (let
        [rel
         (write-temp! "patch/bare.txt" "alpha\nbeta\n")

         patch-one
         (private-fn "patch-one")

         thrown
         (try (patch-one rel "2" "2" "BETA") nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (some? thrown))
        (expect (= :anchor-malformed (:reason (ex-data thrown))))
        (expect (= "alpha\nbeta\n" (slurp rel))))))


(defdescribe
  patch-parse-gate-test
  (it "a write that would break the parse is refused and nothing lands"
      (let
        [rel
         (write-temp! "patch/gate.clj" "(ns gate)\n\n(defn ok [] 1)\n")

         cat-tool
         (comp :result (private-fn "cat-tool"))

         patch-one
         (private-fn "patch-one")

         a3
         (anchor-at (cat-tool rel) 3)

         thrown
         (try (patch-one rel a3 a3 "(defn ok [] 1") nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (some? thrown))
        (expect (= :parse-broken (:reason (ex-data thrown))))
        (expect (string/includes? (ex-message thrown) "would not parse"))
        (expect (string/includes? (ex-message thrown) "parsed clean before this edit"))
        (expect (= "(ns gate)\n\n(defn ok [] 1)\n" (slurp rel)))))
  (it "an ALREADY broken file still accepts an edit — you must be able to repair it"
      (let
        [rel
         (write-temp! "patch/broken.clj" "(ns broken)\n\n(defn oops [] 1\n")

         cat-tool
         (comp :result (private-fn "cat-tool"))

         patch-one
         (private-fn "patch-one")

         a3
         (anchor-at (cat-tool rel) 3)

         out
         (:result (patch-one rel a3 a3 "(defn oops [] 1)"))]

        (expect (string/starts-with? out "patched "))
        (expect (= "(ns broken)\n\n(defn oops [] 1)\n" (slurp rel)))))
  (it "an unsupported language has no parse gate and no parse clause"
      (let
        ;; An extension tree-sitter has no grammar for: prose and config are
        ;; exactly what `patch` exists to reach, and they must not be gated.
        [rel
         (write-temp! "patch/plain.zzz" "hello\nworld\n")

         cat-tool
         (comp :result (private-fn "cat-tool"))

         patch-one
         (private-fn "patch-one")

         out
         (:result (patch-one rel (anchor-at (cat-tool rel) 2) nil "there"))]

        (expect (not (string/includes? out "parse:")))
        (expect (= "hello\nthere\n" (slurp rel)))))
  ;; Regression: the gate ran for EVERY grammar `detect-language` knows, and it
  ;; knows `.txt` as `vimdoc` — whose grammar reports an ERROR node on ordinary
  ;; prose. So every prose patch ended `parse: still broken at line N`, naming a
  ;; line the file often did not even have. Only a CODE language may gate.
  (it "prose and markdown carry no parse verdict, but code still does"
      (let
        [cat-tool
         (comp :result (private-fn "cat-tool"))

         patch-one
         (private-fn "patch-one")

         edit
         (fn [rel content replacement]
           (let [rel (write-temp! rel content)]
             (:result (patch-one rel (anchor-at (cat-tool rel) 2) nil replacement))))]

        (expect (not (string/includes? (edit "patch/prose.txt" "alpha\nbeta\n" "BETA") "parse:")))
        (expect (not (string/includes? (edit "patch/notes.md" "# One\n\ntwo\n" "TWO") "parse:")))
        (expect (string/includes? (edit "patch/code.py" "def f():\n    return 1\n" "    return 2")
                                  "parse: clean")))))


(defdescribe patch-call-shape-test
             ;; Regression: `patch(path, anchor)` reported success and DELETED the line —
             ;; a missing replacement reached the splice as `(str nil)`, the empty string.
             (it "a patch with no replacement is refused, not treated as a deletion"
                 (let
                   [rel
                    (write-temp! "patch/shape-missing.txt" "alpha\nbeta\n")

                    patch-tool
                    (private-fn "patch-tool")

                    thrown
                    (try (patch-tool rel (hashline/line-anchor 2 "beta"))
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]

                   (expect (some? thrown))
                   (expect (= :replacement-missing (:reason (ex-data thrown))))
                   (expect (string/includes? (ex-message thrown) "patch(path, anchor, \"\")"))
                   (expect (= "alpha\nbeta\n" (slurp rel)))))
             ;; Regression: `patch(path, from, to)` — the model naming a SPAN and forgetting
             ;; the text — wrote the string `6:70a` over line 3 and reported success.
             (it "an anchor passed as the replacement is refused, and names the 4-argument call"
                 (let
                   [rel
                    (write-temp! "patch/shape-anchor.txt" "alpha\nbeta\ngamma\n")

                    patch-tool
                    (private-fn "patch-tool")

                    a1
                    (hashline/line-anchor 1 "alpha")

                    a3
                    (hashline/line-anchor 3 "gamma")

                    thrown
                    (try (patch-tool rel a1 a3) nil (catch clojure.lang.ExceptionInfo e e))]

                   (expect (some? thrown))
                   (expect (= :replacement-is-anchor (:reason (ex-data thrown))))
                   (expect (string/includes? (ex-message thrown) (str a1 ".." a3)))
                   (expect (= "alpha\nbeta\ngamma\n" (slurp rel)))
                   ;; The span it named, and the escape hatch for writing that text literally.
                   (expect (string/starts-with? (:result (patch-tool rel a1 a3 "ONE")) "patched "))
                   (expect (= "ONE\n" (slurp rel)))))
             ;; Regression: a replacement copied straight out of `cat` kept its `line:hash│ `
             ;; gutter and landed in the file verbatim, silently — the gutter is an ADDRESS.
             (it "a replacement carrying the gutter is written, and the status line says so"
                 (let
                   [rel
                    (write-temp! "patch/shape-gutter.txt" "alpha\nbeta\n")

                    patch-tool
                    (private-fn "patch-tool")

                    out
                    (:result (patch-tool rel (hashline/line-anchor 2 "beta") "2:5f0│ BETA"))]

                   (expect
                     (string/includes? out "note: the replacement carries a `line:hash│ ` gutter"))
                   (expect (= "alpha\n2:5f0│ BETA\n" (slurp rel))))))


(defdescribe
  grep-returns-anchored-text-test
  (it "line 1 summarizes and every content row carries an anchor"
      (let
        [d
         (temp-dir-path "greptext")

         _
         (write-temp! "greptext/one.txt" "alpha\nZZNEEDLEZZ here\nomega\n")

         grep-tool
         (private-fn "grep-tool")

         out
         (:result (grep-tool {"query" "ZZNEEDLEZZ" "paths" [d]}))

         lines
         (string/split-lines out)]

        (expect (string? out))
        (expect (string/starts-with? (first lines) "grep 'ZZNEEDLEZZ'"))
        (expect (string/includes? (first lines) "1 hit · 1 file"))
        (expect (some #(re-matches #"  \d+:[0-9a-f]{3}│ .*" %) lines))))
  (it "context lines are anchored too, so a context line is directly patchable"
      (let
        [d
         (temp-dir-path "greptextctx")

         _
         (write-temp! "greptextctx/two.txt" "one\ntwo\nZZCTXZZ\nfour\nfive\n")

         grep-tool
         (private-fn "grep-tool")

         rows
         (->> (:result (grep-tool {"query" "ZZCTXZZ" "paths" [d] "context" 1}))
              string/split-lines
              (filter #(string/starts-with? % "  ")))]

        (expect (= 3 (count rows)))
        (expect (every? #(re-matches #"  \d+:[0-9a-f]{3}│ .*" %) rows))))
  (it "zero hits is the summary plus the hint that explains it"
      (let
        [d
         (temp-dir-path "greptextzero")

         _
         (write-temp! "greptextzero/three.txt" "nothing to see\n")

         grep-tool
         (private-fn "grep-tool")

         out
         (:result (grep-tool {"query" "ZZABSENT.*ZZ" "paths" [d]}))]

        (expect (string/includes? out "0 hits · 0 files"))
        (expect (string/includes? out "hint: "))
        (expect (string/includes? out "is_regex: True"))))
  (it
    "a capped sweep names the exact next call on line 1, breadth included"
    (let
      [render
       (private-fn "render-grep-text")

       head
       (first (string/split-lines (render {"query" "defdescribe"
                                           "matches" {}
                                           "paths" []
                                           "hit_count" 50
                                           "file_count" 11
                                           "total_file_count" 136
                                           "total_file_count_is_exact" true
                                           "hits_truncated_by" "limit"
                                           "next_offset" 50})))]

      (expect
        (=
          "grep 'defdescribe'  50 hits · 11 of 136 files  capped by limit → grep({…, \"offset\": 50})"
          head)))))


(defdescribe a-grep-hit-is-a-patch-anchor-test
             ;; The point of the whole scheme: search, then edit, with NO read between.
             (it "a hit's anchor feeds patch directly"
                 (let
                   [d
                    (temp-dir-path "grepanchor")

                    rel
                    (write-temp! "grepanchor/target.txt" "keep\nZZHITZZ line\nkeep\n")

                    grep-tool
                    (private-fn "grep-tool")

                    patch-one
                    (private-fn "patch-one")

                    hit-row
                    (->> (:result (grep-tool {"query" "ZZHITZZ" "paths" [d]}))
                         string/split-lines
                         ;; Content rows are the indented ones; line 1 is the summary
                         ;; and it echoes the query, so it must not be mistaken for a hit.
                         (filter #(re-matches #"  \d+:[0-9a-f]{3}│ .*" %))
                         first)

                    anchor
                    (string/trim (first (string/split hit-row #"│ ")))

                    out
                    (:result (patch-one rel anchor anchor "replaced"))]

                   (expect (re-matches #"\d+:[0-9a-f]{3}" anchor))
                   (expect (string/starts-with? out "patched "))
                   (expect (= "keep\nreplaced\nkeep\n" (slurp rel)))))
             ;; Regression (session fbb1093f): the anchor parser took EVERYTHING
             ;; after the colon as the hash, so a row pasted WHOLE — the only form
             ;; these tools ever print — hashed to `5af│ /**` and matched no line.
             ;; Every paste was refused, and the refusal echoed the same anchor back.
             (it "a whole hit row — indent, gutter, text and all — feeds patch too"
                 (let
                   [d
                    (temp-dir-path "grepanchorwhole")

                    rel
                    (write-temp! "grepanchorwhole/target.txt" "keep\nZZWHOLEZZ LINE\nkeep\n")

                    grep-tool
                    (private-fn "grep-tool")

                    patch-one
                    (private-fn "patch-one")

                    hit-row
                    (->> (:result (grep-tool {"query" "ZZWHOLEZZ" "paths" [d]}))
                         string/split-lines
                         (filter #(re-matches #"  \d+:[0-9a-f]{3}│ .*" %))
                         first)

                    out
                    (:result (patch-one rel hit-row hit-row "replaced"))]

                   (expect (string/includes? hit-row "│ ZZWHOLEZZ LINE"))
                   (expect (string/starts-with? out "patched "))
                   (expect (= "keep\nreplaced\nkeep\n" (slurp rel))))))


(defdescribe
  struct-index-rows-are-patch-anchors-test
  (it "a definition row's anchor feeds patch with no cat between"
      (let
        [rel
         (write-temp! "structanchor/rows.clj"
                      "(ns rows)\n\n(defn alpha [] 1)\n\n(defn beta [] 2)\n")

         index-tool
         (private-fn "index-tool")

         patch-one
         (private-fn "patch-one")

         row
         (->> (get-in (index-tool {"paths" [rel]}) [:result "results" 0 "definitions"])
              (filter #(= "alpha" (get % "name")))
              first)

         out
         (:result (patch-one rel (get row "anchor") (get row "end_anchor") "(defn alpha [] 42)"))]

        ;; The anchor rides BESIDE the line, never instead of it: struct_nodes
        ;; still consumes the row's `line` as data.
        (expect (= 3 (get row "line")))
        (expect (re-matches #"\d+:[0-9a-f]{3}" (get row "anchor")))
        (expect (string/starts-with? out "patched "))
        (expect (string/includes? (slurp rel) "(defn alpha [] 42)"))))
  (it "a struct_nodes entry carries its anchor too"
      (let
        [rel
         (write-temp! "structanchor/nodes.clj" "(ns nodes)\n\n(defn gamma [] 3)\n")

         nodes-tool
         (private-fn "nodes-tool")

         entry
         (first (get-in (nodes-tool {"path" rel "nodes" [{"line" 3}]}) [:result "results"]))]

        (expect (= 3 (get entry "line")))
        (expect (re-matches #"\d+:[0-9a-f]{3}" (get entry "anchor"))))))


(defdescribe
  patch-diff-text-test
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
        (expect (string/includes? out "+LINE-750"))
        ;; No `--- before` / `+++ after` file header: every renderer already
        ;; shows the path and colours lines by their -/+ prefix.
        (expect (not (string/includes? out "--- before")))
        (expect (not (string/includes? out "+++ after")))))
  (it "patch diff keeps real hunks for a huge file with scattered edits"
      (let
        [diff-fn
         (private-fn "unified-diff-text")

         base
         (mapv #(str "line-" %) (range 8000))

         after
         (-> base
             (assoc 100 "CHANGED-100")
             (assoc 4000 "CHANGED-4000")
             (assoc 7900 "CHANGED-7900"))

         out
         (diff-fn (string/join "\n" base) (string/join "\n" after))

         lines
         (string/split-lines out)]

        ;; A file past the old flat line cap used to render as one
        ;; delete-block plus one add-block spanning line 100..7900 —
        ;; hundreds of `-` lines of untouched code. Now: three real
        ;; hunks, numbered at real file lines.
        (expect (= 3 (count (filter #(string/starts-with? % "@@") lines))))
        (expect (< (count lines) 40))
        (expect (string/includes? out "@@ -3998,7 +3998,7 @@"))
        (expect (string/includes? out "+CHANGED-7900"))
        (expect (not (string/includes? out "unchanged line(s) before")))))
  (it "patch diff bounds a many-hunk edit hunk-wise, never mid-hunk"
      (let
        [diff-fn
         (private-fn "unified-diff-text")

         base
         (mapv #(str "line-" %) (range 8000))

         after
         (reduce (fn [v i]
                   (assoc v (* i 120) (str "CHANGED-" i)))
                 base
                 (range 60))

         lines
         (string/split-lines (diff-fn (string/join "\n" base) (string/join "\n" after)))]

        (expect (<= (count lines) 245))
        ;; Every rendered hunk is whole, and the remainder is a count.
        (expect (< 10 (count (filter #(string/starts-with? % "@@") lines)) 60))
        (expect (string/includes? (last lines) "more hunk(s) omitted"))))
  (it
    "patch diff handles insert, delete, and whole-file rewrites"
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
       (diff-fn before after)

       huge-before
       (string/join "\n" (map #(str "line-" %) (range 8000)))

       huge-after
       (string/join "\n" (map #(str "other-" %) (range 8000)))

       rewritten
       (diff-fn huge-before huge-after)]

      (expect (string/includes? inserted "+X"))
      (expect (not (string/includes? inserted "-a")))
      (expect (string/includes? deleted "-c"))
      ;; A WHOLE-FILE REWRITE (nothing of the old content survives) is ONE
      ;; sided: printing every old line as `-` right above every new line as
      ;; `+` showed the file twice and said nothing — everything changed.
      (expect (string/starts-with? changed "--- (replaced, 300 line(s))"))
      (expect (string/includes? changed "+other-0"))
      (expect (not (string/includes? changed "-line-0")))
      (expect (not (string/includes? changed "@@")))
      (expect (< (count (string/split-lines changed)) 260))
      (expect (string/starts-with? rewritten "--- (replaced, 8000 line(s))"))
      (expect (not (string/includes? rewritten "-line-")))
      (expect (< (count (string/split-lines rewritten)) 260))
      (expect (not (string/includes? rewritten "@@")))
      ;; A partial rewrite still gets real hunks: the shared tail survives.
      (let [partial-out (diff-fn "a\nb\nc\nd" "X\nY\nc\nd")]
        (expect (string/includes? partial-out "-a"))
        (expect (string/includes? partial-out "+X"))))))

(defdescribe tool-envelope-test
             (it "tool wrappers return the required contract keys"
                 (let
                   [path
                    (write-temp! "contract/read.clj" "(defn alpha [] 1)\n")

                    index-tool
                    (private-fn "index-tool")

                    out
                    (index-tool {"paths" [path]})

                    required
                    #{:success? :result :error :symbol :tag :metadata}]

                   ;; Envelope keys MUST include the canonical op/* set; extra keys
                   ;; (e.g. :presentation) may also appear.
                   (expect (= required (clojure.set/intersection required (set (keys out)))))
                   (expect (true? (:success? out)))
                   (expect (= :struct_index (:symbol out)))
                   (let [row (get-in out [:result "results" 0])]
                     (expect (= path (get row "path")))
                     (expect (= ["alpha"] (mapv #(get % "name") (get row "definitions")))))
                   (expect (not (contains? out :markdown)))
                   (expect (nil? (:error out)))))
             (it "tool failure envelope carries structured :error"
                 (let
                   [index-symbol
                    (private-fn "index-symbol")

                    on-error
                    (:ext.symbol/on-error-fn index-symbol)

                    out
                    (:result (on-error (ex-info "boom" {}) nil nil ["missing.txt"]))]

                   (expect (false? (:success? out)))
                   (expect (nil? (:result out)))
                   ;; :trace is a preformatted string; first line carries the
                   ;; underlying class name.
                   (expect (string? (get-in out [:error :trace])))
                   (expect (string/includes? (get-in out [:error :trace]) "ExceptionInfo"))
                   (expect (not (contains? out :markdown))))))

;; Regression: `patch` `replace` and `struct_patch` `code` used to write a
;; literal `\u2014` -- the six characters -- into the file whenever the model
;; emitted the escape instead of the em dash itself, so edited files ended up
;; carrying `\u2014` in prose and docstrings where a dash belonged.
(defdescribe
  editing-unicode-escape-decode-test
  "Drifted `\\uXXXX` escapes are decoded ONCE on the way in, for both text and
   structural edits, and only where the escape can only BE drift: an escaped
   `\\\\uXXXX`, a control escape, a private-use code point and a lone surrogate
   are written through verbatim."
  (it "decodes an unescaped escape that names a printable non-ASCII character"
      (expect (= "a \u2014 b" (escapes/decode-unicode-escapes "a \\u2014 b")))
      (expect (= "\u2026" (escapes/decode-unicode-escapes "\\u2026")))
      (expect (= "caf\u00e9" (escapes/decode-unicode-escapes "caf\\u00e9")))
      ;; A surrogate PAIR is one character, so it decodes together.
      (expect (= "\ud83d\ude42" (escapes/decode-unicode-escapes "\\ud83d\\ude42"))))
  (it "leaves every escape a real file may legitimately contain alone"
      (doseq
        [s ["\\\\u2014" ;; backslash escaped: text ABOUT an escape
            "\\n\\t" ;; control escapes
            "\\u001b[0m" ;; ANSI escape inside a string literal
            "\\u0041" ;; ASCII
            "\\ue0a1" ;; private use (icon font)
            "\\ud83d" ;; lone surrogate
            "\\u2028" ;; line separator: never invent a line
            "\\ufeff" ;; byte-order mark
            "\\uXYZW" ;; not an escape at all
            "\\u20"]] ;; truncated
        (expect (= s (escapes/decode-unicode-escapes s))))
      (expect (= "\\\\u2014 vs \u2014" (escapes/decode-unicode-escapes "\\\\u2014 vs \\u2014"))))
  (it "is total: nothing to decode, nothing to scan"
      (expect (= "plain" (escapes/decode-unicode-escapes "plain")))
      (expect (= "" (escapes/decode-unicode-escapes "")))
      (expect (nil? (escapes/decode-unicode-escapes nil))))
  (it "struct_patch writes the character, not the escape"
      (let
        [sp
         (private-fn "struct-patch-tool")

         _
         (temp-dir-path "spesc")

         f
         (str (temp-root) "/spesc/m.clj")]

        (spit (fs/file f) "(def note \"old\")\n")
        (let
          [r
           (sp {"path" f "op" "replace" "target" "note" "code" "(def note \"a \\u2014 b\")"})

           src
           (slurp (fs/file f))]

          (expect (:success? r))
          (expect (string/includes? src "a \u2014 b"))
          (expect (not (string/includes? src "\\u2014")))))))

;; Regression: the decoder's first cut judged an escape with a hand-written
;; range test -- "printable non-ASCII BMP" -- so it decoded escapes no edit can
;; ever mean. A surrogate PAIR was decoded without once looking at the code
;; point it built, so U+E0020 (an invisible tag character) and plane-15 private
;; use reached disk as real characters; on the BMP an unassigned point, a bidi
;; override, a zero-width joiner, a soft hyphen and a non-breaking space all
;; became silent invisible ink where six visible characters used to sit. And a
;; drifted `match` was never decoded at all, so `struct_patch` could not locate
;; the em dash the model was pointing at.
(defdescribe
  editing-unicode-escape-hostile-test
  "Hostile escapes. An escape is decoded only when it names a VISIBLE assigned
   character -- alone or as a surrogate pair -- and every other escape survives
   as the six characters a human can see and fix."
  (it "never decodes invisible or unassigned BMP ink"
      (doseq
        [s ["\\u202e"                               ;; RIGHT-TO-LEFT OVERRIDE: Trojan Source
            "\\u202d" "\\u2066" "\\u2069"           ;; bidi overrides and isolates
            "\\u200b" "\\u200c" "\\u200d"           ;; zero-width space, non-joiner, joiner
            "\\u200e" "\\u2060" "\\u00ad"           ;; LRM, word joiner, soft hyphen
            "\\u00a0" "\\u2007" "\\u202f" "\\u3000" ;; spaces that do not look like spaces
            "\\u0378" "\\u0e00"                     ;; unassigned
            "\\ufffe" "\\uffff"]] ;; noncharacters
        (expect (= s (escapes/decode-unicode-escapes s)) (pr-str s))))
  (it "never builds an invisible or private character out of a surrogate pair"
      (doseq
        [s ["\\udb40\\udc20" ;; U+E0020 TAG SPACE: invisible
            "\\udb40\\udc01" ;; U+E0001 LANGUAGE TAG: deprecated format
            "\\udb80\\udc00" ;; U+F0000 plane-15 private use
            "\\udbc0\\udc00" ;; U+100000 plane-16 private use
            "\\ud8c0\\udc00" ;; U+40000 unassigned
            "\\udbff\\udffe"]] ;; U+10FFFE noncharacter
        (expect (= s (escapes/decode-unicode-escapes s)) (pr-str s))))
  (it "still decodes a pair that names a real character"
      (expect (= "😀" (escapes/decode-unicode-escapes "\\ud83d\\ude00")))
      (expect (= "🎉" (escapes/decode-unicode-escapes "\\ud83c\\udf89")))
      ;; U+10000: a plain assigned letter outside the BMP.
      (expect (= "𐀀" (escapes/decode-unicode-escapes "\\ud800\\udc00"))))
  (it "leaves ASCII and control escapes alone, where the escape is load-bearing"
      (doseq
        [s ["\\u0022"                                                   ;; a JSON string's own quote
            "\\u005c" "\\u0027" "\\u007b" "\\u0009" "\\u000a" "\\u001b" ;; ESC, as written inside an ANSI sequence
            "\\u009f"]] ;; a C1 control
        (expect (= s (escapes/decode-unicode-escapes s)) (pr-str s))))
  (it "decodes from U+00A1, the first visible non-ASCII character"
      (expect (= "¡" (escapes/decode-unicode-escapes "\\u00a1")))
      (expect (= "\\u00a0" (escapes/decode-unicode-escapes "\\u00a0"))))
  (it "survives adversarial backslash runs"
      (doseq
        [[in out] [["\\u2014" "—"] ["\\\\u2014" "\\\\u2014"] ;; even run: text ABOUT an escape
                   ["\\\\\\u2014" "\\\\—"] ;; odd run: escaped backslash, then drift
                   ["\\\\\\\\u2014" "\\\\\\\\u2014"] ["\\\\\\\\\\u2014" "\\\\\\\\—"]
                   ["\\u2014\\u2014" "——"] ["\\u2014x\\u2026" "—x…"]
                   ["ends in a backslash \\" "ends in a backslash \\"] ["\\\\" "\\\\"]
                   ["\\u20" "\\u20"] ;; truncated
                   ["\\u201" "\\u201"] ["\\uXYZW" "\\uXYZW"] ["\\U2014" "\\U2014"] ;; uppercase U is not an escape
                   ["\\ud83d\\\\ude42" "\\ud83d\\\\ude42"] ;; pair split by a doubled backslash
                   ["\\ud83d\\ud83d" "\\ud83d\\ud83d"] ;; two high halves
                   ["\\udc00\\udc00" "\\udc00\\udc00"]]] ;; two low halves
        (expect (= out (escapes/decode-unicode-escapes in)) (pr-str in))))
  (it "is idempotent, and leaves a string with nothing to decode untouched"
      (doseq [s ["a \\u2014 b" "\\\\u2014" "\\ud83d\\ude00" "\\u202e" "plain" ""]]
        (expect (= (escapes/decode-unicode-escapes s)
                   (escapes/decode-unicode-escapes (escapes/decode-unicode-escapes s)))
                (pr-str s)))
      (let [s "no escape in here at all"]
        (expect (identical? s (escapes/decode-unicode-escapes s))))
      ;; Total: a non-string is returned as it came.
      (expect (= 42 (escapes/decode-unicode-escapes 42))))
  (it "struct_patch decodes a drifted `match` under both locators"
      (let
        [sp
         (private-fn "struct-patch-tool")

         _
         (temp-dir-path "spmatch")

         by-name
         (str (temp-root) "/spmatch/name.clj")

         by-at
         (str (temp-root) "/spmatch/at.clj")]

        (spit (fs/file by-name) "(def note \"a — b\")\n")
        (spit (fs/file by-at) "(def note \"a — b\")\n")
        ;; Name-based: `match` names a whole sub-NODE of the definition, here the
        ;; string carrying the dash.
        (expect (:success? (sp {"path" by-name
                                "op" "replace_node"
                                "target" "note"
                                "match" "\"a \\u2014 b\""
                                "code" "\"a - b\""})))
        (expect (= "(def note \"a - b\")\n" (slurp (fs/file by-name))))
        (expect (:success?
                  (sp {"path" by-at "op" "replace_node" "at" [0] "match" "\\u2014" "code" "-"})))
        (expect (= "(def note \"a - b\")\n" (slurp (fs/file by-at))))))
  (it "struct_patch decodes every edit of a batch, and no invisible ink"
      (let
        [sp
         (private-fn "struct-patch-tool")

         _
         (temp-dir-path "spescbatch")

         f
         (str (temp-root) "/spescbatch/m.clj")]

        (spit (fs/file f) "(def a \"1\")\n(def b \"2\")\n")
        (let
          [r
           (sp {"path" f
                "edits" [{"op" "replace" "target" "a" "code" "(def a \"x \\u2014 y\")"}
                         {"op" "replace" "target" "b" "code" "(def b \"p \\u2026 q\")"}
                         {"op" "append" "code" "(def c \"\\u202e\")"}]})

           src
           (slurp (fs/file f))]

          (expect (:success? r))
          (expect (string/includes? src "x — y"))
          (expect (string/includes? src "p … q"))
          (expect (string/includes? src "(def c \"\\u202e\")")))))
  (it "struct_patch rename decodes the new name"
      (let
        [sp
         (private-fn "struct-patch-tool")

         _
         (temp-dir-path "spescrename")

         f
         (str (temp-root) "/spescrename/m.clj")]

        (spit (fs/file f) "(defn widget [] 1)\n(widget)\n")
        (let [r (sp {"path" f "op" "rename" "target" "widget" "code" "caf\\u00e9"})]
          (expect (:success? r))
          (expect (string/includes? (slurp (fs/file f)) "(defn café []")))))
  (it "a decoded `code` still faces the re-parse gate"
      ;; Decoding happens BEFORE the file is parsed, so drift can never smuggle a
      ;; broken form past the structural guard.
      (let
        [sp
         (private-fn "struct-patch-tool")

         _
         (temp-dir-path "spescparse")

         f
         (str (temp-root) "/spescparse/m.clj")]

        (spit (fs/file f) "(def x 1)\n")
        (expect (throws? clojure.lang.ExceptionInfo
                         #(sp {"path" f "op" "replace" "target" "x" "code" "(def x \\u2014"})))
        (expect (= "(def x 1)\n" (slurp (fs/file f))))))
  (it "is total, idempotent and line-preserving on random escape soup"
      ;; A seeded walk over backslash/hex soup. Whatever arrives, the decoder may
      ;; not throw, may not invent a line under a line-addressed patch, may not
      ;; grow the text, and may not introduce a character that is not both
      ;; visible and assigned.
      (let
        [rng
         (java.util.Random. 20250929)

         alphabet
         (into (vec (seq "\\uu0123456789abcdefxyz \n\t\""))
               ["d83d" "de00" "db40" "dc20" "2014" "202e" "00a0"])

         soup
         (fn []
           (apply str
             (repeatedly (inc (long (.nextInt rng 24)))
                         #(nth alphabet (.nextInt rng (count alphabet))))))

         code-points
         (fn [^String s]
           (set (.toArray (.codePoints s))))

         invisible-or-unreal?
         (fn [cp]
           (or (< (long cp) 0xA0)
               (not (Character/isDefined (int cp)))
               (contains? #{(int Character/UNASSIGNED) (int Character/PRIVATE_USE)
                            (int Character/SURROGATE) (int Character/CONTROL) (int Character/FORMAT)
                            (int Character/LINE_SEPARATOR) (int Character/PARAGRAPH_SEPARATOR)
                            (int Character/SPACE_SEPARATOR)}
                          (Character/getType (int cp)))))]

        (dotimes [_ 2000]
          (let
            [in (soup)
             out (escapes/decode-unicode-escapes in)
             invented (remove (code-points in) (code-points out))]

            (expect (= out (escapes/decode-unicode-escapes out)) (pr-str in))
            (expect (= (count (re-seq #"\n" in)) (count (re-seq #"\n" out))) (pr-str in))
            (expect (<= (count out) (count in)) (pr-str in))
            (expect (not-any? invisible-or-unreal? invented) (pr-str in))))))
  (it "folds its hex quad in ASCII only, so a non-ASCII digit is not a digit"
      ;; The decoder reads the four hex digits itself instead of matching a
      ;; regex, and deliberately not with `Character/digit`: that helper answers
      ;; 4 for U+0664 ARABIC-INDIC FOUR and for U+FF14 FULLWIDTH FOUR alike, so
      ;; a quad of those would have decoded into a character nobody typed.
      (doseq
        [quad [[0x662 0x660 0x661 0x664] ;; ARABIC-INDIC 2 0 1 4
               [0xff12 0xff10 0xff11 0xff14] ;; FULLWIDTH 2 0 1 4
               [0x32 0x30 0x31 0xff14]]] ;; one fullwidth digit is enough
        (let [s (str "\\u" (apply str (map #(char (long %)) quad)))]
          (expect (= s (escapes/decode-unicode-escapes s)) (pr-str s)))))
  (it "decodes megabytes in one linear pass"
      ;; Every `patch` `replace` and every `struct_patch` `code` goes through the
      ;; decoder, so it may never be the slow part of an edit: text between
      ;; backslashes is bulk-copied rather than walked character by character,
      ;; and an escape costs no substring and no matcher. All four blobs together
      ;; decode in about 4 ms (the first, regex-and-charAt cut needed 13 ms), so
      ;; the budget below keeps ~600x of room: it is here to catch a quadratic
      ;; scan or a per-character allocation, not to time a CI box.
      (let
        [drift
         (apply str (repeat 40000 "(str \"a\\u2014b\") ;; drift\n"))

         emoji
         (apply str (repeat 40000 "\\ud83d\\ude00"))

         about
         (apply str (repeat 40000 "\\\\u2014 stays\n"))

         runs
         (apply str (repeat 20 (str (apply str (repeat 50000 "\\")) "u2014")))

         blobs
         [drift emoji about runs]

         t0
         (System/nanoTime)

         outs
         (mapv escapes/decode-unicode-escapes blobs)

         ms
         (/ (- (System/nanoTime) t0) 1e6)]

        (expect (<= 3000000 (reduce + (map count blobs))))
        (expect (string/includes? (nth outs 0) "a\u2014b"))
        (expect (not (string/includes? (nth outs 0) "\\u2014")))
        (expect (= (apply str (repeat 40000 "\ud83d\ude00")) (nth outs 1)))
        (expect (= about (nth outs 2)))
        (expect (= runs (nth outs 3)))
        (expect (< ms 2500)
                (str "decode of " (reduce + (map count blobs)) " characters took " ms " ms")))))





(defn- mk-tmp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defdescribe
  multi-root-safe-path-test
  (it
    "accepts paths under a LIVE filesystem root (trunk==clone), rejects paths outside every root"
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
          (expect (throws? clojure.lang.ExceptionInfo #(safe-path "/etc/hosts")))
          ;; Vis's own config home is always available.
          (expect (some? (safe-path (str (System/getProperty "user.home") "/.vis/config.yml"))))))))
  ;; Regression: `struct_index` fans its per-path work out to the pack's scan pool
  ;; (`structural/scan-mapv`), and a Clojure dynamic binding does NOT cross a raw
  ;; Java worker thread. `safe-path` therefore read `*filesystem-roots*` as empty
  ;; and `*workspace-root*` as nil on every worker, so indexing ONE path (which
  ;; runs inline on the calling thread) accepted a file in a secondary filesystem
  ;; root while indexing TWO threw "escapes the allowed workspace roots" for that
  ;; very same file.
  (it "confines a path the SAME way on a scan-pool worker as on the calling thread"
      (let
        [safe-path
         (private-fn "safe-path")

         ;; Deliberately NOT under the system temp dir, ~/.vis or the cwd: those are
         ;; reachable unconditionally and would hide the missing bindings.
         ctx-root
         "/vis-test-secondary-root"

         files
         [(str ctx-root "/a.clj") (str ctx-root "/b.clj")]

         resolve-all
         (fn [paths]
           (structural/scan-mapv #(.getPath ^java.io.File (safe-path %)) paths))]

        (binding
          [workspace/*workspace-root*
           (.getCanonicalPath (java.io.File. (System/getProperty "user.dir")))

           workspace/*filesystem-roots*
           [{:trunk ctx-root :clone ctx-root}]]

          ;; A single item runs inline on this thread — this always worked.
          (expect (= [(first files)] (resolve-all [(first files)])))
          ;; Two or more fan out to the pool and MUST be confined identically.
          (expect (= files (resolve-all files))))))
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
  native-temp-write-capture-dormant-test
  ;; The native twin of the sandbox outbox tap is retired with it: a file tool
  ;; writing scratch into /tmp is not an artifact anyone asked for, so nothing
  ;; streams to the DB and the companion has nothing to show. Only what a tool
  ;; `attach`es is recorded — see `mpl-capture/incidental-capture-enabled?`.
  (it "a write to /tmp no longer streams to the DB attachment sink"
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

          (expect (:success? (write-safe {"path" tmp "content" "temp scratch, not an artifact"})))
          (expect (:success? (write-safe {"path" ws "content" "not captured either"}))))
        ;; NEITHER write reached the sink: the temp capture is off, and a
        ;; workspace write was never captured in the first place.
        (expect (empty? @sink)))))



(defdescribe editing-native-contract-test
             (let
               [struct-description
                (:ext.symbol/description editing/struct-patch-symbol)

                struct-result
                (:ext.symbol/result editing/struct-patch-symbol)

                index-result
                (:ext.symbol/result editing/index-symbol)]

               ;; The Clojure pack's :around hooks REPAIR unbalanced delimiters instead of
               ;; refusing, so "a syntax break is refused" alone was a lie; the editor says
               ;; what actually happens.
               (it "describes parse refusal AND delimiter auto-repair"
                   (expect (string/includes? struct-description "will not parse is REFUSED"))
                   (expect (string/includes? struct-description "delimiters auto-repaired"))
                   ;; "applies in order, never rolled back" is a property of the BATCH, and
                   ;; `doc(name)` is the only place it can be stated now that no schema
                   ;; describes the `edits` parameter.
                   (expect (string/includes? struct-description "never rolled back")))
               (it "documents the Python result shape instead of relying on rendered output"
                   (expect (string/includes? struct-result "`changed`"))
                   (expect (string/includes? struct-result "`diff`"))
                   (expect (string/includes? index-result "definitions")))))

(defdescribe
  outline-path-resolution-test
  "Regression: index must route through safe-path like every other file tool —
   it used the RAW path (slurp resolves against the JVM user.dir, not the
   workspace cwd), so a nested `src/foo.clj` 404'd on the source runtime while cat
   found it. The proof is that safe-path confinement now applies to index."
  (let [index-tool (private-fn "index-tool")]
    (it "resolves a NESTED workspace-relative path"
        (let
          [dir (temp-dir-path "outline-nested/src")
           _ (spit (fs/file (str dir "/foo.clj")) "(ns foo)\n(defn bar [x] (+ x 1))\n")
           r (index-tool {"paths" [(str (temp-root) "/outline-nested/src/foo.clj")]})
           entry (get-in r [:result "results" 0])]

          (expect (:success? r))
          (expect (clojure.string/includes? (str (get entry "skeleton")) "bar"))
          ;; the STRUCTURED sibling: machine-addressable definitions (no skeleton parsing),
          ;; each row the same shape as an occurrences def — name/kind/line/end_line.
          (let
            [defs (get entry "definitions")
             bar (first (filter #(= "bar" (get % "name")) defs))]

            (expect (vector? defs))
            (expect (= "fn" (get bar "kind")))
            (expect (= "[x]" (get bar "signature")))
            (expect (pos-int? (get bar "line")))
            (expect (pos-int? (get bar "end_line")))
            (expect (= 0 (get bar "depth"))))))
    (it "REFUSES a path that escapes the workspace (proves safe-path confinement)"
        (expect (true? (try (index-tool {"paths" ["/etc/hosts"]})
                            false
                            (catch clojure.lang.ExceptionInfo _ true)))))))

(defdescribe
  line-zipper-tool-test
  "A row's LINE from struct_index is a first-class zipper handle: struct_nodes can
   enter at it, and struct_patch can edit the corresponding node."
  (let
    [sexpr-tool
     (private-fn "nodes-tool")

     struct-patch
     (private-fn "struct-patch-tool")]

    (it "struct_nodes enters the zipper at a definition's line"
        (let
          [path
           (write-temp! "line-zipper/read.clj"
                        "(ns my.app)\n\n(defn foo [x]\n  (+ x 1))\n\n(defn bar [y]\n  (* y 2))\n")

           r
           (sexpr-tool {"path" path "line" 6})

           node
           (first (get-in r [:result "results"]))]

          (expect (:success? r))
          ;; `at` is the zipper cursor, `source` the node's verbatim SOURCE CODE.
          (expect (= [2] (get node "at")))
          (expect (= path (get node "path")))
          (expect (clojure.string/includes? (get node "source") "defn bar"))))
    (it "struct_patch edits the node addressed by a line"
        (let
          [path
           (write-temp! "line-zipper/write.clj"
                        "(ns my.app)\n\n(defn foo [x]\n  (+ x 1))\n\n(defn bar [y]\n  (* y 2))\n")

           r
           (struct-patch {"path" path "op" "replace" "line" 6 "code" "(defn bar [y]\n  (- y 2))"})]

          (expect (:success? r))
          (expect (clojure.string/includes? (slurp (fs/file path)) "(- y 2)"))))
    (it "replace_node reuses node-addressing semantics when a line locates the node"
        (let
          [path
           (write-temp! "line-zipper/replace-node.clj" "(ns my.app)\n\n(defn bar [y]\n  (* y 2))\n")

           r
           (struct-patch {"path" path
                          "op" "replace_node"
                          "line" 3
                          "match" "(defn bar [y]\n  (* y 2))"
                          "code" "(defn bar [y]\n  (+ y 2))"})]

          (expect (:success? r))
          (expect (clojure.string/includes? (slurp (fs/file path)) "(+ y 2)"))))
    ;; Regression, issue #100: `match` meant two different things — the unique
    ;; sub-expression to swap under `target`, but a whole-node equality check under
    ;; the node locator, so the documented use always failed with `match does not
    ;; equal the node selected`.
    (it "replace_node swaps a sub-expression INSIDE the node a path locator selected"
        (let
          [path
           (write-temp! "line-zipper/match-subexpr.clj" "(ns my.app)\n\n(defn f [x]\n  (+ x 1))\n")

           r
           (struct-patch
             {"path" path "op" "replace_node" "line" 3 "match" "(+ x 1)" "code" "(+ x 2)"})]

          (expect (:success? r))
          (expect (= "(ns my.app)\n\n(defn f [x]\n  (+ x 2))\n" (slurp (fs/file path))))))
    (it "refuses an ambiguous `match` under a path locator"
        (let
          [path
           (write-temp! "line-zipper/match-ambiguous.clj"
                        "(ns my.app)\n\n(defn f [x]\n  (+ (inc x) (inc x)))\n")

           error
           (try (struct-patch
                  {"path" path "op" "replace_node" "line" 3 "match" "(inc x)" "code" "(dec x)"})
                nil
                (catch clojure.lang.ExceptionInfo e e))]

          (expect (= 2 (:occurrences (ex-data error))))
          (expect (clojure.string/includes? (ex-message error) "is not unique"))
          (expect (= "(ns my.app)\n\n(defn f [x]\n  (+ (inc x) (inc x)))\n"
                     (slurp (fs/file path))))))
    (it "refuses a `match` that occurs nowhere in the located node"
        (let
          [path
           (write-temp! "line-zipper/match-mismatch.clj"
                        "(ns my.app)\n\n(defn foo [x]\n  (+ x 1))\n")

           error
           (try (struct-patch {"path" path
                               "op" "replace_node"
                               "line" 3
                               "match" "(inc x)"
                               "code" "(defn foo [x]\n  (- x 1))"})
                nil
                (catch clojure.lang.ExceptionInfo e e))]

          (expect (= :ext.foundation.editing/struct-locator-match-mismatch (:type (ex-data error))))
          (expect (clojure.string/includes? (ex-message error) "does not occur in"))
          (expect (= 0 (:occurrences (ex-data error))))
          (expect (= "(ns my.app)\n\n(defn foo [x]\n  (+ x 1))\n" (slurp (fs/file path))))))
    (it "a line wins over a serializer-default empty at path"
        (let
          [path
           (write-temp! "line-zipper/empty-at.clj" "(ns my.app)\n\n(defn bar [y]\n  (* y 2))\n")

           r
           (struct-patch
             {"path" path "op" "insert_before" "line" 3 "at" [] "code" "(defn foo [x]\n  (+ x 1))"})

           src
           (slurp (fs/file path))]

          (expect (:success? r))
          (expect (= "(ns my.app)\n\n(defn foo [x]\n  (+ x 1))\n\n(defn bar [y]\n  (* y 2))\n"
                     src))))
    (it "a line that starts no node is refused before zipper navigation"
        (let
          [path
           (write-temp! "line-zipper/no-node.clj" "(ns my.app)\n\n(defn bar [y]\n  (* y 2))\n")

           r
           (sexpr-tool {"path" path "line" 2})]

          (expect (false? (:success? r)))
          (expect (= :line-no-node (get-in r [:error :reason])))))))

(defdescribe
  patch-summary-line-counts-test
  ;; A write/struct_patch summary states HOW BIG the edit was: the model
  ;; wire strips the `"diff"` and the card caps it hunk-wise, so without the
  ;; counts nothing said whether one line or four hundred moved.
  (let
    [summary
     (private-fn "patch-result-file-summary")

     counts
     (fn [before after]
       (get (summary {:op :update :path "a.txt" :before before :after after}) "lines"))]

    (it "counts added, removed and modified lines from the content"
        (expect (= {"added" 0 "removed" 0 "modified" 1} (counts "a\nb\nc\n" "a\nB\nc\n")))
        (expect (= {"added" 2 "removed" 0 "modified" 0} (counts "a\nb\n" "a\nx\ny\nb\n")))
        (expect (= {"added" 0 "removed" 1 "modified" 0} (counts "a\nb\nc\n" "a\nc\n")))
        ;; A replaced chunk is modified for the overlap, added for the surplus.
        (expect (= {"added" 1 "removed" 0 "modified" 1} (counts "a\nb\nc\n" "a\nX\nY\nc\n"))))
    (it "a new file is all additions and a no-op carries no counts"
        (expect (= {"added" 2 "removed" 0 "modified" 0}
                   (get (summary {:op :add :path "a.txt" :before nil :after "a\nb\n"}) "lines")))
        (expect (not (contains? (summary {:op :update :path "a.txt" :before "a\n" :after "a\n"})
                                "lines"))))
    (it "stays exact for a big file whose rendered diff is capped"
        (let
          [before
           (string/join "\n" (map #(str "line-" %) (range 1500)))

           after
           (-> before
               (string/replace "line-750\n" "LINE-750\n")
               (string/replace "line-900\n" "LINE-900\n"))]

          (expect (= {"added" 0 "removed" 0 "modified" 2} (counts before after)))))))

;; ── e2e: REAL tool invocations against REAL temp files ───────────────────────

(defdescribe
  nodes-tool-plural-cross-file-test
  "`struct_nodes` is the PLURAL navigator: many cursors, many files, ONE call.
   Every entry answers with the node's verbatim `source` PLUS the `at` cursor
   struct_patch takes, and a cursor that misses fails closed per ENTRY while its
   siblings still answer."
  (let [nodes (private-fn "nodes-tool")]
    (it "answers many cursors across MANY files (and languages) in one call"
        (let
          [f1 (write-temp! "nodes-plural/a.clj"
                           "(ns a)\n\n(defn zonk [x]\n  (if (pos? x) (* x 2) 0))\n")
           f2 (write-temp! "nodes-plural/b.py" "def helper(n):\n    return n * 2\n")
           r (nodes {"nodes" [f1 {"path" f2 "nav" [{"find_kind" "function_definition"}]}]})
           [c1 c2] (get-in r [:result "results"])]

          (expect (:success? r))
          (expect (= 2 (count (get-in r [:result "results"]))))
          (expect (= f1 (get c1 "path")))
          (expect (= [] (get c1 "at")))
          (expect (string/includes? (get c1 "source") "(defn zonk"))
          (expect (= f2 (get c2 "path")))
          (expect (= [0] (get c2 "at")))
          (expect (string/includes? (get c2 "source") "def helper"))))
    (it "a shared top-level `path` feeds every entry, and at/nav agree on one node"
        (let
          [p (write-temp! "nodes-shared/a.clj"
                          "(ns a)\n\n(defn zonk [x]\n  (if (pos? x) (* x 2) 0))\n")
           r (nodes {"path" p "nodes" [{"at" [1]} {"nav" [{"find" "zonk"}]}]})
           [by-at by-nav] (get-in r [:result "results"])]

          (expect (:success? r))
          (expect (every? #(= p (get % "path")) [by-at by-nav]))
          (expect (string/includes? (get by-at "source") "(defn zonk"))
          (expect (= "zonk" (get by-nav "source")))))
    (it "ONE impossible move is data on THAT entry; its siblings still answer"
        (let
          [p (write-temp! "nodes-partial/a.clj" "(ns a)\n\n(def k 1)\n")
           r (nodes {"path" p "nodes" [{"at" [1]} {"nav" ["down" "right" "right" "right"]}]})
           [ok miss] (get-in r [:result "results"])]

          (expect (:success? r))
          (expect (= "(def k 1)" (get ok "source")))
          (expect (some? (get miss "error")))
          (expect (= "bad-move" (get miss "reason")))))
    (it "a call whose EVERY cursor missed is a failure, not an empty success"
        (let
          [p (write-temp! "nodes-allmiss/a.clj" "(ns a)\n")
           r (nodes {"path" p "nodes" [{"nav" ["up"]} {"nav" [{"find" "nope-nope"}]}]})]

          (expect (false? (boolean (:success? r))))
          (expect (= :struct_nodes (get-in r [:error :mode])))))
    (it "refuses a malformed `nodes` list instead of guessing"
        (expect (throws? clojure.lang.ExceptionInfo #(nodes {"path" "a.clj" "nodes" []})))
        (expect (throws? clojure.lang.ExceptionInfo #(nodes {"nodes" [42]})))
        (expect (throws? clojure.lang.ExceptionInfo #(nodes {"nodes" [{"nav" ["down"]}]}))))))

(defdescribe
  unified-index-occurrences-e2e-test
  "struct_index traces each declared identifier across the supplied files only when
   `include_occurrences` is true."
  (let [idx (private-fn "index-tool")]
    (it
      "indexes files and groups each definition with its uses when requested"
      (let
        [_ (temp-dir-path "occ")
         f1 (str (temp-root) "/occ/lib.clj")
         f2 (str (temp-root) "/occ/use.clj")]

        (spit (fs/file f1) "(defn widget [x] (inc x))\n")
        (spit (fs/file f2) "(ns u)\n(println (widget 1))\n(println (widget 2))\n")
        (let
          [r (idx {"paths" [f1 f2] "include_occurrences" true})
           res (:result r)
           groups (get res "occurrences")
           widget (first (filter #(= "widget" (get % "name")) groups))
           syms (get widget "symbols")
           s (first syms)
           uses (get s "uses")]

          (expect (:success? r))
          (expect (= [f1 f2] (mapv #(get % "path") (get res "results"))))
          (expect (= ["widget" "u"] (mapv #(get % "name") groups)))
          (expect (= 3 (get widget "count"))) ;; 1 def + 2 uses
          (expect (= 1 (get widget "definition_count")))
          (expect (= 1 (count syms)))
          (expect (= "widget" (get s "name")))
          (expect (= "fn" (get s "kind")))
          (expect (= "[x]" (get s "signature")))
          (expect (= f1 (get s "path")))
          (expect (pos-int? (get s "line")))
          (expect (not (contains? s "is_definition")))
          (expect (= 2 (get s "use_count")))
          (expect (= [f2] (mapv #(get % "path") uses)))
          (expect (= 2 (count (get (first uses) "lines"))))
          (expect (= #{"path" "lines"} (set (keys (first uses)))))
          (expect (every? pos-int? (get (first uses) "lines")))
          (expect (not (contains? widget "other_uses"))))))
    (it "omits occurrences unless explicitly requested"
        (let
          [_ (temp-dir-path "occ-omitted")
           f (str (temp-root) "/occ-omitted/lib.clj")]

          (spit (fs/file f) "(defn widget [x] (inc x))\n")
          (doseq [args [{"paths" [f]} {"paths" [f] "include_occurrences" false}]]
            (let [result (:result (idx args))]
              (expect (contains? result "results"))
              (expect (not (contains? result "occurrences")))))))
    (it "deduplicates occurrence scans while preserving duplicate result rows"
        (let
          [_ (temp-dir-path "occ-duplicates")
           f (str (temp-root) "/occ-duplicates/lib.clj")]

          (spit (fs/file f) "(defn widget [x] (widget x))\n")
          (let
            [result (:result (idx {"paths" [f f] "include_occurrences" true}))
             widget (first (filter #(= "widget" (get % "name")) (get result "occurrences")))]

            (expect (= [f f] (mapv #(get % "path") (get result "results"))))
            (expect (= 2 (get widget "count")))
            (expect (= 1 (get widget "definition_count")))
            (expect (= 1 (get widget "scanned")))
            (expect (= 1 (count (get widget "symbols")))))))
    (it "rejects removed selector shapes and a non-boolean occurrence flag"
        (doseq
          [args [{"name" "widget"} {"path" "widget.clj"} "widget.clj"
                 {"paths" ["widget.clj"] "include_occurrences" "yes"}]]
          (expect (throws? clojure.lang.ExceptionInfo #(idx args)))))))

(defdescribe merged-symbol-entry-points-test
             "Occurrence grouping backs the one paths-only struct_index mode."
             (let [idx (private-fn "index-tool")]
               (it "an ambiguous name groups PER SYMBOL and parks the rest in other_uses"
                   (let
                     [_ (temp-dir-path "idxamb")
                      a (str (temp-root) "/idxamb/a.clj")
                      b (str (temp-root) "/idxamb/b.clj")
                      c (str (temp-root) "/idxamb/c.clj")]

                     (spit (fs/file a) "(defn gizmo [x] x)\n(gizmo 1)\n")
                     (spit (fs/file b) "(defn gizmo [x y] y)\n(gizmo 1 2)\n")
                     ;; uses only — owned by neither definition
                     (spit (fs/file c) "(ns c)\n(gizmo 9)\n(gizmo 8)\n")
                     (with-redefs [editing/rg-search (constantly {:files [a b c]})]
                       (let
                         [r (idx {"paths" [a b c] "include_occurrences" true})
                          res (:result r)
                          gizmo (first (filter #(= "gizmo" (get % "name")) (get res "occurrences")))
                          syms (get gizmo "symbols")]

                         (expect (:success? r))
                         (expect (= 2 (get gizmo "definition_count")))
                         (expect (= 2 (count syms)))
                         ;; each definition keeps its own path + signature to disambiguate
                         (expect (= [a b] (mapv #(get % "path") syms)))
                         (expect (= ["[x]" "[x y]"] (mapv #(get % "signature") syms)))
                         ;; a use in a file that defines the name exactly once is attributed there
                         (expect (= [1 1] (mapv #(get % "use_count") syms)))
                         (expect (= [[a] [b]]
                                    (mapv (fn [s]
                                            (mapv #(get % "path") (get s "uses")))
                                          syms)))
                         ;; the third file defines nothing, so ownership is NOT guessed
                         (expect (= [c] (mapv #(get % "path") (get gizmo "other_uses"))))
                         (expect (= 2 (count (get (first (get gizmo "other_uses")) "lines"))))))))
               (it "struct_index without `paths` is refused"
                   (expect (throws? clojure.lang.ExceptionInfo #(idx {}))))
               (it "struct_occurrences is gone entirely"
                   ;; No `struct_occurrences` symbol is exported: occurrence analysis is an
                   ;; explicit struct_index option (a stale Var can survive a REPL :reload, so
                   ;; assert on the exported symbol list, not `resolve`).
                   (expect (not-any? #(= 'struct_occurrences (:ext.symbol/symbol %))
                                     (editing/available-editing-symbols))))))

(defdescribe
  index-tool-e2e-test
  "The paths-only `struct_index` tool indexes a real file and returns its row in
   the ordered batch result without occurrence analysis by default."
  (let [index (private-fn "index-tool")]
    (it "returns the requested file's structural row without occurrences by default"
        (let
          [_ (temp-dir-path "outl")
           f (str (temp-root) "/outl/m.clj")]

          (spit (fs/file f) "(defn add [a b] (+ a b))\n(defn sub [a b] (- a b))\n")
          (let
            [r (index {"paths" [f]})
             result (:result r)
             entry (get-in result ["results" 0])]

            (expect (:success? r))
            (expect (= f (get entry "path")))
            (expect (clojure.string/includes? (get entry "skeleton") "add"))
            (expect (clojure.string/includes? (get entry "skeleton") "sub"))
            (expect (= ["add" "sub"] (mapv #(get % "name") (get entry "definitions"))))
            (expect (not (contains? result "occurrences"))))))))

(defdescribe
  index-tool-range-test
  "struct_index narrows to a single `range` OR several `ranges` windows; a def is
   kept when its span hits ANY window, and the chosen key is echoed back."
  (let
    [index
     (private-fn "index-tool")

     names
     (fn [r]
       (mapv #(get % "name") (get-in r [:result "results" 0 "definitions"])))]

    (it
      "ranges handles one or several windows and is echoed consistently"
      (let
        [_
         (temp-dir-path "idxrange")

         f
         (str (temp-root) "/idxrange/m.clj")]

        (spit (fs/file f) "(defn a [] 1)\n(defn b [] 2)\n(defn c [] 3)\n")
        (let
          [whole
           (index {"paths" [f]})

           one
           (index {"paths" [{"path" f "ranges" [[2 2]]}]})

           multi
           (index {"paths" [{"path" f "ranges" [[1 1] [3 3]]}]})]

          (expect (= ["a" "b" "c"] (names whole)))
          (expect (= ["b"] (names one)))
          (expect (= [[2 2]] (get-in one [:result "results" 0 "ranges"])))
          (expect (not (contains? (get-in one [:result "results" 0]) "range")))
          (expect (= ["a" "c"] (names multi)))
          (expect (= [[1 1] [3 3]] (get-in multi [:result "results" 0 "ranges"])))
          (expect (not (contains? (get-in multi [:result "results" 0]) "range")))
          (expect (= 3 (get-in whole [:result "results" 0 "line_count"])))
          (expect (= 3 (get-in multi [:result "results" 0 "line_count"])))
          ;; cat's whole-file sentinel unslices ONE batched path here too
          (let [unsliced (index {"paths" [{"path" f "ranges" [[-1 -1]]}]})]
            (expect (= ["a" "b" "c"] (names unsliced)))
            (expect (nil? (get-in unsliced [:result "results" 0 "ranges"]))))
          ;; a HALF sentinel is NOT the sentinel: it coerces to a real 1-based
          ;; window exactly like cat instead of indexing a nonsense one
          (let [half (index {"paths" [{"path" f "ranges" [[-1 3]]}]})]
            (expect (= ["a" "b" "c"] (names half)))
            (expect (= [[1 3]] (get-in half [:result "results" 0 "ranges"]))))
          ;; every other cat `ranges` shape coerces here too, echoed normalized
          (let [stringy (index {"paths" [{"path" f "ranges" "2, 2"}]})]
            (expect (= ["b"] (names stringy)))
            (expect (= [[2 2]] (get-in stringy [:result "results" 0 "ranges"]))))
          ;; `ranges` stays OPTIONAL: absent or empty indexes the WHOLE file, the
          ;; same as a bare path entry
          (doseq [spec [{"path" f} {"path" f "ranges" []}]]
            (let [r (index {"paths" [spec]})]
              (expect (= ["a" "b" "c"] (names r)))
              (expect (nil? (get-in r [:result "results" 0 "ranges"])))))
          ;; a malformed scalar raises the range normalizer's OWN guidance, never a raw
          ;; "Don't know how to create ISeq from: java.lang.Long"
          (let
            [msg
             (fn [thunk]
               (try (thunk) nil (catch Throwable t (ex-message t))))

             bad
             {"path" f "ranges" 3}]

            (expect (clojure.string/includes? (str (msg #(index {"paths" [bad]})))
                                              "[[start, end], ...]")))
          (expect (throws? clojure.lang.ExceptionInfo #(index {})))
          (expect (throws? clojure.lang.ExceptionInfo
                           #(index {"paths" [{"path" f "range" [2 2]}]}))))))))

(defdescribe batch-read-tools-test
             (let [index-tool (private-fn "index-tool")]
               (it "batches struct_index paths in request order"
                   (let
                     [_ (temp-dir-path "batch-read")
                      a (str (temp-root) "/batch-read/a.clj")
                      b (str (temp-root) "/batch-read/b.clj")]

                     (spit (fs/file a) "(defn a [] 1)\n")
                     (spit (fs/file b) "(defn b [] 2)\n")
                     (let [results (get-in (index-tool {"paths" [a b]}) [:result "results"])]
                       (expect (= [a b] (mapv #(get % "path") results)))
                       (expect (= ["a" "b"] (mapv #(get-in % ["definitions" 0 "name"]) results))))))
               (it "gives every batched path its OWN ranges"
                   (let
                     [_ (temp-dir-path "batch-ranges")
                      a (str (temp-root) "/batch-ranges/a.clj")
                      b (str (temp-root) "/batch-ranges/b.clj")]

                     (spit (fs/file a) "(defn a [] 1)\n(defn a2 [] 2)\n")
                     (spit (fs/file b) "(defn b [] 1)\n(defn b2 [] 2)\n")
                     (let
                       [results (get-in (index-tool {"paths" [{"path" a "ranges" [[1 1]]}
                                                              {"path" b "ranges" [[2 2]]}]})
                                        [:result "results"])]
                       (expect (= [a b] (mapv #(get % "path") results)))
                       (expect (= [[[1 1]] [[2 2]]]
                                  (mapv #(mapv vec (get % "ranges")) results))))))))

(defdescribe
  rg-tool-e2e-test
  "The `rg` TOOL over real files: the comma-split + smart-case fixes end-to-end."
  (let
    [find-tool
     (grep-data-fn)

     rg
     (fn [& a]
       (apply find-tool a))]

    (it "a comma query matches EITHER term (the session 71a69809 fix, real files)"
        (let
          [d
           (temp-dir-path "rge")

           f
           (str (temp-root) "/rge/a.clj")]

          (spit (fs/file f) "the model line\nthe cycle line\nunrelated\n")
          (let [r (rg {"query" "model, cycle" "paths" [d]})]
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
           (get-in (rg {"query" "MATCH" "paths" [d]}) [:result "matches"])

           ctx
           (get-in (rg {"query" "MATCH" "paths" [d] "context" 1}) [:result "matches"])

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
          (let [r (rg {"query" "key" "paths" [d]})]
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
           (rg {"query" "needle" "paths" [d ghost]})

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

        (with-redefs
          [workspace/allowed-roots
           (constantly [primary other vis-home])

           workspace/no-search-roots
           (constantly #{vis-home})]

          (let [roots (mapv str (:roots (rsr ["."])))]
            ;; ~/.vis pruned from the default sweep …
            (expect (not (some #(clojure.string/starts-with? % vis-home) roots)))
            ;; … while the primary and sibling roots survive, in order
            (expect (= [primary other] roots))))
        ;; the primary is exempt even when catalogued `search: false`: cwd == ~/.vis still scans.
        (with-redefs
          [workspace/allowed-roots
           (constantly [vis-home primary])

           workspace/no-search-roots
           (constantly #{vis-home})]

          (expect (= [vis-home primary] (mapv str (:roots (rsr ["."]))))))))
    (it
      "a real DRAFT clone under the drafts store (~/.vis/drafts) is KEPT in the default sweep even though it is under ~/.vis — so an in-draft session stays searchable, while the raw ~/.vis grant is still pruned"
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
          (with-redefs
            [workspace/allowed-roots (constantly [draft-primary draft-clone vis-home])
             workspace/no-search-roots (constantly #{vis-home})]

            (let [roots (mapv str (:roots (rsr ["."])))]
              ;; both draft clones kept because only the raw catalog root opts out …
              (expect (= [draft-primary draft-clone] roots))
              ;; … and the raw ~/.vis grant is gone.
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
        (let [r (rg {"query" needle "paths" [dir]})]
          (expect (:success? r))
          (expect (= 2 (get-in r [:result "file_count"]))))
        ;; naming ONE EXISTING file searches ONLY that file — NOT its sibling in the
        ;; same dir. An existing file is precise; it is NOT widened to its parent.
        (let [r (rg {"query" needle "paths" [a]})]
          (expect (:success? r))
          (expect (= 1 (get-in r [:result "file_count"])))
          (expect (= 1 (get-in r [:result "hit_count"])))
          ;; an existing path is never reported missing — but the key still ships
          (expect (= [] (get-in r [:result "missing_paths"]))))
        ;; a path that does NOT exist CLIMBS to its nearest existing ancestor dir
        ;; (here `dir`, holding a.clj + b.clj) so the search still runs — and the
        ;; ghost is REPORTED in missing_paths, never a hard error, never silent
        (let
          [ghost
           (str dir "/gone.clj")

           r
           (rg {"query" needle "paths" [ghost]})]

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
          (let
            [r (sp {"path" f "op" "append_child" "at" [1] "code" "3"})
             src (slurp (fs/file f))]

            (expect (:success? r))
            ;; `at [1]` locates the defn, so the new child belongs inside it.
            (expect (clojure.string/includes? src "(defn f [] (do 1 2) 3)")))))
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
  struct-patch-batch-test
  "struct_patch BATCHES at the tool level: one call carries an ORDERED `edits`
   array, top-level keys are shared defaults, and results come back one per edit
   in request order (across one file or several)."
  (let [sp (private-fn "struct-patch-tool")]
    (it "one `edits` batch edits several files in request order"
        (let
          [_ (temp-dir-path "spb")
           f1 (str (temp-root) "/spb/one.clj")
           f2 (str (temp-root) "/spb/two.clj")]

          (spit (fs/file f1) "(defn a [] 1)\n\n(defn b [] 2)\n")
          (spit (fs/file f2) "(defn c [] 3)\n")
          (let
            [r (sp {"edits" [{"path" f1 "op" "replace" "target" "a" "code" "(defn a [] 11)"}
                             {"path" f1 "op" "replace" "target" "b" "code" "(defn b [] 22)"}
                             {"path" f2 "op" "replace" "target" "c" "code" "(defn c [] 33)"}]})]
            (expect (:success? r))
            (expect (= 3 (count (:result r))))
            (expect (= 3 (get-in r [:metadata :edit-count])))
            (expect (= 3 (get-in r [:metadata :changed-count])))
            (expect (clojure.string/includes? (slurp (fs/file f1)) "(defn a [] 11)"))
            (expect (clojure.string/includes? (slurp (fs/file f1)) "(defn b [] 22)"))
            (expect (clojure.string/includes? (slurp (fs/file f2)) "(defn c [] 33)")))))
    (it "top-level keys are shared defaults; each edit sees the previous one's file"
        (let
          [_ (temp-dir-path "spb2")
           f (str (temp-root) "/spb2/m.clj")]

          (spit (fs/file f) "(defn a [] 1)\n")
          (let
            [r (sp {"path" f
                    "edits" [{"op" "rename" "target" "a" "code" "aa"}
                             {"op" "append" "code" "(defn d [] 4)"}]})
             src (slurp (fs/file f))]

            (expect (:success? r))
            (expect (= 2 (count (:result r))))
            (expect (clojure.string/includes? src "(defn aa [] 1)"))
            (expect (clojure.string/includes? src "(defn d [] 4)")))))
    (it "a failing entry stops the batch and names how many edits already applied"
        (let
          [_ (temp-dir-path "spb3")
           f (str (temp-root) "/spb3/m.clj")]

          (spit (fs/file f) "(defn a [] 1)\n")
          (let
            [r (try (sp {"path" f
                         "edits" [{"op" "replace" "target" "a" "code" "(defn a [] 9)"}
                                  {"op" "replace" "target" "nope" "code" "(defn nope [] 0)"}]})
                    (catch Throwable e e))]
            (expect (instance? Throwable r))
            (expect (clojure.string/includes? (ex-message r) "stopped at edit 2 of 2"))
            (expect (= 1 (:applied-count (ex-data r))))
            ;; No rollback: the first edit stands.
            (expect (clojure.string/includes? (slurp (fs/file f)) "(defn a [] 9)")))))))



(defdescribe
  find-files-op-name-test
  "Regression: renaming find_files→grep means the result `:op` must stay in lockstep
   with the symbol name — `op-tag` keys the observation/mutation registry by the wire
   name, so a mismatch throws `Unregistered extension op :find`."
  (it "the grep symbol IS named grep" (expect (= 'grep (:ext.symbol/symbol editing/grep-symbol))))
  (it "grep carries an observation tag (registry-resolvable)"
      (expect (= :observation (:ext.symbol/tag editing/grep-symbol)))))

(defdescribe empty-search-paths-default-test
             "grep scopes are directories; empty scope still means the workspace root."
             (let
               [coerce-find
                (private-fn "coerce-find-spec")

                coerce-rg
                (private-fn "coerce-rg-spec")

                find-paths
                (private-fn "find-arg-paths")]

               (it
                 "grep defaults empty paths to current directory in validation and path protection"
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
               (it "accepts context-lines and rejects a negative/non-integer one"
                   (expect (= 2 (:context (coerce-find [{"query" "needle" "context" 2}]))))
                   (expect (= 0 (:context (coerce-find [{"query" "needle"}]))))
                   (expect (throws? clojure.lang.ExceptionInfo
                                    #(coerce-find [{"query" "needle" "context" -1}])))
                   (expect (throws? clojure.lang.ExceptionInfo
                                    #(coerce-find [{"query" "needle" "context" {"before" 1}}]))))
               (it "rg keeps its own empty-path and file-path semantics"
                   (let [spec {"query" ["FIND_FILES" "CAT"] "paths" []}]
                     (expect (= ["."] (:paths (coerce-rg spec))))))))

;; Regression: `grep(["a", "b"], ["src", "tools"])` — needles, then scopes, the
;; obvious reading of a two-argument search — died on ARGUMENT SHAPE instead of
;; searching, because the second positional meant OPTIONS and the error offered
;; three call shapes at once. There is ONE canonical shape now: a single options
;; map (Python kwargs fold into that same map), for `grep` and `struct_nodes`
;; alike, so nothing positional can be misread.
(defdescribe canonical-options-map-only-test
             "`grep` and `struct_nodes` take ONE options map — never a positional query or path."
             (let
               [grep-tool
                (grep-data-fn)

                nodes-tool
                (private-fn "nodes-tool")

                caught
                (fn [f & args]
                  (try (apply f args) nil (catch clojure.lang.ExceptionInfo e e)))]

               (it "grep refuses every positional shape and its message teaches the map"
                   (let [e (caught grep-tool ["a" "b"] ["src" "tools"])]
                     (expect (some? e))
                     (expect (= :ext.foundation.editing/invalid-find-args (:type (ex-data e))))
                     (expect (string/includes? (ex-message e)
                                               "grep({\"query\": q, \"paths\": [\"src\"]})")))
                   (expect (some? (caught grep-tool "needle")))
                   (expect (some? (caught grep-tool "needle" {"include" ["*.clj"]}))))
               (it "grep searches from the one canonical map"
                   (let
                     [_
                      (write-temp! "canonmap/a.clj" "needle here\n")

                      out
                      (grep-tool {"query" "needle" "paths" [(temp-dir-path "canonmap")]})]

                     (expect (= 1 (get (:result out) "hit_count")))))
               (it "struct_nodes refuses a positional path and answers the canonical map"
                   (let
                     [p
                      (write-temp! "canonnodes/a.clj" "(ns a)\n\n(defn zonk [x] x)\n")

                      e
                      (caught nodes-tool p)]

                     (expect (some? e))
                     (expect (= :ext.foundation.editing/invalid-nodes-args (:type (ex-data e))))
                     (expect (some? (caught nodes-tool p {"line" 3})))
                     (expect (:success? (nodes-tool {"path" p})))))))

(defdescribe find-files-directory-scope-test
             (let [find-files (grep-data-fn)]
               (it "does not widen an existing filename scope to its parent on zero content hits"
                   (let
                     [dir (temp-dir-path "find-file-parent")
                      scoped-file (str dir "/scope.clj")
                      sibling-file (str dir "/sibling.clj")
                      _ (spit (fs/file scoped-file) "before\nnot-it\nafter\n")
                      _ (spit (fs/file sibling-file) "before\nsibling-only-needle\nafter\n")
                      result (:result (find-files {"query" "sibling-only-needle"
                                                   "paths" [scoped-file]}))]

                     (expect (= 0 (get result "hit_count")))
                     (expect (= {} (get result "matches")))
                     (expect (nil? (get result "first_hit")))
                     (expect (= [] (get result "missing_paths")))))
               (it "uses an empty query as grep's ls mode without attempting content search"
                   (let
                     [_ (write-temp! "grep-ls/one.clj" ";; a\n")
                      _ (write-temp! "grep-ls/two.md" "# b\n")
                      _ (write-temp! "grep-ls/sub/three.txt" "c\n")
                      dir (temp-dir-path "grep-ls")
                      result (:result (find-files {"query" "" "paths" [dir] "limit" 10}))
                      names (set (map #(last (string/split % #"/")) (get result "paths")))]

                     (expect (= "" (get result "query")))
                     (expect (= #{"one.clj" "two.md" "three.txt"} names))
                     (expect (= 0 (get result "hit_count")))
                     (expect (empty? (get result "matches")))
                     (expect (not (contains? result "items")))
                     (expect (nil? (get result "hint")))))))

(defdescribe
  grep-searched-paths-reporting-test
  "`searched_paths` reports every physical root actually searched, not the
   caller's shorthand `.` that expanded into those roots."
  (it "expands the default scope into the primary and searchable workspace roots"
      (let
        [grep-tool
         (grep-data-fn)

         rel-path
         (private-fn "rel-path")

         parent
         (temp-dir-path "grep-reported-roots")

         primary
         (str parent "/primary")

         sibling
         (str parent "/sibling")]

        (doseq [dir [primary sibling]]
          (fs/create-dirs dir))
        (spit (fs/file primary "one.txt") "primary\n")
        (spit (fs/file sibling "two.txt") "sibling\n")
        (binding [workspace/*workspace-root* (java.io.File. primary)]
          (with-redefs
            [workspace/allowed-roots (constantly [primary sibling])
             workspace/no-search-roots (constantly #{})
             workspace/filesystem-root-mappings (constantly [])]

            (let
              [result (:result (grep-tool {"query" ""}))
               expected (mapv rel-path (map fs/file [primary sibling]))]

              (expect (= expected (get result "searched_paths")))
              (expect (not= ["."] (get result "searched_paths")))))))))

;; A capped grep had no way to ask for the NEXT page: the only way past the
;; limit was to re-run the same search with a bigger `limit` and pay for the
;; first page all over again.
(defdescribe
  grep-paging-test
  "`offset` + `next_offset` page BOTH grep axes — the CONTENT hits and the
   ranked NAME matches. `next_offset` is null exactly when the page already is
   the whole answer, so a paging loop terminates on a value test."
  (let
    [grep-tool
     (grep-data-fn)

     page
     (fn [spec]
       (:result (grep-tool spec)))

     texts
     (fn [r]
       (->> (get r "matches")
            vals
            (mapcat vals)
            (mapv #(get % "text"))))]

    (it "a capped CONTENT sweep hands back the offset of its next page"
        (let
          [_
           (write-temp! "grep-page/a.txt" (string/join "\n" (map #(str "needle " %) (range 300))))

           dir
           (temp-dir-path "grep-page")

           first-page
           (page {"query" "needle" "paths" [dir]})

           second-page
           (page {"query" "needle" "paths" [dir] "offset" 50})]

          (expect (= 0 (get first-page "offset")))
          (expect (= 50 (get first-page "hit_count")))
          (expect (= "limit" (get first-page "hits_truncated_by")))
          (expect (= 50 (get first-page "next_offset")))
          (expect (= "needle 0" (first (texts first-page))))
          ;; The second page starts exactly where the first stopped: no repeat,
          ;; no gap.
          (expect (= 50 (get second-page "offset")))
          (expect (= "needle 50" (first (texts second-page))))
          (expect (not-any? (set (texts first-page)) (texts second-page)))))
    (it "the LAST page reports next_offset null, so the loop stops"
        (let
          [_
           (write-temp! "grep-page-end/a.txt"
                        (string/join "\n" (map #(str "needle " %) (range 60))))

           dir
           (temp-dir-path "grep-page-end")

           last-page
           (page {"query" "needle" "paths" [dir] "offset" 50})]

          (expect (= 10 (get last-page "hit_count")))
          (expect (nil? (get last-page "hits_truncated_by")))
          ;; TOTAL key: it ships on every result, null when there is no more.
          (expect (contains? last-page "next_offset"))
          (expect (nil? (get last-page "next_offset")))))
    (it "the ranked NAME list pages on the same knob"
        (let
          [_
           (doseq [i (range 30)]
             (write-temp! (str "grep-page-name/pagefile-" i ".clj") "(ns x)\n"))

           dir
           (temp-dir-path "grep-page-name")

           first-page
           (page {"query" "pagefile" "paths" [dir] "limit" 10})

           second-page
           (page {"query" "pagefile" "paths" [dir] "limit" 10 "offset" 10})]

          (expect (= 10 (count (get first-page "paths"))))
          (expect (= "limit" (get first-page "truncated_by")))
          (expect (= 10 (get first-page "next_offset")))
          (expect (= 10 (count (get second-page "paths"))))
          (expect (not-any? (set (get first-page "paths")) (get second-page "paths")))))
    (it "a negative offset is refused at the seam, never silently floored"
        (expect (throws? clojure.lang.ExceptionInfo #(grep-tool {"query" "needle" "offset" -1}))))))

(defdescribe
  grep-truncation-and-literal-dialect-test
  "Two silent dead ends the runaway-loop post-mortems traced back to `grep`:
   a CAPPED content sweep looked complete (the top-level `truncated_by` is the
   NAME list's, so it read `end_of_results` while files were dropped), and an
   rg-style REGEX query returned zero content hits with no word about the
   dialect — both invite the caller to re-run cosmetic variants forever."
  (let
    [content-result
     (private-fn "content-result")

     find-files
     (grep-data-fn)]

    (it "a content sweep capped by the hit limit reports hits_truncated_by"
        (let
          [out (content-result {:hits [{:path "a.clj" :line 1 :text "x"}]
                                :truncated-by :limit
                                :total-file-count 42
                                :total-file-count-exact? true}
                               ["x"])]
          (expect (= "limit" (get out "hits_truncated_by")))
          (expect (= 42 (get out "total_file_count")))))
    (it "a byte-budget cap is reported too"
        (expect (= "bytes"
                   (get (content-result {:hits [{:path "a.clj" :line 1 :text "x"}]
                                         :truncated-by :bytes
                                         :total-file-count 9
                                         :total-file-count-exact? true}
                                        ["x"])
                        "hits_truncated_by"))))
    (it "a COMPLETE content sweep still SHIPS hits_truncated_by, as null"
        (let
          [r (content-result {:hits [{:path "a.clj" :line 1 :text "x"}]
                              :truncated-by :end-of-results
                              :total-file-count 1
                              :total-file-count-exact? true}
                             ["x"])]
          ;; TOTAL result: the key is a VALUE test, never a `contains?` test.
          (expect (contains? r "hits_truncated_by"))
          (expect (nil? (get r "hits_truncated_by")))
          (expect (true? (get r "total_file_count_is_exact")))
          (expect (= 1 (get r "total_file_count")))))
    (it "a regex-looking query that matches no CONTENT says the search is literal"
        (let
          [_
           (write-temp! "grep-regex/needle.clj" "(defn needle [] :ok)\n")

           dir
           (temp-dir-path "grep-regex")

           result
           (:result (find-files {"query" "defn-? +needle" "paths" [dir]}))]

          (expect (= 0 (get result "hit_count")))
          (expect (string/includes? (get result "hint") "LITERAL"))))
    (it "a plain literal query that matches nothing keeps the ordinary hint"
        (let
          [_
           (write-temp! "grep-plain/needle.clj" "(defn needle [] :ok)\n")

           dir
           (temp-dir-path "grep-plain")

           result
           (:result (find-files {"query" "zzz-absent-symbol" "paths" [dir]}))]

          (expect (= 0 (get result "hit_count")))
          (expect (not (string/includes? (get result "hint") "LITERAL")))))
    (it "a literal query that DOES match content gets no hint"
        (let
          [_
           (write-temp! "grep-ok/needle.clj" "(defn needle [] :ok)\n")

           dir
           (temp-dir-path "grep-ok")

           result
           (:result (find-files {"query" "defn needle" "paths" [dir]}))]

          (expect (pos? (get result "hit_count")))
          (expect (nil? (get result "hint")))))))

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

;; Regression, session c3caf9c2-58c8-4678-bf2f-8d3efae2e305: a grep carrying
;; `include ""` — an empty OPTIONAL filter — was refused outright with "rg string
;; values must be non-blank.", losing the whole search, while `include []` and a
;; missing `include` searched everything.
(defdescribe
  rg-blank-include-test
  "An empty include glob restricts nothing and must read as NO filter, exactly
   like nil/[], instead of failing the call."
  (let [coerce-rg (private-fn "coerce-rg-spec")]
    (it "include \"\" is no filter, like a missing include"
        (expect (= [] (:include (coerce-rg {"query" ["x"] "include" ""}))))
        (expect (= [] (:include (coerce-rg {"query" ["x"] "include" "   "}))))
        (expect (= [] (:include (coerce-rg {"query" ["x"]})))))
    (it "a blank entry inside a real list is dropped, the real globs survive"
        (expect (= ["**/*.clj"] (:include (coerce-rg {"query" ["x"] "include" ["" "**/*.clj"]}))))
        (expect (= [] (:include (coerce-rg {"query" ["x"] "include" ["" "  "]})))))
    (it "a non-string include is still refused"
        (let [err (try (coerce-rg {"query" ["x"] "include" [42]}) (catch Exception e e))]
          (expect (= :ext.foundation.editing/invalid-rg-spec (:type (ex-data err))))))
    (it "a blank QUERY term is still an error, since a query is the search itself"
        (let [err (try (coerce-rg {"query" [""]}) (catch Exception e e))]
          (expect (= :ext.foundation.editing/invalid-rg-spec (:type (ex-data err))))))))

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

          (expect (false? (get out "fuzzy")))
          (expect (some #{"channel_tui_footer.clj"}
                        (map #(last (string/split % #"/")) (get out "paths"))))))
    (it "a BLANK query lists every file under the paths like ls (no scoring)"
        (let
          [_ (write-temp! "findls/one.clj" ";; a\n")
           _ (write-temp! "findls/two.md" "# b\n")
           _ (write-temp! "findls/sub/three.txt" "c\n")
           dir (temp-dir-path "findls")
           out (find-search [{"paths" [dir]}])
           names (set (map #(last (string/split % #"/")) (get out "paths")))]

          (expect (= "" (get out "query")))
          (expect (false? (get out "fuzzy")))
          (expect (= [] (get out "matched_terms")))
          (expect (= 3 (get out "item_count")))
          (expect (= #{"one.clj" "two.md" "three.txt"} names))
          (expect (every? #(get % "path") (get out "items")))))
    ;; Regression: the fuzzy fallback capped itself at 20 items, so a filename
    ;; search could never reach grep's own default limit of 50 elements.
    (it "a fuzzy fallback fills grep's default 50-element limit, not a private 20"
        (let
          [_ (doseq [i (range 30)]
               (write-temp! (format "findfuzzcap/alphaonly_%02d.clj" i) ";; x\n"))
           dir (temp-dir-path "findfuzzcap")
           out (find-search [{"query" "alphaonly betaonly" "paths" [dir]}])]

          (expect (true? (get out "fuzzy")))
          (expect (= 50 (get out "limit")))
          (expect (= 30 (get out "item_count")))))
    (it "EVERY grep result carries the SAME TOTAL key set — hit, miss, ls, stale scope"
        (let
          [gt (grep-data-fn)
           _ (write-temp! "greptotal/one.clj" ";; needle-total\n")
           dir (temp-dir-path "greptotal")
           ks #(set (keys (:result %)))
           hit (gt {"query" "needle-total" "paths" [dir]})
           miss (gt {"query" "zzz-nothing-here-xyz" "paths" [dir]})
           ls (gt {"query" "" "paths" [dir]})
           stale (gt {"query" "needle-total" "paths" [(str dir "/gone/deeper.clj")]})]

          ;; One shape for every outcome: caller code indexes a field instead of
          ;; probing for it, so a nil-valued signal can never read as a tool bug.
          (expect (= (ks hit) (ks miss) (ks ls) (ks stale)))
          (expect (every? (ks hit)
                          ["missing_paths" "hits_truncated_by" "file_counts" "total_file_count"
                           "total_file_count_is_exact" "first_hit"]))
          (expect (nil? (get-in hit [:result "hits_truncated_by"])))
          (expect (= [] (get-in hit [:result "missing_paths"])))
          (expect (seq (get-in stale [:result "missing_paths"])))))
    (it "a genuinely-unmatchable query still returns nothing (fuzzy can't invent hits)"
        (let
          [_ (write-temp! "findnone/alpha.clj" ";; x\n")
           dir (temp-dir-path "findnone")
           out (find-search [{"query" "zzzqqq wwwvvv" "paths" [dir]}])]

          (expect (zero? (get out "item_count")))))))

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
     [editing/struct-patch-symbol editing/index-symbol editing/nodes-symbol]]

    (it "a Clojure project advertises every structural editor"
        (doseq [s struct-syms]
          (expect (true? (active? s ["clojure"])))))
    (it "a docs-only (markdown/text) project HIDES them; grep stays"
        (doseq [s struct-syms]
          (expect (false? (active? s ["markdown" "text"]))))
        (doseq [s [editing/grep-symbol]]
          (expect (true? (active? s ["markdown" "text"])))))
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
  gitignore-always-respected-test
  ;; `.gitignore` is ALWAYS honored: there is no per-call opt-out any more. The
  ;; former `is_respect_gitignore` parameter is GONE — the find spec rejects it as
  ;; an unknown key, and `vis.yml`'s `:grep` overlay is the only way to widen what
  ;; search sees. Everything stays on the native fff index; no raw filesystem walk.
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

    (it "rg never surfaces a gitignored file, and the removed flag is inert"
        (let [path (fixture! "gitignore-always-rg")]
          (expect (zero? (:total-file-count (grep {"query" ["NEEDLE_TOKEN"] "paths" [path]}))))
          (expect (zero? (:total-file-count (grep {"query" ["NEEDLE_TOKEN"]
                                                   "paths" [path]
                                                   "is_respect_gitignore" false}))))))
    (it "the find spec REJECTS is_respect_gitignore as an unknown key"
        (let
          [path
           (fixture! "gitignore-always-unknown-key")

           thrown
           (try (find-search [{"query" "secret" "paths" [path] "is_respect_gitignore" false}])
                nil
                (catch Exception e e))]

          (expect (some? thrown))
          (expect (= :ext.foundation.editing/invalid-find-args (:type (ex-data thrown))))
          (expect (= ["is_respect_gitignore"] (:unknown (ex-data thrown))))))
    (it "find_files never surfaces a gitignored file and never walks the tree"
        (let [path (fixture! "gitignore-always-find")]
          (expect (empty? (get (find-search [{"query" "secret" "paths" [path]}]) "paths")))
          (expect (nil? (core-var "find-walk-files")))
          (expect (nil? (core-var "score-walked-candidates")))))))

(defdescribe
  tool-ignore-negation-layering-test
  ;; `.gitignore` still hides a path from git AND our tools by default, but a
  ;; `!`-negation in a TOOL-ONLY `.ignore`/`.rgignore` (files git never reads)
  ;; re-includes it for rg/find_files while `.gitignore` itself keeps hiding it.
  ;; Precedence (LOW→HIGH): .gitignore < .ignore < .rgignore, so a
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
  grep-overlay-config-test
  ;; Issue #23: a `:grep {:include-gitignored-paths [...]}` config overlay
  ;; re-includes chosen gitignored subtrees for rg AND find_files with
  ;; `.gitignore` ALWAYS respected — the walker descends the
  ;; excluded dir (which a `.gitignore` `!` negation can never do: git never
  ;; descends an excluded directory, so a negation on a child is dead code),
  ;; while `:always-exclude` (defaults: `.git/`, `node_modules/`, `target/`, …)
  ;; keeps pruning INSIDE the rescued subtree. Config is the ONLY lever: there
  ;; is no per-call gitignore opt-out.
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
         ;; `config/search-overlay` reads the raw `grep` config block.
         [config/load-config-raw
          (fn []
            {"grep" (cond-> {}
                      (seq (:include-gitignored-paths search-block))
                      (assoc "include_gitignored_paths" (:include-gitignored-paths search-block))

                      (seq (:always-exclude search-block))
                      (assoc "always_exclude" (:always-exclude search-block)))})]
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
    (it "the overlay is the ONLY lever — no per-call gitignore opt-out exists"
        (let [path (fixture! "search-overlay-explicit")]
          ;; without config the gitignored subtree stays hidden …
          (expect (not (has? (rg-files path) "repositories/corp/secret.txt")))
          (overlay! {:include-gitignored-paths ["repositories/"]}
                    (fn []
                      ;; … and with it the rescue applies to EVERY call
                      (expect (has? (rg-files path) "repositories/corp/secret.txt"))
                      (expect (not (has? (rg-files path) "secret_dep.txt")))))))
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
     (fff-index-fn "with-scan-permit*")

     semaphore
     (fff-index-fn "scan-semaphore")

     permits
     (fff-index-fn "scan-max-concurrency")]

    (describe
      "with-scan-permit*"
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
      "fff-index/open! wiring"
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

             open-index!
             (fff-index-fn "open!")]

            (with-redefs
              [fff/create
               (fn [_opts]
                 (reset! seen-during-build (.availablePermits semaphore))
                 fake-idx)

               fff/wait-for-scan
               (fn [_idx _timeout]
                 true)]

              (open-index! (java.io.File. ".")))
            ;; one permit taken while the (stubbed) scan ran
            (expect (= (dec permits) @seen-during-build))
            ;; released once the build returned
            (expect (= permits (.availablePermits semaphore)))))
      (it "releases the permit when fff's scan times out"
          ;; wait-for-scan false → open! closes the idx and throws; the
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

             open-index!
             (fff-index-fn "open!")]

            (with-redefs
              [fff/create
               (fn [_opts]
                 fake-idx)

               fff/wait-for-scan
               (fn [_idx _timeout]
                 false)]

              (expect (throws? clojure.lang.ExceptionInfo #(open-index! (java.io.File. ".")))))
            (expect @closed?)
            (expect (= before (.availablePermits semaphore)))))
      (it "reports the queue wait and the scan wait APART when it times out"
          ;; `wait-for-scan` cannot tell "still walking your tree" from "never got
          ;; a pool thread", and both are billed to the same 30s ceiling — so the
          ;; error carries the two numbers instead of accusing the filesystem.
          (let
            [open-index!
             (fff-index-fn "open!")

             fake-idx
             (reify
               java.io.Closeable
                 (close [_] nil))]

            (with-redefs
              [fff/create
               (fn [_opts]
                 fake-idx)

               fff/wait-for-scan
               (fn [_idx _timeout]
                 false)]

              (let
                [data (try (open-index! (java.io.File. "."))
                           nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                (expect (= :ext.foundation.editing/fff-scan-timeout (:type data)))
                (expect (number? (:queued-ms data)))
                (expect (number? (:scan-ms data)))
                ;; the timing-out scan still holds its own permit
                (expect (pos? (long (:scans-in-flight data))))))))
      (it "never offers more permits than fff's own scan pool can START"
          ;; fff runs every scan on ONE background pool
          ;; (`crates/fff-core/src/parallelism.rs`: `max(cores / 2, 2)`), and a
          ;; permit handed out past that width does not start a scan — it queues
          ;; one, while `wait-for-scan` counts the wait against `scan-timeout-ms`.
          (expect (pos? permits))
          (expect (<= permits (fff-index-fn "fff-scan-pool-width")))))))

(defdescribe
  fff-index-pool-test
  (describe
    "pooled fff index"
    (it
      "is REUSED across searches and still sees a file written microseconds ago"
      (let
        [_
         (write-temp! "fffpool/seed.txt" "seed marker zzpoolseed\n")

         dir
         (temp-dir-path "fffpool")

         rg
         (private-fn "rg-search")

         pool
         (fff-index-fn "pool")

         files
         (fn [q]
           (set (:files (rg {"query" q "paths" [dir] "is_files_only" true}))))

         _
         (files "zzpoolseed")

         pooled
         (count @pool)

         fresh
         (str "zzpoolfresh" (System/nanoTime))

         _
         (write-temp! "fffpool/fresh.txt" (str "hello " fresh "\n"))

         ;; every Vis write path calls this; it is what makes the NEXT search
         ;; rescan instead of waiting on the (async, ~100ms) watcher
         _
         ((fff-index-fn "note-fs-write!"))

         hit
         (files fresh)]

        ;; the search left a live index in the pool …
        (expect (pos? pooled))
        ;; … and the NEXT search reuses it instead of building another
        (expect (= pooled (count @pool)))
        ;; … yet a file written after that index was built is still found,
        ;; because the write bumped the epoch and the lease resynced
        (expect (some (fn [p]
                        (re-find #"fresh\.txt" p))
                      hit))))
    (it
      "does NOT rescan when nothing was written between searches"
      (let
        [_
         (write-temp! "fffnoscan/seed.txt" "zznoscanseed\n")

         dir
         (temp-dir-path "fffnoscan")

         rg
         (private-fn "rg-search")

         pool
         (fff-index-fn "pool")

         note!
         (fff-index-fn "note-fs-write!")

         run
         (fn []
           (rg {"query" "zznoscanseed" "paths" [dir]}))

         _
         (run)

         entry
         (some (fn [[[p _] e]]
                 (when (re-find #"fffnoscan" p) e))
               @pool)

         ^java.util.concurrent.atomic.AtomicLong synced
         (:synced-epoch entry)

         before
         (.get synced)

         _
         (do (run) (run))

         idle
         (.get synced)

         _
         (do (note!) (run))

         after
         (.get synced)]

        ;; steady state is FREE: no write => no rescan => epoch never moves
        (expect (some? entry))
        (expect (= before idle))
        ;; a write moves it exactly once
        (expect (< idle after))
        (expect (= after (do (run) (.get synced))))))))


(defdescribe
  ls-directory-fff-overlay-test
  (it
    "ls directory listing applies the native vis.yml grep overlay"
    (let
      [dir-name
       "ls-fff-overlay"

       rel-dir
       (str (temp-root) "/" dir-name)

       rel-included
       (str rel-dir "/repositories/")

       _
       (write-temp! (str dir-name "/.gitignore") "repositories/\n")

       _
       (write-temp! (str dir-name "/repositories/kept.txt") "visible only through config\n")

       dir
       (temp-dir-path dir-name)

       names
       (fn []
         (->> (ls-rows {"paths" [dir]})
              first
              (#(get % "entries"))
              (map (fn [e]
                     (get e "name")))
              set))]

      ;; The default FFF index honors .gitignore; the live config overlay opens it.
      (expect (not (contains? (names) "repositories")))
      (with-redefs
        [config/load-config-raw (fn []
                                  {"grep" {"include_gitignored_paths" [rel-included]}})]
        (expect (contains? (names) "repositories"))))))

;; Regression, issue #126: `ls` was fff-only, and fff refuses to index a filesystem
;; root or a home directory ("Can not run certain FFF features in a file system root
;; or home directories"), so `ls("/")` and `ls("~")` answered with
;; "rg requires fff for directory search, but fff failed for /" and the ROOT files
;; were unreachable — the one listing a real `ls` never fails at.
(defdescribe ls-unindexable-dir-test
             (it "ls lists a directory fff refuses to index — filesystem root and home"
                 (let [home (System/getProperty "user.home")]
                   ;; the session that reported this had the WHOLE filesystem granted as a root
                   (with-redefs
                     [workspace/allowed-roots (constantly ["/" home])
                      workspace/filesystem-root-mappings (constantly [{:trunk "/" :clone "/"}])]

                     (let
                       [row (fn [spec]
                              (first (ls-rows {"paths" [spec]})))
                        names (fn [spec]
                                (into #{} (map #(get % "name")) (get (row spec) "entries")))
                        root-names (names "/")]

                       (expect (contains? root-names "usr"))
                       (expect (contains? root-names "etc"))
                       ;; dotfiles still need `is_hidden`, exactly as under fff
                       (expect (not-any? #(string/starts-with? % ".") root-names))
                       ;; The root has no dotfiles on a clean Linux runner, but every home does
                       ;; (.bashrc/.profile on Linux, .zshrc/.zshenv on macOS).
                       (expect (some #(string/starts-with? % ".")
                                     (names {"path" home "is_hidden" true})))
                       ;; a home directory is refused for the same reason and must list too
                       (expect (= "dir" (get (row home) "type"))))))))

(defdescribe
  ls-fff-index-reuse-test
  (it
    "ls serves a workspace subdirectory from the WARM workspace index, building no per-directory index"
    (let
      [lease
       (fff-index-fn "lease")

       warm?
       (fff-index-fn "warm?")

       ignore-overlay
       (private-fn "fff-ignore-overlay")

       ls-overlay
       (private-fn "fff-ls-overlay")

       sub-rel
       "src/com/blockether/vis/internal/foundation/editing"

       root
       (.getCanonicalFile (java.io.File. "."))

       sub
       (.getCanonicalFile (java.io.File. sub-rel))

       names
       (fn [path]
         (->> (ls-rows {"paths" [path]})
              first
              (#(get % "entries"))
              (map (fn [e]
                     (get e "name")))
              set))

       _
       (names ".")

       ;; Pool state is PROCESS-WIDE and other namespaces in the same suite may
       ;; already hold an index here, so the pin is the DELTA: listing must not
       ;; warm anything that was cold.
       sub-cold-before
       [(warm? (lease sub true (ignore-overlay))) (warm? (lease sub true (ls-overlay sub-rel)))]

       listed
       (names sub-rel)]

      ;; Listing the workspace root warms the SAME pooled index grep/find use.
      (expect (warm? (lease root true (ignore-overlay))))
      (expect (contains? listed "core.clj"))
      ;; The subdirectory listing came out of that index: no fresh index, no fresh
      ;; watcher, under either pool key the fallback would have used.
      (expect (= sub-cold-before
                 [(warm? (lease sub true (ignore-overlay)))
                  (warm? (lease sub true (ls-overlay sub-rel)))])))))

;; =============================================================================
;; `ls` vs the real filesystem, and the PYTHON SANDBOX surface.
;;
;; `ls` is served out of the fff index (glob/search pages), never `File.listFiles`,
;; so the listing has to be cross-validated against the OS: same names, minus the
;; entries fff legitimately hides (dotfiles, gitignored paths).
;;
;; And `python_execution` is the model's main hand: reading a file is an ORDINARY
;; Python call (`cat`, engine-bound), and listing a directory is the `ls` SHIM — a
;; plain function in the sandbox globals, costing no tool result at all.
;; =============================================================================

(defdescribe
  ls-matches-filesystem-test
  "The fff-backed listing agrees with the OS on directories with nothing ignored."
  (it "returns exactly the non-hidden children `File.listFiles` reports"
      (doseq
        [dir ["src/com/blockether/vis/internal/foundation"
              "src/com/blockether/vis/internal/foundation/editing" "resources/vis-shims"]]
        (let
          [listed (set (map #(get % "name") (get ((private-fn "ls-one") {"path" dir}) "entries")))
           on-disk (set (remove #(string/starts-with? % ".")
                          (map #(.getName ^java.io.File %)
                               (.listFiles (java.io.File. ^String dir)))))]

          (expect (seq listed))
          (expect (= on-disk listed)))))
  ;; The repo root is the interesting case: fff must still WITHHOLD gitignored
  ;; children (`target/`) that `listFiles` happily reports.
  (it "omits gitignored children the OS still lists"
      (let [listed (set (map #(get % "name") (get ((private-fn "ls-one") {"path" "."}) "entries")))]
        (expect (contains? listed "src"))
        (expect (contains? listed "deps.edn"))
        (expect (not (contains? listed "target")))
        (expect (not-any? #(string/starts-with? % ".") listed)))))

(defdescribe
  python-sandbox-read-surface-test
  "`ls` is a sandbox SHIM: an ordinary call inside a `python_execution` block,
   documented there, and really callable. Reading a file's bytes is plain Python."
  (it "ships `ls` as a shim global and binds no retired read verb"
      (let
        [bind
         (extension/builtin-sandbox-bindings (constantly nil))

         docs
         (extension/sandbox-symbol-docs)

         ls-shim
         (some #(when (= "ls" (:shim/name %)) %) (extension/sandbox-shims))]

        ;; `cat` and `patch` are BOUND and DOCUMENTED: the anchored read/write
        ;; pair is a first-class part of the sandbox surface again.
        (expect (some? (get bind 'cat)))
        (expect (some? (get docs 'cat)))
        (expect (some? (get bind 'patch)))
        (expect (some? (get docs 'patch)))
        ;; `ls` left the tool layer too: a shim global, never an engine-bound
        ;; symbol, so it costs no schema and no tool result.
        (expect (nil? (get bind 'ls)))
        (expect (nil? (get docs 'ls)))
        (expect (= ["ls"] (:shim/globals ls-shim)))))
  (it "lists a real directory from real Python"
      (let
        [ctx
         (:python-context (ep/create-python-context (extension/builtin-sandbox-bindings (constantly
                                                                                          nil))))

         result
         (ep/run-python-block ctx
                              (str "names = {e[\"name\"] for e in "
                                   "ls(\"src/com/blockether/vis/internal/foundation/editing\")}\n"
                                   "print(\"core.clj\" in names)")
                              "t1/i1")]

        (expect (nil? (:error result)))
        (expect (= "True\n" (:stdout result)))))
  ;; Regression: `cat` answered its anchored text as a BARE STRING, so the FIRST
  ;; real call from the sandbox died at the extension boundary with "Symbol 'cat'
  ;; must return a canonical :envelope map"; and because its declared parameter
  ;; was named `from` — a Python KEYWORD — the signature stub would not compile,
  ;; so `inspect.signature(cat)` fell back to `(*a, **k)`. Calling the tool fn
  ;; directly, which is what every other cat/patch test does, sees neither.
  (it "cat reads and patch writes from real Python, and both report their parameters"
      (let
        [rel
         (write-temp! "cat/sandbox.txt" "alpha\nbeta\ngamma\n")

         ctx
         (:python-context (ep/create-python-context (extension/builtin-sandbox-bindings (constantly
                                                                                          nil))))

         result
         (ep/run-python-block ctx
                              (str "import inspect\n"
                                   "p = " (pr-str rel)
                                   "\n" "print(inspect.signature(cat))\n"
                                   "print(inspect.signature(patch))\n" "text = cat(p)\n"
                                   "print(text)\n"
                                   "print(patch(p, text.splitlines()[1].split('│ ')[0], 'BETA'))\n")
                              "t1/i2")

         out
         (str (:stdout result))]

        (expect (nil? (:error result)))
        (expect (string/includes? out "(path, start=None, end=None)"))
        (expect (string/includes? out "(path, from_anchor, to_anchor=None, replacement=None)"))
        (expect (re-find #"(?m)^1:[0-9a-f]{3}│ alpha$" out))
        (expect (string/includes? out "→ 1 line"))
        (expect (= "alpha\nBETA\ngamma\n" (slurp rel))))))

;; =============================================================================
;; `ls` ORDERING, and the two sources allowed to answer a listing.
;;
;; A listing can come from the WARM workspace index (the fast path `grep`/`find`
;; already keep hot) or from an index rooted at the listed directory (the
;; fallback). Neither the rows nor their order may depend on which one answered,
;; and the documented order — directories first, then alphabetical — has to hold
;; at EVERY nesting level, not just the top one: a model reading a `depth 3` tree
;; scans children the same way it scans the root.
;; =============================================================================

(defn- ls-order-key
  "One entry's position under the documented listing order."
  [entry]
  [(if (= "dir" (get entry "type")) 0 1) (str (get entry "name"))])

(defn- ls-order-violations
  "Every `[path [worse better]]` adjacent pair that breaks the documented order,
   walking `entries` AND every nested `children` vector."
  [path entries]
  (into (vec (for
               [[a b]
                (partition 2 1 (map ls-order-key entries))

                :when (not (neg? (compare a b)))]

               [path [a b]]))
        (mapcat (fn [entry]
                  (when-let [kids (get entry "children")]
                    (ls-order-violations (get entry "path") kids))))
        entries))

(defdescribe
  ls-ordering-test
  "Directories first, then alphabetical — recursively."
  (it "keeps every nesting level ordered, dotfiles included"
      (doseq
        [dir ["." "src" "test/com/blockether/vis" "src/com/blockether/vis/internal/foundation"
              "resources"]]
        (let
          [entries (get ((private-fn "ls-one") {"path" dir "depth" 3 "is_hidden" true}) "entries")]
          (expect (seq entries))
          (expect (= [] (ls-order-violations dir entries))))))
  (it "sorts a nested children vector, not just the top level"
      (let
        [_
         (write-temp! "lsorder/b-dir/z.txt" "z")

         _
         (write-temp! "lsorder/b-dir/a.txt" "a")

         _
         (write-temp! "lsorder/b-dir/m-sub/x.txt" "x")

         _
         (write-temp! "lsorder/a.txt" "a")

         dir
         (temp-dir-path "lsorder")

         out
         ((private-fn "ls-one") {"path" dir "depth" 2})

         b-dir
         (some #(when (= "b-dir" (get % "name")) %) (get out "entries"))]

        (expect (= ["b-dir" "a.txt"] (mapv #(get % "name") (get out "entries"))))
        ;; the nested vector obeys the same rule: the directory `m-sub` outranks
        ;; both files even though its name sorts between them
        (expect (= ["m-sub" "a.txt" "z.txt"] (mapv #(get % "name") (get b-dir "children"))))
        (expect (= [] (ls-order-violations dir (get out "entries")))))))

(defdescribe
  ls-source-agreement-test
  "The fast path and the fallback are interchangeable."
  (it
    "renders an identical listing whichever index answers"
    (let
      [dir
       "src/com/blockether/vis/internal/foundation"

       root
       (.getCanonicalFile (java.io.File. dir))

       ;; the workspace index has to be warm before it can serve anything
       _
       ((private-fn "ls-one") {"path" "."})

       warm-rows
       ((private-fn "fff-ls-workspace-items") root dir 2 false)

       fallback-rows
       ((private-fn "fff-ls-target-items") root dir 2 false)

       ;; `fff-ls-workspace-items` carries primitive hints, so the stand-in has
       ;; to match its shape
       listing
       (fn [rows]
         (with-redefs
           [editing/fff-ls-workspace-items (fn [_ _ ^long _ _]
                                             rows)]
           (get ((private-fn "ls-one") {"path" dir "depth" 2}) "entries")))]

      ;; the fast path really answered — this is not two runs of the fallback
      (expect (seq warm-rows))
      (expect (= (set (map (juxt :relative-path (comp boolean :directory?)) warm-rows))
                 (set (map (juxt :relative-path (comp boolean :directory?)) fallback-rows))))
      (expect (= (listing warm-rows) (listing fallback-rows)))))
  (it "never pays fff's mixed file+directory merge"
      ;; `search-mixed` re-ranks a union `ls` does not need; going near it is the
      ;; regression this pins (measured 3.3 ms vs 0.8 ms on this repo).
      (with-redefs
        [fff/search-mixed (fn [& _]
                            (throw (ex-info "ls must not call search-mixed" {})))]
        (let
          [root (get ((private-fn "ls-one") {"path" "." "depth" 2}) "entries")
           sub (get ((private-fn "ls-one") {"path" "src/com/blockether/vis/internal/foundation"})
                    "entries")]

          (expect (seq root))
          (expect (contains? (set (map #(get % "name") sub)) "editing")))))
  (it "serves a warm workspace subdirectory without ever building a fresh index"
      ;; `grep`/`find` keep the workspace index hot, and `ls` of a subdirectory has
      ;; to ride it. A fallback listing is still CORRECT, so agreement alone cannot
      ;; catch a silently lost fast path — only counting the fallback can.
      ((private-fn "ls-one") {"path" "."})
      (let [fresh (atom 0)]
        (with-redefs
          [editing/fff-ls-target-items (fn [_ _ ^long _ _]
                                         (swap! fresh inc)
                                         nil)]
          (doseq
            [spec [{"path" "src" "depth" 2} {"path" "test/com/blockether/vis/internal"}
                   {"path" "resources/vis-docs" "depth" 2 "is_hidden" true}]]
            (expect (seq (get ((private-fn "ls-one") spec) "entries")))))
        (expect (zero? @fresh)))))

(defdescribe
  ls-outside-workspace-test
  "A directory OUTSIDE the workspace is LISTED, never indexed.

   Reported as `ls`/`grep` of `~/.vis/models` taking minutes and then failing:
   naming two subdirectories leased a FRESH fff index rooted at that directory,
   which walks it, starts a watcher and builds a bigram CONTENT index over ~790 MB
   of model weights, then throws `fff-scan-timeout` past the 30s ceiling.
   `fff/list-directory` answers the same question with no index and no watcher at
   all — and, unlike the index, it can actually see dotfiles."
  (let
    [list-dir
     (private-fn "list-dir")

     outside!
     (fn []
       (let [dir (fs/path (System/getProperty "java.io.tmpdir") "vis-ls-outside-test")]
         (fs/delete-tree dir)
         (fs/create-dirs (fs/path dir "sub" "deep"))
         (spit (fs/file (fs/path dir "a.txt")) "a")
         (spit (fs/file (fs/path dir ".hidden")) "h")
         (spit (fs/file (fs/path dir "sub" "s.txt")) "s")
         (.getCanonicalFile (fs/file dir))))

     names
     (fn [out]
       (set (map #(get % "name") (get out "entries"))))]

    (it "lists a directory outside the workspace without ever leasing an index"
        (let
          [root
           (outside!)

           out
           (with-redefs
             [editing/fff-ls-target-items
              (fn [_ _ ^long _ _]
                (throw (ex-info "ls must not index a directory outside the workspace" {})))]
             (list-dir root {:depth 2}))]

          (expect (= #{"sub" "a.txt"} (names out)))
          ;; depth still descends through the stateless listing
          (expect (= ["deep" "s.txt"]
                     (mapv #(get % "name")
                           (get (some #(when (= "sub" (get % "name")) %) (get out "entries"))
                                "children"))))))
    (it "shows dotfiles outside the workspace when asked"
        ;; the per-directory index never held them, so `is_hidden` answered the
        ;; same listing whether or not it was set
        (let [root (outside!)]
          (expect (contains? (names (list-dir root {:depth 1 :is_hidden true})) ".hidden"))
          (expect (not (contains? (names (list-dir root {:depth 1})) ".hidden")))))
    ;; Regression: `ls` decided ownership from the RENDERED address, and `rel-path`
    ;; renders a context clone as its TRUNK absolute path — so the workspace itself,
    ;; mounted as a draft clone, read exactly like `/etc` and every listing inside it
    ;; silently lost the index path along with the `vis.yml` overlay it applies.
    (it "treats a context clone as the workspace it is, not as an outside tree"
        (let
          [clone
           (fs/path (System/getProperty "java.io.tmpdir") "vis-ls-clone-test")

           _
           (do (fs/delete-tree clone)
               (fs/create-dirs (fs/path clone "sub"))
               (spit (fs/file (fs/path clone "a.txt")) "a"))

           root
           (.getCanonicalFile (fs/file clone))

           indexed
           (atom 0)]

          (with-redefs
            [workspace/filesystem-root-mappings
             (fn []
               [{:trunk "/somewhere/else/trunk" :clone (.getPath root)}])

             editing/fff-ls-target-items
             (fn [_ _ ^long _ _]
               (swap! indexed inc)
               [{:relative-path "a.txt" :directory? false :size 1}])]

            (expect (= #{"a.txt"} (names (list-dir root {:depth 1}))))
            (expect (= 1 @indexed)))))))

(defdescribe
  grep-large-file-and-deadline-test
  "The two silent failures left behind when fff became grep's ONLY discovery
   path (issue #63 follow-up).

   fff skips files past its content budget WITHOUT saying so and its default is
   10 MB, so a needle living in a 20 MB log/dump made grep answer \"No file NAME
   or CONTENT matched\" — a false negative, strictly worse than the slow scan the
   fff path replaced. And every other bound on a grep is a COUNT, which says
   nothing about time: a pathological tree could ride to the 120 s native-tool
   kill and return nothing at all."
  (let
    [gt
     (grep-data-fn)

     rg-search
     (private-fn "rg-search")

     search-file-content
     (private-fn "search-file-content")

     ;; the VAR, not its value — `with-redefs-fn` needs the Var itself
     budget-var
     (resolve 'com.blockether.vis.internal.foundation.editing.core/rg-search-budget-ms)]

    (it "the index budget and the grep-call budget are the SAME, and both clear fff's 10 MB default"
        ;; Raising only ONE of the two still reads nothing: the index refuses to
        ;; cache content past its own budget, so the native grep never sees it.
        (expect (= (private-fn "rg-fff-grep-max-file-size") (fff-index-fn "max-content-file-size")))
        (expect (> (long (fff-index-fn "max-content-file-size")) (* 10 1024 1024))))
    (it
      "a needle past fff's 10 MB content default is FOUND, not silently dropped"
      (let
        [_
         (write-temp! "greplarge/small.txt" "ZZBIGNEEDLEZZ small\n")

         rel
         (write-temp! "greplarge/big.txt" "")

         big
         (fs/file rel)

         filler
         (apply str (repeat 63 \x))

         ;; 16 MiB of filler, then the needle on the LAST line: past fff's
         ;; MAX_FFFILE_SIZE and past the end of any prefix read.
         _
         (with-open [w (java.io.BufferedWriter. (java.io.FileWriter. ^java.io.File big))]
           (dotimes [_ 262144]
             (.write w ^String filler)
             (.write w "\n"))
           (.write w "before-the-needle\n")
           (.write w "ZZBIGNEEDLEZZ big\n"))

         _
         ((fff-index-fn "note-fs-write!"))

         dir
         (temp-dir-path "greplarge")]

        (try (let
               [hits
                (:hits (rg-search {"query" "ZZBIGNEEDLEZZ" "paths" [dir] "context" 1 "limit" 50}))

                by-name
                (into {}
                      (map (fn [h]
                             [(last (string/split (:path h) #"/")) h]))
                      hits)]

               (expect (> (long (fs/size big)) (* 10 1024 1024)))
               (expect (contains? by-name "small.txt"))
               (expect (contains? by-name "big.txt"))
               ;; streamed, so the bounded ring still holds the preceding line for
               ;; a hit that lands 16 MB into the file
               (expect (= ["before-the-needle"] (mapv second (:before (get by-name "big.txt"))))))
             (finally (fs/delete-if-exists big)))))
    (it "a scan that runs out of WALL-CLOCK time says so instead of end-of-results"
        (let
          [_
           (doseq [i (range 1 6)]
             (write-temp! (str "grepdeadline/f" i ".clj") ";; ZZDEADLINEZZ\n"))

           dir
           (temp-dir-path "grepdeadline")]

          (with-redefs-fn {budget-var 0}
            (fn []
              (let
                [content
                 (rg-search {"query" "ZZDEADLINEZZ" "paths" [dir] "limit" 50})

                 files-only
                 (rg-search {"query" "ZZDEADLINEZZ" "paths" [dir] "limit" 50 "is_files_only" true})]

                ;; PARTIAL, and LABELLED partial: without `:time` these read as
                ;; `end-of-results` and a slice passes as the whole tree.
                (expect (= :time (:truncated-by content)))
                (expect (pos? (count (:hits content))))
                (expect (< (count (:hits content)) 5))
                (expect (false? (:total-file-count-exact? content)))
                (expect (= :time (:truncated-by files-only)))
                (expect (pos? (count (:files files-only))))
                (expect (< (count (:files files-only)) 5)))))))
    (it "grep surfaces the time cap as hits_truncated_by plus a NARROWING hint"
        (let
          [_
           (doseq [i (range 1 4)]
             (write-temp! (str "grepdeadline2/f" i ".clj") ";; ZZDEADLINE2ZZ\n"))

           dir
           (temp-dir-path "grepdeadline2")

           complete
           (:result (gt {"query" "ZZDEADLINE2ZZ" "paths" [dir]}))]

          ;; control: an unhurried sweep is complete and says nothing about time
          (expect (nil? (get complete "hits_truncated_by")))
          (expect (not (string/includes? (str (get complete "hint")) "PARTIAL")))
          (with-redefs-fn {budget-var 0}
            (fn []
              (let [out (:result (gt {"query" "ZZDEADLINE2ZZ" "paths" [dir]}))]
                (expect (= "time" (get out "hits_truncated_by")))
                (expect (string/includes? (str (get out "hint")) "PARTIAL"))
                (expect (string/includes? (str (get out "hint")) "Narrow"))
                (expect (false? (get out "total_file_count_is_exact"))))))))
    (it "streaming a file keeps the SAME context windows the slurping walk produced"
        (let
          [rel
           (write-temp! "grepstream/ctx.txt"
                        (string/join "\n"
                                     (map #(if (#{2 3 9} %) (str "L" % " ZZSTREAMZZ") (str "L" %))
                                          (range 1 11))))

           hits
           (search-file-content (fs/file rel) #(string/includes? % "ZZSTREAMZZ") 2 2)]

          ;; hits stay in LINE order even though a hit is held back until its
          ;; :after window fills, and a window clipped by BOF/EOF ships short
          (expect (= [[2 [1] [3 4]] [3 [1 2] [4 5]] [9 [7 8] [10]]]
                     (mapv (fn [h]
                             [(:line h) (mapv first (:before h)) (mapv first (:after h))])
                           hits)))
          (expect (= ["L2 ZZSTREAMZZ" "L3 ZZSTREAMZZ" "L9 ZZSTREAMZZ"] (mapv :text hits)))))
    (it "zero context asks for no windows at all"
        (let
          [rel
           (write-temp! "grepstream/plain.txt" "a\nZZPLAINZZ\nb\n")

           hits
           (search-file-content (fs/file rel) #(string/includes? % "ZZPLAINZZ") 0 0)]

          (expect (= 1 (count hits)))
          (expect (= 2 (:line (first hits))))
          (expect (not (contains? (first hits) :before)))
          (expect (not (contains? (first hits) :after)))))
    (it "an unreadable file yields no hits instead of blowing up the sweep"
        (expect (= []
                   (search-file-content (fs/file (str (temp-dir-path "grepstream")
                                                      "/does-not-exist.txt"))
                                        (constantly true)
                                        0
                                        0))))))

(defdescribe
  draft-isolated-root-enforcement-test
  (it
    "jail DISABLED: a drafted session's trunk path still remaps into the clone (regression: `/` accepted it verbatim and wrote into the real tree)"
    (let
      [safe-path
       (private-fn "safe-path")

       trunk
       (mk-tmp-dir "vis-iso-trunk")

       clone
       (mk-tmp-dir "vis-iso-clone")]

      (spit (java.io.File. ^String trunk "a.txt") "TRUNK ORIGINAL")
      (spit (java.io.File. ^String clone "a.txt") "IN CLONE")
      (binding
        [workspace/*workspace-root*
         clone

         ;; Exactly what `env-filesystem-roots` binds with the OS jail OFF: the
         ;; session's own trunk↔clone pair plus a host root that matches EVERY
         ;; absolute path.
         workspace/*filesystem-roots*
         [{:trunk trunk :clone clone :draft :copy-and-apply :primary? true}
          {:trunk "/" :clone "/" :draft :shared :no-search? true}]]

        (let [f (safe-path (str trunk "/a.txt"))]
          (expect (string/starts-with? (.getCanonicalPath ^java.io.File f) clone))
          (expect (= "IN CLONE" (slurp f)))))))
  (it "a root the draft policy WITHHOLDS is refused outright, even with a host root granted"
      (let
        [safe-path
         (private-fn "safe-path")

         clone
         (mk-tmp-dir "vis-iso-clone2")

         secret
         (mk-tmp-dir "vis-iso-secret")]

        (spit (java.io.File. ^String secret "c.txt") "SECRET")
        (binding
          [workspace/*workspace-root*
           clone

           workspace/*filesystem-roots*
           [{:trunk secret :clone secret :draft :not-allowed :denied? true}
            {:trunk "/" :clone "/" :draft :shared :no-search? true}]]

          (expect (= #{secret} (workspace/denied-roots)))
          (expect (not-any? #{secret} (workspace/allowed-roots)))
          (expect (throws? clojure.lang.ExceptionInfo #(safe-path (str secret "/c.txt"))))
          (expect (= :ext.foundation.editing/path-denied
                     (try (safe-path (str secret "/c.txt"))
                          nil
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))))


;; Regression: grep answers ONE anchored TEXT block, but the model-facing prose still
;; described a keyed map — the blank-path refusal told the model to use "the keys under
;; matches", so it kept subscripting a string.
(defdescribe grep-is-described-as-text-test
             "Every model-facing description of grep says TEXT, never a map."
             (it "the grep symbol contract promises text and names no result keys"
                 (let
                   [result
                    (:ext.symbol/result editing/grep-symbol)

                    description
                    (:ext.symbol/description editing/grep-symbol)]

                   (expect (string/includes? result "Text, not a map"))
                   (expect (not (string/includes? result "hit_count")))
                   (expect (not (string/includes? description "hit_count")))))
             (it "the blank-path refusal no longer says grep returns a map"
                 (let
                   [safe-path
                    (private-fn "safe-path")

                    message
                    (try (safe-path "") nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]

                   (expect (string/includes? message "anchored TEXT"))
                   (expect (not (string/includes? message "returns a MAP"))))))
