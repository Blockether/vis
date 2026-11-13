(ns com.blockether.vis.internal.foundation.editing.core-test
  "Tests for the editing extension.

   Smoke-checks the loaded extension surface (symbol vector, doc
   strings, prompt fragment) plus behavioral coverage of the
   search verb (`grep`) and the listing verb (`ls`). Most tests invoke host functions directly; fixtures live under
   `target/editing-test/`, inside the workspace roots accepted by `safe-path`."
  (:require [com.blockether.vis.test-python-context :as tpc]
            [babashka.fs :as fs]
            [clojure.set]
            [clojure.string :as string]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.foundation.editing.core :as editing]
            [com.blockether.fff :as fff]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.workspace.fff-index :as fff-index]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.extension.core :as extension]
            [lazytest.core :refer
             [around-each defdescribe describe expect it set-ns-context! throws?]]))

;; Direct editing calls need the registration normally supplied by the manifest.
(set-ns-context! [(around-each [f]
                               (let [registered? (some #(= "foundation-core" (:ext/name %))
                                                       (extension/registered-extensions))]
                                 (when-not registered? (foundation/register!))
                                 (try (f)
                                      (finally (when-not registered?
                                                 (extension/deregister-extension!
                                                   "foundation-core"))))))])

(defn- private-var
  "The Var behind one of `editing.core`'s private tool fns — `with-redefs-fn` needs
   the Var, `private-fn` derefs it."
  [name]
  (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" name)))

(defn- private-fn [name] (deref (private-var name)))

(defn- grep-data-fn
  "grep answers in numbered TEXT now — `grep-tool` IS `render-grep-text` over
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
  (deref (resolve (symbol "com.blockether.vis.internal.workspace.fff-index" name))))

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
    ((fff-index-fn "note-fs-write!") (fs/file rel))
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
  (let [coerce
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
        (let [spec (coerce {"query" ["x"]
                            "path" "src"
                            "glob" "*.clj"
                            "excludes" ["t/**"]
                            "is_counts" true
                            "limit" 5})]
          ;; removed aliases don't set :paths/:include; canonical defaults win
          (expect (= ["."] (:paths spec)))
          (expect (= [] (:include spec)))))
    (it "missing query throws `grep needs query`"
        (let [err (try (coerce {"paths" ["."]}) nil (catch clojure.lang.ExceptionInfo e e))]
          (expect (some? err))
          (expect (clojure.string/includes? (ex-message err) "grep needs"))))
    ;; Regression, session 42f1f324-3b0e-4f4d-abb5-4e93343c44ac: spec errors named the
    ;; internal `rg` engine, not the `grep` tool that the agent called.
    (it "spec errors name the grep tool, never rg"
        (doseq [spec [[] {"query" [42]} {"query" [" "]} {"query" ["x"] "context" -1}]]
          (let [message (try (coerce spec) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
            (expect (clojure.string/starts-with? message "grep "))
            (expect (not (re-find #"\brg\b" message))))))
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
        (let [_
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
        (let [_
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
        (let [_
              (write-temp! "rgstar/a.clj" "(def ^:dynamic *workspace-root* \"/x\")\n")

              out
              (grep {"query" ["*workspace-root*"] "paths" [(temp-dir-path "rgstar")]})]

          (expect (= 1 (count (:hits out))))
          (expect (= "(def ^:dynamic *workspace-root* \"/x\")" (:text (first (:hits out)))))))
    (it ":is_files_only returns distinct :files, never :hits"
        (let [_
              (write-temp! "rgsimplefo/a.py" "alpha\nalpha\n")

              _
              (write-temp! "rgsimplefo/b.py" "alpha\n")

              _
              (write-temp! "rgsimplefo/c.py" "no match\n")

              out
              (grep
                {"query" ["alpha"] "paths" [(temp-dir-path "rgsimplefo")] "is_files_only" true})]

          (expect (contains? out :files))
          (expect (not (contains? out :hits)))
          (expect (= 2 (count (:files out))))))))

(defdescribe
  grep-is-regex-test
  ;; `is_regex` makes CONTENT matching a REGULAR EXPRESSION instead of a literal
  ;; smart-case substring: every term is a pattern, a list is still OR, the
  ;; comma-splitting and trimming are off, the fuzzy NAME axis is off, a pattern
  ;; broken only by a stray literal is repaired, and one that cannot RUN is
  ;; refused rather than answered as zero hits.
  (let [coerce-rg
        (private-fn "coerce-rg-spec")

        coerce-find
        (private-fn "coerce-find-spec")

        matcher
        (private-fn "make-line-matcher")

        rg
        (private-fn "rg-search")

        repair
        (private-fn "repair-regex-needle")

        grep
        (grep-data-fn)]

    (it "coerce-rg-spec carries :is_regex and stops splitting/trimming the pattern"
        (expect (false? (:is_regex (coerce-rg {"query" ["a"]}))))
        (expect (true? (:is_regex (coerce-rg {"query" ["a"] "is_regex" true}))))
        ;; a LITERAL query is comma-split into OR terms; a PATTERN must not be —
        ;; that cuts `a{1,3}` in half and changes what it matches.
        (expect (= ["a{1,3}"] (:needles (coerce-rg {"query" "a{1,3}" "is_regex" true}))))
        (expect (= ["a{1" "3}"] (:needles (coerce-rg {"query" "a{1,3}"})))))
    (it "a pattern broken only by a STRAY LITERAL is repaired, not refused"
        (let [spec (coerce-rg {"query" "foo(" "is_regex" true})]
          (expect (= ["foo\\("] (:needles spec)))
          (expect (= [{:needle "foo(" :pattern "foo\\(" :escaped ["("] :error "Unclosed group"}]
                     (:regex-repairs spec))))
        ;; a pattern that compiles is never touched
        (expect (= [] (:regex-repairs (coerce-rg {"query" "defn-? +grep" "is_regex" true}))))
        ;; the same text is a perfectly good LITERAL search and needs no repair
        (expect (= ["foo("] (:needles (coerce-rg {"query" "foo("}))))
        (expect (= [] (:regex-repairs (coerce-rg {"query" "foo("})))))
    (it "a pattern that does not COMPILE for any other reason is refused, never answered as 0 hits"
        (doseq [pattern ["a{3,1}" "\\p{Bogus}" "(?<name" "\\y"]]
          (let [err (try (coerce-rg {"query" pattern "is_regex" true})
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
            (expect (some? err))
            (expect (string/includes? (ex-message err) "does not compile"))
            (expect (= pattern (:pattern (ex-data err)))))))
    (it "repairs the patterns models really send into the code they meant"
        ;; Collected from grep calls in real sessions that failed to compile.
        (doseq [[needle pattern line]
                [["(defn-? [a-z-]*(trailer|tool-message)[a-z-]*"
                  "\\(defn-? [a-z-]*(trailer|tool-message)[a-z-]*" "(defn- tool-message-text [m]"]
                 ["(is (thrown" "\\(is \\(thrown" "(is (thrown? Exception (f)))"]
                 ["^});" "^}\\);" "});"] ["tui)" "tui\\)" "(:require [vis.tui)"]
                 ["/v1/sessions/${id}" "/v1/sessions/\\$\\{id}" "fetch(`/v1/sessions/${id}`)"]
                 ["#{\"def\"" "#\\{\"def\"" "(#{\"def\" \"defn\"} head)"]
                 ["*workspace-root*" "\\*workspace-root*" "(binding [*workspace-root* dir]"]
                 ["[:session" "\\[:session" "(get-in state [:session :id])"]
                 ["\\Q(\\E(x" "\\Q(\\E\\(x" "((x"] ["path\\" "path\\\\" "C:\\path\\"]]]
          (let [repaired (repair needle)]
            (expect (= pattern (:pattern repaired)))
            (expect (some? (re-find (re-pattern pattern) line)))))
        (expect (nil? (repair "[]a]")))
        (expect (nil? (repair "(defn-? +grep)"))))
    (it "accepts is_regex and retains it in structured unknown-key diagnostics"
        (expect (true? (:is_regex (coerce-find [{"query" "x" "is_regex" true}]))))
        (expect (false? (:is_regex (coerce-find [{"query" "x"}]))))
        (let [err (try (coerce-find [{"query" "x" "is_rgex" true}])
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
          (expect (= "grep: unknown keys: is_rgex. See doc(\"grep\")." (ex-message err)))
          (expect (some #{"is_regex"} (:allowed (ex-data err))))))
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
        (let [_
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
    (it "a pattern the native scanner cannot narrow is ANSWERED, not refused"
        ;; Java compiles lookbehind; the native candidate scanner's Rust regex
        ;; does not, and would fall back to a LITERAL scan whose candidate set
        ;; misses every real hit — a false negative dressed up as an answer.
        ;; Refusing it made a directory scope answer differently from a file
        ;; scope for a pattern that works, so instead the candidate set widens to
        ;; the whole ignore-aware scope and the JVM matcher decides.
        (let [_
              (write-temp! "rgregexlook/a.txt" "foobar\n")

              out
              (rg {"query" "(?<=foo)bar" "paths" [(temp-dir-path "rgregexlook")] "is_regex" true})]

          (expect (= 1 (count (:hits out))))
          (expect (= "foobar" (:text (first (:hits out)))))
          (expect (true? (:prefilter-degraded? out)))))
    (it "grep answers regex CONTENT hits and turns the fuzzy NAME axis off"
        (let [d
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
    (it "a regex broken by a stray literal runs repaired, and the hint shows the fix"
        (let [d
              (temp-dir-path "grepregexrepair")

              _
              (write-temp! "grepregexrepair/a.clj"
                           "(defn- tool-message-text [m] m)\n(defn other [] 1)\n")

              out
              (:result (grep {"query" "(defn-? [a-z-]*(trailer|tool-message)[a-z-]*"
                              "paths" [d]
                              "is_regex" true}))]

          (expect (= 1 (get out "hit_count")))))
    (it "a regex-looking LITERAL query that misses is rescued as a regex"
        (let [d
              (temp-dir-path "grepregexrescue")

              _
              (write-temp! "grepregexrescue/a.clj" "(ns demo)\n(defn needle [] :ok)\n")

              out
              (:result (grep {"query" "^(defn needle" "paths" [d]}))]

          (expect (= 1 (get out "hit_count")))
          ;; a later page belongs to the LITERAL search: it is never rescued
          (expect (zero? (long (get (:result (grep
                                               {"query" "^(defn needle" "paths" [d] "offset" 1}))
                                    "hit_count"))))))
    (it "a pattern that matches EMPTY text is never a rescue"
        (let [d
              (temp-dir-path "grepregexempty")

              _
              (write-temp! "grepregexempty/a.txt" "alpha\nbeta\n")

              out
              (:result (grep {"query" "ZZABSENT|" "paths" [d]}))]

          (expect (zero? (long (get out "hit_count"))))))))

(defdescribe
  cwd-safety-test
  ;; THE non-negotiable invariant: every v/* tool that touches the
  ;; filesystem must refuse any path that escapes (workspace/cwd).
  ;; safe-path is the single gate; this suite proves every mutation
  ;; tool actually routes through it.
  (let [escape-paths ["../escape.txt" "../../etc/passwd" "/etc/passwd" "target/../../escape.txt"]]
    (it "a READ path outside cwd is refused at the same gate"
        ;; Defense in depth: even reads can't leak through path traversal.
        (let [safe-path (private-fn "safe-path")]
          (doseq [p escape-paths]
            (let [err (try (safe-path p) nil (catch clojure.lang.ExceptionInfo e e))]
              (expect (some? err))
              (expect (= :ext.foundation.editing/path-escape (:type (ex-data err))))))))))

(defdescribe editing-extension-loads-test
             (it "editing exposes only the search tool"
                 (expect (= #{'grep}
                            (set (map :ext.symbol/symbol (editing/available-editing-symbols)))))))

(defn- with-fs-gate!
  "Install the host tools' extension-owned `:fs/access` gate for `body`, then
   unregister it even if the test fails."
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
    "grep asks the gate with file-read and refuses with the extension's own sentence"
    (let [seen!
          (atom [])

          hint
          "Use (br/policy) instead of reading this file directly."]

      (with-fs-gate!
        (refusing-gate seen! "protected/secret.edn" hint)
        (fn []
          (let [before
                (:ext.symbol/before-fn (private-fn "grep-symbol"))

                failure
                (:result (before {:extensions (atom [])}
                                 (constantly :ok)
                                 [{"query" "x"
                                   "paths" ["target/editing-test/protected/secret.edn"]}]))]

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
  (it "a configured deny rule refuses grep with no extension gate registered (#263)"
      (let [rule
            (str (.getCanonicalPath ^java.io.File (fs/file "target/editing-test")) "/**/.env")

            env
            {:extensions (atom []) :security-policy {:process-jail {:deny-read-rules [rule]}}}

            before
            (:ext.symbol/before-fn (private-fn "grep-symbol"))

            failure
            (:result (before env
                             (constantly :ok)
                             [{"query" "x" "paths" ["target/editing-test/service/.env"]}]))]

        (expect (some? failure))
        (expect (= :ext.foundation.editing/path-protected
                   (-> failure
                       :error
                       :type)))
        (expect (= :policy
                   (-> failure
                       :error
                       :owner)))
        (expect (clojure.string/includes? (-> failure
                                              :error
                                              :hint)
                                          "jail.filesystem.deny_read"))
        ;; One rule closes one thing: the rest of the workspace still reads.
        (expect (nil? (:result (before env
                                       (constantly :ok)
                                       [{"query" "x" "paths" ["target/editing-test/a.clj"]}]))))))
  (it "a gate hook that THROWS refuses: a boundary that fails open is not a boundary"
      (with-fs-gate! (fn [_env _op _ctx]
                       (throw (ex-info "guard exploded" {})))
                     (fn []
                       (let [before
                             (:ext.symbol/before-fn (private-fn "grep-symbol"))

                             out
                             (before {:extensions (atom [])}
                                     (constantly :ok)
                                     [{"query" "x" "paths" ["target/editing-test/a.clj"]}])]

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
  (it "a gate that reads a file does not recurse: the nested ask is skipped"
      (let [depth (atom 0)]
        (with-fs-gate! (fn [env op ctx]
                         (swap! depth inc)
                         ;; What a guard that reads a file in order to decide looks like from
                         ;; in here: the nested operation re-enters the gate.
                         (extension/run-gate-hooks op env ctx))
                       (fn []
                         (let [before (:ext.symbol/before-fn (private-fn "grep-symbol"))
                               out (before {:extensions (atom [])}
                                           (constantly :ok)
                                           [{"query" "x" "paths" ["target/editing-test/a.clj"]}])]

                           (expect (not (contains? out :result)))
                           (expect (= 1 @depth))))))))

(defdescribe
  vis-ls-test
  ;; `ls` is its OWN surface, never a hidden mode of `cat`: `ls` lists
  ;; directories, `cat` reads files, and each refuses the other's input while
  ;; naming the replacement call. Default hides dotfiles + gitignored paths;
  ;; opts widen the view; depth nests children.
  (it "(ls dir) returns a shallow directory listing envelope"
      (let [_
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
      (let [_
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
    (let [_
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
      ;; hidden and gitignore are independent axes: is_hidden never lists
      ;; gitignored entries, and is_ignored never lists dotfiles
      (expect (not (contains? (names {"is_hidden" true}) "ignored.txt")))
      (expect (not (contains? (names {}) "ignored.txt")))
      (expect (= #{"a.txt" "ignored.txt"} (names {"is_ignored" true})))
      (expect (= #{"a.txt" "ignored.txt" ".hidden" ".gitignore"}
                 (names {"is_hidden" true "is_ignored" true})))))
  (it "(ls dir {\"depth\" 2}) nests a children vector under subdirs"
      (let [_
            (write-temp! "lsdepth/sub/b.txt" "y")

            dir
            (temp-dir-path "lsdepth")

            out
            (first (ls-rows {"paths" [dir] "depth" 2}))

            sub
            (some #(when (= "sub" (get % "name")) %) (get out "entries"))]

        (expect (= 2 (get out "depth")))
        (expect (= ["b.txt"] (mapv #(get % "name") (get sub "children"))))))
  (it "ls refuses a file and directs the caller to open"
      (let [_
            (write-temp! "lsrefuse/b.txt" "x")

            file
            (str (temp-dir-path "lsrefuse") "/b.txt")

            err
            (try (ls-rows {"paths" [file]}) nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :ext.foundation.editing/ls-on-file (:type (ex-data err))))
        (expect (string/includes? (ex-message err) "is a file; read it with `open`."))))
  ;; Regression, issue #267: one file in a batch of confirmed directories aborts the
  ;; whole call instead of listing the rest — `ls` maps directories, Python reads files.
  (it "ls refuses a batch that mixes a directory with a file"
      (let [_
            (write-temp! "lsmixed/sub/b.txt" "x")

            dir
            (temp-dir-path "lsmixed")

            file
            (str (temp-dir-path "lsmixed") "/sub/b.txt")

            err
            (try (ls-rows {"paths" [dir file]}) nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :ext.foundation.editing/ls-on-file (:type (ex-data err))))
        (expect (string/includes? (ex-message err) "b.txt` is a file; read it with `open`."))))
  ;; Regression, issue #126: a missing path must name a real parent to list.
  ;; Namespace-to-path guidance belongs in the prompt, not in every error.
  (it "ls names the nearest existing directory for a path that does not exist"
      (let [_
            (write-temp! "lsnear/real/keep.txt" "x")

            missing
            (str (temp-dir-path "lsnear") "/real/com/blockether/nope")

            err
            (try (ls-rows {"paths" [missing]}) nil (catch clojure.lang.ExceptionInfo e e))]

        (expect (= :ext.foundation.editing/ls-missing-path (:type (ex-data err))))
        (expect (string/includes? (ex-message err) "no such directory"))
        (expect (string/includes? (ex-message err)
                                  (str "list `" (:nearest (ex-data err)) "` first.")))
        (expect (string/ends-with? (:nearest (ex-data err)) "lsnear/real")))))

(defdescribe
  vis-rg-structured-shape-test
  (it "returns the content shape: :hits :truncated-by + breadth counts"
      (let [_
            (write-temp! "rg/a.txt" "alpha needle gamma\nbeta\n")

            _
            (write-temp! "rg/b.txt" "plain line\nanother needle here\n")

            grep
            (private-fn "rg-search")

            out
            (grep {"all" ["needle"] "paths" [(temp-dir-path "rg")]})]

        (expect (= #{:hits :truncated-by :total-file-count :total-file-count-exact?
                     :prefilter-degraded?}
                   (set (keys out))))
        ;; both files match — breadth == displayed file count, fully counted.
        (expect (= 2 (:total-file-count out)))
        (expect (true? (:total-file-count-exact? out)))
        (expect (vector? (:hits out)))
        ;; Omitted context defaults to three, including short boundary windows.
        (expect (every? #(= #{:path :line :text :before :after} (set (keys %))) (:hits out)))
        (expect (= 2 (count (:hits out))))
        (expect (= :end-of-results (:truncated-by out)))))
  (it "query strings are literal, including pipe characters"
      (let [_
            (write-temp! "rgliteral/a.clj" "foo|bar\nfoo only\nbar only\n")

            grep
            (private-fn "rg-search")

            out
            (grep {"all" ["foo|bar"] "paths" [(temp-dir-path "rgliteral")] "include" ["*.clj"]})]

        (expect (= ["foo|bar"] (mapv :text (:hits out))))))
  (it "spec {:all [...]} is an OR alias for :query (same-line AND was removed)"
      (let [_
            (write-temp! "rgall/a.clj"
                         "(defn info-event [x] x)\n(defn other [x] x)\ninfo-event call\n")

            grep
            (private-fn "rg-search")

            out
            (grep
              {"all" ["defn" "info-event"] "paths" [(temp-dir-path "rgall")] "include" ["*.clj"]})]

        ;; OR: every line mentioning EITHER term is a hit.
        (expect (= ["(defn info-event [x] x)" "(defn other [x] x)" "info-event call"]
                   (mapv :text (:hits out))))))
  (it
    "accepts path vectors, include globs, and dedups overlapping roots"
    (let [root
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
    (let [_
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
    (let [grep
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
      (let [_
            (write-temp! "rgposopts/a.clj" "needle here\n")

            env
            (rg {"query" "needle" "paths" [(temp-dir-path "rgposopts")] "include" ["*.clj"]})]

        (expect (= :grep (:symbol env)))
        (expect (= 1 (get (:result env) "hit_count"))))
      ;; UNKNOWN keys are now IGNORED, not fatal — a model that tosses in a stray
      ;; annotation (e.g. `all_note: "defs"`, or an invented `type`/`spec`) still
      ;; gets its search instead of wasting the whole turn. Only recognised keys
      ;; are read; the rest are dropped.
      (let [_
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
      (let [err (try (grep {"anyy" ["needle"] "paths" ["."]})
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? err))
        (expect (= :ext.foundation.editing/invalid-rg-spec (:type (ex-data err)))))))
  (it ":truncated-by :limit when results exceed the configured limit (default 50)"
      ;; The rg sweep ships 50 elements by default. Use 300 hits to force the cap.
      (let [_
            (write-temp! "rgcap/a.txt" (string/join "\n" (map #(str "needle " %) (range 300))))

            grep
            (private-fn "rg-search")

            out
            (grep {"all" ["needle"] "paths" [(temp-dir-path "rgcap")]})]

        (expect (= 50 (count (:hits out))))
        (expect (= :limit (:truncated-by out)))))
  (it "empty result still has :truncated-by :end-of-results, never nil"
      (let [_
            (write-temp! "rgmiss/a.txt" "nothing matches in here\n")

            grep
            (private-fn "rg-search")

            out
            (grep {"all" ["definitely-not-present"] "paths" [(temp-dir-path "rgmiss")]})]

        (expect (= [] (:hits out)))
        (expect (= :end-of-results (:truncated-by out)))))
  ;; Q1+Q2+Q3+Q4 — new option coverage.
  (it ":context N adds N symmetric context lines around each hit"
      (let [_path
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
      (let [_
            (write-temp! "rgfo/src/a.py" "alpha\nalpha\nalpha\n")

            _
            (write-temp! "rgfo/src/b.py" "alpha\n")

            _
            (write-temp! "rgfo/src/c.py" "no match\n")

            grep
            (private-fn "rg-search")

            out
            (grep {"all" ["alpha"] "paths" [(temp-dir-path "rgfo")] "is_files_only" true})]

        (expect (= #{:files :file-counts :truncated-by :total-file-count :total-file-count-exact?
                     :prefilter-degraded?}
                   (set (keys out))))
        (expect (= 2 (:total-file-count out)))
        (expect (true? (:total-file-count-exact? out)))
        (expect (= 2 (count (:files out))))
        (expect (every? string? (:files out)))))
  (it ":context is IGNORED (not rejected) in :is_files_only mode"
      ;; A stray `context` alongside `is_files_only` is harmless — content-mode
      ;; context has no meaning when returning bare file paths, so honor files-only
      ;; instead of hard-failing the whole call.
      (let [grep
            (private-fn "rg-search")

            out
            (grep
              {"any" ["alpha"] "paths" [(temp-dir-path "rgfo")] "is_files_only" true "context" 2})]

        (expect (= #{:files :file-counts :truncated-by :total-file-count :total-file-count-exact?
                     :prefilter-degraded?}
                   (set (keys out))))
        (expect (every? string? (:files out)))))
  (it "keeps a long hit line FULL in the result value (no per-line mutilation)"
      ;; rg never mutilates a hit line. The full :text lives in the result value —
      ;; pickled into `r[\"tN/iN/fN\"]` and rebound into the sandbox — so the model
      ;; recovers the tail with `r[...][\"hits\"][i][\"text\"][N:]` in Python, no `cat`
      ;; roundtrip. Only the WIRE view is bounded (64KB per-observation clip), and
      ;; that clip is non-destructive (it points back to r[...]).
      (let [huge
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
  grep-returns-numbered-text-test
  (it "line 1 summarizes, each file header names its path and each row its line"
      (let [d
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
        (expect (some #(re-matches #".*greptext/one\.txt  \(1\)" %) lines))
        (expect (some #(re-matches #"\d+│ .*" %) lines))))
  (it "context lines are numbered too"
      (let [d
            (temp-dir-path "greptextctx")

            _
            (write-temp! "greptextctx/two.txt" "one\ntwo\nZZCTXZZ\nfour\nfive\n")

            grep-tool
            (private-fn "grep-tool")

            rows
            (->> (:result (grep-tool {"query" "ZZCTXZZ" "paths" [d] "context" 1}))
                 string/split-lines
                 (filter #(re-matches #"\d+│ .*" %)))]

        (expect (= 3 (count rows)))
        (expect (every? #(re-matches #"\d+│ .*" %) rows))))
  (it "keeps phrase boundaries in a multi-term search"
      (let [render (private-fn "render-grep-text")
            head (first (string/split-lines
                          (render {"query" ["alpha beta" "gamma delta"]
                                   "matches" {} "paths" [] "hit_count" 0 "file_count" 0})))]
        (expect (string/starts-with? head "grep 'alpha beta' | 'gamma delta'  "))))
  (it "shows one context line on each side unless more are requested"
      (let [d (temp-dir-path "grep-default-context")
            _ (write-temp! "grep-default-context/a.txt" "a\nb\nZZDEFAULTZZ\nd\ne\n")
            grep (grep-data-fn)
            spec {"query" "ZZDEFAULTZZ" "paths" [d]}
            tuples (fn [out]
                     ((private-fn "grep-hit-tuples")
                       (first (vals (get (:result out) "matches")))))]
        (expect (= [2 3 4] (mapv first (tuples (grep spec)))))
        (expect (= [1 2 3 4 5] (mapv first (tuples (grep (assoc spec "context" 2))))))))
  (it "clips default context safely at a one-line file"
      (let [d
            (temp-dir-path "greptextone")

            _
            (write-temp! "greptextone/only.clj" "ZZONEZZ\n")

            out
            (:result ((grep-data-fn) {"query" "ZZONEZZ" "paths" [d]}))

            hit
            (-> (get out "matches")
                vals
                first
                vals
                first)

            rows
            (->> ((private-fn "render-grep-text") out)
                 string/split-lines
                 (filter #(re-matches #"\d+│ .*" %)))]

        (expect (= [] (get hit "before")))
        (expect (= [] (get hit "after")))
        (expect (= 1 (count rows)))
        (expect (= "1│ ZZONEZZ" (first rows)))))
  (it "a capped sweep gives the cap and the next offset on line 1, breadth included"
      (let [render
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

        ;; A capped page also names the answer that is NOT a page: without it, the
        ;; caller who wants "which files, how many times" rewrites the search as a
        ;; hand-rolled walk instead of asking for files-only.
        ;; The Python runtime reads `next(r)` from the `"offset": N` field of this line.
        (expect (= (str "grep 'defdescribe'  50 hits · 11 of 136 files  capped by limit, next"
                        " \"offset\": 50; 125 more files match — add \"is_files_only\": True for"
                        " every file with its hit count")
                   head))))
  (it "a page capped only on file NAMES does not read as a capped content sweep"
      ;; Issue #438: `0 hits · 0 files  capped by limit` read as a content cap, and
      ;; its offset 50 was the NAME page, not the 500-file files-only page.
      (let [head (first (string/split-lines ((private-fn "render-grep-text")
                                              {"query" "zzq_vis_probe"
                                               "matches" {}
                                               "paths" ["a.clj"]
                                               "hit_count" 0
                                               "file_count" 0
                                               "truncated_by" "limit"
                                               "next_offset" 50})))]
        (expect (= (str "grep 'zzq_vis_probe'  0 hits · 0 files  file NAME matches capped by limit,"
                        " next \"offset\": 50")
                   head)))))

(defn- mk-tmp-dir
  [prefix]
  (.getCanonicalPath (.toFile (java.nio.file.Files/createTempDirectory
                                prefix
                                (make-array java.nio.file.attribute.FileAttribute 0)))))

(defdescribe
  multi-root-safe-path-test
  (it
    "accepts paths under a LIVE filesystem root (trunk==clone), rejects paths outside every root"
    (let [safe-path
          (private-fn "safe-path")

          primary
          (.getCanonicalPath (java.io.File. (System/getProperty "user.dir")))

          ctx-root
          (mk-tmp-dir "vis-ctxroot")]

      (binding [workspace/*workspace-root*
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
  (it
    "expands a leading ~ / ~/ so a home-relative path resolves to the real file (regression: was treated as a literal ~ segment under cwd)"
    (let [safe-path
          (private-fn "safe-path")

          home
          (.getCanonicalPath (java.io.File. (System/getProperty "user.home")))]

      (binding [workspace/*workspace-root*
                home

                workspace/*filesystem-roots*
                nil]

        ;; ~/x and <home>/x must resolve to the SAME real path, NOT <home>/~/x
        (expect (= (.getCanonicalPath ^java.io.File (safe-path (str home "/some-file.txt")))
                   (.getCanonicalPath ^java.io.File (safe-path "~/some-file.txt"))))
        (expect (not (string/includes? (.getPath ^java.io.File (safe-path "~/some-file.txt")) "~")))
        ;; bare ~ resolves to home itself
        (expect (= home (.getCanonicalPath ^java.io.File (safe-path "~")))))))
  (it "ISOLATED filesystem root: address by trunk → edits land in clone, display shows trunk"
      (let [safe-path
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
        (binding [workspace/*workspace-root*
                  primary

                  workspace/*filesystem-roots*
                  [{:trunk trunk :clone clone}]]

          (let [f (safe-path (str trunk "/x.txt"))]
            (expect (string/starts-with? (.getCanonicalPath ^java.io.File f) clone)) ;; lands in clone
            (expect (= "in-clone" (slurp f)))                                        ;; reads clone, NOT trunk
            (expect (= (.replace (str trunk "/x.txt") "\\" "/") (rel-path f)))) ;; display shows real trunk path, `/`-normalized
          (expect (throws? clojure.lang.ExceptionInfo #(safe-path "/etc/hosts"))))))
  ;; A file in an added root under home used to show its whole absolute path
  ;; in cat and grep headers. It now shows `~/…`, and that address round-trips.
  (it
    "a file under home but outside the primary cwd renders as ~/… and resolves back"
    (let [safe-path
          (private-fn "safe-path")

          rel-path
          (private-fn "rel-path")

          home
          (mk-tmp-dir "vis-home")

          primary
          (str home "/primary")

          sibling
          (str home "/sibling")

          prior
          (System/getProperty "user.home")]

      (.mkdirs (java.io.File. ^String primary))
      (.mkdirs (java.io.File. ^String sibling))
      (spit (java.io.File. ^String sibling "x.txt") "in-sibling")
      (try (System/setProperty "user.home" home)
           (binding [workspace/*workspace-root*
                     primary

                     workspace/*filesystem-roots*
                     [{:trunk sibling :clone sibling :draft :shared}]]

             (let [f
                   (safe-path (str sibling "/x.txt"))

                   shown
                   (rel-path f)]

               (expect (= "~/sibling/x.txt" shown))
               (expect (= (.getCanonicalPath ^java.io.File f)
                          (.getCanonicalPath ^java.io.File (safe-path shown))))))
           (finally (System/setProperty "user.home" prior))))))

;; ── e2e: REAL tool invocations against REAL temp files ───────────────────────

(defdescribe
  rg-tool-e2e-test
  "The `rg` TOOL over real files: the comma-split + smart-case fixes end-to-end."
  (let [find-tool
        (grep-data-fn)

        rg
        (fn [& a]
          (apply find-tool a))]

    (it "a comma query matches EITHER term (the session 71a69809 fix, real files)"
        (let [d
              (temp-dir-path "rge")

              f
              (str (temp-root) "/rge/a.clj")]

          (spit (fs/file f) "the model line\nthe cycle line\nunrelated\n")
          (let [r (rg {"query" "model, cycle" "paths" [d]})]
            (expect (:success? r))
            (expect (= 2 (get-in r [:result "hit_count"])))))) ;; both lines, not 0
    (it
      "content value is uniform with explicit zero and positive context"
      (let [d
            (temp-dir-path "rguni")

            f
            (str (temp-root) "/rguni/a.clj")]

        (spit (fs/file f) "L1\nMATCH\nL3\n")
        (let [plain
              (get-in (rg {"query" "MATCH" "paths" [d] "context" 0}) [:result "matches"])

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
        (let [d
              (temp-dir-path "rgc")

              f
              (str (temp-root) "/rgc/a.clj")]

          (spit (fs/file f) "Keymap here\nkeystroke too\nnope\n")
          (let [r (rg {"query" "key" "paths" [d]})]
            (expect (= 2 (get-in r [:result "hit_count"])))))) ;; Keymap + keystroke
    ;; Regression, session 42f1f324-3b0e-4f4d-abb5-4e93343c44ac: a missing scope climbed
    ;; to its parent; `/root/lanterna` became `/root`, and the index failed there.
    (it "a MISSING path is skipped: the other paths are searched and the hint lists the nearest directory"
        (let [d
              (temp-dir-path "rgp")

              ghost
              (str (temp-root) "/rgp/nope.edn")

              _
              (write-temp! "rgp/a.clj" "needle here\n")

              r
              (:result (rg {"query" "needle" "paths" [d ghost]}))]

          (expect (= 1 (get r "hit_count")))
          (expect (clojure.string/includes? (get r "hint") "nope.edn` does not exist"))
          (expect (clojure.string/includes? (get r "hint") "rgp/` contains: a.clj"))
          (expect (not (re-find #"(?i)\b(rg|fff)\b" (get r "hint"))))))
    (it
      "a BLANK/nil paths entry means \"everything\" — widens like \".\", never throws (`[\".github\" \"\"]` case)"
      (let [rsr
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
      (let [rsr
            @#'editing/resolve-search-roots

            home
            (System/getProperty "user.home")

            vis-home
            (str home "/.vis")

            primary
            (str home "/proj")

            other
            (str home "/lib")]

        (with-redefs [workspace/allowed-roots
                      (constantly [primary other vis-home])

                      workspace/no-search-roots
                      (constantly #{vis-home})]

          (let [roots (mapv str (:roots (rsr ["."])))]
            ;; ~/.vis pruned from the default sweep …
            (expect (not (some #(clojure.string/starts-with? % vis-home) roots)))
            ;; … while the primary and sibling roots survive, in order
            (expect (= [primary other] roots))))
        ;; the primary is exempt even when catalogued `search: false`: cwd == ~/.vis still scans.
        (with-redefs [workspace/allowed-roots
                      (constantly [vis-home primary])

                      workspace/no-search-roots
                      (constantly #{vis-home})]

          (expect (= [vis-home primary] (mapv str (:roots (rsr ["."]))))))))
    (it
      "a real DRAFT clone under the drafts store (~/.vis/drafts) is KEPT in the default sweep even though it is under ~/.vis — so an in-draft session stays searchable, while the raw ~/.vis grant is still pruned"
      (let [rsr
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
          (with-redefs [workspace/allowed-roots (constantly [draft-primary draft-clone vis-home])
                        workspace/no-search-roots (constantly #{vis-home})]

            (let [roots (mapv str (:roots (rsr ["."])))]
              ;; both draft clones kept because only the raw catalog root opts out …
              (expect (= [draft-primary draft-clone] roots))
              ;; … and the raw ~/.vis grant is gone.
              (expect (not (some #{vis-home} roots))))))))
    ;; Regression, session 64404d5c-8826-482c-9989-da87f1a502c5: `~/blockether.com` was
    ;; catalogued in `workspace.filesystem` eleven minutes before that repo was cloned, so
    ;; the DEFAULT sweep handed a directory that was NOT THERE to `fff-index/lease`, and
    ;; every grep whose scope widened to "." — `grep({"paths": ["README.md"]})` among them,
    ;; because the NAME axis normalizes a workspace-root file to "." — died with
    ;; `rg fff index root must be a directory` instead of searching the roots that existed.
    (it
      "a catalogued root that is NOT an existing directory is SKIPPED by the sweep instead of aborting it"
      (let [real
            (.getCanonicalPath (fs/file (temp-dir-path "rgd-sweep-real")))

            _
            (write-temp! "rgd-sweep-real/present.clj" "(def sweep-phantom-needle :here)\n")

            phantom
            (str real "-never-cloned")

            _
            (when (fs/exists? phantom) (fs/delete-tree phantom))]

        (with-redefs [workspace/allowed-roots
                      (constantly [real phantom])

                      workspace/no-search-roots
                      (constantly #{})]

          (let [out (:result (rg {"query" "sweep-phantom-needle" "paths" ["."]}))]
            (expect (= 1 (get out "hit_count")))
            (expect (= 1 (get out "file_count")))))))
    (it
      "an EXISTING file is searched as that ONE file (precise — never widened to its dir); a MISSING path is skipped"
      (let [dir
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
          (expect (= 1 (get-in r [:result "hit_count"]))))
        ;; a path that does NOT exist is NOT widened to `dir`: no hits, and the
        ;; hint lists `dir` instead of claiming that nothing matched
        (let [r (:result (rg {"query" needle "paths" [(str dir "/gone.clj")]}))]
          (expect (= 0 (get r "hit_count")))
          (expect (= [] (get r "searched_paths")))
          (expect (clojure.string/includes? (get r "hint") "rgd-precise/` contains: a.clj b.clj."))
          (expect (not (clojure.string/includes? (get r "hint") "No file NAME"))))))))

(defdescribe empty-search-paths-default-test
             "grep preserves file and directory scopes; empty scope keeps the default search."
             (let [coerce-find
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
               (it "coerce-find and find-arg-paths both preserve the explicit file scope"
                   (let [dir
                         (temp-dir-path "find-dir-scope")

                         file
                         (str dir "/one.clj")

                         expected-file
                         ((private-fn "rel-path") (fs/file file))

                         spec
                         {"query" "needle" "paths" [file]}]

                     (spit (fs/file file) "needle\n")
                     (expect (= [file] (:paths (coerce-find [spec]))))
                     ;; The gate checks that same file, not its parent directory.
                     (expect (= [expected-file] (find-paths [spec])))))
               (it "accepts an explicit context and defaults both grep paths to one line"
                   (expect (= 2 (:context (coerce-find [{"query" "needle" "context" 2}]))))
                   (expect (= 0 (:context (coerce-find [{"query" "needle" "context" 0}]))))
                   (expect (= 1 (:context (coerce-find [{"query" "needle"}]))))
                   (expect (= 1 (:context (coerce-rg {"query" "needle"}))))
                   (expect (throws? clojure.lang.ExceptionInfo
                                    #(coerce-find [{"query" "needle" "context" -1}])))
                   (expect (throws? clojure.lang.ExceptionInfo
                                    #(coerce-find [{"query" "needle" "context" {"before" 1}}]))))
               (it "rg keeps its own empty-path and file-path semantics"
                   (let [spec {"query" ["FIND_FILES" "CAT"] "paths" []}]
                     (expect (= ["."] (:paths (coerce-rg spec))))))))

(defdescribe
  grep-limit-aliases-test
  (let [coerce-find
        (private-fn "coerce-find-spec")

        grep
        (grep-data-fn)

        caught
        (fn [f & args]
          (try (apply f args) nil (catch clojure.lang.ExceptionInfo e e)))]

    (it "accepts max_results, max_count and max_matches as aliases for the page limit"
        (doseq [key ["limit" "max_results" "max_count" "max_matches"]]
          (expect (= 7 (:limit (coerce-find [{"query" "needle" key 7}]))))))
    (it "requires a positive integer for every alias"
        (doseq [alias
                ["max_results" "max_count" "max_matches"]

                value
                [0 -1 1.5 "3" nil false]]

          (let [e (caught coerce-find [{"query" "needle" alias value}])]
            (expect (some? e))
            (expect (= :ext.foundation.editing/invalid-find-args (:type (ex-data e))))
            (expect (contains? (ex-data e) :limit))
            (expect (= "grep: \"limit\" must be a positive integer." (ex-message e))))))
    (it "refuses two names for the same limit, including pairs of aliases"
        (doseq [[a b] [["limit" "max_results"] ["limit" "max_count"] ["limit" "max_matches"]
                       ["max_results" "max_count"] ["max_results" "max_matches"]
                       ["max_count" "max_matches"]]]
          (let [e (caught coerce-find [{"query" "needle" a 2 b 2}])]
            (expect (some? e))
            (expect (= :ext.foundation.editing/invalid-find-args (:type (ex-data e))))
            (expect (= "grep: use only one key for \"limit\"." (ex-message e))))))
    (it "caps the whole content page and preserves paging with the limit or any alias"
        (write-temp! "limit-alias/a.clj" "needle first\nneedle second\n")
        (write-temp! "limit-alias/b.clj" "needle third\n")
        (doseq [alias ["limit" "max_results" "max_count" "max_matches"]]
          (let [spec {"query" "needle" "paths" [(temp-dir-path "limit-alias")] "context" 0 alias 2}
                first-page (:result (grep spec))
                next-page (:result (grep (assoc spec "offset" (get first-page "next_offset"))))]

            (expect (= 2 (get first-page "limit")))
            (expect (= 2 (get first-page "hit_count")))
            (expect (= 2 (get first-page "next_offset")))
            (expect (= 1 (get next-page "hit_count")))
            (expect (= 2 (get next-page "offset"))))))))

(defdescribe
  grep-limit-aliases-python-test
  ;; Regression: max_matches must behave like the other limit aliases at the Python boundary.
  (it
    "honors all limit names in maps and kwargs, including automatic pagination"
    (write-temp! "limit-alias-python/a.clj" "needle first\nneedle second\n")
    (write-temp! "limit-alias-python/b.clj" "needle third\n")
    (tpc/with-own
      [ctx (extension/builtin-sandbox-bindings (constantly nil))]
      (let
        [result
         (ep/run-python-block
           ctx
           (str
             "for alias in ('limit', 'max_results', 'max_count', 'max_matches'):\n"
             "    spec = {\"query\": \"needle\", \"paths\": [" (pr-str (temp-dir-path
                                                                         "limit-alias-python"))
             "], \"context\": 0, alias: 2}\n"
             "    for page in (await grep(spec), await grep(**spec)):\n"
             "        print(sum(\"│ needle \" in line for line in str(page).splitlines()))\n"
             "        print(sum(\"│ needle \" in line for line in str(next(page)).splitlines()))\n"
             "grep_doc = str(await doc(\"grep\"))\n"
             "print(all(alias in grep_doc for alias in ('max_results', 'max_count', 'max_matches')))")
           "t1/i1")]
        (expect (nil? (:error result)))
        (expect (= (str (apply str (repeat 4 "2\n1\n2\n1\n")) "True\n") (:stdout result)))))))

(defdescribe
  grep-hidden-and-ignored-test
  ;; Regression: grep rejected is_ignored instead of searching ignored files.
  (it
    "keeps hidden and ignored flags independent across search modes"
    (let [dir
          "grep-hidden-ignored"

          files
          ["marker-visible.txt" ".marker-hidden.txt" ".hidden/marker-child.txt"
           "ignored/marker-ignored.txt" "ignored/.marker-both.txt" "target/marker-cache.txt"
           "overlay/marker-overlay.txt"]

          search
          (grep-data-fn)]

      (write-temp! (str dir "/.gitignore") "ignored/\n")
      (write-temp! (str dir "/.rgignore") "overlay/\n")
      (doseq [file files]
        (write-temp! (str dir "/" file) "marker\n"))
      (doseq [hidden?
              [false true]

              ignored?
              [false true]

              query
              ["marker" "" "^marker$"]]

        (let [spec
              {"query" query
               "paths" [(temp-dir-path dir)]
               "is_hidden" hidden?
               "is_ignored" ignored?
               "is_regex" (= query "^marker$")}

              expected
              (cond-> #{"marker-visible.txt"}
                hidden?
                (into #{".marker-hidden.txt" ".hidden/marker-child.txt"})

                ignored?
                (into #{"ignored/marker-ignored.txt" "target/marker-cache.txt"
                        "overlay/marker-overlay.txt"})

                (and hidden? ignored?)
                (conj "ignored/.marker-both.txt"))

              relative
              (fn [paths]
                (set (map #(subs % (inc (count (str (temp-root) "/" dir)))) paths)))

              result
              (:result (search spec))]

          (when-not (= query "^marker$")
            (expect (= (cond-> expected
                         (and hidden? (= query ""))
                         (into #{".gitignore" ".rgignore"}))
                       (relative (get result "paths")))))
          (when-not (= query "")
            (expect (= expected (relative (keys (get result "matches")))))
            (expect (= expected
                       (relative (keys (get (:result (search (assoc spec "is_files_only" true)))
                                            "file_counts"))))))))))
  (it "preserves both flags through Python maps, kwargs and pagination"
      (write-temp! "grep-ignored-python/.gitignore" "ignored/\n")
      (write-temp! "grep-ignored-python/ignored/.hidden.txt" "marker first\nmarker second\n")
      (tpc/with-own
        [ctx (extension/builtin-sandbox-bindings (constantly nil))]
        (let [result (ep/run-python-block
                       ctx
                       (str "spec = {'query': 'marker', 'paths': ["
                            (pr-str (temp-dir-path "grep-ignored-python"))
                            "], 'is_hidden': True, 'is_ignored': True, 'limit': 1, 'context': 0}\n"
                            "for page in (await grep(spec), await grep(**spec)):\n"
                            "    print('marker first' in str(page))\n"
                            "    print('marker second' in str(next(page)))")
                       "t1/i1")]
          (expect (nil? (:error result)))
          (expect (= "True\nTrue\nTrue\nTrue\n" (:stdout result)))))))

;; Regression: a near-miss key used to cost the WHOLE search — `grep({"query" "x" "glob"
;; "*.clj"})` died on "find spec has unknown keys: glob" over a word nobody could have
;; guessed differently. Each near-miss now folds onto the canonical key it means, and
;; naming ONE idea twice is still refused.
(defdescribe
  grep-near-miss-key-aliases-test
  "Near-miss grep keys fold onto the canonical name; one idea named twice is refused."
  (let [coerce-find
        (private-fn "coerce-find-spec")

        grep
        (grep-data-fn)

        caught
        (fn [f & args]
          (try (apply f args) nil (catch clojure.lang.ExceptionInfo e e)))]

    (it "path, dir and dirs are read as paths"
        (doseq [alias ["path" "dir" "dirs"]]
          (expect (= ["."] (:paths (coerce-find [{"query" "needle" alias ["."]}]))))))
    (it "context_lines, regex and hidden are read as their canonical keys"
        (expect (= 2 (:context (coerce-find [{"query" "needle" "context_lines" 2}]))))
        (expect (true? (:is_regex (coerce-find [{"query" "needle" "regex" true}]))))
        (expect (true? (:is_hidden (coerce-find [{"query" "needle" "hidden" true}])))))
    (it "names grep in short validation errors"
        (doseq [[spec key] [[{"query" [nil]} "query"] [{"paths" 1} "paths"]
                            [{"context" -1} "context"] [{"limit" 0} "limit"]
                            [{"offset" -1} "offset"]]]
          (let [e (caught coerce-find [spec])]
            (expect (some? e))
            (expect (string/starts-with? (ex-message e) (str "grep: \"" key "\" ")))
            (expect (< (count (ex-message e)) 100)))))
    (it "refuses unknown keys with a short grep error and retains the vocabulary in data"
        (let [e (caught coerce-find [{"query" "needle" "reggex" true}])]
          (expect (some? e))
          (expect (= :ext.foundation.editing/invalid-find-args (:type (ex-data e))))
          (expect (= "grep: unknown keys: reggex. See doc(\"grep\")." (ex-message e)))
          (expect (= ["reggex"] (:unknown (ex-data e))))
          (expect (= ["context" "exclude" "include" "is_files_only" "is_hidden" "is_ignored"
                      "is_regex" "limit" "offset" "paths" "query"]
                     (:allowed (ex-data e))))))
    (it "a glob grep filters exactly like include, and excludes folds onto exclude"
        (write-temp! "aliasglob/a.clj" "needle here\n")
        (write-temp! "aliasglob/b.txt" "needle here\n")
        (let [dir
              (temp-dir-path "aliasglob")

              by-glob
              (:result (grep {"query" "needle" "paths" [dir] "glob" ["**/*.clj"]}))

              by-include
              (:result (grep {"query" "needle" "paths" [dir] "include" ["**/*.clj"]}))

              by-excludes
              (:result (grep {"query" "needle" "paths" [dir] "excludes" ["**/*.txt"]}))

              by-exclude
              (:result (grep {"query" "needle" "paths" [dir] "exclude" ["**/*.txt"]}))]

          (expect (= by-include by-glob))
          (expect (= by-exclude by-excludes))
          (expect (< (get by-exclude "hit_count")
                     (get (:result (grep {"query" "needle" "paths" [dir]})) "hit_count")))))
    (it "refuses include and glob together, naming the canonical key"
        (let [e (caught coerce-find [{"query" "needle" "include" ["*.clj"] "glob" ["*.clj"]}])]
          (expect (some? e))
          (expect (= "grep: use only one key for \"include\"." (ex-message e)))))))

 ;; Regression: `grep(["a", "b"], ["src", "tools"])` — needles, then scopes, the
;; obvious reading of a two-argument search — died on ARGUMENT SHAPE instead of
;; searching, because the second positional meant OPTIONS and the error offered
;; three call shapes at once. There is ONE canonical shape now: a single options
;; map (Python kwargs fold into that same map), so nothing positional can be
;; misread.
(defdescribe canonical-options-map-only-test
             "`grep` takes ONE options map — never a positional query."
             (let [grep-tool
                   (grep-data-fn)

                   caught
                   (fn [f & args]
                     (try (apply f args) nil (catch clojure.lang.ExceptionInfo e e)))]

               (it "grep refuses every positional shape and its message teaches the map"
                   (let [e (caught grep-tool ["a" "b"] ["src" "tools"])]
                     (expect (some? e))
                     (expect (= :ext.foundation.editing/invalid-find-args (:type (ex-data e))))
                     (expect (string/starts-with? (ex-message e) "grep:"))
                     (expect (< (count (ex-message e)) 100))
                     (expect (string/includes? (ex-message e)
                                               "grep({\"query\": q, \"paths\": [\"src\"]})")))
                   (expect (some? (caught grep-tool "needle")))
                   (expect (some? (caught grep-tool "needle" {"include" ["*.clj"]}))))
               (it "grep searches from the one canonical map"
                   (let [_
                         (write-temp! "canonmap/a.clj" "needle here\n")

                         out
                         (grep-tool {"query" "needle" "paths" [(temp-dir-path "canonmap")]})]

                     (expect (= 1 (get (:result out) "hit_count")))))))

(defdescribe find-files-directory-scope-test
             (let [find-files (grep-data-fn)]
               (it "does not widen an existing filename scope to its parent on zero content hits"
                   (let [dir (temp-dir-path "find-file-parent")
                         scoped-file (str dir "/scope.clj")
                         sibling-file (str dir "/sibling.clj")
                         _ (spit (fs/file scoped-file) "before\nnot-it\nafter\n")
                         _ (spit (fs/file sibling-file) "before\nsibling-only-needle\nafter\n")
                         result (:result (find-files {"query" "sibling-only-needle"
                                                      "paths" [scoped-file]}))]

                     (expect (= 0 (get result "hit_count")))
                     (expect (= {} (get result "matches")))
                     (expect (nil? (get result "first_hit")))))
               (it "uses an empty query as grep's ls mode without attempting content search"
                   (let [_ (write-temp! "grep-ls/one.clj" ";; a\n")
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
  grep-name-scope-test
  "Explicit paths bound name matching before scanning and pagination."
  (let [primary
        (str (fs/canonicalize (temp-dir-path "grep-name-scope/primary")))

        sibling
        (str (fs/canonicalize (temp-dir-path "grep-name-scope/sibling")))

        roots
        [primary sibling]

        _
        (doseq [[path content] [["primary/qzscope.clj" "qzscope\n"]
                                ["primary/qzscope-neighbor.clj" "qzscope\n"]
                                ["primary/sub/qzscope-child.clj" "qzscope\n"]
                                ["primary/submarine/qzscope-outside.clj" "qzscope\n"]
                                ["sibling/qzscope-external.clj" "qzscope\n"]]]
          (write-temp! (str "grep-name-scope/" path) content))

        lease-var
        (resolve 'com.blockether.vis.internal.workspace.fff-index/lease)

        lease
        @lease-var

        search
        (fn [spec]
          (let [leased (atom [])]
            (binding [workspace/*workspace-root* (fs/file primary)]
              (with-redefs [workspace/allowed-roots (constantly roots)
                            workspace/no-search-roots (constantly #{})
                            workspace/filesystem-root-mappings (constantly [])]

                (with-redefs-fn {lease-var (fn [root & args]
                                             (swap! leased conj (str root))
                                             (apply lease root args))}
                  #(let [result ((private-fn "grep-data") spec)] {:result result
                                                                  :leased (set @leased)}))))))]

    ;; A workspace-root file used to normalize to ".", searching every root.
    (it "a single file never scans its parent or other roots, with or without content hits"
        (doseq [query ["qzscope" "qzscpe" "" "zzzunmatchable"]]
          (let [{:keys [result leased]} (search {"query" query "paths" ["qzscope.clj"]})]
            (expect (= (if (= "zzzunmatchable" query) [] ["qzscope.clj"]) (get result "paths")))
            (expect (= ["qzscope.clj"] (get result "searched_paths")))
            (expect (= (if (= "qzscope" query) 1 0) (get result "hit_count")))
            (expect (empty? leased)))))
    (it "an explicit workspace directory does not expand into the default multi-root sweep"
        (let [{:keys [result leased]} (search {"query" "qzscope" "paths" [primary]})]
          (expect (= #{"qzscope.clj" "qzscope-neighbor.clj" "sub/qzscope-child.clj"
                       "submarine/qzscope-outside.clj"}
                     (set (get result "paths"))))
          (expect (= #{primary} leased))))
    (it "multiple scopes keep only the named file and directory subtree"
        (let [{:keys [result leased]} (search {"query" "qzscope" "paths" ["qzscope.clj" "sub"]})]
          (expect (= #{"qzscope.clj" "sub/qzscope-child.clj"} (set (get result "paths"))))
          (expect (= 2 (get result "hit_count")))
          (expect (= #{(str primary "/sub")} leased))))
    (it "scoped name-only pages do not include neighbors or other roots"
        (let [page
              (fn [offset]
                (:result (search
                           {"query" "" "paths" ["qzscope.clj" "sub"] "limit" 1 "offset" offset})))

              first-page
              (page 0)

              second-page
              (page (get first-page "next_offset"))]

          (expect (= 1 (count (get first-page "paths")) (count (get second-page "paths"))))
          (expect (= #{"qzscope.clj" "sub/qzscope-child.clj"}
                     (set (concat (get first-page "paths") (get second-page "paths")))))))
    (it "overlapping file and directory scopes contribute each name only once"
        (doseq [query ["qzscope" ""]]
          (let [paths (get (:result (search {"query" query "paths" ["qzscope.clj" primary]}))
                           "paths")]
            (expect (= 4 (count paths))))))
    (it "omitting paths retains the default multi-root search"
        (let [{:keys [result leased]} (search {"query" "qzscope"})]
          (expect (= 5 (count (get result "paths"))))
          (expect (= (set roots) leased))))))

(defdescribe
  grep-searched-paths-reporting-test
  "`searched_paths` reports every physical root actually searched, not the
   caller's shorthand `.` that expanded into those roots."
  (it "expands the default scope into the primary and searchable workspace roots"
      (let [grep-tool
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
          (with-redefs [workspace/allowed-roots (constantly [primary sibling])
                        workspace/no-search-roots (constantly #{})
                        workspace/filesystem-root-mappings (constantly [])]

            (let [result (:result (grep-tool {"query" ""}))
                  expected (mapv rel-path (map fs/file [primary sibling]))]

              (expect (= expected (get result "searched_paths")))
              (expect (not= ["."] (get result "searched_paths")))))))))

;; A capped grep had no way to ask for the NEXT page: the only way past the
;; limit was to re-run the same search with a bigger `limit` and pay for the
;; first page all over again.
(defdescribe
  grep-compact-output-test
  (it "does not page through unrelated names after a content match"
      (let [dir (temp-dir-path "grep-name-filter")
            _ (doseq [i (range 4)]
                (write-temp! (str "grep-name-filter/shared_" i ".txt")
                             (if (zero? i) "shared phrase\n" "unrelated\n")))
            out (:result ((grep-data-fn)
                           {"query" ["shared phrase" "extra word"]
                            "paths" [dir] "limit" 2}))]
        (expect (= 1 (get out "hit_count")))
        (expect (= [] (get out "paths")))
        (expect (= "end_of_results" (get out "truncated_by")))
        (expect (nil? (get out "next_offset")))))
  (it "filters names before paging and does not page past the final exact-sized page"
      (let [dir (temp-dir-path "grep-name-page")
            _ (doseq [name ["shared_a" "shared_b" "needle_a" "needle_b"]]
                (write-temp! (str "grep-name-page/" name ".txt") "unrelated\n"))
            spec {"query" ["shared" "needle"] "paths" [dir] "limit" 1}
            search (private-fn "find-search")
            first-page (search [spec] ["needle"])
            last-page (search [(assoc spec "offset" 1)] ["needle"])]
        (expect (= 1 (count (get first-page "paths"))))
        (expect (= 1 (count (get last-page "paths"))))
        (expect (every? #(string/includes? % "needle_")
                        (concat (get first-page "paths") (get last-page "paths"))))
        (expect (not= (get first-page "paths") (get last-page "paths")))
        (expect (= "limit" (get first-page "truncated_by")))
        (expect (= "end_of_results" (get last-page "truncated_by"))))))

(defdescribe
  grep-paging-test
  "`offset` + `next_offset` page BOTH grep axes — the CONTENT hits and the
   ranked NAME matches. `next_offset` is null exactly when the page already is
   the whole answer, so a paging loop terminates on a value test."
  (let [grep-tool
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
        (let [_
              (write-temp! "grep-page/a.txt"
                           (string/join "\n" (map #(str "needle " %) (range 300))))

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
        (let [_
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
        (let [_
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
  (let [content-result
        (private-fn "content-result")

        find-files
        (grep-data-fn)]

    (it "a content sweep capped by the hit limit reports hits_truncated_by"
        (let [out (content-result {:hits [{:path "a.clj" :line 1 :text "x"}]
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
        (let [r (content-result {:hits [{:path "a.clj" :line 1 :text "x"}]
                                 :truncated-by :end-of-results
                                 :total-file-count 1
                                 :total-file-count-exact? true}
                                ["x"])]
          ;; TOTAL result: the key is a VALUE test, never a `contains?` test.
          (expect (contains? r "hits_truncated_by"))
          (expect (nil? (get r "hits_truncated_by")))
          (expect (true? (get r "total_file_count_is_exact")))
          (expect (= 1 (get r "total_file_count")))))
    (it "a regex-looking query that matches no LITERAL content is rescued as a regex"
        (let [_
              (write-temp! "grep-regex/needle.clj" "(defn needle [] :ok)\n")

              dir
              (temp-dir-path "grep-regex")

              result
              (:result (find-files {"query" "defn-? +needle" "paths" [dir]}))]

          (expect (= 1 (get result "hit_count")))))
    (it "a literal query that DOES match content gets no hint"
        (let [_
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
  rg-exclude-globs-test
  "`exclude` is `include`'s complement on the CONTENT axis: globs matched against
   the repo-relative path AND the bare file name, blank-tolerant exactly like
   `include`, and OUTRANKING `include` when both match the same file."
  (let [coerce-rg
        (private-fn "coerce-rg-spec")

        coerce-find
        (private-fn "coerce-find-spec")

        grep
        (private-fn "rg-search")]

    (it "coerce-rg-spec parses :exclude exactly like :include"
        (expect (= [] (:exclude (coerce-rg {"query" ["x"]}))))
        (expect (= ["*_test.clj"] (:exclude (coerce-rg {"query" ["x"] "exclude" "*_test.clj"}))))
        (expect (= ["**/a.clj" "**/b.clj"]
                   (:exclude (coerce-rg {"query" ["x"] "exclude" ["**/a.clj" "**/b.clj"]}))))
        ;; the stringified-array shape LLMs write survives here too
        (expect (= ["**/a.clj" "**/b.clj"]
                   (:exclude (coerce-rg {"query" ["x"] "exclude" "[\"**/a.clj\", \"**/b.clj\"]"}))))
        (expect (= [] (:exclude (coerce-rg {"query" ["x"] "exclude" ""}))))
        (expect (= ["**/*.clj"] (:exclude (coerce-rg {"query" ["x"] "exclude" ["" "**/*.clj"]})))))
    (it "accepts exclude and retains it in structured unknown-key diagnostics"
        (expect (map? (coerce-find [{"query" "x" "exclude" ["**/*_test.clj"]}])))
        (let [err (try (coerce-find [{"query" "x" "exlude" ["a"]}])
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
          (expect (= "grep: unknown keys: exlude. See doc(\"grep\")." (ex-message err)))
          (expect (some #{"exclude"} (:allowed (ex-data err))))))
    (it "rg-search skips every file an exclude glob matches"
        (let [_
              (write-temp! "rgexclude/a.clj" "needle in source\n")

              _
              (write-temp! "rgexclude/a_test.clj" "needle in test\n")

              out
              (grep {"query" ["needle"]
                     "paths" [(temp-dir-path "rgexclude")]
                     "exclude" ["*_test.clj"]})]

          (expect (= ["needle in source"] (mapv :text (:hits out))))))
    (it "exclude WINS over include when both globs match the file"
        (let [_
              (write-temp! "rgexcwins/a.clj" "needle in source\n")

              _
              (write-temp! "rgexcwins/a_test.clj" "needle in test\n")

              out
              (grep {"query" ["needle"]
                     "paths" [(temp-dir-path "rgexcwins")]
                     "include" ["*.clj"]
                     "exclude" ["*_test.clj"]})]

          (expect (= ["needle in source"] (mapv :text (:hits out))))))
    (it
      "an exclude glob matches the relative PATH, not only the file name"
      (let [_
            (write-temp! "rgexcdir/src/a.clj" "needle in source\n")

            _
            (write-temp! "rgexcdir/test/a.clj" "needle in test\n")

            out
            (grep
              {"query" ["needle"] "paths" [(temp-dir-path "rgexcdir")] "exclude" ["**/test/**"]})]

        (expect (= ["needle in source"] (mapv :text (:hits out))))))
    (it "grep hands exclude through to the content sweep"
        (let [_
              (write-temp! "rgexcgrep/a.clj" "needle in source\n")

              _
              (write-temp! "rgexcgrep/a_test.clj" "needle in test\n")

              out
              (:result ((grep-data-fn)
                         {"query" "needle"
                          "paths" [(temp-dir-path "rgexcgrep")]
                          "exclude" ["*_test.clj"]}))]

          (expect (= 1 (get out "hit_count")))))))

(defdescribe
  find-relevance-filter-test
  "Regression: fff's native matcher returns a full page of loose subsequence
   matches with no score (query \"lmstudio\" alone hit 108/489 unrelated paths).
   find-search must post-filter fff's candidates by per-token relevance so only
   genuine hits survive — while staying typo-tolerant and word-order-insensitive."
  (let [relevance
        (private-fn "find-relevance")

        min-score
        (private-fn "find-min-score")

        find-search
        (private-fn "find-search")]

    (it "scores a genuine filename hit far above scattered subsequence noise"
        (let [genuine
              (relevance "lmstudio" "a/b/provider_lmstudio.clj")

              noise
              (relevance "lmstudio" "plugins/foundation_git/src/merge_ops.clj")]

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
        (let [_
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
        (let [_ (write-temp! "findfuzz/render.clj" ";; the visualization renderer\n")
              _ (write-temp! "findfuzz/stream_call_handlers.md" "# stream call docs\n")
              _ (write-temp! "findfuzz/unrelated_widget.clj" ";; nope\n")
              dir (temp-dir-path "findfuzz")
              out (find-search [{"query" "stream call visualization render" "paths" [dir]}])
              names (mapv #(last (string/split % #"/")) (get out "paths"))]

          ;; strict MIN would need ALL five words in one path → nothing; fuzzy saves it
          (expect (true? (get out "fuzzy")))
          (expect (some #{"render.clj"} names))
          ;; the exact-name bullseye (`render` → render.clj) ranks FIRST, above the
          ;; two-common-word loose hit (stream+call → stream_call_handlers.md)
          (expect (= "render.clj" (first names)))
          ;; the terms that actually landed are reported
          (expect (some #{"render"} (get out "matched_terms")))
          ;; a file matching NONE of the terms is not dragged in
          (expect (not (some #{"unrelated_widget.clj"} names)))))
    (it "a precise query that the strict MIN pass satisfies stays NON-fuzzy"
        (let [_ (write-temp! "findprecise/channel_tui_footer.clj" ";; footer\n")
              dir (temp-dir-path "findprecise")
              out (find-search [{"query" "channel tui footer" "paths" [dir]}])]

          (expect (false? (get out "fuzzy")))
          (expect (some #{"channel_tui_footer.clj"}
                        (map #(last (string/split % #"/")) (get out "paths"))))))
    (it "a BLANK query lists every file under the paths like ls (no scoring)"
        (let [_ (write-temp! "findls/one.clj" ";; a\n")
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
        (let [_ (doseq [i (range 30)]
                  (write-temp! (format "findfuzzcap/alphaonly_%02d.clj" i) ";; x\n"))
              dir (temp-dir-path "findfuzzcap")
              out (find-search [{"query" "alphaonly betaonly" "paths" [dir]}])]

          (expect (true? (get out "fuzzy")))
          (expect (= 50 (get out "limit")))
          (expect (= 30 (get out "item_count")))))
    (it "EVERY grep result carries the SAME TOTAL key set — hit, miss and ls"
        (let [gt (grep-data-fn)
              _ (write-temp! "greptotal/one.clj" ";; needle-total\n")
              dir (temp-dir-path "greptotal")
              ks #(set (keys (:result %)))
              hit (gt {"query" "needle-total" "paths" [dir]})
              miss (gt {"query" "zzz-nothing-here-xyz" "paths" [dir]})
              ls (gt {"query" "" "paths" [dir]})]

          ;; One shape for every outcome: caller code indexes a field instead of
          ;; probing for it, so a nil-valued signal can never read as a tool bug.
          (expect (= (ks hit) (ks miss) (ks ls)))
          (expect (not (contains? (ks hit) "missing_paths")))
          (expect (every? (ks hit)
                          ["hits_truncated_by" "file_counts" "total_file_count"
                           "total_file_count_is_exact" "first_hit"]))
          (expect (nil? (get-in hit [:result "hits_truncated_by"])))))
    (it "a genuinely-unmatchable query still returns nothing (fuzzy can't invent hits)"
        (let [_ (write-temp! "findnone/alpha.clj" ";; x\n")
              dir (temp-dir-path "findnone")
              out (find-search [{"query" "zzzqqq wwwvvv" "paths" [dir]}])]

          (expect (zero? (get out "item_count")))))))

(defdescribe
  rg-sort-key-efficiency-test
  ;; `(sort-by rel-path)` used to run `rel-path` (canonicalize SYSCALLS) INSIDE
  ;; the comparator — O(n·log n) calls that pinned a full core for minutes on a
  ;; big tree and, with no interrupt checkpoint in the sort, kept burning long
  ;; AFTER cancellation (the 400%-CPU orphaned-gateway regression). The sort key
  ;; is now computed ONCE per walked file with a `check-interrupt!` poll.
  (let [grep
        (private-fn "rg-search")

        rel-path-var
        (resolve (symbol "com.blockether.vis.internal.foundation.editing.core" "rel-path"))

        corpus!
        (fn [dir n]
          (dotimes [i n]
            (write-temp! (format "%s/f%02d.txt" dir i) (if (zero? i) "alpha\n" "nothing here\n")))
          (temp-dir-path dir))]

    (it "computes the sort key O(n) — once per walked file, not once per comparison"
        (let [n
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
      (let [path
            (corpus! "rgsortint" 8)

            orig
            @rel-path-var

            first-call
            (atom true)]

        (try (let [thrown (try (with-redefs-fn {rel-path-var
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
  (let [grep
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
          (try (let [thrown (try (with-redefs-fn {(core-var "file-hit-count")
                                                  (interrupt-on-first-call (fn [_ _]
                                                                             0))}
                                   #(grep {"query" ["alpha"] "paths" [path] "is_files_only" true}))
                                 nil
                                 (catch InterruptedException e e))]
                 (expect (some? thrown)))
               (finally (Thread/interrupted)))))
    (it "content scan aborts on interrupt instead of scanning to the end"
        (let [path (corpus! "rgscanintc")]
          (try (let [thrown (try (with-redefs-fn {(core-var "search-file-content")
                                                  (interrupt-on-first-call (fn [_ _ _ _]
                                                                             []))}
                                   #(grep {"query" ["alpha"] "paths" [path]}))
                                 nil
                                 (catch InterruptedException e e))]
                 (expect (some? thrown)))
               (finally (Thread/interrupted)))))))

(defdescribe
  gitignore-default-policy-test
  ;; Search respects gitignore by default and rejects the removed is_respect_gitignore key.
  ;; The is_ignored flag and configured overlays can widen the native index.
  (let [grep
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
        (let [path
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
  (let [grep
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

    (it "a `!` in .ignore re-includes a .gitignore'd dir for rg AND find_files (default flag)"
        (let [dir
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
        (let [dir
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
  ;; keeps pruning inside the rescued subtree when is_ignored is false.
  (let [grep
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
            [config/load-config-raw (fn []
                                      {"grep" (cond-> {}
                                                (seq (:include-gitignored-paths search-block))
                                                (assoc "include_gitignored_paths"
                                                  (:include-gitignored-paths search-block))

                                                (seq (:always-exclude search-block))
                                                (assoc "always_exclude"
                                                  (:always-exclude search-block)))})]
            (f)))

        fixture!
        (fn [dir]
          (write-temp! (str dir "/.gitignore") "repositories/\n")
          (write-temp! (str dir "/repositories/corp/secret.txt") "NEEDLE_TOKEN here\n")
          (write-temp! (str dir "/repositories/corp/node_modules/dep/secret_dep.txt")
                       "NEEDLE_TOKEN here\n")
          (temp-dir-path dir))]

    (it "default excludes prune dependency, build and tool directories at every depth"
        (let [dir
              "search-overlay-default-excludes"

              path
              (fixture! dir)

              excluded-dirs
              [".git" "node_modules" "target" "build" "dist" "out" ".gradle" ".m2" "__pycache__"
               ".venv" ".shadow-cljs" "cljs-runtime" "vendor" ".next" ".cpcache" ".clj-kondo"
               ".calva" ".lsp" ".rift"]]

          (doseq [prefix
                  ["repositories/corp/" "repositories/corp/nested/"]

                  excluded-dir
                  excluded-dirs]

            (write-temp! (str dir "/" prefix excluded-dir "/secret_excluded.txt")
                         "NEEDLE_TOKEN here\n"))
          (write-temp! (str dir "/repositories/corp/cljs-runtime-source/secret.txt")
                       "NEEDLE_TOKEN here\n")
          (doseq [include-pattern ["repositories/" "repositories/**"]]
            (overlay! {:include-gitignored-paths [include-pattern]}
                      (fn []
                        (doseq [search [rg-files find-paths]]
                          (let [files (search path)]
                            (expect (has? files "repositories/corp/secret.txt"))
                            (expect (has? files "cljs-runtime-source/secret.txt"))
                            (doseq [excluded-dir excluded-dirs]
                              (expect (not (has? files (str "/" excluded-dir "/"))))))))))))
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
    (it "the configured overlay applies when is_ignored is absent"
        (let [path (fixture! "search-overlay-explicit")]
          ;; without config the gitignored subtree stays hidden …
          (expect (not (has? (rg-files path) "repositories/corp/secret.txt")))
          (overlay! {:include-gitignored-paths ["repositories/"]}
                    (fn []
                      ;; … and with it the rescue applies to EVERY call
                      (expect (has? (rg-files path) "repositories/corp/secret.txt"))
                      (expect (not (has? (rg-files path) "secret_dep.txt")))))))
    ;; Regression, issue #23: an explicit re-include reopened unconditional cache trees.
    (it
      "never reopens cache directories through include_gitignored_paths"
      (let [dir
            "search-overlay-cache-pruning"

            _
            (write-temp! (str dir "/.gitignore") ".cpcache/\nsrc/.cache/\n.ruff_cache/\n")

            _
            (write-temp! (str dir "/secret-visible.txt") "NEEDLE_TOKEN here\n")

            _
            (write-temp! (str dir "/.cpcache/secret-root.txt") "NEEDLE_TOKEN here\n")

            _
            (write-temp! (str dir "/src/.cache/secret-nested.txt") "NEEDLE_TOKEN here\n")

            _
            (write-temp! (str dir "/.ruff_cache/secret-ruff.txt") "NEEDLE_TOKEN here\n")

            path
            (temp-dir-path dir)]

        (overlay! {:include-gitignored-paths [".cpcache/**" "src/.cache/**" ".ruff_cache/**"]}
                  (fn []
                    (let [rg
                          (rg-files path)

                          found
                          (find-paths path)]

                      (expect (has? rg "secret-visible.txt"))
                      (expect (has? found "secret-visible.txt"))
                      (doseq [cache-path [".cpcache/secret-root.txt" "src/.cache/secret-nested.txt"
                                          ".ruff_cache/secret-ruff.txt"]]
                        (expect (not (has? rg cache-path)))
                        (expect (not (has? found cache-path)))))))))
    (it "an explicit :always-exclude REPLACES the defaults"
        (let [path (fixture! "search-overlay-replace")]
          (overlay! {:include-gitignored-paths ["repositories/"] :always-exclude ["*.md"]}
                    (fn []
                      ;; node_modules resurfaces — the default guard list is gone
                      (expect (has? (rg-files path) "secret_dep.txt"))))))))

(defdescribe
  fff-scan-concurrency-guard
  "The bounded-concurrency permit around FRESH fff index scans (rg /
   find_files). Bounds the CPU-heavy scan fan-out without
   serializing it, and never leaks a permit."
  (let [guard
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
          (let [n
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
          (let [seen-during-build
                (atom nil)

                fake-idx
                (reify
                  java.io.Closeable
                    (close [_] nil))

                open-index!
                (fff-index-fn "open!")]

            (with-redefs [fff/create
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
          (let [before
                (.availablePermits semaphore)

                closed?
                (atom false)

                fake-idx
                (reify
                  java.io.Closeable
                    (close [_] (reset! closed? true)))

                open-index!
                (fff-index-fn "open!")]

            (with-redefs [fff/create
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
          (let [open-index!
                (fff-index-fn "open!")

                fake-idx
                (reify
                  java.io.Closeable
                    (close [_] nil))]

            (with-redefs [fff/create
                          (fn [_opts]
                            fake-idx)

                          fff/wait-for-scan
                          (fn [_idx _timeout]
                            false)]

              (let [data (try (open-index! (java.io.File. "."))
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
      (let [_
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
            ((fff-index-fn "note-fs-write!") (fs/file (temp-root) "fffpool/fresh.txt"))

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
      (let [_
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
            (do (note! (fs/file (temp-root) "fffnoscan/seed.txt")) (run))

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
    (let [dir-name
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
      (with-redefs [config/load-config-raw (fn []
                                             {"grep" {"include_gitignored_paths" [rel-included]}})]
        (expect (contains? (names) "repositories"))))))

;; Regression, issue #126: `ls` was fff-only, and fff refuses to index a filesystem
;; root or a home directory ("Can not run certain FFF features in a file system root
;; or home directories"), so `ls("/")` and `ls("~")` answered with
;; "rg requires fff for directory search, but fff failed for /" and the ROOT files
;; were unreachable — the one listing a real `ls` never fails at.
(defdescribe
  ls-unindexable-dir-test
  (it "ls lists a directory fff refuses to index — filesystem root and home"
      (let [home (System/getProperty "user.home")]
        ;; the session that reported this had the WHOLE filesystem granted as a root
        (with-redefs [workspace/allowed-roots (constantly ["/" home])
                      workspace/filesystem-root-mappings (constantly [{:trunk "/" :clone "/"}])]

          (let [row (fn [spec]
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
            (expect (some #(string/starts-with? % ".") (names {"path" home "is_hidden" true})))
            ;; a home directory is refused for the same reason and must list too
            (expect (= "dir" (get (row home) "type"))))))))

(defdescribe
  ls-cold-listing-test
  "Cold listings must not build a recursive content index."
  (it
    "lists the root and subdirectories again without creating an index"
    (let [creates
          (atom 0)

          waits
          (atom 0)

          listed
          (atom [])]

      (with-redefs [editing/fff-ls-workspace-items
                    (fn [_ _ ^long _ _]
                      nil)

                    fff/create
                    (fn [_]
                      (swap! creates inc)
                      (throw (ex-info "A listing must not build an index" {})))

                    fff/wait-for-scan
                    (fn [& _]
                      (swap! waits inc)
                      false)

                    fff/list-directory
                    (fn [path opts]
                      (swap! listed conj [path opts])
                      [{:relative-path "a.txt" :dir? false :size 1}])]

        (doseq [path ["." "src" "." "src"]]
          (expect (= ["a.txt"]
                     (mapv #(get % "name")
                           (get ((private-fn "ls-one") {"path" path "depth" 1}) "entries"))))))
      (expect (zero? @creates))
      (expect (zero? @waits))
      (expect (= 4 (count @listed)))
      (expect (every? #(= 1 (:max-depth (second %))) @listed))))
  (it "passes rebased ignore overlays and listing options to the bounded walk"
      (let [calls
            (atom [])

            root
            (.getCanonicalFile (java.io.File. "src"))]

        (with-redefs [editing/fff-ignore-overlay
                      (constantly {:custom-ignore-filenames [".rgignore"]
                                   :exclude-globs ["src/private/" "*.secret"]
                                   :unignore-globs ["src/vendor/"]})

                      fff/list-directory
                      (fn [path opts]
                        (swap! calls conj [path opts])
                        [{:relative-path "a.txt" :dir? false :size 7}])]

          (expect (= [{:relative-path "a.txt" :directory? false :size 7}]
                     ((private-fn "fff-ls-target-items") root "src" 2 true))))
        (expect (= [[(.getPath root)
                     {:custom-ignore-filenames [".rgignore"]
                      :exclude-globs ["private/" "*.secret"]
                      :unignore-globs ["vendor/"]
                      :max-depth 2
                      :include-hidden? true
                      :respect-ignore-files? true}]]
                   @calls)))))

(defn- warm-workspace-ls-index!
  "Build the index that grep and find normally supply to warm listing tests."
  []
  (fff-index/with-index* (fff-index/lease (.getCanonicalFile (java.io.File. "."))
                                          true
                                          ((private-fn "fff-ignore-overlay")))
                         (constantly nil)))

(defdescribe
  ls-fff-index-reuse-test
  (it
    "ls serves a workspace subdirectory from the WARM workspace index, building no per-directory index"
    (let [lease
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
          (warm-workspace-ls-index!)

          ;; Pool state is PROCESS-WIDE and other namespaces in the same suite may
          ;; already hold an index here, so the pin is the DELTA: listing must not
          ;; warm anything that was cold.
          sub-cold-before
          [(warm? (lease sub true (ignore-overlay))) (warm? (lease sub true (ls-overlay sub-rel)))]

          listed
          (names sub-rel)]

      ;; The search index is warm before either listing runs.
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
;; Cross-check both native listing sources against the OS, minus hidden and ignored entries.
;;
;; And `python_execution` is the model's main hand: reading a file is an ORDINARY
;; Python call (`cat`, engine-bound), and listing a directory is the `ls` SHIM — a
;; plain function in the sandbox globals, costing no tool result at all.
;; =============================================================================

(defdescribe
  ls-matches-filesystem-test
  "The fff-backed listing agrees with the OS on directories with nothing ignored."
  (it
    "returns exactly the non-hidden children `File.listFiles` reports"
    (doseq [dir ["src/com/blockether/vis/internal/foundation"
                 "src/com/blockether/vis/internal/foundation/editing" "resources/vis-host-python"]]
      (let [listed (set (map #(get % "name") (get ((private-fn "ls-one") {"path" dir}) "entries")))
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
      (let [bind
            (extension/builtin-sandbox-bindings (constantly nil))

            docs
            (extension/sandbox-symbol-docs)

            ls-shim
            (some #(when (= "ls" (:shim/name %)) %) (extension/sandbox-shims))]

        ;; The model reads and writes files with plain Python: no `cat` or `patch`.
        (expect (nil? (get bind 'cat)))
        (expect (nil? (get docs 'cat)))
        (expect (nil? (get bind 'patch)))
        (expect (nil? (get docs 'patch)))
        ;; `ls` left the tool layer too: a shim global, never an engine-bound
        ;; symbol, so it costs no schema and no tool result.
        (expect (nil? (get bind 'ls)))
        (expect (nil? (get docs 'ls)))
        (expect (= ["ls"] (:shim/globals ls-shim)))))
  (it "lists a real directory from real Python"
      (tpc/with-own [ctx (extension/builtin-sandbox-bindings (constantly nil))]
                    (let [result (ep/run-python-block
                                   ctx
                                   (str
                                     "tree = "
                                     "ls(\"src/com/blockether/vis/internal/foundation/editing\")\n"
                                     "print(isinstance(tree, str) and \"core.clj\" in tree)")
                                   "t1/i1")]
                      (expect (nil? (:error result)))
                      (expect (= "True\n" (:stdout result)))))))

;; =============================================================================
;; `ls` ORDERING, and the two sources allowed to answer a listing.
;;
;; Warm index records and bounded listing records must render the same rows and order.
;; Directories come first, then alphabetical names, at every nesting level.
;; =============================================================================

(defn- ls-order-key
  "One entry's position under the documented listing order."
  [entry]
  [(if (= "dir" (get entry "type")) 0 1) (str (get entry "name"))])

(defn- ls-order-violations
  "Every `[path [worse better]]` adjacent pair that breaks the documented order,
   walking `entries` AND every nested `children` vector."
  [path entries]
  (into (vec (for [[a b]
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
      (doseq [dir ["." "src" "test/com/blockether/vis" "src/com/blockether/vis/internal/foundation"
                   "resources"]]
        (let [entries (get ((private-fn "ls-one") {"path" dir "depth" 3 "is_hidden" true})
                           "entries")]
          (expect (seq entries))
          (expect (= [] (ls-order-violations dir entries))))))
  (it "sorts a nested children vector, not just the top level"
      (let [_
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
  (it "renders an identical listing from the warm index and bounded walk"
      (let [dir
            "src/com/blockether/vis/internal/foundation"

            root
            (.getCanonicalFile (java.io.File. dir))

            ;; the workspace index has to be warm before it can serve anything
            _
            (warm-workspace-ls-index!)

            warm-rows
            ((private-fn "fff-ls-workspace-items") root dir 2 false)

            fallback-rows
            ((private-fn "fff-ls-target-items") root dir 2 false)

            ;; `fff-ls-workspace-items` carries primitive hints, so the stand-in has
            ;; to match its shape
            listing
            (fn [rows]
              (with-redefs [editing/fff-ls-workspace-items (fn [_ _ ^long _ _]
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
      (with-redefs [fff/search-mixed (fn [& _]
                                       (throw (ex-info "ls must not call search-mixed" {})))]
        (let [root (get ((private-fn "ls-one") {"path" "." "depth" 2}) "entries")
              sub (get ((private-fn "ls-one") {"path" "src/com/blockether/vis/internal/foundation"})
                       "entries")]

          (expect (seq root))
          (expect (contains? (set (map #(get % "name") sub)) "editing")))))
  (it "serves a warm workspace subdirectory without ever building a fresh index"
      ;; `grep`/`find` keep the workspace index hot, and `ls` of a subdirectory has
      ;; to ride it. A fallback listing is still CORRECT, so agreement alone cannot
      ;; catch a silently lost fast path — only counting the fallback can.
      (warm-workspace-ls-index!)
      (let [fresh (atom 0)]
        (with-redefs [editing/fff-ls-target-items (fn [_ _ ^long _ _]
                                                    (swap! fresh inc)
                                                    nil)]
          (doseq [spec [{"path" "src" "depth" 2} {"path" "test/com/blockether/vis/internal"}
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
  (let [list-dir
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
        (let [root
              (outside!)

              out
              (with-redefs [editing/fff-ls-target-items
                            (fn [_ _ ^long _ _]
                              (throw (ex-info "ls must not index a directory outside the workspace"
                                              {})))]
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
    (it "lists a shared auxiliary root without indexing its tree"
        (let [root
              (outside!)

              path
              (.getPath root)]

          (binding [workspace/*filesystem-roots*
                    [{:trunk path :clone path :draft :shared}
                     {:trunk "/" :clone "/" :draft :shared :no-search? true}]]
            (with-redefs [editing/fff-ls-target-items
                          (fn [_ _ ^long _ _]
                            (throw (ex-info "ls must not index a shared auxiliary root" {})))]
              (expect (= #{"sub" "a.txt"} (names (first (ls-rows {"paths" [path] "depth" 1})))))))))
    ;; Regression: `ls` decided ownership from the RENDERED address, and `rel-path`
    ;; renders a context clone as its TRUNK absolute path — so the workspace itself,
    ;; mounted as a draft clone, read exactly like `/etc` and every listing inside it
    ;; silently lost the index path along with the `vis.yml` overlay it applies.
    (it "treats a context clone as the workspace it is, not as an outside tree"
        (let [clone
              (fs/path (System/getProperty "java.io.tmpdir") "vis-ls-clone-test")

              _
              (do (fs/delete-tree clone)
                  (fs/create-dirs (fs/path clone "sub"))
                  (spit (fs/file (fs/path clone "a.txt")) "a"))

              root
              (.getCanonicalFile (fs/file clone))

              indexed
              (atom 0)]

          (with-redefs [workspace/filesystem-root-mappings
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
   nothing about time: a pathological tree could ride to the outer Python eval
   wall and return nothing at all."
  (let [gt
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
      (let [_
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
            ((fff-index-fn "note-fs-write!") big)

            dir
            (temp-dir-path "greplarge")]

        (try (let [hits
                   (:hits (rg-search
                            {"query" "ZZBIGNEEDLEZZ" "paths" [dir] "context" 1 "limit" 50}))

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
        (let [_
              (doseq [i (range 1 6)]
                (write-temp! (str "grepdeadline/f" i ".clj") ";; ZZDEADLINEZZ\n"))

              dir
              (temp-dir-path "grepdeadline")]

          (with-redefs-fn {budget-var 0}
            (fn []
              (let [content
                    (rg-search {"query" "ZZDEADLINEZZ" "paths" [dir] "limit" 50})

                    files-only
                    (rg-search
                      {"query" "ZZDEADLINEZZ" "paths" [dir] "limit" 50 "is_files_only" true})]

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
        (let [_
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
    (it "a native grep page cut by its time budget labels the result PARTIAL"
        ;; Issue #438: a grep with no hits read a large tree for minutes, because
        ;; fff counts its page budget only after a hit unless the call enforces it.
        ;; With the budget enforced, a cut page must not read as end-of-results.
        (let [_
              (write-temp! "grepnativecut/a.clj" ";; ZZNATIVECUTZZ\n")

              dir
              (temp-dir-path "grepnativecut")

              real-grep
              fff/grep

              cut-page
              (fn [idx opts]
                (if (:enforce-time-budget? opts)
                  {:matches [] :count 0 :next-file-offset (inc (long (:file-offset opts 0)))}
                  (real-grep idx opts)))]

          (with-redefs [fff/grep cut-page]
            (let [content (rg-search {"query" "ZZNATIVECUTZZ" "paths" [dir] "limit" 50})
                  files-only
                  (rg-search
                    {"query" "ZZNATIVECUTZZ" "paths" [dir] "limit" 50 "is_files_only" true})]

              (expect (= :time (:truncated-by content)))
              (expect (false? (:total-file-count-exact? content)))
              (expect (= :time (:truncated-by files-only)))
              (expect (false? (:total-file-count-exact? files-only)))))))
    (it "streaming a file keeps the SAME context windows the slurping walk produced"
        (let [rel
              (write-temp! "grepstream/ctx.txt"
                           (string/join "\n"
                                        (map
                                          #(if (#{2 3 9} %) (str "L" % " ZZSTREAMZZ") (str "L" %))
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
        (let [rel
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
    (let [safe-path
          (private-fn "safe-path")

          trunk
          (mk-tmp-dir "vis-iso-trunk")

          clone
          (mk-tmp-dir "vis-iso-clone")]

      (spit (java.io.File. ^String trunk "a.txt") "TRUNK ORIGINAL")
      (spit (java.io.File. ^String clone "a.txt") "IN CLONE")
      (binding [workspace/*workspace-root*
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
      (let [safe-path
            (private-fn "safe-path")

            clone
            (mk-tmp-dir "vis-iso-clone2")

            secret
            (mk-tmp-dir "vis-iso-secret")]

        (spit (java.io.File. ^String secret "c.txt") "SECRET")
        (binding [workspace/*workspace-root*
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
                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))
  ;; hs_err_pid61432: POSIX locks belong to the process, so one file tool read of a
  ;; live `vis.db-shm` released sqlite's wal-index locks for the whole gateway.
  (it "a file this process holds OS locks on is refused while its neighbours are served"
      (let [safe-path
            (private-fn "safe-path")

            root
            (mk-tmp-dir "vis-held")

            shm
            (str root "/vis.db-shm")]

        (spit (java.io.File. ^String root "notes.txt") "NOTES")
        (spit (java.io.File. ^String shm) "")
        (paths/hold-files! ::held-shm [shm])
        (try (binding [workspace/*workspace-root*
                       root

                       workspace/*filesystem-roots*
                       [{:trunk root :clone root :draft :shared :primary? true}]]

               (expect (= "NOTES" (slurp (safe-path (str root "/notes.txt")))))
               (let [data
                     (try (safe-path shm) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                 (expect (= :ext.foundation.editing/path-denied (:type data)))
                 (expect (= :held-lock (:reason data)))))
             (finally (paths/release-held-files! ::held-shm))))))

(defdescribe
  grep-files-only-public-test
  "`is_files_only` reaches the PUBLIC grep and answers WHICH files match and HOW
   MANY times each. The engine implemented the mode all along while the tool
   rejected the key as unknown — and that gap is what sent callers into a
   hand-rolled `os.walk` + `read_text` over the whole tree for a question grep
   could answer in one call."
  (let [grep (grep-data-fn)]
    (it "is accepted as a key and carries a per-file hit count"
        (let [_ (write-temp! "grepfopub/a.txt" "alpha\nalpha\nbeta\n")
              _ (write-temp! "grepfopub/b.txt" "alpha\n")
              _ (write-temp! "grepfopub/c.txt" "nothing here\n")
              d (temp-dir-path "grepfopub")
              out (:result (grep {"query" ["alpha"] "paths" [d] "is_files_only" true}))]

          (expect (true? (get out "is_files_only")))
          (expect (= 2 (get out "file_count")))
          (expect (= 3 (get out "hit_count")))
          ;; densest file first, and the count is the number of matching LINES
          (expect (= [2 1] (mapv #(long (val %)) (get out "file_counts"))))
          (expect (empty? (get out "matches")))
          (expect (nil? (get out "hits_truncated_by")))))
    (it "answers COMPLETELY where the default content page is capped by limit"
        (let [_ (dotimes [i 6]
                  (write-temp! (format "grepfocap/f%d.txt" i) "alpha\nalpha\n"))
              d (temp-dir-path "grepfocap")
              content (:result (grep {"query" ["alpha"] "paths" [d] "limit" 3}))
              files (:result (grep {"query" ["alpha"] "paths" [d] "is_files_only" true}))]

          (expect (= "limit" (get content "hits_truncated_by")))
          (expect (= 6 (get files "file_count")))
          (expect (= 12 (get files "hit_count")))
          (expect (nil? (get files "hits_truncated_by")))))
    (it "an explicit limit still wins over the files-only default"
        (let [_ (dotimes [i 4]
                  (write-temp! (format "grepfolim/f%d.txt" i) "alpha\n"))
              d (temp-dir-path "grepfolim")
              out (:result (grep {"query" ["alpha"] "paths" [d] "is_files_only" true "limit" 2}))]

          (expect (= 2 (get out "file_count")))
          (expect (= "limit" (get out "hits_truncated_by")))
          ;; paging in this mode advances by FILES, since one row is one file
          (expect (= 2 (get out "next_offset")))))
    (it "renders one `path  N` row per file, with no numbered lines"
        (let [_ (write-temp! "grepforender/a.txt" "alpha\nalpha\n")
              d (temp-dir-path "grepforender")
              text
              ;; `files_only` is the accepted alias for the same option
              ((private-fn "render-grep-text")
                (:result (grep {"query" ["alpha"] "paths" [d] "files_only" true})))
              rows (filter #(string/includes? % "a.txt") (string/split-lines text))]

          (expect (string/includes? (first (string/split-lines text)) "2 hits · 1 file"))
          (expect (= 1 (count rows)))
          (expect (string/ends-with? (first rows) "  2"))
          (expect (not (string/includes? text "│")))))))

(defdescribe
  grep-regex-prefilter-degrade-test
  "A pattern `java.util.regex` runs but Rust's `regex` cannot NARROW with —
   lookaround, a backreference — answers the SAME hits for a DIRECTORY scope as
   for a single FILE scope. The directory used to answer `0 hits`: fff falls back
   to literal candidates, the literal matches nothing, and its own
   `:regex-fallback-error` never arrives with the empty result. A silent false
   negative in one scope and real hits in the other is exactly what teaches a
   caller to stop trusting grep."
  (let [grep (grep-data-fn)]
    (it "a lookahead pattern gives a directory the same hits as the file"
        (let [rel (write-temp! "grepdegrade/a.clj" "(defn grepx [] 1)\n(defn- grepx [] 2)\nplain\n")
              d (temp-dir-path "grepdegrade")
              pattern "defn(?!-)\\s+grepx"
              dir-out (:result (grep {"query" pattern "paths" [d] "is_regex" true}))
              file-out (:result (grep {"query" pattern "paths" [rel] "is_regex" true}))]

          (expect (= 1 (get dir-out "hit_count")))
          (expect (= (get file-out "hit_count") (get dir-out "hit_count")))
          (expect (= (mapv (comp str key) (get file-out "matches"))
                     (mapv (comp str key) (get dir-out "matches"))))
          ;; the widened scan is REPORTED, because it costs time
          (expect (string/includes? (str (get dir-out "hint")) "java.util.regex"))))
    (it "a backreference pattern also survives the directory scope"
        (let [_ (write-temp! "grepdegradeb/a.txt" "grepx grepx\nsomething else\n")
              d (temp-dir-path "grepdegradeb")
              out (:result (grep {"query" "(grepx) \\1" "paths" [d] "is_regex" true}))]

          (expect (= 1 (get out "hit_count")))))
    (it "a pattern repaired from a stray literal gives a directory the same hits as the file"
        (let [rel (write-temp! "grepdegradec/a.txt" "function f() {\n  g();\n});\n")
              d (temp-dir-path "grepdegradec")
              dir-out (:result (grep {"query" "^});" "paths" [d] "is_regex" true}))
              file-out (:result (grep {"query" "^});" "paths" [rel] "is_regex" true}))]

          (expect (= 1 (get dir-out "hit_count")))
          (expect (= 1 (get file-out "hit_count")))))
    (it "a pattern that does not COMPILE is still refused, never widened"
        (let [caught (try (grep
                            {"query" "defn[- ]?[a-z-]*root{3,1}" "paths" ["src"] "is_regex" true})
                          nil
                          (catch clojure.lang.ExceptionInfo e (ex-message e)))]
          (expect (string/includes? (str caught) "does not compile"))))))
