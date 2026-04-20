(ns com.blockether.vis.loop.runtime.core
  "SCI sandbox construction + var-index + hook system. This is the runtime
   wiring layer for rlm.sci:

   * `sci-update-binding!` / `create-sci-context` - build and mutate the
     sandbox where user code runs.
   * `build-var-index` - compact table describing user-def'd vars, shown
     to the LLM between iterations.
   * Hook system (`execute-tool`, `wrap-tool-for-sci`, `register-tool-def!`,
     etc.) - per-tool :before / :wrap / :after chains plus global
     observer hooks fired by the query loop."
  (:require
   [clojure.set]
   [clojure.string :as str]
   [clojure.walk]
   [clojure+.core]
   [clojure+.walk]
   [com.blockether.vis.loop.storage.db :as db]

   [com.blockether.vis.loop.tool :as sci-tool]
   [com.blockether.vis.loop.runtime.formatters :as fmt]
   [com.blockether.vis.loop.runtime.tools.core :as sci-tools]
   [com.blockether.vis.loop.runtime.tools.git :as sci-git]
   [com.blockether.vis.loop.knowledge.ontology :as ontology]
   [com.blockether.vis.loop.runtime.shared :as sci-shared
    :refer [EXTRA_BINDINGS ns->sci-map]]
   [com.blockether.vis.loop.runtime.tool-diagnostics :as tool-diag]
   [sci.addons.future :as sci-future]
   [sci.core :as sci]
   [taoensso.trove :as trove]))

;; =============================================================================
;; SCI Context Helpers
;; =============================================================================

(defn sci-update-binding!
  "Update a binding in an existing SCI context.
   Ensures the symbol is a real SCI var before interning the value,
   since bindings from sci/init :namespaces are not SCI vars.

   NOTE: This mutates the SCI sandbox only. If the caller wants the next
   iteration's `<var_index>` context block to reflect the new binding, they
   MUST also call `bump-var-index!` on the env — the var-index is cached and
   only rebuilds when `:current-revision` advances. Every past cache-staleness
   bug (4-iter `(restore-vars …)` spin, turn-3 `*query*=\"Siema\"` replay)
   traces back to forgetting this pair. Prefer `bind-and-bump!` below."
  [sci-ctx sym val]
  (let [ns-obj (sci/find-ns sci-ctx 'sandbox)]
    (sci/eval-string+ sci-ctx (str "(def " sym " nil)") {:ns ns-obj})
    (sci/intern sci-ctx ns-obj sym val)))

(defn bump-var-index!
  "Invalidate the env's cached `<var_index>` so the next `get-var-index` call
   rebuilds it from the live SCI sandbox. No-op when the env has no
   `:var-index-atom` (e.g. ad-hoc test contexts)."
  [env]
  (when-let [atom (:var-index-atom env)]
    (swap! atom update :current-revision (fnil inc 0))))

(defn bind-and-bump!
  "Atomic \"rebind var in SCI + invalidate var-index cache\" — the only API
   call sites should use when mutating runtime bindings that the LLM needs
   to see on the NEXT iteration. Fixes every instance of the model looping
   on `(restore-vars …)` / `(def X …)` because the var_index never caught up."
  [env sym val]
  (sci-update-binding! (:sci-ctx env) sym val)
  (bump-var-index! env))

;; =============================================================================
;; SCI Context Creation
;; =============================================================================

(defn- ->pattern
  "Promote `x` to a `java.util.regex.Pattern` if it's a string. Pass-through
   when it's already a Pattern. Shared helper behind all the auto-promoting
   regex wrappers below."
  ^java.util.regex.Pattern [x]
  (if (instance? java.util.regex.Pattern x)
    x
    (java.util.regex.Pattern/compile (str x))))

(defn- safe-split
  "Drop-in replacement for `clojure.string/split` that auto-promotes a string
   delimiter to a `java.util.regex.Pattern`. Clojure's native `str/split`
   requires a Pattern — LLMs frequently write `(str/split s \"\\n\")` and the
   error surfaces late (often inside a lazy seq realization), burning
   iterations. This wrapper matches what the LLM expects and never weakens the
   Pattern path."
  ([s re] (str/split s (->pattern re)))
  ([s re limit] (str/split s (->pattern re) limit)))

(defn- safe-re-find
  "Shadow for `clojure.core/re-find`: accepts a string pattern in addition to a
   `java.util.regex.Pattern`. Preserves Matcher-arity passthrough unchanged."
  ([x]
   (re-find x))
  ([re s]
   (re-find (->pattern re) (str s))))

(defn- safe-re-seq
  "Shadow for `clojure.core/re-seq`: accepts a string pattern too."
  [re s]
  (re-seq (->pattern re) (str s)))

(defn- safe-re-matches
  "Shadow for `clojure.core/re-matches`: accepts a string pattern too."
  [re s]
  (re-matches (->pattern re) (str s)))

(defn- banned-slurp
  "Sandbox `slurp` override that rejects every call. Agent-facing file
   reads go through `read-file` exclusively — line-numbered output, path
   sanity, size cap, offset/limit paging, and var-source tracking all
   live there. `slurp` returns raw bytes with none of that, making it a
   cache-coherency footgun: a var bound from `slurp` can't be validated
   against the filesystem the next iteration, so the agent silently
   trusts stale content. Better to refuse the call and point at
   `read-file`."
  [& _args]
  (throw (ex-info (str "slurp is banned in the sandbox — use (read-file \"path\") "
                    "or (read-file \"path\" offset limit). "
                    "read-file is the only sanctioned file read: line-numbered, "
                    "size-capped, symlink-safe, and tracked by <var_index> for "
                    "staleness between iterations.")
           {:type :tool/banned :tool 'slurp})))

(defn- format-printable
  "If `v` carries :rlm/format / :rlm/formatted metadata (attached by the tool
   wrapper), return its formatted string. Otherwise return `v` unchanged.

   Used by the sandbox `println` / `print` overrides to render tool results
   with their tool-specific formatter instead of pr-str'ing the raw map.
   Non-IObj values (strings, numbers, keywords) pass through untouched —
   they can't carry metadata and usually render cleanly with plain println."
  [v]
  (if (instance? clojure.lang.IObj v)
    (or (:rlm/formatted (meta v))
        (when-let [f (:rlm/format (meta v))]
          (try (f v) (catch Throwable _ v)))
        v)
    v))

(defn- sandbox-println
  "Sandbox replacement for clojure.core/println. Substitutes formatted strings
   for any arg that was produced by a tool with a :format-result-fn formatter.

   SCI rebinds `sci.core/out` (NOT Clojure's `*out*`) per-iteration via
   `sci/binding`. SCI's built-in println handles this by rebinding `*out*`
   from `@sci.core/out` inside the fn body — we do the same so stdout
   capture keeps working with our override in place."
  [& args]
  (binding [*out* @sci/out]
    (apply println (map format-printable args))))

(defn- sandbox-print
  "Sandbox replacement for clojure.core/print. Same substitution as
   sandbox-println, without the trailing newline."
  [& args]
  (binding [*out* @sci/out]
    (apply print (map format-printable args))))

(defn create-sci-context
  "Creates the SCI sandbox context with all available bindings.

   Params:
   `sub-rlm-query-fn` - Function for simple LLM text queries
   `db-info` - Database info map (can be nil)
   `conversation-ref` - Active conversation lookup ref (can be nil)
   `custom-bindings` - Map of symbol->value for custom bindings (can be nil)"
  [sub-rlm-query-fn _db-info _conversation-ref custom-bindings]
  (let [base-bindings {'sub-rlm-query sub-rlm-query-fn
                       'parse-date sci-shared/parse-date 'date-before? sci-shared/date-before?
                       'date-after? sci-shared/date-after?
                       'days-between sci-shared/days-between 'date-plus-days sci-shared/date-plus-days
                       'date-minus-days sci-shared/date-minus-days 'date-format sci-shared/date-format
                       'today-str sci-shared/today-str
                       ;; Formatter-aware println/print — substitute the tool's
                       ;; :format-result-fn output for args carrying :rlm/format meta.
                       ;; prn is intentionally NOT overridden: it's for data
                       ;; round-trip and must stay verbatim pr-str.
                       'println sandbox-println
                       'print sandbox-print
                       ;; LLM footgun shadows: auto-promote string→Pattern so
                       ;; (re-find "HITL" s), (re-seq "\\d+" s), etc. stop
                       ;; throwing ClassCastException late inside lazy seqs.
                       're-find safe-re-find
                       're-seq safe-re-seq
                       're-matches safe-re-matches
                       ;; `slurp` is BANNED. Every file read goes through
                       ;; `read-file` so var_index can track mtime/size and
                       ;; mark cached reads as `valid`/`stale`/`missing`
                       ;; between iterations. `slurp` bypassed all of that.
                       'slurp banned-slurp}
        all-bindings (merge EXTRA_BINDINGS base-bindings
                       (or custom-bindings {}))
        str-ns  (sci/create-ns 'clojure.string nil)
        set-ns  (sci/create-ns 'clojure.set nil)
        walk-ns (sci/create-ns 'clojure.walk nil)
        plus-ns (sci/create-ns 'clojure+.core nil)
        ;; Patch clojure.string/split so string delimiters auto-promote to
        ;; Patterns. The original raises a late ClassCastException when an
        ;; LLM passes a string, usually after the cast hides inside a lazy
        ;; seq that only realizes during answer/mustache rendering.
        str-ns-copied (assoc (sci/copy-ns clojure.string str-ns)
                        'split (sci/new-var 'split safe-split {:ns str-ns}))
        zp-resolve (fn [sym] (deref (requiring-resolve (symbol "zprint.core" (str sym)))))
        lt-resolve (fn [sym] (deref (requiring-resolve (symbol "lazytest.core" (str sym)))))
        sandbox-ns (sci/create-ns 'sandbox nil)
        sci-ctx (sci/init (sci-future/install {:namespaces {'sandbox (merge all-bindings
                                                                      {'cond+ (sci/copy-var clojure+.core/cond+ sandbox-ns)
                                                                       'if+ (sci/new-var 'if+
                                                                              (fn [_ _ bindings then & [else]]
                                                                                (list 'let [(first bindings) (second bindings)]
                                                                                  (list 'if (first bindings) then else)))
                                                                              {:macro true})
                                                                       'when+ (sci/new-var 'when+
                                                                                (fn [_ _ bindings & body]
                                                                                  (list 'let [(first bindings) (second bindings)]
                                                                                    (cons 'when (cons (first bindings) body))))
                                                                                {:macro true})})
                                                            'clojure.string str-ns-copied
                                                            'clojure.set (sci/copy-ns clojure.set set-ns)
                                                            'clojure.walk (sci/copy-ns clojure+.walk walk-ns)
                                                            'clojure+.core (sci/copy-ns clojure+.core plus-ns)
                                                            'fast-edn.core (ns->sci-map 'fast-edn.core)
                                                            'clojure.edn (ns->sci-map 'fast-edn.core)
                                                            'zprint.core {'zprint-str (zp-resolve 'zprint-str)
                                                                          'zprint (zp-resolve 'zprint)
                                                                          'czprint-str (zp-resolve 'czprint-str)
                                                                          'czprint (zp-resolve 'czprint)
                                                                          'zprint-file-str (zp-resolve 'zprint-file-str)
                                                                          'set-options! (zp-resolve 'set-options!)
                                                                          'configure-all! (zp-resolve 'configure-all!)}
                                                            'clojure.pprint {'pprint (zp-resolve 'zprint)
                                                                             'pprint-str (zp-resolve 'zprint-str)}
                                                            'lazytest.core {'expect-fn (lt-resolve 'expect-fn)
                                                                            'ok? (lt-resolve 'ok?)
                                                                            'throws? (lt-resolve 'throws?)
                                                                            'causes? (lt-resolve 'causes?)
                                                                            'causes-with-msg? (lt-resolve 'causes-with-msg?)}
                                                            'clojure.test {'is (lt-resolve 'expect-fn)
                                                                           'throws? (lt-resolve 'throws?)}
                                                            'charred.api (ns->sci-map 'charred.api)}
                                               :readers {'p (fn [form]
                                                               (list 'do
                                                                 (list 'println (str "#p " (pr-str form) " =>") (list 'pr-str form))
                                                                 form))}
                                               :ns-aliases {'str 'clojure.string
                                                            'edn 'fast-edn.core
                                                            'zp 'zprint.core
                                                            'pprint 'clojure.pprint
                                                            'pp 'clojure.pprint
                                                            'set 'clojure.set
                                                            'walk 'clojure.walk
                                                            'json 'charred.api
                                                            'lt 'lazytest.core
                                                            'test 'clojure.test
                                                            'c+ 'clojure+.core}
                                               :classes {'java.lang.Character Character
                                                         'java.lang.Math Math
                                                         'java.lang.String String
                                                         'java.lang.StringBuilder java.lang.StringBuilder
                                                         'java.lang.Integer Integer
                                                         'java.lang.Long Long
                                                         'java.lang.Double Double
                                                         'java.lang.Float Float
                                                         'java.lang.Byte java.lang.Byte
                                                         'java.lang.Short java.lang.Short
                                                         'java.lang.Boolean Boolean
                                                         'java.lang.Comparable java.lang.Comparable
                                                         'java.lang.Number java.lang.Number
                                                         'java.lang.Exception java.lang.Exception
                                                         'java.util.Collections java.util.Collections
                                                         'java.util.Arrays java.util.Arrays
                                                         'java.util.regex.Pattern java.util.regex.Pattern
                                                         'java.util.regex.Matcher java.util.regex.Matcher
                                                         'java.time.LocalDate java.time.LocalDate
                                                         'java.time.Period java.time.Period
                                                         'java.time.Instant java.time.Instant
                                                         'java.time.LocalDateTime java.time.LocalDateTime
                                                         'java.time.format.DateTimeFormatter java.time.format.DateTimeFormatter
                                                         'java.util.UUID java.util.UUID
                                                         'clojure.lang.PersistentQueue clojure.lang.PersistentQueue
                                                         'clojure.lang.BigInt clojure.lang.BigInt
                                                         'clojure.lang.Ratio clojure.lang.Ratio
                                                         'java.math.BigInteger java.math.BigInteger
                                                         'java.math.BigDecimal java.math.BigDecimal
                                                         'java.util.Base64 java.util.Base64
                                                         'java.net.URLEncoder java.net.URLEncoder
                                                         'java.net.URLDecoder java.net.URLDecoder
                                                         'java.util.Map java.util.Map
                                                         'java.util.List java.util.List
                                                         'java.util.HashMap java.util.HashMap
                                                         'java.util.LinkedHashMap java.util.LinkedHashMap
                                                         'java.util.ArrayList java.util.ArrayList
                                                         'java.util.Random java.util.Random}
                                               :imports '{Boolean java.lang.Boolean
                                                          Byte java.lang.Byte
                                                          Character java.lang.Character
                                                          Comparable java.lang.Comparable
                                                          Double java.lang.Double
                                                          Exception java.lang.Exception
                                                          Float java.lang.Float
                                                          Integer java.lang.Integer
                                                          Long java.lang.Long
                                                          Math java.lang.Math
                                                          Number java.lang.Number
                                                          Short java.lang.Short
                                                          String java.lang.String
                                                          StringBuilder java.lang.StringBuilder
                                                          Arrays java.util.Arrays
                                                          Collections java.util.Collections
                                                          UUID java.util.UUID
                                                          Pattern java.util.regex.Pattern
                                                          Matcher java.util.regex.Matcher
                                                          LocalDate java.time.LocalDate
                                                          LocalDateTime java.time.LocalDateTime
                                                          Instant java.time.Instant
                                                          DateTimeFormatter java.time.format.DateTimeFormatter
                                                          Period java.time.Period
                                                          PersistentQueue clojure.lang.PersistentQueue
                                                          BigInt clojure.lang.BigInt
                                                          Ratio clojure.lang.Ratio
                                                          BigInteger java.math.BigInteger
                                                          BigDecimal java.math.BigDecimal
                                                          Base64 java.util.Base64
                                                          URLEncoder java.net.URLEncoder
                                                          URLDecoder java.net.URLDecoder
                                                          Map java.util.Map
                                                          List java.util.List
                                                          HashMap java.util.HashMap
                                                          LinkedHashMap java.util.LinkedHashMap
                                                          ArrayList java.util.ArrayList
                                                          Random java.util.Random}
                                               ;; `slurp` intentionally NOT denied at the SCI level: we
                                               ;; shadow it in sandbox bindings with `banned-slurp`,
                                               ;; which throws a descriptive ex-info pointing at
                                               ;; `read-file`. Keeping it as a sandbox binding (not a
                                               ;; deny) gives the LLM a useful error message instead of
                                               ;; SCI's generic "not allowed". `spit` stays denied —
                                               ;; `write-file` is the audited path that renders diffs.
                                               ;;
                                               ;; `require`, `import`, `find-ns` are NOT denied either
                                               ;; (real Clojure reach for namespace discovery), but we
                                               ;; deliberately don't advertise them in tool docs so the
                                               ;; LLM's canonical playbook stays narrow.
                                               :deny '[ns eval load-string load-file
                                                       read-string
                                                       spit
                                                       intern
                                                       sh
                                                       *in* *out* *err* *command-line-args*]}))]
    (doseq [[sym doc args] [['sub-rlm-query "Ask a sub-LLM anything. Returns text or structured data." '([prompt] [prompt {:spec spec}])]
                            ['sub-rlm-query-batch "Parallel batch of LLM sub-calls. Returns vector of results." '([[prompt1 prompt2 ...]])]
                            ['request-more-iterations "Request n more iterations. Returns {:granted n :new-budget N}." '([n])]
                            ['context "The data context passed to query-env!." nil]
                            ['parse-date "Parse ISO date string to LocalDate." '([s])]
                            ['today-str "Today as ISO-8601 string." '([])]
                            ['search-documents "Search across documents. Returns markdown. No :in = search everywhere.\n  (search-documents \"query\")                ;; pages+toc\n  (search-documents \"query\" {:in :pages})    ;; pages only\n  (search-documents \"query\" {:in :toc})      ;; TOC only\n  Opts: :top-k :document-id :type" '([query] [query opts])]
                            ['fetch-document-content "Fetch full content by lookup ref.\n  [:node/id \"id\"] -> page text\n  [:doc/id \"id\"]  -> vector of ~4K char pages\n  [:toc/id \"id\"]  -> TOC entry description" '([lookup-ref])]
                            ['search-batch "Parallel multi-query search. Returns markdown. Deduplicates, ranks by vitality.\n  (search-batch [\"schemas\" \"modes\" \"treatment\"])\n  (search-batch [\"q1\" \"q2\"] {:top-k 5 :limit 20})" '([queries] [queries opts])]
                            ['conversation-history "List prior query summaries in the current conversation.\n  (conversation-history)\n  (conversation-history 5) ;; last 5 queries" '([] [n])]
                            ['conversation-code "Get prior query code blocks by query position or ref.\n  (conversation-code 0)\n  (conversation-code [:id uuid])" '([query-selector])]
                            ['conversation-results "Get prior query execution results and restorable vars.\n  (conversation-results 0)" '([query-selector])]
                            ['restore-var "Restore a persisted data var from a prior iteration, binding it in the sandbox.\n  (restore-var 'anomalies)  ;; binds anomalies and returns its value\n  Opts: {:max-scan-queries N} limits lookup to recent queries." '([sym] [sym opts])]
                            ['restore-vars "Batch restore persisted data vars, binding each success in the sandbox.\n  (restore-vars ['a 'b])  ;; returns {a val-a, b {:error {...}}} on partial failure\n  Opts: {:max-scan-queries N} limits lookup to recent queries." '([syms] [syms opts])]
                            ['var-history "All persisted versions of a var, oldest first.\n  (var-history 'anomalies)\n  Returns [{:version 1 :value <val> :code \"(def ...)\" :created-at #inst}]" '([sym])]
                            ['var-diff "Diff between two versions of a var. Dispatch by type:\n  Collections → structural editscript diff\n  Strings → unified line diff\n  Numbers → delta with pct-change\n  Others → simple replacement\n  (var-diff 'anomalies 1 3)\n  Returns {:from-version 1 :to-version 3 :type :structural|:string-diff|:number-delta|:replacement ...}" '([sym from-version to-version])]
                            ['git-search-commits "Query ingested git commits. All filters optional, AND semantics. Cross-repo - pass :document-id to scope.\n  (git-search-commits {:category :bug :since \"2025-06-01\" :path \"src/\" :author-email \"a@x\" :ticket \"SVAR-42\" :limit 20})" '([] [opts])]
                            ['git-commit-history "Recent commits across all ingested repos.\n  (git-commit-history {:limit 20})" '([] [opts])]
                            ['git-commits-by-ticket "Commits that reference a ticket.\n  (git-commits-by-ticket \"SVAR-42\")" '([ticket-ref])]
                            ['git-commit-parents "Parent SHAs of a commit (DB-backed, reads :parents).\n  (git-commit-parents \"abc123\")" '([sha])]
                            ['git-file-history "Commits touching a file, most-recent first. JGit, follows renames.\n  Single repo: (git-file-history \"src/foo.clj\")\n  Multi-repo:  (git-file-history \"/abs/path/svar/src/foo.clj\" {:n 10})" '([path] [path opts])]
                            ['git-blame "Per-line blame attribution, inclusive line range (1-indexed). Follows renames.\n  Single repo: (git-blame \"src/foo.clj\" 42 58)\n  Multi-repo:  (git-blame \"/abs/path/svar/src/foo.clj\" 42 58)\n  Returns vec of {:line :sha :short :author :email :date :content}." '([path from to])]
                            ['git-commit-diff "Unified patch for a commit. SHA auto-dispatches to owning repo. Refs like HEAD are ambiguous multi-repo.\n  (git-commit-diff \"abc123\")" '([sha])]]]
      (when (:val (sci/eval-string+ sci-ctx (str "(resolve '" sym ")") {:ns sandbox-ns}))
        (sci/eval-string+ sci-ctx
          (str "(def ^{:doc " (pr-str doc)
            (when args (str " :arglists (quote " (pr-str args) ")"))
            "} " sym " " sym ")")
          {:ns sandbox-ns})))

    {:sci-ctx sci-ctx
     :sandbox-ns sandbox-ns
     :initial-ns-keys (set (keys (:val (sci/eval-string+ sci-ctx "(ns-publics 'sandbox)" {:ns sandbox-ns}))))}))

;; =============================================================================
;; Var Index
;; =============================================================================

;; =============================================================================
;; Built-in tool registration (all go through register-tool-def! with activation-fns)
;; =============================================================================

(declare execute-tool register-tool-def! wrap-tool-for-sci)

(defn register-builtin-tools!
  "Register all built-in tools on an env via register-tool-def! + sci-update-binding!.
   Called once after env construction. Every tool gets an activation-fn
   that checks real DB state."
  [env]
  (let [db-info          (:db-info env)
        conversation-ref (:conversation-ref env)
        registry         (:tool-registry-atom env)
        sci-ctx          (:sci-ctx env)
        has-db?          (boolean db-info)
        has-conv?        (and has-db? (some? conversation-ref))
        register!        (fn [sym f tool-def]
                           (let [canonical (sci-tool/make-tool-def sym f tool-def)]
                             (register-tool-def! registry sym canonical)
                             (when sci-ctx
                               (sci-update-binding! sci-ctx sym
                                 (wrap-tool-for-sci env sym f registry)))))]
    ;; --- Document tools (active when DB has documents) ---
    (when has-db?
      (let [has-docs? (fn [env] (boolean (seq (db/db-list-documents (:db-info env)))))
            search-fn (sci-tools/make-search-documents-fn db-info)
            fetch-fn  (sci-tools/make-fetch-document-content-fn db-info)]
        (register! 'search-documents search-fn
          {:doc "(search-documents \"query\") or (search-documents \"query\" {:in :pages :top-k 20})"
           :group "documents" :activation-doc "no documents ingested"
           :activation-fn has-docs?
           :examples ["(search-documents \"neural network\")"
                      "(search-documents \"RLHF\" {:in :pages :top-k 20})"
                      "(search-documents \"alignment\" {:document-id \"doc-1\"})"]
           :prompt (fn [env]
                     (str "Full-text search across ingested documents. First stop for \"what do my docs say about X?\".\n"
                       "\n"
                       "- `:in :pages|:toc|:entities` — narrow scope (default: all).\n"
                       "- `:top-k` — max hits (default 10).\n"
                       "- `:document-id` — restrict to one document.\n"
                       "\n"
                       "Returns hits with ids ready for `fetch-document-content`. Search in English — translate non-EN queries first.\n"
                       "\n"
                       (let [n (try (count (db/db-list-documents (:db-info env)))
                                  (catch Throwable t
                                    (trove/log! {:level :warn
                                                 :data {:error (ex-message t)
                                                        :ex-data (ex-data t)
                                                        :class (.getName (class t))
                                                        :stack (mapv str (take 8 (.getStackTrace t)))}
                                                 :msg "db-list-documents count failed — search-documents prompt will report 0 ingested"})
                                    0))]
                          (str "Currently " n " document" (when (not= 1 n) "s") " ingested."))))})
        (register! 'fetch-document-content fetch-fn
          {:doc "(fetch-document-content [:node/id \"id\"]) or [:doc/id \"id\"] or [:toc/id \"id\"]"
           :group "documents" :activation-doc "no documents ingested"
           :activation-fn has-docs?
           :examples ["(fetch-document-content [:node/id \"page-42\"])"
                      "(fetch-document-content [:doc/id \"doc-1\"])"
                      "(fetch-document-content [:toc/id \"toc-3\"])"]
           :prompt "Fetch content behind a `search-documents` hit. Refs: `[:node/id id]` page text, `[:doc/id id]` full doc as pages, `[:toc/id id]` TOC desc, `[:id id]` generic entity."})
        (register! 'search-batch
          (fn search-batch
            ([queries] (when db-info (sci-tools/format-docs (db/db-search-batch db-info queries))))
            ([queries opts] (when db-info (sci-tools/format-docs (db/db-search-batch db-info queries opts)))))
          {:doc "(search-batch [\"q1\" \"q2\"]) — batch search across pages and TOC"
           :group "documents" :activation-doc "no documents ingested"
           :activation-fn has-docs?
           :examples ["(search-batch [\"neural\" \"RLHF\" \"alignment\"])"
                      "(search-batch [\"q1\" \"q2\"] {:top-k-per-query 5})"]
           :prompt "Parallel `search-documents`. Takes a vector of query strings, returns a merged deduplicated hit list. Use when the task wants evidence across several terms."})))
    ;; --- Conversation history tools (active when conversation exists) ---
    (when has-conv?
      (let [has-history? (fn [env] (boolean (:conversation-ref env)))]
        (register! 'conversation-history
          (sci-tools/make-conversation-history-fn db-info conversation-ref)
          {:doc "(conversation-history) or (conversation-history n) — prior query summaries"
           :group "conversation" :activation-doc "no active conversation"
           :activation-fn has-history?
           :examples ["(conversation-history)"
                      "(conversation-history 5)"]
           :format-result-fn fmt/format-conversation-history
           :prompt "Summaries of prior turns in this conversation (text, answer preview, iter count, key vars). Use to decide whether to drill via `conversation-code`/`conversation-results`/`restore-var`."})
        (register! 'conversation-code
          (sci-tools/make-conversation-code-fn db-info conversation-ref)
          {:doc "(conversation-code query-selector) — prior query code blocks"
           :group "conversation" :activation-doc "no active conversation"
           :activation-fn has-history?
           :examples ["(conversation-code :last)"
                      "(conversation-code {:index 0})"]
           :format-result-fn fmt/format-conversation-code
           :prompt "Code blocks from a prior turn. Selector: `:last`, `{:index N}`, `{:query-id id}`. Shows HOW the answer was computed; pair with `conversation-results` for values."})
        (register! 'conversation-results
          (sci-tools/make-conversation-results-fn db-info conversation-ref)
          {:doc "(conversation-results query-selector) — prior query results"
           :group "conversation" :activation-doc "no active conversation"
           :activation-fn has-history?
           :examples ["(conversation-results :last)"
                      "(conversation-results {:index 1})"]
           :format-result-fn fmt/format-conversation-results
           :prompt "Values produced by a prior turn's iterations. Selector: see `conversation-code`. For one var prefer `restore-var`/`var-history`."})))
    ;; --- Restore tools (active when conversation has prior queries) ---
    ;; Special: restore-var also rebinds the value into the SCI sandbox.
    (when has-conv?
      (let [raw-restore-fn (sci-tools/make-restore-var-fn db-info conversation-ref)
            binding-restore-var (fn binding-restore-var
                                  ([sym] (binding-restore-var sym {}))
                                  ([sym opts]
                                   (let [val (raw-restore-fn sym opts)]
                                     (bind-and-bump! env sym val)
                                     val)))
            binding-restore-vars (fn binding-restore-vars
                                   ([syms] (binding-restore-vars syms {}))
                                   ([syms opts]
                                    (into {}
                                      (map (fn [sym]
                                             (try [sym (binding-restore-var sym opts)]
                                               (catch Exception e
                                                 [sym {:error {:type (:type (ex-data e))
                                                               :symbol sym
                                                               :message (ex-message e)}}]))))
                                      syms)))
            has-vars? (fn [env]
                        (boolean (:conversation-ref env)))]
        (register! 'restore-var binding-restore-var
          {:doc "(restore-var 'sym) — fetch + rebind persisted var from prior iterations"
           :group "conversation" :activation-doc "no persisted vars from prior queries"
           :activation-fn has-vars?
           :examples ["(restore-var 'docs)"
                      "(restore-var 'docs {:version 2})"]
           :prompt "Pull a var from an earlier turn into the current SCI sandbox. Rebinds it to the latest value so subsequent iterations reference it without recomputing. Pass `{:version N}` for a specific historical version (see `var-history`)."})
        (register! 'restore-vars binding-restore-vars
          {:doc "(restore-vars ['sym1 'sym2]) — batch restore + rebind"
           :group "conversation" :activation-doc "no persisted vars from prior queries"
           :activation-fn has-vars?
           :examples ["(restore-vars ['docs 'hits 'analysis])"
                      "(restore-vars ['x 'y] {:version 1})"]
           :format-result-fn fmt/format-restore-vars
           :prompt "Parallel `restore-var`. Returns `{sym value}` or `{sym {:error …}}` for missing ones. Use when you know upfront which vars you need."})
         (register! 'var-history
           (sci-tools/make-var-history-fn db-info conversation-ref)
           {:doc "(var-history 'sym) — all persisted versions of a var, oldest first. Each: {:version N :value :code :created-at}. For the last N: `(take-last N (var-history 'sym))`."
            :group "conversation" :activation-doc "no persisted vars from prior queries"
            :activation-fn has-vars?
            :examples ["(var-history 'hits)"
                       "(var-history '*reasoning*)"
                       ;; Last 3 reasonings — the standard idiom when
                       ;; <prior_thinking>'s breadcrumb says to go
                       ;; deeper than the default window.
                       "(take-last 3 (var-history '*reasoning*))"
                       ;; Just the values, dropping metadata.
                       "(mapv :value (take-last 4 (var-history '*reasoning*)))"]
            :format-result-fn fmt/format-var-history
            :prompt (str "Every version of a var across this conversation, oldest first — "
                      "`[{:version N :value :code :created-at}]`. Two common idioms:\n"
                      "  `(take-last 3 (var-history 'sym))` — most recent 3 versions (the "
                      "typical need when <prior_thinking>'s breadcrumb points here).\n"
                      "  `(mapv :value (take-last N (var-history 'sym)))` — just the values, "
                      "dropping metadata (handy for recomposing a window of `*reasoning*`).\n"
                      "Use before `restore-var` when you need a specific version, or to audit "
                      "how a value evolved across iterations.")})
        (register! 'var-diff
          (sci-tools/make-var-diff-fn db-info conversation-ref)
          {:doc "(var-diff 'sym 1 3) — structural diff between two versions. Returns {:edits [...] :edit-count N}"
           :group "conversation" :activation-doc "no persisted vars from prior queries"
           :activation-fn has-vars?
           :examples ["(var-diff 'state 1 3)"
                      "(var-diff 'plan :prev :latest)"]
           :format-result-fn fmt/format-var-diff
           :prompt "Structural diff between two versions of a var. Versions can be ints or `:prev`/`:latest`. Returns `{:edits … :edit-count N}`. Use to answer \"what changed between iter X and Y?\" on large maps/vecs without re-reading both."})))
    ;; --- Git tools (active when repos are attached) ---
    (when has-db?
      (let [has-repos?    (fn [env] (boolean (seq (db/db-list-repos (:db-info env)))))
            git-binds     (sci-git/make-git-sci-bindings db-info)
            ;; Per-tool formatter dispatch. All commit-list tools share one
            ;; formatter; git-commit-diff returns a string (default) so we
            ;; don't override it.
            git-formatter (fn [sym]
                            (case sym
                              git-search-commits   fmt/format-commit-list
                              git-commit-history   fmt/format-commit-list
                              git-commits-by-ticket fmt/format-commit-list
                              git-file-history     fmt/format-commit-list
                              git-commit-parents   fmt/format-commit-parents
                              git-blame            fmt/format-blame
                              nil))
            ;; Each git tool carries its own short prompt AND a header line
            ;; listing currently-attached repos so the LLM knows which repo
            ;; names / paths to pass as the optional `opts`.
            repo-summary-fn (fn [env]
                              (let [repos (try (db/db-list-repos (:db-info env))
                                            (catch Throwable _ []))]
                                (if (seq repos)
                                  (str "Attached repo"
                                    (when (not= 1 (count repos)) "s") ":\n"
                                    (->> repos
                                      (map (fn [{:keys [name path branch head-short commits-ingested]}]
                                             (str "  - " name " (" branch " @ " head-short
                                               ", " commits-ingested " commits) "
                                               "at " path)))
                                      (str/join "\n")))
                                  "No repos attached right now.")))
            ;; All git tools share the same repo roster + the absolute-
            ;; paths caveat. Rendered once as the group's
            ;; <group-preamble>; sibling prompts no longer paste either.
            git-group-preamble (fn [env]
                                 (str (repo-summary-fn env)
                                   "\n\nNote: repo paths must be ABSOLUTE strings in any `:path` opt."))
            tool-prompts {'git-search-commits
                          "Full-text search across commit messages. Prefer one search with an alternation (`\"HITL|approval|confirm\"`) over multiple searches. Narrow with `:author`, `:since`, `:until`."
                          'git-commit-history
                          "Linear commit history for a repo (or a path within it). Narrow with `{:path \"src/foo.clj\" :n 20}`."
                          'git-commits-by-ticket
                          "Find every commit mentioning a ticket id / key. Use when the user cites a JIRA/issue number."
                          'git-commit-parents
                          "Parents of a given commit (SHA or short SHA). Merge commits have 2+ parents."
                          'git-file-history
                          "Commits that touched a specific file path. Pair with `git-blame` for \"when did this line appear?\"."
                          'git-blame
                          "Line-by-line authorship for a file range. Answers \"who last touched line N?\"."
                          'git-commit-diff
                          "Full diff for one commit (SHA or short SHA). Returns patch text — scan for the relevant hunks."}]
        (doseq [[sym f] git-binds]
          (register! sym f
            (cond-> {:doc (str "(" sym " ...) — git tool")
                     :group "git" :activation-doc "no git repos attached"
                     :activation-fn has-repos?
                     :group-preamble git-group-preamble
                     :prompt (get tool-prompts sym "Git tool.")}
              (git-formatter sym) (assoc :format-result-fn (git-formatter sym)))))))
    ;; --- Concept tools (active when concepts exist — cross-conversation) ---
    (when has-db?
      (let [has-concepts?     (fn [env] (boolean (seq (db/list-concepts (:db-info env)))))
            concept-binds     (ontology/make-concept-graph-bindings db-info)
            concept-formatter (fn [sym]
                                (case sym
                                  concept-info    fmt/format-concept-info
                                  remove-concept  fmt/format-concept-mutation
                                  edit-concept    fmt/format-concept-mutation
                                  nil))
            concept-prompts {'concept-info
                             "Inspect a concept in the cross-conversation ontology. Returns definition, aliases, related concepts, and source references. Use when the user mentions a term — prefer the project's shared understanding over guessing."
                             'remove-concept
                             "Soft-delete a concept. Sets `status = removed` without dropping the row. Use when a concept is obsolete or was incorrectly extracted."
                             'edit-concept
                             "Update a concept's definition / aliases / relationships. Only provided fields are changed."}]
        (doseq [[sym f] concept-binds]
          (register! sym f
            (cond-> {:doc (str "(" sym " ...) — concept graph tool")
                     :group "concepts" :activation-doc "no concepts extracted yet"
                     :activation-fn has-concepts?
                     :prompt (or (get concept-prompts sym)
                               "Concept graph tool.")}
              (concept-formatter sym) (assoc :format-result-fn (concept-formatter sym)))))))
    ;; Update initial-ns-keys so get-locals excludes built-in tools
    (when sci-ctx
      (let [current-keys (set (keys (:val (sci/eval-string+ sci-ctx "(ns-publics 'sandbox)"
                                            {:ns (:sandbox-ns env (get-in @(:env sci-ctx) [:namespaces 'sandbox]))}))))]
        (assoc env :initial-ns-keys current-keys)))))

;; MAX_VAR_INDEX_ROWS was removed — every defined var shows up in
;; <var_index>. With the `code` column (bounded to
;; MAX_VAR_INDEX_CODE_CHARS per row) instead of raw value previews,
;; even dozens of vars stay cheap to render.
(def ^:private ^:const MAX_VAR_INDEX_COUNT 1000)
(def ^:private ^:const MAX_VAR_INDEX_CODE_CHARS
  "Per-row budget for the `code` column in <var_index>. We show the
   CODE that produced each var (not a preview of its value) — the
   journal already carries full values for the last iteration, and
   the model can always re-reference a var by name to materialize
   its value into the next journal. Code is bounded by what the
   model typed, so this cap is only a safety rail against pathological
   one-liners, not a lossy summary."
  400)

(def ^:private SYSTEM_VAR_CODE_PLACEHOLDER
  "(SYSTEM — bound each turn by the agent loop)")

;;; ── Var freshness (tool-owned) ──────────────────────────────────────
;;;
;;; Every tool that produces a CACHEABLE result (file reads, git
;;; snapshots, HTTP fetches) opts into the freshness channel by
;;; setting `` in its tool-def and providing
;;; a `:metadata-fn` fn. The fn is the SOLE owner of freshness logic
;;; for its tool — there are no fallbacks, no central parse-expr
;;; heuristics, no `extract-metadata` regex tricks.
;;;
;;; Contract — `:metadata-fn` is a single-arg fn:
;;;
;;;   (fn [{:keys [args result metadata]}]
;;;     => {:metadata <updated-map> :fresh? <bool>})
;;;
;;; Called at TWO phases:
;;;
;;;   a) Seed (right after the tool returns):
;;;      `{:args <tool-args> :result <tool-result> :metadata nil}`
;;;      → must populate `:metadata` from args/result and return
;;;        `:fresh? true` (we just observed the source, by
;;;        definition fresh).
;;;
;;;   b) Re-check (when <var_index> is rendered on a later turn):
;;;      `{:args nil :result nil :metadata <stored-snapshot>}`
;;;      → must re-consult the underlying source (stat the file,
;;;        query git, hit the URL) and return new `:metadata` + a
;;;        fresh `:fresh?` bool.
;;;
;;; Enforcement lives in `register-tool-def!` (see bottom of this
;;; file): `` without a callable
;;; `:metadata-fn` throws on registration, not on first use.

;; No ambient state — metadata seeding happens once, AFTER the SCI
;; eval, inside `restorable-var-snapshots`: each defining expression
;; is parsed to surface its top-level tool symbol + args, and if the
;; tool's registration has `` we invoke
;; `(:metadata-fn tool-def)` with {:args :result :metadata nil} to
;; seed. The resulting map is stored on the var snapshot and persists
;; through `store-iteration!` into the iteration_var_attrs.code map.
;; At render time `build-var-index` looks up the same :metadata-fn fn
;; (via the `:tool` key in the stored metadata) and calls it with
;; the stored metadata for the re-check. One contract, two phases,
;; no dynamic state.

(defn parse-rich-code
  "Decode the `code` column from the `iteration_var_attrs` DB row into
   a uniform map `{:expr :time-ms :metadata}`.

   New writes use `pr-str {:expr … :time-ms … :metadata …}`. Legacy
   writes (and user-supplied raw expressions) are bare strings. We
   accept both. Unparseable inputs degrade to `{:expr <raw>}` so we
   never lose provenance."
  [code-col]
  (cond
    (nil? code-col) nil

    (map? code-col)
    code-col

    (string? code-col)
    (if (str/starts-with? (str/triml code-col) "{")
      (try
        (let [parsed (read-string {:read-cond :allow :features #{:clj}} code-col)]
          (if (map? parsed) parsed {:expr code-col}))
        (catch Throwable _ {:expr code-col}))
      {:expr code-col})

    :else nil))

(defn- render-var-code
  "Render the `code` column entry for one var. We show CODE (plus
   `:time-ms` when known), not a preview of the value — the journal
   already carries the previous iteration's full values and the model
   can reference any var by name to materialize its current value
   into the NEXT journal. Preview columns forced the model to re-read
   files it had already stored (see conversation 1f44852d-…) because
   a bounded slice is ambiguous for structured data like source files
   or hiccup trees."
  [{:keys [system? expr time-ms]}]
  (cond
    system? SYSTEM_VAR_CODE_PLACEHOLDER

    (and (string? expr) (not (str/blank? expr)))
    (let [trimmed (sci-shared/truncate (str/trim expr) MAX_VAR_INDEX_CODE_CHARS)]
      (if time-ms
        (str trimmed " [" time-ms "ms]")
        trimmed))

    :else "-"))

(defn- render-var-freshness
  "Render the `freshness` column by asking the producing tool's own
   `:metadata-fn` fn. `metadata` carries `:tool` (string name) from seed
   time; we use it to look up the current tool-def in `tool-registry`
   and invoke its `:metadata-fn` with `{:args nil :result nil :metadata
   stored}`. The fn returns `{:fresh? bool …}` — we translate that
   boolean into the column label.

   Error handling: `{:type :rlm.freshness/missing}` is the TOOL's
   signal that the backing resource vanished (file deleted, URL 404,
   git ref gone) — we render `MISSING`. Any OTHER exception indicates
   a bug in the :metadata-fn fn itself and propagates uncaught so the
   loop logs it instead of silently disguising a tool bug as a
   missing file.

   SYSTEM vars and anything without stored metadata (or without a
   registered tool) render as `-`."
  [{:keys [metadata system? tool-registry]}]
  (cond
    system? "-"

    (and (map? metadata) (string? (:tool metadata)))
    (let [tool-sym (symbol (:tool metadata))
          tool-def (get tool-registry tool-sym)]
      (if-let [fresh-fn (:metadata-fn tool-def)]
        (try
          (let [{:keys [freshness?]} (fresh-fn {:args nil :result nil :metadata metadata})]
            (if freshness? "fresh" "STALE"))
          (catch clojure.lang.ExceptionInfo ex
            (if (= :rlm.freshness/missing (:type (ex-data ex)))
              "MISSING"
              (throw ex))))
        "-"))

    :else "-"))

(defn- render-var-status
  "Render the `status` column entry — a GC-grade lifecycle hint for
   the agent.

   Values:
     live       — bound in the current sandbox; usable by name.
     forgotten  — dropped from the sandbox (via :forget or auto-forget)
                  but the DB row survives; `(restore-var 'sym)` brings
                  it back.
     SYSTEM     — earmuffed (*query*, *reasoning*, …) — cannot be
                  forgotten, never expires.

   The agent uses this to decide whether to reclaim a persisted var
   instead of recomputing, and which vars are safe to ignore."
  [{:keys [system? persisted?]}]
  (cond
    system?    "SYSTEM"
    persisted? "forgotten"
    :else      "live"))

(defn build-var-index
  "Builds a formatted var index table from user-def'd vars in the SCI context.
   Filters out initial bindings (tools, helpers) using initial-ns-keys.
   Returns nil if no user vars exist.

   Row format: `name | ver | type | size | status | freshness | doc | code`

   The `code` column shows the DEFINING expression for each var plus
   its execution time in ms (`(def x …) [42ms]`) so the model has
   complete provenance without a lossy value preview. For the raw
   value the LLM references the var by name in its own code — the
   next <journal> then shows the full materialized value.

   The `freshness` column shows `fresh` / `STALE` / `MISSING` / `-`
   by invoking the producing tool's `:metadata-fn` fn (registered via
   `register-tool-def!` with ``). The
   stored `:metadata` carries a `:tool` key that names the producing
   tool; `render-var-freshness` looks its fn up in `:tool-registry`
   and calls it with the stored snapshot. No central freshness logic
   lives here — every tool owns its own `:metadata-fn` contract.

   All user vars appear — no `MAX_VAR_INDEX_ROWS` cap. SYSTEM vars
   sort first; the rest are newest-touched first.

   Opts (6-arity):
     :include-persisted? — when false, rows persisted in DB but NOT in
       the live sandbox are suppressed. Set by sub-RLM calls so a
       forked env only sees its own snapshot + its own new vars; it
       never leaks sibling queries' vars into the sub's <var_index>.
       Defaults to true (main RLM — full conversation view)."
  ([sci-ctx initial-ns-keys]
   (build-var-index sci-ctx initial-ns-keys nil nil nil nil))
  ([sci-ctx initial-ns-keys sandbox]
   (build-var-index sci-ctx initial-ns-keys sandbox nil nil nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-ref]
   (build-var-index sci-ctx initial-ns-keys sandbox db-info conversation-ref nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-ref
    {:keys [include-persisted? tool-registry] :or {include-persisted? true}}]
   (try
     (let [sandbox-map (or sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox]))
           var-registry (when (and db-info conversation-ref)
                          (try (db/db-latest-var-registry db-info conversation-ref)
                            (catch Exception _ nil)))
           recency-of (fn [sym]
                        (if-let [ts (some-> (get var-registry sym) :created-at)]
                          (cond (inst? ts) (inst-ms ts)
                            (integer? ts) (long ts)
                            :else Long/MAX_VALUE)
                          Long/MAX_VALUE))
           ;; The `code` column in the DB carries rich `{:expr :time-ms
           ;; :metadata}` maps for new writes and bare strings for
           ;; legacy rows — `parse-rich-code` smooths the two.
           rich-code-for (fn [sym]
                           (parse-rich-code (get-in var-registry [sym :code])))
           live-info (into {}
                       (for [[s v] sandbox-map
                             :when (symbol? s)]
                         (let [rich (rich-code-for s)]
                           [s {:val (if (instance? clojure.lang.IDeref v) @v v)
                               :doc (:doc (meta v))
                               :arglists (:arglists (meta v))
                               :version (get-in var-registry [s :version] 1)
                               :expr (or (:expr rich) (:rlm/def-source (meta v)))
                               :time-ms (:time-ms rich)
                               :metadata (:metadata rich)}])))
           ;; Sub-RLMs (env with :parent-iteration-ref set → caller
           ;; passes :include-persisted? false) see ONLY their forked
           ;; sandbox. Sibling queries in the same conversation must
           ;; not leak into a sub-RLM's <var_index>, otherwise a
           ;; sub-sub-RLM would drown in its grandparent's junk.
           persisted-info (when (and var-registry include-persisted?)
                            (into {}
                              (for [[sym _] var-registry
                                    :when (and (symbol? sym)
                                            (not (contains? live-info sym))
                                            (not (contains? initial-ns-keys sym)))]
                                (let [rich (rich-code-for sym)]
                                  [sym {:val ::persisted
                                        :doc (str "persisted - (restore-var '" sym ") to load")
                                        :expr (:expr rich)
                                        :time-ms (:time-ms rich)
                                        :metadata (:metadata rich)}]))))
            var-info (merge persisted-info live-info)
            earmuffed? (fn [sym]
                         (let [n (name sym)]
                           (and (> (count n) 2)
                             (str/starts-with? n "*")
                             (str/ends-with? n "*"))))
             system-doc (fn [sym]
                          (case (str sym)
                            "*query*"     "(SYSTEM, never forgotten) current user query"
                            "*reasoning*" "(SYSTEM, never forgotten) YOUR thinking from the previous iteration"
                            "*answer*"    "(SYSTEM, never forgotten) final answer from the previous turn in this conversation"
                            "(SYSTEM, never forgotten) agent-bound var"))
            entries (->> var-info
                      (remove (fn [[sym _]] (contains? initial-ns-keys sym)))
                      (sort-by (fn [[sym _]]
                                 [(if (earmuffed? sym) 0 1)
                                  (- (long (recency-of sym)))
                                  (str sym)]))
                      (mapv (fn [[sym {:keys [val doc arglists expr time-ms metadata]}]]
                              (let [persisted? (= val ::persisted)
                                    system?    (earmuffed? sym)
                                    type-label (cond
                                                 persisted? "persisted"
                                                 (nil? val) "nil"
                                                 (fn? val) (if arglists (str "fn " arglists) "fn")
                                                 (map? val) "map"
                                                 (vector? val) "vector"
                                                 (set? val) "set"
                                                 (sequential? val) "seq"
                                                 (string? val) "string"
                                                 (integer? val) "int"
                                                 (float? val) "float"
                                                 (boolean? val) "bool"
                                                 (keyword? val) "keyword"
                                                 (symbol? val) "symbol"
                                                 :else (.getSimpleName (class val)))
                                   size (cond
                                          persisted? "-"
                                          (nil? val) "-"
                                          (string? val) (str (count val) " chars")
                                          (or (map? val) (vector? val) (set? val))
                                          (str (count val) " items")
                                          (sequential? val)
                                          (let [n (bounded-count MAX_VAR_INDEX_COUNT val)]
                                            (if (= n MAX_VAR_INDEX_COUNT)
                                              (str MAX_VAR_INDEX_COUNT "+ items")
                                              (str n " items")))
                                          :else "-")
                                   code-col      (render-var-code {:system? system? :expr expr :time-ms time-ms})
                                   freshness-col (render-var-freshness {:system? system?
                                                                        :metadata metadata
                                                                        :tool-registry tool-registry})
                                   status-col    (render-var-status {:system? system? :persisted? persisted?})
                                   version (get-in var-info [sym :version] 1)
                                   ver-str (str version)]
                                {:name (str sym) :ver ver-str :type type-label :size size
                                 :status status-col
                                 :fresh? freshness-col
                                 :doc (cond
                                        system? (sci-shared/truncate (system-doc sym) 80)
                                        doc (sci-shared/truncate doc 80)
                                        :else "-")
                                 :code code-col
                                 :system? system?}))))]
       (when (seq entries)
         (let [visible entries   ;; full index — no MAX_VAR_INDEX_ROWS cap
               max-name (max 4 (apply max (map #(count (:name %)) visible)))
               max-ver  (max 3 (apply max (map #(count (:ver %)) visible)))
               max-type (max 4 (apply max (map #(count (:type %)) visible)))
               max-size (max 4 (apply max (map #(count (:size %)) visible)))
               max-status (max 6 (apply max (map #(count (:status %)) visible)))
               max-freshness (max 9 (apply max (map #(count (:fresh? %)) visible)))
               max-doc  (max 3 (apply max (map #(count (:doc %)) visible)))
               pad (fn [s n] (str s (apply str (repeat (max 0 (- n (count s))) \space))))
               header (str "  " (pad "name" max-name) " | " (pad "ver" max-ver)
                        " | " (pad "type" max-type) " | " (pad "size" max-size)
                        " | " (pad "status" max-status)
                        " | " (pad "freshness" max-freshness) " | " (pad "doc" max-doc)
                        " | code")
               sep (str "  " (apply str (repeat max-name \-)) "-+-"
                     (apply str (repeat max-ver \-)) "-+-"
                     (apply str (repeat max-type \-)) "-+-"
                     (apply str (repeat max-size \-)) "-+-"
                     (apply str (repeat max-status \-)) "-+-"
                     (apply str (repeat max-freshness \-)) "-+-"
                     (apply str (repeat max-doc \-)) "-+---------")
               rows (mapv (fn [{:keys [name ver type size status freshness doc code]}]
                            (str "  " (pad name max-name)
                              " | " (pad ver max-ver)
                              " | " (pad type max-type)
                              " | " (pad size max-size)
                              " | " (pad status max-status)
                              " | " (pad freshness max-freshness)
                              " | " (pad doc max-doc)
                              " | " code))
                      visible)]
           (str/join "\n" (concat [header sep] rows)))))
     (catch Exception _ nil))))

;; =============================================================================
;; Hook System v3 - per-tool + global hooks with policy/observation split
;; =============================================================================
;;
;; Per-tool hooks form three chains: :before / :wrap / :after.
;; Global hooks live on query-env! opts and are pure observers.

(def MAX_HOOK_DEPTH
  "Ceiling on :invoke recursion depth per top-level tool dispatch."
  8)

(def DEFAULT_QUERY_CTX
  "Fallback query context used by register-env-fn!'s immediate SCI flash
   when no query-env! call is active."
  {:hooks nil
   :iteration-atom nil
   :depth 0
   :parent-dispatch-id nil})

(defn- status-id
  [status]
  (when status
    (keyword "rlm.status" (name status))))

(defn- ensure-error-map
  [err default-id]
  (cond
    (nil? err) nil
    (map? err) (cond-> err
                 (not (:error-id err)) (assoc :error-id default-id))
    :else {:type :rlm/unknown-tool-error
           :error-id default-id
           :message (str err)}))



(defn- gen-anon-id
  [stage]
  (gensym (str "anon/" (name stage) "-")))

(defn- normalize-hook-entry
  [stage entry]
  (cond
    (fn? entry)
    {:id (gen-anon-id stage) :fn entry}

    (and (map? entry) (fn? (:fn entry)))
    (update entry :id #(or % (gen-anon-id stage)))

    :else
    (throw (ex-info (str "Invalid hook entry for :" (name stage) " - "
                      "must be a fn or {:id :fn} map")
             {:type :rlm/invalid-hook-entry :stage stage :entry entry}))))

(defn normalize-hooks
  "Coerce a hook spec for one stage into a canonical vec of {:id :fn} maps."
  [stage hooks]
  (cond
    (nil? hooks) []
    (fn? hooks) [(normalize-hook-entry stage hooks)]
    (map? hooks) [(normalize-hook-entry stage hooks)]
    (sequential? hooks) (mapv #(normalize-hook-entry stage %) hooks)
    :else
    (throw (ex-info (str "Invalid hooks shape for :" (name stage))
             {:type :rlm/invalid-hooks-shape :stage stage :hooks hooks}))))

(defn merge-hook-chain
  "Merge incoming hook entries into an existing chain by :id."
  [existing incoming]
  (let [incoming-by-id (into {} (map (juxt :id identity)) incoming)
        merged (mapv (fn [entry]
                       (if-let [updated (get incoming-by-id (:id entry))]
                         updated
                         entry))
                 existing)
        existing-ids (set (map :id existing))
        appended (vec (remove #(contains? existing-ids (:id %)) incoming))]
    (vec (concat merged appended))))

(defn- merge-tool-def-hooks
  [existing new-def]
  (let [new-before (normalize-hooks :before (:before new-def))
        new-after  (normalize-hooks :after  (:after  new-def))
        new-wrap   (normalize-hooks :wrap   (:wrap   new-def))
        old-hooks  (:hooks existing {})]
    (-> new-def
      (assoc :hooks {:before (merge-hook-chain (:before old-hooks []) new-before)
                     :after  (merge-hook-chain (:after old-hooks [])  new-after)
                     :wrap   (merge-hook-chain (:wrap old-hooks [])   new-wrap)})
      (dissoc :before :after :wrap))))

(defn- run-before-chain
  [hooks invocation]
  (reduce
    (fn [acc {:keys [id fn]}]
      (let [current-inv (assoc invocation :args (:args acc))]
        (try
          (let [ret (fn current-inv)]
            (cond
              (or (nil? ret) (= {} ret))
              acc

              (and (map? ret) (contains? ret :error))
              (reduced (assoc acc
                         :short-circuit {:result nil
                                         :error (ensure-error-map (:error ret)
                                                  :rlm.error/hook-before-returned-error)}
                         :skipped? true))

              (and (map? ret) (contains? ret :skip))
              (reduced (assoc acc
                         :short-circuit {:result (:skip ret) :error nil}
                         :skipped? true))

              (and (map? ret) (contains? ret :args))
              (assoc acc :args (:args ret))

              :else
              (do (trove/log! {:level :warn
                               :data {:stage :before :id id :ret ret}
                               :msg "Before hook returned unknown shape; ignoring"})
                acc)))
          (catch Throwable t
            (reduced (assoc acc
                       :short-circuit {:result nil
                                       :error {:type :hook-exception
                                               :error-id :rlm.error/hook-exception
                                               :message (ex-message t)
                                               :stage :before
                                               :id id}}
                       :skipped? true))))))
    {:args (:args invocation) :short-circuit nil :skipped? false}
    hooks))

(defn- run-after-chain
  [hooks initial-outcome]
  (reduce
    (fn [outcome {:keys [id fn]}]
      (try
        (let [ret (fn outcome)]
          (cond
            (or (nil? ret) (= {} ret))
            outcome

            (and (map? ret) (or (contains? ret :result) (contains? ret :error)))
            (cond-> outcome
              (contains? ret :result) (assoc :result (:result ret))
              (contains? ret :error)  (assoc :error
                                        (ensure-error-map (:error ret)
                                          :rlm.error/hook-after-returned-error)))

            :else
            (do (trove/log! {:level :warn
                             :data {:stage :after :id id :ret ret}
                             :msg "After hook returned unknown shape; ignoring"})
              outcome)))
        (catch Throwable t
          (update outcome :hook-errors (fnil conj [])
            {:stage :after :id id :error {:type :hook-exception
                                          :error-id :rlm.error/hook-exception
                                          :message (ex-message t)}}))))
    initial-outcome
    hooks))

(defn- compose-wrap-chain
  [wraps base-handler]
  (reduce
    (fn [handler {:keys [fn]}]
      (fn handler))
    base-handler
    wraps))

(defn- make-invoke-fn
  [env tool-registry-atom parent-dispatch-id parent-query-ctx]
  (fn invoke [sym args]
    (let [next-depth (inc (:depth parent-query-ctx 0))]
      (when (>= next-depth MAX_HOOK_DEPTH)
        (throw (ex-info (str "Hook :invoke recursion limit reached ("
                          MAX_HOOK_DEPTH ")")
                 {:type :rlm/hook-recursion-limit
                  :depth next-depth
                  :sym sym}))))
    (let [tool-def (get @tool-registry-atom sym)]
      (when-not tool-def
        (throw (ex-info (str "Unknown tool for :invoke - " sym " not registered")
                 {:type :rlm/unknown-invoke-tool :sym sym})))
      (let [child-ctx (-> parent-query-ctx
                        (assoc :hooks nil
                          :depth (inc (:depth parent-query-ctx 0))
                          :parent-dispatch-id parent-dispatch-id))
            outcome (execute-tool env sym (:fn tool-def) (vec args)
                      {:tool-hooks (:hooks tool-def)
                       :tool-def tool-def
                       :tool-registry-atom tool-registry-atom
                       :query-ctx child-ctx})]
        (if (:error outcome)
          (throw (ex-info (get-in outcome [:error :message] "invoke failed")
                   {:type :rlm/invoke-error :outcome outcome}))
          (:result outcome))))))

(defn- apply-tool-formatter
  "Run the tool-def's :format-result-fn on `raw-result` and, when possible,
   attach `:rlm/format` + `:rlm/formatted` metadata so downstream consumers
   (LLM serializer, println override, var index) can skip recomputing.

   Contract:
   - Always returns `{:result value-maybe-with-meta :formatted string}`.
   - Formatter exceptions are swallowed: fallback to pr-str of the raw value
     so one misbehaving formatter can't nuke a tool call.
   - Non-IObj values (strings, numbers, keywords, booleans, nil) pass through
     unchanged — metadata isn't attachable. The serializer still receives
     `:formatted` on the outcome map and can use tool-sym lookup for primitives.
   - When `raw-result` already carries `:rlm/format` meta (e.g. a tool returned
     an already-formatted nested value), we don't overwrite it; the upstream
     formatter wins. This keeps composition sane."
  [tool-def raw-result]
  (let [fmt (:format-result-fn tool-def)
        formatted (if fmt
                    (try (fmt raw-result)
                      (catch Throwable _ (pr-str raw-result)))
                    (pr-str raw-result))
        existing-meta (when (instance? clojure.lang.IObj raw-result) (meta raw-result))
        already-formatted? (contains? existing-meta :rlm/format)
        result' (cond
                  (not (instance? clojure.lang.IObj raw-result)) raw-result
                  already-formatted? raw-result
                  :else (vary-meta raw-result assoc
                          :rlm/format (or fmt (constantly formatted))
                          :rlm/formatted formatted))]
    {:result result' :formatted formatted}))

(defn execute-tool
  "Core tool invocation pipeline. Runs :before chain -> :wrap middleware ->
   fn -> :after chain, firing global :on-tool-invoked / :on-tool-completed
   observers around the whole thing."
  [env sym user-fn args {:keys [tool-hooks tool-def tool-registry-atom query-ctx]}]
  (let [{:keys [hooks iteration-atom parent-dispatch-id]} (or query-ctx DEFAULT_QUERY_CTX)
        dispatch-id (str (java.util.UUID/randomUUID))
        cancel-atom (:cancel-atom env)
        before-hooks (or (:before tool-hooks) [])
        after-hooks  (or (:after tool-hooks)  [])
        wrap-hooks   (or (:wrap tool-hooks)   [])
        invoke-fn (make-invoke-fn env tool-registry-atom dispatch-id
                    (or query-ctx DEFAULT_QUERY_CTX))
        cancel-fn (fn [] (when cancel-atom (reset! cancel-atom true)))
        current-iteration (if iteration-atom @iteration-atom 0)
        invocation {:sym sym
                    :args args
                    :iteration current-iteration
                    :env env
                    :dispatch-id dispatch-id
                    :parent-dispatch-id parent-dispatch-id
                    :invoke invoke-fn
                    :cancel! cancel-fn}]
    (when-let [g (:on-tool-invoked hooks)]
      (try (g invocation)
        (catch Throwable t
          (trove/log! {:level :warn
                       :data {:error (ex-message t)}
                       :msg ":on-tool-invoked observer threw; ignoring"}))))
    (let [{transformed-args :args short-circuit :short-circuit skipped? :skipped?}
          (run-before-chain before-hooks invocation)
          start-ns (System/nanoTime)
          {:keys [result error]}
          (cond
            short-circuit
            short-circuit

            :else
            (let [validate-input  (:validate-input-fn tool-def)
                  validate-output (:validate-output-fn tool-def)
                  rescue-fn       (:rescue-fn tool-def)
                  ;; Wrap validate-output in a local fn so we can reuse it
                  ;; for both the happy path and the rescued path without
                  ;; duplicating the three-branch coerce/default logic.
                  apply-validate-output
                  (fn [validated-inv raw-result]
                    (if validate-output
                      (let [ret (validate-output (assoc validated-inv :result raw-result))]
                        (if (and (map? ret) (contains? ret :result))
                          (:result ret)
                          raw-result))
                      raw-result))
                  base-handler (fn [inv]
                                 (try
                                   (let [validated-inv (if validate-input
                                                         (let [ret (validate-input inv)]
                                                           (if (and (map? ret) (contains? ret :args))
                                                             (assoc inv :args (:args ret))
                                                             inv))
                                                         inv)
                                         args          (:args validated-inv)
                                         ;; Isolate the tool-fn call so its
                                         ;; exceptions can be intercepted by
                                         ;; `:rescue-fn` without catching
                                         ;; validator or reducer errors.
                                         raw-result    (try
                                                         (apply user-fn args)
                                                         (catch Throwable t
                                                           (if rescue-fn
                                                             ;; Rescue handler: may return a value,
                                                             ;; return nil (treated as successful nil),
                                                             ;; or throw (original or replacement error).
                                                             (apply rescue-fn t args)
                                                             (throw t))))
                                         final-result  (apply-validate-output validated-inv raw-result)]
                                     {:result final-result :error nil})
                                   (catch Throwable t
                                     {:result nil
                                      :error {:type :tool-exception
                                              :error-id :rlm.error/tool-exception
                                              :message (ex-message t)
                                              :data (ex-data t)}})))
                  composed (compose-wrap-chain wrap-hooks base-handler)]
              (try
                (composed (assoc invocation :args transformed-args))
                (catch Throwable t
                  {:result nil
                   :error {:type :wrap-exception
                           :error-id :rlm.error/wrap-exception
                           :message (ex-message t)}}))))
          duration-ms (double (/ (- (System/nanoTime) start-ns) 1e6))
          {formatted-result :result formatted-str :formatted}
          (if (or error (nil? tool-def))
            {:result result :formatted nil}
            (apply-tool-formatter tool-def result))
          initial-outcome (merge invocation
                            {:args transformed-args
                             :result formatted-result
                             :formatted formatted-str
                             :error (ensure-error-map error :rlm.error/tool-error)
                             :duration-ms duration-ms
                             :skipped? (boolean skipped?)
                             :hook-errors []})
          final-outcome (let [post-after (run-after-chain after-hooks initial-outcome)
                              status (cond
                                       (:error post-after) :error
                                       (:skipped? post-after) :skipped
                                       :else :success)]
                          (assoc post-after :status status :status-id (status-id status)))]
      (when-let [g (:on-tool-completed hooks)]
        (try (g final-outcome)
          (catch Throwable t
            (trove/log! {:level :warn
                         :data {:error (ex-message t)}
                         :msg ":on-tool-completed observer threw; ignoring"})))) 
      ;; Record execution diagnostics
      (tool-diag/record-execution! sym (long (* duration-ms 1e6)) (boolean (:error final-outcome)))
      final-outcome)))

(defn wrap-tool-for-sci
  "Build the fn that gets bound into the SCI sandbox for a registered tool.
   Returns :result on success or throws an ex-info on :error."
  ([env sym user-fn tool-registry-atom]
   (wrap-tool-for-sci env sym user-fn tool-registry-atom DEFAULT_QUERY_CTX))
  ([env sym user-fn tool-registry-atom query-ctx]
   (fn wrapped-tool [& args]
     (let [outcome (execute-tool env sym user-fn (vec args)
                     {:tool-hooks (get-in @tool-registry-atom [sym :hooks])
                      :tool-def (get @tool-registry-atom sym)
                      :tool-registry-atom tool-registry-atom
                      :query-ctx (assoc query-ctx
                                   :depth 0
                                   :parent-dispatch-id nil)})]
       (if-let [err (:error outcome)]
         (throw (ex-info (or (:message err) "tool error") err))
         (:result outcome))))))

(defn list-tool-hooks
  "Return a map describing the hook chains registered for `sym`."
  [hook-registry-atom sym]
  (when-let [tool-def (get @hook-registry-atom sym)]
    (let [describe (fn [entries]
                     (vec (map-indexed
                            (fn [i {:keys [id fn]}]
                              {:id id
                               :position i
                               :fn-name (or (some-> fn class .getSimpleName) "fn")})
                            entries)))
          hooks (:hooks tool-def {})]
      {:before (describe (:before hooks []))
       :after  (describe (:after hooks []))
       :wrap   (describe (:wrap hooks []))})))

(defn list-registered-tools
  "Return a vec of {:sym :hook-counts} maps summarizing every registered tool."
  [hook-registry-atom]
  (vec
    (for [[sym tool-def] @hook-registry-atom]
      {:sym sym
       :hook-counts {:before (count (get-in tool-def [:hooks :before]))
                     :after  (count (get-in tool-def [:hooks :after]))
                     :wrap   (count (get-in tool-def [:hooks :wrap]))}})))

(defn unregister-hook!
  "Remove a hook entry by :id from a given stage. Returns true/false."
  [hook-registry-atom sym stage id]
  (let [removed? (atom false)]
    (swap! hook-registry-atom
      (fn [registry]
        (if-let [tool-def (get registry sym)]
          (let [old-entries (get-in tool-def [:hooks stage] [])
                new-entries (vec (remove #(= id (:id %)) old-entries))]
            (when (not= (count old-entries) (count new-entries))
              (reset! removed? true))
            (assoc-in registry [sym :hooks stage] new-entries))
          registry)))
    @removed?))

(defn register-tool-def!
  "Register or layer a tool-def in `hook-registry-atom`.

   Enforces the freshness contract: any tool-def marked
   `` MUST ship a callable `:metadata-fn` fn
   (single-arg map — see the `Var freshness` ns docstring section).
   Registration throws `:rlm.tool/missing-freshness-fn` up-front so
   the author gets a loud error at boot, not a surprise nil stacktrace
   at the first call site."
  [hook-registry-atom sym tool-def]
  (when (:metadata-fn tool-def)
    (when-not (fn? (:metadata-fn tool-def))
      (throw (ex-info (str "Tool " sym
                        " declares  but no "
                        ":metadata-fn fn is attached. Provide a fn matching "
                        "(fn [{:keys [args result metadata]}] "
                        "{:metadata <…> :fresh? <bool>}).")
               {:type :rlm.tool/missing-freshness-fn
                :tool sym}))))
  (let [canonical (merge-tool-def-hooks
                    (get @hook-registry-atom sym {})
                    tool-def)]
    (sci-tool/maybe-assert-fn-tool-def! canonical)
    (swap! hook-registry-atom assoc sym canonical)
    canonical))
