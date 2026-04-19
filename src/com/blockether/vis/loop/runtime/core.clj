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
   since bindings from sci/init :namespaces are not SCI vars."
  [sci-ctx sym val]
  (let [ns-obj (sci/find-ns sci-ctx 'sandbox)]
    (sci/eval-string+ sci-ctx (str "(def " sym " nil)") {:ns ns-obj})
    (sci/intern sci-ctx ns-obj sym val)))

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

(def ^:private max-sandbox-slurp-bytes
  "Upper bound on `(slurp ...)` inside the sandbox. Mirrors `read-file`'s
   `max-file-size` (10 MB). Prevents a single agent call from pulling a
   300 MB core.basis into memory by accident."
  (* 10 1024 1024))

(defn- safe-slurp
  "Sandbox `slurp` that mirrors `read-file`'s safety guards but returns raw
   content (no line numbering) so callers can parse with `edn/read-string`,
   `fast-edn/parse-string`, etc.

   Blocks:
   * Symlinked files (no follow-through to arbitrary targets).
   * Directory targets (tells the LLM to use `list-dir`).
   * Files over `max-sandbox-slurp-bytes` (tells the LLM to use
     `(read-file path offset limit)`).

   Everything else falls through to regular `clojure.core/slurp`, so all the
   usual URL/Reader-arity flexibility survives."
  [source & opts]
  (when (string? source)
    (let [f (java.io.File. ^String source)]
      (when (.exists f)
        (when (.isDirectory f)
          (throw (ex-info (str "slurp path is a directory: " source
                            ". Use list-dir to enumerate entries.")
                   {:path source :error :is-directory})))
        (when (java.nio.file.Files/isSymbolicLink (.toPath f))
          (throw (ex-info (str "Refusing to follow symlink: " source)
                   {:path source :error :symlink})))
        (when (> (.length f) max-sandbox-slurp-bytes)
          (throw (ex-info (str "slurp target too large: "
                            (quot (.length f) 1024) "KB. Use "
                            "(read-file path offset limit) for paged reads.")
                   {:path source :size (.length f) :max max-sandbox-slurp-bytes}))))))
  (apply clojure.core/slurp source opts))

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
   for any arg that was produced by a tool with a :format-result formatter.

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
                       ;; :format-result output for args carrying :rlm/format meta.
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
                       ;; Sandbox-safe slurp that reuses read-file's guards.
                       ;; LLMs reach for `slurp` reflexively — giving them a
                       ;; capped, symlink-safe version kills a class of
                       ;; NullPointerException-on-unresolved-var turns.
                       'slurp safe-slurp}
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
                                               ;; `slurp` intentionally NOT denied: we shadow it in
                                               ;; sandbox bindings with `safe-slurp`, which delegates
                                               ;; to read-file-style guards (no symlink follow, 10 MB
                                               ;; cap, directory→friendly error). `spit` stays denied —
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
           :group "Documents" :activation-doc "no documents ingested"
           :activation-fn has-docs?})
        (register! 'fetch-document-content fetch-fn
          {:doc "(fetch-document-content [:node/id \"id\"]) or [:doc/id \"id\"] or [:toc/id \"id\"]"
           :group "Documents" :activation-doc "no documents ingested"
           :activation-fn has-docs?})
        (register! 'search-batch
          (fn search-batch
            ([queries] (when db-info (sci-tools/format-docs (db/db-search-batch db-info queries))))
            ([queries opts] (when db-info (sci-tools/format-docs (db/db-search-batch db-info queries opts)))))
          {:doc "(search-batch [\"q1\" \"q2\"]) — batch search across pages and TOC"
           :group "Documents" :activation-doc "no documents ingested"
           :activation-fn has-docs?})))
    ;; --- Conversation history tools (active when conversation exists) ---
    (when has-conv?
      (let [has-history? (fn [env] (boolean (:conversation-ref env)))]
        (register! 'conversation-history
          (sci-tools/make-conversation-history-fn db-info conversation-ref)
          {:doc "(conversation-history) or (conversation-history n) — prior query summaries"
           :group "Conversation" :activation-doc "no active conversation"
           :activation-fn has-history?
           :format-result fmt/format-conversation-history})
        (register! 'conversation-code
          (sci-tools/make-conversation-code-fn db-info conversation-ref)
          {:doc "(conversation-code query-selector) — prior query code blocks"
           :group "Conversation" :activation-doc "no active conversation"
           :activation-fn has-history?
           :format-result fmt/format-conversation-code})
        (register! 'conversation-results
          (sci-tools/make-conversation-results-fn db-info conversation-ref)
          {:doc "(conversation-results query-selector) — prior query results"
           :group "Conversation" :activation-doc "no active conversation"
           :activation-fn has-history?
           :format-result fmt/format-conversation-results})))
    ;; --- Restore tools (active when conversation has prior queries) ---
    ;; Special: restore-var also rebinds the value into the SCI sandbox.
    (when has-conv?
      (let [raw-restore-fn (sci-tools/make-restore-var-fn db-info conversation-ref)
            binding-restore-var (fn binding-restore-var
                                  ([sym] (binding-restore-var sym {}))
                                  ([sym opts]
                                   (let [val (raw-restore-fn sym opts)]
                                     (sci-update-binding! sci-ctx sym val)
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
           :group "Conversation" :activation-doc "no persisted vars from prior queries"
           :activation-fn has-vars?})
        (register! 'restore-vars binding-restore-vars
          {:doc "(restore-vars ['sym1 'sym2]) — batch restore + rebind"
           :group "Conversation" :activation-doc "no persisted vars from prior queries"
           :activation-fn has-vars?
           :format-result fmt/format-restore-vars})
        (register! 'var-history
          (sci-tools/make-var-history-fn db-info conversation-ref)
          {:doc "(var-history 'sym) — all persisted versions of a var, oldest first. Each: {:version N :value :code :created-at}"
           :group "Conversation" :activation-doc "no persisted vars from prior queries"
           :activation-fn has-vars?
           :format-result fmt/format-var-history})
        (register! 'var-diff
          (sci-tools/make-var-diff-fn db-info conversation-ref)
          {:doc "(var-diff 'sym 1 3) — structural diff between two versions. Returns {:edits [...] :edit-count N}"
           :group "Conversation" :activation-doc "no persisted vars from prior queries"
           :activation-fn has-vars?
           :format-result fmt/format-var-diff})))
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
                              nil))]
        (doseq [[sym f] git-binds]
          (register! sym f
            (cond-> {:doc (str "(" sym " ...) — git tool")
                     :group "Git" :activation-doc "no git repos attached"
                     :activation-fn has-repos?}
              (git-formatter sym) (assoc :format-result (git-formatter sym)))))))
    ;; --- Concept tools (active when concepts exist — cross-conversation) ---
    (when has-db?
      (let [has-concepts?     (fn [env] (boolean (seq (db/list-concepts (:db-info env)))))
            concept-binds     (ontology/make-concept-graph-bindings db-info)
            concept-formatter (fn [sym]
                                (case sym
                                  concept-info    fmt/format-concept-info
                                  remove-concept  fmt/format-concept-mutation
                                  edit-concept    fmt/format-concept-mutation
                                  nil))]
        (doseq [[sym f] concept-binds]
          (register! sym f
            (cond-> {:doc (str "(" sym " ...) — concept graph tool")
                     :group "Concepts" :activation-doc "no concepts extracted yet"
                     :activation-fn has-concepts?}
              (concept-formatter sym) (assoc :format-result (concept-formatter sym)))))))
    ;; Update initial-ns-keys so get-locals excludes built-in tools
    (when sci-ctx
      (let [current-keys (set (keys (:val (sci/eval-string+ sci-ctx "(ns-publics 'sandbox)"
                                            {:ns (:sandbox-ns env (get-in @(:env sci-ctx) [:namespaces 'sandbox]))}))))]
        (assoc env :initial-ns-keys current-keys)))))

(def ^:private ^:const MAX_VAR_INDEX_ROWS 12)
(def ^:private ^:const MAX_VAR_INDEX_COUNT 1000)
(def ^:private ^:const MAX_VAR_INDEX_PREVIEW 150)

(defn- safe-preview-str
  "Render a bounded printable preview for arbitrary values.
   Uses print limits so infinite/lazy seqs (e.g. (range)) do not OOM."
  [val]
  (binding [*print-length* 20
            *print-level* 4]
    (pr-str val)))

(defn build-var-index
  "Builds a formatted var index table from user-def'd vars in the SCI context.
   Filters out initial bindings (tools, helpers) using initial-ns-keys.
   Returns nil if no user vars exist.

   Row format: `name | type | size | doc - preview`
   When `db-info` and `conversation-ref` are provided, rows are sorted
   newest-first by `:iteration-var` `:entity/created-at`; freshly-def'd
   vars (not yet persisted) sort above any DB-recorded ones."
  ([sci-ctx initial-ns-keys]
   (build-var-index sci-ctx initial-ns-keys nil nil nil))
  ([sci-ctx initial-ns-keys sandbox]
   (build-var-index sci-ctx initial-ns-keys sandbox nil nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-ref]
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
           live-info (into {}
                       (for [[s v] sandbox-map
                             :when (symbol? s)]
                         [s {:val (if (instance? clojure.lang.IDeref v) @v v)
                             :doc (:doc (meta v))
                             :arglists (:arglists (meta v))
                             :version (get-in var-registry [s :version] 1)}]))
           persisted-info (when var-registry
                            (into {}
                              (for [[sym {:keys [value]}] var-registry
                                    :when (and (symbol? sym)
                                            (not (contains? live-info sym))
                                            (not (contains? initial-ns-keys sym)))]
                                [sym {:val ::persisted
                                      :doc (str "persisted - (restore-var '" sym ") to load")
                                      :persisted-preview (or value "")}])))
            var-info (merge persisted-info live-info)
            earmuffed? (fn [sym]
                         (let [n (name sym)]
                           (and (> (count n) 2)
                             (str/starts-with? n "*")
                             (str/ends-with? n "*"))))
            ;; Human-readable description for earmuffed SYSTEM vars. These
            ;; are bound by the iteration loop and treated as part of the
            ;; agent's contract surface — readable/usable like any other SCI
            ;; var, but NEVER FORGOTTEN (forget-vars! refuses to drop them).
            ;; The (SYSTEM) prefix signals to the LLM that trying to :forget
            ;; these is a no-op.
            system-doc (fn [sym]
                         (case (str sym)
                           "*query*"     "(SYSTEM, never forgotten) current user query"
                           "*reasoning*" "(SYSTEM, never forgotten) YOUR thinking from the previous iteration"
                           "*answer*"    "(SYSTEM, never forgotten) final answer from the previous turn in this conversation"
                           (str "(SYSTEM, never forgotten) agent-bound var")))
            entries (->> var-info
                      (remove (fn [[sym _]] (contains? initial-ns-keys sym)))
                      ;; Sort order:
                      ;;   1. earmuffed *system* vars FIRST (stable by name so *answer*/*query*/*reasoning* line up)
                      ;;   2. then user vars, most-recently-touched first
                      (sort-by (fn [[sym _]]
                                 [(if (earmuffed? sym) 0 1)
                                  (- (long (recency-of sym)))
                                  (str sym)]))
                      (mapv (fn [[sym {:keys [val doc arglists persisted-preview]}]]
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
                                          persisted? (str (count persisted-preview) " chars")
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
                                    preview (cond
                                              persisted? (sci-shared/truncate persisted-preview MAX_VAR_INDEX_PREVIEW)
                                              (fn? val) "-"
                                              :else (sci-shared/truncate
                                                      (or (when (instance? clojure.lang.IObj val)
                                                            (or (:rlm/formatted (meta val))
                                                                (when-let [f (:rlm/format (meta val))]
                                                                  (try (f val) (catch Throwable _ nil)))))
                                                          (safe-preview-str val))
                                                      MAX_VAR_INDEX_PREVIEW))
                                   version (get-in var-info [sym :version] 1)
                                   ver-str (str version)]
                                ;; SYSTEM vars override the doc column so the LLM
                                ;; sees them labelled `(SYSTEM) …` even though no
                                ;; docstring was attached at def-time.
                                {:name (str sym) :ver ver-str :type type-label :size size
                                 :doc (cond
                                        system? (sci-shared/truncate (system-doc sym) 80)
                                        doc (sci-shared/truncate doc 80)
                                        :else "-")
                                 :preview preview
                                 :system? system?}))))]
       (when (seq entries)
         (let [visible (vec (take MAX_VAR_INDEX_ROWS entries))
               omitted (- (count entries) (count visible))
               max-name (max 4 (apply max (map #(count (:name %)) visible)))
               max-ver  (max 3 (apply max (map #(count (:ver %)) visible)))
               max-type (max 4 (apply max (map #(count (:type %)) visible)))
               max-size (max 4 (apply max (map #(count (:size %)) visible)))
               max-doc  (max 3 (apply max (map #(count (:doc %)) visible)))
               pad (fn [s n] (str s (apply str (repeat (max 0 (- n (count s))) \space))))
               header (str "  " (pad "name" max-name) " | " (pad "ver" max-ver) " | " (pad "type" max-type) " | " (pad "size" max-size) " | " (pad "doc" max-doc) " | preview")
               sep (str "  " (apply str (repeat max-name \-)) "-+-" (apply str (repeat max-ver \-)) "-+-" (apply str (repeat max-type \-)) "-+-" (apply str (repeat max-size \-)) "-+-" (apply str (repeat max-doc \-)) "-+---------")
               rows (mapv (fn [{:keys [name ver type size doc preview]}]
                            (str "  " (pad name max-name) " | " (pad ver max-ver) " | " (pad type max-type) " | " (pad size max-size) " | " (pad doc max-doc) " | " preview))
                      visible)
               footer (when (pos? omitted)
                        (str "  ... " omitted " more vars omitted"))]
           (str/join "\n" (concat [header sep] rows (when footer [footer]))))))
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
  "Run the tool-def's :format-result on `raw-result` and, when possible,
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
  (let [fmt (:format-result tool-def)
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
            (let [validate-input (:validate-input tool-def)
                  validate-output (:validate-output tool-def)
                  base-handler (fn [inv]
                                 (try
                                   (let [validated-inv (if validate-input
                                                         (let [ret (validate-input inv)]
                                                           (if (and (map? ret) (contains? ret :args))
                                                             (assoc inv :args (:args ret))
                                                             inv))
                                                         inv)
                                         raw-result (apply user-fn (:args validated-inv))
                                         final-result (if validate-output
                                                        (let [ret (validate-output (assoc validated-inv :result raw-result))]
                                                          (if (and (map? ret) (contains? ret :result))
                                                            (:result ret)
                                                            raw-result))
                                                        raw-result)]
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
  "Register or layer a tool-def in `hook-registry-atom`."
  [hook-registry-atom sym tool-def]
  (let [canonical (merge-tool-def-hooks
                    (get @hook-registry-atom sym {})
                    tool-def)]
    (sci-tool/maybe-assert-fn-tool-def! canonical)
    (swap! hook-registry-atom assoc sym canonical)
    canonical))
