(ns com.blockether.vis.internal.env
  "SCI sandbox machinery: bindings, sandbox bindings, restore.

   INTERNAL - loop and prompt own this namespace; core re-exports selected
   helpers for extensions/tests. Foundation utilities (storage facade,
   extension specs, format helpers, etc.) live across
   `com.blockether.vis.internal.{persistance,extension,config,registry}`.
   Channels and extensions should prefer the public iteration entry points
   in `com.blockether.vis.internal.loop` (`send!`, `create!`, `turn!`, ...)."
  (:require
   [clojure.set]
   [clojure.spec.alpha]
   [clojure.string :as str]
   [clojure.walk]
   [clojure+.core]
   [clojure+.walk]
   [edamame.core :as edamame]
   [com.blockether.vis.internal.format :as fmt]
   [com.blockether.vis.internal.env.sci-patches]
   [com.blockether.vis.internal.persistance :as persistance]
   [sci.addons.future :as sci-future]
   [sci.core :as sci]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; SCI sandbox + bindings
;; =============================================================================

(defn sci-update-binding!
  "Update a binding in the SCI sandbox.

   Writes through the SCI `def` special form (so the patched
   `eval-def` enforces the docstring contract — we satisfy it with a
   synthetic 'vis-managed engine binding' tag) and then interns the
   actual value. The engine `ctx` snapshot reads the sandbox map directly, so no
   separate invalidation step is needed."
  [sci-ctx sym val]
  (let [ns-obj (sci/find-ns sci-ctx 'sandbox)]
    (sci/eval-string+ sci-ctx (str "(def " sym " \"vis-managed engine binding\" nil)") {:ns ns-obj})
    (sci/intern sci-ctx ns-obj sym val)))

(defn bind-and-bump!
  "Set `sym` to `val` in the env's sandbox. Name preserved for source
   compatibility — the legacy 'bump' (cache invalidation) is gone
   alongside the cache it served. Now a thin
   wrapper over `sci-update-binding!`."
  [env sym val]
  (sci-update-binding! (:sci-ctx env) sym val))

(defn bind-and-bump-with-doc!
  "Like `bind-and-bump!` but writes `doc` as the var's :doc metadata
   so the journal live-vars renderer surfaces a meaningful name + doc
   to the model (e.g. USER_REQUEST -> 'current turn user request')."
  [env sym doc val]
  (let [sci-ctx (:sci-ctx env)
        ns-obj  (sci/find-ns sci-ctx 'sandbox)]
    (sci/eval-string+ sci-ctx
      (str "(def " sym " " (pr-str (or doc "vis-managed engine binding")) " nil)")
      {:ns ns-obj})
    (sci/intern sci-ctx ns-obj sym val)))

(defn push-eval-result!
  "REPL-style stack push for the sandbox `*1` `*2` `*3` recovery slots.
   Called by the iteration loop after each successful top-level form
   eval. Bypasses legacy bindings cache invalidation - these vars are filtered
   out of the model-visible binding view by `:initial-ns-keys`."
  [env value]
  (let [sci-ctx (:sci-ctx env)
        ns-obj  (sci/find-ns sci-ctx 'sandbox)]
    (when ns-obj
      ;; Read current *1, *2 first so we can shift them down without losing
      ;; intermediate state. `sci/intern` overwrites the var binding directly.
      (let [v1 (:val (sci/eval-string+ sci-ctx "*1" {:ns ns-obj}))
            v2 (:val (sci/eval-string+ sci-ctx "*2" {:ns ns-obj}))]
        (sci/intern sci-ctx ns-obj '*3 v2)
        (sci/intern sci-ctx ns-obj '*2 v1)
        (sci/intern sci-ctx ns-obj '*1 value)))))

(defn push-eval-error!
  "Park the most recent uncaught exception in the sandbox `*e` slot. Stack
   `*1`/`*2`/`*3` does NOT advance on exception (the form produced no value)."
  [env throwable]
  (let [sci-ctx (:sci-ctx env)
        ns-obj  (sci/find-ns sci-ctx 'sandbox)]
    (when ns-obj
      (sci/intern sci-ctx ns-obj '*e throwable))))

(defn reset-eval-bindings!
  "Clear `*1` `*2` `*3` `*e` to nil. Called at turn start so a follow-up
   turn does not see the previous turn's leftover values."
  [env]
  (let [sci-ctx (:sci-ctx env)
        ns-obj  (sci/find-ns sci-ctx 'sandbox)]
    (when ns-obj
      (doseq [s '[*1 *2 *3 *e]]
        (sci/intern sci-ctx ns-obj s nil)))))

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
   requires a Pattern - LLMs frequently write `(str/split s \"\\n\")` and the
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
   reads go through `v/cat`, not raw `slurp`. `slurp` returns raw bytes with
   no path-policy hint and no opinionated prompt surface, making it a
   cache-coherency footgun: a var bound from `slurp` can't be validated
   against the filesystem the next iteration, so the agent silently
   trusts stale content. Better to refuse the call and point at the
   sanctioned `v/` file surface."
  [& _args]
  (throw (ex-info (str "slurp is banned in the sandbox - use (v/cat \"path\") "
                    "for full-file reads. The sanctioned file surface stays cwd-safe "
                    "and consistent with the prompt.")
           {:type :tool/banned :tool 'slurp})))

(defn- sandbox-println
  "Sandbox replacement for clojure.core/println.

   SCI rebinds `sci.core/out` (NOT Clojure's `*out*`) per-iteration via
   `sci/binding`. SCI's built-in println handles this by rebinding `*out*`
   from `@sci.core/out` inside the fn body - we do the same so stdout
   capture keeps working with our override in place."
  [& args]
  (binding [*out* @sci/out]
    (apply println args)))

(defn- sandbox-print
  "Sandbox replacement for clojure.core/print.
   Rebinds `*out*` from SCI's out for stdout capture."
  [& args]
  (binding [*out* @sci/out]
    (apply print args)))

;; =============================================================================
;; SCI namespace / binding helpers (moved from shared.clj - sandbox-only)
;; =============================================================================

(defn ns->sci-map
  "Builds an SCI :namespaces entry map from a Clojure namespace's public vars."
  [ns-sym]
  (require ns-sym)
  (into {} (for [[sym v] (ns-publics (the-ns ns-sym))
                 :when (and (var? v) (not (:macro (meta v))))]
             [sym @v])))

(def MODERN_CORE_BINDINGS
  "Clojure core helpers absent from SCI 0.12.51's default `clojure.core`
   snapshot. Install these into both `sandbox` (unqualified use) and
   `clojure.core` (qualified use)."
  {'update-keys update-keys
   'update-vals update-vals})

(def EXTRA_BINDINGS
  "Extra bindings beyond what SCI provides by default. Handle protocol
   primitives (`view` / `summary` / `op` / `handle?`) were retired:
   tools now return plain Clojure maps, so destructuring + standard
   sequence ops are the only API the model needs."
  (merge MODERN_CORE_BINDINGS
    {'abs abs, 'parse-long parse-long, 'parse-double parse-double,
     'parse-boolean parse-boolean, 'parse-uuid parse-uuid,
     'infinite? infinite?, 'NaN? NaN?,
     'url-encode (fn ^String url-encode [^String s] (java.net.URLEncoder/encode s "UTF-8")),
     'url-decode (fn ^String url-decode [^String s] (java.net.URLDecoder/decode s "UTF-8"))}))

(defn create-sci-context
  "Creates the SCI sandbox context with all available bindings.

   Params:
   `custom-bindings` - Map of symbol->value for custom bindings (can be nil)"
  [custom-bindings]
  (let [base-bindings {;; prn is intentionally NOT overridden: it's for data
                       ;; round-trip and must stay verbatim pr-str.
                       'println sandbox-println
                       'print sandbox-print
                       ;; LLM footgun shadows: auto-promote string->Pattern so
                       ;; (re-find "HITL" s), (re-seq "\\d+" s), etc. stop
                       ;; throwing ClassCastException late inside lazy seqs.
                       're-find safe-re-find
                       're-seq safe-re-seq
                       're-matches safe-re-matches
                       ;; `slurp` is BANNED. File reads go through the
                       ;; sanctioned `v/` filesystem surface (`v/cat`
                       ;; for full-file acquisition). `slurp` bypasses
                       ;; the prompt's path-policy and encourages
                       ;; ad-hoc I/O.
                       'slurp banned-slurp}
        all-bindings (merge EXTRA_BINDINGS base-bindings
                       (or custom-bindings {}))
        str-ns  (sci/create-ns 'clojure.string nil)
        set-ns  (sci/create-ns 'clojure.set nil)
        walk-ns   (sci/create-ns 'clojure.walk nil)
        c+walk-ns (sci/create-ns 'clojure+.walk nil)
        plus-ns   (sci/create-ns 'clojure+.core nil)
        spec-ns (sci/create-ns 'clojure.spec.alpha nil)
        ;; Patch clojure.string/split so string delimiters auto-promote to
        ;; Patterns. The original raises a late ClassCastException when an
        ;; LLM passes a string, usually after the cast hides inside a lazy
        ;; seq that only realizes during answer assembly.
        str-ns-copied (assoc (sci/copy-ns clojure.string str-ns)
                        'split (sci/new-var 'split safe-split {:ns str-ns}))

        sandbox-ns (sci/create-ns 'sandbox nil)
        sci-ctx (sci/init
                  (sci-future/install {:namespaces {'sandbox (merge all-bindings
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
                                                    'clojure.core MODERN_CORE_BINDINGS
                                                    'clojure.string str-ns-copied
                                                    'clojure.set (sci/copy-ns clojure.set set-ns)
                                                    'clojure.walk (sci/copy-ns clojure+.walk walk-ns)
                                                    'clojure+.walk (sci/copy-ns clojure+.walk c+walk-ns)
                                                    'clojure+.core (sci/copy-ns clojure+.core plus-ns)
                                                    ;; clojure.spec.alpha - LLMs reach for s/def / s/valid? /
                                                    ;; s/keys / s/and / s/conform reflexively when given a
                                                    ;; data-shape problem. Pre-fix, none of those resolved in
                                                    ;; the sandbox and the model wasted iterations bouncing
                                                    ;; off "could not resolve symbol s/def". `sci/copy-ns`
                                                    ;; carries macros (unlike the lighter `ns->sci-map`),
                                                    ;; which is what spec needs (`s/def`, `s/keys`, `s/fdef`
                                                    ;; are all macros).
                                                    ;;
                                                    ;; `:exclude-when-meta []` is load-bearing: by default
                                                    ;; `copy-ns` skips vars tagged `^:no-doc` or `^:skip-wiki`,
                                                    ;; and EVERY internal helper the spec macros expand into
                                                    ;; (`def-impl`, `and-spec-impl`, `or-spec-impl`,
                                                    ;; `cat-impl`, `every-impl`, `keys-spec-impl`, ...) carries
                                                    ;; `^:skip-wiki`. Without an empty exclude list the
                                                    ;; macros expand to symbols that don't exist in the SCI
                                                    ;; namespace and `(s/def ::id int?)` throws "Unable to
                                                    ;; resolve symbol: clojure.spec.alpha/def-impl".
                                                    'clojure.spec.alpha (sci/copy-ns clojure.spec.alpha spec-ns
                                                                          {:exclude-when-meta []})
                                                    'fast-edn.core (ns->sci-map 'fast-edn.core)
                                                    'clojure.edn (ns->sci-map 'fast-edn.core)
                                                    ;; Sandbox pretty-printing shares the SAME global zprint gate
                                                    ;; as the TUI render path (`format/format-clojure`). Without
                                                    ;; that, a model calling `(pp/pprint-str x)` could race the
                                                    ;; renderer formatting a code block and trip zprint's global
                                                    ;; re-entrancy guard (`Attempted to run zprint with type ...`).
                                                    'zprint.core {'zprint-str      fmt/safe-zprint-str
                                                                  'zprint          fmt/safe-zprint
                                                                  'czprint-str     fmt/safe-czprint-str
                                                                  'czprint         fmt/safe-czprint
                                                                  'zprint-file-str fmt/safe-zprint-file-str
                                                                  'set-options!    fmt/safe-set-zprint-options!
                                                                  'configure-all!  fmt/safe-configure-zprint!}
                                                    'clojure.pprint {'pprint     fmt/safe-pprint
                                                                     'pprint-str fmt/safe-pprint-str}

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
                                                    'c+walk 'clojure+.walk
                                                    'json 'charred.api
                                                    'c+ 'clojure+.core
                                                    's 'clojure.spec.alpha}
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
                                                 ;; Throwable + the common JVM runtime exception subclasses
                                                 ;; the model reaches for after `(catch Throwable t ...)` /
                                                 ;; `(catch Exception e ...)`. Pre-fix, only `java.lang.Exception`
                                                 ;; was on the allow-list; SCI checks method calls against the
                                                 ;; ACTUAL runtime class, so `(catch Exception e (.getMessage e))`
                                                 ;; threw "Method getMessage on class java.lang.NullPointerException
                                                 ;; not allowed!" the moment the thrown instance was an NPE / IAE /
                                                 ;; ISE / etc. Conversation d8aff512-d60d-42b6-a009-041f1bec3891
                                                 ;; tripped this on iter where the model wrote `(catch Exception e
                                                 ;; {:error (.getMessage e)})` and the wrapped value was an NPE.
                                                 ;; Allowing Throwable + the common subclasses makes `.getMessage`
                                                 ;; / `.getCause` / `.getClass` / `.getStackTrace` work uniformly
                                                 ;; regardless of the concrete thrown class.
                                                 'java.lang.Throwable java.lang.Throwable
                                                 'java.lang.Error     java.lang.Error
                                                 'java.lang.RuntimeException java.lang.RuntimeException
                                                 'java.lang.NullPointerException java.lang.NullPointerException
                                                 'java.lang.IllegalArgumentException java.lang.IllegalArgumentException
                                                 'java.lang.IllegalStateException    java.lang.IllegalStateException
                                                 'java.lang.IndexOutOfBoundsException java.lang.IndexOutOfBoundsException
                                                 'java.lang.ArrayIndexOutOfBoundsException java.lang.ArrayIndexOutOfBoundsException
                                                 'java.lang.ClassCastException       java.lang.ClassCastException
                                                 'java.lang.ArithmeticException      java.lang.ArithmeticException
                                                 'java.lang.NumberFormatException    java.lang.NumberFormatException
                                                 'java.lang.UnsupportedOperationException java.lang.UnsupportedOperationException
                                                 'java.lang.InterruptedException     java.lang.InterruptedException
                                                 'java.lang.AssertionError           java.lang.AssertionError
                                                 'java.lang.StackOverflowError       java.lang.StackOverflowError
                                                 'java.lang.OutOfMemoryError         java.lang.OutOfMemoryError
                                                 'java.io.IOException                java.io.IOException
                                                 'java.io.FileNotFoundException      java.io.FileNotFoundException
                                                 'clojure.lang.ExceptionInfo         clojure.lang.ExceptionInfo
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
                                                  ;; Mirror of the new exception classes added to `:classes` above.
                                                  ;; Without these the model can call `(catch java.lang.Throwable t ...)`
                                                  ;; via the FQN but the more idiomatic `(catch Throwable t ...)` /
                                                  ;; `(catch NullPointerException e ...)` short forms wouldn't resolve.
                                                  Throwable                       java.lang.Throwable
                                                  Error                           java.lang.Error
                                                  RuntimeException                java.lang.RuntimeException
                                                  NullPointerException            java.lang.NullPointerException
                                                  IllegalArgumentException        java.lang.IllegalArgumentException
                                                  IllegalStateException           java.lang.IllegalStateException
                                                  IndexOutOfBoundsException       java.lang.IndexOutOfBoundsException
                                                  ArrayIndexOutOfBoundsException  java.lang.ArrayIndexOutOfBoundsException
                                                  ClassCastException              java.lang.ClassCastException
                                                  ArithmeticException             java.lang.ArithmeticException
                                                  NumberFormatException           java.lang.NumberFormatException
                                                  UnsupportedOperationException   java.lang.UnsupportedOperationException
                                                  InterruptedException            java.lang.InterruptedException
                                                  AssertionError                  java.lang.AssertionError
                                                  StackOverflowError              java.lang.StackOverflowError
                                                  OutOfMemoryError                java.lang.OutOfMemoryError
                                                  IOException                     java.io.IOException
                                                  FileNotFoundException           java.io.FileNotFoundException
                                                  ExceptionInfo                   clojure.lang.ExceptionInfo
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
                                               ;; which throws a descriptive ex-info pointing at the
                                               ;; sanctioned `v/` file helpers. Keeping it as a sandbox
                                               ;; binding (not a deny) gives the LLM a useful error
                                               ;; message instead of SCI's generic "not allowed".
                                               ;; `spit` stays denied - `v/patch` is the sanctioned mutation path.
                                               ;;
                                               ;; `require`, `import`, `find-ns` are NOT denied either
                                               ;; (real Clojure reach for namespace discovery); the tool
                                               ;; docs deliberately leave them unadvertised so the LLM's
                                               ;; canonical playbook stays narrow.
                                       ;; `*out*` / `*err*` are NOT denied so model
                                       ;; code can reach for them directly:
                                       ;;   `(binding [*out* writer] ...)`,
                                       ;;   `(.write *out* s)`,
                                       ;;   `(.write *err* s)`.
                                       ;; SCI's `sci/binding [sci/out writer
                                       ;; sci/err writer]` (set per-iteration
                                       ;; in `run-sci-code`) auto-rebinds the
                                       ;; sandbox's `*out*` / `*err*` to those
                                       ;; writers, so model writes land in the
                                       ;; same captured stream the iteration
                                       ;; loop reads back as `:stdout` / `:stderr`.
                                       :deny '[ns eval load-string load-file
                                               read-string
                                               spit
                                               intern
                                               sh
                                               *in* *command-line-args*]}))]
    ;; SCI preloads a few convenience namespaces. Keep model-facing
    ;; introspection and file access on sanctioned `v/` tools instead.
    (swap! (:env sci-ctx)
      (fn [sci-env]
        (-> sci-env
          (update :namespaces dissoc 'clojure.repl 'clojure.java.io)
          (update :ns-aliases dissoc 'repl 'io))))
    ;; REPL-style recovery bindings: `*1` `*2` `*3` carry the last three
    ;; evaluated form values (most recent first); `*e` carries the most
    ;; recent uncaught exception. The iteration loop pushes onto the
    ;; stack after each form's eval and resets all four to nil at turn
    ;; start. They live in the sandbox so model code reads them as plain
    ;; symbols, but they are interned BEFORE `:initial-ns-keys` so the
    ;; baseline filter excludes them from the live-vars line (they're noise to
    ;; the model's user-binding view).
    (sci/eval-string+ sci-ctx
      (str "(def *1 \"previous eval result\" nil) "
        "(def *2 \"second-previous eval result\" nil) "
        "(def *3 \"third-previous eval result\" nil) "
        "(def *e \"most recent uncaught exception\" nil)")
      {:ns sandbox-ns})
    {:sci-ctx sci-ctx
     :sandbox-ns sandbox-ns
     :initial-ns-keys (set (keys (:val (sci/eval-string+ sci-ctx "(ns-publics 'sandbox)" {:ns sandbox-ns}))))}))

;; Engine-owned sandbox names. `ctx` is the only model-visible engine context
;; value; it is rebound by the loop and hidden from live user defs.
(def SYSTEM_VAR_NAMES
  "Engine-owned symbols hidden from user live-var listings. `ctx` is the
   single context value."
  '#{ctx})

(defn system-var-sym?
  "True when `sym` is engine-owned and hidden from user live-var listings."
  [sym]
  (contains? SYSTEM_VAR_NAMES sym))

;; =============================================================================
;; Restore helpers
;; =============================================================================

(def DEF_HEADS_FOR_RESTORE
  "Canonical set of def-shaped heads the engine treats as safe to
   re-evaluate on restore + extract per-var sources from. Single source
   of truth: `loop/extract-def-sources`, `loop/dep-edges-from-source`,
   and `restore-sandbox!` all read the SAME set so a stored
   `:expression` that round-trips through the persistence path will
   also round-trip on restore.

   Class-producing heads (`defrecord` / `deftype` / `defprotocol` /
   `gen-class` / `extend-type` / `extend-protocol` / `definterface` /
   `reify`) are explicitly NOT here — they are refused at the sandbox
   boundary by `validate-no-banned-defs!`, so they can never be
   persisted in the first place."
  '#{def defn defn- defmacro defonce defmulti
     clojure.core/def clojure.core/defn clojure.core/defn-
     clojure.core/defmacro clojure.core/defonce clojure.core/defmulti})

(defn- parsed-def-form?
  "True when `expr` (a string) parses as exactly one top-level form
   whose head symbol is in `DEF_HEADS_FOR_RESTORE` and whose immediate
   next element is a symbol (the var name). Replaces the legacy
   regex-on-source heuristic with an actual edamame parse so prefix
   metadata, whitespace, and comments cannot confuse the check.

   Parse failures, empty strings, and non-string inputs all return
   false. The caller falls back to :restored-via :unavailable, which
   surfaces a clear refusal to the model instead of silently letting
   an exotic form re-run on restore."
  [expr]
  (boolean
    (when (string? expr)
      (try
        (let [forms (edamame/parse-string-all expr {:all true})]
          (when (= 1 (count forms))
            (let [form (first forms)]
              (and (seq? form)
                (symbol? (first form))
                (contains? DEF_HEADS_FOR_RESTORE (first form))
                (symbol? (second form))))))
        (catch Throwable _ false)))))

;; =============================================================================
;; Sandbox restore - rebuild SCI bindings from DB
;; =============================================================================

(defn restore-sandbox!
  "Restore all persisted vars into a SCI sandbox from the DB.

   Reads `db-restore-blocks` (topologically sorted) and for each entry:
   - Data value (nippy-thawed) -> bind directly into the sandbox.
   - Safe `defn` source with `{:vis/ref :expr}` -> eval the `defn` source.
   - Other `{:vis/ref :expr}` values are unavailable and must be recreated
     intentionally; restore never replays arbitrary effectful source.

   Dependencies are guaranteed to appear first.

   Returns a vec of {:name :restored-via (:data | :eval | :unavailable | :skip)
                     :success? boolean ...}."
  [sci-ctx db-info conversation-id]
  (let [entries (persistance/db-restore-blocks db-info conversation-id)]
    (mapv (fn [{:keys [name expr result]}]
            (let [sym (symbol name)]
              (try
                (if (and (map? result) (= :expr (:vis/ref result)))
                  (cond
                    (or (nil? expr) (= expr ";; SYSTEM var"))
                    {:name name :restored-via :skip :success? true}

                    (parsed-def-form? expr)
                    (do (sci/eval-string+ sci-ctx expr
                          {:ns (sci/find-ns sci-ctx 'sandbox)})
                      {:name name :restored-via :eval :success? true})

                    :else
                    {:name name
                     :restored-via :unavailable
                     :success? false
                     :reason :unsafe-restore
                     :guidance "Recreate intentionally to persist a restorable value."})
                  ;; Data value -> bind directly
                  (do (sci-update-binding! sci-ctx sym result)
                    {:name name :restored-via :data :success? true}))
                (catch Throwable e
                  (tel/log! {:level :warn :id ::restore-failed
                             :data {:name name :error (ex-message e)}}
                    (str "Failed to restore " name))
                  {:name name :restored-via :error :success? false
                   :error (ex-message e)}))))
      entries)))

