(ns com.blockether.vis.internal.env
  "SCI sandbox machinery: bindings, sandbox bindings, restore.

   INTERNAL - only `com.blockether.vis.internal.loop` imports this namespace.
   Foundation utilities (storage facade, extension specs, format helpers,
   etc.) live across `com.blockether.vis.internal.{persistance,extension,config,registry}`. Channels and extensions never
   reach into here; they go through the public iteration entry points
   in `com.blockether.vis.internal.loop` (`send!`, `create!`, `turn!`, ...)."
  (:require
   [clojure.set]
   [clojure.spec.alpha]
   [clojure.string :as str]
   [clojure.walk]
   [clojure+.core]
   [clojure+.walk]
   [com.blockether.vis.internal.format :as fmt]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.persistance :as persistance]
   [sci.addons.future :as sci-future]
   [sci.core :as sci]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; SCI sandbox + bindings
;; =============================================================================

(defn sci-update-binding!
  "Update a binding in an existing SCI context.
   Ensures the symbol is a real SCI var before interning the value,
   since bindings from sci/init :namespaces are not SCI vars.

   NOTE: This mutates the SCI sandbox only. If the caller wants the next
   iteration's `<bindings>` context block to reflect the new binding, they
   MUST also call `bump-bindings!` on the env - the bindings is cached and
   only rebuilds when `:current-revision` advances. Every past cache-staleness
   bug (4-iteration `(restore-vars ...)` spin, turn-3 stale request replay)
   traces back to forgetting this pair. Prefer `bind-and-bump!` below."
  [sci-ctx sym val]
  (let [ns-obj (sci/find-ns sci-ctx 'sandbox)]
    (sci/eval-string+ sci-ctx (str "(def " sym " nil)") {:ns ns-obj})
    (sci/intern sci-ctx ns-obj sym val)))

(defn bump-bindings!
  "Invalidate the env's cached `<bindings>` so the next `get-bindings` call
   rebuilds it from the live SCI sandbox. No-op when the env has no
   `:bindings-atom` (e.g. ad-hoc test contexts)."
  [env]
  (when-let [atom (:bindings-atom env)]
    (swap! atom update :current-revision (fnil inc 0))))

(defn bind-and-bump!
  "Atomic \"rebind var in SCI + invalidate bindings cache\" - the only API
   call sites should use when mutating runtime bindings that the LLM needs
   to see on the NEXT iteration. Fixes every instance of the model looping
   on `(restore-vars ...)` / `(def X ...)` because the bindings never caught up."
  [env sym val]
  (sci-update-binding! (:sci-ctx env) sym val)
  (bump-bindings! env))

(defn push-eval-result!
  "REPL-style stack push for the sandbox `*1` `*2` `*3` recovery slots.
   Called by the iteration loop after each successful top-level form
   eval. Bypasses `<bindings>` invalidation - these vars are filtered
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
                    "for full-file reads and (v/preview value eql) for journal/TUI projections. "
                    "The sanctioned file surface stays cwd-safe and consistent with the prompt.")
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
  "Extra bindings beyond what SCI provides by default."
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
                       ;; sanctioned `v/` filesystem surface (`v/cat` for
                       ;; full-file acquisition, `v/preview` for display
                       ;; projection). `slurp` bypasses the prompt's
                       ;; path-policy and encourages ad-hoc I/O.
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
    ;; REPL-style recovery bindings: `*1` `*2` `*3` carry the last three
    ;; evaluated form values (most recent first); `*e` carries the most
    ;; recent uncaught exception. The iteration loop pushes onto the
    ;; stack after each form's eval and resets all four to nil at turn
    ;; start. They live in the sandbox so model code reads them as plain
    ;; symbols, but they are interned BEFORE `:initial-ns-keys` so the
    ;; baseline filter excludes them from `<bindings>` (they're noise to
    ;; the model's user-binding view).
    (sci/eval-string+ sci-ctx "(def *1 nil) (def *2 nil) (def *3 nil) (def *e nil)"
      {:ns sandbox-ns})
    {:sci-ctx sci-ctx
     :sandbox-ns sandbox-ns
     :initial-ns-keys (set (keys (:val (sci/eval-string+ sci-ctx "(ns-publics 'sandbox)" {:ns sandbox-ns}))))}))

(def ^:private ^:const MAX_BINDINGS_COUNT 1000)

;; SYSTEM vars are read-only bindings the loop maintains for the
;; agent. Three lifetime tiers, each tagged by its prefix:
;;
;;   TURN_*         frozen at turn start, immutable for the whole turn
;;     TURN_ID                     UUID of THIS in-flight turn soul.
;;     TURN_POSITION               1-based int position of THIS turn within
;;                                 its conversation_state. Per-conversation
;;                                 monotonic; matches conversation_turn_soul.position.
;;                                 Public-facing turn identifier (channels render
;;                                 "turn 7"; UUIDs stay programmatic-only).
;;                                 (Retired: TURN_USER_REQUEST. The exact human-
;;                                 authored turn text is now read lazily through
;;                                 the sandbox `(turn-request)` primitive - a
;;                                 closure over `:current-user-request-atom` -
;;                                 so the value is not pumped into per-iteration
;;                                 var-history rows. For richer history use
;;                                 `(v/conversation-state)` -> `:current-turn`/`:transcript`.)
;;     TURN_CONVERSATION_STATE_ID  UUID of the latest conversation_state row at
;;                                 turn start. Stable for the whole turn even if a
;;                                 sibling write changes title; only an
;;                                 explicit fork bumps the state id.
;;     TURN_SYSTEM_PROMPT          full assembled system prompt that drove THIS
;;                                 turn (core prompt + every active-extension
;;                                 prompt block). Useful for the model to
;;                                 verify what rules it is operating under
;;                                 without round-tripping through an extension.
;;     TURN_ACTIVE_EXTENSIONS      vec of compact maps describing every extension
;;                                 that passed activation for THIS turn.
;;                                 Iteration-loop binds it once and never mutates
;;                                 again within the turn so the model can rely on
;;                                 a stable view (`(filter ...) TURN_ACTIVE_EXTENSIONS`)
;;                                 without round-tripping through
;;                                 `(v/extensions)`. Shape per element:
;;                                   {:alias              'vis
;;                                    :namespace          'com.blockether.vis.ext.foundation.introspection
;;                                    :doc                "..."
;;                                    :kind               "..."            ;; when present
;;                                    :version            "..."            ;; when present
;;                                    :author             "..."            ;; when present
;;                                    :owner              "..."            ;; when present
;;                                    :license            "..."            ;; when present
;;                                    :registry-id        'v               ;; when present
;;                                    :source-paths       ["..."]
;;                                    :source-mtime-max   1714403520000
;;                                    :source-hash-sha256 "abc..."|nil
;;                                    :symbols            [sym1 sym2 ...]
;;                                    :docs               ["README.md" ...]}
;;
;;   ITERATION_*    rebound at every iteration boundary
;;     TURN_ITERATION_POSITION       1-based int position of THIS iteration
;;                                   within its turn. Per-turn monotonic;
;;                                   matches iteration.position. Bumped every
;;                                   iteration alongside TURN_ITERATION_ID.
;;     TURN_ITERATION_ID             UUID of the most recently persisted iteration
;;                                   row (nil before the first iteration commits;
;;                                   iteration N's :code sees iteration N-1's id
;;                                   because the row for N is written AFTER eval).
;;                                   Prior thinking text used to live under
;;                                   `ITERATION_PREVIOUS_REASONING`; that
;;                                   var was retired once preserved-thinking
;;                                   replay started shipping the canonical
;;                                   assistant message back to the provider.
;;                                   The DB still has every iteration's
;;                                   `:thinking` column for forensics.
;;
;;   CONVERSATION_* conversation-state, mutates freely within the turn
;;     CONVERSATION_STATE_ID         UUID of the current conversation_state branch.
;;                                   Mutates on fork; tools that need branch-aware
;;                                   identity read this. (CONVERSATION_ID and the
;;                                   raw *_SOUL_ID variants were retired — they
;;                                   were aliases / immutable-identity duplicates
;;                                   with no consumers.)
;;     CONVERSATION_TITLE            current conversation title ("" until set).
;;                                   The model writes via the host primitive
;;                                   `(conversation-title "...")`, never by
;;                                   `def`-ing it directly - the SYSTEM-var write
;;                                   guard in `loop.clj` rejects that on principle.
;;     CONVERSATION_PREVIOUS_ANSWER  previous turn's final answer string ("" on
;;                                   the very first turn). Despite being scoped
;;                                   to the conversation, it is rebound at every
;;                                   iteration so a `(answer ...)` call inside
;;                                   iteration N is observable in iteration N+1.
;;
;; UPPERCASE marks them as constants. The set is a fixed registry;
;; adding to it is a deliberate API change. See AGENTS.md -> "SYSTEM
;; vars are UPPERCASE and explicitly defined".
(def SYSTEM_VAR_NAMES
  "Fixed set of SYSTEM-var symbols. Used everywhere a 'is-this-a-system-
   var?' check is needed: bindings sort+status, archive guard, etc.

   See the comment block above this def for full per-name documentation
   and the prefix-based lifetime convention
   (`TURN_*`, `ITERATION_*`, `CONVERSATION_*`).

   Vocabulary note: `ACTIVE` vs `ACCESSIBLE`. An extension is ACTIVE when
   its activation-fn returned truthy and its symbols got intern'd into
   the SCI sandbox (the model can call `v/cat` directly because the var
   is loaded). A skill is ACCESSIBLE when the loader can find it on disk
   and surface its `:name`/`:description`; the body becomes a sandbox var
   only after the model calls `(load-skill \"name\")` - that's the
   internal activation step. Hence: TURN_ACTIVE_EXTENSIONS (loaded) vs
   TURN_ACCESSIBLE_SKILLS (discoverable, lazy-load on demand)."
  '#{TURN_ID
     TURN_POSITION
     TURN_CONVERSATION_STATE_ID
     TURN_SYSTEM_PROMPT
     TURN_ACTIVE_EXTENSIONS
     TURN_ACCESSIBLE_SKILLS
     TURN_ITERATION_ID
     TURN_ITERATION_POSITION
     CONVERSATION_STATE_ID
     CONVERSATION_TITLE
     CONVERSATION_PREVIOUS_ANSWER})

(defn system-var-sym?
  "True when `sym` is one of the registered SYSTEM-var names. The fixed
   set `SYSTEM_VAR_NAMES` is checked by membership, not pattern, so a
   user-defined uppercase var doesn't get misclassified as system."
  [sym]
  (contains? SYSTEM_VAR_NAMES sym))

(defn- var-status-keyword [{:keys [system? persisted?]}]
  (cond
    system? :sys
    persisted? :f
    :else :l))

(defn- var-type-keyword [val persisted?]
  (cond
    persisted? :persisted
    (nil? val) :nil
    (fn? val) :fn
    (map? val) :map
    (vector? val) :vector
    (set? val) :set
    (list? val) :list
    (sequential? val) :seq
    (string? val) :string
    (integer? val) :int
    (float? val) :float
    (boolean? val) :bool
    (keyword? val) :keyword
    (symbol? val) :symbol
    :else (some-> val class .getSimpleName str/lower-case keyword)))

(defn- var-size [val persisted?]
  (cond
    persisted? nil
    (nil? val) nil
    (string? val) (count val)
    (or (map? val) (vector? val) (set? val)) (count val)
    (sequential? val)
    (let [n (bounded-count MAX_BINDINGS_COUNT val)]
      (if (= n MAX_BINDINGS_COUNT) (str MAX_BINDINGS_COUNT "+") n))
    :else nil))

;; ---------------------------------------------------------------------------
;; Type-aware var rendering
;;
;; Cheap values get their actual content inlined so the model never has to
;; round-trip via `(var-history 'sym)` just to see what's in a var. Expensive
;; values fall back to a schema preview (key list, head, size). The render
;; emits VALID Clojure shape - stats live in a `;;` comment line, never as
;; reader-macro metadata onto the symbol (the old `^{:v N :s :l :t :map}` form
;; was fake-Clojure that confused parser priors).
;; ---------------------------------------------------------------------------

(def ^:private STRING_INLINE_MAX_CHARS 8000)
(def ^:private STRING_HEAD_PREVIEW_CHARS 800)
(def ^:private MAP_INLINE_MAX_KEYS 8)
(def ^:private MAP_KEYS_SAMPLE_SIZE 6)
(def ^:private SEQ_INLINE_MAX_ELEMS 5)
(def ^:private SEQ_HEAD_SAMPLE_SIZE 3)
(def ^:private DOCSTRING_FIRST_LINE_CHARS 100)
;; Per-entry char cap when rendering collection bodies. A 5-element
;; vector of 1000-char strings would otherwise blow up to 5000+ chars
;; even though our element-count check passes. This is the SAFETY net.
(def ^:private BINDINGS_BODY_MAX_CHARS 600)
(def ^:private BINDINGS_PRINT_LENGTH 32)
(def ^:private BINDINGS_PRINT_LEVEL 6)
(def ^:private MAX_BINDINGS_ENTRIES 100)
(def ^:private BINDINGS_SUMMARY_MAX_NAMES 10)

(defn- bounded-pr-str
  "Local mirror of `iteration.core/safe-pr-str` for the bindings render
   path - reuses `*print-length*` + `*print-level*` to short-circuit
   pr during printing, then clips the resulting string. Kept here so
   the env-core namespace doesn't depend on iteration.core (would be
   a cycle)."
  [v]
  (let [bounded (binding [*print-length* BINDINGS_PRINT_LENGTH
                          *print-level*  BINDINGS_PRINT_LEVEL]
                  (pr-str v))]
    (if (> (count bounded) BINDINGS_BODY_MAX_CHARS)
      (str (subs bounded 0 BINDINGS_BODY_MAX_CHARS)
        " ...<+" (- (count bounded) BINDINGS_BODY_MAX_CHARS) " chars>")
      bounded)))

(defn- truncate-string [s n]
  (if (and (string? s) (> (count s) n)) (subs s 0 n) s))

(defn- map-keys-preview [val]
  (let [ks (vec (keys val))]
    (if (<= (count ks) MAP_INLINE_MAX_KEYS)
      {:inline? true :keys ks}
      {:inline? false
       :keys-sample (vec (take MAP_KEYS_SAMPLE_SIZE ks))
       :total (count ks)})))

(defn- seq-head-preview [val]
  (let [n (var-size val false)
        sample-count (cond
                       (string? n) SEQ_HEAD_SAMPLE_SIZE          ;; "1000+" - lazy/big
                       (number? n) (min n SEQ_HEAD_SAMPLE_SIZE)
                       :else SEQ_HEAD_SAMPLE_SIZE)
        head (vec (take sample-count val))]
    {:total n :head head}))

(defn- stats-comment
  "Render the per-entry stats as a single `;;` comment line. Replaces the
   fake-Clojure metadata-on-symbol form. `:scope` uses full words
   (`:live | :archived | :system`); `:v` is the persisted version count."
  [{:keys [version status size]}]
  (let [scope-word (case status
                     :sys :system
                     :f   :archived
                     :l   :live
                     status)]
    (str ";; v=" version " scope=" (name scope-word)
      (when (some? size) (str " n=" size)))))

(defn- render-fn-form
  [{:keys [sym arglists doc] :as entry}]
  (let [stats   (stats-comment entry)
        single? (and (sequential? arglists)
                  (= 1 (count arglists))
                  (vector? (first arglists)))
        doc-line (when (and (string? doc) (not (str/blank? doc)))
                   (truncate-string
                     (-> (str doc) str/split-lines first str/trim)
                     DOCSTRING_FIRST_LINE_CHARS))
        body (cond
               doc-line (str " \"" doc-line "\" ...")
               :else " ...")
        sig  (cond
               single? (pr-str (first arglists))
               (seq arglists) (str/join " " (map pr-str arglists))
               :else "[& args]")]
    (str stats "\n(defn " sym " " sig body ")")))

(defn- render-data-form
  "Render a non-fn var with type-aware preview. Cheap values inline; large
   values fall back to a schema map (`{:n N :keys-sample [...]}` etc.).
   When a `:doc` is set on the var meta, embed the first line of the
   docstring before the body - same UX as render-fn-form, so all
   var-defining forms surface their purpose in `<bindings>`."
  [{:keys [sym type val doc] :as entry}]
  (let [stats (stats-comment entry)
        doc-line (when (and (string? doc) (not (str/blank? doc)))
                   (truncate-string
                     (-> (str doc) str/split-lines first str/trim)
                     DOCSTRING_FIRST_LINE_CHARS))
        body  (if (extension/tool-result? val)
                (bounded-pr-str {:tool-result true
                                 :success?    (:success? val)
                                 :op          (get-in val [:info :op])
                                 :hit-count   (some-> val :result :hits count)})
                (case type
                  ;; Scalars: pr-str output is bounded by the type. A
                  ;; literal `true` / `42` / `:foo` cannot blow up.
                  :nil     "nil"
                  :bool    (pr-str val)
                  :int     (pr-str val)
                  :float   (pr-str val)
                  :keyword (pr-str val)
                  :symbol  (pr-str val)
                  ;; Strings: capped before pr-str, so the printed form is
                  ;; bounded by STRING_HEAD_PREVIEW_CHARS + quotes.
                  :string  (let [s val]
                             (if (<= (count s) STRING_INLINE_MAX_CHARS)
                               (pr-str s)
                               (pr-str {:string-size (count s)
                                        :head (truncate-string s STRING_HEAD_PREVIEW_CHARS)})))
                  ;; Collections route through bounded-pr-str so an inline
                  ;; render of a 5-element vec of 1000-char strings can't
                  ;; produce a 5000+ char body.
                  :map     (let [{:keys [inline? keys keys-sample total]} (map-keys-preview val)]
                             (if inline?
                               (bounded-pr-str {:keys keys})
                               (bounded-pr-str {:n total :keys-sample keys-sample})))
                  (:vector :set :list :seq)
                  (let [{:keys [total head]} (seq-head-preview val)]
                    (cond
                      (and (number? total) (<= total SEQ_INLINE_MAX_ELEMS))
                      (bounded-pr-str (vec val))
                      :else
                      (bounded-pr-str {:n total :head head})))
                  ;; unknown type - punt to a value-only stub
                  (pr-str {:type type})))]
    (str stats
      "\n(def " sym
      (when doc-line (str " \"" doc-line "\""))
      " " body ")")))

(defn- render-var-form
  "Live-var render path. Persisted-only entries are routed to the archive
   block by the caller; this function should only see live entries."
  [{:keys [type] :as entry}]
  (if (= type :fn)
    (render-fn-form entry)
    (render-data-form entry)))

(defn- display-sym-name
  [sym]
  (name sym))

(defn- safe-defn-source?
  [expr]
  (boolean
    (when (string? expr)
      (re-find #"^\s*\((?:clojure.core/)?defn(?:-|\s)" expr))))

(defn- unavailable-archive-entry?
  [{:keys [value code]}]
  (and (map? value)
    (= :expr (:vis/ref value))
    (not (safe-defn-source? code))))

(defn- render-symbol-summary
  [label syms]
  (when (seq syms)
    (str ";; " label ": "
      (str/join ", " (map display-sym-name (take BINDINGS_SUMMARY_MAX_NAMES syms)))
      (when (> (count syms) BINDINGS_SUMMARY_MAX_NAMES)
        (str " (+" (- (count syms) BINDINGS_SUMMARY_MAX_NAMES) " more)")))))

(defn- render-bindings-summary
  [{:keys [overflow-live archived unavailable-count]}]
  (let [lines (cond-> []
                (seq overflow-live)
                (conj (str ";; overflow-live-symbols: " (count overflow-live))
                  (render-symbol-summary "hidden live symbols" overflow-live))
                (seq archived)
                (conj (str ";; archived-symbols: " (count archived)
                        (when (pos? (long (or unavailable-count 0)))
                          (str ", unavailable: " unavailable-count)))
                  (render-symbol-summary "recent archived" archived)
                  ";; use (var-history) to browse symbol history/info"))]
    (seq (keep identity lines))))

(defn build-bindings
  "Build the `<bindings>` block from user-defined vars in the SCI sandbox.

   Returns nil when no user vars exist; otherwise a multi-line string
   with one entry per `(def ...)`. SYSTEM vars (every name in
   `SYSTEM_VAR_NAMES` - `TURN_*`, `ITERATION_*`, `CONVERSATION_*`) and
   initial-ns bindings (tools, helpers) are excluded - the model reads
   SYSTEM vars by name directly from the sandbox.

   Sort order: most-recently-bound first."
  ([sci-ctx initial-ns-keys]
   (build-bindings sci-ctx initial-ns-keys nil nil nil nil))
  ([sci-ctx initial-ns-keys sandbox]
   (build-bindings sci-ctx initial-ns-keys sandbox nil nil nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-id]
   (build-bindings sci-ctx initial-ns-keys sandbox db-info conversation-id nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-id _opts]
   (let [sandbox-map (or sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox]))
         var-registry (when (and db-info conversation-id)
                        (persistance/db-latest-var-registry db-info conversation-id))
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
                           :arglists (:arglists (meta v))
                           :doc      (:doc (meta v))
                           :version  (get-in var-registry [s :version] 1)}]))
         keep? (fn [[sym _]]
                 (and (not (contains? initial-ns-keys sym))
                   (not (system-var-sym? sym))))
         live-entries (->> live-info
                        (filter keep?)
                        (sort-by (fn [[sym _]]
                                   [(- (long (recency-of sym))) (str sym)]))
                        (mapv (fn [[sym {:keys [val arglists doc version]}]]
                                {:sym sym
                                 :version version
                                 :status (var-status-keyword {:system? false :persisted? false})
                                 :type (var-type-keyword val false)
                                 :size (var-size val false)
                                 :val val
                                 :arglists arglists
                                 :doc doc})))
         rendered-live (take MAX_BINDINGS_ENTRIES live-entries)
         overflow-live (mapv :sym (drop MAX_BINDINGS_ENTRIES live-entries))
         live-syms     (set (keys live-info))
         archived-entries (->> (or var-registry {})
                            (remove (fn [[sym _]]
                                      (or (contains? live-syms sym)
                                        (contains? initial-ns-keys sym)
                                        (system-var-sym? sym))))
                            (sort-by (fn [[sym _]]
                                       [(- (long (recency-of sym))) (str sym)]))
                            vec)
         archived-syms (mapv first archived-entries)
         unavailable-count (count (filter (comp unavailable-archive-entry? second) archived-entries))
         summary-lines (render-bindings-summary {:overflow-live overflow-live
                                                 :archived archived-syms
                                                 :unavailable-count unavailable-count})
         lines (vec (concat (map render-var-form rendered-live) summary-lines))]
     (when (seq lines)
       (str/join "\n" lines)))))

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

                    (safe-defn-source? expr)
                    (do (sci/eval-string+ sci-ctx expr
                          {:ns (sci/find-ns sci-ctx 'sandbox)})
                      {:name name :restored-via :eval :success? true})

                    :else
                    {:name name
                     :restored-via :unavailable
                     :success? false
                     :reason :unsafe-restore
                     :guidance "Recreate intentionally to persist a new version."})
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

