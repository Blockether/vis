(ns com.blockether.vis.ext.language-clojure.reflection
  "The `:general` lint provider: Clojure COMPILER warnings — reflection and
   boxed math.

   Unlike clj-kondo (static analysis over source text), these warnings only
   exist at COMPILE time: the compiler emits them while it resolves interop /
   code. So this provider COMPILES whatever the lint TARGETS — a `lint_code` code
   string, or each source file being linted — in a throwaway namespace that is
   torn down afterwards, so the running system is never mutated and nothing leaks.

   It compiles the code in a throwaway namespace with `*warn-on-reflection*` and
   `*unchecked-math* :warn-on-boxed` bound, captures the compiler's `*err*`
   stream, and parses each warning line

     `Reflection warning, <file>:<row>:<col> - <message>`
     `Boxed math warning, <file>:<row>:<col> - <message>`

   into the uniform lint finding map, tagged `\"provider\" \"general\"`:
   `{\"file\" \"row\" \"col\" \"level\" \"warning\" \"type\" \"reflection\"|\"boxed-math\"
     \"message\" \"provider\" \"general\"}`."
  (:require [clojure.string :as str]))

(def provider "The provider tag every finding from this namespace carries." "general")

(def ^:private warning-re
  "Matches one compiler warning line: kind, source, row, col, message."
  #"(?m)^(Reflection|Boxed math) warning, (.+?):(\d+):(\d+) - (.+)$")

(defn- warning->finding
  "Shape one regex match (a `[whole kind src row col message]` vector) into the
   uniform finding map. `file` overrides the compiler-reported source (which is
   `null` for a `load-string` snippet); when nil the reported source is kept."
  [file [_ kind src row col message]]
  {"file" (if (str/blank? (str file)) src file)
   "row" (Long/parseLong row)
   "col" (Long/parseLong col)
   "level" "warning"
   "type" (if (= kind "Boxed math") "boxed-math" "reflection")
   "message" message
   "provider" provider})

(def ^:private definition-heads
  "Top-level heads whose evaluation only DEFINES something (a var, a type, a
   method, a namespace alias) and therefore may run as written: later forms need
   those vars/imports to resolve, or the compiler reports noise instead of
   reflection. Everything NOT in here is compiled inside a `(fn [] …)` — the
   warnings are identical, the body never runs."
  '#{ns in-ns require use import refer refer-clojure alias declare set! comment def- defn defn-
     defmacro defmulti defmethod defprotocol definterface definline defstruct deftype defrecord
     extend extend-type extend-protocol gen-class proxy-super})

(def ^:private var-heads
  "`def`/`defonce`: the VAR must exist for later forms, but the initializer is
   ordinary code — `(def x (delete-everything!))`. Interned unbound, initializer
   compiled but not run."
  '#{def defonce})

(def ^:private context-heads
  "Heads handled by [[establish-context!]] rather than the compiler: they do not
   describe code to warn about, they describe WHERE the rest of the file resolves
   its symbols. Applied as ordinary function calls, so no class is generated for
   them at all."
  '#{ns in-ns require use import refer refer-clojure alias})

(def ^:private value-heads
  "The one set that still has to be EVALUATED, and the only `eval` left here.

   A macro is not usable by a later form unless its var holds a value, and a type
   is not referable by name unless its class exists — analysis interns the var
   but assigns nothing, so a file that defines a macro and then uses it would
   fail to analyse every form after it, and lose exactly the warnings this
   provider exists to report. Measured across this repo's largest namespaces
   (`server.clj`, `loop.clj`, `human_input.clj`) not one top-level form falls in
   here, so the common file pays nothing for it."
  '#{defmacro definterface deftype defrecord defprotocol gen-class})

(defn- import-spec!
  "Apply ONE `:import` spec to the current namespace without evaluating it.

   `import` is a macro, so the only eval-free route is the runtime one: resolve
   each class and hand it to the namespace directly. A class that will not
   resolve is skipped rather than fatal — a lint target may name something this
   JVM does not have, and that is a finding for clj-kondo, not a crash here."
  [spec]
  (doseq [^String cname (if (or (vector? spec) (list? spec))
                          (let [[pkg & classes] spec]
                            (map #(str pkg "." %) classes))
                          [(str spec)])]
    (try (.importClass ^clojure.lang.Namespace *ns* (Class/forName cname false (clojure.lang.RT/baseLoader)))
         (catch Throwable _ nil))))

(defn- establish-context!
  "Give the shadow namespace the same resolution context the target file has —
   its requires, aliases, refers and imports — WITHOUT evaluating its `ns` form.

   The `ns` macro expands into exactly these calls, and `require`/`use`/`refer`
   are plain functions, so reading the clauses and calling them reaches the same
   state while generating no class. Requiring a library this JVM already loaded
   is a registry lookup; requiring one it has not is the load it would have done
   anyway, and [[compute-findings]] drops the warnings that come from inside it.

   Every clause is best-effort: a malformed or unsatisfiable one costs its own
   resolution, never the rest of the file."
  [form]
  (let [head (when (seq? form) (first form))]
    (case head
      (ns) (doseq [clause (drop 2 form)
                   :when (seq? clause)]
             (let [[kind & specs] clause]
               (try (case kind
                      (:require) (apply require specs)
                      (:use) (apply use specs)
                      (:import) (run! import-spec! specs)
                      (:refer-clojure) (apply refer 'clojure.core specs)
                      nil)
                    (catch Throwable _ nil))))
      ;; A bare top-level `require`/`import`/… names its specs quoted; unquoting
      ;; is all that separates the form from the call it stands for.
      (require use) (try (apply (if (= 'use head) use require)
                                (map #(if (and (seq? %) (= 'quote (first %))) (second %) %)
                                     (rest form)))
                         (catch Throwable _ nil))
      (import) (run! import-spec!
                     (map #(if (and (seq? %) (= 'quote (first %))) (second %) %) (rest form)))
      (refer-clojure) (try (apply refer 'clojure.core (rest form)) (catch Throwable _ nil))
      nil))
  nil)

(defn- shadowed
  "`form` with the namespace it installs itself into renamed to `shadow`.

   A lint target is usually the source of a namespace that is ALREADY LOADED in
   this JVM. Its `ns`/`in-ns` form must still evaluate — the `:require`s are what
   let the rest of the file resolve — but evaluating it verbatim switches `*ns*`
   to the live namespace, and every `defn` in the file then REDEFINES that
   namespace's vars from the linted bytes. Linting a file was therefore enough
   to reload it: an editor-stale or half-written buffer would quietly replace
   the running code. Renaming the namespace keeps the requires and drops the
   definitions into a throwaway namespace that [[compute-findings]] deletes."
  [form shadow]
  (case (first form)
    ns
    (list* 'ns shadow (rest (rest form)))

    in-ns
    (list 'in-ns (list 'quote shadow))

    form))

(defn- compilable
  "ONE top-level form rewritten so that COMPILING it cannot RUN it, and cannot
   touch this JVM's live namespaces.

   This is the whole safety property of this provider. Lint targets are arbitrary
   files — build scripts, `bb` scripts, `deps.edn`-adjacent tooling — and a script
   is written to DO its work when loaded. Loading `scripts/gen-audit.bb` (whose
   last top-level form is `(System/exit 0)`) inside the gateway JVM shut the
   daemon down and cancelled every running session on the host; that is what this
   rewrite exists to prevent.

   Definitions ([[definition-heads]]) pass through, with `ns`/`in-ns` redirected
   to `shadow` (see [[shadowed]]). `def`/`defonce` become an unbound `def` plus
   their initializer under a `fn`. Anything else — a bare call, a `let` runner, a
   `(System/exit 0)` — is wrapped in `(fn [] …)`: the compiler walks the body and
   emits the same reflection/boxed-math warnings at the same rows, and nothing
   executes."
  [form shadow]
  (let [head (when (seq? form) (first form))]
    (cond (contains? definition-heads head) (shadowed form shadow)
          (and (contains? var-heads head) (symbol? (second form)))
          (let [[_ nm & more] form
                init (when (seq more) (last more))]

            (if (seq more) (list 'do (list 'def nm) (list 'fn [] init)) (list 'def nm)))
          :else (list 'fn [] form))))

(defn- compile-forms!
  "Read `code` form by form (line/column metadata intact, so warning rows keep
   pointing at the real source) and compile each through [[compilable]], with the
   target's own namespace redirected to `shadow`. Reading is `*read-eval*`-free:
   `#=(…)` must not run either. One unreadable or uncompilable form ends/skips
   that form only — the warnings already emitted are kept, exactly as a partially
   compiling file behaved before.

   ANALYSED, not evaluated. Reflection and boxed-math warnings are emitted while
   the compiler RESOLVES a form, so `Compiler/analyze` reports them in full: on a
   file of 60 warning-producing defns both routes return the same 180 findings,
   warning for warning and column for column. What `eval` additionally did was
   generate the bytecode and load it through a fresh `DynamicClassLoader` — work
   this provider never needed, since it only ever reads the compiler's warnings
   and throws the code away.

   This is a SAFETY and honesty change, not a memory fix, and the numbers say so:
   linting this repo's ten largest internal namespaces loads 23 139 classes
   analysed against 23 713 evaluated — 2.4%. Nearly all of that is the target's
   own `:require`s being loaded, which compiles those libraries and happens
   either way; the target's own forms were never the expensive part. Repeat lints
   of one file already cost nothing, because the findings cache answers them.

   The value is that arbitrary linted code is no longer EXECUTED to be inspected.
   [[compilable]]'s `(fn [] …)` wrapper made execution unreachable, but analysis
   removes the code path instead of hiding it.

   [[context-heads]] never reach the compiler ([[establish-context!]] applies
   them as function calls); [[value-heads]] are the narrow set that still must be
   evaluated, because analysis interns a var without giving it the value a later
   macro expansion or type reference needs."
  [code shadow]
  (let [rdr
        (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. (str code)))

        opts
        {:eof ::eof :read-cond :allow :features #{:clj}}]

    (binding [*read-eval* false]
      (loop []

        (let [form (try (read opts rdr) (catch Throwable _ ::eof))]
          (when-not (= ::eof form)
            (let [head (when (seq? form) (first form))]
              (try (cond (contains? context-heads head) (establish-context! form)
                         (contains? value-heads head) (eval (compilable form shadow))
                         :else (clojure.lang.Compiler/analyze
                                 clojure.lang.Compiler$C/STATEMENT
                                 (compilable form shadow)))
                   (catch Throwable _ nil)))
            (recur)))))))

(defn- forget-lib!
  "Drop `lib` from `clojure.core`'s registry of loaded libs.

   Evaluating an `ns` form registers the namespace it names as loaded. Here that
   name is always a throwaway shadow, so without this every single lint leaks a
   symbol into a global ref that only ever grows — and [[loaded-generation]],
   which watches that registry to know when cached findings went stale, would
   see the world change on every call and never reuse anything."
  [lib]
  (dosync (commute (var-get #'clojure.core/*loaded-libs*) disj lib)))

(defn- compute-findings
  "Compile `code` once with the compiler's reflection and boxed-math warnings on
   and return the parsed findings, each stamped with `source`.

   Compiling is the point; RUNNING the target never is, and neither is changing
   this JVM. Linting must be safe on any file a caller names, so every top-level
   form goes through [[compilable]] first: definitions define, and all other code
   is compiled inside a `fn` that is never called. The target's own `ns` is
   renamed ([[shadowed]]), so its definitions land in a throwaway namespace that
   is dropped here instead of redefining a loaded namespace from the linted
   bytes. A hard compile error yields whatever warnings were emitted before it
   (possibly none) — never throws.

   What the target `require`s is deliberately left loaded. Those libs are now
   registered as loaded whether or not this lint triggered them, and unloading a
   namespace the registry still calls loaded would leave a later `require`
   satisfied by nothing.

   Only warnings the compiler attributes to THIS source are kept. A target's
   `ns`/`require` still loads its dependencies, and loading a library compiles
   that library's own sources under the same warning flags: linting one file
   whose deps were not yet loaded otherwise reported hundreds of findings from
   inside those libraries, at their rows, stamped with the target's path."
  [code source]
  (let [shadow
        (gensym "vis-lint-reflect-")

        sw
        (java.io.StringWriter.)]

    (try (binding [*err*
                   sw

                   *warn-on-reflection*
                   true

                   *unchecked-math*
                   :warn-on-boxed

                   *file*
                   source

                   *ns*
                   (create-ns shadow)]

           (clojure.core/refer-clojure)
           (compile-forms! code shadow))
         (into []
               (comp (filter (fn [[_ _ src]]
                               (= src source)))
                     (map (partial warning->finding source)))
               (re-seq warning-re (str sw)))
         (finally (remove-ns shadow) (forget-lib! shadow)))))

(def ^:private cache-capacity
  "How many distinct sources keep their findings — about one project's worth of
   files. Past this the least recently linted source is evicted."
  512)

(defonce
  ^{:private true
    :doc
    "Content digest -> `[generation findings]`, least-recently-used evicted first.

   Compiling IS the cost of this provider: every top-level form of the target is
   analyzed and its bytecode generated (that is what emits the warnings), which
   the JVM serializes, so parallelism buys almost nothing. The same bytes always
   compile to the same warnings, so the only real win is not compiling the same
   bytes twice — re-linting a project after one edit costs a SHA-256 per file
   instead of a compile per file. The generation stored beside each entry says
   which world it was computed in; see [[cached]]."}
  cache
  (java.util.Collections/synchronizedMap
    (proxy [java.util.LinkedHashMap] [64 0.75 true]
      (removeEldestEntry [_eldest] (> (.size ^java.util.Map this) (long cache-capacity))))))

(defonce
  ^{:private true
    :doc
    "Hit/miss counters for [[cache-info]]. Diagnostics only — nothing reads
   them to decide anything."}
  cache-stats
  (atom {"hits" 0 "misses" 0}))

(defn- loaded-generation
  "How many libs are loaded right now.

   Which calls the compiler can resolve — and therefore which of them reflect —
   depends on what is loaded, so a cached finding is only reused while this is
   unchanged. A count is enough: `require` only ever adds, and this registry is
   the one thing a lint of an unrelated file can change under us."
  []
  (count (loaded-libs)))

(defn- digest
  "SHA-256 of the target's bytes, the cache key. Content-addressed on purpose: a
   renamed or copied file reuses the findings, and the `\"file\"` each finding
   reports is stamped on afterwards."
  [^String code]
  (.encodeToString (java.util.Base64/getUrlEncoder)
                   (.digest (java.security.MessageDigest/getInstance "SHA-256")
                            (.getBytes code "UTF-8"))))

(defn- cached
  "Findings for `key`, computed by `compute` on a miss.

   Every entry records the load generation it was produced under and is reused
   only while that still matches: which calls the compiler can resolve depends on
   what is loaded, so a finding is worth exactly as much as the world that
   produced it. The generation is read again AFTER computing, because compiling a
   target loads the target's own dependencies.

   Stale entries are dropped one at a time on lookup instead of clearing the map
   whenever the generation moves: a first project-wide lint loads new libs almost
   every file, and a global reset there kept throwing away everything just
   computed — the second pass over an unchanged project still cost seconds."
  [key compute]
  (let [generation
        (loaded-generation)

        hit
        (.get ^java.util.Map cache key)]

    (if (= generation (nth hit 0 ::miss))
      (do (swap! cache-stats update "hits" inc) (nth hit 1))
      (let [findings (compute)]
        (.put ^java.util.Map cache key [(loaded-generation) findings])
        (swap! cache-stats update "misses" inc)
        findings))))

(defn cache-info
  "Diagnostics for the findings cache: `\"size\"`, `\"hits\"`, `\"misses\"`, and the
   current load `\"generation\"` — entries recorded under an older one are dead
   weight until [[cached]] reaches them."
  []
  (assoc @cache-stats
    "size" (.size ^java.util.Map cache)
    "generation" (loaded-generation)))

(defn reset-cache!
  "Forget every cached target and zero the counters. Nothing in normal linting
   needs this — it exists for tests and for a caller that knows the world changed
   under it."
  []
  (.clear ^java.util.Map cache)
  (reset! cache-stats {"hits" 0 "misses" 0})
  nil)

(defn compile-warnings
  "Compile `code` (a Clojure source string) with the compiler's reflection and
   boxed-math warnings on, and return the parsed findings vector (each tagged
   `\\\"provider\\\" \\\"general\\\"`).

   `file` (optional) is reported as each finding's `\\\"file\\\"` — pass the linted
   target (or `\\\"<stdin>\\\"` for a snippet) so these findings group with the
   clj-kondo ones.

   Unchanged bytes are never compiled twice: findings are cached by content
   digest (see [[cache]]) and the reported `\\\"file\\\"` is stamped on afterwards, so
   re-linting a project only compiles what actually changed. See
   [[compute-findings]] for what one compile does and does not do."
  ([code] (compile-warnings code nil))
  ([code file]
   (if (str/blank? (str code))
     []
     (let [;; The compiler prints `*file*` as a warning's source; keep the old
           ;; `NO_SOURCE_PATH` shape when the caller named no target so every
           ;; warning line still parses, and use it to tell our own warnings from
           ;; those emitted while dependencies load.
           source
           (if (str/blank? (str file)) "NO_SOURCE_PATH" (str file))

           code
           (str code)]

       (mapv #(assoc % "file" source) (cached (digest code) #(compute-findings code source)))))))
