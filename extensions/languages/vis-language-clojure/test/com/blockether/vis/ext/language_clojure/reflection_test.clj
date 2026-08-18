(ns com.blockether.vis.ext.language-clojure.reflection-test
  "Tests for the `:general` lint provider (reflection + boxed-math compiler
   warnings) in `reflection/compile-warnings`."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.reflection :as reflection]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- types [findings] (set (map #(get % "type") findings)))

(defdescribe
  compile-warnings-test
  (it "flags an unresolved interop call as a reflection warning"
      (let [fs
            (reflection/compile-warnings "(defn r [x] (.length x))" "<stdin>")

            refl
            (first (filter #(= "reflection" (get % "type")) fs))]

        (expect (contains? (types fs) "reflection"))
        (expect (= "warning" (get refl "level")))
        (expect (= "general" (get refl "provider")))
        (expect (= "<stdin>" (get refl "file")))
        (expect (number? (get refl "row")))
        (expect (number? (get refl "col")))
        (expect (string? (get refl "message")))))
  (it "flags boxed numeric ops as a boxed-math warning"
      (let [fs
            (reflection/compile-warnings "(defn add [a b] (+ a b))" "<stdin>")

            boxed
            (first (filter #(= "boxed-math" (get % "type")) fs))]

        (expect (contains? (types fs) "boxed-math"))
        (expect (= "general" (get boxed "provider")))
        (expect (= "warning" (get boxed "level")))))
  (it "reports clean, primitive-typed code with no findings"
      (expect (empty? (reflection/compile-warnings
                        "(defn add ^long [^long a ^long b] (unchecked-add a b))"
                        "<stdin>"))))
  (it "returns [] for blank code"
      (expect (= [] (reflection/compile-warnings "" "<stdin>")))
      (expect (= [] (reflection/compile-warnings nil "<stdin>"))))
  (it "never throws on a hard compile error"
      (expect (vector? (reflection/compile-warnings "(this is (not balanced" "<stdin>"))))
  (it "keeps the compiler-reported source when no file is given"
      (let [fs (reflection/compile-warnings "(defn r [x] (.length x))")]
        (expect (contains? (types fs) "reflection"))))
  (it "does not leak the throwaway namespace it compiles in"
      (let [before (set (map ns-name (all-ns)))]
        (reflection/compile-warnings "(ns leaky.probe) (defn r [x] (.length x))" "<stdin>")
        (expect (not (contains? (set (map ns-name (all-ns))) 'leaky.probe)))
        ;; no namespaces leaked at all
        (expect (= before (set (map ns-name (all-ns))))))))

(def side-effects "Bumped only if the provider EVALUATES code it was asked to lint." (atom 0))

(defdescribe
  compile-warnings-never-runs-the-target-test
  "Linting is not running. A lint target is any file the caller names, and a
   script DOES its work when loaded: `scripts/gen-audit.bb` ends in
   `(System/exit 0)`, and loading it inside the gateway JVM stopped the daemon
   and cancelled every session running on the host. Compilation must stay total
   over untrusted source."
  (it "does not evaluate a bare top-level call"
      (reset! side-effects 0)
      (reflection/compile-warnings
        "(swap! com.blockether.vis.ext.language-clojure.reflection-test/side-effects inc)"
        "<stdin>")
      (expect (zero? @side-effects)))
  (it
    "does not evaluate a top-level runner form"
    (reset! side-effects 0)
    (reflection/compile-warnings
      "(let [n 1] (swap! com.blockether.vis.ext.language-clojure.reflection-test/side-effects + n))"
      "<stdin>")
    (expect (zero? @side-effects)))
  (it
    "does not evaluate a `def` initializer, but still interns the var"
    (reset! side-effects 0)
    (let
      [fs
       (reflection/compile-warnings
         (str
           "(def x (swap! com.blockether.vis.ext.language-clojure.reflection-test/side-effects inc))\n"
           "(defn use-x [] x)")
         "<stdin>")]
      (expect (vector? fs))
      (expect (zero? @side-effects))))
  (it "compiles a JVM-ending form without ending this JVM"
      ;; If this regresses, the process running the suite simply disappears.
      (expect (vector? (reflection/compile-warnings "(System/exit 0)" "<stdin>")))
      (expect (vector? (reflection/compile-warnings
                         "(ns probe.script)\n(println \"Wrote audit/README.md\")\n(System/exit 0)"
                         "scripts/gen-audit.bb"))))
  (it "does not run reader-eval"
      (reset! side-effects 0)
      (reflection/compile-warnings
        "(def y #=(swap! com.blockether.vis.ext.language-clojure.reflection-test/side-effects inc))"
        "<stdin>")
      (expect (zero? @side-effects)))
  (it "still warns about code it refuses to run"
      (let [fs (reflection/compile-warnings "(let [x (identity \"s\")] (.length x))" "<stdin>")]
        (expect (contains? (types fs) "reflection"))))
  (it "keeps warning rows pointing at the real source line"
      (let [fs (reflection/compile-warnings "(ns probe.rows)\n\n(defn r [x] (.length x))"
                                            "<stdin>")]
        (expect (= 3 (get (first fs) "row"))))))


(def dependency-loads
  "Counts compile-time dependency loads triggered by [[dependency-loading-code]]."
  (atom 0))

(defn- dependency-loading-code
  "A lint target that loads a DEPENDENCY while it compiles.

   `require` inside an `ns` form does exactly this: it compiles another source
   under the same warning flags, into the same `*err*` capture, and those
   warnings carry that source's name. The macro reproduces it hermetically —
   `Compiler/load` with a foreign source name, run at compile time — so the test
   needs no classpath surgery. Row 5 is the target's own (and only) warning."
  []
  (str "(defmacro load-dependency []\n"
       "  (swap! com.blockether.vis.ext.language-clojure.reflection-test/dependency-loads inc)\n"
       "  (clojure.lang.Compiler/load\n"
       "    (java.io.StringReader. \"(defn dep [x] (.length x))\\n(defn dep-add [a b] (+ a b))\")\n"
       "    \"vis_phantom_dep.clj\" \"vis_phantom_dep.clj\")\n" "  nil)\n"
       "(defn own [s] (.length s))\n" "(load-dependency)\n"))

(defdescribe compile-warnings-attribution-test
             (it "reports only the target's own warnings, never a dependency's"
                 (reset! dependency-loads 0)
                 (let [code
                       (dependency-loading-code)

                       ;; the target's own (and only) warning, wherever it sits
                       own-row
                       (inc (count (take-while #(not (str/starts-with? % "(defn own"))
                                               (str/split-lines code))))

                       fs
                       (reflection/compile-warnings code "/tmp/vis-lint-attribution-target.clj")]

                   ;; The dependency really was compiled here — otherwise this proves nothing.
                   (expect (= 1 @dependency-loads))
                   (expect (= 1 (count fs)))
                   (expect (= own-row (get (first fs) "row")))
                   (expect (= "reflection" (get (first fs) "type")))
                   (expect (= #{"/tmp/vis-lint-attribution-target.clj"}
                              (set (map #(get % "file") fs))))
                   (expect (not-any? #(str/includes? (get % "message") "unchecked_add") fs)))))

(def compiles
  "How many times [[counted-compile]] has been expanded — i.e. how many times the
   compiler actually walked a probe source. Public because a linted target can
   only reach a PUBLIC macro of this namespace."
  (atom 0))

(defmacro counted-compile
  "Bumps [[compiles]] while it is being EXPANDED, and expands to nothing.

   Compilation is the entire cost this provider caches, and it leaves no other
   trace: identical findings prove nothing about whether the compiler ran. This
   macro makes the compile itself observable, so a cache hit can be proven to be
   a hit rather than a fast recompile."
  []
  (swap! compiles inc)
  nil)

(def ^:private probe-source
  "A target that both warns (unresolved interop) and records its own compilation."
  (str "(defn probe [^String s]"
       "  (com.blockether.vis.ext.language-clojure.reflection-test/counted-compile)"
       "  (.nope s))"))

(defn- loaded-libs-ref
  "`clojure.core`'s registry of loaded libs, the signal the cache invalidates on."
  []
  (var-get #'clojure.core/*loaded-libs*))

(defdescribe
  compile-warnings-cache-test
  (it "compiles one source once, however often it is linted"
      (reflection/reset-cache!)
      (reset! compiles 0)
      (let [a
            (reflection/compile-warnings probe-source "a.clj")

            b
            (reflection/compile-warnings probe-source "a.clj")]

        (expect (seq a))
        (expect (contains? (types a) "reflection"))
        (expect (= a b))
        (expect (= 1 @compiles))
        (expect (= 1 (get (reflection/cache-info) "hits")))
        (expect (= 1 (get (reflection/cache-info) "misses")))
        (expect (= 1 (get (reflection/cache-info) "size")))))
  (it "keys on the bytes and restamps the reported file"
      (reflection/reset-cache!)
      (reset! compiles 0)
      (let [a
            (reflection/compile-warnings probe-source "one.clj")

            b
            (reflection/compile-warnings probe-source "two.clj")]

        (expect (= 1 @compiles))
        (expect (every? #(= "two.clj" (get % "file")) b))
        (expect (= (map #(dissoc % "file") a) (map #(dissoc % "file") b)))))
  (it "recompiles a source that changed"
      (reflection/reset-cache!)
      (reset! compiles 0)
      (reflection/compile-warnings probe-source "a.clj")
      (reflection/compile-warnings (str probe-source "\n(defn other [x] (.nope x))") "a.clj")
      (expect (= 2 @compiles))
      (expect (= 2 (get (reflection/cache-info) "size"))))
  (it "forgets everything once a lib is loaded, since resolution can change"
      (reflection/reset-cache!)
      (reset! compiles 0)
      (let [phantom 'vis.lint.phantom.generation]
        (reflection/compile-warnings probe-source "a.clj")
        (try (dosync (commute (loaded-libs-ref) conj phantom))
             (reflection/compile-warnings probe-source "a.clj")
             (finally (dosync (commute (loaded-libs-ref) disj phantom))))
        (expect (= 2 @compiles))
        (expect (= 0 (get (reflection/cache-info) "hits")))))
  (it "empties on reset"
      (reflection/compile-warnings probe-source "a.clj")
      (reflection/reset-cache!)
      (expect (= {"hits" 0 "misses" 0 "size" 0} (dissoc (reflection/cache-info) "generation")))))

(defdescribe compile-warnings-never-touches-a-loaded-namespace-test
             (it "leaves the live namespace, its vars and the lib registry exactly as they were"
                 (let [target
                       'vis-lint-live-namespace-probe

                       live
                       "(ns vis-lint-live-namespace-probe) (defn answer [] :live)"

                       ;; Same namespace, different (stale, warning-producing) bytes — an
                       ;; unsaved buffer, an older checkout, a file being edited right now.
                       stale
                       (str "(ns vis-lint-live-namespace-probe)"
                            " (defn answer [] :linted)"
                            " (defn broken [^String s] (.nope s))")]

                   (try (binding [*ns* *ns*]
                          (load-string live))
                        (let [libs-before
                              (set (loaded-libs))

                              nses-before
                              (set (map ns-name (all-ns)))

                              fs
                              (reflection/compile-warnings stale "stale.clj")]

                          (expect (contains? (types fs) "reflection"))
                          (expect (= :live ((ns-resolve (find-ns target) 'answer))))
                          (expect (= libs-before (set (loaded-libs))))
                          (expect (= nses-before (set (map ns-name (all-ns))))))
                        (finally (remove-ns target)
                                 (dosync (commute (loaded-libs-ref) disj target)))))))
