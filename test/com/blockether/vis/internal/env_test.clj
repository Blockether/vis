(ns com.blockether.vis.internal.env-test
  "Smoke + alias coverage for `internal/env`'s SCI sandbox.

   Why it exists:
     1. AGENTS.md's hard rule — every source namespace ships with a
        `_test.clj`. `env.clj` is the SCI sandbox factory; nothing
        in the runtime is more central, and a regression here
        silently breaks every model call.
     2. The `:ns-aliases` map is the model's main contact surface
        with the host stdlib. The system prompt advertises specific
        short-form aliases (`str/`, `set/`, `walk/`, `s/`, …) and a
        regression that quietly drops one of those mappings would
        surface only in the model trace as `Unable to resolve
        symbol`. Pinning the alias resolution here means a typo /
        accidental drop fails CI instead of a turn.
     3. `clojure.spec.alpha` is special: spec macros expand into
        internal `*-impl` helpers (`def-impl`, `and-spec-impl`,
        `cat-impl`, …) that all carry `^:skip-wiki` meta. Without
        an explicit `:exclude-when-meta []` opt-out, `sci/copy-ns`
        filters the impls out and `(s/def ::id int?)` blows up at
        runtime with `Unable to resolve symbol:
        clojure.spec.alpha/def-impl`. The spec-end-to-end test
        below pins both the macro expansion and the runtime
        behaviour."
  (:require
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.format :as fmt]
   [sci.core :as sci]
   [lazytest.core :refer [defdescribe describe expect it]])
  (:import
   [java.util.concurrent CountDownLatch]))

(defn- fresh-ctx
  "A pristine sandbox per test. Spec definitions write to the
   per-context registry; sharing a context across `it`s would let
   a `(s/def ::id int?)` from one test leak into the next."
  []
  (env/create-sci-context nil))

(defn- eval-in [ctx-map src]
  (let [{:keys [sci-ctx sandbox-ns]} ctx-map]
    (:val (sci/eval-string+ sci-ctx src {:ns sandbox-ns}))))

(defn- run-concurrently!
  "Run `f` and `g` from the same start gate, return a vec of any thrown
   messages tagged with the worker label. The zprint regression needs real
   overlap — sequential calls never tickle the library's global in-flight
   guard."
  [f g]
  (let [gate (CountDownLatch. 1)
        errs (atom [])
        spawn (fn [label thunk]
                (doto
                  (Thread.
                    ^Runnable
                    (fn []
                      (.await gate)
                      (try
                        (thunk)
                        (catch Throwable t
                          (swap! errs conj (str label ": " (or (ex-message t) (str t))))))))
                  (.start)))
        t1   (spawn "f" f)
        t2   (spawn "g" g)]
    (.countDown gate)
    (.join ^Thread t1)
    (.join ^Thread t2)
    @errs))

(defdescribe stdlib-aliases-resolve-in-sandbox-test
  (describe "the short-form aliases the system prompt advertises"
    ;; Each `it` confirms the alias maps to its documented namespace
    ;; AND that a representative call returns a sane value. If the
    ;; alias map ever drifts away from `prompt.clj`'s "Stdlib aliases"
    ;; block these tests fail and tell us which line of the prompt
    ;; lies.
    (it "`str/` -> clojure.string"
      (let [ctx (fresh-ctx)]
        (expect (= "HI"      (eval-in ctx "(str/upper-case \"hi\")")))
        (expect (= true      (eval-in ctx "(str/blank? \"   \")")))
        (expect (= "a, b, c" (eval-in ctx "(str/join \", \" [\"a\" \"b\" \"c\"])")))))

    (it "`set/` -> clojure.set"
      (let [ctx (fresh-ctx)]
        (expect (= #{1 2 3} (eval-in ctx "(set/union #{1 2} #{2 3})")))
        (expect (= #{1}     (eval-in ctx "(set/difference #{1 2} #{2})")))))

    (it "`walk/` -> clojure.walk"
      (let [ctx (fresh-ctx)]
        (expect (= {:a 1 :b 2} (eval-in ctx "(walk/keywordize-keys {\"a\" 1 \"b\" 2})")))))

    (it "`edn/` -> fast-edn.core"
      (let [ctx (fresh-ctx)]
        (expect (= [1 2 3] (eval-in ctx "(edn/read-string \"[1 2 3]\")")))))

    (it "`json/` -> charred.api"
      (let [ctx (fresh-ctx)]
        (expect (= {"a" 1} (eval-in ctx "(json/read-json \"{\\\"a\\\":1}\")")))))

    (it "`pp/` and `pprint/` -> clojure.pprint"
      (let [ctx (fresh-ctx)]
        (expect (string? (eval-in ctx "(pp/pprint-str {:a 1})")))
        (expect (string? (eval-in ctx "(pprint/pprint-str {:a 1})")))))

    (it "`zp/` -> zprint.core"
      (let [ctx (fresh-ctx)]
        (expect (string? (eval-in ctx "(zp/zprint-str {:a 1})")))))

    (it "`lt/` -> lazytest.core"
      (let [ctx (fresh-ctx)]
        ;; lazytest is wired in via a hand-rolled binding map, not
        ;; `sci/copy-ns`, so the alias resolves to a raw fn object,
        ;; not a sci `Var`. That's expected; we just probe that the
        ;; symbol resolves at all (a regression that drops
        ;; `lt -> lazytest.core` from `:ns-aliases` would throw
        ;; "Unable to resolve symbol" here).
        (expect (fn? (eval-in ctx "lt/expect-fn")))
        (expect (fn? (eval-in ctx "lt/throws?")))))

    (it "`test/` -> clojure.test (re-routed through lazytest)"
      (let [ctx (fresh-ctx)]
        ;; clojure.test/is is mapped onto lazytest's expect-fn so
        ;; model-authored test code uses one assertion engine.
        ;; Same alias-resolution probe as `lt/` above.
        (expect (fn? (eval-in ctx "test/is")))))

    (it "`c+/` -> clojure+.core"
      (let [ctx (fresh-ctx)]
        (expect (= :a (eval-in ctx "(c+/cond+ true :a :else :b)")))))))

(defdescribe zprint-concurrency-regression-test
  (describe "sandbox pretty-printing shares the render path's zprint lock"
    ;; The user's conversation hit exactly this race: sandbox
    ;; `(pp/pprint-str …)` formatted a data structure while the TUI
    ;; render thread formatted code with `:parse-string? true`, and
    ;; zprint exploded with `Attempted to run zprint with type ...`.
    ;;
    ;; The regression net below hits the SAME mixed-mode pair:
    ;;   * sandbox alias surface (`pp/pprint-str`) -> structure mode
    ;;   * host render helper (`format-clojure`)  -> zipper mode
    ;;
    ;; Pre-fix, this test reliably produced BOTH symptoms the user
    ;; saw in diagnostics:
    ;;   1. sandbox side throws the re-entrancy exception
    ;;   2. format-clojure swallows its own zprint exception and falls
    ;;      back to the raw unformatted source string
    (it "`pp/pprint-str` can race `format-clojure` without re-entrancy failures"
      (let [ctx       (fresh-ctx)
            src       "(defn f[x](let[a 1](+ a x)))"
            expected  (fmt/format-clojure src 20)
            outputs   (atom [])
            errs      (run-concurrently!
                        #(eval-in ctx
                           "(dotimes [i 200]
                              (pp/pprint-str {:label :sandbox :i i :data (vec (range 100))}))")
                        #(dotimes [_ 200]
                           (swap! outputs conj (fmt/format-clojure src 20))))]
        (expect (= [] errs))
        (expect (= #{expected} (set @outputs)))))))

(defdescribe spec-alpha-end-to-end-test
  (describe "clojure.spec.alpha is fully usable from the sandbox"
    ;; Spec is the regression-net flagship of this file. The macros
    ;; expand into `^:skip-wiki` impl helpers; if we ever drop the
    ;; `:exclude-when-meta []` opt-out from `(sci/copy-ns
    ;; clojure.spec.alpha …)` every assertion below blows up and
    ;; tells us exactly that.

    (it "`s/def` registers a predicate spec; `s/valid?` reads it back"
      (let [ctx (fresh-ctx)]
        (expect (= :sandbox/id (eval-in ctx "(s/def ::id int?)")))
        (expect (= true        (eval-in ctx "(s/valid? ::id 42)")))
        (expect (= false       (eval-in ctx "(s/valid? ::id \"nope\")")))))

    (it "`s/conform` returns the conformed value or `:invalid`"
      (let [ctx (fresh-ctx)]
        (eval-in ctx "(s/def ::id int?)")
        (expect (= 7 (eval-in ctx "(s/conform ::id 7)")))
        (expect (= :clojure.spec.alpha/invalid
                  (eval-in ctx "(s/conform ::id \"x\")")))))

    (it "`s/keys` composes via `:req-un` (the macro that drove the bug)"
      (let [ctx (fresh-ctx)]
        (eval-in ctx "(s/def ::x int?)")
        (eval-in ctx "(s/def ::y int?)")
        (eval-in ctx "(s/def ::point (s/keys :req-un [::x ::y]))")
        (expect (= true  (eval-in ctx "(s/valid? ::point {:x 1 :y 2})")))
        (expect (= false (eval-in ctx "(s/valid? ::point {:x 1})")))
        (let [explanation (eval-in ctx "(s/explain-str ::point {:x 1})")]
          (expect (string? explanation))
          (expect (re-find #"contains\?" explanation)))))

    (it "regex op specs work: `s/and`, `s/coll-of`, `s/cat`"
      (let [ctx (fresh-ctx)]
        (expect (= 5 (eval-in ctx "(s/conform (s/and int? pos?) 5)")))
        (expect (= :clojure.spec.alpha/invalid
                  (eval-in ctx "(s/conform (s/and int? pos?) -3)")))
        (expect (= [1 2 3]
                  (eval-in ctx "(s/conform (s/coll-of int?) [1 2 3])")))
        (expect (= {:n 42 :s "hi"}
                  (eval-in ctx "(s/conform (s/cat :n int? :s string?) [42 \"hi\"])")))))))

(defdescribe sandbox-bindings-smoke-test
  (describe "sandbox factory still produces the basics"
    ;; Trip-wire layer in case someone reshapes `create-sci-context`
    ;; and the alias / spec tests above keep passing for the wrong
    ;; reason (e.g. spec wired but sandbox-ns missing).
    (it "`create-sci-context` returns the ctx + sandbox-ns + initial-ns-keys"
      (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]} (env/create-sci-context nil)]
        (expect (some? sci-ctx))
        (expect (some? sandbox-ns))
        (expect (set? initial-ns-keys))
        (expect (pos? (count initial-ns-keys)))))

    (it "trivial expression eval round-trips"
      (let [ctx (fresh-ctx)]
        (expect (= 3 (eval-in ctx "(+ 1 2)")))
        (expect (= "vis" (eval-in ctx "(name :vis)")))))))

(defdescribe catch-throwable-allows-getmessage-test
  ;; Regression net for the d8aff512 conversation footgun:
  ;; `(catch Exception e (.getMessage e))` blew up with
  ;; "Method getMessage on class java.lang.NullPointerException not
  ;; allowed!" because only `java.lang.Exception` was on SCI's
  ;; `:classes` allow-list — SCI checks methods against the actual
  ;; runtime class, and the thrown instance was an NPE / IAE / ISE.
  ;; The model wrote idiomatic `(catch … (.getMessage e))` and
  ;; got back gibberish for an entire iteration.
  ;;
  ;; Each `it` below exercises one common runtime-exception class.
  ;; A regression that drops one of them from `:classes` /
  ;; `:imports` lights up the matching test instead of bouncing the
  ;; LLM through wasted iterations.
  (describe "every common runtime exception's `.getMessage` is callable"
    (it "NullPointerException — the original repro"
      (let [ctx (fresh-ctx)]
        (expect (= "oops" (eval-in ctx
                            "(try (throw (NullPointerException. \"oops\"))
                                  (catch Exception e (.getMessage e)))")))))

    (it "IllegalArgumentException — `(into 5 [1])` etc."
      (let [ctx (fresh-ctx)]
        (expect (= "bad arg" (eval-in ctx
                               "(try (throw (IllegalArgumentException. \"bad arg\"))
                                     (catch Exception e (.getMessage e)))")))))

    (it "IllegalStateException — typical clj transducer / atom violations"
      (let [ctx (fresh-ctx)]
        (expect (= "bad state" (eval-in ctx
                                 "(try (throw (IllegalStateException. \"bad state\"))
                                       (catch Exception e (.getMessage e)))")))))

    (it "ClassCastException — type errors"
      (let [ctx (fresh-ctx)]
        (expect (= "cast" (eval-in ctx
                            "(try (throw (ClassCastException. \"cast\"))
                                  (catch Exception e (.getMessage e)))")))))

    (it "ArithmeticException — divide by zero"
      (let [ctx (fresh-ctx)]
        ;; The model's most natural form: catch the runtime division
        ;; error and surface its message. Pre-fix this threw
        ;; "Method getMessage on class java.lang.ArithmeticException
        ;; not allowed!".
        (expect (string?
                  (eval-in ctx
                    "(try (/ 1 0)
                          (catch Exception e (.getMessage e)))")))))

    (it "NumberFormatException — bad parse"
      (let [ctx (fresh-ctx)]
        (expect (string?
                  (eval-in ctx
                    "(try (Long/parseLong \"NaN\")
                          (catch Exception e (.getMessage e)))")))))

    (it "`(catch Throwable t …)` short form resolves via :imports"
      ;; Confirms that the import alias was added too — not just the
      ;; FQN class entry. Without the import, `(catch Throwable t)`
      ;; would throw "Could not resolve class: Throwable".
      (let [ctx (fresh-ctx)]
        (expect (= "x" (eval-in ctx
                         "(try (throw (RuntimeException. \"x\"))
                               (catch Throwable t (.getMessage t)))")))))

    (it "`(catch NullPointerException e …)` short form resolves"
      (let [ctx (fresh-ctx)]
        (expect (= "npe" (eval-in ctx
                           "(try (throw (NullPointerException. \"npe\"))
                                 (catch NullPointerException e (.getMessage e)))")))))

    (it "`.getClass` works on caught instances (model often inspects type)"
      (let [ctx (fresh-ctx)]
        ;; Returns the Class object; we just probe that the call
        ;; doesn't throw "method not allowed".
        (expect (some? (eval-in ctx
                         "(try (throw (NullPointerException. \"x\"))
                               (catch Exception e (.getClass e)))")))))))

