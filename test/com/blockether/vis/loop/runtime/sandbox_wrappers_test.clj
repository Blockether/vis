(ns com.blockether.vis.loop.runtime.sandbox-wrappers-test
  "Contract tests for the LLM-friendly sandbox wrappers wired into
   `create-sci-context`:

   * `slurp` is BANNED — the sandbox replaces it with an error pointing at
     `read-file` (line-numbered, size-capped, symlink-safe, tracked by
     <var_index>). Filesystem reads MUST go through `read-file`.
   * `re-find`, `re-seq`, `re-matches` accept a string pattern and auto-promote
     it to `java.util.regex.Pattern`, matching what LLMs naturally write.
   * `str/split` likewise accepts a string delimiter (already tested from the
     HITL angle — smoke-covered here for completeness).
   * `require`, `import`, `find-ns` are quietly allowed (natural Clojure
     reach for namespace discovery, intentionally undocumented so the LLM's
     canonical playbook stays narrow).
   * `spit`, `eval`, `load-string`, `load-file`, `read-string`, `intern`,
     `sh`, and stdin/stdout/stderr vars remain denied."
  (:require [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.vis.loop.runtime.core :as rt]
            [sci.core :as sci]))

;;; ── Fixture ─────────────────────────────────────────────────────────────

(defn- fresh-sandbox
  "Build an isolated SCI ctx whose `sandbox` ns exposes all the bindings a
   real conversation env would see. Returns `{:ev (fn [src]) :ctx sci-ctx}`."
  []
  (let [{:keys [sci-ctx]} (rt/create-sci-context (fn [& _]) nil nil nil)
        sandbox-ns (sci/find-ns sci-ctx 'sandbox)
        ev (fn [src]
             (try
               {:ok (:val (sci/eval-string+ sci-ctx src {:ns sandbox-ns}))}
               (catch Throwable t
                 {:err (or (ex-message t) (.getMessage t))})))]
    {:ev ev :ctx sci-ctx}))

;;; ── slurp ───────────────────────────────────────────────────────────────

(defdescribe slurp-banned-test
  (describe "slurp is BANNED in the sandbox"
    (it "returns an error and points the LLM at read-file"
      (let [{:keys [ev]} (fresh-sandbox)
            {:keys [err]} (ev "(slurp \"deps.edn\")")]
        (expect (some? err))
        (expect (re-find #"banned" err))
        (expect (re-find #"read-file" err))))))

;;; ── re-find / re-seq / re-matches ─────────────────────────────────────

(defdescribe regex-wrappers-test
  (describe "re-find accepts string patterns and Pattern literals"
    (it "string pattern returns the first match"
      (let [{:keys [ev]} (fresh-sandbox)]
        (expect (= "HITL" (:ok (ev "(re-find \"HITL\" \"has HITL in it\")"))))))

    (it "Pattern literal keeps working"
      (let [{:keys [ev]} (fresh-sandbox)]
        (expect (= "HITL" (:ok (ev "(re-find #\"HITL\" \"has HITL in it\")"))))))

    (it "capturing group still returns a vector"
      (let [{:keys [ev]} (fresh-sandbox)]
        (expect (= ["abc42" "42"]
                  (:ok (ev "(re-find \"abc(\\\\d+)\" \"see abc42 there\")")))))))

  (describe "re-seq accepts string patterns"
    (it "returns all matches"
      (let [{:keys [ev]} (fresh-sandbox)]
        (expect (= ["1" "22" "333"]
                  (:ok (ev "(re-seq \"\\\\d+\" \"a1 b22 c333\")")))))))

  (describe "re-matches accepts string patterns"
    (it "full-string match"
      (let [{:keys [ev]} (fresh-sandbox)]
        (expect (= "123" (:ok (ev "(re-matches \"\\\\d+\" \"123\")"))))))
    (it "partial match returns nil like clojure.core/re-matches"
      (let [{:keys [ev]} (fresh-sandbox)]
        (expect (nil? (:ok (ev "(re-matches \"\\\\d+\" \"abc123\")"))))))))

;;; ── str/split already covered in HITL — quick smoke here ─────────────

(defdescribe str-split-smoke-test
  (describe "clojure.string/split string delimiter promotion"
    (it "string delimiter behaves like a regex literal"
      (let [{:keys [ev]} (fresh-sandbox)]
        (expect (= ["a" "b" "c"]
                  (:ok (ev "(clojure.string/split \"a,b,c\" \",\")"))))
        (expect (= ["a" "b" "c"]
                  (:ok (ev "(clojure.string/split \"a,b,c\" #\",\")"))))
        (expect (= ["a" "b,c,d"]
                  (:ok (ev "(clojure.string/split \"a,b,c,d\" \",\" 2)"))))))))

;;; ── require / import / find-ns quietly allowed ──────────────────────

(defdescribe quietly-allowed-namespace-tools-test
  (describe "require, import, find-ns are allowed but undocumented"
    (it "require pulls in a namespace and lets code call its vars"
      (let [{:keys [ev]} (fresh-sandbox)]
        (expect (nil? (:err (ev "(require (quote clojure.set))"))))
        (expect (= #{1 2}
                  (:ok (ev "(clojure.set/union #{1} #{2})"))))))

    (it "import pulls in a Java class and lets code reach its methods"
      (let [{:keys [ev]} (fresh-sandbox)]
        (expect (nil? (:err (ev "(import java.time.LocalDate)"))))
        (expect (= "2025-01-02"
                  (:ok (ev "(str (LocalDate/parse \"2025-01-02\"))"))))))

    (it "find-ns returns a real namespace handle for clojure.core"
      (let [{:keys [ev]} (fresh-sandbox)
            {:keys [ok]} (ev "(str (find-ns (quote clojure.core)))")]
        (expect (= "clojure.core" ok))))))

;;; ── Still denied ────────────────────────────────────────────────────

(defdescribe sandbox-still-denied-test
  (describe "security-critical ops are still blocked"
    (it "spit is denied"
      (let [{:keys [ev]} (fresh-sandbox)
            {:keys [err]} (ev "(spit \"/tmp/vis-sandbox-blocked.txt\" \"x\")")]
        (expect (some? err))))

    (it "eval is denied"
      (let [{:keys [ev]} (fresh-sandbox)
            {:keys [err]} (ev "(eval (quote (+ 1 2)))")]
        (expect (some? err))
        (expect (re-find #"not allowed" err))))

    (it "load-string is denied"
      (let [{:keys [ev]} (fresh-sandbox)
            {:keys [err]} (ev "(load-string \"(+ 1 2)\")")]
        (expect (some? err))))

    (it "read-string is denied"
      (let [{:keys [ev]} (fresh-sandbox)
            {:keys [err]} (ev "(read-string \"(+ 1 2)\")")]
        (expect (some? err))))

    (it "intern is denied"
      (let [{:keys [ev]} (fresh-sandbox)
            {:keys [err]} (ev "(intern (quote user) (quote sneaky) 42)")]
        (expect (some? err))))

    (it "clojure.java.shell/sh is not exposed"
      (let [{:keys [ev]} (fresh-sandbox)
            {:keys [err]} (ev "(clojure.java.shell/sh \"ls\")")]
        (expect (some? err))))))
