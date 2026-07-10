(ns com.blockether.vis.ext.language-typescript-bun.repl-test
  "Managed Bun REPL: runner detection, subprocess lifecycle + persistent eval
   (TS types, top-level await, imports), and the language-facade wiring. The
   live-subprocess tests SKIP when no bun is on PATH so CI without bun stays
   green."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.language-typescript-bun.core :as core]
            [com.blockether.vis.ext.language-typescript-bun.repl-manager :as repl]
            [com.blockether.vis.ext.language-typescript-bun.runner :as runner]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-dir
  ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-bun-ext-" (into-array FileAttribute []))))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- has-bun? [] (runner/available?))

;; ── runner detection (no subprocess) ─────────────────────────────────────────
(defdescribe runner-test
             (it "resolves a launchable bun (PATH or ~/.bun) when available"
                 (when (has-bun?)
                   (let [cmd (runner/resolve-command (System/getProperty "java.io.tmpdir"))]
                     (expect (= 1 (count cmd)))
                     (expect (string? (first cmd)))))))

;; ── live REPL subprocess ─────────────────────────────────────────────────────
(defdescribe
  repl-lifecycle-test
  (it "starts, evaluates TS, persists globals across evals, captures output + errors, stops"
      (when (has-bun?)
        (let [dir (.getPath (tmp-dir))]
          (try (expect (= "up" (get (repl/start! dir {}) "status")))
               ;; last expression's value is captured (REPL semantics)
               (expect (= "2" (get (repl/eval! dir "1+1" 15000) "value")))
               ;; const/let PERSIST across separate evals — a real session
               (repl/eval! dir "const x: number = 21" 15000)
               (expect (= "42" (get (repl/eval! dir "x * 2" 15000) "value")))
               ;; top-level await just works
               (expect (= "99" (get (repl/eval! dir "await Promise.resolve(99)" 15000) "value")))
               ;; stdout is captured, not leaked into the protocol
               (let [r (repl/eval! dir "console.log('hi'); 5" 15000)]
                 (expect (= "hi\n" (get r "out")))
                 (expect (get r "ok")))
               ;; an exception is captured, not thrown into Clojure
               (let [r (repl/eval! dir "JSON.parse('nope')" 15000)]
                 (expect (false? (get r "ok")))
                 (expect (re-find #"SyntaxError" (str (get r "exc")))))
               (expect (= "up" (get (repl/status dir) "status")))
               (repl/stop! dir)
               (expect (= "down" (get (repl/status dir) "status")))
               (finally (repl/stop! dir))))))
  (it "eval before start fails closed with a clear error"
      (let [dir (str (System/getProperty "java.io.tmpdir") "/vis-bun-never-started")]
        (expect (= :ts/no-repl
                   (try (repl/eval! dir "1" 1000)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
  (it "imports a project module and reload() re-imports it cache-busted"
      (when (has-bun?)
        (let [root
              (tmp-dir)

              dir
              (.getPath root)

              mod
              (io/file root "answer.ts")]

          (try (spit mod "export const answer = (): number => 40 + 1;\n")
               (repl/start! dir {})
               (expect (= "41"
                          (get
                            (repl/eval! dir "import { answer } from './answer.ts'; answer()" 15000)
                            "value")))
               ;; edit the module on disk, reload() sees the new code
               (spit mod "export const answer = (): number => 40 + 2;\n")
               (expect
                 (= "42"
                    (get (repl/eval! dir "const m = await reload('./answer.ts'); m.answer()" 15000)
                         "value")))
               (finally (repl/stop! dir) (cleanup root)))))))

;; ── language-facade wiring ───────────────────────────────────────────────────
(defdescribe
  facade-test
  (it "repl_eval auto-starts a REPL for the dir and returns the value"
      (when (has-bun?)
        (let [root
              (tmp-dir)

              dir
              (.getCanonicalPath root)]

          (try (let [r (core/ts-repl-eval-fn {:workspace/root (.getPath root)} "3 * 7")]
                 (expect (:success? r))
                 (expect (= "21" (get-in r [:result "value"]))))
               (finally (repl/stop! dir))))))
  (it "repl_start status/stop lifecycle ops route through the manager"
      (when (has-bun?)
        (let [root
              (tmp-dir)

              dir
              (.getCanonicalPath root)

              env
              {:workspace/root (.getPath root)}]

          (try
            (expect (:success? (core/ts-start-repl-fn env "start" nil)))
            (expect (= "up" (get-in (core/ts-start-repl-fn env "status" nil) [:result "status"])))
            (core/ts-start-repl-fn env "stop" nil)
            (expect (= "down" (get-in (core/ts-start-repl-fn env "status" nil) [:result "status"])))
            (finally (repl/stop! dir))))))
  (it "run_tests shells out to `bun test` and parses the counts"
      (when (has-bun?)
        (let [root (tmp-dir)]
          (try (spit (io/file root "math.test.ts")
                     (str "import { expect, test } from 'bun:test';\n"
                          "test('adds', () => { expect(1 + 1).toBe(2); });\n"))
               (let [r (core/ts-test-fn {:workspace/root (.getPath root)} nil)]
                 (expect (:success? r))
                 (expect (= 0 (get-in r [:result "exit"])))
                 (expect (= 1 (get-in r [:result "passed"])))
                 (expect (contains? #{0 nil} (get-in r [:result "failed"]))))
               (finally (cleanup root)))))))

;; ── activation ───────────────────────────────────────────────────────────────
(def ^:private activation-fn @#'core/activation-fn)

(defdescribe activation-test
             (it "activates on a bunfig.toml workspace"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "bunfig.toml") "[install]\n")
                        (expect (true? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "activates on a bun.lock workspace"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "bun.lock") "{}\n")
                        (expect (true? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "activates on package.json + a .ts source"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "package.json") "{\"name\": \"x\"}\n")
                        (spit (io/file root "app.ts") "export {}\n")
                        (expect (true? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "stays dark on package.json WITHOUT TypeScript"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "package.json") "{\"name\": \"x\"}\n")
                        (spit (io/file root "index.js") "console.log(1)\n")
                        (expect (false? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "stays dark on a non-Bun workspace"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "README.md") "# nope\n")
                        (expect (false? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "stays dark with no :workspace/root" (expect (false? (activation-fn {})))))

(defdescribe
  value-representation-test
  "Real JS/TS values come back as JSON-safe STRUCTURED data, not just an
   inspect string; values that can't be serialized stay LIVE in the REPL and
   are described."
  (it "represents objects / arrays / Maps as nested data"
      (when (has-bun?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! dir {})
               (expect (= {"a" 1 "b" [2 3]}
                          (get (repl/eval! dir "({a: 1, b: [2, 3]})" 15000) "data")))
               (expect (= {"__type__" "Map" "entries" [["k" 1]]}
                          (get (repl/eval! dir "new Map([['k', 1]])" 15000) "data")))
               (expect (= "Object" (get (repl/eval! dir "({})" 15000) "type")))
               (finally (repl/stop! dir))))))
  (it "represents a class instance as a field map tagged with __type__"
      (when (has-bun?)
        (let [dir (.getPath (tmp-dir))]
          (try
            (repl/start! dir {})
            (repl/eval! dir "class P { constructor(public x: number, public y: number) {} }" 15000)
            (expect (= {"x" 3 "y" 4 "__type__" "P"}
                       (get (repl/eval! dir "new P(3, 4)" 15000) "data")))
            (finally (repl/stop! dir))))))
  (it "an OPAQUE value stays LIVE + is described (type/repr/attrs), not lost"
      (when (has-bun?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! dir {})
               (let [d (get (repl/eval! dir "Bun.serve" 15000) "data")]
                 (expect (get d "__opaque__"))
                 (expect (= "function" (get d "__type__")))
                 (expect (string? (get d "__repr__"))))
               ;; bind one, then keep using it across evals — globals persist
               (repl/eval! dir "function* gen() { yield 1; yield 4; } const g = gen()" 15000)
               (expect (= "1" (get (repl/eval! dir "g.next().value" 15000) "value")))
               (expect (= "4" (get (repl/eval! dir "g.next().value" 15000) "value")))
               (finally (repl/stop! dir)))))))
