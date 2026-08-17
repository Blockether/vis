(ns com.blockether.vis.ext.language-clojure.core-test
  "Activation-gate test for the language-clojure extension. Confirms
   the extension activates on Clojure workspaces and stays dark on
   plain ones."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.language-clojure.core :as core]
            [com.blockether.vis.internal.foundation.surface-contract :as contract]
            [com.blockether.vis.ext.language-clojure.format :as fmt]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [com.blockether.vis.ext.language-clojure.test-runner :as test-runner]
            [com.blockether.vis.ext.language-clojure.paren-repair :as repair]
            [com.blockether.vis.internal.foundation.editing.balance :as balance]
            [com.blockether.vis.internal.foundation.editing.zipper :as zipper]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir
  ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-clj-ext-act-" (into-array FileAttribute []))))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- activation-fn
  []
  ;; private — reach into ns directly so the manifest stays the
  ;; public contract.
  @#'core/activation-fn)

(defdescribe
  activation-test
  (it "activates when deps.edn is at the workspace root"
      (let [root (tmp-dir)]
        (try (spit (io/file root "deps.edn") "{:paths [\"src\"]}")
             (expect (true? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
             (finally (cleanup root)))))
  (it "activates when .clj sources exist without any manifest"
      (let [root (tmp-dir)]
        (try (let [src (io/file root "src" "x.clj")]
               (.mkdirs (.getParentFile src))
               (spit src "(ns x)"))
             (expect (true? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
             (finally (cleanup root)))))
  (it "stays dark on a non-Clojure workspace"
      (let [root (tmp-dir)]
        (try (spit (io/file root "README.md") "# nope\n")
             (let [f (io/file root "src" "x.py")]
               (.mkdirs (.getParentFile f))
               (spit f "print('hi')"))
             (expect (false? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
             (finally (cleanup root)))))
  (it "stays dark when :workspace/root is missing" (expect (false? ((activation-fn) {})))))

(defn- classpath-manifests
  "Return every parsed `META-INF/vis-extension/vis.edn` on the classpath.
   `io/resource` only yields the FIRST match (whichever jar loads first),
   but the scanner walks `getResources` — mirror that here so the test
   sees this extension's manifest even when another extension is also
   on the test classpath."
  []
  (let
    [cl
     (.getContextClassLoader (Thread/currentThread))

     urls
     (enumeration-seq (.getResources cl "META-INF/vis-extension/vis.edn"))]

    (mapv (fn [u]
            (read-string (slurp u)))
          urls)))

(defdescribe manifest-discovery-test
             ;; Regression: the extension was invisible because
             ;; `resources/META-INF/vis-extension/vis.edn` did not exist. With no
             ;; manifest the classpath scanner skips the namespace, the ns is
             ;; never `require`d, `(vis/register-extension! …)` never runs, and
             ;; `clj/` shows up nowhere — see conversation
             ;; 11d4f817-fbd1-43ab-a6b4-052c8557af0a issue #3
             ;; (\"Dlaczego CLOJURE extension nie jest widoczny?!\"). The manifest
             ;; is the public discovery contract; keep it pinned by a test so
             ;; nobody silently deletes it again.
             (it "ships a vis-extension manifest with the language-clojure id on the classpath"
                 (let
                   [manifests
                    (classpath-manifests)

                    merged
                    (reduce merge {} manifests)]

                   (expect (seq manifests))
                   (expect (contains? merged 'language-clojure))))
             (it "manifest registers the core namespace under the language-clojure id"
                 (let
                   [manifests
                    (classpath-manifests)

                    merged
                    (reduce merge {} manifests)]

                   (expect (some #{'com.blockether.vis.ext.language-clojure.core}
                                 (get-in merged ['language-clojure :nses]))))))

(defdescribe surface-test
             (it "exposes NO engine verbs — repair+format ride the facade, no clj/ alias"
                 ;; clj_paren_repair / the `clj/` engine are gone: paren repair now rides inside
                 ;; `format` AND the language-tools `:balance-fn` the editors call. The manifest
                 ;; declares no :ext/engine; the constructor scaffolds an EMPTY one (no alias,
                 ;; no symbols).
                 (let [engine (:ext/engine core/vis-extension)]
                   (expect (nil? (:ext.engine/alias engine)))
                   (expect (empty? (:ext.engine/symbols engine)))))
             (it "hands the editors a `:balance-fn` instead of wrapping their ops"
                 ;; The pack no longer intercepts `patch`/`struct_patch` to repair the FRAGMENT a
                 ;; caller passed — that turned an informative refusal into a silently corrupt
                 ;; write. It publishes the repair as data on its language tools and the
                 ;; foundation decides, per edit, whether the repaired FILE is safe to keep.
                 (let
                   [tools
                    (:ext/language-tools core/vis-extension)

                    clj-tools
                    (first (filter #(= "clojure" (:language %)) tools))]

                   (expect (nil? (:ext/op-hooks core/vis-extension)))
                   (expect (some? clj-tools))
                   (expect (= repair/fix-delimiters (:balance-fn clj-tools)))
                   ;; the function is REAL, not a placeholder the foundation would call into a hole
                   (expect (= "(defn f [])" ((:balance-fn clj-tools) "(defn f ["))))))

(defdescribe repl-resource-logs-test
             (it "registers managed nREPL resources with tail-able launcher logs"
                 (let
                   [dir
                    (tmp-dir)

                    sid
                    (str "test-nrepl-logs-" (System/nanoTime))

                    rid
                    (repl-manager/id-of (.getAbsolutePath dir))

                    log
                    (io/file dir "nrepl.log")]

                   (try (spit log "booting\nready\n")
                        (core/register-repl-resource! sid
                                                      (.getAbsolutePath dir)
                                                      ["dev"]
                                                      {"result" "started"
                                                       "id" rid
                                                       "cwd" (.getAbsolutePath dir)
                                                       "status" "up"
                                                       "port" 5555
                                                       "pid" 12345
                                                       "aliases" ["dev"]
                                                       "log" (.getAbsolutePath log)})
                        (let [r (vis/get-resource sid rid)]
                          (expect (= true (get r "can_logs")))
                          (expect (= (.getAbsolutePath log) (get-in r ["detail" "log"])))
                          (expect (= ["booting" "ready"] (vis/resource-logs sid rid))))
                        (finally (vis/unregister-resource! sid rid) (cleanup dir))))))

(defdescribe combined-format-test
             (it "format does BOTH parinfer delimiter repair AND cljfmt"
                 (let
                   [src
                    "(defn f [x]\n  (+ x 1)"

                    ; missing close paren
                    r
                    (core/clj-format-fn src)

                    out
                    (core/clj-repair+format src)]

                   (expect (:success? r))
                   (expect (true? (get-in r [:result "repaired"]))) ; a ) was added
                   ;; format_code returns NO formatted text — only changed? + a char-delta ack
                   (expect (true? (get-in r [:result "changed"])))
                   (expect (number? (get-in r [:result "chars"])))
                   (expect (not (contains? (:result r) "text")))
                   ;; the result NAMES the backend that ran (zprint | cljfmt)
                   (expect (contains? #{"zprint" "cljfmt"} (get-in r [:result "formatter"])))
                   ;; the repaired output is stable: re-running the formatter is a no-op
                   (expect (= out (core/clj-repair+format out))))))

;; Regression: `format_code` ran parinfer with NO direction rule, so a file that lost an
;; opening `(` — character for character a file with one `)` too many — was "repaired" by
;; DELETING that `)`, rewritten on disk, and reported as `"repaired": true`. `(def defaults
;; {..})` became three loose top-level forms and nothing in the result named a line.
(defdescribe format-repair-is-add-only-test
             (it "adds a closer the file omitted and NAMES the line it completed"
                 (let [dir (tmp-dir)]
                   (try (let [f (io/file dir "add.clj")]
                          (spit f "(defn f [x]\n  (inc x)\n")
                          (let
                            [result (:result (core/clj-format-fn {:workspace/root (str dir)}
                                                                 {"path" "add.clj"}))]
                            (expect (true? (get result "repaired")))
                            (expect (= ["line 2 added `)` → `(inc x))`"] (get result "repairs")))
                            (expect (nil? (get result "unbalanced")))
                            (expect (= "(defn f [x]\n  (inc x))\n" (slurp f)))))
                        (finally (cleanup dir)))))
             (it "refuses a repair that would DELETE a delimiter, leaving the file as written"
                 (let [dir (tmp-dir)]
                   (try
                     (let
                       [f (io/file dir "lost_opener.clj")
                        src "(ns demo.core)\n\ndef defaults\n  {:retries 3\n   :timeout 500})\n"]

                       (spit f src)
                       (let
                         [result (:result (core/clj-format-fn {:workspace/root (str dir)}
                                                              {"paths" [(str f)]}))
                          file-result (first (get result "files"))]

                         (expect (false? (get file-result "repaired")))
                         (expect (false? (get file-result "wrote")))
                         (expect (str/includes? (get file-result "unbalanced")
                                                "would delete `)` this file has"))
                         ;; the whole point: on disk, character for character what was written
                         (expect (= src (slurp f)))))
                     (finally (cleanup dir))))))

(defdescribe multi-file-format-test
             (it "formats every file in {\"paths\": [...]} IN PLACE and rolls up per-file changes"
                 (let [dir (tmp-dir)]
                   (try (let
                          [f1 (io/file dir "a.clj")
                           f2 (io/file dir "b.clj")]

                          (spit f1 "(defn f [x]\n(* x 2))\n") ; mis-indented -> changes
                          (spit f2 "(defn g [y] (+ y 1))\n")  ; already tidy -> no change
                          (let
                            [r (core/clj-format-fn {:workspace/root (str dir)}
                                                   {"paths" [(str f1) (str f2)]})
                             files (get-in r [:result "files"])]

                            (expect (:success? r))
                            (expect (= "clj-format" (get-in r [:result "op"])))
                            (expect (= 1 (get-in r [:result "changed"]))) ; only f1 changed
                            (expect (= 2 (count files)))
                            ;; per-file result carries changed/wrote flags
                            (expect (= [true false] (mapv #(get % "changed") files)))
                            (expect (= [true false] (mapv #(get % "wrote") files)))
                            ;; the mis-indented file was actually rewritten on disk
                            (expect (= "(defn f [x]\n  (* x 2))\n" (slurp f1)))
                            (expect (= "(defn g [y] (+ y 1))\n" (slurp f2)))))
                        (finally (cleanup dir))))))

(defdescribe
  single-relative-path-format-test
  (it "resolves a RELATIVE {\"path\"} against the workspace root, not the process CWD"
      (let [dir (tmp-dir)]
        (try (let [sub (io/file dir "sub")]
               (.mkdirs sub)
               (spit (io/file sub "probe.clj") "(defn f [x]\n(* x 2))\n") ; mis-indented -> changes
               ;; the relative path exists ONLY under the workspace root, never under CWD
               (expect (not (.exists (io/file (System/getProperty "user.dir") "sub/probe.clj"))))
               (let [r (core/clj-format-fn {:workspace/root (str dir)} {"path" "sub/probe.clj"})]
                 (expect (:success? r))
                 (expect (= "clj-format" (get-in r [:result "op"])))
                 (expect (true? (get-in r [:result "changed"])))
                 ;; reported path is workspace-relative, and the file on disk was rewritten
                 (expect (= "sub/probe.clj" (get-in r [:result "path"])))
                 (expect (= "(defn f [x]\n  (* x 2))\n" (slurp (io/file sub "probe.clj"))))))
             (finally (cleanup dir))))))

(defdescribe
  relativize-path-home-test
  (it
    "homogenizes a leading user-home to ~ for paths outside root (and the root itself), never a raw /Users/…"
    (let
      [rp
       #'com.blockether.vis.ext.language-clojure.core/relativize-path

       home
       (System/getProperty "user.home")

       root
       (io/file (str home "/vis"))]

      ;; under root -> workspace-relative
      (expect (= "src/foo.clj" (rp root (str home "/vis/src/foo.clj"))))
      ;; outside root but under home -> ~ prefix, not a machine-absolute path
      (expect (= "~/other/foo.clj" (rp root (str home "/other/foo.clj"))))
      ;; the root itself relativizes to "" -> home-homogenized absolute, not blank
      (expect (= "~/vis" (rp root (str home "/vis"))))
      ;; sentinels pass through untouched
      (expect (= "<stdin>" (rp root "<stdin>"))))))


(defdescribe
  single-relative-path-lint-test
  (it "resolves a RELATIVE {\"path\"} against the workspace root, not the process CWD"
      (let [dir (tmp-dir)]
        (try (let [sub (io/file dir "sub")]
               (.mkdirs sub)
               ;; unused binding x -> a clj-kondo warning
               (spit (io/file sub "probe.clj") "(ns sub.probe)\n(defn foo [] (let [x 1] 42))\n")
               ;; the relative path exists ONLY under the workspace root, never under CWD
               (expect (not (.exists (io/file (System/getProperty "user.dir") "sub/probe.clj"))))
               (let
                 [r (core/clj-lint-fn {:workspace/root (str dir)} {"path" "sub/probe.clj"})
                  findings (get-in r [:result "findings"])]

                 (expect (:success? r))
                 ;; the file under root was actually linted (not silently skipped)
                 (expect (= 1 (count findings)))
                 ;; reported file path is workspace-relative
                 (expect (= "sub/probe.clj" (get (first findings) "file")))
                 (expect (= "unused binding x" (get (first findings) "message")))))
             (finally (cleanup dir))))))

(defdescribe
  blank-code-default-does-not-shadow-path-test
  (it
    "a blank `code` default (models emit EVERY key) still lints the given path/paths, not an empty snippet"
    (let [dir (tmp-dir)]
      (try (let [sub (io/file dir "sub")]
             (.mkdirs sub)
             ;; unused binding x -> a clj-kondo warning
             (spit (io/file sub "probe.clj") "(ns sub.probe)\n(defn foo [] (let [x 1] 42))\n")
             ;; the model shape: {"code" ""} alongside a real {"path"} (and empty {"paths"})
             (let
               [r (core/clj-lint-fn {:workspace/root (str dir)}
                                    {"code" "" "path" "sub/probe.clj" "paths" []})
                findings (get-in r [:result "findings"])]

               (expect (:success? r))
               ;; the file was actually linted, NOT skipped as a blank snippet
               (expect (= ["sub/probe.clj"] (get-in r [:result "targets"])))
               (expect (= 1 (count findings)))
               (expect (= "unused binding x" (get (first findings) "message"))))
             ;; format sees the same shape: a blank `code` must format the FILE, not ""
             (spit (io/file sub "fmt.clj") "(defn f [x]\n(* x 2))\n")
             (let
               [r (core/clj-format-fn {:workspace/root (str dir)}
                                      {"code" "" "path" "sub/fmt.clj" "paths" []})]
               (expect (:success? r))
               (expect (= "sub/fmt.clj" (get-in r [:result "path"])))
               (expect (true? (get-in r [:result "changed"])))
               (expect (= "(defn f [x]\n  (* x 2))\n" (slurp (io/file sub "fmt.clj"))))
               (expect (= "(defn f [x]\n  (* x 2))\n" (slurp (io/file sub "fmt.clj"))))))
           (finally (cleanup dir))))))

(defdescribe
  lint-nonexistent-target-errors-test
  (it
    "a named path that resolves to nothing is an actionable ERROR, not a false `clean` (models spun on this)"
    (let [dir (tmp-dir)]
      (try
        ;; a junk `path` beside a REAL `paths` (the exact model shape that spun):
        ;; the old code let `path` shadow `paths` AND a missing path linted 0 files,
        ;; so it falsely reported `clean` with nothing to correct against.
        (let [sub (io/file dir "sub")]
          (.mkdirs sub)
          (spit (io/file sub "probe.clj") "(ns sub.probe)\n(defn foo [] (let [x 1] 42))\n")
          (let
            [r (core/clj-lint-fn {:workspace/root (str dir)}
                                 {"code" "" "path" "/dev/null???" "paths" ["sub"]})]
            ;; a non-existent target now FAILS with a clear, actionable message
            (expect (not (:success? r)))
            (expect (re-find #"lint target does not exist: /dev/null\?\?\?"
                             (str (get-in r [:error :message]))))
            (expect (some? (get-in r [:error :hint])))))
        (finally (cleanup dir))))))

(defdescribe lint-path-and-paths-union-test
             (it "`path` and `paths` are UNIONED, not shadowing — both are linted"
                 (let [dir (tmp-dir)]
                   (try
                     (let [sub (io/file dir "sub")]
                       (.mkdirs sub)
                       (spit (io/file sub "a.clj") "(ns sub.a)\n(defn foo [] (let [x 1] 42))\n")
                       (spit (io/file sub "b.clj") "(ns sub.b)\n(defn bar [] (let [y 2] 7))\n")
                       (let
                         [r (core/clj-lint-fn {:workspace/root (str dir)}
                                              {"path" "sub/a.clj" "paths" ["sub/b.clj"]})
                          files (into #{} (map #(get % "file") (get-in r [:result "findings"])))]

                         (expect (:success? r))
                         ;; both files were actually linted (neither silently dropped)
                         (expect (= #{"sub/a.clj" "sub/b.clj"} files))
                         (expect (= ["sub/a.clj" "sub/b.clj"] (get-in r [:result "targets"])))))
                     (finally (cleanup dir))))))

(defdescribe
  recursive-format-test
  (it
    "formats a DIRECTORY in {\"paths\"} RECURSIVELY, skipping non-Clojure files"
    (let [dir (tmp-dir)]
      (try
        (let [sub (io/file dir "sub")]
          (.mkdirs sub)
          (spit (io/file dir "a.clj") "(defn f [x]\n(* x 2))\n") ; mis-indented -> changes
          (spit (io/file sub "b.cljc") "(defn g [y]\n(+ y 1))\n") ; nested -> changes
          (spit (io/file sub "c.clj") "(defn h [z] (dec z))\n")   ; tidy -> no change
          (spit (io/file dir "notes.txt") "not clojure\n") ; must be ignored
          (let
            [r (core/clj-format-fn {:workspace/root (str dir)} {"paths" [(str dir)]})
             files (get-in r [:result "files"])]

            (expect (:success? r))
            ;; only the 3 Clojure sources, walked recursively; the .txt is skipped
            (expect (= 3 (count files)))
            (expect (= ["a.clj" "sub/b.cljc" "sub/c.clj"] (sort (mapv #(get % "path") files))))
            (expect (= 2 (get-in r [:result "changed"]))) ; a + b changed, c tidy
            ;; findings/files ALSO grouped under the directory (prefix written once)
            ;; and the whole result conforms to the language-surface contract
            (let [by-cwd (get-in r [:result "by-cwd"])]
              (expect (= #{"." "sub"} (set (keys by-cwd))))
              (expect (= #{"a.clj"} (set (keys (get by-cwd ".")))))
              (expect (= #{"b.cljc" "c.clj"} (set (keys (get by-cwd "sub")))))
              (expect (true? (get-in by-cwd ["." "a.clj" "changed"])))
              (expect (false? (get-in by-cwd ["sub" "c.clj" "changed"]))))
            (expect (contract/valid? :format-fn (:result r)))
            (expect (= "(defn f [x]\n  (* x 2))\n" (slurp (io/file dir "a.clj"))))
            (expect (= "(defn g [y]\n  (+ y 1))\n" (slurp (io/file sub "b.cljc"))))
            (expect (= "not clojure\n" (slurp (io/file dir "notes.txt"))))))
        (finally (cleanup dir))))))

(defdescribe default-project-format-test
             (it
               "with no arg / {} formats the workspace's src + test RECURSIVELY, ignoring the rest"
               (let [dir (tmp-dir)]
                 (try (let
                        [src (io/file dir "src")
                         tst (io/file dir "test")]

                        (.mkdirs src)
                        (.mkdirs tst)
                        (spit (io/file src "a.clj") "(defn f [x]\n(* x 2))\n")
                        (spit (io/file tst "a_test.clj") "(defn t [] 1)\n")
                        (spit (io/file dir "ignored.clj") "(def top 1)\n") ; not under src/test
                        (let
                          [empty-map (core/clj-format-fn {:workspace/root (str dir)} {})
                           nil-arg (core/clj-format-fn {:workspace/root (str dir)} nil)
                           paths-of #(sort (mapv (fn [x]
                                                   (get x "path"))
                                                 (get-in % [:result "files"])))]

                          (expect (:success? empty-map))
                          (expect (= ["src/a.clj" "test/a_test.clj"] (paths-of empty-map)))
                          ;; nil arg behaves the same as {}
                          (expect (= ["src/a.clj" "test/a_test.clj"] (paths-of nil-arg)))))
                      (finally (cleanup dir))))))

(defdescribe
  cljfmt-config-test
  (it "honors a project-local .cljfmt.edn (walked up from the file) over cljfmt defaults"
      ;; The churn bug: the hook must READ the nearest .cljfmt.edn, not reformat
      ;; with cljfmt DEFAULTS — a lazytest `it` body indents differently under the
      ;; project's `[[:inner 0]]` override than under stock cljfmt.
      (let [dir (tmp-dir)]
        (try (spit (io/file dir ".cljfmt.edn") "{:extra-indents {myblock [[:inner 0]]}}")
             (let
               [messy "(myblock a\nb\nc)"
                with-cfg (core/clj-repair+format messy (.getPath dir))
                default (core/clj-repair+format messy nil)]

               ;; config-driven indentation differs from stock defaults ...
               (expect (not= with-cfg default))
               ;; ... and equals formatting with the discovered opts
               (expect (= with-cfg (fmt/format-string messy (fmt/cljfmt-opts-for (.getPath dir))))))
             (finally (cleanup dir)))))
  (it "returns nil opts when no config file is found"
      (let [dir (tmp-dir)]
        (try (expect (nil? (fmt/cljfmt-opts-for (.getPath dir)))) (finally (cleanup dir))))))


(defdescribe test-runner-timeout-test
             ;; The pack holds no budget of its own: a second literal beside
             ;; `RUN_TESTS_TIMEOUT_MS` is exactly the drift that knob prevents.
             (it "runs a suite on the shared ten-minute run_tests budget"
                 (expect (= (* 10 60 1000) rt/RUN_TESTS_TIMEOUT_MS))
                 (expect (< rt/RUN_TESTS_TIMEOUT_MS rt/MAX_EVAL_TIMEOUT_MS))))

(defn- with-example-project
  "Run `f` with the PATH of a throwaway workspace holding one test namespace
   (`example.core-test`). PATHS are the only run_tests selector, so every
   fallback case needs a real test file on disk to resolve one from."
  [f]
  (let [root (tmp-dir)]
    (try (.mkdirs (io/file root "test"))
         (spit (io/file root "test" "example_core_test.clj") "(ns example.core-test)\n")
         (f (.getPath root))
         (finally (cleanup root)))))

(defdescribe test-runner-fallback-test
             (it "falls back to the project test CLI when the live nREPL lacks lazytest"
                 (let
                   [called
                    (atom false)

                    result
                    (with-example-project
                      (fn [root]
                        (with-redefs-fn {#'repl-manager/live-repl-for-dir (constantly {:port 54321})
                                         #'test-runner/run-via-repl
                                         (fn [& _]
                                           {"error" "Could not locate lazytest/core"})
                                         #'test-runner/run-via-cli
                                         (fn [_root norm]
                                           (reset! called true)
                                           {"mode" "cli" "ns" (first (:nses norm)) "is_pass" true})}
                          #(test-runner/clj-test-fn {:workspace/root root} {"paths" ["test"]}))))]

                   (expect @called)
                   (expect (= "cli" (get-in result [:result "mode"])))
                   (expect (= "clojure" (get-in result [:result "language"]))))))

(defdescribe
  test-runner-repl-gate-test
  ;; run_tests must never SPAWN: it reuses the REPL this session already
  ;; keeps for the project, and with none it runs the suite in a clean JVM
  ;; through the build tool's own test command.
  (it "runs via the CLI suite when the session has no REPL for the project"
      (let
        [called
         (atom false)

         result
         (with-example-project
           (fn [root]
             (with-redefs-fn {#'repl-manager/live-repl-for-dir (constantly nil)
                              #'repl-manager/start!
                              (fn [& _]
                                (throw (ex-info "run_tests must never start a REPL" {})))
                              #'test-runner/run-via-cli
                              (fn [_root norm]
                                (reset! called true)
                                {"mode" "cli" "ns" (first (:nses norm)) "is_pass" true})}
               #(test-runner/clj-test-fn {:workspace/root root} {"paths" ["test"]}))))]

        (expect @called)
        (expect (= "cli" (get-in result [:result "mode"])))))
  (it "reuses a REPL the session already has, without shelling the CLI"
      (let
        [seen-port
         (atom nil)

         result
         (with-example-project
           (fn [root]
             (with-redefs-fn {#'repl-manager/live-repl-for-dir (constantly {:port 4321})
                              #'test-runner/run-via-repl
                              (fn [_root nses _sel port]
                                (reset! seen-port port)
                                {"mode" "repl" "ns" (first nses) "is_pass" true})
                              #'test-runner/run-via-cli
                              (fn [& _]
                                (throw (ex-info "must not shell the CLI while a REPL is up" {})))}
               #(test-runner/clj-test-fn {:workspace/root root} {"paths" ["test"]}))))]

        (expect (= 4321 @seen-port))
        (expect (= "repl" (get-in result [:result "mode"]))))))

(defdescribe
  test-runner-nested-root-test
  (it "boots the nREPL at the tests' own nested project root (its deps.edn), not the workspace root"
      (let [root (tmp-dir)]
        (try (let
               [svc (io/file root "services" "svc")
                test-dir (io/file svc "test")]

               (.mkdirs test-dir)
               ;; nested project: deps.edn lives at services/svc, NOT the workspace root
               (spit (io/file svc "deps.edn") "{:paths [\"src\" \"test\"]}")
               (spit (io/file test-dir "svc_test.clj") "(ns svc-test)")
               (let [seen (atom nil)]
                 (with-redefs-fn {#'repl-manager/live-repl-for-dir (fn [_sid dir]
                                                                     (reset! seen dir)
                                                                     nil)
                                  #'test-runner/run-via-cli
                                  (fn [_root norm]
                                    {"mode" "cli" "ns" (first (:nses norm)) "is_pass" true})}
                   #(test-runner/clj-test-fn {:workspace/root (.getAbsolutePath root)}
                                             {"paths" ["services/svc/test"]}))
                 ;; the REPL is looked up at services/svc, where deps.edn lives
                 (expect (= (.getCanonicalPath svc) (.getCanonicalPath (io/file @seen))))))
             (finally (cleanup root))))))

(defn- clj-balancer
  "The delimiter repair the pack publishes for Clojure on its language tools — the
   exact function the foundation's editors look up."
  []
  (:balance-fn (first (filter #(= "clojure" (:language %))
                              (:ext/language-tools core/vis-extension)))))

(def ^:private shapes-corpus
  "The shapes a model actually edits, in one file: `ns` with `:require`/`:import`, a
   `def` map, a threading `defn`, a `let` over interop and `println` inside `if`/`do`,
   a `loop`/`recur` and a comment tail. Every case below drops closers off ITS lines."
  (str/join
    "\n"
    ["(ns app.scan" "  (:require [clojure.string :as str])" "  (:import (java.io File)))" ""
     "(def ^:private limit 42)" "" "(def defaults" "  {:retries 3" "   :timeout-ms 500})" ""
     "(defn- normalize [s]" "  (-> s str/trim str/lower-case))" "" "(defn tally [xs]" "  (->> xs"
     "       (map normalize)" "       (into (sorted-map))))" "" "(defn scan [^File dir]"
     "  (let [names (mapv (fn [^File f] (.getName f)) (.listFiles dir))]" "    (if (seq names)"
     "      (do (println \"scanned\" (count names))" "          {:names names})"
     "      {:names []})))" "" "(defn crawl [root]" "  (loop [queue [root] acc []]"
     "    (if-let [d (first queue)]" "      (recur (rest queue) (conj acc (:name d))) ; keep going"
     "      acc)))" "" "(defn describe [x]" "  (cond" "    (map? x) (str \"map of \" (count x))"
     "    (vector? x) (str \"vector of \" (count x))" "    :else (str x)))" ""]))

(defn- code-part
  "`line` up to a trailing comment, right-trimmed, and the comment itself."
  [^String line]
  (let [i (str/index-of line ";")]
    [(str/trimr (if i (subs line 0 i) line)) (if i (str " " (str/triml (subs line i))) "")]))

(defn- drop-closers
  "`line` with the last `n` closers of its code part gone — the mistake this whole
   decision exists for. nil when the line has fewer than `n` to drop."
  [^String line n]
  (let
    [[code tail]
     (code-part line)

     kept
     (loop
       [s
        code

        k
        0]

       (if (and (< k (long n)) (seq s) (#{\) \] \}} (last s)))
         (recur (subs s 0 (dec (count s))) (inc k))
         (when (= k (long n)) s)))]

    (when kept (str kept tail))))
(defn- inside-drops
  "Every way to drop ONE delimiter from `line` that is not one of its trailing closers:
   a closer omitted in the MIDDLE, which the caller's indentation cannot place — it
   comes back at the line's END and regroups the arguments between."
  [^String line]
  (let
    [[code tail]
     (code-part line)

     trailing
     (count (take-while #{\) \] \}} (reverse code)))

     body
     (subs code 0 (- (count code) trailing))]

    (for
      [i
       (range (count body))

       :when (#{\( \) \[ \] \{ \}} (nth body i))]

      (str (subs code 0 i) (subs code (inc (long i))) tail))))

(defn- drop-opener
  "`line` without its first opening delimiter — an opener the model lost, which leaves
   character for character what one closer too many leaves."
  [^String line]
  (let
    [[code tail]
     (code-part line)

     i
     (first (keep-indexed (fn [i c]
                            (when (#{\( \[ \{} c) i))
                          code))]

    (when i (str (subs code 0 (long i)) (subs code (inc (long i))) tail))))

(defn- retypes
  "Every way to RETYPE one delimiter of `line` as a different one: the code stays
   identical, so only the text the line replaced can say the delimiter is not the one
   the caller meant."
  [^String line]
  (let [[code tail] (code-part line)]
    (for
      [i (range (count code))
       :when (#{\( \) \[ \] \{ \}} (nth code i))
       c (disj #{\( \) \[ \] \{ \}} (nth code i))]

      (str (subs code 0 i) c (subs code (inc (long i))) tail))))

(defn- rename-token
  "`line` with its last code token renamed — an edit that REWROTE the line as well as
   breaking it, so its skeleton no longer matches the line it replaced. nil when the
   line ends in no closer to rename in front of."
  [^String line]
  (let
    [[code tail]
     (code-part line)

     trailing
     (count (take-while #{\) \] \}} (reverse code)))

     cut
     (- (count code) trailing)]

    (when (pos? trailing) (str (subs code 0 cut) "x" (subs code cut) tail))))

(defdescribe
  balance-fn-boundary-test
  "The pack's repair reaching the foundation's editors: the SAME `:balance-fn` the
   manifest publishes, run through the foundation's decision, which keeps a repaired
   file only when the repair stayed inside the lines the edit wrote."
  ;; Regression, session 621ba390: a Clojure edit whose replacement was an unbalanced FRAGMENT
  ;; parinfer-repaired ON ITS OWN and retried, so a partial form silently closed itself
  ;; and overwrote a good line; the caller was told only "(delimiters repaired)".
  (it
    "repairs a dropped closer on the line the edit wrote, and names it"
    (let
      [source
       "(ns ok)\n\n(defn ok [] 1)\n\n(defn two [] 2)\n"

       result
       (zipper/edit "clojure" source [1] :replace "(defn ok [] (inc 1)" {:balancer (clj-balancer)})]

      (expect (true? (:ok? result)))
      (expect (= "(ns ok)\n\n(defn ok [] (inc 1))\n\n(defn two [] 2)\n" (:new-source result)))
      ;; the repair is NAMED with the character and the line, never a silent footnote
      (expect (= ["line 3 added `)` → `(defn ok [] (inc 1))`"] (:repairs result)))))
  (it "refuses the reported case: the repair balances a line the edit never wrote"
      (let
        [;; the file from the report — the caller's anchor had drifted onto the binding
         ;; VALUE line, and the replacement was the destructuring fragment
         source
         "(defn f []\n  (let\n    [{:keys [a b]}\n     form\n\n     x\n     1]\n\n    x))\n"

         broken
         (str/replace source "     form\n" "    [{:keys [a b c]}\n")

         verdict
         (balance/rebalance {:balancer (clj-balancer)
                             :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
                             :source broken
                             :spans [[4 4]]})]

        ;; repairing the FRAGMENT alone is what used to be written — a complete, wrong form
        (expect (= "    [{:keys [a b c]}]" ((clj-balancer) "    [{:keys [a b c]}")))
        ;; the whole-file repair instead closes line 3, which this edit never touched
        (expect (false? (:ok? verdict)))
        (expect (str/includes? (:why verdict) "line 3"))))
  ;; A replacement that dropped a QUOTE is not a missing bracket: parinfer has no repair to
  ;; offer, and "no delimiter repair was found" sent the caller looking for a paren.
  (it "names the unterminated string the pack's repair cannot close"
      (let
        [broken
         "(ns ok)\n\n(defn ok [] \"1)\n"

         verdict
         (balance/rebalance {:balancer (clj-balancer)
                             :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
                             :source broken
                             :spans [[3 3]]})]

        (expect (false? (:ok? verdict)))
        (expect (str/includes? (:why verdict) "line 3 opens a string that is never closed"))))
  ;; Regression: `(defn ok [] (inc 1))` typed with an opening paren lost carries a surplus
  ;; closer, and parinfer's answer to that is the file with the closer DELETED — which
  ;; parses, reads as loose symbols, and is character-for-character the same repair as the
  ;; honest `)` too many. Only the DIRECTION of the change can refuse it.
  (it "refuses the pack's own repair when it deletes a closer the caller wrote"
      (let
        [broken
         "(ns ok)\n\n(defn ok [] inc 1))\n"

         verdict
         (balance/rebalance {:balancer (clj-balancer)
                             :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
                             :source broken
                             :spans [[3 3]]})]

        ;; what parinfer alone would have written
        (expect (= "(ns ok)\n\n(defn ok [] inc 1)\n" ((clj-balancer) broken)))
        (expect (false? (:ok? verdict)))
        (expect (str/includes? (:why verdict) "would delete `)` this edit wrote"))))
  ;; One, two and three closers off the END of every line that has them, over every shape
  ;; above: this is the mistake the repair exists for, and each one has to come back
  ;; byte-identical — a repair that lands anywhere else would be a silent rewrite.
  (it
    "restores every closer a model drops off the end of a line, in every shape"
    (let
      [lines
       (str/split-lines shapes-corpus)

       mutate
       (fn [ln text]
         (str/join "\n" (concat (take (dec (long ln)) lines) [text] (drop (long ln) lines) [""])))

       verdicts
       (for
         [ln
          (range 1 (inc (count lines)))

          n
          [1 2 3]

          :let [mut
                (drop-closers (nth lines (dec ln)) n)]
          :when mut]

         [ln n
          (balance/rebalance {:balancer (clj-balancer)
                              :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
                              :source (mutate ln mut)
                              :spans [[ln ln]]})])]

      ;; the matrix is worth nothing if it silently stopped covering the file
      (expect (<= 45 (count verdicts)))
      (expect (= []
                 (vec (for
                        [[ln n v]
                         verdicts

                         :when (not= shapes-corpus (:content v))]

                        [ln n (or (:why v) :wrong-content)]))))))
  ;; The same lines with one closer TOO MANY. Parinfer answers both that and a lost
  ;; opener by DELETING the surplus, so accepting it would write `(def x 1)` typed as
  ;; `def x 1)` as three loose top-level forms that parse. Never accepted, in any shape.
  (it "never accepts a repair that deletes a closer, in any shape"
      (let
        [lines
         (str/split-lines shapes-corpus)

         mutate
         (fn [ln text]
           (str/join "\n" (concat (take (dec (long ln)) lines) [text] (drop (long ln) lines) [""])))

         accepted
         (for
           [ln
            (range 1 (inc (count lines)))

            :let [line
                  (nth lines (dec ln))]
            :when (and (seq (str/trim line)) (nil? (str/index-of line ";")))
            :let [v
                  (balance/rebalance {:balancer (clj-balancer)
                                      :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
                                      :source (mutate ln (str (str/trimr line) ")"))
                                      :spans [[ln ln]]})]
            :when (:ok? v)]

           [ln (:notes v)])]

        (expect (= [] (vec accepted)))))
  ;; Regression: parinfer's own answer to a `[` mistyped as `(` is `(foo (1 2 3))` — the
  ;; caller's VECTOR turned into a call that swallowed the argument standing after it. That
  ;; candidate parses, keeps the line count and the final newline, changes only the line the
  ;; edit wrote and leaves the skeleton identical, so the ORDER of the caller's own
  ;; delimiters is the only thing left that can refuse it.
  (it "refuses the pack's own repair when it retypes a delimiter the caller wrote"
      (let
        [broken
         "(ns ok)\n\n(defn ok [] (foo (1 2] 3))\n"

         verdict
         (balance/rebalance {:balancer (clj-balancer)
                             :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
                             :source broken
                             :spans [[3 3]]})]

        ;; what the pack answers on its own is the corruption
        (expect (= "(ns ok)\n\n(defn ok [] (foo (1 2 3)))\n" ((clj-balancer) broken)))
        (expect (false? (:ok? verdict)))
        (expect (str/includes? (:why verdict) "retype"))))
  (it "leaves an unrepairable edit refused"
      (let
        [source
         "(ns ok)\n\n(defn ok [] 1)\n"

         result
         (zipper/edit "clojure"
                      source
                      [1]
                      :replace
                      "(defn ok [] \"unterminated"
                      {:balancer (clj-balancer)})]

        (expect (= :syntax-broken (get-in result [:error :reason])))))
  ;; A closer omitted INSIDE a line is the one place indentation cannot help: parinfer closes at
  ;; the line's end, and `(map? x) (str …)` came back as `(map? x (str …))` — a cond clause turned
  ;; into a call, parsing, and written. The text the edit replaced says where it sat instead.
  (it
    "seats a closer dropped inside a line where the replaced text had it, in every shape"
    (let
      [lines
       (str/split-lines shapes-corpus)

       mutate
       (fn [ln text]
         (str/join "\n" (concat (take (dec (long ln)) lines) [text] (drop (long ln) lines) [""])))

       verdicts
       (for
         [ln
          (range 1 (inc (count lines)))

          mut
          (inside-drops (nth lines (dec (long ln))))]

         [ln mut
          (balance/rebalance {:balancer (clj-balancer)
                              :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
                              :source (mutate ln mut)
                              :original shapes-corpus
                              :spans [[ln ln]]})])]

      ;; the matrix is worth nothing if it silently stopped covering the file
      (expect (<= 50 (count verdicts)))
      (expect (= []
                 (vec (for
                        [[ln mut v]
                         verdicts

                         :when (not= shapes-corpus (:content v))]

                        [ln mut (or (:why v) :wrong-content)]))))))
  ;; The lost OPENER, in every shape. On its own it is the same string as one closer too many and
  ;; is refused as such; against the line it replaced it is the one that can be proved, and the
  ;; file comes back byte-identical instead of being written as loose top-level forms.
  (it "restores an opener the edit lost, in every shape"
      (let
        [lines
         (str/split-lines shapes-corpus)

         verdicts
         (for
           [ln
            (range 1 (inc (count lines)))

            :let [mut
                  (drop-opener (nth lines (dec (long ln))))]
            :when mut]

           [ln mut
            (balance/rebalance
              {:balancer (clj-balancer)
               :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
               :source (str/join
                         "\n"
                         (concat (take (dec (long ln)) lines) [mut] (drop (long ln) lines) [""]))
               :original shapes-corpus
               :spans [[ln ln]]})])]

        (expect (<= 20 (count verdicts)))
        (expect (= []
                   (vec (for
                          [[ln mut v]
                           verdicts

                           :when (not= shapes-corpus (:content v))]

                          [ln mut (or (:why v) :wrong-content)])))))))

(defdescribe
  seated-repair-in-every-shape-test
  "The text an edit REPLACED, over the shapes a model actually edits and with the real
   parinfer behind it: what a repair can put back from it, and what it must refuse
   instead of guessing at."
  ;; Regression: a closer the model RETYPED as an opener — `(:require [clojure.string :as str])`
  ;; sent as `… :as str()` — passed every rule there was: the code is identical, the repair only
  ;; ADDED, and it stayed on the edited line. `[… str ()]` parses, so it was written. The line the
  ;; edit replaced is the only witness that the `(` was never meant.
  (it
    "refuses every retyped delimiter, in every shape"
    (let
      [lines
       (str/split-lines shapes-corpus)

       verdicts
       (for
         [ln
          (range 1 (inc (count lines)))

          mut
          (retypes (nth lines (dec (long ln))))]

         [ln mut
          (balance/rebalance
            {:balancer (clj-balancer)
             :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
             :source
             (str/join "\n" (concat (take (dec (long ln)) lines) [mut] (drop (long ln) lines) [""]))
             :original shapes-corpus
             :spans [[ln ln]]})])]

      (expect (<= 300 (count verdicts)))
      ;; not one of them may be written, whatever the repair would have made of it
      (expect (= []
                 (vec (for
                        [[ln mut v]
                         verdicts

                         :when (:ok? v)]

                        [ln mut (:content v)]))))
      ;; and the refusal names the substitution, not just "a delimiter moved"
      (expect (<= 100
                  (count (filter (fn [[_ _ v]]
                                   (str/includes? (str (:why v)) "retyped or added, not omitted"))
                                 verdicts))))))
  ;; A line the edit REWROTE as well as broke: its skeleton no longer matches, so the delimiters
  ;; are the caller's own — but it is still the same line, and the text it replaced still says
  ;; where the one it KEPT used to sit. Without that, parinfer closes at the line's end.
  (it
    "seats a delimiter into a line the edit also rewrote, in every shape"
    (let
      [lines
       (str/split-lines shapes-corpus)

       splice
       (fn [ln text]
         (str/join "\n" (concat (take (dec (long ln)) lines) [text] (drop (long ln) lines) [""])))

       cases
       (for
         [ln
          (range 1 (inc (count lines)))

          :let [rewrote
                (rename-token (nth lines (dec (long ln))))]
          :when rewrote
          :let [intended
                (splice ln rewrote)]
          :when (empty? (zipper/error-nodes "clojure" intended))
          mut
          (inside-drops rewrote)]

         [ln mut intended
          (balance/rebalance {:balancer (clj-balancer)
                              :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
                              :source (splice ln mut)
                              :original shapes-corpus
                              :spans [[ln ln]]})])]

      (expect (<= 50 (count cases)))
      (expect (= []
                 (vec (for
                        [[ln mut intended v]
                         cases

                         :when (not= intended (:content v))]

                        [ln mut (or (:why v) :wrong-content)]))))))
  ;; Regression, the third face: the edit SWAPPED two lines and dropped one closer; parinfer closed
  ;; the OTHER line — one this edit left exactly as it found it — the file parsed, and `(if (seq
  ;; names)` with two branches became a call with none.
  (it
    "never closes a line the edit left as it found it, in every shape"
    (let
      [lines
       (str/split-lines shapes-corpus)

       file
       (fn [ls]
         (str (str/join "\n" ls) "\n"))

       cases
       (for
         [i
          (range (dec (count lines)))

          :let [a
                (nth lines i)

                b
                (nth lines (inc (long i)))

                broken
                (drop-closers a 1)]
          :when broken
          :let [intended
                (file (concat (take i lines) [b a] (drop (+ (long i) 2) lines)))]
          :when (empty? (zipper/error-nodes "clojure" intended))]

         [(inc (long i)) intended
          (balance/rebalance {:balancer (clj-balancer)
                              :parses-clean? #(empty? (zipper/error-nodes "clojure" %))
                              :source
                              (file (concat (take i lines) [b broken] (drop (+ (long i) 2) lines)))
                              :original shapes-corpus
                              :spans [[(inc (long i)) (+ (long i) 2)]]})])]

      (expect (<= 8 (count cases)))
      ;; every case is the file the caller meant, or a refusal — never a third file
      (expect (= []
                 (vec (for
                        [[ln intended v]
                         cases

                         :when (and (:ok? v) (not= intended (:content v)))]

                        [ln (:content v)])))))))
