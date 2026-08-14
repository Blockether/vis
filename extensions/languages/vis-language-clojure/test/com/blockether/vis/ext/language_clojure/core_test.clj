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
             (it "defaults run_tests to just under the 5 minute native tool budget"
                 (expect (= 290000 @#'test-runner/default-test-timeout-ms))))

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
                        (with-redefs-fn {#'repl-manager/ensure-repl-for-dir! (constantly {:port
                                                                                          54321})
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

(defdescribe test-runner-repl-gate-test
             (it "runs via the CLI suite when there is no launchable build file (no-launcher)"
                 (let
                   [called
                    (atom false)

                    result
                    (with-example-project
                      (fn [root]
                        (with-redefs-fn {#'repl-manager/ensure-repl-for-dir!
                                         (constantly {"result" "no-launcher" "status" "down"})
                                         #'test-runner/run-via-cli
                                         (fn [_root norm]
                                           (reset! called true)
                                           {"mode" "cli" "ns" (first (:nses norm)) "is_pass" true})}
                          #(test-runner/clj-test-fn {:workspace/root root} {"paths" ["test"]}))))]

                   (expect @called)
                   (expect (= "cli" (get-in result [:result "mode"])))))
             (it "surfaces the launcher's boot-failure story instead of silently CLI-falling-back"
                 (let
                   [cli-called
                    (atom false)

                    result
                    (with-example-project
                      (fn [root]
                        (with-redefs-fn
                          {#'repl-manager/ensure-repl-for-dir!
                           (constantly
                             {"result" "failed"
                              "status" "failed"
                              "message"
                              "nREPL launcher exited before accepting connections (exit 1)"
                              "log_tail" "Syntax error compiling."})
                           #'test-runner/run-via-cli (fn [& _]
                                                       (reset! cli-called true)
                                                       {"mode" "cli"})}
                          #(test-runner/clj-test-fn {:workspace/root root} {"paths" ["test"]}))))

                    r
                    (:result result)]

                   (expect (not @cli-called))
                   (expect (= "repl" (get r "mode")))
                   (expect (str/includes? (get r "error") "not running (status failed)"))
                   (expect (str/includes? (get r "error") "exited before accepting connections"))
                   (expect (= "Syntax error compiling." (get r "log_tail"))))))

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
                 (with-redefs-fn {#'repl-manager/ensure-repl-for-dir! (fn [_sid dir]
                                                                        (reset! seen dir)
                                                                        nil)
                                  #'test-runner/run-via-cli
                                  (fn [_root norm]
                                    {"mode" "cli" "ns" (first (:nses norm)) "is_pass" true})}
                   #(test-runner/clj-test-fn {:workspace/root (.getAbsolutePath root)}
                                             {"paths" ["services/svc/test"]}))
                 ;; the nREPL is autostarted at services/svc, where deps.edn lives
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

        (expect (= :syntax-broken (get-in result [:error :reason]))))))
