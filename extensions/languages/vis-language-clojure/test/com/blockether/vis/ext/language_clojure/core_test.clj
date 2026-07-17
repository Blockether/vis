(ns com.blockether.vis.ext.language-clojure.core-test
  "Activation-gate test for the language-clojure extension. Confirms
   the extension activates on Clojure workspaces and stays dark on
   plain ones."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.language-clojure.core :as core]
            [com.blockether.vis.ext.language-clojure.format :as fmt]
            [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
            [com.blockether.vis.ext.language-clojure.test-runner :as test-runner]
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
  (let [cl
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
                 (let [manifests
                       (classpath-manifests)

                       merged
                       (reduce merge {} manifests)]

                   (expect (seq manifests))
                   (expect (contains? merged 'language-clojure))))
             (it "manifest registers the core namespace under the language-clojure id"
                 (let [manifests
                       (classpath-manifests)

                       merged
                       (reduce merge {} manifests)]

                   (expect (some #{'com.blockether.vis.ext.language-clojure.core}
                                 (get-in merged ['language-clojure :nses]))))))

(defdescribe surface-test
             (it "exposes NO engine verbs — repair+format ride the facade + op-hooks, no clj/ alias"
                 ;; clj_paren_repair / the `clj/` engine are gone: paren repair now rides inside
                 ;; `format` AND the auto-repair op-hook. The manifest declares no :ext/engine;
                 ;; the constructor scaffolds an EMPTY one (no alias, no symbols).
                 (let [engine (:ext/engine core/vis-extension)]
                   (expect (nil? (:ext.engine/alias engine)))
                   (expect (empty? (:ext.engine/symbols engine)))))
             (it "registers its repair/no-fail behavior DECLARATIVELY via :ext/op-hooks"
                 (let [hooks
                       (:ext/op-hooks core/vis-extension)

                       ops
                       (set (map (juxt :op :phase) hooks))]

                   (expect (= 3 (count hooks)))
                   (expect (contains? ops [:write :after]))
                   (expect (contains? ops [:struct_patch :around]))
                   (expect (contains? ops [:patch :around]))
                   ;; every entry names a real fn — no imperative register-op-hook! at load
                   (expect (every? ifn? (map :fn hooks))))))

(defdescribe repl-resource-logs-test
             (it "registers managed nREPL resources with tail-able launcher logs"
                 (let [dir
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
                                                       "dir" (.getAbsolutePath dir)
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
                 (let [src
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
                   ;; the repaired output is stable: re-running the formatter is a no-op
                   (expect (= out (core/clj-repair+format out))))))

(defdescribe multi-file-format-test
             (it "formats every file in {\"paths\": [...]} IN PLACE and rolls up per-file changes"
                 (let [dir (tmp-dir)]
                   (try (let [f1 (io/file dir "a.clj")
                              f2 (io/file dir "b.clj")]

                          (spit f1 "(defn f [x]\n(* x 2))\n") ; mis-indented -> changes
                          (spit f2 "(defn g [y] (+ y 1))\n")  ; already tidy -> no change
                          (let [r (core/clj-format-fn {:workspace/root (str dir)}
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
  single-relative-path-lint-test
  (it "resolves a RELATIVE {\"path\"} against the workspace root, not the process CWD"
      (let [dir (tmp-dir)]
        (try (let [sub (io/file dir "sub")]
               (.mkdirs sub)
               ;; unused binding x -> a clj-kondo warning
               (spit (io/file sub "probe.clj") "(ns sub.probe)\n(defn foo [] (let [x 1] 42))\n")
               ;; the relative path exists ONLY under the workspace root, never under CWD
               (expect (not (.exists (io/file (System/getProperty "user.dir") "sub/probe.clj"))))
               (let [r (core/clj-lint-fn {:workspace/root (str dir)} {"path" "sub/probe.clj"})
                     findings (get-in r [:result "findings"])]

                 (expect (:success? r))
                 ;; the file under root was actually linted (not silently skipped)
                 (expect (= 1 (count findings)))
                 ;; reported file path is workspace-relative
                 (expect (= "sub/probe.clj" (get (first findings) "file")))
                 (expect (= "unused binding x" (get (first findings) "message")))))
             (finally (cleanup dir))))))

(defdescribe
  recursive-format-test
  (it "formats a DIRECTORY in {\"paths\"} RECURSIVELY, skipping non-Clojure files"
      (let [dir (tmp-dir)]
        (try (let [sub (io/file dir "sub")]
               (.mkdirs sub)
               (spit (io/file dir "a.clj") "(defn f [x]\n(* x 2))\n") ; mis-indented -> changes
               (spit (io/file sub "b.cljc") "(defn g [y]\n(+ y 1))\n") ; nested -> changes
               (spit (io/file sub "c.clj") "(defn h [z] (dec z))\n")   ; tidy -> no change
               (spit (io/file dir "notes.txt") "not clojure\n") ; must be ignored
               (let [r (core/clj-format-fn {:workspace/root (str dir)} {"paths" [(str dir)]})
                     files (get-in r [:result "files"])]

                 (expect (:success? r))
                 ;; only the 3 Clojure sources, walked recursively; the .txt is skipped
                 (expect (= 3 (count files)))
                 (expect (= ["a.clj" "sub/b.cljc" "sub/c.clj"] (sort (mapv #(get % "path") files))))
                 (expect (= 2 (get-in r [:result "changed"]))) ; a + b changed, c tidy
                 (expect (= "(defn f [x]\n  (* x 2))\n" (slurp (io/file dir "a.clj"))))
                 (expect (= "(defn g [y]\n  (+ y 1))\n" (slurp (io/file sub "b.cljc"))))
                 (expect (= "not clojure\n" (slurp (io/file dir "notes.txt"))))))
             (finally (cleanup dir))))))

(defdescribe default-project-format-test
             (it
               "with no arg / {} formats the workspace's src + test RECURSIVELY, ignoring the rest"
               (let [dir (tmp-dir)]
                 (try (let [src (io/file dir "src")
                            tst (io/file dir "test")]

                        (.mkdirs src)
                        (.mkdirs tst)
                        (spit (io/file src "a.clj") "(defn f [x]\n(* x 2))\n")
                        (spit (io/file tst "a_test.clj") "(defn t [] 1)\n")
                        (spit (io/file dir "ignored.clj") "(def top 1)\n") ; not under src/test
                        (let [empty-map (core/clj-format-fn {:workspace/root (str dir)} {})
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
             (let [messy "(myblock a\nb\nc)"
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

(defdescribe
  edit-repair-hook-test
  (it "the :after hook repairs+formats a .clj file in place after a successful edit"
      (let [dir
            (tmp-dir)

            f
            (io/file dir "x.clj")]

        (try (spit f "(defn f [x]\n        (+ x 1))\n") ; valid but mis-indented
             (let [res
                   (core/clj-edit-repair-hook {:workspace/root (.getPath dir)}
                                              :struct_patch
                                              [{"path" "x.clj"}]
                                              {:success? true})

                   after
                   (slurp f)]

               (expect (= {:success? true} res)) ; result passes through
               (expect (not= "(defn f [x]\n        (+ x 1))\n" after)) ; reformatted
               (expect (re-find #"\(defn f \[x\]" after)))
             (finally (cleanup dir)))))
  (it "leaves a non-Clojure file untouched"
      (let [dir
            (tmp-dir)

            f
            (io/file dir "x.py")]

        (try (spit f "def g( ):\n  pass\n")
             (core/clj-edit-repair-hook {:workspace/root (.getPath dir)}
                                        :patch
                                        [{"path" "x.py"}]
                                        {:success? true})
             (expect (= "def g( ):\n  pass\n" (slurp f)))
             (finally (cleanup dir)))))
  (it
    "re-diffs the summary against FINAL disk bytes (truthful diff, no false structural flag)"
    (let [dir
          (tmp-dir)

          f
          (io/file dir "x.clj")

          before
          "(defn f [x]\n  (+ x 1))\n"]

      (try
        ;; the raw edit wrote a mis-indented body to disk; the summary's diff
        ;; still shows that col-0 INTENT (the bug this fixes).
        (spit f "(defn f [x]\n(+ x 2))\n")
        (let [result
              {:success? true
               :result [{"path" "x.clj" "op" "update" "changed" true "diff" "STALE"}]
               :metadata {:file-befores [{:path "x.clj" :before before}]}}

              out
              (core/clj-edit-repair-hook {:workspace/root (.getPath dir)}
                                         :patch
                                         [{"path" "x.clj"}]
                                         result)

              summ
              (first (:result out))]

          ;; cljfmt re-indented the body on disk ...
          (expect (re-find #"\n  \(\+ x 2\)\)" (slurp f)))
          ;; ... and the returned diff now reflects THAT, not the col-0 intent.
          (expect (re-find #"\+  \(\+ x 2\)\)" (get summ "diff")))
          (expect (not (contains? summ "repaired"))))
        (finally (cleanup dir)))))
  (it
    "flags a parinfer STRUCTURAL repair loudly on the summary"
    (let [dir
          (tmp-dir)

          f
          (io/file dir "x.clj")

          before
          "(defn f [x]\n  (inc x))\n"]

      (try
        ;; raw edit left an unbalanced delimiter (missing final closer) on disk
        (spit f "(defn g [x]\n  (when x\n    (inc x))\n")
        (let [result
              {:success? true
               :result [{"path" "x.clj" "op" "update" "changed" true "diff" "STALE"}]
               :metadata {:file-befores [{:path "x.clj" :before before}]}}

              out
              (core/clj-edit-repair-hook {:workspace/root (.getPath dir)}
                                         :patch
                                         [{"path" "x.clj"}]
                                         result)

              summ
              (first (:result out))]

          ;; parinfer balanced the file ...
          (expect (re-find #"\(inc x\)\)\)" (slurp f)))
          ;; ... and the summary is loudly flagged so a scope shift can't hide.
          (expect (= true (get summ "repaired")))
          (expect (re-find #"CHANGED STRUCTURE" (get summ "note"))))
        (finally (cleanup dir)))))
  (it "is a no-op when the edit did NOT succeed"
      (let [dir
            (tmp-dir)

            f
            (io/file dir "x.clj")]

        (try (spit f "(defn f [x]\n        (+ x 1))\n")
             (core/clj-edit-repair-hook {:workspace/root (.getPath dir)}
                                        :struct_patch
                                        [{"path" "x.clj"}]
                                        {:success? false})
             (expect (= "(defn f [x]\n        (+ x 1))\n" (slurp f)))
             (finally (cleanup dir))))))

(defdescribe test-runner-timeout-test
             (it "defaults run_tests to just under the 5 minute native tool budget"
                 (expect (= 290000 @#'test-runner/default-test-timeout-ms))))

(defdescribe test-runner-fallback-test
             (it "falls back to the project test CLI when the live nREPL lacks lazytest"
                 (let [called
                       (atom false)

                       result
                       (with-redefs-fn {#'repl-manager/ensure-repl-for-dir! (constantly {:port
                                                                                         54321})
                                        #'test-runner/run-via-repl
                                        (fn [& _]
                                          {"error" "Could not locate lazytest/core"})
                                        #'test-runner/run-via-cli
                                        (fn [_root norm]
                                          (reset! called true)
                                          {"mode" "cli" "ns" (first (:nses norm)) "is_pass" true})}
                         #(test-runner/clj-test-fn {:workspace/root "."} "example.core-test"))]

                   (expect @called)
                   (expect (= "cli" (get-in result [:result "mode"])))
                   (expect (= "clojure" (get-in result [:result "language"]))))))

(defdescribe test-runner-repl-gate-test
             (it "runs via the CLI suite when there is no launchable build file (no-launcher)"
                 (let [called
                       (atom false)

                       result
                       (with-redefs-fn {#'repl-manager/ensure-repl-for-dir!
                                        (constantly {"result" "no-launcher" "status" "down"})
                                        #'test-runner/run-via-cli
                                        (fn [_root norm]
                                          (reset! called true)
                                          {"mode" "cli" "ns" (first (:nses norm)) "is_pass" true})}
                         #(test-runner/clj-test-fn {:workspace/root "."} "example.core-test"))]

                   (expect @called)
                   (expect (= "cli" (get-in result [:result "mode"])))))
             (it "surfaces the launcher's boot-failure story instead of silently CLI-falling-back"
                 (let [cli-called
                       (atom false)

                       result
                       (with-redefs-fn
                         {#'repl-manager/ensure-repl-for-dir!
                          (constantly {"result" "failed"
                                       "status" "failed"
                                       "message"
                                       "nREPL launcher exited before accepting connections (exit 1)"
                                       "log_tail" "Syntax error compiling."})
                          #'test-runner/run-via-cli (fn [& _]
                                                      (reset! cli-called true)
                                                      {"mode" "cli"})}
                         #(test-runner/clj-test-fn {:workspace/root "."} "example.core-test"))

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
        (try (let [svc (io/file root "services" "svc")
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

(defn- balanced? [s] (= (count (re-seq #"\(" s)) (count (re-seq #"\)" s))))

(defdescribe
  struct-patch-no-fail-test
  "The :around middleware makes a Clojure struct_patch repair + retry instead of
   failing on unbalanced delimiters."
  (it "retries a .clj edit with paren-repaired code after the editor refuses it"
      (let [seen
            (atom [])

            ;; fake editor: refuses unbalanced code, accepts balanced. Model args
            ;; are STRING-keyed (strings-only boundary).
            next-fn
            (fn [args]
              (let [code (get (first args) "code")]
                (swap! seen conj code)
                (if (balanced? code)
                  {:success? true :result code}
                  (throw (ex-info "syntax broken"
                                  {:type :ext.foundation.editing/struct-zip-error})))))

            out
            (core/clj-struct-patch-no-fail-around
              {}
              :struct_patch
              [{"path" "x.clj" "code" "(defn f [] (+ 1 2)" "op" "replace"}]
              next-fn)]

        (expect (:success? out))
        (expect (= 2 (count @seen)))        ; raw attempt, then repaired retry
        (expect (balanced? (last @seen))))) ; the retry used balanced code
  (it "passes a NON-clj failure straight through (no repair, no retry)"
      (let [calls
            (atom 0)

            next-fn
            (fn [_]
              (swap! calls inc)
              (throw (ex-info "boom" {})))]

        (expect (true? (try (core/clj-struct-patch-no-fail-around {}
                                                                  :struct_patch
                                                                  [{"path" "x.py" "code" "def f("}]
                                                                  next-fn)
                            false
                            (catch clojure.lang.ExceptionInfo _ true))))
        (expect (= 1 @calls))))
  (it "surfaces the ORIGINAL error when repair can't make the edit succeed"
      (let [next-fn (fn [_]
                      (throw (ex-info "still broken" {:type :unfixable})))]
        (expect (= :unfixable
                   (try (core/clj-struct-patch-no-fail-around {}
                                                              :struct_patch
                                                              [{"path" "x.clj"
                                                                "code" "(defn f [] (+ 1 2)"}]
                                                              next-fn)
                        nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

(defdescribe
  patch-no-fail-test
  "The :around middleware rescues an anchored `patch` that the foundation's
   re-parse guard REFUSED: the refusal's `:metadata` carries the WHOLE-BATCH
   `:candidate-plans` + `:broken-paths`; the hook parinfer-repairs each broken
   candidate as a WHOLE SOURCE (fragment repair can't fix contextual imbalance
   — the session-9c829d10 class: a locally-balanced replacement with a stray
   closer), commits the batch itself, and substitutes a success envelope."
  (it
    "whole-source-repairs the broken candidate, writes the WHOLE batch, returns success"
    (let [root (tmp-dir)]
      (try
        ;; the failing class from session 9c829d10: the replacement line is
        ;; locally balanced but carries a stray `]` — only the FULL file
        ;; shows the imbalance, so fragment repair is a no-op on it.
        (let [before-a "(def entries\n  [])\n"
              before-b "(def other 0)\n"
              broken-candidate
              "(def entries\n  [{:id :a :label \"A\"}\n   {:id :b :label \"B\"}]]\n"
              clean-candidate "(def other 1)\n"
              refusal {:success? false
                       :error {:reason :syntax-error :message "would leave a.clj SYNTAX ERROR"}
                       :metadata {:candidate-plans
                                  [{:path "a.clj" :before before-a :after broken-candidate}
                                   {:path "b.clj" :before before-b :after clean-candidate}]
                                  :broken-paths ["a.clj"]}}
              calls (atom 0)
              out (core/clj-patch-no-fail-around
                    {:workspace/root (.getPath root)}
                    :patch
                    [[{:path "a.clj" :from_anchor "1:abc" :replace "x"}]]
                    (fn [_]
                      (swap! calls inc)
                      refusal))]

          (expect (true? (:success? out)))
          (expect (= 1 @calls)) ; no retry — the hook commits itself
          ;; the broken file landed REPAIRED (balanced), the clean one verbatim
          (let [a (slurp (io/file root "a.clj"))]
            (expect (balanced? a))
            (expect (= (count (re-seq #"\[" a)) (count (re-seq #"\]" a)))))
          (expect (= "(def other 1)\n" (slurp (io/file root "b.clj"))))
          ;; the summary marks the repaired file (result vector is STRING-keyed)
          ;; and still carries the same diff body a normal successful patch would render.
          (expect (= [true nil] (mapv #(get % "repaired") (:result out))))
          (expect (= [true true] (mapv #(boolean (seq (get % "diff"))) (:result out))))
          (expect (= [{:path "a.clj" :before before-a} {:path "b.clj" :before before-b}]
                     (get-in out [:metadata :file-befores]))))
        (finally (cleanup root)))))
  (it
    "surfaces the ORIGINAL refusal when repair would introduce lint errors"
    (let [root (tmp-dir)]
      (try
        ;; This is the git.clj failure mode: the model's replacement closed
        ;; `:require` early, leaving old require vectors as orphan forms.
        ;; Parinfer can make that parse by absorbing the orphan vectors back
        ;; into `:require`, but that produces duplicate requires/conflicting
        ;; aliases — a semantic regression, not a safe delimiter repair.
        (let
          [before
           "(ns demo.core\n  (:require [clojure.string :as str]))\n\n(defn blank? [s]\n  (str/blank? s))\n"
           broken-candidate
           "(ns demo.core\n  (:require [clojure.string :as str]\n            [clojure.set :as set])\n            [clojure.string :as str])\n\n(defn blank? [s]\n  (str/blank? s))\n"
           refusal {:success? false
                    :error {:reason :syntax-error :message "orig"}
                    :metadata {:candidate-plans
                               [{:path "x.clj" :before before :after broken-candidate}]
                               :broken-paths ["x.clj"]}}
           out (core/clj-patch-no-fail-around {:workspace/root (.getPath root)}
                                              :patch
                                              [[{:path "x.clj"}]]
                                              (fn [_]
                                                refusal))]

          (expect (false? (:success? out)))
          (expect (= "orig" (get-in out [:error :message])))
          (expect (not (.exists (io/file root "x.clj")))))
        (finally (cleanup root)))))
  (it "surfaces the ORIGINAL refusal when a broken candidate is NOT delimiter-repairable"
      (let [root (tmp-dir)]
        (try
          ;; a candidate with NO delimiter error (fix-delimiters returns it
          ;; unchanged) — the guard flagged it for some non-delimiter reason,
          ;; so the hook must NOT bury the refusal (and must write NOTHING).
          (let [refusal {:success? false
                         :error {:reason :syntax-error :message "orig"}
                         :metadata {:candidate-plans [{:path "a.clj" :after "(def x 1)\n"}]
                                    :broken-paths ["a.clj"]}}
                out (core/clj-patch-no-fail-around {:workspace/root (.getPath root)}
                                                   :patch
                                                   [[{:path "a.clj"}]]
                                                   (fn [_]
                                                     refusal))]

            (expect (false? (:success? out)))
            (expect (= "orig" (get-in out [:error :message])))
            (expect (not (.exists (io/file root "a.clj")))))
          (finally (cleanup root)))))
  (it "surfaces the ORIGINAL refusal when a broken file is NOT Clojure"
      (let [root (tmp-dir)]
        (try (let [refusal {:success? false
                            :error {:reason :syntax-error}
                            :metadata {:candidate-plans [{:path "x.py" :after "def f(:\n"}]
                                       :broken-paths ["x.py"]}}
                   out (core/clj-patch-no-fail-around {:workspace/root (.getPath root)}
                                                      :patch
                                                      [[{:path "x.py"}]]
                                                      (fn [_]
                                                        refusal))]

               (expect (false? (:success? out)))
               (expect (not (.exists (io/file root "x.py")))))
             (finally (cleanup root)))))
  (it "passes a NON-syntax failure straight through"
      (let [out (core/clj-patch-no-fail-around {}
                                               :patch
                                               [[{:path "x.clj"}]]
                                               (fn [_]
                                                 {:success? false :error {:reason :stale}}))]
        (expect (= :stale (get-in out [:error :reason])))))
  (it "passes a syntax refusal WITHOUT candidate plans straight through (old shape)"
      (let [out (core/clj-patch-no-fail-around {}
                                               :patch
                                               [[{:path "x.clj"}]]
                                               (fn [_]
                                                 {:success? false :error {:reason :syntax-error}}))]
        (expect (false? (:success? out))))))
