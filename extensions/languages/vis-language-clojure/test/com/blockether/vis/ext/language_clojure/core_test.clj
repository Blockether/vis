(ns com.blockether.vis.ext.language-clojure.core-test
  "Activation-gate test for the language-clojure extension. Confirms
   the extension activates on Clojure workspaces and stays dark on
   plain ones."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.language-clojure.core :as core]
   [com.blockether.vis.ext.language-clojure.format :as fmt]
   [com.blockether.vis.ext.language-clojure.ports :as ports]
   [com.blockether.vis.ext.language-clojure.test-runner :as test-runner]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-clj-ext-act-" (into-array FileAttribute []))))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))] (.delete f))))

(defn- activation-fn []
  ;; private — reach into ns directly so the manifest stays the
  ;; public contract.
  @#'core/activation-fn)

(defdescribe activation-test
  (it "activates when deps.edn is at the workspace root"
      (let [root (tmp-dir)]
        (try
          (spit (io/file root "deps.edn") "{:paths [\"src\"]}")
          (expect (true? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
          (finally (cleanup root)))))

  (it "activates when only a .nrepl-port is present"
      (let [root (tmp-dir)]
        (try
          (spit (io/file root ".nrepl-port") "7888")
          (expect (true? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
          (finally (cleanup root)))))

  (it "activates when .clj sources exist without any manifest"
      (let [root (tmp-dir)]
        (try
          (let [src (io/file root "src" "x.clj")]
            (.mkdirs (.getParentFile src))
            (spit src "(ns x)"))
          (expect (true? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
          (finally (cleanup root)))))

  (it "stays dark on a non-Clojure workspace"
      (let [root (tmp-dir)]
        (try
          (spit (io/file root "README.md") "# nope\n")
          (let [f (io/file root "src" "x.py")]
            (.mkdirs (.getParentFile f))
            (spit f "print('hi')"))
          (expect (false? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
          (finally (cleanup root)))))

  (it "stays dark when :workspace/root is missing"
      (expect (false? ((activation-fn) {})))))

(defn- classpath-manifests
  "Return every parsed `META-INF/vis-extension/vis.edn` on the classpath.
   `io/resource` only yields the FIRST match (whichever jar loads first),
   but the scanner walks `getResources` — mirror that here so the test
   sees this extension's manifest even when another extension is also
   on the test classpath."
  []
  (let [cl (.getContextClassLoader (Thread/currentThread))
        urls (enumeration-seq (.getResources cl "META-INF/vis-extension/vis.edn"))]
    (mapv (fn [u] (read-string (slurp u))) urls)))

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
      (let [manifests (classpath-manifests)
            merged    (reduce merge {} manifests)]
        (expect (seq manifests))
        (expect (contains? merged 'language-clojure))))

  (it "manifest registers the core namespace under the language-clojure id"
      (let [manifests (classpath-manifests)
            merged    (reduce merge {} manifests)]
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
      (let [hooks (:ext/op-hooks core/vis-extension)
            ops   (set (map (juxt :op :phase) hooks))]
        (expect (= 5 (count hooks)))
        (expect (contains? ops [:struct_patch :after]))
        (expect (contains? ops [:patch :after]))
        (expect (contains? ops [:write :after]))
        (expect (contains? ops [:struct_patch :around]))
        (expect (contains? ops [:patch :around]))
      ;; every entry names a real fn — no imperative register-op-hook! at load
        (expect (every? ifn? (map :fn hooks))))))

(defdescribe combined-format-test
  (it "format does BOTH parinfer delimiter repair AND cljfmt"
      (let [r (core/clj-format-fn "(defn f [x]\n  (+ x 1)")]   ; missing close paren
        (expect (:success? r))
        (expect (true? (get-in r [:result :repaired?])))       ; a ) was added
        (let [t (get-in r [:result :text])]
          (expect (re-find #"\(defn f \[x\]" t))
        ;; balanced now: repairing the output again is a no-op
          (expect (= t (core/clj-repair+format t)))))))

(defdescribe cljfmt-config-test
  (it "honors a project-local .cljfmt.edn (walked up from the file) over cljfmt defaults"
    ;; The churn bug: the hook must READ the nearest .cljfmt.edn, not reformat
    ;; with cljfmt DEFAULTS — a lazytest `it` body indents differently under the
    ;; project's `[[:inner 0]]` override than under stock cljfmt.
      (let [dir (tmp-dir)]
        (try
          (spit (io/file dir ".cljfmt.edn")
                "{:extra-indents {myblock [[:inner 0]]}}")
          (let [messy    "(myblock a\nb\nc)"
                with-cfg (core/clj-repair+format messy (.getPath dir))
                default  (core/clj-repair+format messy nil)]
          ;; config-driven indentation differs from stock defaults ...
            (expect (not= with-cfg default))
          ;; ... and equals formatting with the discovered opts
            (expect (= with-cfg
                       (fmt/format-string messy (fmt/cljfmt-opts-for (.getPath dir))))))
          (finally (cleanup dir)))))
  (it "returns nil opts when no config file is found"
      (let [dir (tmp-dir)]
        (try
          (expect (nil? (fmt/cljfmt-opts-for (.getPath dir))))
          (finally (cleanup dir))))))

(defdescribe edit-repair-hook-test
  (it "the :after hook repairs+formats a .clj file in place after a successful edit"
      (let [dir (tmp-dir) f (io/file dir "x.clj")]
        (try
          (spit f "(defn f [x]\n        (+ x 1))\n")           ; valid but mis-indented
          (let [res (core/clj-edit-repair-hook {:workspace/root (.getPath dir)}
                                               :struct_patch [{:path "x.clj"}] {:success? true})
                after (slurp f)]
            (expect (= {:success? true} res))                  ; result passes through
            (expect (not= "(defn f [x]\n        (+ x 1))\n" after))   ; reformatted
            (expect (re-find #"\(defn f \[x\]" after)))
          (finally (cleanup dir)))))
  (it "leaves a non-Clojure file untouched"
      (let [dir (tmp-dir) f (io/file dir "x.py")]
        (try
          (spit f "def g( ):\n  pass\n")
          (core/clj-edit-repair-hook {:workspace/root (.getPath dir)}
                                     :patch [{:path "x.py"}] {:success? true})
          (expect (= "def g( ):\n  pass\n" (slurp f)))
          (finally (cleanup dir)))))
  (it "is a no-op when the edit did NOT succeed"
      (let [dir (tmp-dir) f (io/file dir "x.clj")]
        (try
          (spit f "(defn f [x]\n        (+ x 1))\n")
          (core/clj-edit-repair-hook {:workspace/root (.getPath dir)}
                                     :struct_patch [{:path "x.clj"}] {:success? false})
          (expect (= "(defn f [x]\n        (+ x 1))\n" (slurp f)))
          (finally (cleanup dir))))))

(defdescribe test-runner-fallback-test
  (it "falls back to the project test CLI when the live nREPL lacks lazytest"
      (let [called (atom false)
            result (with-redefs-fn {#'ports/find-default (constantly 54321)
                                    #'test-runner/run-via-repl (fn [& _]
                                                                 {:error "Could not locate lazytest/core"})
                                    #'test-runner/run-via-cli (fn [_root norm]
                                                                (reset! called true)
                                                                {:mode "cli" :ns (first (:nses norm)) :pass? true})}
                     #(test-runner/clj-test-fn {:workspace/root "."} "example.core-test"))]
        (expect @called)
        (expect (= "cli" (get-in result [:result :mode])))
        (expect (= "clojure" (get-in result [:result :language]))))))

(defn- balanced? [s]
  (= (count (re-seq #"\(" s)) (count (re-seq #"\)" s))))

(defdescribe struct-patch-no-fail-test
  "The :around middleware makes a Clojure struct_patch repair + retry instead of
   failing on unbalanced delimiters."
  (it "retries a .clj edit with paren-repaired code after the editor refuses it"
      (let [seen    (atom [])
          ;; fake editor: refuses unbalanced code, accepts balanced
            next-fn (fn [args]
                      (let [code (:code (first args))]
                        (swap! seen conj code)
                        (if (balanced? code)
                          {:success? true :result code}
                          (throw (ex-info "syntax broken"
                                          {:type :ext.foundation.editing/struct-zip-error})))))
            out (core/clj-struct-patch-no-fail-around
                 {} :struct_patch
                 [{:path "x.clj" :code "(defn f [] (+ 1 2)" :op "replace"}] next-fn)]
        (expect (:success? out))
        (expect (= 2 (count @seen)))                   ; raw attempt, then repaired retry
        (expect (balanced? (last @seen)))))            ; the retry used balanced code
  (it "passes a NON-clj failure straight through (no repair, no retry)"
      (let [calls   (atom 0)
            next-fn (fn [_] (swap! calls inc) (throw (ex-info "boom" {})))]
        (expect (true? (try (core/clj-struct-patch-no-fail-around
                             {} :struct_patch [{:path "x.py" :code "def f("}] next-fn)
                            false (catch clojure.lang.ExceptionInfo _ true))))
        (expect (= 1 @calls))))
  (it "surfaces the ORIGINAL error when repair can't make the edit succeed"
      (let [next-fn (fn [_] (throw (ex-info "still broken" {:type :unfixable})))]
        (expect (= :unfixable
                   (try (core/clj-struct-patch-no-fail-around
                         {} :struct_patch [{:path "x.clj" :code "(defn f [] (+ 1 2)"}] next-fn)
                        nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

(defdescribe patch-no-fail-test
  "The :around middleware makes a Clojure anchored `patch` repair + retry instead
   of failing on unbalanced delimiters. Unlike struct_patch the foundation patch
   tool RETURNS a `{:success? false … :reason :syntax-error}` envelope (it does
   not throw), so the hook inspects the result and retries once."
  (it "repairs each edit's :replace and retries after a :syntax-error refusal"
      (let [seen    (atom [])
          ;; fake patch tool: refuses unbalanced :replace, accepts balanced
            next-fn (fn [args]
                      (let [rep (get-in (first args) [:edits 0 :replace])]
                        (swap! seen conj rep)
                        (if (balanced? rep)
                          {:success? true :result [{:path "x.clj" :changed? true}]}
                          {:success? false
                           :error {:reason :syntax-error :message "would leave x.clj SYNTAX ERROR"}})))
            arg {:path "x.clj" :edits [{:from_anchor "1:abc" :replace "(defn f [] (+ 1 2)"}]}
            out (core/clj-patch-no-fail-around {} :patch [arg] next-fn)]
        (expect (:success? out))
        (expect (= 2 (count @seen)))                    ; raw attempt, then repaired retry
        (expect (balanced? (last @seen)))))             ; the retry used balanced :replace
  (it "handles the bare-vector (multi-file) edits form with per-edit :path"
      (let [seen    (atom [])
            next-fn (fn [args]
                      (let [rep (get-in (first args) [0 :replace])]
                        (swap! seen conj rep)
                        (if (balanced? rep)
                          {:success? true :result [{:path "a.clj" :changed? true}]}
                          {:success? false :error {:reason :syntax-error}})))
            arg [{:path "a.clj" :from_anchor "1:abc" :replace "(defn f [] (+ 1 2)"}]
            out (core/clj-patch-no-fail-around {} :patch [arg] next-fn)]
        (expect (:success? out))
        (expect (balanced? (last @seen)))))
  (it "passes a NON-syntax failure straight through (no repair, no retry)"
      (let [calls   (atom 0)
            next-fn (fn [_] (swap! calls inc) {:success? false :error {:reason :stale}})
            arg {:path "x.clj" :edits [{:from_anchor "1:abc" :replace "(defn f [] (+ 1 2)"}]}
            out (core/clj-patch-no-fail-around {} :patch [arg] next-fn)]
        (expect (false? (:success? out)))
        (expect (= :stale (get-in out [:error :reason])))
        (expect (= 1 @calls))))                         ; never retried
  (it "leaves a non-clj target untouched (no fragment repair)"
      (let [calls   (atom 0)
            next-fn (fn [_] (swap! calls inc) {:success? false :error {:reason :syntax-error}})
            arg {:path "x.py" :edits [{:from_anchor "1:abc" :replace "def f("}]}
            out (core/clj-patch-no-fail-around {} :patch [arg] next-fn)]
        (expect (false? (:success? out)))
        (expect (= 1 @calls))))                         ; nothing repaired → no retry
  (it "surfaces the ORIGINAL failure when the repaired retry still won't parse"
      (let [calls   (atom 0)
            next-fn (fn [_] (swap! calls inc)
                      {:success? false :error {:reason :syntax-error :message (str "orig#" @calls)}})
            arg {:path "x.clj" :edits [{:from_anchor "1:abc" :replace "(defn f [] (+ 1 2)"}]}
            out (core/clj-patch-no-fail-around {} :patch [arg] next-fn)]
        (expect (false? (:success? out)))
        (expect (= 2 @calls))                           ; original + one repaired retry
        (expect (= "orig#1" (get-in out [:error :message]))))))
