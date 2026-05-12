(ns com.blockether.vis.ext.lang-clojure.core-test
  (:require
   [babashka.fs :as fs]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.lang-clojure.core :as clj-ext]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]))

(defn- clj-manifest-file []
  (let [repo-root-file (io/file "extensions/languages/clojure/resources/META-INF/vis-extension/vis.edn")]
    (if (.exists repo-root-file)
      repo-root-file
      (io/file "resources/META-INF/vis-extension/vis.edn"))))

(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.lang-clojure.core" name))))

(defn- temp-root [name]
  (let [root (fs/file "target/lang-clojure-test" name)]
    (fs/delete-tree root)
    (fs/create-dirs root)
    (.getCanonicalFile root)))

(defdescribe clojure-extension-activation-test
  (it "activates for Clojure marker files"
    (let [root (temp-root "marker")
          clojure-project? (private-fn "clojure-project?")]
      (spit (fs/file root "deps.edn") "{}")
      (expect (true? (clojure-project? root)))))

  (it "activates for nested Clojure source files"
    (let [root (temp-root "source")
          clojure-project? (private-fn "clojure-project?")]
      (fs/create-dirs (fs/file root "src" "demo"))
      (spit (fs/file root "src" "demo" "core.clj") "(ns demo.core)\n")
      (expect (true? (clojure-project? root)))))

  (it "stays inactive when no Clojure marker or source exists"
    (let [root (temp-root "plain")
          clojure-project? (private-fn "clojure-project?")]
      (spit (fs/file root "README.md") "plain\n")
      (expect (false? (clojure-project? root)))))

  (it "registers language-gated z extension metadata"
    (expect (= 'com.blockether.vis.ext.lang-clojure.core
              (:ext/namespace clj-ext/clojure-extension)))
    (expect (= "languages" (:ext/kind clj-ext/clojure-extension)))
    (expect (fn? (:ext/activation-fn clj-ext/clojure-extension))))

  (it "exports z/patch helpers, semantic readers, repair/lsp tools, and rewrite-clj zipper API"
    (let [symbols (set (map :ext.symbol/symbol (:ext/symbols clj-ext/clojure-extension)))]
      (expect (contains? symbols 'source))
      (expect (contains? symbols 'lit))
      (expect (contains? symbols 'patch))
      (expect (contains? symbols 'patch-check))
      (expect (contains? symbols 'forms))
      (expect (contains? symbols 'locators))
      (expect (contains? symbols 'symbols))
      (expect (contains? symbols 'locator-for-symbol))
      (expect (contains? symbols 'inspect))
      (expect (contains? symbols 'find-value))
      (expect (contains? symbols 'replace))
      (expect (contains? symbols 'subedit->))
      (expect (contains? symbols 'repair-range))
      (expect (contains? symbols 'repair-locator))
      (expect (contains? symbols 'repair-file))
      (expect (contains? symbols 'diagnostics))
      (expect (contains? symbols 'rename-plan))
      (expect (contains? symbols 'clean-ns-plan))))

  (it "adds minimal Clojure structural-editing strategy to environment info"
    (let [info ((:ext/environment-prompt-fn clj-ext/clojure-extension) {})]
      (expect (string? info))
      (expect (str/includes? info "Clojure/EDN"))
      (expect (str/includes? info "z/forms"))
      (expect (str/includes? info "z/locators"))
      (expect (str/includes? info "z/symbols"))
      (expect (str/includes? info "add :replace"))
      (expect (str/includes? info "z/patch-check"))
      (expect (str/includes? info "v/patch outside"))
      (expect (not (str/includes? info "clojure.repl/doc")))
      (expect (< (count info) 300))))

  (it "exposes rewrite-clj zipper API as raw composable SCI symbols"
    (let [entry (first (filter #(= 'of-string (:ext.symbol/symbol %))
                         (:ext/symbols clj-ext/clojure-extension)))]
      (expect (fn? (:ext.symbol/fn entry)))
      (expect (true? (:ext.symbol/raw? entry)))
      (expect (seq (:ext.symbol/arglists entry)))))

  (it "makes rewrite-clj threading macros callable inside SCI"
    (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]} (vis/create-sci-context nil)
          env {:sci-ctx sci-ctx
               :sandbox-ns sandbox-ns
               :initial-ns-keys initial-ns-keys
               :extensions (atom [])}
          eval* (fn [s] (:val (sci/eval-string+ sci-ctx s {:ns sandbox-ns})))]
      (vis/install-extension! env clj-ext/clojure-extension)
      (expect (= "(a b)"
                (eval* "(z/root-string (z/of-string \"(a b)\"))")))
      (expect (= "(x b)"
                (eval* "(z/root-string (z/subedit-> (z/of-string \"(a b)\") z/down (z/replace (quote x))))")))
      (expect (= "(x b)"
                (eval* "(z/root-string (z/subedit->> (z/of-string \"(a b)\") (#(z/down %)) (#(z/replace % (quote x)))))")))
      (expect (= "(x b)"
                (eval* "(z/root-string (z/edit-> (z/of-string \"(a b)\") z/down (z/replace (quote x))))")))
      (expect (= "(x b)"
                (eval* "(z/root-string (z/edit->> (z/of-string \"(a b)\") (#(z/down %)) (#(z/replace % (quote x)))))"))))))

(it "ships manifest docs with current envelope syntax only"
  (let [manifest (edn/read-string {:readers {} :default (fn [_ form] form)}
                   (slurp (clj-manifest-file)))
        readme   (get-in manifest ['clj :docs "README.md" :content])]
    (expect (str/includes? readme ":result :files"))
    (expect (str/includes? readme "(get-in (z/locators \"src/foo.clj\") [:result])"))
    (expect (not (str/includes? readme "[:result]")))
    (expect (not (str/includes? readme "[:info :files]")))))
