(ns com.blockether.vis.private-deployment-hygiene-test
  "This repository is public. Blockether's own hosted gateway deployment is
   private infrastructure and must never be described here — not in docs, not in
   YAML, not in a code comment. Everything about that deployment (hostname,
   private bind address, ingress chain, server names, units, playbooks) lives in
   the private infrastructure repository.

   The needles below are assembled at runtime from fragments so that this guard
   never itself contains the strings it forbids."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private pruned-dirs
  "Directory names never scanned: VCS, build output, vendored deps, generated
   mobile projects, caches."
  #{".git" ".cpcache" ".clj-kondo" ".lsp" ".idea" ".vscode" ".gradle" ".vis" "target" "classes"
    "node_modules" "dist" "build" "out" "coverage" "ios" "android" "browsers" "venv" ".venv"
    "__pycache__" ".pytest_cache"})

(def ^:private scanned-extensions
  #{"md" "mdx" "txt" "clj" "cljc" "cljs" "cljd" "edn" "bb" "yml" "yaml" "json" "ts" "tsx" "js" "jsx"
    "mjs" "cjs" "sh" "bash" "zsh" "toml" "ini" "conf" "properties" "service" "py" "html" "css" "sql"
    "gradle" "plist" "xml"})

(def ^:private scanned-names #{"Dockerfile" "Makefile" "Justfile" "Caddyfile" "Procfile"})

(def ^:private max-bytes (* 2 1024 1024))

(def ^:private forbidden
  "Each entry: what it leaks, a runtime-assembled pattern, and the fix."
  [{:what "the private production gateway hostname"
    :re (re-pattern (str "(?i)vis\\." "blockether" "\\.com"))
    :fix
    "use a neutral placeholder (gateway.example.com) and document the real deployment in the private infrastructure repo"}
   {:what "the private production gateway bind address"
    :re (re-pattern (str "\\b10\\.0\\.1\\." "4\\b"))
    :fix "use a neutral placeholder (10.0.0.5) in examples"}])

(defn- scannable?
  [^java.io.File file]
  (let
    [name
     (.getName file)

     ext
     (str/lower-case (or (second (re-find #"\.([^.]+)$" name)) ""))]

    (and (.isFile file)
         (< (.length file) max-bytes)
         (or (contains? scanned-names name) (contains? scanned-extensions ext)))))

(defn- text-files
  [^java.io.File dir]
  (mapcat (fn [^java.io.File file]
            (cond (.isDirectory file)
                  (if (contains? pruned-dirs (.getName file)) [] (text-files file))
                  (scannable? file) [file]
                  :else []))
          (or (.listFiles dir) [])))

(defn- leaks
  "Every `path — what — fix` line found under `root`."
  [root]
  (vec (for
         [^java.io.File file
          (text-files (io/file root))

          :let [text
                (try (slurp file) (catch Exception _ ""))]
          {:keys [what re fix]}
          forbidden

          :when (re-find re text)]

         (str (.getPath file) " leaks " what " — " fix))))

(defdescribe private-deployment-hygiene-test
             (it "never documents Blockether's private gateway deployment anywhere in the repo"
                 (let [found (if (.exists (io/file "deps.edn")) (leaks ".") [])]
                   (expect (empty? found)
                           (str "private deployment details committed to a public repo:\n"
                                (str/join "\n" found))))))
