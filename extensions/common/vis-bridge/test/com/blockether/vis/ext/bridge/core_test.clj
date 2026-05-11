(ns com.blockether.vis.ext.bridge.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.bridge.core :as bridge]
   [com.blockether.vis.ext.bridge.languages.schema :as schema]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- manifest-file []
  (let [repo-root-file (io/file "extensions/common/vis-bridge/resources/META-INF/vis-extension/vis.edn")]
    (if (.exists repo-root-file)
      repo-root-file
      (io/file "resources/META-INF/vis-extension/vis.edn"))))

(defdescribe bridge-extension-test
  (it "ships a parseable vis.edn manifest"
    (let [manifest (edn/read-string (slurp (manifest-file)))]
      (expect (= '[com.blockether.vis.ext.bridge.core] (get-in manifest ['bridge :nses])))
      (expect (str/includes? (get-in manifest ['bridge :docs "README.md" :content]) "bridge/extract-markdown"))))

  (it "exports a valid extension surface"
    (expect (= 'com.blockether.vis.ext.bridge.core (:ext/namespace bridge/vis-extension)))
    (expect (= {:ns 'vis.ext.bridge :alias 'bridge} (:ext/ns-alias bridge/vis-extension)))
    (expect (= '[extract extract-file extract-markdown clojure-lsp-status extract-clojure
                 aggregate-rows fill! extract-and-fill! backfill!]
              (mapv :ext.symbol/sym (:ext/symbols bridge/vis-extension))))
    (expect (fn? (:ext/doctor-check-fn bridge/vis-extension)))))

(defdescribe bridge-generic-extract-test
  (it "extracts one Markdown file through the generic entrypoint"
    (let [result (bridge/extract {:path "README.md"
                                  :language "markdown"
                                  :content "# Title\n\nSee `demo.core/run`."})]
      (expect (= "markdown" (get-in result [:stats :language])))
      (expect (= (bridge/content-sha256 "# Title\n\nSee `demo.core/run`.")
                (get-in result [:stats :hash-sha256])))
      (expect (= #{:file :doc-section} (set (map :kind (:nodes result)))))))

  (it "reads relative paths from the active Vis workspace root"
    (let [root (.getCanonicalFile (io/file (str (java.nio.file.Files/createTempDirectory "bridge-workspace-" (make-array java.nio.file.attribute.FileAttribute 0)))))
          file (io/file root "README.md")]
      (try
        (spit file "# Workspace\n")
        (binding [workspace/*workspace-root* (.getCanonicalPath root)]
          (let [result (bridge/extract {:path "README.md" :language "markdown"})]
            (expect (= "README.md" (get-in result [:stats :path])))
            (expect (= (bridge/content-sha256 "# Workspace\n")
                      (get-in result [:stats :hash-sha256])))))
        (finally
          (io/delete-file file true)
          (io/delete-file root true))))))

(def sample-result
  (schema/extract-result
    {:nodes [(schema/node
               {:kind :symbol
                :language "clojure"
                :name "run"
                :qualified-name "demo.core/run"
                :path "src/demo/core.clj"})]
     :edges [(schema/edge
               {:edge-kind :calls
                :source "demo.core/run"
                :target "demo.core/parse"
                :path "src/demo/core.clj"
                :language "clojure"})]
     :diagnostics []
     :stats {:language "clojure" :path "src/demo/core.clj"}}))

(defdescribe bridge-fill-shape-test
  (it "maps normalized facts to aggregate rows consistently"
    (let [rows (bridge/aggregate-rows sample-result)
          by-kind (group-by :kind rows)]
      (expect (= #{:bridge/node :bridge/edge :bridge/index} (set (keys by-kind))))
      (expect (= "node:demo.core/run" (:key (first (:bridge/node by-kind)))))
      (expect (= "edge:demo.core/run::calls::demo.core/parse" (:key (first (:bridge/edge by-kind)))))
      (expect (= {:path "src/demo/core.clj"
                  :language "clojure"
                  :kind "symbol"
                  :name "run"
                  :visibility "unknown"}
                (:metadata (first (:bridge/node by-kind)))))))

  (it "can override index path explicitly"
    (let [idx (first (filter #(= :bridge/index (:kind %))
                       (bridge/aggregate-rows sample-result {:path "override.clj"})))]
      (expect (= "idx:override.clj" (:key idx)))
      (expect (= "override.clj" (get-in idx [:metadata :path])))))

  (it "stores path hashes on index rows when extraction stats have them"
    (let [result (assoc-in sample-result [:stats :hash-sha256] "abc123")
          idx (first (filter #(= :bridge/index (:kind %))
                       (bridge/aggregate-rows result)))]
      (expect (= "abc123" (get-in idx [:metadata :hash-sha256])))
      (expect (= "abc123" (get-in idx [:content :hash-sha256])))))

  (it "reports result paths and language for replacement policy"
    (expect (= ["src/demo/core.clj"] (bridge/result-paths sample-result)))
    (expect (= "clojure" (bridge/result-language sample-result)))))
