(ns com.blockether.vis.ext.bridge.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.ext.bridge.core :as bridge]
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
    (expect (= '[extract-markdown clojure-lsp-status extract-clojure]
              (mapv :ext.symbol/sym (:ext/symbols bridge/vis-extension))))
    (expect (fn? (:ext/doctor-check-fn bridge/vis-extension)))))
