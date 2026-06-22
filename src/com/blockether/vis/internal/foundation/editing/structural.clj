(ns com.blockether.vis.internal.foundation.editing.structural
  "Unified structural editing via tree-sitter — target a definition by NAME and
   replace / insert around it, for every language the pack understands (Clojure
   included, from our own grammar fork). Built on the same structure extraction
   the `index` tool uses: each definition has a name and a line span, so an edit
   is `locate the node by name -> splice its line span`. Every edit is validated
   by re-parsing the result with tree-sitter and refusing if it introduces a
   syntax error (no broken writes)."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.index :as index])
  (:import [dev.kreuzberg.treesitterlanguagepack
            TreeSitterLanguagePack ProcessConfig ProcessResult StructureItem Span Diagnostic]))

(defn- flatten-items
  "Depth-first [{:name :kind :start :end}] (1-based inclusive lines)."
  [items]
  (mapcat (fn [^StructureItem it]
            (let [^Span s (.span it)]
              (cons {:name  (.name it)
                     :kind  (some-> (.kind it) str str/lower-case)
                     :start (inc (.startLine s))
                     :end   (inc (.endLine s))}
                    (flatten-items (or (.children it) [])))))
          items))

(defn- structure [^String source ^String language]
  (let [cfg (-> (ProcessConfig/builder)
                (.withLanguage language) (.withStructure true) (.build))
        ^ProcessResult res (TreeSitterLanguagePack/process source cfg)]
    (or (.structure res) [])))

(defn- error-diagnostics
  "Error-severity diagnostics for `source` parsed as `language` (empty = clean)."
  [^String source ^String language]
  (let [cfg (-> (ProcessConfig/builder)
                (.withLanguage language) (.withDiagnostics true) (.build))
        ^ProcessResult res (TreeSitterLanguagePack/process source cfg)]
    (->> (or (.diagnostics res) [])
         (filter (fn [^Diagnostic d] (= "error" (some-> (.severity d) str str/lower-case)))))))

(defn- find-target [items target kind]
  (let [matches (cond->> (filter #(= (:name %) target) items)
                  kind (filter #(= (:kind %) (name kind))))]
    (cond
      (empty? matches)
      (throw (ex-info (str "No definition named '" target "'"
                           (when kind (str " of kind " (name kind))) " found.")
                      {:type :ext.foundation.editing/struct-target-not-found :target target}))
      (next matches)
      (throw (ex-info (str (count matches) " definitions named '" target
                           "' — pass :kind to disambiguate ("
                           (str/join ", " (map :kind matches)) ").")
                      {:type :ext.foundation.editing/struct-target-ambiguous :target target}))
      :else (first matches))))

(defn- splice [lines op start end code]
  ;; lines is the 0-based vector; start/end are 1-based inclusive.
  (let [before (subvec lines 0 (dec start))
        item   (subvec lines (dec start) end)
        after  (subvec lines end)]
    (case op
      :replace        (concat before [code] after)
      :insert-before  (concat before [code] item after)
      :insert-after   (concat before item [code] after)
      :append         (concat lines [code])
      (throw (ex-info (str "Unknown structural op: " op)
                      {:type :ext.foundation.editing/struct-bad-op :op op})))))

(defn edit-source
  "Pure structural edit: return the new file content (string), or throw with a
   actionable message. `op` ∈ #{:replace :insert-before :insert-after :append}.
   `:append` ignores `target`; the others locate the definition by `:target`
   (and optional `:kind`). The result is re-parsed and rejected on syntax error."
  [path source {:keys [op target kind code]}]
  (when-not (string? code)
    (throw (ex-info "structural edit needs :code (a string)."
                    {:type :ext.foundation.editing/struct-no-code})))
  (let [language (or (index/detect-language path)
                     (throw (ex-info (str "Unknown language for " path " — use patch(...) instead.")
                                     {:type :ext.foundation.editing/struct-unknown-language :path path})))
        lines    (vec (str/split-lines source))
        items    (flatten-items (structure source language))
        {:keys [start end]} (if (= op :append) {:start 0 :end 0} (find-target items target kind))
        result   (str/join "\n" (splice lines op start end code))
        bad      (error-diagnostics result language)]
    (when (seq bad)
      (throw (ex-info (str "Edit rejected: it introduces a syntax error in " path
                           " (" (count bad) " parse error(s)). The file was not changed.")
                      {:type :ext.foundation.editing/struct-syntax-error :path path})))
    result))
