(ns com.blockether.vis.contract.activity
  "Activity vocabulary, declarations and lossless history pages. Lifecycle stays in the engine."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire])
  (:import [java.nio.charset StandardCharsets]))

(def vocabulary (document/load! "activity"))

(def presenters (set (map keyword (get vocabulary "presenters"))))

(def limits (get vocabulary "limits"))

(defn valid-presentation?
  "Admit explicit headline/summary/content and non-nested sections, within one shared budget."
  [value]
  (let [value (wire/->wire value)]
    (and (document/valid-json? "activity" "presentation" value)
         (let [sections (cons value (get value "sections"))
               blocks (mapcat #(get % "content") sections)
               bytes #(alength (.getBytes ^String % StandardCharsets/UTF_8))]

           (and
             (<= (long (bytes (wire/json-str value))) 32768)
             (<= (count blocks) 32)
             (every? #(<= (long (bytes %)) 512) (mapcat #(map % ["headline" "summary"]) sections))
             (every? (fn [block]
                       (case (get block "type")
                         "progress"
                         (or (nil? (get block "value"))
                             (<= (double (get block "value")) (double (get block "total"))))

                         "table"
                         (every? #(= (count %) (count (get block "columns"))) (get block "rows"))

                         true))
                     blocks))))))

(defn valid-projection?
  "Admit lossless projections; history-bearing transport pages have bounded rows and bytes."
  [value]
  (and (document/valid? "activity" "projection" value)
       (let [value
             (wire/->wire value)

             children
             #(get % "children")

             rows
             (mapcat #(tree-seq (comp seq children) children %) (get value "rows"))

             ids
             (map #(get % "id") rows)]

         (and (= (count ids) (count (set ids)))
              (every? #(or (not (contains? % "presentation"))
                           (valid-presentation? (get % "presentation")))
                      rows)
              (or (nil? (get value "history"))
                  (and (<= (count (filter #(empty? (children %)) rows))
                           (long (get limits "max_page_rows")))
                       (<= (alength (.getBytes ^String (wire/json-str value)
                                               StandardCharsets/UTF_8))
                           (long (get limits "max_page_bytes")))))))))

(defn from-wire
  "Valid projection in engine spelling, or nil. Never accepts retired owner/view keys."
  [value]
  (when (and (document/valid-json? "activity" "projection" value) (valid-projection? value))
    (wire/->engine value)))

(defn- first-invocation-id
  [row]
  (:id (if (= "shell" (:operation row)) (or (first (:children row)) row) row)))

(defn argument-groups
  "Group exact operation and argument-key pairs within one block. Missing keys stay
   separate; displayed summaries never prove equality. Preserve all invocation evidence."
  [rows]
  (let [ordered
        (sort-by :sequence rows)

        group-key
        (fn [row]
          (if-let [key (:argument-key row)]
            [:arguments (:operation row) key]
            [:row (:id row)]))

        grouped
        (group-by group-key ordered)]

    (mapv (fn [key]
            (let [members (get grouped key)]
              {:id (first-invocation-id (first members)) :rows members}))
          (distinct (map group-key ordered)))))

(defn operation-groups
  "One group per exact operation across the block, ordered by first entry.
   Known operations use canonical labels; otherwise use the first nonblank presentation
   headline in invocation order, falling back to the operation name when none is present.
   Preserve member order and shell evidence, with the first invocation as disclosure identity."
  [rows]
  (let [ordered
        (sort-by :sequence rows)

        grouped
        (group-by :operation ordered)]

    (mapv (fn [operation]
            (let [members
                  (get grouped operation)

                  row
                  (first members)]

              {:id (first-invocation-id row)
               :label (or (get-in vocabulary ["operation_groups" operation])
                          (first (remove str/blank? (map (comp :headline :presentation) members)))
                          operation)
               :rows members}))
          (distinct (map :operation ordered)))))

(defn- content-copy-text
  [{:keys [type text columns rows label value total attachment-id]}]
  (case (name type)
    "table"
    (str/join "\n" (map #(str/join "\t" %) (cons columns rows)))

    "progress"
    (str label (when (some? value) (str ": " value "/" total)))

    (or text (str (name type) ": " label " (" attachment-id ")"))))

(defn- row-copy-text
  [{:keys [operation state duration-ms summary result-summary error-summary resources evidence
           presentation is-truncated children]} depth]
  (let [indent
        (apply str (repeat depth "  "))

        detail
        (fn [text]
          (when-not (str/blank? text) (map #(str indent "  " %) (str/split text #"\n" -1))))

        evidence-text
        (fn [{:keys [kind text lines is-truncated is-redacted]}]
          (str/join "\n"
                    (concat [(str (name kind) ":\n" text)]
                            (when (= "diff" (name kind))
                              (concat (map (fn [{:keys [kind text]}]
                                             (str (case (name kind)
                                                    "addition"
                                                    "+"

                                                    "deletion"
                                                    "-"

                                                    "context"
                                                    " "

                                                    "")
                                                  text))
                                           lines)
                                      (when is-truncated ["Diff truncated"])
                                      (when is-redacted ["Diff redacted"]))))))

        sections
        (when presentation (cons presentation (:sections presentation)))

        details
        (concat [summary (when result-summary (str "Result: " result-summary))
                 (when error-summary (str "Error: " error-summary))]
                (map #(str (name (:type %)) ": " (:id %)) resources)
                (map evidence-text evidence)
                (mapcat (fn [{:keys [headline summary content]}]
                          (concat [headline summary] (map content-copy-text content)))
                        sections)
                (when is-truncated ["Details truncated"]))]

    (str/join "\n"
              (concat [(str indent
                            operation
                            " [" (name state)
                            "]" (when (some? duration-ms) (str " (" duration-ms "ms)")))]
                      (mapcat detail details)
                      (mapcat #(vector "" (row-copy-text % (inc depth)))
                              (sort-by :sequence children))))))

(defn copy-text
  "Copy retained invocations from an engine-spelled projection, independent of grouping,
   disclosure and viewport. Include bounded evidence and omissions, never identity keys."
  [{:keys [rows omitted]}]
  (str/join "\n\n"
            (concat ["ACTIVITY"]
                    (map #(row-copy-text % 0) (sort-by :sequence rows))
                    (when (pos? (long (or (:rows omitted) 0)))
                      [(str (:rows omitted)
                            " step"
                            (when (not= 1 (:rows omitted)) "s")
                            " omitted · Activity limit")]))))
