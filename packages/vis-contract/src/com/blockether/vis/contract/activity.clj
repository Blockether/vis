(ns com.blockether.vis.contract.activity
  "Activity vocabulary, declaration and bounded projection admission. Lifecycle stays in the engine."
  (:require [com.blockether.vis.contract.document :as document]
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
  "Admit the closed schema plus globally unique row ids and the canonical UTF-8 byte bound."
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
              (<= (alength (.getBytes ^String (wire/json-str value) StandardCharsets/UTF_8))
                  (long (get limits "max_receipt_bytes")))))))

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
   Known operations use canonical labels; extensions retain their operation names.
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
               :label (get-in vocabulary ["operation_groups" operation] operation)
               :rows members}))
          (distinct (map :operation ordered)))))
