(ns com.blockether.vis.contract.annotations
  "Markdown comments shared by the Companion and TUI. Resolved history remains body text."
  (:require [clojure.string :as str]))

(defn- one-line
  [text]
  (-> (str text)
      (str/replace #"[“”]" "\"")
      (str/replace #"\s+" " ")
      str/trim))

(defn quote-of
  "Collapse a passage to at most 160 UTF-16 units, retaining both ends."
  [selection]
  (let [text (one-line selection)]
    (if (<= (count text) 160)
      text
      (str (str/trim (subs text 0 79)) "…" (str/trim (subs text (- (count text) 79)))))))

(defn parse-annotated
  "Split a recognized final Comments section; leave unrecognized sections untouched."
  [text]
  (let [at
        (.lastIndexOf ^String text "\n## Comments\n")

        comments
        (when (not (neg? at))
          (into []
                (keep (fn [line]
                        (let [line (str/trim line)]
                          (if-let [[_ body] (re-matches #"- \*\*Whole document\*\* — (.*)" line)]
                            {:quote "" :body body}
                            (when-let [[_ quote body] (re-matches #"- \*\*“(.*)”\*\* — (.*)" line)]
                              {:quote quote :body body})))))
                (str/split-lines (subs text (+ at 13)))))]

    (if (seq comments)
      {:body (str/replace (subs text 0 at) #"\s+$" "") :comments comments}
      {:body text :comments []})))

(defn render-annotated
  "Save comments in the existing portable format, without changing resolved history."
  [body comments]
  (let [prose
        (str/replace body #"\s+$" "")

        kept
        (filter #(not (str/blank? (one-line (:body %)))) comments)]

    (str prose
         (when (seq kept)
           (str "\n\n## Comments\n\n"
                (str/join "\n"
                          (map (fn [{:keys [quote body]}]
                                 (str "- **" (if (str/blank? (one-line quote))
                                               "Whole document"
                                               (str "“" (one-line quote) "”"))
                                      "** — " (one-line body)))
                               kept))))
         "\n")))
