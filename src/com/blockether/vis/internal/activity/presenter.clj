(ns com.blockether.vis.internal.activity.presenter
  "Closed semantic presenter registry for Activity rows. Presenters return data,
   never channel markup, and never inspect Python source."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.activity :as contract]))

(def presenters contract/presenters)

(defn presenter-for
  "The explicitly declared presenter, or the bounded generic fallback."
  [_operation declared]
  (if (contains? presenters declared) declared :generic))

(defn classification
  "The symbol entry's explicit tag, or generic when no declaration exists."
  [event]
  (or (:classification event) :generic))

(defn row-summary
  "Bounded already-redacted summary selected from semantic event fields. A shell
   spawn is durable command evidence, not a live-tense status: its ticker phrase
   `running: <command>` becomes `cmd: <command>` in Activity."
  [{:keys [presenter phrase] :as event}]
  (or (when (and (= :shell presenter) (string? phrase) (str/starts-with? phrase "running: "))
        (str "cmd: " (subs phrase (count "running: "))))
      phrase
      (:label event)
      (:result-summary event)
      (:error-summary event)
      (some-> (:operation event)
              name)))

(defn- field [m k] (when (map? m) (get m k (get m (keyword k)))))

(defn- label
  [value]
  (-> (if (keyword? value) (name value) (str value))
      (str/replace #"[_-]" " ")
      str/capitalize))

(defn- scalar
  [value]
  (cond (nil? value) "None"
        (keyword? value) (name value)
        :else (str value)))

(defn- text-block
  [key text]
  {"type" (cond (contains? #{"content" "body" "description" "documentation" "message"} key)
                "markdown"
                (contains? #{"out" "err" "stdout" "stderr" "output" "code" "source"} key) "code"
                :else "text")
   "text" text})

(defn- result-blocks
  "Render bounded public data with contextual scalar-table headings. Nested maps
   use generic detail headings rather than inheriting top-level metric labels."
  [value table-label]
  (cond (map? value) (let [entries
                           (sort-by (comp str key) (dissoc value :op "op"))

                           short?
                           (fn [[k v]]
                             (and (not (contains? #{"content" "body" "description" "documentation"
                                                    "message" "out" "err" "stdout" "stderr" "output"
                                                    "code" "source"}
                                                  (scalar k)))
                                  (not (coll? v))
                                  (<= (count (scalar v)) 256)
                                  (not (str/includes? (scalar v) "\n"))))

                           fields
                           (filter short? entries)

                           bodies
                           (remove short? entries)]

                       (into (if (seq fields)
                               [{"type" "table"
                                 "columns" [table-label "Result"]
                                 "rows" (mapv (fn [[k v]]
                                                [(label k) (scalar v)])
                                              fields)}]
                               [])
                             (mapcat (fn [[k v]]
                                       (cons {"type" "heading" "text" (label k)}
                                             (if (string? v)
                                               [(text-block (scalar k) v)]
                                               (result-blocks v "Detail"))))
                                     bodies)))
        (sequential? value) (if (every? #(not (coll? %)) value)
                              [{"type" "text" "text" (str/join "\n" (map scalar value))}]
                              (vec (mapcat #(result-blocks % table-label) value)))
        (string? value) [{"type" "text" "text" value}]
        :else [{"type" "text" "text" (scalar value)}]))

(defn- code-language
  [path]
  (get {"clj" "clojure"
        "cljc" "clojure"
        "cljs" "clojure"
        "edn" "clojure"
        "py" "python"
        "ts" "typescript"
        "tsx" "tsx"
        "js" "javascript"
        "jsx" "jsx"
        "json" "json"
        "yaml" "yaml"
        "yml" "yaml"
        "toml" "toml"
        "sh" "bash"
        "md" "markdown"
        "css" "css"
        "html" "html"
        "rs" "rust"
        "java" "java"}
       (last (str/split (str path) #"\."))
       "text"))

(defn- read-content [text] (str/replace text #"(?m)^(\d+):[0-9a-f]+│ ?" "$1 │ "))

(defn result-presentation
  "Default result view for an invocation without an authored presentation.
   Input is public, redacted and node-bounded by the event owner. The owner also
   bounds and validates the returned blocks before retention. Never reads files."
  [{:keys [operation label]} value]
  (let [op
        (name operation)

        text
        (when (string? value) value)

        path
        (or label "")

        read-lines
        (when (and (= op "cat") text) (map second (re-seq #"(?m)^(\d+):[0-9a-f]+│" text)))

        headline
        (get {"cat" "Read"
              "patch" "Patched"
              "grep" "Searched"
              "doc" "Read documentation"
              "apropos" "Found symbols"
              "defs" "Read definitions"
              "run_tests" "Ran tests"
              "lint_code" "Linted"
              "format_code" "Formatted"
              "repl_eval" "Evaluated"
              "shell" "Started command"
              "council.publish" "Published message"
              "council.read" "Read thread"
              "council.get" "Read message"
              "council.threads" "Listed threads"
              "council.members" "Listed members"}
             op
             (str/capitalize (str/trim (str/replace op #"[_.-]" " "))))

        summary
        (cond (= op "cat") (str path
                                (when (seq read-lines)
                                  (str " · lines " (first read-lines) "–" (last read-lines))))
              (contains? #{"doc" "defs" "patch" "shell"} op) path
              (= op "grep") (or (first (str/split-lines (or text ""))) "")
              (and (= op "run_tests") (number? (field value "total")))
              (str (or (field value "total") 0) " tests · " (or (field value "fail") 0) " failed")
              :else (str (or (field value "summary") (field value "title") "")))

        content
        (cond (= op "patch") []
              (and (= op "cat") text)
              [{"type" "code" "language" (code-language path) "text" (read-content text)}]
              (and (= op "doc") text) [{"type" "markdown" "text" text}]
              (and (= op "defs") text) [{"type" "code" "language" "python" "text" text}]
              (and (= op "grep") text)
              [{"type" "code" "text" (str/replace text #"(?m)^(\s*\d+):[0-9a-f]+│ ?" "$1 │ ")}]
              :else (result-blocks value
                                   (case op
                                     ("run_tests" "lint_code")
                                     "Metric"

                                     ("council.publish" "council.get")
                                     "Message"

                                     ("council.read" "council.threads")
                                     "Thread"

                                     "council.members"
                                     "Member"

                                     "Detail")))]

    {"headline" headline "summary" summary "content" content}))
