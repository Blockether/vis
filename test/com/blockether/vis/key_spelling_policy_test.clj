(ns com.blockether.vis.key-spelling-policy-test
  "Regression #291: each data kind is read under ONE key spelling. Maps convert
   once at their seam (`wire/->engine`, `wire/->wire`), so production code never
   falls back from one spelling of a key to another."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(def ^:private source-roots ["src" "apps/vis-tui/src" "packages/vis-contract/src"])

(defn- clojure-sources
  []
  (->> source-roots
       (map io/file)
       (filter #(.isDirectory ^java.io.File %))
       (mapcat file-seq)
       (filter #(and (.isFile ^java.io.File %) (re-find #"\.clj[cs]?$" (.getName ^java.io.File %))))
       (sort-by #(.getPath ^java.io.File %))))

(def ^:private keyword-read "`(:key subject)`." #"\((:[A-Za-z_][\w-]*)\s+([^\s()\[\]{}\"]+)")

(def ^:private get-read
  "`(get subject :key)` or `(get subject \"key\")`."
  #"\((?:get|contains\?|find)\s+([^\s()\[\]{}\"]+)\s+(:[A-Za-z_][\w-]*|\"[A-Za-z_][\w-]*\")")

(defn- key-name
  "One name for every spelling of a key: `:repo-root`, `:repo_root` and `\"repo_root\"`."
  [spelling]
  (-> spelling
      (str/replace #"^:|\"" "")
      (str/replace "-" "_")
      str/lower-case))

(defn- mixed-spellings
  "1-based lines where one subject is read under two spellings of one key within
   `window` lines."
  [lines window]
  (->> (for [[index line]
             (map-indexed vector lines)

             :let [code
                   (str/replace line #";.*$" "")]
             [spelling subject]
             (concat (map (fn [[_ k s]]
                            [k s])
                          (re-seq keyword-read code))
                     (map (fn [[_ s k]]
                            [k s])
                          (re-seq get-read code)))]

         {:index index :subject subject :key (key-name spelling) :spelling spelling})
       (group-by (juxt :subject :key))
       vals
       (mapcat (fn [reads]
                 (let [reads (sort-by :index reads)]
                   (for [[a b] (map vector reads (rest reads))
                         :when (and (< (- (long (:index b)) (long (:index a))) (long window))
                                    (not= (:spelling a) (:spelling b)))]

                     (inc (long (:index a)))))))
       distinct
       sort
       vec))

(deftest mixed-spelling-detector-test
  (testing "flags a fallback between spellings of one key"
    (is (= [1] (mixed-spellings ["(or (:repo-root ws)" "    (get ws \"repo_root\"))"] 3)))
    (is (= [1] (mixed-spellings ["(or (:tool_call_id m) (:tool-call-id m))"] 3))))
  (testing "ignores distinct keys, distinct subjects and distant reads"
    (is (= [] (mixed-spellings ["(or (get ws \"repo_root\") (get ws \"root\"))"] 3)))
    (is (= [] (mixed-spellings ["(or (:root a) (get b \"root\"))"] 3)))
    (is (= [] (mixed-spellings ["(:root ws)" "" "" "(get ws \"root\")"] 3)))))

(deftest one-key-spelling-per-read-test
  (testing "production Clojure reads each key of a subject under one spelling"
    (is (every? #(.isDirectory (io/file ^String %)) source-roots))
    (let [offenders (->> (clojure-sources)
                         (mapcat (fn [^java.io.File file]
                                   (map #(str (.getPath file) ":" %)
                                        (mixed-spellings (str/split-lines (slurp file)) 3))))
                         vec)]
      (is (= [] offenders)
          (str "Convert the map once at its seam (wire/->engine or wire/->wire) and read one "
               "spelling. Mixed spellings found at: "
               (str/join ", " offenders))))))
