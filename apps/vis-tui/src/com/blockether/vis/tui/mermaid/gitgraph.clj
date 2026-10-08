(ns com.blockether.vis.tui.mermaid.gitgraph
  "Mermaid git graphs as a vertical commit log, like `git log --graph`.

   Each branch is a lane of its own palette colour, two columns wide. Commits go
   down the rows in the order of the source (up for `BT`); a branch leaves its
   parent commit on a corner and a merge joins the lane of the target branch.
   The labels on the right give the branch name at its first commit, the commit
   id, the tags and the merged or picked commit."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.mermaid.canvas :as c]))

(defn- attrs
  "Map of the `key: value` attributes of a git graph statement."
  [text]
  (into {}
        (map (fn [[_ k quoted bare]]
               [(keyword k) (or quoted bare)]))
        (re-seq #"(id|tag|type|msg|parent|order):\s*(?:\"([^\"]*)\"|(\S+))" text)))

(defn- unquote-name [text] (str/replace (str text) #"^\"|\"$" ""))

(defn- setting
  "Value of the gitGraph setting `key` in the front matter or init directives."
  [settings key]
  (some->> settings
           (re-find (re-pattern (str "['\"]?" key "['\"]?\\s*:\\s*['\"]?([^'\",}\\s]+)")))
           second))

(defn- parse
  "`{:commits :branches}` of the git graph statements in `lines`."
  [lines main-branch]
  (loop [lines
         (map str/trim lines)

         state
         {:commits []
          :branches [{:name main-branch :order 0}]
          :heads {main-branch nil}
          :current main-branch}]

    (if-let [line (first lines)]
      (let [{:keys [commits heads current]} state
            index (count commits)
            add-commit (fn [commit]
                         (-> state
                             (update :commits
                                     conj
                                     (assoc commit
                                       :index index
                                       :branch current))
                             (assoc-in [:heads current] index)))
            by-id (fn [id]
                    (some #(when (= id (:id %)) (:index %)) commits))]

        (recur
          (rest lines)
          (condp re-find line
            #"^commit\b" (let [{:keys [id tag type msg]} (attrs line)]
                           (add-commit {:id id
                                        :tags (some-> tag
                                                      vector)
                                        :type (some-> type
                                                      str/upper-case)
                                        :msg msg
                                        :parents (vec (remove nil? [(get heads current)]))}))
            #"^branch\s" (let [[_ raw] (re-find #"^branch\s+(\"[^\"]+\"|\S+)" line)
                               name (unquote-name raw)
                               order (some-> (:order (attrs line))
                                             parse-long)]

                           (-> state
                               (update :branches conj {:name name :order order})
                               (assoc-in [:heads name] (get heads current))
                               (assoc :current name)))
            #"^(checkout|switch)\s"
            (let [name (unquote-name (second (re-find #"^\S+\s+(\"[^\"]+\"|\S+)" line)))]
              (when-not (contains? heads name) (throw (ex-info (str "no branch " name) {})))
              (assoc state :current name))
            #"^merge\s"
            (let [[_ raw] (re-find #"^merge\s+(\"[^\"]+\"|\S+)" line)
                  from (unquote-name raw)
                  {:keys [id tag type]} (attrs line)]

              (when-not (contains? heads from) (throw (ex-info (str "no branch " from) {})))
              (add-commit {:id id
                           :tags (some-> tag
                                         vector)
                           :type (some-> type
                                         str/upper-case)
                           :kind :merge
                           :from from
                           :parents (vec (distinct (remove nil?
                                                     [(get heads current) (get heads from)])))}))
            #"^cherry-pick\b" (let [{:keys [id tag]} (attrs line)]
                                (when-not (by-id id) (throw (ex-info (str "no commit " id) {})))
                                (add-commit {:kind :pick
                                             :picked id
                                             :tags (some-> tag
                                                           vector)
                                             :parents (vec (remove nil? [(get heads current)]))}))
            state)))
      state)))

(defn- lane-order
  "Branch name -> lane index: the main branch first, then by `order:` and creation."
  [branches main-order]
  (->> branches
       (map-indexed (fn [created branch]
                      [(:name branch)
                       (cond (zero? (long created)) [(double (or main-order 0)) 0]
                             (:order branch) [(double (:order branch)) created]
                             :else [1.0E9 created])]))
       (sort-by second)
       (map-indexed (fn [lane [name _]]
                      [name lane]))
       (into {})))

(defn- vline!
  [canvas col from to tone]
  (let [top
        (min (long from) (long to))

        bottom
        (max (long from) (long to))]

    (when (< top bottom)
      (doseq [row (range top (inc bottom))]
        (c/put-line! canvas
                     row
                     col
                     (bit-or (if (< (long row) bottom) c/down-bit 0)
                             (if (> (long row) top) c/up-bit 0))
                     :solid
                     tone)))))

(defn- hline!
  [canvas row from to tone]
  (let [left
        (min (long from) (long to))

        right
        (max (long from) (long to))]

    (when (< left right)
      (doseq [col (range left (inc right))]
        (c/put-line! canvas
                     row
                     col
                     (bit-or (if (< (long col) right) c/right-bit 0)
                             (if (> (long col) left) c/left-bit 0))
                     :solid
                     tone)))))

(defn- glyph
  [{:keys [kind type]}]
  (cond (= :merge kind) \◉
        (= :pick kind) \⊙
        (= "HIGHLIGHT" type) \■
        (= "REVERSE" type) \✕
        :else \●))

(defn- label
  "Coloured segments to the right of `commit`."
  [{:keys [id msg tags kind from picked branch] :as commit} first-of-branch? show-branches? tone]
  (cond-> []
    (and show-branches? first-of-branch?)
    (conj [branch tone] [" " nil])

    (= :merge kind)
    (conj [(str "merge " from) :chrome] [" " nil])

    (= :pick kind)
    (conj [(str "cherry-pick " picked) :chrome] [" " nil])

    id
    (conj [id (if (= "REVERSE" (:type commit)) :red :text)] [" " nil])

    (and msg (not id))
    (conj [msg :text] [" " nil])

    (seq tags)
    (into (mapcat (fn [tag]
                    [[(str "[" tag "]") :yellow] [" " nil]])
                  tags))))

(defn git-graph
  "Rows of a Mermaid git graph inside `width` columns."
  [{:keys [args lines settings]} width]
  (let [main
        (or (setting settings "mainBranchName") "main")

        show-branches?
        (not= "false" (setting settings "showBranches"))

        up?
        (re-find #"(?i)\bBT\b" (str args))

        {:keys [commits branches]}
        (parse lines main)

        lanes
        (lane-order branches
                    (some-> (setting settings "mainBranchOrder")
                            parse-double))

        lane-tone
        (fn [name]
          (c/series-tone (get lanes name 0)))

        label-col
        (inc (* 2 (count lanes)))

        _
        (when (empty? commits) (throw (ex-info "no commits" {})))

        _
        (when (> (+ label-col 4) (long width)) (throw (ex-info "too many branches" {})))

        last-row
        (* 2 (dec (count commits)))

        row-of
        (fn [index]
          (let [row (* 2 (long index))]
            (if up? (- last-row row) row)))

        col-of
        (fn [commit]
          (* 2 (long (get lanes (:branch commit) 0))))

        canvas
        (c/make-canvas (inc last-row) width)

        first-commits
        (->> commits
             (group-by :branch)
             vals
             (map (comp :index first))
             set)]

    (doseq [commit
            commits

            [at parent-index]
            (map-indexed vector (:parents commit))

            :let [parent
                  (nth commits parent-index)

                  row
                  (row-of (:index commit))

                  col
                  (col-of commit)

                  parent-row
                  (row-of parent-index)

                  parent-col
                  (col-of parent)]]

      (cond (= col parent-col) (vline! canvas col parent-row row (lane-tone (:branch commit)))
            (zero? (long at))
            (do (hline! canvas parent-row parent-col col (lane-tone (:branch commit)))
                (vline! canvas col parent-row row (lane-tone (:branch commit))))
            :else (let [tone (lane-tone (:branch parent))]
                    (vline! canvas parent-col parent-row row tone)
                    (hline! canvas row parent-col col tone))))
    (doseq [commit
            commits

            :let [row
                  (row-of (:index commit))

                  tone
                  (lane-tone (:branch commit))]]

      (c/put-char! canvas
                   row
                   (col-of commit)
                   (glyph commit)
                   (if (= "REVERSE" (:type commit)) :red tone))
      (loop [col
             label-col

             segments
             (c/seg-clip
               (label commit (contains? first-commits (:index commit)) show-branches? tone)
               (- (long width) label-col))]

        (when-let [[text segment-tone] (first segments)]
          (c/put-text! canvas row col text segment-tone)
          (recur (+ (long col) (long (c/width text))) (rest segments)))))
    {:rows (c/canvas->rows canvas)}))
