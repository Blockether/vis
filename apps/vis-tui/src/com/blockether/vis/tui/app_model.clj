(ns com.blockether.vis.tui.app-model
  "Pure model of an app attachment in the terminal: diff lines with stable anchors,
   threads under their anchor line, receipts, and the Save and Send rules.

   The shared operation rules are in `com.blockether.vis.contract.app`. This
   namespace only reads a materialized view and builds gateway actions. It never
   paints and never calls the gateway."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.tui.primitives :as p])
  (:import [java.io ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]
           [java.util UUID]))

(set! *warn-on-reflection* true)

(def media-type "Media type of an app attachment row." app/media-type)

(defn app-row? "True when an attachment row lists an app." [row] (= media-type (:media-type row)))

(defn row-view-id
  "The app view id of an attachment row, or nil."
  [row]
  (let [id (:view-id row)]
    (when-not (str/blank? (str id)) (str id))))

(defn status
  "The lifecycle state an app view shows: `:closed`, `:archived`, `:stale` or `:live`."
  [view]
  (cond (:is-closed view) :closed
        (:is-archived view) :archived
        (:is-stale view) :stale
        :else :live))

(def status-label
  "The badge text of each lifecycle state."
  {:live "Live" :stale "Disconnected" :closed "Closed" :archived "Archived"})

(defn read-only?
  "True when the app takes no more actions: it is closed or archived."
  [view]
  (contains? #{:closed :archived} (status view)))

(defn stale?
  "True when the app waits for its owner: its controls and keys reach nobody."
  [view]
  (= :stale (status view)))

;;; ── Diff lines ─────────────────────────────────────────────────────────────

(defn- unquote-git-path
  [path]
  (if (str/starts-with? path "\"")
    (let [out (ByteArrayOutputStream.)]
      (doseq [token (re-seq #"\\[0-7]{1,3}|\\.|[^\\]+" (subs path 1 (dec (count path))))]
        (if (re-matches #"\\[0-7]{1,3}" token)
          (.write out (Integer/parseInt (subs token 1) 8))
          (.write out
                  ^bytes
                  (.getBytes ^String
                             (get {"\\n" "\n"
                                   "\\t" "\t"
                                   "\\r" "\r"
                                   "\\b" "\b"
                                   "\\f" "\f"
                                   "\\v" "\u000b"
                                   "\\\\" "\\"
                                   "\\\"" "\""}
                                  token
                                  token)
                             StandardCharsets/UTF_8))))
      (.toString out "UTF-8"))
    path))

(defn- strip-prefix
  [path]
  (let [path (unquote-git-path (first (str/split (str path) #"\t")))]
    (cond (= "/dev/null" path) nil
          (re-find #"^[ab]/" path) (subs path 2)
          :else path)))

(defn- patch-code-kind
  [line]
  (case (first line)
    \+
    :add

    \-
    :del

    \space
    :ctx

    nil
    (when (= "" line) :ctx)

    nil))

(defn- patch-line-kind
  [path line]
  (cond (str/starts-with? line "--- ") :old-path
        (str/starts-with? line "+++ ") :new-path
        (str/starts-with? line "@@") :hunk
        path (patch-code-kind line)
        :else nil))

(defn- patch-hunk
  [at rows line]
  (let [[_ o n]
        (re-find #"^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@" line)

        old
        (or (some-> o
                    parse-long)
            (:old at))

        new
        (or (some-> n
                    parse-long)
            (:new at))]

    [(assoc at
       :old old
       :new new) (conj rows {:kind :hunk :text line :path (:path at) :old-start old})]))

(defn- patch-code-line
  [{:keys [path old new] :as at} rows line kind]
  (let [text
        (if (= "" line) "" (subs line 1))

        base
        {:kind kind :text text :path path}]

    (case kind
      :add
      [(update at :new inc)
       (conj rows
             (assoc base
               :new new
               :anchor {:path path :side "new" :line new}))]

      :del
      [(update at :old inc)
       (conj rows
             (assoc base
               :old old
               :anchor {:path path :side "old" :line old}))]

      :ctx
      [(-> at
           (update :old inc)
           (update :new inc))
       (conj rows
             (assoc base
               :old old
               :new new
               :anchor {:path path :side "new" :line new}))])))

(defn- patch-line
  [[at rows] line]
  (let [kind (patch-line-kind (:path at) line)]
    (case kind
      :old-path
      [(assoc at :old-path (strip-prefix (subs line 4))) (conj rows {:kind :meta :text line})]

      :new-path
      (let [new-path (strip-prefix (subs line 4))
            old-path (:old-path at)
            file (or new-path old-path "file")]

        [(assoc at :path file)
         (conj rows
               {:kind :meta :text line}
               {:kind :file
                :text file
                :path file
                :old-path old-path
                :status (cond (nil? old-path) :added
                              (nil? new-path) :deleted
                              (not= old-path new-path) :renamed
                              :else :modified)})])

      :hunk
      (patch-hunk at rows line)

      (:add :del :ctx)
      (patch-code-line at rows line kind)

      [at (conj rows {:kind :meta :text line})])))

(defn- patch-header?
  [row]
  (and (= :meta (:kind row))
       (or (str/blank? (:text row)) (re-find #"^(--- |\+\+\+ |diff --git |index )" (:text row)))))

(defn- text-patch-rows
  [lines]
  (let [[_ rows] (reduce patch-line [{:old-path nil :path nil :old 0 :new 0} []] lines)]
    (vec (remove patch-header? rows))))

(defn- git-file-status
  [lines old-path path]
  (cond (some #(str/starts-with? % "new file mode ") lines) :added
        (some #(str/starts-with? % "deleted file mode ") lines) :deleted
        (not= old-path path) :renamed
        :else :modified))

(defn- git-header-paths
  [line]
  (let [raw
        (subs line (count "diff --git "))

        [_ old-path path]
        (or (re-find #"^(\"(?:\\.|[^\"])*\") (.*)$" raw)
            (re-find #"^(.*?) (\"(?:\\.|[^\"])*\")$" raw)
            (re-find #"^(a/.*?) (b/.*)$" raw))]

    [(strip-prefix old-path) (strip-prefix path)]))

(defn- git-file-rows
  [chunk]
  (let [lines
        (str/split-lines chunk)

        [old-path path]
        (git-header-paths (first lines))

        rows
        (text-patch-rows (rest lines))

        header
        {:kind :file
         :text path
         :path path
         :old-path old-path
         :status (git-file-status lines old-path path)}]

    (into [header] (remove #(#{:file :meta} (:kind %))) rows)))

(defn parse-patch
  "Parse unified diff text into file, hunk, and anchored code rows. Keep binary
   and rename-only file headers even when the patch has no text hunk."
  [patch]
  (let [text
        (str patch)

        chunks
        (re-seq #"(?ms)^diff --git .*?(?=^diff --git |\z)" text)]

    (if (seq chunks)
      (into [] (mapcat git-file-rows) chunks)
      (text-patch-rows (str/split-lines text)))))

(defn anchor-key
  "One comparable key for an anchor map."
  [{:keys [path side line]}]
  [(str path) (str side) (long (or line 0))])

(defn files
  "The file paths of a diff patch, in order."
  [patch]
  (into [] (comp (filter #(= :file (:kind %))) (map :path)) (parse-patch patch)))

(defn change-counts
  "How many lines `patch` adds and removes: `{:add n :del n}`."
  [patch]
  (let [kinds (frequencies (map :kind (parse-patch patch)))]
    {:add (get kinds :add 0) :del (get kinds :del 0)}))

;;; ── Threads ────────────────────────────────────────────────────────────────

(defn author-label
  "The name a message shows. The host sets the author; the client never does."
  [{:keys [author]}]
  (or (not-empty (str (:name author)))
      (case (some-> (:kind author)
                    name)
        "human"
        "You"

        "agent"
        "Agent"

        "extension"
        "Extension"

        "Unknown")))

(defn- wrap-text
  [text width]
  (let [width (max 8 (long width))]
    (into []
          (mapcat (fn [line]
                    (if (str/blank? line)
                      [""]
                      (loop [left line
                             out []]

                        (if (<= (count left) width)
                          (conj out left)
                          (let [cut (or (some-> (str/last-index-of (subs left 0 width) " ")
                                                long
                                                (as-> i (when (pos? (long i)) i)))
                                        width)]
                            (recur (str/triml (subs left cut)) (conj out (subs left 0 cut)))))))))
          (str/split (str text) #"\n" -1))))

(def ^:private thread-rail "    │ ")

(def ^:private message-indent
  "Columns before the text of a thread message: the rail and two spaces."
  (+ (count thread-rail) 2))

(defn thread-rows
  "Rows of one thread, `width` columns wide: each message as an author line and
   its wrapped body, then the saved draft. `base` is merged into every row."
  [thread width base]
  (let [messages (:messages thread)]
    (cond-> []
      (and (empty? messages) (not (:draft thread)))
      (conj (merge base {:kind :thread-empty :text "No messages yet"}))

      :always
      (into (mapcat (fn [message]
                      (into [(merge base
                                    {:kind :message-head
                                     :text (author-label message)
                                     :author-kind (some-> (get-in message [:author :kind])
                                                          name)
                                     :message-id (:id message)})]
                            (map #(merge base {:kind :message :text %}))
                            (wrap-text (:body message) (- (long width) (long message-indent))))))
            messages)

      (:draft thread)
      (into (cons (merge base {:kind :draft-head :text "Draft (saved, not sent)"})
                  (map #(merge base {:kind :draft :text %})
                       (wrap-text (:draft thread) (- (long width) (long message-indent))))))

      (:is-resolved thread)
      (conj (merge base {:kind :thread-empty :text "Resolved"})))))

(defn- word-char? [c] (or (Character/isLetterOrDigit (char c)) (= \_ c)))

(defn- common-edges
  [^String a ^String b]
  (let [na
        (count a)

        nb
        (count b)

        pre
        (loop [i 0]
          (if (and (< i na) (< i nb) (= (.charAt a i) (.charAt b i))) (recur (inc i)) i))

        suf
        (loop [i 0]
          (if (and (< i (- na pre))
                   (< i (- nb pre))
                   (= (.charAt a (- na 1 i)) (.charAt b (- nb 1 i))))
            (recur (inc i))
            i))]

    [pre suf]))

(defn- widen-word
  [^String s ^long from ^long to]
  (let [from
        (loop [i from]
          (if (and (pos? i) (word-char? (.charAt s (dec i)))) (recur (dec i)) i))

        to
        (loop [i to]
          (if (and (< i (count s))
                   (pos? i)
                   (word-char? (.charAt s (dec i)))
                   (word-char? (.charAt s i)))
            (recur (inc i))
            i))]

    [from to]))

(defn word-span
  "The changed `[from to]` of `a` and `b`, widened to whole words. Nil for equal
   lines or a change that covers a whole line, where the row tint is enough."
  [^String a ^String b]
  (let [[pre suf]
        (common-edges a b)

        [af at]
        (widen-word a pre (- (count a) (long suf)))

        [bf bt]
        (widen-word b pre (- (count b) (long suf)))]

    (when-not (or (= a b)
                  (and (zero? (long af)) (= (count a) at))
                  (and (zero? (long bf)) (= (count b) bt)))
      [[af at] [bf bt]])))

(defn pair-words
  "Code `rows` with a `:mark` `[from to]` on each removed line and its paired added
   line: the n-th removed line of a change block pairs with its n-th added line."
  [rows]
  (let [rows
        (vec rows)

        n
        (count rows)]

    (loop [i
           0

           out
           rows]

      (if (>= i n)
        out
        (let [dels
              (take-while #(= :del (:kind (nth rows %))) (range i n))

              j
              (+ i (count dels))

              adds
              (take-while #(= :add (:kind (nth rows %))) (range j n))]

          (if (and (seq dels) (seq adds))
            (recur (+ j (count adds))
                   (reduce (fn [out [d a]]
                             (if-let [[dm am] (word-span (str (:text (nth rows d)))
                                                         (str (:text (nth rows a))))]
                               (-> out
                                   (assoc-in [d :mark] dm)
                                   (assoc-in [a :mark] am))
                               out))
                           out
                           (map vector dels adds)))
            (recur (max (inc i) j) out)))))))

(defn diff-shape
  "Code `rows` of a parsed patch with what the review look needs: each file row holds its
   `:added` and `:removed` counts, each hunk row its `:gap` (the unchanged lines
   before it), and each code row the `:num-w` of the widest line number."
  [rows]
  (let [widths
        (into {}
              (map (fn [[path lines]]
                     [path (count (str (reduce max 0 (mapcat #(keep % [:old :new]) lines))))]))
              (group-by :path rows))

        counts
        (reduce (fn [m {:keys [kind path]}]
                  (case kind
                    :add
                    (update-in m [path :added] (fnil inc 0))

                    :del
                    (update-in m [path :removed] (fnil inc 0))

                    m))
                {}
                rows)]

    (pair-words
      (:rows
        (reduce (fn [{:keys [old-end] :as acc} {:keys [kind path old] :as row}]
                  (case kind
                    :file
                    (-> acc
                        (assoc :old-end 0)
                        (update :rows conj (merge row {:added 0 :removed 0} (get counts path))))

                    :hunk
                    (update acc
                            :rows
                            conj
                            (assoc row :gap (max 0 (- (long (:old-start row)) 1 (long old-end)))))

                    (:add :del :ctx)
                    (cond-> (update acc :rows conj (assoc row :num-w (get widths path 1)))
                      old
                      (assoc :old-end old))

                    (update acc :rows conj row)))
                {:old-end 0 :rows []}
                rows)))))

(defn revision-text
  "The revisions of a diff header: `a → b`, only the revision that is known, or nil."
  [old-revision new-revision]
  (cond (and old-revision new-revision) (str old-revision " → " new-revision)
        new-revision (str new-revision)
        old-revision (str "from " old-revision)))

(defn file-list
  "One selectable `:file-item` row for each file of diff `rows`, with its counts,
   when the diff has more than one file."
  [rows]
  (let [files (filterv #(= :file (:kind %)) rows)]
    (when (< 1 (count files))
      (mapv #(-> %
                 (select-keys [:path :text :status :old-path :added :removed :node-id])
                 (assoc :kind :file-item
                        :is-selectable true))
            files))))

(defn test-file?
  "True for test directories and common test-file names."
  [path]
  (boolean
    (re-find
      #"(?i)(?:^|/)(?:tests?|__tests__|specs?)(?:/|$)|(?:^|/)(?:test_[^/]+|[^/]+(?:[._-](?:test|spec))\.[^/]+)$"
      (str path))))

(defn- context-boundaries
  [rows]
  (into []
        (mapcat (fn [file-rows]
                  (let [file
                        (first file-rows)

                        code?
                        (some :anchor file-rows)

                        visible
                        (remove #(and (= :hunk (:kind %)) (zero? (long (:gap %)))) file-rows)]

                    (cond-> (vec visible)
                      (and code? (not (#{:added :deleted} (:status file))))
                      (conj {:kind :hunk :path (:path file) :trailing? true})))))
        (partition-by :path rows)))

(defn- nonblank-code
  [rows kind]
  (into
    []
    (comp (filter #(= kind (:kind %))) (map #(str/replace (:text %) #"\s+" "")) (remove str/blank?))
    rows))

(defn- whitespace-group
  [rows]
  (if (and (#{:add :del} (:kind (first rows)))
           (= (nonblank-code rows :add) (nonblank-code rows :del)))
    []
    rows))

(defn- visible-patch
  [rows {:keys [hide-tests? hide-whitespace?]}]
  (cond->> rows
    hide-tests?
    (filterv #(not (test-file? (:path %))))

    hide-whitespace?
    (partition-by #(boolean (#{:add :del} (:kind %))))

    hide-whitespace?
    (into [] (mapcat whitespace-group))))

(defn- visible-diff-node
  [node {:keys [hide-tests?]}]
  (if hide-tests?
    (update node
            :threads
            #(filterv (fn [thread]
                        (not (test-file? (get-in thread [:anchor :path]))))
               %))
    node))

(declare split-rows wrap-rows diff-rows*)

(def split-min "Columns from which a diff can show side by side." 100)

(defn diff-layout
  "How a diff shows `width` columns wide: `:split` side by side when the width allows
   and the human did not ask for `unified?`, else `:unified`."
  [width unified?]
  (if (and (not unified?) (>= (long width) (long split-min))) :split :unified))

(defn split-side-w
  "Text columns of one side of a split diff `width` columns wide."
  ^long [width]
  (max 1 (- (quot (+ (long width) 3) 2) 3)))

(defn diff-rows
  "Rows of diff node `node`, `width` columns wide for thread text. A folded file in
   `folded` (a set of paths) shows only its file row. Threads paint under their
   anchor line; a thread whose anchor is not in the patch paints at the end."
  ([node width folded] (diff-rows node width folded nil))
  ([node width folded {:keys [unified? wrap? files?] :as opts}]
   (let [node
         (visible-diff-node node opts)

         layout
         (diff-layout width unified?)

         lines
         (context-boundaries (diff-shape (visible-patch (parse-patch (:patch node)) opts)))

         [head & body]
         (diff-rows* node width folded lines)

         files
         (when (not= false files?) (map #(assoc % :node-id (:id node)) (file-list lines)))

         empty-row
         (when-not (some #(= :file (:kind %)) lines)
           [{:kind :diff-empty :node-id (:id node) :text "No net changes in this selection."}])

         rows
         (into [head] (concat files empty-row body))]

     (cond-> rows
       (= :split layout)
       (split-rows (split-side-w width))

       wrap?
       (wrap-rows width)))))

(defn- thread-summary
  [thread]
  (str (count (:messages thread))
       " message"
       (when (not= 1 (count (:messages thread))) "s")
       (when (:draft thread) " · draft")))

(defn- diff-rows*
  [node width folded lines]
  (let [node-id
        (:id node)

        by-anchor
        (group-by (comp anchor-key :anchor) (app/threads node))

        anchored
        (set (keep #(some-> (:anchor %)
                            anchor-key)
                   lines))

        orphans
        (remove #(contains? anchored (anchor-key (:anchor %))) (app/threads node))

        thread-block
        (fn [thread]
          (let [base {:node-id node-id :thread-id (:id thread) :anchor (:anchor thread)}]
            (into [(merge base
                          {:kind :thread-head :text (thread-summary thread) :is-selectable true})]
                  (thread-rows thread width base))))]

    (into [{:kind :diff-head
            :node-id node-id
            :text (str (or (:label node) "Changes")
                       (some->> (revision-text (:old-revision node) (:new-revision node))
                                (str "  ")))}]
          (concat (:rows
                    (reduce (fn [{:keys [file] :as acc} row]
                              (let [file
                                    (if (= :file (:kind row)) (:path row) file)

                                    hidden?
                                    (and (contains? folded file) (not= :file (:kind row)))]

                                (cond-> (assoc acc :file file)
                                  (not hidden?)
                                  (update :rows
                                          into
                                          (cons (cond-> (assoc row :node-id node-id)
                                                  (:anchor row)
                                                  (assoc :is-selectable true)

                                                  (= :file (:kind row))
                                                  (assoc :is-selectable
                                                    true :is-folded
                                                    (contains? folded file)))
                                                (when-let [anchor (:anchor row)]
                                                  (mapcat thread-block
                                                          (get by-anchor (anchor-key anchor)))))))))
                            {:file nil :rows []}
                            lines))
                  (mapcat thread-block orphans)))))

;;; ── Row text ───────────────────────────────────────────────────────────────

(def ^:private gutter-w 5)

(defn- num-cell
  [row n]
  (let [s (if n (str n) "")]
    (str (apply str (repeat (max 0 (- (long (or (:num-w row) gutter-w)) (count s))) " ")) s)))

(defn file-counts
  "The change counts of a file row: `-12 +9`, or `+11` for a new text file."
  [{:keys [added removed]}]
  (when (or added removed)
    (let [a
          (long (or added 0))

          d
          (long (or removed 0))]

      (str/join " " (remove nil? [(when (or (pos? d) (zero? a)) (str "-" d)) (str "+" a)])))))

(defn file-title
  "A file status and path. Keep the new path when a narrow header cuts a rename."
  ([{:keys [path old-path status]}]
   (str (get {:added "A" :deleted "D" :renamed "R" :modified "M"} status "M")
        " "
        (when (= :renamed status) (str old-path " → "))
        path))
  ([{:keys [path old-path status] :as row} width]
   (let [full
         (file-title row)

         old-room
         (- (long width) 5 (long (p/display-width (str path))))]

     (cond (<= (p/display-width full) width) full
           (and (= :renamed status) (>= old-room 4)) (str "R " (p/ellipsize old-path old-room)
                                                          " → " path)
           :else (p/ellipsize (str (get {:added "A" :deleted "D" :renamed "R"} status "M") " " path)
                              width)))))

(defn hunk-text
  "The separator text of a hunk row: how many unchanged lines it skips."
  [{:keys [gap trailing?]}]
  (if trailing?
    "⋯ More unchanged context may be available"
    (let [gap (long (or gap 0))]
      (if (pos? gap) (str "⋯ " gap " unmodified line" (when (not= 1 gap) "s")) "⋯"))))

(declare row-parts)

(defn- split-side-text
  "One side of a split row as plain text, `w` columns wide."
  [row w]
  (let [{:keys [gutter body]} (if row (row-parts row) {:gutter "" :body ""})]
    (p/pad-right (p/truncate-cols (str gutter body) w) w)))

(defn- thread-row-parts
  "The rail and text of a comment row."
  [row]
  (case (:kind row)
    :thread-head
    {:gutter thread-rail :body (str "Thread · " (:text row))}

    (:message-head :draft-head :thread-empty)
    {:gutter thread-rail :body (str (:text row))}

    (:message :draft)
    {:gutter (str thread-rail "  ") :body (str (:text row))}

    {:gutter "" :body (str (:text row))}))

(defn- code-gutter
  [row]
  (let [mark (get {:add "+" :del "-" :ctx " "} (:kind row))]
    (if-let [side (:split-side row)]
      (str (num-cell row (get row side)) " " mark " ")
      (str (num-cell row (:old row)) " " (num-cell row (:new row)) " " mark " "))))

(defn row-parts
  "The fixed `:gutter` and the scrollable `:body` text of one diff or thread row.
   Only a code row scrolls sideways; the gutter keeps its line numbers in view."
  [row]
  (case (:kind row)
    :file
    {:gutter (if (:is-folded row) "▸ " "▾ ") :body (file-title row)}

    (:add :del :ctx)
    {:gutter (code-gutter row) :body (str (:text row))}

    :hunk
    {:gutter "" :body (hunk-text row)}

    :file-item
    {:gutter "  " :body (file-title row)}

    :split
    {:gutter ""
     :body (str (split-side-text (:left row) (:half row))
                " │ "
                (split-side-text (:right row) (:half row)))}

    (thread-row-parts row)))

(defn- searchable-code
  [row]
  (case (:kind row)
    (:add :del :ctx)
    (when-not (:wrapped row) (or (:source-text row) (:text row)))

    :split
    (when-not (:wrapped row)
      (str (or (get-in row [:left :source-text]) (get-in row [:left :text]))
           "\n"
           (or (get-in row [:right :source-text]) (get-in row [:right :text]))))

    nil))

(defn find-code
  "The next matching code row, with case ignored. Search wraps in either direction."
  ([rows query after] (find-code rows query after :next))
  ([rows query after direction]
   (when-not (str/blank? query)
     (let [q
           (str/lower-case query)

           n
           (count rows)

           start
           (min n (max 0 (inc (long after))))

           order
           (if (= :previous direction)
             (let [before (min n (max 0 (long after)))]
               (concat (range (dec before) -1 -1) (range (dec n) (dec before) -1)))
             (concat (range start n) (range 0 start)))]

       (first (filter #(some-> (searchable-code (nth rows %))
                               str/lower-case
                               (str/includes? q))
                      order))))))

(defn row-tone
  "The live tone a row paints with: `:ok` added, `:error` removed, `:running` a
   hunk or a file, `:warn` a draft, `:idle` meta and thread chrome, nil plain."
  [row]
  (case (:kind row)
    :add
    :ok

    :del
    :error

    (:hunk :file :diff-head)
    :running

    (:draft-head :draft)
    :warn

    (:meta :diff-tools :thread-head :thread-empty)
    :idle

    nil))

;;; ── Actions ────────────────────────────────────────────────────────────────

(defn idempotency-key
  "A fresh Send key: 8 to 128 characters of `[A-Za-z0-9._:-]`."
  []
  (str "tui-" (UUID/randomUUID)))

(defn find-node
  "Node `node-id` of `view`, groups opened, or nil."
  [view node-id]
  (some #(when (= node-id (:id %)) %)
        (tree-seq #(seq (:children %)) :children {:children (:nodes view)})))

(defn draft-action
  "The gateway `draft` action that saves `body` for `target`
   `{:node-id :thread-id :anchor}` against the view state `view`."
  [view {:keys [node-id thread-id anchor]} body]
  (let [node (find-node view node-id)]
    (cond-> {:action :draft :node-id node-id :body (str body) :base-seq (:seq view)}
      thread-id
      (assoc :thread-id thread-id)

      (and (nil? thread-id) anchor)
      (assoc :anchor anchor)

      (app/revision node)
      (assoc :revision (app/revision node)))))

(defn submit-action
  "The gateway `submit` action of one Send attempt with idempotency key `k`.
   The gateway needs `:values` as an object, also when the app has no inputs."
  [view k values]
  {:action :submit :idempotency-key k :base-seq (:seq view) :values (or values {})})

(defn outcome-state
  "What an action outcome means for the surface: `:saved`, `:offline`,
   `:conflict`, `:closed`, `:invalid` or `:failed`."
  [{:keys [is-accepted reason]}]
  (cond is-accepted :saved
        (= "offline" reason) :offline
        (#{"conflict" "stale_revision"} reason) :conflict
        (= "closed" reason) :closed
        (= "invalid" reason) :invalid
        :else :failed))

(def save-label
  "What the Save state reads."
  {:saving "Saving…"
   :saved "Saved"
   :offline "Offline: not saved"
   :conflict "Conflict: the app changed, review and save again"
   :closed "Closed: read only"
   :invalid "Not saved: check the input"
   :failed "Save failed"})

(def send-label
  "What the Send state reads before a receipt exists."
  {:sending "Sending…"
   :offline "Offline: not sent, retry keeps the same key"
   :conflict "Conflict: the app changed, send again"
   :closed "Closed: read only"
   :invalid "Not sent: check the input"
   :failed "Send failed, retry keeps the same key"})

(defn receipt-rows
  "One row per Send receipt of `view`, newest first."
  [view]
  (mapv (fn [{:keys [id status comments values created-at]}]
          {:kind :receipt
           :status (some-> status
                           name)
           :submission-id id
           :text (str "Sent · "
                      (some-> status
                              name)
                      (when (seq comments)
                        (str " · " (count comments)
                             " comment" (when (not= 1 (count comments)) "s")))
                      (when (seq values)
                        (str " · " (count values) " value" (when (not= 1 (count values)) "s")))
                      " · "
                      id)
           :created-at created-at})
        (reverse (:submissions view))))

(defn pending-drafts
  "How many saved drafts the app holds: what one Send posts."
  [view]
  (count (filter :draft
                 (mapcat app/threads
                         (filter
                           (comp app/thread-types :type)
                           (tree-seq #(seq (:children %)) :children {:children (:nodes view)}))))))

;;; ── Split and wrap ─────────────────────────────────────────────────────────

(def ^:private code-kinds #{:add :del :ctx})

(defn- split-row
  [left right half]
  (merge (dissoc (or right left) :mark :text :old :new)
         {:kind :split
          :left (when left (assoc left :split-side :old))
          :right (when right (assoc right :split-side :new))
          :half half}))

(defn split-rows
  "Diff `rows` side by side: an unchanged line on both sides, the n-th removed line
   of a change block next to its n-th added line. Other rows stay as they are.
   `half` is the text width of one side."
  [rows half]
  (let [rows
        (vec rows)

        n
        (count rows)]

    (loop [i
           0

           out
           []]

      (if (>= i n)
        out
        (let [row (nth rows i)]
          (case (:kind row)
            :ctx
            (recur (inc i) (conj out (split-row row row half)))

            (:del :add)
            (let [dels (vec (take-while #(= :del (:kind %)) (subvec rows i)))
                  j (+ i (count dels))
                  adds (vec (take-while #(= :add (:kind %)) (subvec rows j)))
                  k (max (count dels) (count adds))]

              (recur (+ j (count adds))
                     (into out (map #(split-row (get dels %) (get adds %) half)) (range k))))

            (recur (inc i) (conj out row))))))))

(defn- wrap-code
  "Code `row` cut into rows of at most `w` columns with its gutter. The later rows
   have no line numbers and no anchor, so the selection skips them."
  [row w]
  (let [{:keys [gutter body]}
        (row-parts row)

        room
        (max 1 (- (long w) (long (p/display-width gutter))))

        chunks
        (p/fold-cols body room)

        first-n
        (count (first chunks))]

    (into [(cond-> (assoc row
                     :text (first chunks)
                     :source-text (:text row))
             (:mark row)
             (update :mark
                     (fn [[from to]]
                       (when (< (long from) first-n) [from (min (long to) first-n)]))))]
          (map #(-> row
                    (dissoc :anchor :is-selectable :mark :old :new)
                    (assoc :text %
                           :wrapped true)))
          (rest chunks))))

(defn wrap-rows
  "Diff `rows` with long code lines wrapped to `width` columns instead of cut. A
   split row wraps each side on its own, and the sides stay next to each other."
  [rows width]
  (into []
        (mapcat
          (fn [row]
            (cond (code-kinds (:kind row)) (wrap-code row width)
                  (= :split (:kind row))
                  (let [half
                        (:half row)

                        ls
                        (some-> (:left row)
                                (wrap-code half))

                        rs
                        (some-> (:right row)
                                (wrap-code half))]

                    (cons (assoc row
                            :left (first ls)
                            :right (first rs))
                          (map #(-> (split-row (get (vec ls) %) (get (vec rs) %) half)
                                    (dissoc :anchor :is-selectable)
                                    (assoc :wrapped true))
                               (range 1 (max (count ls) (count rs))))))
                  :else [row])))
        rows))
