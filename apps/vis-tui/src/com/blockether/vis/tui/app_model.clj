(ns com.blockether.vis.tui.app-model
  "Pure model of an app attachment in the terminal: diff lines with stable anchors,
   threads under their anchor line, receipts, and the Save and Send rules.

   The shared operation rules are in `com.blockether.vis.contract.app`. This
   namespace only reads a materialized view and builds gateway actions. It never
   paints and never calls the gateway."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.app :as app])
  (:import [java.util UUID]))

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

(defn- strip-prefix
  [path]
  (let [path (str/trim (first (str/split (str path) #"\t")))]
    (cond (= "/dev/null" path) nil
          (re-find #"^[ab]/" path) (subs path 2)
          :else path)))

(defn parse-patch
  "The rows of unified diff text `patch`, in order. Each row has `:kind` (`:file`,
   `:meta`, `:hunk`, `:add`, `:del` or `:ctx`) and `:text`. A code row also has
   `:path`, `:old` and `:new` line numbers and its stable `:anchor`
   `{:path :side :line}`: side \"old\" for a removed line, else \"new\"."
  [patch]
  (loop [lines
         (str/split-lines (str patch))

         {:keys [old-path path old new] :as at}
         {:old-path nil :path nil :old 0 :new 0}

         rows
         []]

    (if-let [line (first lines)]
      (let [more (rest lines)]
        (cond (str/starts-with? line "--- ") (recur more
                                                    (assoc at
                                                      :old-path (strip-prefix (subs line 4)))
                                                    (conj rows {:kind :meta :text line}))
              (str/starts-with? line "+++ ")
              (let [file (or (strip-prefix (subs line 4)) old-path "file")]
                (recur more
                       (assoc at :path file)
                       (conj rows {:kind :meta :text line} {:kind :file :text file :path file})))
              (str/starts-with? line "@@")
              (let [[_ o n] (re-find #"^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@" line)]
                (recur more
                       (assoc at
                         :old (or (some-> o
                                          parse-long)
                                  old)
                         :new (or (some-> n
                                          parse-long)
                                  new))
                       (conj rows {:kind :hunk :text line :path path})))
              (and path (str/starts-with? line "+"))
              (recur more
                     (update at :new inc)
                     (conj rows
                           {:kind :add
                            :text (subs line 1)
                            :path path
                            :new new
                            :anchor {:path path :side "new" :line new}}))
              (and path (str/starts-with? line "-"))
              (recur more
                     (update at :old inc)
                     (conj rows
                           {:kind :del
                            :text (subs line 1)
                            :path path
                            :old old
                            :anchor {:path path :side "old" :line old}}))
              (and path (or (str/starts-with? line " ") (= "" line)))
              (recur more
                     (-> at
                         (update :old inc)
                         (update :new inc))
                     (conj rows
                           {:kind :ctx
                            :text (if (= "" line) "" (subs line 1))
                            :path path
                            :old old
                            :new new
                            :anchor {:path path :side "new" :line new}}))
              :else (recur more at (conj rows {:kind :meta :text line}))))
      ;; A `diff --git` header before the file name is noise once the file row exists,
      ;; and so is a blank line outside a file.
      (vec (remove #(and (= :meta (:kind %))
                         (or (str/blank? (:text %))
                             (re-find #"^(--- |\+\+\+ |diff --git |index )" (:text %))))
             rows)))))

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
                            (wrap-text (:body message) (- (long width) 2)))))
            messages)

      (:draft thread)
      (into (cons (merge base {:kind :draft-head :text "Draft (saved, not sent)"})
                  (map #(merge base {:kind :draft :text %})
                       (wrap-text (:draft thread) (- (long width) 2)))))

      (:is-resolved thread)
      (conj (merge base {:kind :thread-empty :text "Resolved"})))))

(defn diff-rows
  "Rows of diff node `node`, `width` columns wide for thread text. A folded file in
   `folded` (a set of paths) shows only its file row. Threads paint under their
   anchor line; a thread whose anchor is not in the patch paints at the end."
  [node width folded]
  (let [node-id
        (:id node)

        by-anchor
        (group-by (comp anchor-key :anchor) (app/threads node))

        lines
        (parse-patch (:patch node))

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
                          {:kind :thread-head
                           :text (str (count (:messages thread))
                                      " message"
                                      (when (not= 1 (count (:messages thread))) "s")
                                      (when (:draft thread) " · draft"))
                           :is-selectable true})]
                  (thread-rows thread width base))))]

    (into [{:kind :diff-head
            :node-id node-id
            :text (str (or (:label node) "Changes")
                       (when (or (:old-revision node) (:new-revision node))
                         (str "  " (or (:old-revision node) "?")
                              " → " (or (:new-revision node) "?"))))}]
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
  [n]
  (let [s (if n (str n) "")]
    (str (apply str (repeat (max 0 (- (long gutter-w) (count s))) " ")) s)))

(def ^:private thread-rail "    │ ")

(defn row-parts
  "The fixed `:gutter` and the scrollable `:body` text of one diff or thread row.
   Only a code row scrolls sideways; the gutter keeps its line numbers in view."
  [row]
  (case (:kind row)
    :file
    {:gutter (if (:is-folded row) "▸ " "▾ ") :body (str (:text row))}

    :add
    {:gutter (str (num-cell nil) " " (num-cell (:new row)) " + ") :body (str (:text row))}

    :del
    {:gutter (str (num-cell (:old row)) " " (num-cell nil) " - ") :body (str (:text row))}

    :ctx
    {:gutter (str (num-cell (:old row)) " " (num-cell (:new row)) "   ") :body (str (:text row))}

    :thread-head
    {:gutter thread-rail :body (str "Thread · " (:text row))}

    :message-head
    {:gutter thread-rail :body (str (:text row))}

    (:message :draft)
    {:gutter (str thread-rail "  ") :body (str (:text row))}

    (:draft-head :thread-empty)
    {:gutter thread-rail :body (str (:text row))}

    {:gutter "" :body (str (:text row))}))

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

    (:meta :thread-head :thread-empty)
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
