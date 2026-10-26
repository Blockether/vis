(ns com.blockether.vis.contract.app
  "Pure rules of app attachments: threads, drafts and Send receipts.

   An app attachment is a live view with the `attachment` placement. The engine
   and the TUI apply these rules to the same engine-shaped maps, so both paint the
   same threads and receipts. Every function here is pure: ids, authors and times
   come from the caller."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]))

(def ^:private schema (document/schema-document "view"))

(defn- definition [name] (get-in schema ["$defs" name]))

(defn- component
  [type-name]
  (some #(when (= type-name (get-in % ["properties" "type" "const"])) %)
        (get (definition "component") "oneOf")))

(def media-type
  "Media type of the attachment row that lists an app."
  (get (definition "app_attachment") "contentMediaType"))

(def placement "Internal placement of an app attachment." :attachment)

(def bounds
  "How many receipts, threads, messages, Send values and Send comments an app
   holds, and the longest text."
  {:submissions (get-in (definition "live_view") ["properties" "submissions" "maxItems"])
   :threads (get-in (component "diff") ["properties" "threads" "maxItems"])
   :messages (get-in (definition "thread") ["properties" "messages" "maxItems"])
   :values (get-in (definition "submission") ["properties" "values" "maxItems"])
   :comments (get-in (definition "submission") ["properties" "comments" "maxItems"])
   :body (get-in (definition "message") ["properties" "body" "maxLength"])
   :note (get-in (definition "submission") ["properties" "note" "maxLength"])})

(def ^:private key-rule (definition "idempotency_key"))

(def ^:private key-pattern (re-pattern (get key-rule "pattern")))

(defn idempotency-key?
  "True when `k` is a valid idempotency key of a Send."
  [k]
  (and (string? k)
       (<= (long (get key-rule "minLength")) (count k) (long (get key-rule "maxLength")))
       (some? (re-matches key-pattern k))))

(def submission-statuses
  "Delivery stages of a Send, in order."
  (vec (get (definition "submission_status") "enum")))

(def settled-statuses "Stages after which a receipt no longer changes." #{"answered" "failed"})

(def thread-types "Node types that hold threads." #{:diff :thread})

(defn- refuse!
  [node-id reason]
  (throw (ex-info (str "app node " node-id ": " reason)
                  {:type :app/invalid :node-id node-id :reason reason})))

(defn app? "True when `view` is an app attachment." [view] (= placement (:placement view)))

(defn blank-text?
  "True for nil or text with only whitespace."
  [text]
  (or (nil? text) (str/blank? (str text))))

(defn threads
  "The threads of node `node`: every thread of a diff, or the thread node itself."
  [node]
  (case (:type node)
    :diff
    (vec (:threads node))

    :thread
    [(-> node
         (select-keys [:id :anchor :messages :draft :is-resolved])
         (update :messages vec))]

    []))

(defn find-thread
  "Thread `thread-id` of node `node`, or nil."
  [node thread-id]
  (some #(when (= thread-id (:id %)) %) (threads node)))

(defn- checked-thread
  [node thread]
  (when-not (and (map? thread) (string? (:id thread)) (not (str/blank? (:id thread))))
    (refuse! (:id node) "a thread needs an id"))
  (when (> (count (:messages thread)) (long (:messages bounds)))
    (refuse! (:id node) (str "a thread holds at most " (:messages bounds) " messages")))
  (when (and (some? (:draft thread)) (blank-text? (:draft thread)))
    (refuse! (:id node) "a saved draft is never blank"))
  (when (and (= :diff (:type node)) (not (map? (:anchor thread))))
    (refuse! (:id node) "a diff thread needs an anchor"))
  (update thread :messages #(vec (or % []))))

(defn upsert-thread
  "Node `node` with `thread` in place. A diff replaces the thread with the same
   id or adds it. A thread node takes the messages, draft, anchor and resolution."
  [node thread]
  (let [thread (checked-thread node thread)]
    (case (:type node)
      :diff
      (let [all (vec (:threads node))
            at (first (keep-indexed (fn [i t]
                                      (when (= (:id t) (:id thread)) i))
                                    all))]

        (when (and (nil? at) (>= (count all) (long (:threads bounds))))
          (refuse! (:id node) (str "a diff holds at most " (:threads bounds) " threads")))
        (assoc node :threads (if at (assoc all at thread) (conj all thread))))

      :thread
      (do (when-not (= (:id node) (:id thread))
            (refuse! (:id node) "a thread node takes only its own thread"))
          (merge (dissoc node :anchor :draft :is-resolved)
                 (select-keys thread [:anchor :messages :draft :is-resolved])))

      (refuse! (:id node) "only a diff or a thread node holds threads"))))

(defn remove-threads
  "Diff node `node` without the threads in `thread-ids`. A missing id changes nothing."
  [node thread-ids]
  (when-not (= :diff (:type node)) (refuse! (:id node) "only a diff removes threads"))
  (let [gone (set thread-ids)]
    (update node :threads #(into [] (remove (comp gone :id)) %))))

(defn upsert-submission
  "View `view` with receipt `submission` in place. When the view holds too many
   receipts, the oldest settled receipt goes first, then the oldest receipt."
  [view submission]
  (let [all
        (vec (:submissions view))

        at
        (first (keep-indexed (fn [i s]
                               (when (= (:id s) (:id submission)) i))
                             all))

        all
        (if at (assoc all at submission) (conj all submission))

        limit
        (long (:submissions bounds))

        trim
        (fn [subs]
          (if (<= (count subs) limit)
            subs
            (let [drop-at (or (first (keep-indexed (fn [i s]
                                                     (when (settled-statuses (:status s)) i))
                                                   subs))
                              0)]
              (recur (into (subvec subs 0 drop-at) (subvec subs (inc drop-at)))))))]

    (assoc view :submissions (trim all))))

(defn find-submission
  "Receipt `submission-id` of `view`, or nil."
  [view submission-id]
  (some #(when (= submission-id (:id %)) %) (:submissions view)))

(defn submission-for-key
  "The receipt that idempotency key `k` made in `view`, or nil."
  [view k]
  (some #(when (= k (:idempotency-key %)) %) (:submissions view)))

(defn draft-op
  "The patch operation that saves `body` as the draft of a thread in `node`, or
   nil when nothing changes. `action` holds `:thread-id`, `:anchor` and `:body`.
   A new thread takes `new-id`. Blank text removes a draft, and a new blank thread
   is never created."
  [node {:keys [thread-id anchor body]} new-id]
  (let [text (when-not (blank-text? body) (str body))]
    (when (and text (> (count text) (long (:body bounds))))
      (refuse! (:id node) (str "a draft holds at most " (:body bounds) " characters")))
    (case (:type node)
      :thread
      (let [thread (first (threads node))]
        (when (and thread-id (not= thread-id (:id node)))
          (refuse! (:id node) "a thread node has only its own thread"))
        (cond text {:op :thread :node-id (:id node) :thread (assoc thread :draft text)}
              (:draft thread) {:op :thread :node-id (:id node) :thread (dissoc thread :draft)}))

      :diff
      (if thread-id
        (let [thread (or (find-thread node thread-id) (refuse! (:id node) "no such thread"))]
          (cond text {:op :thread :node-id (:id node) :thread (assoc thread :draft text)}
                (seq (:messages thread))
                (when (:draft thread)
                  {:op :thread :node-id (:id node) :thread (dissoc thread :draft)})
                :else {:op :remove :node-id (:id node) :item-ids [thread-id]}))
        (when text
          (when-not (map? anchor) (refuse! (:id node) "a new diff thread needs an anchor"))
          {:op :thread
           :node-id (:id node)
           :thread {:id new-id :anchor anchor :messages [] :draft text}}))

      (refuse! (:id node) "only a diff or a thread node takes a draft"))))

(defn post-drafts
  "Post every saved draft of the thread nodes in `nodes` as a message. `post`
   holds `:author`, `:at`, `:submission-id` and `:new-id`, a function that gives
   one fresh message id per call. Answers `{:ops [...] :comments [...]}`."
  [nodes {:keys [author at submission-id new-id]}]
  (reduce (fn [acc node]
            (reduce
              (fn [acc thread]
                (if (blank-text? (:draft thread))
                  acc
                  (let [message
                        (cond-> {:id (new-id) :author author :body (:draft thread) :created-at at}
                          submission-id
                          (assoc :submission-id submission-id))

                        posted
                        (-> thread
                            (dissoc :draft)
                            (update :messages (fnil conj []) message))]

                    (-> acc
                        (update :ops conj {:op :thread :node-id (:id node) :thread posted})
                        (update :comments
                                conj
                                (cond-> {:node-id (:id node)
                                         :thread-id (:id thread)
                                         :message-id (:id message)
                                         :body (:body message)}
                                  (:anchor thread)
                                  (assoc :anchor (:anchor thread))))))))
              acc
              (threads node)))
          {:ops [] :comments []}
          (filter (comp thread-types :type) nodes)))

(defn reply-op
  "The patch operation that adds `message` to a thread of `node`. Without
   `:thread-id`, a diff starts thread `:new-thread-id` at `:anchor`."
  [node {:keys [thread-id anchor new-thread-id]} message]
  (when (blank-text? (:body message)) (refuse! (:id node) "a reply needs text"))
  (when (> (count (:body message)) (long (:body bounds)))
    (refuse! (:id node) (str "a reply holds at most " (:body bounds) " characters")))
  (let [thread (cond (= :thread (:type node)) (first (threads node))
                     thread-id (or (find-thread node thread-id)
                                   (refuse! (:id node) "no such thread"))
                     (map? anchor) {:id new-thread-id :anchor anchor :messages []}
                     :else (refuse! (:id node) "a reply names a thread or an anchor"))]
    {:op :thread :node-id (:id node) :thread (update thread :messages (fnil conj []) message)}))

(defn revision
  "The reviewed revision of node `node`: the `new_revision` of a diff, else nil."
  [node]
  (when (= :diff (:type node)) (:new-revision node)))
