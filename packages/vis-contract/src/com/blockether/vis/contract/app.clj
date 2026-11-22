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

(def ^:private canvas-thread-limits
  (get-in (component "canvas") ["properties" "threads" "items" "allOf" 1 "properties"]))

(def bounds
  "How many receipts, threads, messages, Send values and Send comments an app
   holds, and the longest texts: a message body, a Send note and the note for the
   model."
  {:submissions (get-in (definition "live_view") ["properties" "submissions" "maxItems"])
   :threads (get-in (component "diff") ["properties" "threads" "maxItems"])
   :messages (get-in (definition "thread") ["properties" "messages" "maxItems"])
   :values (get-in (definition "submission") ["properties" "values" "maxItems"])
   :comments (get-in (definition "submission") ["properties" "comments" "maxItems"])
   :canvas-draft (get-in canvas-thread-limits ["draft" "maxLength"])
   :canvas-messages (get-in canvas-thread-limits ["messages" "maxItems"])
   :canvas-body (get-in canvas-thread-limits ["messages" "items" "properties" "body" "maxLength"])
   :body (get-in (definition "message") ["properties" "body" "maxLength"])
   :note (get-in (definition "submission") ["properties" "note" "maxLength"])
   :agent-note (get-in (definition "live_view") ["properties" "agent_note" "maxLength"])
   :log (get-in (definition "live_view") ["properties" "log" "maxItems"])
   :log-message (get-in (definition "log_entry") ["properties" "message" "maxLength"])
   :log-detail (get-in (definition "log_entry") ["properties" "detail" "maxLength"])})

(def icons "The icon names an app can declare." (set (get (definition "app_icon") "enum")))

(def default-icon "The icon of an app that declares none." (get (definition "app_icon") "default"))

(defn icon-mark
  "A font-independent terminal mark for the icon of app view."
  [view]
  (let [marks (get (definition "app_icon") "x-vis-terminal")]
    (get marks (:icon view) (get marks default-icon))))

;; Surfaces: where an app shows

(def ^:private surfaces-rule (definition "app_surfaces"))

(def surface-defaults
  "The surfaces of an app that declares none: `{:inline true :session \"sidebar\"}`."
  (into {}
        (map (fn [[k v]]
               [(keyword k) (get v "default")]))
        (get surfaces-rule "properties")))

(def session-surfaces
  "Where the session apps button can open an app, `off` included."
  (vec (get-in surfaces-rule ["properties" "session" "enum"])))

(def ^:private settings-key-rule (get-in (definition "live_view") ["properties" "settings_key"]))

(defn- slug
  [s]
  (-> (str/lower-case (str s))
      (str/replace #"[^a-z0-9]+" "_")
      (str/replace #"^_+|_+$" "")))

(defn settings-key
  "The id prefix of the settings of app `view`, or nil for an app without a key.
   It joins the owning extension (or `sandbox`) and the settings name. Without a
   settings name, the key is the name. So apps with different keys share their
   settings only when they declare one name. A long prefix keeps its start and
   ends in a hash, so two names never share one."
  [view]
  (when-let [k (some-> (:key view)
                       str
                       str/trim
                       not-empty)]
    (let [settings-name (or (some-> (:settings-name view)
                                    str
                                    str/trim
                                    not-empty)
                            k)
          base (str "app_" (slug (str (or (:extension view) "sandbox") "_" settings-name)))
          limit (long (get settings-key-rule "maxLength"))]

      (if (<= (count base) limit)
        base
        (str (str/replace (subs base 0 (- limit 9)) #"_+$" "")
             "_"
             (format "%08x" (bit-and 0xffffffff (long (hash base)))))))))

(defn effective-surfaces
  "The surfaces that a reader of app `view` gets. `setting` answers the value of a
   setting id, or nil when the setting does not exist. `apps_inline` and
   `apps_session` turn a surface off for every app. The app's own settings win
   over what the app declared."
  [view setting]
  (let [declared
        (merge surface-defaults (:surfaces view))

        own
        (settings-key view)

        own-value
        (fn [suffix fallback]
          (let [v (when own (setting (str own suffix)))]
            (if (nil? v) fallback v)))

        inline
        (own-value "_inline" (:inline declared))

        session
        (own-value "_session" (:session declared))]

    {:inline (and (not (false? (setting "apps_inline"))) (true? inline))
     :session (if (false? (setting "apps_session"))
                "off"
                (if (some #{session} session-surfaces) session (:session surface-defaults)))}))

(def scopes
  "Who an app can belong to: its session, or no session at all."
  (vec (get (definition "app_scope") "enum")))

(defn global?
  "True when app `view` belongs to no session. Only an SDK program opens one."
  [view]
  (= "global"
     (some-> (:scope view)
             name)))

(defn persistent?
  "True when global app `view` comes back after a gateway restart. A session app
   always comes back."
  [view]
  (not (and (global? view) (false? (:is-persistent view)))))

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

(def thread-types "Node types that hold threads." #{:canvas :diff :thread})

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
    (:canvas :diff)
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

(defn resolution-only?
  "True for a thread op that changes only the resolved flag."
  [thread]
  (and (map? thread) (contains? thread :is-resolved) (not (contains? thread :messages))))

(defn- fresh-thread-id
  "An unused thread id. A canvas shares ids between threads and cards."
  [node candidate]
  (let [held (into (set (map :id (threads node))) (map :id) (:cards node))]
    (if (held candidate)
      (first (remove held (map #(str candidate "-" %) (iterate inc 1))))
      candidate)))

(defn owner-thread
  "Thread `thread` of an owner's thread op on node `node` as it lands: a message that
   the host stored keeps the stored copy, found by id, and the saved draft of the human
   stays. An owner never writes a draft."
  [node thread]
  (let [before
        (find-thread node (:id thread))

        posted
        (into {} (map (juxt :id identity)) (:messages before))]

    (cond-> (-> thread
                (update :messages
                        (fn [messages]
                          (mapv #(get posted (:id %) %) messages)))
                (dissoc :draft))
      (:draft before)
      (assoc :draft (:draft before)))))

(defn upsert-thread
  "Diff or thread node `node` with `thread` in place. A diff replaces the thread
   with the same id or adds it. A thread node takes the messages, draft, anchor and
   resolution. The patch target table and the node check of the host own the rules."
  [node thread]
  (let [thread (cond-> thread
                 (map? thread)
                 (update :messages #(vec (or % []))))]
    (if (= :thread (:type node))
      (merge (dissoc node :anchor :draft :is-resolved)
             (select-keys thread [:anchor :messages :draft :is-resolved]))
      (let [all (vec (:threads node))
            at (first (keep-indexed (fn [i t]
                                      (when (= (:id t) (:id thread)) i))
                                    all))]

        (assoc node :threads (if at (assoc all at thread) (conj all thread)))))))

(defn remove-threads
  "Diff node `node` without the threads in `thread-ids`. A missing id changes nothing."
  [node thread-ids]
  (let [gone (set thread-ids)]
    (cond-> node
      (contains? node :threads)
      (update :threads
              #(filterv (fn [thread]
                          (not (or (gone (:id thread))
                                   (and (= :canvas (:type node)) (gone (:item-id thread))))))
                 %)))))

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

(defn- check-body!
  [node body draft?]
  (let [limit (long (get
                      bounds
                      (if (= :canvas (:type node)) (if draft? :canvas-draft :canvas-body) :body)))]
    (when (and body (> (.codePointCount ^String body 0 (.length ^String body)) limit))
      (refuse! (:id node)
               (str (if draft? "a draft" "a reply") " holds at most " limit " characters")))))

(defn- full-canvas-thread?
  [node thread]
  (and (= :canvas (:type node))
       (>= (count (:messages thread)) (long (:canvas-messages bounds)))))

(defn- check-message-room!
  [node thread]
  (when (full-canvas-thread? node thread)
    (refuse! (:id node) (str "a canvas thread holds at most " (:canvas-messages bounds) " messages"))))

(defn- append-message
  [node thread message]
  (check-message-room! node thread)
  (update thread :messages (fnil conj []) message))

(defn draft-op
  "The patch operation that saves `body` as the draft of a thread in `node`, or
   nil when nothing changes. `action` holds `:thread-id`, `:anchor` and `:body`.
   A new thread takes `new-id`. A nil body removes a draft. Blank text removes the
   draft of a thread without messages, and a new blank thread is never created. On
   a thread with messages, blank text saves an empty draft: the human adds the
   messages to the next message of the session without a note."
  [node {:keys [thread-id anchor item-id body]} new-id]
  (let [text
        (when-not (blank-text? body) (str body))

        saved
        (fn [thread]
          (cond text (assoc thread :draft text)
                (and (some? body) (seq (:messages thread))) (assoc thread :draft "")
                (contains? thread :draft) (dissoc thread :draft)))]

    (check-body! node text true)
    (case (:type node)
      :thread
      (let [thread (first (threads node))]
        (when (and thread-id (not= thread-id (:id node)))
          (refuse! (:id node) "a thread node has only its own thread"))
        (when-let [after (saved thread)]
          (when (not= after thread) {:op :thread :node-id (:id node) :thread after})))

      (:canvas :diff)
      (if thread-id
        (let [thread
              (or (find-thread node thread-id) (refuse! (:id node) "no such thread"))

              after
              (saved thread)]

          (when text (check-message-room! node thread))
          (cond (and (nil? text) (empty? (:messages thread)))
                {:op :remove :node-id (:id node) :item-ids [thread-id]}
                (and after (not= after thread)) {:op :thread :node-id (:id node) :thread after}))
        (when text
          (let [target (if (= :canvas (:type node))
                         (do (when (blank-text? item-id)
                               (refuse! (:id node) "a new canvas thread needs an item id"))
                             (when-not (some #(= item-id (:id %)) (:cards node))
                               (refuse! (:id node) "the canvas has no item with that id"))
                             {:item-id item-id})
                         (if (map? anchor)
                           {:anchor anchor}
                           (refuse! (:id node) "a new diff thread needs an anchor")))]
            {:op :thread
             :node-id (:id node)
             :thread (assoc target
                       :id (fresh-thread-id node new-id)
                       :messages []
                       :draft text)})))

      (refuse! (:id node) "only a canvas, a diff or a thread node takes a draft"))))

(defn drafts
  "The saved drafts of the thread nodes in the component tree `nodes`: what the
   human adds to the next message of the session. A draft names its node, its
   thread and its anchor, and a thread with messages gives them as `:quote`."
  [nodes]
  (into []
        (for [node
              (rest (tree-seq #(seq (:children %)) :children {:children nodes}))

              :when (thread-types (:type node))
              thread
              (threads node)

              :when (string? (:draft thread))]

          (cond-> {:node-id (:id node) :thread-id (:id thread) :body (:draft thread)}
            (:anchor thread)
            (assoc :anchor (:anchor thread))

            (:item-id thread)
            (assoc :item-id (:item-id thread))

            (seq (:messages thread))
            (assoc :quote (str/join "\n\n" (map :body (:messages thread))))))))

(defn post-drafts
  "Send every nonblank draft of the thread nodes in `nodes`. A full canvas thread
   does not take another message, but its draft becomes a comment on the Send.
   `post` holds `:author`, `:at`, `:submission-id` and `:new-id`, a function that
   gives one fresh message id per call. Answers `{:ops [...] :comments [...]}`."
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
                        (cond-> (dissoc thread :draft)
                          (not (full-canvas-thread? node thread))
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
                                  (assoc :anchor (:anchor thread))

                                  (:item-id thread)
                                  (assoc :item-id (:item-id thread))))))))
              acc
              (threads node)))
          {:ops [] :comments []}
          (filter (comp thread-types :type) nodes)))

(defn reply-op
  "The patch operation that adds `message` to a thread of `node`. Without
   `:thread-id`, a diff starts thread `:new-thread-id` at `:anchor`."
  [node {:keys [thread-id anchor item-id new-thread-id]} message]
  (when (blank-text? (:body message)) (refuse! (:id node) "a reply needs text"))
  (check-body! node (:body message) false)
  (let [thread (cond (= :thread (:type node)) (first (threads node))
                     thread-id (or (find-thread node thread-id)
                                   (refuse! (:id node) "no such thread"))
                     (and (= :canvas (:type node)) (not (blank-text? item-id)))
                     {:id (fresh-thread-id node new-thread-id) :item-id item-id :messages []}
                     (and (= :diff (:type node)) (map? anchor))
                     {:id (fresh-thread-id node new-thread-id) :anchor anchor :messages []}
                     :else (refuse! (:id node) "a reply names a thread or an anchor"))]
    {:op :thread :node-id (:id node) :thread (append-message node thread message)}))

(defn revision
  "The reviewed revision of node `node`: the `new_revision` of a diff, else nil."
  [node]
  (when (= :diff (:type node)) (:new-revision node)))

;; App shortcuts

(def ^:private key-definition (definition "shortcut_key"))

(def shortcut-names
  "Named keys that an app shortcut can bind, besides one printable character."
  (vec (get-in key-definition ["oneOf" 1 "enum"])))

(def ^:private named-key? (set shortcut-names))

(def ^:private char-key-pattern (re-pattern (get-in key-definition ["oneOf" 0 "pattern"])))

(def shortcut-key-text
  "The valid shortcut keys, as one sentence part that a refusal can quote."
  (str "one printable ASCII character from ! to ~ (case-sensitive) or one of "
       (str/join ", " shortcut-names)))

(defn shortcut-key?
  "True when `k` is a key that an app shortcut can bind."
  [k]
  (and (string? k) (or (some? (re-matches char-key-pattern k)) (contains? named-key? k))))

(defn- refuse-view!
  [reason]
  (throw (ex-info (str "app: " reason) {:type :app/invalid :reason reason})))

(defn set-shortcuts
  "App `view` with `shortcuts` in place of its old list. An empty list removes
   every shortcut. The shared check of the patched view owns their rules."
  [view shortcuts]
  (if (seq shortcuts) (assoc view :shortcuts (vec shortcuts)) (dissoc view :shortcuts)))

(defn find-shortcut
  "The shortcut of `view` that binds key `k`, or nil."
  [view k]
  (some #(when (= k (:key %)) %) (:shortcuts view)))

(defn agent-note-error
  "nil when `text` can be the note of `view` for the model, otherwise the reason.
   Only an app takes a note. A blank text is valid: it removes the note."
  [view text]
  (cond (not (app? view)) "only an app (placement attachment) takes a note for the model"
        (not (string? text)) "the note for the model must be text"
        (> (count text) (long (:agent-note bounds)))
        (str "the note for the model holds at most " (:agent-note bounds) " characters")))

(defn set-agent-note
  "App `view` with `text` as its note for the model. The model reads the note
   with the next user message. A blank text removes the note."
  [view text]
  (some-> (agent-note-error view text)
          refuse-view!)
  (if (blank-text? text) (dissoc view :agent-note) (assoc view :agent-note (str/trim text))))

;; Debug log

(def log-levels "How serious an app log entry can be." (set (get (definition "log_level") "enum")))

(def log-sources
  "Who can write an app log entry."
  (set (get-in (definition "log_entry") ["properties" "source" "enum"])))

(defn- code-points
  "How many code points `text` holds; a JSON Schema `maxLength` counts these."
  ^long [^String text]
  (.codePointCount text 0 (.length text)))

(defn- head
  "The first `n` code points of `text`."
  [^String text n]
  (subs text 0 (.offsetByCodePoints text 0 (int n))))

(defn- tail
  "The last `n` code points of `text`."
  [^String text n]
  (subs text (.offsetByCodePoints text (.length text) (- (int n)))))

(defn- clip-message
  "`text` cut to `limit` code points, the last one an ellipsis."
  [text limit]
  (let [limit (long limit)]
    (if (<= (code-points text) limit) text (str (head text (dec limit)) "…"))))

(def ^:private detail-gap "The line that marks the cut middle of a long detail." "\n…\n")

(defn- clip-detail
  "`text` cut to `limit` code points. A long text keeps its start and its end, so a
   stack trace keeps both its message and its innermost cause."
  [text limit]
  (let [limit (long limit)]
    (if (<= (code-points text) limit)
      text
      (let [kept (- limit (code-points detail-gap))
            start (quot kept 2)]

        (str (head text start) detail-gap (tail text (- kept start)))))))

(defn cut-log-texts
  "Log op or entry `entry` with its texts inside the schema bounds. The message is
   trimmed and cut at its end. A long detail loses its middle, and a blank detail
   is dropped. Other values stay as they are."
  [{:keys [message detail] :as entry}]
  (cond-> entry
    (string? message)
    (assoc :message (clip-message (str/trim message) (:log-message bounds)))

    (blank-text? detail)
    (dissoc :detail)

    (string? detail)
    (cond->
      (not (blank-text? detail))
      (assoc :detail (clip-detail detail (:log-detail bounds))))))

(defn log-entry-error
  "nil when `entry` can go into the log of `view`, otherwise the reason. Only an
   app takes a log entry, and an entry needs a known level and a message."
  [view {:keys [level message detail]}]
  (cond (not (app? view)) "only an app (placement attachment) takes a log entry"
        (not (contains? log-levels
                        (some-> level
                                name)))
        (str "a log entry needs a level: " (str/join ", " (sort log-levels)))
        (blank-text? message) "a log entry needs a message"
        (not (or (nil? detail) (string? detail))) "the detail of a log entry must be text"))

(defn append-log
  "App `view` with `entry` at the end of its log. The entry takes the next `seq`.
   `:at` and `:source` come from the caller. Long texts are cut by
   [[cut-log-texts]], so an entry is never lost. The log keeps only its newest
   entries."
  [view {:keys [level at source] :as entry}]
  (some-> (log-entry-error view entry)
          refuse-view!)
  (when-not (contains? log-sources
                       (some-> source
                               name))
    (refuse-view! (str "a log entry needs a source: " (str/join ", " (sort log-sources)))))
  (let [log
        (vec (:log view))

        {:keys [message detail]}
        (cut-log-texts entry)

        added
        (cond-> {:seq (inc (long (or (:seq (peek log)) 0)))
                 :at (long at)
                 :level (name level)
                 :source (name source)
                 :message message}
          detail
          (assoc :detail detail))

        kept
        (conj log added)

        extra
        (- (count kept) (long (:log bounds)))]

    ;; `into` copies: a subvec would keep every dropped entry alive.
    (assoc view :log (if (pos? extra) (into [] (subvec kept extra)) kept))))
