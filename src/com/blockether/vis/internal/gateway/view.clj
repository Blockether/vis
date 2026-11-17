(ns com.blockether.vis.internal.gateway.view
  "Gateway transport for the shared View lifecycle.

   The engine publishes `:view/open`, `:view/patch` and `:view/close` envelopes on
   every selected channel. This namespace projects the `:app` copy into the matching
   `view.open`, `view.patch` and `view.close` session events, preserving `:kind` so
   clients choose capability policy without guessing from event names.

   An `:input` View blocks while a `:live` View streams coalesced patches on
   [[live-flush-ms]]. Operator intent for either kind enters through one [[action!]]
   and one kind-independent REST resource; the engine owns the closed action
   vocabulary and capability checks.

   A `:view/send` envelope carries a Send of an app of the python_execution sandbox.
   It becomes one bounded notification turn of the session that owns the app.

   An input View belongs only to the session that raises it. A managed subagent
   never raises one: the engine refuses its request, because its leader answers
   for it. Each input View carries an `:origin` that names the session which asks."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.channel.events :as channel-events]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.view.app-surfaces :as app-surfaces]
            [com.blockether.vis.internal.view.core :as view]
            [com.blockether.vis.internal.view.sink :as sink]
            [com.blockether.vis.contract.view :as view-spec]
            [com.blockether.vis.contract.wire :as wire]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(set! *warn-on-reflection* true)

(def channel-id "Channel the companion app is served on." :app)

(def ^:private listener-id ::gateway)

(defn- session-of
  "Session id a request view / close event belongs to, as a string, or nil."
  [m]
  (some-> (:session-id m)
          str
          str/trim
          not-empty))

(defn- session-link
  "Title and parent session of `sid` as `{:title … :parent-id …}`, or nil when the
   store does not know the session. A lookup failure answers nil: origin data is
   help for the reader, never a reason to lose the form."
  [sid]
  (try (when-let [db (lp/db-info)]
         (let [record (lp/by-id sid)
               parent (some-> (agents/info db sid)
                              :parent_id
                              str
                              str/trim
                              not-empty)]

           (when (or record parent)
             (cond-> {}
               (some-> (:title record)
                       str/trim
                       not-empty)
               (assoc :title (str/trim (:title record)))

               parent
               (assoc :parent-id parent)))))
       (catch Throwable _ nil)))

(defn- origin-of
  "The `:origin` stamp of an input View raised in session `sid`."
  [sid]
  (let [{:keys [title parent-id]}
        (session-link sid)

        parent-title
        (when parent-id (:title (session-link parent-id)))]

    (cond-> {:session-id sid}
      title
      (assoc :title title)

      parent-id
      (assoc :parent-session-id parent-id)

      parent-title
      (assoc :parent-title parent-title))))

(defn- with-origin
  "Input View `view` with its `:origin` stamp."
  [view]
  (if-let [sid (session-of view)]
    (assoc view :origin (origin-of sid))
    view))

(defn person-may-answer?
  "Whether the person may be asked in session `sid`. This is the ask gate of the
   engine: a managed subagent may not ask, because its leader answers for it."
  [sid]
  (agents/person-answers? (lp/db-info) (str sid)))

(defn view-of
  "Open View `view-id` when session `sid` may act on it, as a kind/document
   descriptor, else nil. This is the ownership gate for the shared action route.
   A View belongs only to its session."
  [sid view-id]
  (or (when-let [{:keys [view] :as descriptor} (view/open-view (str view-id))]
        (when (= (str sid) (session-of view)) descriptor))
      ;; A closed app stays readable. After a restart, its record answers.
      (when-let [app (view/app-view sid view-id)]
        {:kind :live :view app})))

(defn input-views
  "Input Views session `sid` is BLOCKED on right now, oldest first, each with its
   `:origin`."
  [sid]
  (let [sid (str sid)]
    (into [] (comp (filter #(= sid (session-of %))) (map with-origin)) (view/pending-requests))))

(defn input-view-of
  "Input View `view-id` when it is pending in `sid`, else nil."
  [sid view-id]
  (let [{:keys [kind view]} (view-of sid view-id)]
    (when (= :input kind) view)))

(defn action!
  "Apply one closed operator action to open View `view-id`. Kind policy and all
   validation stay in the engine, so every remote surface receives the same verdict.
   The decoded action's own keys become engine keywords here, once; the `values`
   it submits stay keyed by field name."
  [view-id action]
  (view/action! (str view-id)
                (cond-> action
                  (map? action)
                  (update-keys wire/engine-key))))

;; --- Live views (a run the human WATCHES) ---

(def ^:private live-flush-ms
  "How long a view's patches are held before they are published as ONE session
   event.

   Not an optimization: `state/append-event!` is a DURABLE publish, so an
   extension draining a build log would park its own thread on the journal
   writer once per line, and the cross-process journal is force-truncated past
   16 MB mid-turn. Nothing is lost by waiting: the engine already accepted and
   RECORDED every patch in the view's sink file before this bridge saw it, and a
   client that misses a frame repairs from [[live-view-of]].

   An APP frame is not durable at all. An app such as a game patches on every
   tick, and its record and [[live-view-of]] already keep its current picture, so
   a copy in the session journal only fills the replay of the transcript. Every
   app surface reads that picture when it opens and again after a gap. The frame
   skips the cross-process journal too: only the process that holds the app can
   serve its picture, so no other process can apply the frame."
  100)

(def ^:private live-merge-cap
  "The most items one coalesced `append` may carry — the engine's own
   `:max-patch-lines`. A merged frame stays the size of a patch the engine would
   have accepted, so a client's reducer is never handed something the producer
   could not have sent."
  500)

(defonce ^:private buffered (atom {}))

(defonce ^:private flusher
  (delay (Executors/newSingleThreadScheduledExecutor (util/daemon-thread-factory
                                                       "vis-live-view-flush"))))

(defn- op-node
  "The node an op addresses. `add-node` names it inside the spec it carries."
  [op]
  (or (:node-id op) (:id (:node-spec op))))

(defn- append-key
  "The one payload key an `append` carries — `:lines` for a log, the keyed
   collection for everything else."
  [op]
  (first (filter #(contains? op %)
                 [:lines :rows :stats :steps :lanes :badges :items :series :links])))

(defn- coalesce
  "Fold `op` into `ops`, merging it into the last operation with the same owner
   when two updates in one phone frame supersede each other.

   `set` MERGES on the engine's side, so two sets on one node are one set with
   the later keys winning; `append` upserts by id, so two appends are one append
   with the items still in order. Everything
   structural — `add-node`, `remove-node`, `clear`, `remove` — is kept exactly as
   it stands: it decides what the node IS, and a surface that never saw it would
   paint a view the record does not have. Nothing is ever reordered: a node op
   merges only into the last op on its OWN node, and only when no `add-node` or
   `remove-node` stands after that op. Such an op can drop the node with its group
   and add it again, and the merged op would then come before the node it changes."
  [ops op]
  (let [node
        (op-node op)

        at
        (when node
          (last (keep-indexed (fn [i earlier]
                                (when (= node (op-node earlier)) i))
                              ops)))

        prior
        (when (and (some? at)
                   (not-any? #(contains? #{:add-node :remove-node} (:op %))
                             (subvec ops (inc (long at)))))
          (nth ops at))

        k
        (append-key op)]

    (cond (nil? prior) (conj ops op)
          (= :set (:op op) (:op prior)) (assoc ops at (merge prior op))
          (and (= :append (:op op) (:op prior))
               (some? k)
               (= k (append-key prior))
               (<= (+ (count (get prior k)) (count (get op k))) (long live-merge-cap)))
          (assoc ops at (update prior k into (get op k)))
          :else (conj ops op))))

(defn global-listed?
  "Whether the person lets the surfaces list global apps: setting `apps_global`."
  []
  (not (false? (toggles/value-of "apps_global"))))

(defn- publish-global!
  "Offer one event of a global app on the fleet stream, when global apps show."
  [type payload]
  (when (global-listed?) (state/publish-fleet-event! type payload)))

(declare chars-above?)

;; --- View deltas (an app reader that is behind reads the edits of long strings) ---

(defonce
  ^{:private true
    :doc
    "App view id to `{:frames … :chars … :seq …}`. `:frames` holds the newest
   published frames as `{:delta … :chars …}`, oldest first and without a gap.
   `:chars` is the sum of their `:chars` and `:seq` the seq of the newest frame."}
  deltas
  (atom {}))

(def ^:private frame-chars
  "The characters that each kept frame counts for, more than its strings: a frame
   without strings also takes room."
  64)

(def ^:private edit-distance-cap
  "The most lines that [[string-edits]] lets come and go between two strings. A
   larger change comes as one edit."
  512)

(defn- range-edit
  "The edit (view.json `string_edit`) that turns `base` from `a0` to `a1` into
   `target` from `b0` to `b1`: the common start and the common end of the two
   ranges stay. No edit splits a surrogate pair. Nil when the ranges are equal."
  [^String base a0 a1 ^String target b0 b1]
  (let [a0
        (long a0)

        a1
        (long a1)

        b0
        (long b0)

        b1
        (long b1)

        n
        (- a1 a0)

        m
        (- b1 b0)

        limit
        (min n m)

        start
        (loop [i 0]
          (if (and (< i limit) (== (int (.charAt base (+ a0 i))) (int (.charAt target (+ b0 i)))))
            (recur (inc i))
            i))

        start
        (if (and (pos? start) (Character/isHighSurrogate (.charAt base (+ a0 (dec start)))))
          (dec start)
          start)

        room
        (- limit start)

        end
        (loop [k 0]
          (if (and (< k room)
                   (== (int (.charAt base (- a1 k 1))) (int (.charAt target (- b1 k 1)))))
            (recur (inc k))
            k))

        end
        (if (and (pos? end) (Character/isLowSurrogate (.charAt base (- a1 end)))) (dec end) end)]

    (when-not (and (== start n) (== start m))
      {:start (+ a0 start)
       :delete (- n start end)
       :insert (.substring target (int (+ b0 start)) (int (- b1 end)))})))

(defn- line-starts
  "The start of each line of `text` from `from` to `to`, then `to`."
  ^ints [^String text ^long from ^long to]
  (loop [at
         from

         starts
         (transient [from])]

    (let [newline (.indexOf text 10 (int at))]
      (if (or (neg? newline) (>= (inc newline) to))
        (int-array (persistent! (conj! starts to)))
        (recur (inc newline) (conj! starts (inc newline)))))))

(defn- line-ids
  "One number for each line of `text` that `starts` gives. Equal lines have the same
   number in `seen`."
  ^ints [^String text ^ints starts ^java.util.HashMap seen]
  (let [n
        (dec (alength starts))

        ids
        (int-array n)]

    (dotimes [i n]
      (let [line (.substring text (aget starts i) (aget starts (inc i)))
            id (or (.get seen line)
                   (let [id (.size seen)]
                     (.put seen line id)
                     id))]

        (aset ids i (int id))))
    ids))

(defn- changed-lines
  "The lines of `a` that go and the lines of `b` that come in a shortest change of
   `a` to `b`, as two boolean arrays. Nil when more than [[edit-distance-cap]]
   lines come and go."
  [^ints a ^ints b]
  (let [n
        (alength a)

        m
        (alength b)

        cap
        (min (+ n m) (long edit-distance-cap))

        ;; Diagonal k is at index k + mid. The furthest x of each diagonal is in `v`.
        mid
        (inc cap)

        v
        (int-array (+ 3 (* 2 cap)))

        trace
        (object-array (inc cap))

        down?
        (fn [^ints v ^long d ^long k]
          (or (== k (- d))
              (and (not (== k d)) (< (aget v (+ mid (dec k))) (aget v (+ mid (inc k)))))))

        distance
        (loop [d 0]
          (when (<= d cap)
            (aset trace d (aclone v))
            (or (loop [k (- d)]
                  (when (<= k d)
                    (let [x0 (long (if (down? v d k)
                                     (aget v (+ mid (inc k)))
                                     (inc (aget v (+ mid (dec k))))))
                          x (loop [x x0]
                              (let [y (- x k)]
                                (if (and (< x n) (< y m) (== (aget a x) (aget b y)))
                                  (recur (inc x))
                                  x)))]

                      (aset v (+ mid k) (int x))
                      (if (and (>= x n) (>= (- x k) m)) d (recur (+ k 2))))))
                (recur (inc d)))))]

    (when distance
      (let [gone
            (boolean-array n)

            come
            (boolean-array m)]

        (loop [d
               (long distance)

               x
               n

               y
               m]

          (when (pos? d)
            (let [before
                  ^ints (aget trace d)

                  k
                  (- x y)

                  from-k
                  (if (down? before d k) (inc k) (dec k))

                  from-x
                  (long (aget before (+ mid from-k)))

                  from-y
                  (- from-x from-k)]

              (if (> from-k k) (aset come from-y true) (aset gone from-x true))
              (recur (dec d) from-x from-y))))
        [gone come]))))

(defn- line-edits
  "The edits that turn `base` from `a0` to `a1` into `target` from `b0` to `b1`,
   one for each group of changed lines. Both ranges start and end at a line. Nil
   when [[changed-lines]] gives no answer."
  [^String base a0 a1 ^String target b0 b1]
  (let [seen
        (java.util.HashMap.)

        ^ints a-starts
        (line-starts base (long a0) (long a1))

        ^ints b-starts
        (line-starts target (long b0) (long b1))

        n
        (dec (alength a-starts))

        m
        (dec (alength b-starts))]

    (when-let [[^booleans gone ^booleans come] (changed-lines (line-ids base a-starts seen)
                                                              (line-ids target b-starts seen))]
      (loop [i 0
             j 0
             edits []]

        (cond (and (< i n) (< j m) (not (aget gone i)) (not (aget come j)))
              (recur (inc i) (inc j) edits)
              (or (< i n) (< j m))
              (let [i2 (long (loop [i i]
                               (if (and (< i n) (aget gone i)) (recur (inc i)) i)))
                    j2 (long (loop [j j]
                               (if (and (< j m) (aget come j)) (recur (inc j)) j)))
                    edit (range-edit base
                                     (aget a-starts i)
                                     (aget a-starts i2)
                                     target
                                     (aget b-starts j)
                                     (aget b-starts j2))]

                (when-not (and (== i i2) (== j j2))
                  (recur i2
                         j2
                         (cond-> edits
                           edit
                           (conj edit)))))
              :else edits)))))

(defn string-edits
  "The edits (view.json `string_edit`) that turn `base` into `target`, in ascending
   order and without overlap. Each group of changed lines is one edit, so two
   changes far from each other do not carry the text between them. A string
   without lines, and a change of more than [[edit-distance-cap]] lines, gives one
   edit from the first change to the last. No edit splits a surrogate pair."
  [^String base ^String target]
  (let [n
        (.length base)

        m
        (.length target)]

    (if-let [{:keys [start delete insert] :as whole} (range-edit base 0 n target 0 m)]
      (let [start (long start)
            delete (long delete)]

        (or (when (and (pos? delete) (pos? (count insert)))
              (let [;; The lines that hold the change, whole.
                    from (inc (.lastIndexOf base 10 (int (dec start))))
                    newline (.indexOf base 10 (int (+ start delete)))
                    to (long (if (neg? newline) n (inc newline)))]

                (line-edits base from to target from (- m (- n to)))))
            [whole]))
      [])))

(defn- long-string?
  [value]
  (and (string? value) (> (count value) (long view-spec/splice-above-chars))))

(defn- nodes-in
  "Node `node` and each node in it."
  [node]
  (tree-seq #(seq (:children %)) :children node))

(defn- patch-bases
  "The long strings that view `before` holds in the nodes that the `set` and
   `remove-node` operations of `ops` name, and in the nodes in them: `[node-id key]`
   to string."
  [before ops]
  (let [ids (into #{} (comp (filter #(contains? #{:set :remove-node} (:op %))) (map :node-id)) ops)]
    (into {}
          (for [named (nodes-in {:children (:nodes before)})
                :when (contains? ids (:id named))
                node (nodes-in named)
                [k value] node
                :when (and (:id node) (long-string? value))]

            [[(:id node) k] value]))))

(defn- spliced
  "Map `values` of node `id`, a `set` operation or a node spec, without each long
   string that `base-of` has a base for, and the splices (view.json `patch_splice`)
   that carry them. `named?` puts the node id in each splice."
  [base-of index id values named?]
  (reduce-kv (fn [[values splices] k value]
               (if-let [base (when (and id (long-string? value)) (base-of id k))]
                 [(dissoc values k)
                  (conj splices
                        (cond-> {:op-index index
                                 :key (wire/wire-key k)
                                 :base-hash (.hashCode ^String base)
                                 :edits (string-edits base value)}
                          named?
                          (assoc :node-id id)))]
                 [values splices]))
             [values []]
             values))

(defn- spliced-spec
  "Node spec `spec` of an `add-node` operation and the nodes in it, as [[spliced]]
   gives them."
  [base-of index spec]
  (let [[spec splices]
        (spliced base-of index (:id spec) spec true)

        children
        (mapv #(spliced-spec base-of index %) (:children spec))]

    [(cond-> spec
       (seq children)
       (assoc :children (mapv first children))) (into splices (mapcat second) children)]))

(defn- delta-of
  "The view delta (view.json `view_delta`) of app frame `frame`. `bases` has the
   long strings of the view before the frame. The base of a string is the value
   that an earlier operation of the frame gave the node, else the string in
   `bases`. A node that the frame adds has no string in `bases`."
  [bases frame]
  (let [{:keys [ops splices]}
        (reduce (fn [{:keys [known fresh] :as acc} [index op]]
                  (let [base-of (fn [id k]
                                  (let [value (cond (contains? known [id k]) (get known [id k])
                                                    (contains? fresh id) nil
                                                    :else (get bases [id k]))]
                                    (when (long-string? value) value)))
                        spec-nodes (when (= :add-node (:op op)) (nodes-in (:node-spec op)))
                        [spliced-op splices] (case (:op op)
                                               :set
                                               (spliced base-of index (:node-id op) op false)

                                               :add-node
                                               (let [[spec splices]
                                                     (spliced-spec base-of index (:node-spec op))]
                                                 [(assoc op :node-spec spec) splices])

                                               [op []])
                        values (case (:op op)
                                 :set
                                 [[(:node-id op) (dissoc op :op :node-id)]]

                                 :add-node
                                 (map (juxt :id #(dissoc % :children)) spec-nodes)

                                 [])]

                    (-> acc
                        (update :ops conj spliced-op)
                        (update :splices into splices)
                        (update :fresh into (map :id) spec-nodes)
                        (assoc :known (reduce (fn [known [id node]]
                                                (reduce-kv (fn [known k value]
                                                             (assoc known [id k] value))
                                                           known
                                                           node))
                                              known
                                              values)))))
                {:known {} :fresh #{} :ops [] :splices []}
                (map-indexed vector (:ops frame)))]
    {:first-seq (:first-seq frame)
     :patch {:view-id (:view-id frame) :seq (:seq frame) :ops ops}
     :splices splices}))

(defn- chars-of
  "The number of characters in the strings of `value`."
  ^long [value]
  (cond (string? value) (count value)
        (map? value) (long (transduce (map chars-of) + 0 (vals value)))
        (sequential? value) (long (transduce (map chars-of) + 0 value))
        :else 0))

(defn- remember-delta!
  "Keep app frame `frame` as a view delta. A frame that does not continue the kept
   frames starts them again. The oldest frames go when the kept frames count for
   more than [[view-spec/delta-chars-kept]] characters."
  [frame]
  (let [delta
        (delta-of (:bases frame) frame)

        size
        (+ (long frame-chars) (chars-of [(get-in delta [:patch :ops]) (:splices delta)]))]

    (swap! deltas update
      (:view-id frame)
      (fn [{:keys [frames chars seq]}]
        (let [continues? (and seq (== (inc (long seq)) (long (:first-seq frame))))]
          (loop [frames (conj (if continues? frames clojure.lang.PersistentQueue/EMPTY)
                              {:delta delta :chars size})
                 chars (+ size (long (if continues? chars 0)))]

            (if (> chars (long view-spec/delta-chars-kept))
              (recur (pop frames) (- chars (long (:chars (peek frames)))))
              {:frames frames :chars chars :seq (:seq frame)})))))
    nil))

(defn deltas-since
  "The view deltas (view.json `view_delta`) that take app `view` from seq `since` to
   its newest published frame, oldest first. Nil when the reader must take the
   whole view: the gateway does not hold these frames, or their strings are not
   shorter than the strings of the view. A reader at the newest frame is behind
   when the app has a newer seq and no frame waits: a new owner changed the icon."
  [view since]
  (let [{:keys [frames seq]}
        (get @deltas (:id view))

        missed
        (drop-while #(not= (inc (long since)) (long (:first-seq (:delta %)))) frames)]

    (cond (nil? seq) nil
          (= since seq) (when (or (= seq (:seq view)) (contains? @buffered (:id view))) [])
          (empty? missed) nil
          (chars-above? view (transduce (map :chars) + 0 missed)) (mapv :delta missed)
          :else nil)))

(defn- publish-patches!
  "Publish one coalesced frame. `first_seq`..`seq` is the range of engine
   patches it carries, so a client can tell a COALESCED jump from a real gap: a
   frame that does not continue the one before it is a resync, not loss. The
   frame of a global app goes to the fleet stream. The frame of an app is kept as
   a view delta first, so a reader that the event wakes finds it."
  [frame]
  (when (and (:is-app frame) (seq (:ops frame))) (remember-delta! frame))
  (when (and (:is-global frame) (seq (:ops frame)))
    (publish-global! gateway-contract/view-patch-event
                     {:kind :live
                      :view-id (:view-id frame)
                      :first-seq (:first-seq frame)
                      :patch {:view-id (:view-id frame) :seq (:seq frame) :ops (:ops frame)}}))
  (when (and (:session-id frame) (seq (:ops frame)))
    (state/append-event! (:session-id frame)
                         gateway-contract/view-patch-event
                         {:kind :live
                          :view-id (:view-id frame)
                          :first-seq (:first-seq frame)
                          :patch {:view-id (:view-id frame) :seq (:seq frame) :ops (:ops frame)}}
                         {:store? (not (:is-app frame)) :mirror? (not (:is-app frame))})))

(defn- flush-view!
  "Publish and forget whatever `view-id` has buffered."
  [view-id]
  (let [[old _] (swap-vals! buffered dissoc view-id)]
    (some-> (get old view-id)
            publish-patches!)))

(defn flush-live-patches!
  "Publish every buffered patch NOW. The tick decides when this happens in a
   running gateway; a caller that needs the stream settled — a test, a shutdown —
   asks for it."
  []
  (let [[old _] (swap-vals! buffered empty)]
    (run! publish-patches! (vals old))
    nil))

(defn- buffer-patch!
  "Hold one accepted patch for `view-id` and make sure a flush is coming. The
   frame of an app is published live only. `before` is the app before the patch:
   the frame keeps the long strings that the patch replaces, for its view delta."
  [sid view-id patch app? global? before]
  ;; The swap that STARTS a frame schedules its flush. A freshness read taken
  ;; before the swap could miss a flush that emptied the buffer in between, and
  ;; then no later patch scheduled one: the view froze while its record grew.
  (let [bases
        (when before (patch-bases before (:ops patch)))

        [old _]
        (swap-vals! buffered
                    update
                    view-id
                    (fn [frame]
                      (let [frame
                            (or frame
                                {:session-id sid
                                 :view-id view-id
                                 :first-seq (:seq patch)
                                 :ops []
                                 :is-app app?
                                 :is-global global?})

                            ;; A node that the frame added has no string before the frame.
                            added
                            (into #{}
                                  (comp (filter #(= :add-node (:op %)))
                                        (mapcat #(nodes-in (:node-spec %)))
                                        (keep :id))
                                  (:ops frame))

                            bases
                            (into {}
                                  (remove (fn [[[id _] _]]
                                            (contains? added id)))
                                  bases)]

                        (-> frame
                            (assoc :seq (:seq patch))
                            ;; The first string of a node and key is the one before the frame.
                            (update :bases #(merge bases %))
                            (update :ops #(reduce coalesce % (:ops patch)))))))]

    (when (nil? (get old view-id))
      (.schedule ^ScheduledExecutorService @flusher
                 ^Runnable
                 (fn []
                   (flush-view! view-id))
                 (long live-flush-ms)
                 TimeUnit/MILLISECONDS))
    nil))

(defn- summary
  "The list entry of app `view` without its body: a list shows the name and the state
   of an app, and the client reads the body when a person opens it. Another view
   stays whole, because a transcript shows it in place."
  [view]
  (if (app/app? view)
    (assoc (apply dissoc view view-spec/live-summary-omitted-keys) :is-summary true)
    view))

(defn live-views
  "The live views of session `sid`, oldest first — the resync a client reads after
   joining late, waking up or losing its stream. A `placement` (`inline`, `panel`
   or `attachment`) keeps only the views it names. An app stays in the list after
   it closes, until it is archived. With `archived?`, the list holds only the
   archived apps. With `summary?`, an app comes without its body."
  ([sid] (live-views sid nil false false))
  ([sid placement] (live-views sid placement false false))
  ([sid placement archived?] (live-views sid placement archived? false))
  ([sid placement archived? summary?]
   (let [sid
         (str sid)

         placement
         (some-> placement
                 name
                 keyword)

         apps
         (filter #(= (boolean archived?) (boolean (:is-archived %))) (view/session-apps sid))

         views
         (when-not archived? (filter #(= sid (session-of %)) (view/live-views)))]

     (->> (concat views apps)
          (filter #(or (nil? placement) (= placement (:placement %))))
          (sort-by :created-at)
          (mapv (cond-> app-surfaces/shown
                  summary?
                  (comp summary)))))))

(defn live-view-of
  "Live view `view-id` when it belongs to `sid`, else nil. Every REST answer goes
   through this: a view id from another session must not be readable from here."
  [sid view-id]
  (let [{:keys [kind view]} (view-of sid view-id)]
    (when (= :live kind) (app-surfaces/shown view))))

(defn- chars-above?
  "True when the strings of `value` hold more than `limit` characters. It stops there."
  [value limit]
  (let [left (volatile! (long limit))]
    (letfn [(above? [x]
              (cond (string? x) (neg? (long (vswap! left
                                                    (fn [n]
                                                      (- (long n) (count x))))))
                    (map? x) (boolean (some above? (vals x)))
                    (sequential? x) (boolean (some above? x))
                    :else false))]
      (above? value))))

(declare global-app-of)

(defn elided-app-patch
  "Wire event `event` as a stream that asked for elided app patches gets it. The
   `view.patch` of an app whose strings hold more characters than
   [[view-spec/patch-elide-above-chars]] leaves out its patch and says so. A client
   that shows the app reads the view again; another client does no work for it.
   Every other event stays as it is."
  [event]
  (if (and (= gateway-contract/view-patch-event (get event "type"))
           (chars-above? (get-in event ["patch" "ops"]) view-spec/patch-elide-above-chars)
           (some-> (if (= "fleet" (get event "scope"))
                     (global-app-of (get event "view_id"))
                     (:view (view-of (get event "session_id") (get event "view_id"))))
                   app/app?))
    (-> event
        (dissoc "patch")
        (assoc "is_elided" true
               "last_seq" (get-in event ["patch" "seq"])))
    event))

(def ^:private summary-omitted-wire-keys
  "The wire names of the keys that the summary of an app leaves out."
  (mapv wire/wire-key view-spec/live-summary-omitted-keys))

(defn summarized-app-open
  "Wire event `event` as a stream that asked for app summaries gets it. The
   `view.open` of an app whose body holds more characters than
   [[view-spec/patch-elide-above-chars]] carries the summary of the app, as a list
   with `summary?` does. A client that has the view at that seq keeps its body;
   another client reads the app when it shows it. Every other event stays as it is."
  [event]
  (let [view (get event "view")]
    (if (and (= gateway-contract/view-open-event (get event "type"))
             (= (name app/placement) (get view "placement"))
             (chars-above? (select-keys view summary-omitted-wire-keys)
                           view-spec/patch-elide-above-chars))
      (assoc event "view" (assoc (apply dissoc view summary-omitted-wire-keys) "is_summary" true))
      event)))

(defn global-apps
  "The global apps, oldest first. A closed app stays listed until it is archived;
   with `archived?`, only the archived apps. With `summary?`, each app comes without
   its body. Empty while `apps_global` is off."
  ([] (global-apps false false))
  ([archived?] (global-apps archived? false))
  ([archived? summary?]
   (if (global-listed?)
     (into []
           (comp (filter #(= (boolean archived?) (boolean (:is-archived %))))
                 (map (if summary? summary identity)))
           (view/global-apps))
     [])))

(defn global-app-of
  "Global app `view-id`, open or closed, else nil. A session view never answers
   here, so the global routes cannot read a session."
  [view-id]
  (when-let [app (or (some-> (view/open-view (str view-id))
                             :view)
                     (view/app-view nil view-id))]
    (when (and (app/app? app) (app/global? app) (nil? (session-of app))) app)))

(def ^:private live-log-page
  "Lines one log page answers: a WINDOW, the same amount a surface holds live
   ([[view-spec/log-defaults]]). It is both the default and the ceiling — a client
   asking for more than a window is asking for the file, and the record IS the
   file."
  (:window-lines view-spec/log-defaults))

(defn live-log-range
  "Read one bounded page of a live or closed log. `query` is a case-insensitive
   literal substring; `from` counts matches, not source lines. Nil paging values
   use the log-window policy, which is also the maximum response size."
  [sid view-id node-id from limit & [query]]
  (sink/log-range (sink/view-file (str sid) (str view-id))
                  node-id
                  (max 0 (long (or from 0)))
                  (min (long live-log-page) (max 1 (long (or limit live-log-page))))
                  query))

;; --- App Sends (a person hands work back to the agent) ---

(def ^:private send-title-chars
  "The most characters of an app title that a Send notification quotes."
  80)

(defn- counted [n noun] (str n " " noun (when-not (= 1 n) "s")))

(defn- send-summary
  "The app title and the size of Send `event`, such as `\"Review\": 2 comments, 1 value`."
  [{:keys [title submission]}]
  (let [title
        (str/trim (str title))

        limit
        (long send-title-chars)]

    (str "\"" (if (> (count title) limit) (str (subs title 0 (dec limit)) "…") title)
         "\": " (counted (count (:comments submission)) "comment")
         ", " (counted (count (:values submission)) "value"))))

(defn- send-request
  "The bounded notification that Send `event` puts into the context of the agent:
   the app, the receipt and the calls that read and answer it, never the review."
  [{:keys [view-id submission] :as event}]
  (let [id (:id submission)]
    (str "App Send: a person sent the app "
         (send-summary event)
         " (view_id "
         view-id
         ", receipt "
         id
         "). Read it with app = attach({\"view_id\": \""
         view-id
         "\", \"media_type\": \""
         app/media-type
         "\"}) and app.submission(\""
         id
         "\"). Answer with app.reply(node_id, body, submission_id=\""
         id
         "\"), or move the receipt with app.set_status(\""
         id
         "\", status, note=...).")))

(defn- deliver-send!
  "Queue one notification turn for Send `event` in the session of its sandbox app,
   then mark the receipt delivered. The session queue applies: a busy session runs
   the turn later. Only an accepted receipt is offered, so a Send that the owner
   already read is not offered twice. A refusal is logged, and the receipt stays
   accepted for a later offer."
  [{:keys [view-id submission] :as event}]
  (let [sid
        (session-of event)

        submission-id
        (:id submission)]

    (try (when (and sid
                    submission-id
                    (= "accepted"
                       (some-> (view/app-view sid view-id)
                               (app/find-submission submission-id)
                               :status)))
           (let [result (state/submit-turn!
                          sid
                          {:request (send-request event)
                           :display-request (str "Sent from the app " (send-summary event))
                           :idempotency-key (str "app-send:" view-id ":" submission-id)})]
             (if (:turn result)
               (view/mark-send-delivered! view-id submission-id)
               (tel/log! {:level :warn
                          :id ::send-not-delivered
                          :data {:session-id sid
                                 :view-id view-id
                                 :submission-id submission-id
                                 :error (:error result)
                                 :message (:message result)}
                          :msg
                          "The session did not take an app Send; its receipt stays accepted"}))))
         (catch Throwable t
           (tel/log!
             {:level :warn
              :id ::send-delivery-failed
              :data
              {:session-id sid :view-id view-id :submission-id submission-id :error (ex-message t)}
              :msg "Vis could not give an app Send to its session"})))))

(defn on-channel-event!
  "Project one canonical `:app` View event into the session journal. A Send of a
   sandbox app becomes a notification turn of its session."
  [event]
  (case (:op event)
    :view/open
    (let [sid (session-of event)]
      (cond (:is-global event)
            (publish-global! gateway-contract/view-open-event
                             {:kind (:kind event) :view-id (:view-id event) :view (:view event)})
            (and sid (= :input (:kind event)))
            (state/append-event! sid
                                 gateway-contract/view-open-event
                                 {:kind :input
                                  :view-id (:view-id event)
                                  :view (assoc (:view event) :origin (origin-of sid))})
            sid (state/append-event!
                  sid
                  gateway-contract/view-open-event
                  {:kind (:kind event) :view-id (:view-id event) :view (:view event)})))

    :view/patch
    (when (= :live (:kind event))
      (buffer-patch! (session-of event)
                     (:view-id event)
                     (:patch event)
                     (boolean (:is-app event))
                     (boolean (:is-global event))
                     (:before event)))

    :view/close
    (let [sid (session-of event)]
      ;; An ending never overtakes buffered live work.
      (when (= :live (:kind event)) (flush-view! (:view-id event)))
      (swap! deltas dissoc (:view-id event))
      (when (:is-global event)
        (publish-global! gateway-contract/view-close-event
                         {:kind (:kind event) :view-id (:view-id event) :result (:result event)}))
      (when sid
        (state/append-event!
          sid
          gateway-contract/view-close-event
          {:kind (:kind event) :view-id (:view-id event) :result (:result event)})))

    :view/send
    (deliver-send! event)

    nil)
  nil)

(defn install!
  "Subscribe the gateway to `:app` View lifecycle events. Idempotent."
  []
  (channel-events/add-channel-event-listener! channel-id listener-id on-channel-event!)
  ;; The saved pictures of owned views come back as stale views.
  (view/restore-owned!)
  nil)

(defn uninstall!
  "Drop the subscription installed by [[install!]]. Anything a view still has
   buffered is published first: a gateway going away must not swallow patches
   the engine already accepted."
  []
  (channel-events/remove-channel-event-listener! channel-id listener-id)
  (flush-live-patches!)
  (view/flush-owned!)
  nil)
