(ns com.blockether.vis.tui.canvas-map
  "Cell layout, local camera, and visible runs of a canvas."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.tui.primitives :as p])
  (:import [com.blockether.vis.render BoundedCache]
           [com.googlecode.lanterna TerminalTextUtils]))

(set! *warn-on-reflection* true)

(def ^:private defs (get (document/schema-document "view") "$defs"))

(def ^:private parent-limit
  (long (get-in defs ["canvas_card" "properties" "parent" "x-vis-max-depth"])))

(def ^:private mark-defaults
  (into
    {}
    (map (fn [k]
           [k (get-in defs ["canvas_edge" "properties" (str/replace (name k) "-" "_") "default"])]))
    [:start-mark :end-mark]))

(defn cards-in
  "Canvas nodes in reading order."
  [view]
  (filterv #(= :canvas (:type %))
    (tree-seq #(seq (:children %)) :children {:children (:nodes view)})))

(defn- frame? [card] (= :frame (keyword (:kind card))))

(defn- frame-parents
  [cards]
  (let [frames
        (into #{} (comp (filter frame?) (map :id)) cards)

        links
        (into {}
              (keep (fn [{:keys [id parent]}]
                      (when (frames parent) [id parent])))
              cards)]

    (into {}
          (filter (fn [[id parent]]
                    (loop [id*
                           parent

                           seen
                           #{id}

                           depth
                           1]

                      (cond (or (seen id*) (> (long depth) parent-limit)) false
                            (links id*) (recur (links id*) (conj seen id*) (inc (long depth)))
                            :else true))))
          links)))

(defn- dependencies
  [card by-id]
  (into #{}
        (keep (fn [{:keys [from]}]
                (when (and (not= (:id card) from) (contains? by-id from)) from)))
        (:layout-edges card)))

(defn- ranks
  "Order each step after its dependencies. Break cycles in declaration order."
  [cards]
  (let [by-id
        (into {} (map (juxt :id identity)) cards)

        ids
        (mapv :id cards)

        incoming
        (into {}
              (map (fn [card]
                     [(:id card) (dependencies card by-id)]))
              cards)

        outgoing
        (reduce-kv (fn [acc id sources]
                     (reduce #(update %1 %2 (fnil conj []) id) acc sources))
                   {}
                   incoming)]

    (loop [ready
           (into clojure.lang.PersistentQueue/EMPTY (filter #(empty? (incoming %))) ids)

           left
           (set ids)

           counts
           (update-vals incoming count)

           result
           {}]

      (if (empty? left)
        result
        (let [id
              (or (peek ready) (first (filter left ids)))

              ready
              (if (seq ready) (pop ready) ready)

              rank
              (or (:column (by-id id))
                  (reduce (fn [rank source]
                            (if-let [r (result source)]
                              (max rank (inc (double r)))
                              rank))
                          0
                          (incoming id)))

              [ready counts]
              (reduce (fn [[q counts] target]
                        (let [pending (dec (long (counts target)))]
                          [(cond-> q
                             (and (zero? pending) (left target))
                             (conj target)) (assoc counts target pending)]))
                      [ready counts]
                      (outgoing id))]

          (recur ready (disj left id) counts (assoc result id rank)))))))

(defn- flat
  [s]
  (-> (str s)
      TerminalTextUtils/sanitizeForPaint
      (str/replace #"[\r\n\t]+" " ")))

(defn- card-body
  [card level width]
  (when-not (= :far level)
    (let [body
          (concat (when-let [s (:meta card)]
                    [s])
                  (str/split-lines (str (:text card)))
                  (keep identity
                        [(get-in card [:picture :alt]) (get-in card [:media :alt])
                         (:caption card)]))

          rows
          (mapcat #(p/word-wrap (flat %) width) body)]

      (into [] (take (if (= :near level) 2000 2)) rows))))

(defn- pack-column
  [cards down?]
  (reduce (fn [[pos boxes] {:keys [w h] :as card}]
            [(+ (long pos) (long (if down? w h)) (if down? 6 3))
             (conj boxes (assoc card (if down? :x :y) pos))])
          [0 []]
          cards))

(defn- pack
  [cards down?]
  (let [rank
        (ranks cards)

        groups
        (->> cards
             (group-by (comp rank :id))
             (sort-by key))]

    (second (reduce (fn [[pos boxes] [_ cards]]
                      (let [[_ packed]
                            (pack-column (sort-by #(or (:row %) 0) cards) down?)

                            cross
                            (if down? :h :w)]

                        [(+ (long pos) (long (reduce max 0 (map cross packed))) (if down? 4 8))
                         (into boxes (map #(assoc % (if down? :y :x) pos)) packed)]))
                    [0 []]
                    groups))))

(defn- extent
  [boxes]
  (reduce (fn [[w h] {:keys [x y] bw :w bh :h}]
            [(max (long w) (+ (long x) (long bw))) (max (long h) (+ (long y) (long bh)))])
          [0 0]
          boxes))

(defn- sized
  [children level down? card]
  (let [frame?
        (frame? card)

        children
        (when frame? (pack children down?))

        [child-w child-h]
        (extent children)

        width
        (case level
          :far
          12

          :near
          46

          26)

        title
        (flat (:title card))

        w
        (long (if frame? (max width (+ 6 (long child-w))) width))

        body
        (card-body card level (- w 4))

        h
        (long (if frame? (+ 3 (long child-h)) (if (= level :far) 1 (+ 3 (count body)))))]

    (assoc card
      :w w
      :h h
      :title title
      :body body
      :children children)))

(defn- frame-dependencies
  [cards parent]
  (let [paths
        (into {}
              (map (fn [{:keys [id]}]
                     [id (vec (reverse (cons id (take-while some? (rest (iterate parent id))))))]))
              cards)]
    (reduce (fn [links {:keys [id edges]}]
              (reduce (fn [links {:keys [from]}]
                        (let [pairs (map vector (paths from) (paths id))
                              [source target] (first (drop-while (fn [[a b]]
                                                                   (= a b))
                                                                 pairs))]

                          (if source (update links target (fnil conj []) {:from source}) links)))
                      links
                      edges))
            {}
            cards)))

(defn- forest
  [cards level down?]
  (let [parent
        (frame-parents cards)

        links
        (frame-dependencies cards parent)

        children
        (group-by #(parent (:id %)) cards)]

    (letfn [(size [card]
              (sized (mapv size (children (:id card)))
                     level
                     down?
                     (assoc card :layout-edges (links (:id card)))))]
      (pack (mapv size (children nil)) down?))))

(defn- positioned
  [boxes left top depth]
  (mapcat (fn [{:keys [x y children] :as box}]
            (let [x
                  (+ (long left) (long x))

                  y
                  (+ (long top) (long y))]

              (cons (assoc (dissoc box :children)
                      :x x
                      :y y
                      :depth depth)
                    (positioned children (+ x 3) (+ y 2) (inc (long depth))))))
          boxes))

(defn- run [x y text kind id] {:x x :y y :w (p/display-width text) :text text :kind kind :id id})

(defn- border-chars
  [kind shape border]
  (case (keyword border)
    :none
    [" " " " " " " " " " " "]

    :double
    ["╔" "╗" "╚" "╝" "═" "║"]

    :thick
    ["┏" "┓" "┗" "┛" "━" "┃"]

    :dashed
    ["┌" "┐" "└" "┘" "╌" "╎"]

    :dotted
    ["┌" "┐" "└" "┘" "┄" "┆"]

    (cond (= :frame (keyword kind)) ["╔" "╗" "╚" "╝" "═" "║"]
          (= :diamond (keyword shape)) ["╱" "╲" "╲" "╱" "─" "│"]
          (#{:pill :ellipse :rounded :cylinder} (keyword shape)) ["╭" "╮" "╰" "╯" "─" "│"]
          :else ["┌" "┐" "└" "┘" "─" "│"])))

(defn- card-runs
  [{:keys [id x y w h kind title body shape border] :as box}]
  (let [x
        (long x)

        y
        (long y)

        w
        (long w)

        h
        (long h)

        [tl tr bl br hz vt]
        (border-chars kind shape border)

        interior
        (- w 2)

        title
        (p/ellipsize title (- w 4))

        content
        (apply str (repeat interior hz))

        bottom
        (str bl content br)]

    (cond
      (= h 1)
      [(run x y (str "[" (p/pad-right (p/ellipsize (:title box) interior) interior) "]") :card id)]
      (frame? box)
      (into [(run x
                  y
                  (str tl " " title " " (apply str (repeat (- w 4 (p/display-width title)) hz)) tr)
                  :card
                  id) (run x (+ y h -1) bottom :card id)]
            (mapcat (fn [row]
                      [(run x row vt :card id) (run (+ x w -1) row vt :card id)]))
            (range (inc y) (+ y h -1)))
      :else (into [(run x y (str tl content tr) :card id) (run x (+ y h -1) bottom :card id)]
                  (map (fn [offset]
                         (let [text
                               (if (zero? (long offset)) title (nth body (dec (long offset)) ""))]
                           (run x
                                (+ y (long offset) 1)
                                (str vt " " (p/pad-right (p/ellipsize text (- w 4)) (- w 4)) " " vt)
                                :card
                                id))))
                  (range (- h 2))))))

(defn- center [{:keys [x y w h]}] [(+ (long x) (quot (long w) 2)) (+ (long y) (quot (long h) 2))])

(defn- ports
  [origin target down?]
  (let [[sx sy]
        (center origin)

        [tx ty]
        (center target)]

    (if down?
      (if (< (long sy) (long ty))
        [[sx (+ (long (:y origin)) (long (:h origin)))] [tx (dec (long (:y target)))]]
        [[sx (dec (long (:y origin)))] [tx (+ (long (:y target)) (long (:h target)))]])
      (if (< (long sx) (long tx))
        [[(+ (long (:x origin)) (long (:w origin))) sy] [(dec (long (:x target))) ty]]
        [[(dec (long (:x origin))) sy] [(+ (long (:x target)) (long (:w target))) ty]]))))

(defn- mark
  [edge k direction]
  (case (keyword (or (edge k) (mark-defaults k)))
    :arrow
    ({:up "↑" :down "↓" :left "←" :right "→"} direction)

    :triangle
    ({:up "▲" :down "▼" :left "◀" :right "▶"} direction)

    :circle
    "○"

    :diamond
    "◇"

    :bar
    (if (#{:up :down} direction) "─" "│")

    nil))

(defn- segment-run
  [[x y] [tx ty] style id]
  (let [x
        (long x)

        y
        (long y)

        tx
        (long tx)

        ty
        (long ty)

        [hz vt]
        (case style
          :dashed
          ["╌" "╎"]

          :dotted
          ["┄" "┆"]

          :double
          ["═" "║"]

          :thick
          ["━" "┃"]

          ["─" "│"])]

    (cond (= y ty) [(run (min x tx) y (apply str (repeat (inc (abs (- tx x))) hz)) :edge id)]
          (= x tx) (mapv #(run x % vt :edge id) (range (min y ty) (inc (max y ty)))))))

(defn- direction
  [[x y] [tx ty]]
  (cond (< (long x) (long tx)) :right
        (> (long x) (long tx)) :left
        (< (long y) (long ty)) :down
        :else :up))

(defn- corner
  [prev at next]
  (case (bit-or (long ({:right 1 :down 2 :left 4 :up 8} (direction at prev)))
                (long ({:right 1 :down 2 :left 4 :up 8} (direction at next))))
    3
    "┌"

    6
    "┐"

    9
    "└"

    12
    "┘"

    5
    "─"

    10
    "│"

    "┼"))

(defn- route
  [source target down?]
  (let [[[x y :as start] [tx ty :as end]]
        (ports source target down?)

        x
        (long x)

        y
        (long y)

        tx
        (long tx)

        ty
        (long ty)]

    (cond (and down? (= y ty)) (let [lane (max 0 (dec y))]
                                 [start [x lane] [tx lane] end])
          down? (let [lane (quot (+ y ty) 2)]
                  (vec (dedupe [start [x lane] [tx lane] end])))
          (= x tx) (let [lane (max 0 (dec x))]
                     [start [lane y] [lane ty] end])
          :else (let [lane (quot (+ x tx) 2)]
                  (vec (dedupe [start [lane y] [lane ty] end]))))))

(defn- between
  [[x y] [tx ty] offset]
  (let [x
        (long x)

        tx
        (long tx)

        y
        (long y)

        ty
        (long ty)

        offset
        (long offset)]

    (if (= x tx) [x (+ y (* (if (< y ty) 1 -1) offset))] [(+ x (* (if (< x tx) 1 -1) offset)) y])))

(defn- edge-runs
  [points edge id]
  (let [[start next]
        points

        end
        (peek points)

        previous
        (nth points (- (count points) 2))

        s
        (mark edge :start-mark (direction next start))

        e
        (mark edge :end-mark (direction previous end))

        segments
        (partition 2 1 points)

        label
        (:label edge)]

    (cond-> (into (into []
                        (mapcat #(segment-run (first %) (second %) (keyword (:line edge)) id))
                        segments)
                  (map (fn [[a [x y :as at] b]]
                         (run x y (corner a at b) :edge id)))
                  (partition 3 1 points))
      s
      (conj (run (first start) (second start) s :edge id))

      e
      (conj (run (first end) (second end) e :edge id))

      label
      (conj (let [[[x y] [tx _]] (or (some (fn [[[x y] [tx ty] :as segment]]
                                             (when (and (= y ty) (> (abs (- (long tx) (long x))) 3))
                                               segment))
                                           segments)
                                     (first segments))]
              (run (inc (min (long x) (long tx)))
                   (max 0 (dec (long y)))
                   (p/ellipsize (flat label) 12)
                   :edge-label
                   id))))))

(defn- edge-plans
  [boxes by-id down?]
  (into []
        (mapcat (fn [{:keys [id edges] :as box}]
                  (keep (fn [{:keys [from line is-active animation] :as edge}]
                          (when-let [source
                                     (and (not= from id) (not= :none (keyword line)) (by-id from))]
                            (let [points (route source box down?)]
                              (when (> (count points) 1)
                                {:runs (edge-runs points edge [from id])
                                 :motion (when is-active
                                           (let [xs (map first points)
                                                 ys (map second points)]

                                             {:points points
                                              :id [from id]
                                              :mode (keyword animation)
                                              :bounds [(reduce min xs) (reduce min ys)
                                                       (reduce max xs) (reduce max ys)]}))}))))
                        edges)))
        boxes))

(defn layout
  "Lay out cards at a cell scale. Coordinates are independent of the camera."
  [cards level flow]
  (let [down?
        (= :down flow)

        boxes
        (into [] (positioned (forest cards level down?) 1 1 0))

        by-id
        (into {} (map (juxt :id identity)) boxes)

        edges
        (edge-plans boxes by-id down?)

        runs
        (let [edges (into [] (mapcat :runs) edges)]
          (into (into (filterv #(= :edge (:kind %)) edges) (mapcat card-runs) boxes)
                (filter #(= :edge-label (:kind %))) edges))

        [w h]
        (extent boxes)]

    {:w (max (inc (long w))
             (reduce (fn [w r]
                       (max (long w) (+ (long (:x r)) (long (:w r)))))
                     1
                     runs))
     :h (max (inc (long h)) (inc (long (reduce max 0 (map :y runs)))))
     :boxes by-id
     :order (mapv :id cards)
     :rows (group-by :y runs)
     :motion (into [] (keep :motion) edges)
     :level level
     :flow flow}))

(def ^:private ^BoundedCache layouts (BoundedCache. 12))

(def ^:private last-cards (atom nil))

(defn- laid-out
  [cards level flow]
  ;; Do not hash large card text again on every idle frame.
  (let [last
        @last-cards

        k
        [level flow]]

    (or (and (identical? cards (:source last)) (get-in last [:plans k]))
        (let [cache-key
              [cards level flow]

              plan
              (or (.get ^BoundedCache layouts cache-key)
                  (let [plan (layout cards level flow)]
                    (.put ^BoundedCache layouts cache-key plan)
                    plan))]

          (swap! last-cards (fn [entry]
                              (if (identical? cards (:source entry))
                                (assoc-in entry [:plans k] plan)
                                {:source cards :plans {k plan}})))
          plan))))

(def ^:private levels [:overview :far :normal :near])

(defn- fraction
  [value total screen]
  (long (Math/round (* (double (max 0 (dec (long screen))))
                       (/ (double value) (double (max 1 (dec (long total)))))))))

(defn- overview
  [plan [width height]]
  (let [boxes
        (update-vals (:boxes plan)
                     (fn [box]
                       (let [[x y] (center box)]
                         (assoc box
                           :x (fraction x (:w plan) width)
                           :y (fraction y (:h plan) height)
                           :w 1
                           :h 1))))

        runs
        (mapv (fn [id]
                (let [{:keys [x y]} (boxes id)]
                  (run x y "•" :card id)))
              (:order plan))]

    (assoc plan
      :level :overview
      :w width
      :h height
      :rows (group-by :y runs)
      :motion []
      :boxes boxes)))

(def ^:private ^BoundedCache overviews (BoundedCache. 8))

(defn- effective-flow
  [node width]
  (case (keyword (:flow node))
    :down
    :down

    :right
    :right

    (if (< (long width) 64) :down :right)))

(defn plan-for
  "Return a layout, including a viewport-sized overview when far tiles cannot fit."
  [node level [w h :as viewport]]
  (let [flow
        (effective-flow node w)

        plan
        (laid-out (:cards node) (if (= :overview level) :far level) flow)]

    (if (and (= :overview level) (or (> (long (:w plan)) (long w)) (> (long (:h plan)) (long h))))
      (let [k [(:rows plan) w h]]
        (or (.get ^BoundedCache overviews k)
            (let [v (overview plan viewport)]
              (.put ^BoundedCache overviews k v)
              v)))
      plan)))

(defn clamp-camera
  "Keep the board on screen and centre an axis that already fits."
  [plan [w h] [x y]]
  (let [clamp (fn [value board screen]
                (let [delta (- (long board) (long screen))]
                  (if (neg? delta) (quot delta 2) (p/clamp value 0 delta))))]
    [(clamp x (:w plan) w) (clamp y (:h plan) h)]))

(defn- focus-cell
  [{:keys [x y w h] :as box} [vw vh]]
  (let [[cx cy] (center box)]
    [(if (> (- (long w) (long vw)) 6) (+ (long x) 2) cx)
     (if (> (long h) (long vh)) (+ (long y) (if (frame? box) 0 1)) cy)]))

(defn- centre-camera
  [plan [w h :as viewport] card]
  (let [[x y] (if-let [box (get (:boxes plan) card)]
                (focus-cell box viewport)
                [(quot (long (:w plan)) 2) (quot (long (:h plan)) 2)])]
    (clamp-camera plan viewport [(- (long x) (quot (long w) 2)) (- (long y) (quot (long h) 2))])))

(defn- camera-spec [node] [(:zoom node) (:focus node) (boolean (:is-locked node))])

(defn- owner-camera
  [st node viewport]
  (let [[zoom owner-focus]
        (camera-spec node)

        zoom
        (double (or zoom 0))

        level
        (cond (zero? zoom) :overview
              (< zoom 0.8) :far
              (> zoom 1.5) :near
              :else :normal)

        plan
        (plan-for node level viewport)

        focus
        (or (when (get-in plan [:boxes owner-focus]) owner-focus)
            (when (get-in plan [:boxes (:focus st)]) (:focus st))
            (first (:order plan)))]

    (assoc st
      :level level
      :position (centre-camera plan viewport (when-not (= :overview level) focus))
      :owner (camera-spec node)
      :viewport viewport
      :id (:id node)
      :flow (:flow plan)
      :manual? false
      :focus focus)))

(def ^:private ^:const glide-ms 150)

(defn camera-at
  "The interpolated top-left cell at `now`."
  [{[x y :as position] :position [fx fy] :from start :started} now]
  (if (and start (< (- (long now) (long start)) (long glide-ms)))
    (let [t
          (/ (double (max 0 (- (long now) (long start)))) (double glide-ms))

          ease
          (- 1.0 (* (- 1.0 t) (- 1.0 t) (- 1.0 t)))]

      [(Math/round (+ (double fx) (* ease (- (double x) (double fx)))))
       (Math/round (+ (double fy) (* ease (- (double y) (double fy)))))])
    position))

(defn- move-to
  [st next now]
  (let [current (camera-at st now)]
    (cond (= next (:position st)) st
          (= next current) (dissoc (assoc st :position next) :from :started)
          :else (assoc st
                  :position next
                  :from current
                  :started now))))

(defn gliding? [st now] (and (:started st) (< (- (long now) (long (:started st))) (long glide-ms))))

(defn- visible-camera
  [plan [w h :as viewport] [x y :as position] focus]
  (if-let [box (get (:boxes plan) focus)]
    (let [[cx cy] (focus-cell box viewport)
          keep (fn [p v size extent midpoint]
                 (let [p (long p)
                       v (long v)
                       size (long size)
                       extent (long extent)]

                   (if (> extent size)
                     (- (long midpoint) (quot size 2))
                     (min v (max p (- (+ v extent) size))))))]

      (clamp-camera plan
                    viewport
                    [(keep x (:x box) w (:w box) cx) (keep y (:y box) h (:h box) cy)]))
    (clamp-camera plan viewport position)))

(defn reconcile
  "Follow an owner change unless the local camera has moved. Keep focus on resize."
  [st node viewport now]
  (let [spec
        (camera-spec node)

        st
        (if (or (not= (:id node) (:id st)) (not (:position st)))
          (owner-camera (select-keys st [:focus]) node viewport)
          (if (and (not= (:owner st) spec) (or (:is-locked node) (not (:manual? st))))
            (let [next (owner-camera st node viewport)]
              (move-to (assoc next :position (:position st)) (:position next) now))
            st))

        plan
        (plan-for node (:level st) viewport)

        focus
        (if (get-in plan [:boxes (:focus st)]) (:focus st) (first (:order plan)))

        resize?
        (or (not= viewport (:viewport st)) (not= (:flow plan) (:flow st)))

        position
        (if (and resize? (not (:is-locked node)))
          (visible-camera plan viewport (:position st) focus)
          (if resize?
            (centre-camera plan viewport (:focus node))
            (clamp-camera plan viewport (:position st))))]

    (cond-> (assoc st
              :focus focus
              :viewport viewport
              :flow (:flow plan)
              :position position
              :owner spec)
      (or resize? (not (gliding? st now)))
      (dissoc :from :started))))

(defn reset-camera
  [st node viewport now]
  (let [next (owner-camera {} node viewport)]
    (move-to (assoc next :position (:position st)) (:position next) now)))

(defn pan
  [st node viewport dx dy now]
  (if (:is-locked node)
    st
    (let [plan
          (plan-for node (:level st) viewport)

          [x y]
          (:position st)

          position
          (clamp-camera plan viewport [(+ (long x) (long dx)) (+ (long y) (long dy))])]

      (if (= position (:position st)) st (assoc (move-to st position now) :manual? true)))))

(declare hit-card)

(defn zoom
  "Change scale about a visible focused card, or about the specified screen cell."
  [st node [w h :as viewport] delta point now]
  (if (:is-locked node)
    st
    (let [index
          (.indexOf ^java.util.List levels (:level st))

          level
          (nth levels (p/clamp (+ (long index) (long delta)) 0 (dec (count levels))))]

      (if (= level (:level st))
        st
        (let [old
              (plan-for node (:level st) viewport)

              plan
              (plan-for node level viewport)

              [x y]
              (camera-at st now)

              [x y]
              [(long x) (long y)]

              focus
              (get-in old [:boxes (:focus st)])

              [cx cy]
              (when focus (focus-cell focus viewport))

              anchor?
              (and (nil? point)
                   focus
                   (<= x (long cx) (+ x (dec (long w))))
                   (<= y (long cy) (+ y (dec (long h)))))

              [px py]
              (or point
                  (if anchor?
                    [(- (long cx) x) (- (long cy) y)]
                    [(quot (long w) 2) (quot (long h) 2)]))

              world-x (+ x (long px))
              world-y (+ y (long py))
              under-pointer (when point (hit-card old [world-x world-y]))
              old-box (get-in old [:boxes under-pointer])
              new-box (get-in plan [:boxes under-pointer])
              [wx wy]
              (cond anchor? (focus-cell (get-in plan [:boxes (:focus st)]) viewport)
                    new-box [(+ (long (:x new-box)) (fraction (- world-x (long (:x old-box))) (:w old-box) (:w new-box)))
                             (+ (long (:y new-box)) (fraction (- world-y (long (:y old-box))) (:h old-box) (:h new-box)))]
                    :else [(fraction world-x (:w old) (:w plan))
                           (fraction world-y (:h old) (:h plan))])

              next
              (clamp-camera plan viewport [(- (long wx) (long px)) (- (long wy) (long py))])]

          (-> st
              (assoc :level level
                     :position next
                     :manual? true)
              (dissoc :from :started)))))))

(defn fit
  [st node viewport now]
  (if (:is-locked node)
    st
    (let [plan (plan-for node :overview viewport)]
      (move-to (assoc st
                 :level :overview
                 :manual? true)
               (centre-camera plan viewport nil)
               now))))

(defn card-visible?
  "True if part of the card is inside the displayed camera."
  [plan st [w h :as viewport] id now]
  (when-let [{x :x y :y bw :w bh :h} (get-in plan [:boxes id])]
    (let [[left top] (clamp-camera plan viewport (camera-at st now))]
      (and (< (long x) (+ (long left) (long w)))
           (< (long y) (+ (long top) (long h)))
           (< (long left) (+ (long x) (long bw)))
           (< (long top) (+ (long y) (long bh)))))))

(defn focus-card
  [st node viewport id now]
  (let [plan (plan-for node (:level st) viewport)]
    (if (or (nil? id) (not (contains? (:boxes plan) id))
            (and (:is-locked node) (not (card-visible? plan st viewport id now))))
      st
      (let [st (assoc st :focus id)
            position (visible-camera plan viewport (:position st) id)]

        (if (:is-locked node)
          st
          (cond-> (move-to st position now)
            (not= position (:position st))
            (assoc :manual? true)))))))

(defn tab-card
  [st node viewport step now]
  (let [plan (plan-for node (:level st) viewport)
        order (cond->> (:order plan)
                (:is-locked node) (filterv #(card-visible? plan st viewport % now)))
        index (.indexOf ^java.util.List order (:focus st))]
    (if (seq order)
      (focus-card
        st
        node
        viewport
        (nth order (mod (+ (if (and (= -1 index) (neg? (long step))) 0 index) (long step)) (count order)))
        now)
      st)))

(defn hit-card
  [plan [x y]]
  (reduce-kv (fn [found id {bx :x by :y :keys [w h depth]}]
               (if (and (<= (long bx) (long x) (+ (long bx) (dec (long w))))
                        (<= (long by) (long y) (+ (long by) (dec (long h))))
                        (or (nil? found)
                            (>= (long depth) (long (get-in plan [:boxes found :depth])))))
                 id
                 found))
             nil
             (:boxes plan)))

(defn- crop-text
  [^String text ^long start ^long width]
  (if (zero? start)
    (p/truncate-cols text width)
    (let [prefix
          (p/col-prefix-end text start)

          cols
          (p/display-width (.substring text 0 prefix))]

      (if (= cols start)
        (p/truncate-cols (.substring text prefix) width)
        ;; A wide cell crosses the left edge. Keep its visible columns blank.
        (let [end
              (p/col-prefix-end text (inc start))

              gap
              (min width (- (p/display-width (.substring text 0 end)) start))]

          (str (apply str (repeat gap " "))
               (p/truncate-cols (.substring text end) (- width gap))))))))

(defn- crop-run
  [{:keys [x y w text] :as r} left top right]
  (let [x
        (long x)

        left
        (long left)

        right
        (long right)

        start
        (max x left)

        end
        (min (+ x (long w)) right)]

    (when (< start end)
      (assoc r
        :x (- start left)
        :y (- (long y) (long top))
        :w (- end start)
        :text (if (and (= start x) (= end (+ x (long w))))
                text
                (crop-text text (- start x) (- end start)))))))

(defn- motion-run
  [{:keys [points id mode]} step]
  (let [[a b]
        (nth (vec (partition 2 1 points)) (mod (quot (long step) 3) (dec (count points))))

        length
        (+ (abs (- (long (first a)) (long (first b))))
           (abs (- (long (second a)) (long (second b)))))

        offset
        (quot (* (max 0 (dec length)) (inc (rem (long step) 3))) 4)

        [x y]
        (between a b offset)]

    (when (> length 2)
      (run x
           y
           (case mode
             :dash
             "═"

             :dot
             "●"

             :pulse
             "◆"

             "•")
           :edge
           id))))

(defn- visible-motion
  [plan left top width height]
  (let [right
        (+ (long left) (long width))

        bottom
        (+ (long top) (long height))]

    (filter (fn [{[x0 y0 x1 y1] :bounds}]
              (and (< (long x0) right)
                   (< (long y0) bottom)
                   (>= (long x1) (long left))
                   (>= (long y1) (long top))))
            (:motion plan))))

(defn moving?
  "True if a moving arrow can enter the current viewport."
  [plan st [w h :as viewport] now]
  (let [[left top] (clamp-camera plan viewport (camera-at st now))]
    (boolean (seq (visible-motion plan left top w h)))))

(defn runs
  "Crop cached runs to the camera. Paint edges, then cards, then focused overview marks."
  [plan st [w h :as viewport] now step]
  (let [[left top]
        (clamp-camera plan viewport (camera-at st now))

        right
        (+ (long left) (long w))

        visible
        (mapcat (:rows plan) (range (long top) (+ (long top) (long h))))

        motion
        (when step (keep #(motion-run % step) (visible-motion plan left top w h)))

        focus
        (:focus st)

        visible
        (if (= :overview (:level plan))
          (cond-> (remove #(= focus (:id %)) visible)
            (get-in plan [:boxes focus])
            (concat (let [{:keys [x y]} (get-in plan [:boxes focus])]
                      [(run x y "◆" :card focus)])))
          (if (seq motion)
            (concat (filter #(= :edge (:kind %)) visible)
                    motion
                    (remove #(= :edge (:kind %)) visible))
            visible))]

    (into [] (keep #(crop-run % left top right)) visible)))

(defn selected-run?
  [r focus selected]
  (if (#{:edge :edge-label} (:kind r))
    (boolean (some #{focus} (:id r)))
    (or (= focus (:id r)) (contains? selected (:id r)))))

(defn paint!
  "Paint the visible map without measuring card content."
  [g plan st [left top] viewport now step inks selected]
  (doseq [{:keys [x y text kind] :as r} (runs plan st viewport now step)]
    (p/set-colors! g
                   (get inks (cond (selected-run? r (:focus st) selected) :selected
                               (= kind :edge-label) :edge
                               :else kind))
                   (:paper inks))
    (p/put-str! g (+ (long left) (long x)) (+ (long top) (long y)) text)))
