(ns com.blockether.vis.ext.channel-tui.flex
  "Tiny declarative layout layer — the React/Ink flavour for the TUI.

   Instead of threading `col`/`row`/width by hand (the `(reduce (fn [x …] (+ x
   (widget! g x row …))) start items)` pattern repeated across the header,
   footer, tab strip and dialogs), you describe a UI as a tree of measured
   NODES and let `render!` place them.

   A NODE is a map `{:w <cols> :h <rows> :paint (fn [g col row])}`. Every
   builder returns a node, and `row`/`col` are themselves nodes, so layouts
   NEST like React elements:

       (flex/render! g x y
         (flex/row [(flex/text \" help \" {:bold? true})
                    (flex/node w #(button! %1 %2 %3 …))]   ; bridge existing widgets
                   {:gap 1 :align :right :width slot-w}))

   The bridge `node` wraps any existing `foo!` paint-thunk (which already knows
   its own width and click registration), so nothing about the drawing or hover
   contract changes — only the coordinate arithmetic disappears."
  (:require [com.blockether.vis.ext.channel-tui.primitives :as p]))

(set! *unchecked-math* :warn-on-boxed)

;; ── nodes ─────────────────────────────────────────────────────────────────────

(defn node
  "Wrap an explicit size + paint thunk as a layout node. `paint` is
   `(fn [g col row])`; its return value is ignored (the node's width is `w`).
   Height defaults to 1 (single row) — pass `h` for multi-row widgets."
  ([w paint] (node w 1 paint))
  ([w h paint] {:w (max 0 (long w)) :h (max 0 (long h)) :paint paint}))

(defn node?
  "True when `x` is a layout node."
  [x]
  (and (map? x) (contains? x :w) (contains? x :paint)))

(defn width ^long [n] (long (:w n 0)))
(defn height ^long [n] (long (:h n 1)))

(defn text
  "A one-row text node. Paints `s` in optional `:fg`/`:bg` with optional
   `:bold?`. Width is the string's DISPLAY width, so wide glyphs measure right."
  ([s] (text s nil))
  ([s {:keys [fg bg bold?]}]
   (let [s (str s)]
     (node (p/display-width s)
           (fn [g col row]
             (p/clear-styles! g)
             (when (or fg bg) (p/set-colors! g fg bg))
             (when bold? (p/enable! g p/BOLD))
             (p/put-str! g col row s)
             (p/clear-styles! g))))))

(defn spacer
  "A fixed-size empty node — a horizontal (or vertical) gap that paints nothing."
  ([w] (spacer w 1))
  ([w h]
   (node w
         h
         (fn [_ _ _]
           nil))))

(defn- as-node
  [x]
  (cond (nil? x) nil
        (node? x) x
        (string? x) (text x)
        :else (throw (ex-info "not a flex node" {:x x}))))

;; ── combinators ─────────────────────────────────────────────────────────────

(defn row
  "Horizontal box: lay `children` out left→right with `:gap` blank cells
   between them. `children` are nodes (bare strings are lifted to `text`
   nodes; nils are dropped). Returns a NODE whose intrinsic width is the sum
   of child widths plus gaps and whose height is the tallest child.

   With `:width` the row occupies EXACTLY that many cells and `:align`
   (`:left` | `:center` | `:right`, default `:left`) positions the child
   cluster within the slack."
  ([children] (row children nil))
  ([children {:keys [gap align] fixed-w :width :or {gap 0 align :left}}]
   (let
     [gap
      (long gap)

      kids
      (vec (keep as-node children))

      inner
      (long (+ (long (reduce + 0 (map width kids))) (* gap (max 0 (dec (count kids))))))

      w
      (long (if fixed-w (max (long fixed-w) inner) inner))

      h
      (long (reduce max 1 (map height kids)))]

     (node w
           h
           (fn [g col row]
             (let
               [slack
                (max 0 (- w inner))

                pad
                (long (case align
                        :right
                        slack

                        :center
                        (quot slack 2)

                        0))]

               (reduce (fn [x kid]
                         (let [x (long x)]
                           ((:paint kid) g x (long row))
                           (+ x (width kid) gap)))
                       (+ (long col) pad)
                       kids)
               nil))))))

(defn col
  "Vertical box: stack `children` top→bottom, each advanced by its own
   height, with `:gap` blank rows between them. Width is the widest child."
  ([children] (col children nil))
  ([children {:keys [gap] :or {gap 0}}]
   (let
     [gap
      (long gap)

      kids
      (vec (keep as-node children))

      w
      (long (reduce max 0 (map width kids)))

      h
      (long (+ (long (reduce + 0 (map height kids))) (* gap (max 0 (dec (count kids))))))]

     (node w
           h
           (fn [g col row]
             (reduce (fn [r kid]
                       (let [r (long r)]
                         ((:paint kid) g (long col) r)
                         (+ r (height kid) gap)))
                     (long row)
                     kids)
             nil)))))

;; ── render ────────────────────────────────────────────────────────────────────

(defn render!
  "Paint node `n` at absolute cell (col,row) and return its width. Accepts a
   bare string (lifted to a `text` node) or nil (paints nothing, returns 0)."
  ^long [g col row n]
  (if-let [n (as-node n)]
    (do ((:paint n) g (long col) (long row)) (width n))
    0))
