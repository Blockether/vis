;; The debug log of an app, as a dialog.
(ns com.blockether.vis.tui.app-log
  "The debug log of an app: what its owner and Vis wrote, oldest first.

   The rows, the chip label and the keys are pure. [[show!]] runs the dialog and
   reads the newest copy of the app on each frame, so new entries appear while the
   dialog is open."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* :warn-on-boxed)

(def ^:private level-rank {"debug" 0 "info" 1 "warn" 2 "error" 3})

(def filters
  "The level filters in the order that `f` steps through: the lowest level shown."
  [nil "info" "warn" "error"])

(defn next-filter
  "The filter after `current`. The last filter goes back to all entries."
  [current]
  (let [i (.indexOf ^java.util.List filters current)]
    (nth filters (mod (inc (long i)) (count filters)))))

(defn error-count
  "How many entries of `log` have the level error."
  ^long [log]
  (count (filter #(= "error"
                     (some-> (:level %)
                             name))
                 log)))

(defn chip-label
  "The chrome label of an app with `log`, or nil for an empty log."
  [log]
  (when (seq log)
    (let [n (error-count log)]
      (if (pos? n)
        (str " Log (" n (if (= 1 n) " error" " errors") ") ")
        (str " Log (" (count log) ") ")))))

(defn visible
  "The entries of `log` that filter `min-level` shows, oldest first."
  [log min-level]
  (let [floor (long (get level-rank min-level 0))]
    (filterv #(>= (long (get level-rank
                             (some-> (:level %)
                                     name)
                             0))
                  floor)
      log)))

(def ^:private clock (DateTimeFormatter/ofPattern "HH:mm:ss.SSS"))

(defn- clock-text
  [at]
  (.format ^DateTimeFormatter clock
           (.atZone (Instant/ofEpochMilli (long (or at 0))) (ZoneId/systemDefault))))

(def message-min
  "Message columns that a log row keeps before the source and the level get shorter."
  16)

(defn entry-columns
  "The columns of a log row `w` cells wide: `:source?` and `:level` width. The message
   gets its room first: a narrow row drops the source, then shortens the level to one letter."
  [w]
  (let [room (fn [level-w source?]
               (- (long w) 14 (long level-w) (if source? 6 0)))]
    (cond (>= (long (room 5 true)) (long message-min)) {:source? true :level 5}
          (>= (long (room 5 false)) (long message-min)) {:source? false :level 5}
          :else {:source? false :level 1})))

(defn entry-text
  "One entry as one line `w` cells wide: time to the millisecond, level, source and
   message. [[entry-columns]] decides which columns fit."
  ([entry] (entry-text entry Long/MAX_VALUE))
  ([{:keys [at level source message detail]} w]
   (let [{:keys [source?] level-w :level}
         (entry-columns w)

         level-text
         (str/upper-case (name (or level "info")))]

     (str (clock-text at)
          "  "
          (if (= 1 (long level-w)) (subs level-text 0 1) (format "%-5s" level-text))
          "  "
          (when source? (str (format "%-4s" (name (or source "app"))) "  "))
          (str/replace (str message) #"\s*\n\s*" " ")
          (when detail "  ▸")))))

(defn rows
  "The rows of the dialog: one row for each shown entry, and the lines of its detail
   under an expanded entry. `state` holds `:expanded` (a set of seq) and `:filter`.
   `w` is the row width in cells."
  ([log state] (rows log state Long/MAX_VALUE))
  ([log {:keys [expanded filter]} w]
   (into []
         (mapcat (fn [{:keys [seq detail level] :as entry}]
                   (cons {:kind :entry
                          :seq seq
                          :level (some-> level
                                         name)
                          :text (entry-text entry w)}
                         (when (and detail (contains? expanded seq))
                           (map (fn [line]
                                  {:kind :detail :seq seq :text (str "    " line)})
                                (str/split-lines detail))))))
         (visible log filter))))

(defn- entry-indexes
  [rows]
  (into []
        (keep-indexed (fn [i r]
                        (when (= :entry (:kind r)) i)))
        rows))

(defn- toggle-detail
  [state rows]
  (if-let [s (:seq (nth rows (:selected state) nil))]
    (update state :expanded #(if (contains? % s) (disj % s) (conj % s)))
    state))

(defn- move-selection
  [state stops direction]
  (let [at
        (or (first (keep-indexed (fn [k i]
                                   (when (= i (:selected state)) k))
                                 stops))
            0)

        next-index
        (p/clamp (+ (long at) (long direction)) 0 (max 0 (dec (count stops))))]

    (assoc state :selected (nth stops next-index 0))))

(defn handle-key
  "The next dialog state for `key`, or `{::dlg/done nil}` to close."
  [state ^KeyStroke key rows]
  (let [kt
        (.getKeyType key)

        ch
        (when (= KeyType/Character kt) (.getCharacter key))

        stops
        (entry-indexes rows)

        step
        (fn [d]
          (move-selection state stops d))]

    (cond (= KeyType/Escape kt) {::dlg/done nil}
          (= KeyType/ArrowUp kt) (step -1)
          (= KeyType/ArrowDown kt) (step 1)
          (= KeyType/Home kt) (step (- (count stops)))
          (= KeyType/End kt) (step (count stops))
          (= KeyType/Enter kt) (toggle-detail state rows)
          (= \f ch) (assoc state
                      :filter (next-filter (:filter state))
                      :selected 0
                      :scroll 0)
          :else state)))

(defn- level-ink
  [level]
  (case level
    "error"
    t/status-bad

    "warn"
    t/warning-fg

    "debug"
    t/dialog-hint

    t/dialog-fg))

(defn reconcile-state
  "`state` after the rows change: the selection stays on an entry row, else moves to
   the last entry, and the scroll keeps it inside `content-h` rows."
  [state rows content-h]
  (let [stops
        (entry-indexes rows)

        sel
        (long (or (some #{(:selected state)} stops) (last stops) 0))

        h
        (long content-h)

        scroll
        (-> (long (:scroll state))
            (min sel)
            (max (inc (- sel h)))
            (p/clamp 0 (max 0 (- (count rows) h))))]

    (assoc state
      :selected sel
      :scroll scroll)))

(def ^:private hint [["↑/↓" "move"] ["Enter" "detail"] ["f" "level"] ["Esc" "close"]])

(defn hint-pairs
  "The hint pairs that fit `inner-w`. `Esc close` always stays."
  [inner-w]
  (dlg/fit-hint-keeping-last hint (max 0 (- (long inner-w) 2))))

(defn- row-width
  "Cells of a log row inside a dialog `inner-w` wide."
  ^long [inner-w]
  (max 1 (- (long inner-w) 3)))

(defn component
  "The `dlg/run-modal!` component of the log of the app that `current` returns."
  [current]
  {:init {:selected 0 :scroll 0 :expanded #{} :filter nil}
   :measure (fn [state cols rows-count]
              (let [view
                    (current)

                    content-w
                    (dlg/default-content-width cols)

                    bounds
                    (dlg/dialog-bounds cols rows-count content-w (max 8 (- (long rows-count) 8)))

                    layout
                    (dlg/dialog-layout bounds)]

                (merge layout
                       {:cols cols
                        :rows-count rows-count
                        :content-w content-w
                        :bounds bounds
                        :view view
                        :rows (rows (:log view) state (row-width (:inner-w bounds)))})))
   :reconcile (fn [state {:keys [rows content-h]}]
                (reconcile-state state rows content-h))
   :paint (fn [g state
               {:keys [cols rows-count content-w bounds content-top content-h hint-row view rows]}]
            (let [{:keys [left inner-w]}
                  bounds

                  left
                  (long left)

                  top
                  (long content-top)

                  w
                  (row-width inner-w)]

              (dlg/draw-dialog-chrome! g
                                       cols
                                       rows-count
                                       (str "Log · "
                                            (or (not-empty (str (:title view))) "App")
                                            (when-let [f (:filter state)]
                                              (str " · " f " and above")))
                                       content-w
                                       (max 8 (- (long rows-count) 8)))
              (p/set-colors! g t/dialog-fg t/dialog-bg)
              (p/fill-rect! g (inc left) top inner-w content-h)
              (if (empty? rows)
                (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                    (p/put-str! g (+ left 2) top (p/ellipsize "The filter shows no entries" w)))
                (dotimes [i (min (long content-h) (- (count rows) (long (:scroll state))))]
                  (let [idx (+ (long (:scroll state)) i)
                        row (nth rows idx)
                        text (p/ellipsize (:text row) w)]

                    (if (= idx (:selected state))
                      (dlg/draw-selectable-row! g left (+ top i) inner-w true (str " " text))
                      (do (p/set-colors!
                            g
                            (if (= :detail (:kind row)) t/dialog-hint (level-ink (:level row)))
                            t/dialog-bg)
                          (p/put-str! g (+ left 2) (+ top i) text))))))
              (dlg/draw-hint-bar! g left hint-row inner-w (hint-pairs inner-w))
              nil))
   :on-key (fn [state key {:keys [rows]}]
             (if (instance? KeyStroke key) (handle-key state key rows) state))})

(defn show!
  "Open the log of the app that `current` returns until the human closes it."
  [^TerminalScreen screen current]
  (dlg/run-modal! screen (component current)))
