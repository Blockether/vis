(ns com.blockether.vis.internal.python.readiness
  "What a turn waits for before its first iteration: the extensions of its scopes (#498).
   `loop/send!` binds one cell for each turn. The extension load marks the cell while the
   turn waits for another load or prepares a package. The gateway reads the cell to show the wait.")

(set! *warn-on-reflection* true)

(def ^:dynamic *cell*
  "The readiness cell of the turn on this thread: an atom of nil or
   `{:stage :waiting|:preparing :package name-or-nil :since-ms ms}`. Nil outside a turn."
  nil)

(defn cell
  "A new empty readiness cell for one turn."
  []
  (atom nil))

(defn mark!
  "Record that the turn of this thread waits (`:waiting`) or prepares (`:preparing`) the package
   `package`, nil when no package is known. A repeated mark of the same stage and package keeps
   its first time. Without a turn cell this does nothing."
  [stage package]
  (when-let [c *cell*]
    (swap! c (fn [current]
               (if (and current (= stage (:stage current)) (= package (:package current)))
                 current
                 {:stage stage :package package :since-ms (System/currentTimeMillis)})))))

(defn marking*
  "Call `f` with the turn cell marked as `stage` and `package`, then restore the earlier mark."
  [stage package f]
  (if-let [c *cell*]
    (let [earlier @c]
      (mark! stage package)
      (try (f) (finally (reset! c earlier))))
    (f)))

(defmacro marking
  "Run `body` with the turn cell marked as `stage` and `package`; see [[marking*]]."
  [stage package & body]
  `(marking* ~stage ~package (fn [] ~@body)))
