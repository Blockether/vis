(ns com.blockether.vis.tui.time-axis
  "The round marks of a time axis, shared by the live `timeline` node and the agent dock.")

(def ^:private axis-steps
  "The round steps of a timeline axis, in seconds — the Companion's `AXIS_STEPS`,
   so both surfaces mark the same times. The axis takes the least step that gives
   fewer than five marks."
  [10 15 30 60 120 300 600 900 1800 3600 7200])

(defn axis-marks
  "The marks of a time axis `ms` long, in ms from its start, on one round step."
  [ms]
  (let [seconds
        (/ (double (max 0 (long ms))) 1000.0)

        step
        (long (or (some #(when (< (/ seconds (double %)) 5.0) %) axis-steps)
                  (* 3600 (long (Math/ceil (/ seconds 4.0 3600.0))))))]

    (mapv #(* (long %) step 1000) (range (inc (long (Math/floor (/ seconds (double step)))))))))
