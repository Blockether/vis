(ns benches.shape-metrics
  "VIS-9 measurement — token weight of TOOL RESULT SHAPES, before/after the
   'state-the-shared-thing-once' reshapes.

   The reshapes were shipped and asserted compact by inspection; this turns the
   assertion into a NUMBER. We measure the model-facing structured value (what
   rides the trailer), not the TUI render — using the engine's OWN counter
   (`tokens/count-pr-tokens`, cl100k_base over pr-str), the same one
   `bound-form-result` uses to decide trailer clipping. So the delta is in the
   exact units the engine budgets in.

   Method: for each tool we obtain the SHIPPED (new) value from live repo data,
   then reconstruct the OLD flat value by INVERTING the reshape (un-group /
   inline the legend / keep all derivable fields). The inverse is lossless —
   same information, just the pre-reshape layout — so the delta isolates layout
   cost, nothing else.

   NOTE: the git_log / git_blame / git_status rows were retired together with
   the JGit-backed foundation-git extension (git is now a single shell-out
   tool). Only the `ls` group reshape remains measurable here.

   Load in the dev nREPL:
     (require 'benches.shape-metrics :reload)
     (benches.shape-metrics/report \"/Users/<you>/vis\")"
  (:require [com.blockether.vis.internal.tokens :as tk]
            [com.blockether.vis.internal.workspace :as ws]
            [com.blockether.vis.internal.foundation.editing.core :as ec]
            [clojure.java.io :as io]))

;; --- inverses: reconstruct the OLD (pre-reshape) flat shape, losslessly ---

(defn- ungroup-ls
  "Invert `:groups` back to the OLD flat `:entries` shape: every file carries
   its FULL directory prefix again and every dir row carries `:size nil` — the
   exact prefix-duplication the grouping removed. Pre-order is preserved."
  [{:keys [groups path] :as result}]
  (let [entries (vec
                  (mapcat
                    (fn [{:keys [dir files]}]
                      (concat
                        (when (not= dir path) [{:path dir :type :dir :size nil}])
                        (map (fn [{:keys [name size]}]
                               {:path (str dir "/" name) :type :file :size size})
                          files)))
                    groups))]
    (-> result (dissoc :groups) (assoc :entries entries))))

;; --- per-tool measurement against live repo data ---

(defn- weigh [tool old-v new-v]
  (let [o (tk/count-pr-tokens old-v)
        n (tk/count-pr-tokens new-v)]
    {:tool tool :old-tokens o :new-tokens n :saved (- o n)
     :pct (if (zero? o) 0.0 (* 100.0 (/ (double (- o n)) o)))}))

(defn measure-ls [root rel opts]
  (binding [ws/*workspace-root* (io/file root)]
    (let [new-v (@#'ec/list-files rel opts)]
      (assoc (weigh (str "ls " rel) (ungroup-ls new-v) new-v)
        :detail (str (:entry-count new-v) " entries / " (count (:groups new-v)) " groups")))))

(defn report
  "Print the before/after token table for the reshaped tools against `root`.
   ls points at the bloated 4clojure bench dir (the ticket trigger)."
  [root]
  (let [rows [(measure-ls root "dev/benches/4clojure" {:depth 4 :limit 600})]]
    (println "VIS-9 TOOL-RESULT SHAPE — token weight (cl100k, pr-str), old vs new")
    (println (apply str (repeat 78 "─")))
    (printf "%-34s %8s %8s %8s %7s\n" "tool" "old" "new" "saved" "pct")
    (doseq [r rows]
      (printf "%-34s %8d %8d %8d %6.1f%%   %s\n"
        (:tool r) (:old-tokens r) (:new-tokens r) (:saved r) (:pct r) (:detail r)))
    (println (apply str (repeat 78 "─")))
    (let [o (reduce + (map :old-tokens rows)) n (reduce + (map :new-tokens rows))]
      (printf "%-34s %8d %8d %8d %6.1f%%\n" "TOTAL" o n (- o n)
        (* 100.0 (/ (double (- o n)) o))))
    rows))
