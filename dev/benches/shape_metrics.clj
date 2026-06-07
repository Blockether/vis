(ns benches.shape-metrics
  "VIS-9 measurement — token weight of TOOL RESULT SHAPES, before/after the
   'state-the-shared-thing-once' reshapes (ls groups, git_status code-groups,
   git_blame commit-legend, git_log slim-commit).

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

   Load in the dev nREPL:
     (require 'benches.shape-metrics :reload)
     (benches.shape-metrics/report \"/Users/<you>/vis\")"
  (:require [com.blockether.vis.internal.tokens :as tk]
            [com.blockether.vis.internal.workspace :as ws]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.foundation.editing.core :as ec]
            [com.blockether.vis.ext.foundation-git.core :as gc]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn- inline-blame-legend
  "Invert the commit-legend back to OLD per-line shape: each line re-inlines
   :author/:email/:at from the legend (the repetition legendize removed)."
  [{:keys [commits lines] :as result}]
  (-> result
    (dissoc :commits)
    (assoc :lines
      (mapv (fn [{:keys [line sha content]}]
              (let [c (get commits sha)]
                {:line line :sha sha :content content
                 :author (:author c) :email (:email c) :at (:at c)}))
        lines))))

(defn- fat-status
  "Invert code-grouped `:changes` back to OLD flat porcelain: one row per file,
   the status code repeated on every row."
  [{:keys [changes] :as result}]
  (-> result
    (dissoc :changes)
    (assoc :entries
      (vec (for [[code files] changes, f files] {:status code :file f})))))

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

(defn measure-git-log [root limit]
  (let [f      (io/file root)
        raw    (vec (git/recent-commits f limit {}))           ; canonical (fat)
        new-v  {:branch (:branch (git/status-snapshot f)) :commits (mapv @#'gc/slim-commit raw)}
        old-v  {:branch (:branch new-v) :commits raw}]
    (assoc (weigh (str "git_log " limit) old-v new-v)
      :detail (str (count raw) " commits"))))

(defn measure-git-blame [root rel]
  (let [f     (io/file root)
        raw   (git/blame-file f rel {})
        new-v (@#'gc/legendize-blame raw)
        old-v (inline-blame-legend new-v)]                     ; == raw line shape
    (assoc (weigh (str "git_blame " rel) old-v new-v)
      :detail (str (:total new-v) " lines / " (count (:commits new-v)) " commits"))))

(defn measure-git-status [root]
  (let [f       (io/file root)
        snap    (git/status-snapshot f)
        new-v   (-> snap (dissoc :entries) (assoc :changes (@#'gc/group-status-by-code (:entries snap))))
        old-v   (fat-status new-v)]
    (assoc (weigh "git_status (WT)" old-v new-v)
      :detail (str (count (:entries snap)) " changed files"))))

(defn report
  "Print the before/after token table for the reshaped tools against `root`.
   ls points at the bloated 4clojure bench dir (the ticket trigger)."
  [root]
  (let [rows [(measure-ls root "dev/benches/4clojure" {:depth 4 :limit 600})
              (measure-git-log root 50)
              (measure-git-blame root "src/com/blockether/vis/internal/loop.clj")
              (measure-git-status root)]]
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
