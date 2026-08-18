(ns com.blockether.vis.internal.human-input.live-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.human-input.live :as live]
            [com.blockether.vis.internal.human-input.spec :as hs]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- view
  "A materialized view carrying `nodes`; everything else is what the engine
   stamps on one."
  [& nodes]
  (live/materialize {:id "v"
                     :title "Watching a CI run"
                     :session-id "s"
                     :channel-ids [:tui]
                     :nodes (vec nodes)
                     :timeout-ms 0
                     :seq 0
                     :created-at "2026-08-17T19:52:46Z"}))

(defn- patched
  "`v` after one patch carrying `ops`. Every patch advances `:seq`, exactly as
   the engine stamps it."
  [v & ops]
  (live/apply-patch v {:view-id "v" :seq (inc (long (:seq v))) :ops (vec ops)}))

(defn- node "The node `id` addresses." [v id] (first (filter #(= id (:id %)) (:nodes v))))

(defn- refusal
  "The one line the engine refused with, or nil when it accepted."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- table-node
  [order]
  {:id "t"
   :type :table
   :columns [{:id "name" :label "Name"} {:id "n" :label "N" :align :right}]
   :rows []
   :max-rows 8
   :order order})

(defn- row [id name n] {:id id :cells [name n]})

(defn- row-ids
  "The row ids of table `t` in the order the table DECLARED — what every
   surface paints."
  [v]
  (mapv :id (live/ordered-rows (node v "t"))))

(defn- log-node
  ([] (log-node 4))
  ([window] {:id "log" :type :log :label "Output" :lines [] :window-lines window}))

(defdescribe
  materializer-test
  (it "refuses an op that names a node the view has not got, listing the ones it has"
      (let
        [v
         (view (log-node) {:id "run" :type :status :text "queued" :tone :idle})

         why
         (refusal #(patched v {:op :append :node-id "nope" :lines ["x"]}))]

        (expect (str/includes? why "no such node"))
        (expect (str/includes? why "log"))
        (expect (str/includes? why "run"))))
  (it "refuses a patch that does not advance the view's seq, so a replay cannot land twice"
      (let
        [v
         (patched (view (log-node)) {:op :append :node-id "log" :lines ["one"]})

         why
         (refusal #(live/apply-patch
                     v
                     {:view-id "v" :seq 1 :ops [{:op :append :node-id "log" :lines ["one"]}]}))]

        (expect (str/includes? why "does not advance"))
        (expect (= ["one"] (:lines (node v "log"))))))
  (it "refuses a patch addressed to another view"
      (expect (str/includes? (refusal #(live/apply-patch (view (log-node))
                                                         {:view-id "other"
                                                          :seq 1
                                                          :ops [{:op :clear :node-id "log"}]}))
                             "names view other")))
  (it "is all or nothing: a patch whose second op refuses leaves the view it was handed"
      (let
        [v
         (view (log-node))

         before
         (patched v {:op :append :node-id "log" :lines ["kept"]})]

        (expect (refusal #(patched before
                                   {:op :append :node-id "log" :lines ["lost"]}
                                   {:op :append :node-id "gone" :lines ["lost"]})))
        (expect (= ["kept"] (:lines (node before "log"))))))
  (it "refuses a set carrying a key the node type has not got, naming what it does set"
      (let
        [why (refusal #(patched (view {:id "run" :type :status :text "queued" :tone :idle})
                                {:op :set :node-id "run" :value 0.5}))]
        (expect (str/includes? why "status node has no value to set"))
        (expect (str/includes? why "text"))))
  (it "refuses appending rows to a log and lines to a table, naming both"
      (let [v (view (log-node) (table-node :insertion))]
        (expect (str/includes?
                  (refusal #(patched v {:op :append :node-id "log" :rows [(row "a" "A" "1")]}))
                  "appends lines, not rows"))
        (expect (str/includes? (refusal #(patched v {:op :append :node-id "t" :lines ["x"]}))
                               "appends rows, not lines"))))
  (it "treats removing an absent row and clearing an empty table as no-ops that still advance seq"
      (let
        [v (-> (view (table-node :insertion))
               (patched {:op :remove :node-id "t" :item-ids ["never-there"]})
               (patched {:op :clear :node-id "t"}))]
        (expect (= [] (row-ids v)))
        (expect (= 2 (:seq v)))))
  (it "adds a node mid-run after the one it names, and refuses a second node with the same id"
      (let
        [v (-> (view (log-node) (table-node :insertion))
               (patched {:op :add-node
                         :after "log"
                         :node-spec {:id "extra" :type :status :text "scanning" :tone :running}}))]
        (expect (= ["log" "extra" "t"] (mapv :id (:nodes v))))
        (expect (nil? (hs/live-view-error v)))
        (expect (str/includes? (refusal #(patched
                                           v
                                           {:op :add-node
                                            :node-spec
                                            {:id "extra" :type :status :text "again" :tone :idle}}))
                               "already in the view"))))
  (it "drops a node idempotently but refuses a WRITE to one that is gone"
      (let
        [v (-> (view (log-node) (table-node :insertion))
               (patched {:op :remove-node :node-id "t"})
               (patched {:op :remove-node :node-id "t"}))]
        (expect (= ["log"] (mapv :id (:nodes v))))
        (expect (str/includes? (refusal
                                 #(patched v {:op :append :node-id "t" :rows [(row "a" "A" "1")]}))
                               "no such node"))))
  (it
    "grows and drops a node inside a ROW, because a group is layout — not another address space"
    (let
      [v
       (view
         {:id "state" :type :status :text "queued" :tone :idle}
         {:id "reading" :type :group :direction :row :fields [(log-node) (table-node :insertion)]})

       beside
       (fn [w]
         (mapv :id (:fields (second (:nodes w)))))

       grown
       (patched v
                {:op :add-node
                 :after "log"
                 :node-spec {:id "extra" :type :status :text "scanning" :tone :running}})

       written
       (patched grown {:op :append :node-id "t" :rows [(row "a" "A" "1")]})

       cut
       (patched v {:op :remove-node :node-id "reading"})]

      (expect (= ["log" "extra" "t"] (beside grown))
              "`after` names a SIBLING, so the node lands in that sibling's own row")
      (expect (nil? (hs/live-view-error grown)))
      (expect (= ["a"] (mapv :id (:rows (last (:fields (second (:nodes written)))))))
              "and a patch reaches a nested node by its id alone")
      (expect (= ["state"] (mapv :id (:nodes cut)))
              "dropping a row takes the nodes it was holding with it")
      (expect (str/includes? (refusal
                               #(patched cut {:op :append :node-id "t" :rows [(row "b" "B" "2")]}))
                             "no such node"))))
  (it
    "refuses a table past its bound with the bound, the node and the log named — and trims nothing"
    (let
      [v
       (patched
         (view (table-node :insertion))
         {:op :append :node-id "t" :rows (mapv #(row (str %) (str "row " %) (str %)) (range 8))})

       why
       (refusal #(patched v {:op :append :node-id "t" :rows [(row "9" "one too many" "9")]}))]

      (expect (str/includes? why "at most 8 items"))
      (expect (str/includes? why "`log`"))
      (expect (str/includes? why "t"))
      (expect (= 8 (count (:rows (node v "t")))))))
  (it "refuses one over-full patch with `split it` rather than a bound on the node"
      (let
        [why (refusal #(patched (view (log-node 100000))
                                {:op :append
                                 :node-id "log"
                                 :lines (mapv str
                                              (range (inc (long (:max-patch-lines
                                                                  hs/log-defaults)))))}))]
        (expect (str/includes? why "one patch carries at most"))
        (expect (str/includes? why "split it"))))
  (it "keeps a log's WINDOW while counting the record, and clearing empties the window only"
      (let
        [v (-> (view (log-node 4))
               (patched {:op :append :node-id "log" :lines ["1" "2" "3"]})
               (patched {:op :append :node-id "log" :lines ["4" "5" "6"]}))]
        (expect (= ["3" "4" "5" "6"] (:lines (node v "log"))))
        (expect (= 6 (:total-lines (node v "log"))))
        (let [cleared (patched v {:op :clear :node-id "log"})]
          (expect (= [] (:lines (node cleared "log"))))
          (expect (= 6 (:total-lines (node cleared "log")))))))
  (it "keeps the materialized view legal after every patch"
      (let
        [v (-> (view (log-node)
                     (table-node :insertion)
                     {:id "p" :type :progress :label "Jobs"}
                     {:id "s" :type :stat :label "Score" :stats []}
                     {:id "k" :type :steps :label "Steps" :steps []}
                     {:id "l" :type :link :label "Links" :links []})
               (patched
                 {:op :set :node-id "p" :value 0.5 :done 9 :total 18}
                 {:op :append
                  :node-id "s"
                  :stats [{:id "ok" :label "passed" :value-text "9" :tone :ok}]}
                 {:op :append :node-id "k" :steps [{:id "one" :label "Set up job" :tone :ok}]}
                 {:op :append
                  :node-id "l"
                  :links
                  [{:id "run" :label "Run" :target-kind :url :target "https://example.com/run"}]}))]
        (expect (nil? (hs/live-view-error v))))))

;; A table is the one node whose shape the human keeps reading while it moves,
;; so the same script is asserted row by row under each declared order.
(def ^:private script
  "add a, add b, add c, update b, remove a, re-add a, clear, add d."
  [{:op :append :node-id "t" :rows [(row "a" "Alpha" "3")]}
   {:op :append :node-id "t" :rows [(row "b" "Bravo" "1")]}
   {:op :append :node-id "t" :rows [(row "c" "Charlie" "2")]}
   {:op :append :node-id "t" :rows [(row "b" "Bravo (running)" "1")]}
   {:op :remove :node-id "t" :item-ids ["a"]}
   {:op :append :node-id "t" :rows [(row "a" "Alpha again" "3")]}])

(defn- run-script
  "The view after each step of [[script]], so a stage can be asserted by index."
  [order]
  (reductions (fn [v op]
                (patched v op))
              (view (table-node order))
              script))

(defdescribe
  table-order-test
  (it "in insertion order an update keeps the row's slot and a re-add arrives at the END"
      (let [stages (vec (run-script :insertion))]
        (expect (= ["a"] (row-ids (nth stages 1))))
        (expect (= ["a" "b" "c"] (row-ids (nth stages 3))))
        (expect (= ["a" "b" "c"] (row-ids (nth stages 4))))
        (expect (= "Bravo (running)"
                   (first (:cells (first (filter #(= "b" (:id %))
                                                 (:rows (node (nth stages 4) "t"))))))))
        (expect (= ["b" "c"] (row-ids (nth stages 5))))
        (expect (= ["b" "c" "a"] (row-ids (nth stages 6))))))
  (it "newest-first is the mirror of insertion, from the same record"
      (let [stages (vec (run-script :newest-first))]
        (expect (= ["c" "b" "a"] (row-ids (nth stages 3))))
        (expect (= ["a" "c" "b"] (row-ids (nth stages 6))))))
  (it
    "a declared column order sorts numerically when every cell is a number, ties keeping insertion order"
    (let [stages (vec (run-script {:by "n" :dir :asc}))]
      (expect (= ["b" "c" "a"] (row-ids (nth stages 3))))
      (expect (= ["b" "c" "a"] (row-ids (nth stages 6))))))
  (it "a descending order reverses it and still keeps blanks last"
      (let
        [v (-> (view (table-node {:by "n" :dir :desc}))
               (patched {:op :append
                         :node-id "t"
                         :rows [(row "a" "Alpha" "3") (row "blank" "Blank" "")
                                (row "b" "Bravo" "10")]}))]
        (expect (= ["b" "a" "blank"] (row-ids v)))))
  (it "sorts as text when the column is not numeric, case-insensitively"
      (let
        [v (-> (view (table-node {:by "name"}))
               (patched
                 {:op :append :node-id "t" :rows [(row "a" "beta" "1") (row "b" "Alpha" "2")]}))]
        (expect (= ["b" "a"] (row-ids v)))))
  (it "refuses an order naming a column the table does not declare, at declaration"
      (expect (some? (hs/live-node-error (table-node {:by "nope"}))))))

(def ^:private ci-view
  "The shape an extension watching a CI run declares: what is happening, how far
   it has got, the score, one row per job, the failing step's output, and the
   pointer the human opens."
  (->
    (view {:id "run"
           :type :status
           :text "3 of 18 jobs failed"
           :tone :error
           :detail "workflow CI on main"}
          {:id "jobs" :type :progress :label "Jobs" :value 1.0 :done 18 :total 18}
          {:id "score"
           :type :stat
           :label "Score"
           :stats [{:id "passed" :label "passed" :value-text "15" :tone :ok}
                   {:id "failed" :label "failed" :value-text "3" :tone :error}]}
          {:id "steps"
           :type :steps
           :label "Failing job"
           :steps [{:id "1" :label "Set up job" :tone :ok}
                   {:id "2" :label "Run tests" :tone :error :detail "181 assertions, 2 failed"}
                   {:id "3" :label "Upload artifacts" :tone :idle}]}
          (assoc (table-node :insertion)
            :id "t"
            :label "Jobs"
            :columns [{:id "job" :label "Job"} {:id "took" :label "Took" :align :right}])
          {:id "log" :type :log :label "Failing step" :lines [] :window-lines 2000}
          {:id "links"
           :type :link
           :label "Links"
           :links [{:id "run"
                    :label "Run on GitHub"
                    :target-kind :url
                    :target "https://github.com/Blockether/vis/actions/runs/1"}
                   {:id "log-file" :label "Saved log" :target-kind :path :target "target/ci.log"}]})
    (patched {:op :append
              :node-id "t"
              :rows [{:id "1" :cells ["tests / ubuntu-latest" "13m0s"] :tone :error}
                     {:id "2" :cells ["classpath / vis-channel-tui" "21s"] :tone :ok}]}
             {:op :append :node-id "log" :lines ["FAIL in (live-view-test)" "expected: 2 rows"]})))

;; The model's surface is DATA. The human watches the stream and reads the
;; document; the model is handed the picture as values, so nothing it acts on has
;; to be recovered from a sentence.

(defdescribe
  picture-test
  (it "hands the model the ids it declared, so a node it read is a node it can patch"
      (expect (= ["run" "jobs" "score" "steps" "t" "log" "links"]
                 (mapv :id (:nodes (:view (live/picture ci-view)))))))
  (it "hands over values, not prose: a tone is a keyword and a stat is its own number"
      (let [score (node (:view (live/picture ci-view)) "score")]
        (expect (= :error (:tone (node (:view (live/picture ci-view)) "run"))))
        (expect (= [{:id "passed" :label "passed" :value-text "15" :tone :ok}
                    {:id "failed" :label "failed" :value-text "3" :tone :error}]
                   (:stats score)))))
  (it "leaves the mount behind: a picture is the title, the description and the nodes"
      (expect (= #{:title :nodes} (set (keys (:view (live/picture ci-view)))))))
  (it "gives every node back as a node the engine would accept"
      (expect (every? nil? (map hs/live-node-error (:nodes (:view (live/picture ci-view)))))))
  (it "gives the model the log's TAIL and counts what stayed in the record"
      (let
        [v
         (patched (view (log-node 100000))
                  {:op :append :node-id "log" :lines (mapv #(str "line " %) (range 300))})

         {:keys [view elided]}
         (live/picture v)]

        (expect (= 120 (count (:lines (node view "log")))))
        (expect (= "line 180" (first (:lines (node view "log")))))
        (expect (= 300 (:total-lines (node view "log"))))
        (expect (= [{:node-id "log" :items 180}] elided))))
  (it "budgets a table the same way, and says how many rows it left"
      (let
        [v
         (patched
           (view (table-node :insertion))
           {:op :append :node-id "t" :rows [(row "a" "A" "1") (row "b" "B" "2") (row "c" "C" "3")]})

         {:keys [view elided]}
         (live/picture v {:table-rows 2})]

        (expect (= ["a" "b"] (mapv :id (:rows (node view "t")))))
        (expect (= [{:node-id "t" :items 1}] elided))))
  (it
    "FLATTENS the rows a surface arranges, because the model reads content, not layout"
    (let
      [v
       (view
         {:id "state" :type :status :text "queued" :tone :idle}
         {:id "reading" :type :group :direction :row :fields [(log-node) (table-node :insertion)]})

       md
       (live/->markdown v)]

      (expect (= ["state" "log" "t"] (mapv :id (:nodes (:view (live/picture v)))))
              "a group is not a node the model reads — its children are")
      (expect (not (str/includes? md "reading")))
      (expect (= md (live/->markdown (:view (live/parse-markdown md))))
              "so the document read back renders identically, flat")))
  (it
    "applies the order the table declared and then says `insertion`, so mounting the picture again cannot sort it twice"
    (let
      [v
       (patched
         (view (table-node :newest-first))
         {:op :append :node-id "t" :rows [(row "a" "A" "1") (row "b" "B" "2") (row "c" "C" "3")]})

       t
       (node (:view (live/picture v)) "t")]

      (expect (= ["c" "b" "a"] (mapv :id (:rows t))))
      (expect (= :insertion (:order t)))))
  (it "says nothing about elisions when the budget cut nothing"
      (expect (empty? (:elided (live/picture ci-view))))))
(defdescribe
  markdown-test
  (it "renders the whole view, verdict first, in one markdown document"
      (expect
        (= (str/join "\n"
                     ["# Watching a CI run" ""
                      "> **interrupted** — this view did not finish · stopped after 18 jobs" ""
                      "[error] **3 of 18 jobs failed**" "_workflow CI on main_" "" "### Jobs"
                      "**100%** · 18/18 done" "" "### Score"
                      "**passed** 15 [ok] · **failed** 3 [error]" "" "### Failing job"
                      "- [ok] Set up job" "- [error] Run tests — 181 assertions, 2 failed"
                      "- [idle] Upload artifacts" "" "### Jobs" "| ! | Job | Took |"
                      "| --- | --- | ---: |" "| error | tests / ubuntu-latest | 13m0s |"
                      "| ok | classpath / vis-channel-tui | 21s |" "" "### Failing step" "```"
                      "FAIL in (live-view-test)" "expected: 2 rows" "```" "" "### Links"
                      "- [Run on GitHub](https://github.com/Blockether/vis/actions/runs/1)"
                      "- Saved log — `target/ci.log`"])
           (live/->markdown ci-view
                            {:result {:view-id "v"
                                      :is-completed false
                                      :reason :interrupted
                                      :summary "stopped after 18 jobs"}}))))
  (it "renders without a verdict while the view is still open"
      (let [md (live/->markdown ci-view)]
        (expect (str/starts-with? md "# Watching a CI run\n\n[error]"))
        (expect (not (str/includes? md "interrupted")))))
  (it "gives the model the log's TAIL and says how many lines it left in the record"
      (let
        [v
         (patched (view (log-node 100000))
                  {:op :append :node-id "log" :lines (mapv #(str "line " %) (range 100))})

         md
         (live/->markdown v {:log-tail-lines 3})]

        (expect (str/includes? md "line 97\nline 98\nline 99"))
        (expect (not (str/includes? md "line 96")))
        (expect (str/includes? md "_… 97 earlier lines — the view's record keeps them all_"))))
  (it "gives the model a window of a long table and says how many rows it left"
      (let
        [v
         (patched
           (view (assoc (table-node :insertion) :max-rows 100))
           {:op :append :node-id "t" :rows (mapv #(row (str %) (str "row " %) (str %)) (range 30))})

         md
         (live/->markdown v {:table-rows 2})]

        (expect (str/includes? md "| row 1 | 1 |"))
        (expect (not (str/includes? md "| row 2 | 2 |")))
        (expect (str/includes? md "_… 28 more rows — the view's record keeps them all_"))))
  (it "grows the leading tone column only when a row carries a tone"
      (let
        [plain (patched (view (table-node :insertion))
                        {:op :append :node-id "t" :rows [(row "a" "Alpha" "1")]})]
        (expect (str/includes? (live/->markdown plain) "| Name | N |"))
        (expect (not (str/includes? (live/->markdown plain) "| ! |")))))
  (it "keeps a cell that carries a pipe or a newline inside its own row"
      (let
        [v (patched (view (table-node :insertion))
                    {:op :append :node-id "t" :rows [(row "a" "a | b\nc" "1")]})]
        (expect (str/includes? (live/->markdown v) "| a \\| b c | 1 |"))))
  (it "fences a log around whatever backticks its lines carry"
      (let
        [v (patched (view (log-node 10))
                    {:op :append :node-id "log" :lines ["```" "still the log"]})]
        (expect (str/includes? (live/->markdown v) "````\n```\nstill the log\n````"))))
  (it "says so plainly when a node has nothing in it yet"
      (let
        [md (live/->markdown (view (log-node)
                                   (table-node :insertion)
                                   {:id "s" :type :stat :label "Score" :stats []}))]
        (expect (str/includes? md "_no output yet_"))
        (expect (str/includes? md "_no rows yet_"))
        (expect (str/includes? md "_nothing counted yet_"))))
  (it "renders an indeterminate progress as work, not as zero"
      (expect (str/includes? (live/->markdown
                               (view {:id "p" :type :progress :label "Scanning" :done 7}))
                             "_working_ · 7 done")))
  (it "counts a progress that declared its parts, instead of calling it indeterminate"
      (let [md (live/->markdown (view {:id "p" :type :progress :label "Jobs" :done 15 :total 18}))]
        (expect (str/includes? md "**83%** · 15/18 done"))
        (expect (not (str/includes? md "_working_")))
        ;; and the law holds for it: a document that says 83% parses back to a
        ;; view that renders 83% again.
        (expect (= md (live/->markdown (:view (live/parse-markdown md))))))))

;; The model's surface is TWO-WAY. Everything below leans on one law: a picture
;; that elided nothing renders back exactly, so `->markdown` and
;; `parse-markdown` cannot drift apart without a test here going red.

(defn- rich-view
  "One view painting every node type, carrying what breaks a naive render: a pipe
   and a separator inside a cell, a fence inside a log, two nodes sharing a
   label, and a row that stops before the last column."
  []
  (view {:id "run" :type :status :label "Run" :text "completed" :tone :ok :detail "18 jobs"}
        {:id "pace" :type :progress :label "Jobs" :value 0.5 :done 9 :total 18}
        {:id "score"
         :type :stat
         :label "Score"
         :stats [{:id "p" :label "passed" :value-text "18" :tone :ok}
                 {:id "f" :label "failed" :value-text "0 · none" :tone :warn}]}
        {:id "phases"
         :type :steps
         :label "Phases"
         :steps [{:id "a" :label "checkout" :tone :ok :detail "12s"}
                 {:id "b" :label "test" :tone :running :value 0.42}]}
        {:id "out"
         :type :log
         :label "Output"
         :window-lines 2000
         :lines ["$ clojure -M:test" "``` fenced inside" "done"]}
        {:id "jobs"
         :type :table
         :label "Jobs"
         :max-rows 5000
         :order :insertion
         :columns [{:id "job" :label "Job"} {:id "took" :label "Took" :align :right}]
         :rows [{:id "r1" :cells ["tests | ubuntu\nlatest" "13m0s"] :tone :ok}
                {:id "r2" :cells ["lint"] :tone :warn}]}
        {:id "links"
         :type :link
         :label "Links"
         :links
         [{:id "l1" :label "run" :target-kind :url :target "https://example.com/run" :tone :ok}
          {:id "l2" :label "output" :target-kind :path :target "/tmp/out.txt"}
          {:id "l3" :label "artifact" :target-kind :attachment :target "att-1"}]}))

(defn- repainted
  "`view` rendered, read back, and rendered again — the two pictures, so a test
   compares what the model would actually be handed."
  ([v] (repainted v nil))
  ([v result]
   (let
     [md
      (live/->markdown v {:result result})

      back
      (live/parse-markdown md)]

     {:md md :again (live/->markdown (:view back) {:result (:result back)}) :back back})))

(defdescribe
  markdown-round-trip-test
  (it "reads its own picture back as the view that painted it, node for node"
      (let
        [{:keys [md again back]} (repainted (rich-view)
                                            {:view-id "v"
                                             :is-completed false
                                             :reason :interrupted
                                             :summary "the human stopped watching"
                                             :error "gh exited 1"})]
        (expect (= md again))
        (expect (empty? (:elided back)))
        (expect (= [:status :progress :stat :steps :log :table :link]
                   (mapv :type (:nodes (:view back)))))
        (expect (= ["Run" "Jobs" "Score" "Phases" "Output" "Jobs" "Links"]
                   (mapv :label (:nodes (:view back)))))
        ;; Two nodes share a label, so the second earns a numbered address rather
        ;; than colliding with the first.
        (expect (= ["run" "jobs" "score" "phases" "output" "jobs-2" "links"]
                   (mapv :id (:nodes (:view back)))))
        (expect (= "Watching a CI run" (:title (:view back))))))
  (it "gives every parsed node back to the engine as a node the engine accepts"
      (let [{:keys [back]} (repainted (rich-view))]
        (expect (every? nil? (map hs/live-node-error (:nodes (:view back)))))))
  (it
    "carries the verdict both ways, telling the summary from what went wrong"
    (let
      [{:keys [back]} (repainted (rich-view)
                                 {:view-id "v"
                                  :is-completed false
                                  :reason :failed
                                  :summary "3 of 18 jobs failed"
                                  :error "gh exited 1"})]
      (expect
        (= {:is-completed false :reason :failed :summary "3 of 18 jobs failed" :error "gh exited 1"}
           (:result back)))))
  (it "paints an empty state that names its own type, and a table keeps its columns"
      (let
        [{:keys [md again back]} (repainted (view {:id "s" :type :stat :label "Score" :stats []}
                                                  {:id "t" :type :steps :label "Phases" :steps []}
                                                  (log-node)
                                                  (table-node :insertion)
                                                  {:id "k" :type :link :label "Links" :links []}
                                                  {:id "p" :type :progress :label "Scanning"}))]
        (expect (= md again))
        (expect (= [:stat :steps :log :table :link :progress] (mapv :type (:nodes (:view back)))))
        ;; A table with no rows still says what it is watching: the header is the
        ;; declaration, and `_no rows yet_` goes UNDER it.
        (expect (= ["Name" "N"] (mapv :label (:columns (nth (:nodes (:view back)) 3)))))
        (expect (= [:right] (keep :align (:columns (nth (:nodes (:view back)) 3)))))))
  (it "keeps a pipe inside a cell and flattens the newline that would end the row"
      (let
        [{:keys [back]}
         (repainted (rich-view))

         table
         (nth (:nodes (:view back)) 5)]

        (expect (= ["tests | ubuntu latest" "13m0s"] (:cells (first (:rows table)))))
        ;; The row that stopped short is painted to the full width, so it reads back that way.
        (expect (= ["lint" ""] (:cells (second (:rows table)))))
        (expect (= [:ok :warn] (mapv :tone (:rows table))))))
  (it "repaints a truncated log exactly, because the count of what scrolled past is stamped"
      (let
        [{:keys [md again back]}
         (repainted (view (assoc (log-node) :lines (mapv #(str "line " %) (range 300)))))

         node
         (first (:nodes (:view back)))]

        (expect (= md again))
        (expect (= 300 (:total-lines node)))
        (expect (= 120 (count (:lines node))))
        (expect (= [{:node-id "output" :items 180}] (:elided back)))))
  (it "says which rows a budget left behind rather than pretending it holds them"
      (let
        [{:keys [back]} (repainted (view (assoc (table-node :insertion)
                                           :label "Jobs"
                                           :max-rows 5000
                                           :rows (mapv (fn [i]
                                                         (row (str "r" i) (str i) "1"))
                                                       (range 60)))))]
        (expect (= [{:node-id "jobs" :items 10}] (:elided back)))
        (expect (= 50 (count (:rows (first (:nodes (:view back)))))))))
  (it "hands a hand-written picture to the engine, addressed by what its labels earn"
      (let
        [authored
         (str
           "# Deploying\n_three hosts_\n\n" "### Progress\n**0%** · 0/3 done\n\n"
           "### Hosts\n| Host | State |\n| --- | --- |\n| alpha | queued |\n| beta | queued |\n\n"
           "### Output\n_no output yet_")

         declared
         (:view (live/parse-markdown authored))

         running
         (live/apply-patch (merge (view) declared)
                           {:view-id "v"
                            :seq 1
                            :ops [{:op :append
                                   :node-id "hosts"
                                   :rows [{:id "alpha" :cells ["alpha" "done"] :tone :ok}]}
                                  {:op :append :node-id "output" :lines ["alpha: ok"]}]})]

        (expect (= ["progress" "hosts" "output"] (mapv :id (:nodes declared))))
        (expect (= "three hosts" (:description declared)))
        ;; A row is addressed by the cell the eye reads first, so the patch lands
        ;; ON the row it names instead of appending a second `alpha`.
        (expect (= ["alpha" "beta"] (mapv :id (:rows (node running "hosts")))))
        (expect (str/includes? (live/->markdown running) "| ok | alpha | done |"))
        (expect (str/includes? (live/->markdown running) "alpha: ok"))))
  (it "refuses a picture no view could have painted, naming the line to fix"
      (expect (= "a view opens with `# <title>`" (refusal #(live/parse-markdown "not a title"))))
      (expect (= "no live node paints this: \"not a node at all\""
                 (refusal #(live/parse-markdown "# T\n\n### X\nnot a node at all"))))
      (expect (= "a heading with nothing under it paints no node"
                 (refusal #(live/parse-markdown "# T\n\n### X\n"))))
      (expect (= "a log's code fence is never closed"
                 (refusal #(live/parse-markdown "# T\n\n### Out\n```\nstill going"))))
      (expect (= "a row paints more cells than the table declares: 2 against 1"
                 (refusal #(live/parse-markdown "# T\n\n### X\n| A |\n| --- |\n| a | b |"))))
      (expect (str/starts-with? (str (refusal #(live/parse-markdown
                                                 "# T\n\n> **exploded**\n\n### X\n[ok] **hi**")))
                                "no view ends \"exploded\""))))
