(ns com.blockether.vis.ext.channel-tui.components-test
  (:require
   [com.blockether.vis.ext.channel-tui.components :as comps]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- well-formed-line?
  "A LINE is a non-empty vec of SEGs; a SEG is [text color bold?]."
  [line]
  (and (vector? line)
    (seq line)
    (every? (fn [seg] (and (vector? seg) (string? (first seg)))) line)))

(defn- line-w-survives?
  "Mirror the F2 panel's line-w: map display-width over (first seg). Must not
   throw — a bare seg leaking in as a line made (first seg) a Character."
  [line]
  (number? (reduce + 0 (map (comp p/display-width first) line))))

(defn- line-text [line] (apply str (map first line)))

(defn- has-line?
  [pattern lines]
  (some #(re-find pattern (line-text %)) lines))

(defdescribe context-overlay-lines-test
  (describe "task-overlay-lines emits well-formed LINES (regression: TUI freeze)"
    ;; The acceptance sub-line was spliced with `into`, leaking a bare
    ;; [text color bold] SEG where a LINE (vec-of-segs) belongs. The F2 panel's
    ;; line-w then mapped display-width over (first seg) = a Character →
    ;; ClassCastException every render frame → frozen TUI. Every element must
    ;; be a vec of segs so line-w only ever sees strings.
    ;; Cards are now MULTI-ROW (wrapped title + meta row + optional
    ;; acceptance/deps sub-lines + blank spacer). Exact counts are an
    ;; implementation detail of the layout; the load-bearing invariants are
    ;; that EVERY row stays a well-formed vec-of-segs and line-w never sees a
    ;; bare Character — that is what guards against the freeze regression.
    (it "a task WITH :acceptance yields a well-formed multi-row card"
      (let [lines (#'comps/task-overlay-lines
                   {:work {:title "do x" :status :doing
                           :acceptance "x compiles; tests pass"}}
                   60)]
        (expect (>= (count lines) 3))
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))))

    (it "a task WITHOUT :acceptance yields a well-formed multi-row card"
      (let [lines (#'comps/task-overlay-lines {:work {:title "y" :status :done}} 60)]
        (expect (>= (count lines) 2))
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))))

    (it "empty tasks yields a well-formed placeholder line"
      (let [lines (#'comps/task-overlay-lines {} 60)]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines)))))

  (describe "fact-overlay-lines emits well-formed LINES"
    (it "facts (with + without :files) and the empty case stay well-formed"
      (let [lines (#'comps/fact-overlay-lines
                   {:a {:content "alpha" :status :active
                        :files [{:path "x.clj"}]}
                    :b {:content "beta" :status :superseded}}
                   60 #{})
            empty-lines (#'comps/fact-overlay-lines {} 60 #{})]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))
        (expect (every? well-formed-line? empty-lines)))))

  (describe "dag-overlay-lines emits a bounded revision card"
    (it "renders task graph, advance state, and typed relationships as well-formed lines"
      (let [lines (#'comps/dag-overlay-lines
                   {:tracked? true
                    :revision-id "12345678-abcd"
                    :checkpoint? true
                    :undo? true
                    :redo-count 2
                    :task-count 2
                    :fact-count 1
                    :node-count 3
                    :edge-count 2
                    :root-count 1
                    :workspace-change-count 4
                    :task-update-count 1
                    :fact-update-count 1
                    :parent-revision-id "87654321-parent"
                    :redo-revision-ids ["abcdef12-child"]
                    :updated-at-ms 1234
                    :answered? true
                    :advance-tasks ["render"]
                    :advance-facts ["verified"]
                    :nodes [{:id "task:goal" :kind :task :status :doing
                             :label "Ship the DAG viewer" :born "t1/i1/f1"}
                            {:id "task:render" :kind :task :status :done
                             :label "Render verbose DAG" :parent "task:goal"
                             :depends-on ["fact:verified"]
                             :acceptance "F2 shows lineage, nodes, edges, and file changes"
                             :evidence "130 TUI tests pass" :verified? true}
                            {:id "fact:verified" :kind :fact :status :active
                             :label "Focused tests pass"}]
                    :edges [{:from "task:render" :relation :parent :to "task:goal"}
                            {:from "task:render" :relation :depends-on :to "fact:verified"}]
                    :workspace-changes [{:status :modify :path "components.clj"
                                         :before-sha256 "before"
                                         :after-sha256 "after"}]
                    :truncated-node-count 0
                    :truncated-edge-count 0
                    :truncated-change-count 0}
                   64)]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))
        (expect (has-line? #"ADVANCE" lines))
        (expect (has-line? #"RELATIONSHIPS" lines))
        (expect (has-line? #"answer included" lines))
        (expect (has-line? #"task:render" lines))
        (expect (has-line? #"↳ .*fact:verified" lines))
        (expect (not (has-line? #"[├└]─ .*fact:verified" lines)))
        (expect (has-line? #"task:render needs -> fact:verified" lines))
        (expect (has-line? #"fact:verified" lines))
        (expect (has-line? #"modify  components.clj" lines))))

    (it "does not duplicate a flat task list in the graph section"
      (let [lines (#'comps/dag-overlay-lines
                   {:task-count 2
                    :fact-count 0
                    :node-count 2
                    :edge-count 0
                    :root-count 2
                    :nodes [{:id "task:a" :kind :task :status :todo}
                            {:id "task:b" :kind :task :status :doing}]
                    :edges []
                    :workspace-changes []}
                   64)]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))
        (expect (has-line? #"No task relationships yet" lines))
        (expect (not (has-line? #"[├└]─ .*task:a" lines)))
        (expect (not (has-line? #"[├└]─ .*task:b" lines))))))

  (describe "policy and evidence sections"
    (it "policy obligation rows are well formed and expose policy metadata"
      (let [lines (#'comps/policy-task-lines
                   {"policy.obligation.foundation-bridge.unit"
                    {:title "Policy obligation: unit-tests"
                     :status :todo
                     :policy/provider "foundation-bridge"
                     :policy/status "open"
                     :policy/evidence-kind "unit-tests"
                     :policy/subject "vis"
                     :policy/evidence-id "unit"
                     :policy/required-evidence ["unit-tests"]}}
                   72)]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))
        (expect (has-line? #"Policy obligation: unit-tests" lines))
        (expect (has-line? #"policy status open" lines))
        (expect (has-line? #"requires unit-tests" lines))))

    (it "policy evidence rows are well formed and expose receipt metadata"
      (let [lines (#'comps/policy-fact-lines
                   {"policy.evidence.foundation-bridge.unit"
                    {:content "Policy evidence `unit` reported failed for unit-tests."
                     :status :active
                     :policy/provider "foundation-bridge"
                     :policy/evidence-id "unit"
                     :policy/status "failed"
                     :policy/evidence-kind "unit-tests"
                     :policy/role "required"
                     :policy/subject "vis"
                     :policy/stale? true
                     :policy/receipt-path ".bridge/ephemeral/evidence/unit.yaml"}}
                   72)]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))
        (expect (has-line? #"unit  ·  failed" lines))
        (expect (has-line? #"path" lines))
        (expect (has-line? #"\.bridge/ephemeral/evidence/unit.yaml" lines)))))

  (describe "observation/evidence summary rows"
    (it "observation rows show compact persisted read/search state"
      (let [lines (#'comps/observation-lines
                   {:files [{:path "src/core.clj"
                             :scope "t1/i1/f1"
                             :range [1 8]
                             :summary "src/core.clj lines 1..8"
                             :covered_by "t1/i1/f0"}]
                    :searches [{:scope "t1/i1/f2"
                                :summary "rg 2 hits in 1 file"
                                :repeat_of "t1/i1/f1"}]
                    :stale [{:path "src/old.clj"
                             :payload-scope "t1/i1/f3"
                             :result-summary "stale read"}]}
                   80)]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))
        (expect (has-line? #"src/core.clj" lines))
        (expect (has-line? #"repeat of t1/i1/f1" lines))
        (expect (has-line? #"stale read" lines))))

    (it "DAG evidence rows group receipts by task"
      (let [lines (#'comps/dag-evidence-lines
                   {"render" [{:id "unit"
                               :kind "unit-tests"
                               :status "accepted"
                               :scope "t1/i2/f1"
                               :summary "tests passed"
                               :observations [1 2]}]}
                   80)]
        (expect (every? well-formed-line? lines))
        (expect (every? line-w-survives? lines))
        (expect (has-line? #"render" lines))
        (expect (has-line? #"unit  ·  unit-tests  ·  accepted" lines))
        (expect (has-line? #"observations 1, 2" lines))))))
