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
    (it "renders checkpoint metadata and typed edges as well-formed lines"
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
        (expect (some #(re-find #"tracked revision 12345678"
                         (apply str (map first %)))
                  lines))
        (expect (some #(re-find #"task:render needs"
                         (apply str (map first %)))
                  lines))
        (expect (some #(re-find #"Render verbose DAG"
                         (apply str (map first %)))
                  lines))
        (expect (some #(re-find #"modify  components.clj"
                         (apply str (map first %)))
                  lines))))))
