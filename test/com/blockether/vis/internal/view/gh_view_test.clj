(ns com.blockether.vis.internal.view.gh-view-test
  "The gh extension's ops, replayed through the engine that receives them.

   `.vis/extensions/gh.py` builds a live view from `gh run view --json`. Its own Python test
   captures what two real polls of one real CI run make it SAY, into
   `.vis/extensions/fixtures/ops.json`; here that same file crosses the engine, so a node key, a
   tone or an item shape the engine would refuse is a red test HERE rather than a refusal in front
   of a human halfway through a fifteen-minute run.

   The fixtures are shared on purpose: Python pins the picture its host paints, Clojure pins the
   picture the ENGINE paints from the same ops, and the two must agree."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.view :as hi]
            [com.blockether.vis.internal.view.materializer :as live]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private ops-file (io/file ".vis" "extensions" "fixtures" "ops.json"))

(def ^:private picture-file (io/file ".vis" "extensions" "fixtures" "view.json"))

(def ^:private live-views-dir
  "The private var every view record hangs under, redefined per test so nothing here
   writes anywhere near the developer's own `~/.vis`."
  (requiring-resolve 'com.blockether.vis.internal.view.sink/views-dir))

(defn- fixture [file] (json/read-json (slurp file) :key-fn identity))

(defn- replay
  "Every op of `ops`, in order, through the engine's own live dispatch.

   The fixture carries no view id — the extension learns it from `open` — so each handle op is
   addressed to the view the open just answered, exactly as the Python side addresses it."
  [ops]
  (let [dir
        (io/file (System/getProperty "java.io.tmpdir") (str "vis-views-" (random-uuid)))

        session-id
        (str "vis-test-" (random-uuid))]

    (with-redefs-fn {live-views-dir (constantly dir)}
      (fn []
        (let [view-id (volatile! nil)]
          (mapv (fn [op]
                  (let [envelope (if (= "open" (get op "op"))
                                   (assoc op "view" (assoc (get op "view") "session_id" session-id))
                                   (assoc op "view_id" @view-id))
                        final-view (when (= "close" (get op "op"))
                                     (:view (hi/live-dispatch {"op" "state" "view_id" @view-id})))
                        answer (cond-> (hi/live-dispatch envelope)
                                 final-view
                                 (assoc :final-view final-view))]

                    (vreset! view-id (:view-id answer))
                    answer))
                ops))))))

(defn- node [view id] (first (filter #(= id (:id %)) (:nodes view))))

(defdescribe
  gh-extension-live-test
  (it
    "accepts every op the gh extension says, keeps the picture, and returns only its string"
    (let [ops
          (fixture ops-file)

          answers
          (replay ops)

          result
          (:result (last answers))

          model-report
          (json/read-json result :key-fn keyword)

          view
          (:final-view (last answers))

          opened-view
          (:view (first answers))

          patch-ops
          (mapcat #(get-in % ["patch" "ops"] []) ops)]

      ;; Every push before the close was accepted: the wire the extension speaks IS this one.
      (expect (every? :is-open (butlast answers)))
      (expect (string? result))
      (expect (= "failure" (get-in model-report [:run :conclusion])))
      (expect (= 6 (count (:jobs model-report))))
      (expect (= [:95742028770] (vec (keys (:failed_logs model-report)))))
      ;; The log is NOT declared with the rest: it arrives as one `add-node` addressed
      ;; `after` "run", and the engine's own ordering is what puts it second here.
      (expect (= ["run" "output" "progress" "score" "jobs" "steps" "links"]
                 (mapv :id (:nodes view))))
      (expect (= [:status :log :progress :stat :table :steps :link] (mapv :type (:nodes view))))
      ;; The status says the WORK, not the arithmetic: counting jobs left it unchanged for
      ;; 23 minutes of a 97-minute run. The counting moved to the bar, which counts steps.
      (expect (= "tests · vis-agent + vis-contract (PyPI packages) failed"
                 (:text (node view "run"))))
      (expect (= :error (:tone (node view "run"))))
      (expect (= [36 36] ((juxt :done :total) (node view "progress"))))
      (expect (= ["1" "5" "0" "28m 33s"] (mapv :value-text (:stats (node view "score")))))
      ;; A row is addressed by the job's databaseId, so a job that changes state keeps its slot.
      (let [jobs-node
            (node view "jobs")

            opened-jobs-node
            (node opened-view "jobs")

            jobs
            (:rows jobs-node)]

        ;; The live view starts with every concurrently running job selected. Interactive-only
        ;; state is intentionally absent from the budgeted verdict the model reads.
        (expect (true? (:is-selectable opened-jobs-node)))
        (expect (= ["95742028721" "95742028781"] (:selected-ids opened-jobs-node)))
        (expect (= 6 (count jobs)))
        (expect (= ["95742028721" "95742028770" "95742028781" "95742028809" "95742028943"
                    "95742029230"]
                   (mapv :id jobs)))
        (expect (= :error (:tone (first (filter #(= "95742028770" (:id %)) jobs)))))
        (expect (= [:ok :error :ok :ok :ok :ok] (mapv :tone jobs))))
      ;; The checklist follows the selected job: the failing job's steps, not the running one's.
      (expect (= 10 (count (:steps (node view "steps")))))
      (expect (some #(= :error (:tone %)) (:steps (node view "steps"))))
      ;; The pane exists only while it holds something. GitHub publishes a job's log when
      ;; the job ENDS, so nothing is drawn for the two running ones and the engine receives
      ;; exactly one add — under the status, where the eye already is when something breaks.
      (expect (= [["output" "run"]]
                 (into []
                       (comp (filter #(= "add-node" (get % "op")))
                             (map (juxt #(get-in % ["node_spec" "id"]) #(get % "after"))))
                       patch-ops)))
      (expect (not-any? #(= "· GitHub publishes this job's raw log when the job ends"
                            (first (get % "lines")))
                        patch-ops))
      ;; The run-wide Activity duplicate is not part of the extension contract.
      (expect (not-any? #(= "activity" (get % "node_id")) patch-ops))
      (expect (= 6 (count (:lines (node view "output")))))
      (expect (= "Failure · vis-agent + vis-contract (PyPI packages)"
                 (:label (node view "output"))))
      (expect (str/ends-with? (last (:lines (node view "output")))
                              "##[error]Process completed with exit code 1."))
      (expect (= ["run" "95742028770"] (mapv :id (:links (node view "links")))))))
  (it "renders the preserved human picture into the durable document"
      (let [view
            (:final-view (last (replay (fixture ops-file))))

            document
            (live/->markdown view)]

        (expect (str/includes? document "tests · vis-agent + vis-contract (PyPI packages) failed"))
        (expect (str/includes? document "**100%** · 36/36 done"))
        (expect (str/includes? document "tests / vis-agent + vis-contract (PyPI packages)"))
        (expect (str/includes? document "exit code 1"))))
  (it "paints the picture the extension's own host painted, key for key"
      (let [engine
            (:final-view (last (replay (fixture ops-file))))

            outside
            (fixture picture-file)]

        ;; Two hosts, one contract: the mirror Python tests against and the engine a session runs
        ;; must answer the same nodes, in the same order, holding the same values.
        (expect (= (mapv #(get % "id") (get outside "nodes")) (mapv :id (:nodes engine))))
        (expect (= (mapv #(get % "type") (get outside "nodes"))
                   (mapv (comp name :type) (:nodes engine))))
        (expect (= (get (first (get outside "nodes")) "text") (:text (node engine "run"))))
        (expect (= (mapv #(get % "id")
                         (get (some #(when (= "jobs" (get % "id")) %) (get outside "nodes"))
                              "rows"))
                   (mapv :id (:rows (node engine "jobs")))))))
  (it "crosses the strings-only seam a Python extension actually speaks"
      (let [dir
            (io/file (System/getProperty "java.io.tmpdir") (str "vis-views-" (random-uuid)))

            opened
            (first (fixture ops-file))]

        (with-redefs-fn {live-views-dir (constantly dir)}
          (fn []
            (let [envelope
                  (assoc opened
                    "view" (assoc (get opened "view") "session_id" (str "vis-test-" (random-uuid))))

                  answer
                  (json/read-json (hi/live-json! (json/write-json-str envelope)) :key-fn identity)]

              (expect (true? (get answer "is_open")))
              ;; What the FIRST envelope declares — the log is not among them. It arrives in a
              ;; later patch, addressed `after` "run", and only once there is something to read.
              (expect (= ["run" "progress" "score" "jobs" "steps" "links"]
                         (mapv #(get % "id") (get-in answer ["view" "nodes"])))))))))
  ;; Regression, session f8115c8c-b997-49bf-a22b-81816d961fe3: a watch that ran to the end
  ;; died at its own close. The archive pictures an extension seals are the ones `state`
  ;; ANSWERED it — snake_case JSON — and the engine held them to its own kebab-case
  ;; vocabulary, so every selection snapshot was refused as an invalid live view. The shared
  ;; golden could not see it: the Python side drops `selection_snapshots` before recording ops.
  (it
    "seals the archive pictures the extension builds out of what `state` answered it"
    (let [dir
          (io/file (System/getProperty "java.io.tmpdir") (str "vis-views-" (random-uuid)))

          opened
          (first (fixture ops-file))]

      (with-redefs-fn {live-views-dir (constantly dir)}
        (fn []
          (let [seam
                (fn [envelope]
                  (json/read-json (hi/live-json! (json/write-json-str envelope)) :key-fn identity))

                view-id
                (get (seam (assoc opened
                             "view" (assoc (get opened "view")
                                      "session_id" (str "vis-test-" (random-uuid)))))
                     "view_id")

                picture
                (get (seam {"op" "state" "view_id" view-id}) "view")

                answer
                (seam {"op" "close"
                       "view_id" view-id
                       "ending" {"model_result" "1 of 6 jobs failed"
                                 "selection_snapshots" [{"node_id" "jobs"
                                                         "selected_ids" ["95742028770"]
                                                         "view" picture}]}})]

            ;; Archive-only: accepted, and still never folded into what the model reads.
            (expect (false? (get answer "is_open")))
            (expect (= "1 of 6 jobs failed" (get answer "result")))))))))
