(ns com.blockether.vis.internal.human-input.gh-live-test
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
            [com.blockether.vis.internal.human-input :as hi]
            [com.blockether.vis.internal.human-input.live :as live]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private ops-file (io/file ".vis" "extensions" "fixtures" "ops.json"))

(def ^:private picture-file (io/file ".vis" "extensions" "fixtures" "view.json"))

(def ^:private live-views-dir
  "The private var every view record hangs under, redefined per test so nothing here
   writes anywhere near the developer's own `~/.vis`."
  (requiring-resolve 'com.blockether.vis.internal.human-input.live-sink/views-dir))

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
      ;; Seven distinct answers: overview, selected-job details, and the run link.
      (expect (= ["run" "progress" "score" "jobs" "steps" "output" "links"]
                 (mapv :id (:nodes view))))
      (expect (= [:status :progress :stat :table :steps :log :link] (mapv :type (:nodes view))))
      (expect (= "6 of 6 jobs finished, 1 failed" (:text (node view "run"))))
      (expect (= :error (:tone (node view "run"))))
      (expect (= [6 6] ((juxt :done :total) (node view "progress"))))
      (expect (= ["5" "1" "0" "0"] (mapv :value-text (:stats (node view "score")))))
      ;; A row is addressed by the job's databaseId, so a job that changes state keeps its slot.
      (let [jobs-node
            (node view "jobs")

            opened-jobs-node
            (node opened-view "jobs")

            jobs
            (:rows jobs-node)]

        ;; The live view starts with every concurrently running job in focus. Interactive-only
        ;; state is intentionally absent from the budgeted verdict the model reads.
        (expect (true? (:is-focusable opened-jobs-node)))
        (expect (= ["95742028721" "95742028781"] (:focused-ids opened-jobs-node)))
        (expect (= 6 (count jobs)))
        (expect (= ["95742028721" "95742028770" "95742028781" "95742028809" "95742028943"
                    "95742029230"]
                   (mapv :id jobs)))
        (expect (= :error (:tone (first (filter #(= "95742028770" (:id %)) jobs)))))
        (expect (= [:ok :error :ok :ok :ok :ok] (mapv :tone jobs))))
      ;; The checklist follows the job in focus: the failing job's steps, not the running one's.
      (expect (= 10 (count (:steps (node view "steps")))))
      (expect (some #(= :error (:tone %)) (:steps (node view "steps"))))
      ;; A running selection's log pane states only what GitHub still withholds. Repeating the
      ;; steps panel there wrote one more copy of the same placeholder into the record per tick.
      (expect (some (fn [op]
                      (and (= "output" (get op "node_id"))
                           (= ["── tests / macos-latest · log"
                               "· GitHub publishes this job's raw log when the job ends"
                               "── tests / ubuntu-latest · log"
                               "· GitHub publishes this job's raw log when the job ends"]
                              (get op "lines"))))
                    patch-ops))
      ;; The run-wide Activity duplicate is not part of the extension contract.
      (expect (not-any? #(= "activity" (get % "node_id")) patch-ops))
      (expect (= 7 (count (:lines (node view "output")))))
      (expect (str/starts-with? (first (:lines (node view "output")))
                                "── tests / vis-agent + vis-contract (PyPI packages) · log"))
      (expect (str/ends-with? (last (:lines (node view "output")))
                              "##[error]Process completed with exit code 1."))
      (expect (= ["run" "95742028770"] (mapv :id (:links (node view "links")))))))
  (it "renders the preserved human picture into the durable document"
      (let [view
            (:final-view (last (replay (fixture ops-file))))

            document
            (live/->markdown view)]

        (expect (str/includes? document "6 of 6 jobs finished, 1 failed"))
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
        (expect (= (mapv #(get % "id") (get (nth (get outside "nodes") 3) "rows"))
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
              (expect (= ["run" "progress" "score" "jobs" "steps" "output" "links"]
                         (mapv #(get % "id") (get-in answer ["view" "nodes"])))))))))
  ;; Regression, session f8115c8c-b997-49bf-a22b-81816d961fe3: a watch that ran to the end
  ;; died at its own close. The archive pictures an extension seals are the ones `state`
  ;; ANSWERED it — snake_case JSON — and the engine held them to its own kebab-case
  ;; vocabulary, so every focus snapshot was refused as an invalid live view. The shared
  ;; golden could not see it: the Python side drops `focus_snapshots` before recording ops.
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
                                 "focus_snapshots" [{"node_id" "jobs"
                                                     "focused_ids" ["95742028770"]
                                                     "view" picture}]}})]

            ;; Archive-only: accepted, and still never folded into what the model reads.
            (expect (false? (get answer "is_open")))
            (expect (= "1 of 6 jobs failed" (get answer "result")))))))))
