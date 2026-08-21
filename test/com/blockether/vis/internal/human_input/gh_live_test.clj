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

          view
          (:final-view (last answers))

          opened-view
          (:view (first answers))

          patch-ops
          (mapcat #(get-in % ["patch" "ops"] []) ops)]

      ;; Every push before the close was accepted: the wire the extension speaks IS this one.
      (expect (every? :is-open (butlast answers)))
      (expect (string? result))
      (expect (str/starts-with? result "6 of 6 jobs finished, 1 failed"))
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
      ;; The selected-job log proves running work is moving before GitHub publishes raw logs.
      (expect (some (fn [op]
                      (and (= "output" (get op "node_id"))
                           (some #(= "▶ Run test suite · 10m 00s" %) (get op "lines"))))
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
                         (mapv #(get % "id") (get-in answer ["view" "nodes"]))))))))))
