(ns com.blockether.vis.tui.session-metrics-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna.input KeyStroke MouseAction MouseActionType]
           [com.googlecode.lanterna TerminalPosition]))

(def measured-usage
  "Deterministic Companion-shaped data for production terminal/HTML review."
  {"input_tokens" 842190
   "output_tokens" 12842
   "cost_usd" 1.2749
   "fold_count" 2
   "turn_count" 8
   "iteration_count" 24
   "tool_call_count" 61
   "cache_read_share_percent" 72.3
   "reusable_prefix_coverage_percent" 91.2
   "prompt_cache_sample_count" 20
   "prompt_cache_estimated_sample_count" 3
   "model" "example-model"
   "provider" "example-provider"
   "duration_ms" 184000
   "health"
   {"last_request_tokens" 96000
    "budget_tokens" 120000
    "reminder_tokens" 90000
    "model_input_limit" 200000
    "call" 24
    "stale" false
    "breakdown" [{"label" "Instructions" "tokens" 6400 "path" "/workspace/AGENTS.md"}
                 {"label" "Tool definitions" "tokens" 8200} {"label" "History" "tokens" 79200}]
    "roots" [{"path" "/workspace/project"
              "guidance" {"status" "available" "path" "/workspace/project/AGENTS.md" "tokens" 840}}
             {"path" "/workspace/資料" "guidance" {"status" "missing"}}
             {"path" "/workspace/restricted" "guidance" {"status" "error"}}
             {"path" "/workspace/legacy"}]}})

(defn review-component
  "Use the actual modal painter, never a facsimile."
  []
  (dlg/session-metrics-component {} {:phase :ready :usage measured-usage}))

(defn- lines
  [component state cols rows]
  (str/join "\n" (map :text (:lines ((:measure component) state cols rows)))))

(def health-parity-fixture
  "One wire fixture consumed by the Companion and TUI regression suites."
  (wire/parse-json (slurp (io/resource "vis-contract/fixtures/session-health.json"))))

;; #186: both clients must show the same request estimate and provider measurement.
(deftest shared-health-accounting-parity
  (doseq [{:strs [name health expected]} (get health-parity-fixture "cases")]
    (let [usage (cond-> (get health-parity-fixture "usage")
                  health
                  (assoc "health" health))
          snapshot (with-redefs [client/request! (fn [& _]
                                                   {:status 200
                                                    :body (wire/json-str {"usage" usage})})]
                     (client/session-usage "fixture-session"))
          component (dlg/session-metrics-component {} snapshot)
          state (assoc (:init component)
                  :parts? true
                  :roots? true)
          geom ((:measure component) state 160 80)
          rows (:lines geom)
          text (str/replace (str/join " " (map :text rows)) #"\s+" " ")
          values (into {} (keep #(when (:label %) [(:label %) (:value %)])) rows)]

      (is (= usage (:usage snapshot)) name)
      (is (str/includes? text (get expected "pressure")) name)
      (is (= (get expected "estimate") (get values "Local estimate")) name)
      (is (= (get expected "reported") (get values "Provider-reported input")) name)
      (is (= (get expected "difference") (get values "Estimate − reported")) name)
      (is (= "2100000" (get values "Total input")) name)
      (if-let [percent (get expected "percent")]
        (do (is (str/ends-with? (get values "Context / working budget") (str percent "%")) name)
            (is (= (min 1.0
                        (/ (double (get health "last_request_tokens"))
                           (double (get health "budget_tokens"))))
                   (some :meter rows))
                name))
        (is (not-any? :meter rows) name))
      (if-let [projection (get expected "projection")]
        (do (is (str/includes? text (str projection " · not measured usage")) name)
            (is (str/includes? text (get expected "scope")) name)
            (is (str/includes? text "Svar tokenizes") name)
            (is (str/includes? text "including cached input") name))
        (is (not-any? #(= :parts? (:toggle %)) rows) name))
      (when (get health "stale") (is (str/includes? text "Earlier measurement") name))
      (is (not (str/includes? text "four characters per token")) name))))

(deftest metrics-fields-and-disclosures
  (let [component
        (review-component)

        initial
        (:init component)

        full
        (assoc initial
          :parts? true
          :roots? true)

        text
        (str/replace (lines component full 100 45) #"\s+" " ")]

    (is (not (str/includes? text "not live")))
    (doseq [label ["Session health" "Fold reminder" "Last measured call" "#24"
                   "Context / working budget" "96000 / 120000" "80%" "24000 budget left"
                   "Reminder at 90000" "200000" "Instructions" "Tool definitions" "History"
                   "4 available" "1 with guidance estimates" "840 tokens on disk"
                   "No AGENTS.md or CLAUDE.md" "Could not read guidance"
                   "Guidance estimate unavailable" "Svar tokenizes" "Disk estimates"
                   "Session totals" "repeated context" "Total input" "842190" "Total output" "12842"
                   "$1.2749" "Folds" "Turns" "Calls" "Tools" "72%" "≈91%" "20 of 24 calls"
                   "example-model" "example-provider" "Active"]]
      (is (str/includes? text label) label))
    (is (not (str/includes? (lines component initial 100 45) "Tool definitions")))
    (is (= full
           (-> initial
               ((fn [s]
                  ((:on-key component) s (cap/key-stroke \b) {})))
               ((fn [s]
                  ((:on-key component) s (cap/key-stroke \f) {}))))))))

(deftest metrics-width-and-directory-spacing
  (let [component
        (review-component)

        state
        (assoc (:init component) :roots? true)

        geom
        ((:measure component) state 160 50)

        rows
        (mapv :text (:lines geom))]

    (is (= 92 (:content-w geom)))
    (doseq [path (map #(get % "path") (get-in measured-usage ["health" "roots"]))]
      (let [index (.indexOf ^java.util.List rows path)]
        (is (pos? index))
        (when (pos? index) (is (= "" (nth rows (dec index)))))))))

(deftest missing-empty-error-and-pressure
  (doseq [[snapshot expected] [[{:phase :loading} "Reading session metrics"]
                               [{:phase :error} "reopen to retry"]
                               [{:phase :ready} "No measured calls yet"]
                               [{:phase :ready :usage {}} "Context measurement unavailable"]]]
    (let [component (dlg/session-metrics-component {} snapshot)]
      (is (str/includes? (lines component (:init component) 80 40) expected))))
  (doseq [[health expected] [[{"last_request_tokens" 1} "Budget not reported"]
                             [{"last_request_tokens" 90 "budget_tokens" 100 "reminder_tokens" 80}
                              "Fold reminder"]
                             [{"last_request_tokens" 100 "budget_tokens" 100} "Over budget"]
                             [{"last_request_tokens" 100 "budget_tokens" 90 "model_input_limit" 100}
                              "Input limit reached"]
                             [{"last_request_tokens" 20 "budget_tokens" 100} "Within budget"]]]
    (let [component (dlg/session-metrics-component {} {:phase :ready :usage {"health" health}})]
      (is (str/includes? (lines component (:init component) 80 40) expected))))
  (let [component
        (dlg/session-metrics-component {:model "pinned-model"} {:phase :ready :usage {}})

        text
        (lines component (:init component) 80 40)]

    (is (str/includes? text "pinned-model"))
    (is (str/includes? text "—"))
    (is (not (str/includes? text "0%")))))

(deftest metrics-keyboard-pointer-and-resize
  (let [component
        (review-component)

        initial
        (:init component)

        geom
        ((:measure component) initial 80 40)

        handle
        (:on-key component)

        end
        (handle initial (cap/key-stroke :end) geom)

        disclosure-index
        (first (keep-indexed #(when (:toggle %2) %1) (:lines geom)))

        click
        (MouseAction. MouseActionType/CLICK_RELEASE
                      1
                      (TerminalPosition. (+ 2 (int (get-in geom [:bounds :left])))
                                         (+ (int (:content-top geom)) (int disclosure-index))))]

    (is (= (:max-scroll geom) (:scroll end)))
    (is (= 0 (:scroll (handle end (cap/key-stroke :home) geom))))
    (is (pos? (:scroll (handle initial (cap/key-stroke :page-down) geom))))
    (is (:parts? (handle initial click geom)))
    (is (= {::dlg/done nil} (handle initial (cap/key-stroke :esc) geom)))
    (is (= {::dlg/done nil} (handle initial (KeyStroke. \g true false) geom)))
    (is (= :session-metrics (:action (input/resolve-prefix-key (cap/key-stroke \u) {:prefix :cx}))))
    (is (= "C-x u" (keymap/label-for :session-metrics)))
    (is (some #(= :session-metrics (:id %)) dlg/palette-commands))
    (doseq [cols [40 60 80 120]]
      (let [small ((:measure component)
                    (assoc initial
                      :parts? true
                      :roots? true)
                    cols
                    24)
            state ((:reconcile component) (assoc initial :scroll 9999) small)]

        (is (< (long (get-in small [:bounds :right])) cols))
        (is (= (:max-scroll small) (:scroll state)))
        (is (every? #(<= (p/display-width (:text %)) (:text-w small)) (:lines small)))))))

(deftest canonical-usage-client
  (let [seen (atom nil)]
    (with-redefs [client/request! (fn [& args]
                                    (reset! seen args)
                                    {:status 200 :body "{\"usage\":null}"})]
      (is (= {:phase :ready :usage nil} (client/session-usage "session/id")))
      (is (= [:get "/v1/sessions/session%2Fid/usage"] (vec (take 2 @seen)))))
    (with-redefs [client/request! (fn [& _]
                                    {:status 503})]
      (is (= {:phase :error} (client/session-usage "id"))))
    (with-redefs [client/request! (fn [& _]
                                    (throw (ex-info "offline" {})))]
      (is (= {:phase :error} (client/session-usage "id"))))))

(deftest production-terminal-grid
  (doseq [cols
          [40 80 120]

          usage
          [measured-usage
           (assoc (get health-parity-fixture "usage")
             "health" (get-in health-parity-fixture ["cases" 0 "health"]))]]

    (let [capture
          (cap/capture! {:cols cols
                         :rows 40
                         :keys [:end :home \b \f :esc]
                         :paint! (fn [{:keys [screen g]}]
                                   (p/set-bg! g theme/terminal-bg)
                                   (p/fill-rect! g 0 0 cols 40)
                                   (dlg/run-modal! screen
                                                   (dlg/session-metrics-component
                                                     {}
                                                     {:phase :ready :usage usage})))})

          text
          (-> (cap/frame-text capture)
              (str/replace #"[│█]" " ")
              (str/replace #"\s+" " "))]

      (is (nil? (:error capture)))
      (is (str/includes? text "Session metrics"))
      (is (str/includes? text "Session health"))
      (when (= "prepared-request" (get-in usage ["health" "counted_projection"]))
        (doseq [expected ["Prepared request" "165,953 tokens" "162,177 tokens" "+3,776 tokens"]]
          (is (str/includes? text expected) (str cols " columns: " expected)))))))

(deftest dismiss-loading-cancels-fetch
  (let [started
        (promise)

        cancelled
        (promise)]

    (with-redefs [client/session-usage (fn [_]
                                         (deliver started true)
                                         (try (Thread/sleep 30000)
                                              {:phase :ready}
                                              (catch InterruptedException _
                                                (deliver cancelled true))))]
      (let [capture (cap/capture! {:keys [:esc]
                                   :paint!
                                   (fn [{:keys [screen]}]
                                     (dlg/session-metrics-dialog! screen "captured-session" {}))})]
        (is (nil? (:error capture)))
        (is (str/includes? (cap/frame-text capture) "Reading session metrics"))
        (when (realized? started) (is (= true (deref cancelled 1000 false))))))))

(deftest loading-repaints-without-a-keystroke
  (let [seen
        (atom [])

        paint
        (:paint (review-component))]

    (with-redefs [client/session-usage
                  (fn [_]
                    (Thread/sleep 40)
                    {:phase :ready :usage measured-usage})

                  dlg/read-modal-key!
                  (fn [_]
                    (cap/key-stroke :esc))

                  dlg/session-metrics-component
                  (let [build dlg/session-metrics-component]
                    (fn [session snapshot]
                      (assoc (build session snapshot)
                        :paint (fn [g state geom]
                                 (swap! seen conj (str/join " " (map :text (:lines geom))))
                                 (paint g state geom)))))]

      (let [capture (cap/capture! {:paint! (fn [{:keys [screen]}]
                                             (dlg/session-metrics-dialog! screen "id" {}))})]
        (is (nil? (:error capture)))
        (is (str/includes? (first @seen) "Reading session metrics"))
        (is (str/includes? (last @seen) "Session health"))))))
