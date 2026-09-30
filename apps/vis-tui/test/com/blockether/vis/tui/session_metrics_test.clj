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
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke MouseAction MouseActionType]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal
            VirtualTerminalListener]))

(def measured-usage
  "Deterministic Companion-shaped data for production terminal/HTML review."
  {"input_tokens" 842190
   "output_tokens" 12842
   "cost_usd" 1.2749
   "fold_count" 2
   "turn_count" 8
   "iteration_count" 24
   "tool_call_count" 61
   "cache_read_share_percent" 72
   "reusable_prefix_coverage_percent" 91
   "reusable_prefix_estimated" true
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
    "budget_state" "fold-reminder"
    "budget_used_percent" 80
    "budget_used_ratio" 0.8
    "budget_remaining_tokens" 24000
    "estimated_input_tokens" 93800
    "estimate_difference_tokens" -2200
    "estimate_difference_percent" -2.3
    "root_count" 4
    "estimated_root_count" 1
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
(defdescribe
  shared-health-accounting-parity
  (it
    "shared health accounting parity"
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

        (expect (= usage (:usage snapshot)) name)
        (expect (str/includes? text (get expected "pressure")) name)
        (expect (= (get expected "estimate") (get values "Local estimate")) name)
        (expect (= (get expected "reported") (get values "Provider-reported input")) name)
        (expect (= (get expected "difference") (get values "Estimate − reported")) name)
        (expect (= "2100000" (get values "Total input")) name)
        (if-let [percent (get expected "percent")]
          (do (expect (str/ends-with? (get values "Context / working budget") (str percent "%"))
                      name)
              (expect (= (get health "budget_used_ratio") (some :meter rows)) name))
          (expect (not-any? :meter rows) name))
        (if-let [projection (get expected "projection")]
          (expect (str/includes? text (str projection " · not measured usage")) name)
          (expect (not-any? #(= :parts? (:toggle %)) rows) name))
        (when (get health "stale") (expect (str/includes? text "Earlier measurement") name))
        (doseq [note ["Local estimates describe" "Svar tokenizes" "four characters per token"]]
          (expect (not (str/includes? text note)) name))))))

(defdescribe
  supplied-metrics-are-not-recalculated
  (it "supplied metrics are not recalculated"
      ;; #186: inconsistent source rows intentionally expose calculations in clients.
      (let [health
            (assoc (get measured-usage "health")
              "last_request_tokens" 300000
              "breakdown" [{"label" "Partial row" "tokens" 1}]
              "estimated_input_tokens" 120
              "estimate_difference_tokens" 20
              "estimate_difference_percent" 20.0
              "budget_state" "within-budget"
              "budget_used_percent" 17
              "budget_used_ratio" 0.17
              "budget_remaining_tokens" 123
              "root_count" 7
              "estimated_root_count" 6)

            component
            (dlg/session-metrics-component {} {:phase :ready :usage {"health" health}})

            state
            (assoc (:init component) :parts? true)

            rows
            (:lines ((:measure component) state 160 80))

            text
            (str/replace (str/join " " (map :text rows)) #"\s+" " ")]

        (doseq [value ["Within budget" "17%" "123 budget left"
                       "7 available · 6 with guidance estimates" "120 tokens"
                       "+20 tokens (+20.0%)"]]
          (expect (str/includes? text value) value))
        (expect (= 0.17 (some :meter rows))))))

(defdescribe
  supplied-cache-metrics-are-not-reclassified
  (it "supplied cache metrics are not reclassified"
      ;; #186: deliberately conflicting counts must not override the supplied metric.
      (doseq [[estimated? samples expected] [[false 3 "91%"] [true 0 "≈91%"]]]
        (let [usage (assoc measured-usage
                      "reusable_prefix_estimated" estimated?
                      "prompt_cache_estimated_sample_count" samples)
              component (dlg/session-metrics-component {} {:phase :ready :usage usage})
              rows (:lines ((:measure component) (:init component) 160 80))]

          (expect (= expected (:value (first (filter #(= "Reuse coverage" (:label %)) rows)))))))))

(defdescribe
  metrics-fields-and-disclosures
  (it
    "metrics fields and disclosures"
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

      (expect (not (str/includes? text "not live")))
      (doseq [label ["Session health" "Fold reminder" "Last measured call" "#24"
                     "Context / working budget" "96000 / 120000" "80%" "24000 budget left"
                     "Reminder at 90000" "200000" "Instructions" "Tool definitions" "History"
                     "4 available" "1 with guidance estimates" "840 tokens on disk"
                     "No AGENTS.md or CLAUDE.md" "Could not read guidance"
                     "Guidance estimate unavailable" "Disk estimates" "Session totals"
                     "repeated context" "Total input" "842190" "Total output" "12842" "$1.2749"
                     "Folds" "Turns" "Calls" "Tools" "72%" "≈91%" "20 of 24 calls" "example-model"
                     "example-provider" "Active"]]
        (expect (str/includes? text label) label))
      (expect (not (str/includes? (lines component initial 100 45) "Tool definitions")))
      (expect (= full
                 (-> initial
                     ((fn [s]
                        ((:on-key component) s (cap/key-stroke \b) {})))
                     ((fn [s]
                        ((:on-key component) s (cap/key-stroke \f) {})))))))))

(defdescribe metrics-width-and-directory-spacing
             (it "metrics width and directory spacing"
                 (let [component
                       (review-component)

                       state
                       (assoc (:init component) :roots? true)

                       geom
                       ((:measure component) state 160 50)

                       rows
                       (mapv :text (:lines geom))]

                   (expect (= 120 (:content-w geom)))
                   (doseq [path (map #(get % "path") (get-in measured-usage ["health" "roots"]))]
                     (let [index (.indexOf ^java.util.List rows path)]
                       (expect (pos? index))
                       (when (pos? index) (expect (= "" (nth rows (dec index))))))))))

(defdescribe disclosed-rows-sit-under-their-heading
             (it "disclosed rows sit under their heading"
                 ;; The chevron owns the left margin, so the rows an open section discloses have
                 ;; to start under its label instead of hanging two columns to its left.
                 (let [capture
                       (cap/capture! {:cols 100
                                      :rows 80
                                      :keys [\b \f :esc]
                                      :paint! #(dlg/run-modal! (:screen %) (review-component))})

                       painted
                       (str/split-lines (cap/frame-text capture))

                       column
                       (fn [needle]
                         (some #(str/index-of % needle) painted))

                       margin
                       (column "Session health")]

                   (expect (nil? (:error capture)))
                   (expect (some? margin))
                   (doseq [heading ["▾ Context breakdown" "▾ Linked filesystems" "Session totals"]]
                     (expect (= margin (column heading)) heading))
                   (doseq [nested ["Logical request · not measured usage" "Local estimate"
                                   "/workspace/AGENTS.md" "4 available · 1 with guidance estimates"
                                   "/workspace/project" "No AGENTS.md or CLAUDE.md"
                                   "Disk estimates do not add"]]
                     (expect (= (+ 2 margin) (column nested)) nested)))))

(defdescribe
  missing-empty-error-and-pressure
  (it "missing empty error and pressure"
      (doseq [[snapshot expected] [[{:phase :loading} "Reading session metrics"]
                                   [{:phase :error} "reopen to retry"]
                                   [{:phase :ready} "No measured calls yet"]
                                   [{:phase :ready :usage {}} "Context measurement unavailable"]]]
        (let [component (dlg/session-metrics-component {} snapshot)]
          (expect (str/includes? (lines component (:init component) 80 40) expected))))
      ;; Raw facts alone must not trigger a client-side reconstruction.
      (let [component
            (dlg/session-metrics-component {}
                                           {:phase :ready
                                            :usage {"health" {"last_request_tokens" 90
                                                              "budget_tokens" 100
                                                              "breakdown" [{"label" "Partial"
                                                                            "tokens" 1}]}}})

            rows
            (:lines ((:measure component) (:init component) 80 40))]

        (expect (str/includes? (lines component (:init component) 80 40) "Budget not reported"))
        (expect (not-any? :meter rows))
        (expect (not-any? #(= :parts? (:toggle %)) rows)))
      (let [component
            (dlg/session-metrics-component {:model "pinned-model"} {:phase :ready :usage {}})

            text
            (lines component (:init component) 80 40)]

        (expect (str/includes? text "pinned-model"))
        (expect (str/includes? text "—"))
        (expect (not (str/includes? text "0%"))))))

(defdescribe metrics-compact-top-aligned-layout
             (it "metrics compact top aligned layout"
                 (let [component
                       (review-component)

                       geom
                       ((:measure component) (:init component) 120 70)

                       bounds
                       (:bounds geom)]

                   (expect (> (:content-w geom) (dlg/default-content-width 120)))
                   (expect (= (+ (:top bounds) 3) (:content-top geom)))
                   (expect (= (count (:lines geom)) (:content-h geom)))
                   (expect (< (- (:bottom bounds) (:top bounds)) 50)))))

(defdescribe
  metrics-keyboard-pointer-and-resize
  (it
    "metrics keyboard pointer and resize"
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

      (expect (= (:max-scroll geom) (:scroll end)))
      (expect (= 0 (:scroll (handle end (cap/key-stroke :home) geom))))
      (expect (pos? (:scroll (handle initial (cap/key-stroke :page-down) geom))))
      (expect (:parts? (handle initial click geom)))
      (expect (= {::dlg/done nil} (handle initial (cap/key-stroke :esc) geom)))
      (expect (= {::dlg/done nil} (handle initial (KeyStroke. \g true false) geom)))
      (expect (= :session-metrics
                 (:action (input/resolve-prefix-key (cap/key-stroke \u) {:prefix :cx}))))
      (expect (= "C-x u" (keymap/label-for :session-metrics)))
      (expect (some #(= :session-metrics (:id %)) dlg/palette-commands))
      (doseq [cols [40 60 80 120]]
        (let [small ((:measure component)
                      (assoc initial
                        :parts? true
                        :roots? true)
                      cols
                      24)
              state ((:reconcile component) (assoc initial :scroll 9999) small)]

          (expect (< (long (get-in small [:bounds :right])) cols))
          (expect (= (:max-scroll small) (:scroll state)))
          (expect (every? #(<= (p/display-width (:text %)) (:text-w small)) (:lines small))))))))

(defdescribe canonical-usage-client
             (it "canonical usage client"
                 (let [seen (atom nil)]
                   (with-redefs [client/request! (fn [& args]
                                                   (reset! seen args)
                                                   {:status 200 :body "{\"usage\":null}"})]
                     (expect (= {:phase :ready :usage nil} (client/session-usage "session/id")))
                     (expect (= [:get "/v1/sessions/session%2Fid/usage"] (vec (take 2 @seen)))))
                   (with-redefs [client/request! (fn [& _]
                                                   {:status 503})]
                     (expect (= {:phase :error} (client/session-usage "id"))))
                   (with-redefs [client/request! (fn [& _]
                                                   (throw (ex-info "offline" {})))]
                     (expect (= {:phase :error} (client/session-usage "id")))))))

(defdescribe
  metric-labels-and-values-are-bold
  (it
    "metric labels and values are bold"
    (doseq [cols
            [40 80 120]

            :let [component
                  (review-component)

                  state
                  (assoc (:init component)
                    :parts? true
                    :roots? true)

                  geom
                  ((:measure component) state cols 80)]
            scroll
            (distinct [0 (:max-scroll geom)])]

      (let [capture
            (cap/capture! {:cols cols
                           :rows 80
                           :paint! (fn [{:keys [g]}]
                                     ((:paint component) g (assoc state :scroll scroll) geom))})

            frame
            (last (:frames capture))

            x
            (+ 2 (long (get-in geom [:bounds :left])))

            visible
            (take (:content-h geom) (drop scroll (:lines geom)))]

        (expect (nil? (:error capture)))
        (doseq [[i {:keys [text label tone]}]
                (map-indexed vector visible)

                :when (or label (#{:heading :hint} tone))]

          (let [cells
                (subvec (nth frame (+ (long (:content-top geom)) (long i)))
                        x
                        (+ x (p/display-width text)))

                content
                (remove #(str/blank? (:ch %)) cells)]

            (expect (seq content) text)
            (expect (= #{(boolean (or label (= :heading tone)))} (set (map :bold content)))
                    (str cols " columns: " text))))))))

(defdescribe
  metrics-dialog-background-test
  (describe
    "Dialog background"
    (it "restores chat cells and styles after details expand and collapse"
        (doseq [keys [[\b \b :esc] [\f \f :esc] [\b \f \b \f :esc] [\b \f \f \b :esc]
                      [\b \b \b \b :esc]]]
          (let [capture (cap/capture!
                          {:cols 140
                           :rows 60
                           :keys keys
                           :paint! (fn [{:keys [screen g]}]
                                     (p/set-colors! g theme/dialog-hint theme/terminal-bg)
                                     (p/fill-rect! g 0 0 140 60)
                                     (p/styled g
                                               [p/BOLD]
                                               (p/put-str! g 6 4 "Chat above the compact dialog")
                                               (p/put-str! g 6 50 "Chat below the compact dialog"))
                                     (dlg/run-modal! screen (review-component)))})
                frames (:frames capture)
                restored? (= (first frames) (last frames))]

            (expect (nil? (:error capture)))
            (expect (= (count keys) (count frames)))
            (expect (not= (first frames) (second frames)))
            (expect restored?
                    (str keys " must restore the original frame, including its styles")))))
    (it
      "discards the old background when the terminal grows and shrinks"
      (let [capture
            (cap/capture!
              {:cols 140
               :rows 60
               :keys [\b \b :esc]
               :paint! (fn [{:keys [screen g ^DefaultVirtualTerminal terminal]}]
                         (p/set-bg! g theme/terminal-bg)
                         (p/fill-rect! g 0 0 140 60)
                         (p/put-str! g 6 4 "Background from before the resize")
                         (let [flushes (atom 0)]
                           (.addVirtualTerminalListener
                             terminal
                             (reify
                               VirtualTerminalListener
                                 (onFlush [_]
                                   (case (swap! flushes inc)
                                     1
                                     (.setTerminalSize terminal (TerminalSize. 160 66))

                                     2
                                     (.setTerminalSize terminal (TerminalSize. 140 60))

                                     nil))
                                 (onBell [_])
                                 (onClose [_])
                                 (onResized [_ _terminal _size])))
                           (dlg/run-modal! screen (review-component))))})

            reference
            (cap/capture! {:cols 140
                           :rows 60
                           :keys [:esc]
                           :paint! (fn [{:keys [screen g]}]
                                     (p/set-bg! g theme/terminal-bg)
                                     (p/fill-rect! g 0 0 140 60)
                                     (dlg/run-modal! screen (review-component)))})

            restored?
            (= (last (:frames reference)) (last (:frames capture)))]

        (expect (nil? (:error capture)))
        (expect (nil? (:error reference)))
        (expect (= 3 (count (:frames capture))))
        (expect restored? "The resized frame must not restore cells from the old background")))))

(defdescribe
  production-terminal-grid
  (it "production terminal grid"
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

          (expect (nil? (:error capture)))
          (expect (str/includes? text "Session metrics"))
          (expect (str/includes? text "Session health"))
          (when (= "prepared-request" (get-in usage ["health" "counted_projection"]))
            (doseq [expected ["Prepared request" "165,953 tokens" "162,177 tokens" "+3,776 tokens"]]
              (expect (str/includes? text expected) (str cols " columns: " expected))))))))

(defdescribe
  dismiss-loading-cancels-fetch
  (it "dismiss loading cancels fetch"
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
          (let [capture (cap/capture!
                          {:keys [:esc]
                           :paint! (fn [{:keys [screen]}]
                                     (dlg/session-metrics-dialog! screen "captured-session" {}))})]
            (expect (nil? (:error capture)))
            (expect (str/includes? (cap/frame-text capture) "Reading session metrics"))
            (when (realized? started) (expect (= true (deref cancelled 1000 false)))))))))

(defdescribe
  loading-repaints-without-a-keystroke
  (it "loading repaints without a keystroke"
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
            (expect (nil? (:error capture)))
            (expect (str/includes? (first @seen) "Reading session metrics"))
            (expect (str/includes? (last @seen) "Session health")))))))
