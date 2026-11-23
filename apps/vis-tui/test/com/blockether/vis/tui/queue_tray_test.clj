;; The queue frame above the prompt: its rows, its controls and their hit regions.
(ns com.blockether.vis.tui.queue-tray-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.queue-tray :as queue-tray]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private pending
  [{:text "first queued message" :turn-id "t1"} {:text "/help" :turn-id "t2"}
   {:text "local only" :unsent? true}
   {:text "third queued message" :turn-id "t3" :deliver "next_iteration"}])

(def ^:private running {:pending pending :running? true :session-id "s1"})

(defn- line-with
  "The tray text line that contains `needle`."
  [queue cols needle]
  (first (filter #(str/includes? % needle) (queue-tray/text queue cols))))

(defn- regions
  "The queue hit regions that the painter registers for `queue` at `cols` columns."
  [queue cols]
  (.reset interactions/hit-map)
  (.beginFrame interactions/hit-map)
  (let [capture (cap/capture! {:cols cols
                               :rows 12
                               :paint! (fn [{:keys [g]}] (queue-tray/draw! g queue 0 cols))})]
    (.commitFrame interactions/hit-map)
    (expect (nil? (:error capture)))
    (filterv #(contains? #{:queue-send-now :queue-send-all-now :queue-edit} (:kind %))
      (.current interactions/hit-map))))

(defdescribe
  queue-tray-controls-test
  ;; While a turn runs, the band offers `send all now` and every markable row its own button.
  ;; A marked row says `next iter` and its button reads `Unsend`. A command (`/…`, `!…`) and a
  ;; local-only row show no button. A held queue shows no send control.
  (it "shows send all now on the band and a button on each markable row"
      (expect (str/includes? (line-with running 130 "QUEUED") "QUEUED · 4"))
      (expect (str/includes? (line-with running 130 "QUEUED") "send all now (C-x q s)"))
      (expect (str/includes? (line-with running 130 "#1") "Send it now (C-x q 1)"))
      (expect (not (str/includes? (line-with running 130 "/help") "Send")))
      (expect (str/includes? (line-with running 130 "#3") "unsent: local only"))
      (expect (not (str/includes? (line-with running 130 "#3") "Send")))
      (let [marked (line-with running 130 "#4")]
        (expect (str/includes? marked "next iter"))
        (expect (str/includes? marked "Unsend (C-x q 4)"))))
  (it "drops send all now once every markable row is marked"
      (let [queue (assoc running
                    :pending (mapv #(cond-> % (:turn-id %) (assoc :deliver "next_iteration"))
                               pending))]
        (expect (not (str/includes? (line-with queue 130 "QUEUED") "send all")))))
  ;; Regression, user report: one queued message showed `Send all now` next to its own button.
  (it "shows no send all now for a single markable row"
      (let [queue (assoc running :pending (subvec pending 0 2))]
        (expect (str/includes? (line-with queue 130 "QUEUED") "QUEUED · 2"))
        (expect (not (str/includes? (line-with queue 130 "QUEUED") "send all")))
        (expect (str/includes? (line-with queue 130 "#1") "Send it now"))))
  (it "shows no send control while the queue is held"
      (let [queue (assoc running :running? false :paused {:reason "turn_failed"})]
        (expect (str/includes? (line-with queue 130 "QUEUED") "QUEUED · 4 · paused"))
        (expect (not-any? #(str/includes? % "Send") (queue-tray/text queue 130)))
        (expect (not-any? #(= :queue-send-now (:kind %)) (regions queue 130)))))
  (it "takes no row without a queued message"
      (expect (zero? (queue-tray/height (assoc running :pending []) 130)))))

(defdescribe
  queue-tray-regions-test
  (it "registers each control once, at the right edge of the frame"
      (let [found (regions running 130)
            by-kind (group-by :kind found)
            sends (sort-by :turn-id (:queue-send-now by-kind))
            right-edge #(+ (long (:col (:bounds %))) (long (:width (:bounds %))))]
        (expect (= "s1" (:session-id (first (:queue-send-all-now by-kind)))))
        (expect (= 1 (count (:queue-edit by-kind))))
        ;; One button per markable row (t1, t3): not the command, not the local row.
        (expect (= ["t1" "t3"] (mapv :turn-id sends)))
        (expect (= ["next_iteration" "turn_end"] (mapv :deliver sends)))
        ;; The buttons and `edit` end at the right border of the frame (column cols - 2).
        (expect (= [128 128 127] (mapv right-edge (conj (vec sends) (first (:queue-edit by-kind))))))
        (expect (= [[:queue-send-now "s1" "t1"] [:queue-send-now "s1" "t3"]]
                   (mapv interactions/label-key sends))))))

(defdescribe
  queue-tray-focus-test
  ;; C-x q e: the hint names the focus keys, a marked row shows a check and the buttons go.
  (it "checks marked rows, hides the buttons and names the focus keys"
      (let [queue (assoc running :focus {:index 1 :marked #{"t1"} :undo? true})
            text (queue-tray/text queue 130)]
        (expect (str/includes? (line-with queue 130 "#1") "✓ first queued message"))
        (expect (= [false true false false]
                   (mapv (comp boolean :selected?) (filter #(= :row (:kind %)) (queue-tray/lines queue 130)))))
        (expect (not-any? #(str/includes? % "Send") text))
        (expect (not (str/includes? (line-with queue 130 "QUEUED") "send all")))
        (expect (some #(str/includes? % "Space mark · Enter edit") text))
        (expect (some #(str/includes? % "u undo") text))
        (expect (empty? (remove #(= :queue-edit (:kind %)) (regions queue 130)))))))

(defdescribe
  queue-tray-preview-test
  (it "shows a queued image as its filename, never the raw drop path"
      (let [queue {:pending [{:text "/var/folders/67/T/clipboard-2026-07-26-151209.png\nLOOK AT THIS"}
                             ;; Gateway-mirrored row: the daemon already chipped it.
                             {:text "/tmp/other.png" :preview-text "other.png"}]
                   :running? true
                   :session-id "s1"}
            text (queue-tray/text queue 130)]
        (expect (some #(str/includes? % "clipboard-2026-07-26-151209.png") text))
        (expect (not-any? #(str/includes? % "/var/folders/67/T/") text))
        (expect (some #(str/includes? % "other.png") text))
        (expect (not-any? #(str/includes? % "/tmp/other.png") text)))))

(defdescribe
  queue-tray-under-team-test
  ;; Visual check: under the agent team frame the blank line above the tray split the two frames.
  (it "touches the team frame above it, else stands a blank line below the rows"
      (expect (= "" (first (queue-tray/text running 100))))
      (let [under (queue-tray/text (assoc running :under-team? true) 100)]
        (expect (str/starts-with? (first under) "┌"))
        (expect (= (dec (queue-tray/height running 100))
                   (queue-tray/height (assoc running :under-team? true) 100))))))
