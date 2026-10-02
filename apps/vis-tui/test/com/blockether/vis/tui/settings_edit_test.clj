(ns com.blockether.vis.tui.settings-edit-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.settings-model :as sm]
            [com.blockether.vis.tui.client :as vis])
  (:import [com.googlecode.lanterna.input KeyStroke]
           [com.googlecode.lanterna.screen TerminalScreen]))

(def ^:private row
  {"id" "plans"
   "type" "boolean"
   "enabled" false
   "label" "Plans"
   "source" "default"
   "scope" "session"
   "is_override" false
   "inherited_value" false
   "inherited_source" "default"
   "applies" "next_request"})

(def ^:private catalog
  {"revision" "initial"
   "scope" "session"
   "target_id" "selected"
   "groups" [{"id" "response" "title" "Planning" "toggles" [row]}]})

(defn- capture-editor
  [keys calls &
   [{:keys [read! apply!]
     :or {read! (fn [& _]
                  catalog)
          apply! (fn [& _]
                   (assoc catalog "revision" "saved"))}}]]
  (with-redefs [vis/gateway-settings
                read!

                vis/gateway-toggle-setting!
                (fn [id target]
                  (swap! calls conj [:post id target])
                  (assoc row "enabled" true))

                vis/apply-settings!
                (fn [& args]
                  (swap! calls conj (into [:patch] args))
                  (apply apply! args))]

    (with-redefs-fn {#'dlg/load-inventories! dlg/load-settings-inventory!}
      #(let [result
             (cap/capture! {:cols 100
                            :rows 30
                            :keys keys
                            :paint! (fn [{:keys [screen]}]
                                      (try (dlg/settings-dialog! screen
                                                                 {}
                                                                 {:focus-section "Planning"
                                                                  :context-session-id "context"
                                                                  :settings-target {:scope "session"
                                                                                    :target-id
                                                                                    "selected"}})
                                           (finally (.stopScreen ^TerminalScreen screen))))})]
         (when-let [error (:error result)]
           (throw error)) result))))

(defdescribe
  staged-settings-editor
  (describe "protected gateway drafts"
            (it "does not write a gateway setting when Enter only stages a change"
                (let [calls (atom [])]
                  (capture-editor [:enter :esc \y] calls)
                  (expect (= [] @calls))))
            (it "sends one explicit batch only after F2 and the complete review"
                (let [calls (atom [])]
                  (capture-editor [:enter :f2 :down :f2 :esc] calls)
                  (expect (= [[:patch "initial" [{"id" "plans" "action" "value" "value" true}]
                               {:scope "session" :target-id "selected"} :tui "context"]]
                             @calls))))
            (it "applies the reviewed batch with Ctrl+S as well as F2"
                (let [calls
                      (atom [])

                      ctrl-s
                      (KeyStroke. \s true false false)]

                  (capture-editor [:enter ctrl-s :down ctrl-s :esc] calls)
                  (expect (= [[:patch "initial" [{"id" "plans" "action" "value" "value" true}]
                               {:scope "session" :target-id "selected"} :tui "context"]]
                             @calls))))
            (it "keeps editing when close confirmation is declined"
                (let [calls
                      (atom [])

                      result
                      (capture-editor [:enter :esc \n :f2 :down :f2 :esc] calls)]

                  (expect (= 1 (count @calls)))
                  (expect (some #(str/includes? (cap/frame-text %) "Pending changes")
                                (:frames result)))))
            (it "discards without writing when F3 is confirmed"
                (let [calls (atom [])]
                  (capture-editor [:enter :f3 \y :esc] calls)
                  (expect (empty? @calls))))
            (it "keeps the draft after a failed Apply"
                (let [calls
                      (atom [])

                      result
                      (capture-editor [:enter :f2 :down :f2 \q :esc \y]
                                      calls
                                      {:apply! (fn [& _]
                                                 (throw (ex-info "Connection lost" {})))})]

                  (expect (= 1 (count @calls)))
                  (expect (some #(str/includes? (cap/frame-text %) "Your draft is kept")
                                (:frames result))))))
  (describe "text editing"
            (it "starts after the current text and supports movement, deletion and new lines"
                (let [calls
                      (atom [])

                      text-row
                      {"id" "note"
                       "type" "string"
                       "value" "abc"
                       "label" "Note"
                       "source" "default"
                       "scope" "session"
                       "is_override" false
                       "inherited_value" "abc"
                       "inherited_source" "default"
                       "applies" "next_request"}]

                  (capture-editor [:enter :left :left :backspace :end \d :enter \e :up :home :delete
                                   :f2 :f2 :down :f2 :esc]
                                  calls
                                  {:read! (fn [& _]
                                            (assoc catalog
                                              "groups" [{"id" "response"
                                                         "title" "Planning"
                                                         "toggles" [text-row]}]))})
                  (expect (= [[:patch "initial" [{"id" "note" "action" "value" "value" "cd\ne"}]
                               {:scope "session" :target-id "selected"} :tui "context"]]
                             @calls))))))

(defdescribe
  typed-settings-drafts
  (describe
    "typed drafts and conflicts"
    (it
      "preserves explicit false and structured values without encoding JSON strings"
      (let [structured
            (assoc row
              "id" "jail_network"
              "type" "object"
              "value" {"allowed_domains" []})

            base
            (assoc catalog "groups" [{"toggles" [row structured]}])

            draft
            (-> (sm/start base)
                (sm/stage "plans" "value" false)
                (sm/stage "jail_network" "value" {"allow_private" false "inbound_ports" [8080]}))]

        (expect (false? (get (get-in draft [:changes "plans"]) "value")))
        (expect (some #(map? (get % "value")) (sm/changes draft)))
        (expect (sm/permission-change? draft))))
    (it "receives a new revision without replacing a dirty draft and explicitly rebases it"
        (let [draft
              (sm/stage (sm/start catalog) "plans" "value" true)

              latest
              (assoc catalog "revision" "new")

              received
              (sm/receive draft latest)]

          (expect (= "initial" (get-in received [:base "revision"])))
          (expect (= latest (:latest received)))
          (expect (= "new" (get-in (sm/rebase received) [:base "revision"])))
          (expect (= (sm/changes draft) (sm/changes (sm/rebase received))))))
    (it "previews inheritance from the fallback rather than the selected own value"
        (let [base
              (assoc catalog
                "groups" [{"toggles" [(assoc row
                                        "is_override" true
                                        "own_value" true
                                        "enabled" true)]}])

              draft
              (sm/stage (sm/start base) "plans" "inherit" nil)]

          (expect (= [{"id" "plans" "action" "inherit"}] (sm/changes draft)))
          (expect (false? (sm/setting-value (first (sm/rows (sm/preview draft))))))))
    (it
      "shows full structured before and after values in review"
      (let [base
            (assoc catalog
              "groups" [{"toggles" [(assoc row
                                      "id" "jail_network"
                                      "type" "object"
                                      "value" {"allowed_domains" ["gateway.example.com"]})]}])

            lines
            (sm/review-lines
              (sm/stage (sm/start base) "jail_network" "value" {"allowed_domains" ["127.0.0.1"]}))]

        (expect (some #(str/includes? % "gateway.example.com") lines))
        (expect (some #(str/includes? % "127.0.0.1") lines)))))
  (describe
    "portable profiles"
    (it "exports only selected explicit safe values and never credential fields"
        (let [secret
              (assoc row
                "id" "api_key"
                "type" "string"
                "is_override" true
                "own_value" "not-exported")

              base
              (assoc catalog
                "groups" [{"toggles" [(assoc row
                                        "is_override" true
                                        "own_value" false
                                        "enabled" true) secret]}])

              profile
              (sm/export-profile "Portable" base)]

          (expect (= [{"id" "plans" "action" "value" "value" false}] (get profile "changes")))
          (expect (= profile (sm/parse-profile (wire/json-str profile) base)))))
    (it "rejects unknown ids, duplicate entries, wrong types and inherit with a value"
        (doseq [changes [[{"id" "unknown" "action" "value" "value" true}]
                         [{"id" "plans" "action" "value" "value" "true"}]
                         [{"id" "plans" "action" "inherit" "value" false}]
                         [{"id" "plans" "action" "inherit"} {"id" "plans" "action" "inherit"}]]]
          (expect (try (sm/parse-profile (wire/json-str
                                           {"version" 1 "name" "Invalid" "changes" changes})
                                         catalog)
                       false
                       (catch Exception _ true)))))
    (it "stages an imported profile without a gateway write"
        (let [profile
              (sm/parse-profile (wire/json-str {"version" 1
                                                "name" "Imported"
                                                "changes"
                                                [{"id" "plans" "action" "value" "value" false}]})
                                catalog)

              draft
              (reduce (fn [draft {:strs [id action value]}]
                        (sm/stage draft id action value))
                      (sm/start catalog)
                      (get profile "changes"))]

          (expect (sm/dirty? draft))
          (expect (= false (get (first (sm/changes draft)) "value")))))))

(defn- scripted-structured-edit
  [row picks reads & [{:keys [lists notes]}]]
  (let [picks
        (atom picks)

        reads
        (atom reads)

        lists
        (atom lists)

        take!
        (fn [queue]
          (let [value (first @queue)]
            (swap! queue rest)
            value))]

    (with-redefs-fn {#'dlg/settings-pick! (fn [& _]
                                            (take! picks))
                     #'dlg/mini-read! (fn [& _]
                                        (take! reads))
                     #'dlg/settings-list-editor! (fn [& _]
                                                   (take! lists))
                     #'dlg/mini-note! (fn [_ _ _ title text]
                                        (swap! notes conj [title text]))}
      #((var-get #'dlg/settings-structured-editor!) nil nil nil row))))

(defdescribe
  guided-network-rules
  (describe "host rules without Advanced JSON"
            (it "adds a host rule with its access and an allowed request as typed values"
                (let [row {:label "Network"
                           :setting {"editor" "network"}
                           :toggle-value {"allowed_domains" ["gateway.example.com"]}}]
                  (expect (= {"allowed_domains" ["gateway.example.com"]
                              "rules" [{"host" "gateway.example.com"
                                        "access" "read-only"
                                        "allow" [{"method" "GET" "path" "/v1/*"}]}]}
                             (scripted-structured-edit row
                                                       ["rules" :add 0 "access" "read-only" 0
                                                        "allow" :add 0 "path" :done :done :done]
                                                       ["gateway.example.com" "GET" "/v1/*"])))))
            (it "refuses an out-of-range port and keeps the rule unchanged"
                (let [notes
                      (atom [])

                      row
                      {:label "Network" :setting {"editor" "network"} :toggle-value {}}]

                  (expect (= {"rules" [{"host" "gateway.example.com"}]}
                             (scripted-structured-edit row
                                                       ["rules" :add 0 "ports" :done :done]
                                                       ["gateway.example.com"]
                                                       {:lists [["70000"]] :notes notes})))
                  (expect (= [["Invalid ports" "Use one valid integer port per line."]] @notes))))))
