(ns com.blockether.vis.ext.channel-tui.transient.spec-test
  "The transient's CONTRACT on its own: the vocabulary every surface reads
   (`item-types` and the trait sets derived from it), the closed shapes a
   producer may hand the component, and the one-line prose each explainer
   answers with.

   The last test is the drift guard: every transient this channel actually
   ships is run past `spec-error`, so a band that grew a typo'd key, a second
   row on the same keystroke or two rows reporting the same `:id` fails HERE
   instead of in front of a human."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.ext.channel-tui.drafts :as drafts]
            [com.blockether.vis.ext.channel-tui.mcp-model :as mcp-model]
            [com.blockether.vis.ext.channel-tui.provider :as provider]
            [com.blockether.vis.ext.channel-tui.transient.spec :as sp]))

(def ^:private legal-item
  {:key "h" :type :switch :id :no-verify :label "Disable hooks" :arg "--no-verify"})

(def ^:private legal-spec
  {:title "Commit"
   :groups [{:title "Arguments" :items [legal-item]}
            {:title "Commands"
             :items
             [{:key "t" :type :option :id :topic :label "Topic" :arg "%topic=" :prompt "Topic"}
              {:key "c" :type :action :id :commit :label "Commit staged"}]}]})

(def ^:private legal-region {:left 0 :inner-w 78 :text-w 70 :hint-row 28})

(defn- one-line?
  "An explainer answers with prose a hint row can carry: present, and one line."
  [why]
  (and (string? why) (seq why) (not (str/includes? why "\n"))))

(defdescribe transient-vocabulary-test
             (it "every item type declares every trait, so no branch reads a missing key"
                 (expect (= #{:switch :option :action} (set (keys sp/item-types))))
                 (expect (every? #(= #{:is-flag :is-valued :is-command} (set (keys %)))
                                 (vals sp/item-types))))
             (it "the trait sets are DERIVED from that one table, never a second copy"
                 (expect (= #{:switch :option} sp/flag-types))
                 (expect (= #{:option} sp/valued-types))
                 (expect (= #{:action} sp/command-types))
                 ;; A flag toggles in place and a command ends the run: nothing is both.
                 (expect (empty? (set/intersection sp/flag-types sp/command-types)))
                 ;; Everything a transient can offer is one of the two.
                 (expect (= (set (keys sp/item-types)) (set/union sp/flag-types sp/command-types))))
             (it "the key sets are derived too: a value key is an item key"
                 (expect (every? sp/item-keys sp/value-keys))
                 (expect (contains? sp/item-keys :key))
                 (expect (contains? sp/spec-keys :read-option))))

(defdescribe
  transient-item-contract-test
  (it "a legal row passes"
      (expect (nil? (sp/item-error legal-item)))
      (expect (nil? (sp/item-error {:key "c" :type :action :id :commit :label "Commit"}))))
  (it "the map is CLOSED: a typo'd key is a row that silently does nothing"
      (expect (one-line? (sp/item-error (assoc legal-item :labl "Disable hooks")))))
  (it "a binding is ONE character, because that is what a key loop delivers"
      (expect (one-line? (sp/item-error (assoc legal-item :key "ho"))))
      (expect (one-line? (sp/item-error (assoc legal-item :key ""))))
      ;; A Character `:key` reads fine but indexes as a Character, so the key
      ;; loop — which looks a keystroke up as `(str ch)` — would never find it.
      (expect (one-line? (sp/item-error (assoc legal-item :key \h)))))
  (it "every row needs a type from the vocabulary, an id and a label"
      (expect (one-line? (sp/item-error (assoc legal-item :type :toggle))))
      (expect (one-line? (sp/item-error (dissoc legal-item :id))))
      (expect (one-line? (sp/item-error (dissoc legal-item :label)))))
  (it "`:arg` is what a FLAG contributes; a command contributes none"
      (expect (one-line? (sp/item-error
                           {:key "c" :type :action :id :commit :label "Commit" :arg "--nope"})))
      (expect (one-line? (sp/item-error (assoc legal-item :arg "")))))
  (it "how a value is asked for only means something on a VALUED row"
      (expect (nil? (sp/item-error {:key "t"
                                    :type :option
                                    :id :topic
                                    :label "Topic"
                                    :prompt "Topic"
                                    :mask \•
                                    :secret? true})))
      (expect (one-line? (sp/item-error (assoc legal-item :prompt "Topic"))))
      (expect (one-line? (sp/item-error (assoc legal-item :secret? true))))
      (expect (one-line? (sp/item-error
                           {:key "t" :type :option :id :topic :label "Topic" :secret? "yes"})))))

(defdescribe
  transient-spec-contract-test
  (it "a legal transient passes, with or without a title"
      (expect (nil? (sp/spec-error legal-spec)))
      (expect (nil? (sp/spec-error (dissoc legal-spec :title)))))
  (it "groups and items may be LAZY: a band builds its rows from live data"
      (expect (nil? (sp/spec-error {:groups (map identity (:groups legal-spec))})))
      (expect (nil? (sp/spec-error {:groups [{:title "Commands"
                                              :items (map (fn [i]
                                                            {:key (str i)
                                                             :type :action
                                                             :id (keyword (str "a" i))
                                                             :label "Go"})
                                                          (range 3))}]}))))
  (it "an empty popup is not a popup"
      (expect (one-line? (sp/spec-error {:groups []})))
      (expect (one-line? (sp/spec-error {:groups [{:title "Commands" :items []}]})))
      (expect (one-line? (sp/spec-error {}))))
  (it "two rows on one keystroke leave one of them unreachable"
      (expect (one-line? (sp/spec-error
                           {:groups [{:title "Commands"
                                      :items
                                      [{:key "c" :type :action :id :commit :label "Commit"}
                                       {:key "c" :type :action :id :clean :label "Clean"}]}]}))))
  (it "two rows reporting one `:id` make the result ambiguous"
      (expect (one-line? (sp/spec-error
                           {:groups [{:title "Commands"
                                      :items
                                      [{:key "c" :type :action :id :commit :label "Commit"}
                                       {:key "a" :type :action :id :commit :label "Amend"}]}]}))))
  (it "the spec map is closed, and `:read-option` must be callable"
      (expect (one-line? (sp/spec-error (assoc legal-spec :footer "hi"))))
      (expect (one-line? (sp/spec-error (assoc legal-spec :read-option "nope"))))
      (expect (nil? (sp/spec-error (assoc legal-spec :read-option (constantly "x")))))))

(defdescribe transient-surface-contract-test
             (it "a region is the rectangle the component paints into"
                 (expect (nil? (sp/region-error legal-region)))
                 ;; Assembled by a surface, so it may carry its own extras.
                 (expect (nil? (sp/region-error (assoc legal-region
                                                  :min-row 4
                                                  :restore! (fn [_ _])))))
                 (expect (one-line? (sp/region-error (dissoc legal-region :hint-row))))
                 (expect (one-line? (sp/region-error (assoc legal-region :inner-w 0)))))
             (it "a SIDELESS band wipes the FULL width, so it has to be told what it is"
                 (expect (one-line? (sp/region-error (assoc legal-region :is-sideless true))))
                 (expect (nil? (sp/region-error (assoc legal-region
                                                  :is-sideless true
                                                  :cols 100)))))
             (it "a host paints, flushes and answers keystrokes"
                 (expect (nil? (sp/host-error {:g :graphics
                                               :hint-bar! (fn [& _])
                                               :refresh! (fn [])
                                               :read-key! (fn [])})))
                 (expect (one-line? (sp/host-error {:g :graphics
                                                    :hint-bar! (fn [& _])
                                                    :refresh! (fn [])})))
                 (expect (one-line? (sp/host-error {:g :graphics
                                                    :hint-bar! (fn [& _])
                                                    :refresh! (fn [])
                                                    :read-key! :not-a-fn})))))

(defdescribe transient-state-contract-test
             (it "the run state is armed flags and the values options hold"
                 (expect (nil? (sp/state-error {:switches #{:no-verify} :options {:topic "fix"}})))
                 (expect (one-line? (sp/state-error {:switches [:no-verify] :options {}})))
                 (expect (one-line? (sp/state-error {:switches #{} :options {} :page 2}))))
             (it "an OPTION carries text a row can PAINT — never nil, blank or a widget"
                 (expect (nil? (sp/option-value-error "fix")))
                 (expect (one-line? (sp/option-value-error "")))
                 (expect (one-line? (sp/option-value-error "   ")))
                 (expect (one-line? (sp/option-value-error 42)))
                 (expect (one-line? (sp/option-value-error nil)))))

(defdescribe shipped-transients-are-legal-test
             (it "every transient this channel ships satisfies the contract it declares"
                 (let
                   [drafts
                    [{:id "d1" :name "d1" :title "Fix the thing"}]

                    shipped
                    {"drafts/spec" (drafts/spec drafts)
                     "drafts/switch-spec" (drafts/switch-spec drafts)
                     "drafts/start-in-spec" drafts/start-in-spec
                     "provider/provider-transient-spec"
                     (provider/provider-transient-spec
                       [{:key "a" :type :action :id :add :label "Add a provider"}])
                     "provider/api-key-transient-spec" (provider/api-key-transient-spec)
                     "provider/model-transient-spec"
                     (provider/model-transient-spec [{:id "gpt" :label "gpt"}] 0 #{})
                     "provider/preset-transient-spec"
                     (provider/preset-transient-spec [{:id "p" :label "Preset"}] 0)
                     "provider/local-setup-transient-spec"
                     (provider/local-setup-transient-spec "Ollama" "http://127.0.0.1:11434")
                     "mcp-model/server-transient-spec" (mcp-model/server-transient-spec
                                                         {:id "srv" :name "srv"})}]

                   (expect (= {}
                              (into {}
                                    (keep (fn [[nm spec]]
                                            (when-let [why (sp/spec-error spec)]
                                              [nm why])))
                                    shipped))))))
