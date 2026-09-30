(ns com.blockether.vis.tui.settings-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.client :as vis])
  (:import [com.googlecode.lanterna TerminalPosition]
           [com.googlecode.lanterna.input MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]))

(def ^:private engine-names ["vis-lang-python" "einmal" "foundation-mcp" "vis-lang-clojure"])

(def ^:private settings-rows
  (vec (concat [{:type :section :label "Extension engines"}]
               (for [label engine-names]
                 {:type :registry-toggle
                  :toggle-id (str "compact-settings::" label)
                  :toggle-type :enum
                  :toggle-value "auto"
                  :choices ["auto" "on" "off"]
                  :label label
                  :source "default"
                  :description "Load this extension when its tools are needed."})
               [{:type :section :label "Paths and access"}
                {:type :toggle
                 :key :read-files
                 :label "Read filesystem"
                 :description "Allow filesystem reads."} {:type :section :label "Response"}
                {:type :toggle :key :thinking :label "Show reasoning"}])))

(defn- capture-settings
  "Capture the production settings loop with a deterministic catalog and no gateway."
  [rows keys &
   {:keys [cols callbacks values load!]
    :or {cols 100 callbacks {} values {} load! (constantly nil)}}]
  (let [result (cap/capture! {:cols cols
                              :rows 30
                              :keys keys
                              :paint! (fn [{:keys [screen]}]
                                        (try (with-redefs-fn {#'dlg/settings-rows
                                                              (if (fn? rows) rows (constantly rows))
                                                              #'dlg/load-inventories! load!}
                                               #(dlg/settings-dialog! screen values callbacks))
                                             (finally (.stopScreen ^TerminalScreen screen))))})]
    (when-let [error (:error result)]
      (throw error))
    result))

(defdescribe
  compact-settings-test
  (describe "selected category"
            (it "shows only the focused category, not the rest of the catalog"
                (let [frame (cap/frame-text (capture-settings settings-rows
                                                              [:esc]
                                                              :callbacks
                                                              {:focus-section
                                                               "Extension engines"}))]
                  (expect (every? #(str/includes? frame %) engine-names))
                  (expect (not (str/includes? frame "Read filesystem")))
                  (expect (not (str/includes? frame "Show reasoning"))))))
  (describe
    "compact rows"
    (it "uses one painted line per setting and keeps prose out of the list"
        (let [rows
              (subvec settings-rows 0 5)

              entries
              (#'dlg/settings-render-entries rows 16)

              frame
              (cap/frame-text
                (capture-settings rows [:esc] :callbacks {:focus-section "Extension engines"}))]

          (expect (= 4 (count (filter #(= :option (:part %)) entries))))
          (expect (not-any? #(= :option-desc (:part %)) entries))
          (expect (not (str/includes? frame "Load this extension")))))))

(defdescribe
  compact-settings-navigation-test
  (it "changes category with Tab and Shift+Tab, including without a sidebar"
      (doseq [cols
              [40 100]

              [key expected]
              [[:tab :read-files] [:reverse-tab :thinking]]]

        (let [capture (capture-settings settings-rows
                                        [key :enter :esc]
                                        :cols cols
                                        :values {:read-files false :thinking false}
                                        :callbacks {:focus-section "Extension engines"})]
          (expect (= (assoc {:read-files false :thinking false} expected true) (:ret capture))))))
  (it "wraps category navigation without showing another category's settings"
      (let [frame (cap/frame-text (capture-settings settings-rows [:tab :tab :tab :esc]))]
        (expect (every? #(str/includes? frame %) engine-names))
        (expect (not (str/includes? frame "Read filesystem")))))
  (it "keeps arrow and page movement inside the selected category"
      (let [capture (capture-settings settings-rows
                                      [:down :page-down :up :enter :esc]
                                      :values {:read-files false :thinking false}
                                      :callbacks {:focus-section "Paths and access"})]
        (expect (= {:read-files true :thinking false} (:ret capture)))))
  (it "shows an empty category's explanation instead of activating an unrelated setting"
      (let [rows
            (into settings-rows
                  [{:type :section :label "MCP Servers"}
                   {:type :info :label "No MCP servers" :description "Add a server to start."}])

            capture
            (capture-settings rows [:enter :f1 :esc] :callbacks {:focus-section "MCP Servers"})

            frame
            (cap/frame-text capture)]

        (expect (= {} (:ret capture)))
        (expect (str/includes? frame "No MCP servers"))
        (expect (str/includes? frame "Add a server to start."))))
  (it "refocuses the requested category when the initial inventory arrives"
      (let [rows
            (atom (subvec settings-rows 5))

            capture
            (capture-settings #(deref rows)
                              [:esc]
                              :load! #(reset! rows settings-rows)
                              :callbacks {:focus-section "Extension engines"})

            frame
            (cap/frame-text capture)]

        (expect (every? #(str/includes? frame %) engine-names))
        (expect (not (str/includes? frame "Read filesystem")))))
  (it
    "keeps the active category visible and maps clicks after the sidebar scrolls"
    (let [rows
          (vec
            (mapcat (fn [i]
                      [{:type :section :label (str "Category " i)}
                       {:type :toggle :key (keyword (str "setting-" i)) :label (str "Setting " i)}])
                    (range 24)))

          callbacks
          {:focus-section "Category 23"}

          frame
          (cap/frame-text (capture-settings rows [:esc] :cols 120 :callbacks callbacks))

          lines
          (str/split-lines frame)

          y
          (first (keep-indexed (fn [i line]
                                 (when (str/includes? line "Category 22") i))
                               lines))

          x
          (.indexOf ^String (nth lines y) "Category 22")

          capture
          (capture-settings rows
                            [(MouseAction. MouseActionType/CLICK_DOWN 0 (TerminalPosition. x y))
                             (MouseAction. MouseActionType/CLICK_RELEASE 0 (TerminalPosition. x y))
                             :enter :esc]
                            :cols 120
                            :callbacks callbacks)]

      (expect (= 2 (count (re-seq #"Category 23" frame))))
      (expect (= {:setting-22 true} (:ret capture)))
      (expect (str/includes? (cap/frame-text capture) "Setting 22")))))

(defdescribe compact-settings-search-test
             (it "searches hidden descriptions across all categories and edits the matching setting"
                 (let [capture (capture-settings settings-rows
                                                 (concat "filesystem reads" [:enter :esc :esc])
                                                 :values
                                                 {:read-files false})]
                   (expect (= {:read-files true} (:ret capture)))
                   (expect (some #(str/includes? (cap/frame-text %) "Read filesystem")
                                 (:frames capture)))))
             (it "handles no results and clears the query before closing"
                 (let [capture (capture-settings settings-rows
                                                 (concat "no matching setting"
                                                         [:down :up :page-down :page-up :f1 :enter
                                                          :esc :esc]))]
                   (expect (= {} (:ret capture)))
                   (expect (every? #(str/includes? (cap/frame-text capture) %) engine-names)))))

(defdescribe
  compact-settings-details-test
  (it "aligns four single-line values and explains engine modes only once"
      (let [frame
            (cap/frame-text (capture-settings settings-rows [:esc]))

            lines
            (str/split-lines frame)

            engine-lines
            (filter #(some (fn [label]
                             (str/includes? % label))
                           engine-names)
                    lines)

            indexes
            (keep-indexed (fn [i line]
                            (when (some #(str/includes? line %) engine-names) i))
                          lines)]

        (expect (= 4 (count engine-lines)))
        (expect (= (range (first indexes) (+ 4 (first indexes))) indexes))
        (expect (every? #(str/includes? % "Auto") engine-lines))
        (expect (apply = (map #(.indexOf ^String % "Auto") engine-lines)))
        (expect (= 1 (count (re-seq #"when applicable" frame))))))
  (it "opens full descriptions and source with F1 and returns without saving"
      (doseq [cols [40 100]]
        (let [capture (capture-settings settings-rows [:f1 :esc :esc] :cols cols)
              frames (mapv cap/frame-text (:frames capture))]

          (expect (= {} (:ret capture)))
          (expect (not (str/includes? (first frames) "Source:")))
          (expect (some #(str/includes? % "Source: default") frames))
          (expect (some #(str/includes? % "extension when") frames))
          (expect (not (str/includes? (last frames) "Source:"))))))
  (it "changes the selected value with Enter in details"
      (let [capture (capture-settings settings-rows
                                      [:f1 :enter :esc]
                                      :values {:read-files false}
                                      :callbacks {:focus-section "Paths and access"})]
        (expect (= {:read-files true} (:ret capture)))))
  (it
    "keeps source and inheritance metadata out of the list and uses one row per override"
    (let [rows
          (#'dlg/catalog-toggle-rows
           [{"title" "Paths and access"
             "toggles" [{"id" "compact_override"
                         "type" "boolean"
                         "enabled" true
                         "label" "Read filesystem"
                         "description" "Allow filesystem reads."
                         "source" "project"
                         "is_override" true}]}])

          calls
          (atom [])

          target
          {:scope :project :target-id "example-project"}

          capture
          (with-redefs [vis/inherit-setting! (fn [id scope]
                                               (swap! calls conj [id scope])
                                               {"id" id "enabled" false})]
            (with-redefs-fn {#'dlg/load-settings-inventory! (constantly nil)}
              #(capture-settings rows [:f1 \i :esc] :callbacks {:settings-target target})))

          frames
          (mapv cap/frame-text (:frames capture))]

      (expect (= 2 (count rows)))
      (expect (= "Allow filesystem reads." (:description (second rows))))
      (expect (= [["compact_override" target]] @calls))
      (expect (str/includes? (first frames) "[Override]"))
      (expect (not (str/includes? (first frames) "Source:")))
      (expect (some #(str/includes? % "Source: project") frames))))
  (it "explains locked values and prevents both edit and inherit in details"
      (let [row
            {:type :registry-toggle
             :toggle-id "compact_locked"
             :toggle-type :boolean
             :toggle-value false
             :is-override? true
             :source "project"
             :label "Read filesystem"
             :description "Allow filesystem reads."
             :locked "Project settings decide this value. Change it in Project settings."}

            calls
            (atom [])

            capture
            (with-redefs [vis/inherit-setting!
                          (fn [& args]
                            (swap! calls conj args))

                          vis/set-setting-value!
                          (fn [& args]
                            (swap! calls conj args))]

              (capture-settings [{:type :section :label "Paths and access"} row]
                                [:f1 :enter \i :esc :esc]))]

        (expect (empty? @calls))
        (expect (some #(str/includes? (cap/frame-text %) "Project settings decide")
                      (:frames capture)))))
  (it "keeps text settings editable in the compact list"
      (let [rows
            (#'dlg/catalog-toggle-rows
             [{"title" "Paths and access"
               "toggles" [{"id" "compact_text"
                           "type" "string"
                           "value" "example"
                           "label" "Workspace label"
                           "source" "project"}]}])

            calls
            (atom [])]

        (with-redefs [vis/set-setting-value! (fn [id value target]
                                               (swap! calls conj [id value target])
                                               {"id" id "value" value})]
          (with-redefs-fn {#'dlg/load-settings-inventory! (constantly nil)}
            #(capture-settings rows [:enter \x :enter :esc])))
        (expect (= [["compact_text" "examplex" nil]] @calls)))))

(defdescribe compact-settings-empty-state-test
             (it "shows a useful message for a search with no matches"
                 (let [capture (capture-settings settings-rows (concat "no match" [:esc :esc]))]
                   (expect (some #(str/includes? (cap/frame-text %) "No matching settings")
                                 (:frames capture)))))
             (it "preserves gateway errors before the first category"
                 (let [rows
                       (into [{:type :info
                               :tone :bad
                               :label "Settings unavailable"
                               :description "Gateway connection refused."}]
                             (subvec settings-rows 5))

                       frame
                       (cap/frame-text (capture-settings rows [:esc]))]

                   (expect (str/includes? frame "Settings unavailable"))
                   (expect (str/includes? frame "Gateway connection refused."))))
             (it "does not claim a default source when the catalog is unavailable"
                 (let [lines
                       (#'dlg/settings-details-lines (second settings-rows) {})

                       unknown
                       (#'dlg/settings-details-lines (dissoc (second settings-rows) :source) {})]

                   (expect (some #{"Source: default"} lines))
                   (expect (some #{"Source: unavailable"} unknown)))))
