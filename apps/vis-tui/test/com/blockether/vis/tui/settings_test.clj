(ns com.blockether.vis.tui.settings-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna TerminalPosition TextColor]
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

(def ^:private long-settings-rows
  (vec (mapcat (fn [i]
                 [{:type :section :label (str "Category " i)}
                  {:type :toggle :key (keyword (str "setting-" i)) :label (str "Setting " i)}])
               (range 24))))

(defn- capture-settings
  "Capture the production settings loop with a deterministic catalog and no gateway.
   Settings opens with every section folded. Only a test about the folds passes
   `:folded? true`; other tests see every section open."
  [rows keys &
   {:keys [cols callbacks values load! folded?]
    :or {cols 100 callbacks {} values {} load! (constantly nil) folded? false}}]
  (let [fold
        @#'dlg/settings-fold-rows

        result
        (cap/capture! {:cols cols
                       :rows 30
                       :keys keys
                       :paint! (fn [{:keys [screen]}]
                                 (try (with-redefs-fn
                                        {#'dlg/settings-rows (if (fn? rows) rows (constantly rows))
                                         #'dlg/load-inventories! load!
                                         #'dlg/settings-fold-rows
                                         (if folded?
                                           fold
                                           (fn [rows opened]
                                             (fold rows (into (set opened) (keep :label) rows))))}
                                        #(dlg/settings-dialog! screen values callbacks))
                                      (finally (.stopScreen ^TerminalScreen screen))))})]

    (when-let [error (:error result)]
      (throw error))
    result))

(defdescribe
  compact-settings-test
  (describe "full catalog"
            (it "keeps other sections visible when an extension section is focused"
                (let [frame (cap/frame-text (capture-settings settings-rows
                                                              [:esc]
                                                              :callbacks
                                                              {:focus-section
                                                               "Extension engines"}))]
                  (expect (every? #(str/includes? frame %) engine-names))
                  (expect (str/includes? frame "Read filesystem"))
                  (expect (str/includes? frame "Show reasoning")))))
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
  (it "does not switch or hide sections with Tab or Shift+Tab"
      (doseq [cols
              [40 100]

              key
              [:tab :reverse-tab]]

        (let [capture (capture-settings settings-rows
                                        [key :enter :down :esc]
                                        :cols cols
                                        :values {:read-files false :thinking false}
                                        :callbacks {:focus-section "Paths and access"})]
          (expect (= {:read-files true :thinking false} (:ret capture)))
          (expect (str/includes? (cap/frame-text capture) "Show reasoning")))))
  (it "moves across section boundaries with the arrow keys"
      (doseq [cols
              [40 100]

              [section key expected]
              [["Paths and access" :down :thinking] ["Response" :up :read-files]]]

        (let [capture (capture-settings settings-rows
                                        [key :enter :esc]
                                        :cols cols
                                        :values {:read-files false :thinking false}
                                        :callbacks {:focus-section section})]
          (expect (= (assoc {:read-files false :thinking false} expected true) (:ret capture))))))
  (it "moves across section boundaries with the mouse wheel"
      (doseq [[section wheel expected] [["Paths and access" MouseActionType/SCROLL_DOWN :thinking]
                                        ["Response" MouseActionType/SCROLL_UP :read-files]]]
        (let [capture (capture-settings settings-rows
                                        [(MouseAction. wheel 0 (TerminalPosition. 10 10)) :enter
                                         :esc]
                                        :values {:read-files false :thinking false}
                                        :callbacks {:focus-section section})]
          (expect (= (assoc {:read-files false :thinking false} expected true) (:ret capture))))))
  (it "opens with every section folded except the one it focuses"
      (let [frame (cap/frame-text (capture-settings settings-rows [:esc] :folded? true))]
        (expect (every? #(str/includes? frame %)
                        ["Extension engines ▸" "Paths and access ▸" "Response ▸"]))
        (expect (not (str/includes? frame "Read filesystem")))
        ;; Enter on the first folded header opens it.
        (expect (str/includes? (cap/frame-text
                                 (capture-settings settings-rows [:enter :esc] :folded? true))
                               "Extension engines ▾"))))
  (it "folds a section with Left and opens it with Right or Enter"
      (let [run
            #(capture-settings settings-rows
                               %
                               :folded? true
                               :values {:read-files false :thinking false}
                               :callbacks {:focus-section "Paths and access"})

            focused
            (cap/frame-text (run [:esc]))

            folded
            (cap/frame-text (run [:left :esc]))]

        (expect (str/includes? focused "Paths and access ▾"))
        (expect (str/includes? focused "Read filesystem"))
        (expect (str/includes? focused "Response ▸"))
        (expect (str/includes? folded "Paths and access ▸"))
        (expect (not (str/includes? folded "Read filesystem")))
        ;; The sidebar still counts the hidden settings.
        (expect
          (= [["Extension engines" 4] ["Paths and access" 1] ["Response" 1]]
             (mapv (juxt :label :count)
                   (#'dlg/settings-toc (#'dlg/settings-fold-rows settings-rows #{"Response"}) 0))))
        (doseq [open-key [:right :enter]]
          (expect (= {:read-files true :thinking false}
                     (:ret (run [:left open-key :enter :esc])))))))
  (it "folds and opens a section with a press on its header"
      (let [[y x]
            (first (keep-indexed (fn [y ^String line]
                                   (let [x (.indexOf line "── Response")]
                                     (when (<= 0 x) [y x])))
                                 (str/split-lines
                                   (cap/frame-text
                                     (capture-settings settings-rows [:esc] :folded? true)))))

            press
            (fn [x y]
              [(MouseAction. MouseActionType/CLICK_DOWN 0 (TerminalPosition. x y))
               (MouseAction. MouseActionType/CLICK_RELEASE 0 (TerminalPosition. x y))])

            opened
            (cap/frame-text
              (capture-settings settings-rows (conj (press (+ x 4) y) :esc) :folded? true))

            folded
            (cap/frame-text (capture-settings settings-rows
                                              (concat (press (+ x 4) y) (press (+ x 4) y) [:esc])
                                              :folded?
                                              true))]

        (expect (str/includes? opened "Response ▾"))
        (expect (str/includes? opened "Show reasoning"))
        (expect (str/includes? folded "Response ▸"))
        (expect (not (str/includes? folded "Show reasoning")))))
  (it "shows every match while a search runs"
      (let [capture
            (capture-settings settings-rows (concat "show" [:esc :esc]) :folded? true)

            ;; The frame before the first Esc shows the search result.
            searched
            (cap/frame-text capture (- (count (:frames capture)) 2))]

        (expect (str/includes? searched "Show reasoning"))
        (expect (not (str/includes? searched "▸")))
        ;; Clearing the search shows the fold again.
        (expect (str/includes? (cap/frame-text capture) "Response ▸"))))
  (it "pages through the full catalog in either direction without a category switch"
      (doseq [cols
              [40 100]

              [section key initial]
              [["Category 0" :page-down :setting-0] ["Category 23" :page-up :setting-23]]]

        (let [capture (capture-settings long-settings-rows
                                        [key :enter :esc]
                                        :cols cols
                                        :callbacks {:focus-section section})]
          (expect (= 1 (count (:ret capture))))
          (expect (not (contains? (:ret capture) initial))))))
  (it "jumps to the first and last settings with Home and End"
      (doseq [[key expected] [[:home :setting-0] [:end :setting-23]]]
        (let [capture (capture-settings long-settings-rows
                                        [key :enter :esc]
                                        :callbacks
                                        {:focus-section "Category 12"})]
          (expect (= {expected true} (:ret capture))))))
  (it "shows an empty section's explanation instead of activating an unrelated setting"
      (let [rows
            (vec (concat
                   (subvec settings-rows 0 5)
                   [{:type :section :label "MCP Servers"}
                    {:type :info :label "No MCP servers" :description "Add a server to start."}]
                   (subvec settings-rows 5)))

            capture
            (capture-settings rows [:enter :f1 :esc] :callbacks {:focus-section "MCP Servers"})

            frame
            (cap/frame-text capture)]

        (expect (= {} (:ret capture)))
        (expect (str/includes? frame "No MCP servers"))
        (expect (str/includes? frame "Add a server to start."))))
  (it "refocuses the requested section when the initial inventory arrives without hiding others"
      (let [rows
            (atom (subvec settings-rows 0 5))

            capture
            (capture-settings #(deref rows)
                              [:enter :esc]
                              :values {:read-files false :thinking false}
                              :load! #(reset! rows settings-rows)
                              :callbacks {:focus-section "Paths and access"})

            frame
            (cap/frame-text capture)]

        (expect (= {:read-files true :thinking false} (:ret capture)))
        (expect (every? #(str/includes? frame %) engine-names))
        (expect (str/includes? frame "Read filesystem"))))
  (it "keeps the active section visible and maps scroll-jump clicks after the sidebar scrolls"
      (let [callbacks
            {:focus-section "Category 23"}

            frame
            (cap/frame-text
              (capture-settings long-settings-rows [:esc] :cols 120 :callbacks callbacks))

            lines
            (str/split-lines frame)

            y
            (first (keep-indexed (fn [i line]
                                   (when (re-find #"^\s*│ Category 22\s" line) i))
                                 lines))

            x
            (.indexOf ^String (nth lines y) "Category 22")

            capture
            (capture-settings long-settings-rows
                              [(MouseAction. MouseActionType/CLICK_DOWN 0 (TerminalPosition. x y))
                               (MouseAction. MouseActionType/CLICK_RELEASE
                                             0
                                             (TerminalPosition. x y)) :enter :esc]
                              :cols 120
                              :callbacks callbacks)]

        (expect (= 2 (count (re-seq #"Category 23" frame))))
        (expect (= {:setting-22 true} (:ret capture)))
        (expect (str/includes? (cap/frame-text capture) "Setting 22"))
        (expect (str/includes? (cap/frame-text capture) "Setting 23")))))

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
    (let [target
          {:scope :project :target-id "example-project"}

          ;; A scoped dialog projects its rows with its target bound.
          rows
          (binding [dlg/*settings-target* target]
            (#'dlg/catalog-toggle-rows
             [{"title" "Paths and access"
               "toggles" [{"id" "compact_override"
                           "type" "boolean"
                           "enabled" true
                           "label" "Read filesystem"
                           "description" "Allow filesystem reads."
                           "source" "project"
                           "is_override" true}]}]))

          calls
          (atom [])

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
  (it "marks no override and offers no inherit in global settings"
      (let [rows
            (#'dlg/catalog-toggle-rows
             [{"title" "Paths and access"
               "toggles" [{"id" "compact_override"
                           "type" "boolean"
                           "enabled" true
                           "label" "Read filesystem"
                           "description" "Allow filesystem reads."
                           "source" "global"
                           "is_override" true}]}])

            calls
            (atom [])

            capture
            (with-redefs [vis/inherit-setting! (fn [id scope]
                                                 (swap! calls conj [id scope])
                                                 {})]
              (with-redefs-fn {#'dlg/load-settings-inventory! (constantly nil)}
                #(capture-settings rows [:f1 \i :esc :esc])))

            frames
            (mapv cap/frame-text (:frames capture))]

        (expect (false? (:is-override? (second rows))))
        (expect (empty? @calls))
        (expect (not-any? #(str/includes? % "[Override]") frames))
        (expect (not-any? #(str/includes? % "overrides the inherited value") frames))))
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

(defn- typed-rows
  "Catalog rows for one typed gateway setting."
  [setting]
  (#'dlg/catalog-toggle-rows
   [{"title" "Typed values" "toggles" [(merge {"source" "default"} setting)]}]))

(defn- capture-saves
  "Edit `rows` with `keys`; answer the values sent to the gateway and the notes shown."
  [rows keys]
  (let [calls
        (atom [])

        notes
        (atom [])]

    (with-redefs [vis/set-setting-value! (fn [id value target]
                                           (swap! calls conj [id value target])
                                           {"id" id "value" value})]
      (with-redefs-fn {#'dlg/load-settings-inventory! (constantly nil)
                       #'dlg/mini-note! (fn [_ _ _ title text]
                                          (swap! notes conj [title text]))}
        #(capture-settings rows keys)))
    {:calls @calls :notes @notes}))

(defdescribe
  typed-settings-test
  (describe
    "immediate saves"
    (it "saves a number when the reader confirms it"
        (expect (= {:calls [["compact_number" 12 nil]] :notes []}
                   (capture-saves
                     (typed-rows
                       {"id" "compact_number" "type" "number" "value" 4 "label" "Parallel tools"})
                     [:enter :backspace \1 \2 :enter :esc]))))
    (it "keeps invalid number text for correction and saves only a valid number"
        (expect (= {:calls [["compact_number" 4.5 nil]]
                    :notes [["Invalid number" "Enter a finite number. Your text is kept."]]}
                   (capture-saves
                     (typed-rows
                       {"id" "compact_number" "type" "number" "value" 4 "label" "Parallel tools"})
                     [:enter \x :enter :backspace \. \5 :enter :esc]))))
    (it "edits list entries with movement, deletion and new lines before it saves them"
        ;; The text editor once ignored arrows, Backspace, Delete and Enter (1bf1b2471).
        (expect (= {:calls [["compact_list" ["cd" "e"] nil]] :notes []}
                   (capture-saves (typed-rows {"id" "compact_list"
                                               "type" "array"
                                               "editor" "list"
                                               "value" ["abc"]
                                               "label" "Denied executables"})
                                  [:enter :left :left :backspace :end \d :enter \e :up :home :delete
                                   :f2 :esc]))))))

(defn- scripted-structured-edit
  [row picks reads & [{:keys [lists notes menus]}]]
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

    (with-redefs-fn {#'dlg/settings-pick! (fn [_ title items]
                                            (expect (not-any? #(= :advanced (:value %)) items))
                                            (when menus (swap! menus assoc title items))
                                            (take! picks))
                     #'dlg/mini-read! (fn [& _]
                                        (take! reads))
                     #'dlg/settings-list-editor! (fn [& _]
                                                   (take! lists))
                     #'dlg/mini-note! (fn [_ _ _ title text]
                                        (swap! notes conj [title text]))}
      #((var-get #'dlg/settings-structured-editor!) nil nil nil row))))

(defdescribe
  guided-settings-editors-test
  (describe
    "ordinary controls"
    (it "edits workspace paths and preserves other attributes"
        (let [entry {"id" "docs" "path" "~/docs" "description" "Project notes"}]
          (expect (= [(assoc entry "path" "~/project")]
                     (scripted-structured-edit
                       {:label "Workspace roots" :setting {"editor" "paths"} :toggle-value [entry]}
                       [0 "path" :done]
                       ["~/project"])))))
    (it "edits allowed filesystem paths without changing blocked paths"
        (let [value {"allow" ["~/docs"] "deny_read" ["~/private"] "deny_write" ["~/readonly"]}]
          (expect (= (assoc value "allow" ["~/project"])
                     (scripted-structured-edit {:label "Filesystem access"
                                                :setting {"editor" "filesystem"}
                                                :toggle-value value}
                                               ["allow" :done]
                                               []
                                               {:lists [["~/project"]]}))))))
  (it "directs unsupported settings to the configuration file without opening a raw editor"
      (let [editors
            (atom [])

            notes
            (atom [])]

        (with-redefs-fn {#'dlg/settings-text-editor! (fn [& args]
                                                       (swap! editors conj args)
                                                       nil)
                         #'dlg/mini-note! (fn [_ _ _ title text]
                                            (swap! notes conj [title text]))}
          #(expect (nil? (#'dlg/settings-structured-editor!
                          nil
                          nil
                          nil
                          {:label "Custom configuration" :setting {} :toggle-value {}}))))
        (expect (empty? @editors))
        (expect (= [["Edit configuration file" "Edit this setting in your configuration file."]]
                   @notes)))))

(defdescribe
  guided-network-rules-test
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

(defdescribe
  unique-access-choices-test
  (it "shows two workspace access choices and saves a canonical value"
      (let [menus
            (atom {})

            entry
            {"id" "docs" "path" "~/docs" "access" "ro"}]

        (expect (= [(assoc entry "access" "read-write")]
                   (scripted-structured-edit
                     {:label "Workspace roots" :setting {"editor" "paths"} :toggle-value [entry]}
                     [0 "access" "read-write" :done]
                     []
                     {:menus menus})))
        (expect (= ["read-only" "read-write"] (mapv :value (get @menus "Workspace access"))))))
  (it "shows three network access choices and preserves the deny option"
      (let [menus
            (atom {})

            entry
            {"host" "gateway.example.com" "access" "closed"}]

        (expect (= {"rules" [(assoc entry "access" "none")]}
                   (scripted-structured-edit {:label "Network"
                                              :setting {"editor" "network"}
                                              :toggle-value {"rules" [entry]}}
                                             ["rules" 0 "access" "none" :done :done]
                                             []
                                             {:menus menus})))
        (expect (= ["read-only" "read-write" "none"]
                   (mapv :value (get @menus "Host rules · access")))))))

(defn- text-row
  "The line and the columns of `texts`, when all of them are on one line of `frame`."
  [frame & texts]
  (first (keep-indexed (fn [y ^String line]
                         (when (every? #(str/includes? line %) texts)
                           (into [y] (map #(.indexOf line ^String %)) texts)))
                       (str/split-lines frame))))

(defn- rgb [^TextColor color] [(.getRed color) (.getGreen color) (.getBlue color)])

(defn- extension-action-calls
  "Use the Extensions header button in `rows` with `keys`, and return the catalog reads and
   reloads."
  ([keys] (extension-action-calls (#'dlg/extension-action-rows) keys))
  ([rows keys]
   (let [calls (atom [])]
     (with-redefs [vis/gateway-reload-extensions! (fn [scope]
                                                    (swap! calls conj [:reload scope])
                                                    {"loaded" 1 "failed" 0})]
       (with-redefs-fn {#'dlg/load-settings-inventory! (fn []
                                                         (swap! calls conj [:catalog])
                                                         {:status :ok})
                        #'dlg/mini-note! (constantly nil)}
         #(capture-settings rows keys)))
     @calls)))

(defn- extension-test-actions
  "`n` plain action rows whose ids start with `prefix`."
  [prefix n]
  (mapv #(hash-map :type :action :id (keyword (str prefix %)) :label (str prefix %)) (range n)))

(defdescribe
  extension-catalog-test
  ;; #302: a failed extension must remain visible even without registered settings.
  (it
    "shows failed and stale extensions with their scope and error"
    (let [groups
          [{"title" "broken.py"
            "extension" {"name" "broken.py"
                         "origin" "project"
                         "path" ".vis/extensions/broken.py"
                         "status" "failed"
                         "error" "Invalid Python syntax"}
            "toggles" []}
           {"title" "notifier"
            "extension" {"name" "notifier"
                         "origin" "global"
                         "path" "~/.vis/extensions/notifier.py"
                         "status" "stale"
                         "error" "Missing dependency"}
            "toggles"
            [{"id" "notifier_enabled" "label" "Desktop alerts" "type" "boolean" "enabled" false}]}]

          rows
          (#'dlg/catalog-toggle-rows groups)

          frame
          (cap/frame-text (capture-settings rows [:esc]))

          line
          (fn [text]
            (first (filter #(str/includes? % text) (str/split-lines frame))))]

      (expect (= [[:extension "broken.py" "project" nil] [:info "Extension failed to load" nil nil]
                  [:extension "notifier" "global" nil]
                  [:info "Reload failed — using last loaded version" nil nil]
                  [:registry-toggle "Desktop alerts" nil 1]]
                 (mapv (juxt :type :label :tag :depth) rows)))
      (expect (str/includes? (str (line "broken.py")) "project"))
      (expect (str/includes? (str (line "notifier")) "global"))
      (expect (str/includes? frame "Invalid Python syntax"))
      (expect (str/includes? frame "Missing dependency"))
      (expect (not (str/includes? frame "Project extension")))
      (expect (not (str/includes? frame "Machine extension")))
      (expect (not (str/includes? frame ".vis/extensions")))
      (expect (empty? (#'dlg/catalog-toggle-rows
                       [{"title" "builtin"
                         "extension" {"name" "builtin" "origin" "built_in" "status" "loaded"}
                         "toggles" []}])))))
  (it
    "puts each extension on one row with its scope and its choice"
    (let [engine
          (fn [id label]
            {"id" id
             "label" label
             "type" "enum"
             "value" "auto"
             "choices" ["auto" "on" "off"]
             "description" "Auto detects applicability; On stays active; Off denies tools."})

          groups
          [{"title" "Agents"
            "toggles" [{"id" "subagents" "label" "Subagents" "type" "boolean" "enabled" true}]}
           {"title" "foundation-mcp"
            "extension" {"name" "foundation-mcp" "origin" "built_in" "status" "loaded"}
            "toggles" [(engine "engines_1" "foundation-mcp")]}
           {"title" "vis-spel"
            "extension" {"name" "vis-spel" "origin" "global" "status" "loaded"}
            "toggles"
            [(engine "engines_2" "vis-spel")
             {"id" "skills_1" "label" "vis-spel/browser" "type" "boolean" "enabled" true}]}]

          rows
          (binding [dlg/*settings-target*
                    {:scope :project :target-id "example-project"}

                    dlg/*local-settings-inventory*
                    (atom {:status :ok :groups groups :error nil})

                    dlg/*local-mcp-inventory*
                    (atom {:status :unloaded :servers [] :error nil})]

            (#'dlg/settings-rows))

          lines
          (str/split-lines (cap/frame-text (capture-settings rows [:esc])))

          line
          (fn [text]
            (first (filter #(str/includes? % text) lines)))]

      (expect (= [[:section "Agents" nil nil] [:registry-toggle "Subagents" nil nil]
                  [:section "Extensions" nil nil] [:registry-toggle "foundation-mcp" nil nil]
                  [:registry-toggle "vis-spel" "global" nil] [:subsection "Skills" nil 1]
                  [:registry-toggle "browser" nil 1]]
                 (mapv (juxt :type :label :tag :depth) rows)))
      ;; The one action stands inside the Extensions header rule, not on a row of its own.
      (expect (= "Reload" (:label (:button (nth rows 2)))))
      ;; The chevron after the name folds the section.
      (expect (re-find #"── Extensions ▾ ─+  Reload  ──" (str (line "── Extensions"))))
      ;; The Reload button in the Extensions header is not counted as a setting.
      (expect (= [["Agents" 1] ["Extensions" 3]]
                 (mapv (juxt :label :count) (#'dlg/settings-toc rows 0))))
      ;; The choice row is the extension's own row, so its name shows once.
      (expect (= 1 (count (filter #(str/includes? % "foundation-mcp") lines))))
      (expect (= 1 (count (filter #(str/includes? % "vis-spel") lines))))
      (expect (re-find #"vis-spel\s+global\s+Auto" (str (line "vis-spel "))))
      ;; A packaged skill stands one level in from its extension, without its name again.
      (expect (= (+ 2 (long (str/index-of (line "vis-spel ") "vis-spel")))
                 (long (str/index-of (line " browser") "browser"))))
      ;; The skills of an extension stand under their own label, in line with their marks.
      (let [head (count (take-while #(not (str/includes? % "vis-spel ")) lines))]
        (expect (str/includes? (nth lines (inc head)) " Skills "))
        (expect (str/includes? (nth lines (+ 2 head)) " browser ")))
      (expect (= (long (str/index-of (line " browser") "●"))
                 (long (str/index-of (line " Skills ") "Skills"))))
      ;; A search for the whole skill name still finds the shorter row.
      (expect (= ["Extensions" "vis-spel" "Skills" "browser"]
                 (mapv :label (#'dlg/filter-settings-rows rows "vis-spel/browser"))))
      ;; A narrow pane drops the scope before it cuts a name.
      (let [narrow (str/split-lines (cap/frame-text (capture-settings rows [:esc] :cols 48)))]
        (expect (some #(re-find #"◆ vis-spel\s" %) narrow))
        (expect (not-any? #(str/includes? % "global") narrow)))))
  (it
    "keeps the extension name row with the settings that a search finds"
    (let [rows
          (#'dlg/catalog-toggle-rows
           [{"title" "vis-spel"
             "extension" {"name" "vis-spel" "origin" "global" "status" "loaded"}
             "toggles"
             [{"id" "skills_1" "label" "vis-spel/browser" "type" "boolean" "enabled" true}]}
            {"title" "notifier"
             "extension" {"name" "notifier" "origin" "global" "status" "loaded"}
             "toggles"
             [{"id" "engines_1"
               "label" "notifier"
               "type" "enum"
               "value" "auto"
               "choices" ["auto" "on" "off"]}
              {"id" "notifier_enabled" "label" "Desktop alerts" "type" "boolean" "enabled" false}]}
            {"title" "broken.py"
             "extension" {"name" "broken.py"
                          "origin" "project"
                          "status" "failed"
                          "error" "Invalid Python syntax"}
             "toggles" []}])

          labels
          #(mapv :label (#'dlg/filter-settings-rows rows %))]

      ;; The Skills label of an earlier extension stays out of a later extension's match.
      (expect (= ["notifier" "Desktop alerts"] (labels "desktop")))
      (expect (= ["notifier" "Desktop alerts"] (labels "notifier")))
      (expect (= ["broken.py" "Extension failed to load"] (labels "broken")))
      ;; A skill keeps its extension and its Skills label.
      (expect (= ["vis-spel" "Skills" "browser"] (labels "browser")))
      (expect (= ["vis-spel" "Skills" "browser"] (labels "vis-spel")))))
  (it "reloads extension code only from the Extensions header button, then reads the list"
      (let [target
            {:scope :project :target-id "example-project"}

            calls
            (atom [])

            notes
            (atom [])]

        (expect (= [] (extension-action-calls [:esc])))
        (with-redefs [vis/gateway-reload-extensions! (fn [scope]
                                                       (swap! calls conj [:reload scope])
                                                       {"loaded" 2 "failed" 1})]
          (with-redefs-fn {#'dlg/load-settings-inventory! (fn []
                                                            (swap! calls conj [:catalog])
                                                            {:status :ok})
                           #'dlg/mini-note! (fn [_ _ _ title text]
                                              (swap! notes conj [title text]))}
            #(capture-settings (#'dlg/extension-action-rows)
                               [:enter :esc]
                               :callbacks
                               {:settings-target target})))
        (expect (= [[:reload target] [:catalog]] @calls))
        (expect (= [["Extensions reloaded"
                     "2 loaded, 1 failed. Each failed extension shows its error."]]
                   @notes))))
  (it "fills the Reload button with the accent only while it is selected"
      (let [capture
            (capture-settings (conj (#'dlg/extension-action-rows)
                                    {:type :action :id :other :label "Other"})
                              [:down :esc])

            [y rule-x reload-x]
            (text-row (cap/frame-text capture 0) "── Extensions" "Reload")

            bg
            (fn [frame x]
              (:bg (get-in frame [y x])))

            accent
            (rgb t/header-active-tab-bg)]

        (expect (< (long rule-x) (long reload-x)))
        (expect (= accent (bg (first (:frames capture)) reload-x)))
        (expect (not= accent (bg (peek (:frames capture)) reload-x)))))
  (it "runs Reload when you click the button, not the rest of the header"
      (let [[y rule-x reload-x]
            (text-row (cap/frame-text (capture-settings (#'dlg/extension-action-rows) [:esc]))
                      "── Extensions"
                      "Reload")

            click
            (fn [x]
              [(MouseAction. MouseActionType/CLICK_DOWN 0 (TerminalPosition. x y))
               (MouseAction. MouseActionType/CLICK_RELEASE 0 (TerminalPosition. x y)) :esc])]

        (expect (= [[:reload nil] [:catalog]] (extension-action-calls (click reload-x))))
        (expect (= [] (extension-action-calls (click rule-x))))))
  (it "selects the Reload button that a search finds"
      (let [rows (into (extension-test-actions "a" 2) (#'dlg/extension-action-rows))]
        (expect (= [] (extension-action-calls rows [:enter :esc])))
        (expect (= [[:reload nil] [:catalog]]
                   (extension-action-calls rows (concat "reload" [:enter :esc :esc]))))))
  (it "pages down from the Reload button instead of a jump back to the top"
      (let [rows
            (vec (concat [{:type :section :label "General"}]
                         (extension-test-actions "a" 10)
                         (#'dlg/extension-action-rows)
                         (extension-test-actions "c" 3)))

            entries
            (vec (map-indexed (fn [i {:keys [type]}]
                                {:row-idx i :part (if (= :section type) :section :option)})
                              rows))]

        (expect (< 11 (long (#'dlg/settings-page-selection rows entries 11 3 1))))))
  (it "keeps the whole Reload button in a narrow window and cuts the section name first"
      (let [frame
            (cap/frame-text (capture-settings (#'dlg/extension-action-rows) [:esc] :cols 48))

            [y]
            (text-row frame "── Ext…" " Reload  ──")]

        (expect (some? y))))
  (it "places the button before two rule characters and cuts the section label first"
      (let [place #(#'dlg/settings-section-layout (first (#'dlg/extension-action-rows)) 0 %)]
        ;; `── Extensions ─ ` + ` Reload ` + ` ──` takes 27 columns.
        (expect (= {:label "Extensions" :button {:x 16 :end 24 :label "Reload"}} (place 27)))
        (expect (= {:label "Ext…" :button {:x 10 :end 18 :label "Reload"}} (place 21)))
        (expect (= {:label "…" :button {:x 7 :end 14 :label "Relo…"}} (place 17)))
        (expect (= {:label "Extensions"} (place 12)))
        (expect (= {:label "Planning"}
                   (#'dlg/settings-section-layout {:type :section :label "Planning"} 0 80)))))
  (it "asks for a Vis update when the gateway has no reload route"
      (let [notes (atom [])]
        (with-redefs [vis/gateway-reload-extensions!
                      (fn [_]
                        (throw (ex-info "no such route"
                                        {:http-status 404 "error" {"type" "not-found"}})))]
          (with-redefs-fn {#'dlg/load-settings-inventory! (constantly {:status :ok})
                           #'dlg/mini-note! (fn [_ _ _ title text]
                                              (swap! notes conj [title text]))}
            #(capture-settings (#'dlg/extension-action-rows) [:enter :esc])))
        (expect (= [["Extensions not reloaded"
                     "This gateway does not support extension reload. Update Vis on that machine."]]
                   @notes)))))
