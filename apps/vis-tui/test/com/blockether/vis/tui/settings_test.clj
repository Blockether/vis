(ns com.blockether.vis.tui.settings-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna TerminalPosition TextColor$RGB]
           [com.googlecode.lanterna.input MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]))

(def ^:private engine-names ["vis-lang-python" "einmal" "language-clojure" "vis-lang-clojure"])

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
  "Capture the production settings loop with a deterministic catalog and no gateway."
  [rows keys &
   {:keys [cols callbacks values load!]
    :or {cols 100 callbacks {} values {} load! (constantly nil)}}]
  (let [result (cap/capture! {:cols cols
                              :rows 34
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
          (expect (not (str/includes? frame "Load this extension")))))
    (it "starts each row at its name and puts one blank line above each header"
        ;; The rows had a mark before each name, and a header stood right under a setting.
        (let [rows
              [{:type :section :label "General"} {:type :toggle :key :agent :label "Agent"}
               {:type :subsection :label "View"} {:type :toggle :key :theme :label "Theme"}
               {:type :section :label "Providers"} {:type :subsection :label "Configured"}
               {:type :toggle :key :account :label "Account"}]

              capture
              (capture-settings rows [:esc] :values {:agent true})

              grid
              (last (:frames capture))

              lines
              (str/split-lines (cap/frame-text capture))

              at
              (fn [text]
                (first (keep-indexed (fn [y line]
                                       (when-let [x (str/index-of line (str " " text " "))]
                                         [y (inc (long x))]))
                                     lines)))

              [view-y view-x]
              (at "View")]

          (expect (= [:section :option :gap :subsection :option :gap :section :subsection :option]
                     (mapv :part (#'dlg/settings-render-entries rows 40))))
          (expect (= #{view-x} (set (map (comp second at) ["Agent" "View" "Theme" "Account"]))))
          (expect (not-any? #(str/includes? % "•") lines))
          (expect (str/blank? (subs (nth lines (dec (long view-y))) view-x (+ (long view-x) 20))))
          (expect (str/includes? (nth lines (dec (long (first (at "Theme"))))) " View "))
          (expect (:bold (get-in grid [view-y view-x])))))
    (it "keeps a long label whole when its own value is short"
        ;; Live check: one long URL value cut every label in the list to 16 columns.
        (let [frame (cap/frame-text
                      (capture-settings
                        (#'dlg/catalog-toggle-rows
                         [{"title" "Tools"
                           "toggles"
                           [{"id" "kb_url"
                             "type" "string"
                             "editor" "text"
                             "label" "Knowledge base server"
                             "value"
                             "http://127.0.0.1:7999/knowledge-base/team/projects/archive/2026"
                             "source" "global"}
                            {"id" "kb_token"
                             "type" "string"
                             "editor" "secret"
                             "label" "Knowledge base token"
                             "is_set" true
                             "source" "global"}]}])
                        [:esc]))]
          (expect (str/includes? frame "Knowledge base token"))
          (expect (str/includes? frame "Saved"))
          (expect (str/includes? frame "http://127.0.0.1:7999/knowledge-base"))))))

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
  (it "folds a section with Left and opens it with Right or Enter"
      (let [run
            #(capture-settings settings-rows
                               %
                               :values {:read-files false :thinking false}
                               :callbacks {:focus-section "Paths and access"})

            focused
            (cap/frame-text (run [:esc]))

            folded
            (cap/frame-text (run [:left :esc]))]

        (expect (str/includes? focused "Paths and access ▾"))
        (expect (str/includes? focused "Read filesystem"))
        (expect (str/includes? focused "Response ▾"))
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
  (it
    "folds and opens a section with a press on its header"
    (let [[y x]
          (first (keep-indexed (fn [y ^String line]
                                 (let [x (.indexOf line "── Response")]
                                   (when (<= 0 x) [y x])))
                               (str/split-lines (cap/frame-text (capture-settings settings-rows
                                                                                  [:esc])))))

          press
          (fn [x y]
            [(MouseAction. MouseActionType/CLICK_DOWN 0 (TerminalPosition. x y))
             (MouseAction. MouseActionType/CLICK_RELEASE 0 (TerminalPosition. x y))])

          folded
          (cap/frame-text (capture-settings settings-rows (conj (press (+ x 4) y) :esc)))

          opened
          (cap/frame-text (capture-settings settings-rows
                                            (concat (press (+ x 4) y) (press (+ x 4) y) [:esc])))]

      (expect (str/includes? folded "Response ▸"))
      (expect (not (str/includes? folded "Show reasoning")))
      (expect (str/includes? opened "Response ▾"))
      (expect (str/includes? opened "Show reasoning"))))
  (it "shows every match while a search runs"
      (let [capture
            (capture-settings settings-rows (concat "show" [:esc :esc]))

            ;; The frame before the first Esc shows the search result.
            searched
            (cap/frame-text capture (- (count (:frames capture)) 2))]

        (expect (str/includes? searched "Show reasoning"))
        (expect (not (str/includes? searched "▾")))
        ;; Clearing the search shows the fold chevrons again.
        (expect (str/includes? (cap/frame-text capture) "Response ▾"))))
  (it "keeps every value on the selected row legible on light and dark themes"
      ;; #339: on a light theme the value column kept its dark ink on the dark bar.
      (let [before @t/active-theme-id]
        (try (doseq [theme-id ["blockether-light" "vis-light" "vis-dark"]]
               (t/apply-theme! theme-id)
               (let [row (some (fn [row]
                                 (when (str/includes? (apply str (map #(str (:ch %)) row))
                                                      "vis-lang-python")
                                   row))
                               (last (:frames (capture-settings settings-rows [:esc]))))
                     rgb (fn [[r g b]]
                           (TextColor$RGB. (int r) (int g) (int b)))]

                 (expect (some #(= "a" (str (:ch %))) row) "the row shows its value")
                 (doseq [:let [bar (:bg (first (filter #(= "v" (str (:ch %))) row)))]
                         cell row
                         :when (and (= bar (:bg cell)) (not (str/blank? (str (:ch cell)))))]

                   (expect (<= t/legible-contrast
                               (t/contrast-ratio (rgb (:fg cell)) (rgb (:bg cell))))
                           (str theme-id " cell " (:ch cell))))))
             (finally (t/apply-theme! before)))))
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
  (it "opens full descriptions and source with F1 and returns without saving"
      (doseq [cols [40 100]]
        (let [capture (capture-settings settings-rows [:f1 :esc :esc] :cols cols)
              frames (mapv cap/frame-text (:frames capture))]

          (expect (= {} (:ret capture)))
          (expect (not (str/includes? (first frames) "Source:")))
          (expect (some #(str/includes? % "Source: default") frames))
          (expect (some #(str/includes? % "extension when") frames))
          (expect (not (str/includes? (last frames) "Source:"))))))
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

(defn- capture-secret
  "Run `keys` on one secret row; answer the gateway calls, the notes and the frame texts."
  [setting keys & {:keys [fail]}]
  (let [calls
        (atom [])

        notes
        (atom [])

        capture
        (with-redefs [vis/set-setting-value!
                      (fn [id value target]
                        (swap! calls conj [:value id value target])
                        (when fail (throw (ex-info (fail value) {})))
                        {"id" id "editor" "secret" "is_set" true})

                      vis/inherit-setting!
                      (fn [id target]
                        (swap! calls conj [:inherit id target])
                        {"id" id "editor" "secret" "is_set" false})]

          (with-redefs-fn {#'dlg/load-settings-inventory! (constantly nil)
                           #'dlg/mini-note! (fn [_ _ _ title text]
                                              (swap! notes conj [title text]))}
            #(capture-settings (typed-rows (merge {"id" "compact_secret"
                                                   "type" "string"
                                                   "editor" "secret"
                                                   "label" "Access token"
                                                   "max_length" 64}
                                                  setting))
                               keys)))]

    {:calls @calls :notes @notes :frames (mapv cap/frame-text (:frames capture))}))

(defdescribe
  secret-settings-test
  (describe
    "rows"
    (it
      "shows only whether a value is saved, never a value the row carries"
      (let [[unset]
            (filterv #(= :secret-setting (:type %))
              (typed-rows
                {"id" "s" "type" "string" "editor" "secret" "is_set" false "label" "Access token"}))

            [saved]
            (filterv #(= :secret-setting (:type %))
              (typed-rows {"id" "s"
                           "type" "string"
                           "editor" "secret"
                           "is_set" true
                           "label" "Access token"
                           "value" "hunter2"
                           "own_value" "hunter2"
                           "inherited_value" "hunter2"}))]

        (expect (= "Not set" (#'dlg/settings-option-value unset {})))
        (expect (= "Saved" (#'dlg/settings-option-value saved {})))
        (expect (not (str/includes? (pr-str saved) "hunter2")))
        (expect (not-any? #(str/includes? % "hunter2") (#'dlg/settings-details-lines saved {}))))))
  (describe "edits"
            (it "sets a value through a masked input that never echoes the text"
                (let [{:keys [calls frames]} (capture-secret {"is_set" false}
                                                             [:enter \s \3 \c :enter :esc])]
                  (expect (= [[:value "compact_secret" "s3c" nil]] calls))
                  (expect (some #(str/includes? % "***") frames))
                  (expect (not-any? #(str/includes? % "s3c") frames))))
            (it "refuses blank text and keeps typed text out of a cancelled edit"
                (expect (empty? (:calls (capture-secret {"is_set" false}
                                                        [:enter \space :enter \x :esc :esc])))))
            (it "stops the input at the row's maximum length"
                (expect (= [[:value "compact_secret" "abc" nil]]
                           (:calls (capture-secret {"is_set" false "max_length" 3}
                                                   [:enter \a \b \c \d :enter :esc])))))
            (it "removes a value that this scope holds only after confirmation"
                (expect (empty? (:calls (capture-secret {"is_set" true "source" "global"}
                                                        [:enter \x \n :esc]))))
                (expect (= [[:inherit "compact_secret" nil]]
                           (:calls (capture-secret {"is_set" true "source" "global"}
                                                   [:enter \x \y :esc])))))
            (it "offers no Remove for a value that another scope holds"
                (let [{:keys [calls frames]} (capture-secret {"is_set" true "source" "project"}
                                                             [:enter \x \y :esc :esc])]
                  (expect (empty? calls))
                  (expect (not-any? #(str/includes? % "Remove") frames))))
            (it "shows the gateway error without the typed value"
                (let [{:keys [notes]} (capture-secret {"is_set" false}
                                                      [:enter \s \3 \c \r \3 \t :enter :esc]
                                                      :fail
                                                      #(str "Value " % " is not valid."))]
                  (expect (= [["Setting not saved" "Value *** is not valid."]] notes))))))

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
  (it "edits a tools list as one entry per line (issue #416)"
      (expect (= ["vis.shell.*"]
                 (scripted-structured-edit {:label "Denied tools"
                                            :setting {"editor" "tools"}
                                            :toggle-value ["vis.files.read"]}
                                           []
                                           []
                                           {:lists [["vis.shell.*"]]}))))
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
           {"title" "language-clojure"
            "extension" {"name" "language-clojure" "origin" "built_in" "status" "loaded"}
            "toggles" [(engine "engines_1" "language-clojure")]}
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
                  [:section "Tools" nil nil] [:subsection "Extensions" nil nil]
                  [:registry-toggle "language-clojure" nil nil]
                  [:registry-toggle "vis-spel" "global" nil] [:subsection "Skills" nil 1]
                  [:registry-toggle "browser" nil 1]]
                 (mapv (juxt :type :label :tag :depth) rows)))
      ;; The one action stands inside the Tools header rule, not on a row of its own.
      (expect (= "Reload" (:label (:button (nth rows 2)))))
      ;; The chevron after the name folds the section.
      (expect (re-find #"── Tools ▾ ─+  Reload  ──" (str (line "── Tools"))))
      ;; The Reload button in the Tools header is not counted as a setting.
      (expect (= [["Agents" 1] ["Tools" 3]]
                 (mapv (juxt :label :count) (#'dlg/settings-toc rows 0))))
      ;; The choice row is the extension's own row, so its name shows once.
      (expect (= 1 (count (filter #(str/includes? % "language-clojure") lines))))
      (expect (= 1 (count (filter #(str/includes? % "vis-spel") lines))))
      (expect (re-find #"vis-spel\s+global\s+Auto" (str (line "vis-spel "))))
      ;; A packaged skill stands one level in from its extension, without its name again.
      (expect (= (+ 2 (long (str/index-of (line "vis-spel ") "vis-spel")))
                 (long (str/index-of (line " browser") "browser"))))
      ;; The skills of an extension stand under their own label, in line with their names.
      (let [head (count (take-while #(not (str/includes? % "vis-spel ")) lines))]
        (expect (str/includes? (nth lines (inc head)) " Skills "))
        (expect (str/includes? (nth lines (+ 2 head)) " browser ")))
      (expect (= (long (str/index-of (line " browser") "browser"))
                 (long (str/index-of (line " Skills ") "Skills"))))
      ;; A search for the whole skill name still finds the shorter row.
      (expect (= ["Tools" "vis-spel" "Skills" "browser"]
                 (mapv :label (#'dlg/filter-settings-rows rows "vis-spel/browser"))))
      ;; A narrow pane drops the scope before it cuts a name.
      (let [narrow (str/split-lines (cap/frame-text (capture-settings rows [:esc] :cols 48)))]
        (expect (some #(re-find #"\svis-spel\s" %) narrow))
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
                     "2 loaded, 1 failed.\nEach failed extension shows its error."]]
                   @notes))))
  ;; Regression, issue #348: a machine reload with no machine extensions said
  ;; "Extensions reloaded — 0 loaded, 0 failed." and named no scope or directory.
  ;; Issue #373: a machine reload also names the project extensions of the session.
  (it
    "names each reloaded scope, its directory and what it found"
    (let [note
          #'dlg/extension-reload-note

          machine
          (fn [loaded failed extensions]
            {"scope" "global"
             "dirs" ["/home/user/.vis/extensions"]
             "loaded" loaded
             "failed" failed
             "extensions" extensions})]

      (expect (= ["No machine extensions found"
                  (str "No machine extensions in /home/user/.vis/extensions.\n"
                       "Project extensions reload in Project settings.")]
                 (note {"loaded" 0 "failed" 0 "scopes" [(machine 0 0 [])]} true nil nil)))
      (expect (= ["No machine extensions found"
                  (str "No machine extensions in /home/user/.vis/extensions.\n"
                       "This project has 2 extensions (clj, py).\n"
                       "Project extensions reload in Project settings.")]
                 (note {"loaded" 0 "failed" 0 "scopes" [(machine 0 0 [])]}
                       true
                       nil
                       {"target_id" "p1" "count" 2 "names" ["clj" "py"]})))
      (expect
        (= ["Extensions reloaded"
            (str "Machine extensions (/home/user/.vis/extensions): 1 loaded (memo), 0 failed.\n"
                 "Project extensions (/work/app/.vis/extensions): 2 loaded (clj, py), 1 failed.\n"
                 "Each failed extension shows its error.")]
           (note {"loaded" 3
                  "failed" 1
                  "scopes" [(machine 1 0 ["memo"])
                            {"scope" "project"
                             "dirs" ["/work/app/.vis/extensions"]
                             "loaded" 2
                             "failed" 1
                             "extensions" ["clj" "py"]}]}
                 false
                 nil
                 nil)))
      (expect
        (= ["No extensions found"
            "No machine extensions in /home/user/.vis/extensions.\nNo project extensions in /p."]
           (note {"loaded" 0
                  "failed" 0
                  "scopes"
                  [(machine 0 0 [])
                   {"scope" "project" "dirs" ["/p"] "loaded" 0 "failed" 0 "extensions" []}]}
                 false
                 nil
                 nil)))
      (expect (str/ends-with? (second (note {"loaded" 9
                                             "failed" 0
                                             "scopes" [(machine 9 0 (mapv #(str "e" %) (range 9)))]}
                                            true
                                            "offline"
                                            nil))
                              (str "e7 and 1 more), 0 failed.\n"
                                   "Project extensions reload in Project settings.\n"
                                   "Settings unavailable: offline")))))
  (it "runs Reload when you click the button, not the rest of the header"
      (let [[y rule-x reload-x]
            (text-row (cap/frame-text (capture-settings (#'dlg/extension-action-rows) [:esc]))
                      "── Tools"
                      "Reload")

            click
            (fn [x]
              [(MouseAction. MouseActionType/CLICK_DOWN 0 (TerminalPosition. x y))
               (MouseAction. MouseActionType/CLICK_RELEASE 0 (TerminalPosition. x y)) :esc])]

        (expect (= [[:reload nil] [:catalog]] (extension-action-calls (click reload-x))))
        (expect (= [] (extension-action-calls (click rule-x))))))
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
  (it "places the button before two rule characters and cuts the section label first"
      (let [place #(#'dlg/settings-section-layout (first (#'dlg/extension-action-rows)) 0 %)]
        ;; `── Tools ─ ` + ` Reload ` + ` ──` takes 22 columns.
        (expect (= {:label "Tools" :button {:x 11 :end 19 :label "Reload"}} (place 22)))
        (expect (= {:label "To…" :button {:x 9 :end 17 :label "Reload"}} (place 20)))
        (expect (= {:label "…" :button {:x 7 :end 14 :label "Relo…"}} (place 17)))
        (expect (= {:label "Tools"} (place 12)))
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

(defdescribe
  nested-settings-test
  (it "shows each nested setting one level in from its parent, at any depth"
      (let [rows (#'dlg/catalog-toggle-rows
                  [{"id" "general"
                    "title" "General"
                    "toggles" [{"id" "council"
                                "label" "Council"
                                "type" "boolean"
                                "enabled" true
                                "children" [{"id" "council_scope"
                                             "label" "Council scope"
                                             "type" "enum"
                                             "value" "local"
                                             "choices" ["local"]
                                             "children" [{"id" "council_scope_a"
                                                          "label" "Allow A"
                                                          "type" "boolean"
                                                          "enabled" true}]}]}
                               {"id" "notifier_enabled"
                                "label" "Desktop alerts"
                                "type" "boolean"
                                "enabled" false}]}])]
        (expect (= [[nil nil] ["council" nil] ["council_scope" 1] ["council_scope_a" 2]
                    ["notifier_enabled" nil]]
                   (mapv (juxt :toggle-id :depth) rows)))))
  (it "updates a nested row and keeps the rows under it"
      (let [rows [{"id" "council"
                   "enabled" true
                   "children" [{"id" "council_scope"
                                "value" "local"
                                "children" [{"id" "council_scope_a" "enabled" true}]}]}]]
        (expect (= [{"id" "council"
                     "enabled" true
                     "children" [{"id" "council_scope"
                                  "value" "scope-a"
                                  "children" [{"id" "council_scope_a" "enabled" true}]}]}]
                   (#'dlg/merge-setting-row rows {"id" "council_scope" "value" "scope-a"})))
        (expect (= false
                   (get-in (#'dlg/merge-setting-row rows {"id" "council" "enabled" false})
                           [0 "enabled"])))
        (expect (= 1
                   (count (get-in (#'dlg/merge-setting-row rows {"id" "council" "enabled" false})
                                  [0 "children"])))))))
