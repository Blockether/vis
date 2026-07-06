(ns com.blockether.vis.internal.commandline-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- sample-root
  []
  {:cmd/name "vis"
   :cmd/doc "root"
   :cmd/subcommands [{:cmd/name "providers"
                      :cmd/doc "providers"
                      :cmd/subcommands [{:cmd/name "list"
                                         :cmd/doc "list providers"
                                         :cmd/run-fn (fn [_ _]
                                                       :listed)}]}]})

(defdescribe dispatch-help-test
             (it "reports unknown nested subcommands before rendering parent help"
                 (let [printed
                       (atom nil)

                       result
                       (commandline/dispatch! (sample-root)
                                              ["vis" "providers" "missing" "--help"]
                                              {:print-fn #(reset! printed %)})]

                   (expect (= :error (:status result)))
                   (expect (str/includes? @printed "Unknown command: vis providers missing"))
                   (expect (str/includes? @printed "USAGE"))))
             (it "keeps known leaf help working"
                 (let [printed
                       (atom nil)

                       result
                       (commandline/dispatch! (sample-root)
                                              ["vis" "providers" "list" "--help"]
                                              {:print-fn #(reset! printed %)})]

                   (expect (= :help (:status result)))
                   (expect (str/includes? @printed "vis providers list")))))

;; =============================================================================
;; Help rendering -- split COMMANDS / EXTENSION COMMANDS + extra sections.
;; =============================================================================

(defn- mixed-children-cmd
  []
  {:cmd/name "ext"
   :cmd/doc "Inspect, scaffold, or run an extension-contributed CLI command."
   :cmd/usage "vis ext <subcmd>"
   :cmd/subcommands
   [{:cmd/name "list" :cmd/doc "List every registered extension." :cmd/internal? true}
    {:cmd/name "scaffold" :cmd/doc "Create a user extension scaffold." :cmd/internal? true}
    {:cmd/name "voice" :cmd/doc "Voice extension commands."}
    {:cmd/name "audit" :cmd/doc "Audit extension commands."}]
   :cmd/extra-sections [{:title "INSTALLED EXTENSIONS"
                         :body
                         "  foundation   Foundation v/ ...\n  bridge       Bridge verification."}]})

(defdescribe
  render-command-grouping-test
  (it "splits internal vs contributed children into named sections, no `---- extensions ----`"
      (binding [commandline/*color-enabled?* false]
        (let [out (commandline/render-command (mixed-children-cmd) ["vis" "ext"])]
          (expect (str/includes? out "COMMANDS"))
          (expect (str/includes? out "EXTENSION COMMANDS"))
          (expect (str/includes? out "list"))
          (expect (str/includes? out "scaffold"))
          (expect (str/includes? out "voice"))
          (expect (str/includes? out "audit"))
          (expect (not (str/includes? out "---- extensions ----")))
          ;; Internal block must appear before the extension block.
          (let [a (.indexOf ^String out "list")
                b (.indexOf ^String out "voice")]

            (expect (and (pos? a) (pos? b) (< a b)))))))
  (it "renders :cmd/extra-sections after the subcommand block"
      (binding [commandline/*color-enabled?* false]
        (let [out (commandline/render-command (mixed-children-cmd) ["vis" "ext"])]
          (expect (str/includes? out "INSTALLED EXTENSIONS"))
          (expect (str/includes? out "Bridge verification."))
          (let [a (.indexOf ^String out "EXTENSION COMMANDS")
                b (.indexOf ^String out "INSTALLED EXTENSIONS")]

            (expect (and (pos? a) (pos? b) (< a b)))))))
  (it "falls back to a single SUBCOMMANDS section when children are not mixed"
      (binding [commandline/*color-enabled?* false]
        (let [out (commandline/render-command {:cmd/name "only-internal"
                                               :cmd/doc "x"
                                               :cmd/subcommands
                                               [{:cmd/name "a" :cmd/doc "a" :cmd/internal? true}
                                                {:cmd/name "b" :cmd/doc "b" :cmd/internal? true}]}
                                              ["vis" "only-internal"])]
          (expect (str/includes? out "SUBCOMMANDS"))
          (expect (not (str/includes? out "EXTENSION COMMANDS"))))))
  (it "supports :cmd/extra-sections as a 0-arg fn (deferred discovery)"
      (binding [commandline/*color-enabled?* false]
        (let [calls (atom 0)
              cmd {:cmd/name "x"
                   :cmd/doc "x"
                   :cmd/extra-sections (fn []
                                         (swap! calls inc)
                                         [{:title "DYNAMIC" :body "  body line"}])}
              out (commandline/render-command cmd ["vis" "x"])]

          (expect (= 1 @calls))
          (expect (str/includes? out "DYNAMIC"))
          (expect (str/includes? out "body line"))))))
