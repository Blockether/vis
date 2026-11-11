(ns com.blockether.vis.internal.commandline-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- sample-root
  []
  {:cmd/name "vis-agent"
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
                                              ["vis-agent" "providers" "missing" "--help"]
                                              {:print-fn #(reset! printed %)})]

                   (expect (= :error (:status result)))
                   (expect (str/includes? @printed "USAGE"))))
             (it "keeps known leaf help working"
                 (let [printed
                       (atom nil)

                       result
                       (commandline/dispatch! (sample-root)
                                              ["vis-agent" "providers" "list" "--help"]
                                              {:print-fn #(reset! printed %)})]

                   (expect (= :help (:status result)))
                   (expect (str/includes? @printed "vis-agent providers list")))))

(defdescribe
  dispatch-refusal-test
  (it "refuses a word under a parent that has no run-fn"
      (doseq [args [["vis-agent" "providers" "missing"] ["vis-agent" "channels" "nope"]]]
        (let [root (update (sample-root)
                           :cmd/subcommands
                           conj
                           {:cmd/name "channels" :cmd/doc "channels" :cmd/subcommands []})
              result (commandline/dispatch! root args {:print-fn nil})]

          (expect (= :error (:status result)) (pr-str args))
          (expect (str/starts-with? (:error result) "Unknown command:")))))
  (it "refuses a flag that a command without flags does not declare"
      (let [result (commandline/dispatch! (sample-root)
                                          ["vis-agent" "providers" "list" "--json"]
                                          {:print-fn nil})]
        (expect (= :error (:status result)))
        (expect (str/starts-with? (:error result) "Unknown flag: --json"))))
  (it "passes every token to a raw-args command"
      (let [root
            {:cmd/name "vis-agent"
             :cmd/doc "root"
             :cmd/subcommands [{:cmd/name "python"
                                :cmd/doc "python"
                                :cmd/raw-args? true
                                :cmd/run-fn (fn [_ residual]
                                              residual)}]}

            result
            (commandline/dispatch! root ["vis-agent" "python" "-c" "x" "--any"] {:print-fn nil})]

        (expect (= :ok (:status result)))
        (expect (= ["-c" "x" "--any"] (vec (:result result)))))))

;; Help rendering -- split COMMANDS / EXTENSION COMMANDS + extra sections.

(defn- mixed-children-cmd
  []
  {:cmd/name "extension"
   :cmd/doc "Inspect or run an extension-contributed CLI command."
   :cmd/usage "vis-agent extension <subcmd>"
   :cmd/subcommands
   [{:cmd/name "list" :cmd/doc "List every registered extension." :cmd/internal? true}
    {:cmd/name "inspect" :cmd/doc "Inspect extension metadata." :cmd/internal? true}
    {:cmd/name "search" :cmd/doc "Search extension commands."}
    {:cmd/name "audit" :cmd/doc "Audit extension commands."}]
   :cmd/extra-sections [{:title "INSTALLED EXTENSIONS"
                         :body
                         "  foundation   Foundation v/ ...\n  bridge       Bridge verification."}]})

(defdescribe
  render-command-grouping-test
  (it "splits internal vs contributed children into named sections, no `---- extensions ----`"
      (binding [commandline/*color-enabled?* false]
        (let [out (commandline/render-command (mixed-children-cmd) ["vis" "extension"])]
          (expect (str/includes? out "COMMANDS"))
          (expect (str/includes? out "EXTENSION COMMANDS"))
          (expect (str/includes? out "list"))
          (expect (str/includes? out "inspect"))
          (expect (str/includes? out "search"))
          (expect (str/includes? out "audit"))
          (expect (not (str/includes? out "---- extensions ----")))
          ;; Internal block must appear before the extension block.
          (let [a (.indexOf ^String out "list")
                b (.indexOf ^String out "search")]

            (expect (and (pos? a) (pos? b) (< a b)))))))
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

;; render-tree -- the root doc owns its own layout.
