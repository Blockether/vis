(ns com.blockether.vis.internal.decisions.cli
  "Install pinned decision models into the gateway's persistent model store."
  (:require [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.decisions.assets :as assets]))

(defn- cli-out! [s] (.println ^java.io.PrintStream config/original-stdout (str s)))

(defn- download-command
  [parsed _residual]
  (config/init-cli!)
  (let [id
        (get parsed "model")

        training?
        (boolean (get parsed "training"))

        installed
        (assets/download-model! id training?)]

    (doseq [[kind dir] installed]
      (cli-out! (str (name kind) ": " dir)))
    (when training?
      (cli-out! (str "To install CPython 3.12 training dependencies offline, run: sh "
                     (get installed :wheels)
                     "/install.sh NEW_ENV_DIRECTORY")))))

(defn- status-command
  [_parsed _residual]
  (config/init-cli!)
  (doseq [model (assets/manifest)]
    (let [inference (assets/artifact model :inference)
          dir (assets/install-dir model :inference)]

      (cli-out! (str (:id model)
                     "@" (:revision model)
                     ": " (if (assets/installed? inference dir) "installed" "not installed"))))))

(def command
  {:cmd/name "decisions"
   :cmd/doc "Install and inspect pinned decision models."
   :cmd/usage "vis-agent decisions models <download|status>"
   :cmd/subcommands
   [{:cmd/name "models"
     :cmd/doc "Manage local decision model assets."
     :cmd/usage "vis-agent decisions models <download|status>"
     :cmd/subcommands
     [{:cmd/name "download"
       :cmd/doc "Download the FP32 model, and optionally its offline training resources."
       :cmd/usage "vis-agent decisions models download --model NAME [--training]"
       :cmd/args [{:name "model"
                   :kind :flag
                   :type :string
                   :required true
                   :doc "Exact model name from the pinned catalog."}
                  {:name "training"
                   :kind :flag
                   :type :boolean
                   :doc "Also download the checkpoint and local CPython 3.12 wheelhouse."}]
       :cmd/run-fn #'download-command}
      {:cmd/name "status"
       :cmd/doc "List pinned models and their installed FP32 bundles."
       :cmd/usage "vis-agent decisions models status"
       :cmd/run-fn #'status-command}]}]})
