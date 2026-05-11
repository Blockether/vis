(ns com.blockether.vis.ext.bridge.doctor
  "Bridge doctor checks."
  (:require
   [com.blockether.vis.ext.bridge.languages.clojure-lsp :as clj-lsp]))

(defn- clojure-lsp-check [_env]
  (let [{:keys [available? command path version error stderr]} (clj-lsp/executable-status)]
    (if available?
      [{:level :info
        :message (str "Bridge clojure-lsp: available at " path
                   (when (seq version) (str " (" version ")")))
        :data {:command command :path path :version version}}]
      [{:level :warn
        :message (str "Bridge clojure-lsp: external CLI not found for command `" command "`.")
        :remediation "Install clojure-lsp and ensure it is on PATH, or set BRIDGE_CLOJURE_LSP_CMD. Bridge will fall back to syntax-only edamame extraction for Clojure."
        :data {:command command :error error :stderr stderr}}])))

(defn- commonmark-check [_env]
  (try
    (Class/forName "org.commonmark.parser.Parser")
    [{:level :info
      :message "Bridge CommonMark: available for Markdown extraction."}]
    (catch Throwable t
      [{:level :error
        :message (str "Bridge CommonMark: missing: " (or (ex-message t) t))
        :remediation "Ensure org.commonmark/commonmark is on the Vis classpath."}])))

(defn- stamp [check-id msgs]
  (mapv #(assoc % :check-id check-id) msgs))

(defn check-fn
  "Bridge `:ext/doctor-check-fn`. Reports Markdown parser and external
   clojure-lsp readiness."
  [env]
  (vec
    (concat
      (stamp ::commonmark (commonmark-check env))
      (stamp ::clojure-lsp (clojure-lsp-check env)))))
