(ns com.blockether.vis.ext.bridge.core
  "Bridge extension entry point."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.bridge.doctor :as doctor]
   [com.blockether.vis.ext.bridge.extract-clojure-basic :as basic]
   [com.blockether.vis.ext.bridge.extract-clojure-lsp :as clj-lsp]
   [com.blockether.vis.ext.bridge.extract-markdown :as md]))

(defn- slurp-path [path]
  (slurp (io/file (str path))))

(defn extract-markdown
  "Extract Bridge Markdown facts from `path` using CommonMark. Returns normalized
   `{:nodes :edges :diagnostics :stats}` data; does not write to storage."
  [path]
  (md/extract-file (str path) (slurp-path path)))

(defn clojure-lsp-status
  "Return external clojure-lsp availability used by Bridge's Clojure extractor."
  ([] (clojure-lsp-status nil))
  ([opts]
   (clj-lsp/executable-status opts)))

(defn extract-clojure
  "Extract Bridge Clojure facts for a project.

   Options:
   - `:project-root` root path, default `.`
   - `:backend` `:clojure-lsp` (default when available) or `:basic`
   - `:path` source file path for `:basic` backend
   - `:content` optional source text for `:basic` backend

   External clojure-lsp is preferred. Basic edamame extraction is syntax-only."
  ([] (extract-clojure nil))
  ([opts]
   (let [opts (or opts {})
         backend (or (:backend opts)
                   (when (clj-lsp/available? opts) :clojure-lsp)
                   :basic)]
     (case backend
       :clojure-lsp (clj-lsp/extract-project opts)
       :basic (let [path (or (:path opts)
                           (throw (ex-info ":basic Clojure extraction requires :path"
                                    {:type :bridge/missing-path})))
                    content (or (:content opts) (slurp-path path))]
                (basic/extract-file path content))
       (throw (ex-info "Unknown Bridge Clojure extractor backend"
                {:type :bridge/unknown-backend
                 :backend backend}))))))

(defn- render-summary [result]
  (let [{:keys [stats]} result]
    (str "Bridge extraction: "
      (or (:language stats) "unknown")
      ", nodes=" (count (:nodes result))
      ", edges=" (count (:edges result))
      (when-let [path (:path stats)] (str ", path=" path))
      (when-let [backend (:backend stats)] (str ", backend=" backend)))))

(defn- render-channel [result]
  (str (render-summary result)
    "\n\n```clojure\n"
    (pr-str (update result :nodes #(take 20 %)))
    "\n```"))

(def bridge-symbols
  [(vis/symbol #'extract-markdown
     {:journal-render-fn render-summary
      :channel-render-fn render-channel})
   (vis/symbol #'clojure-lsp-status
     {:journal-render-fn #(str "clojure-lsp available=" (:available? %)
                            (when-let [path (:path %)] (str ", path=" path)))
      :channel-render-fn #(str "```clojure\n" (pr-str %) "\n```")})
   (vis/symbol #'extract-clojure
     {:journal-render-fn render-summary
      :channel-render-fn render-channel})])

(defn- prompt [_env]
  (str (vis/render-prompt
         {:ext/doc "Bridge codebase graph tools"
          :ext/ns-alias {:alias 'bridge}
          :ext/symbols bridge-symbols})
    "\nBridge v1 extracts Markdown via CommonMark and Clojure via external clojure-lsp when available. Results are normalized facts: :nodes, :edges, :diagnostics, :stats."))

(def vis-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.bridge.core
     :ext/doc "Bridge codebase knowledge graph extension: CommonMark Markdown extraction and Clojure semantic extraction via external clojure-lsp."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "code-intelligence"
     :ext/ns-alias {:ns 'vis.ext.bridge :alias 'bridge}
     :ext/symbols bridge-symbols
     :ext/prompt prompt
     :ext/doctor-check-fn doctor/check-fn}))

(vis/register-extension! vis-extension)
