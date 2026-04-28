(ns com.blockether.vis.extension-api-test
  "Cross-package tests for the extension/runtime contract.

   The pure extension authoring tests (`ext/symbol`, `ext/value`,
   `ext/render-prompt`, `ext/extension`) live with the standalone
   facade: see `com.blockether.vis.extension-test`.
   This file only exercises the integration points that need vis-core
   on the classpath:

   - `vis/active-extensions` + `vis/assemble-system-prompt`
   - the no-re-export guard that keeps the contract from leaking back
     into the runtime namespace."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.extension :as ext]
   [com.blockether.vis.loop.core :as loop-core]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as environment-core]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

(def ^:private cat-symbol
  (ext/symbol 'cat (fn [& _] nil)
    {:doc "Read a file preview."
     :arglists '([path] [path offset limit])}))

(def ^:private retries-value
  (ext/value 'max-retries 3
    {:doc "Maximum retry attempts."}))

(defdescribe extension-runtime-composition-test

  (it "assembles canonical extension prompt inside the loop and appends extra notes"
    (let [environment {:extensions (atom [(ext/extension
                                            {:ext/namespace 'com.acme.ext.fs
                                             :ext/doc       "Filesystem tools"
                                             :ext/group     "filesystem"
                                             :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
                                             :ext/prompt    "RULES:\n- Discover paths first."
                                             :ext/symbols   [cat-symbol retries-value]})])}
          ;; assemble-system-prompt requires :active-extensions — compute
          ;; ONCE per call site (here, once per snapshot) and pass in.
          active-exts   (vis/active-extensions environment)
          system-prompt (vis/assemble-system-prompt environment
                          {:active-extensions active-exts})]
      (expect (str/includes? system-prompt "[namespace: vis → vis.ext.tools]"))
      (expect (str/includes? system-prompt "Filesystem tools (use vis/ prefix)"))
      (expect (str/includes? system-prompt "- (vis/cat path) or (vis/cat path offset limit) — Read a file preview."))
      (expect (str/includes? system-prompt "- vis/max-retries — Maximum retry attempts."))
      (expect (str/includes? system-prompt "RULES:\n- Discover paths first."))))

  (it "register-extension! applies :autobind-fn and records footer events"
    (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]}
          (environment-core/create-sci-context nil)
          environment {:extensions (atom [])
                       :sci-ctx sci-ctx
                       :sandbox-ns sandbox-ns
                       :initial-ns-keys initial-ns-keys
                       :autobind-events-atom (atom [])
                       :autobind-registry-atom (atom {})}
          extension
          (ext/extension
            {:ext/namespace 'com.acme.ext.autobind
             :ext/doc       "Autobind fixture"
             :ext/group     "filesystem"
             :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
             :ext/prompt    "placeholder"
             :ext/symbols   [(ext/symbol 'echo-path (fn [path] (str "content:" path))
                               {:doc "Echo path"
                                :arglists '([path])
                                :autobind-fn (fn [{:keys [args result]}]
                                               {:bindings [{:kind :file
                                                            :id (first args)
                                                            :content result
                                                            :tag result}]})})]})
          _ (loop-core/register-extension! environment extension)
          _ (sci/eval-string+ sci-ctx "(vis/echo-path \"src/core.clj\")"
              {:ns (sci/find-ns sci-ctx 'sandbox)})
          first-bound
          (:val (sci/eval-string+ sci-ctx "file__src__core-clj"
                  {:ns (sci/find-ns sci-ctx 'sandbox)}))
          first-events @(:autobind-events-atom environment)
          _ (sci/eval-string+ sci-ctx "(vis/echo-path \"src/core.clj\")"
              {:ns (sci/find-ns sci-ctx 'sandbox)})
          second-events @(:autobind-events-atom environment)
          last-event (last second-events)]
      (expect (= "content:src/core.clj" first-bound))
      (expect (= :bound (:status (first first-events))))
      (expect (string? (:footer (first first-events))))
      (expect (= :unchanged (:status last-event)))))

  (it "vis.core does not re-export the extension contract"
    ;; The extension authoring API lives on `com.blockether.vis.extension`;
    ;; vis.core is the runtime facade only. Re-exporting these
    ;; names from vis.core would drag the extension library into
    ;; the runtime classpath. Fail loud if any leak in.
    (expect (not (some #{'extension 'symbol 'value 'register-global!
                         'registered-extensions 'discover-extensions!
                         'load-extension! 'reload-extension!
                         'render-extension-prompt 'preview-extension-prompt}
                   (keys (ns-publics 'com.blockether.vis.core)))))))
