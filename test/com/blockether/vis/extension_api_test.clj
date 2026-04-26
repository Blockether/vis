(ns com.blockether.vis.extension-api-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.loop.runtime.conversation.environment.extension :as ext]
   [lazytest.core :refer [defdescribe it expect]]))

(def read-symbol
  (vis/symbol 'read-file (fn [& _] nil)
    {:doc "Read a file preview."
     :arglists '([path] [path offset limit])}))

(def retries-value
  (vis/value 'max-retries 3
    {:doc "Maximum retry attempts."}))

(defdescribe extension-prompt-rendering-test

  (it "renders canonical prompt text from symbol docstrings + arglists"
    (expect
      (= "Filesystem tools (use fs/ prefix; positional args only)\n- (fs/read-file path) or (fs/read-file path offset limit) — Read a file preview.\n- fs/max-retries — Maximum retry attempts.\nRULES:\n- Discover paths first."
        (vis/render-extension-prompt
          {:ext/doc "Filesystem tools"
           :ext/ns-alias {:ns 'vis.ext.fs :alias 'fs}
           :ext/symbols [read-symbol retries-value]
           :usage-note "positional args only"
           :notes ["RULES:" "- Discover paths first."]}))))

  (it "assembles canonical extension prompt inside the loop and appends extra notes"
    (let [environment {:extensions (atom [(vis/extension
                                           {:ext/namespace 'com.acme.ext.fs
                                            :ext/doc "Filesystem tools"
                                            :ext/group "filesystem"
                                            :ext/ns-alias {:ns 'vis.ext.fs :alias 'fs}
                                            :ext/prompt "RULES:\n- Discover paths first."
                                            :ext/symbols [read-symbol retries-value]})])}
          ;; assemble-system-prompt requires :active-extensions — compute
          ;; ONCE per call site (here, once per snapshot) and pass in.
          active-exts   (vis/active-extensions environment)
          system-prompt (vis/assemble-system-prompt environment
                          {:active-extensions active-exts})]
      (expect (str/includes? system-prompt "[namespace: fs → vis.ext.fs]"))
      (expect (str/includes? system-prompt "Filesystem tools (use fs/ prefix)"))
      (expect (str/includes? system-prompt "- (fs/read-file path) or (fs/read-file path offset limit) — Read a file preview."))
      (expect (str/includes? system-prompt "- fs/max-retries — Maximum retry attempts."))
      (expect (str/includes? system-prompt "RULES:\n- Discover paths first."))))

  (it "re-exports extension helpers from com.blockether.vis.core"
    (expect (identical? ext/extension vis/extension))
    (expect (identical? ext/symbol vis/symbol))
    (expect (identical? ext/value vis/value))
    (expect (identical? ext/render-prompt vis/render-extension-prompt))
    (expect (identical? ext/render-prompt vis/preview-extension-prompt))
    (expect (identical? ext/register-global! vis/register-global!))
    (expect (identical? ext/registered-extensions vis/registered-extensions))
    (expect (identical? ext/discover-extensions! vis/discover-extensions!))
    (expect (identical? ext/load-extension! vis/load-extension!))
    (expect (identical? ext/reload-extension! vis/reload-extension!))))
