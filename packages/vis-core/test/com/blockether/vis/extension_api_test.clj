(ns com.blockether.vis.extension-api-test
  "Cross-package tests for the extension/runtime contract.

   The pure extension authoring tests (`ext/symbol`, `ext/value`,
   `ext/render-prompt`, `ext/extension`) live with the standalone
   library: see `vis-extension/test/com/blockether/vis/extension_test.clj`.
   This file only exercises the integration points that need vis-core
   on the classpath:

   - `vis/active-extensions` + `vis/assemble-system-prompt`
   - the no-re-export guard that keeps the contract from leaking back
     into the runtime namespace."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.extension :as ext]
   [lazytest.core :refer [defdescribe it expect]]))

(def ^:private read-symbol
  (ext/symbol 'read-file (fn [& _] nil)
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
                                            :ext/ns-alias  {:ns 'vis.ext.fs :alias 'fs}
                                            :ext/prompt    "RULES:\n- Discover paths first."
                                            :ext/symbols   [read-symbol retries-value]})])}
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

  (it "vis.core no longer re-exports the extension contract"
    ;; The split lives forever — if these come back, the extension
    ;; library got dragged into the runtime again. Fail loud.
    (expect (not (some #{'extension 'symbol 'value 'register-global!
                         'registered-extensions 'discover-extensions!
                         'load-extension! 'reload-extension!
                         'render-extension-prompt 'preview-extension-prompt}
                   (keys (ns-publics 'com.blockether.vis.core)))))))
