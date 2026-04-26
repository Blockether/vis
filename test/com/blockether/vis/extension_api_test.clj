(ns com.blockether.vis.extension-api-test
  "Tests pinning the public extension contract.

   Note the split: extension authoring (`symbol`, `value`, `extension`,
   `render-prompt`, `register-global!`) lives in
   `com.blockether.vis.extension` (standalone library). Runtime
   composition (`active-extensions`, `assemble-system-prompt`) stays in
   `com.blockether.vis.core`. The two are intentionally separate so
   extensions can pull just the contract without dragging the vis
   runtime."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.extension :as ext]
   [lazytest.core :refer [defdescribe it expect]]))

(def read-symbol
  (ext/symbol 'read-file (fn [& _] nil)
    {:doc "Read a file preview."
     :arglists '([path] [path offset limit])}))

(def retries-value
  (ext/value 'max-retries 3
    {:doc "Maximum retry attempts."}))

(defdescribe extension-prompt-rendering-test

  (it "renders canonical prompt text from symbol docstrings + arglists"
    (expect
      (= "Filesystem tools (use fs/ prefix; positional args only)\n- (fs/read-file path) or (fs/read-file path offset limit) — Read a file preview.\n- fs/max-retries — Maximum retry attempts.\nRULES:\n- Discover paths first."
        (ext/render-prompt
          {:ext/doc "Filesystem tools"
           :ext/ns-alias {:ns 'vis.ext.fs :alias 'fs}
           :ext/symbols [read-symbol retries-value]
           :usage-note "positional args only"
           :notes ["RULES:" "- Discover paths first."]}))))

  (it "assembles canonical extension prompt inside the loop and appends extra notes"
    (let [environment {:extensions (atom [(ext/extension
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

  (it "vis.core no longer re-exports the extension contract"
    ;; The split lives forever — if these come back, the extension
    ;; library got dragged into the runtime again. Fail loud.
    (expect (not (some #{'extension 'symbol 'value 'register-global!
                         'registered-extensions 'discover-extensions!
                         'load-extension! 'reload-extension!
                         'render-extension-prompt 'preview-extension-prompt}
                   (keys (ns-publics 'com.blockether.vis.core)))))))
