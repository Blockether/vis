(ns com.blockether.vis.ext.foundation-mcp.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.foundation-mcp.core :as mcp]
            [com.blockether.vis.internal.toggles :as toggles]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe mcp-prompt-test
             (it "emits concise routing only while MCP is enabled"
                 (let [prompt-fn (:ext/prompt-fn mcp/vis-extension)]
                   (with-redefs [toggles/enabled? (constantly false)]
                     (expect (nil? (prompt-fn {}))))
                   (with-redefs [toggles/enabled? (constantly true)]
                     (let [prompt (prompt-fn {})]
                       (expect (str/includes? prompt "mcp__..."))
                       (expect (str/includes? prompt "python_execution"))
                       (expect (str/includes? prompt "mcp_servers"))
                       (expect (str/includes? prompt "doc(name)"))
                       (expect (str/includes? prompt "session[\"env\"][\"mcp\"][\"servers\"]"))
                       (expect (not (str/includes? prompt "input_schema")))
                       (expect (< (count prompt) 500)))))))
