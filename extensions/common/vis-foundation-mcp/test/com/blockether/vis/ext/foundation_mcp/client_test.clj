(ns com.blockether.vis.ext.foundation-mcp.client-test
  "Exercises the MCP client's stdio transport + JSON-RPC handshake against a
   tiny fake server (test/resources/fake_mcp_server.py). Skips gracefully when
   python3 or the script isn't present so CI without python stays green."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.foundation-mcp.client :as mcp]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private server-path "extensions/common/vis-foundation-mcp/test/resources/fake_mcp_server.py")

(defn- on-path
  "Absolute path of `exe` if it's an executable on PATH, else nil."
  [exe]
  (some (fn [d]
          (let [f (io/file d exe)]
            (when (.canExecute f) (.getPath f))))
        (str/split (or (System/getenv "PATH") "") #":")))

(defdescribe mcp-stdio-client-test
             (it "initialize + tools/list + tools/call roundtrip over stdio"
                 (let [py
                       (on-path "python3")

                       f
                       (io/file server-path)]

                   (if-not (and py (.exists f))
                     (expect true) ; prereqs absent — skip, don't fail CI
                     (let [conn (mcp/connect "fake"
                                             {:transport :stdio :command py :args [(.getPath f)]})]
                       (try
                         ;; initialize handshake surfaced the server identity
                         (expect (= "fake" (get (:server-info conn) "name")))
                         (expect (true? (mcp/alive? conn)))
                         ;; tools/list (cached after first call)
                         (let [tools (mcp/list-tools conn)]
                           (expect (= 1 (count tools)))
                           (expect (= "echo" (get (first tools) "name")))
                           (expect (identical? tools (mcp/list-tools conn))))
                         ;; tools/call returns the content block
                         (let [r (mcp/call-tool conn "echo" {"msg" "hi"})]
                           (expect (false? (boolean (get r "isError"))))
                           (expect (= "echo: hi" (get-in r ["content" 0 "text"]))))
                         ;; unknown tool surfaces isError from the server
                         (let [r (mcp/call-tool conn "nope" {})]
                           (expect (true? (boolean (get r "isError")))))
                         (finally (mcp/close conn) (expect (false? (mcp/alive? conn))))))))))
