(ns com.blockether.vis.internal.foundation.mcp.cli-test
  "`vis-agent mcp` takes the arguments and scopes of `claude mcp`."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.foundation.mcp.cli :as mcp-cli]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.test-temp :as test-temp]
            [lazytest.core :refer [defdescribe it expect]]))

(defn- spec-of
  [words]
  (mcp-cli/add-spec (mcp-cli/parse-words #{"scope" "transport" "env" "header"} words)))

(defn- refusal
  [f]
  (try (f)
       nil
       (catch clojure.lang.ExceptionInfo e (when (:vis/user-error (ex-data e)) (ex-message e)))))

(defn- cli!
  "Run the `mcp` subcommand `f` in workspace `dir`; return its printed lines."
  [dir f & words]
  (let [out (atom [])]
    (with-redefs [commandline/stdout! #(swap! out conj %)
                  config/init-cli! (fn [])
                  workspace/cwd-root (constantly (str dir))]

      (f nil words))
    @out))

(defdescribe
  claude-arguments
  (it "reads the commands of the Claude Code manual"
      (expect (= {"transport" "stdio"
                  "command" "npx"
                  "args" ["-y" "airtable-mcp-server"]
                  "env" {"AIRTABLE_API_KEY" "YOUR_KEY"}}
                 (spec-of ["--transport" "stdio" "--env" "AIRTABLE_API_KEY=YOUR_KEY" "airtable" "--"
                           "npx" "-y" "airtable-mcp-server"])))
      (expect (= {"transport" "streamable_http"
                  "url" "https://api.example.com/mcp"
                  "headers" {"Authorization" "Bearer your-token"}}
                 (spec-of ["--transport" "http" "secure-api" "https://api.example.com/mcp"
                           "--header" "Authorization: Bearer your-token"])))
      (expect (= {"transport" "sse" "url" "https://mcp.asana.com/sse" "headers" {"X-Api-Key" "abc"}}
                 (spec-of ["-t" "sse" "-H" "X-Api-Key: abc" "asana" "https://mcp.asana.com/sse"])))
      (expect (= {"transport" "stdio" "command" "srv" "args" ["--help"] "env" {"A" "1" "B" "2"}}
                 (spec-of ["-e" "A=1" "B=2" "local" "--" "srv" "--help"]))))
  (it "refuses a server option before -- with the fix"
      (expect (re-find #"Unknown option '-y'.*after --"
                       (refusal #(spec-of ["airtable" "npx" "-y" "airtable-mcp-server"])))))
  (it "refuses the shapes that claude mcp add refuses"
      (expect (re-find #"Invalid environment variable format: KEY"
                       (refusal #(spec-of ["-e" "KEY" "x" "--" "cmd"]))))
      (expect (re-find #"Invalid name a\.b" (refusal #(spec-of ["a.b" "cmd"]))))
      (expect (re-find #"Invalid transport type: ws" (refusal #(spec-of ["-t" "ws" "x" "u"]))))))

(defdescribe
  claude-scopes
  (it "writes .mcp.json for --scope project, and the engine reads it below vis.yml"
      (let [dir (test-temp/temp-dir "vis-mcp-project")]
        (cli! dir
              #'mcp-cli/cli-mcp-add!
              "-s" "project"
              "-t" "http"
              "notion" "https://mcp.notion.com/mcp")
        (cli! dir #'mcp-cli/cli-mcp-add! "--scope=project" "docs" "--" "docs-mcp" "--stdio")
        (expect (= {"mcpServers"
                    {"notion" {"type" "http" "url" "https://mcp.notion.com/mcp"}
                     "docs" {"type" "stdio" "command" "docs-mcp" "args" ["--stdio"] "env" {}}}}
                   (json/read-json (slurp (io/file dir ".mcp.json")))))
        (spit (io/file dir "vis.yml")
              "mcp:\n  servers:\n    docs:\n      transport: stdio\n      command: team-docs\n")
        (binding [workspace/*workspace-root* (str dir)]
          (expect (= {"notion" {"transport" "streamable_http" "url" "https://mcp.notion.com/mcp"}
                      "docs" {"transport" "stdio" "command" "team-docs"}}
                     (get-in (config/load-project-root-config-raw) ["mcp" "servers"]))))))
  (it "keeps a local server private, refuses a second add, and asks for a scope on remove"
      (let [dir (test-temp/temp-dir "vis-mcp-local")]
        (cli! dir #'mcp-cli/cli-mcp-add! "-e" "TOKEN=${TOKEN}" "fs" "--" "fs-mcp")
        (expect (= {"transport" "stdio" "command" "fs-mcp" "env" {"TOKEN" "${TOKEN}"}}
                   (binding [workspace/*workspace-root* (str dir)]
                     (get-in (config/load-project-config-raw) ["mcp" "servers" "fs"]))))
        (expect (= "MCP server fs already exists in local config"
                   (refusal #(cli! dir #'mcp-cli/cli-mcp-add! "fs" "other"))))
        (cli! dir #'mcp-cli/cli-mcp-add-json! "-s" "project" "fs" "{\"command\":\"fs-mcp\"}")
        (expect (re-find #"exists in multiple scopes"
                         (refusal #(cli! dir #'mcp-cli/cli-mcp-remove! "fs"))))
        (expect (= ["Removed MCP server fs from local config"
                    (str "File modified: " (io/file dir ".vis" "config.yml"))]
                   (cli! dir #'mcp-cli/cli-mcp-remove! "fs" "-s" "local")))
        (expect (not (.exists (io/file dir ".vis" "config.yml"))))))
  (it "matches a name the way the engine keys it"
      (let [dir (test-temp/temp-dir "vis-mcp-names")]
        (cli! dir #'mcp-cli/cli-mcp-add! "My_Srv" "--" "srv-mcp")
        (expect (= "MCP server my-srv already exists in local config"
                   (refusal #(cli! dir #'mcp-cli/cli-mcp-add! "my-srv" "other"))))
        (cli! dir #'mcp-cli/cli-mcp-remove! "my-srv")
        (expect (not (.exists (io/file dir ".vis" "config.yml")))))))
