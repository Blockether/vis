(ns com.blockether.vis.internal.config.mcp-approval
  "Approval of the servers in the shared `.mcp.json`, as in Claude Code. A person
   approves or rejects each server in a file that the person owns, under `mcp`:
   `enable_all_project_mcp_servers`, `enabled_mcpjson_servers` and
   `disabled_mcpjson_servers`. These are the Claude Code settings
   `enableAllProjectMcpServers`, `enabledMcpjsonServers` and `disabledMcpjsonServers`.
   A committed file cannot approve its own servers. Vis starts only an approved
   server, because a stdio server runs a command of the repository.")

(def choice-keys
  "The keys under `mcp` that keep the choices."
  ["enable_all_project_mcp_servers" "enabled_mcpjson_servers" "disabled_mcpjson_servers"])

(defn status
  "`approved`, `rejected` or `pending` for the `.mcp.json` server `name`. `choices`
   is the `mcp` block of the files that the person owns. A rejection wins."
  [choices name]
  (let [listed? (fn [k]
                  (some #(= (str name) (str %)) (get choices k)))]
    (cond (listed? "disabled_mcpjson_servers") "rejected"
          (or (true? (get choices "enable_all_project_mcp_servers"))
              (listed? "enabled_mcpjson_servers"))
          "approved"
          :else "pending")))

(defn approved
  "The servers of `servers`, a map by `.mcp.json` name, that `choices` approve."
  [choices servers]
  (into {} (filter #(= "approved" (status choices (key %)))) servers))

(defn choose
  "The config map `raw` with the choice `choice` for the `.mcp.json` server `name`:
   `:approve`, `:approve-all`, `:reject`, or `:forget` to drop a choice."
  [raw name choice]
  (let [listed
        (fn [raw k]
          (vec (remove #{name} (get-in raw ["mcp" k]))))

        raw
        (-> raw
            (assoc-in ["mcp" "enabled_mcpjson_servers"] (listed raw "enabled_mcpjson_servers"))
            (assoc-in ["mcp" "disabled_mcpjson_servers"] (listed raw "disabled_mcpjson_servers")))

        raw
        (case choice
          :approve-all
          (-> raw
              (update-in ["mcp" "enabled_mcpjson_servers"] conj name)
              (assoc-in ["mcp" "enable_all_project_mcp_servers"] true))

          :approve
          (update-in raw ["mcp" "enabled_mcpjson_servers"] conj name)

          :reject
          (update-in raw ["mcp" "disabled_mcpjson_servers"] conj name)

          :forget
          raw)]

    (let [raw (reduce (fn [raw k]
                        (cond-> raw
                          (empty? (get-in raw ["mcp" k]))
                          (update "mcp" dissoc k)))
                      raw
                      ["enabled_mcpjson_servers" "disabled_mcpjson_servers"])]
      (cond-> raw
        (empty? (get raw "mcp"))
        (dissoc "mcp")))))

(defn reset
  "The config map `raw` without any choice."
  [raw]
  (let [raw (apply update raw "mcp" dissoc choice-keys)]
    (cond-> raw
      (empty? (get raw "mcp"))
      (dissoc "mcp"))))
