# vis-exa extension

Package: `com.blockether/vis-exa`. Source: `extensions/common/vis-exa/`.

`vis-exa` adds Exa MCP search tools under the `exa/` SCI alias.

## Tools

```clojure
(def r (exa/web-search "latest Clojure release" {:num-results 5}))
(get-in r [:result :content])

(def c (exa/code-context "rewrite-clj z/find-value examples" {:tokens-num 12000}))
(get-in c [:result :content])
```

Tool results are Vis envelopes. Payload lives under `:result`; do not read `(:content r)` or `[:result ...]`.

## Endpoint and token behavior

The extension calls Exa's HTTP MCP endpoint:

```text
https://mcp.exa.ai/mcp
```

Basic usage works without an API key. Set `EXA_API_KEY` or `EXA_MCP_API_KEY` for higher limits. The key is passed as the MCP endpoint query parameter `exaApiKey` and redacted from info/errors.

## Configuration

Environment variables are supported directly:

| Variable | Meaning | Default |
| --- | --- | --- |
| `EXA_MCP_URL` | MCP endpoint URL | `https://mcp.exa.ai/mcp` |
| `EXA_MCP_TOOLS` | comma-separated tool allowlist | `web_search_exa,get_code_context_exa` |
| `EXA_API_KEY`, `EXA_MCP_API_KEY` | optional Exa API key | unset |
| `EXA_MCP_TIMEOUT_MS` | request timeout | `30000` |
| `EXA_MCP_PROTOCOL_VERSION` | MCP initialize version | `2025-06-18` |
| `EXA_MCP_MAX_BYTES` | max retained output bytes | `51200` |
| `EXA_MCP_MAX_LINES` | max retained output lines | `2000` |
| `EXA_MCP_CONFIG` | explicit JSON config path | unset |

JSON config is also supported through an explicit `EXA_MCP_CONFIG` path:

```json
{
  "url": "https://mcp.exa.ai/mcp",
  "apiKey": "${EXA_API_KEY}",
  "tools": ["web_search_exa", "get_code_context_exa"],
  "timeoutMs": 30000,
  "protocolVersion": "2025-06-18",
  "maxBytes": 51200,
  "maxLines": 2000
}
```

Config string values can reference environment variables as `$NAME`, `${NAME}`, or `env:NAME`.

## Output bounds

The MCP content is rendered into `[:result :content]`, then bounded by `:max-bytes` and `:max-lines` call options, capped by configured `maxBytes` and `maxLines`. If truncated, the full output is written to a temp file and `[:result :temp-file]` points at it.
