# IDEAS

## 1) Runtime secret store (tool-safe secrets)

- Problem: current redaction only protects logs; secrets can still leak through tool results, traces, prompts, and saved conversation history.
- Proposal: add `com.blockether.vis.rlm.secrets` with:
  - `secret/put!` (store secret by key)
  - `secret/exists?` (presence check)
  - `secret/compare` (constant-time compare)
  - internal-only `secret/get` (never exposed as a normal tool)
- Add policy: tools may receive secret handles/ids, never raw secret values unless explicitly required.
- Why this is better than pure prompt discipline: gives technical enforcement, not just convention.

### Notes vs PI / opencode style setups

- PI/opencode-style flows usually rely on env vars + prompt constraints + output redaction; that helps ergonomics but does not fully prevent accidental model exposure.
- A runtime secret store adds stronger guarantees: least-privilege access, explicit read points, and auditability around secret use.
- This also creates a clean path to future backends (OS keychain, KMS, encrypted local store) without changing tool contracts.

## 2) LLM-path redaction (not just logs)

- Problem: `redact.clj` masks log output, but redaction is not systematically applied before model calls.
- Proposal: apply redaction on all outbound model-facing payloads:
  - message history
  - tool outputs returned to model
  - trace/progress snippets included in prompts
- Keep current log redaction, but treat it as a final safety layer, not the primary one.

## 3) Sub-RLM routing should be strategy-driven

- Problem: `cheap-sub-rlm-fn` currently hardcodes `{:optimize :cost}` in `query.clj`.
- Proposal: allow parent turn/runtime policy to select routing strategy (cost, latency, quality, or explicit model/profile).
- Initial shape:
  - runtime default in config
  - per-query override in options
  - optional parent-suggested strategy from the model/tool call context
- Goal: make sub-query execution intentional and controllable instead of globally fixed.

## 4) Patch engine strictness follow-ups

- Done: strict hunk header format + range/body count validation are implemented.
- Follow-up idea: include richer diagnostics in mismatch errors (header values, computed counts, and hunk preview) to speed up troubleshooting.
