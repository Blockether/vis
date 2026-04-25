<h2 align="center">
  <br/>vis<br/>
</h2>

<div align="center">
— an <a href="https://arxiv.org/abs/2512.24601">RLM-inspired</a> coding agent built from the ground up. Works with any text-based model.
<br/>
<sub>Code-eval over tool-calls. O(1) context per iteration. Secure-by-default SCI sandbox.</sub>
</div>

<div align="center">
  <h2>
    <a href="https://clojars.org/com.blockether/vis"><img src="https://img.shields.io/clojars/v/com.blockether/vis?color=%23007ec6&label=clojars" alt="Clojars version"></a>
    <a href="https://github.com/Blockether/vis/blob/main/LICENSE">
      <img src="https://img.shields.io/badge/license-Apache%202.0-green" alt="License - Apache 2.0">
    </a>
    <a href="https://blockether.github.io/vis/">
      <img src="https://img.shields.io/badge/docs-mdBook-blue" alt="Docs">
    </a>
  </h2>
</div>

<div align="center">
<h3>

[Rationale](#rationale) • [Docs](https://blockether.github.io/vis/)

</h3>
</div>

## Rationale

Every mainstream coding agent accumulates messages into a growing context
window, then compacts when it overflows. Vis takes a different approach
inspired by [Recursive Language Models](https://arxiv.org/abs/2512.24601):
the context is an **external environment** the model interacts with
through code.

The model writes Clojure. A sandboxed [SCI](https://github.com/babashka/sci)
interpreter executes it. Results flow back as a compact journal. State
lives in named vars and SQLite, not in the token budget.

| | Tool-call agents | Vis |
|---|---|---|
| **Context growth** | O(n) — all messages accumulated | O(1) — fixed per iteration |
| **Compaction** | Required | Never needed |
| **Ops per LLM call** | N tools (harness-dispatched) | N code blocks (LLM-composed, async via futures) |
| **API requirement** | `function_calling` / `tool_use` | Any model that outputs JSON |
| **State** | Context window only | Named vars + SQLite |
| **Security** | Permission prompts | Deny-by-default sandbox |

→ **[Full rationale with diagrams](https://blockether.github.io/vis/rationale.html)**

→ **[Full documentation](https://blockether.github.io/vis/)**

## License

Copyright 2025-2026 Blockether

Licensed under the [Apache License, Version 2.0](LICENSE).
