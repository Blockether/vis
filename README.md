<p align="center">
  <img src="https://raw.githubusercontent.com/Blockether/vis/main/docs/src/logo.png" alt="Vis logo" width="200">
</p>

# Vis

A from-the-ground-up coding agent inspired by
[Recursive Language Models](https://arxiv.org/abs/2512.24601) (Zhang,
Kraska & Khattab, 2025). Works with any text-based model.

Instead of accumulating messages into an ever-growing context window,
Vis treats the context as an **external environment** the model
interacts with through code. The model writes Clojure, a sandboxed
[SCI](https://github.com/babashka/sci) interpreter executes it, and
results flow back as a compact journal. State lives in named vars and
a SQLite DB, not in the token budget. No compaction, no sliding
windows, no "summarize the last 50 messages".

[Visit the book to learn more.](https://blockether.github.io/vis/)