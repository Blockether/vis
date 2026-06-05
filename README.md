# Vis

Vis is a from-the-ground-up coding agent inspired by [Recursive Language Models](https://arxiv.org/abs/2512.24601) (Zhang, Kraska & Khattab, 2025): instead of spending the context window on an ever-growing transcript, the model writes Clojure into a sandboxed [SCI](https://github.com/babashka/sci) runtime, keeps state in named vars and SQLite, and treats context as an external environment it can inspect and change.

## Installation

### Prerequisites

- [Clojure CLI](https://clojure.org/guides/install_clojure) 1.12+
- Java 21+ (JDK 25 recommended)

### Setup

Clone the repository and start the development nREPL:

```bash
git clone https://github.com/blockether/vis.git
cd vis
./bin/dev nrepl
```

This boots the project nREPL on `:7888` and writes `.nrepl-port`.

### Verify

Run the quick verification suite before committing changes:

```bash
./verify.sh --quick
```
