# Reproduction Guide - Agent Failure Modes and Recovery

Conversation: `eeaf9651-06c7-4dda-9e97-877fcef06337`
Channel: :tui
Created: Thu Apr 30 10:34:11 CEST 2026
Total turns: 

---

## Diagnostic Summary

- **Total failures:** 41
- **Repetition loop detected:** true
- **By classification:** {:code-execution-error 13, :unresolved-symbol 28}

### Failure Clusters

| Signature | Count |
|-----------|-------|
| `[:code :unresolved-symbol "ExceptionInfo"]` | 28 |
| `[:code :code-execution-error "ExceptionInfo"]` | 5 |

---

## Turn-by-Turn Breakdown

### Turn `7a4fa411-b511-4c87-af91-aa9e1098b806`
- **Goal:** We have something called system prompt copy on the fucking TUI side but I think also in core. I want to remove it. I don't need it anymore. Pruuune!
- **Status:** 
- **Attempts:** 
- **Cost:** 
- **Iterations:** 0
- **Failures:** 
- **Errors:** 

### Turn `f9501570-1740-402f-be85-78a8e78675d4`
- **Goal:** Ok, there is also in the TUI this some % left in the footer and I'm thinking what is it's purpose if we ARE IN THE FUCKING RLM?! I think we don't need it anymore?!
- **Status:** 
- **Attempts:** 
- **Cost:** 
- **Iterations:** 0
- **Failures:** 
- **Errors:** 

### Turn `6bfddde1-6c51-4d94-906a-380a35eb4bd5`
- **Goal:** What REMAINING?!
- **Status:** 
- **Attempts:** 
- **Cost:** 
- **Iterations:** 0
- **Failures:** 
- **Errors:** 

### Turn `363de6c6-e612-495f-a119-cdb7821e0bae`
- **Goal:** Can you kindly kindly check the errors which happened in this conversation?
- **Status:** 
- **Attempts:** 
- **Cost:** 
- **Iterations:** 0
- **Failures:** 
- **Errors:** 

### Turn `760d9435-88ca-41d0-94e3-d4b63baa4cdf`
- **Goal:** Ok, can you kindly WRITE THE REPRODUCTION.md WITH THE REAL EXAMPLES, REAL PROBLEMS AND THE HOW YOU WOULD SOLVE THEM ETC?!
- **Status:** 
- **Attempts:** 
- **Cost:** 
- **Iterations:** 0
- **Failures:** 
- **Errors:** 

### Turn `990487e1-e335-4f8c-8cef-bdbfe5c1c7a2`
- **Goal:** Can you retry?
- **Status:** 
- **Attempts:** 
- **Cost:** 
- **Iterations:** 0
- **Failures:** 
- **Errors:** 

### Turn `c167af8f-9642-4e28-b7ba-9788eba1574d`
- **Goal:** Can you retry?
- **Status:** 
- **Attempts:** 
- **Cost:** 
- **Iterations:** 0
- **Failures:** 
- **Errors:** 

### Turn `5588740f-3423-4c23-8d36-d77e407732fc`
- **Goal:** Please add more information like the conversation-id, full thinking, journal at that time etc...
- **Status:** 
- **Attempts:** 
- **Cost:** 
- **Iterations:** 0
- **Failures:** 
- **Errors:** 


---

## Detailed Failure Log

| # | Turn Goal | Iter | Tool | Class | Message | Code (truncated) |
|---|-----------|------|------|-------|---------|-------------------|
| 1 | We have something called system prompt c | 2 | `def` | :code-execution-error | `ExceptionInfo: Path not found: /Users/fierycod/vis/src/com/b` | `(def tui-ls (v/ls "src/com/blockether/vis/tui"))` |
| 2 | We have something called system prompt c | 4 | `def` | :code-execution-error | `ExceptionInfo: Path not found: /Users/fierycod/vis/src/com/b` | `(def schema-hits (v/rg ["system_prompt" "system-prompt"] "sr` |
| 3 | We have something called system prompt c | 4 | `def` | :code-execution-error | `ExceptionInfo: Path not found: /Users/fierycod/vis/src/com/b` | `(def conv-hits (v/rg [":system-prompt" "system-prompt-copy"]` |
| 4 | We have something called system prompt c | 7 | `Let` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: Let` | `Let` |
| 5 | We have something called system prompt c | 7 | `me` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: me` | `me` |
| 6 | We have something called system prompt c | 7 | `dig` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: dig` | `dig` |
| 7 | We have something called system prompt c | 7 | `deeper` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: deeper` | `deeper` |
| 8 | We have something called system prompt c | 7 | `what` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: what` | `what` |
| 9 | We have something called system prompt c | 7 | `"system` | :code-execution-error | `Bare string literal in :code. Prose belongs in :answer (the ` | `"system prompt copy"` |
| 10 | We have something called system prompt c | 7 | `actually` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: actually` | `actually` |
| 11 | We have something called system prompt c | 7 | `is.` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: is.` | `is.` |
| 12 | We have something called system prompt c | 7 | `The` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: The` | `The` |
| 13 | We have something called system prompt c | 7 | `broad` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: broad` | `broad` |
| 14 | We have something called system prompt c | 7 | `search` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: search` | `search` |
| 15 | We have something called system prompt c | 7 | `didn't` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: didn't` | `didn't` |
| 16 | We have something called system prompt c | 7 | `anything` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: anything` | `anything` |
| 17 | We have something called system prompt c | 7 | `with` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: with` | `with` |
| 18 | We have something called system prompt c | 7 | `"copy"` | :code-execution-error | `Bare string literal in :code. Prose belongs in :answer (the ` | `"copy"` |
| 19 | We have something called system prompt c | 7 | `in` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: in` | `in` |
| 20 | We have something called system prompt c | 7 | `the` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: the` | `the` |
| 21 | We have something called system prompt c | 7 | `so` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: so` | `so` |
| 22 | We have something called system prompt c | 7 | `let` | :code-execution-error | `IllegalStateException: Can't take value of a macro: #'clojur` | `let` |
| 23 | We have something called system prompt c | 7 | `me` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: me` | `me` |
| 24 | We have something called system prompt c | 7 | `look` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: look` | `look` |
| 25 | We have something called system prompt c | 7 | `at` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: at` | `at` |
| 26 | We have something called system prompt c | 7 | `the` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: the` | `the` |
| 27 | We have something called system prompt c | 7 | `TUI` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: TUI` | `TUI` |
| 28 | We have something called system prompt c | 7 | `dialogs/screen` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: dialogs/screen` | `dialogs/screen` |
| 29 | We have something called system prompt c | 7 | `and` | :code-execution-error | `IllegalStateException: Can't take value of a macro: #'clojur` | `and` |
| 30 | We have something called system prompt c | 7 | `the` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: the` | `the` |
| 31 | We have something called system prompt c | 7 | `conversation` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: conversation` | `conversation` |
| 32 | We have something called system prompt c | 7 | `data` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: data` | `data` |
| 33 | We have something called system prompt c | 7 | `model` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: model` | `model` |
| 34 | We have something called system prompt c | 7 | `more` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: more` | `more` |
| 35 | We have something called system prompt c | 7 | `carefully.` | :unresolved-symbol | `ExceptionInfo: Unable to resolve symbol: carefully.` | `carefully.` |
| 36 | Ok, there is also in the TUI this some % | 7 | `def` | :code-execution-error | `ExceptionInfo: Unmatched delimiter: ] [at line 148, column 1` | `(def f3
  (z/zedit "extensions/channels/vis-channel-tui/src/` |
| 37 | Ok, there is also in the TUI this some % | 7 | `def` | :code-execution-error | `ExceptionInfo: Unmatched delimiter: ] [at line 148, column 1` | `(def f4
  (z/zedit "extensions/channels/vis-channel-tui/src/` |
| 38 | What REMAINING?! | 2 | `def` | :code-execution-error | `ArityException: Wrong number of args (1) passed to: com.bloc` | `(def dup-check (v/rg ["cost-str   (format-cost (session-cost` |
| 39 | Ok, can you kindly WRITE THE REPRODUCTIO | 0 | `conversation-title` | :code-execution-error | `Parse error: Invalid symbol: with:` | `(conversation-title "Agent failure reproduction guide")

(le` |
| 40 | Ok, can you kindly WRITE THE REPRODUCTIO | 1 | `conversation-title` | :code-execution-error | `Parse error: Invalid symbol: fixes:` | `(conversation-title "Agent failure reproduction guide")

(de` |
| 41 | Can you retry? | 0 | `conversation-title` | :code-execution-error | `Parse error: Invalid symbol: failures:` | `(conversation-title "Agent failure reproduction guide")

(de` |

---

## Failure 1 - Wrong File Paths (Blind Assumption)

### Symptom

`v/ls` and `v/rg` throw `ExceptionInfo: Path not found`.

### Real occurrences

Turn: `We have something called system prompt copy on the fucking TUI side but I think also in core. I want`

| Iteration | Code | Error |
|-----------|------|-------|
| 2 | `(def tui-ls (v/ls "src/com/blockether/vis/tui"))` | `Path not found: .../src/com/blockether/vis/tui` |
| 4 | `(v/rg ["system_prompt"] "src/com/blockether/vis/internal/db.clj")` | `Path not found: .../db.clj` |
| 4 | `(v/rg [":system-prompt" "system-prompt-copy"] "src/.../db.clj")` | `Path not found` |

### Root cause

Agent guessed paths from naming conventions instead of probing the
actual filesystem. The Vis repo uses a polylith-style `extensions/`
layout. The old `src/com/blockether/vis/tui` path never existed.

### Prevention

Always probe the filesystem before assuming a path exists.

    ;; WRONG - assuming paths
    (def tui-ls (v/ls "src/com/blockether/vis/tui"))

    ;; CORRECT - discover first
    (def src-tree (v/ls "src"))
    src-tree
    (def ext-tree (v/ls "extensions"))
    ext-tree
    (def hits (v/rg ["system-prompt-copy"] "extensions"))
    hits

---

## Failure 2 - Reader Boundary Split (Prose Leaks Into Code)

### Symptom

28 unresolved-symbol errors from a single iteration. Bare English words
like `Let`, `me`, `dig`, `deeper`, `what` parsed as Clojure symbols.

### Real occurrence

Turn: `We have something called system prompt copy on the fucking TUI side but I think also in core. I want`
Iteration: 7

The agent emitted the following as a code block:

    Let me dig deeper what "system prompt copy" actually is.

Each token was parsed individually:

    Let     -> Unable to resolve symbol: Let
    me      -> Unable to resolve symbol: me
    dig     -> Unable to resolve symbol: dig
    deeper  -> Unable to resolve symbol: deeper
    what    -> Unable to resolve symbol: what
    "system prompt copy" -> Bare string literal in :code

28 unresolved-symbol errors in a single iteration.

### Root cause

A multi-line code block got fragmented by the tokenization boundary.
The agent emitted English prose inside a code block. Each word became
a bare symbol that the SCI evaluator tried to resolve.

### Prevention

Code blocks must contain ONLY valid Clojure forms. Prose goes in
the answer block. Comments inside code blocks are fine when preceded
by `;;`.

    ;; WRONG - prose in code block
    Let me check the file structure and find where system-prompt-copy is defined.

    ;; CORRECT - pure Clojure with optional ;; comments
    (def src-tree (v/ls "src"))
    src-tree
    ;; Find the system-prompt-copy references
    (def hits (v/rg ["system-prompt-copy"] "src"))
    hits

---

## Failure 3 - Chained v/edit Breaks Syntax

### Symptom

`z/zedit` fails with `Unmatched delimiter` after a prior `v/edit`
left the file with duplicate or malformed content.

### Real occurrence

Two `v/edit` calls targeted overlapping regions in the same `let`
binding vector in `footer.clj`:

    ;; BEFORE (correct)
    ;;        ctx        (ctx-left-info messages model)
    ;;        cost-str   (format-cost (session-cost messages))]

    ;; v/edit 1 removed the ctx binding line
    ;; v/edit 2 then created a DUPLICATE cost-str binding
    ;;        cost-str   (format-cost ...))]        cost-str   (format-cost ...))]

Subsequent z/zedit calls all failed:

    (z/zedit "footer.clj" (fn [zl] ...))
    ;; => ExceptionInfo: Unmatched delimiter: ] [at line 148, column 114]

### Root cause

The first `v/edit` replacement string ended with `]` (closing the
let vector). The second `v/edit` matched the already-modified line
and appended instead of replacing, creating an unbalanced `]]`.

### Prevention

After every `v/edit`, immediately verify the file still parses.

    ;; After editing, run a no-op zedit to verify parse
    (z/zedit "footer.clj" identity)
    ;; If this returns without error, the file is structurally sound.

    ;; If it fails, read the corrupted area and fix it
    (def broken-area (v/cat "footer.clj" {:offset 140 :limit 20}))
    broken-area
    (v/edit "footer.clj"
      "cost-str   (format-cost ...))]        cost-str   (format-cost ...)]"
      "cost-str   (format-cost (session-cost messages))]")
    ;; Verify again
    (z/zedit "footer.clj" identity)

---

## Failure 4 - Incomplete Prune (Dead Code Left Behind)

### Symptom

A feature is removed from its usage site but helper functions,
imports, and comments referencing it remain.

### Real occurrence

The ctx-left percentage was removed from `build-segments` (the usage
site), but four helper functions and the `svar-router` import were
still present:

    line 32:  (:require [com.blockether.svar.internal.router :as svar-router]
    line 65:  (defn- last-assistant-tokens
    line 73:  (defn- estimate-next-context-chars
    line 94:  (defn- ctx-left-info
    line 116: (defn- ctx-color

### Prevention

After removing a feature from its usage site, grep for ALL related
concepts across the entire source tree.

    ;; Step 1 - Remove the usage site
    (v/edit "footer.clj" "old-segment-code" "")

    ;; Step 2 - Grep for ALL references to the removed concept
    (v/rg ["ctx-left" "context-limit" "ctx-color" "last-assistant-tokens"]
      "extensions/channels/vis-channel-tui/src")

    ;; Step 3 - Remove each hit (functions, imports, comments)

    ;; Step 4 - Verify zero hits
    (v/rg ["ctx-left"] "extensions/channels/vis-channel-tui/src")
    ;; => {:hits [] :truncated-by :end-of-results}

---

## Failure 5 - v/rg Arity Mismatch (Single File vs Directory)

### Symptom

`ArityException: Wrong number of args (1) passed to: .../grep-files`

### Real occurrence

    (v/rg ["cost-str"] "extensions/.../footer.clj")
    ;; => ArityException

### Root cause

`v/rg` expects a directory path, not a single file.

### Prevention

Always pass a directory to `v/rg`. Filter results by filename.

    ;; WRONG - passing a single file
    (v/rg ["pattern"] "specific-file.clj")

    ;; CORRECT - pass the containing directory, filter hits
    (def hits (v/rg ["pattern"] "extensions/channels/vis-channel-tui/src"))
    (filter #(.endsWith (:path %) "footer.clj") (:hits hits))

    ;; ALTERNATIVE - read the file and search with re-seq
    (def content (v/cat "specific-file.clj"))
    (keep-indexed (fn [i l] (when (re-find #"pattern" l) {:line (inc i) :text l}))
      (:lines content))
