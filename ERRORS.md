# TUI Keybinding Change Errors

## Context
Attempted to change TUI keybindings:
- Arrow Up/Down → history cycling (was scroll)
- Remove Ctrl+P/Ctrl+N history bindings

## Errors from Previous Iteration

### 1. Wrong search strings in v/edit calls

**i0.52** — `v/edit` on `input.clj` line 744:
```
search: " :arrow-up   (scroll-up! screen)"
ERROR: search not found
```
**Actual code at line 744:**
```clojure
      KeyType/ArrowUp    {:action :scroll-up :state state}
```
The search string used fictional function-call syntax `(scroll-up! screen)` instead of the actual data-driven action map `{:action :scroll-up :state state}`.

### 2. Wrong search strings for arrow-down

**i0.53** — `v/edit` on `input.clj` line 745:
```
search: " :arrow-down (scroll-down! screen)"
ERROR: search not found
```
**Actual code at line 745:**
```clojure
      KeyType/ArrowDown  {:action :scroll-down :state state}
```
Same issue — fictional function-call syntax vs actual data map.

### 3. Wrong search strings for Ctrl+P/Ctrl+N removal

**i0.55** — `v/edit` on `input.clj` lines 728-730:
```
search: "  :ctrl-p     (history-prev! input)\n  :ctrl-n     (history-next! input)\n  :backspace"
ERROR: search not found
```
**Actual code at lines 728-730:**
```clojure
          ;; Ctrl+P / Ctrl+N — input history (Emacs-style)
          (and ctrl (= c \p)) {:action :history-up :state state}
          (and ctrl (= c \n)) {:action :history-down :state state}
```
The search used `:ctrl-p` keyword-style dispatch and fictional `(history-prev! input)` calls. The real code uses `(and ctrl (= c \p))` guards inside a `cond` and returns action maps.

### 4. Nonexistent function referenced in test file

**i0.72** — Created test file referencing `#'input/make-input` which does not exist.
```clojure
(is (some? #'input/make-input))
```
The actual public functions in input.clj are: `empty-input`, `input->text`, `insert-char`, `insert-newline`, `delete-backward`, `move-left`, `move-right`, `move-up`, `move-down`, `paste-text`, `handle-key`, etc.

### 5. No actual edits succeeded

All three `v/edit` calls failed. The source files remain unchanged. The only file written was a broken test file.

### 6. Repeated failed attempts to read non-existent core.clj

**i0.13 through i0.36** — 10+ failed reads of `core.clj` that doesn't exist:
```
ERROR: File not found: .../channel_tui/core.clj
```
Wasted many iterations probing a file that doesn't exist instead of reading the actual dispatch code in `screen.clj`.

## Root Causes

1. **Hallucinated API shape** — Assumed function-call dispatch `(scroll-up! screen)` instead of reading the actual data-driven `{:action :scroll-up :state state}` pattern.
2. **Hallucinated key names** — Used `:ctrl-p`/`:arrow-up` keywords instead of reading the actual Lanterna `condp` = `ktype` dispatch with `KeyType/ArrowUp` and `(and ctrl (= c \p))` guards.
3. **Did not read the target code before editing** — Made 40+ exploration reads but never read lines 700-757 of input.clj (the actual `handle-key` function) before attempting edits.
4. **Test file referenced non-existent var** — Wrote `#'input/make-input` without checking what public vars actually exist.
5. **Repeated file-not-found probes** — Kept trying core.clj after the first 404.

## How to Fix

1. Read `input.clj` lines 700-757 to get exact current code of `handle-key`
2. Read `screen.clj` lines 940-990 to get exact action dispatch (already partially visible in journal)
3. Use the EXACT text from those reads as `v/edit` search strings
4. For test file, reference an actual public var like `#'input/empty-input`
5. Also update hint strings in `screen.clj` line 84 (`Ctrl+P/N history` → `↑↓ history`)
