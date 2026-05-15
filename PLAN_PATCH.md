# PLAN_PATCH.md

Goal: make `v/patch` accept Codex `apply_patch` pattern, while preserving current exact-replacement path as compatibility mode.

## Target behavior

Support Codex patch envelope:

```text
*** Begin Patch
*** Update File: path/to/file
@@ optional context
 old context
-old line
+new line
*** Add File: new.txt
+contents
*** Delete File: old.txt
*** End Patch
```

Operations:

- `*** Add File: <path>` creates file. Following lines must start with `+`.
- `*** Delete File: <path>` deletes existing file.
- `*** Update File: <path>` patches existing file.
- Optional `*** Move to: <new-path>` renames updated file.
- Hunks start with `@@` or `@@ <context>`.
- Hunk lines start with:
  - space: unchanged context line
  - `-`: old line
  - `+`: new line
- Optional `*** End of File` marks EOF-sensitive hunk.
- File refs stay workspace-safe through existing `safe-path` checks.

Keep current API too:

```clojure
(v/patch [{:path "x" :search "old" :replace "new"}])
```

## Public API plan

`v/patch` dispatch by input shape:

1. String input starting with `*** Begin Patch` -> Codex patch mode.
2. Map/vector with `:path :search :replace` -> existing exact mode.
3. Anything else -> clear error showing both valid shapes.

Examples:

```clojure
(v/patch "*** Begin Patch\n*** Update File: src/foo.clj\n@@\n-old\n+new\n*** End Patch\n")
```

```clojure
(v/patch [{:path "src/foo.clj" :search "old" :replace "new"}])
```

## Implementation plan

### 1. Add parser namespace

New namespace:

`extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/patch.clj`

Responsibilities:

- parse patch string into hunk data
- validate marker order
- preserve line content after prefix char
- reject malformed hunks with line number
- return pure data, no filesystem writes

Data shape:

```clojure
{:hunks [{:op :add
          :path "hello.txt"
          :contents "Hello\n"}
         {:op :delete
          :path "old.txt"}
         {:op :update
          :path "src/foo.clj"
          :move-to nil
          :chunks [{:change-context "defn foo"
                    :old-lines ["old"]
                    :new-lines ["new"]
                    :end-of-file? false}]}]}
```

### 2. Port Codex seek behavior

Implement `seek-sequence` matching order from Codex:

1. exact line sequence
2. ignore trailing whitespace
3. ignore leading + trailing whitespace
4. normalize common Unicode punctuation/spaces, then compare trimmed

Unicode normalization set:

- dashes: `2010 2011 2012 2013 2014 2015 2212` -> `-`
- single quotes: `2018 2019 201A 201B` -> `'`
- double quotes: `201C 201D 201E 201F` -> `"`
- odd spaces: `00A0 2002 2003 2004 2005 2006 2007 2008 2009 200A 202F 205F 3000` -> space

EOF behavior:

- if `:end-of-file? true`, first try match at EOF position, then fallback scan from current index.

### 3. Compute replacements like Codex

For each update chunk:

- split original file by `\n`
- drop trailing empty split element if file ends in newline
- maintain `line-index`
- if chunk has `:change-context`, seek one-line context from `line-index`; set `line-index` to context hit + 1
- if `:old-lines` empty, insert at EOF
- else seek `:old-lines` from `line-index`
- if not found and old lines end with `""`, retry without trailing empty sentinel; do same for new lines
- collect replacements as `[start old-len new-lines]`
- sort by start index
- apply replacements descending
- ensure output ends with newline, same as Codex

### 4. Filesystem application

Plan all hunks before writing when possible, unlike Codex partial-write behavior.

Reason: Vis current `v/patch` contract says all edits validate before any write. Keep safer invariant unless explicitly changed.

Per hunk:

- Add:
  - path must be inside cwd
  - parent dirs created through existing helper
  - fail if path is directory
  - decide overwrite policy: match Codex current behavior = writes content and may overwrite existing file; record overwritten content in result info
- Delete:
  - path must exist
  - path must not be directory
  - delete after full validation
- Update:
  - source file must exist and not be directory
  - compute new content from chunks
  - if `:move-to`, validate destination path inside cwd
  - write new content to destination, remove old path if moved

### 5. Result shape

Return existing tool-success structure with richer info:

```clojure
{:op :v/patch
 :path "src/foo.clj"
 :kind :file
 :result [{:path "src/foo.clj" :op :update}]
 :info {:mode :codex-apply-patch
        :files [{:path "src/foo.clj"
                 :op :update
                 :changed? true
                 :before "..."
                 :after "..."
                 :lines-before 10
                 :lines-after 11}]}}
```

Exact mode keeps current `:mode :exact-replace` optional.

### 6. `v/patch-check`

Support patch string input too.

For Codex mode:

- parse patch
- compute all planned file changes
- do not write
- return:

```clojure
{:valid? true
 :mode :codex-apply-patch
 :checks [{:path "src/foo.clj" :op :update :valid? true}]
 :failures []}
```

For exact mode: keep existing output.

### 7. Tests

Add tests in:

`extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/editing/patch_test.clj`

Coverage:

- parse add/update/delete/move patch
- add file creates expected content
- delete removes file
- update replaces lines with context
- multiple hunks same file
- pure insertion with no old lines
- EOF hunk behavior
- trailing whitespace fuzzy match
- trim fuzzy match
- Unicode punctuation fuzzy match
- malformed patch reports line number
- path escape rejected
- `v/patch-check` writes nothing
- exact mode still works unchanged
- all validation before writes: failing later hunk leaves earlier files unchanged

### 8. Docs/prompt updates

Update tool doc string near `patch-tool`:

- mention two accepted shapes
- include Codex envelope example
- keep exact mode guidance

Update introspection hints that currently say canonical exact text only.

Files likely touched:

- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/core.clj`
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/patch.clj`
- `extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/editing/core_test.clj`
- `extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/editing/patch_test.clj`
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/introspection.clj`

### 9. Verification

After code edits:

```bash
./verify.sh --quick
./verify.sh
```

Docs-only creation of this plan needs no verification.

## Compatibility decision

Recommended: do not remove exact mode. Add Codex mode. Models can use exact mode for deterministic small edits and Codex mode for larger diff-like edits.

## Source parity checklist

Codex parity items to match:

- `*** Begin Patch` / `*** End Patch` envelope
- `*** Add File:`
- `*** Delete File:`
- `*** Update File:`
- `*** Move to:`
- `@@` optional context marker
- line prefixes ` `, `-`, `+`
- `*** End of File`
- lenient boundary trim
- seek-sequence fuzzy matching order
- EOF trailing empty-line retry
- final newline normalization
- summary includes add/update/delete/move info

Intentional Vis difference:

- validate full patch before writes, preserving current all-or-nothing safety.
