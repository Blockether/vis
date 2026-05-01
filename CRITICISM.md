# Criticism: prompt budget, provenance, and extension/tool contracts

This document captures the main criticism of the current Vis system-prompt and extension/tool metadata model, with special attention to provenance.

## 1. System prompt: strong runtime model, weak cost discipline

The current system prompt is conceptually good at one thing: it teaches the model how to behave like an iterative coding agent.

Strengths:

- Clear read/eval/observe loop.
- Strong `(answer ...)` contract.
- Good guidance around state, gates, verification, and evidence.
- Explicit project rules reduce ambiguity during code changes.

Weaknesses:

- The prompt is too large for every turn.
- It mixes too many layers:
  - core runtime contract,
  - tool reference,
  - project policy,
  - environment snapshot,
  - skills registry,
  - formatting helper manual.
- It over-optimizes for non-trivial coding tasks and does not provide a strong fast-path for simple meta questions.
- It increases latency and token cost even when the user only asked for a short factual answer.
- It does not guarantee final-answer hygiene; the example still produced malformed markdown.

## 2. The provenance problem is only partially solved

Vis already has several useful provenance surfaces, but they are incomplete and inconsistent.

### 2.1 Extension-level provenance exists

The extension spec already supports:

- `:ext/version`
- `:ext/author`
- `:ext/owner`
- `:ext/license`
- `:ext/namespace`
- `:ext/doc`
- `:ext/kind`

See:

- `docs/src/extensions/spec.md`
- `src/com/blockether/vis/internal/extension.clj`

This is good. It means Vis already has a notion of package provenance and authorship.

### 2.2 But the turn-level snapshot drops part of that provenance

`TURN_ACTIVE_EXTENSIONS` currently exposes a reduced snapshot roughly shaped like:

- `:alias`
- `:namespace`
- `:doc`
- `:version`
- `:kind`
- `:symbols`
- `:docs`

See:

- `src/com/blockether/vis/internal/prompt.clj`

Important provenance is missing from the model-facing snapshot:

- `:author`
- `:owner`
- `:license`
- manifest/source identity
- whether the extension is first-party, vendored, or user-installed

So the metadata exists in the extension registry, but not enough of it reaches the model in the most important place.

### 2.3 Tool-call provenance exists, but its schema is too loose

Tool-like symbols already return a structured envelope with:

- `:ok?`
- `:result`
- `:result-shape`
- `:provenance`
- `:markdown`
- `:error`

See:

- `src/com/blockether/vis/internal/tool_result.clj`

That is a strong direction.

However, `:provenance` is currently only enforced as `map?`.

That means Vis guarantees the presence of a provenance field, but not its semantics.
There is no shared machine-readable contract for:

- which provenance keys are always present,
- which are tool-specific,
- which are required for read tools vs write tools,
- how to interpret timing/path/source/truncation facts.

In practice, this is only a partially-specified envelope.

### 2.4 Foundation tools use provenance well, but locally

The foundation editing tools attach useful provenance such as:

- operation id,
- target path,
- timing,
- truncation,
- hit counts,
- change facts.

See:

- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/core.clj`

This is good local discipline.

But it is still local discipline, not a globally enforced contract across all extensions.

## 3. We do not yet have a full tool spec

If "full spec" means a machine-checkable contract for what a tool is, what it accepts, what it returns, and where it came from, then the answer is: not yet.

Currently we have:

- human-facing `:doc`
- `:arglists`
- `:examples`
- optional `:result-spec`
- optional hooks (`:before-fn`, `:after-fn`, `:on-error-fn`, extension-level parse rescue)

This is useful, but incomplete.

What is missing:

- a machine-readable input schema for args,
- a machine-readable schema for opts maps,
- a first-class provenance schema,
- a first-class declaration of side effects,
- a first-class declaration of trust level / origin / ownership.

So the current model is closer to:

- documented functions with optional output validation,

than to:

- fully specified tools.

## 4. Enforcement is partial, not end-to-end

### 4.1 Good: extension maps are validated

The extension builder and validator do enforce a real spec for the extension map.

See:

- `src/com/blockether/vis/internal/extension.clj`

That gives Vis a strong registration boundary.

### 4.2 Good: some tool outputs are validated

Foundation tools attach `:result-spec ::tool/tool-result`, so their public results are checked.

This is a solid pattern.

### 4.3 Bad: the most important provenance field is under-specified

Because `::provenance` is just `map?`, extensions can technically comply while emitting inconsistent or low-value provenance.

So the contract says:

- provenance exists,

but not:

- provenance means the same thing everywhere.

### 4.4 Bad: there is at least one concrete docs/spec/runtime drift

There is a real mismatch around symbol-level parse-error rescue.

The docs mention symbol-level `:on-parse-error-fn`:

- `docs/src/extensions/spec.md`
- `docs/src/extensions/hooks.md`

The runtime also looks for `:ext.symbol/on-parse-error-fn` during parse rescue:

- `src/com/blockether/vis/internal/extension.clj`

But the symbol-entry spec/builder path does not fully define/store that field alongside the other symbol options.

That is exactly the kind of drift a "full spec" system should prevent.

In other words: the feature is documented and partly implemented in runtime behavior, but not cleanly enforced in the extension DSL surface.

## 5. Recommended direction

## 5.1 Separate provenance into layers

Vis should distinguish at least four layers:

1. **Extension provenance**
   - namespace
   - version
   - author
   - owner
   - license
   - package/source id
   - first-party vs third-party vs user-local

2. **Symbol provenance**
   - extension namespace
   - symbol name
   - public alias
   - doc source
   - declared input/output contracts

3. **Invocation provenance**
   - started-at / finished-at / duration
   - arguments summary
   - environment context if relevant
   - retries / repairs / hook interventions

4. **Result provenance**
   - target path/query/source
   - truncation
   - counts
   - changed files
   - data freshness / cache info when applicable

Right now these layers are mixed together or only partially present.

## 5.2 Make more provenance visible to the model

Expand `TURN_ACTIVE_EXTENSIONS` to include at least:

- `:author`
- `:owner`
- `:license`
- registry / manifest id
- maybe `:source` or installation origin

Without this, the metadata exists but the model cannot reliably cite it when asked about provenance.

## 5.3 Introduce a first-class symbol input spec

A better symbol contract would include something like:

- `:input-spec` or `:args-spec`
- optional `:opts-spec`
- `:result-spec`
- `:provenance-spec`

That would let extensions declare more than prose and examples.

## 5.4 Tighten the tool result contract

Keep the common envelope, but make provenance stronger.

At minimum:

- define required common provenance keys for all tool results,
- define optional per-tool-family keys,
- validate both success and failure provenance.

For example, all tool results could require:

- `:op`
- `:started-at-ms`
- `:finished-at-ms`
- `:duration-ms`

and then layer tool-family specifics on top.

## 5.5 Add conformance tests for extension metadata drift

Vis should have tests that fail when:

- docs mention symbol options that the builder cannot accept,
- runtime reads keys that the builder/spec never produces,
- extension prompt snapshots omit provenance fields that the registry considers canonical.

This is especially important for the extension DSL because it is the public authoring surface.

## 6. Bottom line

The current system is already better than an untyped "just call random functions" agent runtime.
It has real structure.

But it does not yet have a full provenance model or a full tool contract.

What exists today is:

- good extension metadata,
- good local tool envelopes,
- partial output validation,
- incomplete model-facing provenance,
- incomplete end-to-end enforcement.

The next step should not be "add more prose".
The next step should be:

- promote provenance to a first-class contract,
- surface that contract to the model,
- enforce it uniformly across extensions.
