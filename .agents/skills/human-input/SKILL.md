---
name: human-input
description: >
  The Vis human-input (HITL) contract: which namespace parses, declares,
  validates or refuses, the closed clojure.spec vocabulary and its five check
  seams, and the TUI/companion mirrors that must not drift. Read before touching
  human input, dialogs fields or their specs.
---

# Human input (HITL) contract

The contract documents itself where it lives; read the docstring before changing either layer. `src/com/blockether/vis/internal/human_input.clj` PARSES, `internal/human_input/spec.clj` DECLARES the normalized form and OWNS the closed vocabulary, `internal/human_input/validation.clj` decides validator arity, `src/com/blockether/vis/human_input.clj` is the builder surface that refuses at the call site, and `internal/extension_check.clj` judges a Python extension without running it. Repo-wide rules that no single namespace can own:

- **No schema library beyond `clojure.spec.alpha`,** and never a second copy of the vocabulary beside the parser. Keys are added once in `spec.clj`; the parser DERIVES the snake_case spellings it accepts (`wire-keys`), so a key reaches the wire from one edit. The spec is checked at five seams and nowhere else — `checked-field`, `checked-group`, `checked-decor`, `checked-request`, and `checked-answer` inside `settle!`, the one funnel every answer passes through — once per request, never per keystroke. Views are NOT specced: `request->view` strips `:is-secret` and `:validate`.
- **The spec is the vocabulary of every SURFACE, not only the engine.** The TUI reads `text-types`, `choice-types`, `range-defaults` and `otp-defaults` off `human-input.spec`; the companion, which cannot require a Clojure namespace, mirrors them as `HUMAN_INPUT_FIELD_TYPES`, `HUMAN_INPUT_DECOR_TYPES`, `HUMAN_INPUT_NODE_TYPES`, `HUMAN_INPUT_RANGE_DEFAULTS`, `HUMAN_INPUT_OTP_DEFAULTS`, `HUMAN_INPUT_CHOICE_MARKS`. That mirror is not trusted: `human_input_cross_channel_test.clj` READS the TypeScript and fails when a type, a bound or a choice glyph drifts from the engine's table or from `dialogs/choice-mark`, so `●`/`○`/`[✓]`/`[ ]` mean the same thing in the terminal and in the app.
- **`human-input.fixture.json` is `request->view` verbatim,** and the Clojure suite pins that it holds one node of EVERY kind — so rendering it in `HumanInputPrompt.test.tsx` is the app's proof of complete support.
- **The two builder surfaces are ONE vocabulary with one spelling per node** — `com.blockether.vis.human-input` (Clojure) and the `vis.*` block in `resources/vis-python/extension_bootstrap.py`. A new host callback is registered in `host-member-names` or the static checker breaks with a `NameError`.
