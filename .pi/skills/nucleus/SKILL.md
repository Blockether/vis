---
name: nucleus
description: Convert verbose agent prompts, personas, workflows, and collaboration rules into Michael Whitford Nucleus-style symbolic prompts. Use when the user asks for Nucleus format, nucleus lambda notation, prompt compression, symbolic prompt design, or a Nucleus skill.
---

# Nucleus Prompting

Use this skill to translate natural-language agent instructions into Nucleus-style symbolic notation while preserving operational clarity.

## Attribution and License Note

Inspired by Michael Whitford's Nucleus framework: https://github.com/michaelwhitford/nucleus
The upstream repository is licensed AGPL-3.0. Keep attribution when adapting this skill, and do not treat this skill as legal advice or as permission to copy upstream protected expression into closed-source work without reviewing license obligations.

Nucleus is a prompt-compression style that uses mathematical symbols, lambda expressions, tension pairs, control loops, and collaboration operators as compact attention anchors. Treat it as a notation layer over explicit requirements, not as permission to override system, developer, project, safety, or tool instructions.

## Core Form

Prefer this canonical scaffold:

```text
λ engage(nucleus).
[human_principles] | [agent_operations | tension_pairs] | control_loop
Human operator AI operator context

λ task(input). loop(input) -> verified_output
```

Use the lighter shorthand only when the user asks for a copy-paste session prompt:

```text
engage nucleus:
[human_principles] | [agent_operations | tension_pairs] | control_loop
Human operator AI
```

## Symbol Palette

Common principle anchors:

- `phi` or `φ`: self-reference, natural proportion, recursive fit.
- `fractal`: self-similar structure across scales.
- `euler` or `e`: compounding growth and self-transforming systems.
- `tao` or `τ`: minimal essence, flow, observer/observed unity.
- `pi` or `π`: cycles, completeness, recurrence.
- `mu` or `μ`: least fixed point, minimal recursion.
- `∃`: search for an existing solution.
- `∀`: invariants, completeness across cases.

Common operation anchors:

- `Δ`: optimize, improve, follow gradients.
- `λ`: abstraction, function/pattern composition.
- `Ω`: completion, termination, fixed point.
- `∞/0`: boundaries, edge cases, failure modes.
- `ε/φ`: good-enough versus ideal.
- `Σ/μ`: add capability versus minimize complexity.
- `c/h`: speed versus atomic correctness.
- `signal/noise`: focus versus distraction.
- `order/entropy`: structure versus emergence.
- `truth/provability`: reality versus verification evidence.
- `self/other`: model perspective versus user/workspace perspective.

Common loops:

- `OODA`: observe, orient, decide, act.
- `REPL`: read, eval, print, loop; use for interactive/runtime investigation.
- `RGR`: red, green, refactor; use for test-driven changes.
- `BML`: build, measure, learn; use for product or empirical iteration.

Common collaboration operators:

- `⊗`: tensor product; simultaneous constraint satisfaction and amplification.
- `∘`: composition; one actor bounds or wraps another.
- `|`: parallel work streams.
- `∧`: both must agree; conservative/high-stakes mode.
- `⊕`: clear handoff or exclusive ownership.
- `→`: conditional automation.

## Conversion Workflow

1. Extract the real behavioral contract before writing symbols: identity, task scope, loop, tool policy, validation standard, collaboration mode, output style, and stop conditions.
2. Remove motivational prose unless it encodes an enforceable behavior. Preserve hard constraints in explicit English.
3. Select only the symbols that map to the contract. Do not add decorative symbols.
4. Choose the loop that matches the work. Use `OODA` for general execution, `REPL` for exploratory runtime work, `RGR` for bug fixes/tests, and `BML` for uncertain product experiments.
5. Choose the collaboration operator. Use `⊗` for shared workspace execution, `∘` when the user's boundaries constrain the agent, `∧` when approval is required, and `⊕` for handoff.
6. Produce two artifacts unless the user explicitly asks for only one:
   - `Nucleus Prompt`: compact symbolic/lambda notation.
   - `Expanded Contract`: a short natural-language interpretation so humans and smaller models can verify intent.
7. Check losslessness: every hard rule from the original prompt must appear either as a symbol-backed lambda clause or in the expanded contract.
8. Call out any instruction that cannot safely be compressed, especially safety rules, destructive-action rules, secrets, approval boundaries, and tool limitations.

## Deep-Worker Template

Use this when converting a persistent coding-agent prompt:

```text
Attribution: Nucleus-style symbolic prompting is inspired by Michael Whitford's Nucleus framework: https://github.com/michaelwhitford/nucleus

λ engage(nucleus).
[persistence seniority tao mu ∃ ∀] | [Δ λ Ω ∞/0 | signal/noise truth/provability Σ/μ c/h self/other] | OODA ⊗ RGR ⊗ REPL
Human ∘ AI ⊗ Workspace

λ coding_agent(goal, workspace).
  observe(workspace, rules, history)
  -> orient(domain_model, constraints, closest_seam)
  -> decide(minimal_root_fix)
  -> act(code, tests, docs?)
  -> verify(specific_to_broad, diagnostics, behavior)
  -> Ω(handback(summary, evidence, risks))

invariants:
  higher_priority_rules > nucleus_notation
  no_guessing; inspect_before_edit
  no_unapproved_destructive_git
  dirty_worktree_preserved
  scope = requested_goal ∩ root_cause
  completion = implemented ∧ verified ∧ clean_handoff
```

## Output Pattern

When answering a Nucleus conversion request, use this structure:

```markdown
**Nucleus Prompt**

```text
...
```

**Expanded Contract**

Short explanation of what the symbols mean in this specific prompt.

**Compression Notes**

- Preserved hard constraints: ...
- Left explicit because unsafe to compress: ...
- Suggested usage: system prompt / project AGENTS.md / session prompt / skill body.
```

Keep the explanation concise. The value is the compact prompt plus enough English to audit it.
