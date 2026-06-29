# Vis — Product Context

## Register

**Brand** (marketing / docs). Design IS part of the product here — these are
the public docs and landing for an open-source coding agent. The goal is a
landing + docs that read as a serious engineering tool, not a SaaS template.

The app/runtime UI of Vis itself is out of scope for this brief; this file
governs the **docs site** (`resources/vis-docs/`, rendered by
`src/com/blockether/vis/internal/docs.clj`).

## What it is

Vis is a coding agent that treats context as an environment, not a transcript.
The model writes Python into a sandboxed GraalPy runtime, keeps durable state
outside the context window, and inspects/changes the host project through
tools. Written in Clojure, ships as a single GraalVM native binary, works with
any text-based model.

## Who uses it

Developers — from individual hackers to small eng teams — who want a coding
agent they can actually reason about. They're technical, skeptical of hype, and
notice when something looks templated or decorative. They read docs to
understand *how* something works, not to be sold to.

## Personality

**Precise / engineered.** Serious, understated, confident through restraint.
Reads like a real engineering tool — closer to Stripe's API docs or Linear's
site than a marketing landing. Confident, not loud. The craft is in the
spacing, the type, and the accuracy of the claims — not in gradients or
decoration.

## Anti-references (what it must NOT look like)

- **The cream/gold current look.** Warm-paper backgrounds (#fffdf9, #faf6ee)
  with gold accents are the saturated AI-default of 2026 and read as templated.
  This is the #1 thing to leave behind.
- Generic SaaS landing pages (big metric numbers, identical card grids,
  gradient hero text, "hero-metric template").
- Dark-mode-for-its-own-sake developer cosplay (neon-on-black terminal
  aesthetic).

## Strategic design principles

- **Restraint as a signal of quality.** Whitespace and precision do the work
  that decoration does in lesser docs. The page should feel *built*, not
  *designed at*.
- **Accuracy over persuasion.** Every claim on the page is verifiable from the
  codebase. Copy states what Vis does in plain terms, never sells.
- **One focal point per zone.** The hero leads with the idea (context as
  environment), not with three competing elements.
- **Type carries the personality.** A deliberate type scale and pairing, not
  the default Inter-everywhere.
- **Reading experience first.** Docs are read for understanding; line length,
  contrast, and rhythm serve long-form reading, not marketing scanability.

## Accessibility

WCAG AA at minimum. Body text ≥ 4.5:1 (target ≥ 7:1 where feasible).
Reduced-motion respected. No information conveyed by color alone.
Keyboard-navigable. Responsive down to mobile.
