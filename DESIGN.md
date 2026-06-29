# Vis Docs — Design System

Composed from `PRODUCT.md` (precise / engineered, anti cream-gold) and the
`impeccable` brand seed `oklch(0.550 0.105 230.0)` (cobalt). Strategy:
**restrained** — pure white surface, cobalt primary carries the identity,
accent ≤10%. Mood phrase: *"cold harbor at dawn — fog-muted light over still
steel water, the quiet competence before the boats leave."*

## Color (OKLCH — never hex in source)

| Token | OKLCH | Hex (ref only) | Role |
|---|---|---|---|
| `--bg` | `oklch(1.000 0.000 0)` | `#ffffff` | Pure white surface. NOT warm. |
| `--surface` | `oklch(0.980 0.004 250)` | `#f6f7f9` | Panels, code blocks. Faint cool tint, not warm. |
| `--line` | `oklch(0.920 0.006 250)` | `#e2e6eb` | Hairline borders, dividers. |
| `--ink` | `oklch(0.270 0.020 260)` | `#2b2f36` | Body text. Cool-tinted near-black. ≥12:1 vs bg. |
| `--muted` | `oklch(0.510 0.015 258)` | `#6b7280` | Secondary text. ≥5:1 vs bg. |
| `--primary` | `oklch(0.520 0.130 250)` | `#2f5fcc` | Cobalt. Links, brand, key actions. |
| `--primary-press` | `oklch(0.440 0.150 252)` | `#1f4fb0` | Link hover / pressed. |
| `--accent` | `oklch(0.620 0.135 195)` | `#0f9bb3` | Teal — second brand color. Used sparingly (badges, status). Distinct in hue AND lightness from primary. |

### Hard rules (from impeccable)
- OKLCH in source. Hex only as a comment reference.
- `ink` vs `bg` contrast ≥ 7:1. `muted` vs `bg` ≥ 4.5:1.
- `primary` chroma 0.13 (≤ 0.23 cap). Not fluorescent.
- `primary` vs `accent` contrast ≥ 1.7 (cobalt 250° vs teal 195° — distinct).

## Type

Deliberate pairing, not Inter-everywhere. **Geometric grotesque + humanist
mono** — the contrast axis the skill calls for.

| Role | Family | Weight / size |
|---|---|---|
| Display (h1 hero) | `'Inter Tight'`, system fallback | 700, clamp(1.75rem, 3.4vw, 2.4rem) |
| Headings (h2/h3) | `'Inter Tight'` | 600 |
| Body | `'Inter'` | 400, 17px, line-height 1.7, max 65–75ch |
| Code / mono | `'JetBrains Mono'` | 400 |

- Display letter-spacing: **-0.02em** (floor is -0.04; tighter touches).
- Solid color headings — **never gradient text** (absolute ban).
- `text-wrap: balance` on h1–h3; `text-wrap: pretty` on prose.

## Layout

- `--measure: 44rem` content max (≈70ch).
- Generous vertical rhythm; spacing varies for cadence, not uniform.
- No cards as default. No nested cards. No side-stripe borders.
- Responsive grid without breakpoints: `repeat(auto-fit, minmax(280px, 1fr))`.

## Motion

- Reduced-motion respected (`@media (prefers-reduced-motion: reduce)`).
- Ease-out-quart only. No bounce, no elastic. Short distances (≤20px).

## Bans (match-and-refuse)
- Gradient text. Side-stripe `border-left > 1px`. Ghost-card (1px border +
  ≥16px shadow). Hero-metric template. Identical card grids. Eyebrow above
  every section. Warm/cream body bg.
