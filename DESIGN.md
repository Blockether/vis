# Vis Docs — Design System

The docs site aligns to the **official Vis theme**
(`src/com/blockether/vis/internal/theme.clj`) — the single source of truth
shared by the TUI and the web app channel. The docs render their own CSS
(`docs.clj`), but every color matches the official `light-palette` so the docs,
TUI, and web app read as one brand.

## Color

Token values are hex (matching `theme.clj`'s RGB-triple source style). The docs
define CSS vars that mirror the theme roles:

| Docs token | Hex | Theme source (`light-palette`) | Role |
|---|---|---|---|
| `--bg` | `#fff` | `:terminal-bg` | Pure white surface |
| `--fg` / ink | `#1e1e1e` | `:text-fg` | Body text (16.7:1) |
| `--dim` | `#505050` | (between text/border) | Secondary text (8.1:1) |
| `--primary` | `#2563eb` | `:header-active-tab-bg` | Indigo — **fills/borders only** (3.1:1, not for text) |
| `--primary-press` | `#0a32a0` | `:header-hover-fg` | Deep indigo — nav-active text (10.6:1) |
| `--link` | `#1e5ac8` | `:link-chrome-fg` | Link text (6.3:1) |
| `--accent` / gold | `#a16207` | `:code-result-fg` (amber-700) | Gold accent |
| `--success` | `#28a03c` | `:status-ok` | Status green |
| `--danger` | `#dc3232` | `:status-bad` | Status red |
| `--code-bg` | `#f0f3f8` | `:code-block-bg` | Code block background |

### Syntax tokens (match TUI `code-syntax-*`)
keyword `#196e76` · string `#965028` · number `#1e5ab4` · special `#7846aa` ·
comment `#787878` · punctuation `#505050` — all verbatim from `theme.clj`.

### Hard rules
- Text colors must hit ≥4.5:1 on white; `--primary` (#2563eb) is **never** used
  for text on white (it's a fill color) — use `--link` or `--primary-press`.
- No gradient text, no side-stripe borders, no cream/warm backgrounds.
- Light-only for now (dark mode is a follow-up; the TUI already has `vis-dark`).

## Type

Single grotesque family with committed weight contrast (stronger than a timid
pair). **Hanken Grotesk** (variable, 100-900) for everything; **JetBrains Mono**
(variable) for code. Neither is on the impeccable reflex-reject list
(Inter/Roboto/Geist/Fraunces/etc.).

| Role | Weight / size |
|---|---|
| Display (h1 hero) | 700, clamp(1.75rem, 3.4vw, 2.4rem) |
| Headings (h2/h3) | 600 |
| Body | 400, 1.0625rem, line-height 1.7, max 65–75ch |
| Code | 400 |

## Layout & motion
- `--measure: 44rem` content max.
- ease-out-quart (`cubic-bezier(0.25,1,0.5,1)`); reduced-motion respected.
- Font preloaded; gzip on the HTML doc.
