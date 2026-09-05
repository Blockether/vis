# Companion implementation and review

Paths are relative to `apps/vis-companion/`. Read the module contract in
`src/components/ui.tsx` when adding or changing a control. Its vocabulary is closed: use the shipped
controls, not lookalike buttons. Call-site `className` may position only; visible appearance belongs
to a prop or variant. Tailwind v4 tokens only, no component CSS, CSS modules, CSS-in-JS or inline
styles. `sm:` owns available-space layout; `mouse:` owns pointer density.

Never edit generated `ios/` or `android/`; native behavior goes through
`scripts/ios-prepare.mjs` or `scripts/android-prepare.mjs`.

Use stable Storybook for components and the running app for screens. Proposals render production
source, not copied mockups. Spel navigates, measures and exercises the render; do not add MCP or
a second browser layer.

Split by ownership, not line count or DOM shape. A screen owns routing, data loading and
orchestration; a feature component may own one coherent interaction and all of its visible states so
it can be rendered and tested alone. Keep a one-use screen fragment local. A generic control enters
`ui.tsx` only at its second real call site, with both callers converted in the same commit. Never
extract a wrapper, a speculative prop matrix or a preview-only twin of production markup.

Every reusable companion visual component and meaningful state has a story in the same commit.
Vocabulary controls live in `ui.stories.tsx`; data-heavy components use their colocated story and the
one fixture module `src/dev/story-data.ts`. Stories never fetch, wait on timers or generate random
data.

## Inspect the shipped render

From `apps/vis-companion`:

```bash
npm run storybook                         # 127.0.0.1:6006
# isolated shipped story, in a real theme:
STORY='http://127.0.0.1:6006/iframe.html?id=<story-id>&viewMode=story&globals=theme:<theme-id>'
SESSION="agent-$(date +%s)"
spel --session "$SESSION" set device "iPhone 14" &&
spel --session "$SESSION" --content-boundaries open "$STORY" &&
spel --session "$SESSION" wait --text '<story-owned copy>' &&
spel --session "$SESSION" --content-boundaries snapshot -i -c
```

For a fine pointer, replace device emulation with `set viewport 1280 800`. If the story id is unknown,
open the Storybook manager, take `snapshot -i -c -a`, select the story through its fresh `@ref`, then
open its isolated iframe route. Wait for copy or a role owned by the story — the preview shell and its
spinner are not readiness. Use one unique Spel session for the whole task, re-snapshot after repaint,
and close only that session when every comparison is finished. Read `spel <command> --help` before
guessing an argument.

Canonical companion review frames are phone 393×852, tablet 834×1194 and desktop 1280×800. At
each relevant frame and theme, use `snapshot -i -c`, `get box` and `styles` to settle claims; exercise
every changed interaction and inspect a screenshot yourself. A green build is not a visual review.

## Attach a design review

When delivering a visual proposal or review, attach self-contained HTML built from the final
production source, not a screenshot or copied markup. The artifact is temporary evidence, not
tracked source. Open it with Spel before attachment.

**One-to-one means one implementation, not similar pixels:**

1. The artifact imports the exact production component or composed story from `src/**`; it never
   copies JSX, serializes `outerHTML`, redraws a control or carries preview-only CSS.
2. It uses the same story args and deterministic fixture, decorators/providers, theme, production
   `index.css`, fonts, icons, viewport and input mode as the reviewed render. Tailwind scans both
   `src/**` and the artifact entry so no production class silently disappears.
3. Vite bundles React and inlines JS, CSS, fonts and images into one file. The result has no
   `localhost`, network fetch, external script, stylesheet or asset dependency.
4. A backend, gateway or native API may be replaced only at its existing boundary. Name that fixture
   or adapter beside the attachment; never claim that boundary is live.
5. Spel opens both the Storybook/running-app state and the standalone file at the same frame, then
   checks accessible names and states, representative boxes and styles, and each important
   interaction result. A mismatch rejects the artifact.

This is exact production rendering and behaviour **inside the component boundary**, with explicit
fixture data; it is not a claim that the attachment has the app's credentials or native shell.

## Completion

Verify changed interactions and states. Run the relevant tests, `npm run lint`,
`npm run test:storybook` and `npm run build` for UI code changes. Tests pin behavior and vocabulary
boundaries, not literal paint tokens. A green build is not a visual review. Report fixture seams
and any unavailable device or theme coverage rather than claiming it passed.
