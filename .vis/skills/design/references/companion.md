# Companion implementation and review

Paths are relative to `apps/vis-companion/`. Read `src/components/ui.tsx` when
adding or changing controls. Use its existing controls. A call-site `className`
may control position only; props and variants control appearance. Use Tailwind
v4 tokens, not component CSS, CSS modules, CSS-in-JS or inline styles. Use
`sm:` for available-space layout and `mouse:` for pointer density.

Never edit generated `ios/` or `android/`; native behavior goes through
`scripts/ios-prepare.mjs` or `scripts/android-prepare.mjs`.

Use stable Storybook for components and the running app for screens. Render
production source for proposals. Use Spel for navigation, measurements and
interaction checks; do not add MCP or another browser tool.

Split components by responsibility rather than line count or DOM structure.
Screens handle routing and data loading. A feature component can contain one
interaction and its states so it can be tested independently. Keep single-use
fragments local. Add a generic control to `ui.tsx` when a second caller needs
it, and convert both callers in the same commit. Do not create unused props,
unnecessary wrappers or preview-only copies.

Add stories for reusable components and their relevant states in the same
commit. Controls use `ui.stories.tsx`; data-heavy components use colocated
stories and `src/dev/story-data.ts`. Stories must not fetch data, wait on
timers or generate random data.

## Inspect production rendering

From `apps/vis-companion`:

```bash
npm run storybook                         # 127.0.0.1:6006
# Open one production story with an app theme:
STORY='http://127.0.0.1:6006/iframe.html?id=<story-id>&viewMode=story&globals=theme:<theme-id>'
SESSION="agent-$(date +%s)"
spel --session "$SESSION" set device "iPhone 14" &&
spel --session "$SESSION" --content-boundaries open "$STORY" &&
spel --session "$SESSION" wait --text '<story-owned copy>' &&
spel --session "$SESSION" --content-boundaries snapshot -i -c
```

For a mouse, use `set viewport 1280 800` instead of device emulation. To find
a story id, open the Storybook manager, take `snapshot -i -c -a`, select the
story using its current `@ref`, and open its iframe URL. Wait for the story's
text or role, not the preview container or spinner. Use one unique Spel session
for the task. Take a new snapshot after rendering changes and close only that
session when finished. Read command help for unknown arguments.

Review at phone 393×852, tablet 834×1194 and desktop 1280×800 sizes as relevant.
Use `snapshot -i -c`, `get box` and `styles` in each relevant size and theme.
Test changed interactions and inspect screenshots; a build does not verify
appearance.

## Attach a design review

When delivering a visual proposal or review, attach self-contained HTML built from the final
production source, not a screenshot or copied markup. The artifact is temporary evidence, not
tracked source. Open it with Spel before attachment.

**The artifact must use the production implementation:**

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

The attachment demonstrates component rendering and behavior with fixture data.
It does not include the app's credentials or native runtime.

## Completion

Verify changed interactions and states. Run the relevant tests, `npm run lint`,
`npm run test:storybook` and `npm run build` for UI code changes. Test behavior
and component contracts, not literal color values. Inspect the rendered UI
separately. Report fixture replacements and untested devices or themes.
