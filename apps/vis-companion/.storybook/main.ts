import type { StorybookConfig } from '@storybook/react-vite';

/**
 * THE GALLERY DRAWS WHAT SHIPS.
 *
 * A design proposal used to be a hand-written HTML frame: every class, token and
 * radius copied by hand, so the frame could disagree with `src/**` and no reader
 * could tell which one was wrong. A story IMPORTS the component instead, so the
 * only thing a frame can show is the code we ship — and a proposal becomes a diff
 * in `src/**`, photographed, then kept or reverted. It still costs no file.
 *
 * The builder reads the app's OWN `vite.config.ts`, so Tailwind v4, the React
 * Compiler pass, the aliases and the `@fontsource-variable` faces are the same
 * here as under `npm run dev`. There is nothing to keep in sync, and no second
 * place where a token can be spelled.
 *
 * `vite build` graphs `index.html` alone, so not one byte of this reaches the
 * product; `storybook build` writes `storybook-static/`, which is ignored.
 *
 * PINNED TO THE 10.6 LINE ON PURPOSE. Storybook ships its own agent skill —
 * `npx storybook skills get stories | write-story | setup`, the loader of which is
 * installed verbatim at `.vis/skills/stories/` — and that skill refuses to work
 * below 10.6, so the tool that draws the gallery is also the tool that says how a
 * story is written. `latest` is still 10.5.x, which is why every Storybook package
 * here is the exact same `10.6.0-beta.0` and they move together; when 10.6 goes
 * stable this is one `npx storybook upgrade`.
 *
 * Each addon earns its line. `addon-mcp` is what serves those skills and the
 * story-discovery tools; `addon-docs` renders the docstring above each story, so
 * the prose beside a control is READ instead of scrolled past; `addon-a11y` runs
 * axe over whichever story is open, which is the roles-and-labels half of the
 * design contract checked by a machine rather than by eye.
 */
const config: StorybookConfig = {
  stories: ['../src/**/*.stories.@(ts|tsx)'],
  framework: { name: '@storybook/react-vite', options: {} },
  addons: ['@storybook/addon-mcp', '@storybook/addon-docs', '@storybook/addon-a11y'],
};

export default config;
