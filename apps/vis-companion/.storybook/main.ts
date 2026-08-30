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
 * Storybook stays on its stable line and does one job: render the shipped UI.
 * Spel owns browser automation, measurement and screenshots, so this preview needs
 * neither an agent protocol nor prerelease change-detection tools.
 *
 * Each addon earns its line: `addon-docs` renders component prose; `addon-a11y`
 * explains axe's roles, labels and contrast findings; `addon-vitest` makes axe plus
 * every `play` flow a Chromium test instead of an optional panel.
 */
const config: StorybookConfig = {
  stories: ['../src/**/*.stories.@(ts|tsx)'],
  framework: { name: '@storybook/react-vite', options: {} },
  addons: [
    '@storybook/addon-docs',
    '@storybook/addon-a11y',
    '@storybook/addon-vitest',
  ],
};

export default config;
