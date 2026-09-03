import type { StorybookConfig } from '@storybook/react-vite';

/**
 * Storybook renders production components through the app's Vite configuration and is
 * excluded from the product build. Docs, accessibility and Vitest addons support the
 * repository's design review and browser-test flow.
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
