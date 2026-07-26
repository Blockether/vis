// React rules enforcer: official eslint-plugin-react-hooks `recommended-latest`
// preset — rules-of-hooks + exhaustive-deps PLUS the React Compiler's static
// analyzer (purity, immutability, preserve-manual-memoization, set-state-in-*).
// Run with `npm run lint`.
import reactHooks from 'eslint-plugin-react-hooks';
import tseslint from 'typescript-eslint';

export default tseslint.config(
	{ ignores: ['dist', 'ios', 'android', 'node_modules'] },
	{
		files: ['src/**/*.{ts,tsx}'],
		languageOptions: {
			parser: tseslint.parser,
			parserOptions: { ecmaFeatures: { jsx: true } },
		},
		extends: [reactHooks.configs.flat['recommended-latest']],
	},
);
