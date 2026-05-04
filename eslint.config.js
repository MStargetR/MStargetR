import js from '@eslint/js';
import { defineConfig } from 'eslint/config';
import globals from 'globals';

export default defineConfig([
  { ignores: ['.claude/', 'docs/', 'node_modules/'] },
  {
    languageOptions: { globals: globals.node },
    plugins: { js },
    extends: ['js/recommended'],
    files: ['**/*.{js,mjs,cjs}'],
  },
  {
    files: ['inst/shiny/**/*.js'],
    languageOptions: {
      globals: {
        ...globals.browser,
        Shiny: 'readonly',
        $: 'readonly',
        jQuery: 'readonly',
      },
    },
    rules: {
      'no-unused-vars': ['error', { caughtErrors: 'none' }],
    },
  },
]);
