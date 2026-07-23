// Generates capacitor.config.json from the typed capacitor.config.mts source.
//
// jiti transpiles the TypeScript itself (its own oxc/babel-based loader) and
// never imports the project's `typescript` package, so this works with TS 7
// (tsgo) where the Capacitor CLI's own `.ts` loader crashes. See the header of
// capacitor.config.mts for the full rationale.
import { createJiti } from 'jiti';
import { writeFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';

const here = dirname(fileURLToPath(import.meta.url));
const root = resolve(here, '..');
const source = resolve(root, 'capacitor.config.mts');
const target = resolve(root, 'capacitor.config.json');

const jiti = createJiti(import.meta.url);
const mod = await jiti.import(source, { default: true });
const config = mod?.default ?? mod;

if (!config || typeof config !== 'object' || !config.appId) {
  console.error('gen-capacitor-config: capacitor.config.mts did not export a valid config');
  process.exit(1);
}

writeFileSync(target, JSON.stringify(config, null, 2) + '\n');
console.log(`capacitor.config.json regenerated from capacitor.config.mts (appId ${config.appId})`);
