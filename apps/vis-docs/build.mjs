import { build } from 'esbuild';
import { cp, readFile, rm, writeFile } from 'node:fs/promises';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { security } from './headers.js';

const dist = new URL('./dist/', import.meta.url);
await rm(dist, { recursive: true, force: true });
// Use the engine's renderer and dependency pins, not a second Markdown implementation.
execFileSync('clojure', ['-M', '-e', `(require '[com.blockether.vis.internal.docs.core :as docs]) (docs/build-site! ${JSON.stringify(fileURLToPath(dist))} {:public? true})`], {cwd: fileURLToPath(new URL('../../', import.meta.url)), stdio: 'inherit'});
await cp(new URL('./web/style.css', import.meta.url), new URL('assets/catalog.css', dist));
await build({entryPoints: [fileURLToPath(new URL('./web/main.js', import.meta.url))], outfile: fileURLToPath(new URL('assets/app.js', dist)), bundle: true, format: 'esm', target: 'es2022', minify: true});
await writeFile(new URL('_headers', dist), '/*\n'+Object.entries(security).map(([key,value])=>`  ${key}: ${value}\n`).join('')+'  Cache-Control: public, max-age=0, must-revalidate\n');
// Keep / and explicit .html URLs static without spending a Worker request on the home page.
await writeFile(new URL('_redirects', dist), '/ /index.html 200\n');
// A mismatch must fail the build, not silently introduce another theme.
const source = await readFile(new URL('../../resources/vis-docs/assets/theme.css', import.meta.url));
const output = await readFile(new URL('assets/theme.css', dist));
if (!source.equals(output)) throw new Error('Documentation stylesheet differs');
