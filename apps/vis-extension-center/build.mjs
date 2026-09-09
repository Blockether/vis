import { build } from 'esbuild';
import { cp, mkdir, readFile, rm } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';

const docs = new URL('../../resources/vis-docs/assets/', import.meta.url);
await rm(new URL('./dist/', import.meta.url), { recursive: true, force: true });
await mkdir(new URL('./dist/assets/', import.meta.url), { recursive: true });
for (const file of ['theme.css', 'fonts', 'blockether.png']) {
  await cp(new URL(file, docs), new URL('./dist/assets/' + file, import.meta.url), { recursive: true });
}
await cp(new URL('./web/style.css', import.meta.url), new URL('./dist/assets/catalog.css', import.meta.url));
await build({ entryPoints: [fileURLToPath(new URL('./web/main.js', import.meta.url))], outfile: fileURLToPath(new URL('./dist/assets/app.js', import.meta.url)), bundle: true, format: 'esm', target: 'es2022', minify: true });
// An asset mismatch must fail the build, not silently introduce a second theme.
const source = await readFile(new URL('theme.css', docs));
const output = await readFile(new URL('./dist/assets/theme.css', import.meta.url));
if (!source.equals(output)) throw new Error('Documentation stylesheet differs');
