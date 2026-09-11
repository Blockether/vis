import { build } from 'esbuild';
import { cp, readFile, rm, writeFile } from 'node:fs/promises';
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { security } from './headers.js';
import { JSDOM } from 'jsdom';
import sharp from 'sharp';
import { origin, metadataHead, extensionIcon, sitemap } from './web/discovery.js';

const dist = new URL('./dist/', import.meta.url);
await rm(dist, { recursive: true, force: true });
// Use the engine's renderer and dependency pins, not a second Markdown implementation.
execFileSync(
  'clojure',
  [
    '-M',
    '-e',
    `(require '[com.blockether.vis.internal.docs.core :as docs] '[charred.api :as json]) (docs/build-site! ${JSON.stringify(fileURLToPath(dist))} {:public? true}) (spit ${JSON.stringify(fileURLToPath(new URL('source.json', dist)))} (json/write-json-str (docs/collect)))`,
  ],
  { cwd: fileURLToPath(new URL('../../', import.meta.url)), stdio: 'inherit' },
);
const { site, pages } = JSON.parse(await readFile(new URL('source.json', dist), 'utf8'));
await rm(new URL('source.json', dist));
for (const page of pages) {
  const file = new URL(page.slug + '.html', dist);
  const dom = new JSDOM(await readFile(file, 'utf8'));
  const document = dom.window.document;
  document.querySelector('title').remove();
  document.querySelector('meta[name="description"]').remove();
  document.head.insertAdjacentHTML(
    'beforeend',
    metadataHead({
      title: page.title + ' · ' + site.title + ' · Blockether',
      description: page.blurb,
      path: page.slug === 'index' ? '/' : '/' + page.slug + '.html',
      type: 'TechArticle',
      markdown: '/' + page.slug + '.md',
    }),
  );
  const link = document.querySelector('.center-link');
  link.innerHTML = extensionIcon;
  link.title = 'Extension Center';
  link.setAttribute('aria-label', 'Extension Center');
  await writeFile(file, dom.serialize());
  dom.window.close();
  await writeFile(new URL(page.slug + '.md', dist), page.md);
}
await writeFile(
  new URL('sitemap.xml', dist),
  sitemap(['/sitemap-docs.xml', '/extensions/sitemap.xml'], true),
);
await writeFile(
  new URL('sitemap-docs.xml', dist),
  sitemap(pages.map((page) => (page.slug === 'index' ? '/' : '/' + page.slug + '.html'))),
);
await writeFile(
  new URL('robots.txt', dist),
  `User-agent: *\nAllow: /\nDisallow: /api/\n\nSitemap: ${origin}/sitemap.xml\n`,
);
await writeFile(
  new URL('llms.txt', dist),
  `# Vis\n\n> ${site.tagline}\n\n## Documentation\n\n` +
    pages.map((page) => `- [${page.title}](${origin}/${page.slug}.md): ${page.blurb}\n`).join('') +
    `\n## Extension Center\n\n- [Public extensions](${origin}/extensions/llms.txt): Live catalog index.\n- [Catalog API](${origin}/api/extensions): Approved listings as JSON.\n\n## Optional\n\n- [Full documentation](${origin}/llms-full.txt)\n`,
);
await writeFile(
  new URL('llms-full.txt', dist),
  '# Vis documentation\n\n' +
    pages.map((page) => `Source: ${origin}/${page.slug}.md\n\n${page.md}`).join('\n\n---\n\n') +
    `\n\nLive extension catalog: ${origin}/extensions/llms.txt\n`,
);
// The header logo is only 287×256 and transparent; generate discovery images from the master.
const logo = await sharp(fileURLToPath(new URL('../../logo.png', import.meta.url)))
  .trim()
  .png()
  .toBuffer();
for (const [name, size] of [
  ['favicon-16.png', 16],
  ['favicon-32.png', 32],
  ['favicon-48.png', 48],
  ['apple-touch-icon.png', 180],
  ['icon-192.png', 192],
  ['icon-512.png', 512],
]) {
  await sharp(logo)
    .resize(size, size, { fit: 'contain', background: '#ffffff' })
    .flatten({ background: '#ffffff' })
    .png()
    .toFile(fileURLToPath(new URL(name, dist)));
}
// Opaque pixels and explicit margins keep link previews legible on client-selected backgrounds.
await sharp(logo)
  .resize(480, 480, { fit: 'contain', background: '#ffffff' })
  .flatten({ background: '#ffffff' })
  .extend({ left: 360, right: 360, top: 75, bottom: 75, background: '#ffffff' })
  .png()
  .toFile(fileURLToPath(new URL('assets/social-preview.png', dist)));
const favicon = await readFile(new URL('favicon-32.png', dist));
const ico = Buffer.alloc(22);
ico.writeUInt16LE(1, 2);
ico.writeUInt16LE(1, 4);
ico[6] = 32;
ico[7] = 32;
ico.writeUInt16LE(1, 10);
ico.writeUInt16LE(24, 12);
ico.writeUInt32LE(favicon.length, 14);
ico.writeUInt32LE(22, 18);
await writeFile(new URL('favicon.ico', dist), Buffer.concat([ico, favicon]));
await writeFile(
  new URL('site.webmanifest', dist),
  JSON.stringify({
    name: 'Vis documentation and extensions',
    short_name: 'Vis',
    start_url: '/',
    display: 'browser',
    icons: [192, 512].map((size) => ({
      src: `/icon-${size}.png`,
      sizes: `${size}x${size}`,
      type: 'image/png',
    })),
  }),
);
await cp(new URL('./web/style.css', import.meta.url), new URL('assets/catalog.css', dist));
await build({
  entryPoints: [fileURLToPath(new URL('./web/main.js', import.meta.url))],
  outfile: fileURLToPath(new URL('assets/app.js', dist)),
  bundle: true,
  format: 'esm',
  target: 'es2022',
  minify: true,
});
await writeFile(
  new URL('_headers', dist),
  '/*\n' +
    Object.entries(security)
      .map(([key, value]) => `  ${key}: ${value}\n`)
      .join('') +
    '  Cache-Control: public, max-age=0, must-revalidate\n',
);
// Keep / and explicit .html URLs static without spending a Worker request on the home page.
await writeFile(new URL('_redirects', dist), '/ /index.html 200\n');
// A mismatch must fail the build, not silently introduce another theme.
const source = await readFile(
  new URL('../../resources/vis-docs/assets/theme.css', import.meta.url),
);
const output = await readFile(new URL('assets/theme.css', dist));
if (!source.equals(output)) throw new Error('Documentation stylesheet differs');
