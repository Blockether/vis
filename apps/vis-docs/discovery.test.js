import { expect, test } from 'vitest';
import { readFileSync, readdirSync } from 'node:fs';
import { JSDOM } from 'jsdom';
import sharp from 'sharp';
import { origin, catalogMetadata, sitemap } from './web/discovery.js';
import { renderPage } from './web/render.js';
import items from './web/catalog.fixture.json';

const read = (name) => readFileSync('dist/' + name, 'utf8');
function xmlLocations(text) {
  const dom = new JSDOM(text, { contentType: 'application/xml' });
  try {
    return [...dom.window.document.querySelectorAll('loc')].map((node) => node.textContent);
  } finally {
    dom.window.close();
  }
}
test('every generated document has canonical metadata, an accessible icon and a Markdown alternate', () => {
  const pages = readdirSync('dist').filter((name) => name.endsWith('.html'));
  const locations = xmlLocations(read('sitemap-docs.xml'));
  const descriptions = new Set();
  for (const file of pages) {
    const dom = new JSDOM(read(file));
    try {
      const d = dom.window.document,
        canonical = origin + (file === 'index.html' ? '/' : '/' + file);
      expect(d.querySelectorAll('title')).toHaveLength(1);
      expect(d.querySelector('link[rel="canonical"]').href).toBe(canonical);
      expect(locations).toContain(canonical);
      const description = d.querySelector('meta[name="description"]').content;
      expect(description.length).toBeGreaterThanOrEqual(80);
      expect(description.length).toBeLessThanOrEqual(170);
      descriptions.add(description);
      expect(d.title).toMatch(/ · Vis · Blockether$/);
      expect(d.querySelector('link[rel="icon"][sizes="48x48"]').getAttribute('href')).toBe(
        '/vis-icon-48.png',
      );
      expect(d.querySelector('link[rel="icon"][type="image/x-icon"]').getAttribute('href')).toBe(
        '/vis-icon.ico',
      );
      expect(d.querySelector('a[href="https://blockether.com"]')).not.toBeNull();
      expect(d.querySelector('meta[property="og:url"]').content).toBe(canonical);
      expect(d.querySelector('meta[property="og:title"]').content).toBe(d.title);
      expect(d.querySelector('meta[name="twitter:description"]').content).toBe(description);
      const schema = JSON.parse(d.querySelector('script[type="application/ld+json"]').textContent);
      expect(schema.url).toBe(canonical);
      expect(schema['@type']).toBe('TechArticle');
      expect(schema.publisher).toEqual({
        '@type': 'Organization',
        name: 'Blockether',
        url: 'https://blockether.com/',
      });
      const md = file.replace('.html', '.md');
      expect(d.querySelector('link[type="text/markdown"]').getAttribute('href')).toBe('/' + md);
      expect(read('llms.txt')).toContain(origin + '/' + md);
      expect(read('llms-full.txt')).toContain(read(md));
      expect(d.querySelector('.center-link').getAttribute('aria-label')).toBe('Extension Center');
      expect(d.querySelector('.center-link svg')).not.toBeNull();
      expect(d.querySelector('.center-link').textContent).toBe('');
      expect(d.querySelector('.side a[href="/extensions/"]').textContent).toBe('Extension Center');
    } finally {
      dom.window.close();
    }
  }
  expect(locations).toHaveLength(pages.length);
  expect(new Set(locations).size).toBe(pages.length);
  expect(descriptions.size).toBe(pages.length);
  expect(xmlLocations(read('sitemap.xml'))).toEqual([
    origin + '/sitemap-docs.xml',
    origin + '/sitemap-python-sdk.xml',
    origin + '/extensions/sitemap.xml',
  ]);
  expect(read('robots.txt')).toContain('Sitemap: ' + origin + '/sitemap.xml');
  expect(read('llms.txt')).toContain(origin + '/extensions/llms.txt');
  expect(readdirSync('dist')).not.toContain('source.json');
});

test.each([16, 32, 48, 180, 192, 512])(
  'site icon %i reuses the transparent Companion artwork',
  async (size) => {
    const companion = new JSDOM(readFileSync('../vis-companion/index.html', 'utf8'));
    try {
      const href = companion.window.document.querySelector('link[rel="icon"]').getAttribute('href');
      const source = readFileSync('../vis-companion/public' + href);
      const name = `vis-icon-${size}.png`;
      const actual = readFileSync('dist/' + name);
      expect(await sharp(actual).metadata()).toMatchObject({
        width: size,
        height: size,
        hasAlpha: true,
      });
      // Safari can select the touch icon: every size must keep the app's transparency and framing.
      expect((await sharp(actual).stats()).isOpaque).toBe(false);
      const corner = await sharp(actual)
        .extract({ left: 0, top: 0, width: 1, height: 1 })
        .ensureAlpha()
        .raw()
        .toBuffer();
      expect(corner[3]).toBe(0);
      const expected = await sharp(source)
        .resize(size, size, { fit: 'contain', background: '#00000000' })
        .png()
        .toBuffer();
      expect(actual).toEqual(expected);
    } finally {
      companion.window.close();
    }
  },
);

test('the ICO embeds the transparent 32-bit favicon with a matching directory', async () => {
  const ico = readFileSync('dist/vis-icon.ico');
  expect(readFileSync('dist/favicon.ico')).toEqual(ico);
  expect(ico.readUInt16LE(2)).toBe(1);
  expect(ico.readUInt16LE(4)).toBe(1);
  expect(ico[6]).toBe(32);
  expect(ico[7]).toBe(32);
  expect(ico.readUInt16LE(12)).toBe(32);
  expect(ico.readUInt32LE(14)).toBe(ico.length - 22);
  expect(ico.readUInt32LE(18)).toBe(22);
  expect(ico.subarray(22)).toEqual(readFileSync('dist/vis-icon-32.png'));
  expect(await sharp(ico.subarray(22)).metadata()).toMatchObject({ channels: 4, hasAlpha: true });
});

test('the manifest points to the transparent app icons with matching sizes', () => {
  const manifest = JSON.parse(read('vis.webmanifest'));
  expect(manifest.icons).toEqual(
    [192, 512].map((size) => ({
      src: `/vis-icon-${size}.png`,
      sizes: `${size}x${size}`,
      type: 'image/png',
    })),
  );
});

test('docs, SDK and catalog pages replace every cached icon URL, including the touch icon', () => {
  for (const html of [
    read('motivation.html'),
    read('python-sdk-api/blockether/vis/engine.html'),
    renderPage({ items }),
    renderPage({ items, item: items[0] }),
  ]) {
    const dom = new JSDOM(html);
    try {
      expect(
        [
          ...dom.window.document.querySelectorAll(
            'link[rel="icon"], link[rel="apple-touch-icon"], link[rel="manifest"]',
          ),
        ].map((node) => node.getAttribute('href')),
      ).toEqual([
        '/vis-icon.ico',
        '/vis-icon-48.png',
        '/vis-icon-32.png',
        '/vis-icon-16.png',
        '/vis-icon-180.png',
        '/vis.webmanifest',
      ]);
    } finally {
      dom.window.close();
    }
  }
});

test('docs and catalog previews use a separate opaque social image with room around the logo', async () => {
  for (const html of [
    read('index.html'),
    renderPage({ items }),
    renderPage({ items, item: items[0] }),
  ]) {
    const dom = new JSDOM(html);
    try {
      const d = dom.window.document;
      expect(d.querySelector('meta[property="og:image"]').content).toBe(
        origin + '/assets/social-preview.png',
      );
      expect(d.querySelector('meta[property="og:image:type"]').content).toBe('image/png');
      expect(d.querySelector('meta[property="og:image:width"]').content).toBe('1200');
      expect(d.querySelector('meta[property="og:image:height"]').content).toBe('630');
      expect(d.querySelector('meta[name="twitter:image"]').content).toBe(
        origin + '/assets/social-preview.png',
      );
      expect(d.querySelector('meta[name="twitter:card"]').content).toBe('summary_large_image');
    } finally {
      dom.window.close();
    }
  }
  const image = sharp('dist/assets/social-preview.png');
  expect(await image.metadata()).toMatchObject({ width: 1200, height: 630 });
  expect((await image.stats()).isOpaque).toBe(true);
  const corner = await image
    .clone()
    .extract({ left: 0, top: 0, width: 1, height: 1 })
    .removeAlpha()
    .raw()
    .toBuffer();
  expect([...corner]).toEqual([255, 255, 255]);
  const { info } = await image.clone().trim().toBuffer({ resolveWithObject: true });
  expect(info.width).toBeGreaterThan(300);
  expect(info.width).toBeLessThanOrEqual(480);
  expect(info.height).toBeGreaterThan(300);
  expect(info.height).toBeLessThanOrEqual(480);
});
test('catalog SSR supplies item-specific metadata and never turns metadata into executable markup', () => {
  const item = {
    ...items[0],
    repository: 'Example/GitHub-Tools',
    owner: 'Example',
    name: 'Quoted "name"',
    description: 'Text <tag> & punctuation',
  };
  const dom = new JSDOM(renderPage({ items: [item], item }));
  try {
    const d = dom.window.document;
    expect(d.querySelector('meta[name="description"]').content).toBe(item.description);
    expect(d.querySelector('link[rel="canonical"]').href).toBe(
      origin + '/extensions/example/github-tools',
    );
    const schema = JSON.parse(d.querySelector('script[type="application/ld+json"]').textContent);
    expect(schema.description).toBe(item.description);
    expect(schema.publisher.url).toBe('https://blockether.com/');
    expect(schema.mainEntity).toMatchObject({
      '@type': 'SoftwareSourceCode',
      name: item.repository.toLowerCase(),
      codeRepository: item.repository_url,
      version: item.version,
      programmingLanguage: 'Python',
    });
    expect(d.title).toBe(item.repository.toLowerCase() + ' · Vis · Blockether');
    expect(d.querySelectorAll('head script')).toHaveLength(1);
    expect(d.querySelector('link[rel="icon"][sizes="48x48"]').getAttribute('href')).toBe(
      '/vis-icon-48.png',
    );
  } finally {
    dom.window.close();
  }
  expect(catalogMetadata({ error: 'Unavailable' })).toContain('noindex, follow');
  expect(catalogMetadata({ detailError: true })).toContain('noindex, follow');
  expect(catalogMetadata({ search: '?q=example' })).toContain('href="' + origin + '/extensions/"');
  expect(xmlLocations(sitemap(['/extensions/']))).toEqual([origin + '/extensions/']);
});
