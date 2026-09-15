import { expect, test } from 'vitest';
import { execFileSync } from 'node:child_process';
import { existsSync, readFileSync, readdirSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { JSDOM, requestInterceptor, VirtualConsole } from 'jsdom';
import { origin } from './web/discovery.js';

const prefix = '/python-sdk-api/';
const files = readdirSync('dist' + prefix, { recursive: true }).filter((name) =>
  name.endsWith('.html'),
);
const read = (path) => readFileSync('dist' + path, 'utf8');

test('pdoc documents public modules, re-exports and typed methods from the SDK checkout', () => {
  expect(files).toContain('index.html');
  expect(files).toContain('blockether/vis/extension.html');
  expect(files).toContain('blockether/vis/activity.html');
  expect(files).toContain('blockether/vis/views.html');
  expect(files.some((file) => /(^|\/)_[^/]+/.test(file))).toBe(false);
  const exports = JSON.parse(
    execFileSync(
      process.env.PYTHON || 'python3',
      [
        '-c',
        'import json; import blockether.vis.engine as engine; print(json.dumps(engine.__all__))',
      ],
      {
        env: {
          ...process.env,
          PYTHONPATH: fileURLToPath(new URL('../../packages/vis-agent/src/', import.meta.url)),
        },
        encoding: 'utf8',
      },
    ),
  );
  const dom = new JSDOM(read(prefix + 'blockether/vis/engine.html'));
  try {
    const document = dom.window.document;
    expect(exports).toContain('Subagent');
    for (const name of exports.filter((name) => name !== 'Subagent'))
      expect(document.getElementById(name), name).not.toBeNull();
    for (const name of [
      'Agent.run',
      'Agent.send',
      'LocalEngine',
      'GatewayClient',
      'ExecutionLayer',
    ])
      expect(document.getElementById(name), name).not.toBeNull();
    const agent = document.getElementById('Agent').textContent;
    expect(agent).toContain('execution_layer:');
    expect(agent).toContain('ExecutionLayer');
    expect(agent).toContain('extensions=()');
    expect(document.querySelector('.view-source-button')).toBeNull();
    expect(document.body.textContent).toContain('development API');
    expect(document.querySelector('meta[name="generator"]').content).toMatch(/^pdoc /);
  } finally {
    dom.window.close();
  }
});

test('experimental APIs stay out of generated pages, navigation and search', () => {
  const experimental =
    /\bSubagent\b|publish_spawn|subagents|council_wake|Council\.(?:cancel|route|wake)/i;
  for (const name of files) expect(experimental.test(read(prefix + name)), name).toBe(false);
  expect(experimental.test(read(prefix + 'search.js')), 'search index').toBe(false);
  const dom = new JSDOM(read(prefix + 'blockether/vis/extension.html'));
  try {
    expect(dom.window.document.getElementById('council')).toBeNull();
  } finally {
    dom.window.close();
  }
});

test('API headings have unique anchors without duplicating member definitions', () => {
  for (const name of files.filter((name) => name !== 'index.html')) {
    const dom = new JSDOM(read(prefix + name));
    try {
      const ids = [...dom.window.document.querySelectorAll('[id]')].map((node) => node.id);
      expect(ids.length, name).toBe(new Set(ids).size);
    } finally {
      dom.window.close();
    }
  }
});

test('generated API pages have discovery metadata and no inline executable content', () => {
  const sitemap = read('/sitemap-python-sdk.xml');
  const docs = new JSDOM(read('/index.html'));
  const favicons = [...docs.window.document.querySelectorAll('link[rel="icon"]')].map(
    (node) => node.outerHTML,
  );
  docs.window.close();
  for (const name of files.filter((name) => name !== 'index.html')) {
    const path = prefix + name;
    const dom = new JSDOM(read(path));
    try {
      const document = dom.window.document;
      expect(document.querySelectorAll('title')).toHaveLength(1);
      expect(document.title).toMatch(/ · Python SDK API · Vis · Blockether$/);
      expect(document.querySelector('link[rel="canonical"]').href).toBe(origin + path);
      expect(
        [...document.querySelectorAll('link[rel="icon"]')].map((node) => node.outerHTML),
      ).toEqual(favicons);
      expect(document.querySelector('meta[name="description"]').content).toContain(
        'development source',
      );
      expect(sitemap).toContain(`<loc>${origin}${path}</loc>`);
      expect(read('/llms.txt')).toContain(origin + path);
      expect(document.querySelectorAll('style, [style]')).toHaveLength(0);
      expect(
        document.querySelectorAll('script:not([src]):not([type="application/ld+json"])'),
      ).toHaveLength(0);
      expect(document.querySelectorAll('link[rel="stylesheet"]')).not.toHaveLength(0);
      for (const node of document.querySelectorAll('*')) {
        for (const attribute of node.attributes) {
          expect(attribute.name, path).not.toMatch(/^on/i);
          expect(attribute.value.trim(), path).not.toMatch(/^javascript:/i);
        }
      }
    } finally {
      dom.window.close();
    }
  }
  expect(sitemap).not.toContain('index.html');
  for (const path of ['../../README.md', '../../packages/vis-agent/README.md'])
    expect(readFileSync(path, 'utf8')).toContain(origin + prefix);
  expect(read('/python-sdk.html')).toContain(origin + prefix);
});

test('every local API link, anchor and asset resolves, including the landing redirect', () => {
  const documents = new Map();
  const getDocument = (path) => {
    if (!documents.has(path)) documents.set(path, new JSDOM(read(path), { url: origin + path }));
    return documents.get(path).window.document;
  };
  try {
    for (const name of files) {
      const path = prefix + name;
      const document = getDocument(path);
      const links = [...document.querySelectorAll('[href], [src]')].map(
        (node) => node.getAttribute('href') ?? node.getAttribute('src'),
      );
      const redirect = document.querySelector('meta[http-equiv="refresh"]');
      if (redirect) links.push(redirect.content.split('url=')[1]);
      for (const link of links) {
        const url = new URL(link, origin + path);
        if (url.origin !== origin) continue;
        const target = url.pathname.endsWith('/') ? url.pathname + 'index.html' : url.pathname;
        expect(existsSync('dist' + target), `${path} → ${link}`).toBe(true);
        if (url.hash && target.endsWith('.html'))
          expect(
            getDocument(target).getElementById(decodeURIComponent(url.hash.slice(1))),
            `${path} → ${link}`,
          ).not.toBeNull();
      }
    }
  } finally {
    for (const dom of documents.values()) dom.window.close();
  }
});

test.each(['blockether/vis.html', 'blockether/vis/engine.html'])(
  'externalized pdoc search works from %s without a remote dependency',
  async (name) => {
    const requested = [];
    const errors = [];
    const virtualConsole = new VirtualConsole();
    virtualConsole.on('jsdomError', (error) => errors.push(error.message));
    const dom = new JSDOM(read(prefix + name), {
      url: origin + prefix + name,
      runScripts: 'dangerously',
      resources: {
        interceptors: [
          requestInterceptor((request) => {
            const asset = new URL(request.url);
            expect(asset.origin).toBe(origin);
            requested.push(asset.pathname);
            return new Response(readFileSync('dist' + asset.pathname), {
              headers: {
                'Content-Type': asset.pathname.endsWith('.css')
                  ? 'text/css'
                  : 'application/javascript',
              },
            });
          }),
        ],
      },
      virtualConsole,
      beforeParse(window) {
        window.scrollTo = () => {};
      },
    });
    try {
      await new Promise((resolve) => dom.window.addEventListener('load', resolve, { once: true }));
      const document = dom.window.document;
      const input = document.querySelector('input[type="search"]');
      input.focus();
      input.value = 'LocalEngine';
      input.dispatchEvent(new dom.window.Event('input'));
      await expect
        .poll(() => document.querySelectorAll('.search-result').length)
        .toBeGreaterThan(0);
      expect(
        [...document.querySelectorAll('.search-result a')].some((link) =>
          link.href.endsWith('engine.html#LocalEngine'),
        ),
      ).toBe(true);
      expect(requested).toContain(prefix + 'search.js');
      input.value = 'zzzzzzzzzzzzzzq';
      input.dispatchEvent(new dom.window.Event('input'));
      expect(document.querySelector('main').textContent).toContain('No search results');
      input.value = '';
      input.dispatchEvent(new dom.window.Event('input'));
      expect(document.querySelector('.search-result')).toBeNull();
      expect(document.querySelector('main').textContent).toContain('blockether.vis');
      expect(errors).toEqual([]);
    } finally {
      dom.window.close();
    }
  },
);
