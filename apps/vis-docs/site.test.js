import { expect, test } from 'vitest';
import { readFileSync, readdirSync, existsSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { JSDOM } from 'jsdom';
import { security } from './headers.js';
import { createTestHarness } from 'wrangler';

const htmlFiles = readdirSync('dist').filter((file) => file.endsWith('.html'));
test('the canonical renderer builds documentation, with one exact CSS and no inline executables', () => {
  expect(htmlFiles.length).toBeGreaterThan(10);
  expect(
    readFileSync('dist/assets/theme.css').equals(
      readFileSync('../../resources/vis-docs/assets/theme.css'),
    ),
  ).toBe(true);
  expect(
    readFileSync('dist/assets/prism.min.js').equals(
      readFileSync('../../resources/vis-transcript/prism.min.js'),
    ),
  ).toBe(true);
  expect(readFileSync('dist/assets/docs.js', 'utf8')).toContain('Prism.highlightAll()');
  for (const file of htmlFiles) {
    const dom = new JSDOM(readFileSync('dist/' + file, 'utf8'), {
      url: 'https://gateway.example.com/' + file,
    });
    try {
      const document = dom.window.document;
      expect(
        document.querySelector(
          'style, script:not([src]):not([type="application/ld+json"]), [onclick]',
        ),
        file,
      ).toBeNull();
      expect(document.querySelector('.top .center-link').getAttribute('href'), file).toBe(
        '/extensions/',
      );
      for (const node of document.querySelectorAll('a[href], link[href], script[src], img[src]')) {
        const url = new URL(
          node.getAttribute('href') || node.getAttribute('src'),
          dom.window.location.href,
        );
        if (url.origin !== 'https://gateway.example.com' || url.pathname.startsWith('/extensions/'))
          continue;
        expect(
          existsSync('dist' + (url.pathname === '/' ? '/index.html' : url.pathname)),
          file + ': ' + url.pathname,
        ).toBe(true);
      }
    } finally {
      dom.window.close();
    }
  }
});

test('Council diagrams are accessible, self-contained and match their Mermaid sources', () => {
  const font = readFileSync('../../resources/vis-docs/assets/fonts/jetbrains-mono.woff2').toString(
    'base64',
  );
  for (const name of ['council-messages', 'council-modules']) {
    const source = readFileSync(`dist/assets/diagrams/${name}.mmd`);
    const svg = readFileSync(`dist/assets/diagrams/${name}.svg`, 'utf8');
    expect(source).toEqual(readFileSync(`../../resources/vis-docs/assets/diagrams/${name}.mmd`));
    expect(svg).toBe(readFileSync(`../../resources/vis-docs/assets/diagrams/${name}.svg`, 'utf8'));
    const dom = new JSDOM(svg, {
      contentType: 'image/svg+xml',
      url: `https://gateway.example.com/assets/diagrams/${name}.svg`,
    });
    try {
      const image = dom.window.document.documentElement;
      expect(image.getAttribute('data-source-sha256')).toBe(
        createHash('sha256').update(source).digest('hex'),
      );
      for (const attribute of ['aria-labelledby', 'aria-describedby']) {
        const label = dom.window.document.getElementById(image.getAttribute(attribute));
        expect(label?.textContent.length).toBeGreaterThan(20);
      }
      expect(dom.window.document.querySelectorAll('script, foreignObject, image').length).toBe(0);
      expect(svg).toContain(`data:font/woff2;base64,${font}`);
      expect(svg).not.toMatch(/(?:href|src)="https?:/);
    } finally {
      dom.window.close();
    }
  }
});

test('docs shortcuts and catalog buttons use one shared control and font contract', () => {
  const dom = new JSDOM(
    '<style>' +
      readFileSync('dist/assets/theme.css', 'utf8') +
      '</style><style>' +
      readFileSync('dist/assets/catalog.css', 'utf8') +
      '</style>',
  );
  try {
    const rules = [...dom.window.document.styleSheets[0].cssRules];
    const control = rules.find(
      (rule) =>
        rule.selectorText
          ?.split(',')
          .map((s) => s.trim())
          .includes('button') && rule.selectorText.includes('.quick-links a'),
    );
    expect(control).toBeDefined();
    expect(control.style.getPropertyValue('font-size')).toBe('var(--text-small)');
    expect(control.style.getPropertyValue('font-weight')).toBe('500');
    expect(control.style.getPropertyValue('background')).toBe('var(--bg-soft)');
    const primary = rules.find((rule) => rule.selectorText === 'button.primary');
    expect(primary.style.getPropertyValue('background')).toBe('var(--fg)');
    expect(readFileSync('dist/assets/fonts/jetbrains-mono.woff2')).toEqual(
      readFileSync('../../resources/vis-docs/assets/fonts/jetbrains-mono.woff2'),
    );
    expect(readFileSync('dist/assets/catalog.css', 'utf8')).not.toMatch(
      /font-family|@font-face|button\.primary/,
    );
    const touch = rules.find((rule) => /pointer:\s*coarse/.test(rule.conditionText));
    expect(
      [...touch.cssRules]
        .find((rule) => rule.selectorText.includes('input'))
        .style.getPropertyValue('min-height'),
    ).toBe('2.75rem');
    const input = [...dom.window.document.styleSheets[1].cssRules].find((rule) =>
      rule.selectorText?.startsWith('input:not'),
    );
    expect(input.style.getPropertyValue('min-height')).toBe('');
  } finally {
    dom.window.close();
  }
});
test('phone submission sheets fill the dynamic viewport with a safe-area header and scrolling body', () => {
  const dom = new JSDOM(
    '<style>' +
      readFileSync('../../resources/vis-docs/assets/theme.css', 'utf8') +
      '</style><style>' +
      readFileSync('web/style.css', 'utf8') +
      '</style><dialog class="content"></dialog>',
  );
  try {
    const rules = [...dom.window.document.styleSheets[1].cssRules];
    expect(dom.window.getComputedStyle(dom.window.document.querySelector('dialog')).maxWidth).toBe(
      'none',
    );
    const phone = rules.find((rule) =>
      rule.conditionText?.includes('(pointer: coarse) and (max-height: 560px)'),
    );
    const sheet = [...phone.cssRules].find((rule) => rule.selectorText === 'dialog.content');
    for (const [name, value] of Object.entries({
      width: '100%',
      height: '100dvh',
      'max-width': 'none',
      'max-height': 'none',
      margin: '0px',
      border: '0px',
    }))
      expect(sheet.style.getPropertyValue(name), name).toBe(value);
    expect(
      rules.find((rule) => rule.selectorText === 'dialog[open]').style.getPropertyValue('display'),
    ).toBe('flex');
    expect(
      rules
        .find((rule) => rule.selectorText === '.dialog-body')
        .style.getPropertyValue('overflow-y'),
    ).toBe('auto');
    expect(
      rules.find((rule) => rule.selectorText === '.dialog-head').style.getPropertyValue('padding'),
    ).toContain('safe-area-inset-top');
    expect(
      rules.find((rule) => rule.selectorText === '.dialog-body').style.getPropertyValue('padding'),
    ).toContain('safe-area-inset-bottom');
  } finally {
    dom.window.close();
  }
});
test('the static upload contains only public output and the same security policy', () => {
  const output = readdirSync('dist', { recursive: true });
  expect(
    output.some((file) =>
      /(^|\/)(?:[^/]*fixture[^/]*|schema\.sql|wrangler[^/]*|\.?deployment[^/]*|package(?:-lock)?\.json|node_modules|\.env[^/]*|\.vars[^/]*)$/.test(
        file,
      ),
    ),
  ).toBe(false);
  const headers = readFileSync('dist/_headers', 'utf8');
  for (const [key, value] of Object.entries(security))
    expect(headers).toContain(`${key}: ${value}`);
  const config = JSON.parse(readFileSync('wrangler.jsonc', 'utf8'));
  expect(config.assets.run_worker_first).toEqual(['/extensions', '/extensions/*', '/api/*']);
  expect(config.assets.html_handling).toBe('none');
  expect(config.d1_databases[0].database_name).toBe('vis-extension-center');
});
test('the static site exports sharp store badges without allowing inline scripts', () => {
  for (const name of ['testflight', 'google-play']) {
    const badge = readFileSync(`dist/assets/install-${name}.png`);
    expect(badge).toEqual(readFileSync(`../../resources/vis-docs/assets/install-${name}.png`));
    expect([badge.readUInt32BE(16), badge.readUInt32BE(20)]).toEqual([672, 168]);
  }
  const policy = security['Content-Security-Policy'];
  expect(policy.match(/img-src([^;]*)/)[1]).toContain("'self'");
  for (const directive of ['script-src', 'style-src']) {
    const sources = policy.match(new RegExp(directive + '([^;]*)'))[1];
    expect(sources).not.toContain('data:');
    expect(sources).not.toContain('unsafe-inline');
  }
});
test('Wrangler serves the home page, HTML paths and assets with production routing and headers', async ({
  onTestFailed,
}) => {
  const site = createTestHarness({ workers: [{ configPath: './wrangler.jsonc' }] });
  onTestFailed(() => site.debug());
  try {
    await site.listen();
    for (const path of [
      '/',
      '/index.html',
      '/extending.html',
      '/assets/theme.css',
      '/assets/docs.js',
      '/assets/prism.min.js',
      '/assets/fonts/jetbrains-mono.woff2',
      '/assets/social-preview.png',
      '/favicon.ico',
      '/favicon-32.png',
      '/favicon-48.png',
      '/apple-touch-icon.png',
      '/site.webmanifest',
      '/robots.txt',
      '/sitemap.xml',
      '/sitemap-docs.xml',
      '/llms.txt',
      '/llms-full.txt',
      '/extending.md',
    ]) {
      const response = await site.fetch(path, { redirect: 'manual' });
      expect(response.status, path).toBe(200);
      expect(response.headers.get('Content-Security-Policy'), path).toBe(
        security['Content-Security-Policy'],
      );
      // Drain every response: leaving asset streams open can block the harness teardown.
      expect(
        Buffer.from(await response.arrayBuffer()).equals(
          readFileSync('dist' + (path === '/' ? '/index.html' : path)),
        ),
        path,
      ).toBe(true);
      if (path.endsWith('.xml'))
        expect(response.headers.get('content-type')).toMatch(/(?:application|text)\/xml/);
      if (path.endsWith('.txt'))
        expect(response.headers.get('content-type')).toContain('text/plain');
    }
    const head = await site.fetch('/', { method: 'HEAD' });
    expect(head.status).toBe(200);
    expect(await head.text()).toBe('');
    const missing = await site.fetch('/missing.html');
    expect(missing.status).toBe(404);
    await missing.arrayBuffer();
    // The isolated harness has no schema: catalog failure must not break static docs.
    const catalog = await site.fetch('/extensions/');
    expect(catalog.status).toBe(503);
    expect(await catalog.text()).toContain('id="catalog-data"');
    const home = await site.fetch('/');
    expect(home.status).toBe(200);
    expect(await home.text()).toBe(readFileSync('dist/index.html', 'utf8'));
  } finally {
    await site.close();
  }
});
