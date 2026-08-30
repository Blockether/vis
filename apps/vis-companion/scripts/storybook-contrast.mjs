/**
 * Prove every story against every shipped palette in a fresh document.
 *
 * axe caches flattened translucent paints within a document, so changing six
 * theme attributes around one mounted story can report a light paper beneath
 * dark-theme ink. A URL per story/palette is both faster than six builds and
 * faithful to how the app first paints a selected theme.
 */
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { createRequire } from 'node:module';
import { extname, resolve, sep } from 'node:path';
import { chromium } from 'playwright';

const require = createRequire(import.meta.url);
const axeSource = await readFile(require.resolve('axe-core/axe.min.js'), 'utf8');
const staticRoot = resolve(process.argv[2] ?? 'storybook-static');
const themes = [
  'blockether-light',
  'blockether-dark',
  'solarized-light',
  'solarized-dark',
  'vis-light',
  'vis-dark',
];
const mime = {
  '.css': 'text/css; charset=utf-8',
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.svg': 'image/svg+xml',
  '.woff2': 'font/woff2',
};

const server = createServer(async (request, response) => {
  try {
    const pathname = decodeURIComponent(new URL(request.url ?? '/', 'http://localhost').pathname);
    const file = resolve(staticRoot, pathname === '/' ? 'index.html' : `.${pathname}`);
    if (file !== staticRoot && !file.startsWith(`${staticRoot}${sep}`)) {
      response.writeHead(403).end();
      return;
    }
    const body = await readFile(file);
    response.writeHead(200, { 'content-type': mime[extname(file)] ?? 'application/octet-stream' });
    response.end(body);
  } catch {
    response.writeHead(404).end();
  }
});

await new Promise((resolveListen) => server.listen(0, '127.0.0.1', resolveListen));
const address = server.address();
if (typeof address === 'string' || address === null) throw new Error('Static Storybook did not bind a TCP port.');
const base = `http://127.0.0.1:${address.port}`;
const browser = await chromium.launch({ headless: true });

try {
  const index = await (await fetch(`${base}/index.json`)).json();
  const storyIds = Object.values(index.entries)
    .filter((entry) => entry.type === 'story')
    .map((entry) => entry.id)
    .sort();
  const jobs = storyIds.flatMap((id) => themes.map((theme) => ({ id, theme })));
  const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
  const failures = [];
  let cursor = 0;

  async function runJob(page, { id, theme }) {
    const query = new URLSearchParams({ id, globals: `theme:${theme}` });
    await page.goto(`${base}/iframe.html?${query}`, { waitUntil: 'networkidle', timeout: 30_000 });
    await page.waitForSelector('#storybook-root > *', { timeout: 15_000 });
    await page.evaluate(() => document.fonts?.ready);
    await page.addScriptTag({ content: axeSource });
    const violations = await page.evaluate(async () => {
      const result = await axe.run(document, {
        iframes: false,
        runOnly: { type: 'rule', values: ['color-contrast'] },
      });
      return result.violations.flatMap((violation) =>
        violation.nodes.map((node) => ({
          rule: violation.id,
          target: String(node.target),
          data: node.any[0]?.data ?? {},
        })),
      );
    });
    failures.push(...violations.map((violation) => ({ id, theme, ...violation })));
  }

  async function worker() {
    const page = await context.newPage();
    try {
      while (cursor < jobs.length) {
        const job = jobs[cursor];
        cursor += 1;
        try {
          await runJob(page, job);
        } catch (error) {
          failures.push({ ...job, rule: 'scanner-error', target: String(error), data: {} });
        }
      }
    } finally {
      await page.close();
    }
  }

  await Promise.all(Array.from({ length: 6 }, () => worker()));
  await context.close();

  if (failures.length > 0) {
    for (const failure of failures.slice(0, 100)) {
      console.error(
        `${failure.theme} · ${failure.id} · ${failure.rule} · ${failure.target} · ${JSON.stringify(failure.data)}`,
      );
    }
    if (failures.length > 100) console.error(`…and ${failures.length - 100} more.`);
    throw new Error(`${failures.length} Storybook contrast checks failed.`);
  }

  console.log(`${storyIds.length} stories × ${themes.length} themes: contrast clean.`);
} finally {
  await browser.close();
  await new Promise((resolveClose, rejectClose) =>
    server.close((error) => (error ? rejectClose(error) : resolveClose())),
  );
}
