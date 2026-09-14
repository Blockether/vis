/**
 * Prove every story against every shipped palette in a fresh document.
 *
 * axe caches flattened translucent paints within a document, so changing every
 * theme attribute around one mounted story can report a light paper beneath
 * dark-theme ink. A URL per story/palette is both faster than repeated builds and
 * faithful to how the app first paints a selected theme.
 */
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { createRequire } from 'node:module';
import { extname, resolve, sep } from 'node:path';
import { chromium } from 'playwright';

const require = createRequire(import.meta.url);
const [axeSource, themeCatalog] = await Promise.all([
  readFile(require.resolve('axe-core/axe.min.js'), 'utf8'),
  readFile(resolve('src/lib/themes.generated.ts'), 'utf8'),
]);
const staticRoot = resolve(process.argv[2] ?? 'storybook-static');
const themes = [...themeCatalog.matchAll(/\bid: '([^']+)'/g)].map((match) => match[1]);
if (themes.length === 0) throw new Error('The generated application theme catalog is empty.');
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
    response.writeHead(200, {
      'content-type': mime[extname(file)] ?? 'application/octet-stream',
    });
    response.end(body);
  } catch {
    response.writeHead(404).end();
  }
});

await new Promise((resolveListen) => server.listen(0, '127.0.0.1', resolveListen));
const address = server.address();
if (typeof address === 'string' || address === null)
  throw new Error('Static Storybook did not bind a TCP port.');
const base = `http://127.0.0.1:${address.port}`;
const browser = await chromium.launch({ headless: true });

try {
  const index = await (await fetch(`${base}/index.json`)).json();
  const storyIds = Object.values(index.entries)
    .filter((entry) => entry.type === 'story')
    .map((entry) => entry.id)
    .sort();
  const hoverStories = new Set([
    'session-jump-to-latest--default',
    ...storyIds.filter((id) => id.startsWith('vocabulary-controls--')),
    'components-artifacts-sheet--files',
    'components-data-table--opened',
    'components-live-view--finished-jobs',
    'components-iteration-trace--code-with-result',
    'components-menu--verbs',
    'components-menu--danger-focused',
    'components-manage-projects-sheet--browsing-pointer',
    'screens-session-list--desktop-hover',
  ]);
  for (const id of hoverStories) {
    if (!storyIds.includes(id)) throw new Error(`Missing hover regression story: ${id}`);
  }
  const jobs = storyIds.flatMap((id) =>
    themes.flatMap((theme) =>
      hoverStories.has(id)
        ? [
            { id, theme },
            { id, theme, hover: true },
          ]
        : [{ id, theme }],
    ),
  );
  const context = await browser.newContext({
    viewport: { width: 1280, height: 800 },
    reducedMotion: 'reduce',
  });
  const failures = [];
  let cursor = 0;
  let hoverChecks = 0;

  async function runJob(page, { id, theme, hover }) {
    const query = new URLSearchParams({ id, globals: `theme:${theme}` });
    await page.goto(`${base}/iframe.html?${query}`, {
      waitUntil: 'networkidle',
      timeout: 30_000,
    });
    await page.waitForSelector('#storybook-root > *', { timeout: 15_000 });
    await page.evaluate(() => document.fonts?.ready);
    await page.waitForFunction(
      () => window.__STORYBOOK_PREVIEW__?.currentRender?.phase === 'finished',
    );
    // Real pointer regression: DOM userEvent.hover does not activate CSS :hover.
    // Include ancestors and children: a list/group must not restore a hover slab
    // behind a frameless plus. Selected, disabled and inverse controls are included.
    if (hover) {
      const controls = page.locator('button, a[href], [role="button"], summary, tbody tr');
      for (const control of await controls.all()) {
        if (!(await control.isVisible())) continue;
        await control.scrollIntoViewIfNeeded();
        await page.mouse.move(0, 0);
        // A modal can leave mounted controls behind its backdrop. They are not
        // pointer targets until it closes; never force a hover through the overlay.
        const isExposed = await control.evaluate((element) => {
          const box = element.getBoundingClientRect();
          const hit = document.elementFromPoint(box.x + box.width / 2, box.y + box.height / 2);
          return hit === element || element.contains(hit);
        });
        if (!isExposed) continue;
        const before = await control.evaluate(readSurfaces);
        await control.hover();
        const after = await control.evaluate(readSurfaces);
        if (JSON.stringify(before) !== JSON.stringify(after)) {
          failures.push({
            id,
            theme,
            rule: 'hover-surface',
            target: await control.evaluate(
              (element) => element.getAttribute('aria-label') || element.textContent?.trim(),
            ),
            data: { before, after },
          });
        }
        hoverChecks += 1;
        await audit(page, id, theme);
      }
      return;
    }
    await audit(page, id, theme);
  }

  function readSurfaces(control) {
    const elements = [control, ...control.querySelectorAll('*')];
    for (let parent = control.parentElement; parent; parent = parent.parentElement) {
      elements.push(parent);
    }
    return elements
      .filter((element) => !element.closest('svg'))
      .map((element) => {
        const style = getComputedStyle(element);
        return {
          background: style.backgroundColor,
          image: style.backgroundImage,
          shadow: style.boxShadow,
          disabledInk: element.matches(':disabled') ? style.color : null,
          radius: style.borderRadius,
          borders: ['Top', 'Right', 'Bottom', 'Left'].map((side) => [
            style[`border${side}Width`],
            parseFloat(style[`border${side}Width`]) > 0 ? style[`border${side}Color`] : null,
          ]),
          width: element.getBoundingClientRect().width,
          height: element.getBoundingClientRect().height,
        };
      });
  }

  async function audit(page, id, theme) {
    const violations = await page.evaluate(async (source) => {
      // Audit every rendered icon-only action, including portal and artifact controls.
      // SVG strokes express the glyph itself; only HTML enclosures are forbidden.
      const frames = [];
      for (const button of document.querySelectorAll('button, a, [role="button"]')) {
        if (!button.querySelector('svg') || button.innerText.trim() || !button.checkVisibility())
          continue;
        for (const element of [button, ...button.querySelectorAll('*')]) {
          if (element.closest('svg')) continue;
          const style = getComputedStyle(element);
          const borders = ['Top', 'Right', 'Bottom', 'Left'].map((side) =>
            parseFloat(style[`border${side}Width`]),
          );
          if (borders.some((width) => width > 0) || parseFloat(style.borderRadius) > 0) {
            frames.push({
              rule: 'icon-frame',
              target: button.getAttribute('aria-label') || button.getAttribute('title'),
              data: { borders, radius: style.borderRadius },
            });
          }
        }
      }
      // Keep the scanner independent of Storybook's concurrent a11y addon.
      const previousAxe = window.axe;
      const module = { exports: {} };
      try {
        new Function('module', source)(module);
      } finally {
        window.axe = previousAxe;
      }
      const result = await module.exports.run(document, {
        iframes: false,
        runOnly: { type: 'rule', values: ['color-contrast'] },
      });
      return frames.concat(
        result.violations.flatMap((violation) =>
          violation.nodes.map((node) => ({
            rule: violation.id,
            target: String(node.target),
            data: node.any[0]?.data ?? {},
          })),
        ),
      );
    }, axeSource);
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
          failures.push({
            ...job,
            rule: 'scanner-error',
            target: String(error),
            data: {},
          });
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
    throw new Error(`${failures.length} Storybook contrast/icon-frame/hover checks failed.`);
  }

  console.log(
    `${storyIds.length} stories × ${themes.length} themes: contrast and icon frames clean; ${hoverChecks} desktop hover checks clean.`,
  );
} finally {
  await browser.close();
  await new Promise((resolveClose, rejectClose) =>
    server.close((error) => (error ? rejectClose(error) : resolveClose())),
  );
}
