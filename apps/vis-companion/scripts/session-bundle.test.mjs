import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';
import { build } from 'vite';
import { expect, it } from 'vitest';

// Node and Vite's development optimizer initialize CommonJS differently from the
// shipped bundle. Open its lazy session chunk in a fresh browser, not jsdom.
it('loads the production session bundle on a cold mobile launch', async () => {
  const { output } = await build({
    root: fileURLToPath(new URL('..', import.meta.url)),
    logLevel: 'error',
    build: { write: false },
  });
  const sessionChunk = output.find(
    (entry) => entry.type === 'chunk' && entry.facadeModuleId?.endsWith('/SessionScreen.tsx'),
  );
  expect(sessionChunk).toBeDefined();
  const chunksWith = (path) =>
    output
      .filter((entry) => entry.type === 'chunk' && entry.moduleIds.some((id) => id.includes(path)))
      .map((chunk) => chunk.fileName);
  // Justice ships inside the prose bundle, not as a separately loaded script.
  const prose = chunksWith('/src/components/JustifiedProse.tsx');
  expect(prose).toHaveLength(1);
  expect(chunksWith('/@kitlangton/justice/')).toEqual(prose);
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({ viewport: { width: 390, height: 844 }, isMobile: true });
    const errors = [];
    page.on('pageerror', (error) => errors.push(error.message));
    await page.route('http://127.0.0.1/**', async (route) => {
      const pathname = new URL(route.request().url()).pathname.slice(1) || 'index.html';
      const asset = output.find((entry) => entry.fileName === pathname);
      if (!asset) return route.fulfill({ status: 404, body: '' });
      await route.fulfill({
        contentType: pathname.endsWith('.js')
          ? 'text/javascript'
          : pathname.endsWith('.css')
            ? 'text/css'
            : pathname.endsWith('.html')
              ? 'text/html'
              : 'application/octet-stream',
        body: asset.type === 'chunk' ? asset.code : Buffer.from(asset.source),
      });
    });
    await page.goto('http://127.0.0.1/');
    const importSession = () =>
      page.evaluate(`(async () => {
      try {
        await import(${JSON.stringify(`/${sessionChunk.fileName}`)});
        return { loaded: true };
      } catch (error) {
        return { loaded: false, error: String(error) };
      }
    })()`);
    expect(await importSession()).toEqual({ loaded: true });
    const highlighted = await page.evaluate(() => {
      const examples = {
        bash: 'echo "$HOME"',
        clojure: '(def answer 42)',
        css: 'body { color: red; }',
        diff: '+added',
        java: 'class Example {}',
        javascript: 'const answer = 42;',
        json: '{"answer": 42}',
        markdown: '**answer**',
        python: 'def answer(): return 42',
        rust: 'fn main() {}',
        typescript: 'const answer: number = 42;',
        jsx: '<Answer value={42} />',
        tsx: '<Answer value={42 as number} />',
        yaml: 'answer: 42',
      };
      return Object.entries(examples).map(([language, code]) => ({
        language,
        highlighted: window.Prism.highlight(
          code,
          window.Prism.languages[language],
          language,
        ).includes('class="token '),
      }));
    });
    expect(highlighted).toHaveLength(14);
    for (const { language, highlighted: hasTokens } of highlighted) {
      expect(hasTokens, language).toBe(true);
    }
    // #282 follow-up: eager prose loading must retain the native-text fallback.
    // Justice constructs its segmenter at module evaluation, not on prepare().
    await page.addInitScript(() => {
      Object.defineProperty(Intl, 'Segmenter', { value: undefined });
    });
    await page.reload();
    expect(await page.evaluate(() => typeof Intl.Segmenter)).toBe('undefined');
    expect(await importSession()).toEqual({ loaded: true });
    expect(errors).toEqual([]);
  } finally {
    await browser.close();
  }
}, 120_000);
