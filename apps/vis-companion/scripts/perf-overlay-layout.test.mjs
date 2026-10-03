import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';
import { build } from 'vite';
import { expect, it } from 'vitest';

// The overlay mounts outside the app shell, so only its own layout clears the notch.
// Run the production bundle in Chromium with real CSS safe-area values, not jsdom.
it.runIf(process.env.CI)('keeps memory controls tappable inside every safe area', async () => {
  const { output } = await build({
    root: fileURLToPath(new URL('..', import.meta.url)),
    logLevel: 'error',
    build: { write: false },
  });
  const browser = await chromium.launch();
  try {
    const context = await browser.newContext({ isMobile: true, hasTouch: true });
    const page = await context.newPage();
    await page.route('http://127.0.0.1/**', async (route) => {
      const pathname = new URL(route.request().url()).pathname.slice(1) || 'index.html';
      const asset = output.find((entry) => entry.fileName === pathname);
      if (!asset) return route.fulfill({ status: 404, body: '' });
      await route.fulfill({
        contentType: pathname.endsWith('.js') ? 'text/javascript'
          : pathname.endsWith('.css') ? 'text/css'
            : pathname.endsWith('.html') ? 'text/html' : 'application/octet-stream',
        body: asset.type === 'chunk' ? asset.code : Buffer.from(asset.source),
      });
    });
    const cdp = await context.newCDPSession(page);
    const cases = [
      { width: 393, height: 852, top: 62, bottom: 34, left: 0, right: 0 },
      { width: 852, height: 393, top: 0, bottom: 21, left: 62, right: 62 },
      { width: 320, height: 568, top: 20, bottom: 0, left: 0, right: 0 },
      { width: 768, height: 1024, top: 24, bottom: 20, left: 0, right: 0 },
    ];
    for (const { width, height, ...insets } of cases) {
      await page.setViewportSize({ width, height });
      await cdp.send('Emulation.setSafeAreaInsetsOverride', { insets });
      await page.goto('http://127.0.0.1/?perf=1');
      const panel = page.getByRole('region', { name: 'Memory overlay', exact: true });
      await panel.waitFor();
      const insideSafeArea = async (locator) => {
        const box = await locator.boundingBox();
        expect(box).not.toBeNull();
        expect(box.x).toBeGreaterThanOrEqual(insets.left + 8);
        expect(box.y).toBeGreaterThanOrEqual(insets.top + 8);
        expect(box.x + box.width).toBeLessThanOrEqual(width - insets.right - 8);
        expect(box.y + box.height).toBeLessThanOrEqual(height - insets.bottom - 8);
      };
      await insideSafeArea(panel);
      for (const button of await panel.getByRole('button').all()) {
        await insideSafeArea(button);
        const box = await button.boundingBox();
        expect(box.height).toBeGreaterThanOrEqual(44);
        expect(box.width).toBeGreaterThanOrEqual(44);
      }
      await panel.getByRole('button', { name: 'Show items' }).tap();
      expect(await panel.getByRole('button', { name: 'Show bytes' }).isVisible()).toBe(true);
      await panel.getByRole('button', { name: 'Set baseline' }).tap();
      const details = panel.getByRole('region', { name: 'Memory details' });
      await details.evaluate((element) => { element.scrollTop = element.scrollHeight; });
      if (width > height) expect(await details.evaluate((element) => element.scrollTop)).toBeGreaterThan(0);
      const minimize = panel.getByRole('button', { name: 'Minimize memory overlay' });
      await insideSafeArea(minimize);
      await minimize.tap();
      const summary = page.getByRole('button', { name: /^Memory .* listeners$/ });
      await summary.waitFor();
      await insideSafeArea(summary);
      expect((await summary.boundingBox()).height).toBeGreaterThanOrEqual(44);
      await summary.tap();
      await panel.waitFor();
      expect(await panel.getByRole('heading', { name: 'Listeners added since the baseline' }).count()).toBe(1);
    }
  } finally {
    await browser.close();
  }
}, 60_000);
