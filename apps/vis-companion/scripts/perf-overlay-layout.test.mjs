import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';
import { build } from 'vite';
import { expect, it } from 'vitest';

// Shared controls extend their touch targets beyond their visible faces.
async function touchTarget(locator) {
  return locator.evaluate((element) => {
    const box = element.getBoundingClientRect();
    const style = getComputedStyle(element);
    const reach = getComputedStyle(element, '::after');
    let { left, right, top, bottom } = box;
    const px = (value) => Number.parseFloat(value) || 0;
    if (reach.content !== 'none' && reach.content !== 'normal') {
      left = Math.min(left, box.left + px(style.borderLeftWidth) + px(reach.left));
      right = Math.max(right, box.right - px(style.borderRightWidth) - px(reach.right));
      top = Math.min(top, box.top + px(style.borderTopWidth) + px(reach.top));
      bottom = Math.max(bottom, box.bottom - px(style.borderBottomWidth) - px(reach.bottom));
    }
    const x = (left + right) / 2;
    const y = (top + bottom) / 2;
    const points = [[left + 1, y], [right - 1, y], [x, top + 1], [x, bottom - 1]];
    return {
      x: left, y: top, width: right - left, height: bottom - top,
      reachable: points.every(([x, y]) => element.contains(document.elementFromPoint(x, y))),
      framed: ['Top', 'Right', 'Bottom', 'Left'].some((side) =>
        px(style[`border${side}Width`]) > 0
        && style[`border${side}Style`] !== 'none'
        && style[`border${side}Color`] !== 'rgba(0, 0, 0, 0)'),
    };
  });
}

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
      { width: 852, height: 393, top: 0, bottom: 21, left: 62, right: 62, expectScroll: true },
      { width: 320, height: 568, top: 20, bottom: 0, left: 0, right: 0 },
      { width: 768, height: 1024, top: 24, bottom: 20, left: 0, right: 0 },
      { width: 1440, height: 900, top: 0, bottom: 0, left: 0, right: 0 },
    ];
    for (const { width, height, expectScroll = false, ...insets } of cases) {
      await page.setViewportSize({ width, height });
      await cdp.send('Emulation.setSafeAreaInsetsOverride', { insets });
      await page.goto('http://127.0.0.1/?perf=1');
      const panel = page.getByRole('region', { name: 'Memory overlay', exact: true });
      await panel.waitFor();
      const insideSafeArea = async (locator, control = false) => {
        const box = control ? await touchTarget(locator) : await locator.boundingBox();
        expect(box).not.toBeNull();
        expect(box.x).toBeGreaterThanOrEqual(insets.left + 8);
        expect(box.y).toBeGreaterThanOrEqual(insets.top + 8);
        expect(box.x + box.width).toBeLessThanOrEqual(width - insets.right - 8);
        expect(box.y + box.height).toBeLessThanOrEqual(height - insets.bottom - 8);
        if (control) {
          expect(box.height).toBeGreaterThanOrEqual(44);
          expect(box.width).toBeGreaterThanOrEqual(44);
          expect(box.reachable).toBe(true);
          expect(box.framed).toBe(false);
        }
      };
      const fillsSafeArea = async () => {
        const box = await panel.boundingBox();
        expect(box.x).toBeCloseTo(insets.left + 8, 1);
        expect(box.y).toBeCloseTo(insets.top + 8, 1);
        expect(box.width).toBeCloseTo(width - insets.left - insets.right - 16, 1);
        expect(box.height).toBeCloseTo(height - insets.top - insets.bottom - 16, 1);
      };
      await insideSafeArea(panel);
      await fillsSafeArea();
      const minimizeControl = panel.getByRole('button', { name: 'Minimize memory overlay' });
      const controlBox = await minimizeControl.boundingBox();
      const iconBox = await minimizeControl.locator('svg').boundingBox();
      const contentBox = await minimizeControl.evaluate((element) => {
        const boxes = [...element.children].map((child) => child.getBoundingClientRect());
        return { left: Math.min(...boxes.map((box) => box.left)), right: Math.max(...boxes.map((box) => box.right)) };
      });
      // Keep the icon and its label centered within the control, not against its left padding.
      expect(Math.abs((contentBox.left + contentBox.right) / 2 - controlBox.x - controlBox.width / 2)).toBeLessThanOrEqual(0.5);
      expect(Math.abs(iconBox.y + iconBox.height / 2 - controlBox.y - controlBox.height / 2)).toBeLessThanOrEqual(0.5);
      for (const button of await panel.getByRole('button').all()) {
        await insideSafeArea(button, true);
      }
      await panel.getByRole('button', { name: 'Show items' }).tap();
      expect(await panel.getByRole('button', { name: 'Show bytes' }).isVisible()).toBe(true);
      await panel.getByRole('button', { name: 'Set baseline' }).tap();
      const details = panel.getByRole('region', { name: 'Memory details' });
      await details.evaluate((element) => { element.scrollTop = element.scrollHeight; });
      if (expectScroll) expect(await details.evaluate((element) => element.scrollTop)).toBeGreaterThan(0);
      const minimize = panel.getByRole('button', { name: 'Minimize memory overlay' });
      await insideSafeArea(minimize, true);
      await minimize.tap();
      const summary = page.getByRole('button', { name: /^Memory .* listeners$/ });
      await summary.waitFor();
      await insideSafeArea(summary, true);
      expect((await page.locator('#vis-perf').boundingBox()).height).toBeLessThan(height / 2);
      await summary.tap();
      await panel.waitFor();
      await fillsSafeArea();
      expect(await panel.getByRole('heading', { name: 'Listeners added since the baseline' }).count()).toBe(1);
    }
  } finally {
    await browser.close();
  }
}, 60_000);
