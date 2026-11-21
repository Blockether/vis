import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';
import { build } from 'vite';
import { beforeAll, expect, it } from 'vitest';

// The desk row of a session: the session list, then the transcript with the side panel
// that live views and sidebar apps share. No gateway and no stored user data.
const fixture = `
import { createRoot } from 'react-dom/client';
import { ResizablePanel } from './components/LivePanel';
import './index.css';

createRoot(document.getElementById('root')!).render(
  <main className="flex h-dvh w-full overflow-x-hidden">
    <div data-testid="list" className="shrink-0" style={{ width: 400 }} />
    <div className="h-full min-h-0 min-w-0 flex-1">
      <div className="flex h-full min-h-0 w-full">
        <section data-testid="transcript" className="h-full min-h-0 min-w-0 flex-1" />
        <ResizablePanel>Review</ResizablePanel>
      </div>
    </div>
  </main>,
);
`;

let output;
beforeAll(async () => {
  if (!process.env.CI) return;
  ({ output } = await build({
    root: fileURLToPath(new URL('..', import.meta.url)),
    logLevel: 'error',
    plugins: [{
      name: 'side-panel-fixture',
      enforce: 'pre',
      transform(code, id) {
        return id.endsWith('/src/main.tsx') ? { code: fixture, map: null } : undefined;
      },
    }],
    build: { write: false },
  }));
}, 60_000);

async function layout(page) {
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
  return page.evaluate(() => {
    const main = document.querySelector('main');
    const panel = document.querySelector('[role="separator"]').parentElement.getBoundingClientRect();
    return {
      listRight: document.querySelector('[data-testid="list"]').getBoundingClientRect().right,
      transcript: document.querySelector('[data-testid="transcript"]').getBoundingClientRect().width,
      panelLeft: panel.left,
      panelRight: panel.right,
      panelWidth: panel.width,
      hidden: main.scrollWidth - main.clientWidth,
      chosen: localStorage.getItem('vis.live-panel.chosen-width'),
    };
  });
}

// User report 2026-11-22: the review panel grew until the transcript was gone, and then
// it stood over the session list.
it.runIf(process.env.CI)('keeps the side panel beside the session list and the transcript', async () => {
  const browser = await chromium.launch();
  try {
    const page = await browser.newPage({ viewport: { width: 1200, height: 800 } });
    await page.route('http://127.0.0.1/**', async route => {
      const pathname = new URL(route.request().url()).pathname.slice(1) || 'index.html';
      const asset = output.find(entry => entry.fileName === pathname);
      if (!asset) return route.fulfill({ status: 404, body: '' });
      await route.fulfill({
        contentType: pathname.endsWith('.js') ? 'text/javascript'
          : pathname.endsWith('.css') ? 'text/css'
            : pathname.endsWith('.html') ? 'text/html' : 'application/octet-stream',
        body: asset.type === 'chunk' ? asset.code : Buffer.from(asset.source),
      });
    });
    await page.goto('http://127.0.0.1/');
    const edge = page.getByRole('separator', { name: 'Resize live views' });
    const grip = await edge.boundingBox();
    await page.mouse.move(grip.x + grip.width / 2, 400);
    await page.mouse.down();
    await page.mouse.move(5, 400, { steps: 5 });
    await page.mouse.up();

    const wide = await layout(page);
    expect(wide.transcript, 'the transcript keeps the width of a phone').toBeGreaterThanOrEqual(320);
    expect(wide.panelRight).toBeLessThanOrEqual(1200);
    expect(wide.hidden, 'nothing stands outside the window').toBeLessThanOrEqual(0);
    expect(wide.chosen, 'the kept width is the width on the screen').toBe(String(wide.panelWidth));

    // A smaller window takes the room back, and a larger window gives the chosen width back.
    await page.setViewportSize({ width: 900, height: 800 });
    const narrow = await layout(page);
    expect(narrow.panelLeft).toBeGreaterThanOrEqual(narrow.listRight);
    expect(narrow.panelRight).toBeLessThanOrEqual(900);
    expect(narrow.hidden).toBeLessThanOrEqual(0);
    await page.setViewportSize({ width: 1200, height: 800 });
    expect((await layout(page)).panelWidth).toBe(wide.panelWidth);
  } finally {
    await browser.close();
  }
}, 60_000);
