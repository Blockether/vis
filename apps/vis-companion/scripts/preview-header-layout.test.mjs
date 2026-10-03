import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';
import { build } from 'vite';
import { expect, it } from 'vitest';

// Use the real preview components and production CSS without a gateway connection.
const fixture = `
import { useState } from 'react';
import { createRoot } from 'react-dom/client';
import { ArtifactsSheet } from './components/ArtifactsSheet';
import { OverlayScreen } from './components/DocArtifact';
import { ImageViewer } from './components/ImageViewer';
import { DownloadIcon } from './components/icons';
import { BandButton, Text } from './components/ui';
import { STORY_PICTURES } from './dev/story-data';
import type { GatewayClient } from './lib/gateway';
import './index.css';

const query = new URLSearchParams(location.search);
const name = query.get('name')!;
const kind = query.get('kind');
const artifact = {
  key: 'i1:0', kind: 'file' as const, name, media: 'BIN',
  mediaType: 'application/octet-stream', size: 1, sizeLabel: '1 B',
  turn: 1, iterationId: 'i1', index: 0, version: 1,
};
const client = {
  attachmentUrl: async () => 'data:application/octet-stream;base64,AA==',
  attachmentBlob: async () => new Blob(['0']),
  retainAttachment: () => () => {},
} as unknown as GatewayClient;
function Preview() {
  const [closed, setClosed] = useState(false);
  const onClose = () => setClosed(true);
  return <>
    <Text as="h3" variant="heading" className="sr-only">Heading reference</Text>
    {closed ? <p>Preview closed</p> : kind === 'image' ? (
      <ImageViewer src={STORY_PICTURES[0].src} name={name} onClose={onClose} />
    ) : kind === 'artifact' ? (
      <ArtifactsSheet client={client} sid="preview" artifacts={[artifact]} initialArtifact={artifact} onClose={onClose} />
    ) : (
      <OverlayScreen title={name} subtitle="12 rows · 2 columns" actions={<BandButton label="Download"><DownloadIcon /></BandButton>} onClose={onClose}>
        <p>Document preview</p>
      </OverlayScreen>
    )}
  </>;
}
createRoot(document.getElementById('root')!).render(<Preview />);
`;

// A long filename must not turn the phone's preview header into a multi-line banner.
it.runIf(process.env.CI)('keeps image and artifact headers compact with long names', async () => {
  const { output } = await build({
    root: fileURLToPath(new URL('..', import.meta.url)),
    logLevel: 'error',
    plugins: [{
      name: 'preview-header-fixture',
      enforce: 'pre',
      transform(code, id) {
        return id.endsWith('/src/main.tsx') ? { code: fixture, map: null } : undefined;
      },
    }],
    build: { write: false },
  });
  const browser = await chromium.launch();
  try {
    for (const viewport of [{ width: 320, height: 568 }, { width: 393, height: 852 }, { width: 852, height: 393 }]) {
      const context = await browser.newContext({ viewport, isMobile: true, hasTouch: true });
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
      await cdp.send('Emulation.setSafeAreaInsetsOverride', { insets: { top: viewport.width < viewport.height ? 47 : 0 } });
      const rows = [];
      for (const kind of ['image', 'document', 'artifact']) {
        const heights = [];
        for (const name of ['report.png', 'vis-memory-overlay-full-size-scrolled-upright.png']) {
          await page.goto('http://127.0.0.1/?' + new URLSearchParams({ kind, name }));
          const title = page.getByRole('heading', { name, exact: true });
          await title.waitFor();
          if (kind === 'artifact') await page.getByText('BIN · 1 B · ready to share').waitFor();
          const header = title.locator('xpath=ancestor::header');
          const close = header.getByRole('button', { name: 'Close ' + name, exact: true });
          const metrics = await title.evaluate((element) => {
            const heading = getComputedStyle(element);
            const header = element.closest('header');
            const band = getComputedStyle(header);
            const reference = getComputedStyle(document.querySelector('h3'));
            return {
              height: header.getBoundingClientRect().height - parseFloat(band.paddingTop) - parseFloat(band.borderTopWidth),
              lineHeight: parseFloat(heading.lineHeight),
              titleHeight: element.getBoundingClientRect().height,
              font: heading.font,
              size: heading.fontSize,
              referenceSize: reference.fontSize,
            };
          });
          expect(metrics.titleHeight).toBeLessThanOrEqual(metrics.lineHeight);
          expect(metrics.size).toBe(metrics.referenceSize);
          expect(await title.getAttribute('title')).toBe(name);
          const closeBox = await close.boundingBox();
          expect(closeBox.width).toBeGreaterThanOrEqual(44);
          expect(closeBox.height).toBeGreaterThanOrEqual(44);
          const titleBox = await title.boundingBox();
          expect(titleBox.x + titleBox.width).toBeLessThanOrEqual(closeBox.x);
          heights.push(metrics.height);
          rows.push(metrics);
          await close.tap();
          if (kind === 'artifact') {
            expect(await page.getByRole('dialog', { name, exact: true }).count()).toBe(0);
          } else {
            await page.getByText('Preview closed', { exact: true }).waitFor();
          }
        }
        expect(heights[1]).toBeCloseTo(heights[0], 1);
      }
      expect(new Set(rows.map((row) => row.font)).size).toBe(1);
      expect(new Set(rows.map((row) => row.height)).size).toBe(1);
      await context.close();
    }
  } finally {
    await browser.close();
  }
}, 60_000);
