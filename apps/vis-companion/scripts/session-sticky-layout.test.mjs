import { fileURLToPath } from 'node:url';
import { chromium, devices, webkit } from 'playwright';
import { build } from 'vite';
import { beforeAll, expect, it } from 'vitest';

// Exercise the production list and CSS without a gateway or stored user data.
const fixture = `
import { createRoot } from 'react-dom/client';
import { ProjectGroup } from './screens/sessions/SessionProjectGroups';
import { STORY_FLEET_CONNS, STORY_NEWER_PROJECT, STORY_PROJECT_CLIENT, storyFleetFetch } from './dev/story-data';
import { machineKey } from './lib/fleet';
import { projectFoldKey, writeProjectFold } from './lib/project-fold';
import { applyTheme, resolveTheme } from './lib/theme';
import './index.css';

const conn = STORY_FLEET_CONNS[0];
const base = STORY_NEWER_PROJECT;
const groups = ['Wallet work', 'Receipts'].map((name, index) => ({
  id: 'sticky-group-' + index, project_id: base.projectId, name,
  color: index ? 'amber' : 'blue', position: index, session_count: 18,
}));
const rows = Array.from({ length: 54 }, (_, index) => ({
  ...base.rows[3], id: 'sticky-row-' + index, title: 'Session ' + index,
  group_id: index < 36 ? groups[Math.floor(index / 18)].id : null,
}));
const projects = [
  { ...base, rows, groups },
  { ...base, root: '/After', name: 'After', projectId: 'after', groups: [],
    rows: rows.slice(36).map(row => ({ ...row, id: 'after-' + row.id, project_root: '/After', project_id: 'after' })) },
];
globalThis.fetch = storyFleetFetch(projects);
applyTheme(resolveTheme('blockether-light'));
const noop = () => {};
const context = {
  getClient: () => STORY_PROJECT_CLIENT, drafts: {}, previewId: null, preview: null, needle: '', openRow: null,
  actions: {
    commands: { open: noop, rename: async () => {}, requestDelete: noop, toggleStar: noop },
    deletion: { target: null, isBusy: false, error: null, confirm: noop, cancel: noop },
  },
};
for (const project of projects) writeProjectFold(projectFoldKey(machineKey(conn), project.root), true);
createRoot(document.getElementById('root')!).render(
  <div className="flex h-dvh flex-col bg-page">
    <div style={{ height: 48, flexShrink: 0 }} aria-label="Session toolbar">Sessions</div>
    <div className="min-h-0 flex-1 overflow-y-auto" data-testid="scroll-pane">
      {projects.map(project => <ProjectGroup key={project.root}
        group={{ root: project.root, label: project.name, projectId: project.projectId,
          tally: { count: 1934, live: 2, awaiting: 0, unread: 28 }, sessions: project.rows }}
        machine={{ conn, sessions: project.rows }} context={context}
        reading={{ pageSize: 20, isVisible: true }}
        creation={{ state: null, start: async () => {} }} initiallyOpen />)}
    </div>
  </div>,
);
`;

let output;
beforeAll(async () => {
  if (!process.env.CI) return;
  ({ output } = await build({
    root: fileURLToPath(new URL('..', import.meta.url)),
    logLevel: 'error',
    plugins: [{
      name: 'session-sticky-fixture',
      enforce: 'pre',
      transform(code, id) {
        return id.endsWith('/src/main.tsx') ? { code: fixture, map: null } : undefined;
      },
    }],
    build: { write: false },
  }));
}, 60_000);

async function frame(page) {
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
}

async function scrollTo(page, locator, offset) {
  await locator.evaluate((element, offset) => {
    const pane = document.querySelector('[data-testid="scroll-pane"]');
    pane.scrollTop += element.getBoundingClientRect().top - pane.getBoundingClientRect().top - offset;
  }, offset);
  await frame(page);
}

async function box(locator) {
  return locator.evaluate(element => {
    const rect = element.getBoundingClientRect();
    return { top: rect.top, bottom: rect.bottom, height: rect.height };
  });
}

// Regression: keep project → Groups → current group together throughout a phone scroll.
for (const [engine, device] of [[chromium, 'Pixel 7'], [webkit, 'iPhone 13']]) {
  it.runIf(process.env.CI)(`stacks mobile session headings without gaps in ${engine.name()}`, async () => {
    const browser = await engine.launch();
    try {
      const context = await browser.newContext({ ...devices[device] });
      const page = await context.newPage();
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
      const project = page.locator('[data-project-root="/CryptoSafe"]');
      const header = project.locator('header');
      const groups = project.getByText('Groups', { exact: true }).locator('..');
      const sessions = project.getByText('Sessions', { exact: true }).locator('..');
      await project.locator('[data-session-id="sticky-row-53"]').waitFor();
      await page.evaluate(() => document.fonts.ready);

      for (const viewport of [{ width: 393, height: 852 }, { width: 320, height: 568 }, { width: 852, height: 393 }]) {
        await page.setViewportSize(viewport);
        await frame(page);
        for (const [name, rowId] of [['Wallet work', 5], ['Receipts', 23]]) {
          await scrollTo(page, project.locator(`[data-session-id="sticky-row-${rowId}"]`), 220);
          const projectBox = await box(header);
          const groupsBox = await box(groups);
          const groupBox = await box(project.getByRole('button', { name: 'Collapse ' + name, exact: true }).locator('..'));
          expect(projectBox.top).toBe(48);
          expect(Math.abs(groupsBox.top - projectBox.bottom), 'project / Groups gap').toBeLessThanOrEqual(1);
          expect(Math.abs(groupBox.top - groupsBox.bottom), 'Groups / current group gap').toBeLessThanOrEqual(1);
        }
      }

      // A wrapped header changes its children's offsets without a scroll handler.
      await scrollTo(page, project.locator('[data-session-id="sticky-row-5"]'), 220);
      for (const height of [76, 44]) {
        await groups.evaluate((element, height) => { element.style.minHeight = height + 'px'; }, height);
        await frame(page);
        const group = project.getByRole('button', { name: 'Collapse Wallet work', exact: true }).locator('..');
        expect(Math.abs((await box(group)).top - (await box(groups)).bottom)).toBeLessThanOrEqual(1);
      }

      // The outgoing group cannot cover the next group, set, or project.
      await scrollTo(page, project.locator('[data-session-id="sticky-row-41"]'), 180);
      expect(Math.abs((await box(sessions)).top - (await box(header)).bottom)).toBeLessThanOrEqual(1);
      expect((await box(groups)).bottom).toBeLessThanOrEqual((await box(header)).bottom);
      const next = page.locator('[data-project-root="/After"]');
      await scrollTo(page, next.locator('[data-session-id="after-sticky-row-42"]'), 180);
      expect((await box(next.locator('header'))).top).toBe(48);
      expect((await box(header)).bottom).toBeLessThanOrEqual(48);
      await context.close();
    } finally {
      await browser.close();
    }
  }, 60_000);
}
