import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent } from 'storybook/test';

import type { MemoryCell, PerfReport } from '../lib/perf';
import { PerfOverlay } from './PerfOverlay';

const KB = 1024;
const MB = 1024 * KB;

function session(id: string, title: string, holdings: Array<[source: string, bytes: number, entries: number]>): MemoryCell[] {
  return holdings.map(([source, bytes, entries]) => ({ source, session: id, title, bytes, entries }));
}

/** Twenty minutes of work across six sessions, the shape `npm run perf` reports. */
const MANY_SESSIONS: PerfReport = {
  at: 0,
  heap: { used: 96.4 * MB, total: 128 * MB, limit: 4096 * MB },
  domNodes: 9_412,
  listeners: {
    live: 671,
    detached: 0,
    detachedByTarget: [],
    groups: [
      { target: 'div', type: 'pointerdown', site: 'SwipeActions (SwipeActions.tsx:212)', live: 118, added: 240 },
      { target: 'window', type: 'resize', site: 'useViewportHeight (viewport.ts:31)', live: 42, added: 60 },
      { target: 'AbortSignal', type: 'abort', site: 'linkSignals (gateway.ts:5096)', live: 12, added: 380 },
      { target: 'document', type: 'visibilitychange', site: 'SessionStreamHub (subscriptions.ts:88)', live: 6, added: 6 },
    ],
  },
  intervals: { live: 3, sites: [{ site: 'PerfOverlay (PerfOverlay.tsx:145)', live: 1 }] },
  timeouts: { pending: 14, sites: [] },
  observers: [
    { kind: 'ResizeObserver', observers: 41, targets: 1_108, detached: 0 },
    { kind: 'IntersectionObserver', observers: 3, targets: 24, detached: 0 },
  ],
  objectUrls: { live: 18, bytes: 6.2 * MB },
  cells: [
    ...session('s-gateway', 'Refactor the gateway client', [
      ['transcript', 38.4 * MB, 1],
      ['session', 12 * KB, 1],
      ['stream buffer', 3.9 * MB, 822],
      ['watched streams', 0, 1],
      ['stream listeners', 0, 4],
    ]),
    ...session('s-release', 'Release notes for 0.2.29', [
      ['transcript', 9.6 * MB, 1],
      ['stream buffer', 1.9 * MB, 774],
      ['watched streams', 0, 1],
    ]),
    ...session('s-splash', 'Android splash screen', [
      ['transcript', 4.2 * MB, 1],
      ['watched streams', 0, 1],
    ]),
    ...session('s-overlay', 'Memory overlay', [
      ['transcript', 2.1 * MB, 1],
      ['stream buffer', 1.8 * MB, 330],
      ['watched streams', 0, 1],
    ]),
    ...session('s-flaky', 'Fix a flaky test', [['transcript', 640 * KB, 1]]),
    ...session('s-week', 'Plan the week', [
      ['session', 8 * KB, 1],
      ['watched streams', 0, 1],
    ]),
    { source: 'sessions', bytes: 420 * KB, entries: 180 },
    { source: 'settings', bytes: 36 * KB, entries: 1 },
    { source: 'gateway clients', bytes: 0, entries: 2 },
  ],
};

const meta = {
  title: 'Diagnostics/Memory overlay',
  component: PerfOverlay,
  parameters: { layout: 'padded' },
  args: { read: () => MANY_SESSIONS, refreshMs: 0 },
} satisfies Meta<typeof PerfOverlay>;

export default meta;
type Story = StoryObj<typeof meta>;

export const ManySessions: Story = {};

export const LeftOnRemovedElements: Story = {
  args: {
    read: () => ({
      ...MANY_SESSIONS,
      listeners: {
        ...MANY_SESSIONS.listeners,
        detached: 7,
        detachedByTarget: [{ site: 'div', live: 7 }],
      },
      observers: [{ kind: 'ResizeObserver', observers: 41, targets: 1_108, detached: 5 }],
    }),
  },
};

/** Safari and the macOS desktop app do not report the JS heap. */
export const WithoutHeapSize: Story = {
  args: { read: () => ({ ...MANY_SESSIONS, heap: null }) },
};

export const Minimized: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Minimize memory overlay' }));
    await expect(canvas.getByRole('button', { name: /^Memory 96\.4 MB · 671 listeners$/ })).toBeVisible();
  },
};
