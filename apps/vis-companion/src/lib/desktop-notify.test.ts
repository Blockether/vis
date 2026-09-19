// @vitest-environment jsdom
// The desktop app cannot be reached by push at all: its WKWebView has no `PushManager` and no
// service worker for the app scheme, so nothing arrives while it is closed. Alerts are raised
// locally instead, from the fleet the sessions list already polls, and this is the contract for
// when one is raised: news only, once, and never a backlog replayed at whoever just switched on.
import { afterEach, beforeEach, expect, it, vi } from 'vitest';

// `vi.mock` factories run at import time, before module-scope `const`s of this file exist --
// the shared state has to be hoisted with them.
const native = vi.hoisted(() => ({ store: new Map<string, string>() }));

vi.mock('@capacitor/preferences', () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => ({ value: native.store.get(key) ?? null }),
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
    remove: async ({ key }: { key: string }) => {
      native.store.delete(key);
    },
  },
}));

/** What the machine that raised the banner says it says, when it can be reached. */
const remote = vi.hoisted(() => ({
  alert: { title: 'a', body: 'Shipped the fix.' },
  asked: [] as string[],
  isReachable: true,
}));

vi.mock('./gateway', () => ({
  GatewayClient: class {
    async sessionAlert(_sid: string, reason: string) {
      if (!remote.isReachable) throw new Error('machine unreachable');
      remote.asked.push(reason);
      return remote.alert;
    }
  },
}));

import { notifyDesktopFleet, resetDesktopAlerts } from './desktop-notify';
import { setGatewayNotify } from './storage';
import type { FleetMachine } from './fleet';
import type { Session } from './types';

type DesktopWindow = Window & { __TAURI__?: { core: { invoke: ReturnType<typeof vi.fn> } } };

/** The Pake desktop window: one command channel, counted here rather than delivered. */
function stubDesktopHost() {
  const invoke = vi.fn(
    async (command: string, _payload?: { options: { title: string; body: string } }) =>
      command === 'plugin:notification|is_permission_granted' ? true : 'granted',
  );
  (window as DesktopWindow).__TAURI__ = { core: { invoke } };
  return invoke;
}

const MACHINE = 'http://10.0.0.5:7890';

const session = (id: string, fields: Partial<Session> = {}): Session =>
  ({ id, title: id, answer_count: 0, ...fields }) as Session;

const fleet = (sessions: Session[] | null, error: string | null = null): FleetMachine[] => [
  { conn: { url: MACHINE, label: 'buildbox' }, sessions, error } as FleetMachine,
];

/** Every alert this window asked the host to raise. */
const alerts = (invoke: ReturnType<typeof stubDesktopHost>) =>
  invoke.mock.calls
    .filter(([command]) => command === 'plugin:notification|notify')
    .map(([, payload]) => payload?.options);

beforeEach(async () => {
  native.store.clear();
  remote.alert = { title: 'a', body: 'Shipped the fix.' };
  remote.asked = [];
  remote.isReachable = true;
  resetDesktopAlerts();
  await setGatewayNotify(MACHINE, true);
});

afterEach(() => {
  delete (window as DesktopWindow).__TAURI__;
  vi.restoreAllMocks();
});

it('says nothing about the sessions that were already there', async () => {
  const invoke = stubDesktopHost();

  await notifyDesktopFleet(fleet([session('a', { answer_count: 3, is_awaiting_input: true })]));

  expect(alerts(invoke)).toEqual([]);
});

it('raises one alert for a new answer, and does not raise it twice', async () => {
  const invoke = stubDesktopHost();
  await notifyDesktopFleet(fleet([session('a', { answer_count: 3 })]));

  await notifyDesktopFleet(fleet([session('a', { answer_count: 4 })]));
  await notifyDesktopFleet(fleet([session('a', { answer_count: 4 })]));

  expect(alerts(invoke)).toEqual([{ title: 'a', body: 'Shipped the fix.' }]);
});

it('reports the question rather than the answer that carried it', async () => {
  const invoke = stubDesktopHost();
  await notifyDesktopFleet(fleet([session('deploy', { answer_count: 3 })]));
  remote.alert = {
    title: 'Action needed — Which branch should I deploy?',
    body: 'main is two commits ahead of the tag.',
  };

  await notifyDesktopFleet(
    fleet([session('deploy', { answer_count: 4, is_awaiting_input: true })]),
  );

  expect(remote.asked).toEqual(['question']);
  expect(alerts(invoke)).toEqual([
    {
      title: 'Action needed — Which branch should I deploy?',
      body: 'main is two commits ahead of the tag.',
    },
  ]);
});

it('keeps watching a machine whose switch is off, and never replays what it missed', async () => {
  const invoke = stubDesktopHost();
  await setGatewayNotify(MACHINE, false);
  await notifyDesktopFleet(fleet([session('a', { answer_count: 3 })]));
  await notifyDesktopFleet(fleet([session('a', { answer_count: 9 })]));

  await setGatewayNotify(MACHINE, true);
  await notifyDesktopFleet(fleet([session('a', { answer_count: 9 })]));
  expect(alerts(invoke)).toEqual([]);

  await notifyDesktopFleet(fleet([session('a', { answer_count: 10 })]));
  expect(alerts(invoke)).toHaveLength(1);
});

it('holds its place through an unreachable machine instead of starting over', async () => {
  const invoke = stubDesktopHost();
  await notifyDesktopFleet(fleet([session('a', { answer_count: 3 })]));

  await notifyDesktopFleet(fleet(null, 'machine unreachable'));
  await notifyDesktopFleet(fleet([session('a', { answer_count: 3 })]));

  expect(alerts(invoke)).toEqual([]);
});

it('is inert in a browser tab, which has real push instead', async () => {
  await notifyDesktopFleet(fleet([session('a', { answer_count: 3 })]));
  await notifyDesktopFleet(fleet([session('a', { answer_count: 4 })]));

  // Nothing was watched without the channel, so the desktop window's first pass still seeds.
  const invoke = stubDesktopHost();
  await notifyDesktopFleet(fleet([session('a', { answer_count: 5 })]));

  expect(alerts(invoke)).toEqual([]);
});

it('says what the gateway says, so this window and a phone read alike', async () => {
  const invoke = stubDesktopHost();
  await notifyDesktopFleet(fleet([session('deploy', { answer_count: 3 })]));
  // The gateway flattens the answer out of markdown (`gateway/push.clj`); nothing is worded twice.
  remote.alert = { title: 'deploy', body: 'Done • Shipped v2 to prod • Release notes' };

  await notifyDesktopFleet(fleet([session('deploy', { answer_count: 4 })]));

  expect(remote.asked).toEqual(['answer']);
  expect(alerts(invoke)).toEqual([
    { title: 'deploy', body: 'Done • Shipped v2 to prod • Release notes' },
  ]);
});

it('still alerts when the machine cannot say what it answered', async () => {
  const invoke = stubDesktopHost();
  await notifyDesktopFleet(fleet([session('a', { answer_count: 3 })]));
  remote.isReachable = false;

  await notifyDesktopFleet(fleet([session('a', { answer_count: 4 })]));

  expect(alerts(invoke)).toEqual([{ title: 'a', body: 'Turn finished.' }]);
});

it('names the machine only while more than one is watched', async () => {
  const invoke = stubDesktopHost();
  const laptop = 'http://10.0.0.6:7890';
  await setGatewayNotify(laptop, true);
  remote.alert = { title: 'deploy', body: 'Shipped the fix.' };
  const pair = (answers: number): FleetMachine[] => [
    ...fleet([session('deploy', { answer_count: answers })]),
    {
      conn: { url: laptop, label: 'laptop' },
      sessions: [session('notes')],
      error: null,
    } as FleetMachine,
  ];
  await notifyDesktopFleet(pair(3));

  await notifyDesktopFleet(pair(4));

  expect(alerts(invoke)).toEqual([{ title: 'deploy — buildbox', body: 'Shipped the fix.' }]);
});
