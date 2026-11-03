import { describe, expect, it } from 'vitest';

import { liveViewFromWire } from './live-view';

import {
  appFrameStep,
  appSurfacesFromWire,
  isAppSurfaceSetting,
  pickedSessionApp,
  pressReveal,
  revealedApp,
  sessionAppSlot,
  sessionApps,
  shownAppFrame,
  type AppSurfaces,
} from './app-surfaces';

type App = { id: string; placement: 'attachment' | 'panel'; surfaces?: AppSurfaces };
const app = (id: string, session?: AppSurfaces['session'], inline = true): App => ({
  id,
  placement: 'attachment',
  ...(session ? { surfaces: { inline, session } } : {}),
});

describe('session apps list', () => {
  // The session apps button must not list an app that the person turned off.
  it('lists each app whose session surface is not off, and no panel view', () => {
    const apps: App[] = [app('a'), app('b', 'off'), app('c', 'chat'), { id: 'p', placement: 'panel' }];
    expect(sessionApps(apps).map((one) => one.id)).toEqual(['a', 'c']);
  });
});

// User report 2026-11-08: a Live apps gallery card opened its app, but the screen
// stayed on the gallery, so the person never saw the app.
describe('app reveal', () => {
  const gallery = liveViewFromWire({
    id: 'gallery',
    title: 'Live apps',
    nodes: [
      {
        id: 'apps',
        type: 'group',
        direction: 'grid',
        children: [
          { id: 'open-todo', type: 'card', title: 'Todo', is_selectable: true, reveals: 'todo' },
          { id: 'plain', type: 'card', title: 'Plain', is_selectable: true },
        ],
      },
    ],
  })!;

  it('reveals the key of a pressed node, also inside a group, and nothing for a plain node', () => {
    expect(pressReveal(gallery, 'open-todo')).toBe('todo');
    expect(pressReveal(gallery, 'plain')).toBeNull();
    expect(pressReveal(gallery, 'missing')).toBeNull();
  });

  type Open = { id: string; extension?: string; key?: string; is_closed?: boolean };
  const reveal = { extension: 'live-apps', key: 'todo' };

  it('shows the open app of the same owner with that key', () => {
    const apps: Open[] = [
      { id: 'other-owner', extension: 'someone', key: 'todo' },
      { id: 'closed', extension: 'live-apps', key: 'todo', is_closed: true },
      { id: 'todo', extension: 'live-apps', key: 'todo' },
      { id: 'kanban', extension: 'live-apps', key: 'kanban' },
    ];
    expect(revealedApp(apps, reveal)).toBe('todo');
  });

  it('waits while the app is not open yet', () => {
    expect(revealedApp([{ id: 'kanban', extension: 'live-apps', key: 'kanban' }], reveal)).toBeNull();
  });
});

describe('sessionAppSlot', () => {
  it('opens a sidebar app beside the chat on a desk and as a sheet on a phone', () => {
    expect(sessionAppSlot(app('a', 'sidebar'), true)).toBe('sidebar');
    expect(sessionAppSlot(app('a', 'sidebar'), false)).toBe('sheet');
  });

  it('opens a chat app over the chat area on every width', () => {
    expect(sessionAppSlot(app('a', 'chat'), true)).toBe('chat');
    expect(sessionAppSlot(app('a', 'chat'), false)).toBe('chat');
  });

  it('opens an app without surfaces in the sidebar, and an off app nowhere', () => {
    expect(sessionAppSlot(app('a'), true)).toBe('sidebar');
    expect(sessionAppSlot(app('a', 'off'), true)).toBeNull();
  });
});

describe('pickedSessionApp', () => {
  // A saved setting can turn an opened app off: its slot must close.
  it('drops a picked app that the settings turned off or that left the list', () => {
    expect(pickedSessionApp([app('a', 'chat')], 'a', true)?.slot).toBe('chat');
    expect(pickedSessionApp([app('a', 'off')], 'a', true)).toBeNull();
    expect(pickedSessionApp([app('b')], 'a', true)).toBeNull();
    expect(pickedSessionApp([app('a')], null, true)).toBeNull();
  });
});

describe('app frame in the chat turn', () => {
  it('leaves full screen first on Escape, then closes', () => {
    const full = appFrameStep('inline', 'full', true);
    expect(full).toBe('full');
    const back = appFrameStep(full, 'escape', true);
    expect(back).toBe('inline');
    expect(appFrameStep(back, 'escape', true)).toBe('row');
  });

  it('opens an app without the inline surface full screen and closes it to the row', () => {
    expect(appFrameStep('row', 'open', false)).toBe('full');
    expect(appFrameStep('full', 'escape', false)).toBe('row');
    expect(appFrameStep('row', 'open', true)).toBe('inline');
  });

  // The person turned the inline surface off while the app was open in the turn.
  it('never shows the frame of an app without the inline surface in the turn', () => {
    expect(shownAppFrame('inline', false)).toBe('full');
    expect(shownAppFrame('inline', true)).toBe('inline');
    expect(shownAppFrame('row', false)).toBe('row');
  });
});

describe('app surfaces on the wire', () => {
  it('reads an unknown session surface as the schema default', () => {
    expect(appSurfacesFromWire({ inline: false, session: 'floating' })).toEqual({ inline: false, session: 'sidebar' });
    expect(appSurfacesFromWire({ session: 'chat' })).toEqual({ inline: true, session: 'chat' });
  });

  // A save of these settings must make the clients read the apps again.
  it('knows the settings that move apps', () => {
    expect(isAppSurfaceSetting('apps_inline')).toBe(true);
    expect(isAppSurfaceSetting('apps_session')).toBe(true);
    expect(isAppSurfaceSetting('apps_global')).toBe(true);
    expect(isAppSurfaceSetting('app_sdk_board_session')).toBe(true);
    expect(isAppSurfaceSetting('app_sdk_board_inline')).toBe(true);
    expect(isAppSurfaceSetting('reasoning_effort')).toBe(false);
  });
});
