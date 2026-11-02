import type { Meta, StoryObj } from '@storybook/react-vite';
import { useCallback, useState, type ReactNode } from 'react';
import { expect, userEvent, waitFor, within } from 'storybook/test';

import { sessionApps } from '../lib/app-surfaces';
import { liveViewFromWire, type LiveView } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import type { IterationAttachment } from '../lib/types';
import { StoryAppHost, TODO, type WireApp } from '../dev/story-apps';
import { AppAttachmentRow, AppSurface, SessionAppsDialog, SessionAppViewsContext } from './AppAttachment';
import { SessionAppSlot } from './SessionAppSlot';

/**
 * WHERE A SESSION APP SHOWS. The engine sends the surfaces after the person's
 * settings: `inline` decides the frame in the chat turn, and `session` decides
 * where the session apps list opens the app.
 */
const meta = {
  title: 'Session/Live view/App surfaces',
  parameters: { layout: 'fullscreen' },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

const ATTACHMENT = { index: 0, view_id: TODO.id, filename: 'todo-list.app.json' } as IterationAttachment;

/** A keyed app with its surfaces, as the session list sends it. */
function listed(id: string, title: string, surfaces: WireApp['surfaces']): LiveView {
  const view = liveViewFromWire({
    id,
    title,
    key: id,
    placement: 'attachment',
    seq: 1,
    surfaces,
    nodes: [{ id: 'break', type: 'divider' }],
  });
  if (!view) throw new Error('story app must parse');
  return view;
}

const LISTED = [
  listed('app-board', 'Release board', { inline: true, session: 'sidebar' }),
  listed('app-review', 'Code review', { inline: false, session: 'chat' }),
  // The person turned the session surface of this app off: the list leaves it out.
  listed('app-metrics', 'Metrics', { inline: true, session: 'off' }),
];

/** The todo app in one in-memory host, with the surfaces that `over` sets. */
type Host = Pick<StoryAppHost<unknown>, 'client' | 'sid' | 'hub' | 'view'>;

function WithHost({ over, children }: { over?: Partial<WireApp>; children: (host: Host) => ReactNode }) {
  const [host] = useState(() => new StoryAppHost(TODO, { key: 'todo', ...over }));
  return (
    <SessionEventsContext.Provider value={host.hub}>
      <SessionAppViewsContext.Provider value={host.view.surfaces ? [listed(TODO.id, TODO.title, host.view.surfaces)] : []}>
        {children(host)}
      </SessionAppViewsContext.Provider>
    </SessionEventsContext.Provider>
  );
}

const DESK_WIDTH = 1280;
const DESK_HEIGHT = 800;

/**
 * A desk-sized session pane, scaled down to the story width. The story draws the
 * desk itself, so a direct load of its iframe in a narrow window still shows the
 * whole desk: the panel beside the chat, or the app over the whole chat.
 */
function DeskFrame({ children }: { children: ReactNode }) {
  const [width, setWidth] = useState(DESK_WIDTH);
  const measure = useCallback((node: HTMLDivElement | null) => {
    if (!node) return;
    setWidth(node.clientWidth || DESK_WIDTH);
    if (typeof ResizeObserver === 'undefined') return;
    const observer = new ResizeObserver(() => setWidth(node.clientWidth || DESK_WIDTH));
    observer.observe(node);
    return () => observer.disconnect();
  }, []);
  const scale = Math.min(1, width / DESK_WIDTH);
  return (
    <div ref={measure} className="w-full overflow-hidden" style={{ height: DESK_HEIGHT * scale }}>
      <div
        data-session-surface
        className="relative flex min-w-0"
        style={{ width: DESK_WIDTH, height: DESK_HEIGHT, transform: `scale(${scale})`, transformOrigin: 'top left' }}
      >
        {children}
      </div>
    </div>
  );
}

/** A stand-in for the transcript beside or under the app. */
function Chat({ children }: { children?: ReactNode }) {
  return (
    <section aria-label="Chat" className="relative flex min-h-0 min-w-0 flex-1 flex-col bg-ink p-4">
      <p className="font-mono text-meta text-muted">Make a release checklist for Friday.</p>
      {children}
    </section>
  );
}

/** The app in the chat turn that opened it, with its Full screen control. */
export const Inline: Story = {
  render: () => (
    <WithHost over={{ surfaces: { inline: true, session: 'sidebar' } }}>
      {(host) => (
        <div data-session-surface className="relative flex h-dvh flex-col gap-3 bg-ink p-4">
          <p className="font-mono text-meta text-muted">Make a release checklist for Friday.</p>
          <AppSurface
            client={host.client}
            sid={host.sid}
            viewId={TODO.id}
            fallbackTitle={TODO.title}
            isInline
            onFullScreen={() => undefined}
            onClose={() => undefined}
          />
        </div>
      )}
    </WithHost>
  ),
};

/** An app without the inline surface: the turn shows only its row, which opens it full screen. */
export const InlineOff: Story = {
  render: () => (
    <WithHost over={{ surfaces: { inline: false, session: 'sidebar' } }}>
      {(host) => (
        <div data-session-surface className="relative flex h-dvh flex-col gap-3 bg-ink p-4">
          <p className="font-mono text-meta text-muted">Make a release checklist for Friday.</p>
          <AppAttachmentRow client={host.client} sid={host.sid} attachment={ATTACHMENT} />
        </div>
      )}
    </WithHost>
  ),
};

/**
 * Escape leaves full screen first and gives back the frame in the turn, then
 * closes the app to its row.
 */
export const FullScreen: Story = {
  render: () => (
    <WithHost over={{ surfaces: { inline: true, session: 'sidebar' } }}>
      {(host) => (
        <div data-session-surface className="relative flex h-dvh flex-col gap-3 bg-ink p-4">
          <AppAttachmentRow client={host.client} sid={host.sid} attachment={ATTACHMENT} />
        </div>
      )}
    </WithHost>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(canvas.getByRole('button', { name: 'Open app todo-list' }));
    await userEvent.click(await canvas.findByRole('button', { name: `Full screen ${TODO.title}` }));
    await page.findByRole('dialog');
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(page.queryByRole('dialog')).toBeNull());
    const frame = await canvas.findByRole('region', { name: TODO.title });
    frame.querySelector<HTMLElement>('[data-app-surface]')?.focus();
    await userEvent.keyboard('{Escape}');
    await canvas.findByRole('button', { name: 'Open app todo-list' });
  },
};

/** The list of the header button. An app whose session surface is off is not in it. */
export const AppsList: Story = {
  render: () => (
    <div data-session-surface className="relative h-dvh bg-ink">
      <SessionAppsDialog apps={sessionApps(LISTED)} onPick={() => undefined} onClose={() => undefined} />
    </div>
  ),
};

/** An app opened in the side panel next to the chat, on a desk. */
export const Sidebar: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  render: () => (
    <WithHost over={{ surfaces: { inline: true, session: 'sidebar' } }}>
      {(host) => (
        <DeskFrame>
          <Chat />
          <SessionAppSlot
            client={host.client}
            sid={host.sid}
            view={listed(TODO.id, TODO.title, host.view.surfaces)}
            slot="sidebar"
            onClose={() => undefined}
          />
        </DeskFrame>
      )}
    </WithHost>
  ),
};

/** An app that fills the chat area, with Back to the chat. */
export const ChatArea: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  render: () => (
    <WithHost over={{ surfaces: { inline: true, session: 'chat' } }}>
      {(host) => (
        <DeskFrame>
          <Chat>
            <SessionAppSlot
              client={host.client}
              sid={host.sid}
              view={listed(TODO.id, TODO.title, host.view.surfaces)}
              slot="chat"
              onClose={() => undefined}
            />
          </Chat>
        </DeskFrame>
      )}
    </WithHost>
  ),
};

/** On a phone there is no room beside the chat: a side-panel app takes the whole pane. */
export const Sheet: Story = {
  render: () => (
    <WithHost over={{ surfaces: { inline: true, session: 'sidebar' } }}>
      {(host) => (
        <div data-session-surface className="relative flex h-dvh min-w-0 flex-col">
          <Chat />
          <SessionAppSlot
            client={host.client}
            sid={host.sid}
            view={listed(TODO.id, TODO.title, host.view.surfaces)}
            slot="sheet"
            onClose={() => undefined}
          />
        </div>
      )}
    </WithHost>
  ),
};
