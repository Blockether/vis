import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState, type ReactNode } from 'react';

import { liveViewFromWire } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import { StoryAppHost, type StoryApp, type WireApp } from '../dev/story-apps';
import REVIEW from '../dev/review-app.json';
import { SessionAppViewsContext } from './AppAttachment';
import { SessionAppSlot } from './SessionAppSlot';

/** The review app as `vis-review` paints it over the T3 Code fixture repository. */
const REVIEW_APP: StoryApp<null> = {
  id: 'app-review',
  title: 'Review',
  initial: () => null,
  render: () => {
    const app = REVIEW as unknown as WireApp;
    return { nodes: app.nodes, shortcuts: app.shortcuts };
  },
  act: (state) => ({ state }),
};

const meta = {
  title: 'Session/Live view/Review app',
  parameters: { layout: 'fullscreen' },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

function Desk({ children }: { children: ReactNode }) {
  return (
    <div data-session-surface className="relative flex min-w-0" style={{ width: '100vw', height: '100vh' }}>
      <section aria-label="Chat" className="relative flex min-h-0 min-w-0 flex-1 flex-col bg-ink p-4" />
      {children}
    </div>
  );
}

/** The review app in the session sidebar, beside the chat. */
export const Sidebar: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  render: () => <ReviewSlot slot="sidebar" />,
};

/** The review app on a phone, where it covers the whole pane. */
export const Phone: Story = {
  globals: { viewport: { value: 'mobile1', isRotated: false } },
  render: () => <ReviewSlot slot="sheet" />,
};

function ReviewSlot({ slot }: { slot: 'sidebar' | 'sheet' }) {
  const [host] = useState(
    () => new StoryAppHost(REVIEW_APP, { key: 'review', surfaces: { inline: false, session: 'sidebar' } }),
  );
  const view = liveViewFromWire({ ...host.view, placement: 'attachment' });
  if (!view) throw new Error('the review app must parse');
  return (
    <SessionEventsContext.Provider value={host.hub}>
      <SessionAppViewsContext.Provider value={[view]}>
        <Desk>
          <SessionAppSlot client={host.client} sid={host.sid} view={view} slot={slot} onClose={() => undefined} />
        </Desk>
      </SessionAppViewsContext.Provider>
    </SessionEventsContext.Provider>
  );
}
