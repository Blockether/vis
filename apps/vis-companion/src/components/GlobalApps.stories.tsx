import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';

import { liveViewFromWire, type LiveView } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import { StoryAppHost, TODO } from '../dev/story-apps';
import { AppSurface } from './AppAttachment';
import { GlobalApps } from './GlobalApps';

/**
 * GLOBAL APPS BELONG TO NO SESSION. Only an SDK program opens one. The shell
 * header lists them, and a pick opens the app over the whole shell.
 */
const meta = {
  title: 'App/Global apps',
  parameters: { layout: 'fullscreen' },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

function global(id: string, title: string, over: Record<string, unknown> = {}): LiveView {
  const view = liveViewFromWire({
    id,
    title,
    key: id,
    scope: 'global',
    placement: 'attachment',
    seq: 1,
    nodes: [{ id: 'break', type: 'divider' }],
    ...over,
  });
  if (!view) throw new Error('story app must parse');
  return view;
}

const APPS = [
  global('app-standup', 'Team standup'),
  // The gateway restarted and the program has not adopted its app again.
  global('app-builds', 'Build radiator', { is_stale: true }),
  global('app-poll', 'Lunch poll', { is_closed: true }),
];

/** The list that the header button opens, with a live, a stale and a closed app. */
export const Default: Story = {
  render: function Render() {
    const [host] = useState(() => new StoryAppHost(TODO, { key: 'todo', scope: 'global' }));
    return (
      <div className="relative h-dvh bg-ink">
        <GlobalApps client={host.client} hub={host.hub} apps={APPS} isListOpen onListClose={() => undefined} />
      </div>
    );
  },
};

/** An open global app whose program is not connected: it shows its saved picture and takes no actions. */
export const ProgramNotConnected: Story = {
  render: function Render() {
    const [host] = useState(() => new StoryAppHost(TODO, { key: 'todo', scope: 'global', is_stale: true }));
    return (
      <SessionEventsContext.Provider value={host.hub}>
        <div className="relative h-dvh bg-ink">
          <AppSurface client={host.client} sid={null} viewId={TODO.id} fallbackTitle={TODO.title} onClose={() => undefined} />
        </div>
      </SessionEventsContext.Provider>
    );
  },
};
