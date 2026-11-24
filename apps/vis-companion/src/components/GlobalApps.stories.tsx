import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect, screen } from 'storybook/test';

import { liveViewFromWire } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import { StoryAppHost, TODO } from '../dev/story-apps';
import { AppSurface } from './AppAttachment';
import { GlobalApps } from './GlobalApps';

/**
 * GLOBAL APPS BELONG TO NO SESSION. Only an SDK program opens one. The shell
 * header shows one icon for each, and a tap opens the app over the whole shell.
 */
const meta = {
  title: 'App/Global apps',
  parameters: { layout: 'fullscreen' },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

const PARSED = liveViewFromWire({ id: TODO.id, title: TODO.title, key: 'todo', scope: 'global', placement: 'attachment', seq: 1, nodes: [{ id: 'break', type: 'divider' }] });
if (!PARSED) throw new Error('story app must parse');
const LISTED = PARSED;

/** A global app picked from its header icon, open over the whole shell. */
export const Default: Story = {
  render: function Render() {
    const [host] = useState(() => new StoryAppHost(TODO, { key: 'todo', scope: 'global' }));
    return (
      <div className="relative h-dvh bg-ink">
        <GlobalApps client={host.client} hub={host.hub} apps={[LISTED]} openId={TODO.id} onClose={() => undefined} />
      </div>
    );
  },
  play: async () => {
    await expect((await screen.findAllByText(TODO.title)).length).toBeGreaterThan(0);
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
