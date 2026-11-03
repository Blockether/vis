import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState, type ReactNode } from 'react';
import { expect, userEvent, within } from 'storybook/test';

import { SessionEventsContext } from '../lib/session-events';
import { StoryAppHost, TODO, type WireApp } from '../dev/story-apps';
import { AppSurface } from './AppAttachment';

/**
 * THE DEBUG LOG OF AN APP. The band of every app surface shows a Log control
 * when the log has entries, with the count of errors. Its panel lists the
 * entries oldest first, filters them by level and copies them.
 */
const meta = {
  title: 'Session/Live view/App log',
  parameters: { layout: 'fullscreen' },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

const AT = Date.UTC(2026, 4, 12, 9, 30, 0);

const LOG = [
  { seq: 1, at: AT, level: 'info', source: 'app', message: 'Loaded 4 items from the saved state' },
  { seq: 2, at: AT + 120, level: 'debug', source: 'app', message: 'Render took 3 ms' },
  { seq: 3, at: AT + 2_400, level: 'warn', source: 'app', message: 'The item "Ship" has no due date' },
  {
    seq: 4,
    at: AT + 5_031,
    level: 'error',
    source: 'host',
    message: 'The callback of action "toggle" failed',
    detail: [
      "KeyError: 'done'",
      '  File "todo.py", line 41, in on_toggle',
      "    item['done'] = not item['done']",
    ].join('\n'),
  },
];

/** The todo app in the chat turn, with the log that `over` gives it. */
function Turn({ over }: { over: Partial<WireApp> }) {
  const [host] = useState(() => new StoryAppHost(TODO, { key: 'todo', ...over }));
  return (
    <SessionEventsContext.Provider value={host.hub}>
      <Pane>
        <AppSurface
          client={host.client}
          sid={host.sid}
          viewId={TODO.id}
          fallbackTitle={TODO.title}
          isInline
          onFullScreen={() => undefined}
          onClose={() => undefined}
        />
      </Pane>
    </SessionEventsContext.Provider>
  );
}

function Pane({ children }: { children: ReactNode }) {
  return (
    <div data-session-surface className="relative flex h-dvh flex-col gap-3 bg-ink p-4">
      {children}
    </div>
  );
}

/** A log with an error from the host: the badge counts it, and the panel opens on a press. */
export const WithErrors: Story = {
  render: () => <Turn over={{ log: LOG }} />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(await canvas.findByRole('button', { name: /^Log of .*, 1 error$/ }));
    const panel = canvas.getByRole('region', { name: /^Log of / });
    await userEvent.click(within(panel).getByRole('button', { name: /^Show detail of/ }));
    await expect(within(panel).getByLabelText(/^Detail of/)).toBeVisible();
    await userEvent.click(within(panel).getByRole('button', { name: /^Error/ }));
    await expect(within(panel).getAllByRole('listitem')).toHaveLength(1);
  },
};

/** An app with an empty log: the band shows no Log control. */
export const Empty: Story = {
  render: () => <Turn over={{ log: [] }} />,
};
