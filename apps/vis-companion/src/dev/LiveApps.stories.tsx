import type { Meta, StoryObj } from '@storybook/react-vite';
import { useEffect, useState } from 'react';
import { expect, userEvent, waitFor, within } from 'storybook/test';

import { AppSurface } from '../components/AppAttachment';
import { SessionEventsContext } from '../lib/session-events';
import {
  buildStep,
  COUNTER,
  DASHBOARD,
  type Job,
  SORT,
  StoryAppHost,
  TODO,
  type StoryApp,
  type WireApp,
} from './story-apps';
import type { ViewAction } from '../lib/view';

/**
 * LIVE APPS ARE ANY APP. Each story opens one real app in the shipped app surface.
 * The owner is an in-memory host that answers the same gateway calls and session
 * events as a real extension or SDK owner: actions, `view.open` and `view.patch`.
 */
const meta = {
  title: 'Session/Live view/Apps',
  parameters: { layout: 'fullscreen' },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

/** One app, opened in the session pane as a person opens it from the transcript. */
function OpenApp<S>({
  app,
  over,
  refuse,
  stream,
}: {
  app: StoryApp<S>;
  over?: Partial<WireApp>;
  refuse?: (action: ViewAction) => string | undefined;
  stream?: (host: StoryAppHost<S>) => () => void;
}) {
  const [host] = useState(() => new StoryAppHost(app, over, refuse));
  useEffect(() => stream?.(host), [host, stream]);
  return (
    <SessionEventsContext.Provider value={host.hub}>
      <div data-session-surface className="relative h-dvh">
        <AppSurface
          client={host.client}
          sid={host.sid}
          viewId={app.id}
          fallbackTitle={app.title}
          onClose={() => undefined}
        />
      </div>
    </SessionEventsContext.Provider>
  );
}

/** The legend entry labelled `label`, inside the app's key toolbar. Its name also says its key. */
async function legend(canvasElement: HTMLElement, label: string) {
  const bar = await within(canvasElement).findByRole('toolbar', { name: 'App keys' });
  const named = within(bar)
    .getAllByRole('button')
    .find((entry) => entry.getAttribute('aria-label')?.startsWith(`${label}, key `));
  if (!named) throw new Error(`no legend entry ${label}`);
  return named;
}

/** The app body, without its key legend: a node and its legend entry share one name. */
async function body(canvasElement: HTMLElement) {
  await waitFor(() => expect(canvasElement.querySelector('[data-app-surface]')).not.toBeNull());
  return within(canvasElement.querySelector<HTMLElement>('[data-app-surface]')!);
}

/** The value of one stat, by its label: the hero figure of a lone stat, or the term's value. */
function stat(canvasElement: HTMLElement, label: string): string {
  const term = within(canvasElement).getAllByText(label)[0];
  const definition = term.closest('dt')?.nextElementSibling?.querySelector('span');
  if (definition) return definition.textContent?.trim() ?? '';
  const hero = term.closest('div')?.parentElement?.querySelector('p');
  return hero?.textContent?.trim() ?? '';
}

/** A todo list: an input the owner validates and clears, a selectable table and keys. */
export const Todo: Story = {
  render: () => <OpenApp app={TODO} />,
  play: async ({ canvasElement }) => {
    const canvas = await body(canvasElement);
    const field = await canvas.findByRole('textbox', { name: /New task/ });
    // The owner refuses an empty task, and the field says why.
    await userEvent.click(canvas.getByRole('button', { name: /^Add/ }));
    await expect(await canvas.findByText('Type a task first.')).toBeVisible();
    // A key typed into the field is text, not a shortcut: `x` and `c` stay letters.
    await userEvent.type(field, 'Fix the box');
    await expect(field).toHaveValue('Fix the box');
    await expect(canvas.getByText('1 of 2 open')).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: /^Add/ }));
    await waitFor(() => expect(canvas.getByText('2 of 3 open')).toBeVisible());
    await expect(canvas.getByText('Fix the box')).toBeVisible();
    // The owner cleared its own input after Add.
    await waitFor(() => expect(field).toHaveValue(''));
    await expect(canvas.queryByText('Type a task first.')).toBeNull();
    // A row press toggles the task; a key outside the field finishes the first open task.
    await userEvent.click(canvas.getByText('Fix the box'));
    await waitFor(() => expect(canvas.getByText('1 of 3 open')).toBeVisible());
    field.blur();
    await userEvent.keyboard('x');
    await waitFor(() => expect(canvas.getByText('0 of 3 open')).toBeVisible());
    await userEvent.keyboard('c');
    await waitFor(() => expect(canvas.getByText('Nothing to do')).toBeVisible());
  },
};

/** One more package each 400 ms. The returned stop ends the stream when the story leaves. */
function streamBuild(host: StoryAppHost<Job>) {
  return host.stream(buildStep, 400);
}

/** A dashboard: status, progress, stats, steps, a streaming log, a wide table and a pause key. */
export const Dashboard: Story = {
  render: () => <OpenApp app={DASHBOARD} stream={streamBuild} />,
  play: async ({ canvasElement }) => {
    const canvas = await body(canvasElement);
    await canvas.findByText('Building release');
    await expect(await canvas.findByText(/package-11 built/, undefined, { timeout: 4000 })).toBeInTheDocument();
    await userEvent.keyboard('p');
    await waitFor(() => expect(canvas.getByText('Paused')).toBeVisible());
    await expect(canvas.getByRole('button', { name: /Resume/ })).toBeVisible();
    // A paused build streams nothing.
    const lines = canvasElement.textContent?.match(/package-\d+ built/g)?.length ?? 0;
    await new Promise((resolve) => setTimeout(resolve, 900));
    await expect(canvasElement.textContent?.match(/package-\d+ built/g)?.length ?? 0).toBe(lines);
    await userEvent.click(await legend(canvasElement, 'Resume'));
    await waitFor(() => expect(canvas.getByText('Building release')).toBeVisible());
  },
};

/** Six neutral rows, one choice for each item and ranked buttons. Check it on a phone width (#430). */
export const Sort: Story = {
  render: () => <OpenApp app={SORT} />,
};

/** A closed app is a record: no key legend, no live controls and no key reaches it. */
export const Closed: Story = {
  render: () => <OpenApp app={COUNTER} over={{ is_closed: true }} />,
  play: async ({ canvasElement }) => {
    const canvas = await body(canvasElement);
    await expect(await within(canvasElement).findByText('Closed')).toBeVisible();
    await expect(canvas.queryByRole('toolbar', { name: 'App keys' })).toBeNull();
    await expect(canvas.getByRole('button', { name: 'Plus one' })).toBeDisabled();
    await userEvent.keyboard('+');
    await expect(stat(canvasElement, 'Count')).toBe('0');
  },
};
