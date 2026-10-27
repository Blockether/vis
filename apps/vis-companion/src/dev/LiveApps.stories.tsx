import type { Meta, StoryObj } from '@storybook/react-vite';
import { useEffect, useState } from 'react';
import { expect, userEvent, waitFor, within } from 'storybook/test';

import { AppSurface } from '../components/AppAttachment';
import { SessionEventsContext } from '../lib/session-events';
import {
  BOARD,
  buildStep,
  COUNTER,
  DASHBOARD,
  GAME_2048,
  type Job,
  StoryAppHost,
  TODO,
  type StoryApp,
  type WireApp,
  WIZARD,
} from './story-apps';
import type { ViewAction } from '../lib/view';

/**
 * LIVE APPS ARE ANY APP. Each story opens one real app in the shipped app surface.
 * The owner is an in-memory host that answers the same gateway calls and session
 * events as a real extension or SDK owner: actions, `view.open` and `view.patch`.
 */
const meta = {
  title: 'Live View/Apps',
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

/** A counter: two buttons, a stat and five keys, two of them owner actions. */
export const Counter: Story = {
  render: () => <OpenApp app={COUNTER} />,
  play: async ({ canvasElement }) => {
    const canvas = await body(canvasElement);
    await canvas.findByRole('button', { name: 'Plus one' });
    await expect(stat(canvasElement, 'Count')).toBe('0');
    await expect(canvas.getByRole('button', { name: 'Reset' })).toBeDisabled();
    await userEvent.click(canvas.getByRole('button', { name: 'Plus one' }));
    await waitFor(() => expect(stat(canvasElement, 'Count')).toBe('1'));
    // A key that names a node presses that node, once the node takes presses again.
    await waitFor(() => expect(canvas.getByRole('button', { name: 'Plus one' })).toBeEnabled());
    await userEvent.keyboard('+');
    await waitFor(() => expect(stat(canvasElement, 'Count')).toBe('2'));
    // A key that names an action goes to the owner, in the order of the presses.
    await userEvent.keyboard('{ArrowUp}{ArrowUp}{ArrowDown}');
    await waitFor(() => expect(stat(canvasElement, 'Count')).toBe('12'));
    // A legend tap is the same press: a phone has no keys.
    await userEvent.click(await legend(canvasElement, 'Minus ten'));
    await waitFor(() => expect(stat(canvasElement, 'Count')).toBe('2'));
    await waitFor(() => expect(canvas.getByRole('button', { name: 'Reset' })).toBeEnabled());
    await userEvent.keyboard('r');
    await waitFor(() => expect(stat(canvasElement, 'Count')).toBe('0'));
    // A modified key is the browser's, not the app's.
    await userEvent.keyboard('{Control>}+{/Control}');
    await expect(stat(canvasElement, 'Count')).toBe('0');
  },
};

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

/** A board: a grid of column groups with selectable cards that move on each press. */
export const Board: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  render: () => <OpenApp app={BOARD} />,
  play: async ({ canvasElement }) => {
    const canvas = await body(canvasElement);
    await canvas.findByText('Todo (2)');
    await userEvent.click(canvas.getByRole('button', { name: /Dark theme check/ }));
    await waitFor(() => expect(canvas.getByText('Doing (2)')).toBeVisible());
    await expect(canvas.getByText('Todo (1)')).toBeVisible();
    await userEvent.keyboard('n');
    await waitFor(() => expect(canvas.getByText('Todo (2)')).toBeVisible());
    await expect(canvas.getByRole('button', { name: /Card 5/ })).toBeVisible();
  },
};

/** 2048: a 4x4 table, arrow keys that the owner reads in order, and a restart key. */
export const Game2048: Story = {
  name: '2048',
  render: () => <OpenApp app={GAME_2048} />,
  play: async ({ canvasElement }) => {
    const canvas = await body(canvasElement);
    const grid = await canvas.findByLabelText('Board');
    const board = () => grid.textContent ?? '';
    const before = board();
    for (const key of ['{ArrowLeft}', '{ArrowUp}', '{ArrowRight}', '{ArrowDown}', '{ArrowLeft}', '{ArrowUp}']) {
      await userEvent.keyboard(key);
    }
    await waitFor(() => expect(board()).not.toBe(before));
    await waitFor(() => expect(Number(stat(canvasElement, 'Score'))).toBeGreaterThan(0));
    // The legend presses the same moves.
    await userEvent.click(await legend(canvasElement, 'Right'));
    await userEvent.keyboard('n');
    await waitFor(() => expect(stat(canvasElement, 'Score')).toBe('0'));
  },
};

/** The owner leaves after 600 ms and adopts the app again 1.2 s later. */
function ownerLeavesAndReturns(host: StoryAppHost<{ count: number }>) {
  let step = 0;
  return host.stream((state) => {
    step += 1;
    if (step === 1) host.disconnect();
    if (step === 3) host.adopt();
    return { state, ops: [] };
  }, 600);
}

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

/** A refusal shows its word, and the app keeps its state. */
export const Refused: Story = {
  render: () => (
    <OpenApp app={COUNTER} refuse={(action) => (action.action === 'shortcut' ? 'unbound' : undefined)} />
  ),
  play: async ({ canvasElement }) => {
    const canvas = await body(canvasElement);
    await canvas.findByRole('button', { name: 'Plus one' });
    await userEvent.keyboard('{ArrowUp}');
    await expect(await within(canvasElement).findByText('No such key')).toBeVisible();
    await expect(stat(canvasElement, 'Count')).toBe('0');
  },
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

/** A deploy wizard: radios, checkboxes, owner validation and keys that still work after a click. */
export const Wizard: Story = {
  render: () => <OpenApp app={WIZARD} />,
  play: async ({ canvasElement }) => {
    const canvas = await body(canvasElement);
    // The opened app holds the focus, so its keys work before any click.
    await waitFor(() => expect(document.activeElement).toBe(canvasElement.querySelector('[data-app-surface]')));
    // Two keys share the label Up: their names say their keys.
    await expect(await legend(canvasElement, 'Up')).toHaveAccessibleName('Up, key Up arrow');
    await userEvent.click(await canvas.findByRole('radio', { name: /Production/ }));
    await userEvent.click(canvas.getByRole('checkbox', { name: /United States/ }));
    // A key after a click on a checkbox still reaches the app.
    await userEvent.keyboard('n');
    const confirm = await canvas.findByRole('checkbox', { name: /I read the release notes/ });
    // The owner refuses Next until the box is ticked.
    await userEvent.keyboard('n');
    await expect(await canvas.findByText('Read the release notes first.')).toBeVisible();
    await userEvent.click(confirm);
    await userEvent.keyboard('n');
    await expect(await canvas.findByText('Deployed to production')).toBeVisible();
    await userEvent.keyboard('r');
    await expect(await canvas.findByRole('radio', { name: /Staging/ })).toHaveAttribute('aria-checked', 'true');
  },
};

/** The owner goes away and comes back: the app is read only between, then live again. */
export const Reconnect: Story = {
  render: () => <OpenApp app={COUNTER} stream={ownerLeavesAndReturns} />,
  play: async ({ canvasElement }) => {
    const canvas = await body(canvasElement);
    await canvas.findByRole('button', { name: 'Plus one' });
    await expect(await within(canvasElement).findByText('Disconnected', undefined, { timeout: 3000 })).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Plus one' })).toBeDisabled();
    await expect(within(canvasElement).queryByRole('toolbar', { name: 'App keys' })).toBeNull();
    await userEvent.keyboard('+');
    await expect(stat(canvasElement, 'Count')).toBe('0');
    await expect(await within(canvasElement).findByText('Live', undefined, { timeout: 4000 })).toBeVisible();
    await waitFor(() => expect(canvas.getByRole('button', { name: 'Plus one' })).toBeEnabled());
    await userEvent.keyboard('+');
    await waitFor(() => expect(stat(canvasElement, 'Count')).toBe('1'));
  },
};
