import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, within } from 'storybook/test';

import { liveViewFromWire, type LiveView } from '../lib/live-view';
import { AppListRow } from './AppAttachment';

/** One app of the session list in each state that a card shows. */
function app(id: string, title: string, over: Record<string, unknown> = {}): LiveView {
  const view = liveViewFromWire({
    id,
    title,
    placement: 'attachment',
    seq: 1,
    nodes: [{ id: 'break', type: 'divider' }],
    ...over,
  });
  if (!view) throw new Error('story app must parse');
  return view;
}

const APPS = [
  app('app-live', 'Code review'),
  app('app-stale', 'Release form', { is_stale: true }),
  app('app-closed', 'Deploy monitor', { is_closed: true }),
];

const meta = {
  title: 'Session/Live View/App cards',
  component: AppListRow,
  args: { view: APPS[0], onOpen: fn() },
  render: (args) => (
    <ul aria-label="Apps" className="w-full max-w-md">
      {APPS.map((view) => (
        <li key={view.id}>
          <AppListRow view={view} onOpen={args.onOpen} />
        </li>
      ))}
    </ul>
  ),
} satisfies Meta<typeof AppListRow>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Each card names its app and its state, and a press opens that app. */
export const States: Story = {
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByRole('button', { name: 'Open app Code review' })).toHaveTextContent('Live');
    await expect(canvas.getByRole('button', { name: 'Open app Release form' })).toHaveTextContent(
      'Disconnected',
    );
    await expect(canvas.getByRole('button', { name: 'Open app Deploy monitor' })).toHaveTextContent('Closed');
    canvas.getByRole('button', { name: 'Open app Deploy monitor' }).click();
    await expect(args.onOpen).toHaveBeenCalledWith(APPS[2]);
  },
};
