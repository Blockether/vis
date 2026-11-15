import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, within } from 'storybook/test';

import { liveViewFromWire, type LiveView } from '../lib/live-view';
import { AppListRow } from './AppAttachment';
import { APP_ICONS } from '../lib/view';

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
  app('app-live', 'Code review', { icon: 'review' }),
  app('app-stale', 'Release form', { is_stale: true, icon: 'checklist' }),
  app('app-closed', 'Deploy monitor', { is_closed: true, icon: 'rocket' }),
];

const meta = {
  title: 'Session/Live view/App cards',
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
export const AllStates: Story = {
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

/** Every app icon at the size used in the app list. */
export const IconCatalog: Story = {
  render: (args) => (
    <ul aria-label="App icon catalog" className="grid grid-cols-1 gap-x-6 sm:grid-cols-2 lg:grid-cols-4">
      {APP_ICONS.map((icon) => (
        <li key={icon}>
          <AppListRow view={app(`icon-${icon}`, icon, { icon })} onOpen={args.onOpen} />
        </li>
      ))}
    </ul>
  ),
};
