import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { liveViewFromWire, type LiveView } from '../lib/live-view';
import { LiveViewPanel } from './LiveView';
import type { AppNodeActions } from './AppNodes';

/**
 * An app attachment for code review (issue #421): a diff with a thread on a line,
 * a saved draft, Send and its receipts. The widths are the phone widths of the
 * issue and a desktop width.
 */
function review(): LiveView {
  const view = liveViewFromWire({
    id: 'app-review',
    title: 'Code review',
    placement: 'attachment',
    seq: 3,
    nodes: [
      {
        id: 'review',
        type: 'diff',
        label: 'Review',
        old_revision: '4537230',
        new_revision: '9f1c2ab',
        patch: [
          'diff --git a/src/app/view/core.clj b/src/app/view/core.clj',
          '--- a/src/app/view/core.clj',
          '+++ b/src/app/view/core.clj',
          '@@ -10,4 +10,5 @@ (ns app.view.core)',
          ' (defn open-live! [session-id view] (record-view session-id (normalize-live-view view)))',
          '-(defn close-live! [id] (sink/close! id))',
          '+(defn close-live! [id reason] (sink/close! id {:reason reason :at (System/currentTimeMillis)}))',
          '+(defn archive-live! [id] (sink/archive! id))',
          ' (defn live-events [id] (:items @(events id)))',
        ].join('\n'),
        threads: [
          {
            id: 't1',
            anchor: { path: 'src/app/view/core.clj', side: 'new', line: 11 },
            messages: [
              {
                id: 'm1',
                author: { kind: 'agent', name: 'Vis' },
                body: 'The close reason is now part of the record.',
                created_at: 1,
              },
            ],
            draft: 'Keep the old arity for one release?',
          },
        ],
      },
    ],
    submissions: [
      {
        id: 's1',
        idempotency_key: 'k1',
        node_id: 'review',
        status: 'answered',
        created_at: 1,
        updated_at: 2,
        comments: [{ node_id: 'review', thread_id: 't1', message_id: 'm0', body: 'Earlier' }],
      },
    ],
  });
  if (!view) throw new Error('the review fixture must be paintable');
  return view;
}

const app: AppNodeActions = {
  isReadOnly: false,
  submissions: review().submissions ?? [],
  draft: fn(async () => ({ status: 'saved' as const, seq: 4 })),
  send: fn(async () => ({ status: 'saved' as const, seq: 5 })),
};

const meta = {
  title: 'Session/Artifacts/Code review',
  component: LiveViewPanel,
  parameters: { layout: 'padded' },
  args: { view: review(), inDialog: true, app },
} satisfies Meta<typeof LiveViewPanel>;

export default meta;

type Story = StoryObj<typeof meta>;

/** jsdom lays nothing out, so layout checks run only in a browser Storybook. */
function hasLayout(element: Element): boolean {
  return element.getBoundingClientRect().width > 0;
}

async function fits(canvasElement: HTMLElement) {
  const canvas = within(canvasElement);
  await expect(canvas.getByText('Keep the old arity for one release?')).toBeInTheDocument();
  if (!hasLayout(canvasElement)) return;
  const panel = canvasElement.querySelector<HTMLElement>('.live-view-panel')!;
  // The panel never scrolls sideways: only the code does.
  await expect(panel.scrollWidth).toBeLessThanOrEqual(panel.clientWidth + 1);
  const lines = canvasElement.querySelector<HTMLElement>('[data-diff-lines]')!;
  await expect(lines.clientWidth).toBeLessThanOrEqual(panel.clientWidth);
  const thread = canvas.getByRole('region', { name: /^Thread on/ });
  const file = thread.closest('section[aria-label]') as HTMLElement;
  // The thread takes the full width of its file under the line.
  await expect(thread.getBoundingClientRect().width).toBeGreaterThan(file.clientWidth * 0.85);
}

function width(px: string) {
  return [
    (Story: () => React.JSX.Element) => (
      <div style={{ width: px }}>
        <Story />
      </div>
    ),
  ];
}

/** A desktop: a keyboard activates a line number and saves a new comment. */
export const Default: Story = {
  decorators: width('64rem'),
  play: async ({ args, canvasElement }) => {
    await fits(canvasElement);
    const canvas = within(canvasElement);
    const line = canvas.getByRole('button', { name: /old line 11$/ });
    line.focus();
    await userEvent.keyboard('{Enter}');
    await userEvent.type(canvas.getByRole('textbox', { name: /^Comment on/ }), 'Why remove it?');
    await userEvent.click(canvas.getByRole('button', { name: 'Save' }));
    await expect(args.app?.draft).toHaveBeenCalledWith('review', {
      anchor: { path: 'src/app/view/core.clj', side: 'old', line: 11 },
      body: 'Why remove it?',
    });
    await expect(await canvas.findByText('Saved')).toBeInTheDocument();
  },
};
