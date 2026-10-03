import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_FLEET_CONNS, STORY_NEWER_PROJECT, storyFleetFetch } from '../dev/story-data';
import { machineKey } from '../lib/fleet';
import { groupFoldKey, projectFoldKey, readProjectFold, writeProjectFold } from '../lib/project-fold';
import { SessionsScreen } from './SessionsScreen';

const meta = {
  title: 'Screens/Project hierarchy',
  component: SessionsScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="flex h-dvh w-full max-w-120 flex-col bg-page">
        <Story />
      </div>
    ),
  ],
  beforeEach: () => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch();
    return () => {
      globalThis.fetch = previous;
    };
  },
  args: {
    conns: STORY_FLEET_CONNS,
    primary: STORY_FLEET_CONNS[0],
    query: '',
    onQuery: fn(),
    subscriptions: null,
    onOpen: fn(),
    onSearch: fn(),
    isSearchOpen: false,
    onCloseSearch: fn(),
    isVisible: true,
  },
} satisfies Meta<typeof SessionsScreen>;

export default meta;
type Story = StoryObj<typeof meta>;

export const GroupedSessions: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const title = await page.findByText('infrastructure');
    const group = title.closest<HTMLElement>('[data-project-root]')!;
    const header = title.closest('header')!;
    const collapsed = page.queryByRole('button', { name: 'Expand infrastructure' });
    if (collapsed) await userEvent.click(collapsed);
    await within(group).findByText('Rotate the relay signing key');
    await expect(within(header).queryByRole('button', { name: /^Actions for/ })).toBeNull();
    await expect(header.querySelector('[data-swipe-track]')).toBeNull();

    // Regression: repeated project controls and row controls must stay unframed. Each set
    // under the band carries its own menu, so the menus are found in the group.
    const menus = within(group).getAllByRole('button', { name: /^Actions for (groups|sessions) in / });
    await expect(menus).toHaveLength(2);
    const rows = header.nextElementSibling!;
    // THE SET CARRIES THE LINE between the band and its page: the word is ruled on both
    // edges, the first session under it adds none, and an internal row wears the row's own
    // hairline instead of the band's.
    const set = within(rows as HTMLElement).getByText('Sessions').parentElement!;
    await expect(set.parentElement!.querySelectorAll('[data-session-id]').length).toBeGreaterThan(
      1,
    );
    await userEvent.click(page.getByRole('button', { name: 'Collapse infrastructure' }));
    await expect(group.querySelector('[data-session-id]')).toBeNull();
    await expect(within(header).getByRole('button', { name: 'Expand infrastructure' })).toBeEnabled();
    await userEvent.click(page.getByRole('button', { name: 'Expand infrastructure' }));
    await within(group).findByText('Rotate the relay signing key');
  },
};

export const GroupedSessionsDark: Story = {
  ...GroupedSessions,
  tags: ['!test'],
  globals: { theme: 'blockether-dark' },
};

const STATUS_GROUP = 'group-statuses';
const STATUS_NAME = 'Review work with a long name that must not hide pending input';

/** Group counts stay visible on small phones, with larger text and behind a fold. */
export const GroupStatuses: Story = {
  beforeEach: () => {
    const previous = globalThis.fetch;
    const project = {
      ...STORY_NEWER_PROJECT,
      name: 'Status checks',
      rows: Array.from({ length: 36 }, (_, index) => ({
        ...STORY_NEWER_PROJECT.rows[0],
        id: `group-status-${index}`, title: `Grouped session ${index + 1}`,
        group_id: STATUS_GROUP, live: index < 23, is_awaiting_input: index < 12,
        awaiting_input_count: index < 12 ? 3 : 0,
        answer_count: 6, is_unread: index >= 23, unread_answers: index >= 23 ? 5 : 0,
      })),
      groups: [{ id: STATUS_GROUP, project_id: STORY_NEWER_PROJECT.projectId,
        name: STATUS_NAME, color: 'blue', position: 0, session_count: 36 }],
    };
    const base = machineKey(STORY_FLEET_CONNS[0]);
    const folds = [
      { key: groupFoldKey(base, project.root, STATUS_GROUP), open: false },
      { key: projectFoldKey(base, project.root), open: true },
    ];
    const previousFolds = folds.map(({ key }) => ({ key, open: readProjectFold(key) }));
    for (const { key, open } of folds) writeProjectFold(key, open);
    globalThis.fetch = storyFleetFetch([project]);
    return () => {
      globalThis.fetch = previous;
      for (const { key, open } of previousFolds) writeProjectFold(key, open ?? false);
    };
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const toggle = await page.findByRole('button', { name: `Expand ${STATUS_NAME}` });
    const badges = await Promise.all(['12 HITL', '11 LIVE', '13 NEW'].map((text) =>
      within(toggle).findByText(text),
    ));
    await expect(toggle).toHaveAccessibleDescription(
      '12 sessions need input. 11 live sessions. 13 sessions with new answers.',
    );
    await expect(canvasElement.querySelector('[data-session-id="group-status-0"]')).toBeNull();
    const header = page.getByRole('button', { name: 'Collapse Status checks' }).closest('header')!;
    const projectCounts = ['36 sessions', '12 HITL', '11 LIVE', '13 NEW'].map((text) => within(header).getByText(text));
    const namesAndCounts = [
      { name: within(toggle).getByText(STATUS_NAME), counts: badges, parent: toggle },
      { name: within(header).getByText('Status checks'), counts: projectCounts, parent: header },
    ];
    const screen = page.getByRole('region', { name: 'Sessions' });
    const doc = canvasElement.ownerDocument;
    const previousFontSize = doc.documentElement.style.fontSize;
    const previousWidth = screen.style.width;
    const typeSteps = ['--text-ui', '--text-ui--line-height', '--text-body', '--text-body--line-height', '--text-chip', '--text-chip--line-height'];
    const previousSteps = typeSteps.map((name) => ({ name, value: screen.style.getPropertyValue(name) }));
    try {
      for (const scale of [1, 1.3]) {
        doc.documentElement.style.fontSize = `${16 * scale}px`;
        // The app uses pixel type steps. Scale their tokens, not only rem spacing.
        for (const [index, size] of [11, 16, 12, 18, 8, 14].entries()) {
          screen.style.setProperty(typeSteps[index], `${size * scale}px`);
        }
        for (const width of [320, 375, 393, 626]) {
          screen.style.width = `${width}px`;
          for (const { name, counts, parent } of namesAndCounts) {
            const bounds = parent.getBoundingClientRect();
            // jsdom checks behavior; the browser also checks actual layout.
            if (bounds.width === 0) continue;
            const nameBox = name.getBoundingClientRect();
            await expect(nameBox.width).toBeGreaterThan(0);
            for (const count of counts) {
              const box = count.getBoundingClientRect();
              await expect(box.width).toBeGreaterThan(0);
              await expect(box.left).toBeGreaterThanOrEqual(nameBox.right);
              await expect(box.right).toBeLessThanOrEqual(bounds.right);
              await expect(box.bottom).toBeLessThanOrEqual(bounds.bottom);
            }
          }
        }
      }
    } finally {
      doc.documentElement.style.fontSize = previousFontSize;
      screen.style.width = previousWidth;
      for (const { name, value } of previousSteps) {
        if (value) screen.style.setProperty(name, value);
        else screen.style.removeProperty(name);
      }
    }
    await userEvent.click(toggle);
    const expanded = page.getByRole('button', { name: `Collapse ${STATUS_NAME}` });
    await expect(expanded).toHaveTextContent(/12 HITL.*11 LIVE.*13 NEW/);
    await expect(canvasElement.querySelector('[data-session-id="group-status-0"]')).toHaveAttribute('data-session-id', 'group-status-0');
    await userEvent.click(expanded);
    await expect(page.getByRole('button', { name: `Expand ${STATUS_NAME}` })).toHaveTextContent(
      /12 HITL.*11 LIVE.*13 NEW/,
    );
  },
};
