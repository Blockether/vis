import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';

import { STORY_FLEET_CONNS, storyFleetFetch } from '../dev/story-data';
import { SessionsScreen } from './SessionsScreen';

/** The shipped screen over fixture responses, scoped to one story's lifecycle. */

const meta = {
  title: 'Screens/Session list',
  component: SessionsScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="flex h-dvh w-full flex-col bg-page">
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
    isVisible: true,
  },
} satisfies Meta<typeof SessionsScreen>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Four checkouts on one machine, including a paged project on a phone-width rail. */
export const Fleet: Story = {
  decorators: [
    (Story) => (
      <div className="w-[393px]">
        <Story />
      </div>
    ),
  ],
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    // This first assertion waits for the screen's asynchronous gateway fixture.
    await expect(await page.findByText('uberworkspace', {}, { timeout: 5000 })).toBeVisible();
    await expect(await page.findByText('svar')).toBeVisible();
    await expect(await page.findByTitle('~/rewrite')).toBeVisible();
    // Response order must not determine which repository appears first.
    const roots = Array.from(
      canvasElement.querySelectorAll<HTMLElement>('[data-project-root]'),
    ).map((group) => group.dataset.projectRoot);
    await expect(roots).toHaveLength(4);
    await expect(roots).toEqual([...roots].sort());
    const expand = page.queryByRole('button', { name: 'Expand uberworkspace' });
    if (expand) await userEvent.click(expand);
    await expect(
      await page.findByRole('navigation', { name: 'Pages of uberworkspace sessions' }),
    ).toBeVisible();
    const doc = canvasElement.ownerDocument;
    const win = doc.defaultView!;
    // The machine switch and project navigation share a centered, balanced strip.
    const projects = page.getByRole('button', { name: 'Projects on tower' });
    const machines = page.getByRole('group', { name: 'Machines' });
    const strip = machines.parentElement!;
    const stripStyle = win.getComputedStyle(strip);
    if (win.innerWidth < 640) {
      await expect(stripStyle.paddingTop).toBe('12px');
      await expect(stripStyle.paddingBottom).toBe(stripStyle.paddingTop);
    }
    const folderBox = projects.getBoundingClientRect();
    const machinesBox = machines.getBoundingClientRect();
    await expect(folderBox.y + folderBox.height / 2).toBe(machinesBox.y + machinesBox.height / 2);
    // Regression: the project has one hover action, a session has three. Neither
    // strip may move the permanent + / disclosure off the list's shared right edge.
    const project = canvasElement.querySelector('[data-project-root="~/rewrite"]')!;
    const create = within(project as HTMLElement).getByRole('button', { name: /^New session/ });
    const disclosure = (
      await within(project as HTMLElement).findAllByRole('button', {
        name: /^Show details for/,
      })
    )[0];
    if (!win.matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      // The permanent controls must keep the project's paper, not the session panel's.
      await expect(win.getComputedStyle(create.parentElement!.parentElement!).backgroundColor).toBe(
        win.getComputedStyle(create.closest('header')!).backgroundColor,
      );
      return;
    }
    const before = [create, disclosure].map((control) => control.getBoundingClientRect());
    await expect(before[0].right).toBe(before[1].right);

    for (const control of [create, disclosure]) {
      const track = control.closest<HTMLElement>('[data-swipe-track]')!;
      const strip = within(track).getByRole('group', { name: / actions$/ });
      const controls = [control, ...within(strip).getAllByRole('button')];
      const content = track.firstElementChild!.firstElementChild!;
      await userEvent.hover(control);
      control.focus();
      await waitFor(() => expect(win.getComputedStyle(strip).opacity).toBe('1'));
      await expect(track.scrollWidth).toBe(track.clientWidth);
      await expect(content.getBoundingClientRect().width).toBeGreaterThan(0);
      await expect(content.getBoundingClientRect().right).toBeLessThanOrEqual(
        strip.getBoundingClientRect().left,
      );
      await expect(strip.getBoundingClientRect().right).toBeLessThanOrEqual(
        control.getBoundingClientRect().left,
      );
      // Geometry plus hit-testing catches content under a strip, which DOM clicks miss.
      for (const button of controls) {
        const box = button.getBoundingClientRect();
        await expect(box.width).toBeGreaterThanOrEqual(28);
        await expect(box.height).toBeGreaterThanOrEqual(28);
        await expect(
          button.contains(doc.elementFromPoint(box.x + box.width / 2, box.y + box.height / 2)),
        ).toBe(true);
      }
      control.blur();
      await userEvent.unhover(control);
      await waitFor(() => expect(win.getComputedStyle(strip).opacity).toBe('0'));
      await expect(win.getComputedStyle(strip).pointerEvents).toBe('none');
    }
    for (const [index, control] of [create, disclosure].entries()) {
      await expect(control.getBoundingClientRect().x).toBe(before[index].x);
    }
  },
};

/** The same production bands in the desktop sidebar and a full-width list. */
export const NarrowRail: Story = {
  ...Fleet,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  decorators: [
    (Story) => (
      <div className="w-80">
        <Story />
      </div>
    ),
  ],
};

export const Desktop: Story = {
  ...Fleet,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  decorators: [],
};

export const TouchFleet: Story = {
  ...Fleet,
  globals: { viewport: { value: 'phone', isRotated: false } },
};

/** A successful delete must settle even when the fixture keeps same-root rows. */
export const DeleteProject: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const project = await page.findByRole('region', { name: 'uberworkspace sessions' });
    const group = within(project);
    const ask = async () => {
      const create = group.getByRole('button', { name: /^New session/ });
      create.focus();
      await userEvent.hover(create);
      const actions = await group.findByRole('group', { name: 'uberworkspace actions' });
      await waitFor(() => expect(getComputedStyle(actions).pointerEvents).toBe('auto'));
      await userEvent.click(within(actions).getByRole('button', { name: 'Delete' }));
    };

    await ask();
    await expect(group.getByRole('group', { name: 'Delete uberworkspace?' })).toBeVisible();
    await userEvent.click(group.getByRole('button', { name: 'No, keep' }));
    await expect(group.getByRole('group', { name: 'uberworkspace actions' })).toBeInTheDocument();

    await ask();
    await userEvent.click(group.getByRole('button', { name: 'Yes, delete' }));
    // Held same-root rows survive; without the saved name, the band uses its folder name.
    await expect(await page.findByRole('group', { name: 'rewrite actions' })).toBeInTheDocument();
    await expect(page.queryByText('Deleting...')).toBeNull();
    await expect(page.queryByRole('group', { name: 'Delete rewrite?' })).toBeNull();
    await expect(page.getByRole('region', { name: 'reviewer sessions' })).toBeVisible();
  },
};

/** The complete navigator takes the review frame, not a fixed phone-width wrapper. */
export const ResponsiveFleet: Story = {
  play: Fleet.play,
};
