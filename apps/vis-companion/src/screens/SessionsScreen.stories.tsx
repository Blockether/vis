import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_FLEET_CONNS, STORY_GATEWAYS, storyFleetFetch } from '../dev/story-data';
import { SessionsScreen } from './SessionsScreen';

/** The shipped screen over fixture responses, scoped to one story's lifecycle. */

const meta = {
  title: 'Session list/Screen',
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
    isSearchOpen: false,
    onCloseSearch: fn(),
    isVisible: true,
  },
} satisfies Meta<typeof SessionsScreen>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Four checkouts on one machine, including a paged project on a phone-width rail. */
export const Default: Story = {
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
    await expect(await page.findByText('uberworkspace')).toBeVisible();
    await expect(await page.findByText('svar')).toBeVisible();
    // Response order must not determine which repository appears first.
    const roots = Array.from(
      canvasElement.querySelectorAll<HTMLElement>('[data-project-root]'),
    ).map((group) => group.dataset.projectRoot);
    await expect(roots).toHaveLength(4);
    await expect(roots).toEqual([...roots].sort());
    const expand = page.queryByRole('button', { name: 'Expand uberworkspace' });
    if (expand) await userEvent.click(expand);
    await expect(
      await page.findByRole('navigation', {
        name: 'Pages of uberworkspace sessions',
      }),
    ).toBeVisible();
    const doc = canvasElement.ownerDocument;
    const win = doc.defaultView!;
    // Action counts must not move the permanent controls off the shared edge.
    const project = canvasElement.querySelector('[data-project-root="~/rewrite"]')!;
    // Compact paging stays beside the project identity and creation action on every device.
    let pager = page.getByRole('navigation', {
      name: 'Pages of uberworkspace sessions',
    });
    const fold = page.getByRole('button', { name: 'Collapse uberworkspace' });
    // The project band stays uniform across its disclosure and the paging under it.
    const header = fold.closest('header')!;
    // The band carries the boundary; the steps that move the list stand under it, on the
    // set's own header.
    const band = header.parentElement!;
    const pageValue = (
      within(pager).getByRole('textbox', { name: 'Current page' }) as HTMLInputElement
    ).value;
    await userEvent.click(fold);
    // THE STEPS LEAVE WITH THE SET THEY MOVE: a folded project paints no list, so it offers
    // no way to page one. The band keeps its own shape and its own controls.
    await expect(within(band).queryByRole('navigation')).toBeNull();
    await userEvent.click(fold);
    pager = await page.findByRole('navigation', { name: 'Pages of uberworkspace sessions' });
    await expect(pager).not.toHaveAttribute('aria-disabled');
    const pageField = within(pager).getByRole('textbox', { name: 'Current page' });
    // The PLACE is kept: the project comes back standing on the page the reader left it on.
    await expect(pageField).toHaveValue(pageValue);
    await expect(pageField).toBeEnabled();
    await expect(within(pager).getByRole('button', { name: 'Next page' })).toBeEnabled();
    const bandMenus = within(header).getAllByRole('button', { name: /^Actions for/ });
    await expect(bandMenus).toHaveLength(1);
    await expect(bandMenus[0]).toHaveAccessibleName('Actions for uberworkspace');
    await expect(header.querySelector('[data-swipe-track]')).toBeNull();
    // The set menu stands on the set it acts on, not on the band, so it is found in the
    // project — and only after the fold above, which unmounts it with the list it serves.
    const actions = within(project as HTMLElement).getByRole('button', {
      name: /^Actions for sessions in /,
    });
    if (win.matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      await userEvent.hover(fold);
      await userEvent.unhover(fold);
      await userEvent.hover(actions);
      await userEvent.unhover(actions);
    }
    await expect(
      project.querySelectorAll('nav[aria-label="Pages of uberworkspace sessions"]'),
    ).toHaveLength(1);
    const previous = within(pager).getByRole('button', { name: 'Previous page' });
    const next = within(pager).getByRole('button', { name: 'Next page' });
    expect(within(pager).getAllByRole('button')).toHaveLength(2);
    expect(within(pager).queryByRole('button', { name: /^Page \d/ })).toBeNull();
    expect(within(pager).queryByText('…')).toBeNull();
    const current = within(pager).getByRole('textbox', { name: 'Current page' });
    const pageTarget = current.closest('label')!;
    await expect(current).toHaveValue('1');
    await expect(previous).toBeDisabled();
    await expect(next).toBeVisible();
    const firstPageRows = [...project.querySelectorAll('[data-session-id]')].map((row) =>
      row.getAttribute('data-session-id'),
    );
    await userEvent.click(next);
    await expect(await within(pager).findByText(/^Page 2 of /)).toHaveAttribute(
      'aria-live',
      'polite',
    );
    await expect(fold).toHaveAttribute('aria-expanded', 'true');
    expect(
      [...project.querySelectorAll('[data-session-id]')].map((row) =>
        row.getAttribute('data-session-id'),
      ),
    ).not.toEqual(firstPageRows);
    await userEvent.click(next);
    await expect(await within(pager).findByText(/^Page 3 of /)).toBeInTheDocument();
    await userEvent.click(previous);
    await expect(await within(pager).findByText(/^Page 2 of /)).toBeInTheDocument();
    await userEvent.click(previous);
    await expect(await within(pager).findByText(/^Page 1 of /)).toBeInTheDocument();
    await userEvent.click(pageTarget);
    await userEvent.keyboard('3{Enter}');
    await expect(await within(pager).findByText(/^Page 3 of /)).toBeInTheDocument();
    expect(
      [...project.querySelectorAll('[data-session-id]')].map((row) =>
        row.getAttribute('data-session-id'),
      ),
    ).not.toEqual(firstPageRows);
    await userEvent.click(pageTarget);
    await userEvent.keyboard('1{Enter}');
    await expect(await within(pager).findByText(/^Page 1 of /)).toBeInTheDocument();
    expect(
      [...project.querySelectorAll('[data-session-id]')].map((row) =>
        row.getAttribute('data-session-id'),
      ),
    ).toEqual(firstPageRows);
    if (!win.matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      return;
    }
    const disclosure = (
      await within(project as HTMLElement).findAllByRole('button', {
        name: /^Show details for/,
      })
    )[0];

    for (const control of [disclosure]) {
      const track = control.closest<HTMLElement>('[data-swipe-track]')!;
      const trigger = within(track).getByRole('button', {
        name: /^Actions for/,
      });
      await expect(trigger).toBeVisible();
      await userEvent.hover(control);
      await userEvent.click(trigger);
      const menu = within(doc.body).getByRole('dialog');
      await expect(menu).toBeVisible();
      await expect(within(menu).getByRole('button', { name: 'Delete' })).toBeVisible();
      await userEvent.keyboard('{Escape}');
      await expect(trigger).toHaveFocus();
      await expect(within(doc.body).queryByRole('dialog')).not.toBeInTheDocument();
    }
  },
};

/** A share stays above the destination switch while the reader chooses another machine. */
export const Sharing: Story = {
  args: {
    conns: STORY_GATEWAYS.slice(0, 2),
    share: {
      files: [{ path: '/cache/PLAN.md', name: 'PLAN.md', type: 'text/markdown' }],
    },
    onDiscardShare: fn(),
  },
  play: async ({ canvasElement, args }) => {
    const page = within(canvasElement);
    const notice = (await page.findByText('Sharing')).closest('[role="status"]')!;
    const machines = page.getByRole('group', { name: 'Machines' });
    const tabs = within(machines).getAllByRole('button');
    await userEvent.click(tabs[1]);
    await expect(tabs[1]).toHaveAttribute('aria-pressed', 'true');
    await expect(notice).toBeVisible();
    await expect(notice).toHaveTextContent('PLAN.md');
    await userEvent.click(page.getByRole('button', { name: 'Discard the share' }));
    await expect(args.onDiscardShare).toHaveBeenCalledOnce();
  },
};
