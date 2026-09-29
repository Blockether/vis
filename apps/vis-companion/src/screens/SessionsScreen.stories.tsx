import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_FLEET_CONNS, STORY_GATEWAYS, storyFleetFetch } from '../dev/story-data';
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
    isSearchOpen: false,
    onCloseSearch: fn(),
    isVisible: true,
  },
} satisfies Meta<typeof SessionsScreen>;

export default meta;

type Story = StoryObj<typeof meta>;

/** A phone-width strip keeps machines and the icon-only Projects action on one line. */
export const FlatMachineBarCompact: Story = {
  args: {
    conns: [{ ...STORY_FLEET_CONNS[0], alts: ['https://gateway.example.com'] }, STORY_GATEWAYS[1]],
  },
  decorators: [
    (Story) => (
      <div className="w-[393px]">
        <Story />
      </div>
    ),
  ],
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    await page.findByRole('button', { name: 'tower' });
    const other = page.getByRole('button', { name: 'macbook-pro-16-work' });
    const projects = page.getByRole('button', { name: 'Projects on tower' });
    await expect(projects).not.toHaveTextContent('Projects');
    await expect(projects).toHaveClass('border-0');
    await expect(other).toHaveClass('bg-level-project');
    await expect(page.queryByRole('group', { name: /Addresses on/ })).toBeNull();
  },
};

/** A wide strip keeps the same controls in a single row without address cards. */
export const FlatMachineBarWide: Story = {
  args: {
    conns: [{ ...STORY_FLEET_CONNS[0], alts: ['https://gateway.example.com'] }, STORY_GATEWAYS[1]],
  },
  decorators: [
    (Story) => (
      <div className="w-[900px]">
        <Story />
      </div>
    ),
  ],
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const projects = page.getByRole('button', { name: 'Projects on tower' });
    await expect(await page.findByRole('button', { name: 'macbook-pro-16-work' })).toBeVisible();
    await expect(projects).not.toHaveTextContent('Projects');
    await expect(page.queryByRole('group', { name: /Addresses on/ })).toBeNull();
  },
};

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
    await expect(await page.findByText('uberworkspace')).toBeVisible();
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
    await expect(within(header).queryByRole('button', { name: /^Actions for/ })).toBeNull();
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

/** The same production bands in the desktop sidebar and a full-width list. */
export const NarrowRail: Story = {
  ...Fleet,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  decorators: [
    (Story) => (
      <div className="h-full min-w-80 w-[33%]">
        <Story />
      </div>
    ),
  ],
  play: async (context) => {
    const page = within(context.canvasElement);
    const screen = page.getByRole('region', { name: 'Sessions' });
    const project = page.getByRole('region', {
      name: 'uberworkspace sessions',
    });
    const pager = within(project).getByRole('navigation');
    const pageCount = Number(
      within(pager)
        .getByText(/^Page 1 of /)
        .textContent!.split(' of ')[1],
    );
    const checkEdges = async () => {
      const bands = screen.querySelectorAll('[data-project-root] > header');
      // Count the bands so a changed structure cannot leave this check matching nothing.
      await expect(bands).toHaveLength(screen.querySelectorAll('[data-project-root]').length);
    };
    await checkEdges();
    // Forward reaches every page and both ends of the pager; one step back covers the other
    // control. Walking every page back again only repeated the same layouts.
    const walk = [...Array.from({ length: pageCount - 1 }, (_, index) => index + 2), pageCount - 1];
    let current = 1;
    for (const target of walk) {
      await userEvent.click(
        within(pager).getByRole('button', {
          name: target > current ? 'Next page' : 'Previous page',
        }),
      );
      current = target;
      await expect(
        await within(pager).findByText(`Page ${target} of ${pageCount}`),
      ).toBeInTheDocument();
      await checkEdges();
    }
    for (const toggle of page.getAllByRole('button', { name: /^Collapse / })) {
      await userEvent.click(toggle);
    }
    await checkEdges();
  },
};

export const Desktop: Story = {
  ...Fleet,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  decorators: [],
};

/** Resting rows for the real-pointer audit; interaction playback must not open a menu. */
export const DesktopHover: Story = {
  ...Desktop,
  play: undefined,
};

export const TouchFleet: Story = {
  ...Fleet,
  tags: ['!test'],
  globals: { viewport: { value: 'phone', isRotated: false } },
};

/** A successful delete must settle even when the fixture keeps same-root rows. */
export const DeleteProject: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    await page.findByRole('region', { name: 'uberworkspace sessions' });
    await userEvent.click(page.getByRole('button', { name: 'Projects on tower' }));
    const sheet = within(
      await within(canvasElement.ownerDocument.body).findByRole('dialog', {
        name: 'Manage projects on tower',
      }),
    );
    const ask = async () => {
      await userEvent.click(
        sheet.getByRole('button', {
          name: 'Remove every transcript in uberworkspace',
        }),
      );
    };

    await ask();
    await expect(sheet.getByRole('group', { name: 'Delete uberworkspace?' })).toBeVisible();
    await userEvent.click(sheet.getByRole('button', { name: 'No, keep' }));
    await expect(
      sheet.getByRole('button', {
        name: 'Remove every transcript in uberworkspace',
      }),
    ).toBeVisible();

    await ask();
    await userEvent.click(sheet.getByRole('button', { name: 'Yes, delete' }));
    await expect(sheet.queryByRole('group', { name: 'Delete uberworkspace?' })).toBeNull();
    await userEvent.keyboard('{Escape}');
    // Held same-root rows survive; without the saved name, the band uses its folder name.
    await expect(await page.findByRole('region', { name: 'rewrite sessions' })).toBeVisible();
    await expect(page.queryByText('Deleting...')).toBeNull();
    await expect(page.getByRole('region', { name: 'reviewer sessions' })).toBeVisible();
  },
};

/** The complete navigator takes the review frame, not a fixed phone-width wrapper. */
export const ResponsiveFleet: Story = {
  play: Fleet.play,
};

/** Counts retain the project-name column on small phones and with larger text. */
export const MobileProjectAlignment: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    await page.findByText('uberworkspace');
    const screen = page.getByRole('region', { name: 'Sessions' });
    const doc = canvasElement.ownerDocument;
    const previousFontSize = doc.documentElement.style.fontSize;
    const previousWidth = screen.style.width;
    const scroller = screen.querySelector<HTMLElement>('.overflow-y-auto')!;
    // A classic scrollbar reserves space on Linux; macOS normally overlays it.
    // Exercise the same narrower content box on both platforms.
    const scrollbarStyle = doc.createElement('style');
    scrollbarStyle.textContent = `
      [data-alignment-scrollbar] { scrollbar-width: auto !important; scrollbar-color: auto !important; }
      [data-alignment-scrollbar]::-webkit-scrollbar { display: block !important; width: 10px; }
    `;
    scroller.dataset.alignmentScrollbar = '';
    doc.head.append(scrollbarStyle);
    try {
      for (const scale of [1, 1.3]) {
        doc.documentElement.style.fontSize = `${16 * scale}px`;
        for (const width of [320, 375, 393]) {
          screen.style.width = `${width}px`;
        }
      }
    } finally {
      doc.documentElement.style.fontSize = previousFontSize;
      screen.style.width = previousWidth;
      delete scroller.dataset.alignmentScrollbar;
      scrollbarStyle.remove();
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

export const SharingPhone: Story = {
  ...Sharing,
  tags: ['!test'],
  globals: { viewport: { value: 'phone', isRotated: false } },
};

export const SharingDesktop: Story = {
  ...Sharing,
  globals: { viewport: { value: 'desktop', isRotated: false } },
  decorators: [
    (Story) => (
      <div className="h-full min-w-80 w-[33%]">
        <Story />
      </div>
    ),
  ],
};

/** The viewport seam must not scroll away or add a second layout border. */
export const FixedViewportSeam: Story = {
  ...NarrowRail,
  tags: ['!test'],
  play: async ({ canvasElement }) => {
    const screen = within(canvasElement).getByRole('region', { name: 'Sessions' });
    const scroller = screen.querySelector<HTMLElement>('.overflow-y-auto')!;
    const viewport = scroller.parentElement!;
    const top = viewport.getBoundingClientRect().top;
    await expect(scroller.scrollHeight).toBeGreaterThan(scroller.clientHeight);
    for (const offset of [0, 48, scroller.scrollHeight]) {
      scroller.scrollTo({ top: offset, behavior: 'instant' });
      await expect(scroller.scrollTop).toBe(
        Math.min(offset, scroller.scrollHeight - scroller.clientHeight),
      );
      const seam = getComputedStyle(viewport, '::before');
      await expect(seam.position).toBe('absolute');
      await expect(seam.top).toBe('0px');
      await expect(seam.borderTopWidth).toBe('1px');
      await expect(seam.borderTopStyle).toBe('solid');
      await expect(seam.zIndex).toBe('20');
      await expect(seam.pointerEvents).toBe('none');
      await expect(viewport.getBoundingClientRect().top).toBe(top);
      await expect(scroller.getBoundingClientRect().top).toBe(top);
      await expect(Math.round(parseFloat(seam.width))).toBe(viewport.clientWidth);
    }
  },
};

/** A collapsed final project must not stack its edge with a machine frame. */
export const CollapsedProjectEdges: Story = {
  ...Desktop,
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const machine = await page.findByRole('region', { name: 'tower projects' });
    for (const toggle of page.getAllByRole('button', { name: /^Collapse / })) {
      await userEvent.click(toggle);
    }
    const headers = machine.querySelectorAll('header');
    await expect(headers.length).toBeGreaterThan(1);
    const last = headers[headers.length - 1];
    await userEvent.click(within(last).getByRole('button', { name: /^Expand / }));
  },
};
