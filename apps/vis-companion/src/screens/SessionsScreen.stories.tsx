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
      await page.findByRole('navigation', {
        name: 'Pages of uberworkspace sessions',
      }),
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
    // Action counts must not move the permanent controls off the shared edge.
    const project = canvasElement.querySelector('[data-project-root="~/rewrite"]')!;
    // Compact paging stays beside the project identity and creation action on every device.
    let pager = page.getByRole('navigation', {
      name: 'Pages of uberworkspace sessions',
    });
    const fold = page.getByRole('button', { name: 'Collapse uberworkspace' });
    // The project band stays uniform across its disclosure, paging and creation control.
    const header = fold.closest('header')!;
    const menu = within(header).getByRole('button', { name: /^Groups in / });
    // The band carries the boundary; the steps that move the list stand under it, on the
    // set's own header.
    const band = header.parentElement!;
    await expect(win.getComputedStyle(header).borderBottomWidth).toBe('1px');
    const pageValue = (
      within(pager).getByRole('textbox', { name: 'Current page' }) as HTMLInputElement
    ).value;
    await userEvent.click(fold);
    await expect(win.getComputedStyle(header).borderBottomWidth).toBe('1px');
    // THE STEPS LEAVE WITH THE SET THEY MOVE: a folded project paints no list, so it offers
    // no way to page one. The band keeps its own shape and its own controls.
    await expect(within(band).queryByRole('navigation')).toBeNull();
    await userEvent.click(fold);
    pager = await page.findByRole('navigation', { name: 'Pages of uberworkspace sessions' });
    await expect(win.getComputedStyle(header).borderBottomWidth).toBe('1px');
    await expect(pager).not.toHaveAttribute('aria-disabled');
    const pageField = within(pager).getByRole('textbox', { name: 'Current page' });
    // The PLACE is kept: the project comes back standing on the page the reader left it on.
    await expect(pageField).toHaveValue(pageValue);
    await expect(pageField).toBeEnabled();
    await expect(within(pager).getByRole('button', { name: 'Next page' })).toBeEnabled();
    await expect(within(header).queryByRole('button', { name: /^Actions for/ })).toBeNull();
    await expect(header.querySelector('[data-swipe-track]')).toBeNull();
    // The plus stands on the set it creates in, not on the band, so it is found in the
    // project — and only after the fold above, which unmounts it with the list it grows.
    const create = within(project as HTMLElement).getByRole('button', { name: /^New session/ });
    if (win.matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      await userEvent.hover(fold);
      const band = win.getComputedStyle(header).backgroundColor;
      await expect(win.getComputedStyle(fold).backgroundColor).toBe('rgba(0, 0, 0, 0)');
      await userEvent.unhover(fold);
      await userEvent.hover(create);
      await expect(win.getComputedStyle(header).backgroundColor).toBe(band);
      await userEvent.unhover(create);
    }
    const centerY = (node: Element) => {
      const box = node.getBoundingClientRect();
      return box.y + box.height / 2;
    };
    await expect(
      project.querySelectorAll('nav[aria-label="Pages of uberworkspace sessions"]'),
    ).toHaveLength(1);
    // THE BAND KEEPS ITS OWN PAIR on one line — the fold and the menu — and the plus stands
    // on the line of the set it creates in, beside that set's steps.
    await expect(centerY(fold)).toBe(centerY(menu));
    await expect(centerY(create)).toBe(centerY(pager));
    const pointer = win.matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    await expect(fold.getBoundingClientRect().height).toBeGreaterThanOrEqual(pointer ? 28 : 44);
    const qualifier = header.querySelector('[title]')!;
    // Regression: a responsive column span reset the start column and created an
    // implicit track, shifting counts right and squeezing ordinary project names.
    const projectName = within(header).getByText('uberworkspace', { exact: true });
    await expect(qualifier.getBoundingClientRect().left).toBe(
      projectName.getBoundingClientRect().left,
    );
    await expect(projectName.scrollWidth).toBe(projectName.clientWidth);
    // THE BAND KEEPS ONE SHAPE: the steps stand under it, over the set they move, so the
    // caption never has to give way to them.
    await expect(pager.getBoundingClientRect().top).toBeGreaterThanOrEqual(
      header.getBoundingClientRect().bottom,
    );
    const previous = within(pager).getByRole('button', { name: 'Previous page' });
    const next = within(pager).getByRole('button', { name: 'Next page' });
    expect(within(pager).getAllByRole('button')).toHaveLength(2);
    expect(within(pager).queryByRole('button', { name: /^Page \d/ })).toBeNull();
    expect(within(pager).queryByText('…')).toBeNull();
    const current = within(pager).getByRole('textbox', { name: 'Current page' });
    const pageTarget = current.closest('label')!;
    await expect(current).toHaveValue('1');
    for (const control of [previous, next]) {
      await expect(centerY(control)).toBe(centerY(pager));
      // Under a pointer the steps take the band's 24px step: they are navigation beside the
      // set's own count, not a row's own mark. Touch shares the rail's reach.
      await expect(control.getBoundingClientRect().height).toBe(
        pointer ? 24 : create.getBoundingClientRect().height,
      );
    }
    // Include invisible touch reach, not just the small visible arrow faces.
    const reachOf = (control: Element) => {
      const box = control.getBoundingClientRect();
      const reach = win.getComputedStyle(control, '::after');
      const left = reach.content === 'none' ? 0 : Math.min(0, parseFloat(reach.left) || 0);
      const right = reach.content === 'none' ? 0 : Math.min(0, parseFloat(reach.right) || 0);
      const top = reach.content === 'none' ? 0 : Math.min(0, parseFloat(reach.top) || 0);
      const bottom = reach.content === 'none' ? 0 : Math.min(0, parseFloat(reach.bottom) || 0);
      return {
        left: box.left + left,
        right: box.right - right,
        width: box.width - left - right,
        height: box.height - top - bottom,
      };
    };
    // The rail's own marks — the fold and `+` — keep the 28px pointer face; the band's menu
    // and the set's page steps take the 24px step. Touch reaches 44px for all six.
    const railFace = pointer ? 28 : 44;
    const bandStep = pointer ? 24 : 44;
    const bandRow = [fold, menu].map(reachOf);
    const stepRow = [previous, pageTarget, next, create].map(reachOf);
    [fold, create, menu, previous, pageTarget, next].map(reachOf).forEach((target, index) => {
      const minimum = index < 2 ? railFace : bandStep;
      expect(target.width).toBeGreaterThanOrEqual(minimum);
      expect(target.height).toBeGreaterThanOrEqual(minimum);
    });
    // Reported after BLO-167 (paraphrased: a plus standing on the left is unacceptable, the
    // three dots belong on the right): the band's own controls hold its trailing edge and the
    // menu is the last of them. The page steps read left to right on the set's own line.
    for (const row of [bandRow, stepRow]) {
      for (let index = 1; index < row.length; index += 1) {
        expect(row[index].left).toBeGreaterThan(row[index - 1].left);
      }
    }
    for (const control of [fold, create, menu, previous, pageTarget, next]) {
      const box = control.getBoundingClientRect();
      expect(
        control.contains(doc.elementFromPoint(box.x + box.width / 2, box.y + box.height / 2)),
      ).toBe(true);
    }
    expect(bandRow[1].right).toBeLessThanOrEqual(project.getBoundingClientRect().right);
    expect(stepRow[3].right).toBeLessThanOrEqual(project.getBoundingClientRect().right);
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
      // Controls stay transparent on the opaque Sessions set, which prevents rows from
      // showing through when the set header sticks beneath the project band.
      const set = within(project as HTMLElement).getByText('Sessions').parentElement!;
      for (
        let element: HTMLElement | null = create;
        element && element !== set;
        element = element.parentElement
      ) {
        await expect(win.getComputedStyle(element).backgroundColor).toBe('rgba(0, 0, 0, 0)');
      }
      await expect(win.getComputedStyle(set).backgroundColor).not.toBe('rgba(0, 0, 0, 0)');
      return;
    }
    const disclosure = (
      await within(project as HTMLElement).findAllByRole('button', {
        name: /^Show details for/,
      })
    )[0];
    const before = [create, disclosure].map((control) => control.getBoundingClientRect());
    const rowMenu = within(disclosure.closest<HTMLElement>('[data-swipe-track]')!).getByRole(
      'button',
      { name: /^Actions for/ },
    );
    // The project and row menus keep the same trailing edge. The Sessions plus now
    // ends its own strip at that edge, after the pager rather than before it.
    await expect(menu.getBoundingClientRect().right).toBe(rowMenu.getBoundingClientRect().right);
    await expect(before[0].right).toBe(menu.getBoundingClientRect().right);
    await expect(before[1].right).toBeLessThanOrEqual(rowMenu.getBoundingClientRect().left);

    for (const control of [disclosure]) {
      const track = control.closest<HTMLElement>('[data-swipe-track]')!;
      const trigger = within(track).getByRole('button', {
        name: /^Actions for/,
      });
      const content = track.firstElementChild!.firstElementChild!;
      await expect(trigger).toBeVisible();
      await expect(track.scrollWidth).toBe(track.clientWidth);
      await expect(content.getBoundingClientRect().width).toBeGreaterThan(track.clientWidth - 90);
      const start = content.getBoundingClientRect();
      await userEvent.hover(control);
      await expect(content.getBoundingClientRect().width).toBe(start.width);
      for (const button of [trigger, control]) {
        const box = button.getBoundingClientRect();
        await expect(box.width).toBeGreaterThanOrEqual(28);
        await expect(box.height).toBeGreaterThanOrEqual(28);
        await expect(
          button.contains(doc.elementFromPoint(box.x + box.width / 2, box.y + box.height / 2)),
        ).toBe(true);
      }
      await userEvent.click(trigger);
      const menu = within(doc.body).getByRole('dialog');
      await expect(menu).toBeVisible();
      await expect(within(menu).getByRole('button', { name: 'Delete' })).toBeVisible();
      await userEvent.keyboard('{Escape}');
      await expect(trigger).toHaveFocus();
      await expect(within(doc.body).queryByRole('dialog')).not.toBeInTheDocument();
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
      <div className="h-full min-w-80 w-[33%]">
        <Story />
      </div>
    ),
  ],
  play: async (context) => {
    await Fleet.play!(context);
    const page = within(context.canvasElement);
    const screen = page.getByRole('region', { name: 'Sessions' });
    const scroller = screen.querySelector<HTMLElement>('.overflow-y-auto')!;
    const project = page.getByRole('region', {
      name: 'uberworkspace sessions',
    });
    const pager = within(project).getByRole('navigation');
    const pageCount = Number(
      within(pager)
        .getByText(/^Page 1 of /)
        .textContent!.split(' of ')[1],
    );
    const centerY = (node: Element) => {
      const box = node.getBoundingClientRect();
      return box.y + box.height / 2;
    };
    const checkEdges = async () => {
      const box = scroller.getBoundingClientRect();
      for (const band of screen.querySelectorAll('[data-project-root] > div > header')) {
        await expect(band.getBoundingClientRect().left).toBe(box.left);
        await expect(Math.round(band.getBoundingClientRect().right - box.left)).toBe(
          scroller.clientWidth,
        );
      }
      for (const row of project.querySelectorAll('[data-swipe-track]')) {
        await expect(Math.round(row.getBoundingClientRect().right - box.left)).toBe(
          scroller.clientWidth,
        );
      }
    };
    // Regression: the old rail forced pages onto a second line and reserved an empty
    // scrollbar lane even after every project was collapsed.
    await expect(
      screen.getBoundingClientRect().width / context.canvasElement.getBoundingClientRect().width,
    ).toBeCloseTo(0.33, 2);
    await expect(getComputedStyle(scroller).scrollbarGutter).toBe('auto');
    // Regression: hide the scrollbar and its lane, not the scrollable content.
    await expect(getComputedStyle(scroller).scrollbarWidth).toBe('none');
    await expect(getComputedStyle(scroller, '::-webkit-scrollbar').display).toBe('none');
    await expect(getComputedStyle(scroller).overflowY).toBe('auto');
    await expect(scroller.clientWidth).toBe(Math.round(scroller.getBoundingClientRect().width));
    await expect(scroller.scrollHeight).toBeGreaterThan(scroller.clientHeight);
    scroller.scrollTo({ top: 48, behavior: 'instant' });
    await expect(scroller.scrollTop).toBe(48);
    scroller.scrollTo({ top: 0, behavior: 'instant' });
    await expect(scroller.scrollTop).toBe(0);
    await checkEdges();
    const forward = Array.from({ length: pageCount - 1 }, (_, index) => index + 2);
    const backward = Array.from({ length: pageCount - 1 }, (_, index) => pageCount - index - 1);
    let current = 1;
    const pagerCenter = centerY(pager);
    for (const target of [...forward, ...backward]) {
      await userEvent.click(
        within(pager).getByRole('button', {
          name: target > current ? 'Next page' : 'Previous page',
        }),
      );
      current = target;
      await expect(
        await within(pager).findByText(`Page ${target} of ${pageCount}`),
      ).toBeInTheDocument();
      await expect(centerY(pager)).toBe(pagerCenter);
      for (const control of within(pager).getAllByRole('button')) {
        await expect(centerY(control)).toBe(pagerCenter);
      }
      await checkEdges();
    }
    for (const toggle of page.getAllByRole('button', { name: /^Collapse / })) {
      await userEvent.click(toggle);
    }
    await expect(scroller.scrollHeight).toBe(scroller.clientHeight);
    await expect(scroller.clientWidth).toBe(Math.round(scroller.getBoundingClientRect().width));
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
    await page.findByText('uberworkspace', {}, { timeout: 5000 });
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
          expect(screen.getBoundingClientRect().width).toBe(width);
          expect(scroller.getBoundingClientRect().width).toBe(width);
          expect(scroller.clientWidth).toBe(width - 10);
          for (const name of ['infrastructure', 'uberworkspace', 'svar', 'reviewer']) {
            const title = page.getByText(name, { exact: true });
            const header = title.closest('header')!;
            const qualifier = header.querySelector('[title]')!;
            expect(qualifier.getBoundingClientRect().left).toBe(
              title.getBoundingClientRect().left,
            );
            expect(header.getBoundingClientRect().left).toBe(scroller.getBoundingClientRect().left);
            expect(header.getBoundingClientRect().width).toBe(scroller.clientWidth);
            expect(qualifier.getBoundingClientRect().right).toBeLessThanOrEqual(
              header.getBoundingClientRect().right,
            );
            const pager = header.querySelector('nav');
            if (pager) {
              expect(pager.getBoundingClientRect().right).toBeLessThanOrEqual(
                header.getBoundingClientRect().right,
              );
              expect(qualifier.getBoundingClientRect().top).toBeGreaterThanOrEqual(
                pager.getBoundingClientRect().bottom,
              );
            }
          }
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
    await expect(notice.getBoundingClientRect().bottom).toBeLessThanOrEqual(
      machines.getBoundingClientRect().top,
    );
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
    for (const header of headers) {
      await expect(getComputedStyle(header).borderBottomWidth).toBe('1px');
    }
    await expect(getComputedStyle(machine).borderBottomWidth).toBe('0px');
    const last = headers[headers.length - 1];
    await userEvent.click(within(last).getByRole('button', { name: /^Expand / }));
    await expect(getComputedStyle(last).borderBottomWidth).toBe('1px');
    await expect(getComputedStyle(machine).borderBottomWidth).toBe('0px');
  },
};
