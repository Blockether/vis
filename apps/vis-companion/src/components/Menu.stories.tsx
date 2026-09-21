import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { MENU_VERBS, STORY_SESSION } from '../dev/story-data';
import { ForkIcon, ProjectsIcon, StarIcon, TrashIcon } from './icons';
import { Menu, MenuBack, MenuHeading, MenuItem, MenuNote } from './Menu';
import { BandButton } from './ui';

/**
 * THE RARER VERBS OF ONE ROW, HUNG UNDER THE `⋯` THAT ASKED FOR THEM.
 *
 * It is a popover placed at the anchor's own box at EVERY width: a phone gets the
 * panel a desktop gets, clamped to the screen, not a sheet sliding up from the
 * bottom edge of it.
 *
 * A menu is a LIST OF VERBS, never a form: each row is a title, an optional fact
 * on the same line, and at most one hint under it. When a verb needs a second
 * step, the panel walks — `MenuBack` replaces the band and the same panel holds
 * the step, so nothing is stacked over the panel.
 */
const meta = {
  title: 'Components/Menu',
  component: Menu,
} satisfies Meta<typeof Menu>;

export default meta;

type Story = StoryObj<typeof meta>;

const AT = { top: 96, left: 82 };
const noop = () => {};
const selectedVerb = fn();
const closedMenu = fn();

/** The session row's own verbs, with the row's name in the band. */
export const Verbs: Story = {
  args: {
    label: `Actions for ${STORY_SESSION.title}`,
    at: AT,
    onDismiss: noop,
    children: (
      <>
        <MenuHeading closeLabel="Close the session menu" onClose={closedMenu}>
          {STORY_SESSION.title}
        </MenuHeading>
        {MENU_VERBS.map((verb, index) => (
          <MenuItem
            key={verb.title}
            title={verb.title}
            meta={'meta' in verb ? verb.meta : undefined}
            hint={'hint' in verb ? verb.hint : undefined}
            badge={'badge' in verb ? verb.badge : undefined}
            onSelect={index === 0 ? selectedVerb : noop}
          />
        ))}
        <MenuItem
          title="Delete session"
          tone="danger"
          icon={<TrashIcon className="size-3.5" />}
          onSelect={noop}
        />
        <MenuNote>Deleting is immediate on {STORY_SESSION.machine}.</MenuNote>
      </>
    ),
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(page.getByRole('button', { name: /Open on tower/ }));
    await expect(selectedVerb).toHaveBeenCalledOnce();
    await userEvent.click(page.getByRole('button', { name: 'Close the session menu' }));
    await expect(closedMenu).toHaveBeenCalledOnce();
  },
};

// Keep the destructive focus paint mounted for the all-theme contrast scan.
export const DangerFocused: Story = {
  ...Verbs,
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const action = page.getByRole('button', { name: 'Delete session' });
    action.focus();
    await expect(action).toHaveFocus();
  },
};

/** A band that also COMMITS: the cells stand beside the way out, never a Button. */
export const BandWithCells: Story = {
  args: {
    label: 'Projects on tower',
    at: AT,
    onDismiss: noop,
    children: (
      <>
        <MenuHeading
          closeLabel="Close the projects menu"
          onClose={noop}
          cells={<BandButton isPrimary>Add</BandButton>}
        >
          Projects on tower
        </MenuHeading>
        <MenuItem
          title="vis"
          meta="42 sessions"
          icon={<ProjectsIcon className="size-3.5" />}
          onSelect={noop}
        />
        <MenuItem title="svar" meta="6 sessions" onSelect={noop} />
        <MenuItem title="infrastructure" meta="2 sessions" onSelect={noop} />
      </>
    ),
  },
};

/** A second step inside the same panel: the band becomes the way back. */
export const Stepped: Story = {
  args: {
    label: 'Fork this session',
    at: AT,
    onDismiss: noop,
    children: (
      <>
        <MenuBack label="Back to the session menu" onBack={noop}>
          Fork from turn 61
        </MenuBack>
        <MenuItem
          title="Fork on tower"
          meta="2 live"
          icon={<ForkIcon className="size-3.5" />}
          onSelect={noop}
        />
        <MenuItem
          title="Fork on macbook-pro-16-work"
          hint="Idle since 11:57"
          icon={<ForkIcon className="size-3.5" />}
          onSelect={noop}
        />
        <MenuItem
          title="Star the original"
          icon={<StarIcon className="size-3.5" />}
          onSelect={noop}
        />
      </>
    ),
  },
};

/** Nothing to offer is still an answer, and it is a sentence rather than a blank. */
export const Empty: Story = {
  args: {
    label: 'Projects on mini',
    at: AT,
    onDismiss: noop,
    children: (
      <>
        <MenuHeading closeLabel="Close the projects menu" onClose={noop}>
          Projects on mini
        </MenuHeading>
        <MenuNote>mini is not answering, so it has no projects to offer.</MenuNote>
      </>
    ),
  },
};

/**
 * A menu with no room to drop STANDS on its anchor instead: the panel's own foot is
 * pinned just above the control, whatever height the list inside it turns out to be.
 */
export const StandingOnItsAnchor: Story = {
  args: {
    label: 'Projects on tower',
    at: { left: 82, bottom: 240 },
    onDismiss: noop,
    children: (
      <>
        <MenuHeading>Projects on tower</MenuHeading>
        <MenuItem
          title="vis"
          meta="42 sessions"
          icon={<ProjectsIcon className="size-3.5" />}
          onSelect={noop}
        />
        <MenuItem title="svar" meta="6 sessions" onSelect={noop} />
      </>
    ),
  },
  play: async ({ canvasElement }) => {
    const win = canvasElement.ownerDocument.defaultView!;
    const panel = within(canvasElement.ownerDocument.body).getByRole('dialog', {
      name: 'Projects on tower',
    });
    const paint = win.getComputedStyle(panel);
    // ONE vertical edge is pinned, and a panel standing above its anchor pins its
    // foot. This project runs at PHONE width, where the panel was once a sheet docked
    // across the bottom edge and `at` was ignored: it holds its anchor's column and
    // its own foot here too. The geometry behind it is `anchored-menu.test.ts`.
    await expect(win.innerWidth).toBeLessThan(640);
    await expect(paint.bottom).toBe('240px');
    await expect(paint.left).toBe('82px');
    await expect(panel.getBoundingClientRect().width).toBe(Math.min(320, win.innerWidth - 24));
    await expect(paint.getPropertyValue('--menu-top').trim()).toBe('');
  },
};

/**
 * A window with room on neither side of the anchor: the panel still hangs off the
 * control that opened it and gives up HEIGHT instead of its place, scrolling inside
 * the cap the placement hands it.
 */
export const SqueezedAgainstItsAnchor: Story = {
  args: {
    label: 'Projects on relay',
    at: { left: 82, bottom: 240, maxHeight: 180 },
    onDismiss: noop,
    children: (
      <>
        <MenuHeading>Projects on relay</MenuHeading>
        <MenuItem title="vis" meta="42 sessions" onSelect={noop} />
        <MenuItem title="svar" meta="6 sessions" onSelect={noop} />
      </>
    ),
  },
  play: async ({ canvasElement }) => {
    const win = canvasElement.ownerDocument.defaultView!;
    const panel = within(canvasElement.ownerDocument.body).getByRole('dialog', {
      name: 'Projects on relay',
    });
    const paint = win.getComputedStyle(panel);
    // Same phone width as above, and the cap the placement handed the panel is the
    // one it wears — never the height of the screen it used to fill from the bottom.
    await expect(paint.maxHeight).toBe('180px');
    await expect(paint.bottom).toBe('240px');
    await expect(paint.getPropertyValue('--menu-top').trim()).toBe('');
  },
};
