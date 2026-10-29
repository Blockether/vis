import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { MENU_VERBS, STORY_SESSION } from '../dev/story-data';
import { TrashIcon } from './icons';
import { Menu, MenuHeading, MenuItem, MenuNote } from './Menu';

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
  title: 'Foundations/Menu',
  component: Menu,
} satisfies Meta<typeof Menu>;

export default meta;

type Story = StoryObj<typeof meta>;

const AT = { top: 96, left: 82 };
const noop = () => {};
const selectedVerb = fn();
const closedMenu = fn();

/** The session row's own verbs, with the row's name in the band. */
export const Default: Story = {
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
export const DestructiveFocused: Story = {
  ...Default,
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const action = page.getByRole('button', { name: 'Delete session' });
    action.focus();
    await expect(action).toHaveFocus();
  },
};
