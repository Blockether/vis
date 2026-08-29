import type { Meta, StoryObj } from '@storybook/react-vite';
import { MENU_VERBS, STORY_SESSION } from '../dev/story-data';
import { ForkIcon, ProjectsIcon, StarIcon, TrashIcon } from './icons';
import { Menu, MenuBack, MenuHeading, MenuItem, MenuNote } from './Menu';
import { BandButton } from './ui';

/**
 * THE RARER VERBS OF ONE ROW, HUNG UNDER THE `⋯` THAT ASKED FOR THEM.
 *
 * From `sm:` up it is a popover placed at the anchor's own box; on a phone it is a
 * sheet from the bottom edge, and `at` is ignored — which is why the frame you read
 * this in changes the control and not just its width.
 *
 * A menu is a LIST OF VERBS, never a form: each row is a title, an optional fact
 * on the same line, and at most one hint under it. When a verb needs a second
 * step, the panel walks — `MenuBack` replaces the band and the same panel holds
 * the step, so nothing is stacked over a phone sheet.
 */
const meta = {
  title: 'Components/Menu',
  component: Menu,
} satisfies Meta<typeof Menu>;

export default meta;

type Story = StoryObj<typeof meta>;

const AT = { top: 96, left: 360 };

/** The session row's own verbs, with the row's name in the band. */
export const Verbs: Story = {
  args: {
    label: `Actions for ${STORY_SESSION.title}`,
    at: AT,
    onDismiss: () => undefined,
    children: (
      <>
        <MenuHeading closeLabel="Close the session menu" onClose={() => undefined}>
          {STORY_SESSION.title}
        </MenuHeading>
        {MENU_VERBS.map((verb) => (
          <MenuItem
            key={verb.title}
            title={verb.title}
            meta={'meta' in verb ? verb.meta : undefined}
            hint={'hint' in verb ? verb.hint : undefined}
            badge={'badge' in verb ? verb.badge : undefined}
            onSelect={() => undefined}
          />
        ))}
        <MenuItem
          title="Delete session"
          tone="danger"
          icon={<TrashIcon className="size-3.5" />}
          onSelect={() => undefined}
        />
        <MenuNote>Deleting is immediate on {STORY_SESSION.machine}.</MenuNote>
      </>
    ),
  },
};

/** A band that also COMMITS: the cells stand beside the way out, never a Button. */
export const BandWithCells: Story = {
  args: {
    label: 'Projects on tower',
    at: AT,
    onDismiss: () => undefined,
    children: (
      <>
        <MenuHeading
          closeLabel="Close the projects menu"
          onClose={() => undefined}
          cells={<BandButton isPrimary>Add</BandButton>}
        >
          Projects on tower
        </MenuHeading>
        <MenuItem
          title="vis"
          meta="42 sessions"
          icon={<ProjectsIcon className="size-3.5" />}
          onSelect={() => undefined}
        />
        <MenuItem title="svar" meta="6 sessions" onSelect={() => undefined} />
        <MenuItem title="infrastructure" meta="2 sessions" onSelect={() => undefined} />
      </>
    ),
  },
};

/** A second step inside the same panel: the band becomes the way back. */
export const Stepped: Story = {
  args: {
    label: 'Fork this session',
    at: AT,
    onDismiss: () => undefined,
    children: (
      <>
        <MenuBack label="Back to the session menu" onBack={() => undefined}>
          Fork from turn 61
        </MenuBack>
        <MenuItem
          title="Fork on tower"
          meta="2 live"
          icon={<ForkIcon className="size-3.5" />}
          onSelect={() => undefined}
        />
        <MenuItem
          title="Fork on macbook-pro-16-work"
          hint="Idle since 11:57"
          icon={<ForkIcon className="size-3.5" />}
          onSelect={() => undefined}
        />
        <MenuItem
          title="Star the original"
          icon={<StarIcon className="size-3.5" />}
          onSelect={() => undefined}
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
    onDismiss: () => undefined,
    children: (
      <>
        <MenuHeading closeLabel="Close the projects menu" onClose={() => undefined}>
          Projects on mini
        </MenuHeading>
        <MenuNote>mini is not answering, so it has no projects to offer.</MenuNote>
      </>
    ),
  },
};
