import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState, type ReactNode } from 'react';
import {
  ArrowDownIcon,
  CopyIcon,
  DownloadIcon,
  MicIcon,
  PlusIcon,
  SendIcon,
  SettingsIcon,
  StopIcon,
} from './icons';
import {
  BackButton,
  BandLabel,
  Banner,
  Button,
  Chip,
  ChoiceCell,
  CloseButton,
  ComposerButton,
  CopyChip,
  Disclosure,
  IconButton,
  Input,
  KebabButton,
  ListRow,
  LoadMore,
  Meter,
  MetaButton,
  OptionRow,
  Pill,
  SearchField,
  SectionHeader,
  Spinner,
  Switch,
  TextButton,
  UnreadBadge,
} from './ui';

/**
 * THE VOCABULARY, DRAWN ONCE EACH, BY THE CODE THAT SHIPS IT.
 *
 * The design skill asks a design artifact to open with a component sheet: every
 * control drawn once, at the size the code paints it. This IS that sheet, and it
 * cannot drift — it imports `ui.tsx` rather than describing it, so a story is
 * wrong only when the app is wrong.
 *
 * Read it at two frames. The phone frame is the 32px touch face; the desktop
 * frame is the 24-28px pointer face, because `mouse:` is a media query and the
 * frame answers it. Read it in more than one palette too: `--dialog-title` is the
 * accent in Blockether Dark and is not in Blockether Light, so a fill that reads
 * as chrome in one theme reads as the screen's one accent in the other.
 *
 * What is NOT here: screens. A screen needs real data — 1000 sessions, a truncated
 * title, an offline machine — and that is `npm run dev` against a live gateway.
 * The gallery owns the vocabulary; the dev server owns the product.
 */
const meta = {
  title: 'Vocabulary/Controls',
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

/** One captioned group. The caption is prose ABOUT a control, never a label ON one. */
function Group({ of, children }: { of: string; children: ReactNode }) {
  return (
    <section className="flex flex-col gap-2">
      <p className="font-mono text-meta text-dialog-hint">{of}</p>
      <div className="flex flex-wrap items-center gap-2">{children}</div>
    </section>
  );
}

function Sheet({ children }: { children: ReactNode }) {
  return <div className="flex flex-col gap-5 p-4">{children}</div>;
}

export const Buttons: Story = {
  render: () => (
    <Sheet>
      <Group of="Button, four ranks">
        <Button variant="primary">Connect</Button>
        <Button variant="secondary">Rename</Button>
        <Button variant="quiet">Cancel</Button>
        <Button variant="danger">Delete</Button>
        <Button variant="secondary" disabled>
          Disabled
        </Button>
      </Group>
      <Group of="Button, three densities">
        <Button variant="secondary">Default</Button>
        <Button variant="secondary" density="compact">
          Compact
        </Button>
        <Button variant="secondary" density="panel">
          Panel
        </Button>
      </Group>
      <Group of="The word-only verbs">
        <TextButton>Show more</TextButton>
        <TextButton isToken>claude-opus-5</TextButton>
        <LoadMore label="Load earlier turns" isEarlier>
          Earlier
        </LoadMore>
      </Group>
      <Group of="Pill, the one control that floats over the page">
        <Pill>
          <ArrowDownIcon className="size-3" />
          Latest
        </Pill>
      </Group>
    </Sheet>
  ),
};

export const Marks: Story = {
  render: () => (
    <Sheet>
      <Group of="Icon-only, so the name is spoken">
        <IconButton label="Settings" variant="quiet">
          <SettingsIcon />
        </IconButton>
        <IconButton label="Settings" variant="secondary">
          <SettingsIcon />
        </IconButton>
        <KebabButton label="Actions for tower" />
        <KebabButton label="Actions for tower" isOpen />
      </Group>
      <Group of="Navigation and the ways out">
        <BackButton label="Back to sessions" />
        <CloseButton label="Close the attachment" />
        <CloseButton label="Close settings" isBand />
      </Group>
      <Group of="Work in progress">
        <Spinner />
        <Spinner tone="accent" />
        <Meter value={0.62} label="Context used" />
        <UnreadBadge count={12} />
      </Group>
    </Sheet>
  ),
};

export const Chips: Story = {
  render: () => (
    <Sheet>
      <Group of="Chip, a state that toggles">
        <Chip>All</Chip>
        <Chip isOn>Running</Chip>
      </Group>
      <Group of="CopyChip, two densities">
        <CopyChip value="fd3c03f9" label="Copy session id">
          fd3c03f9
        </CopyChip>
        <CopyChip value="fd3c03f9" label="Copy session id" density="compact">
          fd3c03f9
        </CopyChip>
      </Group>
      <Group of="Band furniture">
        <BandLabel>Recent</BandLabel>
        <SectionHeader rule="Machines">Machines</SectionHeader>
      </Group>
    </Sheet>
  ),
};

/** A field owns its value, so the sheet has to hold one. */
function SearchDemo() {
  const [value, setValue] = useState('');
  return (
    <SearchField
      value={value}
      onValue={setValue}
      label="Search sessions"
      placeholder="Search sessions"
      className="w-full"
    />
  );
}

function SwitchDemo() {
  const [isOn, setIsOn] = useState(true);
  return (
    <Switch
      label="Notify on this machine"
      isOn={isOn}
      onClick={() => setIsOn((on) => !on)}
    />
  );
}

export const Fields: Story = {
  render: () => (
    <Sheet>
      <Group of="Input">
        <Input placeholder="Project name" className="w-full" />
      </Group>
      <Group of="SearchField, the glass inside the box">
        <SearchDemo />
      </Group>
      <Group of="Switch">
        <SwitchDemo />
        <Switch label="Busy" isOn isBusy />
      </Group>
    </Sheet>
  ),
};

export const Rows: Story = {
  render: () => (
    <Sheet>
      <Group of="ListRow, a plane and not a control">
        <ListRow className="w-full">A session that is not selected</ListRow>
        <ListRow className="w-full" isSelected>
          The selected one
        </ListRow>
        <ListRow className="w-full" isFramed>
          Framed, inside a panel
        </ListRow>
      </Group>
      <Group of="ChoiceCell, with the action that belongs to the row">
        <ChoiceCell
          className="w-full"
          title="Piper English"
          sub="downloading, 42%"
          isSelected={false}
          leadingAction={{
            label: 'Download Piper English',
            icon: <DownloadIcon className="size-3" />,
            onClick: () => undefined,
          }}
        />
        <ChoiceCell className="w-full" title="System voice" sub="ready" isSelected />
      </Group>
      <Group of="Disclosure and OptionRow">
        <Disclosure className="w-full" isOpen={false}>
          Thinking
        </Disclosure>
        <OptionRow className="w-full" isActive>
          Reasoning, high
        </OptionRow>
      </Group>
    </Sheet>
  ),
};

/**
 * The composer strip, in the order the screen builds it: what goes INTO the
 * message on the left, what happens to the TURN on the right. The stop slot is
 * reserved whether or not a turn is running, so the strip never changes width.
 */
export const Composer: Story = {
  render: () => (
    <Sheet>
      <Group of="Tones of the one composer control">
        <ComposerButton label="Attach">
          <PlusIcon className="size-3.5" />
        </ComposerButton>
        <ComposerButton label="Dictate">
          <MicIcon />
        </ComposerButton>
        <ComposerButton label="Voice conversation" tone="voice">
          <MicIcon />
        </ComposerButton>
        <ComposerButton label="Stop recording" tone="recording">
          <MicIcon />
        </ComposerButton>
        <ComposerButton label="Stop response" tone="stop">
          <StopIcon className="size-3 text-err" />
        </ComposerButton>
        <ComposerButton label="Send message" tone="send">
          <SendIcon className="size-3.5" />
        </ComposerButton>
        <ComposerButton label="Send message" tone="send" disabled>
          <SendIcon className="size-3.5" />
        </ComposerButton>
      </Group>
      <Group of="The meta line under it">
        <MetaButton isPicker>claude-opus-5</MetaButton>
        <MetaButton>high</MetaButton>
      </Group>
    </Sheet>
  ),
};

export const Feedback: Story = {
  render: () => (
    <Sheet>
      <Group of="Banner, four kinds">
        <Banner kind="neutral">The machine is paired and idle.</Banner>
      </Group>
      <Group of=" ">
        <Banner kind="ok" title="Signed in">
          The provider accepted the device code.
        </Banner>
      </Group>
      <Group of=" ">
        <Banner kind="warn">Quota is nearly spent on this plan.</Banner>
      </Group>
      <Group of=" ">
        <Banner
          kind="err"
          title="Authentication rejected"
          dismiss={{ label: 'Dismiss', onClick: () => undefined }}
        >
          Sign in again to keep this provider.
        </Banner>
      </Group>
      <Group of="A copy that already happened">
        <CopyChip value="done" label="Copy the id" density="compact">
          <CopyIcon className="size-3" />
        </CopyChip>
      </Group>
    </Sheet>
  ),
};
