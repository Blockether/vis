import type { Meta, StoryObj } from '@storybook/react-vite';
import { fn } from 'storybook/test';
import { useState, type ReactNode } from 'react';
import { RECORDING_PEAKS, STORY_MACHINES, STORY_SESSION } from '../dev/story-data';
import { HUMAN_INPUT_CHOICE_MARKS } from '../lib/human-input';
import {
  ArrowDownIcon,
  CircleCheckIcon,
  CircleDotIcon,
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
  BandButton,
  BandLabel,
  BandTally,
  Banner,
  Button,
  Chip,
  ChoiceCell,
  ChoiceRow,
  CloseButton,
  ComposerButton,
  ConfirmRow,
  CopyChip,
  DialogFrame,
  DialogHeader,
  Disclosure,
  EditableName,
  HeaderActions,
  HeaderMeta,
  HeaderTally,
  HeaderTitle,
  IconButton,
  Input,
  KebabButton,
  ListRow,
  LiveCount,
  LiveTally,
  LoadMore,
  machineTagFace,
  MachineMark,
  MachineProjectsButton,
  MachineSwitcher,
  MachineTab,
  Meter,
  MetaButton,
  Modal,
  NewSessionButton,
  NotifyConnectionSwitch,
  OptionRow,
  OverlayScreen,
  Pager,
  Pill,
  ProjectCrumb,
  ProjectStatusCounts,
  PullToSearchHint,
  RowDisclosure,
  SearchField,
  SectionGap,
  SectionHeader,
  SettingsChoiceDisclosure,
  SettingsChoiceGroup,
  SettingsDisclosure,
  Slider,
  Spinner,
  Switch,
  TableSelectionButton,
  TableSelectionRow,
  TextButton,
  UnreadBadge,
  Waveform,
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
 *
 * A control that needs DATA to say anything is drawn beside its own module
 * instead — `ActivityPanel`, `DataTable`, `Media`, `Menu`, `SwipeActions`,
 * `TextArtifact` — and every one of them reads that data from `dev/story-data`,
 * so the fleet, the session and the payloads are the SAME ones in every frame.
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
            onClick: fn(),
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
          dismiss={{ label: 'Dismiss', onClick: fn() }}
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


/** A pick that owns which one is picked, because a sheet has to show both faces. */
function ChoiceRowDemo() {
  const [picked, setPicked] = useState('production');
  const [any, setAny] = useState<string[]>(['tests']);
  const toggle = (value: string) =>
    setAny((on) => (on.includes(value) ? on.filter((v) => v !== value) : [...on, value]));
  return (
    <div className="flex w-full flex-col gap-3">
      <div className="flex flex-col gap-1" role="radiogroup" aria-label="Environment">
        {['production', 'staging'].map((value) => (
          <ChoiceRow
            key={value}
            isOn={picked === value}
            role="radio"
            aria-checked={picked === value}
            mark={
              picked === value
                ? HUMAN_INPUT_CHOICE_MARKS.exclusiveOn
                : HUMAN_INPUT_CHOICE_MARKS.exclusiveOff
            }
            onClick={() => setPicked(value)}
          >
            {value}
          </ChoiceRow>
        ))}
      </div>
      <div className="flex flex-col gap-1" role="group" aria-label="What to run">
        {['tests', 'lint'].map((value) => (
          <ChoiceRow
            key={value}
            isOn={any.includes(value)}
            aria-pressed={any.includes(value)}
            mark={
              any.includes(value)
                ? HUMAN_INPUT_CHOICE_MARKS.inclusiveOn
                : HUMAN_INPUT_CHOICE_MARKS.inclusiveOff
            }
            onClick={() => toggle(value)}
          >
            {value}
          </ChoiceRow>
        ))}
      </div>
    </div>
  );
}

/**
 * The two rows a live run is read through: the `<tr>` says which one is current,
 * the button inside it is what a finger presses and what a screen reader names.
 */
export const Selection: Story = {
  render: () => (
    <Sheet>
      <Group of="ChoiceRow — pick one, then pick any">
        <ChoiceRowDemo />
      </Group>
      <Group of="TableSelectionRow, and the button that fills it">
        <table className="w-full table-fixed">
          <tbody>
            <TableSelectionRow isSelected>
              <td className="p-0 align-top">
                <TableSelectionButton
                  isSelected
                  mark={<CircleDotIcon className="size-3" />}
                  aria-label="Select tests / macos"
                >
                  tests / macos
                </TableSelectionButton>
              </td>
            </TableSelectionRow>
            <TableSelectionRow>
              <td className="p-0 align-top">
                <TableSelectionButton
                  mark={<CircleCheckIcon className="size-3" />}
                  aria-label="Select tests / linux"
                >
                  tests / linux
                </TableSelectionButton>
              </td>
            </TableSelectionRow>
          </tbody>
        </table>
      </Group>
      <Group of="ConfirmRow — the question, and what committing costs">
        <ConfirmRow
          question={`Delete ${STORY_SESSION.title}?`}
          cost="61 turns and every artifact go with it."
          confirmLabel="Delete"
          onKeep={fn()}
          onConfirm={fn()}
        />
      </Group>
    </Sheet>
  ),
};

/** A name that edits in place has to hold the value it is editing. */
function EditableNameDemo() {
  const [name, setName] = useState<string>(STORY_MACHINES[0].name);
  return (
    <EditableName
      value={name}
      label={`Rename ${name}`}
      face={machineTagFace(STORY_MACHINES[0].color)}
      onCommit={setName}
    />
  );
}

function PagerDemo() {
  const [page, setPage] = useState(2);
  return <Pager page={page} pageCount={7} onPage={setPage} label="vis sessions" />;
}

/**
 * WHAT A BAND SAYS ABOUT WHAT IS UNDER IT. Every one of these is INK on the band's
 * own paper — no plates, no second frame — because a header that stacks controls
 * on a coloured band reads as a second toolbar rather than as the list's own name.
 */
export const Bands: Story = {
  render: () => (
    <Sheet>
      <Group of="A machine's own band: the name IS the rename control">
        <HeaderTitle
          mark={<MachineMark color={STORY_MACHINES[0].color} />}
          name={STORY_MACHINES[0].name}
          qualifier={STORY_SESSION.where}
          qualifierTitle={STORY_SESSION.where}
          onRename={fn()}
          renameLabel={`Rename ${STORY_MACHINES[0].name}`}
        />
      </Group>
      <Group of="A project, and the fold that exposes its sessions">
        <ProjectCrumb
          name={STORY_SESSION.project}
          qualifier={STORY_SESSION.where}
          qualifierTitle={STORY_SESSION.where}
          disclosure={{ isOpen: true, onToggle: fn(), label: 'Collapse vis' }}
        />
      </Group>
      <Group of="What a band counts">
        <HeaderMeta>
          <HeaderTally count={STORY_SESSION.turns} unit="turn" />
          <LiveCount count={2} />
          <LiveTally count={2} />
          <UnreadBadge count={4} />
          <BandTally>42</BandTally>
        </HeaderMeta>
      </Group>
      <Group of="States a project can be in, told apart">
        <ProjectStatusCounts live={3} awaiting={1} unread={4} />
        <ProjectStatusCounts live={0} />
      </Group>
      <Group of="The trailing cluster of a row">
        <HeaderActions>
          <RowDisclosure label={`Show details for ${STORY_SESSION.id}`} isOpen={false} />
          <KebabButton label={`Actions for ${STORY_SESSION.id}`} />
        </HeaderActions>
      </Group>
      <Group of="A name that edits in place, and the step through a long list">
        <EditableNameDemo />
        <PagerDemo />
      </Group>
      <Group of="The rule between two sections, which is paper and not a line">
        <div className="w-full">
          <SectionHeader rule="Machines">Machines</SectionHeader>
          <SectionGap />
          <BandLabel>Recent</BandLabel>
        </div>
      </Group>
    </Sheet>
  ),
};

function MachineSwitcherDemo() {
  const [on, setOn] = useState<string>(STORY_MACHINES[0].name);
  return (
    <MachineSwitcher>
      {STORY_MACHINES.map((machine) => (
        <MachineTab
          key={machine.name}
          isOn={on === machine.name}
          hasUnread={machine.unread > 0}
          isDown={machine.isDown}
          label={`Switch to ${machine.name}`}
          title={machine.isDown ? `${machine.name} is not answering` : machine.name}
          onClick={() => setOn(machine.name)}
        >
          {machine.name}
        </MachineTab>
      ))}
    </MachineSwitcher>
  );
}

/**
 * A MACHINE IS A HUE AND A NAME, never a hue alone. Down is the same hue drained
 * to an outline: it is still that computer, and nothing is behind it.
 */
export const Machines: Story = {
  render: () => (
    <Sheet>
      <Group of="MachineMark, two sizes and the state that is not answering">
        {STORY_MACHINES.map((machine) => (
          <MachineMark key={machine.name} color={machine.color} isHollow={machine.isDown} />
        ))}
        <MachineMark color={STORY_MACHINES[0].color} size="banner" />
        <MachineMark color={STORY_MACHINES[2].color} size="banner" isHollow />
      </Group>
      <Group of="The switcher, with news on one tile and a machine that is down">
        <MachineSwitcherDemo />
      </Group>
      <Group of="What a machine's footer offers">
        <NewSessionButton
          machine={STORY_MACHINES[0].name}
          where={STORY_SESSION.where}
          onPress={fn()}
        />
        <NewSessionButton
          machine={STORY_MACHINES[0].name}
          busyLabel="Starting…"
          onPress={fn()}
        />
        <MachineProjectsButton machine={STORY_MACHINES[0].name} onPress={fn()} />
        <MachineProjectsButton machine={STORY_MACHINES[0].name} isQuiet onPress={fn()} />
      </Group>
    </Sheet>
  ),
};

function SettingsChoiceDemo() {
  const [open, setOpen] = useState(true);
  const [engine, setEngine] = useState('piper');
  return (
    <div className="grid w-full grid-cols-1 gap-px bg-dialog-edge">
      <div className="grid bg-input">
        <SettingsChoiceDisclosure
          title="Piper (gateway)"
          sub="ready"
          isSelected={engine === 'piper'}
          isOpen={open}
          controls="story-piper-settings"
          onSelect={() => setEngine('piper')}
          onToggle={() => setOpen((one) => !one)}
        />
      </div>
      <div className="grid bg-input">
        <SettingsChoiceDisclosure
          title="This device"
          sub="system TTS"
          isSelected={engine === 'device'}
          isOpen={false}
          controls="story-device-settings"
          onSelect={() => setEngine('device')}
          onToggle={fn()}
        />
      </div>
    </div>
  );
}

function SliderDemo() {
  const [rate, setRate] = useState(120);
  return (
    <Slider
      min={60}
      max={220}
      value={rate}
      aria-label="Speaking rate"
      onChange={(event) => setRate(Number(event.target.value))}
      className="w-full"
    />
  );
}

function WaveformDemo() {
  const [at, setAt] = useState(0.34);
  return (
    <Waveform
      peaks={RECORDING_PEAKS}
      value={at}
      label="Memo, 1:12"
      onSeek={setAt}
      className="w-full"
    />
  );
}

/** A SETTINGS PANEL IS ROWS, and a row is a question with its answer beside it. */
export const Settings: Story = {
  render: () => (
    <Sheet>
      <Group of="A setting that opens, and the value it already holds">
        <SettingsDisclosure
          label="Voice"
          value="Piper English"
          isOpen={false}
          className="w-full"
        />
      </Group>
      <Group of="A group of choices, one of them opened">
        <SettingsChoiceGroup label="TTS engines">
          <SettingsChoiceDemo />
        </SettingsChoiceGroup>
      </Group>
      <Group of="The switch that speaks for one machine">
        <NotifyConnectionSwitch
          machine={STORY_MACHINES[0].name}
          isOn
          onClick={fn()}
        />
        <NotifyConnectionSwitch
          machine={STORY_MACHINES[1].name}
          isOn={false}
          isChecking
          onClick={fn()}
        />
      </Group>
      <Group of="A bounded number is dragged, and audio is scrubbed">
        <SliderDemo />
        <WaveformDemo />
      </Group>
    </Sheet>
  ),
};

/**
 * EVERY SURFACE THAT OPENS OVER ANOTHER WEARS THE SAME BAND. There is one dialog
 * header in this app; a `fit` sheet is a SIZE of the same modal, not a second one.
 * These stories paint over the whole frame on purpose — a dialog photographed on
 * white paper never shows whether it reads as an interruption.
 */
export const Dialogs: Story = {
  render: () => (
    <Modal size="fit" onDismiss={fn()}>
      <DialogFrame
        title="Delete this session?"
        subtitle={STORY_SESSION.title}
        actions={<BandButton isPrimary>Delete</BandButton>}
        closeLabel="Close the delete dialog"
        onClose={fn()}
      >
        <div className="p-4">
          <ConfirmRow
            question={`Delete ${STORY_SESSION.title}?`}
            cost="61 turns and every artifact go with it."
            confirmLabel="Delete"
            onKeep={fn()}
            onConfirm={fn()}
          />
        </div>
      </DialogFrame>
    </Modal>
  ),
};

/** The full-height dialog: a list inside it gets every pixel the glass has. */
export const DialogFull: Story = {
  render: () => (
    <Modal onDismiss={fn()}>
      <DialogFrame
        title={`Projects on ${STORY_MACHINES[0].name}`}
        subtitle={STORY_SESSION.where}
        actions={<BandButton isPrimary>Add</BandButton>}
        closeLabel="Close the projects dialog"
        onClose={fn()}
      >
        <div className="flex flex-col">
          <ListRow isFramed>vis</ListRow>
          <ListRow isFramed isSelected>
            svar
          </ListRow>
          <ListRow isFramed>infrastructure</ListRow>
        </div>
      </DialogFrame>
    </Modal>
  ),
};

/** An artifact opened over the transcript: the band reports, the body is the file. */
export const Overlay: Story = {
  render: () => (
    <OverlayScreen
      title="fleet.csv"
      subtitle="7 rows × 5 cols · 268 B"
      actions={<BandButton>Download</BandButton>}
      onClose={fn()}
    >
      <div className="p-4">
        <p className="font-mono text-meta text-dialog-hint">
          The artifact stands here, and the band above it is the same one every
          dialog wears.
        </p>
      </div>
    </OverlayScreen>
  ),
};

/** The band alone, which is what four hand-built title bars used to be. */
export const Band: Story = {
  render: () => (
    <Sheet>
      <Group of="DialogHeader, with a subtitle and a cell of its own">
        <div className="w-full">
          <DialogHeader
            title="Settings"
            subtitle={`${STORY_MACHINES[0].name} · protocol 7`}
            actions={<BandButton>Export</BandButton>}
            closeLabel="Close settings"
            onClose={fn()}
          />
        </div>
      </Group>
      <Group of="Stacked over another band, and clearing the notch">
        <div className="w-full">
          <DialogHeader
            title="fleet.csv"
            isStacked
            closeLabel="Close fleet.csv"
            onClose={fn()}
          />
        </div>
      </Group>
    </Sheet>
  ),
};

/** The pull that finds the search field, in the three states it can be in. */
export const Gestures: Story = {
  render: () => (
    <Sheet>
      {(['none', 'pulling', 'armed'] as const).map((phase) => (
        <Group key={phase} of={`PullToSearchHint — ${phase}`}>
          <div className="relative h-16 w-full overflow-hidden bg-level-project">
            <PullToSearchHint phase={phase} />
          </div>
        </Group>
      ))}
    </Sheet>
  ),
};
