import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import { useRef, useState, type ReactNode } from 'react';
import { STORY_MACHINES, STORY_SESSION } from '../dev/story-data';
import { HUMAN_INPUT_CHOICE_MARKS } from '../lib/human-input';
import { Markdown } from './ChatContent';
import { OverlayScreen } from './DocArtifact';
import {
  CheckIcon,
  DownloadIcon,
  MicIcon,
  PlusIcon,
  RefreshIcon,
  SendIcon,
  SearchIcon,
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
  Checkbox,
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
  ExecutionAction,
  IconButton,
  Input,
  ListRow,
  LoadMore,
  MetaButton,
  Modal,
  NotifyConnectionSwitch,
  OptionRow,
  SettingsChoiceDisclosure,
  SettingsChoiceGroup,
  SettingsDisclosure,
  SidebarToggle,
  Spinner,
  Select,
  Switch,
  Text,
  TextButton,
  ViewHeading,
  ViewLayout,
  ViewParagraph,
} from './ui';
import {
  HeaderActions,
  HeaderMeta,
  HeaderTally,
  HeaderTitle,
  MachineGap,
  MachineProjectsButton,
  MachineSwitcher,
  MachineTab,
  NewSessionButton,
  Pager,
  ProjectCrumb,
  ProjectStatusCounts,
  PullToSearchHint,
  RowDisclosure,
  SectionHeader,
} from './SessionNavigator';

/**
 * THE VOCABULARY, DRAWN ONCE EACH, BY THE CODE THAT SHIPS IT.
 *
 * This component sheet draws each control once using `ui.tsx`, at the size
 * the app renders it.
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

const noop = () => {};

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

/** Reading roles share one family without giving every line the same emphasis. */
export const Typography: Story = {
  render: () => (
    <Sheet>
      <DialogHeader title="Settings" />
      <Text as="h3" variant="heading">
        Application
      </Text>
      <Text as="h4" variant="section">
        Transcript
      </Text>
      <div className="space-y-1">
        <Text as="p" variant="label">
          Show Python code and results
        </Text>
        <Text as="p" variant="description">
          Show source code and raw results before Activity. Turn off to show only Activity.
        </Text>
        <Text as="p" variant="meta">
          Signed-in session · expires in 6 days
        </Text>
      </div>
      <ChoiceCell title="Blockether Light" sub="Selected theme" isSelected isLeaf />
      <ChoiceCell title="Blockether Dark" sub="Available theme" isSelected={false} isLeaf />
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const title = canvas.getByRole('heading', { name: 'Settings' });
    const heading = canvas.getByRole('heading', { name: 'Application' });
    const section = canvas.getByRole('heading', { name: 'Transcript' });
    const label = canvas.getByText('Show Python code and results');
    const description = canvas.getByText(/^Show source code/);
    const metadata = canvas.getByText(/^Signed-in session/);
    const size = (element: Element) => parseFloat(getComputedStyle(element).fontSize);
    await expect(size(title)).toBeGreaterThan(size(heading));
    await expect(size(heading)).toBe(size(label));
    await expect(size(label)).toBeGreaterThan(size(description));
    await expect(size(section)).toBe(size(description));
    await expect(size(description)).toBeGreaterThan(size(metadata));
    for (const element of [title, heading, section, label, description, metadata]) {
      await expect(getComputedStyle(element).fontFamily).toBe(getComputedStyle(title).fontFamily);
      await expect(getComputedStyle(element).textTransform).toBe('none');
      await expect(getComputedStyle(element).letterSpacing).toBe('normal');
    }
    await expect(getComputedStyle(heading).fontWeight).toBe('600');
    await expect(getComputedStyle(label).fontWeight).toBe('500');
    await expect(getComputedStyle(description).fontWeight).toBe('400');
    const selected = canvas.getByRole('button', { name: /Blockether Light/ });
    await expect(getComputedStyle(canvas.getByText('Selected theme')).color).toBe(
      getComputedStyle(selected).color,
    );
    await expect(getComputedStyle(canvas.getByText('Blockether Dark')).fontWeight).toBe('400');
  },
};

export const TypographyPointer: Story = {
  ...Typography,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** Icons stay unframed in every state; glyph strokes are not CSS borders. */
async function expectUnframedIcon(button: HTMLElement) {
  for (const element of [button, ...button.querySelectorAll('*')]) {
    if (element.closest('svg')) continue;
    const style = getComputedStyle(element);
    for (const side of ['Top', 'Right', 'Bottom', 'Left'] as const) {
      await expect(parseFloat(style[`border${side}Width`])).toBe(0);
    }
    await expect(parseFloat(style.borderRadius)).toBe(0);
    await expect(style.boxShadow).toBe('none');
  }
}

/** RUN is a launch target, not a collapse control. */
export const ExecutionLaunch: Story = {
  render: () => (
    <Sheet>
      <Group of="Open a live view or its saved record">
        <ExecutionAction aria-label="Open run Build pool" onClick={noop}>
          <BandLabel>RUN</BandLabel>
          <span>Build pool</span>
        </ExecutionAction>
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const launch = canvas.getByRole('button', { name: 'Open run Build pool' });
    await expect(launch).not.toHaveAttribute('aria-expanded');
    await expect(launch.querySelector('svg')).toBeNull();
    await userEvent.click(launch);
    await expect(launch).toHaveFocus();
  },
};

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
        <TextButton isBand>Interrupt</TextButton>
        <LoadMore label="Load earlier turns" onClick={() => {}}>
          Earlier
        </LoadMore>
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    for (const name of [
      'Connect',
      'Rename',
      'Cancel',
      'Delete',
      'Disabled',
      'Default',
      'Compact',
      'Panel',
    ]) {
      const button = canvas.getByRole('button', { name });
      await expect(button.getBoundingClientRect().height).toBe(pointer ? 28 : 32);
      if (!pointer) {
        const reach = getComputedStyle(button, '::after');
        await expect(parseFloat(reach.height)).toBeGreaterThanOrEqual(44);
      }
    }
    await expect(canvas.getByRole('button', { name: 'Disabled' })).toBeDisabled();
    await userEvent.tab();
    await expect(canvas.getByRole('button', { name: 'Connect' })).toHaveFocus();
  },
};

export const Marks: Story = {
  render: () => (
    <Sheet>
      <Group of="Icon-only, so the name is spoken">
        {(['primary', 'secondary', 'quiet', 'danger', 'overlay', 'remove'] as const).map(
          (variant) => (
            <span key={variant} className="flex gap-2">
              <IconButton label={`${variant} action`} variant={variant}>
                <SettingsIcon />
              </IconButton>
              <IconButton label={`${variant} unavailable`} variant={variant} disabled>
                <SettingsIcon />
              </IconButton>
            </span>
          ),
        )}
      </Group>
      <Group of="Band chrome, named by its mark">
        <div className="flex min-h-12 justify-end bg-dialog-title text-dialog-title-foreground">
          <BandButton label="Refresh models">
            <RefreshIcon />
          </BandButton>
          <BandButton label="Save changes" isPrimary>
            <CheckIcon />
          </BandButton>
          <BandButton label="Save unavailable" isPrimary disabled>
            <CheckIcon />
          </BandButton>
        </div>
      </Group>
      <Group of="Navigation and the ways out">
        <BackButton label="Back to sessions" />
        <SidebarToggle isShown />
        <SidebarToggle isShown={false} />
        <CloseButton label="Close the attachment" />
        <CloseButton label="Remove queued message" isStandalone />
        <CloseButton label="Close settings" isBand />
      </Group>
      <Group of="Work in progress">
        <Spinner />
        <Spinner tone="accent" />
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    // Regression: circles and row-end borders must not return around any icon.
    for (const button of canvas.getAllByRole('button')) {
      await expectUnframedIcon(button);
    }
    await expect(canvas.getByRole('button', { name: 'Save unavailable' })).toBeDisabled();
    const refresh = canvas.getByRole('button', { name: 'Refresh models' });
    refresh.focus();
    await expect(refresh).toHaveFocus();
    await expectUnframedIcon(refresh);
    const focusMark = getComputedStyle(refresh, '::before');
    await expect(parseFloat(focusMark.height)).toBeGreaterThan(0);
    await userEvent.keyboard('{Enter}');
  },
};

export const CodeCopy: Story = {
  render: () => (
    <Sheet>
      <Markdown>
        {[
          'After starting the new version, retry:',
          '',
          '```bash',
          'vis-agent python uv sync --project ./einmal --locked',
          '```',
          '',
          'Then run `/reload`. The gateway was not restarted.',
          '',
          '```diff',
          '--- a/config.txt',
          '+++ b/config.txt',
          '@@ -1 +1 @@',
          '-before',
          '+after',
          '```',
        ].join('\n')}
      </Markdown>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    // Storybook supplies a clipboard stub; native clipboard behavior is not exercised here.
    const user = userEvent.setup();
    const buttons = canvas.getAllByRole('button', { name: 'Copy code' });
    const codeRegions = canvas.getAllByRole('region', { name: /code$/i });
    for (const region of codeRegions) {
      const button = region.parentElement!.querySelector('button')!;
      button.focus();
      await user.tab();
      await expect(region).toHaveFocus();
    }
    for (const button of buttons) {
      await expect(button.textContent).toBe('');
      const box = button.getBoundingClientRect();
      const blockElement = button.closest('.relative.bg-code')!;
      const block = blockElement.getBoundingClientRect();
      // Long, horizontally scrolling code must not show through the copy icon.
      await expect(getComputedStyle(button.parentElement!).backgroundColor).toBe(
        getComputedStyle(blockElement).backgroundColor,
      );
      await expect(box.left).toBeGreaterThan(block.left + block.width / 2);
      await expect(box.right).toBeLessThanOrEqual(block.right);
      await expect(box.top).toBeGreaterThanOrEqual(block.top);
      await expect(box.bottom).toBeLessThanOrEqual(block.bottom);
    }
    await user.click(buttons[0]);
    await expect(canvas.getByRole('button', { name: 'Copied' })).toHaveAttribute('title', 'Copied');
  },
};

/** Clipboard access is stubbed; the production control owns loading and feedback. */
export const AsyncCopy: Story = {
  render: () => <CopyChip value={async () => 'Complete activity history'} label="Copy activity" />,
  play: async ({ canvas }) => {
    const user = userEvent.setup();
    await user.click(canvas.getByRole('button', { name: 'Copy activity' }));
    await expect(await canvas.findByRole('button', { name: 'Copied' })).toBeEnabled();
    await expect(navigator.clipboard.readText()).resolves.toBe('Complete activity history');
  },
};

export const CopyPending: Story = {
  render: () => (
    <CopyChip
      value={(signal) =>
        new Promise((_resolve, reject) => {
          signal.addEventListener('abort', () => reject(signal.reason));
        })
      }
      label="Copy activity"
    />
  ),
  play: async ({ canvas }) => {
    const user = userEvent.setup();
    await user.click(canvas.getByRole('button', { name: 'Copy activity' }));
    await expect(canvas.getByRole('button', { name: 'Copying…' })).toBeDisabled();
    await expect(canvas.getByRole('button', { name: 'Copying…' })).toHaveAttribute(
      'aria-busy',
      'true',
    );
  },
};

export const CopyUnavailable: Story = {
  render: () => (
    <CopyChip
      value={async () => {
        throw new Error('Clipboard access denied.');
      }}
      label="Copy activity"
    />
  ),
  play: async ({ canvas }) => {
    const user = userEvent.setup();
    await user.click(canvas.getByRole('button', { name: 'Copy activity' }));
    await expect(await canvas.findByRole('alert')).toHaveTextContent('Clipboard access denied.');
    await expect(canvas.getByRole('button', { name: 'Copy failed. Try again.' })).toBeEnabled();
  },
};

export const Chips: Story = {
  render: () => (
    <Sheet>
      <Group of="Chip, a state that toggles">
        <Chip>All</Chip>
        <Chip isOn>Running</Chip>
      </Group>
      <Group of="CopyChip, icon-only and labeled">
        <CopyChip value="vis-agent python uv sync --project ./einmal --locked" label="Copy code" />
        <CopyChip value="fd3c03f9" label="Copy session id">
          fd3c03f9
        </CopyChip>
        <CopyChip value="fd3c03f9" label="Copy session id" density="compact">
          fd3c03f9
        </CopyChip>
      </Group>
      <Group of="CopyChip, execution-band target">
        <div className="flex min-h-11 items-center gap-2 mouse:min-h-7">
          <BandLabel>Activity</BandLabel>
          <CopyChip
            value="ACTIVITY\n\ngrep [succeeded]\n  2 matches"
            label="Copy activity"
            density="compact"
            edge
          />
        </div>
      </Group>
      <Group of="Band furniture">
        <BandLabel>Recent</BandLabel>
        <SectionHeader>Machines</SectionHeader>
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const icon = canvas.getByRole('button', { name: 'Copy code' });
    await icon.ownerDocument.fonts.ready;
    const height = icon.getBoundingClientRect().height;
    for (const button of [
      canvas.getByRole('button', { name: 'All' }),
      canvas.getByRole('button', { name: 'Running' }),
      ...canvas.getAllByRole('button', { name: 'Copy session id' }),
    ]) {
      await expect(button.getBoundingClientRect().height).toBe(height);
      if (!matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
        await expect(parseFloat(getComputedStyle(button, '::after').height)).toBeGreaterThanOrEqual(
          44,
        );
      }
    }
  },
};

export const ChipsPointer: Story = {
  ...Chips,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

function SwitchDemo() {
  const [isOn, setIsOn] = useState(true);
  return <Switch label="Notify on this machine" isOn={isOn} onClick={() => setIsOn((on) => !on)} />;
}

function SearchFieldDemo() {
  const [value, setValue] = useState('timeout');
  const input = useRef<HTMLInputElement>(null);
  return (
    <Input
      ref={input}
      type="search"
      aria-label="Search output"
      placeholder="Search output"
      value={value}
      onChange={(event) => setValue(event.target.value)}
      className="flex-1"
      icon={<SearchIcon className="size-3" />}
      action={
        value ? (
          <CloseButton
            label="Clear search"
            onClick={() => {
              setValue('');
              input.current?.focus();
            }}
          />
        ) : null
      }
    />
  );
}

export const Fields: Story = {
  render: () => (
    <Sheet>
      <Group of="Input with an icon action">
        <Input aria-label="Project name" placeholder="Project name" className="min-w-0 flex-1" />
        <IconButton label="Add project">
          <PlusIcon className="size-3" />
        </IconButton>
        <Button variant="secondary">Create</Button>
      </Group>
      <Group of="Voice settings input">
        <Input aria-label="Voice name" placeholder="Voice name" className="min-w-0 flex-1" />
        <Button variant="secondary" density="panel">
          Save voice
        </Button>
      </Group>
      <Group of="Search input">
        <SearchFieldDemo />
        <Button variant="secondary" density="panel">
          Find
        </Button>
      </Group>
      <Group of="Password, read-only and disabled inputs">
        <Input type="password" aria-label="API key" defaultValue="example-key" className="flex-1" />
        <Input aria-label="Machine address" defaultValue="127.0.0.1" readOnly className="flex-1" />
        <Input
          aria-label="Unavailable voice"
          placeholder="Voice unavailable"
          disabled
          className="flex-1"
        />
      </Group>
      <Group of="Switch">
        <SwitchDemo />
        <Switch label="Busy" isOn isBusy />
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const input = canvas.getByRole('textbox', { name: 'Project name' });
    await input.ownerDocument.fonts.ready;
    for (const [fieldName, actionNames] of [
      ['Project name', ['Add project', 'Create']],
      ['Voice name', ['Save voice']],
      ['Search output', ['Find']],
      ['API key', []],
      ['Machine address', []],
      ['Unavailable voice', []],
    ] as const) {
      const fieldInput = canvas.getByLabelText(fieldName);
      const field = fieldInput.getBoundingClientRect();
      // Regression: form inputs must match across locations, not only their own action.
      await expect(field.height).toBeCloseTo(input.getBoundingClientRect().height, 0);
      for (const name of actionNames) {
        const button = canvas.getByRole('button', { name });
        const action = button.getBoundingClientRect();
        await expect(action.top).toBeCloseTo(field.top, 0);
        await expect(action.height).toBeCloseTo(field.height, 0);
        if (name !== 'Add project') {
          await expect(getComputedStyle(fieldInput).fontSize).toBe(
            getComputedStyle(button).fontSize,
          );
        }
      }
    }
    await userEvent.type(input, 'Companion');
    await expect(input).toHaveValue('Companion');
    const search = canvas.getByRole('searchbox', { name: 'Search output' });
    await userEvent.click(canvas.getByRole('button', { name: 'Clear search' }));
    await expect(search).toHaveValue('');
    await expect(search).toHaveFocus();
    await userEvent.type(search, 'gateway');
    await expect(search).toHaveValue('gateway');
    await userEvent.type(canvas.getByLabelText('Machine address'), 'changed');
    await expect(canvas.getByLabelText('Machine address')).toHaveValue('127.0.0.1');
    await expect(canvas.getByLabelText('Unavailable voice')).toBeDisabled();
    if (!matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      const box = input.getBoundingClientRect();
      const strip = input.ownerDocument.elementFromPoint(box.left + box.width / 2, box.bottom + 5);
      await expect(strip).toBe(input.parentElement);
      await userEvent.click(strip!);
      await expect(input).toHaveFocus();
      const upperStrip = input.ownerDocument.elementFromPoint(
        box.left + box.width / 2,
        box.top - 5,
      );
      await expect(upperStrip).toBe(input.parentElement);
    }
    const notifications = canvas.getByRole('switch', {
      name: /^Notify on this machine/,
    });
    await expect(notifications).toHaveAttribute('aria-checked', 'true');
    await userEvent.click(notifications);
    await expect(notifications).toHaveAttribute('aria-checked', 'false');
  },
};

export const FieldsPointer: Story = {
  ...Fields,
  globals: { viewport: { value: 'desktop', isRotated: false } },
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
        <ListRow className="w-full" density="compact">
          Compact band, touch-safe
        </ListRow>
      </Group>
      <Group of="ChoiceCell, with the action that belongs to the row">
        <div className="w-full" role="group" aria-label="Voices">
          <ChoiceCell
            className="w-full"
            title="Piper English"
            sub="downloading, 42%"
            isSelected={false}
            leadingAction={{
              label: 'Download Piper English',
              icon: <DownloadIcon className="size-3" />,
              onClick: noop,
            }}
          />
          <ChoiceCell className="w-full" title="System voice" sub="ready" isSelected />
        </div>
      </Group>
      <Group of="Disclosure and OptionRow">
        <Disclosure className="w-full" isOpen={false}>
          Thinking
        </Disclosure>
        <Disclosure className="w-full" isOpen={false} tone="chronology">
          Searched · 18 matches
        </Disclosure>
        <Disclosure className="w-full" isOpen={false} tone="execution">
          Read ×8 · 6 files
        </Disclosure>
        <Disclosure className="w-full" isOpen={false} tone="execution" inlineChevron>
          CODE +12 more
        </Disclosure>
        <div className="isolate w-full">
          <Disclosure isOpen={false} tone="execution" density="compact">
            Read src/config.ts
          </Disclosure>
        </div>
        <Disclosure className="w-full" isOpen tone="execution" inlineChevron>
          RESULT
        </Disclosure>
        <div className="w-full" role="listbox" aria-label="Reasoning effort">
          <OptionRow className="w-full" isActive>
            Reasoning, high
          </OptionRow>
        </div>
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    await expectUnframedIcon(canvas.getByRole('button', { name: 'Download Piper English' }));
  },
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
      <Group of="Holding and voice overlays">
        <ComposerButton label="Switching microphone" isHolding>
          <MicIcon />
        </ComposerButton>
        <ComposerButton label="Voice overlay" tone="voice" surface="overlay">
          <MicIcon />
        </ComposerButton>
        <ComposerButton label="Recording overlay" tone="recording" surface="overlay">
          <MicIcon />
        </ComposerButton>
      </Group>
      <Group of="The meta line under it">
        <MetaButton isPicker>claude-opus-5</MetaButton>
        <MetaButton>high</MetaButton>
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    // Regression: attach and microphone faces were narrower than send/stop.
    for (const name of [
      'Attach',
      'Dictate',
      'Voice conversation',
      'Stop recording',
      'Stop response',
      'Send message',
      'Switching microphone',
      'Voice overlay',
      'Recording overlay',
    ]) {
      for (const button of canvas.getAllByRole('button', { name })) {
        const box = button.getBoundingClientRect();
        await expect(box.width).toBe(box.height);
        await expectUnframedIcon(button);
        if (name.endsWith('overlay')) {
          await expect(box.width).toBe(44);
        } else {
          // Strip reach is one pitch wide — the 32px box plus the strip's 4px gap —
          // and 44px tall, so neighbours tile instead of covering each other.
          const reach = getComputedStyle(button, '::after');
          if (reach.content !== 'none') {
            await expect(parseFloat(reach.width)).toBe(36);
            await expect(parseFloat(reach.height)).toBe(44);
          }
        }
      }
    }
    for (const name of ['claude-opus-5', 'high']) {
      const button = canvas.getByRole('button', { name });
      await expect(button.getBoundingClientRect().height).toBe(
        canvas.getByRole('button', { name: 'Attach' }).getBoundingClientRect().height,
      );
    }
  },
};

export const Feedback: Story = {
  render: () => (
    <Sheet>
      <Group of="Banner, four kinds">
        <Banner kind="neutral">The machine is paired and idle.</Banner>
      </Group>
      <Group of="Untitled success">
        <Banner kind="ok">The issue was saved.</Banner>
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
          dismiss={{ label: 'Dismiss', onClick: noop }}
        >
          Sign in again to keep this provider.
        </Banner>
      </Group>
      <Group of="Icon-only copy">
        <CopyChip value="done" label="Copy the id" density="compact" />
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    await expectUnframedIcon(canvas.getByRole('button', { name: 'Copy the id' }));
  },
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
          <Checkbox key={value} isOn={any.includes(value)} onClick={() => toggle(value)}>
            {value}
          </Checkbox>
        ))}
        <Checkbox isOn={false} disabled>
          Request a signing key that is unavailable on this device
        </Checkbox>
      </div>
    </div>
  );
}

export const Selection: Story = {
  render: () => (
    <Sheet>
      <Group of="ChoiceRow and Checkbox — choose one or any">
        <ChoiceRowDemo />
      </Group>
      <Group of="ConfirmRow — the question, and what committing costs">
        <ConfirmRow
          question={`Delete ${STORY_SESSION.title}?`}
          cost="61 turns and every artifact go with it."
          confirmLabel="Delete"
          onKeep={noop}
          onConfirm={noop}
        />
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const staging = canvas.getByRole('radio', { name: 'staging' });
    await userEvent.click(staging);
    await expect(staging).toHaveAttribute('aria-checked', 'true');

    const lint = canvas.getByRole('checkbox', { name: 'lint' });
    await userEvent.click(lint);
    await expect(lint).toHaveAttribute('aria-checked', 'true');
    await expect(
      canvas.getByRole('checkbox', {
        name: 'Request a signing key that is unavailable on this device',
      }),
    ).toBeDisabled();
  },
};

/** The production header's name edits in place. */
function HeaderRenameDemo() {
  const [name, setName] = useState<string>(STORY_MACHINES[0].name);
  return <HeaderTitle name={name} onRename={setName} renameLabel={`Rename ${name}`} />;
}

function PagerDemo({
  initialPage = 1,
  disabled = false,
}: {
  initialPage?: number;
  disabled?: boolean;
}) {
  const [page, setPage] = useState(initialPage);
  return (
    <Pager page={page} pageCount={104} onPage={setPage} label="vis sessions" disabled={disabled} />
  );
}

// The same compact counter and explicit steps on phones, tablets and desktop rails.
export const ProjectPages: Story = {
  render: () => (
    <Sheet>
      <PagerDemo />
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const pager = canvas.getByRole('navigation', { name: 'Pages of vis sessions' });
    const previous = canvas.getByRole('button', { name: 'Previous page' });
    const next = canvas.getByRole('button', { name: 'Next page' });
    const current = canvas.getByRole('textbox', { name: 'Current page' }) as HTMLInputElement;
    const pageTarget = current.closest('label')!;
    await expect(previous).toBeVisible();
    await expect(previous).toBeDisabled();
    await expect(next).toBeVisible();
    await expect(current).toHaveValue('1');
    expect(within(pager).getAllByRole('button')).toHaveLength(2);
    expect(canvas.queryByRole('button', { name: /^Page \d/ })).toBeNull();
    expect(canvas.queryByText('…')).toBeNull();
    await userEvent.click(next);
    await expect(canvas.getByText('Page 2 of 104')).toHaveAttribute('aria-live', 'polite');
    await expect(next).toHaveFocus();
    await userEvent.keyboard('{Enter}');
    await expect(canvas.getByText('Page 3 of 104')).toBeInTheDocument();
    await userEvent.keyboard(' ');
    await expect(canvas.getByText('Page 4 of 104')).toBeInTheDocument();
    await expect(next).toHaveFocus();
    await userEvent.click(previous);
    await expect(current).toHaveValue('3');
    await userEvent.click(previous);
    await userEvent.click(previous);
    await expect(current).toHaveValue('1');
    await expect(previous).toBeDisabled();

    // Regression: the editable digit must not leave extra whitespace beside the left arrow.
    await pager.ownerDocument.fonts.ready;
    const fieldBox = current.getBoundingClientRect();
    const totalBox = canvas.getByText('/104').getBoundingClientRect();
    const previousIcon = previous.querySelector('svg')!.getBoundingClientRect();
    const nextIcon = next.querySelector('svg')!.getBoundingClientRect();
    const font = getComputedStyle(current);
    const context = pager.ownerDocument.createElement('canvas').getContext('2d')!;
    context.font = `${font.fontWeight} ${font.fontSize} ${font.fontFamily}`;
    expect(Math.abs(fieldBox.width - context.measureText(current.value).width)).toBeLessThan(1);
    expect(
      Math.abs(fieldBox.left - previousIcon.right - (nextIcon.left - totalBox.right)),
    ).toBeLessThan(1);
    // Regression: keep arrows close to the ink on both phone and desktop.
    expect(fieldBox.left - previousIcon.right).toBeLessThanOrEqual(24);
    expect(nextIcon.left - totalBox.right).toBeLessThanOrEqual(24);
    expect(totalBox.left - fieldBox.right).toBeLessThan(1);

    // The whole counter is a hit target; only its current number is editable.
    await userEvent.click(pageTarget);
    await expect(current).toHaveFocus();
    expect([current.selectionStart, current.selectionEnd]).toEqual([0, 1]);
    await userEvent.keyboard('64');
    await expect(canvas.getByText('Page 1 of 104')).toBeInTheDocument();
    await userEvent.keyboard('{Enter}');
    await expect(current).toHaveValue('64');
    await expect(canvas.getByText('Page 64 of 104')).toBeInTheDocument();
    await userEvent.click(current);
    await userEvent.keyboard('7{Escape}');
    await expect(current).toHaveValue('64');
    await expect(current).not.toHaveFocus();
    await userEvent.click(current);
    await userEvent.keyboard('1');
    await userEvent.tab();
    await expect(current).toHaveValue('1');
    await expect(previous).toBeDisabled();

    const targets = [previous, pageTarget, next].map((button) => {
      const box = button.getBoundingClientRect();
      const reach = getComputedStyle(button, '::after');
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
    });
    const win = pager.ownerDocument.defaultView!;
    const pointer = win.matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    // Navigation beside a header's facts takes the band's pointer step (24px), not the
    // 28px face of a control on the trailing rail. Touch is untouched: the same cluster
    // keeps its 44px reach through invisible slop.
    for (const target of targets) {
      expect(target.width).toBeGreaterThanOrEqual(pointer ? 24 : 44);
      expect(target.height).toBeGreaterThanOrEqual(pointer ? 24 : 44);
    }
    expect(targets[1].left - targets[0].right).toBeGreaterThanOrEqual(0);
    expect(targets[2].left - targets[1].right).toBeGreaterThanOrEqual(0);
    const document = pager.ownerDocument.documentElement;
    expect(document.scrollWidth).toBeLessThanOrEqual(document.clientWidth);
    expect(targets[0].left).toBeGreaterThanOrEqual(0);
    expect(targets[2].right).toBeLessThanOrEqual(document.clientWidth);
  },
};

/** A set that cannot move keeps its counter, and offers neither steps nor editing. */
export const ProjectPagesDisabled: Story = {
  render: () => (
    <Sheet>
      <PagerDemo initialPage={2} disabled />
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const pager = canvas.getByRole('navigation', { name: 'Pages of vis sessions' });
    const current = canvas.getByRole('textbox', { name: 'Current page' });
    const previous = canvas.getByRole('button', { name: 'Previous page' });
    const next = canvas.getByRole('button', { name: 'Next page' });
    const win = pager.ownerDocument.defaultView!;
    await expect(pager).toBeVisible();
    await expect(pager).toHaveAttribute('aria-disabled', 'true');
    for (const control of [previous, current, next]) {
      await expect(control).toBeDisabled();
      await userEvent.click(control);
      control.focus();
      await expect(control).not.toHaveFocus();
    }
    await userEvent.tab();
    await expect(pager.contains(pager.ownerDocument.activeElement)).toBe(false);
    await expect(current).toHaveValue('2');
    await userEvent.hover(current);
    await expect(win.getComputedStyle(current).color).toBe(win.getComputedStyle(next).color);
    await expect(win.getComputedStyle(canvas.getByText('/104')).color).toBe(
      win.getComputedStyle(next).color,
    );
    await userEvent.unhover(current);
  },
};

/** The arrows stay put when the counter grows, and stop at the final page. */
export const ProjectPagesEnd: Story = {
  globals: { viewport: { value: 'desktop' } },
  render: () => (
    <Sheet>
      <PagerDemo initialPage={99} />
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const previous = canvas.getByRole('button', { name: 'Previous page' });
    const next = canvas.getByRole('button', { name: 'Next page' });
    const positions = [previous, next].map((button) => button.getBoundingClientRect().x);
    const current = canvas.getByRole('textbox', { name: 'Current page' });
    for (const page of [1, 9, 10, 99]) {
      await userEvent.click(current);
      await userEvent.keyboard(`${page}{Enter}`);
      await expect(current).toHaveValue(String(page));
      expect([previous, next].map((button) => button.getBoundingClientRect().x)).toEqual(positions);
    }
    for (let page = 100; page <= 104; page += 1) {
      await userEvent.click(next);
      await expect(canvas.getByText(`Page ${page} of 104`)).toBeInTheDocument();
      expect([previous, next].map((button) => button.getBoundingClientRect().x)).toEqual(positions);
    }
    await expect(next).toBeVisible();
    await expect(next).toBeDisabled();
    await userEvent.click(previous);
    await expect(canvas.getByRole('textbox', { name: 'Current page' })).toHaveValue('103');
    await expect(next).toBeEnabled();
  },
};

export const ProjectPagesDesktop: Story = {
  ...ProjectPages,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

export const ProjectPagesEditing: Story = {
  ...ProjectPagesDesktop,
  play: async ({ canvas }) => {
    const current = canvas.getByRole('textbox', { name: 'Current page' });
    await userEvent.click(current);
    await userEvent.keyboard('42');
    await expect(current).toHaveValue('42');
    await expect(current).toHaveFocus();
    await expect(canvas.getByText('Page 1 of 104')).toBeInTheDocument();
  },
};

export const ProjectPagesNarrowRail: Story = {
  ...ProjectPagesDesktop,
  decorators: [
    (Story) => (
      <div className="w-80">
        <Story />
      </div>
    ),
  ],
};

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
          name={STORY_MACHINES[0].name}
          qualifier={STORY_SESSION.where}
          qualifierTitle={STORY_SESSION.where}
          onRename={noop}
          renameLabel={`Rename ${STORY_MACHINES[0].name}`}
        />
      </Group>
      <Group of="A project, and the fold that exposes its sessions">
        <ProjectCrumb
          name={STORY_SESSION.project}
          qualifier={STORY_SESSION.where}
          qualifierTitle={STORY_SESSION.where}
          disclosure={{ isOpen: true, onToggle: noop, label: 'Collapse vis' }}
        />
      </Group>
      <Group of="What a band counts">
        <HeaderMeta>
          <HeaderTally count={STORY_SESSION.turns} unit="turn" />
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
        </HeaderActions>
      </Group>
      <Group of="A header name that edits in place, and the step through a long list">
        <HeaderRenameDemo />
        <PagerDemo />
      </Group>
      <Group of="What separates one machine from the next, which is distance and not a line">
        <div className="w-full">
          <SectionHeader>Machines</SectionHeader>
          <BandLabel>Recent</BandLabel>
          <MachineGap />
          <BandLabel>Another machine</BandLabel>
        </div>
      </Group>
    </Sheet>
  ),
};

/** The machine whose first read of this run has not landed: the title explains the pending check. */
const STORY_CHECKING = STORY_MACHINES[1].name;

function MachineSwitcherDemo() {
  const [on, setOn] = useState<string>(STORY_MACHINES[0].name);
  return (
    <MachineSwitcher>
      {STORY_MACHINES.map((machine) => {
        const isChecking = !machine.isDown && machine.name === STORY_CHECKING;
        return (
          <MachineTab
            key={machine.name}
            isOn={on === machine.name}
            hasUnread={machine.unread > 0}
            isDown={machine.isDown}
            label={machine.isDown ? `Reconnect to ${machine.name}` : undefined}
            title={
              machine.isDown
                ? `${machine.name} is not answering`
                : isChecking
                  ? `Checking ${machine.name}…`
                  : undefined
            }
            onClick={() => {
              if (!machine.isDown) setOn(machine.name);
            }}
          >
            {machine.name}
          </MachineTab>
        );
      })}
    </MachineSwitcher>
  );
}

/**
 * The switcher is name-first: cool unread fill, warm selection, error ink for retry.
 * A cached machine keeps its pending-check tooltip alongside any unread activity.
 */
export const Machines: Story = {
  render: () => (
    <Sheet>
      <Group of="The switcher: news on one tile, one still being checked, one that is down">
        <MachineSwitcherDemo />
      </Group>
      <Group of="What a machine's footer offers">
        <NewSessionButton
          machine={STORY_MACHINES[0].name}
          where={STORY_SESSION.where}
          onPress={noop}
        />
        <NewSessionButton machine={STORY_MACHINES[0].name} isBusy onPress={noop} />
        <NewSessionButton machine={STORY_MACHINES[2].name} disabled onPress={noop} />
        <MachineProjectsButton machine={STORY_MACHINES[0].name} onPress={noop} />
        <MachineProjectsButton machine={STORY_MACHINES[0].name} isQuiet onPress={noop} />
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const buttons = canvas.getAllByRole('button', { name: /^New session/ });
    // Busy and disabled states must not restore the old project circles.
    for (const button of buttons) {
      await expectUnframedIcon(button);
      // The glyph must follow the control's hover and disabled foreground.
      const glyph = button.querySelector('svg')!;
      await expect(getComputedStyle(glyph).color).toBe(getComputedStyle(button).color);
    }
    await expect(buttons).toHaveLength(3);
    await expect(buttons[0]).toBeEnabled();
    await expect(buttons[1]).toBeDisabled();
    await expect(buttons[1]).toHaveAttribute('aria-busy', 'true');
    await expect(buttons[2]).toBeDisabled();

    const chosen = canvas.getByRole('button', { name: 'tower' });
    const unread = canvas.getByRole('button', { name: 'macbook-pro-16-work unread' });
    const down = canvas.getByRole('button', { name: 'Reconnect to mini' });
    const track = chosen.parentElement!;
    const style = (element: Element) => getComputedStyle(element);
    const unreadSurface = style(unread).backgroundColor;
    await expect(chosen).toHaveAttribute('aria-pressed', 'true');
    if (chosen.ownerDocument.documentElement.dataset.theme !== 'high-contrast-dark') {
      await expect(style(chosen).backgroundColor).not.toBe(style(track).backgroundColor);
    }
    await expect(style(chosen).color).not.toBe(style(chosen).backgroundColor);
    await expect(style(chosen).boxShadow).toContain('inset');
    await expect(unread).toHaveAttribute('aria-pressed', 'false');
    await expect(unreadSurface).not.toBe(style(track).backgroundColor);
    await expect(unreadSurface).not.toBe(style(chosen).backgroundColor);
    await expect(style(unread).color).not.toBe(unreadSurface);
    await expect(unread.querySelectorAll('span')).toHaveLength(1);
    await expect(unread.querySelector('.sr-only')).toHaveTextContent('unread');
    await expect(down).not.toHaveAttribute('aria-pressed');
    await expect(down.querySelector('span')).toBeNull();
    await expect(style(down).color).not.toBe(style(chosen).color);

    await userEvent.click(unread);
    await expect(unread).toHaveAttribute('aria-pressed', 'true');
    await expect(chosen).toHaveAttribute('aria-pressed', 'false');
    await expect(style(unread).backgroundColor).toBe(unreadSurface);
    await expect(style(unread).boxShadow).toContain('inset');
    await expect(style(unread).color).not.toBe(style(unread).backgroundColor);

    await userEvent.click(down);
    await expect(down).not.toHaveAttribute('aria-pressed');
    await expect(unread).toHaveAttribute('aria-pressed', 'true');
    await expect(style(down).backgroundColor).toBe(style(chosen).backgroundColor);
    await expect(style(down).boxShadow).toBe('none');
  },
};

export const MachinesDark: Story = {
  ...Machines,
  globals: { theme: 'blockether-dark' },
};

export const MachinesHighContrast: Story = {
  ...Machines,
  globals: { theme: 'high-contrast-dark' },
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
        {open && (
          <p id="story-piper-settings" className="px-3 py-2 font-mono text-meta text-dialog-hint">
            English · downloaded
          </p>
        )}
      </div>
      <div className="grid bg-input">
        <SettingsChoiceDisclosure
          title="This device"
          sub="system TTS"
          isSelected={engine === 'device'}
          isOpen={false}
          controls="story-device-settings"
          onSelect={() => setEngine('device')}
          onToggle={() => undefined}
        />
        <p id="story-device-settings" hidden>
          System voice settings
        </p>
      </div>
    </div>
  );
}

/** A SETTINGS PANEL IS ROWS, and a row is a question with its answer beside it. */
export const Settings: Story = {
  render: () => (
    <Sheet>
      <Group of="A setting that opens, and the value it already holds">
        <SettingsDisclosure label="Voice" value="Piper English" isOpen={false} className="w-full" />
      </Group>
      <Group of="A group of choices, one of them opened">
        <SettingsChoiceGroup label="TTS engines">
          <SettingsChoiceDemo />
        </SettingsChoiceGroup>
      </Group>
      <Group of="The switch that speaks for one machine">
        <NotifyConnectionSwitch machine={STORY_MACHINES[0].name} isOn onClick={noop} />
        <NotifyConnectionSwitch
          machine={STORY_MACHINES[1].name}
          isOn={false}
          isChecking
          onClick={noop}
        />
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const labels = ['Voice', 'Piper (gateway)', 'This device'].map((name) =>
      canvas.getByText(name),
    );
    await labels[0].ownerDocument.fonts.ready;
    for (const label of labels) {
      const style = getComputedStyle(label);
      await expect(style.fontSize).toBe('13px');
      await expect(style.lineHeight).toBe('20px');
      await expect(style.fontFamily).toBe(getComputedStyle(labels[0]).fontFamily);
    }
    for (const value of ['Piper English', 'ready', 'system TTS']) {
      const style = getComputedStyle(canvas.getByText(value));
      await expect(style.fontSize).toBe('11px');
      await expect(style.lineHeight).toBe('16px');
    }
    await expect(
      getComputedStyle(canvas.getByRole('heading', { name: 'TTS engines' })).fontSize,
    ).toBe('12px');
    for (const button of canvas.getAllByRole('button', { name: /^Settings for/ })) {
      await expectUnframedIcon(button);
      await userEvent.click(button);
      await expectUnframedIcon(button);
    }
    await userEvent.click(canvas.getByText('This device'));
    await expect(canvas.getByText('This device').closest('button')).toHaveAttribute(
      'aria-pressed',
      'true',
    );
    await expect(canvas.getByText('Piper (gateway)').closest('button')).toHaveAttribute(
      'aria-pressed',
      'false',
    );
  },
};

export const SettingsPointer: Story = {
  ...Settings,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/**
 * EVERY SURFACE THAT OPENS OVER ANOTHER WEARS THE SAME BAND. There is one dialog
 * header in this app; a `fit` sheet is a SIZE of the same modal, not a second one.
 * These stories paint over the whole frame on purpose — a dialog photographed on
 * white paper never shows whether it reads as an interruption.
 */
export const Dialogs: Story = {
  render: () => (
    <Modal size="fit" onDismiss={noop}>
      <DialogFrame
        title="Delete this session?"
        subtitle={STORY_SESSION.title}
        actions={<BandButton isPrimary>Delete</BandButton>}
        closeLabel="Close the delete dialog"
        onClose={noop}
      >
        <div className="p-4">
          <ConfirmRow
            question={`Delete ${STORY_SESSION.title}?`}
            cost="61 turns and every artifact go with it."
            confirmLabel="Delete"
            onKeep={noop}
            onConfirm={noop}
          />
        </div>
      </DialogFrame>
    </Modal>
  ),
};

/** The full-height dialog: a list inside it gets every pixel the glass has. */
export const DialogFull: Story = {
  render: () => (
    <Modal onDismiss={noop}>
      <DialogFrame
        title={`Projects on ${STORY_MACHINES[0].name}`}
        subtitle={STORY_SESSION.where}
        actions={<BandButton isPrimary>Add</BandButton>}
        closeLabel="Close the projects dialog"
        onClose={noop}
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
      onClose={noop}
    >
      <div className="p-4">
        <p className="font-mono text-meta text-dialog-hint">
          The artifact stands here, and the band above it is the same one every dialog wears.
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
            onClose={noop}
          />
        </div>
      </Group>
      <Group of="Stacked over another band, and clearing the notch">
        <div className="w-full">
          <DialogHeader title="fleet.csv" isStacked closeLabel="Close fleet.csv" onClose={noop} />
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
          <div className="relative h-16 w-full transform-gpu overflow-hidden bg-level-project">
            <PullToSearchHint phase={phase} />
          </div>
        </Group>
      ))}
    </Sheet>
  ),
};

const reviewModes = [
  { value: 'off', label: 'Off' },
  { value: 'human', label: 'Governed by human' },
  { value: 'automatic', label: 'Automatic' },
];

/** Closed, saving, empty and unavailable choices use the same production picker. */
function ChoiceControls() {
  const [mode, setMode] = useState('human');
  return (
    <Sheet>
      <Group of="Select — review mode">
        <Input aria-label="Review name" defaultValue="Companion" className="min-w-0 flex-1" />
        <Select
          aria-label="Review mode"
          value={mode}
          onValueChange={setMode}
          options={reviewModes}
        />
        <Button variant="secondary">Save review</Button>
      </Group>
      <Group of="Select — saving">
        <Select
          aria-label="Saving review mode"
          value="human"
          onValueChange={noop}
          options={reviewModes}
          disabled
          aria-busy
        />
      </Group>
      <Group of="Select — no choices available">
        <Select aria-label="Unavailable model" value="" onValueChange={noop} options={[]} />
      </Group>
      <Group of="Select — one unavailable choice">
        <Select
          aria-label="Draft backend"
          value="auto"
          onValueChange={noop}
          options={[
            { value: 'auto', label: 'Automatic' },
            { value: 'rift', label: 'Rift · not installed', disabled: true },
            { value: '', label: 'Off' },
          ]}
        />
      </Group>
    </Sheet>
  );
}

export const ClosedChoices: Story = {
  render: () => <ChoiceControls />,
  play: async ({ canvas, canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const mode = canvas.getByRole('combobox', { name: 'Review mode' });
    const field = canvas.getByRole('textbox', { name: 'Review name' }).getBoundingClientRect();
    const action = canvas.getByRole('button', { name: 'Save review' }).getBoundingClientRect();
    const face = mode.getBoundingClientRect();
    await expect(face.height).toBe(field.height);
    await expect(face.top).toBe(field.top);
    await expect(action.height).toBe(field.height);
    await expect(action.top).toBe(field.top);
    await userEvent.click(mode);
    await userEvent.click(page.getByRole('option', { name: 'Automatic' }));
    await expect(mode).toHaveTextContent('Automatic');
    await expect(mode).toHaveFocus();
    await expect(canvas.getByRole('combobox', { name: 'Saving review mode' })).toBeDisabled();
    await expect(canvas.getByRole('combobox', { name: 'Unavailable model' })).toBeDisabled();
  },
};

export const OpenChoices: Story = {
  render: () => <ChoiceControls />,
  play: async ({ canvas, canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const trigger = canvas.getByRole('combobox', { name: 'Review mode' });
    await userEvent.click(trigger);
    const option = page.getByRole('option', { name: 'Off' });
    const background = getComputedStyle(option).backgroundColor;
    const border = getComputedStyle(option).borderColor;
    const shadow = getComputedStyle(option).boxShadow;
    await userEvent.hover(option);
    await expect(getComputedStyle(option).backgroundColor).toBe(background);
    await expect(getComputedStyle(option).borderColor).toBe(border);
    await expect(getComputedStyle(option).boxShadow).toBe(shadow);
    await expect(page.getByRole('option', { name: 'Governed by human' })).toHaveAttribute(
      'aria-selected',
      'true',
    );
    await expect(trigger).toHaveAttribute('aria-expanded', 'true');
  },
};

/** A long list must escape a clipped modal column and fit above a bottom-edge trigger. */
function LongChoicesInDialog() {
  const [value, setValue] = useState('');
  const [open, setOpen] = useState(true);
  return open ? (
    <Modal size="fit" onDismiss={() => setOpen(false)}>
      <DialogFrame title="Project defaults" onClose={() => setOpen(false)}>
        <div className="flex justify-end overflow-hidden p-4">
          <Select
            aria-label="Project"
            className="w-64"
            value={value}
            onValueChange={setValue}
            options={[
              { value: '', label: 'Unassigned' },
              ...Array.from({ length: 30 }, (_, index) => ({
                value: String(index),
                label: `Project ${index + 1} · /workspace/companion/accessibility-and-keyboard-navigation`,
              })),
            ]}
          />
        </div>
      </DialogFrame>
    </Modal>
  ) : (
    <Button onClick={() => setOpen(true)}>Open project defaults</Button>
  );
}

export const LongChoices: Story = {
  render: () => <LongChoicesInDialog />,
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const trigger = page.getByRole('combobox', { name: 'Project' });
    await userEvent.click(trigger);
    const list = page.getByRole('listbox', { name: 'Project' });
    const box = list.getBoundingClientRect();
    await expect(box.top).toBeGreaterThanOrEqual(0);
    await expect(box.left).toBeGreaterThanOrEqual(0);
    await expect(box.right).toBeLessThanOrEqual(window.innerWidth);
    await expect(box.bottom).toBeLessThanOrEqual(window.innerHeight);
    await userEvent.keyboard('{End}{Enter}');
    await expect(trigger).toHaveTextContent('Project 30');
    await userEvent.click(trigger);
    await userEvent.keyboard('{Home}{Escape}');
    await expect(trigger).toHaveTextContent('Project 30');
    await expect(trigger).toHaveFocus();
    await expect(page.getByRole('dialog', { name: 'Project defaults' })).toBeVisible();
  },
};

/** The same layouts serve forms and live output, including narrow nested columns. */
export const ViewLayouts: Story = {
  render: () => (
    <Sheet>
      <Group of="ViewLayout — responsive row with a nested row">
        <ViewLayout direction="row" data-testid="view-row" className="w-full shrink-0">
          <ViewLayout>
            <ViewHeading>Connection</ViewHeading>
            <ViewParagraph>gateway.example.com</ViewParagraph>
          </ViewLayout>
          <ViewLayout direction="row" data-testid="nested-row">
            <ViewParagraph>Port 5432</ViewParagraph>
            <ViewParagraph>Encrypted transport</ViewParagraph>
          </ViewLayout>
        </ViewLayout>
      </Group>
      <Group of="ChoiceRow — long, selected and disabled">
        <ViewLayout className="w-full max-w-72">
          <ChoiceRow isOn={false} mark={HUMAN_INPUT_CHOICE_MARKS.exclusiveOff} aria-pressed={false}>
            Require an encrypted connection to gateway.example.com before continuing
          </ChoiceRow>
          <ChoiceRow isOn mark={HUMAN_INPUT_CHOICE_MARKS.exclusiveOn} aria-pressed>
            Use this connection
          </ChoiceRow>
          <ChoiceRow isOn={false} mark={HUMAN_INPUT_CHOICE_MARKS.exclusiveOff} disabled>
            Connection unavailable
          </ChoiceRow>
        </ViewLayout>
      </Group>
      <Group of="ViewLayout — empty">
        <ViewLayout direction="row" data-testid="empty-layout" />
      </Group>
    </Sheet>
  ),
  play: async ({ canvas }) => {
    const row = canvas.getByTestId('view-row');
    const nested = canvas.getByTestId('nested-row');
    const originalWidth = row.style.width;
    try {
      // Container width, not a breakpoint: these checks also run on a phone.
      row.style.width = '640px';
      const [first, second] = [...row.children].map((child) => child.getBoundingClientRect());
      await expect(first.top).toBe(second.top);
      await expect(first.width).toBe(second.width);
      await expect(second.left - first.right).toBe(12);
      const [port, transport] = [...nested.children].map((child) => child.getBoundingClientRect());
      await expect(port.left).toBe(transport.left);
      await expect(transport.top - port.bottom).toBe(12);
      row.style.width = '280px';
      const [narrowFirst, narrowSecond] = [...row.children].map((child) =>
        child.getBoundingClientRect(),
      );
      await expect(narrowSecond.left).toBe(narrowFirst.left);
      await expect(narrowSecond.top - narrowFirst.bottom).toBe(12);
      await expect(row.scrollWidth).toBeLessThanOrEqual(row.clientWidth);
      row.style.width = '160px';
      await expect(row.scrollWidth).toBeLessThanOrEqual(row.clientWidth);
    } finally {
      row.style.width = originalWidth;
    }
    const minimum = matchMedia('(min-width: 640px) and (pointer: fine)').matches ? 28 : 44;
    for (const button of canvas.getAllByRole('button')) {
      await expect(button.getBoundingClientRect().height).toBeGreaterThanOrEqual(minimum);
      await expect(button.scrollWidth).toBeLessThanOrEqual(button.clientWidth);
    }
    await expect(canvas.getByRole('button', { name: 'Connection unavailable' })).toBeDisabled();
    await expect(canvas.getByTestId('empty-layout').children).toHaveLength(0);
    await expect(canvas.getByTestId('empty-layout').getBoundingClientRect().height).toBe(0);
  },
};
