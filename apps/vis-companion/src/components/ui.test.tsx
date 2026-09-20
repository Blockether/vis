// @vitest-environment jsdom
// What the UI vocabulary DOES: what each control renders, what it announces, and
// the states it can be in. A control is exercised by rendering it and asking the
// accessibility tree — name, role, state — the way a screen reader or a user sees
// it, never by matching class strings in markup or source text.
//
// What a control LOOKS like is not asserted here. It is drawn from the shipped
// component by Storybook (`ui.stories.tsx`) and looked at. Rules about the SOURCE
// (the closed vocabulary, corner and shadow rungs, named ways out) live in
// `ui.conventions.test.ts`, which scans files instead of rendering them.
import { render, screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';

import {
  BackButton,
  BandButton,
  Chip,
  ChoiceCell,
  ChoiceRow,
  ComposerButton,
  ConfirmRow,
  CopyChip,
  CloseButton,
  DialogFrame,
  DialogHeader,
  Disclosure,
  ExecutionAction,
  IconButton,
  Input,
  ListRow,
  LoadMore,
  MetaButton,
  NotifyConnectionSwitch,
  SettingsChoiceDisclosure,
  SettingsDisclosure,
  SettingsHeader,
  SidebarToggle,
  Switch,
  Text,
  ViewHeading,
  ViewLayout,
  ViewParagraph,
} from './ui';
import {
  HeaderTally,
  HeaderTitle,
  MachineGap,
  MachineMark,
  MachineProjectsButton,
  MachineTab,
  NewSessionButton,
  Pager,
  ProjectCrumb,
  ProjectStatusCounts,
  RowDisclosure,
} from './SessionNavigator';
import { MenuHeading } from './Menu';
import { MACHINE_COLORS } from '../lib/machine-colors';

describe('settings headers', () => {
  it('leaves a heading inert while its trailing switch owns the interaction', () => {
    render(
      <SettingsHeader
        action={<NotifyConnectionSwitch machine="visgw" isOn={false} onClick={() => {}} />}
      >
        <Text as="h4" variant="section">
          Notifications
        </Text>
      </SettingsHeader>,
    );

    expect(screen.getByRole('heading', { name: 'Notifications' })).toBeInTheDocument();
    const control = screen.getByRole('switch', { name: 'Notifications from visgw: off' });
    expect(control).toBeInTheDocument();
    expect(screen.queryByRole('button')).not.toBeInTheDocument();
  });

  it.each([false, true])('announces a full-header disclosure with expanded=%s', (isOpen) => {
    const label = `${isOpen ? 'Hide' : 'Show'} diagnostics`;
    render(
      <SettingsHeader disclosure={{ isOpen, label, onToggle: () => {} }}>
        <Text variant="section" role="heading">
          Diagnostics
        </Text>
      </SettingsHeader>,
    );

    expect(screen.getAllByRole('button')).toHaveLength(1);
    expect(screen.getByRole('button')).toHaveAccessibleName(label);
    expect(screen.getByRole('button')).toHaveAttribute('aria-expanded', String(isOpen));
  });
});

describe('ProjectCrumb', () => {
  it('is a disclosure that names its project and its state', () => {
    render(
      <ProjectCrumb name="vis" qualifier="~/vis" disclosure={{ isOpen: true, onToggle: () => {}, label: 'Collapse vis' }} />,
    );

    expect(screen.getByRole('button', { name: 'Collapse vis' })).toHaveAttribute(
      'aria-expanded',
      'true',
    );
  });
});

// Regression: the project plus used accent ink instead of the standard icon color,
// and the word "New session" repeated once per project header.
describe('NewSessionButton', () => {
  it('is a PLUS at rest, named for the machine it starts on', () => {
    render(<NewSessionButton machine="tower" onPress={() => {}} />);

    const button = screen.getByRole('button', { name: 'New session on tower' });
    // Icon-only: no visible word repeats across the project headers.
    expect(button).toHaveTextContent('');
    expect(button).toHaveAttribute('title', 'New session on tower');
  });

  it('puts the project on the tooltip, where the header has no room for a path', () => {
    render(<NewSessionButton machine="tower" where="vis" onPress={() => {}} />);

    expect(screen.getByRole('button')).toHaveAttribute('title', 'New session on tower, in vis');
  });

  it('is refused while the machine is busy or not answering', () => {
    const { rerender } = render(<NewSessionButton machine="tower" disabled onPress={() => {}} />);
    expect(screen.getByRole('button')).toBeDisabled();

    rerender(<NewSessionButton machine="tower" isBusy onPress={() => {}} />);
    expect(screen.getByRole('button')).toBeDisabled();
  });

  // Regression: replacing the compact plus with a word widened one project header
  // while its neighbours kept the one-mark rhythm; progress must stay a spinner.
  it('reports the wait while a create is in flight, without growing a word', () => {
    render(<NewSessionButton machine="tower" isBusy onPress={() => {}} />);

    const button = screen.getByRole('button', { name: 'New session on tower' });
    expect(button).toHaveTextContent('');
    expect(button).toHaveAttribute('aria-busy', 'true');
    expect(button).toHaveAttribute('aria-live', 'polite');
  });
});

describe('IconButton', () => {
  it('is named, because it carries no word', () => {
    render(
      <IconButton label="Actions for tower">
        <span aria-hidden>⋯</span>
      </IconButton>,
    );

    expect(screen.getByRole('button', { name: 'Actions for tower' })).toBeInTheDocument();
  });

  it('can be refused', () => {
    render(<IconButton label="Actions for tower" disabled />);
    expect(screen.getByRole('button')).toBeDisabled();
  });
});

// Regression (reported: every surface spelled its own ✕, five different boxes on
// one phone): one component now, with no tone or size to choose.
describe('CloseButton', () => {
  it('is named for what it closes', () => {
    render(<CloseButton label="Close artifacts" onClick={() => {}} />);

    const button = screen.getByRole('button', { name: 'Close artifacts' });
    expect(button).toHaveAttribute('title', 'Close artifacts');
  });

  it('answers every surface with the same control', () => {
    const { unmount } = render(<CloseButton label="Clear search" onClick={() => {}} />);
    const first = screen.getByRole('button');
    unmount();

    render(<CloseButton label="Remove queued message 1" onClick={() => {}} />);
    const second = screen.getByRole('button');

    expect(second.getAttribute('class')).toBe(first.getAttribute('class'));
  });
});

describe('BandButton', () => {
  it('speaks an icon-only cell without putting the name back on screen', () => {
    render(
      <BandButton label="Refresh models">
        <span aria-hidden="true" />
      </BandButton>,
    );

    const button = screen.getByRole('button', { name: 'Refresh models' });
    expect(button).toHaveTextContent('');
    expect(button).toHaveAttribute('title', 'Refresh models');
  });

  it('carries a word when it has one', () => {
    render(<BandButton>Refresh</BandButton>);
    expect(screen.getByRole('button', { name: 'Refresh' })).toBeInTheDocument();
  });
});

// Regression, user report (project status values ran together and only LIVE was
// summarized): live, human demand and finished unread work are separate states.
describe('ProjectStatusCounts', () => {
  it('separates all actionable states and does not double-count waiting as live', () => {
    const { container } = render(<ProjectStatusCounts live={5} awaiting={2} unread={3} />);

    expect(container).toHaveTextContent('3 live');
    expect(container).toHaveTextContent('2 needs input');
    expect(container).toHaveTextContent('3 new');
  });

  it('renders no separators when there is no status to report', () => {
    const { container } = render(<ProjectStatusCounts live={0} />);
    expect(container).toBeEmptyDOMElement();
  });
});

describe('Pager', () => {
  it('renders nothing for a project that fits on one page', () => {
    const { container } = render(
      <Pager page={1} pageCount={1} onPage={() => {}} label="vis sessions" />,
    );
    expect(container).toBeEmptyDOMElement();
  });

  it('offers only previous/next steps and announces the position once', () => {
    const { container } = render(
      <Pager page={1} pageCount={80} onPage={() => {}} label="vis sessions" />,
    );

    expect(screen.getByRole('navigation', { name: 'Pages of vis sessions' })).toBeInTheDocument();
    const steps = screen.getAllByRole('button');
    expect(steps).toHaveLength(2);
    expect(steps.map((step) => step.getAttribute('aria-label'))).toEqual([
      'Previous page',
      'Next page',
    ]);
    expect(container.textContent!.split('Page 1 of 80')).toHaveLength(2);
  });
});

describe('MachineGap', () => {
  it('is air, not a line: decoration only', () => {
    const { container } = render(<MachineGap />);
    expect(container.firstElementChild).toHaveAttribute('aria-hidden', 'true');
  });
});

// Regression, user report (the phone header printed "725" over a list): a number
// with no noun is a different sentence, not a shorter one.
describe('HeaderTally', () => {
  it('prints the number AND its noun on every screen', () => {
    const { container } = render(<HeaderTally count={699} unit="session" />);
    expect(container).toHaveTextContent('699 sessions');
  });

  it('counts one of a thing in the singular', () => {
    const { container } = render(<HeaderTally count={1} unit="project" />);
    expect(container).toHaveTextContent('1 project');
  });
});

describe('HeaderTitle', () => {
  it('is a name, not a control, until it is renamable', () => {
    const { rerender } = render(<HeaderTitle name="tower" qualifier="10.0.0.5:7890" />);
    expect(screen.queryByRole('button')).not.toBeInTheDocument();

    rerender(
      <HeaderTitle
        name="tower"
        qualifier="10.0.0.5:7890"
        onRename={() => {}}
        renameLabel="Rename tower"
      />,
    );
    expect(screen.getByRole('button', { name: 'Rename tower' })).toBeInTheDocument();
  });
});

describe('RowDisclosure', () => {
  it('names what it opens and reports whether it is open', () => {
    const { rerender } = render(<RowDisclosure isOpen={false} label="Show details for Untitled" />);
    expect(screen.getByRole('button', { name: 'Show details for Untitled' })).toHaveAttribute(
      'aria-expanded',
      'false',
    );

    rerender(<RowDisclosure isOpen label="Show details for Untitled" />);
    expect(screen.getByRole('button')).toHaveAttribute('aria-expanded', 'true');
  });
});

// The mark's whole job is the hue it paints, so its states are pinned by the
// token they render: solid when heard from, breathing outline until then, and a
// still outline once the machine is down (down wins over checking).
describe('MachineMark', () => {
  const hue = MACHINE_COLORS[7]!;
  const mark = (props: { isHollow?: boolean; isChecking?: boolean } = {}) =>
    render(<MachineMark color={hue} {...props} />).container.firstElementChild!;

  it('is decoration only, solid when the machine has answered', () => {
    const { container } = render(<MachineMark color={hue} />);
    expect(container.firstElementChild).toHaveAttribute('aria-hidden', 'true');
    expect(mark()).toHaveClass(hue.dot);
  });

  it('keeps the outline breathing while the machine is being checked', () => {
    const checking = mark({ isChecking: true });
    expect(checking).toHaveClass(hue.rail);
    expect(checking).toHaveClass('animate-pulse');
  });

  it('goes still once the machine is down', () => {
    const down = mark({ isHollow: true, isChecking: true });
    expect(down).toHaveClass(hue.rail);
    expect(down).not.toHaveClass('animate-pulse');
  });
});

describe('MachineTab', () => {
  it('says whether it is the machine that is on', () => {
    const { rerender } = render(
      <MachineTab isOn onClick={() => {}}>
        tower
      </MachineTab>,
    );
    expect(screen.getByRole('button', { name: 'tower' })).toHaveAttribute(
      'aria-pressed',
      'true',
    );

    rerender(
      <MachineTab isOn={false} onClick={() => {}}>
        tower
      </MachineTab>,
    );
    expect(screen.getByRole('button')).toHaveAttribute('aria-pressed', 'false');
  });

  // Regression: the tab carried live and unread counts, so the reader had to learn
  // a colour code to tell two numbers apart. News is a HIGHLIGHT.
  it('marks unread with one mark and the word, never a number', () => {
    render(
      <MachineTab isOn={false} hasUnread onClick={() => {}}>
        tower
      </MachineTab>,
    );

    const tab = screen.getByRole('button', { name: /tower\s*unread/ });
    expect(tab.textContent).not.toMatch(/\d/);
  });

  // Regression (reported: offline tabs stayed live and scoped the screen to a
  // machine with nothing to show): a machine that is not answering is a retry.
  it('drains a machine that is not answering and makes its press a retry', () => {
    render(
      <MachineTab
        isOn={false}
        isDown
        label="Reconnect to tower"
        title="tower is not answering - Failed to fetch"
        onClick={() => {}}
      >
        tower
      </MachineTab>,
    );

    const retry = screen.getByRole('button', { name: 'Reconnect to tower' });
    expect(retry).not.toHaveAttribute('aria-pressed');
    expect(retry).toHaveAttribute('aria-live', 'polite');
    expect(retry).toHaveAttribute('title', 'tower is not answering - Failed to fetch');
  });

  // Regression (reported: the error should be RED and say "Unable to connect"):
  // the answer arrives in the tile that was pressed.
  it('answers the press in the tile that was pressed', () => {
    render(
      <MachineTab isOn={false} isDown note="Unable to connect" isNoteError onClick={() => {}}>
        tower
      </MachineTab>,
    );

    expect(screen.getByRole('button', { name: /Unable to connect/ })).toBeInTheDocument();
  });
});

describe('MachineProjectsButton', () => {
  it('is named for what it opens, because it carries no word', () => {
    render(<MachineProjectsButton machine="tower" onPress={() => {}} />);

    expect(
      screen.getByRole('button', { name: 'Projects on tower' }),
    ).toBeInTheDocument();
  });
});

// Regression, user report (a two-line confirmation took the whole glass): the
// question replaces the row it is about, and the refusal comes first.
describe('ConfirmRow', () => {
  it('puts the refusal first and spends the red on the commitment alone', () => {
    render(
      <ConfirmRow question="Delete alpha?" confirmLabel="Yes, delete" onKeep={() => {}} onConfirm={() => {}} />,
    );

    const [keep, commit] = screen.getAllByRole('button');
    expect(keep).toHaveTextContent(/keep/i);
    expect(commit).toHaveTextContent('Yes, delete');
  });

  it('spells the cost of the destructive answer inside the question', () => {
    render(
      <ConfirmRow
        question="Remove Codex?"
        cost="Signs out on the gateway machine."
        confirmLabel="Yes, remove"
        onKeep={() => {}}
        onConfirm={() => {}}
      />,
    );

    expect(screen.getByText('Signs out on the gateway machine.')).toBeInTheDocument();
  });

  // Regression, user report: the answers' own 48px floor is not the height of the row
  // they replace, so a 52px session row left the list four pixels shorter the moment
  // its question appeared. The row hands over what it stood.
  it('stands what the row it replaces stood', () => {
    render(
      <ConfirmRow
        question="Delete alpha?"
        confirmLabel="Yes, delete"
        rowHeight={52}
        onKeep={() => {}}
        onConfirm={() => {}}
      />,
    );

    expect(screen.getByRole('group', { name: 'Delete alpha?' })).toHaveStyle({
      minHeight: '52px',
    });
  });
});

describe('the second vocabulary: chips, rows, disclosures', () => {
  it('Chip says whether it is the one that is on', () => {
    const { rerender } = render(
      <Chip isOn onClick={() => {}}>
        IMAGES
      </Chip>,
    );
    expect(screen.getByRole('button', { name: 'IMAGES' })).toHaveAttribute(
      'aria-pressed',
      'true',
    );

    rerender(
      <Chip isOn={false} onClick={() => {}}>
        IMAGES
      </Chip>,
    );
    expect(screen.getByRole('button')).toHaveAttribute('aria-pressed', 'false');
  });

  it('LoadMore hears its own name, and is a rule rather than a button when nothing loads', () => {
    const { rerender } = render(
      <LoadMore label="Load 12 more artifacts" onClick={() => {}}>
        Load 12 more
      </LoadMore>,
    );
    expect(screen.getByRole('button', { name: 'Load 12 more artifacts' })).toHaveTextContent(
      'Load 12 more',
    );

    rerender(<LoadMore label="2 more lines">2 more lines</LoadMore>);
    expect(screen.queryByRole('button')).not.toBeInTheDocument();
    expect(screen.getByLabelText('2 more lines')).toHaveTextContent('2 more lines');
  });

  it('CopyChip leads with the icon and the name, and shows the value it copies', () => {
    render(
      <CopyChip value="abc" label="Copy session id">
        abc12345
      </CopyChip>,
    );
    expect(screen.getByRole('button', { name: 'Copy session id' })).toHaveTextContent(
      'abc12345',
    );
  });

  it('CopyChip uses the shared quiet icon button when no visible value is supplied', () => {
    render(<CopyChip value="abc" label="Copy code" />);
    expect(screen.getByRole('button', { name: 'Copy code' })).toHaveTextContent('');
  });

  it('ListRow is pressable under its own name', () => {
    render(<ListRow onClick={() => {}}>anthropic</ListRow>);
    expect(screen.getByRole('button', { name: 'anthropic' })).toBeInTheDocument();
  });

  // Regression #222: RUN launches a viewer, never a disclosure or form submission.
  it('ExecutionAction provides a named button without disclosure state', () => {
    render(
      <ExecutionAction aria-label="Open run Build pool">
        <span>Build pool</span>
      </ExecutionAction>,
    );

    const action = screen.getByRole('button', { name: 'Open run Build pool' });
    expect(action).not.toHaveAttribute('aria-expanded');
    expect(action).toHaveAttribute('type', 'button');
  });

  it('Disclosure reports its state and stays the transcript scroll anchor', () => {
    const { rerender } = render(
      <Disclosure isOpen={false} onClick={() => {}}>
        <span>THINKING</span>
      </Disclosure>,
    );

    const toggle = screen.getByRole('button', { name: 'THINKING' });
    expect(toggle).toHaveAttribute('aria-expanded', 'false');
    // SessionScreen keeps the viewport still by finding exactly this attribute.
    expect(toggle).toHaveAttribute('data-disclosure-toggle');

    rerender(
      <Disclosure isOpen onClick={() => {}}>
        <span>THINKING</span>
      </Disclosure>,
    );
    expect(screen.getByRole('button')).toHaveAttribute('aria-expanded', 'true');
  });

  it('ChoiceRow turns amber when it is the answer, and the glyph is decoration', () => {
    render(
      <ChoiceRow isOn mark="●" onClick={() => {}}>
        production
      </ChoiceRow>,
    );
    expect(screen.getByRole('button', { name: 'production' })).toBeInTheDocument();
  });

  describe('view presentation', () => {
    it('keeps nested direction, child order and an empty layout without controls', () => {
      const { container } = render(
        <ViewLayout direction="row">
          <ViewHeading>Connection</ViewHeading>
          <ViewLayout>
            <ViewParagraph>Host before port</ViewParagraph>
            <ViewLayout direction="row" />
          </ViewLayout>
        </ViewLayout>,
      );

      expect(container.querySelectorAll('[data-view-layout="row"]')).toHaveLength(2);
      expect(container.querySelectorAll('[data-view-layout="column"]')).toHaveLength(1);
      expect(container.innerHTML.indexOf('Connection')).toBeLessThan(
        container.innerHTML.indexOf('Host before port'),
      );
      expect(container.querySelector('button, input, [tabindex]')).toBeNull();
    });

    it('keeps native heading levels and leaves text parsing to the caller', () => {
      for (const level of [1, 2, 3, 4, 5, 6] as const) {
        const { unmount } = render(<ViewHeading level={level}>Review</ViewHeading>);
        expect(screen.getByRole('heading', { level, name: 'Review' })).toBeInTheDocument();
        unmount();
      }

      render(<ViewHeading>Section</ViewHeading>);
      expect(screen.getByRole('heading', { level: 3 })).toBeInTheDocument();
    });

    it('renders paragraph text literally and rich text as given', () => {
      const { container } = render(
        <>
          <ViewParagraph>{'**Keep literal** & safe'}</ViewParagraph>
          <ViewParagraph>
            <strong>Rich text</strong>
          </ViewParagraph>
        </>,
      );

      expect(container).toHaveTextContent('**Keep literal** & safe');
      expect(container.querySelector('strong')).toHaveTextContent('Rich text');
    });
  });
});

describe('a setting is picked and switched by one control each', () => {
  it('fills the chosen cell and marks it once', () => {
    const { rerender } = render(<ChoiceCell title="Gruvbox" sub="dark" isSelected />);
    expect(screen.getByRole('button', { name: /Gruvbox/ })).toHaveAttribute(
      'aria-pressed',
      'true',
    );
    expect(screen.getByText('●')).toBeInTheDocument();

    rerender(<ChoiceCell title="Gruvbox" sub="dark" isSelected={false} />);
    expect(screen.getByRole('button')).toHaveAttribute('aria-pressed', 'false');
    expect(screen.getByText('○')).toBeInTheDocument();
  });

  it('hides the selection mark when an adjacent action takes its place', () => {
    render(
      <ChoiceCell title="John" sub="not downloaded yet" isSelected={false} showSelectionMark={false} />,
    );
    expect(screen.queryByText('○')).not.toBeInTheDocument();
  });

  it('keeps the leading action its own control beside the choice', () => {
    render(
      <ChoiceCell
        title="Albert"
        sub="en-US"
        isSelected={false}
        leadingAction={{ label: 'Play Albert', icon: <span>▶</span>, onClick: () => {} }}
      />,
    );

    expect(screen.getAllByRole('button')).toHaveLength(2);
    expect(screen.getByRole('button', { name: 'Play Albert' })).toBeInTheDocument();
    expect(screen.getByRole('button', { pressed: false })).toHaveTextContent('Albert');
  });

  it('splits engine selection from its independently collapsed settings', () => {
    render(
      <SettingsChoiceDisclosure
        title="Piper (gateway)"
        sub="ready"
        isSelected
        isOpen={false}
        controls="piper-settings"
        onSelect={() => {}}
        onToggle={() => {}}
      />,
    );

    const settings = screen.getByRole('button', { name: 'Settings for Piper (gateway)' });
    expect(settings).toHaveAttribute('aria-expanded', 'false');
    expect(settings).toHaveAttribute('aria-controls', 'piper-settings');

    const buttons = screen.getAllByRole('button');
    expect(buttons).toHaveLength(2);
    const choice = buttons.find((button) => button.getAttribute('aria-pressed') === 'true');
    expect(choice).toBeVisible();
    expect(choice).toHaveAccessibleName(/Piper \(gateway\)/);
  });

  it('opens a settings direction with one full-row control', () => {
    const { rerender } = render(<SettingsDisclosure label="ASR" value="Parakeet (local)" isOpen={false} />);
    const control = screen.getByRole('button', { name: /ASR/ });
    expect(control).toHaveTextContent('Parakeet (local)');
    expect(control).toHaveAttribute('aria-expanded', 'false');

    rerender(<SettingsDisclosure label="TTS" value="This device" isOpen />);
    expect(screen.getByRole('button')).toHaveAttribute('aria-expanded', 'true');
  });

  // Regression, user report (paraphrased: these have to be real toggles, the modern
  // kind): the control spelled its state as the word ON or OFF beside the label.
  it('says on or off with the knob, and never with a word', () => {
    const { rerender } = render(<Switch label="Web search" isOn />);
    const on = screen.getByRole('switch', { name: 'Web search: on' });
    expect(on).toHaveAttribute('aria-checked', 'true');
    expect(on.textContent).toBe('');

    rerender(<Switch label="Web search" isOn={false} />);
    expect(screen.getByRole('switch', { name: 'Web search: off' })).toHaveAttribute(
      'aria-checked',
      'false',
    );
  });

  it('pulses the knob while a round trip is in flight', () => {
    render(<Switch label="Web search" isOn isBusy />);
    expect(screen.getByRole('switch')).toHaveAttribute('aria-busy', 'true');
  });
});

// Regression, user report (paraphrased: same device, four entries, and no way to
// just see whether alerts arrive): the control answers the reader's question.
describe('the notifications control answers one question', () => {
  const control = (props: Partial<Parameters<typeof NotifyConnectionSwitch>[0]> = {}) =>
    render(<NotifyConnectionSwitch machine="visgw" isOn={false} onClick={() => {}} {...props} />);

  it('states whether this device is connected — in the knob, and only there', () => {
    control({ isOn: true }).unmount();
    control({ isOn: false });
    expect(screen.getByRole('switch', { name: 'Notifications from visgw: off' })).toHaveAttribute(
      'aria-checked',
      'false',
    );
  });

  it('carries the verb in its title and keeps one control for both directions', () => {
    control({ isOn: false });
    expect(screen.getByRole('switch')).toHaveAttribute(
      'title',
      'Connect notifications from visgw',
    );
    expect(screen.getAllByRole('switch')).toHaveLength(1);
  });

  it('flips the verb with the state, still as one control', () => {
    control({ isOn: true });
    expect(screen.getByRole('switch')).toHaveAttribute(
      'title',
      'Disconnect notifications from visgw',
    );
    expect(screen.getAllByRole('switch')).toHaveLength(1);
  });

  it('says which way it is moving, and asks before it answers', () => {
    control({ isBusy: true }).unmount();
    control({ isChecking: true });
    const checking = screen.getByRole('switch');
    expect(checking).toHaveAttribute('aria-busy', 'true');
    expect(checking).toHaveAttribute(
      'title',
      'Asking visgw whether this device is registered',
    );
  });
});

describe('the composer and the meta strip', () => {
  it('names an icon-only composer control', () => {
    render(<ComposerButton label="Dictate message">{'●'}</ComposerButton>);
    expect(screen.getByRole('button', { name: 'Dictate message' })).toBeInTheDocument();
  });

  // Regression: MetaButton destructured `children` and rendered a self-closing
  // button, so the model name and level showed as two empty boxes.
  it('says the word it was given', () => {
    const { rerender } = render(<MetaButton isPicker>opus</MetaButton>);
    expect(screen.getByRole('button', { name: 'opus' })).toBeInTheDocument();

    rerender(<MetaButton>quick</MetaButton>);
    expect(screen.getByRole('button', { name: 'quick' })).toBeInTheDocument();
  });
});

describe('the way back and the desk', () => {
  it('BackButton names where it goes', () => {
    render(<BackButton label="Back to sessions" />);
    expect(screen.getByRole('button', { name: 'Back to sessions' })).toBeInTheDocument();
  });

  it('SidebarToggle says which way the list is about to go', () => {
    const { rerender } = render(<SidebarToggle isShown />);
    expect(screen.getByRole('button', { name: 'Hide the session list' })).toHaveAttribute(
      'aria-expanded',
      'true',
    );

    rerender(<SidebarToggle isShown={false} />);
    expect(screen.getByRole('button', { name: 'Show the session list' })).toHaveAttribute(
      'aria-expanded',
      'false',
    );
  });
});

// The browser paints a password field's mask its own way; the vocabulary only
// adds room between the dots, so a pasted key can be counted while typing.
describe('Input', () => {
  it('forwards native field semantics instead of applying them to the touch wrapper', () => {
    const { container } = render(
      <Input
        id="voice-name"
        name="voice"
        aria-label="Voice name"
        aria-invalid
        required
        disabled
        readOnly
        defaultValue="My voice"
        className="flex-1"
      />,
    );

    const field = screen.getByLabelText('Voice name');
    expect(field).toHaveAttribute('id', 'voice-name');
    expect(field).toHaveAttribute('name', 'voice');
    expect(field).toHaveAttribute('aria-invalid', 'true');
    expect(field).toBeRequired();
    expect(field).toBeDisabled();
    expect(field).toHaveValue('My voice');
    // Placement classes belong to the wrapper, never to the native field.
    expect(field).not.toHaveClass('flex-1');
    expect(container.querySelector('.flex-1')).toBeInTheDocument();
  });
});

// Regression, user report (paraphrased: go over all close buttons and ensure we
// use them consistently): one gesture, named for the thing it leaves.
describe('one way out, and it says what it closes', () => {
  it("takes the dialog's own title as the name of its way out", () => {
    render(
      <DialogFrame title="Machine settings" onClose={() => {}}>
        body
      </DialogFrame>,
    );
    expect(screen.getByRole('button', { name: 'Close Machine settings' })).toBeInTheDocument();
  });

  it('lets a dialog say what LEAVING does instead', () => {
    render(
      <DialogFrame title="Which branch?" closeLabel="Cancel this request" onClose={() => {}}>
        body
      </DialogFrame>,
    );
    expect(screen.getByRole('button', { name: 'Cancel this request' })).toBeInTheDocument();
  });

  it('gives a band with nowhere to go no way out at all', () => {
    const { container } = render(<DialogHeader title="How to fix it" />);
    expect(container.querySelector('button')).toBeNull();
  });

  it('closes a menu band by the name of the panel it holds', () => {
    render(
      <MenuHeading onClose={() => {}} closeLabel="Close projects on tower">
        Projects · tower
      </MenuHeading>,
    );
    expect(
      screen.getByRole('button', { name: 'Close projects on tower' }),
    ).toBeInTheDocument();
  });

  it('leaves a menu band with nowhere to go unnamed but whole', () => {
    const { container } = render(<MenuHeading>Projects · tower</MenuHeading>);
    expect(container.querySelector('button')).toBeNull();
    expect(container).toHaveTextContent('Projects · tower');
  });
});
