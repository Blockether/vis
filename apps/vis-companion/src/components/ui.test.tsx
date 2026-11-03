// @vitest-environment jsdom
// What the UI vocabulary DOES: what each control renders, what it announces, and
// the states it can be in. A control is exercised by rendering it and asking the
// accessibility tree — name, role, state — the way a screen reader or a user sees
// it, never by matching class strings in markup or source text.
//
// What a control LOOKS like is not asserted here. It is drawn from the shipped
// component by Storybook (`ui.stories.tsx`) and looked at. Rules about the SOURCE
// (the closed vocabulary, corner and shadow rungs, named ways out) are review
// rules in the header of `ui.tsx`, not tests.
import { act, fireEvent, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';

import { ConfirmRow, HoverNote, Input, ViewHeading, ViewLayout, ViewParagraph } from './ui';
import { ProjectStatusCounts } from './SessionNavigator';

// Regression, user report (project status values ran together and only LIVE was
// summarized): live, human demand and finished unread work are separate states.
describe('ProjectStatusCounts', () => {
  it('separates all actionable states and does not double-count waiting as live', () => {
    const { container } = render(<ProjectStatusCounts live={5} awaiting={2} unread={3} />);

    expect(container).toHaveTextContent('2 HITL');
    expect(container).toHaveTextContent('3 LIVE');
    expect(container).toHaveTextContent('3 NEW');
    expect(Array.from(container.querySelectorAll('[aria-hidden]'), (node) => node.textContent)).toEqual(['·', '·']);
    expect(container.querySelector('.animate-pulse')).toBeNull();
  });

  // Regression, user report (paraphrased: STOPPED is missing from the group and project
  // headers): a stopped conversation was counted as NEW.
  it('paints STOPPED as the part of NEW whose newest turn stopped', () => {
    const { container, rerender } = render(
      <ProjectStatusCounts live={2} stopped={1} unread={3} isAfterTotal />,
    );

    expect(container.textContent).toBe('|2 LIVE·1 STOPPED·2 NEW');

    rerender(<ProjectStatusCounts live={0} stopped={2} unread={2} isAfterTotal />);
    expect(container.textContent).toBe('|2 STOPPED');
  });
});

// Regression, user report (a two-line confirmation took the whole glass): the
// question replaces the row it is about, and the refusal comes first.
describe('ConfirmRow', () => {

  // #412: Enter answers yes, as in a dialog, while the focus stays on the safe answer.
  it('answers yes on Enter while the refusal holds the focus', async () => {
    const user = userEvent.setup();
    const onKeep = vi.fn();
    const onConfirm = vi.fn();
    render(<ConfirmRow question="Delete alpha?" confirmLabel="Yes, delete" onKeep={onKeep} onConfirm={onConfirm} />);

    expect(screen.getByRole('button', { name: 'No, keep' })).toHaveFocus();
    await user.keyboard('{Enter}');
    expect(onConfirm).toHaveBeenCalledTimes(1);
    expect(onKeep).not.toHaveBeenCalled();
  });

  it('ignores a held Enter, a busy answer and Enter with a modifier', () => {
    const onConfirm = vi.fn();
    const { rerender } = render(
      <ConfirmRow question="Delete alpha?" confirmLabel="Yes, delete" onKeep={() => {}} onConfirm={onConfirm} />,
    );
    const keep = screen.getByRole('button', { name: 'No, keep' });
    // The repeat of the key that opened the question must not answer it.
    fireEvent.keyDown(keep, { key: 'Enter', repeat: true });
    fireEvent.keyDown(keep, { key: 'Enter', metaKey: true });
    rerender(
      <ConfirmRow question="Delete alpha?" confirmLabel="Deleting..." isBusy onKeep={() => {}} onConfirm={onConfirm} />,
    );
    fireEvent.keyDown(keep, { key: 'Enter' });
    expect(onConfirm).not.toHaveBeenCalled();
  });

  it('keeps on Enter after the keyboard chooses the refusal', async () => {
    const user = userEvent.setup();
    const onKeep = vi.fn();
    const onConfirm = vi.fn();
    render(<ConfirmRow question="Delete alpha?" confirmLabel="Yes, delete" onKeep={onKeep} onConfirm={onConfirm} />);

    await user.tab();
    expect(screen.getByRole('button', { name: 'Yes, delete' })).toHaveFocus();
    await user.tab({ shift: true });
    await user.keyboard('{Enter}');
    expect(onKeep).toHaveBeenCalledTimes(1);
    expect(onConfirm).not.toHaveBeenCalled();
  });

});

describe('the second vocabulary: chips, rows, disclosures', () => {
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

    it('renders paragraph text literally and rich text as given', () => {
      const { container } = render(
        <>
          <ViewParagraph>{'**Keep literal** & safe'}</ViewParagraph>
          <ViewParagraph>
            <strong>Rich text</strong>
          </ViewParagraph>
          <ViewParagraph as="div">
            <p>Block text</p>
          </ViewParagraph>
        </>,
      );

      expect(container).toHaveTextContent('**Keep literal** & safe');
      expect(container.querySelector('strong')).toHaveTextContent('Rich text');
      const block = container.children[2];
      expect(block.tagName).toBe('DIV');
      expect(block.querySelector('p')).toHaveTextContent('Block text');
    });
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

// Regression, user report (the hover over a subagent row should look native): the system
// `title` box of a long task covered most of the window. The note is the app's own layer.
describe('HoverNote', () => {
  const note = (label: string) => (
    <HoverNote note={`${label} task`}>
      <span>{label}</span>
    </HoverNote>
  );

  it('shows its note to a resting mouse and closes it on a press', () => {
    vi.useFakeTimers();
    try {
      render(note('First'));
      fireEvent.pointerEnter(screen.getByText('First'), { pointerType: 'mouse' });
      act(() => vi.advanceTimersByTime(400));
      expect(screen.queryByRole('tooltip')).toBeNull();
      act(() => vi.advanceTimersByTime(100));
      expect(screen.getByRole('tooltip')).toHaveTextContent('First task');
      fireEvent.pointerDown(document.body);
      expect(screen.queryByRole('tooltip')).toBeNull();
    } finally {
      vi.useRealTimers();
    }
  });

  it('never opens for a finger, which has no hover', () => {
    vi.useFakeTimers();
    try {
      render(note('First'));
      fireEvent.pointerEnter(screen.getByText('First'), { pointerType: 'touch' });
      act(() => vi.advanceTimersByTime(1000));
      expect(screen.queryByRole('tooltip')).toBeNull();
    } finally {
      vi.useRealTimers();
    }
  });

  it('shows the next note at once only right after one closed, like desktop tooltips', () => {
    vi.useFakeTimers();
    try {
      render(
        <>
          {note('First')}
          {note('Second')}
        </>,
      );
      fireEvent.pointerEnter(screen.getByText('First'), { pointerType: 'mouse' });
      act(() => vi.advanceTimersByTime(500));
      fireEvent.pointerLeave(screen.getByText('First'), { pointerType: 'mouse' });
      fireEvent.pointerEnter(screen.getByText('Second'), { pointerType: 'mouse' });
      act(() => vi.advanceTimersByTime(1));
      expect(screen.getByRole('tooltip')).toHaveTextContent('Second task');

      fireEvent.pointerLeave(screen.getByText('Second'), { pointerType: 'mouse' });
      act(() => vi.advanceTimersByTime(1000));
      fireEvent.pointerEnter(screen.getByText('First'), { pointerType: 'mouse' });
      act(() => vi.advanceTimersByTime(100));
      expect(screen.queryByRole('tooltip')).toBeNull();
    } finally {
      vi.useRealTimers();
    }
  });

  // Regression, user report (the note still covered the session header): the dock row sits in
  // the footer of the session, and its note must stay below the header of the session.
  it('keeps the note of a footer row below the header of its area', () => {
    vi.useFakeTimers();
    const boxes: Record<string, DOMRect> = {
      SECTION: new DOMRect(0, 0, 1000, window.innerHeight),
      HEADER: new DOMRect(0, 0, 1000, 100),
      BUTTON: new DOMRect(0, window.innerHeight - 60, 1000, 24),
    };
    const rect = vi.spyOn(Element.prototype, 'getBoundingClientRect').mockImplementation(function (this: Element) {
      return boxes[this.tagName] ?? new DOMRect();
    });
    const height = vi.spyOn(HTMLElement.prototype, 'offsetHeight', 'get').mockReturnValue(5000);
    try {
      render(
        <section data-hover-note-bounds>
          <header>Session</header>
          <footer>
            <button type="button">
              <HoverNote note="A long task">
                <span>Row</span>
              </HoverNote>
            </button>
          </footer>
        </section>,
      );
      fireEvent.pointerEnter(screen.getByText('Row'), { pointerType: 'mouse' });
      act(() => vi.advanceTimersByTime(1000));
      // The row starts 60px above the foot. Above it: the gap of 6px, then the margin of 12px.
      expect(screen.getByRole('tooltip').style.maxHeight).toBe(`${window.innerHeight - 60 - 6 - 12 - 100}px`);
    } finally {
      rect.mockRestore();
      height.mockRestore();
      vi.useRealTimers();
    }
  });
});
