/** @vitest-environment jsdom */
import { createRef, useState } from 'react';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import userEvent, { PointerEventsCheckLevel } from '@testing-library/user-event';
import { describe, expect, it, onTestFinished, vi } from 'vitest';

import { dismissTopLayer } from '../lib/edge-back';
import { Button, DialogFrame, Modal, Select } from './ui';

const choices = [
  { value: 'auto', label: 'Automatic' },
  { value: 'worktree', label: 'Worktree', disabled: true },
  { value: 'rift', label: 'Rift' },
  { value: '', label: 'Off' },
];

function BackendChoice() {
  const [value, setValue] = useState('auto');
  return (
    <>
      <Select aria-label="Draft backend" value={value} onValueChange={setValue} options={choices} />
      <Button variant="secondary">Continue</Button>
    </>
  );
}

describe('Select', () => {
  it('opens a custom listbox and commits empty-string choices without submitting the form', async () => {
    const submit = vi.fn((event: React.FormEvent) => event.preventDefault());
    render(
      <form onSubmit={submit}>
        <BackendChoice />
      </form>,
    );
    const trigger = screen.getByRole('combobox', { name: 'Draft backend' });
    expect(trigger.tagName).toBe('BUTTON');
    expect(trigger).toHaveAttribute('aria-expanded', 'false');
    expect(trigger).toHaveTextContent('Automatic');
    await userEvent.click(trigger);
    const listbox = screen.getByRole('listbox');
    expect(trigger).toHaveAttribute('aria-controls', listbox.id);
    expect(screen.getByRole('option', { name: 'Automatic' })).toHaveAttribute(
      'aria-selected',
      'true',
    );
    await userEvent.click(screen.getByRole('option', { name: 'Off' }));
    expect(trigger).toHaveTextContent('Off');
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    await waitFor(() => expect(trigger).toHaveFocus());
    expect(submit).not.toHaveBeenCalled();
  });

  it('navigates with arrows, Home/End and typeahead, skipping unavailable choices', async () => {
    render(<BackendChoice />);
    const trigger = screen.getByRole('combobox', { name: 'Draft backend' });
    await userEvent.tab();
    expect(trigger).toHaveFocus();
    await userEvent.keyboard('{ArrowDown}');
    await waitFor(() => expect(screen.getByRole('option', { name: 'Automatic' })).toHaveFocus());
    await userEvent.keyboard('{ArrowDown}');
    await waitFor(() => expect(screen.getByRole('option', { name: 'Rift' })).toHaveFocus());
    expect(trigger).toHaveTextContent('Automatic');
    await userEvent.keyboard('{End}');
    await waitFor(() => expect(screen.getByRole('option', { name: 'Off' })).toHaveFocus());
    await userEvent.keyboard('{Home}');
    await waitFor(() => expect(screen.getByRole('option', { name: 'Automatic' })).toHaveFocus());
    await userEvent.keyboard('r');
    await waitFor(() => expect(screen.getByRole('option', { name: 'Rift' })).toHaveFocus());
    await userEvent.keyboard('{Enter}');
    expect(trigger).toHaveTextContent('Rift');
    await waitFor(() => expect(trigger).toHaveFocus());
    await userEvent.tab();
    expect(screen.getByRole('button', { name: 'Continue' })).toHaveFocus();
  });

  it('cancels navigation on Escape without closing the containing dialog', async () => {
    const dismiss = vi.fn();
    render(
      <div
        onKeyDown={(event) => {
          if (event.key === 'Escape') dismiss();
        }}
      >
        <Modal onDismiss={dismiss} size="fit">
          <DialogFrame title="Draft settings">
            <BackendChoice />
          </DialogFrame>
        </Modal>
      </div>,
    );
    const trigger = screen.getByRole('combobox', { name: 'Draft backend' });
    await userEvent.click(trigger);
    await userEvent.keyboard('{End}{Escape}');
    await waitFor(() => expect(trigger).toHaveFocus());
    expect(trigger).toHaveTextContent('Automatic');
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    expect(dismiss).not.toHaveBeenCalled();
  });

  it('dismisses an outside press without committing the highlighted choice', async () => {
    render(<BackendChoice />);
    const trigger = screen.getByRole('combobox', { name: 'Draft backend' });
    await userEvent.click(trigger);
    await userEvent.keyboard('{End}');
    fireEvent.pointerDown(document.body, { pointerType: 'mouse' });
    await waitFor(() => expect(screen.queryByRole('listbox')).not.toBeInTheDocument());
    expect(trigger).toHaveTextContent('Automatic');
  });

  it.each(['single', 'multiple'])('keeps the %s picker closed after a second touch on its trigger', async (mode) => {
    // iOS can send a click to the trigger after the outside pointerdown closes the list.
    const user = userEvent.setup({ pointerEventsCheck: PointerEventsCheckLevel.Never });
    render(mode === 'single' ? <BackendChoice /> : <GroupFilter />);
    const trigger = screen.getByRole('combobox');
    await user.pointer({ keys: '[TouchA]', target: trigger });
    expect(screen.getByRole('listbox')).toBeVisible();

    await user.pointer({ keys: '[TouchA>]', target: trigger });
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    await user.pointer({ keys: '[/TouchA]', target: trigger });
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    expect(trigger).toHaveAttribute('aria-expanded', 'false');
    expect(trigger).toHaveTextContent(mode === 'single' ? 'Automatic' : 'All groups');
    await waitFor(() => expect(trigger).toHaveFocus());

    await user.pointer({ keys: '[TouchA]', target: trigger });
    expect(screen.getByRole('listbox')).toBeVisible();
  });

  it('ignores the touch click retargeted from the inert backdrop to the trigger', async () => {
    const user = userEvent.setup({ pointerEventsCheck: PointerEventsCheckLevel.Never });
    render(<BackendChoice />);
    const trigger = screen.getByRole('combobox');
    await user.pointer({ keys: '[TouchA]', target: trigger });
    await user.pointer({ keys: '[TouchA>]', target: document.body });
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    await user.pointer({ keys: '[/TouchA]', target: trigger });
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();

    await user.pointer({ keys: '[TouchA]', target: trigger });
    expect(screen.getByRole('listbox')).toBeVisible();
  });

  it.each(['keyboard', 'assistive click'])('allows %s activation after an outside touch', async (input) => {
    const user = userEvent.setup({ pointerEventsCheck: PointerEventsCheckLevel.Never });
    render(<BackendChoice />);
    const trigger = screen.getByRole('combobox');
    await user.pointer({ keys: '[TouchA]', target: trigger });
    await user.pointer({ keys: '[TouchA]', target: document.body });
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    await waitFor(() => expect(trigger).toHaveFocus());

    if (input === 'keyboard') await user.keyboard('{Enter}');
    else fireEvent.click(trigger, { detail: 0 });
    expect(screen.getByRole('listbox')).toBeVisible();
  });

  it('keeps the controlled value until the owner accepts the change, then follows external updates', async () => {
    const change = vi.fn();
    const props = {
      'aria-label': 'Draft backend',
      value: 'auto',
      onValueChange: change,
      options: choices,
    };
    const { rerender } = render(<Select {...props} />);
    const trigger = screen.getByRole('combobox');
    await userEvent.click(trigger);
    await userEvent.click(screen.getByRole('option', { name: 'Rift' }));
    expect(change).toHaveBeenCalledExactlyOnceWith('rift');
    expect(trigger).toHaveTextContent('Automatic');
    rerender(<Select {...props} value="rift" />);
    expect(trigger).toHaveTextContent('Rift');
  });

  it('disables empty or saving controls, closes if disabled while open, and forwards focus', async () => {
    const ref = createRef<HTMLButtonElement>();
    const props = {
      'aria-label': 'Draft backend',
      value: 'auto',
      onValueChange: vi.fn(),
      options: choices,
    };
    const { rerender } = render(<Select {...props} ref={ref} />);
    const trigger = screen.getByRole('combobox');
    expect(ref.current).toBe(trigger);
    await userEvent.click(trigger);
    rerender(<Select {...props} ref={ref} disabled aria-busy />);
    expect(trigger).toBeDisabled();
    expect(trigger).toHaveAttribute('aria-busy', 'true');
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    await userEvent.click(trigger);
    expect(props.onValueChange).not.toHaveBeenCalled();
    rerender(<Select {...props} />);
    expect(trigger).toBeEnabled();
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    rerender(<Select {...props} options={[]} />);
    expect(trigger).toBeDisabled();
    expect(trigger).toHaveTextContent('No options available');
  });

  it('hides the page behind the open picker from assistive tools and restores only its own attributes', async () => {
    // `inert` restyled the whole page on open and again on close. The scrim takes every press instead.
    const alreadyHidden = render(<Button>Unavailable</Button>).container;
    alreadyHidden.setAttribute('aria-hidden', 'true');
    const liveRegion = render(<div aria-live="polite">Changes saved</div>).container;
    const { container, unmount } = render(<BackendChoice />);
    const trigger = screen.getByRole('combobox');
    expect(container).not.toHaveAttribute('aria-hidden');

    await userEvent.click(trigger);
    expect(container).toHaveAttribute('aria-hidden', 'true');
    expect(container).not.toHaveAttribute('inert');
    expect(alreadyHidden).toHaveAttribute('aria-hidden', 'true');
    expect(liveRegion).not.toHaveAttribute('aria-hidden');
    await userEvent.keyboard('{Escape}');
    expect(container).not.toHaveAttribute('aria-hidden');
    expect(alreadyHidden).toHaveAttribute('aria-hidden', 'true');
    await waitFor(() => expect(trigger).toHaveFocus());

    await userEvent.click(trigger);
    expect(container).toHaveAttribute('aria-hidden', 'true');
    unmount();
    expect(container).not.toHaveAttribute('aria-hidden');
    expect(alreadyHidden).toHaveAttribute('aria-hidden', 'true');
    expect(liveRegion).not.toHaveAttribute('aria-hidden');
  });

  it('preserves unknown saved values instead of silently choosing a different option', () => {
    render(
      <Select
        aria-label="Draft backend"
        value="remote-backend"
        onValueChange={vi.fn()}
        options={choices}
      />,
    );
    expect(screen.getByRole('combobox')).toHaveTextContent('remote-backend');
  });

  it('opens beside its trigger in one pass and leaves the page behind unlocked', async () => {
    // Regression: the Radix list locked the page scroll and pointer events and placed
    // itself in later passes, so a phone showed it seconds after the tap.
    onTestFinished(() => {
      vi.restoreAllMocks();
    });
    const box = (left: number, top: number, width: number, height: number) =>
      ({ left, top, width, height, right: left + width, bottom: top + height, x: left, y: top }) as DOMRect;
    vi.spyOn(HTMLElement.prototype, 'getBoundingClientRect').mockImplementation(function (this: HTMLElement) {
      if (this.getAttribute('role') === 'combobox') return box(40, 780, 120, 32);
      if (this.getAttribute('role') === 'presentation') return box(0, 0, 390, 844);
      return box(0, 0, 0, 0);
    });
    vi.spyOn(HTMLElement.prototype, 'offsetHeight', 'get').mockImplementation(function (this: HTMLElement) {
      return this.getAttribute('role') === 'listbox' ? 200 : 0;
    });
    const styleSheets = document.head.querySelectorAll('style').length;
    render(<BackendChoice />);
    await userEvent.click(screen.getByRole('combobox', { name: 'Draft backend' }));
    const listbox = screen.getByRole('listbox');
    // A trigger near the foot of the screen has no room under it: the list stands on it.
    expect(listbox.style.left).toBe('40px');
    expect(listbox.style.bottom).toBe('70px');
    expect(listbox.style.top).toBe('');
    expect(document.body).not.toHaveAttribute('style');
    expect(document.body).not.toHaveAttribute('data-scroll-locked');
    expect(document.head.querySelectorAll('style')).toHaveLength(styleSheets);
  });

  it('closes from its scrim or Tab without committing, and returns focus to the trigger', async () => {
    render(<BackendChoice />);
    const trigger = screen.getByRole('combobox', { name: 'Draft backend' });
    await userEvent.click(trigger);
    await userEvent.click(screen.getByRole('listbox').parentElement as HTMLElement);
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    await waitFor(() => expect(trigger).toHaveFocus());

    await userEvent.keyboard('{ArrowDown}{End}{Tab}');
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    await waitFor(() => expect(trigger).toHaveFocus());
    expect(trigger).toHaveTextContent('Automatic');
  });

  it('closes on the phone back before the screen under it', async () => {
    render(<BackendChoice />);
    const trigger = screen.getByRole('combobox', { name: 'Draft backend' });
    await userEvent.click(trigger);
    act(() => {
      expect(dismissTopLayer()).toBe(true);
    });
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    await waitFor(() => expect(trigger).toHaveFocus());
    expect(dismissTopLayer()).toBe(false);
  });
});

const groupChoices = [
  { value: 'plan', label: 'Planning' },
  { value: 'review', label: 'Review' },
  { value: 'old', label: 'Archive', disabled: true },
];

function GroupFilter() {
  const [values, setValues] = useState<string[]>([]);
  return (
    <>
      <Select
        aria-label="Groups"
        values={values}
        onValuesChange={setValues}
        noneLabel="All groups"
        options={groupChoices}
      />
      <Button variant="secondary">Continue</Button>
    </>
  );
}

describe('Select with several choices', () => {
  it('toggles choices by tap, Space and Enter while the list stays open, and Escape returns focus', async () => {
    render(<GroupFilter />);
    const trigger = screen.getByRole('combobox', { name: 'Groups' });
    expect(trigger).toHaveTextContent('All groups');
    await userEvent.click(trigger);
    const listbox = screen.getByRole('listbox', { name: 'Groups' });
    expect(listbox).toHaveAttribute('aria-multiselectable', 'true');
    expect(screen.getByRole('option', { name: 'All groups' })).toHaveAttribute('aria-selected', 'true');
    await userEvent.click(screen.getByRole('option', { name: 'Planning' }));
    expect(screen.getByRole('listbox')).toBe(listbox);
    expect(screen.getByRole('option', { name: 'Planning' })).toHaveAttribute('aria-selected', 'true');
    expect(screen.getByRole('option', { name: 'All groups' })).toHaveAttribute('aria-selected', 'false');
    await userEvent.keyboard('{ArrowDown}');
    await waitFor(() => expect(screen.getByRole('option', { name: 'Review' })).toHaveFocus());
    await userEvent.keyboard(' ');
    expect(trigger).toHaveTextContent('Planning, Review');
    await userEvent.keyboard('{Enter}');
    expect(trigger).toHaveTextContent('Planning');
    expect(screen.getByRole('listbox')).toBe(listbox);
    await userEvent.keyboard('{Escape}');
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
    await waitFor(() => expect(trigger).toHaveFocus());
    expect(trigger).toHaveTextContent('Planning');
  });

  it('clears every choice from its none row and closes', async () => {
    render(<GroupFilter />);
    const trigger = screen.getByRole('combobox', { name: 'Groups' });
    await userEvent.click(trigger);
    await userEvent.click(screen.getByRole('option', { name: 'Planning' }));
    await userEvent.click(screen.getByRole('option', { name: 'Review' }));
    expect(trigger).toHaveTextContent('Planning, Review');
    await userEvent.click(screen.getByRole('option', { name: 'All groups' }));
    expect(trigger).toHaveTextContent('All groups');
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument();
  });

  it('closes on an outside press and is unavailable with nothing to choose', async () => {
    render(
      <>
        <GroupFilter />
        <Select aria-label="Empty groups" values={[]} onValuesChange={vi.fn()} noneLabel="All groups" options={[]} />
      </>,
    );
    const trigger = screen.getByRole('combobox', { name: 'Groups' });
    await userEvent.click(trigger);
    await userEvent.click(screen.getByRole('option', { name: 'Review' }));
    fireEvent.pointerDown(document.body);
    await waitFor(() => expect(screen.queryByRole('listbox')).not.toBeInTheDocument());
    expect(trigger).toHaveTextContent('Review');
    const empty = screen.getByRole('combobox', { name: 'Empty groups' });
    expect(empty).toBeDisabled();
    expect(empty).toHaveTextContent('All groups');
  });
});
