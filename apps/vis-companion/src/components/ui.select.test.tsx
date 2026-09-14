/** @vitest-environment jsdom */
import { createRef, useState } from 'react';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it, vi } from 'vitest';
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

  it('isolates the open picker and restores only its own inert attributes on close and unmount', async () => {
    const alreadyInert = render(<Button>Unavailable</Button>).container;
    alreadyInert.setAttribute('inert', '');
    const liveRegion = render(<div aria-live="polite">Changes saved</div>).container;
    const { container, unmount } = render(<BackendChoice />);
    const trigger = screen.getByRole('combobox');
    expect(container).not.toHaveAttribute('inert');

    await userEvent.click(trigger);
    expect(container).toHaveAttribute('inert');
    expect(alreadyInert).toHaveAttribute('inert');
    expect(liveRegion).not.toHaveAttribute('inert');
    await userEvent.keyboard('{Escape}');
    expect(container).not.toHaveAttribute('inert');
    expect(alreadyInert).toHaveAttribute('inert');
    await waitFor(() => expect(trigger).toHaveFocus());

    await userEvent.click(trigger);
    expect(container).toHaveAttribute('inert');
    unmount();
    expect(container).not.toHaveAttribute('inert');
    expect(alreadyInert).toHaveAttribute('inert');
    expect(liveRegion).not.toHaveAttribute('inert');
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
});
