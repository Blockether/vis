/** @vitest-environment jsdom */
import { useState } from 'react';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it } from 'vitest';

import { Calendar, DateTimeField } from './Calendar';

const day = (value: string) => document.querySelector<HTMLButtonElement>(`[data-day="${value}"]`);

function Month({ initial, min }: { initial: string; min?: string }) {
  const [value, setValue] = useState(initial);
  return (
    <>
      <Calendar value={value} today="2026-10-11" min={min} onValueChange={setValue} />
      <output>{value}</output>
    </>
  );
}

function Field({ initial }: { initial: string }) {
  const [value, setValue] = useState(initial);
  return (
    <>
      <DateTimeField aria-label="Run at" value={value} today="2026-10-11" isFutureOnly onValueChange={setValue} />
      <output>{value}</output>
    </>
  );
}

describe('Calendar', () => {
  it('draws six Monday-first weeks and marks today, the choice and the days before min', async () => {
    render(<Month initial="2026-10-14" min="2026-10-11" />);
    const days = [...document.querySelectorAll<HTMLElement>('[data-day]')].map((cell) => cell.dataset.day);
    expect(days).toHaveLength(42);
    expect(days[0]).toBe('2026-09-28');
    expect(days.at(-1)).toBe('2026-11-08');
    expect(day('2026-10-11')).toHaveAttribute('aria-current', 'date');
    expect(day('2026-10-14')?.parentElement).toHaveAttribute('aria-selected', 'true');
    expect(day('2026-10-14')).toHaveAttribute('tabindex', '0');
    expect(day('2026-10-10')).toHaveAttribute('aria-disabled', 'true');

    await userEvent.click(day('2026-10-10') as HTMLElement);
    expect(screen.getByRole('status')).toHaveTextContent('2026-10-14');
    await userEvent.click(day('2026-11-02') as HTMLElement);
    expect(screen.getByRole('status')).toHaveTextContent('2026-11-02');
    expect(day('2026-10-01')).toBeNull();
  });

  it('moves by day, week and month from the keyboard and chooses with Enter', async () => {
    render(<Month initial="2026-10-31" />);
    (day('2026-10-31') as HTMLElement).focus();
    await userEvent.keyboard('{ArrowRight}');
    await waitFor(() => expect(day('2026-11-01')).toHaveFocus());
    await userEvent.keyboard('{ArrowDown}');
    await waitFor(() => expect(day('2026-11-08')).toHaveFocus());
    await userEvent.keyboard('{Home}');
    await waitFor(() => expect(day('2026-11-02')).toHaveFocus());
    await userEvent.keyboard('{PageUp}');
    await waitFor(() => expect(day('2026-10-02')).toHaveFocus());
    await userEvent.keyboard('{Shift>}{PageDown}{/Shift}');
    await waitFor(() => expect(day('2027-10-02')).toHaveFocus());
    await userEvent.keyboard('{Enter}');
    expect(screen.getByRole('status')).toHaveTextContent('2027-10-02');
  });

  it('keeps the last day of a shorter month and comes back to today', async () => {
    render(<Month initial="2027-01-31" />);
    await userEvent.click(screen.getByRole('button', { name: 'Next month' }));
    expect(day('2027-02-28')).toHaveAttribute('tabindex', '0');
    await userEvent.click(screen.getByRole('button', { name: 'Today' }));
    await waitFor(() => expect(day('2026-10-11')).toHaveFocus());
    expect(screen.queryByRole('button', { name: 'Today' })).not.toBeInTheDocument();
  });
});

describe('DateTimeField', () => {
  it('opens on the chosen day, keeps the time and closes back to the field', async () => {
    render(<Field initial="2026-10-11T14:00" />);
    const field = screen.getByRole('button', { name: 'Run at' });
    expect(field).toHaveTextContent('14:00');
    await userEvent.click(field);
    expect(screen.getByRole('dialog', { name: 'Run at' })).toBeVisible();
    await waitFor(() => expect(day('2026-10-11')).toHaveFocus());
    await userEvent.click(day('2026-10-20') as HTMLElement);
    expect(screen.getByRole('status', { hidden: true })).toHaveTextContent('2026-10-20T14:00');
    await userEvent.keyboard('{Escape}');
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
    await waitFor(() => expect(field).toHaveFocus());
  });

  it('types and steps the time and closes on Enter', async () => {
    render(<Field initial="2026-10-12T14:00" />);
    await userEvent.click(screen.getByRole('button', { name: 'Run at' }));
    const hours = screen.getByRole('spinbutton', { name: 'Hours' });
    const minutes = screen.getByRole('spinbutton', { name: 'Minutes' });
    await userEvent.click(hours);
    await userEvent.keyboard('09');
    expect(screen.getByRole('status', { hidden: true })).toHaveTextContent('2026-10-12T09:00');
    expect(minutes).toHaveFocus();
    await userEvent.keyboard('{ArrowDown}');
    expect(screen.getByRole('status', { hidden: true })).toHaveTextContent('2026-10-12T09:59');
    fireEvent.change(minutes, { target: { value: '7' } });
    expect(screen.getByRole('status', { hidden: true })).toHaveTextContent('2026-10-12T09:07');
    await userEvent.keyboard('{Enter}');
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });

  it('reads the typed digit, not the caret position, in a focused segment', async () => {
    render(<Field initial="2026-10-12T22:00" />);
    await userEvent.click(screen.getByRole('button', { name: 'Run at' }));
    const hours = screen.getByRole('spinbutton', { name: 'Hours' }) as HTMLInputElement;
    hours.focus();
    // The caret sits between the two digits of 22, as after a second press on the box.
    hours.setSelectionRange(1, 1);
    await userEvent.keyboard('7');
    expect(screen.getByRole('status', { hidden: true })).toHaveTextContent('2026-10-12T07:00');
    expect(screen.getByRole('spinbutton', { name: 'Minutes' })).toHaveFocus();
  });

  it('keeps Tab inside the open panel', async () => {
    render(<Field initial="2026-10-12T14:00" />);
    await userEvent.click(screen.getByRole('button', { name: 'Run at' }));
    const done = screen.getByRole('button', { name: 'Done' });
    done.focus();
    await userEvent.tab();
    expect(screen.getByRole('dialog').contains(document.activeElement)).toBe(true);
    expect(document.activeElement).not.toBe(done);
  });
});
