// @vitest-environment jsdom
// #233: live Activity must keep mounted rows and the reader's disclosure choices.
import { fireEvent, render, screen } from '@testing-library/react';
import { expect, it } from 'vitest';
import { ActivityPanel } from './ActivityPanel';
import {
  groupedActivityHistoryPage as historyPage,
  GROUPED_ACTIVITY_HISTORY_IDS as ids,
} from '../dev/activity-history';

function page(id: string) {
  const activity = historyPage(id);
  activity.rows.forEach((row) => {
    row.presentation = {
      headline: row.summary,
      summary: '',
      content: [{ type: 'text', text: row.evidence[0].text }],
    };
  });
  return activity;
}

function openFirstRead() {
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  const group = screen.queryByRole('button', { name: /Read ×2/ });
  if (group) fireEvent.click(group);
  const row = document.querySelector('[data-activity-row]')!;
  fireEvent.click(row.querySelector('button')!);
  return { row, content: screen.getByText('Read result 1-1') };
}

it('retains group, row and content nodes across live history revisions', () => {
  const activity = page(ids[0]);
  const view = render(<ActivityPanel activity={activity} />);
  const { row, content } = openFirstRead();
  const group = screen.getByRole('button', { name: /Read ×2/ });
  for (let revision = 2; revision <= 4; revision++) {
    const updated = structuredClone(activity);
    updated.history!.revision = revision;
    updated.rows[1].duration_ms = revision * 100;
    view.rerender(<ActivityPanel activity={updated} />);
    expect(group.isConnected).toBe(true);
    expect(group).toHaveAttribute('aria-expanded', 'true');
    expect(row.isConnected).toBe(true);
    expect(content).toBeVisible();
  }
});

it.each([false, true])(
  'keeps a singleton mounted when a repeat arrives (same arguments: %s)',
  (same) => {
    const activity = page(ids[0]);
    delete activity.history;
    if (same)
      activity.rows.forEach((row) => {
        row.argument_key = 'same';
      });
    const view = render(
      <ActivityPanel activity={{ ...activity, rows: activity.rows.slice(0, 1) }} />,
    );
    const { row, content } = openFirstRead();
    view.rerender(<ActivityPanel activity={activity} />);
    expect(row.isConnected).toBe(true);
    expect(content).toBeVisible();
    expect(screen.getByRole('button', { name: /Read ×2/ })).toHaveAttribute(
      'aria-expanded',
      'true',
    );
  },
);

it('retains the first receipt when another form adds Activity to the turn', () => {
  const activity = page(ids[0]);
  const view = render(<ActivityPanel activity={[activity]} />);
  const { row, content } = openFirstRead();
  view.rerender(<ActivityPanel activity={[activity, page(ids[1])]} />);
  expect(row.isConnected).toBe(true);
  expect(content).toBeVisible();
  expect(screen.getByRole('button', { name: /Read ×4/ })).toHaveAttribute('aria-expanded', 'true');
});
