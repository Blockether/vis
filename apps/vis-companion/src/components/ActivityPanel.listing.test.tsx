// @vitest-environment jsdom
// Regression #251: List summaries stay concise until a specific call is opened.
import { fireEvent, render, screen, within } from '@testing-library/react';
import { expect, it } from 'vitest';
import { ACTIVITY_LISTING, ACTIVITY_LISTING_BATCH } from '../dev/story-data';
import { ActivityPanel } from './ActivityPanel';

function listings() {
  const activity = structuredClone(ACTIVITY_LISTING);
  activity.rows.push({
    ...structuredClone(activity.rows[0]),
    id: 'ls-2',
    sequence: 2,
    presentation: {
      headline: 'Listed apps/vis-companion/test',
      summary: '0 directories · 1 file',
      content: [{ type: 'table', columns: ['Name'], rows: [['second.test.ts']] }],
    },
  });
  activity.counts.succeeded = 2;
  return activity;
}

it.each([false, true])('keeps grouped List concise (streamed: %s)', (streamed) => {
  const activity = listings();
  const view = render(
    <ActivityPanel activity={streamed ? { ...activity, rows: activity.rows.slice(0, 1) } : activity} />,
  );
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  if (streamed) view.rerender(<ActivityPanel activity={activity} />);
  const group = screen.getByRole('button', { name: /List ×2/ });
  expect(group).toHaveAttribute('aria-expanded', 'false');
  expect(document.querySelector('[data-activity-row]')).toBeNull();
  expect(screen.queryByRole('table')).toBeNull();

  fireEvent.click(group);
  expect(screen.getByText('3 directories · 2 files')).toBeVisible();
  expect(screen.queryByRole('table')).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: /Listed apps\/vis-companion\/test/ }));
  expect(screen.getAllByRole('table')).toHaveLength(1);
  expect(screen.getByText('second.test.ts')).toBeVisible();
  expect(screen.queryByText('main.tsx')).toBeNull();

  view.rerender(<ActivityPanel activity={structuredClone(activity)} />);
  expect(group).toHaveAttribute('aria-expanded', 'true');
  expect(screen.getByText('second.test.ts')).toBeVisible();
});

it('keeps multi-directory sections behind their List call', () => {
  render(<ActivityPanel activity={ACTIVITY_LISTING_BATCH} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(screen.getByText('7 entries')).toBeVisible();
  expect(document.querySelector('[data-activity-section]')).toBeNull();
  const call = screen.getByRole('button', { name: /Listed 2 directories/ });
  fireEvent.click(call);
  expect(document.querySelectorAll('[data-activity-section]')).toHaveLength(2);
  expect(screen.queryByRole('table')).toBeNull();
  const section = document.querySelectorAll('[data-activity-section]')[1] as HTMLElement;
  fireEvent.click(within(section).getByRole('button'));
  expect(screen.getByText('listing.test.ts')).toBeVisible();
  expect(screen.queryByText('main.tsx')).toBeNull();
  fireEvent.click(call);
  expect(document.querySelector('[data-activity-section]')).toBeNull();
  expect(screen.queryByRole('table')).toBeNull();
});

it.each(['running', 'failed'] as const)('does not auto-open %s List details', (state) => {
  const activity = structuredClone(ACTIVITY_LISTING);
  activity.rows[0].state = state;
  render(<ActivityPanel activity={activity} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  const call = screen.getByRole('button', { name: /Listed apps\/vis-companion\/src/ });
  expect(call).toHaveAttribute('aria-expanded', 'false');
  expect(screen.queryByRole('table')).toBeNull();
  fireEvent.click(call);
  expect(screen.getByRole('table')).toBeVisible();
});
