// @vitest-environment jsdom
// Regression #201: group titles use presentation without changing operation identity.
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, expect, it } from 'vitest';
import groupingCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-groups.json';
import { activityProjectionFromWire } from '../lib/activity';
import { ActivityPanel } from './ActivityPanel';

afterEach(cleanup);

it('labels extension groups and preserves disclosures, arguments and live failures', () => {
  const sample = groupingCases.find(
    (sample) => sample.name === 'extension presentations label exact-operation groups',
  )!;
  const activity = activityProjectionFromWire(sample.projection)!;
  const { rerender } = render(<ActivityPanel activity={activity} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  expect(
    [...document.querySelectorAll('[data-activity-group]')].map((group) =>
      group.getAttribute('data-activity-group'),
    ),
  ).toEqual(['search-1', 'status-1', 'lookup-1']);
  expect(screen.getByRole('button', { name: /Search reviews ×3/ })).toBeTruthy();
  expect(screen.getByRole('button', { name: /Search reviews ×2/ })).toBeTruthy();
  expect(screen.getByRole('button', { name: /Check review deployment ×2/ })).toBeTruthy();
  expect(screen.getByText(/Closed reviews · Review service unavailable/)).toBeTruthy();
  expect(screen.getByText(/Waiting for deployment · running/)).toBeTruthy();
  expect(document.body.textContent).not.toContain('reviews.search');
  expect(document.body.textContent).not.toContain('reviews.deployment_status');

  fireEvent.click(screen.getByRole('button', { name: /Search reviews ×3/ }));
  const group = screen.getByRole('list', {
    name: 'Search reviews ×3 operations',
  });
  expect(
    group.querySelector('[data-activity-arguments]')?.getAttribute('data-activity-arguments'),
  ).toBe('search-1');
  fireEvent.click(within(group).getByRole('button', { name: /Changes: 0 ×2/ }));
  expect(
    [...group.querySelectorAll('[data-activity-row]')].map((row) =>
      row.getAttribute('data-activity-row'),
    ),
  ).toEqual(['search-1', 'search-3', 'search-2']);

  const settled = structuredClone(activity);
  settled.rows.find((row) => row.id === 'status-1')!.state = 'succeeded';
  settled.state = 'failed';
  settled.counts = { running: 0, succeeded: 6, failed: 1, cancelled: 0 };
  rerender(<ActivityPanel activity={settled} />);
  expect(
    screen.getByRole('button', { name: /Search reviews ×3/ }).getAttribute('aria-expanded'),
  ).toBe('true');
  expect(
    within(group)
      .getByRole('button', { name: /Changes: 0 ×2/ })
      .getAttribute('aria-expanded'),
  ).toBe('true');
});

it('keeps an extension disclosure open when its fallback gains a headline', () => {
  const sample = groupingCases.find(
    (sample) =>
      sample.name ===
      'first nonblank extension headline wins in entry order with operation fallback',
  )!;
  const presented = activityProjectionFromWire(sample.projection)!;
  const missing = structuredClone(presented);
  missing.rows.forEach((row) => {
    delete row.presentation;
  });
  const { rerender } = render(<ActivityPanel activity={missing} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: /reviews.search ×4/ }));
  rerender(<ActivityPanel activity={presented} />);
  expect(
    screen.getByRole('button', { name: /Search open reviews ×4/ }).getAttribute('aria-expanded'),
  ).toBe('true');
  expect(screen.getByRole('button', { name: /reviews.custom ×2/ })).toBeTruthy();
  expect(screen.getByRole('button', { name: /Read ×2/ })).toBeTruthy();
});
