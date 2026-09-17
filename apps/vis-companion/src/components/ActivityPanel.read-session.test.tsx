// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it } from 'vitest';
import readSessionFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-read-session.json';
import { activityProjectionFromWire } from '../lib/activity';
import { ActivityPanel } from './ActivityPanel';

afterEach(cleanup);

// Regression #230: the engine-generated multi-turn result stays compact until
// the reader chooses the relevant evidence; rendering never repeats a failure.
it('shows session usage and turn boundaries before independently revealing full evidence', () => {
  const activity = activityProjectionFromWire(readSessionFixture)!;
  render(<ActivityPanel activity={activity} />);
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  fireEvent.click(screen.getByRole('button', { name: /Read session/ }));
  for (const heading of ['Current turn', 'Usage', 'Diagnosis', 'Turns']) {
    expect(screen.getByRole('heading', { name: heading })).toBeVisible();
  }
  const main = document.querySelector('[data-activity-content]')!;
  expect(main.textContent!.length).toBeLessThan(2000);
  for (const count of ['12345', '6700', '100', '5545', '890', '120']) {
    expect(main.textContent).toContain(count);
  }
  expect(main.textContent).toContain('$0.125');
  expect(main.textContent).not.toContain('[REDACTED]');
  expect(document.body.textContent).not.toContain('Final request requirement.');
  expect(document.body.textContent).not.toContain('Final failure detail.');
  const turns = screen.getByRole('button', { name: 'Turn details' });
  const failures = screen.getByRole('button', { name: 'Failure details' });
  expect(turns.getAttribute('aria-expanded')).toBe('false');
  expect(failures.getAttribute('aria-expanded')).toBe('false');
  fireEvent.click(turns);
  expect(document.body.textContent).toContain('Final request requirement.');
  expect(document.body.textContent).not.toContain('Final failure detail.');
  fireEvent.click(failures);
  expect(document.body.textContent!.split('Final failure detail.')).toHaveLength(2);
  expect(document.body.textContent).not.toContain('fixture-secret');
  fireEvent.click(turns);
  expect(document.body.textContent).not.toContain('Final request requirement.');
  expect(document.body.textContent).toContain('Final failure detail.');
});
