// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it } from 'vitest';
import { IterationTrace } from './ChatContent';
import type { ActivityProjection } from '../lib/activity';
import { reduceRunningTurnEvent, type RunningTurn } from '../lib/running-turn';
import type { SseEvent } from '../lib/types';

afterEach(cleanup);

const outcomes = ['succeeded', 'failed', 'cancelled'] as const;
type Outcome = (typeof outcomes)[number];

function snapshot(state: 'running' | Outcome, revision?: number): ActivityProjection {
  const running = state === 'running';
  const headline = running ? 'Checking suite' : `Suite ${state}`;
  return {
    ...(revision === undefined
      ? {}
      : {
          history: {
            id: '11111111-2222-3333-4444-555555555555',
            revision,
            total: 1,
            after: 0,
            next_after: null,
          },
        }),
    state,
    counts: { running: 0, succeeded: 0, failed: 0, cancelled: 0, [state]: 1 },
    rows: [
      {
        id: 'suite-call',
        sequence: 1,
        operation: 'suite',
        presenter: 'tests',
        signal: 'verification',
        state,
        summary: headline,
        ...(running ? {} : { duration_ms: 125, result_summary: `Final ${state} evidence` }),
        resources: [],
        evidence: [],
        presentation: {
          headline,
          summary: running ? 'Waiting for suite' : `Final ${state} summary`,
          content: running
            ? [{ type: 'progress', label: 'Suite progress' }]
            : [{ type: 'text', text: `Final ${state} evidence` }],
        },
      },
    ],
    omitted: { rows: 0, by_classification: {} },
  };
}

function apply(turn: RunningTurn | null, value: Record<string, unknown>): RunningTurn {
  const next = reduceRunningTurnEvent(turn, value as unknown as SseEvent);
  if (!next) throw new Error('expected a running turn');
  return next;
}

function start(): RunningTurn {
  return apply(apply(null, { type: 'turn.started', turn_id: 'lifecycle-turn' }), {
    type: 'block.started',
    iteration: 1,
    form_index: 0,
    scope: 'python',
    code: "shell('npm test')",
  });
}

function activityEvent(turn: RunningTurn, activity: ActivityProjection): RunningTurn {
  return apply(turn, { type: 'block.activity', iteration: 1, form_index: 0, activity });
}

function trace(turn: RunningTurn) {
  return <IterationTrace iterations={turn.iterations} showCode={false} whole />;
}

it.each(outcomes.flatMap((outcome) => [false, true].map((history) => ({ outcome, history }))))(
  'replaces running Activity with $outcome through live events (history: $history)',
  ({ outcome, history }) => {
    const running = activityEvent(start(), snapshot('running', history ? 1 : undefined));
    const view = render(trace(running));
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    expect(screen.getByText('Checking suite')).toBeVisible();
    expect(screen.getByText('Suite progress · In progress')).toBeVisible();
    expect(screen.getByLabelText('Running')).toBeVisible();

    const settled = activityEvent(running, snapshot(outcome, history ? 2 : undefined));
    view.rerender(trace(settled));
    expect(screen.getByRole('button', { name: 'Collapse Activity' })).toBeVisible();
    expect(document.querySelectorAll('[data-activity-row]')).toHaveLength(1);
    expect(screen.queryByText('Checking suite')).toBeNull();
    expect(screen.queryByText('Waiting for suite')).toBeNull();
    expect(screen.queryByText(/In progress/)).toBeNull();
    expect(screen.queryByLabelText('Running')).toBeNull();
    expect(screen.queryByRole('progressbar')).toBeNull();
    expect(screen.getByText(`Suite ${outcome}`)).toBeVisible();
    expect(screen.getByText(`Final ${outcome} summary`)).toBeVisible();
    const row = screen.getByRole('button', { name: new RegExp(`Suite ${outcome}`) });
    if (row.getAttribute('aria-expanded') !== 'true') fireEvent.click(row);
    expect(screen.getByText(`Final ${outcome} evidence`)).toBeVisible();
  },
);

it.each(
  outcomes.flatMap((outcome) => [false, true].map((determinate) => ({ outcome, determinate }))),
)(
  'settles retained progress after $outcome without inventing counts (determinate: $determinate)',
  ({ outcome, determinate }) => {
    const initial = snapshot('running');
    initial.rows[0].presentation!.content = [
      { type: 'progress', label: 'Suite progress', ...(determinate ? { value: 1, total: 3 } : {}) },
    ];
    const running = activityEvent(start(), initial);
    const view = render(trace(running));
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    expect(screen.getByRole('progressbar', { name: 'Suite progress' })).toBeVisible();

    // A terminal callback returning None retains authored presentation, not lifecycle state.
    const terminal = snapshot(outcome);
    terminal.rows[0].presentation = initial.rows[0].presentation;
    view.rerender(trace(activityEvent(running, terminal)));
    const row = screen.getByRole('button', { name: /Checking suite/ });
    if (row.getAttribute('aria-expanded') !== 'true') fireEvent.click(row);
    expect(screen.queryByLabelText('Running')).toBeNull();
    expect(screen.queryByText(/In progress/)).toBeNull();
    expect(screen.getByText('Checking suite')).toBeVisible();
    if (determinate) {
      expect(screen.getByText('Suite progress · 1 / 3')).toBeVisible();
      expect(screen.getByRole('progressbar').getAttribute('value')).toBe('1');
      expect(screen.getByRole('progressbar').getAttribute('max')).toBe('3');
    } else {
      expect(screen.getByText('Suite progress · Stopped')).toBeVisible();
      expect(screen.queryByRole('progressbar')).toBeNull();
    }
  },
);

it.each(outcomes)('does not resurrect running after a %s revision', (outcome) => {
  const running = activityEvent(start(), snapshot('running', 1));
  const settled = activityEvent(running, snapshot(outcome, 2));
  const view = render(trace(settled));
  fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
  const replay = activityEvent(settled, snapshot('running', 1));
  view.rerender(trace(replay));
  expect(screen.queryByText('Checking suite')).toBeNull();
  expect(screen.queryByLabelText('Running')).toBeNull();
  expect(screen.getByText(`Suite ${outcome}`)).toBeVisible();
  expect(replay).toBe(settled);
});

it('accepts a newer running invocation without reopening an already settled operation', () => {
  const settled = activityEvent(start(), snapshot('succeeded', 2));
  const next = snapshot('running', 3);
  next.rows[0].id = 'next-suite-call';
  next.rows[0].sequence = 2;
  next.rows.unshift(snapshot('succeeded').rows[0]);
  next.history!.total = 2;
  next.counts.succeeded = 1;
  const updated = activityEvent(settled, next);
  expect(updated.iterations[0].forms?.[0].activity).toEqual(next);
});
