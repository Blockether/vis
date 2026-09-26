// @vitest-environment jsdom
import { act, cleanup, fireEvent, render } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';
import { AssistantMessage } from './ChatContent';
import {
  readFinishedTurnsExpanded,
  setFinishedTurnsExpanded,
} from '../lib/transcript-display';
import digestCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-digest.json';
import type { TranscriptTurn } from '../lib/types';

afterEach(() => {
  cleanup();
  setFinishedTurnsExpanded(false);
  localStorage.clear();
});

const [failing, external] = digestCases.filter((sample) => sample.valid).map((s) => s.digest);
const malformed = digestCases.find((sample) => !sample.valid)!.digest;

const search = {
  state: 'succeeded',
  counts: { running: 0, succeeded: 1, failed: 0, cancelled: 0 },
  rows: [
    {
      id: 'call-1',
      sequence: 1,
      operation: 'grep',
      presenter: 'observation',
      signal: 'observation',
      state: 'succeeded',
      summary: '3 matches',
      resources: [],
      evidence: [],
    },
  ],
  omitted: { rows: 0, by_classification: {} },
};

function turnWith(digest: unknown, status = 'completed'): TranscriptTurn {
  return {
    turn_id: 'digest-turn',
    status,
    iterations: [
      { id: 'iteration-1', position: 1, forms: [{ source: 'search()', activity: search }] },
    ],
    content: [{ id: 'answer', type: 'prose', markdown: 'The parser still fails one test.' }],
    ...(digest === undefined ? {} : { digest }),
  } as unknown as TranscriptTurn;
}

const trace = (view: ReturnType<typeof render>) =>
  view.container.querySelector('[data-activity-axis]');

describe('a finished turn folded to its digest', () => {
  it('shows the one-line summary, the outcomes that need the reader, then the answer', () => {
    const view = render(<AssistantMessage turn={turnWith(failing)} />);
    const row = view.getByRole('button', { name: failing.summary });
    const attention = view.container.querySelector('[data-activity-attention]');

    expect(row).toHaveAttribute('aria-expanded', 'false');
    // A failing check is the loud thing on the row: the summary takes the failure ink.
    expect(view.getByText(failing.summary)).toHaveClass('text-err');
    expect(attention?.textContent).toContain('3 passed, 1 failed');
    expect(trace(view)).toBeNull();
    expect(view.container.textContent).toContain('The parser still fails one test.');
  });

  it('opens the whole trace beneath the digest and keeps it after folding again', () => {
    const view = render(<AssistantMessage turn={turnWith(failing)} />);
    const row = view.getByRole('button', { name: failing.summary });

    fireEvent.click(row);
    expect(row).toHaveAttribute('aria-expanded', 'true');
    expect(trace(view)).toBeVisible();
    expect(view.container.querySelector('[data-activity-attention]')).toBeNull();

    fireEvent.click(row);
    expect(row).toHaveAttribute('aria-expanded', 'false');
    expect(trace(view)).not.toBeVisible();
    expect(view.container.querySelector('[data-activity-attention]')).toHaveTextContent(
      '3 passed, 1 failed',
    );
  });

  it('keeps a turn without open problems in the ordinary ink and pins nothing', () => {
    const view = render(<AssistantMessage turn={turnWith(external)} />);

    expect(view.getByText(external.summary)).not.toHaveClass('text-err');
    expect(view.container.querySelector('[data-activity-attention]')).toBeNull();
    expect(trace(view)).toBeNull();
  });

  it('shows every step once the reader expands finished turns on this device', () => {
    expect(readFinishedTurnsExpanded()).toBe(false);
    act(() => setFinishedTurnsExpanded(true));
    expect(localStorage.getItem('vis.expand_finished_turns')).toBe('expanded');

    const view = render(<AssistantMessage turn={turnWith(failing)} />);
    expect(view.queryByRole('button', { name: failing.summary })).toBeNull();
    expect(trace(view)).toBeVisible();

    act(() => setFinishedTurnsExpanded(false));
    expect(localStorage.getItem('vis.expand_finished_turns')).toBe('folded');
    expect(view.getByRole('button', { name: failing.summary })).toBeInTheDocument();
    expect(trace(view)).toBeNull();
  });

  it.each([
    ['the turn being written', turnWith(failing, 'running'), true],
    ['a turn without a digest', turnWith(undefined), false],
    ['a digest that breaks the contract', turnWith(malformed), false],
  ])('never folds %s', (_name, turn, streaming) => {
    const view = render(<AssistantMessage turn={turn} streaming={streaming} />);

    expect(view.container.querySelector('[data-turn-digest]')).toBeNull();
    expect(trace(view)).toBeVisible();
  });
});
