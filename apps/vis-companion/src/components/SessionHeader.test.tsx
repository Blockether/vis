// @vitest-environment jsdom

import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
import { STORY_GOAL } from '../dev/story-data';
import { SessionHeader } from './SessionHeader';

const model = {
  title: 'Investigate stalled stream',
  sessionId: '123e4567-e89b-12d3-a456-426614174000',
  connected: true,
  artifacts: { count: 3, isOpen: false },
} as const;

describe('SessionHeader', () => {
  // Regression, user report (paraphrased): the trailing mark's dots ran across instead
  // of down, and behind it the artifacts sat over a captioned id chip. Both verbs are
  // rows now — a mark, then what pressing it does — stacked one under the other.
  it('hangs both session verbs as marked rows and copies without closing the menu', async () => {
    const written: string[] = [];
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { writeText: async (text: string) => void written.push(text) },
    });
    render(
      <SessionHeader
        model={{ ...model, artifacts: { count: 3, isOpen: true } }}
        commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
      />,
    );
    fireEvent.click(screen.getByRole('button', { name: /^Session actions/ }));

    const rows = within(screen.getByRole('dialog', { name: 'Session actions' })).getAllByRole(
      'button',
    );
    expect(rows).toHaveLength(2);
    // An open surface names the verb that closes it, and every row LEADS with its mark.
    expect(rows[0]).toHaveTextContent('Hide artifacts (3)');
    expect(rows[1]).toHaveTextContent('Copy session id');
    expect(rows.map((row) => Boolean(row.firstElementChild?.querySelector('svg')))).toEqual([
      true,
      true,
    ]);

    fireEvent.click(rows[1]);
    await waitFor(() =>
      expect(written).toEqual(['vis_session_id#123e4567-e89b-12d3-a456-426614174000']),
    );
    // The clipboard answers nothing on its own, so the row keeps the panel standing and
    // says so itself.
    expect(screen.getByRole('dialog', { name: 'Session actions' })).toBeInTheDocument();
    await waitFor(() => expect(rows[1]).toHaveTextContent('copied'));
  });

  // The screen under this header reads Escape as "cancel the running turn", so an open
  // menu has to spend that key itself.
  it('closes its menu on Escape without spending the key on the screen below', () => {
    const below = vi.fn();
    window.addEventListener('keydown', below);
    try {
      render(
        <SessionHeader model={model} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />,
      );
      fireEvent.click(screen.getByRole('button', { name: /^Session actions/ }));
      expect(screen.getByRole('dialog', { name: 'Session actions' })).toBeInTheDocument();

      fireEvent.keyDown(document.body, { key: 'Escape' });
      expect(screen.queryByRole('dialog', { name: 'Session actions' })).not.toBeInTheDocument();
      expect(below).not.toHaveBeenCalled();
    } finally {
      window.removeEventListener('keydown', below);
    }
  });
  it('shows only status and time as text that opens nothing, without the objective', () => {
    const { rerender } = render(
      <SessionHeader model={model} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />,
    );
    expect(screen.queryByText(/^Goal:/)).not.toBeInTheDocument();
    const goal = {
      ...STORY_GOAL,
      objective: 'Verify a very long objective '.repeat(50),
      status: 'blocked' as const,
    };
    rerender(
      <SessionHeader
        model={{ ...model, goal }}
        commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
      />,
    );
    const goalText = screen.getByText(/^Goal: Blocked/);
    expect(goalText).toHaveTextContent(/^Goal: Blocked - 32s$/);
    expect(goalText.closest('button')).toBeNull();
    fireEvent.click(goalText);
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
    expect(screen.queryByText(goal.objective.trim())).not.toBeInTheDocument();
  });
  it('shows no goal after /goal --clear or a complete goal', () => {
    for (const status of ['cleared', 'complete'] as const) {
      const { unmount } = render(
        <SessionHeader
          model={{ ...model, goal: { ...STORY_GOAL, status } }}
          commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
        />,
      );
      expect(screen.queryByText(/^Goal:/)).not.toBeInTheDocument();
      unmount();
    }
  });
  it('ticks active wall time, freezes inactive goals and cleans up the clock', () => {
    vi.useFakeTimers();
    vi.setSystemTime(STORY_GOAL.updated_at + 3_693_000);
    try {
      const { rerender, unmount } = render(
        <SessionHeader
          model={{ ...model, goal: STORY_GOAL }}
          commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
        />,
      );
      const goalText = () => screen.getByText(/^Goal:/);
      expect(goalText()).toHaveTextContent('Goal: Active - 1h 2m 5s');
      act(() => vi.advanceTimersByTime(2_000));
      expect(goalText()).toHaveTextContent('Goal: Active - 1h 2m 7s');
      for (const status of [
        'paused',
        'blocked',
        'budget_limited',
        'cancelled',
      ] as const) {
        rerender(
          <SessionHeader
            model={{ ...model, goal: { ...STORY_GOAL, status, time_used_ms: 3_727_000 } }}
            commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
          />,
        );
        act(() => vi.advanceTimersByTime(5_000));
        expect(goalText()).toHaveTextContent(/ - 1h 2m 7s$/);
      }
      rerender(
        <SessionHeader
          model={{
            ...model,
            goal: { ...STORY_GOAL, time_used_ms: 3_727_000, updated_at: Date.now() },
          }}
          commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
        />,
      );
      act(() => vi.advanceTimersByTime(1_000));
      expect(goalText()).toHaveTextContent('Goal: Active - 1h 2m 8s');
      rerender(
        <SessionHeader
          model={{
            ...model,
            goal: {
              ...STORY_GOAL,
              id: 'replacement',
              time_used_ms: 0,
              updated_at: Date.now() + 1_000,
            },
          }}
          commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
        />,
      );
      expect(goalText()).toHaveTextContent('Goal: Active - 0s');
      unmount();
      expect(vi.getTimerCount()).toBe(0);
    } finally {
      vi.useRealTimers();
    }
  });
});
