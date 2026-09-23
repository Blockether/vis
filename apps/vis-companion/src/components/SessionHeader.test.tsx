// @vitest-environment jsdom

import { Capacitor } from '@capacitor/core';
import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import gatewaySchema from '../../../../packages/vis-contract/resources/vis-contract/schema/gateway.json';
import { STORY_GOAL } from '../dev/story-data';
import type { SessionGoal } from '../lib/types';
import { SessionHeader } from './SessionHeader';

const model = {
  title: 'Investigate stalled stream',
  sessionId: '123e4567-e89b-12d3-a456-426614174000',
  connected: true,
  artifacts: { count: 3, isOpen: false },
} as const;

describe('SessionHeader', () => {
  it.each(gatewaySchema.$defs.session_goal.properties.status.oneOf)(
    'shows the schema title for $const',
    ({ const: status, title }) => {
      render(
        <SessionHeader
          model={{ ...model, goal: { ...STORY_GOAL, status: status as SessionGoal['status'] } }}
          commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
        />,
      );
      expect(screen.getByRole('button', { name: /^Goal:/ })).toHaveTextContent(`Goal: ${title}`);
    },
  );
  it('exposes one session identity and its two navigation commands', () => {
    const back = vi.fn();
    const toggleArtifacts = vi.fn();
    render(<SessionHeader model={model} commands={{ back, toggleArtifacts }} />);

    expect(screen.getByRole('heading', { name: model.title })).toBeInTheDocument();
    expect(screen.getByText('Connected')).toBeInTheDocument();
    // The band keeps the sentence; the id and the artifacts stand behind one kebab, whose
    // own name still carries the count the artifacts chip used to paint out loud.
    expect(screen.queryByRole('button', { name: /^Copy session id/ })).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Session actions, 3 artifacts' }));

    expect(screen.getByRole('dialog', { name: 'Session actions' })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /^Copy session id/ })).toHaveTextContent('123e4567');
    fireEvent.click(screen.getByRole('button', { name: 'Open artifacts (3)' }));
    expect(toggleArtifacts).toHaveBeenCalledOnce();
    expect(screen.queryByRole('dialog', { name: 'Session actions' })).not.toBeInTheDocument();

    fireEvent.click(screen.getByRole('button', { name: 'Back to sessions' }));
    expect(back).toHaveBeenCalledOnce();
  });

  it('renders the reconnecting state and omits an empty artifact door', () => {
    render(
      <SessionHeader
        model={{
          ...model,
          connected: false,
          artifacts: { count: 0, isOpen: false },
        }}
        commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
      />,
    );

    expect(screen.getByText('Reconnecting')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Session actions' }));
    expect(screen.queryByRole('button', { name: /artifacts/i })).not.toBeInTheDocument();
    expect(screen.getByRole('button', { name: /^Copy session id/ })).toBeInTheDocument();
  });

  // Regression: moving controls beside the iPhone island obscured their intended
  // row. Give the whole session header a safe inset so its actions sit below it.
  it('keeps iPhone session actions below the island', () => {
    const platform = vi.spyOn(Capacitor, 'getPlatform').mockReturnValue('ios');
    const native = vi.spyOn(Capacitor, 'isNativePlatform').mockReturnValue(true);
    try {
      render(<SessionHeader model={model} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />);
      const header = screen.getByRole('heading', { name: model.title }).closest('header')!;
      expect(header).toHaveClass('flex', 'pt-[env(safe-area-inset-top)]', 'sm:pt-0');
      expect(header).not.toHaveClass('grid');
      expect(screen.getByRole('button', { name: 'Back to sessions' })).not.toHaveClass('row-start-1');
      expect(
        screen.getByRole('button', { name: 'Session actions, 3 artifacts' }).parentElement,
      ).not.toHaveClass('row-start-1');
    } finally {
      native.mockRestore();
      platform.mockRestore();
    }
  });

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
  it('shows only explicitly supplied goals and opens the full objective', () => {
    const { rerender } = render(
      <SessionHeader model={model} commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }} />,
    );
    expect(screen.queryByRole('button', { name: /^Goal:/ })).not.toBeInTheDocument();
    const goal = {
      ...STORY_GOAL,
      objective: 'Verify a very long objective '.repeat(50),
      status: 'blocked' as const,
      reason: 'Awaiting test credentials.',
    };
    rerender(
      <SessionHeader
        model={{ ...model, goal }}
        commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
      />,
    );
    const goalButton = screen.getByRole('button', { name: /^Goal: Blocked/ });
    expect(goalButton).toHaveTextContent(/^Goal: Blocked - 32s$/);
    expect(screen.queryByText(goal.objective.trim())).not.toBeInTheDocument();
    fireEvent.click(goalButton);
    expect(screen.getByText(goal.objective.trim())).toBeInTheDocument();
    expect(screen.getByRole('dialog', { name: 'Session goal' })).toBeInTheDocument();
    expect(screen.getByText(goal.reason)).toBeInTheDocument();
    expect(screen.getByText('Iterations: 12 / 30')).toBeInTheDocument();
    expect(screen.getByText('Time in goal: 32s')).toBeInTheDocument();
    expect(screen.queryByText(/tokens used/)).not.toBeInTheDocument();
    expect(screen.queryByText(/100,000 budget/)).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Close session goal' }));
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });
  it('distinguishes unlimited iterations from a reached iteration limit', () => {
    const { rerender } = render(
      <SessionHeader
        model={{ ...model, goal: { ...STORY_GOAL, iteration_budget: null } }}
        commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
      />,
    );
    fireEvent.click(screen.getByRole('button', { name: /^Goal:/ }));
    expect(screen.getByText('Iterations: 12 / unlimited')).toBeInTheDocument();
    rerender(
      <SessionHeader
        model={{ ...model, goal: { ...STORY_GOAL, status: 'budget_limited', iterations_used: 30 } }}
        commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
      />,
    );
    expect(screen.getByText('Iterations: 30 / 30')).toBeInTheDocument();
    expect(screen.getByText('Iteration limit reached')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Close session goal' }));
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
      expect(screen.getByRole('button', { name: /^Goal:/ })).toHaveTextContent(
        'Goal: Active - 1h 2m 5s',
      );
      fireEvent.click(screen.getByRole('button', { name: /^Goal:/ }));
      expect(screen.getByText('Time in goal: 1h 2m 5s')).toBeInTheDocument();
      act(() => vi.advanceTimersByTime(2_000));
      expect(screen.getByText('Time in goal: 1h 2m 7s')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /^Goal:/ })).toHaveTextContent(
        'Goal: Active - 1h 2m 7s',
      );
      for (const status of [
        'paused',
        'blocked',
        'budget_limited',
        'complete',
        'cancelled',
      ] as const) {
        rerender(
          <SessionHeader
            model={{ ...model, goal: { ...STORY_GOAL, status, time_used_ms: 3_727_000 } }}
            commands={{ back: vi.fn(), toggleArtifacts: vi.fn() }}
          />,
        );
        act(() => vi.advanceTimersByTime(5_000));
        expect(screen.getByText('Time in goal: 1h 2m 7s')).toBeInTheDocument();
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
      expect(screen.getByText('Time in goal: 1h 2m 8s')).toBeInTheDocument();
      fireEvent.click(screen.getByRole('button', { name: 'Close session goal' }));
      act(() => vi.advanceTimersByTime(10_000));
      fireEvent.click(screen.getByRole('button', { name: /^Goal:/ }));
      expect(screen.getByText('Time in goal: 1h 2m 18s')).toBeInTheDocument();
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
      expect(screen.getByText('Time in goal: 0s')).toBeInTheDocument();
      unmount();
      expect(vi.getTimerCount()).toBe(0);
    } finally {
      vi.useRealTimers();
    }
  });
});
