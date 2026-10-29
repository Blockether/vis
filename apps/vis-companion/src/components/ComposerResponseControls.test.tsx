// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { ComposerResponseControls } from './ComposerResponseControls';

afterEach(cleanup);

describe('composer response controls', () => {
  it('lists every reasoning level and keeps the current one without a write', () => {
    const chooseReasoning = vi.fn();
    render(
      <ComposerResponseControls
        controls={{
          model: {
            value: 'gpt-6-astra',
            title: 'openai-codex/gpt-6-astra',
            choose: vi.fn(),
          },
          reasoning: {
            label: 'Thinking level',
            value: 'high',
            choices: ['none', 'low', 'medium', 'high', 'xhigh'],
            busy: false,
            choose: chooseReasoning,
          },
        }}
      />,
    );
    const chip = screen.getByRole('button', {
      name: 'Thinking level — high, choose a level',
    });
    expect(chip).toHaveAttribute('aria-haspopup', 'dialog');
    expect(chip).toHaveAttribute('aria-expanded', 'false');

    fireEvent.click(chip);
    const list = screen.getByRole('dialog', { name: 'Thinking level' });
    expect(chip).toHaveAttribute('aria-expanded', 'true');
    expect(
      within(list)
        .getAllByRole('button')
        .map((row) => row.textContent),
    ).toEqual(['none', 'low', 'medium', 'highcurrent', 'xhigh']);
    fireEvent.click(within(list).getByRole('button', { name: /^high/ }));
    expect(screen.queryByRole('dialog')).toBeNull();
    expect(chooseReasoning).not.toHaveBeenCalled();

    // Escape closes the list before the screen can read it as "cancel the turn".
    fireEvent.click(chip);
    const cancelTurn = vi.fn();
    window.addEventListener('keydown', cancelTurn);
    fireEvent.keyDown(document.body, { key: 'Escape' });
    window.removeEventListener('keydown', cancelTurn);
    expect(screen.queryByRole('dialog')).toBeNull();
    expect(cancelTurn).not.toHaveBeenCalled();
  });

  it('steps the simplified modes with one tap and opens no list', () => {
    const cycleReasoning = vi.fn();
    render(
      <ComposerResponseControls
        controls={{
          model: {
            value: 'gpt-6-astra',
            title: 'openai-codex/gpt-6-astra',
            choose: vi.fn(),
          },
          reasoning: {
            label: 'Reasoning',
            value: 'balanced',
            busy: false,
            cycle: cycleReasoning,
          },
        }}
      />,
    );

    const chip = screen.getByRole('button', {
      name: 'Reasoning — balanced, tap for the next level',
    });
    expect(chip).not.toHaveAttribute('aria-haspopup');
    fireEvent.click(chip);

    expect(cycleReasoning).toHaveBeenCalledOnce();
    expect(screen.queryByRole('dialog')).toBeNull();
  });

  it('omits response knobs the provider does not expose', () => {
    render(
      <ComposerResponseControls
        controls={{
          model: {
            value: 'model',
            title: 'Change provider and model',
            choose: vi.fn(),
          },
        }}
      />,
    );

    expect(screen.getAllByRole('button')).toHaveLength(1);
    expect(screen.queryByRole('button', { name: /Reasoning/ })).toBeNull();
    expect(screen.queryByRole('button', { name: /Thinking summary/ })).toBeNull();
  });
});
