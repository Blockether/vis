// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { ComposerResponseControls } from './ComposerResponseControls';

afterEach(cleanup);

describe('composer response controls', () => {
  it('owns the complete response-option vocabulary', () => {
    const choose = vi.fn();
    const chooseReasoning = vi.fn();
    const cycleVerbosity = vi.fn();
    const toggleThinking = vi.fn();
    const toggleFast = vi.fn();
    render(
      <ComposerResponseControls
        controls={{
          model: {
            value: 'claude-opus-5',
            title: 'anthropic/claude-opus-5',
            choose,
          },
          reasoning: {
            label: 'Reasoning',
            value: 'high',
            choices: ['low', 'medium', 'high'],
            busy: false,
            choose: chooseReasoning,
          },
          verbosity: {
            label: 'Verbosity',
            value: 'medium',
            busy: false,
            cycle: cycleVerbosity,
          },
          thinking: { enabled: false, busy: false, toggle: toggleThinking },
          fast: { enabled: true, busy: false, toggle: toggleFast },
        }}
      />,
    );

    // Desktop settings stay secondary without shrinking mobile labels or touch reach.
    for (const button of screen.getAllByRole('button')) {
      expect(button).toHaveClass(
        'text-meta',
        'tracking-normal',
        'min-h-8',
        'mouse:min-h-7',
        'mouse:tracking-normal',
        'mouse:[&>svg]:size-2.5',
      );
      expect(button).not.toHaveClass('mouse:tracking-[0.08em]');
    }
    const row = screen.getByRole('button', { name: 'Change provider and model' }).parentElement!;
    expect(row).toHaveClass('gap-1', 'pt-1', 'mouse:gap-1.5', 'mouse:pt-0.5');
    for (const divider of row.querySelectorAll(':scope > [aria-hidden="true"]')) {
      expect(divider).toHaveClass('h-2.5', 'mouse:h-2');
    }

    fireEvent.click(screen.getByRole('button', { name: 'Change provider and model' }));
    fireEvent.click(screen.getByRole('button', { name: 'Reasoning — high, choose a level' }));
    fireEvent.click(
      within(screen.getByRole('dialog', { name: 'Reasoning' })).getByRole('button', {
        name: 'low',
      }),
    );
    fireEvent.click(
      screen.getByRole('button', {
        name: 'Verbosity — medium, tap for the next level',
      }),
    );
    const thinking = screen.getByRole('button', { name: 'Thinking summary — off' });
    expect(thinking).toHaveTextContent('omitted');
    fireEvent.click(thinking);
    fireEvent.click(screen.getByRole('button', { name: 'Fast mode — on' }));

    expect(choose).toHaveBeenCalledOnce();
    expect(chooseReasoning).toHaveBeenCalledWith('low');
    expect(cycleVerbosity).toHaveBeenCalledOnce();
    expect(toggleThinking).toHaveBeenCalledOnce();
    expect(toggleFast).toHaveBeenCalledOnce();
  });

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
