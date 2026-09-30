// @vitest-environment jsdom
import { render } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import type { TranscriptTurn } from '../lib/types';
import { AssistantMessage } from './ChatContent';

const goalTurn: TranscriptTurn = {
  turn_id: 'goal-usage',
  request: '/goal Verify the usage footer',
  status: 'completed',
  content: [{ id: 'goal-answer', type: 'prose', markdown: 'Goal verified.' }],
  provider: 'openai',
  model: 'gpt-test',
  input_tokens: 100,
  output_tokens: 20,
  input_cache_read_tokens: 70,
  total_cost: 0.0123,
  duration_ms: 1_200,
};

describe('goal usage footer', () => {
  // Regression: /goal invokes the model loop, not a command-only response.
  it.each(['/goal Verify the usage footer', '/goal --resume', '  /goal Verify the usage footer'])(
    'shows provider, model, tokens and cost after %j completes',
    (request) => {
      const view = render(<AssistantMessage turn={{ ...goalTurn, request }} />);
      expect(view.container).toHaveTextContent('Goal verified.');
      const footer = view.container.querySelector('footer');
      expect(footer).toHaveTextContent('openai/gpt-test');
      expect(footer).toHaveTextContent('100→20 ↺ 70');
      expect(footer).toHaveTextContent('~$0.0123');
    },
  );

  it.each(['cancelled', 'error'])('keeps spent usage on a %s goal turn', (status) => {
    const view = render(<AssistantMessage turn={{ ...goalTurn, status }} />);
    expect(view.container.querySelector('footer')).toHaveTextContent('100→20 ↺ 70');
    expect(view.container.querySelector('footer')).toHaveTextContent('~$0.0123');
  });

  it('keeps the persisted summary and actual routing after restoring a goal turn', () => {
    const meta = 'openai/gpt-actual · 100→20 (cached 70) · ~$0.0123 · 1.2s';
    const view = render(
      <AssistantMessage
        turn={{ ...goalTurn, meta_summary: meta, llm_actual: { provider: 'openai', model: 'gpt-actual' } }}
      />,
    );
    expect(view.container.querySelector('footer')).toHaveTextContent(meta);
  });

  it.each([
    { input_tokens: 100, total_cost: 0 },
    { input_tokens: 0, total_cost: 0.0123 },
  ])('shows a footer when either tokens or cost records provider work: %j', (usage) => {
    const view = render(
      <AssistantMessage turn={{ ...goalTurn, output_tokens: 0, input_cache_read_tokens: 0, ...usage }} />,
    );
    expect(view.container.querySelector('footer')).toHaveTextContent('openai/gpt-test');
  });

  it.each(['/goal', '/goal --pause', '/goal --cancel', '/reload', '!printf ok'])(
    'omits model metadata for %j without provider usage',
    (request) => {
      const view = render(
        <AssistantMessage
          turn={{ ...goalTurn, request, input_tokens: 0, output_tokens: 0, total_cost: 0 }}
        />,
      );
      expect(view.container.querySelector('footer')).not.toBeInTheDocument();
    },
  );

  it.each(['running', 'streaming', 'queued', 'pending'])(
    'does not show a finished summary while a goal is %s',
    (status) => {
      const view = render(<AssistantMessage turn={{ ...goalTurn, status }} />);
      expect(view.container.querySelector('footer')).not.toBeInTheDocument();
    },
  );

  it('retains the ordinary model-turn footer', () => {
    const view = render(<AssistantMessage turn={{ ...goalTurn, request: 'Verify the usage footer' }} />);
    expect(view.container.querySelector('footer')).toHaveTextContent('100→20 ↺ 70');
  });
});
