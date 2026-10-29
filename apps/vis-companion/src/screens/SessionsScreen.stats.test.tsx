// @vitest-environment jsdom
import { screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

// Regression, user report ("remove the top errors and top tools, we don't use them
// anymore"): the expanded session card printed TOP TOOLS and TOP ERRORS pairs under
// its number grid — two rankings nobody read, one of them the only warn-coloured ink
// on the card. A gateway that still sends `top_tools`/`top_errors` must not bring
// them back.
describe('the expanded session card', () => {
  const usage = {
    health: {
      last_request_tokens: 32_000,
      budget_tokens: 200_000,
      reminder_tokens: 150_000,
      model_input_limit: 272_000,
      call: 5,
      // Derived budget fields come from the gateway, not client-side arithmetic.
      budget_state: 'within-budget',
      budget_used_percent: 16,
      budget_used_ratio: 0.16,
      budget_remaining_tokens: 168_000,
      estimated_input_tokens: 32_000,
      estimate_difference_tokens: 0,
      estimate_difference_percent: 0,
      breakdown: [{ label: 'Main AGENTS.md', tokens: 1_200, path: '/work/AGENTS.md' }],
      roots: [
        {
          path: '/work/linked',
          guidance: { status: 'available', path: '/work/linked/AGENTS.md', tokens: 800 },
        },
      ],
    },
    turn_count: 2,
    iteration_count: 5,
    tool_call_count: 59,
    fold_count: 0,
    input_tokens: 85_000,
    output_tokens: 2_300,
    cache_read_share_percent: 77,
    reusable_prefix_coverage_percent: 98,
    prompt_cache_reusable_tokens: 81_000,
    prompt_cache_reused_tokens: 79_400,
    prompt_cache_sample_count: 4,
    prompt_cache_estimated_sample_count: 1,
    reusable_prefix_estimated: true,
    cost_usd: 0.21,
    duration_ms: 47_000,
    provider: 'anthropic-coding-plan',
    model: 'claude-opus-5',
    top_tools: [{ name: 'python_execution', count: 59 }],
    error_count: 3,
    top_errors: [{ name: 'python_execution', count: 3 }],
  };

  const open = async (body: Record<string, unknown> = usage) => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [listSession({ id: 's1', title: 'Chat session' })],
          routes: { '/v1/sessions/s1/usage': { usage: body } },
        },
      ],
    });
    restore = view.restore;
    await screen.findByText('Chat session');
    await userEvent.click(screen.getByRole('button', { name: 'Show details for Chat session' }));
    return view;
  };

  it('renders the measured request from the usage endpoint, not lifetime totals', async () => {
    const view = await open();
    expect(await screen.findByRole('meter', { name: 'Context budget' })).toBeVisible();
    expect(screen.getByText('16%')).toBeVisible();
    expect(screen.getByText('32k')).toBeVisible();
    expect(screen.getByText('85k')).toBeVisible();
    await userEvent.click(screen.getByRole('button', { name: /Context breakdown/ }));
    expect(screen.getByText('/work/AGENTS.md')).toBeVisible();
    await userEvent.click(screen.getByRole('button', { name: /Linked filesystems/ }));
    expect(view.requests.filter((r) => r.path.endsWith('/usage'))).toHaveLength(1);
  });

  it('splits the cost between the session and its subagents', async () => {
    await open({
      ...usage,
      total_cost_usd: 25.46,
      subagents: {
        agent_count: 100,
        active_count: 4,
        iteration_count: 1_659,
        input_tokens: 154_000_000,
        output_tokens: 768_000,
        cost_usd: 25.25,
      },
    });
    const subagents = await screen.findByRole('region', { name: 'Subagents' });
    expect(screen.getByText('Session cost')).toBeVisible();
    expect(screen.queryByText('Cost')).toBeNull();
    for (const text of ['100 · 4 active', '1.7k', '154M', '768k', '$25.25', '$25.46']) {
      expect(subagents).toHaveTextContent(text);
    }
  });

  it('shows no subagent block for a session without subagents', async () => {
    await open();
    expect(await screen.findByText('Cost')).toBeVisible();
    expect(screen.queryByRole('region', { name: 'Subagents' })).toBeNull();
  });

  it('puts both cache metrics in a separate explained row', async () => {
    await open();
    const cachedInput = await screen.findByText('Cached input');
    const reuseCoverage = screen.getByText('Reuse coverage');
    expect(
      screen.getByText(
        'Estimated share of reusable prior input recovered from cache · 4 of 5 calls',
      ),
    ).toBeVisible();
    expect(cachedInput.closest('dl')).toBe(reuseCoverage.closest('dl'));
    expect(cachedInput.closest('dl')).not.toBe(screen.getByText('Turns').closest('dl'));
    expect(screen.getByText('77%')).toBeVisible();
    expect(screen.getByText('≈98%')).toBeVisible();
  });

  // Regression, user report ("alignment is wrong"): the cache row put its label on the
  // left and pushed its percentage to the right edge of each half, so both values
  // floated away from the left-aligned label-over-value columns of the grid above it.
  it('stacks each cache value under its own label', async () => {
    await open();
    const label = await screen.findByText('Cached input');
    const value = screen.getByText('77%');
    expect(value.previousElementSibling).toBe(label);
    expect(value.parentElement).toBe(label.parentElement);
  });
});
