// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import { GatewayClient } from '../lib/gateway';
import type { GatewayConn, ProviderResetOutcome, RouterProvider } from '../lib/types';
import { ProviderRows, useProviderAuth } from './ProviderAuth';
import { ProviderLimitReset } from './ProviderLimitReset';

afterEach(() => { cleanup(); vi.unstubAllGlobals(); localStorage.clear(); });
const credits = { status: 'ok' as const, account_id: 'account-1', available_count: 2 };

it('requires explicit confirmation, focuses Cancel, and never submits on Escape', () => {
  const consume = vi.fn();
  render(<ProviderLimitReset credits={credits} onConsume={consume} />);
  fireEvent.click(screen.getByRole('button', { name: 'Reset limits…' }));
  expect(consume).not.toHaveBeenCalled();
  expect(document.activeElement).toBe(screen.getByRole('button', { name: 'Cancel' }));
  expect(screen.getByText(/Account: account-1/)).toBeTruthy();
  expect(screen.getByText(/all devices and sessions/)).toBeTruthy();
  fireEvent.keyDown(document.activeElement!, { key: 'Escape' });
  expect(screen.queryByRole('button', { name: 'Use 1 reset' })).toBeNull();
  expect(consume).not.toHaveBeenCalled();
});

it('distinguishes zero, missing, unsupported and loading without offering a new spend', () => {
  const base = { onConsume: vi.fn() };
  const view = render(<ProviderLimitReset {...base} credits={{ ...credits, available_count: 0 }} />);
  expect(screen.getByText('0 resets available')).toBeTruthy();
  expect(screen.getByRole('button', { name: 'Reset limits…' })).toBeDisabled();
  view.rerender(<ProviderLimitReset {...base} />);
  expect(screen.getByText('The gateway did not report reset availability. Check that it is up to date.')).toBeTruthy();
  view.rerender(<ProviderLimitReset {...base} credits={{ status: 'error' }} />);
  expect(screen.getByText('Available resets could not be checked.')).toBeTruthy();
  expect(screen.queryByRole('button', { name: 'Reset limits…' })).toBeNull();
  view.rerender(<ProviderLimitReset {...base} credits={{ status: 'unsupported' }} />);
  expect(screen.getByText(/not available for this account/)).toBeTruthy();
  view.rerender(<ProviderLimitReset {...base} credits={credits} isChecking />);
  expect(screen.getByText('Checking available resets…')).toBeTruthy();
  expect(screen.getByRole('button', { name: 'Reset limits…' })).toBeDisabled();
});

it.each<ProviderResetOutcome>(['reset', 'nothing_to_reset', 'no_credit', 'already_redeemed'])('reports %s and prevents double submission while pending', async outcome => {
  let finish!: (result: ProviderResetOutcome) => void;
  const consume = vi.fn(() => new Promise<ProviderResetOutcome>(resolve => { finish = resolve; }));
  render(<ProviderLimitReset credits={credits} onConsume={consume} />);
  fireEvent.click(screen.getByRole('button', { name: 'Reset limits…' }));
  fireEvent.click(screen.getByRole('button', { name: 'Use 1 reset' }));
  fireEvent.click(screen.getByRole('button', { name: 'Checking result…' }));
  expect(consume).toHaveBeenCalledExactlyOnceWith('account-1');
  expect(screen.getByRole('button', { name: 'Cancel' })).toBeDisabled();
  await act(async () => finish(outcome));
  expect(screen.queryByRole('button', { name: 'Use 1 reset' })).toBeNull();
  expect(screen.getAllByRole('status')).toHaveLength(2);
});

it('refuses an account switch after confirmation opened', () => {
  const consume = vi.fn();
  const props = { onConsume: consume };
  const view = render(<ProviderLimitReset {...props} credits={credits} />);
  fireEvent.click(screen.getByRole('button', { name: 'Reset limits…' }));
  view.rerender(<ProviderLimitReset {...props} credits={{ ...credits, account_id: 'account-2' }} />);
  expect(screen.getByRole('button', { name: 'Use 1 reset' })).toBeDisabled();
  expect(screen.getByRole('alert').textContent).toContain('account changed');
  expect(consume).not.toHaveBeenCalled();
});

function ConnectedRows({ client }: { client: GatewayClient }) {
  return <ProviderRows auth={useProviderAuth(client)} />;
}

// Regression: a gateway started before reset support returned limits but no
// reset_credits. Refresh belongs to the provider's swipe actions, not the reset panel.
it('refreshes missing reset data from the row action without consuming a reset', async () => {
  const client = new GatewayClient({ id: 'reset-refresh', url: 'https://gateway.example.com', token: 'test' } as GatewayConn);
  const provider: RouterProvider = {
    id: 'openai-codex', label: 'OpenAI Codex', models: ['gpt-5'], is_default: true, default_model: 'gpt-5', is_fallback: false, fallback_model: null,
    status: { is_authenticated: true, auth_state: 'verified' }, limits: { status: 'ok', dynamic: { limits: [] } },
  };
  vi.spyOn(client, 'router').mockResolvedValue([provider]);
  vi.spyOn(client, 'providerStatus').mockResolvedValue(provider.status!);
  const read = vi.spyOn(client, 'providerLimits').mockResolvedValue(provider.limits!);
  const consume = vi.spyOn(client, 'consumeProviderResetCredit');
  render(<ConnectedRows client={client} />);
  fireEvent.click(await screen.findByRole('button', { name: /OpenAI Codex/i, expanded: false }));
  await screen.findByText('The gateway did not report reset availability. Check that it is up to date.');
  expect(screen.queryByRole('button', { name: 'Refresh limits' })).toBeNull();
  read.mockResolvedValue({ status: 'ok', dynamic: { reset_credits: { ...credits, available_count: 3 } } });
  fireEvent.click(screen.getByRole('button', { name: 'Refresh limits for OpenAI Codex' }));
  await screen.findByText('3 resets available');
  expect(read).toHaveBeenCalledTimes(2);
  expect(consume).not.toHaveBeenCalled();
  expect(screen.getByRole('button', { name: /OpenAI Codex/i, expanded: true })).toBeTruthy();
});

it('crosses the real rows, hook and HTTP client; refreshes quotas and retries a lost response after reopening', async () => {
  let count = 1;
  let failRefresh = false;
  const calls: { path: string; body?: { account_id: string; idempotency_key: string } }[] = [];
  const provider: RouterProvider = {
    id: 'openai-codex', label: 'OpenAI Codex', models: ['gpt-5'], is_default: true, default_model: 'gpt-5', is_fallback: false, fallback_model: null,
    status: { is_authenticated: true, auth_state: 'verified' },
  };
  const limits = () => ({ status: 'ok', dynamic: { reset_credits: { ...credits, available_count: count }, limits: [{ label: 'Weekly', used_percent: 100 }] } });
  vi.stubGlobal('fetch', vi.fn(async (url, init) => {
    const path = new URL(String(url)).pathname;
    const body = init?.body ? JSON.parse(String(init.body)) : undefined;
    calls.push({ path, body });
    if (path.endsWith('/consume')) {
      count = 0;
      return new Response(JSON.stringify(calls.filter(call => call.body).length === 1 ? { error: 'reset-unconfirmed' } : { outcome: 'already_redeemed' }), { status: calls.filter(call => call.body).length === 1 ? 502 : 200 });
    }
    if (path === '/v1/router') return new Response(JSON.stringify({ providers: [{ ...provider, limits: limits() }] }));
    if (path.endsWith('/status')) return new Response(JSON.stringify({ status: provider.status }));
    if (path.endsWith('/limits')) return new Response(JSON.stringify({ report: limits() }), { status: failRefresh ? 503 : 200 });
    throw new Error(`Unexpected endpoint ${path}`);
  }));
  const client = new GatewayClient({ id: 'ui-reset', url: 'https://gateway.example.com', token: 'test' } as GatewayConn);
  const first = render(<ConnectedRows client={client} />);
  fireEvent.click(await screen.findByRole('button', { name: /OpenAI Codex/i, expanded: false }));
  await waitFor(() => expect(screen.getByRole('button', { name: 'Reset limits…' })).toBeEnabled());
  fireEvent.click(screen.getByRole('button', { name: 'Reset limits…' }));
  fireEvent.click(screen.getByRole('button', { name: 'Use 1 reset' }));
  await screen.findByText(/Reset could not be confirmed/);
  expect(screen.getByText('0 resets available')).toBeTruthy();
  first.unmount();
  render(<ConnectedRows client={client} />);
  fireEvent.click(await screen.findByRole('button', { name: /OpenAI Codex/i, expanded: false }));
  await waitFor(() => expect(screen.getByRole('button', { name: 'Check reset result…' })).toBeEnabled());
  fireEvent.click(screen.getByRole('button', { name: 'Check reset result…' }));
  failRefresh = true;
  fireEvent.click(screen.getByRole('button', { name: 'Retry same request' }));
  await screen.findByText(/already processed/);
  expect(screen.getByText('Available resets could not be checked.')).toBeTruthy();
  const posts = calls.filter(call => call.body);
  expect(posts).toHaveLength(2);
  expect(posts[0].body).toEqual(posts[1].body);
  expect(calls.filter(call => call.path.endsWith('/limits')).length).toBeGreaterThanOrEqual(4);
  expect(client.hasPendingProviderReset('openai-codex', 'account-1')).toBe(false);
});

// Run against provider-reset-test/with-codex-backend in a separate JVM. Only
// the catalog row is a UI fixture; status, quota and mutation use real HTTP.
it.runIf(!!process.env.VIS_CODEX_RESET_E2E_URL)('end to end through the real gateway and simulated Codex', async () => {
  const client = new GatewayClient({ id: 'reset-e2e', url: process.env.VIS_CODEX_RESET_E2E_URL!, token: 'test' } as GatewayConn);
  vi.spyOn(client, 'router').mockImplementation(async () => [{
    id: 'openai-codex', label: 'OpenAI Codex', models: ['gpt-5'], is_default: true,
    default_model: 'gpt-5', is_fallback: false, fallback_model: null,
    status: await client.providerStatus('openai-codex'), limits: await client.providerLimits('openai-codex'),
  }]);
  const view = render(<ConnectedRows client={client} />);
  async function openRow() {
    fireEvent.click(await screen.findByRole('button', { name: /OpenAI Codex/i, expanded: false }));
  }
  async function submit(trigger: string, confirm: string) {
    await waitFor(() => expect(screen.getByRole('button', { name: trigger })).toBeEnabled());
    fireEvent.click(screen.getByRole('button', { name: trigger }));
    expect(screen.getByText('Account: test-account')).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: confirm }));
  }
  await openRow();
  await screen.findByText('2 resets available');
  await submit('Reset limits…', 'Use 1 reset');
  await screen.findByText(/Reset could not be confirmed/);
  expect(screen.getByText('1 reset available')).toBeTruthy();
  view.unmount();
  render(<ConnectedRows client={client} />);
  await openRow();
  await submit('Check reset result…', 'Retry same request');
  await screen.findByText(/already processed/);
  expect(screen.getByText('1 reset available')).toBeTruthy();
  await submit('Reset limits…', 'Use 1 reset');
  await screen.findByText('Limits reset. Your task has not been resent.');
  expect(screen.getByText('0 resets available')).toBeTruthy();
  expect(screen.getByRole('button', { name: 'Reset limits…' })).toBeDisabled();
});
