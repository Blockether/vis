// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { renderToStaticMarkup } from 'react-dom/server';
import { afterEach, describe, expect, it } from 'vitest';
import type { AuthFlow, ProviderPreset, RouterProvider } from '../lib/types';
import {
  AddProviderButton,
  isProviderAuthed,
  ProviderNotice,
  ProviderRows,
  providerStatusDot,
  providerStatusLine,
  unscopedMessage,
  type ProviderAuth,
} from './ProviderAuth';

afterEach(cleanup);

const preset = (id: string): ProviderPreset => ({
  id,
  label: id.toUpperCase(),
  auth_kind: 'oauth',
  is_local: false,
  models: [],
});

const provider = (id: string): RouterProvider => ({
  id,
  label: id.toUpperCase(),
  models: ['m1'],
  is_default: false,
  default_model: null,
  is_fallback: false,
  fallback_model: null,
});

/**
 * Only the fields the collapsed panel reads. A static render runs no effect and
 * no handler, which is exactly the question here: what does the FIRST paint of
 * this panel put on screen for a machine with nothing left to add.
 */
const auth = (presets: ProviderPreset[] | null): ProviderAuth =>
  ({
    presets,
    pending: null,
    loadPresets: async () => {},
    addProvider: async () => {},
  }) as unknown as ProviderAuth;

/** The fields a notice reads: the two banners and the flow, nothing else. */
const state = (fields: Partial<ProviderAuth>): ProviderAuth =>
  ({
    err: null,
    note: null,
    flow: null,
    pending: null,
    apiKey: '',
    redirectUrl: '',
    setApiKey: () => {},
    setRedirectUrl: () => {},
    finishApiKey: async () => {},
    finishPkce: async () => {},
    cancelFlow: async () => {},
    ...fields,
  }) as unknown as ProviderAuth;

const apiKeyFlow = (providerId: string): AuthFlow => ({
  flow_id: 'f1',
  provider_id: providerId,
  kind: 'api-key',
  instructions: ['Z.ai (Coding Plan) requires a static API key.'],
});

describe('AddProviderButton', () => {
  it('offers nothing when every provider this machine knows is already configured', () => {
    expect(renderToStaticMarkup(<AddProviderButton auth={auth([])} />)).toBe('');
  });

  it('stays silent until the gateway has said what is addable', () => {
    expect(renderToStaticMarkup(<AddProviderButton auth={auth(null)} />)).toBe('');
  });

  it('is one band verb, and the picker it opens is a sheet rather than a standing panel', () => {
    const html = renderToStaticMarkup(<AddProviderButton auth={auth([preset('ollama')])} />);
    expect(html).toContain('Add a provider');
    // The list of presets belongs to the sheet, so nothing of it paints until
    // the band verb is pressed.
    expect(html).not.toContain('OLLAMA');
  });
});

// Reported: the Providers screen painted "Z.ai (Coding Plan): signed out." and
// the whole "Finish sign-in" API-key form ABOVE the provider list, so a status
// and a key prompt belonging to one provider floated outside every provider.
describe('ProviderNotice', () => {
  it('paints the running sign-in inside the provider the flow belongs to', () => {
    const html = renderToStaticMarkup(
      <ProviderNotice
        auth={state({ flow: apiKeyFlow('zai-coding-plan') })}
        provider={provider('zai-coding-plan')}
      />,
    );
    expect(html).toContain('Finish sign-in');
    expect(html).toContain('Paste the provider API key');
  });

  it('says nothing in the other providers while one of them is signing in', () => {
    const html = renderToStaticMarkup(
      <ProviderNotice
        auth={state({ flow: apiKeyFlow('zai-coding-plan') })}
        provider={provider('anthropic')}
      />,
    );
    expect(html).toBe('');
  });

  it("keeps a provider's own verdict in that provider and out of the others", () => {
    const signedOut = state({
      note: { text: 'Z.AI: signed out.', providerId: 'zai-coding-plan' },
    });
    expect(
      renderToStaticMarkup(
        <ProviderNotice auth={signedOut} provider={provider('zai-coding-plan')} />,
      ),
    ).toContain('signed out.');
    expect(
      renderToStaticMarkup(<ProviderNotice auth={signedOut} provider={provider('anthropic')} />),
    ).toBe('');
  });

  it('paints a failure in the provider that failed', () => {
    const html = renderToStaticMarkup(
      <ProviderNotice
        auth={state({ err: { text: 'Authorization failed.', providerId: 'anthropic' } })}
        provider={provider('anthropic')}
      />,
    );
    expect(html).toContain('Authorization failed.');
  });
});

describe('unscopedMessage', () => {
  const rows = [provider('anthropic'), provider('zai-coding-plan')];

  it('keeps a fleet-wide message at the panel level', () => {
    const message = { text: 'Gateway unreachable.', providerId: null };
    expect(unscopedMessage(message, rows)).toBe(message);
  });

  it('hands a provider message to that provider instead', () => {
    expect(unscopedMessage({ text: 'Signed out.', providerId: 'anthropic' }, rows)).toBeNull();
  });

  it('never swallows a message whose provider is not on screen', () => {
    const message = { text: 'Removed OLLAMA.', providerId: 'ollama' };
    expect(unscopedMessage(message, rows)).toBe(message);
    expect(unscopedMessage(message, null)).toBe(message);
  });
});

describe('provider authentication verdict', () => {
  it('paints a live quota rejection as an authentication error instead of signed in', () => {
    const rejected = provider('zai-coding-plan');
    rejected.status = { is_authenticated: true, source: 'config' };
    rejected.limits = {
      status: 'unauthenticated',
      dynamic: {
        limits: [],
        note: 'Z.ai (Coding Plan) rejected the current API key.',
      },
    };

    expect(isProviderAuthed(rejected)).toBe(false);
    expect(providerStatusDot(rejected)).toEqual({
      glyph: '●',
      tone: 'text-err',
      label: 'Authentication error',
    });
    expect(providerStatusLine(rejected)).toBe(
      'Z.ai (Coding Plan) rejected the current API key.',
    );
  });
});

// A provider stays compact until its chevron disclosure is opened.
describe('ProviderRows', () => {
  const signedIn = (fields: Partial<RouterProvider> = {}): RouterProvider => ({
    ...provider('github-copilot'),
    status: { is_authenticated: true, label: 'signed-in session' },
    ...fields,
  });

  it('opens a signed-in provider and shows every reported limit without source detail', () => {
    render(
      <ProviderRows
        auth={state({
          providers: [
            signedIn({
              limits: {
                dynamic: {
                  limits: [
                    { label: 'Chat', limit: 100, remaining: 62 },
                    { label: 'Completions', limit: 100, remaining: 16 },
                  ],
                  note: 'Live quota source detail.',
                },
              },
            }),
          ],
          recheck: async () => {},
        })}
      />,
    );

    const row = screen.getByText('GITHUB-COPILOT').closest('button');
    expect(row).not.toBeNull();
    if (!row) throw new Error('provider row missing');
    expect(row.getAttribute('aria-expanded')).toBe('false');
    expect(screen.queryByRole('region', { name: 'GITHUB-COPILOT limits' })).toBeNull();

    fireEvent.click(row);

    expect(row.getAttribute('aria-expanded')).toBe('true');
    const limits = screen.getByRole('region', { name: 'GITHUB-COPILOT limits' });
    expect(limits.textContent).toContain('Chat 62% left');
    expect(limits.textContent).toContain('Completions 16% left');
    expect(screen.queryByText('Live quota source detail.')).toBeNull();

    fireEvent.click(row);
    expect(row.getAttribute('aria-expanded')).toBe('false');
    expect(screen.queryByRole('region', { name: 'GITHUB-COPILOT limits' })).toBeNull();
  });

  it('presses into a live re-check for an account that is already signed in', () => {
    const asked: string[] = [];
    render(
      <ProviderRows
        auth={state({
          providers: [signedIn()],
          recheck: async (providerId: string) => {
            asked.push(providerId);
          },
        })}
      />,
    );
    fireEvent.click(screen.getByText('GITHUB-COPILOT'));
    expect(asked).toEqual(['github-copilot']);
  });

  it('keeps the disclosure quiet while its live re-check is in flight', () => {
    render(
      <ProviderRows
        auth={state({
          providers: [signedIn()],
          pending: 'status:github-copilot',
          recheck: async () => {},
        })}
      />,
    );

    const row = screen.getByText('GITHUB-COPILOT').closest('button');
    expect(row).not.toBeNull();
    if (!row) throw new Error('provider row missing');
    fireEvent.click(row);

    expect(screen.getByText('Signed in')).toBeTruthy();
    expect(screen.queryByText(/Checking/)).toBeNull();
    expect(screen.queryByText('No limits reported by this provider.')).toBeNull();
    expect(row.querySelector('[title="Signed in"]')?.className).not.toContain('animate-pulse');
  });

  it('presses into the sign-in of an account that has none', () => {
    const started: string[] = [];
    render(
      <ProviderRows
        auth={state({
          providers: [provider('anthropic')],
          signIn: async (row: RouterProvider) => {
            started.push(row.id);
          },
        })}
      />,
    );
    expect(screen.getByText('Sign in')).toBeTruthy();
    fireEvent.click(screen.getByText('ANTHROPIC'));
    expect(started).toEqual(['anthropic']);
  });

  it('hangs that account’s own models under the rank verb instead of guessing one', () => {
    render(
      <ProviderRows
        auth={state({ providers: [signedIn({ models: ['glm-5.3', 'glm-5.3-air'] })] })}
      />,
    );
    fireEvent.click(screen.getByRole('button', { name: 'Run every turn on GITHUB-COPILOT' }));
    expect(screen.getByText('glm-5.3-air')).toBeTruthy();
  });

  it('never offers the fallback to the provider that already runs every turn', () => {
    const html = renderToStaticMarkup(
      <ProviderRows
        auth={state({
          providers: [signedIn({ is_default: true, default_model: 'gpt-5' })],
        })}
      />,
    );
    expect(html).toContain('Run every turn on GITHUB-COPILOT');
    expect(html).not.toContain('Fall back to GITHUB-COPILOT');
  });

  it('keeps removal as the row’s last verb, and asks inside the row before it destroys', () => {
    render(<ProviderRows auth={state({ providers: [provider('anthropic')] })} />);
    fireEvent.click(
      screen.getByRole('button', {
        name: 'Sign out of ANTHROPIC and remove it from this machine',
      }),
    );
    expect(screen.getByRole('group', { name: 'Remove ANTHROPIC?' })).toBeTruthy();
    expect(screen.getByText('Yes, remove')).toBeTruthy();
  });
});
