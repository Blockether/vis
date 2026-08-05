import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import type { AuthFlow, ProviderPreset, RouterProvider } from '../lib/types';
import {
  AddProviderPanel,
  ProviderNotice,
  unscopedMessage,
  type ProviderAuth,
} from './ProviderAuth';

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

describe('AddProviderPanel', () => {
  it('offers nothing when every provider this machine knows is already configured', () => {
    expect(renderToStaticMarkup(<AddProviderPanel auth={auth([])} />)).toBe('');
  });

  it('stays silent until the gateway has said what is addable', () => {
    expect(renderToStaticMarkup(<AddProviderPanel auth={auth(null)} />)).toBe('');
  });

  it('offers the button when the machine can still add something', () => {
    const html = renderToStaticMarkup(<AddProviderPanel auth={auth([preset('ollama')])} />);
    expect(html).toContain('Add provider');
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
