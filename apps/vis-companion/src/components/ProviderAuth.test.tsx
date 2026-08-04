import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import type { ProviderPreset } from '../lib/types';
import { AddProviderPanel, type ProviderAuth } from './ProviderAuth';

const preset = (id: string): ProviderPreset => ({
  id,
  label: id.toUpperCase(),
  auth_kind: 'oauth',
  is_local: false,
  models: [],
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
