// @vitest-environment jsdom
// The picker opens in `Modal`, which PORTALS into the document — there is no document
// in the `node` environment. Each case renders the dialog and reads where it landed.
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { STORY_PROVIDERS, STORY_ROUTER_CLIENT, STORY_SESSION } from '../dev/story-data';
import type { GatewayClient } from '../lib/gateway';
import type { RouterProvider } from '../lib/types';
import { ProviderRouterDialog } from './RouterScreen';

const noop = () => {};

// THE PICK BELONGS TO ONE SESSION, SO ITS DIALOG STANDS IN THAT SESSION'S PANE.
// Regression, user report: on a wide window the picker was centred on the whole
// application, straddling the session list beside the transcript rather than opening
// in the session whose model it pins.
describe('the model picker stands in the session it pins', () => {
  const openPicker = () => {
    // One picker in the document at a time: a second mount would leave the first
    // dialog in the body these cases read.
    cleanup();
    render(
      <ProviderRouterDialog client={STORY_ROUTER_CLIENT} sid={STORY_SESSION.id} onClose={noop} />,
    );
    return screen.getByRole('dialog', { name: 'Model' });
  };

  it('mounts in the session pane, leaving the desk beside it uncovered', () => {
    const shell = document.createElement('div');
    shell.setAttribute('data-viewport-shell', '');
    const pane = document.createElement('div');
    pane.setAttribute('data-session-surface', '');
    document.body.append(shell, pane);
    try {
      const dialog = openPicker();
      expect(pane.contains(dialog)).toBe(true);
      expect(shell.contains(dialog)).toBe(false);
    } finally {
      shell.remove();
      pane.remove();
    }
  });

  // No session on screen is no pane to stand in, and the pick may not disappear with
  // it: the app shell keeps the keyboard behaviour every layer mounts for.
  it('falls back to the app shell when no session pane is on screen', () => {
    const shell = document.createElement('div');
    shell.setAttribute('data-viewport-shell', '');
    document.body.append(shell);
    try {
      const dialog = openPicker();
      expect(shell.contains(dialog)).toBe(true);
    } finally {
      shell.remove();
    }
  });
});

// Regression, issue #406: every signed-out provider offered the same OAuth wording, and
// its button opened machine settings without the provider it was pressed for.
describe('a signed-out provider opens its own row in machine settings', () => {
  const signedOut = (id: string, auth_kind: RouterProvider['auth_kind']): RouterProvider => ({
    ...STORY_PROVIDERS[0],
    id,
    label: id.toUpperCase(),
    is_default: false,
    auth_kind,
    status: { is_authenticated: false, auth_state: 'unverified' },
  });
  const fleet = [
    signedOut('token-plan', 'api-key'),
    signedOut('oauth-plan', 'oauth'),
    signedOut('minted', 'command'),
  ];
  const client = {
    ...STORY_ROUTER_CLIENT,
    cachedRouter: () => fleet,
    router: async () => fleet,
  } as unknown as GatewayClient;

  it.each([
    ['token-plan', 'Enter API key — machine settings'],
    ['oauth-plan', 'Sign in — machine settings'],
    ['minted', 'Open in machine settings'],
  ])('names the step of %s and passes its id', (id, label) => {
    cleanup();
    const onClose = vi.fn();
    const onManageProviders = vi.fn();
    render(
      <ProviderRouterDialog
        client={client}
        sid={STORY_SESSION.id}
        onClose={onClose}
        onManageProviders={onManageProviders}
      />,
    );
    fireEvent.click(screen.getByRole('button', { name: new RegExp(id.toUpperCase()) }));
    fireEvent.click(screen.getByRole('button', { name: label }));
    expect(onClose).toHaveBeenCalledOnce();
    expect(onManageProviders.mock.calls).toEqual([[id]]);
  });

  it('opens the whole fleet from the title band, without a provider', () => {
    cleanup();
    const onManageProviders = vi.fn();
    render(
      <ProviderRouterDialog
        client={client}
        sid={STORY_SESSION.id}
        onClose={noop}
        onManageProviders={onManageProviders}
      />,
    );
    fireEvent.click(screen.getByRole('button', { name: 'Open provider settings' }));
    expect(onManageProviders.mock.calls).toEqual([[undefined]]);
  });
});
