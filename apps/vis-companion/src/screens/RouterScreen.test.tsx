// @vitest-environment jsdom
// The picker opens in `Modal`, which PORTALS into the document — there is no document
// in the `node` environment. Each case renders the dialog and reads where it landed.
import { cleanup, render, screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';

import { STORY_ROUTER_CLIENT, STORY_SESSION } from '../dev/story-data';
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
      // Scrim and box alike: the pane is the bound, so nothing outside it is dimmed.
      expect(pane.firstElementChild).toHaveClass('absolute', 'inset-0');
      expect(pane.firstElementChild).not.toHaveClass('fixed');
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
      expect(shell.firstElementChild).toHaveClass('absolute', 'inset-0');
    } finally {
      shell.remove();
    }
  });
});
