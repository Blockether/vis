// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { AddMachine } from './Machines';

// Regression, user report ("on a desktop I cannot possibly be scanning a QR — I
// AM the desktop"): the pairing panel offered "Scan QR" everywhere, including the
// machine whose terminal prints the code and whose webcam faces the reader.

function pointing(kind: 'fine' | 'coarse') {
  vi.stubGlobal('matchMedia', (query: string) => ({
    matches: query.includes('pointer: fine') ? kind === 'fine' : false,
    media: query,
    onchange: null,
    addListener: () => undefined,
    removeListener: () => undefined,
    addEventListener: () => undefined,
    removeEventListener: () => undefined,
    dispatchEvent: () => false,
  }));
}

afterEach(() => {
  vi.unstubAllGlobals();
});

describe('the QR scanner is a verb for the device in your hand', () => {
  it('is not offered under a mouse', () => {
    pointing('fine');
    render(<AddMachine onAdd={vi.fn(async () => {})} />);
    expect(screen.queryByRole('button', { name: 'Scan QR' })).toBeNull();
    // The other way in stays: the link is what a desktop pastes.
    expect(screen.getByRole('button', { name: 'Pair' })).toBeTruthy();
  });

  it('is offered on a touch screen', () => {
    pointing('coarse');
    render(<AddMachine onAdd={vi.fn(async () => {})} />);
    expect(screen.getByRole('button', { name: 'Scan QR' })).toBeTruthy();
  });
});
