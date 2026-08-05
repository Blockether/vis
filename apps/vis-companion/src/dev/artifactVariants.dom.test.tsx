// @vitest-environment jsdom
import { act, type ReactElement } from 'react';
import { createRoot } from 'react-dom/client';
import { afterEach, describe, expect, it } from 'vitest';

import {
  ArtifactsDockVariant,
  ArtifactsSheetVariant,
} from './artifactVariants';

(
  globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }
).IS_REACT_ACT_ENVIRONMENT = true;

/**
 * The static markup proves the controls exist; only clicking them proves they
 * do the right thing. Both findings pinned here came out of driving the gallery
 * in a real browser, and neither is visible in a screenshot.
 */
let cleanup: (() => void) | null = null;

function mount(node: ReactElement) {
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  act(() => root.render(node));
  cleanup = () => {
    act(() => root.unmount());
    host.remove();
  };
  return host;
}

afterEach(() => {
  cleanup?.();
  cleanup = null;
});

const click = (element: Element | null) => {
  expect(element).not.toBeNull();
  act(() => (element as HTMLElement).click());
};

const chip = (host: HTMLElement) =>
  host.querySelector<HTMLButtonElement>('button[aria-controls]');
const tiles = (host: HTMLElement) =>
  host.querySelectorAll('button[aria-label^="Open "]');
const filter = (host: HTMLElement, label: string) =>
  host.querySelector<HTMLButtonElement>(`button[aria-label^="${label},"]`);

describe('the artifacts chip drives its surface', () => {
  it('closes and reopens the sheet it claims to control', () => {
    const host = mount(<ArtifactsSheetVariant state="default" />);
    expect(chip(host)?.getAttribute('aria-expanded')).toBe('true');
    expect(tiles(host).length).toBe(12);

    click(chip(host));
    expect(chip(host)?.getAttribute('aria-expanded')).toBe('false');
    expect(host.querySelector('#artifacts-surface')).toBeNull();
    expect(tiles(host).length).toBe(0);

    click(chip(host));
    expect(host.querySelector('#artifacts-surface')).not.toBeNull();
    expect(tiles(host).length).toBe(12);
  });

  it('reopens on everything, never on a filter set minutes ago', () => {
    // Found in the browser: filtering to Documents, closing the sheet and
    // reopening it later showed three tiles of twelve with nothing on screen
    // explaining where the other nine had gone.
    const host = mount(<ArtifactsSheetVariant state="default" />);
    click(filter(host, 'Documents'));
    expect(tiles(host).length).toBe(3);
    expect(filter(host, 'Documents')?.getAttribute('aria-pressed')).toBe(
      'true',
    );
    expect(filter(host, 'All')?.getAttribute('aria-pressed')).toBe('false');

    click(chip(host));
    click(chip(host));
    expect(tiles(host).length).toBe(12);
    expect(filter(host, 'All')?.getAttribute('aria-pressed')).toBe('true');
  });

  it('narrows to exactly the kind that was asked for', () => {
    const host = mount(<ArtifactsSheetVariant state="default" />);
    click(filter(host, 'Pictures'));
    const shown = [...tiles(host)].map(
      (tile) =>
        (tile.getAttribute('aria-label') ?? '')
          .replace(/^Open /, '')
          .split(',')[0],
    );
    expect(shown.length).toBeGreaterThan(0);
    expect(
      shown.every((name) =>
        /\.(png|jpg|jpeg|webp|gif|svg|mp4|mov)$/.test(name),
      ),
    ).toBe(true);
    expect(shown.some((name) => name.endsWith('.pdf'))).toBe(false);
  });

  it('refuses a filter with nothing behind it', () => {
    const host = mount(<ArtifactsSheetVariant state="docs" />);
    const pictures = filter(host, 'Pictures');
    expect(pictures?.disabled).toBe(true);
    const before = tiles(host).length;
    act(() => (pictures as HTMLButtonElement).click());
    expect(tiles(host).length).toBe(before);
  });

  it('pins and unpins the dock without covering the transcript', () => {
    const host = mount(<ArtifactsDockVariant state="default" />);
    // Both bands are in the tree; `sm:` alone decides which one is on screen.
    expect(host.querySelector('#artifacts-dock-strip')).not.toBeNull();
    expect(host.querySelector('#artifacts-dock-column')).not.toBeNull();
    const controls = chip(host)?.getAttribute('aria-controls') ?? '';
    expect(controls.split(' ').sort()).toEqual([
      'artifacts-dock-column',
      'artifacts-dock-strip',
    ]);

    click(chip(host));
    expect(host.querySelector('#artifacts-dock-strip')).toBeNull();
    expect(host.querySelector('#artifacts-dock-column')).toBeNull();
    expect(tiles(host).length).toBe(0);
  });

  it('gives a session that produced nothing no chip to press', () => {
    const host = mount(<ArtifactsSheetVariant state="empty" />);
    expect(chip(host)).toBeNull();
    expect(tiles(host).length).toBe(0);
  });
});
