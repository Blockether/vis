// @vitest-environment jsdom
import html from '../../index.html?raw';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { currentBrand, loadBrand, setGatewayBrand } from './brand';
import { brandFault } from './brand-record';
import { paintStoredTheme } from './theme';
import { THEMES } from './themes.generated';

const dark = THEMES.find((theme) => theme.mode === 'dark')!;
const light = THEMES.find((theme) => theme.mode === 'light')!;
const link = { icon: 'git', label: 'Source', href: 'https://example.test/code' };

beforeEach(() => {
  localStorage.clear();
  delete window.__VIS_BRAND__;
  setGatewayBrand({});
  delete document.documentElement.dataset.theme;
  document.documentElement.removeAttribute('style');
});
afterEach(() => vi.unstubAllGlobals());

describe('the white label of the web app', () => {
  it('puts the gateway brand on the build brand, key by key', () => {
    window.__VIS_BRAND__ = { name: 'Acme', logo: '/acme.svg' };

    expect(setGatewayBrand({ name: 'Acme Cloud', links: [link] })).toBe(true);

    expect(currentBrand()).toEqual({ name: 'Acme Cloud', logo: '/acme.svg', links: [link] });
    expect(document.title).toBe('Acme Cloud');
  });

  it('shows no part of a brand that breaks a rule', () => {
    const faults = [
      { name: 'Acme', href: 'javascript:alert(1)' },
      { name: 'Acme', logo: 'http://example.test/logo.png' },
      { name: 'Acme', links: [{ ...link, icon: 'no-such-icon' }] },
      { name: 'Acme', theme: { base: 'a-theme-this-app-does-not-ship' } },
      { name: 'Acme', theme: { base: dark.id, colors: { '--bg': 'url(https://example.test)' } } },
    ];
    for (const brand of faults) {
      expect(brandFault(brand)).toEqual(expect.any(String));
      setGatewayBrand(brand);
      expect(currentBrand()).toEqual({});
    }
  });

  it('keeps the saved brand when the gateway does not answer', async () => {
    setGatewayBrand({ name: 'Acme' });
    vi.stubGlobal('fetch', async () => new Response('down', { status: 502 }));

    expect(await loadBrand('web')).toBe(false);
    expect(currentBrand()).toEqual({ name: 'Acme' });

    vi.stubGlobal('fetch', async () => Response.json({}));
    expect(await loadBrand('web')).toBe(true);
    expect(currentBrand()).toEqual({});
    expect(document.title).toBe('Vis');
  });
});

describe('the theme of a white label', () => {
  const theme = { base: dark.id, label: 'Acme Night', colors: { '--bg': '#101010' } };

  it('is the only palette while the brand has it, and leaves no color behind', () => {
    localStorage.setItem('vis.themePref', light.id);
    const root = document.documentElement;

    setGatewayBrand({ theme });
    expect(paintStoredTheme()).toEqual({ ...dark, label: 'Acme Night' });
    expect(root.dataset.theme).toBe(dark.id);
    expect(root.style.getPropertyValue('--bg')).toBe('#101010');

    setGatewayBrand({});
    expect(paintStoredTheme()).toEqual(light);
    expect(root.style.getPropertyValue('--bg')).toBe('');
  });

  it('paints from the saved brand during parse, before the bundle loads', () => {
    const inline = /<script>([\s\S]*?)<\/script>/.exec(html)?.[1] ?? '';
    localStorage.setItem('vis.themePref', light.id);
    setGatewayBrand({ theme });
    const root = document.documentElement;
    delete root.dataset.theme;

    new Function('document', 'localStorage', inline)(document, localStorage);

    expect(root.dataset.theme).toBe(dark.id);
    expect(root.style.getPropertyValue('--bg')).toBe('#101010');
  });
});
