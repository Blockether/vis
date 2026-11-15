/**
 * The brand that this page shows: the brand of the build, with the brand of the
 * gateway on top of it, key by key.
 *
 * The gateway brand is saved in localStorage, so the first frame of the next
 * visit already shows it (`index.html` reads the same key for the theme).
 */
import { useSyncExternalStore } from 'react';
import { brandOf, type Brand } from './brand-record';
import { PROTOCOL_HEADERS } from './compat';

export type { Brand, BrandLink, BrandTheme } from './brand-record';

const BRAND_KEY = 'vis.webBrand';
export const VIS_NAME = 'Vis';
export const VIS_LOGO = '/vis-logo.png';

/** The brand of the build: the Vite config writes the `VIS_WEB_BRAND` file into the page. */
function buildBrand(): Brand {
  return brandOf(typeof window === 'undefined' ? {} : (window.__VIS_BRAND__ ?? {}));
}

function savedBrand(): Brand {
  try {
    return brandOf(JSON.parse(globalThis.localStorage?.getItem(BRAND_KEY) ?? '{}'));
  } catch {
    return {};
  }
}

let brand: Brand = { ...buildBrand(), ...savedBrand() };
const listeners = new Set<() => void>();

export function currentBrand(): Brand {
  return brand;
}

function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}

export function useBrand(): Brand {
  return useSyncExternalStore(subscribe, currentBrand);
}

/** Give the page title and the tab icon the name and the logo that changed. */
function paintDocument(next: Brand, shown: Brand): void {
  if (typeof document === 'undefined') return;
  if (next.name !== shown.name) document.title = next.name ?? VIS_NAME;
  if (next.logo === shown.logo) return;
  const icon = document.querySelector<HTMLLinkElement>('link[rel="icon"]');
  if (!icon) return;
  // The logo of a brand is not always a PNG.
  icon.removeAttribute('type');
  icon.href = next.logo ?? VIS_LOGO;
}

/**
 * Take the answer of `GET /v1/web/brand` as the brand of the gateway and save it.
 * Answers whether the shown brand changed.
 */
export function setGatewayBrand(value: unknown): boolean {
  const gateway = brandOf(value);
  try {
    if (Object.keys(gateway).length > 0) globalThis.localStorage?.setItem(BRAND_KEY, JSON.stringify(gateway));
    else globalThis.localStorage?.removeItem(BRAND_KEY);
  } catch {
    /* Private mode or a full store: the next visit asks the gateway again. */
  }
  const next = { ...buildBrand(), ...gateway };
  if (JSON.stringify(next) === JSON.stringify(brand)) return false;
  paintDocument(next, brand);
  brand = next;
  for (const listener of listeners) listener();
  return true;
}

/**
 * Show the saved brand in the document, then ask the gateway that served this page
 * for its brand. Only the `web` bundle has such a gateway. Answers whether the
 * shown brand changed, so the caller paints the theme again.
 */
export async function loadBrand(mode: string = import.meta.env.MODE): Promise<boolean> {
  // The page already shows the brand of the build.
  paintDocument(brand, buildBrand());
  if (mode !== 'web') return false;
  try {
    const response = await fetch('/v1/web/brand', { headers: PROTOCOL_HEADERS });
    // A failed answer is not "no brand": the saved brand stays.
    return response.ok ? setGatewayBrand(await response.json()) : false;
  } catch {
    return false;
  }
}
