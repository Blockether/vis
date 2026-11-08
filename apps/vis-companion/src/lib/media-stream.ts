import { Capacitor } from '@capacitor/core';

/** The path where the Android app reads a gateway stream for the page (`MediaProxyClient`). */
export const MEDIA_PROXY_PATH = '/_vis/media';

/**
 * WHERE A MEDIA ELEMENT LOADS A GATEWAY STREAM (#449).
 *
 * The Android web view serves the page from `https://localhost`. It blocks an `http:`
 * audio or video element under that page as mixed content, but `fetch` to the same
 * gateway works. So on Android an `http:` stream plays through the app origin: the
 * native `MediaProxyClient` (stamped by `scripts/android-prepare.mjs`) reads it from the
 * gateway with the same Range header and gives the bytes to the player. Every other case
 * (iOS, the web build, an `https:` gateway) loads the stream directly.
 */
export function mediaStreamSrc(
  absolute: string,
  platform: string = Capacitor.getPlatform(),
  page: Pick<Location, 'origin' | 'protocol'> = window.location,
): string {
  if (platform !== 'android' || page.protocol !== 'https:' || !/^http:/i.test(absolute))
    return absolute;
  return `${page.origin}${MEDIA_PROXY_PATH}?src=${encodeURIComponent(absolute)}`;
}
