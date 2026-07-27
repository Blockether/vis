// Catch `vis://gateway?...` links opened from a QR reader, a paste into the
// system, or a Universal Link. On the web this is a no-op (the plugin throws),
// so callers can rely on it existing everywhere.

import { App } from '@capacitor/app';

export async function onPairingLink(
  handler: (url: string) => void,
): Promise<() => void> {
  try {
    const seen = new Set<string>();
    const once = (url: string) => {
      if (!url || seen.has(url)) return;
      seen.add(url);
      handler(url);
    };
    const sub = await App.addListener('appUrlOpen', (event) => {
      once(event.url);
    });
    // Cold start: the link that LAUNCHED the app has already been delivered by
    // the time React mounts and this listener attaches, so `appUrlOpen` never
    // fires for it. Without this, tapping a pairing link while the app is not
    // running opens a blank Connect screen — the exact case a fresh install hits.
    try {
      const launch = await App.getLaunchUrl();
      if (launch?.url) once(launch.url);
    } catch {
      /* web / plugin unavailable */
    }
    return () => {
      void sub.remove();
    };
  } catch {
    return () => {};
  }
}
