// Catch `vis://gateway?...` links opened from a QR reader, a paste into the
// system, or a Universal Link. On the web this is a no-op (the plugin throws),
// so callers can rely on it existing everywhere.

import { App } from '@capacitor/app';

export async function onPairingLink(
  handler: (url: string) => void,
): Promise<() => void> {
  try {
    const sub = await App.addListener('appUrlOpen', (event) => {
      if (event.url) handler(event.url);
    });
    return () => {
      void sub.remove();
    };
  } catch {
    return () => {};
  }
}
