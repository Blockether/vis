import { afterEach, expect, it, vi } from 'vitest';
import { onPairingLink } from './deeplink';

const native = vi.hoisted(() => ({
  handler: (_: { url: string }) => {},
  launch: '',
  remove: vi.fn(),
}));
vi.mock('@capacitor/app', () => ({
  App: {
    addListener: vi.fn(async (_event, handler) => {
      native.handler = handler;
      return { remove: native.remove };
    }),
    getLaunchUrl: vi.fn(async () => ({ url: native.launch })),
  },
}));
afterEach(() => {
  native.launch = '';
  vi.clearAllMocks();
});

it('never routes cold or warm OAuth returns into pairing/share handling', async () => {
  const callback = 'com.blockether.viscompanion://oauth/callback?code=test-code&state=test-state';
  native.launch = callback;
  const pairing = vi.fn();
  const stop = await onPairingLink(pairing);
  native.handler({ url: callback });
  native.handler({
    url: callback.replace('com.blockether.viscompanion:', 'COM.BLOCKETHER.VISCOMPANION:'),
  });
  expect(pairing).not.toHaveBeenCalled();
  const link = 'vis://gateway?url=https://gateway.example.com';
  native.handler({ url: link });
  native.handler({ url: link });
  expect(pairing).toHaveBeenCalledExactlyOnceWith(link);
  stop();
  expect(native.remove).toHaveBeenCalledOnce();
});
