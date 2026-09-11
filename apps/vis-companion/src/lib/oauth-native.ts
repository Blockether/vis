/** Native loopback reception is a transport, not a provider adapter.
 * The native host binds only this device's loopback, opens a system browser and returns
 * one validated callback. No credentials, token exchange, relay or callback persistence.
 */
import { Capacitor, registerPlugin } from '@capacitor/core';
import type { SignInFlow } from './types';

export interface NativeOAuth {
  authorize(options: {
    flowId: string;
    authorizationUrl: string;
    redirectUri: string;
    state: string;
    expiresAt: number;
  }): Promise<{ url: string }>;
  reopen(options: { flowId: string }): Promise<void>;
  cancel(options: { flowId: string }): Promise<void>;
}
let plugin: NativeOAuth | undefined;
export const nativeOAuth = () => (plugin ??= registerPlugin<NativeOAuth>('OAuthLoopback'));
export function hasNativeLoopback(flow: SignInFlow): boolean {
  if (flow.kind !== 'pkce' || !flow.redirect_uri || !Capacitor.isNativePlatform()) return false;
  try {
    const uri = new URL(flow.redirect_uri);
    return (
      uri.protocol === 'http:' &&
      ['localhost', '127.0.0.1', '[::1]'].includes(uri.hostname) &&
      Number(uri.port) >= 1024 &&
      !uri.username &&
      !uri.password &&
      !uri.search &&
      !uri.hash &&
      Capacitor.isPluginAvailable('OAuthLoopback')
    );
  } catch {
    return false;
  }
}
