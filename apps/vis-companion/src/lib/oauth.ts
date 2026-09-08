/** One client-side sign-in lifecycle for MCP and model providers.
 * The caller binds protocol operations to ONE paired gateway/flow; this module knows no
 * provider names or endpoints. Callback validation, serial polling, manual completion,
 * retries, expiry and disposal are identical. Tokens and PKCE remain on the gateway.
 * No relay, hosted fallback, callback persistence or unbound cold-start return.
 */
import type { AuthVerdict, SignInFlow } from './types';
import { hasNativeLoopback, nativeOAuth } from './oauth-native';
export const openAuthUrl = (url: string): void => { window.open(url, '_blank', 'noopener,noreferrer'); };

/** A browser on this device cannot reach loopback on a remote gateway. */
export function clientAuthFlow<T extends SignInFlow>(flow: T, gatewayBase: string): T {
  if (hasNativeLoopback(flow)) return { ...flow, callback_mode: 'loopback', instructions: [
    'Approve sign-in in the browser. Vis receives the callback on this device and finishes automatically.'] };
  if (flow.callback_mode !== 'loopback') return flow;
  try { if (['127.0.0.1', 'localhost', '[::1]'].includes(new URL(gatewayBase).hostname)) return flow; }
  catch { /* Unknown is not proof of a local gateway. */ }
  return { ...flow, callback_mode: 'manual', instructions: [...(flow.instructions ?? []),
    'For a gateway on another device, copy the final browser URL into Vis.'] };
}

export interface AuthTransport {
  complete(input: string): Promise<AuthVerdict>;
  poll(): Promise<AuthVerdict>;
  cancel(): Promise<unknown>;
}
export interface AuthWatch { stop(): void; open(): void; complete(input: string): Promise<void> }
function authorizationState(url: string | undefined, redirect: string | undefined): string | undefined {
  try {
    const authorization = new URL(url ?? '');
    const state = authorization.searchParams.get('state');
    if (!state || authorization.protocol !== 'https:' || authorization.username || authorization.password || authorization.hash
        || authorization.searchParams.getAll('state').length !== 1
        || authorization.searchParams.getAll('redirect_uri').length !== 1
        || authorization.searchParams.get('redirect_uri') !== redirect) return;
    return state;
  } catch { return; }
}
function verifiedReturn(input: string, state: string, redirect = APP_CALLBACK): string | undefined {
  if (input.length > 8192) return;
  try {
    const url = new URL(input);
    if (`${url.protocol}//${url.host}${url.pathname}` !== redirect || url.username || url.password || url.hash) return;
    const query = url.searchParams;
    if ([...query.keys()].some(key => query.getAll(key).length !== 1)
        || query.get('state') !== state || query.has('code') === query.has('error')) return;
    const key = query.has('code') ? 'code' : 'error';
    const value = query.get(key);
    if (!value?.trim()) return;
    return `${redirect}?${new URLSearchParams({ state, [key]: value })}`;
  } catch { return; }
}

export function watchAuth(flow: SignInFlow, transport: AuthTransport,
  onVerdict: (verdict: AuthVerdict) => void, onConnection?: (message: string | null) => void): AuthWatch {
  const deadline = Math.min(flow.expires_at ?? Infinity, Date.now() + 900_000);
  const every = Math.max(2000, Math.min(flow.interval_ms ?? 2000, 900_000));
  let stopped = false;
  let claimed = false;
  let pendingInput: string | undefined;
  let running: Promise<void> | undefined;
  let timer: ReturnType<typeof setTimeout> | undefined;
  let expiry: ReturnType<typeof setTimeout> | undefined;
  let removeListener: (() => void) | undefined;
  const stop = () => {
    if (stopped) return;
    stopped = true;
    pendingInput = undefined;
    clearTimeout(timer); clearTimeout(expiry);
    removeListener?.();
    void transport.cancel().catch(() => {});
  };
  const finish = (verdict: AuthVerdict) => {
    if (stopped || verdict.status === 'pending') return;
    stop(); onVerdict(verdict);
  };
  const fail = (message: string) => finish({ status: 'error', message });
  const expired = () => !Number.isFinite(deadline) || Date.now() >= deadline;
  const schedule = () => {
    if (!stopped && flow.kind !== 'api-key') timer = setTimeout(() => { void step(); }, every);
  };
  const step = (): Promise<void> => {
    if (stopped) return Promise.resolve();
    if (running) return running;
    clearTimeout(timer);
    running = (async () => {
      try {
        if (expired()) { fail('Authorization timed out. Start sign-in again.'); return; }
        if (pendingInput) {
          try { finish(await transport.complete(pendingInput)); }
          catch { /* A lost response is resolved by polling before retrying the same input. */ }
        }
        if (!stopped && !expired()) {
          try {
            const verdict = await transport.poll();
            if (!stopped) { onConnection?.(null); finish(verdict); }
          } catch { if (!stopped) onConnection?.('Connection interrupted. Retrying authorization status…'); }
        }
      } finally { running = undefined; schedule(); }
    })();
    return running;
  };
  const complete = async (input: string) => {
    clearTimeout(timer);
    while (running) await running;
    clearTimeout(timer);
    if (stopped || expired()) return;
    // Manual input uses the same serialized operation as an app return, but a rejected
    // paste is shown to the caller rather than automatically resubmitted.
    running = (async () => {
      try { finish(await transport.complete(input)); }
      finally { running = undefined; schedule(); }
    })();
    return running;
  };
  const url = flow.url ?? flow.verification_uri;
  const local = hasNativeLoopback(flow) ? nativeOAuth() : undefined;
  const open = () => {
    if (stopped || expired() || !url) return;
    if (local) void local.reopen({ flowId: flow.flow_id }).catch(() => fail('Cannot reopen sign-in. Start sign-in again.'));
    else openAuthUrl(url);
  };
  const start = async () => {
    try {
      if (expired()) { fail('Authorization timed out. Start sign-in again.'); return; }
      expiry = setTimeout(() => fail('Authorization timed out. Start sign-in again.'), deadline - Date.now());
      if (local && url) {
        const state = authorizationState(url, flow.redirect_uri);
        if (!state) {
          fail('This host cannot receive the requested callback. Start sign-in again.'); return;
        }
        removeListener = () => { void local.cancel({ flowId: flow.flow_id }).catch(() => {}); };
        void local.authorize({ flowId: flow.flow_id, authorizationUrl: url, redirectUri: flow.redirect_uri!,
          state, expiresAt: deadline }).then(({ url: returned }) => {
          if (stopped || claimed || expired()) return;
          const input = verifiedReturn(returned, state, flow.redirect_uri);
          if (!input) { fail('Invalid sign-in return. Start sign-in again.'); return; }
          claimed = true; pendingInput = input; void step();
        }).catch(() => { if (!stopped) fail('Sign-in was closed or could not receive its callback. Start sign-in again.'); });
        schedule(); return;
      }
      if (url) openAuthUrl(url);
      schedule();
    } catch { fail('Cannot open secure sign-in. Start sign-in again.'); }
  };
  void start();
  return { stop, open, complete };
}
