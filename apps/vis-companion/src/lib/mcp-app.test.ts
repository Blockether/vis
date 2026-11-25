import { describe, expect, it, vi } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/mcp-app.json';
import {
  McpAppBridge,
  MCP_APP_PROTOCOL,
  MCP_APP_VIEW_SANDBOX,
  parseMcpAppEnvelope,
  withContentSecurityPolicy,
  type McpAppEnvelope,
  type McpAppHost,
} from './mcp-app';
import { VIEW_AGENT_NOTE_MAX } from './view';

const envelope = parseMcpAppEnvelope(JSON.stringify(fixture.envelope)) as McpAppEnvelope;

function bridgeWith(host: Partial<McpAppHost> = {}) {
  const posted: Record<string, unknown>[] = [];
  const full: McpAppHost = {
    rpc: vi.fn(async () => ({ result: { content: [] } })),
    openLink: vi.fn(),
    sendMessage: vi.fn(async () => true),
    setModelContext: vi.fn(async () => undefined),
    setDisplayMode: vi.fn(),
    resize: vi.fn(),
    ...host,
  };
  const bridge = new McpAppBridge(envelope, full, (message) => posted.push(message), { displayMode: 'inline' });
  return { bridge, posted, host: full };
}

const request = (id: number, method: string, params: unknown = {}) => ({ jsonrpc: '2.0', id, method, params });
const notify = (method: string, params: unknown = {}) => ({ jsonrpc: '2.0', method, params });

async function initialized(bridge: McpAppBridge, modes: string[] = ['inline']) {
  await bridge.handle(request(1, 'ui/initialize', { appCapabilities: { availableDisplayModes: modes } }));
  await bridge.handle(notify('ui/notifications/initialized'));
}

describe('MCP App envelope', () => {
  it('reads the contract fixture and refuses another revision', () => {
    expect(envelope?.server).toBe('acme-cloud');
    expect(parseMcpAppEnvelope(JSON.stringify({ ...fixture.envelope, protocol_version: '2025-01-01' }))).toBeNull();
    expect(parseMcpAppEnvelope('not json')).toBeNull();
  });

  it('puts the policy of the view before each script of the view', () => {
    const html = withContentSecurityPolicy('<html><head><script>x()</script></head></html>', `default-src 'none'; a "b"`);
    expect(html.indexOf('Content-Security-Policy')).toBeLessThan(html.indexOf('<script>'));
    expect(html).toContain('content="default-src \'none\'; a &quot;b&quot;"');
    expect(withContentSecurityPolicy('<p>x</p>', 'p')).toMatch(/^<!doctype html><html><head><meta http-equiv/);
  });
});

describe('MCP App bridge', () => {
  it('loads the view in the proxy with its policy and never with the app origin', async () => {
    const { bridge, posted } = bridgeWith();
    await bridge.handle(notify('ui/notifications/sandbox-proxy-ready'));
    expect(posted).toHaveLength(1);
    const { method, params } = posted[0] as { method: string; params: Record<string, unknown> };
    expect(method).toBe('ui/notifications/sandbox-resource-ready');
    expect(params.html).toContain(envelope.content_security_policy);
    expect(params.sandbox).toBe(MCP_APP_VIEW_SANDBOX);
    expect(String(params.sandbox)).not.toContain('allow-same-origin');
    expect(params.csp).toEqual(envelope.csp);
    expect(params.permissions).toEqual(envelope.permissions);
  });

  it('answers initialize, then gives the tool input and the result one time, in order', async () => {
    const { bridge, posted } = bridgeWith();
    await bridge.handle(request(1, 'ui/initialize', { appCapabilities: {} }));
    expect(posted).toHaveLength(1);
    const answer = posted[0] as { id: number; result: Record<string, any> };
    expect(answer.id).toBe(1);
    expect(answer.result.protocolVersion).toBe(MCP_APP_PROTOCOL);
    expect(Object.keys(answer.result.hostCapabilities)).toEqual(
      expect.arrayContaining(['openLinks', 'serverTools', 'serverResources', 'logging', 'sandbox']),
    );
    expect(answer.result.hostContext.toolInfo).toEqual({ id: envelope.call_id, tool: envelope.tool });
    expect(answer.result.hostContext.displayMode).toBe('inline');

    bridge.updateContext({ theme: 'dark' });
    expect(posted).toHaveLength(1);

    await bridge.handle(notify('ui/notifications/initialized'));
    await bridge.handle(notify('ui/notifications/initialized'));
    expect(posted.slice(1)).toEqual([
      notify('ui/notifications/tool-input', { arguments: envelope.arguments }),
      notify('ui/notifications/tool-result', envelope.result),
    ]);
    bridge.updateContext({ theme: 'light' });
    expect(posted[3]).toEqual(notify('ui/notifications/host-context-changed', { theme: 'light' }));
  });

  it('sends a server request of the view through the host and gives back its answer or error', async () => {
    const rpc = vi
      .fn<McpAppHost['rpc']>()
      .mockResolvedValueOnce({ result: { content: [{ type: 'text', text: 'ok' }] } })
      .mockResolvedValueOnce({ error: { code: 403, message: 'keeps the tool for the model' } });
    const { bridge, posted } = bridgeWith({ rpc });
    await bridge.handle(request(7, 'tools/call', { name: 'audit', arguments: {} }));
    await bridge.handle(request(8, 'resources/read', { uri: 'ui://acme-cloud/x' }));
    expect(rpc).toHaveBeenNthCalledWith(1, 'tools/call', { name: 'audit', arguments: {} });
    expect(posted).toEqual([
      { jsonrpc: '2.0', id: 7, result: { content: [{ type: 'text', text: 'ok' }] } },
      { jsonrpc: '2.0', id: 8, error: { code: 403, message: 'keeps the tool for the model' } },
    ]);
  });

  it('answers ping, refuses an unknown request and ignores an unknown notification', async () => {
    const { bridge, posted } = bridgeWith();
    await bridge.handle(request(1, 'ping'));
    await bridge.handle(request(2, 'sampling/createMessage'));
    await bridge.handle(notify('ui/notifications/unknown'));
    expect(posted).toEqual([
      { jsonrpc: '2.0', id: 1, result: {} },
      { jsonrpc: '2.0', id: 2, error: { code: -32601, message: 'Method not found: sampling/createMessage' } },
    ]);
  });

  it('opens only an http or https link', async () => {
    const { bridge, posted, host } = bridgeWith();
    await bridge.handle(request(1, 'ui/open-link', { url: 'javascript:alert(1)' }));
    await bridge.handle(request(2, 'ui/open-link', { url: 'https://acme.example/d1' }));
    expect((posted[0] as { error: { code: number } }).error.code).toBe(-32602);
    expect(posted[1]).toEqual({ jsonrpc: '2.0', id: 2, result: {} });
    expect(host.openLink).toHaveBeenCalledExactlyOnceWith('https://acme.example/d1');
  });

  it('sends a user message only when the person agrees', async () => {
    const sendMessage = vi.fn<McpAppHost['sendMessage']>().mockResolvedValueOnce(false).mockResolvedValueOnce(true);
    const { bridge, posted } = bridgeWith({ sendMessage });
    const message = { role: 'user', content: { type: 'text', text: 'Promote d2' } };
    await bridge.handle(request(1, 'ui/message', message));
    await bridge.handle(request(2, 'ui/message', message));
    expect(sendMessage).toHaveBeenCalledWith('Promote d2');
    expect(posted).toEqual([
      { jsonrpc: '2.0', id: 1, error: { code: -32000, message: 'Message sending denied' } },
      { jsonrpc: '2.0', id: 2, result: {} },
    ]);
  });

  it('changes the display mode only to a mode that the view and the host both have', async () => {
    const { bridge, posted, host } = bridgeWith();
    await initialized(bridge, ['inline']);
    await bridge.handle(request(2, 'ui/request-display-mode', { mode: 'fullscreen' }));
    expect(posted.at(-1)).toEqual({ jsonrpc: '2.0', id: 2, result: { mode: 'inline' } });
    expect(host.setDisplayMode).not.toHaveBeenCalled();

    const other = bridgeWith();
    await initialized(other.bridge, ['inline', 'fullscreen', 'pip']);
    await other.bridge.handle(request(2, 'ui/request-display-mode', { mode: 'pip' }));
    await other.bridge.handle(request(3, 'ui/request-display-mode', { mode: 'fullscreen' }));
    expect(other.posted.slice(-2)).toEqual([
      { jsonrpc: '2.0', id: 2, result: { mode: 'inline' } },
      { jsonrpc: '2.0', id: 3, result: { mode: 'fullscreen' } },
    ]);
    expect(other.host.setDisplayMode).toHaveBeenCalledExactlyOnceWith('fullscreen');
  });

  it('keeps the model context of the view within the note bound', async () => {
    const { bridge, host } = bridgeWith();
    await bridge.handle(
      request(1, 'ui/update-model-context', {
        content: [{ type: 'text', text: 'Selected d2' }],
        structuredContent: { selected: 'd2' },
      }),
    );
    await bridge.handle(request(2, 'ui/update-model-context', { content: [{ type: 'text', text: 'x'.repeat(VIEW_AGENT_NOTE_MAX + 50) }] }));
    expect(host.setModelContext).toHaveBeenNthCalledWith(1, 'Selected d2\n{"selected":"d2"}');
    expect((host.setModelContext as ReturnType<typeof vi.fn>).mock.calls[1][0]).toHaveLength(VIEW_AGENT_NOTE_MAX);
  });

  it('resizes to the size that the view reports', async () => {
    const { bridge, host } = bridgeWith();
    await bridge.handle(notify('ui/notifications/size-changed', { width: 400, height: 260 }));
    expect(host.resize).toHaveBeenCalledExactlyOnceWith({ width: 400, height: 260 });
  });

  it('asks the view to save its state before teardown and then stops', async () => {
    const { bridge, posted } = bridgeWith();
    await initialized(bridge);
    const done = bridge.teardown('closed');
    const asked = posted.at(-1) as { id: string; method: string; params: unknown };
    expect(asked.method).toBe('ui/resource-teardown');
    expect(asked.params).toEqual({ reason: 'closed' });
    await bridge.handle({ jsonrpc: '2.0', id: asked.id, result: {} });
    await done;
    const count = posted.length;
    await bridge.handle(request(9, 'ping'));
    expect(posted).toHaveLength(count);
  });
});
