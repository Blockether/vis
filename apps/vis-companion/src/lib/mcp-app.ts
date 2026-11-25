// MCP Apps host (extension `io.modelcontextprotocol/ui`, revision 2026-01-26).
//
// A tool call that names a `ui://` view gives its turn an attachment of media
// type `MCP_APP_MEDIA`: the view HTML, the tool call and its result, and the
// policy that the engine derived from the view's metadata. The app hosts it in
// two frames. The outer frame is the sandbox proxy: an opaque origin, so it can
// never read the app's storage or the gateway token. The inner frame is the view,
// with the content security policy of the view. `McpAppBridge` is the host side
// of the JSON-RPC dialect that the view speaks through the proxy.

import { VIEW_AGENT_NOTE_MAX } from './view';

export const MCP_APP_MEDIA = 'application/vnd.vis.mcp-app+json';
export const MCP_APP_PROTOCOL = '2026-01-26';

/** The display modes that this host can give a view. */
export const MCP_APP_DISPLAY_MODES = ['inline', 'fullscreen'] as const;
export type McpAppDisplayMode = (typeof MCP_APP_DISPLAY_MODES)[number];

/** The bytes of an MCP App attachment (gateway.json `mcp_app_envelope`). */
export interface McpAppEnvelope {
  protocol_version: string;
  call_id: string;
  server: string;
  tool: { name: string; [key: string]: unknown };
  resource_uri: string;
  arguments: Record<string, unknown>;
  result: Record<string, unknown>;
  html: string;
  csp: Record<string, string[]>;
  content_security_policy: string;
  permissions: Record<string, Record<string, never>>;
  allow: string;
  prefers_border: boolean;
  domain?: string;
}

/** The gateway answer to one view request (gateway.json `mcp_app_rpc_answer`). */
export type McpAppRpcAnswer = { result: unknown } | { error: { code: number; message: string } };

const isObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === 'object' && value !== null && !Array.isArray(value);

/** The envelope in the text of an MCP App attachment, or null when the text is not one. */
export function parseMcpAppEnvelope(text: string): McpAppEnvelope | null {
  let value: unknown;
  try {
    value = JSON.parse(text);
  } catch {
    return null;
  }
  if (!isObject(value)) return null;
  const strings = ['call_id', 'server', 'resource_uri', 'html', 'content_security_policy', 'allow'];
  if (value.protocol_version !== MCP_APP_PROTOCOL) return null;
  if (!strings.every((key) => typeof value[key] === 'string')) return null;
  if (!isObject(value.tool) || typeof value.tool.name !== 'string') return null;
  if (!isObject(value.arguments) || !isObject(value.result) || !isObject(value.csp)) return null;
  if (!isObject(value.permissions) || typeof value.prefers_border !== 'boolean') return null;
  return value as unknown as McpAppEnvelope;
}

const escapeAttribute = (text: string): string =>
  text.replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

/**
 * The view HTML with its content security policy as the first element of its
 * head, so that the policy applies before any script of the view runs.
 */
export function withContentSecurityPolicy(html: string, policy: string): string {
  const meta = `<meta http-equiv="Content-Security-Policy" content="${escapeAttribute(policy)}">`;
  const head = /<head(\s[^>]*)?>/i.exec(html);
  if (head) return html.slice(0, head.index + head[0].length) + meta + html.slice(head.index + head[0].length);
  const root = /<html(\s[^>]*)?>/i.exec(html);
  if (root) {
    const at = root.index + root[0].length;
    return `${html.slice(0, at)}<head>${meta}</head>${html.slice(at)}`;
  }
  return `<!doctype html><html><head>${meta}</head><body>${html}</body></html>`;
}

/** The sandbox of the inner view frame. It never gets `allow-same-origin`. */
export const MCP_APP_VIEW_SANDBOX = 'allow-scripts allow-forms';

/**
 * The sandbox proxy document. It tells the host that it is ready, loads the view
 * from `ui/notifications/sandbox-resource-ready`, and forwards every other message
 * between the host and the view, in both directions.
 */
export const MCP_APP_PROXY_HTML = `<!doctype html><html><head><meta charset="utf-8">
<style>html,body{margin:0;padding:0;height:100%;background:transparent}iframe{border:0;width:100%;height:100%;display:block}</style>
</head><body><script>
(function () {
  var view = null;
  var PREFIX = 'ui/notifications/sandbox-';
  function method(data) { return data && typeof data === 'object' ? data.method : undefined; }
  window.addEventListener('message', function (event) {
    var data = event.data;
    var name = method(data);
    if (event.source === window.parent) {
      if (name === PREFIX + 'resource-ready') {
        var p = data.params || {};
        if (view) view.remove();
        view = document.createElement('iframe');
        view.setAttribute('sandbox', typeof p.sandbox === 'string' ? p.sandbox : 'allow-scripts');
        if (typeof p.allow === 'string' && p.allow) view.setAttribute('allow', p.allow);
        view.setAttribute('referrerpolicy', 'no-referrer');
        view.srcdoc = String(p.html || '');
        document.body.appendChild(view);
        return;
      }
      if (typeof name === 'string' && name.indexOf(PREFIX) === 0) return;
      if (view && view.contentWindow) view.contentWindow.postMessage(data, '*');
      return;
    }
    if (view && event.source === view.contentWindow) {
      if (typeof name === 'string' && name.indexOf(PREFIX) === 0) return;
      window.parent.postMessage(data, '*');
    }
  });
  window.parent.postMessage({ jsonrpc: '2.0', method: PREFIX + 'proxy-ready', params: {} }, '*');
})();
</script></body></html>`;

/** What the bridge needs from the app around the view. */
export interface McpAppHost {
  /** One MCP request to the server of the view, through the gateway. */
  rpc(method: string, params: unknown): Promise<McpAppRpcAnswer>;
  /** Open an http(s) URL outside the app. */
  openLink(url: string): void;
  /** Ask the person, then send a user message to the session. False: the person refused. */
  sendMessage(text: string): Promise<boolean>;
  /** Keep the context that the model reads with the next user message. */
  setModelContext(text: string): Promise<void>;
  /** The view asked for a display mode that the host and the view both have. */
  setDisplayMode(mode: McpAppDisplayMode): void;
  /** The view reports its size. */
  resize(size: { width?: number; height?: number }): void;
}

/** The host context that the view receives with `ui/initialize`. */
export interface McpAppContext {
  theme?: 'light' | 'dark';
  displayMode: McpAppDisplayMode;
  locale?: string;
  timeZone?: string;
  platform?: 'web' | 'desktop' | 'mobile';
  containerDimensions?: Record<string, number>;
}

type Message = { jsonrpc?: string; id?: string | number; method?: string; params?: unknown; result?: unknown; error?: unknown };

const METHOD_NOT_FOUND = -32601;
const INVALID_PARAMS = -32602;
const DENIED = -32000;
/** The MCP methods that a view may send to its server through the host. */
const SERVER_METHODS = new Set(['tools/call', 'tools/list', 'resources/read', 'resources/list', 'resources/templates/list']);
/** How long a view has to answer `ui/resource-teardown`. */
const TEARDOWN_WAIT_MS = 1500;

/** The text blocks of MCP content, joined. */
function contentText(content: unknown): string {
  const blocks = Array.isArray(content) ? content : content === undefined ? [] : [content];
  return blocks
    .filter((block): block is { type: string; text: string } => isObject(block) && block.type === 'text' && typeof block.text === 'string')
    .map((block) => block.text)
    .join('\n');
}

/**
 * The host side of one MCP App view. `handle` takes each message that the proxy
 * frame posts, and `post` sends a message to it. The bridge sends no message to
 * the view before `ui/notifications/initialized`, then the tool input and the
 * tool result, each one time.
 */
export class McpAppBridge {
  private initialized = false;
  private delivered = false;
  private closed = false;
  private viewModes: string[] = [];
  private nextId = 1;
  private readonly waiting = new Map<string | number, (message: Message) => void>();
  private context: McpAppContext;
  private readonly envelope: McpAppEnvelope;
  private readonly host: McpAppHost;
  private readonly post: (message: Message) => void;
  private readonly hostInfo: { name: string; version: string };

  constructor(
    envelope: McpAppEnvelope,
    host: McpAppHost,
    post: (message: Message) => void,
    context: McpAppContext,
    hostInfo = { name: 'vis-companion', version: '1' },
  ) {
    this.envelope = envelope;
    this.host = host;
    this.post = post;
    this.context = context;
    this.hostInfo = hostInfo;
  }

  /** Whether the view finished its initialization. */
  get isInitialized(): boolean {
    return this.initialized;
  }

  /** The current display mode of the view. */
  get displayMode(): McpAppDisplayMode {
    return this.context.displayMode;
  }

  /** Handle one message from the proxy frame. */
  async handle(data: unknown): Promise<void> {
    if (this.closed || !isObject(data)) return;
    const message = data as Message;
    if (message.method === undefined) {
      if (message.id !== undefined) this.waiting.get(message.id)?.(message);
      return;
    }
    if (typeof message.method !== 'string') return;
    const isRequest = message.id !== undefined && message.id !== null;
    const answer = await this.dispatch(message.method, message.params, isRequest);
    if (!isRequest || answer === undefined || this.closed) return;
    this.post({ jsonrpc: '2.0', id: message.id, ...answer });
  }

  /** Give the view a changed part of the host context. */
  updateContext(change: Partial<McpAppContext>): void {
    this.context = { ...this.context, ...change };
    if (this.initialized && !this.closed) {
      this.post({ jsonrpc: '2.0', method: 'ui/notifications/host-context-changed', params: change });
    }
  }

  /**
   * Ask the view to save its state before the host removes it. The bridge waits a
   * short time for the answer, then it stops in each case.
   */
  async teardown(reason: string): Promise<void> {
    if (this.closed) return;
    if (this.initialized) {
      const id = `host-${this.nextId++}`;
      const answered = new Promise<void>((resolve) => {
        this.waiting.set(id, () => resolve());
        setTimeout(resolve, TEARDOWN_WAIT_MS);
      });
      this.post({ jsonrpc: '2.0', id, method: 'ui/resource-teardown', params: { reason } });
      await answered;
      this.waiting.delete(id);
    }
    this.closed = true;
  }

  private hostContext(): Record<string, unknown> {
    return {
      toolInfo: { id: this.envelope.call_id, tool: this.envelope.tool },
      availableDisplayModes: [...MCP_APP_DISPLAY_MODES],
      userAgent: this.hostInfo.name,
      ...this.context,
    };
  }

  private async dispatch(
    method: string,
    params: unknown,
    isRequest: boolean,
  ): Promise<{ result: unknown } | { error: { code: number; message: string } } | undefined> {
    const given = isObject(params) ? params : {};
    switch (method) {
      case 'ui/notifications/sandbox-proxy-ready':
        this.post({
          jsonrpc: '2.0',
          method: 'ui/notifications/sandbox-resource-ready',
          params: {
            html: withContentSecurityPolicy(this.envelope.html, this.envelope.content_security_policy),
            sandbox: MCP_APP_VIEW_SANDBOX,
            csp: this.envelope.csp,
            permissions: this.envelope.permissions,
            allow: this.envelope.allow,
          },
        });
        return undefined;
      case 'ui/initialize': {
        const capabilities = isObject(given.appCapabilities) ? given.appCapabilities : {};
        this.viewModes = Array.isArray(capabilities.availableDisplayModes)
          ? capabilities.availableDisplayModes.filter((mode): mode is string => typeof mode === 'string')
          : [];
        return {
          result: {
            protocolVersion: MCP_APP_PROTOCOL,
            hostCapabilities: {
              openLinks: {},
              serverTools: {},
              serverResources: {},
              logging: {},
              sandbox: { permissions: this.envelope.permissions, csp: this.envelope.csp },
            },
            hostInfo: this.hostInfo,
            hostContext: this.hostContext(),
          },
        };
      }
      case 'ui/notifications/initialized':
        this.initialized = true;
        if (!this.delivered) {
          this.delivered = true;
          this.post({ jsonrpc: '2.0', method: 'ui/notifications/tool-input', params: { arguments: this.envelope.arguments } });
          this.post({ jsonrpc: '2.0', method: 'ui/notifications/tool-result', params: this.envelope.result });
        }
        return undefined;
      case 'ping':
        return { result: {} };
      case 'notifications/message':
        console.info('mcp app log', this.envelope.server, given);
        return undefined;
      case 'ui/notifications/size-changed': {
        const size: { width?: number; height?: number } = {};
        if (typeof given.width === 'number' && given.width > 0) size.width = given.width;
        if (typeof given.height === 'number' && given.height > 0) size.height = given.height;
        this.host.resize(size);
        return undefined;
      }
      case 'ui/open-link': {
        const url = typeof given.url === 'string' ? given.url : '';
        let parsed: URL | null = null;
        try {
          parsed = new URL(url);
        } catch {
          parsed = null;
        }
        if (!parsed || (parsed.protocol !== 'https:' && parsed.protocol !== 'http:')) {
          return { error: { code: INVALID_PARAMS, message: 'Only an http or https URL can open.' } };
        }
        this.host.openLink(parsed.href);
        return { result: {} };
      }
      case 'ui/message': {
        const text = contentText(given.content).trim();
        if (given.role !== 'user' || !text) {
          return { error: { code: INVALID_PARAMS, message: 'A message needs the user role and text content.' } };
        }
        const sent = await this.host.sendMessage(text);
        return sent ? { result: {} } : { error: { code: DENIED, message: 'Message sending denied' } };
      }
      case 'ui/request-display-mode': {
        const mode = given.mode;
        const known = (MCP_APP_DISPLAY_MODES as readonly string[]).includes(String(mode));
        if (known && this.viewModes.includes(String(mode)) && mode !== this.context.displayMode) {
          this.context = { ...this.context, displayMode: mode as McpAppDisplayMode };
          this.host.setDisplayMode(mode as McpAppDisplayMode);
        }
        return { result: { mode: this.context.displayMode } };
      }
      case 'ui/update-model-context': {
        const parts = [contentText(given.content)];
        if (given.structuredContent !== undefined) parts.push(JSON.stringify(given.structuredContent));
        const text = parts.filter(Boolean).join('\n').slice(0, VIEW_AGENT_NOTE_MAX);
        await this.host.setModelContext(text);
        return { result: {} };
      }
      default:
        if (SERVER_METHODS.has(method)) {
          try {
            return await this.host.rpc(method, params ?? {});
          } catch (cause) {
            return { error: { code: DENIED, message: cause instanceof Error ? cause.message : 'The request failed.' } };
          }
        }
        return isRequest ? { error: { code: METHOD_NOT_FOUND, message: `Method not found: ${method}` } } : undefined;
    }
  }
}

// An MCP App that a link asked to open full screen, for example the browser that
// the terminal client opened. The key is the session and `<iteration id>/<index>`.
const requestedOpens = new Set<string>();
const openKey = (sid: string, app: string) => `${sid}\u0000${app}`;

/** The route value that names one MCP App attachment (`#/s/<sid>?app=<value>`). */
export function mcpAppRouteValue(iterationId: string, index: number): string {
  return `${iterationId}/${index}`;
}

/** Ask the attachment that `app` names to open full screen when it renders. */
export function requestMcpAppOpen(sid: string, app: string): void {
  requestedOpens.add(openKey(sid, app));
}

/** Whether a link asked this attachment to open full screen. */
export function isMcpAppOpenRequested(sid: string, iterationId: string, index: number): boolean {
  return requestedOpens.has(openKey(sid, mcpAppRouteValue(iterationId, index)));
}

/** Forget the request after the person closed the app. */
export function clearMcpAppOpen(sid: string, iterationId: string, index: number): void {
  requestedOpens.delete(openKey(sid, mcpAppRouteValue(iterationId, index)));
}
