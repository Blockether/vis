import { memo, useCallback, useEffect, useRef, useState } from 'react';
import { openExternalUrl } from '../lib/desktop';
import type { GatewayClient } from '../lib/gateway';
import {
  clearMcpAppOpen,
  isMcpAppOpenRequested,
  McpAppBridge,
  MCP_APP_PROXY_HTML,
  parseMcpAppEnvelope,
  type McpAppDisplayMode,
  type McpAppEnvelope,
} from '../lib/mcp-app';
import type { IterationAttachment } from '../lib/types';
import { OverlayScreen } from './DocArtifact';
import { FullScreenIcon } from './icons';
import { ListRow, TextButton } from './ui';

/** The tallest that an inline view can make itself. */
const INLINE_MAX_HEIGHT = 720;
const INLINE_MIN_HEIGHT = 80;
const INLINE_START_HEIGHT = 320;

/** The name of the tool, as its view shows it. */
function appTitle(envelope: McpAppEnvelope): string {
  const title = envelope.tool.title;
  return typeof title === 'string' && title.trim() ? title : envelope.tool.name;
}

/** A message that the view asks to send, until the person decides. */
type Consent = { text: string; decide: (send: boolean) => void };

/**
 * One MCP App view: the sandbox proxy frame and the bridge to it. The view lives
 * as long as this component. `mode` is the display mode that the view receives.
 */
export function McpAppFrame({
  client,
  sid,
  attachment,
  envelope,
  mode,
  onMode,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
  envelope: McpAppEnvelope;
  mode: McpAppDisplayMode;
  onMode: (mode: McpAppDisplayMode) => void;
}) {
  const frame = useRef<HTMLIFrameElement | null>(null);
  const [height, setHeight] = useState(INLINE_START_HEIGHT);
  const [consent, setConsent] = useState<Consent | null>(null);
  const modeChange = useRef(onMode);
  useEffect(() => {
    modeChange.current = onMode;
  }, [onMode]);
  const iterationId = attachment.iteration_id ?? '';
  const index = attachment.index ?? 0;

  useEffect(() => {
    const target = frame.current;
    if (!target) return;
    const bridge = new McpAppBridge(
      envelope,
      {
        rpc: (method, params) => client.mcpAppRpc(sid, envelope.server, method, params),
        openLink: (url) => openExternalUrl(url),
        sendMessage: (text) =>
          new Promise<boolean>((resolve) => {
            setConsent({
              text,
              decide: (send) => {
                setConsent(null);
                if (!send) return resolve(false);
                client.submitTurn(sid, text, { displayRequest: text }).then(
                  () => resolve(true),
                  (error: unknown) => {
                    console.warn('mcp app: message failed', { sid, error });
                    resolve(false);
                  },
                );
              },
            });
          }),
        setModelContext: (text) =>
          iterationId ? client.setPageAgentNote(sid, iterationId, index, text) : Promise.resolve(),
        setDisplayMode: (next) => modeChange.current(next),
        resize: (size) => {
          if (size.height) setHeight(Math.min(INLINE_MAX_HEIGHT, Math.max(INLINE_MIN_HEIGHT, Math.ceil(size.height))));
        },
      },
      (message) => target.contentWindow?.postMessage(message, '*'),
      {
        displayMode: mode,
        theme: window.matchMedia?.('(prefers-color-scheme: dark)').matches ? 'dark' : 'light',
        locale: navigator.language,
        timeZone: Intl.DateTimeFormat().resolvedOptions().timeZone,
        platform: /Android|iPhone|iPad/i.test(navigator.userAgent) ? 'mobile' : 'web',
        containerDimensions: mode === 'inline' ? { maxHeight: INLINE_MAX_HEIGHT } : {},
      },
    );
    const onMessage = (event: MessageEvent) => {
      if (event.source !== target.contentWindow) return;
      void bridge.handle(event.data);
    };
    window.addEventListener('message', onMessage);
    return () => {
      window.removeEventListener('message', onMessage);
      void bridge.teardown('host closed the view');
    };
    // A frame keeps its mode: a new mode is a new frame (`McpAppAttachment`).
  }, [client, sid, envelope, iterationId, index, mode]);

  const border = envelope.prefers_border ? 'border border-hairline' : '';
  return (
    <div className={`flex min-h-0 min-w-0 flex-col ${mode === 'fullscreen' ? 'flex-1' : ''}`}>
      <iframe
        ref={frame}
        title={appTitle(envelope)}
        srcDoc={MCP_APP_PROXY_HTML}
        sandbox="allow-scripts"
        allow={envelope.allow || undefined}
        referrerPolicy="no-referrer"
        className={`block w-full min-w-0 bg-transparent ${border} ${mode === 'fullscreen' ? 'min-h-0 flex-1' : ''}`}
        style={mode === 'inline' ? { height } : undefined}
      />
      {consent && (
        <div role="alertdialog" aria-label="The app wants to send a message" className="flex min-w-0 flex-wrap items-center gap-2 border-t border-hairline px-2 py-1.5">
          <span className="min-w-0 flex-1 truncate font-mono text-ui text-muted" title={consent.text}>
            Send “{consent.text}”?
          </span>
          <TextButton onClick={() => consent.decide(true)}>Send</TextButton>
          <TextButton onClick={() => consent.decide(false)}>Deny</TextButton>
        </div>
      )}
    </div>
  );
}

/**
 * An MCP App in the transcript. It renders in place, and full screen opens it in
 * the session overlay. It opens full screen at once when a link asked for it
 * (`requestMcpAppOpen`), for example the browser that the terminal client opened.
 */
export const McpAppAttachment = memo(function McpAppAttachment({
  client,
  sid,
  attachment,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
}) {
  const iterationId = attachment.iteration_id ?? '';
  const index = attachment.index ?? 0;
  const [envelope, setEnvelope] = useState<McpAppEnvelope | null>(null);
  const [failed, setFailed] = useState(false);
  const [mode, setMode] = useState<McpAppDisplayMode>(() =>
    isMcpAppOpenRequested(sid, iterationId, index) ? 'fullscreen' : 'inline',
  );

  useEffect(() => {
    if (!iterationId) return;
    let alive = true;
    client
      .attachmentBlob(sid, iterationId, index)
      .then((blob) => blob.text())
      .then((text) => {
        if (!alive) return;
        const parsed = parseMcpAppEnvelope(text);
        if (parsed) setEnvelope(parsed);
        else setFailed(true);
      })
      .catch(() => {
        if (alive) setFailed(true);
      });
    return () => {
      alive = false;
    };
  }, [client, sid, iterationId, index]);

  const close = useCallback(() => {
    clearMcpAppOpen(sid, iterationId, index);
    setMode('inline');
  }, [sid, iterationId, index]);

  if (!iterationId) return null;
  const name = envelope ? appTitle(envelope) : attachment.filename || 'MCP app';
  if (failed) {
    return <p className="font-mono text-ui text-footer-muted">✗ {name}: the app cannot open.</p>;
  }
  if (!envelope) return <p className="font-mono text-ui text-footer-muted">Opening {name}…</p>;
  const subtitle = `${envelope.server} · ${envelope.tool.name}`;
  return (
    <div className="min-w-0">
      {mode === 'fullscreen' ? (
        <>
          <ListRow onClick={() => setMode('fullscreen')} title={name} aria-label={`MCP app ${name}`}>
            <FullScreenIcon className="size-4 shrink-0 text-accent-ink" />
            <span className="min-w-0 flex-1 truncate font-mono text-ui text-muted">{name}</span>
          </ListRow>
          <OverlayScreen title={name} subtitle={subtitle} onClose={close}>
            <McpAppFrame client={client} sid={sid} attachment={attachment} envelope={envelope} mode="fullscreen" onMode={setMode} />
          </OverlayScreen>
        </>
      ) : (
        <div className="grid min-w-0 gap-1">
          <div className="flex min-w-0 items-center gap-2">
            <span className="min-w-0 flex-1 truncate font-mono text-meta text-footer-muted">
              {name} · {subtitle}
            </span>
            <TextButton aria-label={`Open ${name} full screen`} onClick={() => setMode('fullscreen')}>
              <FullScreenIcon className="size-3.5" />
            </TextButton>
          </div>
          <McpAppFrame client={client} sid={sid} attachment={attachment} envelope={envelope} mode="inline" onMode={setMode} />
        </div>
      )}
    </div>
  );
});
