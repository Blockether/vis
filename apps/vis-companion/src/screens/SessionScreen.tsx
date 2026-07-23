import { useCallback, useEffect, useRef, useState } from 'react';
import { AssistantMessage, Markdown, UserMessage } from '../components/ChatContent';
import { Banner } from '../components/ui';
import type { GatewayClient } from '../lib/gateway';
import type { Session, SseEvent, TranscriptTurn } from '../lib/types';

interface LiveTurn {
  id?: string;
  request: string;
  prose: string;
  reasoning: string;
  operation?: string;
  operationCount: number;
  status: 'running' | 'failed' | 'cancelled';
}

const TERMINAL_EVENTS = new Set(['turn.completed', 'turn.failed', 'turn.cancelled']);

function stringField(event: SseEvent, key: string): string {
  const value = event[key];
  return typeof value === 'string' ? value : '';
}

function applyText(current: string, event: SseEvent): string {
  const cumulative = stringField(event, 'cumulative');
  return cumulative || current + stringField(event, 'text');
}

export function SessionScreen({
  client,
  sid,
  onBack,
}: {
  client: GatewayClient;
  sid: string;
  onBack: () => void;
}) {
  const [session, setSession] = useState<Session | null>(null);
  const [turns, setTurns] = useState<TranscriptTurn[]>([]);
  const [prompt, setPrompt] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [connected, setConnected] = useState(false);
  const [running, setRunning] = useState(false);
  const [liveTurn, setLiveTurn] = useState<LiveTurn | null>(null);
  const [showJump, setShowJump] = useState(false);
  const scrollRef = useRef<HTMLDivElement>(null);
  const composerRef = useRef<HTMLTextAreaElement>(null);
  const followingRef = useRef(true);

  const scrollToEnd = useCallback((behavior: ScrollBehavior = 'smooth') => {
    const viewport = scrollRef.current;
    if (!viewport) return;
    viewport.scrollTo({ top: viewport.scrollHeight, behavior });
    followingRef.current = true;
    setShowJump(false);
  }, []);

  const loadTranscript = useCallback(async () => {
    try {
      const next = await client.transcript(sid);
      setTurns(next);
      setError(null);
      return true;
    } catch (cause) {
      setError((cause as Error).message);
      return false;
    } finally {
      setLoading(false);
    }
  }, [client, sid]);

  useEffect(() => {
    const controller = new AbortController();
    void Promise.all([
      loadTranscript(),
      client.session(sid, controller.signal).then(setSession).catch(() => undefined),
    ]);
    return () => controller.abort();
  }, [client, sid, loadTranscript]);

  useEffect(() => {
    async function settle(event: SseEvent) {
      const type = event.type;
      setRunning(false);
      const loaded = await loadTranscript();
      if (loaded) setLiveTurn(null);
      if (type === 'turn.failed') {
        setError(stringField(event, 'message') || stringField(event, 'error') || 'The turn failed.');
      }
    }

    const stop = client.streamEvents(
      sid,
      (event) => {
        const type = event.type;
        if (type === 'turn.started') {
          setRunning(true);
          setLiveTurn({
            id: stringField(event, 'turn_id'),
            request: stringField(event, 'request'),
            prose: '',
            reasoning: '',
            operationCount: 0,
            status: 'running',
          });
          return;
        }

        if (type === 'content.block.delta') {
          const field = stringField(event, 'field');
          setLiveTurn((turn) => {
            if (!turn) return turn;
            if (field === 'markdown') return { ...turn, prose: applyText(turn.prose, event) };
            if (field === 'text') return { ...turn, reasoning: applyText(turn.reasoning, event) };
            return turn;
          });
          return;
        }

        if (type === 'iteration.completed') {
          setLiveTurn((turn) => {
            if (!turn) return turn;
            return {
              ...turn,
              prose: stringField(event, 'assistant_prose') || turn.prose,
              reasoning: stringField(event, 'thinking') || turn.reasoning,
              operation: undefined,
            };
          });
          return;
        }

        if (type === 'block.preview' || type === 'block.started') {
          setLiveTurn((turn) => turn ? {
            ...turn,
            operation: type === 'block.preview'
              ? `Native call: ${stringField(event, 'result_summary') || stringField(event, 'tool_name') || 'tool'}`
              : stringField(event, 'tool_name') || stringField(event, 'scope') || 'Running operation',
            operationCount: type === 'block.started' ? turn.operationCount + 1 : turn.operationCount,
          } : turn);
          return;
        }

        if (type === 'block.output') {
          setLiveTurn((turn) => turn ? { ...turn, operation: undefined } : turn);
          return;
        }

        if (TERMINAL_EVENTS.has(type)) void settle(event);
      },
      {
        onOpen: () => setConnected(true),
        onError: () => setConnected(false),
      },
    );

    return () => {
      stop();
      setConnected(false);
    };
  }, [client, sid, loadTranscript]);

  useEffect(() => {
    if (followingRef.current) requestAnimationFrame(() => scrollToEnd('auto'));
  }, [turns, liveTurn, scrollToEnd]);

  useEffect(() => {
    const textarea = composerRef.current;
    if (!textarea) return;
    textarea.style.height = '0px';
    textarea.style.height = `${Math.min(textarea.scrollHeight, 160)}px`;
  }, [prompt]);

  async function send() {
    const request = prompt.trim();
    if (!request || running) return;

    setPrompt('');
    setError(null);
    setRunning(true);
    setLiveTurn({ request, prose: '', reasoning: '', operationCount: 0, status: 'running' });
    followingRef.current = true;
    requestAnimationFrame(() => scrollToEnd());

    try {
      const submitted = await client.submitTurn(sid, request);
      setLiveTurn((turn) => turn ? { ...turn, id: submitted.turn_id ?? submitted.id } : turn);
    } catch (cause) {
      setRunning(false);
      setLiveTurn(null);
      setPrompt(request);
      setError((cause as Error).message);
      requestAnimationFrame(() => composerRef.current?.focus());
    }
  }

  async function cancel() {
    try {
      await client.cancelCurrentTurn(sid);
      setLiveTurn((turn) => turn ? { ...turn, status: 'cancelled', operation: undefined } : turn);
    } catch (cause) {
      setError((cause as Error).message);
    }
  }

  function handleScroll() {
    const viewport = scrollRef.current;
    if (!viewport) return;
    const distance = viewport.scrollHeight - viewport.scrollTop - viewport.clientHeight;
    followingRef.current = distance < 96;
    setShowJump(!followingRef.current);
  }

  const title = session?.title?.trim() || 'Chat';

  return (
    <section className="relative flex h-full min-h-0 flex-col overflow-hidden bg-ink">
      <header className="z-10 flex min-h-15 shrink-0 items-center gap-3 border-b border-edge/70 bg-panel/90 px-3.5 py-2 backdrop-blur-xl max-sm:min-h-13 max-sm:pl-2.5">
        <button
          type="button"
          className="grid size-10 place-items-center rounded-xl border border-transparent bg-transparent text-3xl leading-none text-white/70 transition hover:border-edge hover:bg-panel-2 hover:text-white"
          onClick={onBack}
          aria-label="Back to sessions"
        >
          <span aria-hidden="true">‹</span>
        </button>
        <div className="min-w-0 flex-1">
          <h1 className="truncate text-sm font-semibold">{title}</h1>
          <div className="flex items-center gap-1.5 text-[11px] text-white/40">
            <span
              className={`size-1.5 rounded-full ${connected ? 'bg-ok shadow-[0_0_0.5rem_var(--ok)]' : 'bg-white/20'}`}
            />
            {connected ? 'Gateway connected' : 'Reconnecting'}
          </div>
        </div>
        <span className="hidden max-w-[30%] truncate font-mono text-[10px] text-white/25 sm:block">{sid}</span>
      </header>

      <div
        ref={scrollRef}
        className="min-h-0 flex-1 overflow-x-hidden overflow-y-auto overscroll-contain scroll-smooth motion-reduce:scroll-auto"
        onScroll={handleScroll}
        role="log"
        aria-live="polite"
      >
        <div className="mx-auto min-h-full w-full max-w-4xl px-4 pb-12 pt-6 sm:px-8 sm:pt-10">
          {error && <Banner kind="err">{error}</Banner>}

          {loading && !turns.length ? (
            <div className="flex justify-center gap-1.5 py-20" aria-label="Loading conversation">
              <span className="size-1.5 animate-bounce rounded-full bg-accent motion-reduce:animate-none" />
              <span className="size-1.5 animate-bounce rounded-full bg-accent [animation-delay:150ms] motion-reduce:animate-none" />
              <span className="size-1.5 animate-bounce rounded-full bg-accent [animation-delay:300ms] motion-reduce:animate-none" />
            </div>
          ) : !turns.length && !liveTurn ? (
            <div className="flex min-h-[55vh] flex-col items-center justify-center text-center">
              <div
                className="grid size-11 place-items-center rounded-xl border border-accent/35 bg-accent/10 font-mono text-lg font-bold text-accent shadow-lg shadow-accent/10"
                aria-hidden="true"
              >
                v
              </div>
              <h2 className="mb-1 mt-4 text-lg font-semibold">Start a conversation</h2>
              <p className="max-w-sm text-sm leading-6 text-white/45">
                This session is ready. Ask Vis to inspect, explain, or change your project.
              </p>
            </div>
          ) : null}

          {turns.map((turn, index) => {
            const request = turn.user_request ?? turn.request ?? '';
            return (
              <div className={index === 0 ? '' : 'mt-14'} key={turn.id ?? turn.turn_id}>
                {request && <UserMessage>{request}</UserMessage>}
                <AssistantMessage turn={turn} />
              </div>
            );
          })}

          {liveTurn && (
            <div className={turns.length ? 'mt-14' : ''} data-live="true">
              {liveTurn.request && <UserMessage>{liveTurn.request}</UserMessage>}
              <article className="mt-6 flex items-start gap-3 max-sm:gap-2.5">
                <div
                  className="grid size-7 shrink-0 place-items-center rounded-lg border border-accent/35 bg-accent/10 font-mono text-xs font-bold text-accent shadow-lg shadow-accent/10 max-sm:size-6 max-sm:rounded-md max-sm:text-[10px]"
                  aria-hidden="true"
                >
                  v
                </div>
                <div className="min-w-0 flex-1">
                  {liveTurn.reasoning && (
                    <details className="group mb-3 text-xs text-white/60">
                      <summary className="flex list-none cursor-pointer select-none items-center gap-2 py-1 hover:text-white/80 [&::-webkit-details-marker]:hidden">
                        <span className="font-mono text-accent">⌁</span>
                        Reasoning
                        <span className="inline-block transition-transform duration-150 group-open:rotate-90">›</span>
                      </summary>
                      <div className="mb-3 ml-2 mt-2 border-l border-edge py-0.5 pl-4 text-[13px] leading-relaxed text-white/55">
                        <Markdown>{liveTurn.reasoning}</Markdown>
                      </div>
                    </details>
                  )}
                  {liveTurn.prose ? (
                    <div className="text-[15px] leading-7 text-white/95">
                      <Markdown>{liveTurn.prose}</Markdown>
                      <span className="ml-1 inline-block h-[1em] w-1.5 translate-y-[0.15em] animate-pulse rounded-sm bg-accent motion-reduce:animate-none" />
                    </div>
                  ) : (
                    <div className="flex min-h-7 items-center gap-2.5 text-sm text-white/50">
                      <span className="flex gap-1">
                        <i className="size-1.5 animate-bounce rounded-full bg-accent motion-reduce:animate-none" />
                        <i className="size-1.5 animate-bounce rounded-full bg-accent [animation-delay:150ms] motion-reduce:animate-none" />
                        <i className="size-1.5 animate-bounce rounded-full bg-accent [animation-delay:300ms] motion-reduce:animate-none" />
                      </span>
                      <span>{liveTurn.operation ?? (liveTurn.status === 'cancelled' ? 'Stopping…' : 'Thinking…')}</span>
                    </div>
                  )}
                  {liveTurn.operationCount > 0 && (
                    <div className="mt-3 font-mono text-[10px] text-white/30">
                      {liveTurn.operationCount} {liveTurn.operationCount === 1 ? 'operation' : 'operations'}
                    </div>
                  )}
                </div>
              </article>
            </div>
          )}
        </div>
      </div>

      {showJump && (
        <button
          type="button"
          className="absolute bottom-24 left-1/2 z-20 -translate-x-1/2 rounded-full border border-edge bg-panel-2/95 px-3 py-1.5 text-xs text-white/75 shadow-xl shadow-black/40 backdrop-blur-xl transition hover:text-white max-sm:bottom-20"
          onClick={() => scrollToEnd()}
        >
          ↓ Newest
        </button>
      )}

      <footer className="z-10 shrink-0 border-t border-edge/70 bg-ink/90 px-3 pb-2 pt-3 backdrop-blur-xl sm:px-[max(1.5rem,calc((100%_-_50rem)/2))] max-sm:pb-1.5 max-sm:pt-2">
        <div className="flex items-end gap-2 rounded-2xl border border-edge bg-panel/90 p-2 shadow-2xl shadow-black/30 transition focus-within:border-accent/60 focus-within:ring-2 focus-within:ring-accent/10">
          <textarea
            ref={composerRef}
            rows={1}
            value={prompt}
            disabled={running}
            placeholder={running ? 'Vis is working…' : 'Message Vis'}
            aria-label="Message Vis"
            className="min-h-9 max-h-40 flex-1 resize-none overflow-y-auto border-0 bg-transparent px-1.5 py-2 text-[15px] leading-5 text-white outline-none placeholder:text-white/30 disabled:opacity-60"
            onChange={(event) => setPrompt(event.target.value)}
            onKeyDown={(event) => {
              if (event.key === 'Enter' && !event.shiftKey && !event.nativeEvent.isComposing) {
                event.preventDefault();
                void send();
              }
            }}
          />
          {running ? (
            <button
              type="button"
              className="grid size-9 shrink-0 place-items-center rounded-xl bg-err/15 transition hover:bg-err/25"
              onClick={cancel}
              aria-label="Stop response"
            >
              <span className="size-2.5 rounded-sm bg-err" />
            </button>
          ) : (
            <button
              type="button"
              className="grid size-9 shrink-0 place-items-center rounded-xl bg-accent font-bold text-ink transition hover:-translate-y-px hover:brightness-110 disabled:translate-y-0 disabled:opacity-30"
              onClick={send}
              disabled={!prompt.trim()}
              aria-label="Send message"
            >
              ↑
            </button>
          )}
        </div>
        <p className="mt-1.5 text-center text-[10px] text-white/25 max-sm:hidden">
          Enter to send · Shift+Enter for a new line
        </p>
      </footer>
    </section>
  );
}
