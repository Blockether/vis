import { useEffect, useMemo, useRef, useState, type KeyboardEvent } from 'react';
import type { AgentFiles, Subagent } from '../lib/types';
import { isAsking } from './AgentTeam';
import { ChevronIcon, CircleAlertIcon, MARK_NUDGE, RoleIcon } from './icons';
import { Button } from './ui';

/** The kind of an open request; each kind has its own answers. */
export type RequestKind = 'budget' | 'files' | 'question' | 'form';

/** One open request of one agent. */
export interface AgentRequest {
  key: string;
  kind: RequestKind;
  agent: Subagent;
  /** The A-number of the agent in its team: its start order. */
  number: number;
  text: string;
}

const FALLBACK_ITERATIONS = 10;
const DENY_REASON = 'Not now. Continue with what you have.';

// The kind of an agent's open question. A budget or file request also carries question text.
function questionKind(agent: Subagent): RequestKind | null {
  if (agent.budget_request) return 'budget';
  if (agent.files_request) return 'files';
  return agent.question ? 'question' : null;
}

/**
 * The open requests of the agents that `sid` leads, oldest first by Council entry.
 * The requests of one agent stand together. A form has no entry, so it comes last.
 */
export function openRequests(agents: readonly Subagent[], sid: string): AgentRequest[] {
  const teams = new Map<string, Subagent[]>();
  for (const agent of agents) teams.set(agent.team_id, [...(teams.get(agent.team_id) ?? []), agent]);
  const age = (agent: Subagent) => agent.question_entry_id ?? Number.MAX_SAFE_INTEGER;
  return agents
    .filter((agent) => agent.parent_id === sid && isAsking(agent))
    .sort((a, b) => age(a) - age(b))
    .flatMap((agent) => {
      const number = (teams.get(agent.team_id) ?? []).indexOf(agent) + 1;
      const kind = questionKind(agent);
      const requests: AgentRequest[] = [];
      if (kind) {
        const text = agent.question ?? agent.budget_request?.reason ?? agent.files_request?.reason ?? '';
        requests.push({ key: `${agent.session_id}:${kind}`, kind, agent, number, text });
      }
      if (agent.pending_input) {
        const count = agent.pending_input_count ?? 1;
        const text = count > 1 ? `Waits for you on ${count} forms.` : 'Waits for you on a form.';
        requests.push({ key: `${agent.session_id}:form`, kind: 'form', agent, number, text });
      }
      return requests;
    });
}

// The requested rules, one line each, so a person sees what a grant gives.
function fileLines(files: AgentFiles | undefined): string[] {
  if (!files) return [];
  return [
    ...(files.write ?? []).map((rule) => `write ${rule}`),
    ...(files.read_only ?? []).map((rule) => `read ${rule}`),
    ...(files.deny ?? []).map((rule) => `deny ${rule}`),
  ];
}

// The answers that the inbox calls. Each one gives an error text, or null when it worked.
export interface InboxActions {
  answer: (child: string, text: string) => Promise<string | null>;
  extend: (child: string, iterations: number) => Promise<string | null>;
  deny: (child: string, reason: string) => Promise<string | null>;
  grant: (child: string, files: AgentFiles) => Promise<string | null>;
}

const FIELD = 'w-full resize-y border border-edge bg-input px-2.5 py-1 font-mono text-meta text-white placeholder:text-dialog-hint focus:border-accent focus:outline-none focus:ring-1 focus:ring-accent/30 disabled:text-muted sm:text-ui';
const LINK = 'rounded-none font-bold text-dialog-hint hover:text-white hover:underline focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent disabled:opacity-50';

/**
 * The open requests of a leader's agents, above its composer. One request has the
 * focus and its answers; the others wait in a short list. Later moves the focused
 * request to the end. In the inbox, Tab in an empty answer goes to the next request
 * and Esc moves the focused one to the end.
 */
export function AgentInbox({ agents, sid, pending, actions, onOpen }: {
  agents: readonly Subagent[];
  sid: string;
  pending: string | null;
  actions: InboxActions;
  onOpen: (sid: string) => void;
}) {
  const requests = useMemo(() => openRequests(agents, sid), [agents, sid]);
  const [later, setLater] = useState<string[]>([]);
  const [focus, setFocus] = useState<string | null>(null);
  const [seen, setSeen] = useState<ReadonlySet<string>>(() => new Set());
  const [folded, setFolded] = useState(false);
  const [drafts, setDrafts] = useState<Record<string, string>>({});
  const [denying, setDenying] = useState<Record<string, string>>({});
  const [errors, setErrors] = useState<Record<string, string>>({});
  const field = useRef<HTMLTextAreaElement | HTMLInputElement | null>(null);
  // Later requests keep the order in which the person moved them.
  const ordered = useMemo(() => {
    const rank = (key: string) => later.indexOf(key);
    return [...requests].sort((a, b) => rank(a.key) - rank(b.key));
  }, [requests, later]);
  const current = ordered.find((request) => request.key === focus) ?? ordered[0];
  // A request that the person had in focus is not new when it comes back.
  useEffect(() => {
    if (!current) return;
    const key = current.key;
    return () => setSeen((was) => (was.has(key) ? was : new Set([...was, key])));
  }, [current?.key]); // eslint-disable-line react-hooks/exhaustive-deps
  if (!current) return null;
  const position = ordered.indexOf(current);
  const agentCount = new Set(ordered.map((request) => request.agent.session_id)).size;
  const go = (step: number) => {
    const next = ordered[(position + step + ordered.length) % ordered.length];
    setFocus(next.key);
    window.setTimeout(() => field.current?.focus(), 0);
  };
  const putLater = () => {
    const key = current.key;
    setLater((was) => [...was.filter((known) => known !== key), key]);
    setFocus(ordered[(position + 1) % ordered.length]?.key ?? null);
  };
  const child = current.agent.session_id;
  const busy = pending === child;
  const run = async (call: Promise<string | null>, clear?: () => void) => {
    const error = await call;
    setErrors((was) => {
      const { [current.key]: _, ...rest } = was;
      return error ? { ...rest, [current.key]: error } : rest;
    });
    if (!error) clear?.();
  };
  const onKeyDown = (event: KeyboardEvent<HTMLElement>) => {
    if (event.key === 'Escape') {
      event.preventDefault();
      event.stopPropagation();
      putLater();
    } else if (event.key === 'Tab' && !event.shiftKey && ordered.length > 1) {
      const target = event.target as HTMLTextAreaElement | HTMLInputElement;
      if (target === field.current && !target.value) {
        event.preventDefault();
        go(1);
      }
    }
  };
  const draft = drafts[current.key] ?? '';
  const reason = denying[current.key];
  const iterations = current.agent.budget_request?.iterations ?? FALLBACK_ITERATIONS;
  const files = current.agent.files_request?.files;
  const error = errors[current.key];
  const send = () => {
    const text = draft.trim();
    if (text) void run(actions.answer(child, text), () => setDrafts(({ [current.key]: _, ...rest }) => rest));
  };
  const deny = () => {
    if (reason?.trim()) void run(actions.deny(child, reason.trim()), () => setDenying(({ [current.key]: _, ...rest }) => rest));
  };
  return (
    <section
      aria-label="Questions"
      onKeyDown={onKeyDown}
      className="mb-1.5 border border-dialog-edge bg-panel font-mono text-ui text-white"
      data-agent-inbox
    >
      <div className="flex items-center gap-2 px-3 py-1.5">
        <button
          type="button"
          aria-expanded={!folded}
          onClick={() => setFolded(!folded)}
          className="flex min-w-0 flex-1 items-center gap-2 rounded-none text-left font-bold focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-accent"
        >
          <ChevronIcon open={!folded} className={`size-3 shrink-0 ${MARK_NUDGE} text-dialog-hint`} />
          <CircleAlertIcon className={`size-3 shrink-0 ${MARK_NUDGE} text-warn`} />
          <span className="truncate">
            {ordered.length === 1 ? '1 question' : `${ordered.length} questions`}
            {agentCount > 1 && ` · ${agentCount} agents`}
          </span>
        </button>
        {ordered.length > 1 && (
          <span className="flex shrink-0 items-center gap-1 text-dialog-hint tabular-nums">
            <button type="button" aria-label="Previous question" className={LINK} onClick={() => go(-1)}>
              <ChevronIcon back className={`size-3 ${MARK_NUDGE}`} />
            </button>
            <span>{position + 1} of {ordered.length}</span>
            <button type="button" aria-label="Next question" className={LINK} onClick={() => go(1)}>
              <ChevronIcon className={`size-3 ${MARK_NUDGE}`} />
            </button>
          </span>
        )}
      </div>
      {!folded && (
        <div className="space-y-2 px-3 pb-2">
          <article aria-label={`A${current.number} ${current.agent.task}`} className="space-y-1.5 border-l-2 border-warn pl-2">
            <header className="flex min-w-0 items-center gap-2">
              <span className="shrink-0 font-bold">A{current.number}</span>
              {current.agent.role && <RoleIcon role={current.agent.role} className={`size-3 shrink-0 ${MARK_NUDGE} text-dialog-hint`} />}
              <span className="min-w-0 flex-1 truncate text-dialog-hint" title={current.agent.task}>{current.agent.task}</span>
              {!seen.has(current.key) && <span className="shrink-0 font-bold text-accent-ink">NEW</span>}
              <span className="shrink-0 text-dialog-hint tabular-nums">
                {current.agent.iterations_used}/{current.agent.iteration_budget}
              </span>
            </header>
            <p className="whitespace-pre-wrap break-words">{current.text}</p>
            {current.kind === 'files' && (
              <ul aria-label="Requested files" className="text-dialog-hint">
                {fileLines(files).map((line) => <li key={line} className="truncate" title={line}>{line}</li>)}
              </ul>
            )}
            {current.kind === 'question' && (
              <textarea
                ref={(node) => { field.current = node; }}
                aria-label={`Answer A${current.number}`}
                rows={2}
                maxLength={8192}
                disabled={busy}
                value={draft}
                placeholder="Write your answer"
                onChange={(event) => setDrafts((was) => ({ ...was, [current.key]: event.target.value }))}
                onKeyDown={(event) => {
                  if (event.key === 'Enter' && (event.metaKey || event.ctrlKey)) {
                    event.preventDefault();
                    send();
                  }
                }}
                className={FIELD}
              />
            )}
            {reason !== undefined && (
              <input
                ref={(node) => { field.current = node; }}
                aria-label="Reason"
                maxLength={2000}
                disabled={busy}
                value={reason}
                onChange={(event) => setDenying((was) => ({ ...was, [current.key]: event.target.value }))}
                onKeyDown={(event) => {
                  if (event.key === 'Enter') {
                    event.preventDefault();
                    deny();
                  }
                }}
                className={FIELD}
              />
            )}
            {error && <p role="alert" className="text-err-ink">{error}</p>}
            <div className="flex flex-wrap items-center gap-2">
              {current.kind === 'question' && (
                <Button density="inline" disabled={busy || !draft.trim()} onClick={send}>Send</Button>
              )}
              {current.kind === 'budget' && reason === undefined && (
                <Button density="inline" disabled={busy} onClick={() => void run(actions.extend(child, iterations))}>
                  Grant +{iterations} iterations
                </Button>
              )}
              {current.kind === 'files' && reason === undefined && (
                <Button density="inline" disabled={busy || !files} onClick={() => files && void run(actions.grant(child, files))}>
                  Grant files
                </Button>
              )}
              {(current.kind === 'budget' || current.kind === 'files') && (reason === undefined ? (
                <Button
                  density="inline"
                  variant="secondary"
                  disabled={busy}
                  onClick={() => {
                    setDenying((was) => ({ ...was, [current.key]: DENY_REASON }));
                    window.setTimeout(() => field.current?.focus(), 0);
                  }}
                >
                  Deny
                </Button>
              ) : <>
                <Button density="inline" variant="danger" disabled={busy || !reason.trim()} onClick={deny}>Send deny</Button>
                <Button
                  density="inline"
                  variant="secondary"
                  disabled={busy}
                  onClick={() => setDenying(({ [current.key]: _, ...rest }) => rest)}
                >
                  Keep request
                </Button>
              </>)}
              {current.kind === 'form' && (
                <Button density="inline" onClick={() => onOpen(child)}>Open agent</Button>
              )}
              {ordered.length > 1 && (
                <Button density="inline" variant="quiet" disabled={busy} onClick={putLater}>Later</Button>
              )}
            </div>
          </article>
          {ordered.length > 1 && (
            <ul aria-label="Other questions" className="space-y-0.5">
              {ordered.filter((request) => request !== current).map((request) => (
                <li key={request.key}>
                  <button
                    type="button"
                    onClick={() => setFocus(request.key)}
                    className="flex w-full min-w-0 items-center gap-2 rounded-none text-left text-dialog-hint hover:text-white focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-accent"
                  >
                    <span className="w-[3ch] shrink-0 font-bold text-white">A{request.number}</span>
                    <span className="flex w-3 shrink-0">
                      {request.agent.role && <RoleIcon role={request.agent.role} className={`size-3 ${MARK_NUDGE}`} />}
                    </span>
                    <span className="min-w-0 flex-1 truncate">{request.text}</span>
                    {!seen.has(request.key) && <span className="shrink-0 font-bold text-accent-ink">NEW</span>}
                  </button>
                </li>
              ))}
            </ul>
          )}
        </div>
      )}
    </section>
  );
}
