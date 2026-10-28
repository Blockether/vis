import type { GatewayClient } from '../lib/gateway';
import type { RunningTurn } from '../lib/running-turn';
import type { AgentTimeline, Session, Subagent, TranscriptTurn } from '../lib/types';
import { STORY_COMPOSER_CLIENT, STORY_COMPOSER_SESSION } from './story-data';

/** Subagent fixtures in the shape of the agents contract: gateway rows and timelines. */
export const STORY_AGENT_LEADER = 'subagents-story';
export const STORY_AGENT_DONE_TURN = 'subagents-turn-1';
export const STORY_AGENT_WAKE_TURN = 'subagents-turn-2';
export const STORY_AGENT_LIVE_TURN = 'subagents-turn-3';

// Times count back from the load of the story, so a running row shows a short time.
const NOW = Date.now();
const MINUTE = 60_000;

const member = (team: string, id: string, minutesAgo: number, row: Partial<Subagent>): Subagent => ({
  session_id: id,
  parent_id: STORY_AGENT_LEADER,
  leader_id: STORY_AGENT_LEADER,
  team_id: team,
  task: 'Do one bounded part of the work.',
  status: 'running',
  iteration_budget: 120,
  iterations_used: 14,
  provider: 'anthropic',
  model: 'claude-opus-5',
  created_at: NOW - minutesAgo * MINUTE,
  resumed_at: null,
  ...row,
});

const ended = (row: Subagent, minutes: number): Subagent => ({ ...row, finished_at: (row.created_at ?? NOW) + minutes * MINUTE + 4_000 });

/**
 * The team of the running turn: one agent in each live state, and one that already passed.
 * The native tests agent asks its leader for iterations. Its row shows its newest note: only
 * the leader reads and answers the question.
 */
export const STORY_LIVE_TEAM: Subagent[] = [
  member(STORY_AGENT_LIVE_TURN, 'live-routes', 1.1, {
    role: 'research', title: 'Map the agents routes', task: 'List every gateway route that the subagent views read.',
    note: 'Found the list, cancel and timeline routes. Now I read the wait route.',
    activity: 'Search files agents route',
  }),
  member(STORY_AGENT_LIVE_TURN, 'live-dock', 1.05, {
    role: 'implement', task: 'Rebuild the dock above the composer.', iterations_used: 31,
    activity: 'Patched AgentDock.tsx',
  }),
  member(STORY_AGENT_LIVE_TURN, 'live-native', 1.0, {
    role: 'verify', title: 'Native tests', task: 'Run the native tests against the built image.', iterations_used: 120,
    note: 'The image builds. 12 of 40 native tests ran, and all of them pass.',
    question: 'I used my budget. Can I have 10 more iterations?', question_entry_id: 49,
    budget_request: { iterations: 10, reason: 'The native tests are slow' },
  }),
  member(STORY_AGENT_LIVE_TURN, 'live-store', 0.9, {
    role: 'implement', title: 'Sign in to the store', task: 'Upload the build to the store.', pending_input: true, pending_input_count: 1,
  }),
  member(STORY_AGENT_LIVE_TURN, 'live-links', 0.2, {
    role: 'audit', title: 'Docs links', task: 'Check every link in the docs.\nReport the dead links with their pages.',
    status: 'queued', iterations_used: 0,
  }),
  ended(member(STORY_AGENT_LIVE_TURN, 'live-contract', 0.95, {
    role: 'verify', title: 'Contract tests', status: 'completed', outcome: 'passed', result: 'All 85 SDK tests pass',
  }), 0.6),
];

/** A finished team: every end state, untitled agents, and a long title and result. */
export const STORY_DONE_TEAM: Subagent[] = [
  ended(member(STORY_AGENT_DONE_TURN, 'done-timeline', 30, {
    role: 'research', title: 'Read the timeline route', status: 'completed',
    result: 'The route reads turns, notes and Council messages, oldest first.',
  }), 2),
  ended(member(STORY_AGENT_DONE_TURN, 'done-unit', 29.9, {
    role: 'verify', title: 'Unit tests', status: 'completed', outcome: 'passed', result: '38 of 38 tests pass',
  }), 1.5),
  ended(member(STORY_AGENT_DONE_TURN, 'done-gateway', 29.8, {
    role: 'verify', status: 'completed', outcome: 'failed', result: '2 of 40 tests fail: the timeline of a grandchild gives 404',
  }), 4),
  ended(member(STORY_AGENT_DONE_TURN, 'done-fix', 29.7, {
    role: 'implement', title: 'Fix the 404', status: 'failed', result: 'Stopped on a merge conflict in state.clj',
  }), 3),
  ended(member(STORY_AGENT_DONE_TURN, 'done-old', 29.6, { role: 'research', status: 'cancelled' }), 0.5),
  ended(member(STORY_AGENT_DONE_TURN, 'done-audit', 29.5, {
    role: 'audit', title: 'Review the Companion', status: 'budget_limited', iterations_used: 120,
    result: 'Read 41 of 60 files before the budget ended',
  }), 12),
  ended(member(STORY_AGENT_DONE_TURN, 'done-long', 29.4, {
    role: 'implement', title: 'Rebuild every subagent view of the app', status: 'completed',
    result: 'Rebuilt the dock, the transcript block, the timeline sheet and the crumb, and removed every role icon and status mark from them',
  }), 62),
];

/** The timeline of one agent, from its own rows: start, a note, a message and its end. */
export function storyTimeline(agent: Subagent): AgentTimeline {
  const start = agent.created_at ?? NOW;
  const items: AgentTimeline['items'] = [
    { at: start, kind: 'started', text: agent.task },
    { at: start + 9_000, kind: 'note', text: 'Read the contract and the current views first.' },
  ];
  items.push({ at: start + 20_000, kind: 'note', text: agent.note ?? agent.activity ?? 'Changed the rows and ran the checks.' });
  if (agent.finished_at) {
    items.push({
      at: agent.finished_at,
      kind: 'finished',
      status: agent.status,
      ...(agent.outcome ? { outcome: agent.outcome } : {}),
      ...(agent.result ? { text: agent.result } : {}),
    });
  }
  return { session_id: agent.session_id, items };
}

const turn = (id: string, position: number, row: Partial<TranscriptTurn>): TranscriptTurn => ({
  turn_id: id,
  position,
  status: 'completed',
  created_at: NOW - (40 - position * 10) * MINUTE,
  iterations: [],
  model: 'claude-opus-5',
  provider: 'anthropic',
  duration_ms: 240_000,
  ...row,
});

/** The leader's transcript: a turn with its finished team, then a wake from one of its children. */
export const STORY_AGENT_TURNS: TranscriptTurn[] = [
  turn(STORY_AGENT_DONE_TURN, 1, {
    request: 'Review the subagent routes and run the checks in parallel.',
    content: [{ id: 'a1', type: 'prose', markdown: 'Six of seven agents finished. The gateway tests found one 404 for a grandchild timeline; the fix stopped on a merge conflict.' }],
  }),
  turn(STORY_AGENT_WAKE_TURN, 2, {
    request: 'Council notification #61. Read the attributed Council input.',
    request_kind: 'council',
    council: {
      entry_id: 61, thread_id: 58, kind: 'informational', author_session_id: 'done-fix',
      content: 'Fix the 404 stopped: merge conflict in state.clj.',
    },
    content: [{ id: 'a2', type: 'prose', markdown: 'I take the conflict over myself and start the next team.' }],
  }),
];

const session = (running: boolean): Session => ({
  ...STORY_COMPOSER_SESSION,
  id: STORY_AGENT_LEADER,
  title: 'Subagent views',
  turn_count: STORY_AGENT_TURNS.length + (running ? 1 : 0),
  ...(running ? { status: 'running' as const, live: true, current_turn_id: STORY_AGENT_LIVE_TURN } : {}),
});

const runningTurn: RunningTurn = {
  id: STORY_AGENT_LIVE_TURN,
  request: 'Ship the new dock and check it in every theme.',
  answer: '',
  iterations: [],
  startedAt: NOW - 1.2 * MINUTE,
  status: 'running',
};

/** A leader session over the agents contract: running with a live team, or idle after it. */
export function storyAgentClient(running: boolean): GatewayClient {
  const agents = running ? [...STORY_DONE_TEAM, ...STORY_LIVE_TEAM] : STORY_DONE_TEAM;
  const answers: Record<string, unknown> = {
    cachedSession: () => session(running),
    session: async () => session(running),
    cachedTranscript: () => STORY_AGENT_TURNS,
    transcript: async () => STORY_AGENT_TURNS,
    transcriptWindow: () => ({ offset: 0, total: STORY_AGENT_TURNS.length }),
    cachedRunningTurn: () => (running ? { turn: runningTurn, seq: 1 } : null),
    agents: async () => agents,
    agentTimeline: async (_sid: string, child: string) => {
      const agent = agents.find((row) => row.session_id === child);
      return agent ? storyTimeline(agent) : { session_id: child, items: [] };
    },
    cancelAgent: async () => undefined,
  };
  return new Proxy(answers, {
    get(target, key) {
      if (typeof key === 'string' && key in target) return target[key];
      return Reflect.get(STORY_COMPOSER_CLIENT, key);
    },
  }) as unknown as GatewayClient;
}
