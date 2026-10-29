import { describe, expect, it } from 'vitest';

import activityFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity.json';
import {
  providerWaitDetail,
  reduceRunningTurnEvent,
  type RunningTurn,
  type ProviderWait,
} from './running-turn';
import type { SseEvent } from './types';

const runningTurn = {
  id: 'gw-thinking',
  request: 'show your plan',
  answer: '',
  iterations: [],
  startedAt: Date.now(),
  status: 'running' as const,
};

function event(value: Record<string, unknown>): SseEvent {
  return value as unknown as SseEvent;
}

function reasoning(text: string): SseEvent {
  return event({
    type: 'content.block.delta',
    turn_id: 'gw-thinking',
    seq: 6,
    iteration: 0,
    block_id: 'gw-thinking:reasoning:0',
    field: 'text',
    text,
    offset: 0,
  });
}

function completed(thinking: string | null): SseEvent {
  return event({
    type: 'iteration.completed',
    turn_id: 'gw-thinking',
    seq: 7,
    iteration: 0,
    thinking,
  });
}

function thought(turn: ReturnType<typeof reduceRunningTurnEvent>): string | undefined {
  return turn?.iterations[0]?.thinking;
}

describe('Council request provenance', () => {
  it('preserves the actual entry and kind from the live event', () => {
    const council = { entry_id: 42, thread_id: 42, kind: 'coordination', content: 'Actual request' };
    const turn = reduceRunningTurnEvent(
      null,
      event({
        type: 'turn.started',
        turn_id: 'council-turn',
        request: council.content,
        request_kind: 'council',
        council,
      }),
    );
    expect(turn?.requestKind).toBe('council');
    expect(turn?.request).toBe(council.content);
    expect(turn?.council).toEqual(council);
    expect(reduceRunningTurnEvent(turn, event({ type: 'turn.completed' }))?.council).toEqual(council);
  });
});

// Shared-settlement contract, task td-a972c8: the Companion retained its raw streamed fragment when
// the shared settled boundary deliberately returned no reasoning, while the TUI
// removed it. A complete Markdown heading disappeared from the TUI instead.
describe('shared reasoning settlement', () => {
  it('keeps a complete heading supplied by the settled boundary', () => {
    const running = reduceRunningTurnEvent(runningTurn, reasoning('**Planning**'));
    expect(thought(running)).toBe('**Planning**');
    expect(thought(reduceRunningTurnEvent(running, completed('**Planning**')))).toBe(
      '**Planning**',
    );
  });

  it('removes partially streamed prose when the settled boundary rejects it', () => {
    const running = reduceRunningTurnEvent(runningTurn, reasoning('I should inspect the parser…'));
    expect(thought(running)).toBe('I should inspect the parser…');
    expect(thought(reduceRunningTurnEvent(running, completed(null)))).toBe('');
  });
});

describe('Activity ownership inside a running turn', () => {
  const started = () =>
    reduceRunningTurnEvent(
      reduceRunningTurnEvent(null, event({ type: 'turn.started', turn_id: 't-activity' })),
      event({
        type: 'block.started',
        iteration: 1,
        form_index: 0,
        scope: 'python',
        code: 'work()',
      }),
    );

  it('never invents a form for an orphan Activity snapshot', () => {
    const turn = reduceRunningTurnEvent(
      reduceRunningTurnEvent(null, event({ type: 'turn.started', turn_id: 't-orphan' })),
      event({
        type: 'block.activity',
        iteration: 1,
        form_index: 0,
        activity: activityFixture,
      }),
    );

    expect(turn?.iterations).toEqual([]);

    // The iteration exists but owns no such form: the frame must change nothing.
    const owner = started();
    expect(
      reduceRunningTurnEvent(
        owner,
        event({
          type: 'block.activity',
          iteration: 1,
          form_index: 7,
          activity: activityFixture,
        }),
      ),
    ).toBe(owner);
  });

  it('keeps ticker progress separate from the form-owned Activity snapshot', () => {
    const withActivity = reduceRunningTurnEvent(
      started(),
      event({
        type: 'block.activity',
        iteration: 1,
        form_index: 0,
        activity: activityFixture,
      }),
    );
    const withProgress = reduceRunningTurnEvent(
      withActivity,
      event({
        type: 'turn.progress',
        progress: 'provider-call',
        iteration: 1,
        model: 'model-a',
      }),
    );

    expect(withProgress?.progress).toMatchObject({
      kind: 'provider-call',
      model: 'model-a',
    });
    expect(withProgress?.iterations[0]?.forms?.[0]?.activity?.state).toBe('running');
    expect(withProgress).not.toHaveProperty('activity');
  });

  it('accepts settled Activity only from block.activity, never block.output', () => {
    const settledActivity = { ...activityFixture, state: 'succeeded' };
    const settled = reduceRunningTurnEvent(
      started(),
      event({
        type: 'block.activity',
        iteration: 1,
        form_index: 0,
        activity: settledActivity,
      }),
    );
    const afterOutput = reduceRunningTurnEvent(
      settled,
      event({
        type: 'block.output',
        iteration: 1,
        form_index: 0,
        code: 'work()',
        stdout: 'done\n',
        activity: activityFixture,
      }),
    );

    expect(afterOutput?.iterations[0]?.forms?.[0]?.activity?.state).toBe('succeeded');
  });

  it('ignores a form frame that has no owner coordinate', () => {
    const turn = reduceRunningTurnEvent(
      reduceRunningTurnEvent(null, event({ type: 'turn.started', turn_id: 't-ownerless' })),
      event({
        type: 'block.started',
        iteration: 1,
        scope: 'python',
        code: 'work()',
      }),
    );

    expect(turn?.iterations).toEqual([]);
  });
});

describe('canonical turn header metadata', () => {
  const createdAt = 1_789_545_327_000;

  it('learns metadata from a progress frame after the durable turn is allocated', () => {
    const updated = reduceRunningTurnEvent(
      runningTurn,
      event({
        type: 'turn.progress',
        turn_id: runningTurn.id,
        position: 42,
        created_at: createdAt,
        progress: 'thinking',
        iteration: 7,
      }),
    );
    expect(updated).toMatchObject({
      position: 42,
      createdAt,
      startedAt: runningTurn.startedAt,
      progress: { kind: 'thinking', iteration: 7 },
    });
    expect(reduceRunningTurnEvent(updated, reasoning('working'))).toMatchObject({
      position: 42,
      createdAt,
    });
  });

  it('does not take metadata from a different turn', () => {
    const updated = reduceRunningTurnEvent(
      runningTurn,
      event({
        type: 'turn.progress',
        turn_id: 'other-turn',
        position: 99,
        created_at: createdAt,
      }),
    );
    expect(updated?.position).toBeUndefined();
    expect(updated?.createdAt).toBeUndefined();
  });

  it('uses a known start-frame position without replacing the device elapsed clock', () => {
    const updated = reduceRunningTurnEvent(
      runningTurn,
      event({
        type: 'turn.started',
        turn_id: runningTurn.id,
        position: 42,
        created_at: createdAt,
        started_at: createdAt,
      }),
    );
    expect(updated).toMatchObject({ position: 42, createdAt, startedAt: runningTurn.startedAt });
  });
});

// Regression, reported from a phone: "when a new turn starts there is sometimes a
// flicker, as if the status went from sent to accepted by the gateway". The
// elapsed line under "Vis" counts `Date.now() - startedAt` on the DEVICE, and
// `turn.started` carries the GATEWAY host's `started_at`: adopting it moved the
// counter by the whole clock difference the instant the frame landed — forward by
// the skew, or back to 0ms on a phone running ahead.
describe('the turn this device started', () => {
  const optimistic = { ...runningTurn, id: '', startedAt: 1_000_000 };
  const startedFrame = (id: string, at: number) =>
    event({ type: 'turn.started', turn_id: id, request: 'show your plan', started_at: at });

  it('keeps its own clock when turn.started names it', () => {
    const started = reduceRunningTurnEvent(optimistic, startedFrame('gw-1', 1_009_000));

    expect(started?.startedAt).toBe(1_000_000);
    expect(started?.id).toBe('gw-1');
  });

  it("takes the gateway's stamp for a turn it is not painting yet", () => {
    expect(reduceRunningTurnEvent(null, startedFrame('gw-2', 1_700_000))?.startedAt).toBe(
      1_700_000,
    );
  });
});

// A queued message sent with `→` (PLAN-queue-send-now, Task 4): the gateway's stored
// `turn.input` frame names the step the message reached, and the bubble belongs to
// that step, before any of its reasoning.
describe('a queued message delivered into the running turn', () => {
  const input = (iteration: number, messages: unknown[]) =>
    event({ type: 'turn.input', turn_id: runningTurn.id, seq: 9, iteration, messages });
  const sent = { queued_turn_id: 'q-1', request: 'Also check the tests' };

  it('places the messages on the step the frame names, creating that step', () => {
    const turn = reduceRunningTurnEvent(runningTurn, input(2, [sent]));

    expect(turn?.iterations.map((iteration) => iteration.position)).toEqual([2]);
    expect(turn?.iterations[0]?.user_input).toEqual([sent]);
  });

  it('keeps the reasoning that step already streamed', () => {
    const thinking = reduceRunningTurnEvent(runningTurn, reasoning('plan'));
    const turn = reduceRunningTurnEvent(thinking, input(0, [sent]));

    expect(turn?.iterations[0]).toMatchObject({ thinking: 'plan', user_input: [sent] });
  });

  it('replaces the list on a replayed frame instead of doubling it', () => {
    const once = reduceRunningTurnEvent(runningTurn, input(1, [sent]));
    const twice = reduceRunningTurnEvent(once, input(1, [sent]));

    expect(twice?.iterations[0]?.user_input).toEqual([sent]);
  });

  it('ignores a frame without a readable message', () => {
    const turn = reduceRunningTurnEvent(runningTurn, input(1, [{ queued_turn_id: 'q-2' }]));

    expect(turn).toBe(runningTurn);
  });

  it('leaves a settled bubble alone', () => {
    const settled = { ...runningTurn, status: 'completed' as const };

    expect(reduceRunningTurnEvent(settled, input(1, [sent]))).toBe(settled);
  });
});

describe('the provider wait notice', () => {
  const notice = (extra: Record<string, unknown> = {}) =>
    event({
      type: 'turn.progress',
      turn_id: 'gw-thinking',
      progress: 'provider-wait',
      iteration: 2,
      model: 'model-a',
      silent_ms: 25_000,
      connection: 'alive',
      awaiting_output: true,
      deadline_in_ms: 215_000,
      deadline_action: 'retry',
      ...extra,
    });

  const wait: ProviderWait = {
    silentMs: 25_000,
    receivedAt: 1_000,
    awaitingOutput: true,
    connection: 'alive',
    deadline: { inMs: 215_000, action: 'retry' },
  };

  it('keeps the silence, the connection and the deadline of the attempt', () => {
    const before = Date.now();
    const turn = reduceRunningTurnEvent(runningTurn, notice());

    expect(turn?.progress).toMatchObject({ kind: 'provider-wait', iteration: 2, model: 'model-a' });
    expect(turn?.progress?.wait).toMatchObject({
      silentMs: 25_000,
      awaitingOutput: true,
      connection: 'alive',
      deadline: { inMs: 215_000, action: 'retry' },
    });
    expect(turn?.progress?.wait?.receivedAt).toBeGreaterThanOrEqual(before);
  });

  it('ends when the model sends output', () => {
    const waiting = reduceRunningTurnEvent(runningTurn, notice());

    expect(reduceRunningTurnEvent(waiting, reasoning('Plan'))?.progress).toBeUndefined();
  });

  it('moves both clocks on from the moment the notice arrived', () => {
    expect(providerWaitDetail(wait, 1_000)).toBe(
      'No response for 25s · connection alive · retry in 3m 35s',
    );
    expect(providerWaitDetail(wait, 41_000)).toBe(
      'No response for 1m 5s · connection alive · retry in 2m 55s',
    );
  });

});

// A Python block can run for minutes without a tool call. The ticker said only
// "running", so a busy block looked like a hung session.
describe('the busy Python notice', () => {
  const notice = (extra: Record<string, unknown> = {}) =>
    event({
      type: 'turn.progress',
      turn_id: 'gw-thinking',
      progress: 'python',
      iteration: 3,
      busy_ms: 72_000,
      deadline_in_ms: 228_000,
      cpu_percent: 99,
      ...extra,
    });

  it('keeps the busy time, the CPU use and the deadline of the block', () => {
    const before = Date.now();
    const turn = reduceRunningTurnEvent(runningTurn, notice());

    expect(turn?.progress).toMatchObject({ kind: 'python', iteration: 3 });
    expect(turn?.progress?.busy).toMatchObject({ busyMs: 72_000, cpuPercent: 99, deadlineInMs: 228_000 });
    expect(turn?.progress?.busy?.receivedAt).toBeGreaterThanOrEqual(before);
  });

  it('ends the notice when the block ends', () => {
    const turn = reduceRunningTurnEvent(
      runningTurn,
      notice({ busy_ms: undefined, deadline_in_ms: undefined, cpu_percent: undefined }),
    );

    expect(turn?.progress?.kind).toBe('python');
    expect(turn?.progress?.busy).toBeUndefined();
  });

});

describe('live block text', () => {
  const thinking = (offset: number, text: string) => event({ ...reasoning(text), offset });
  const fold = (frames: SseEvent[]) =>
    frames.reduce<RunningTurn | null>((turn, frame) => reduceRunningTurnEvent(turn, frame), runningTurn);

  it('appends each delta at its offset, and a repeated frame changes nothing', () => {
    const turn = fold([thinking(0, 'Read '), thinking(5, 'the file.'), thinking(5, 'the file.')]);
    expect(thought(turn)).toBe('Read the file.');
  });

  it('shows the newest text after a gap and rebuilds the block on replay', () => {
    const gap = fold([thinking(5, 'the file.')]);
    expect(thought(gap)).toBe('the file.');
    expect(gap?.streamBases?.['gw-thinking:reasoning:0']).toBe(5);

    const replayed = [thinking(0, 'Read '), thinking(5, 'the file.'), thinking(14, ' Then edit.')].reduce(
      (turn, frame) => reduceRunningTurnEvent(turn, frame),
      gap,
    );
    expect(thought(replayed)).toBe('Read the file. Then edit.');
  });
});
