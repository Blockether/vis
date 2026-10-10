// @vitest-environment jsdom
import { describe, expect, it, vi } from 'vitest';
import { act, fireEvent, screen, waitFor } from '@testing-library/react';

import { renderSessionScreen, sessionFixture } from './session-screen-harness';
import activityFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity.json';
import { reduceRunningTurnEvent } from '../lib/running-turn';
import type { SseEvent, TranscriptTurn } from '../lib/types';

describe('turn header metadata', () => {
  it.each([false, true])('keeps the canonical position and datetime when live=%s', async (live) => {
    const createdAt = new Date(2026, 8, 16, 14, 35, 27).getTime();
    renderSessionScreen({
      session: sessionFixture({
        live,
        current_turn_id: live ? 'turn-42' : null,
        running_started_at: live ? createdAt : undefined,
      }),
      client: {
        transcript: async () => [
          {
            turn_id: 'turn-42',
            position: 42,
            created_at: createdAt,
            request: 'A paginated request',
            status: live ? 'running' : 'completed',
            iterations: [],
          },
        ],
      },
    });
    await screen.findByText('A paginated request');
    await waitFor(() => {
      const headers = [...document.querySelectorAll('article > div:first-child')];
      const stamps = headers.filter((header) => header.textContent?.includes('T42'));
      expect(stamps).toHaveLength(2);
      for (const header of stamps) {
        expect(header).toHaveTextContent('16/09/2026, 14:35:27 / T42');
      }
    });
  });

  it('uses canonical live metadata before the transcript row is available', async () => {
    const createdAt = new Date(2026, 8, 16, 14, 35, 27).getTime();
    renderSessionScreen({
      session: sessionFixture({
        live: true,
        current_turn_id: 'turn-42',
        running_request: 'An adopted request',
        running_position: 42,
        running_created_at: createdAt,
        running_started_at: createdAt + 10_000,
      }),
      client: { transcript: async () => [], turnTrace: async () => [] },
    });
    await screen.findByText('An adopted request');
    await waitFor(() => {
      const headers = [...document.querySelectorAll('article > div:first-child')];
      const stamps = headers.filter((header) => header.textContent?.includes('T42'));
      expect(stamps).toHaveLength(2);
      for (const header of stamps) {
        expect(header).toHaveTextContent('16/09/2026, 14:35:27 / T42');
      }
    });
  });
});

describe('Council transcript requests', () => {
  it.each([false, true])('keeps persisted provenance when live=%s', async (live) => {
    const council = {
      entry_id: 42,
      thread_id: 42,
      kind: 'coordination' as const,
      content: 'Actual peer request',
    };
    renderSessionScreen({
      session: sessionFixture({
        live,
        current_turn_id: live ? 'council-turn' : null,
        running_request: live ? council.content : undefined,
        running_request_kind: live ? 'council' : undefined,
        running_council: live ? council : undefined,
      }),
      client: {
        transcript: () => Promise.resolve([
          {
            turn_id: 'council-turn',
            request: council.content,
            request_kind: 'council',
            council,
            status: live ? 'running' : 'done',
            iterations: [],
            content: [],
          },
        ]),
      },
    });
    expect(await screen.findByText('Actual peer request')).toBeInTheDocument();
    await waitFor(() => {
      const heading = document.querySelector('article .text-you-role');
      expect(heading).toHaveTextContent('Council');
      expect(heading).toHaveTextContent('Coordination');
    });
  });
});

// A transcript row is durable content, not a liveness lease. If the canonical
// session read fails, the client must not invent a running turn from stale SQL.
describe('a running transcript row without canonical session state', () => {
  const runningRow = {
    turn_id: 't1',
    request: 'check the logs',
    status: 'running',
    created_at: Date.now(),
    iterations: [],
  };

  it('renders the row as history without creating live work', async () => {
    renderSessionScreen({
      client: {
        session: () => Promise.reject(new Error('network down')),
        transcript: () => Promise.resolve([runningRow]),
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
      },
    });

    expect(await screen.findByText('check the logs')).toBeInTheDocument();
    await waitFor(() => expect(document.querySelector('[data-live="true"]')).toBeNull());
  });

  // Regression, issue reported in session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25:
  // opening a turn already at iteration 420 replayed its journal from iteration 1,
  // so the live ticker visibly counted through old work before reaching the present.
  it("paints the replay's latest iteration before its older frames", async () => {
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    const never = new Promise<never>(() => {});

    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't-live',
        running_request: 'keep working',
      }),
      client: {
        cachedTranscript: () => [],
        transcript: () => never,
        turnTrace: () => never,
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
        subscribeSession: (_sid: string, on: (event: Record<string, unknown>) => void) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });

    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(2));
    const emit = (event: Record<string, unknown>) => {
      for (const listener of listeners) listener(event);
    };

    act(() => {
      emit({
        type: 'subscription.ready',
        session_id: 's1',
        current_turn_id: 't-live',
        is_live: true,
        latest_iteration: 420,
      });
      emit({
        type: 'turn.started',
        session_id: 's1',
        turn_id: 't-live',
        request: 'keep working',
        seq: 1,
      });
    });

    expect((await screen.findAllByText(/Vis is working \(iter 420\)/)).length).toBeGreaterThan(0);

    act(() => {
      emit({
        type: 'iteration.completed',
        session_id: 's1',
        turn_id: 't-live',
        iteration: 1,
        seq: 2,
      });
    });

    expect(screen.queryByText(/\(iter 1\)/)).toBeNull();
    expect(screen.getAllByText(/\(iter 420\)/).length).toBeGreaterThan(0);
  });

  it('keeps an already painted answer when its terminal arrived while away', async () => {
    const visible = {
      id: 'gateway-turn',
      request: 'current voice turn',
      answer: 'This answer was already visible.',
      iterations: [],
      startedAt: Date.now(),
      status: 'running' as const,
    };

    renderSessionScreen({
      client: {
        cachedRunningTurn: () => ({ turn: visible, seq: 42 }),
        cachedTranscript: () => [],
        // Keep the persisted handover pending for the duration of the assertion.
        transcript: () => new Promise(() => {}),
      },
      subscriptions: {
        hasEndedTurn: () => true,
      },
    });

    expect(await screen.findByText('This answer was already visible.')).toBeInTheDocument();
    expect(screen.queryByText(/Vis is waiting for an update/)).toBeNull();
  });

  it('shows a gateway-local transcript without replacing the running recording', async () => {
    const bytes = 'AAAAIGZ0eXBNNEEg';
    const fetchTurnAttachments = vi.fn().mockResolvedValue([
      {
        id: 'stored-audio',
        filename: 'memo.m4a',
        media_type: 'audio/mp4',
        base64: bytes,
        transcription: 'gateway local words',
      },
    ]);
    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 'audio-turn',
        running_request: 'listen',
      }),
      client: {
        cachedRunningTurn: () => ({
          turn: {
            id: 'audio-turn',
            request: 'listen',
            answer: '',
            iterations: [],
            startedAt: Date.now(),
            status: 'running',
            attachments: [
              {
                filename: 'memo.m4a',
                media_type: 'audio/mp4',
                base64: bytes,
              },
            ],
          },
          seq: 1,
        }),
        cachedTranscript: () => [],
        transcript: () => new Promise(() => {}),
        fetchTurnAttachments,
      },
    });

    await waitFor(() =>
      expect(fetchTurnAttachments).toHaveBeenCalledWith(
        's1',
        'audio-turn',
        expect.any(AbortSignal),
        true,
      ),
    );
    const transcript = await screen.findByText('TRANSCRIPTION');
    act(() => transcript.click());
    expect(screen.getByText(/gateway local words/)).toBeInTheDocument();
    expect(document.querySelectorAll('audio')).toHaveLength(1);
  });

  // Regression, same report: a submit paints an optimistic bubble with nothing
  // in it, and the hub still remembers the PREVIOUS turn's terminal frame. Seeding
  // that empty bubble as `completed` renders the assistant rail as a bare "Vis" —
  // no phase, no clock, no answer — for the whole turn.
  it('does not seed a bubble that never painted anything', async () => {
    const justSent = {
      request: 'run the tests',
      answer: '',
      iterations: [],
      startedAt: Date.now(),
      status: 'running' as const,
    };

    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't1',
        running_request: 'check the logs',
      }),
      client: {
        cachedRunningTurn: () => ({ turn: justSent, seq: 7 }),
        cachedTranscript: () => [],
        transcript: () => Promise.resolve([runningRow]),
      },
      subscriptions: {
        hasEndedTurn: () => true,
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
      },
    });

    // Canonical gateway state names the active turn; the empty optimistic bubble
    // cannot claim an older terminal frame.
    expect(await screen.findByText('check the logs')).toBeInTheDocument();
    expect((await screen.findAllByText(/Vis sent your message/)).length).toBeGreaterThan(0);
  });

  // Regression, session 3960d3aa-e090-41af-927d-f9ae2e24f200: the last
  // reasoning text survives a 30-minute tool pause, but is not its live phase.
  it('uses current tool progress rather than old reasoning for the live label', async () => {
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't-live',
        running_request: 'check benchmark',
      }),
      client: {
        cachedRunningTurn: () => ({
          turn: {
            id: 't-live',
            request: 'check benchmark',
            answer: '',
            status: 'running',
            startedAt: Date.now(),
            iterations: [{ position: 1, thinking: 'Earlier reasoning', forms: [] }],
          },
          seq: 7,
        }),
        cachedTranscript: () => [],
        transcript: () => Promise.resolve([]),
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
        subscribeSession: (_sid: string, on: (event: Record<string, unknown>) => void) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });

    expect((await screen.findAllByText(/Vis is working \(iter 1\)/)).length).toBeGreaterThan(0);
    expect(screen.queryByText(/Vis is thinking \(iter 1\)/)).toBeNull();
    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(1));
    act(() => {
      for (const listener of listeners)
        listener({
          type: 'block.started',
          session_id: 's1',
          turn_id: 't-live',
          block_id: 'code-1',
          iteration: 1,
          form_index: 0,
          code: 'start_task()',
          seq: 8,
        });
    });
    expect((await screen.findAllByText(/Vis is running code \(iter 1\)/)).length).toBeGreaterThan(0);

    act(() => {
      for (const listener of listeners)
        listener({
          type: 'turn.progress',
          session_id: 's1',
          turn_id: 't-live',
          iteration: 1,
          progress: 'tool',
          phrase: 'waiting up to 1825s for: sleep 1800',
          seq: 9,
        });
    });
    expect(
      (await screen.findAllByText(/Vis is waiting up to 1825s for: sleep 1800 \(iter 1\)/))
        .length,
    ).toBeGreaterThan(0);
  });

  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: with a CI watch on screen,
  // still read "Vis is thinking (iter 30)... 10m 1s" while the panel under it was
  // filling in and offering an Interrupt — a hang, in the one place the answer was.
  it('names the live panel instead of saying Vis is thinking', async () => {
    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't1',
        running_request: 'check the logs',
      }),
      client: {
        transcript: () =>
          Promise.resolve([
            {
              ...runningRow,
              iterations: [{ position: 1, thinking: 'weighing it up', forms: [] }],
            },
          ]),
        liveViews: () =>
          Promise.resolve([{ id: 'v1', title: 'CI · run 42', description: '', nodes: [] }]),
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
      },
    });

    const phases = await screen.findAllByText(/Vis is showing CI · run 42 — live \(iter 1\)/);
    expect(phases.length).toBeGreaterThan(0);
    expect(screen.queryByText(/Vis is thinking/)).toBeNull();

    const title = screen
      .getAllByText('CI · run 42')
      .find((candidate) => candidate.closest('section')) as HTMLElement;
    const panel = title.closest('section') as HTMLElement;
    const phase = phases[0];
    expect(panel.compareDocumentPosition(phase) & Node.DOCUMENT_POSITION_FOLLOWING).not.toBe(0);
  });

  // Protocol 8 deleted the whole anchoring problem these cases guarded (td-65cdf6:
  // one Activity claimed by two rows, an anchor reused across turns, an unanchored
  // copy stranded in a detached rail). A snapshot is a field of the form that
  // produced it, so it cannot be claimed twice, placed wrongly, or orphaned. What
  // is left to prove is that the frames carrying it land on the right form.
  // The frames land through the real subscription, the same way the screen sees
  // them in production.
  const withLiveBlock = async (
    frames: (emit: (event: Record<string, unknown>) => void) => void,
  ) => {
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    renderSessionScreen({
      client: {
        cachedRunningTurn: () => ({
          turn: {
            id: 'activity-turn',
            request: 'inspect the run',
            answer: '',
            status: 'running',
            startedAt: Date.now(),
            iterations: [
              {
                id: 'iteration-41',
                position: 41,
                forms: [{ block_id: 0, source: 'inspect_run()' }],
              },
            ],
          },
          seq: 42,
        }),
        transcript: () => Promise.resolve([]),
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
        subscribeSession: (_sid: string, on: (event: Record<string, unknown>) => void) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });
    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(1));
    act(() => {
      frames((event) => {
        for (const listener of listeners) listener(event);
      });
    });
  };

  it('puts a running Activity snapshot on the block that produced it', async () => {
    await withLiveBlock((emit) => {
      emit({
        type: 'block.activity',
        iteration: 41,
        form_index: 0,
        activity: activityFixture,
      });
    });

    fireEvent.click(await screen.findByRole('button', { name: /^Expand steps/ }));
    expect((await screen.findAllByRole('button', { name: 'Expand code' })).length).toBe(1);
  });

  it('replaces a running snapshot with the settled block.activity revision', async () => {
    await withLiveBlock((emit) => {
      emit({
        type: 'block.activity',
        iteration: 41,
        form_index: 0,
        activity: activityFixture,
      });
      emit({
        type: 'block.activity',
        iteration: 41,
        form_index: 0,
        activity: { ...activityFixture, state: 'succeeded' },
      });
      emit({
        type: 'block.output',
        iteration: 41,
        form_index: 0,
        code: 'inspect_run()',
        stdout: 'done\n',
        duration_ms: 1_200,
      });
    });

    await waitFor(() => expect(screen.queryByText(/RUNNING · SUITE/)).toBeNull());
  });
});

describe('the wait between a submit and the first token', () => {
  it('says the message is sent, then names the model it waits on', async () => {
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    const never = new Promise<never>(() => {});

    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't-live',
        running_request: 'measure the wait',
      }),
      client: {
        cachedTranscript: () => [],
        transcript: () => never,
        turnTrace: () => never,
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
        subscribeSession: (_sid: string, on: (event: Record<string, unknown>) => void) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });

    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(2));
    const emit = (event: Record<string, unknown>) => {
      for (const listener of listeners) listener(event);
    };

    act(() => {
      emit({
        type: 'subscription.ready',
        session_id: 's1',
        current_turn_id: 't-live',
        is_live: true,
      });
      emit({
        type: 'turn.started',
        session_id: 's1',
        turn_id: 't-live',
        request: 'measure the wait',
        seq: 1,
      });
    });

    // Nothing has come back yet, and the one thing this screen knows for certain
    // is that the message left.
    expect((await screen.findAllByText(/Vis sent your message/)).length).toBeGreaterThan(0);
    expect(screen.queryByText(/Vis is waiting for an update/)).toBeNull();

    act(() => {
      emit({
        type: 'turn.progress',
        session_id: 's1',
        turn_id: 't-live',
        progress: 'attachment-transcription',
        iteration: 1,
        seq: 2,
      });
    });

    expect(
      (await screen.findAllByText(/Vis is transcribing recordings \(up to 5 min\)/)).length,
    ).toBeGreaterThan(0);

    act(() => {
      emit({
        type: 'turn.progress',
        session_id: 's1',
        turn_id: 't-live',
        progress: 'provider-call',
        iteration: 1,
        reason: 'user-submit',
        model: 'claude-opus-5',
        seq: 3,
      });
    });

    expect((await screen.findAllByText(/Vis is calling claude-opus-5/)).length).toBeGreaterThan(0);
    expect(screen.queryByText(/Vis is transcribing recordings/)).toBeNull();
  });

  it('says how long the model is silent and when Svar acts, until output arrives', async () => {
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    const never = new Promise<never>(() => {});

    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't-live',
        running_request: 'measure the wait',
      }),
      client: {
        cachedTranscript: () => [],
        transcript: () => never,
        turnTrace: () => never,
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
        subscribeSession: (_sid: string, on: (event: Record<string, unknown>) => void) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });

    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(2));
    const emit = (event: Record<string, unknown>) => {
      for (const listener of listeners) listener(event);
    };

    act(() => {
      emit({
        type: 'subscription.ready',
        session_id: 's1',
        current_turn_id: 't-live',
        is_live: true,
      });
      emit({
        type: 'turn.started',
        session_id: 's1',
        turn_id: 't-live',
        request: 'measure the wait',
        seq: 1,
      });
      emit({
        type: 'turn.progress',
        session_id: 's1',
        turn_id: 't-live',
        progress: 'provider-wait',
        iteration: 1,
        model: 'claude-opus-5',
        silent_ms: 20_000,
        connection: 'alive',
        awaiting_output: true,
        deadline_in_ms: 180_500,
        deadline_action: 'retry',
        seq: 2,
      });
    });

    expect(
      (await screen.findAllByText(/Vis is waiting for claude-opus-5 \(iter 1\)/)).length,
    ).toBeGreaterThan(0);
    // The clocks move once a second; allow one tick on a slow runner.
    expect(
      await screen.findByText(
        /^No response for 2[01]s · connection alive · retry in (3m 0s|2m 59s)$/,
      ),
    ).toBeInTheDocument();

    act(() => {
      emit({
        type: 'content.block.delta',
        session_id: 's1',
        turn_id: 't-live',
        iteration: 1,
        block_id: 't-live:reasoning:1',
        field: 'text',
        text: 'Plan',
        cumulative: 'Plan',
        seq: 3,
      });
    });

    await waitFor(() => expect(screen.queryByText(/No response for/)).toBeNull());
    expect(screen.queryByText(/Vis is waiting for/)).toBeNull();
  });
});

// Regression: `form_index` is a number, and the reducer once read the form
// coordinate with a string-only helper. Every frame then saw no owner, and a second
// form carrying nothing but Activity was painted under the code block.
describe("a form frame's numeric form_index", () => {
  const frame = (event: Record<string, unknown>) => event as unknown as SseEvent;

  it('puts the running snapshot on the block that is already there', () => {
    const started = reduceRunningTurnEvent(
      reduceRunningTurnEvent(null, frame({ type: 'turn.started', turn_id: 't-block' })),
      frame({ type: 'block.started', iteration: 1, form_index: 0, code: 'grep()' }),
    );
    const turn = reduceRunningTurnEvent(
      started,
      frame({
        type: 'block.activity',
        iteration: 1,
        form_index: 0,
        activity: activityFixture,
      }),
    );

    const forms = turn?.iterations[0]?.forms ?? [];
    expect(forms).toHaveLength(1);
    expect(forms[0].code).toBe('grep()');
    expect(forms[0].activity?.state).toBe('running');
  });
});

// Reopening a large live turn must not fetch its hidden trace during adoption. In Compact
// mode the trace reads it when it nears the reader; no control counts the hidden steps.
describe('a windowed running turn', () => {
  it.each([false, true])('reads no hidden steps during adoption when live=%s', async (live) => {
    const turnTrace = vi.fn();
    renderSessionScreen({
      session: sessionFixture({ live, current_turn_id: live ? 't-large' : null }),
      client: {
        turnTrace,
        transcript: async () => [{
          turn_id: 't-large', status: live ? 'running' : 'done',
          request: 'A large request', iterations_offset: 99, iterations_total: 100,
          iterations: [{ id: 'i100', position: 100, assistant_prose: 'Latest visible progress' }],
        }],
      },
    });
    const progress = await screen.findByText('Latest visible progress');
    expect(progress).toBeVisible();
    expect(progress.closest('article')?.querySelector('[data-anchor="skip"]')).toBeInstanceOf(
      HTMLElement,
    );
    expect(screen.queryByRole('button', { name: /earlier step/ })).toBeNull();
    expect(turnTrace).not.toHaveBeenCalled();
  });
});

describe('a queued turn that starts before the finished row lands', () => {
  // The next turn (a queue drain, an automation, a Council wake) starts in the same
  // breath as the terminal frame. Its bubble used to replace the finished one at once,
  // so the finished turn left the transcript until its durable row was read, and the
  // reader at the end of the page was thrown up to the turn before it.
  it('keeps the finished turn on screen until its row replaces it', async () => {
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    let persisted: TranscriptTurn[] = [];
    const finished = {
      id: 't-done',
      request: 'the first question',
      answer: 'The first answer.',
      iterations: [],
      startedAt: Date.now(),
      status: 'running' as const,
    };

    renderSessionScreen({
      session: sessionFixture({ status: 'running', live: true, current_turn_id: 't-done' }),
      client: {
        cachedRunningTurn: () => ({ turn: finished, seq: 1 }),
        cachedTranscript: () => [],
        transcript: async () => persisted,
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
        subscribeSession: (_sid: string, on: (event: Record<string, unknown>) => void) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });

    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(1));
    expect(await screen.findByText('The first answer.')).toBeInTheDocument();
    act(() => {
      for (const listener of listeners) {
        listener({
          type: 'turn.completed',
          turn_id: 't-done',
          seq: 2,
          content: [{ id: 'answer', type: 'prose', markdown: 'The first answer.' }],
        });
        listener({
          type: 'turn.started',
          turn_id: 't-next',
          request: 'the queued question',
          seq: 3,
        });
      }
    });

    expect(await screen.findByText('the queued question')).toBeInTheDocument();
    expect(screen.getByText('The first answer.')).toBeInTheDocument();
    expect(screen.getByText('the first question')).toBeInTheDocument();
    expect(document.querySelector('[data-settling][data-turn-id="t-done"]')).not.toBeNull();

    persisted = [
      {
        turn_id: 't-done',
        position: 1,
        request: 'the first question',
        status: 'completed',
        created_at: Date.now(),
        completed_at: Date.now(),
        content: [{ id: 'answer', type: 'prose', markdown: 'The first answer.' }],
        iterations: [],
      } as TranscriptTurn,
    ];
    // The row replaces the retained copy: the finished turn is painted once.
    await waitFor(() => expect(document.querySelector('[data-settling]')).toBeNull(), {
      timeout: 3000,
    });
    expect(document.querySelectorAll('[data-turn-id="t-done"]')).toHaveLength(1);
    expect(screen.getAllByText('The first answer.')).toHaveLength(1);
    expect(screen.getByText('the queued question')).toBeInTheDocument();
  });
});
