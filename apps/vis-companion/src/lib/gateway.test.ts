import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

class MemoryStorage implements Storage {
  private readonly rows = new Map<string, string>();

  get length(): number {
    return this.rows.size;
  }

  clear(): void {
    this.rows.clear();
  }

  getItem(key: string): string | null {
    return this.rows.get(key) ?? null;
  }

  key(index: number): string | null {
    return Array.from(this.rows.keys())[index] ?? null;
  }

  removeItem(key: string): void {
    this.rows.delete(key);
  }

  setItem(key: string, value: string): void {
    this.rows.set(key, value);
  }
}

const storage = new MemoryStorage();
const conn = { url: 'http://gateway.example.com:7890' };
const sessions = [{ id: 'session-1', title: 'Cached session' }];

beforeEach(() => {
  storage.clear();
  vi.resetModules();
  vi.stubGlobal('localStorage', storage);
  vi.stubGlobal('window', globalThis);
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

// Regression: a cold-start client used to re-download the complete session list.
describe('GatewayClient session-list validators', () => {
  it('revalidates a persisted session snapshot with its head ETag after a cold start', async () => {
    const firstFetch = vi.fn().mockResolvedValue(
      new Response(
        JSON.stringify({ sessions, total: 1, has_more: false }),
        { headers: { ETag: '"sessions-v1"' } },
      ),
    );
    vi.stubGlobal('fetch', firstFetch);
    const first = await import('./gateway');

    await new first.GatewayClient(conn).listSessions();
    first.persistGatewayCaches();

    vi.resetModules();
    const secondFetch = vi.fn().mockImplementation((_url: string, init: RequestInit) => {
      expect(new Headers(init.headers).get('If-None-Match')).toBe('"sessions-v1"');
      return Promise.resolve(new Response(null, { status: 304 }));
    });
    vi.stubGlobal('fetch', secondFetch);
    const second = await import('./gateway');
    const client = new second.GatewayClient(conn);
    const cached = client.cachedSessions();

    await expect(client.listSessions()).resolves.toBe(cached);
    expect(secondFetch).toHaveBeenCalledOnce();
  });
});

// The one fact about its own list the gateway cannot know is which sessions are
// holding words typed on THIS device. It is SENT — and a window whose ETag was
// issued while the overlay said one thing must never answer a list asked for
// with another, or the row just typed into stays missing until something else
// moves.
describe('GatewayClient session-list overlay', () => {
  it('sends the sessions holding unsent words, and re-asks when they change', async () => {
    const answered = vi.fn().mockImplementation(() =>
      Promise.resolve(
        new Response(JSON.stringify({ sessions, total: 1, has_more: false }), {
          headers: { ETag: '"sessions-v1"' },
        }),
      ),
    );
    vi.stubGlobal('fetch', answered);
    const mod = await import('./gateway');
    const drafts = await import('./draft-messages');
    const client = new mod.GatewayClient(conn);

    await client.listSessions();
    expect(String(answered.mock.calls[0][0])).not.toContain('dirty=');

    drafts.writeDraftMessage(drafts.draftMessageKey(client.base, 'session-2'), {
      text: 'half a thought',
    });
    await client.listSessions();

    expect(String(answered.mock.calls[1][0])).toContain('dirty=session-2');
    const init = answered.mock.calls[1][1] as RequestInit;
    expect(new Headers(init.headers).get('If-None-Match')).toBe(null);
  });
});

// Regression, user report (paraphrased: "opening the session list fires request after
// request for every page, over and over, as though the totals kept accumulating"): the
// list DRAINED the machine — every window below the head, on every poll — and then re-cut
// what came back into a page of ten. Measured against a 1192-session machine: 12 serial
// requests every ten seconds, eleven of them reporting that nothing had happened.
describe('GatewayClient session list', () => {
  const fleet = (count: number) =>
    Array.from({ length: count }, (_, index) => ({
      id: `session-${index}`,
      title: `Session ${index}`,
      // Freshest content first, which is the order the gateway ranks them in.
      modified_at: 1_000_000 - index,
    }));

  type Row = { id: string; title: string; modified_at: number };

  /**
   * The head window `url` asks for, validated as the gateway validates it: the ETag
   * covers the rows and the total, so a rename is a 200 and an untouched head is a 304.
   */
  const gateway = (rows: () => Row[]) =>
    vi.fn((url: string, init?: RequestInit) => {
      const limit = Number(
        new URL(url, 'http://gateway.example.com').searchParams.get('limit') ?? 100,
      );
      const page = rows().slice(0, limit);
      const etag = `"${rows().length}-${page.map((row) => `${row.id}@${row.title}`).join('|')}"`;
      if (new Headers(init?.headers).get('If-None-Match') === etag)
        return Promise.resolve(new Response(null, { status: 304, headers: { ETag: etag } }));
      return Promise.resolve(
        new Response(
          JSON.stringify({
            sessions: page,
            total: rows().length,
            has_more: page.length < rows().length,
            next_cursor: page.length ? `${page[page.length - 1]!.modified_at}:${page[page.length - 1]!.id}` : null,
          }),
          { headers: { ETag: etag } },
        ),
      );
    });

  it('reads ONE window however deep the machine is', async () => {
    const rows = fleet(1192);
    const fetched = gateway(() => rows);
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');

    const list = await new GatewayClient(conn).listSessions();

    // One request, and never an `after=`: what is below the head is answered BESIDE
    // the window (`total`, `overview`, `awaiting`) or by the project's own page.
    expect(fetched).toHaveBeenCalledTimes(1);
    expect(String(fetched.mock.calls[0]![0])).not.toContain('after=');
    expect(list).toHaveLength(100);
    expect(list[0]?.id).toBe('session-0');
  });

  it('hands back the SAME array when the head did not move', async () => {
    const rows = fleet(1192);
    const fetched = gateway(() => rows);
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);

    const first = await client.listSessions();
    const second = await client.listSessions();

    // A 304 is answered from the pin, so the rows are the very objects already on
    // screen — React bails out of the whole list instead of re-rendering it.
    expect(fetched).toHaveBeenCalledTimes(2);
    expect(second).toBe(first);
  });

  it('takes the window the gateway re-cut when a row moved', async () => {
    let rows = fleet(1192);
    const fetched = gateway(() => rows);
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);

    const first = await client.listSessions();
    // The oldest session finishes a turn and ranks to the very top. The head is a
    // WINDOW of the gateway's own ordering, so it comes back re-cut, not spliced here.
    rows = [{ ...rows[1191]!, modified_at: 2_000_000 }, ...rows.slice(0, 1191)];
    const second = await client.listSessions();

    expect(second).not.toBe(first);
    expect(second[0]?.id).toBe('session-1191');
    expect(second).toHaveLength(100);
    // Still one request per poll, and every row served once.
    expect(fetched).toHaveBeenCalledTimes(2);
    expect(new Set(second.map((row) => row.id)).size).toBe(second.length);
  });
});

// The transcript cache is a per-machine working set: recently opened sessions
// remain instant, while one busy gateway cannot evict another gateway's history.
describe('GatewayClient rolling transcript cache', () => {
  const transcriptResponse = (sid: string) =>
    new Response(
      JSON.stringify({
        turns: [{ id: `turn-${sid}`, status: 'completed' }],
        total: 1,
        offset: 0,
        has_more: false,
      }),
    );

  it('keeps the ten most recently used transcripts on each machine', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn((input: string) => {
        const match = new URL(String(input)).pathname.match(
          /\/v1\/sessions\/([^/]+)\/transcript/,
        );
        return Promise.resolve(transcriptResponse(decodeURIComponent(match?.[1] ?? 'missing')));
      }),
    );
    const { GatewayClient, persistGatewayCaches } = await import('./gateway');
    const first = new GatewayClient(conn);
    const second = new GatewayClient({ url: 'http://second.example.com:7890' });

    for (let index = 1; index <= 10; index += 1) {
      await first.transcript(`session-${index}`);
    }
    await second.transcript('other-session');
    expect(first.cachedTranscript('session-1')).not.toBeNull();

    await first.transcript('session-11');

    expect(first.cachedTranscript('session-2')).toBeNull();
    expect(first.cachedTranscript('session-1')).not.toBeNull();
    expect(first.cachedTranscript('session-11')).not.toBeNull();
    expect(second.cachedTranscript('other-session')).not.toBeNull();

    persistGatewayCaches();
    vi.resetModules();
    const cold = await import('./gateway');
    const coldFirst = new cold.GatewayClient(conn);
    const coldSecond = new cold.GatewayClient({ url: 'http://second.example.com:7890' });
    expect(coldFirst.cachedTranscript('session-2')).toBeNull();
    expect(coldFirst.cachedTranscript('session-1')).not.toBeNull();
    expect(coldFirst.cachedTranscript('session-11')).not.toBeNull();
    expect(coldSecond.cachedTranscript('other-session')).not.toBeNull();
  });

  it('prefetches active transcripts behind the session-list response', async () => {
    const rows = [
      {
        id: 'active-session',
        title: 'Active',
        live: true,
        turn_count: 1,
        modified_at: '2026-08-15T12:00:00Z',
      },
      {
        id: 'idle-session',
        title: 'Idle',
        live: false,
        turn_count: 1,
        modified_at: '2026-08-15T11:00:00Z',
      },
    ];
    let releaseTranscript!: (response: Response) => void;
    const pendingTranscript = new Promise<Response>((resolve) => {
      releaseTranscript = resolve;
    });
    const fetched = vi.fn((input: string) => {
      const path = new URL(String(input)).pathname;
      if (path.endsWith('/transcript')) return pendingTranscript;
      return Promise.resolve(
        new Response(JSON.stringify({ sessions: rows, total: rows.length, has_more: false })),
      );
    });
    vi.stubGlobal('fetch', fetched);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);

    await expect(client.listSessions()).resolves.toHaveLength(2);
    expect(client.cachedTranscript('active-session')).toBeNull();
    expect(
      fetched.mock.calls.some(([url]) => String(url).includes('/active-session/transcript')),
    ).toBe(true);
    expect(
      fetched.mock.calls.some(([url]) => String(url).includes('/idle-session/transcript')),
    ).toBe(false);

    releaseTranscript(
      new Response(
        JSON.stringify({
          turns: [{ id: 'turn-active-session', status: 'running' }],
          total: 1,
          offset: 0,
          has_more: false,
        }),
      ),
    );
    await vi.waitFor(() => expect(client.cachedTranscript('active-session')).not.toBeNull());
    await client.listSessions();
    await Promise.resolve();

    expect(
      fetched.mock.calls.filter(([url]) => String(url).includes('/active-session/transcript')),
    ).toHaveLength(1);
  });
});

// Regression: slash discovery used a gateway-global route, so the palette was resolved
// against the daemon's launch directory instead of the open session's nested project.
describe('GatewayClient session slash palette', () => {
  it('requests slash commands in the session scope', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(JSON.stringify({ commands: [{ name: '/impeccable init', doc: 'Initialize' }] })),
    );
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);

    await client.slashes('session-1');

    expect(fetchMock).toHaveBeenCalledOnce();
    expect(String(fetchMock.mock.calls[0]?.[0])).toContain(
      '/v1/sessions/session-1/slashes',
    );
  });
});

// Regression: the gateway scoped POST /v1/sessions/:sid/cancel-current to the
// idempotency_key its submitter sent, and the app sent none — so every Stop in the
// mobile app and the web answered 409 :not-owner and the turn kept running.
describe('GatewayClient turn cancellation', () => {
  it('cancels the current turn under the correlation id it submitted with', async () => {
    const fetchMock = vi
      .fn()
      .mockImplementation(() =>
        Promise.resolve(new Response(JSON.stringify({ turn_id: 'turn-1' }))),
      );
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);

    await client.submitTurn('session-1', 'hello');
    await client.cancelCurrentTurn('session-1');

    const submitBody = JSON.parse(String(fetchMock.mock.calls[0]?.[1]?.body));
    const cancelBody = JSON.parse(String(fetchMock.mock.calls[1]?.[1]?.body));
    expect(String(fetchMock.mock.calls[1]?.[0])).toContain(
      '/v1/sessions/session-1/cancel-current',
    );
    expect(submitBody.idempotency_key).toBeTruthy();
    expect(cancelBody.idempotency_key).toBe(submitBody.idempotency_key);
  });

  it('captures per-turn submission options', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(JSON.stringify({ turn_id: 'turn-fast' })),
    );
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');

    await new GatewayClient(conn).submitTurn('session-1', 'hello', {
      extraBody: { service_tier: 'priority' },
      turnFeatures: { voice_projection: true },
    });

    const body = JSON.parse(String(fetchMock.mock.calls[0]?.[1]?.body));
    expect(body.extra_body).toEqual({ service_tier: 'priority' });
    expect(body.turn_features).toEqual({ voice_projection: true });
  });

  it('cancels a known turn by id, which needs no correlation id', async () => {
    const fetchMock = vi.fn().mockResolvedValue(new Response(JSON.stringify({})));
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');

    await new GatewayClient(conn).cancelTurn('session-1', 'turn-1');

    expect(String(fetchMock.mock.calls[0]?.[0])).toContain(
      '/v1/sessions/session-1/turns/turn-1/cancel',
    );
  });
});

// Regression, user report: saving a new revision of an artifact reported the new
// version, but the artifacts sheet kept listing the old cut and the comments and
// highlights just written were nowhere on screen. A revision is appended to an
// iteration that ALREADY exists, so the session row never moves, the transcript
// revalidation is a no-op, and the turns the sheet is derived from stay stale.
describe('GatewayClient artifact revisions', () => {
  const held = () => [
    {
      id: 'turn-1',
      iterations: [
        {
          id: 'iteration-1',
          attachments: [
            {
              index: 0,
              iteration_id: 'iteration-1',
              filename: 'notes.md',
              media_type: 'text/markdown',
              version: 1,
            },
          ],
        },
      ],
    },
  ];

  const revisionFetch = () =>
    vi.fn().mockImplementation((input: unknown) =>
      Promise.resolve(
        String(input).includes('/attachments')
          ? new Response(
              JSON.stringify({
                index: 1,
                iteration_id: 'iteration-1',
                filename: 'notes.md',
                media_type: 'text/markdown',
                version: 2,
              }),
              { status: 201 },
            )
          : new Response(JSON.stringify({ turns: held() })),
      ),
    );

  it('folds a saved revision into the transcript it holds and hands it back', async () => {
    vi.stubGlobal('fetch', revisionFetch());
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);
    await client.transcript('session-1');

    const seen: unknown[] = [];
    const stop = client.onArtifactRevision('session-1', (turns) => seen.push(turns));
    const saved = await client.saveArtifactText(
      'session-1',
      'iteration-1',
      'notes.md',
      'text/markdown',
      '# annotated',
    );
    stop();

    expect(saved.version).toBe(2);
    expect(saved.iteration_id).toBe('iteration-1');
    const cached = client.cachedTranscript('session-1');
    expect(cached?.[0].iterations?.[0].attachments).toHaveLength(2);
    expect(cached?.[0].iterations?.[0].attachments?.[1].version).toBe(2);
    expect(seen).toEqual([cached]);
  });

  it('stops telling a screen that left', async () => {
    vi.stubGlobal('fetch', revisionFetch());
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);
    await client.transcript('session-1');

    const seen: unknown[] = [];
    client.onArtifactRevision('session-1', (turns) => seen.push(turns))();
    await client.saveArtifactText(
      'session-1',
      'iteration-1',
      'notes.md',
      'text/markdown',
      '# annotated',
    );

    expect(seen).toEqual([]);
  });
});

// Reported from a phone: the sessions list says a session has been answered, but
// opening it paints the transcript from before that answer and never corrects
// itself. The body a `transcript` read wrote was stamped with the session row
// the CACHE happened to hold at that moment — a row that can already describe a
// turn the body does not contain — so every later `transcriptIfMoved` answered
// "nothing moved" about a transcript that was missing the newest turn.
describe('GatewayClient transcript revalidation', () => {
  it('does not stamp a transcript page with a session row that outruns it', async () => {
    const answered = {
      id: 'session-1',
      title: 'Cached session',
      turn_count: 2,
      modified_at: '2026-08-14T03:00:02Z',
    };
    const stale = [{ id: 'turn-1', user_request: 'first', status: 'completed' }];
    const fresh = [
      ...stale,
      { id: 'turn-2', user_request: 'second', status: 'completed' },
    ];
    let turns = stale;
    vi.stubGlobal(
      'fetch',
      vi.fn().mockImplementation((url: string) =>
        Promise.resolve(
          new Response(
            JSON.stringify(
              String(url).includes('/transcript')
                ? { turns, total: turns.length, offset: 0, has_more: false }
                : answered,
            ),
          ),
        ),
      ),
    );
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);

    // The list already knows the session was answered; the transcript read that
    // follows still comes back one turn short.
    await client.session('session-1');
    await client.transcript('session-1');
    expect(client.cachedTranscript('session-1')).toHaveLength(1);

    // Re-opening the session must go and fetch, not trust that stamp.
    turns = fresh;
    expect(await client.transcriptIfMoved('session-1', answered)).toHaveLength(2);
  });
});

// A pocket voice IS a recording, so "create a voice" is an upload: the clip has to
// reach the gateway as ITSELF, and everything said about it rides in the query.
describe('GatewayClient imported voices', () => {
  it('posts the recording verbatim to the machine, not to a session', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(
        JSON.stringify({ voice: { id: 'my-own', label: 'My Own', is_imported: true } }),
        { status: 201 },
      ),
    );
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');
    const clip = new Blob([new Uint8Array([82, 73, 70, 70])], { type: 'audio/wav' });

    const voice = await new GatewayClient(conn).importSpeechVoice(
      clip,
      {
        name: 'My Own',
        lang: 'en-GB',
        text: 'what the clip says',
      },
      { engine: 'pocket-tts-local' },
    );

    const [url, init] = fetchMock.mock.calls[0] ?? [];
    const asked = new URL(String(url));
    expect(asked.pathname).toBe('/v1/speech/voices');
    expect(asked.searchParams.get('name')).toBe('My Own');
    expect(asked.searchParams.get('lang')).toBe('en-GB');
    expect(asked.searchParams.get('text')).toBe('what the clip says');
    expect(asked.searchParams.get('engine')).toBe('pocket-tts-local');
    // The BYTES travel, not a JSON envelope around them — and the clip keeps its own
    // media type, so nothing stamps `application/json` on a WAV.
    expect((init as RequestInit).body).toBe(clip);
    expect(new Headers((init as RequestInit).headers).get('Content-Type')).toBeNull();
    expect(voice.id).toBe('my-own');
  });

  it('lists and forgets voices on the machine-level route', async () => {
    const fetchMock = vi
      .fn()
      .mockImplementation(
        () =>
          new Response(JSON.stringify({ engine: { id: 'pocket-tts' }, voices: [] })),
      );
    vi.stubGlobal('fetch', fetchMock);
    const { GatewayClient } = await import('./gateway');
    const client = new GatewayClient(conn);

    await client.speechVoices({ engine: 'pocket-tts-local' });
    await client.forgetSpeechVoice('my own', { engine: 'pocket-tts-local' });

    const listed = new URL(String(fetchMock.mock.calls[0]?.[0]));
    const forgotten = new URL(String(fetchMock.mock.calls[1]?.[0]));
    expect(listed.pathname).toBe('/v1/speech/voices');
    expect(listed.searchParams.get('engine')).toBe('pocket-tts-local');
    expect(listed.pathname).not.toContain('/sessions/');
    expect(forgotten.pathname).toBe('/v1/speech/voices/my%20own');
    expect(forgotten.searchParams.get('engine')).toBe('pocket-tts-local');
    expect(fetchMock.mock.calls[1]?.[1]?.method).toBe('DELETE');
  });
});

// A reply spoken by the machine that answered it: the caller wants the AUDIO, and a
// long line's job is plumbing the client hides.
describe('GatewayClient speakText', () => {
  it('answers with the audio bytes a short line returns, never a text read', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      new Response(new Uint8Array([82, 73, 70, 70]), {
        headers: { 'Content-Type': 'audio/wav' },
      }),
    );
    vi.stubGlobal('fetch', fetchMock);
    const mod = await import('./gateway');

    const audio = await new mod.GatewayClient(conn).speakText(
      'session-1',
      'Say this out loud',
      { voice: 'kristin', engine: 'pocket-tts-local' },
    );

    expect(audio.size).toBe(4);
    const [url, init] = fetchMock.mock.calls[0] as [string, RequestInit];
    expect(url).toBe(
      'http://gateway.example.com:7890/v1/sessions/session-1/speech?engine=pocket-tts-local',
    );
    expect(init.method).toBe('POST');
    expect(JSON.parse(init.body as string)).toEqual({
      text: 'Say this out loud',
      voice: 'kristin',
    });
  });

  it('follows a 202 job to its audio and forgets the job afterwards', async () => {
    const seen: string[] = [];
    const fetchMock = vi.fn().mockImplementation((url: string, init: RequestInit) => {
      seen.push(`${init.method} ${url.replace('http://gateway.example.com:7890', '')}`);
      if (url.endsWith('/speech') && init.method === 'POST') {
        return Promise.resolve(
          new Response(JSON.stringify({ id: 'job-1', phase: 'queued', progress: 0, is_done: false }), {
            status: 202,
            headers: { 'Content-Type': 'application/json' },
          }),
        );
      }
      if (url.endsWith('/audio')) {
        return Promise.resolve(new Response(new Uint8Array([1, 2, 3])));
      }
      if (init.method === 'DELETE') return Promise.resolve(new Response(null, { status: 204 }));
      return Promise.resolve(
        new Response(JSON.stringify({ id: 'job-1', phase: 'spoken', progress: 1, is_done: true }), {
          headers: { 'Content-Type': 'application/json' },
        }),
      );
    });
    vi.stubGlobal('fetch', fetchMock);
    const mod = await import('./gateway');

    const audio = await new mod.GatewayClient(conn).speakText('session-1', 'A long answer');

    expect(audio.size).toBe(3);
    expect(seen).toEqual([
      'POST /v1/sessions/session-1/speech',
      'GET /v1/sessions/session-1/speech/jobs/job-1',
      'GET /v1/sessions/session-1/speech/jobs/job-1/audio',
      'DELETE /v1/sessions/session-1/speech/jobs/job-1',
    ]);
  });

  it('names what the machine refused instead of a bare status', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn().mockResolvedValue(
        new Response(JSON.stringify({ error: 'no speech engine is registered' }), {
          status: 501,
          headers: { 'Content-Type': 'application/json' },
        }),
      ),
    );
    const mod = await import('./gateway');

    await expect(
      new mod.GatewayClient(conn).speakText('session-1', 'Anything'),
    ).rejects.toThrow('no speech engine is registered');
  });
});

// Regression, user report: the model was installed and voice still failed, and the app
// only ever showed "HTTP 501" — the sentence the gateway wrote never reached the screen.
describe('GatewayClient voice engines', () => {
  it('asks the MACHINE about each direction, with no session in the path', async () => {
    const fetchMock = vi.fn().mockImplementation(() =>
      Promise.resolve(
        new Response(JSON.stringify({ status: 'ready', engine: 'parakeet-local' })),
      ),
    );
    vi.stubGlobal('fetch', fetchMock);
    const mod = await import('./gateway');
    const client = new mod.GatewayClient(conn);

    expect((await client.voiceModel({ engine: 'parakeet-local' })).status).toBe('ready');
    await client.speechModel({ start: true, engine: 'pocket-tts-local' });

    const calls = fetchMock.mock.calls.map((call: unknown[]) => {
      const init = call[1] as RequestInit | undefined;
      return `${init?.method ?? 'GET'} ${String(call[0])}`;
    });
    expect(calls).toEqual([
      'GET http://gateway.example.com:7890/v1/voice/model?engine=parakeet-local',
      'POST http://gateway.example.com:7890/v1/speech/model?engine=pocket-tts-local',
    ]);
    expect(calls.some((call: string) => call.includes('/sessions/'))).toBe(false);
  });

  it('carries the refusal the gateway wrote, and what failed to load with it', async () => {
    const reasons = ['com.blockether.vis.ext.foundation-voice: UnsatisfiedLinkError'];
    vi.stubGlobal(
      'fetch',
      vi.fn().mockResolvedValue(
        new Response(
          JSON.stringify({
            error: 'no voice transcription engine is registered - ' + reasons[0],
            reasons,
          }),
          { status: 501 },
        ),
      ),
    );
    const mod = await import('./gateway');

    await expect(new mod.GatewayClient(conn).voiceModel()).rejects.toMatchObject({
      status: 501,
      message: 'no voice transcription engine is registered - ' + reasons[0],
    });
  });
});

// Regression, user report (paraphrased: "it cannot fire four or five requests at
// every machine — it has to be one"): the same device list was fetched by the
// launch sweep, again by push registration asking whether that machine can sign
// for this device, again to answer the notifications row in advance, and again
// by the panel the moment it was opened.
describe('GatewayClient device list', () => {
  const listing = () => ({
    devices: [],
    push: { is_available: true, provider: 'apns', devices: 0 },
  });

  it('answers every caller of one machine from a single request', async () => {
    const asked = vi
      .fn()
      .mockImplementation(() =>
        Promise.resolve(new Response(JSON.stringify(listing()))),
      );
    vi.stubGlobal('fetch', asked);
    const mod = await import('./gateway');
    const client = new mod.GatewayClient(conn);

    // Two callers overlapping (the sweep and a panel opening on top of it),
    // then a third arriving after they settled, then push registration asking
    // the same machine whether it can sign at all.
    const [first, second] = await Promise.all([
      client.devices(),
      client.devices(),
    ]);
    const third = await client.devices();
    await new mod.GatewayClient(conn).pushTarget().status();

    expect(asked).toHaveBeenCalledOnce();
    expect(second).toBe(first);
    expect(third).toBe(first);
  });

  it('asks again once this device is taken off that machine', async () => {
    const asked = vi.fn().mockImplementation((_url: string, init?: RequestInit) =>
      Promise.resolve(
        new Response(
          JSON.stringify(
            init?.method === 'DELETE' ? { is_removed: true } : listing(),
          ),
        ),
      ),
    );
    vi.stubGlobal('fetch', asked);
    const mod = await import('./gateway');
    const client = new mod.GatewayClient(conn);

    await client.devices();
    await client.unregisterDevice('device-token');
    await client.devices();

    // The read, the delete, and the read that is no longer allowed to answer
    // from a list this app itself just changed.
    expect(asked).toHaveBeenCalledTimes(3);
  });
});
