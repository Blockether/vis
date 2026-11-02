// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';
import { act, fireEvent, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';

import { GatewayClient } from '../lib/gateway';
import type { RouterProvider } from '../lib/types';
import { renderSessionScreen, subscriptionHub } from './session-screen-harness';

function routerProvider(id: string, overrides: Partial<RouterProvider> = {}): RouterProvider {
  return {
    id,
    is_managed: false,
    label: id,
    models: ['gpt-6-astra'],
    is_default: true,
    default_model: 'gpt-6-astra',
    is_fallback: false,
    fallback_model: null,
    model_details: [
      {
        name: 'gpt-6-astra',
        is_reasoning_effort_configurable: true,
        verbosity_style: 'openai-text',
      },
    ],
    ...overrides,
  };
}

function turnSubmitter() {
  let turn = 0;
  return vi.fn(
    (_sid: string, _request: string, _options?: { extraBody?: Record<string, unknown> }) =>
      Promise.resolve({ turn_id: `turn-${++turn}`, status: 'running' }),
  );
}

const toggle = (id: string, label: string, value: string, choices: string[]) => ({
  id,
  label,
  type: 'enum',
  value,
  choices,
});

afterEach(() => vi.unstubAllGlobals());
describe('composer response controls', () => {

  // Regression: on iOS, the focused composer's native caret paints through the
  // model sheet when a new session opens its picker without dismissing focus.
  it('blurs the composer before opening the model picker', () => {
    renderSessionScreen({ client: { onProviderLimits: () => () => {} } });
    const composer = screen.getByRole('textbox', { name: 'Message Vis' });
    composer.focus();
    expect(document.activeElement).toBe(composer);

    // A phone tap on a button need not move DOM focus off the textarea.
    fireEvent.click(screen.getByRole('button', { name: 'Change provider and model' }));

    expect(screen.getByRole('dialog', { name: 'Model' })).toBeInTheDocument();
    expect(document.activeElement).not.toBe(composer);
  });

  it('orders reasoning, verbosity, then fast mode and cycles verbosity', async () => {
    const user = userEvent.setup();
    const reasoning = toggle('reasoning_level', 'Reasoning effort', 'balanced', [
      'low',
      'balanced',
      'deep',
    ]);
    const verbosity = toggle('verbosity', 'Verbosity', 'low', ['low', 'medium', 'high']);
    const fast = { id: 'codex_fast_mode', label: 'Fast mode', type: 'boolean', enabled: false };
    const setSetting = vi.fn((id: string) =>
      Promise.resolve(id === 'verbosity' ? { ...verbosity, value: 'medium' } : fast),
    );
    const fleet = [routerProvider('openai-codex')];
    renderSessionScreen({
      client: {
        cachedDefaultModel: () => ({ provider: 'openai-codex', model: 'gpt-6-astra' }),
        defaultModel: () => Promise.resolve({ provider: 'openai-codex', model: 'gpt-6-astra' }),
        cachedRouter: () => fleet,
        router: () => Promise.resolve(fleet),
        cachedSetting: (id: string) =>
          id === 'reasoning_level' ? reasoning : id === 'verbosity' ? verbosity : fast,
        setting: (id: string) =>
          Promise.resolve(
            id === 'reasoning_level' ? reasoning : id === 'verbosity' ? verbosity : fast,
          ),
        setSetting,
      },
    });

    const fastButton = await screen.findByRole('button', { name: /fast mode — off/i });
    const verbosityButton = await screen.findByRole('button', {
      name: /verbosity — low, tap for the next level/i,
    });
    const reasoningButton = screen.getByRole('button', { name: /reasoning effort — balanced/i });
    // Each chip LEADS WITH A MARK: `◇`, `≡` and `»` used to stand in for
    // one, in the body face, beside real icons.
    expect(reasoningButton).toHaveTextContent('balanced');
    expect(verbosityButton).toHaveTextContent('low');
    expect(fastButton).toHaveTextContent('standard');
    expect(
      reasoningButton.compareDocumentPosition(verbosityButton) & Node.DOCUMENT_POSITION_FOLLOWING,
    ).toBe(Node.DOCUMENT_POSITION_FOLLOWING);
    expect(
      verbosityButton.compareDocumentPosition(fastButton) & Node.DOCUMENT_POSITION_FOLLOWING,
    ).toBe(Node.DOCUMENT_POSITION_FOLLOWING);
    // The GPT model has no thinking display, so its cached toggle stays hidden.
    expect(screen.queryByRole('button', { name: /thinking summary/i })).not.toBeInTheDocument();
    await user.click(verbosityButton);

    expect(setSetting).toHaveBeenCalledWith('verbosity', 'cycle', undefined, { scope: 'session', target_id: 's1' });
    await waitFor(() =>
      expect(screen.getByRole('button', { name: /verbosity — medium/i })).toBeInTheDocument(),
    );
  });

  describe('thinking summary switch', () => {
    const thinking = { id: 'thinking_summary', label: 'Thinking summary', type: 'boolean', enabled: true };
    const claudeFleet = [
      routerProvider('anthropic', {
        models: ['claude-opus-5'],
        default_model: 'claude-opus-5',
        model_details: [
          {
            name: 'claude-opus-5',
            is_reasoning_effort_configurable: true,
            verbosity_style: null,
            thinking_display_style: 'anthropic-display',
          },
        ],
      }),
    ];

    function renderWithThinking(setSetting: unknown) {
      const reasoning = toggle('reasoning_level', 'Reasoning effort', 'balanced', ['low', 'balanced', 'deep']);
      const setting = (id: string) =>
        id === 'reasoning_level' ? reasoning : id === 'thinking_summary' ? thinking : null;
      const settingReads = vi.fn((id: string) => Promise.resolve(setting(id)));
      renderSessionScreen({
        client: {
          cachedDefaultModel: () => ({ provider: 'anthropic', model: 'claude-opus-5' }),
          defaultModel: () => Promise.resolve({ provider: 'anthropic', model: 'claude-opus-5' }),
          cachedRouter: () => claudeFleet,
          router: () => Promise.resolve(claudeFleet),
          cachedSetting: setting,
          setting: settingReads,
          setSetting,
        },
      });
      return settingReads;
    }

    it('shows only for Claude adaptive thinking and writes the exact next value', async () => {
      const user = userEvent.setup();
      const setSetting = vi.fn(() => Promise.resolve({ ...thinking, enabled: false }));
      renderWithThinking(setSetting);

      const thinkingButton = await screen.findByRole('button', { name: /thinking summary — on/i });
      const reasoningButton = screen.getByRole('button', { name: /reasoning effort — balanced/i });
      expect(thinkingButton).toHaveTextContent('summarized');
      expect(
        reasoningButton.compareDocumentPosition(thinkingButton) & Node.DOCUMENT_POSITION_FOLLOWING,
      ).toBe(Node.DOCUMENT_POSITION_FOLLOWING);
      await user.click(thinkingButton);

      expect(setSetting).toHaveBeenCalledWith('thinking_summary', 'value', false, {
        scope: 'session',
        target_id: 's1',
      });
      await waitFor(() =>
        expect(screen.getByRole('button', { name: /thinking summary — off/i })).toHaveTextContent('omitted'),
      );
    });

    // Regression, user report: switching summarized and omitted was very slow. The word
    // waited for the gateway's answer.
    it('shows the new state before the gateway answers, and a failed write puts it back', async () => {
      const user = userEvent.setup();
      let fail: (reason: Error) => void = () => {};
      const setSetting = vi.fn(
        () =>
          new Promise((_resolve, reject) => {
            fail = reject;
          }),
      );
      renderWithThinking(setSetting);

      await user.click(await screen.findByRole('button', { name: /thinking summary — on/i }));
      expect(screen.getByRole('button', { name: /thinking summary — off/i })).toHaveTextContent('omitted');

      await act(async () => fail(new Error('Gateway refused the setting')));
      expect(screen.getByRole('button', { name: /thinking summary — on/i })).toHaveTextContent('summarized');
      expect(await screen.findByText('Gateway refused the setting')).toBeInTheDocument();
    });

    // Regression, user report: switching summarized and omitted was very slow. Each tap
    // also read every response control again, and each answer painted the whole screen.
    it('reads no control again after its own switch write', async () => {
      const user = userEvent.setup();
      const off = { ...thinking, enabled: false };
      vi.stubGlobal(
        'fetch',
        vi.fn(async () => new Response(JSON.stringify(off), { headers: { 'Content-Type': 'application/json' } })),
      );
      const gateway = new GatewayClient({ url: 'http://gateway.example.com' });
      const settingReads = renderWithThinking(gateway.setSetting.bind(gateway));
      await user.click(await screen.findByRole('button', { name: /thinking summary — on/i }));
      await screen.findByRole('button', { name: /thinking summary — off/i });
      settingReads.mockClear();

      await user.click(screen.getByRole('button', { name: /thinking summary — off/i }));
      await act(async () => {});

      expect(settingReads).not.toHaveBeenCalled();
    });
  });

  // Simplified modes step with one tap. With them off, the chip lists the exact levels,
  // so a tap never jumps past the wanted one.
  describe('reasoning level list', () => {
    const ladder = ['none', 'minimal', 'low', 'medium', 'high', 'xhigh', 'max'];
    const exactModel = routerProvider('openai-codex', {
      model_details: [
        {
          name: 'gpt-6-astra',
          is_reasoning_effort_configurable: true,
          verbosity_style: null,
          reasoning_effort_options: ['low', 'medium', 'high', 'xhigh'],
        },
      ],
    });

    let settingReads = vi.fn((_id: string, _signal?: AbortSignal, _target?: unknown) =>
      Promise.resolve<unknown>(null),
    );

    function renderWithReasoning(simplifiedEnabled: boolean, setSetting: ReturnType<typeof vi.fn>) {
      const settings = new Map<string, unknown>([
        [
          'reasoning_level',
          toggle('reasoning_level', 'Reasoning effort', 'balanced', ['quick', 'balanced', 'deep']),
        ],
        ['reasoning_effort', toggle('reasoning_effort', 'Thinking level', 'max', ladder)],
        [
          'simplified_thinking_modes',
          {
            id: 'simplified_thinking_modes',
            label: 'Simplified thinking modes',
            type: 'boolean',
            enabled: simplifiedEnabled,
          },
        ],
      ]);
      const setting = (id: string) => settings.get(id) ?? null;
      settingReads = vi.fn((id: string, _signal?: AbortSignal, _target?: unknown) => Promise.resolve(setting(id)));
      const fleet = [exactModel];
      renderSessionScreen({
        client: {
          cachedDefaultModel: () => ({
            provider: 'openai-codex',
            model: 'gpt-6-astra',
          }),
          defaultModel: () => Promise.resolve({ provider: 'openai-codex', model: 'gpt-6-astra' }),
          cachedRouter: () => fleet,
          router: () => Promise.resolve(fleet),
          cachedSetting: setting,
          setting: settingReads,
          setSetting,
        },
      });
      return settings;
    }

    // Regression: Settings turned off simplified thinking modes, and the open session
    // kept its quick, balanced and deep cycle until it was reopened.
    it('lists the exact levels at once when Settings turns simplified thinking modes off', async () => {
      const settings = renderWithReasoning(true, vi.fn());
      await screen.findByRole('button', { name: 'Reasoning effort — balanced, tap for the next level' });

      const off = {
        id: 'simplified_thinking_modes',
        label: 'Simplified thinking modes',
        type: 'boolean',
        enabled: false,
      };
      settings.set('simplified_thinking_modes', off);
      vi.stubGlobal(
        'fetch',
        vi.fn(async () => new Response(JSON.stringify(off), { headers: { 'Content-Type': 'application/json' } })),
      );
      try {
        // The Settings dialog writes through its own client for the same machine.
        await act(() => new GatewayClient({ url: 'http://gateway.example.com' }).setSetting(off.id, 'toggle'));
      } finally {
        vi.unstubAllGlobals();
      }

      expect(await screen.findByRole('button', { name: 'Thinking level — xhigh, choose a level' })).toBeVisible();
    });

    it('reads simplified thinking modes from the gateway, not from the session', async () => {
      renderWithReasoning(true, vi.fn());
      await screen.findByRole('button', { name: 'Reasoning effort — balanced, tap for the next level' });
      const reads = settingReads.mock.calls.filter(([id]) => id === 'simplified_thinking_modes');
      expect(reads.length).toBeGreaterThan(0);
      for (const [, , target] of reads) expect(target).toBeUndefined();
      const levels = settingReads.mock.calls.filter(([id]) => id === 'reasoning_level');
      expect(levels[0]?.[2]).toEqual(expect.objectContaining({ scope: 'session' }));
    });
    it('steps to the next of quick, balanced and deep while simplified thinking modes are on', async () => {
      const user = userEvent.setup();
      const setSetting = vi.fn((id: string) =>
        Promise.resolve(toggle(id, 'Reasoning effort', 'deep', ['quick', 'balanced', 'deep'])),
      );
      renderWithReasoning(true, setSetting);

      await user.click(
        await screen.findByRole('button', {
          name: 'Reasoning effort — balanced, tap for the next level',
        }),
      );

      // One tap steps to the next level; no list opens.
      expect(screen.queryByRole('dialog', { name: 'Reasoning effort' })).toBeNull();
      expect(setSetting).toHaveBeenCalledWith('reasoning_level', 'cycle', undefined, {
        scope: 'session',
        target_id: 's1',
      });
      await waitFor(() =>
        expect(
          screen.getByRole('button', { name: /reasoning effort — deep/i }),
        ).toBeInTheDocument(),
      );
    });

    it('lists the exact levels of the model while simplified thinking modes are off', async () => {
      const user = userEvent.setup();
      const setSetting = vi.fn((id: string, _action: string, value: string) =>
        Promise.resolve(toggle(id, 'Thinking level', value, ladder)),
      );
      renderWithReasoning(false, setSetting);

      // The saved `max` is above every rung of this model: the next turn sends `xhigh`.
      await user.click(
        await screen.findByRole('button', {
          name: 'Thinking level — xhigh, choose a level',
        }),
      );
      const list = screen.getByRole('dialog', { name: 'Thinking level' });
      expect(
        within(list)
          .getAllByRole('button')
          .map((row) => row.textContent),
      ).toEqual(['low', 'medium', 'high', 'xhighcurrent']);
      await user.click(within(list).getByRole('button', { name: 'low' }));

      expect(setSetting).toHaveBeenCalledWith('reasoning_effort', 'value', 'low', {
        scope: 'session',
        target_id: 's1',
      });
      await waitFor(() =>
        expect(screen.getByRole('button', { name: /thinking level — low/i })).toBeInTheDocument(),
      );
    });
  });
  // Regression, reported session b30f87ac-f20e-4d7f-9fd2-416788d10527:
  // Fast mode was encoded as an OpenAI-only request field before routing finished.
  it('leaves Fast mode to the gateway submission snapshot', async () => {
    const user = userEvent.setup();
    const fast = { id: 'codex_fast_mode', label: 'Fast mode', type: 'boolean', enabled: true };
    const submitTurn = vi.fn(
      (
        _sid: string,
        _request: string,
        _options?: {
          extraBody?: Record<string, unknown>;
          turnFeatures?: Record<string, boolean>;
        },
      ) => Promise.resolve({ turn_id: 'turn-1', status: 'running' }),
    );
    const codex = { provider: 'openai-codex', model: 'gpt-5.6' };

    renderSessionScreen({
      client: {
        cachedSessionModel: () => codex,
        sessionModel: () => Promise.resolve(codex),
        cachedSetting: (id: string) => (id === 'codex_fast_mode' ? fast : null),
        setting: (id: string) => Promise.resolve(id === 'codex_fast_mode' ? fast : null),
        submitTurn,
      },
    });

    await user.type(screen.getByRole('textbox', { name: 'Message Vis' }), 'hello');
    await user.click(screen.getByRole('button', { name: 'Send message' }));

    await waitFor(() => expect(submitTurn).toHaveBeenCalled());
    const options = submitTurn.mock.calls[0]?.[2];
    expect(options?.extraBody).toBeUndefined();
    expect(options?.turnFeatures).toBeUndefined();
  });

  // Response chips edit the session; cached UI values must not override submission snapshots.
  it.each(['openai-codex', 'github-copilot', 'custom-responses'])(
    'shows chosen Astra verbosity for %s without resending it on immediate or queued turns',
    async (provider) => {
      const user = userEvent.setup();
      const pref = { provider, model: 'gpt-6-astra' };
      let verbosity = toggle('verbosity', 'Verbosity', 'low', ['low', 'medium', 'high']);
      const submitTurn = turnSubmitter();
      const fleet = [routerProvider(provider)];
      renderSessionScreen({
        client: {
          cachedSessionModel: () => pref,
          sessionModel: () => Promise.resolve(pref),
          cachedRouter: () => fleet,
          router: () => Promise.resolve(fleet),
          cachedSetting: (id: string) => (id === 'verbosity' ? verbosity : null),
          setting: (id: string) => Promise.resolve(id === 'verbosity' ? verbosity : null),
          setSetting: vi.fn(async () => {
            const choices = verbosity.choices;
            verbosity = {
              ...verbosity,
              value: choices[(choices.indexOf(verbosity.value) + 1) % choices.length]!,
            };
            return verbosity;
          }),
          submitTurn,
        },
      });
      for (const [index, level] of ['low', 'medium', 'high', 'low'].entries()) {
        const chip = await screen.findByRole('button', {
          name: `Verbosity — ${level}, tap for the next level`,
        });
        await user.type(screen.getByRole('textbox', { name: 'Message Vis' }), `answer ${index}`);
        await user.click(
          screen.getByRole('button', { name: index ? 'Queue message' : 'Send message' }),
        );
        await waitFor(() => expect(submitTurn).toHaveBeenCalledTimes(index + 1));
        expect(submitTurn.mock.calls[index]?.[2]?.extraBody).toBeUndefined();
        await user.click(chip);
      }
    },
  );

  it.each([null, undefined])(
    'hides verbosity and omits its request field when wire capability is %s, even on Codex',
    async (style) => {
      const user = userEvent.setup();
      const pref = { provider: 'openai-codex', model: 'gpt-6-astra' };
      const verbosity = toggle('verbosity', 'Verbosity', 'high', ['low', 'medium', 'high']);
      const fleet = [
        routerProvider(pref.provider, {
          model_details:
            style === undefined
              ? undefined
              : [
                  {
                    name: pref.model,
                    is_reasoning_effort_configurable: true,
                    verbosity_style: style,
                  },
                ],
        }),
      ];
      const submitTurn = turnSubmitter();
      renderSessionScreen({
        client: {
          cachedSessionModel: () => pref,
          sessionModel: () => Promise.resolve(pref),
          cachedRouter: () => fleet,
          router: () => Promise.resolve(fleet),
          cachedSetting: (id: string) => (id === 'verbosity' ? verbosity : null),
          setting: (id: string) => Promise.resolve(id === 'verbosity' ? verbosity : null),
          submitTurn,
        },
      });
      expect(screen.queryByRole('button', { name: /verbosity/i })).not.toBeInTheDocument();
      await user.type(screen.getByRole('textbox', { name: 'Message Vis' }), 'hello');
      await user.click(screen.getByRole('button', { name: 'Send message' }));
      await waitFor(() => expect(submitTurn).toHaveBeenCalledOnce());
      expect(submitTurn.mock.calls[0]?.[2]?.extraBody).toBeUndefined();
      expect(screen.queryByRole('button', { name: /verbosity/i })).not.toBeInTheDocument();
    },
  );

  it('uses the session model, not the global default, and follows model changes', async () => {
    const user = userEvent.setup();
    const hub = subscriptionHub();
    const verbosity = toggle('verbosity', 'Verbosity', 'high', ['low', 'medium', 'high']);
    const defaultPref = { provider: 'openai-codex', model: 'gpt-6-astra' };
    const sessionPref = { provider: 'github-copilot', model: 'claude-opus-5' };
    const fleet = [
      routerProvider(defaultPref.provider),
      routerProvider(sessionPref.provider, {
        is_default: false,
        models: ['gpt-6-astra', 'claude-opus-5'],
        model_details: [
          {
            name: 'gpt-6-astra',
            is_reasoning_effort_configurable: true,
            verbosity_style: 'openai-text',
          },
          { name: 'claude-opus-5', is_reasoning_effort_configurable: true, verbosity_style: null },
        ],
      }),
    ];
    const submitTurn = turnSubmitter();
    renderSessionScreen({
      client: {
        cachedSessionModel: () => sessionPref,
        sessionModel: () => Promise.resolve(sessionPref),
        noteSessionModel: (_sid: string, pref: unknown) => pref,
        cachedDefaultModel: () => defaultPref,
        defaultModel: () => Promise.resolve(defaultPref),
        cachedRouter: () => fleet,
        router: () => Promise.resolve(fleet),
        cachedSetting: (id: string) => (id === 'verbosity' ? verbosity : null),
        setting: (id: string) => Promise.resolve(id === 'verbosity' ? verbosity : null),
        submitTurn,
      },
      subscriptions: hub,
    });
    for (const [index, model] of ['claude-opus-5', 'gpt-6-astra', 'claude-opus-5'].entries()) {
      if (index) {
        act(() =>
          hub.emit({
            type: 'session.model_updated',
            provider: sessionPref.provider,
            model,
          } as never),
        );
      }
      const expected = model === 'gpt-6-astra' ? { text: { verbosity: 'high' } } : undefined;
      await waitFor(() =>
        expect(Boolean(screen.queryByRole('button', { name: /verbosity — high/i }))).toBe(
          Boolean(expected),
        ),
      );
      await user.type(screen.getByRole('textbox', { name: 'Message Vis' }), `answer ${index}`);
      await user.click(
        screen.getByRole('button', { name: index ? 'Queue message' : 'Send message' }),
      );
      await waitFor(() => expect(submitTurn).toHaveBeenCalledTimes(index + 1));
      expect(submitTurn.mock.calls[index]?.[2]?.extraBody).toBeUndefined();
    }
  });

  // Regression, session 75371ea8-06af-4853-9d13-1056672df5db: a failed
  // initial read left Astra's verbosity absent after the connection recovered.
  it.each([
    ['/v1/settings/verbosity', 'reconnect'],
    ['/v1/router', 'reconnect'],
    ['/v1/settings/verbosity', 'wake'],
    ['/v1/router', 'wake'],
    ['/v1/settings/verbosity', 'session switch'],
    ['/v1/router', 'session switch'],
  ])('recovers %s on %s through the real gateway client', async (failedPath, recovery) => {
    const user = userEvent.setup();
    const pref = { provider: 'github-copilot', model: 'gpt-6-astra' };
    const fleet = [routerProvider(pref.provider)];
    let verbosity = toggle('verbosity', 'Verbosity', 'low', ['low', 'medium', 'high']);
    let offline = true;
    const fetcher = vi.fn(async (url: string, init?: RequestInit) => {
      const path = new URL(url).pathname;
      if (offline && path.endsWith(failedPath)) throw new TypeError('Network unavailable');
      if (path.endsWith('/v1/router')) return Response.json({ providers: fleet });
      if (path.endsWith('/v1/settings') && init?.method === 'POST') {
        expect(JSON.parse(String(init.body))).toMatchObject({ id: 'verbosity', action: 'cycle' });
        verbosity = { ...verbosity, value: 'medium' };
      }
      return Response.json(
        path.endsWith('verbosity') || init?.method === 'POST' ? verbosity : null,
      );
    });
    vi.stubGlobal('fetch', fetcher);
    const gateway = new GatewayClient({
      url: `http://gateway.example.com/${recovery}${failedPath}`,
    });
    const submitTurn = turnSubmitter();
    let connectionState = false;
    const connectionListeners = new Set<(connected: boolean) => void>();
    const view = renderSessionScreen({
      client: {
        cachedSessionModel: () => pref,
        sessionModel: () => Promise.resolve(pref),
        cachedRouter: gateway.cachedRouter.bind(gateway),
        router: gateway.router.bind(gateway),
        defaultModel: gateway.defaultModel.bind(gateway),
        cachedSetting: gateway.cachedSetting.bind(gateway),
        setting: gateway.setting.bind(gateway),
        setSetting: gateway.setSetting.bind(gateway),
        submitTurn,
      },
      subscriptions: {
        subscribeConnection: (listener: (connected: boolean) => void) => {
          connectionListeners.add(listener);
          listener(connectionState);
          return () => connectionListeners.delete(listener);
        },
      },
    });
    await act(async () => {});
    expect(fetcher.mock.calls.some(([url]) => new URL(url).pathname.endsWith(failedPath))).toBe(true);
    expect(screen.queryByRole('button', { name: /verbosity/i })).not.toBeInTheDocument();
    offline = false;
    act(() => {
      if (recovery === 'reconnect') {
        connectionState = true;
        for (const listener of [...connectionListeners]) listener(true);
      } else if (recovery === 'session switch') {
        view.rerenderSession('s2');
      } else {
        window.dispatchEvent(new Event('online'));
      }
    });
    const chip = await screen.findByRole('button', { name: /verbosity — low/i });
    await user.click(chip);
    await screen.findByRole('button', { name: /verbosity — medium/i });
    // A later failed refresh must retain the recovered, chosen value.
    gateway.invalidateRouter();
    offline = true;
    const attempts = fetcher.mock.calls.filter(([url]) => new URL(url).pathname.endsWith(failedPath)).length;
    act(() => window.dispatchEvent(new Event('online')));
    await waitFor(() =>
      expect(fetcher.mock.calls.filter(([url]) => new URL(url).pathname.endsWith(failedPath)).length).toBeGreaterThan(
        attempts,
      ),
    );
    expect(screen.getByRole('button', { name: /verbosity — medium/i })).toBeInTheDocument();
    await user.type(screen.getByRole('textbox', { name: 'Message Vis' }), 'hello');
    await user.click(screen.getByRole('button', { name: 'Send message' }));
    await waitFor(() => expect(submitTurn).toHaveBeenCalledOnce());
    expect(submitTurn.mock.calls[0]?.[2]?.extraBody).toBeUndefined();
  });

  // Regression, session e2a68329-6a26-4cc7-a9f2-8b91a318a65d: each session open read every
  // response control twice, once at mount and again when the event socket first connected.
  it('reads the controls again only after a dropped connection comes back', async () => {
    const pref = { provider: 'github-copilot', model: 'gpt-6-astra' };
    const fleet = [routerProvider(pref.provider)];
    const verbosity = toggle('verbosity', 'Verbosity', 'low', ['low', 'medium', 'high']);
    const fetcher = vi.fn(async (url: string) => {
      const path = new URL(url).pathname;
      if (path.endsWith('/v1/router')) return Response.json({ providers: fleet });
      return Response.json(path.endsWith('verbosity') ? verbosity : null);
    });
    vi.stubGlobal('fetch', fetcher);
    const gateway = new GatewayClient({ url: 'http://gateway.example.com/first-connect' });
    const verbosityReads = () =>
      fetcher.mock.calls.filter(([url]) => new URL(url).pathname.endsWith('/v1/settings/verbosity')).length;
    const connectionListeners = new Set<(connected: boolean) => void>();
    const emit = (connected: boolean) =>
      act(() => {
        for (const listener of [...connectionListeners]) listener(connected);
      });
    renderSessionScreen({
      client: {
        cachedSessionModel: () => pref,
        sessionModel: () => Promise.resolve(pref),
        cachedRouter: gateway.cachedRouter.bind(gateway),
        router: gateway.router.bind(gateway),
        defaultModel: gateway.defaultModel.bind(gateway),
        cachedSetting: gateway.cachedSetting.bind(gateway),
        setting: gateway.setting.bind(gateway),
        setSetting: gateway.setSetting.bind(gateway),
        submitTurn: turnSubmitter(),
      },
      subscriptions: {
        subscribeConnection: (listener: (connected: boolean) => void) => {
          connectionListeners.add(listener);
          listener(false);
          return () => connectionListeners.delete(listener);
        },
      },
    });
    await screen.findByRole('button', { name: /verbosity — low/i });
    expect(verbosityReads()).toBe(1);
    emit(true);
    await act(async () => {});
    expect(verbosityReads()).toBe(1);
    emit(false);
    emit(true);
    await waitFor(() => expect(verbosityReads()).toBe(2));
  });

  // Regression, issue #311: a project `.vis/config.yml` overlay sets the session's own
  // default. The chip named the gateway default from `/v1/router` instead.
  it('names the default of the session project, not the gateway default', async () => {
    const own = { provider: 'openai-codex', model: 'gpt-6-luna' };
    const fleet = [
      routerProvider('anthropic-coding-plan', { default_model: 'claude-opus-5-5', models: ['claude-opus-5-5'] }),
      routerProvider('openai-codex', { is_default: false, models: ['gpt-6-astra', 'gpt-6-luna'] }),
    ];
    const fetcher = vi.fn(async (url: string) => {
      const path = new URL(url).pathname;
      if (path.endsWith('/v1/router')) return Response.json({ providers: fleet });
      if (path.endsWith('/v1/sessions/s1/model')) return Response.json({ model: null, default: own });
      return Response.json(null);
    });
    vi.stubGlobal('fetch', fetcher);
    const gateway = new GatewayClient({ url: 'http://gateway.example.com/session-default' });
    renderSessionScreen({
      client: {
        cachedSessionModel: () => null,
        sessionModel: () => Promise.resolve(null),
        cachedDefaultModel: gateway.cachedDefaultModel.bind(gateway),
        defaultModel: gateway.defaultModel.bind(gateway),
        cachedRouter: gateway.cachedRouter.bind(gateway),
        router: gateway.router.bind(gateway),
      },
    });
    const chip = await screen.findByRole('button', { name: 'Change provider and model' });
    await waitFor(() => expect(chip).toHaveAttribute('title', 'openai-codex/gpt-6-luna'));
    expect(chip).toHaveTextContent('gpt-6-luna');
    // A reopened session paints its own default on the first frame.
    expect(gateway.cachedDefaultModel('s1')).toEqual(own);
    // Another session keeps its own default.
    expect(gateway.cachedDefaultModel('s2')).not.toEqual(own);
  });
  it('enables verbosity when an uncached fleet arrives for the default model', async () => {
    let resolveRouter!: (fleet: RouterProvider[]) => void;
    const router = new Promise<RouterProvider[]>((resolve) => {
      resolveRouter = resolve;
    });
    const pref = { provider: 'github-copilot', model: 'gpt-6-astra' };
    const verbosity = toggle('verbosity', 'Verbosity', 'high', ['low', 'medium', 'high']);
    renderSessionScreen({
      client: {
        cachedDefaultModel: () => pref,
        defaultModel: () => Promise.resolve(pref),
        router: () => router,
        cachedSetting: (id: string) => (id === 'verbosity' ? verbosity : null),
        setting: (id: string) => Promise.resolve(id === 'verbosity' ? verbosity : null),
      },
    });
    expect(screen.queryByRole('button', { name: /verbosity/i })).not.toBeInTheDocument();
    await act(async () => {
      resolveRouter([routerProvider(pref.provider)]);
    });
    expect(await screen.findByRole('button', { name: /verbosity — high/i })).toBeInTheDocument();
  });
  it('does not let an initial model read overwrite a newer gateway model event', async () => {
    let resolveSessionModel!: (pref: { provider: string; model: string }) => void;
    const sessionModel = vi.fn(
      () =>
        new Promise<{ provider: string; model: string }>((resolve) => {
          resolveSessionModel = resolve;
        }),
    );
    const hub = subscriptionHub();
    const noteSessionModel = vi.fn((_sid: string, pref: unknown) => pref);
    const fast = { id: 'codex_fast_mode', label: 'Fast mode', type: 'boolean', enabled: true };
    const codex = { provider: 'openai-codex', model: 'gpt-5.6' };

    renderSessionScreen({
      client: {
        cachedSessionModel: () => codex,
        sessionModel,
        noteSessionModel,
        cachedSetting: (id: string) => (id === 'codex_fast_mode' ? fast : null),
        setting: (id: string) => Promise.resolve(id === 'codex_fast_mode' ? fast : null),
      },
      subscriptions: hub,
    });

    expect(await screen.findByRole('button', { name: /fast mode — on/i })).toBeInTheDocument();
    hub.emit({
      type: 'session.model_updated',
      provider: 'anthropic-coding-plan',
      model: 'claude-opus-5',
    } as never);
    await waitFor(() =>
      expect(screen.queryByRole('button', { name: /fast mode/i })).not.toBeInTheDocument(),
    );

    await act(async () => {
      resolveSessionModel(codex);
      await Promise.resolve();
    });
    expect(screen.queryByRole('button', { name: /fast mode/i })).not.toBeInTheDocument();
  });
});
