// @vitest-environment jsdom
import { describe, expect, it } from 'vitest';
import { act, screen, waitFor } from '@testing-library/react';

import { renderSessionScreen, sessionFixture } from './session-screen-harness';
import { transcriptEnterClass } from '../components/ChatContent';

// Every switch between sessions mounts this screen anew, so its first frame is what
// the reader sees on each switch. Anything that fades in there, or corrects itself a
// frame later, reads as a flicker.

const never = new Promise<never>(() => {});

type Listener = (event: Record<string, unknown>) => void;

const connectedHub = {
  subscribeConnection: (on: (live: boolean) => void) => {
    on(true);
    return () => {};
  },
};

/** A connected hub whose session stream the test drives. */
function drivenHub() {
  const listeners = new Set<Listener>();
  return {
    listeners,
    hub: {
      ...connectedHub,
      subscribeSession: (_sid: string, on: Listener) => {
        listeners.add(on);
        return () => listeners.delete(on);
      },
    },
    emit: (event: Record<string, unknown>) => {
      for (const listener of listeners) listener(event);
    },
  };
}

async function liveBubble(): Promise<Element> {
  return waitFor(() => {
    const bubble = document.querySelector('[data-live="true"]');
    expect(bubble).toBeInstanceOf(HTMLDivElement);
    return bubble as Element;
  });
}

describe('a session screen opened by a switch', () => {
  it('paints its pane at once, without an entry fade', () => {
    renderSessionScreen({ subscriptions: connectedHub });
    const surface = document.querySelector('[data-session-surface]');
    expect(surface?.tagName).toBe('SECTION');
    expect(surface?.className).not.toMatch(/starting:|transition/);
  });

  it('never paints "Reconnecting" over a connected hub', () => {
    const changes: MutationRecord[] = [];
    const watch = new MutationObserver((records) => changes.push(...records));
    watch.observe(document.body, {
      subtree: true,
      childList: true,
      characterData: true,
      characterDataOldValue: true,
    });
    renderSessionScreen({ subscriptions: connectedHub });
    changes.push(...watch.takeRecords());
    watch.disconnect();
    const replaced = changes.flatMap((record) => [
      record.oldValue ?? '',
      ...Array.from(record.removedNodes, (node) => node.textContent ?? ''),
    ]);
    expect(replaced.join('\n')).not.toContain('Reconnecting');
    expect(screen.getByText('Connected')).toBeInTheDocument();
  });

  it('shows the running turn it re-enters with still', async () => {
    const minuteAgo = Date.now() - 60_000;
    renderSessionScreen({
      session: sessionFixture({
        status: 'running',
        live: true,
        current_turn_id: 't-live',
        running_request: 'keep working',
      }),
      client: {
        cachedRunningTurn: () => ({
          seq: 1,
          turn: {
            id: 't-live',
            request: 'keep working',
            answer: '',
            iterations: [],
            startedAt: minuteAgo,
            createdAt: minuteAgo,
            status: 'running',
          },
        }),
        transcript: () => never,
        turnTrace: () => never,
      },
      subscriptions: connectedHub,
    });
    expect((await liveBubble()).className).not.toContain(transcriptEnterClass);
  });

  it.each([
    ['still', 'a replayed turn that started before the screen opened', -60_000],
    ['with the live animation', 'a turn that starts while the reader watches', 0],
  ])('shows %s %s', async (_how, _what, age) => {
    const { listeners, hub, emit } = drivenHub();
    renderSessionScreen({
      client: { transcript: () => never, turnTrace: () => never },
      subscriptions: hub,
    });
    await waitFor(() => expect(listeners.size).toBeGreaterThan(0));
    act(() => {
      emit({
        type: 'turn.started',
        session_id: 's1',
        turn_id: 't-live',
        request: 'keep working',
        created_at: Date.now() + age,
        seq: 1,
      });
    });
    const classes = (await liveBubble()).className;
    if (age < 0) expect(classes).not.toContain(transcriptEnterClass);
    else expect(classes).toContain(transcriptEnterClass);
  });
});
