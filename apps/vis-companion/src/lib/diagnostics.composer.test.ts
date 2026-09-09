// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { act, createEvent, fireEvent, screen } from '@testing-library/react';

const appendFile = vi.hoisted(() => vi.fn(async (_options: { data: string }) => {}));
const awayListeners = vi.hoisted(() => new Set<() => void>());
vi.mock('@capacitor/filesystem', () => ({
  Directory: { LibraryNoCloud: 'LIBRARY_NO_CLOUD' },
  Encoding: { UTF8: 'utf8' },
  Filesystem: { mkdir: async () => {}, appendFile },
}));
vi.mock('./wake', () => ({
  onAway: (listener: () => void) => {
    awayListeners.add(listener);
    return () => { awayListeners.delete(listener); };
  },
  onWake: () => () => {},
}));

import { flushDiagnostics, watchComposerInputDiagnostics } from './diagnostics';
import { renderSessionScreen } from '../screens/session-screen-harness';

let textarea: HTMLTextAreaElement;
let now = 1_000;
let frameId = 0;
const frames = new Map<number, FrameRequestCallback>();
let stop = () => {};

beforeEach(() => {
  vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout'] });
  now = 1_000;
  frameId = 0;
  frames.clear();
  appendFile.mockClear();
  vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible');
  vi.spyOn(window.performance, 'now').mockImplementation(() => now);
  vi.spyOn(window, 'requestAnimationFrame').mockImplementation((callback) => {
    frames.set(++frameId, callback);
    return frameId;
  });
  vi.spyOn(window, 'cancelAnimationFrame').mockImplementation((id) => { frames.delete(id); });
  textarea = document.createElement('textarea');
  document.body.append(textarea);
});

afterEach(async () => {
  stop();
  stop = () => {};
  textarea.remove();
  await flushDiagnostics();
  vi.restoreAllMocks();
  vi.useRealTimers();
});

function watch() {
  stop = watchComposerInputDiagnostics(textarea, 'session-42');
}

function input(text: string, inputType = 'insertText', stamp = now, isComposing = false) {
  const event = createEvent.input(textarea, {
    target: { value: text }, inputType, data: text, isComposing,
  });
  Object.defineProperty(event, 'timeStamp', { value: stamp });
  fireEvent(textarea, event);
}

function paintAfter(ms: number) {
  now += ms;
  const pending = [...frames.values()];
  frames.clear();
  act(() => { for (const callback of pending) callback(now); });
}

async function advance(ms: number) {
  now += ms;
  act(() => { vi.advanceTimersByTime(ms); });
  await flushDiagnostics();
}

function records() {
  return appendFile.mock.calls.map(([call]) => JSON.parse(call.data))
    .filter((record) => record.scope === 'composer');
}

describe('composer typing diagnostics', () => {
  it('aggregates delays and corrections without recording typed or replacement text', async () => {
    watch();
    const privateText = 'Prywatna wiadomość z polskimi znakami';
    input(privateText, 'insertText', now - 140);
    input(privateText, 'insertReplacementText');
    input(privateText, 'insertCompositionText', now, true);
    input(privateText, privateText);
    input(privateText, 'deleteContentBackward');
    input(privateText, 'insertFromPaste');
    expect(appendFile).not.toHaveBeenCalled();
    paintAfter(180);
    expect(appendFile).not.toHaveBeenCalled();
    await advance(5_000);

    expect(records()).toHaveLength(1);
    expect(records()[0]).toMatchObject({
      level: 'warn', event: 'typing_summary',
      details: {
        session_id: 'session-42', reason: 'interval', input_count: 6,
        replacement_count: 1, composing_count: 1, deletion_count: 1, paste_count: 1,
        input_delay_sample_count: 6, max_input_delay_ms: 140, slow_input_count: 1,
        frame_count: 1, max_next_frame_ms: 180, slow_frame_count: 1,
        unmeasured_input_count: 0, max_draft_chars: privateText.length,
      },
    });
    expect(JSON.stringify(records())).not.toContain(privateText);
  });

  it('does not read the text value or force layout to measure input', async () => {
    watch();
    const text = 'Zażółć gęślą jaźń';
    const event = createEvent.input(textarea, { target: { value: text } });
    const forbidden = () => { throw new Error('Diagnostics must not read text or layout'); };
    for (const property of ['value', 'clientHeight', 'scrollHeight'] as const) {
      vi.spyOn(textarea, property, 'get').mockImplementation(forbidden);
    }
    vi.spyOn(textarea, 'getBoundingClientRect').mockImplementation(forbidden);
    fireEvent(textarea, event);
    paintAfter(16);
    await advance(5_000);
    expect(records()[0].details.max_draft_chars).toBe(text.length);
  });

  it('bounds writes and frame callbacks during a burst and stops work when idle', async () => {
    watch();
    for (let index = 0; index < 100; index += 1) input('a'.repeat(index + 1));
    expect(frames.size).toBe(1);
    expect(appendFile).not.toHaveBeenCalled();
    paintAfter(16);
    await advance(5_000);
    expect(records()).toHaveLength(1);
    expect(records()[0]).toMatchObject({
      level: 'info', details: { input_count: 100, frame_count: 1, max_next_frame_ms: 16 },
    });
    await advance(60_000);
    expect(records()).toHaveLength(1);
    expect(vi.getTimerCount()).toBe(0);
    expect(frames.size).toBe(0);
  });

  it.each(['blur', 'background', 'unmount'] as const)(
    'flushes on %s without counting the suspended frame as typing lag',
    async (reason) => {
      watch();
      input('tekst');
      if (reason === 'blur') fireEvent.blur(textarea);
      else if (reason === 'background') for (const away of awayListeners) away();
      else stop();
      expect(frames.size).toBe(0);
      await advance(60_000);
      paintAfter(16);
      expect(records()).toHaveLength(1);
      expect(records()[0]).toMatchObject({
        level: 'info', details: {
          reason, input_count: 1, frame_count: 0, max_next_frame_ms: 0,
          unmeasured_input_count: 1,
        },
      });
    },
  );

  it('normalizes epoch event timestamps and ignores missing or invalid clocks', async () => {
    watch();
    input('a', 'insertText', window.performance.timeOrigin + now - 45);
    input('b', 'insertText', 0);
    input('c', 'insertText', now + 1_000);
    input('d', 'insertText', Number.NaN);
    paintAfter(16);
    await advance(5_000);
    expect(records()[0].details).toMatchObject({
      input_count: 4, input_delay_sample_count: 1, max_input_delay_ms: 45,
    });
  });

  it('ignores hidden input and detaches all listeners on cleanup', async () => {
    watch();
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('hidden');
    input('ukryte');
    stop();
    expect(awayListeners.size).toBe(0);
    vi.spyOn(document, 'visibilityState', 'get').mockReturnValue('visible');
    input('po zamknięciu');
    await advance(5_000);
    expect(records()).toEqual([]);
    expect(frames.size).toBe(0);
  });

  it('records the real session composer and switches correlation on navigation', async () => {
    textarea.remove();
    const view = renderSessionScreen();
    textarea = screen.getByLabelText('Message Vis') as HTMLTextAreaElement;
    input('Wiadomość', 'insertReplacementText', now - 120);
    paintAfter(150);
    await advance(5_000);
    expect(records()[0]).toMatchObject({
      event: 'typing_summary', details: { session_id: 's1', replacement_count: 1 },
    });
    view.rerenderSession('s2');
    input('Następna wiadomość');
    paintAfter(16);
    await advance(5_000);
    expect(records().map((record) => record.details.session_id)).toEqual(['s1', 's2']);
    view.unmount();
  });
});
