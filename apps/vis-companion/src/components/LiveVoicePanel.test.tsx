// @vitest-environment jsdom
import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { LiveVoicePanel, LiveVoicePanelContent } from './LiveVoicePanel';
import type { LiveVoiceGateway } from '../lib/live-voice';

const media = vi.hoisted(() => ({
  reason: null as string | null,
  mobile: false,
  platform: 'web',
  factory: vi.fn(),
  close: vi.fn(),
  capture: vi.fn(),
}));
vi.mock('@capacitor/core', () => ({ Capacitor: { getPlatform: () => media.platform } }));
vi.mock('../lib/live-voice-audio', () => ({
  liveVoiceDeviceReason: () => media.reason,
  liveVoiceNeedsVerification: () => media.mobile,
  createLiveVoiceAudio: (allowed: boolean) => {
    media.factory(allowed);
    return {
      prepare: async () => undefined,
      capture: media.capture,
      stopCapture: vi.fn(),
      play: vi.fn(),
      stopOutput: vi.fn(),
      close: media.close,
    };
  },
}));
const state = {
  voice_session_id: 'v',
  capture_generation: 1,
  output_epoch: 1,
  capture: 'live',
  utterance: 'idle',
  agent: 'idle',
  output: 'idle',
};
class Socket {
  static last: Socket;
  readyState = 1;
  bufferedAmount = 0;
  binaryType = '';
  onmessage: ((event: { data: string }) => void) | null = null;
  onerror = null;
  onclose = null;
  onopen = null;
  send = vi.fn();
  close = vi.fn();
  constructor() {
    Socket.last = this;
  }
}
function gateway(enabled = true): LiveVoiceGateway {
  return {
    liveVoiceCapabilities: vi.fn(async () => ({ enabled, available: enabled })),
    startLiveVoice: vi.fn(async () => ({
      voice_session_id: 'v',
      client_id: 'c',
      media_path: '/media',
      ticket: 'ticket',
      state,
      limits: {},
      capabilities: { enabled: true, available: true },
    })),
    liveVoiceSnapshot: vi.fn(async () => ({ state, receipts: [] })),
    endLiveVoice: vi.fn(async () => undefined),
    liveVoiceMediaUrl: () => 'wss://gateway.example.com/media',
  };
}
beforeEach(() => {
  media.reason = null;
  media.mobile = false;
  media.platform = 'web';
  media.factory.mockReset();
  media.close.mockReset();
  media.capture.mockReset().mockResolvedValue(undefined);
  vi.stubGlobal('WebSocket', Socket);
});
afterEach(() => vi.unstubAllGlobals());

describe('hands-free opt-in panel', () => {
  it('keeps one live session when its controls move to an artifact overlay', async () => {
    const target = document.createElement('div');
    document.body.append(target);
    const client = gateway();
    const props = {
      client, sid: 's', disabled: false, startRequest: 1,
      onActive: vi.fn(), onBegin: vi.fn(),
    };
    const rendered = render(<LiveVoicePanel {...props} />);
    try {
      await waitFor(() => expect(client.startLiveVoice).toHaveBeenCalledTimes(1));
      rendered.rerender(<LiveVoicePanel {...props} controlsTarget={target} />);
      expect(within(target).getByRole('button', { name: 'End voice mode' })).toBeVisible();
      expect(client.endLiveVoice).not.toHaveBeenCalled();
      expect(media.close).not.toHaveBeenCalled();
      rendered.rerender(<LiveVoicePanel {...props} controlsTarget={null} />);
      expect(target).toBeEmptyDOMElement();
      expect(client.startLiveVoice).toHaveBeenCalledTimes(1);
      fireEvent.click(screen.getByRole('button', { name: 'End voice mode' }));
      await waitFor(() => expect(client.endLiveVoice).toHaveBeenCalledTimes(1));
    } finally {
      rendered.unmount();
      target.remove();
    }
  });

  it.each([
    ['running', 'playing', false, 'Speaking', 'Task running', 'Speaking'],
    [
      'waiting_for_input',
      'playing',
      false,
      'Waiting for you',
      'Use the session input controls.',
      'Speaking',
    ],
    ['queued', 'synthesizing', true, 'Microphone muted', 'Task queued', 'Preparing speech'],
    ['cancelling', 'muted', false, 'Listening', 'Cancellation requested', 'Speech muted'],
  ] as const)(
    'shows %s and %s independently of capture',
    (agent, output, muted, headline, taskLabel, outputLabel) => {
      const action = vi.fn();
      render(
        <LiveVoicePanelContent
          disabled={false}
          reason={null}
          view={{
            phase: 'live',
            muted,
            outputMuted: false,
            transcript: '',
            final: false,
            state: { ...state, agent, output },
            turnId: null,
            message: 'Choose a task with the task controls.',
            reconciled: false,
            receipts: [],
          }}
          onStart={action}
          onMute={action}
          onOutputMute={action}
          onStopSpeech={action}
          onCancelTask={action}
          onEnd={action}
        />,
      );
      // One headline: the state that the person must know first.
      for (const other of ['Speaking', 'Waiting for you', 'Microphone muted', 'Listening']) {
        expect(screen.queryAllByText(other, { selector: 'p' })).toHaveLength(other === headline ? 1 : 0);
      }
      expect(screen.getByRole('button', { name: 'Stop speech' })).toHaveProperty(
        'disabled',
        output === 'muted',
      );
      expect(screen.getByText(taskLabel)).toBeInTheDocument();
      expect(screen.getByText(outputLabel)).toBeInTheDocument();
      expect(screen.getByText('Choose a task with the task controls.')).toBeInTheDocument();
      expect(action).not.toHaveBeenCalled();
    },
  );
  it('does not infer pending input from cancellation message text', () => {
    const action = vi.fn();
    render(
      <LiveVoicePanelContent
        disabled={false}
        reason={null}
        view={{
          phase: 'live',
          muted: false,
          outputMuted: false,
          transcript: '',
          final: false,
          state,
          turnId: null,
          message: 'An approval was mentioned.',
          reconciled: false,
          receipts: [],
        }}
        onStart={action}
        onMute={action}
        onOutputMute={action}
        onStopSpeech={action}
        onCancelTask={action}
        onEnd={action}
      />,
    );
    expect(screen.queryByText(/Waiting for you/)).not.toBeInTheDocument();
    expect(action).not.toHaveBeenCalled();
  });
  it('stays hidden and does not request media before a microphone action', async () => {
    const client = gateway();
    const { container } = render(<LiveVoicePanel client={client} sid="s" disabled={false} onActive={vi.fn()} onBegin={vi.fn()} />);
    expect(container).toBeEmptyDOMElement();
    expect(client.liveVoiceCapabilities).not.toHaveBeenCalled();
    expect(media.factory).not.toHaveBeenCalled();
  });
  it('reports an unavailable gateway after an explicit request and allows retry', async () => {
    const client = gateway(false);
    render(<LiveVoicePanel client={client} sid="s" startRequest={1} disabled={false} onActive={vi.fn()} onBegin={vi.fn()} />);
    expect(await screen.findByText('Hands-free voice is unavailable on this gateway.')).toBeInTheDocument();
    expect(client.startLiveVoice).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Retry voice' }));
    await waitFor(() => expect(client.liveVoiceCapabilities).toHaveBeenCalledTimes(2));
  });
  it('starts iOS without a testing checkbox and keeps native audio enabled', async () => {
    media.mobile = true;
    media.platform = 'ios';
    const client = gateway();
    render(<LiveVoicePanel client={client} sid="s" startRequest={1} disabled={false} onActive={vi.fn()} onBegin={vi.fn()} />);
    await waitFor(() => expect(client.startLiveVoice).toHaveBeenCalledTimes(1));
    expect(media.factory).toHaveBeenCalledWith(true);
    expect(screen.queryByRole('checkbox')).not.toBeInTheDocument();
    expect(screen.queryByText(/pilot|Mobile test|requests admitted/)).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'End voice mode' }));
    await waitFor(() => expect(client.endLiveVoice).toHaveBeenCalled());
    await waitFor(() => expect(screen.queryByRole('region', { name: 'Hands-free voice' })).not.toBeInTheDocument());
  });
  it('retains Android verification after the microphone action', async () => {
    media.mobile = true;
    media.platform = 'android';
    const client = gateway();
    render(<LiveVoicePanel client={client} sid="s" startRequest={1} disabled={false} onActive={vi.fn()} onBegin={vi.fn()} />);
    expect(await screen.findByRole('button', { name: 'Start voice' })).toBeDisabled();
    fireEvent.click(screen.getByRole('checkbox', { name: 'Enable mobile audio testing' }));
    fireEvent.click(screen.getByRole('button', { name: 'Start voice' }));
    await waitFor(() => expect(client.startLiveVoice).toHaveBeenCalledTimes(1));
  });
  it('asks for Android verification again when voice starts after End', async () => {
    media.mobile = true;
    media.platform = 'android';
    const client = gateway();
    const props = { client, sid: 's', disabled: false, onActive: vi.fn(), onBegin: vi.fn() };
    const rendered = render(<LiveVoicePanel {...props} startRequest={1} />);
    fireEvent.click(await screen.findByRole('checkbox', { name: 'Enable mobile audio testing' }));
    fireEvent.click(screen.getByRole('button', { name: 'Start voice' }));
    await waitFor(() => expect(client.startLiveVoice).toHaveBeenCalledTimes(1));
    fireEvent.click(screen.getByRole('button', { name: 'End voice mode' }));
    await waitFor(() => expect(screen.queryByRole('region', { name: 'Hands-free voice' })).not.toBeInTheDocument());
    rendered.rerender(<LiveVoicePanel {...props} startRequest={2} />);
    fireEvent.click(await screen.findByRole('checkbox', { name: 'Enable mobile audio testing' }));
    fireEvent.click(screen.getByRole('button', { name: 'Start voice' }));
    await waitFor(() => expect(client.startLiveVoice).toHaveBeenCalledTimes(2));
  });
  it('reports unsupported media only after a start request', async () => {
    media.reason = 'Install an app build with native hands-free audio support.';
    const client = gateway();
    render(<LiveVoicePanel client={client} sid="s" startRequest={1} disabled={false} onActive={vi.fn()} onBegin={vi.fn()} />);
    expect(await screen.findByText(media.reason)).toBeInTheDocument();
    expect(client.startLiveVoice).not.toHaveBeenCalled();
  });
  it('offers separate mute, speech stop, task cancel and mode end actions', async () => {
    const client = gateway();
    const onActive = vi.fn();
    const props = { client, sid: 's', disabled: false, onActive, onBegin: vi.fn() };
    const rendered = render(<LiveVoicePanel {...props} startRequest={1} />);
    await waitFor(() => expect(client.startLiveVoice).toHaveBeenCalled());
    await act(async () => {
      Socket.last.onmessage?.({ data: JSON.stringify({ type: 'session.ready', state, capabilities: {}, limits: {} }) });
    });
    fireEvent.click(screen.getByRole('button', { name: 'Mute microphone' }));
    expect(screen.getByRole('button', { name: 'Unmute microphone' })).toHaveAttribute('aria-pressed', 'true');
    expect(screen.getByRole('button', { name: 'Stop speech' })).toBeInTheDocument();
    await act(async () => {
      Socket.last.onmessage?.({ data: JSON.stringify({ type: 'utterance.admitted', utterance_id: 1, turn_id: 't', idempotency_key: 'v:1', status: 'running' }) });
    });
    fireEvent.click(screen.getByRole('button', { name: 'More voice controls' }));
    fireEvent.click(screen.getByRole('button', { name: 'Cancel voice task' }));
    expect(Socket.last.send).toHaveBeenCalledWith(JSON.stringify({ type: 'task.cancel', turn_id: 't' }));
    fireEvent.click(screen.getByRole('button', { name: 'End voice mode' }));
    await waitFor(() => expect(client.endLiveVoice).toHaveBeenCalled());
    expect(onActive).toHaveBeenLastCalledWith(false);
    rendered.rerender(<LiveVoicePanel {...props} startRequest={2} />);
    await waitFor(() => expect(client.startLiveVoice).toHaveBeenLastCalledWith('s', expect.any(String), true));
    rendered.unmount();
    expect(media.close).toHaveBeenCalledTimes(2);
  });
  it('ends a pending connection and ignores its late capabilities', async () => {
    const client = gateway();
    let resolve!: (value: { enabled: boolean; available: boolean }) => void;
    vi.mocked(client.liveVoiceCapabilities).mockImplementation(() => new Promise((done) => {
      resolve = done;
    }));
    const onActive = vi.fn();
    render(
      <LiveVoicePanel client={client} sid="s" startRequest={1} disabled={false}
        onActive={onActive} onBegin={vi.fn()} />,
    );
    await waitFor(() => expect(client.liveVoiceCapabilities).toHaveBeenCalled());
    fireEvent.click(screen.getByRole('button', { name: 'End voice mode' }));
    await act(async () => resolve({ enabled: true, available: true }));
    expect(client.startLiveVoice).not.toHaveBeenCalled();
    expect(onActive).toHaveBeenLastCalledWith(false);
    expect(screen.queryByRole('status')).not.toBeInTheDocument();
  });
  it('shows a connection failure and starts safely on retry', async () => {
    const client = gateway();
    vi.mocked(client.liveVoiceCapabilities).mockRejectedValueOnce(new Error('Gateway unreachable.'));
    render(
      <LiveVoicePanel client={client} sid="s" startRequest={1} disabled={false}
        onActive={vi.fn()} onBegin={vi.fn()} />,
    );
    expect(await screen.findByText('Gateway unreachable.')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Retry voice' }));
    await waitFor(() => expect(client.startLiveVoice).toHaveBeenCalledTimes(1));
    expect(screen.queryByText('Gateway unreachable.')).not.toBeInTheDocument();
  });
  it('discards late capabilities after leaving the session', async () => {
    const client = gateway();
    let resolve!: (value: { enabled: boolean; available: boolean }) => void;
    vi.mocked(client.liveVoiceCapabilities).mockImplementation(() => new Promise((done) => { resolve = done; }));
    const props = { client, disabled: false, onActive: vi.fn(), onBegin: vi.fn() };
    const rendered = render(<LiveVoicePanel {...props} key="a" sid="a" startRequest={1} />);
    await waitFor(() => expect(client.liveVoiceCapabilities).toHaveBeenCalled());
    rendered.rerender(<LiveVoicePanel {...props} key="b" sid="b" />);
    await act(async () => resolve({ enabled: true, available: true }));
    expect(client.startLiveVoice).not.toHaveBeenCalled();
    expect(screen.queryByRole('button', { name: 'Start voice' })).not.toBeInTheDocument();
  });
});
