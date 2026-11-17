import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';
import { createPortal } from 'react-dom';
import { Capacitor } from '@capacitor/core';
import { Checkbox, TextButton } from './ui';
import {
  createLiveVoiceAudio,
  liveVoiceDeviceReason,
  liveVoiceNeedsVerification,
} from '../lib/live-voice-audio';
import { LiveVoiceSession } from '../lib/live-voice';
import type { LiveVoiceView, LiveVoiceGateway } from '../lib/live-voice';

/** Start voice only after an explicit microphone action. */
export function LiveVoicePanel({
  client,
  sid,
  disabled,
  startRequest = 0,
  controlsTarget,
  onActive,
  onBegin,
}: {
  client: LiveVoiceGateway;
  sid: string;
  disabled: boolean;
  startRequest?: number;
  controlsTarget?: Element | null;
  onActive: (active: boolean) => void;
  onBegin: () => void;
}) {
  const [view, setView] = useState<LiveVoiceView | null>(null);
  const [reason, setReason] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [requested, setRequested] = useState(false);
  const [muted, setMuted] = useState(false);
  const [mobile] = useState(liveVoiceNeedsVerification);
  const [needsConsent] = useState(
    () => liveVoiceNeedsVerification() && Capacitor.getPlatform() !== 'ios',
  );
  const [consent, setConsent] = useState(false);
  const session = useRef<LiveVoiceSession | null>(null);
  const attempt = useRef(0);
  const handledRequest = useRef(0);
  const pending = useRef(false);
  useEffect(() => {
    return () => {
      attempt.current += 1;
      pending.current = false;
      const previous = session.current;
      session.current = null;
      void previous?.stop();
      onActive(false);
    };
  }, [client, sid, onActive]);
  const start = useCallback(async () => {
    if (disabled || pending.current) return;
    setRequested(true);
    if (needsConsent && !consent) return;
    const id = ++attempt.current;
    pending.current = true;
    setLoading(true);
    setReason(null);
    onBegin();
    onActive(true);
    try {
      await session.current?.stop();
      if (attempt.current !== id) return;
      session.current = null;
      setView(null);
      const deviceReason = liveVoiceDeviceReason();
      if (deviceReason) throw new Error(deviceReason);
      const capabilities = await client.liveVoiceCapabilities();
      if (attempt.current !== id) return;
      if (!capabilities.enabled || !capabilities.available) {
        throw new Error(capabilities.reason ?? 'Hands-free voice is unavailable on this gateway.');
      }
      const next = new LiveVoiceSession({
        gateway: client,
        sid,
        audio: createLiveVoiceAudio(mobile),
        muted,
        changed: (state) => {
          if (session.current !== next) return;
          setView(state);
          const active = state.phase === 'starting' || state.phase === 'live';
          onActive(active);
          if (!active) setConsent(false);
        },
      });
      session.current = next;
      void next.start();
    } catch (error) {
      if (attempt.current !== id) return;
      setReason(error instanceof Error ? error.message : 'Could not connect to voice. Try again.');
      onActive(false);
    } finally {
      if (attempt.current === id) {
        pending.current = false;
        setLoading(false);
      }
    }
  }, [client, sid, disabled, needsConsent, consent, mobile, muted, onActive, onBegin]);
  useEffect(() => {
    if (!startRequest || handledRequest.current === startRequest) return;
    handledRequest.current = startRequest;
    void start();
  }, [startRequest, start]);
  const renderControls = (content: ReactNode) =>
    controlsTarget ? createPortal(content, controlsTarget) : content;
  if (!requested) return null;
  if (loading) {
    return renderControls(
      <div className="flex items-center gap-3 px-2 py-1 text-chip">
        <span role="status">Connecting…</span>
        <TextButton onClick={() => {
          attempt.current += 1;
          pending.current = false;
          setLoading(false);
          setRequested(false);
          onActive(false);
        }}>
          End voice mode
        </TextButton>
      </div>
    );
  }
  if (view?.phase === 'closed' && view.reconciled && !view.message) return null;
  return renderControls(
    <LiveVoicePanelContent
      view={view}
      disabled={disabled}
      mobile={needsConsent}
      allowMobileTest={consent}
      onAllowMobileTest={setConsent}
      reason={reason}
      onStart={() => void start()}
      onMute={(next) => {
        setMuted(next);
        session.current?.setMuted(next);
      }}
      onOutputMute={(next) => session.current?.setOutputMuted(next)}
      onStopSpeech={() => session.current?.interrupt()}
      onCancelTask={(turnId) => session.current?.cancelTask(turnId)}
      onEnd={() => {
        void session.current?.stop();
      }}
    />
  );
}

/** Present media state without acquiring a device or opening a connection. */
export function LiveVoicePanelContent({
  view,
  disabled,
  reason,
  onStart,
  onMute,
  onOutputMute,
  onStopSpeech,
  onCancelTask,
  onEnd,
  mobile = false,
  allowMobileTest = false,
  onAllowMobileTest,
}: {
  view: LiveVoiceView | null;
  disabled: boolean;
  reason: string | null;
  onStart: () => void;
  onMute: (muted: boolean) => void;
  onOutputMute: (muted: boolean) => void;
  onStopSpeech: () => void;
  onCancelTask: (turnId: string) => void;
  onEnd: () => void;
  mobile?: boolean;
  allowMobileTest?: boolean;
  onAllowMobileTest?: (allowed: boolean) => void;
}) {
  const active = view?.phase === 'starting' || view?.phase === 'live';
  return (
    <section
      aria-label="Hands-free voice"
      className="border-b border-dialog-edge px-2 py-1 font-mono text-chip"
    >
      {!active ? (
        <div>
          {mobile && (
            <>
              <p className="text-muted">
                Mobile audio is unverified. Use low volume. Test speaker echo before relying on
                hands-free voice.
              </p>
              <Checkbox
                isOn={allowMobileTest}
                onClick={() => onAllowMobileTest?.(!allowMobileTest)}
              >
                Enable mobile audio testing
              </Checkbox>
            </>
          )}
          <TextButton disabled={disabled || (mobile && !allowMobileTest)} onClick={onStart}>
            {view || reason ? 'Retry voice' : 'Start voice'}
          </TextButton>
        </div>
      ) : (
        <div className="flex flex-wrap items-center gap-3">
          <span role="status">
            {view.phase === 'starting'
              ? 'Connecting…'
              : view.muted
                ? 'Microphone muted'
                : 'Listening'}
          </span>
          {view.state?.agent === 'running' && <span role="status">Task running</span>}
          {view.state?.agent === 'queued' && <span role="status">Task queued</span>}
          {view.state?.agent === 'cancelling' && <span role="status">Cancellation requested</span>}
          {view.state?.agent === 'waiting_for_input' && (
            <span role="status">Waiting for you · Use the session input controls</span>
          )}
          {view.state?.output === 'playing' && !view.outputMuted && (
            <span role="status">Speaking</span>
          )}
          {(view.state?.output === 'committed' || view.state?.output === 'synthesizing') &&
            !view.outputMuted && <span role="status">Preparing speech</span>}
          {(view.outputMuted || view.state?.output === 'muted') && (
            <span role="status">Speech muted</span>
          )}
          <TextButton aria-pressed={view.muted} onClick={() => onMute(!view.muted)}>
            {view.muted ? 'Unmute microphone' : 'Mute microphone'}
          </TextButton>
          <TextButton
            aria-pressed={view.outputMuted}
            onClick={() => onOutputMute(!view.outputMuted)}
          >
            {view.outputMuted ? 'Unmute speech' : 'Mute speech'}
          </TextButton>
          <TextButton onClick={onStopSpeech}>Stop speech</TextButton>
          {view.turnId && (
            <TextButton onClick={() => onCancelTask(view.turnId!)}>Cancel voice task</TextButton>
          )}
          <TextButton onClick={onEnd}>End voice mode</TextButton>
        </div>
      )}
      {reason && <p role="status">{reason}</p>}
      {view?.transcript && (
        <p aria-label={view.final ? 'Final transcript' : 'Provisional transcript'}>
          {view.transcript}
        </p>
      )}
      {view?.message && <p role="status">{view.message}</p>}
      {view && !active && !view.reconciled && (
        <p role="status">
          Check the session before repeating a request. Its admission may be uncertain.
        </p>
      )}
    </section>
  );
}
