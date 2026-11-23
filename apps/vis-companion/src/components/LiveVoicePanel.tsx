import { useCallback, useEffect, useRef, useState, type ReactNode } from 'react';
import { createPortal } from 'react-dom';
import { Capacitor } from '@capacitor/core';
import {
  CircleAlertIcon,
  CircleXIcon,
  CloseIcon,
  LoadingIcon,
  MicIcon,
  MicOffIcon,
  MoreIcon,
  SpeakerIcon,
  StopIcon,
} from './icons';
import { Menu, MenuItem, MENU_WIDTH } from './Menu';
import { Button, Checkbox, IconButton } from './ui';
import { menuPosition, type MenuPosition } from '../lib/anchored-menu';
import {
  createLiveVoiceAudio,
  liveVoiceDeviceReason,
  liveVoiceNeedsVerification,
} from '../lib/live-voice-audio';
import { LiveVoiceSession, liveVoiceStatus } from '../lib/live-voice';
import type { LiveVoiceView, LiveVoiceGateway } from '../lib/live-voice';
import {
  claimLiveVoice,
  currentLiveVoice,
  isLiveVoiceActive,
  isLiveVoiceOf,
  updateLiveVoice,
  useLiveVoice,
} from '../lib/live-voice-owner';

const VOICE_ELSEWHERE = 'Hands-free voice is live in another session. End it there first.';

/** Start voice only after an explicit microphone action. Leaving the session keeps the voice. */
export function LiveVoicePanel({
  client,
  sid,
  title,
  disabled,
  startRequest = 0,
  controlsTarget,
  onActive,
  onBegin,
}: {
  client: LiveVoiceGateway & { readonly base: string };
  sid: string;
  title: string;
  disabled: boolean;
  startRequest?: number;
  controlsTarget?: Element | null;
  onActive: (active: boolean) => void;
  onBegin: () => void;
}) {
  const voice = useLiveVoice();
  const mine = isLiveVoiceOf(voice, client.base, sid) ? voice : null;
  // A restart hides the closed view of the voice that it replaces.
  const [dismissed, setDismissed] = useState<object | null>(null);
  const view = mine && mine.session !== dismissed ? mine.view : null;
  const mineSession = mine?.session;
  useEffect(() => {
    if (mineSession) updateLiveVoice(mineSession, { title });
  }, [mineSession, title]);
  const [reason, setReason] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [requested, setRequested] = useState(false);
  const [muted, setMuted] = useState(false);
  const [mobile] = useState(liveVoiceNeedsVerification);
  const [needsConsent] = useState(
    () => liveVoiceNeedsVerification() && Capacitor.getPlatform() !== 'ios',
  );
  const [consent, setConsent] = useState(false);
  const ownSession = useCallback(() => {
    const current = currentLiveVoice();
    return isLiveVoiceOf(current, client.base, sid) ? current?.session ?? null : null;
  }, [client, sid]);
  const attempt = useRef(0);
  const handledRequest = useRef(0);
  const pending = useRef(false);
  // Only a pending start ends with the screen. A started voice belongs to the device.
  useEffect(() => {
    return () => {
      attempt.current += 1;
      pending.current = false;
      onActive(false);
    };
  }, [client, sid, onActive]);
  useEffect(() => {
    if (!view) return;
    const active = view.phase === 'starting' || view.phase === 'live';
    // A lock-screen control can mute. A retry starts with the same microphone state.
    if (active) setMuted(view.muted);
    onActive(active);
    if (!active) setConsent(false);
  }, [view, onActive]);
  const start = useCallback(async () => {
    if (disabled || pending.current) return;
    setRequested(true);
    if (needsConsent && !consent) {
      // Consent is absent only after the last session ended. Show the consent again,
      // not that session: its closed view hides the panel.
      const previous = ownSession();
      void previous?.stop();
      setDismissed(previous);
      return;
    }
    const other = currentLiveVoice();
    if (isLiveVoiceActive(other) && !isLiveVoiceOf(other, client.base, sid)) {
      setReason(VOICE_ELSEWHERE);
      return;
    }
    const id = ++attempt.current;
    pending.current = true;
    setLoading(true);
    setReason(null);
    onBegin();
    onActive(true);
    try {
      const previous = ownSession();
      await previous?.stop();
      if (attempt.current !== id) return;
      setDismissed(previous);
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
        changed: (state) => updateLiveVoice(next, { view: state }),
      });
      if (!claimLiveVoice({ base: client.base, sid, title, session: next, view: next.current() }))
        throw new Error(VOICE_ELSEWHERE);
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
  }, [client, sid, title, disabled, needsConsent, consent, mobile, muted, onActive, onBegin, ownSession]);
  useEffect(() => {
    if (!startRequest || handledRequest.current === startRequest) return;
    handledRequest.current = startRequest;
    void start();
  }, [startRequest, start]);
  const renderControls = (content: ReactNode) =>
    controlsTarget ? createPortal(content, controlsTarget) : content;
  if (!requested && !view) return null;
  if (loading) {
    return renderControls(
      <LiveVoicePanelContent
        view={null}
        connecting
        disabled={disabled}
        reason={null}
        onStart={() => undefined}
        onMute={() => undefined}
        onOutputMute={() => undefined}
        onStopSpeech={() => undefined}
        onCancelTask={() => undefined}
        onEnd={() => {
          attempt.current += 1;
          pending.current = false;
          setLoading(false);
          setRequested(false);
          onActive(false);
        }}
      />,
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
        ownSession()?.setMuted(next);
      }}
      onOutputMute={(next) => ownSession()?.setOutputMuted(next)}
      onStopSpeech={() => ownSession()?.interrupt()}
      onCancelTask={(turnId) => ownSession()?.cancelTask(turnId)}
      onEnd={() => {
        void ownSession()?.stop();
      }}
    />
  );
}

/** Present media state without acquiring a device or opening a connection. */
export function LiveVoicePanelContent({
  view,
  connecting = false,
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
  /** The gateway has no session yet. Only End is available. */
  connecting?: boolean;
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
  const [menu, setMenu] = useState<MenuPosition | null>(null);
  const status = connecting
    ? ({ state: 'connecting', muted: false, speaking: false, task: null } as const)
    : view
      ? liveVoiceStatus(view)
      : null;
  const stopped = !status && (view !== null || reason !== null);
  const output = view?.state?.output;
  const speech =
    view?.outputMuted || output === 'muted'
      ? 'Speech muted'
      : output === 'playing'
        ? 'Speaking'
        : output === 'committed' || output === 'synthesizing'
          ? 'Preparing speech'
          : null;
  const face = status
    ? {
        connecting: { title: 'Connecting…', Icon: LoadingIcon, edge: 'border-l-muted' },
        listening: { title: 'Listening', Icon: MicIcon, edge: 'border-l-white' },
        speaking: { title: 'Speaking', Icon: SpeakerIcon, edge: 'border-l-accent' },
        muted: { title: 'Microphone muted', Icon: MicOffIcon, edge: 'border-l-muted' },
        waiting: {
          title: 'Waiting for you',
          Icon: CircleAlertIcon,
          edge: 'border-l-warn-edge',
          // The controls stay on the usual surface: their colours are made for it.
          head: '-mx-3 -mt-2 mb-2 bg-warn-surface px-3 py-2',
        },
      }[status.state]
    : stopped
      ? { title: 'Voice stopped', Icon: CircleXIcon, edge: 'border-l-err' }
      : null;
  // The headline shows one state. The line below shows the task and the speech.
  const details = status
    ? [
        status.state === 'waiting'
          ? 'Use the session input controls.'
          : status.task &&
            { running: 'Task running', queued: 'Task queued', cancelling: 'Cancellation requested' }[
              status.task
            ],
        speech === face?.title ? null : speech,
      ].filter((detail): detail is string => Boolean(detail))
    : [];
  const live = status !== null && status.state !== 'connecting';
  const stoppable =
    live && !view?.outputMuted && (output === 'playing' || output === 'committed' || output === 'synthesizing');
  // The labels stay on one line. A narrow panel drops the marks, not the words.
  const cell = 'flex flex-1 items-center justify-center gap-1.5 whitespace-nowrap';
  const mark = 'shrink-0 @max-[21rem]:hidden';
  return (
    <section
      aria-label="Hands-free voice"
      className={`@container border border-l-[3px] border-field-edge px-3 py-2 font-mono ${face?.edge ?? 'border-l-white'}`}
    >
      {face && (
        <div role="status" className={`flex items-start gap-2 ${'head' in face ? face.head : ''}`}>
          <face.Icon className="mt-0.5 size-[18px] shrink-0" />
          <div className="min-w-0">
            <p className="text-subhead font-bold">{face.title}</p>
            {details.length > 0 && (
              <p className="flex flex-wrap gap-x-3 text-ui text-muted">
                {details.map((detail) => (
                  <span key={detail}>{detail}</span>
                ))}
              </p>
            )}
          </div>
        </div>
      )}
      {view?.transcript && (
        <p
          aria-label={view.final ? 'Final transcript' : 'Provisional transcript'}
          className={`mt-1 truncate text-body ${view.final ? '' : 'text-muted'}`}
        >
          {view.transcript}
        </p>
      )}
      {reason && (
        <p role="status" className="mt-1 text-ui">
          {reason}
        </p>
      )}
      {view?.message && (
        <p role="status" className="mt-1 text-ui">
          {view.message}
        </p>
      )}
      {view && !status && !view.reconciled && (
        <p role="status" className="mt-1 text-ui text-muted">
          Check the session before repeating a request. Its admission may be uncertain.
        </p>
      )}
      {status ? (
        <div className="mt-2 flex items-center gap-2">
          <Button
            variant={status.muted ? 'primary' : 'secondary'}
            className={cell}
            disabled={!live}
            aria-label={status.muted ? 'Unmute microphone' : 'Mute microphone'}
            aria-pressed={status.muted}
            onClick={() => onMute(!status.muted)}
          >
            {status.muted ? (
              <MicIcon className={`size-3.5 ${mark}`} />
            ) : (
              <MicOffIcon className={`size-3.5 ${mark}`} />
            )}
            {status.muted ? 'Unmute' : 'Mute'}
          </Button>
          <Button variant="secondary" className={cell} disabled={!stoppable} onClick={onStopSpeech}>
            <StopIcon className={`size-3 ${mark}`} />
            Stop speech
          </Button>
          <Button variant="danger" className={cell} aria-label="End voice mode" onClick={onEnd}>
            <CloseIcon className={`size-3.5 ${mark}`} />
            End
          </Button>
          <IconButton
            label="More voice controls"
            variant="quiet"
            disabled={!live}
            aria-haspopup="dialog"
            aria-expanded={menu !== null}
            onClick={(event) =>
              setMenu(menuPosition(event.currentTarget.getBoundingClientRect(), MENU_WIDTH))
            }
          >
            <MoreIcon className="size-3.5" />
          </IconButton>
          {menu && view && (
            <Menu label="More voice controls" at={menu} onDismiss={() => setMenu(null)}>
              <MenuItem
                title={view.outputMuted ? 'Unmute speech' : 'Mute speech'}
                icon={<SpeakerIcon className="size-3.5" />}
                onSelect={() => {
                  setMenu(null);
                  onOutputMute(!view.outputMuted);
                }}
              />
              <MenuItem
                title="Cancel voice task"
                icon={<CloseIcon className="size-3.5" />}
                tone="danger"
                disabled={!view.turnId}
                onSelect={() => {
                  setMenu(null);
                  if (view.turnId) onCancelTask(view.turnId);
                }}
              />
            </Menu>
          )}
        </div>
      ) : (
        <div className={face ? 'mt-2' : ''}>
          {mobile && (
            <>
              <p className="text-ui text-muted">
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
          <Button disabled={disabled || (mobile && !allowMobileTest)} onClick={onStart}>
            {stopped ? 'Retry voice' : 'Start voice'}
          </Button>
        </div>
      )}
    </section>
  );
}
