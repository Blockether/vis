import { CloseIcon, MicIcon } from './icons';
import { Button } from './ui';
import { liveVoiceStatus } from '../lib/live-voice';
import type { LiveVoiceOwner } from '../lib/live-voice-owner';

const TITLES = {
  connecting: 'Voice connecting',
  listening: 'Voice listening',
  speaking: 'Voice speaking',
  muted: 'Voice muted',
  waiting: 'Voice waiting for you',
} as const;

/** The device voice while its session is not on screen: where it runs, and a way back or out. */
export function LiveVoiceDock({ voice, onOpen }: { voice: LiveVoiceOwner; onOpen: (() => void) | null }) {
  const status = liveVoiceStatus(voice.view);
  if (!status) return null;
  return (
    <section
      aria-label="Hands-free voice in another session"
      className="flex shrink-0 items-center gap-2 border-b border-field-edge px-3 py-1.5 font-mono text-ui"
    >
      <MicIcon className="size-3.5 shrink-0" />
      <p role="status" className="min-w-0 flex-1 truncate">
        {TITLES[status.state]} <span className="text-muted">· {voice.title}</span>
      </p>
      {onOpen && (
        <Button variant="secondary" density="inline" onClick={onOpen}>
          Open session
        </Button>
      )}
      <Button
        variant="danger"
        density="inline"
        aria-label="End voice mode"
        onClick={() => void voice.session.stop()}
      >
        <CloseIcon className="size-3" />
        End
      </Button>
    </section>
  );
}
