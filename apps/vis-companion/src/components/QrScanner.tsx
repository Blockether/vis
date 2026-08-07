import { useEffect, useRef, useState } from 'react';
import { Button, Spinner } from './ui';
import {
  decodeFrame,
  liveScanSupported,
  photoScanSupported,
  scanQrFromPhoto,
  scanWithNativeScanner,
} from '../lib/scan';

interface Props {
  /** Fires once, with the raw decoded string. The overlay stops itself first. */
  onResult: (raw: string) => void;
  onCancel: () => void;
}

type Phase = 'starting' | 'live' | 'busy' | 'error';

/** Permission prompts and the first camera frame take a beat; past this, say so. */
const SLOW_START_MS = 2500;
/** How long a live preview may find nothing before it offers the photo fallback. */
const NO_HIT_HINT_MS = 9000;

/**
 * Full-screen live QR scanner drawn in the webview.
 *
 * It decodes every frame instead of making you shoot a photo, so a code held in
 * front of the phone is picked up in well under a second. The still-photo
 * capture survives only as a fallback for when the preview cannot start.
 *
 * Opening a camera is never instant — iOS may prompt for permission and the
 * first frame lands hundreds of milliseconds later — so every wait is stated on
 * screen with a live spinner rather than a black rectangle that looks broken.
 */
export function QrScanner({ onResult, onCancel }: Props) {
  const videoRef = useRef<HTMLVideoElement | null>(null);
  const canvasRef = useRef<HTMLCanvasElement | null>(null);
  const streamRef = useRef<MediaStream | null>(null);
  const frameRef = useRef<number | null>(null);
  // A hit can land while a teardown is already in flight; without this the
  // overlay would report twice and open two connections.
  const doneRef = useRef(false);
  const [phase, setPhase] = useState<Phase>('starting');
  const [error, setError] = useState('');
  // Elapsed-time hints: one timer per phase, so "slow" is measured from the
  // moment that phase began rather than from mount.
  const [slowStart, setSlowStart] = useState(false);
  const [noHitYet, setNoHitYet] = useState(false);

  useEffect(() => {
    if (phase === 'starting') {
      setSlowStart(false);
      const timer = window.setTimeout(() => setSlowStart(true), SLOW_START_MS);
      return () => window.clearTimeout(timer);
    }
    if (phase === 'live') {
      setNoHitYet(false);
      const timer = window.setTimeout(() => setNoHitYet(true), NO_HIT_HINT_MS);
      return () => window.clearTimeout(timer);
    }
    return undefined;
  }, [phase]);

  function stopCamera() {
    if (frameRef.current !== null) {
      cancelAnimationFrame(frameRef.current);
      frameRef.current = null;
    }
    streamRef.current?.getTracks().forEach((track) => track.stop());
    streamRef.current = null;
    const video = videoRef.current;
    if (video) video.srcObject = null;
  }

  function finish(raw: string) {
    if (doneRef.current) return;
    doneRef.current = true;
    stopCamera();
    onResult(raw);
  }

  function cancel() {
    if (doneRef.current) return;
    doneRef.current = true;
    stopCamera();
    onCancel();
  }

  async function shootPhoto() {
    setPhase('busy');
    setError('');
    try {
      const raw = await scanQrFromPhoto();
      if (raw) {
        finish(raw);
        return;
      }
      setError('No QR code in that photo — fill the frame with the code and retry');
    } catch (cause) {
      const text = (cause as Error).message || '';
      if (/cancel/i.test(text)) {
        setError('');
      } else {
        setError(text || 'Camera unavailable');
      }
    }
    setPhase(streamRef.current ? 'live' : 'error');
  }

  useEffect(() => {
    let cancelled = false;

    async function start() {
      // Native shells use the official Capacitor scanner: Apple Vision on iOS
      // and the native implementation on Android. The webview/jsQR preview is
      // retained as a browser fallback, not as the normal iPhone path.
      try {
        const native = await scanWithNativeScanner();
        if (cancelled) return;
        if (native) {
          finish(native);
          return;
        }
      } catch (cause) {
        if (cancelled) return;
        const text = (cause as Error).message || '';
        if (/cancel/i.test(text)) {
          cancel();
          return;
        }
        // If native scanner startup itself fails, the in-webview scanner still
        // gives the user a working route instead of a dead end.
      }
      if (!liveScanSupported()) {
        if (!cancelled) {
          setError('This device cannot open a live camera preview');
          setPhase('error');
        }
        return;
      }
      try {
        const stream = await navigator.mediaDevices.getUserMedia({
          video: {
            facingMode: { ideal: 'environment' },
            width: { ideal: 1920 },
            height: { ideal: 1080 },
          },
          audio: false,
        });
        if (cancelled) {
          stream.getTracks().forEach((track) => track.stop());
          return;
        }
        streamRef.current = stream;
        const video = videoRef.current;
        if (!video) return;
        video.srcObject = stream;
        await video.play().catch(() => undefined);
        if (cancelled) return;
        setPhase('live');
        tick();
      } catch (cause) {
        if (cancelled) return;
        const name = (cause as Error).name || '';
        setError(
          /NotAllowed|Security/i.test(name)
            ? 'Camera access was denied — enable it in Settings ▸ Vis'
            : 'Could not start the camera preview',
        );
        setPhase('error');
      }
    }

    // ~10 decodes/second is well past what a hand-held code needs, and leaves
    // the main thread free enough that the preview stays smooth.
    let last = 0;
    function tick() {
      frameRef.current = requestAnimationFrame((now) => {
        const video = videoRef.current;
        const canvas = canvasRef.current;
        if (doneRef.current || !video || !canvas) return;
        if (now - last > 100 && video.readyState >= 2) {
          last = now;
          try {
            const hit = decodeFrame(video, canvas);
            if (hit) {
              finish(hit);
              return;
            }
          } catch {
            // A frame that cannot be sampled is not fatal; try the next one.
          }
        }
        tick();
      });
    }

    void start();
    return () => {
      cancelled = true;
      stopCamera();
    };
    // Mount-only: the camera must not restart on every render.
  }, []);

  const waiting = phase === 'starting' || phase === 'busy';
  const waitingLabel = phase === 'busy' ? 'Reading the photo' : 'Starting the camera';
  const waitingHint =
    phase === 'busy'
      ? 'Decoding at full resolution — this takes a moment'
      : slowStart
        ? 'Still waiting on the camera. If iOS asked for permission, tap Allow.'
        : 'iOS may ask for camera permission';

  return (
    <div className="fixed inset-0 z-50 flex flex-col bg-black">
      <div className="relative flex-1 overflow-hidden">
        <video
          ref={videoRef}
          className="absolute inset-0 h-full w-full object-cover"
          playsInline
          muted
          autoPlay
        />
        <canvas ref={canvasRef} className="hidden" />

        {/* The aiming frame appears only once frames are actually being decoded:
            drawing it over a dead preview is what made the scanner look like a
            plain camera that simply ignored the code. */}
        {phase === 'live' && (
          <div
            className="pointer-events-none absolute inset-0 grid place-items-center"
            aria-hidden="true"
          >
            <div className="aspect-square w-[68vmin] border-2 border-accent/80 shadow-[0_0_0_100vmax_rgb(0_0_0/0.45)]" />
          </div>
        )}

        {waiting && (
          <div
            className="absolute inset-0 grid place-items-center bg-black/70 px-6"
            role="status"
            aria-live="polite"
          >
            <div className="flex flex-col items-center gap-2 text-center">
              <span className="font-mono text-display text-accent">
                <Spinner />
              </span>
              <p className="font-mono text-ui text-white">{waitingLabel}…</p>
              <p className="max-w-xs font-mono text-meta text-white/70">{waitingHint}</p>
            </div>
          </div>
        )}

        {phase === 'live' && (
          <p className="absolute inset-x-0 bottom-4 px-6 text-center font-mono text-meta text-white/85">
            <span className="text-accent">
              <Spinner />
            </span>{' '}
            {noHitYet
              ? 'Nothing yet — fill the frame with the code, or take a photo instead'
              : 'Point at the QR in your terminal — it reads itself'}
          </p>
        )}
      </div>

      <div className="space-y-2 border-t border-dialog-edge bg-panel px-[max(0.75rem,env(safe-area-inset-left))] pb-[max(0.75rem,env(safe-area-inset-bottom))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-3">
        {error && (
          <p className="border border-err/50 bg-err/10 px-3 py-2 font-mono text-meta text-err" role="status">
            {error}
          </p>
        )}
        <div className="flex gap-2">
          <Button variant="secondary" className="flex-1" onClick={cancel}>
            Cancel
          </Button>
          {photoScanSupported() && (
            <Button
              variant="secondary"
              className="flex-1"
              onClick={shootPhoto}
              disabled={phase === 'busy'}
            >
              {phase === 'busy' ? 'Reading…' : 'Take a photo instead'}
            </Button>
          )}
        </div>
      </div>
    </div>
  );
}
