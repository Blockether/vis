import type { ReactNode } from 'react';
import { isNewerVersion, type EngineTask, type LocalEngine } from '../lib/local-engine';
import { Button, Spinner, Text } from './ui';
import { CircleCheckIcon, CircleDashedIcon, CircleXIcon } from './icons';

const DOING: Record<EngineTask, string> = {
  install: 'Installing Vis…',
  update: 'Updating Vis…',
  start: 'Starting the gateway…',
};
const STOPPED: Record<EngineTask, string> = {
  install: 'The installation stopped',
  update: 'The update stopped',
  start: 'The gateway did not start',
};
/** How many of the newest log lines a failed task shows. */
const LOG_TAIL = 6;

/** One state: its mark, its name, one line about it, and its verbs. */
function Row({
  mark,
  ink = '',
  title,
  note,
  children,
}: {
  mark: ReactNode;
  ink?: string;
  title: string;
  note: string;
  children: ReactNode;
}) {
  return (
    <div className="flex flex-wrap items-center gap-x-3 gap-y-2">
      <span className={`flex w-4 shrink-0 justify-center ${ink}`}>{mark}</span>
      <span className="min-w-0 flex-1">
        <Text variant="label" className="block truncate">
          {title}
        </Text>
        <Text variant="meta" className="mt-0.5 block truncate">
          {note}
        </Text>
      </span>
      {children}
    </div>
  );
}

/**
 * VIS ON THIS COMPUTER, for the person who has only the desktop app: one row for the
 * state of the engine and its gateway, with the verb that the state permits.
 *
 * It owns no host call. `lib/local-engine` holds the state and the verbs, so the
 * pairing page and Settings show the same object.
 */
export function ThisComputer({
  engine,
  isChecking = false,
  isCheckFailed = false,
  isUpdateShown = true,
  onInstall,
  onUpdate,
  onStart,
  onStop,
  onCheck,
  onConnect,
}: {
  engine: LocalEngine;
  /** A check for a newer release is in flight. */
  isChecking?: boolean;
  isCheckFailed?: boolean;
  /** Settings shows the update. The pairing page does not. */
  isUpdateShown?: boolean;
  onInstall: () => void;
  onUpdate: () => void;
  onStart: () => void;
  /** Stop the task in progress. */
  onStop: () => void;
  onCheck: () => void;
  /** Pair with the gateway. Present when it runs without a token and is not paired. */
  onConnect?: () => void;
}) {
  if (engine.kind === 'absent') {
    return (
      <Row
        mark={<CircleDashedIcon />}
        ink="text-dialog-hint"
        title="Vis is not installed on this computer"
        note="Install it to run sessions here."
      >
        <Button variant="primary" onClick={onInstall}>
          Install
        </Button>
      </Row>
    );
  }

  if (engine.kind === 'working') {
    return (
      <div role="status" aria-live="polite">
        <Row
          mark={<Spinner tone="accent" />}
          title={DOING[engine.task]}
          note={engine.log.at(-1) ?? 'This can take a minute.'}
        >
          <Button variant="secondary" onClick={onStop}>
            Stop
          </Button>
        </Row>
      </div>
    );
  }

  if (engine.kind === 'failed') {
    const retry = { install: onInstall, update: onUpdate, start: onStart }[engine.task];
    const tail = engine.log.slice(-LOG_TAIL);
    return (
      <div className="space-y-3">
        <Row
          mark={<CircleXIcon />}
          ink="text-err"
          title={STOPPED[engine.task]}
          note={engine.reason}
        >
          <Button variant="primary" onClick={retry}>
            Try again
          </Button>
        </Row>
        {tail.length > 0 && (
          <pre
            aria-label="Log"
            className="max-h-40 overflow-y-auto whitespace-pre-wrap border border-dialog-edge bg-input px-3 py-2 font-mono text-meta text-dialog-hint [overflow-wrap:anywhere]"
          >
            {tail.join('\n')}
          </pre>
        )}
      </div>
    );
  }

  const isRunning = engine.kind === 'running';
  const isRelease = engine.track === 'release';
  const hasUpdate = engine.latest !== undefined && isNewerVersion(engine.latest, engine.version);
  const note = !isRunning
    ? 'The gateway does not run.'
    : !isUpdateShown
      ? onConnect
        ? 'The gateway runs.'
        : 'The gateway asks for a pairing link.'
      : hasUpdate
        ? `Vis ${engine.latest} is available.`
        : isCheckFailed
          ? 'The check for updates got no answer.'
          : engine.latest
            ? 'This is the newest release.'
            : 'The gateway runs.';
  return (
    <Row
      mark={isRunning ? <CircleCheckIcon /> : <CircleDashedIcon />}
      ink={isRunning ? 'text-ok' : 'text-dialog-hint'}
      title={`Vis ${engine.version}${isRelease ? '' : ` · ${engine.track}`}`}
      note={note}
    >
      {!isRunning && (
        <Button variant="primary" onClick={onStart}>
          Start
        </Button>
      )}
      {isRunning && onConnect && (
        <Button variant="primary" onClick={onConnect}>
          Connect
        </Button>
      )}
      {isUpdateShown &&
        (hasUpdate || !isRelease ? (
          <Button
            variant={isRunning && hasUpdate && !onConnect ? 'primary' : 'secondary'}
            onClick={onUpdate}
          >
            Update
          </Button>
        ) : (
          <Button variant="secondary" onClick={onCheck} disabled={isChecking}>
            {isChecking ? 'Checking…' : 'Check for updates'}
          </Button>
        ))}
    </Row>
  );
}
