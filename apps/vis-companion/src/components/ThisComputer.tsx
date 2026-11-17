import { PairCommand } from './Machines';
import { Button, Spinner, Text } from './ui';
import { CircleCheckIcon, CircleDashedIcon, CircleXIcon } from './icons';

/** The work that the desktop host does for Vis on this computer. */
export type EngineTask = 'install' | 'update' | 'start';

/**
 * WHAT THE DESKTOP HOST KNOWS ABOUT VIS ON THIS COMPUTER.
 *
 * Only the desktop app on a platform that has a Vis engine holds this state. A browser,
 * a phone, Windows and an Intel Mac have none, and show no part of this view.
 *
 * `step` is an index into the steps of `task`. `log` holds the lines that the installer
 * or the `vis-agent` command printed, oldest first. `latest` is the newest release that
 * a check found: absent before a check, equal to `version` when nothing is newer.
 */
export type LocalEngine =
  | { kind: 'absent' }
  | { kind: 'working'; task: EngineTask; step: number; log: string[] }
  | { kind: 'failed'; task: EngineTask; step: number; reason: string; log: string[] }
  | { kind: 'stopped'; version: string; latest?: string }
  | { kind: 'running'; version: string; address: string; latest?: string };

const INSTALL_COMMAND =
  'curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash';

/** Each task as the person reads it: its steps in order, and the same work in a terminal. */
const TASKS: Record<
  EngineTask,
  { doing: string; stopped: string; steps: string[]; command: string }
> = {
  install: {
    doing: 'Installing Vis',
    stopped: 'The installation stopped',
    steps: ['Download the installer', 'Install Vis', 'Start the gateway', 'Connect this app'],
    command: INSTALL_COMMAND,
  },
  update: {
    doing: 'Updating Vis',
    stopped: 'The update stopped',
    steps: ['Install the update', 'Restart the gateway'],
    command: 'vis-agent update',
  },
  start: {
    doing: 'Starting the gateway',
    stopped: 'The gateway did not start',
    steps: ['Start the gateway'],
    command: 'vis-agent gateway start',
  },
};

/** The mark column of the report, shared by its header and its steps. */
const MARK = 'flex w-4 shrink-0 justify-center';
/** How many of the newest log lines stay in view. */
const LOG_TAIL = 6;

/**
 * THE REPORT OF ONE TASK: its name, each step with its state, and the newest lines of
 * the log. A failed task keeps the same report, so the person sees where it stopped.
 */
function WorkReport({
  engine,
  onStop,
  onRetry,
}: {
  engine: Extract<LocalEngine, { kind: 'working' | 'failed' }>;
  onStop: () => void;
  onRetry: () => void;
}) {
  const task = TASKS[engine.task];
  const isFailed = engine.kind === 'failed';
  const tail = engine.log.slice(-LOG_TAIL);
  return (
    <div className="space-y-3">
      <div
        className="overflow-hidden border border-dialog-edge bg-panel"
        role="status"
        aria-live="polite"
      >
        <header className="flex items-center gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
          <span className={`${MARK} ${isFailed ? 'text-err' : ''}`}>
            {isFailed ? <CircleXIcon /> : <Spinner tone="accent" />}
          </span>
          <span className="min-w-0 flex-1">
            <Text variant="label" className="block truncate">
              {isFailed ? task.stopped : task.doing}
            </Text>
            <Text variant="meta" className="mt-0.5 block">
              {isFailed
                ? engine.reason
                : `Step ${engine.step + 1} of ${task.steps.length} · ${task.steps[engine.step]}`}
            </Text>
          </span>
          {isFailed ? (
            <Button variant="primary" onClick={onRetry}>
              Try again
            </Button>
          ) : (
            <Button variant="secondary" onClick={onStop}>
              Stop
            </Button>
          )}
        </header>
        <ol className="divide-y divide-dialog-edge">
          {task.steps.map((title, index) => {
            const isDone = index < engine.step;
            const isCurrent = index === engine.step;
            const ink = isDone
              ? 'text-ok'
              : isCurrent
                ? isFailed
                  ? 'text-err'
                  : 'text-accent-ink'
                : 'text-dialog-hint';
            return (
              <li key={title} className="flex items-center gap-2 px-3 py-2">
                <span className={`${MARK} ${ink}`}>
                  {isDone ? (
                    <CircleCheckIcon />
                  ) : isCurrent ? (
                    isFailed ? (
                      <CircleXIcon />
                    ) : (
                      <Spinner />
                    )
                  ) : (
                    <CircleDashedIcon />
                  )}
                </span>
                <Text variant={isCurrent ? 'label' : 'meta'} className="min-w-0 flex-1 truncate">
                  {title}
                </Text>
                <span className={`shrink-0 ${ink}`}>
                  <Text variant="meta" tone="inherit">
                    {isDone ? 'done' : isCurrent ? (isFailed ? 'stopped' : 'in progress') : 'waiting'}
                  </Text>
                </span>
              </li>
            );
          })}
        </ol>
        {tail.length > 0 && (
          <pre
            aria-label="Log"
            className="max-h-40 overflow-y-auto whitespace-pre-wrap border-t border-dialog-edge bg-input px-3 py-2 font-mono text-meta text-dialog-hint [overflow-wrap:anywhere]"
          >
            {tail.join('\n')}
          </pre>
        )}
      </div>
      {isFailed && (
        <div className="space-y-2">
          <Text as="p" variant="description">
            You can also do this step in a terminal:
          </Text>
          <PairCommand value={task.command} />
        </div>
      )}
    </div>
  );
}

/** One line of the offer: what the installation does, in two columns. */
function Fact({ name, children }: { name: string; children: string }) {
  return (
    <div className="grid grid-cols-[5.5rem_minmax(0,1fr)] gap-x-2">
      <dt>
        <Text variant="meta">{name}</Text>
      </dt>
      <dd>
        <Text variant="option">{children}</Text>
      </dd>
    </div>
  );
}

/**
 * VIS ON THIS COMPUTER, for the person who has only the desktop app: the offer to
 * install the engine, the report while the host installs, starts or updates it, and
 * the installed engine with its gateway and its update.
 *
 * It owns no host call. The owner reads the state from the desktop host and gives
 * the verbs, so the pairing page and Settings show the same object.
 */
export function ThisComputer({
  engine,
  isChecking = false,
  onInstall,
  onUpdate,
  onStart,
  onStop,
  onCheck,
}: {
  engine: LocalEngine;
  /** A check for a newer release is in flight. */
  isChecking?: boolean;
  onInstall: () => void;
  onUpdate: () => void;
  onStart: () => void;
  /** Stop the task in progress. */
  onStop: () => void;
  onCheck: () => void;
}) {
  if (engine.kind === 'absent') {
    return (
      <div className="min-w-0 space-y-4">
        <div className="space-y-2">
          <Text as="h3" variant="heading">
            Vis is not installed on this computer
          </Text>
          <Text as="p" variant="description">
            This app shows the sessions of a Vis engine. Install the engine here, and this app
            starts it each time that it opens.
          </Text>
        </div>
        <dl className="space-y-1.5">
          <Fact name="Installs">The vis-agent command in ~/.local/bin and its files in ~/.vis</Fact>
          <Fact name="Access">No administrator password</Fact>
          <Fact name="Source">
            github.com/Blockether/vis, each file checked against its SHA-256
          </Fact>
        </dl>
        <div className="flex flex-wrap items-center gap-x-3 gap-y-2">
          <Button variant="primary" onClick={onInstall}>
            Install Vis
          </Button>
          <Text variant="meta">Vis runs on another machine? Add that machine below.</Text>
        </div>
      </div>
    );
  }

  if (engine.kind === 'working' || engine.kind === 'failed') {
    const retry = { install: onInstall, update: onUpdate, start: onStart }[engine.task];
    return <WorkReport engine={engine} onStop={onStop} onRetry={retry} />;
  }

  const isRunning = engine.kind === 'running';
  const hasUpdate = engine.latest !== undefined && engine.latest !== engine.version;
  const isCurrent = engine.latest === engine.version;
  return (
    <div className="min-w-0 divide-y divide-dialog-edge">
      <div className="flex flex-wrap items-center gap-x-3 gap-y-2 pb-3">
        <span className={`${MARK} ${isRunning ? 'text-ok' : 'text-dialog-hint'}`}>
          {isRunning ? <CircleCheckIcon /> : <CircleDashedIcon />}
        </span>
        <span className="min-w-0 flex-1">
          <Text variant="label" className="block truncate">
            Vis {engine.version}
          </Text>
          <Text variant="meta" className="mt-0.5 block">
            {isRunning
              ? `The gateway runs at ${engine.address} and starts with this app.`
              : 'The gateway does not run.'}
          </Text>
        </span>
        {!isRunning && (
          <Button variant="primary" onClick={onStart}>
            Start the gateway
          </Button>
        )}
      </div>
      <div className="flex flex-wrap items-center gap-x-3 gap-y-2 pt-3">
        <span className={MARK} />
        <span className="min-w-0 flex-1">
          <Text variant="label" className="block truncate">
            {hasUpdate
              ? `Vis ${engine.latest} is available`
              : isCurrent
                ? 'This is the newest release'
                : 'Updates'}
          </Text>
          <Text variant="meta" className="mt-0.5 block">
            {hasUpdate
              ? 'The gateway restarts after the update when no session is at work.'
              : 'An update replaces the engine and restarts its gateway.'}
          </Text>
        </span>
        {hasUpdate ? (
          <Button variant="primary" onClick={onUpdate}>
            Update
          </Button>
        ) : (
          <Button variant="secondary" onClick={onCheck} disabled={isChecking}>
            {isChecking ? 'Checking…' : 'Check for updates'}
          </Button>
        )}
      </div>
    </div>
  );
}
