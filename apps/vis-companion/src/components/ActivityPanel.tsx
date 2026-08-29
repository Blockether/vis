import { useState } from 'react';
import { BandLabel, Disclosure } from './ui';
import {
  CircleCheckIcon,
  CircleDashedIcon,
  CircleDotIcon,
  CircleSlashIcon,
  CircleXIcon,
  MARK_NUDGE,
} from './icons';
import type { ActivityProjection } from '../lib/activity';

/**
 * ACTIVITY, PAINTED WHERE IT BELONGS — inside the form that produced it.
 *
 * This is NOT a Live View, and it is a separate component for the same reason
 * it is a separate wire shape: a Live View is a surface a run OPENS, addressed
 * by its own id, closed when the run ends; Activity is a field OF one form,
 * replaced whole while the form runs and settled with it. They share a
 * transport and nothing else, so they change on their own schedules.
 *
 * The panel takes the projection itself, never a view: protocol 7 stopped
 * shipping Activity as a classified view addressed from a distance by an
 * anchor, so there is no record to unwrap and no classification to branch on.
 * One slot paints both states — the same component under the same key while the
 * form runs and once it has landed — which is what stops the transcript from
 * swapping a live surface for an artifact under the reader.
 */

const ACTIVITY_FACE = {
  idle: {
    rail: 'border-dialog-hint',
    ink: 'text-dialog-hint',
    mark: <CircleDashedIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Idle',
  },
  running: {
    rail: 'border-accent',
    ink: 'text-accent-ink',
    mark: <CircleDotIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Running',
  },
  succeeded: {
    rail: 'border-ok',
    ink: 'text-ok',
    mark: <CircleCheckIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Done',
  },
  failed: {
    rail: 'border-err',
    ink: 'text-err-ink',
    mark: <CircleXIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Failed',
  },
  cancelled: {
    rail: 'border-dialog-hint',
    ink: 'text-dialog-hint',
    mark: <CircleSlashIcon className={`size-3 ${MARK_NUDGE}`} />,
    label: 'Cancelled',
  },
} as const;

function formatActivityDuration(value?: number): string | null {
  if (value == null || !Number.isFinite(value) || value <= 0) return null;
  const milliseconds = Math.trunc(value);
  if (milliseconds < 1_000) return `${milliseconds}ms`;
  if (milliseconds < 60_000) return `${(milliseconds / 1_000).toFixed(1)}s`;
  const minutes = Math.floor(milliseconds / 60_000);
  return `${minutes}m ${Math.floor((milliseconds % 60_000) / 1_000)}s`;
}

function activityRowSummary(row: ActivityProjection['rows'][number]): string {
  const summary = row.summary.trim();
  const command =
    (row.presenter === 'shell' || row.operation.toLowerCase() === 'shell') &&
    summary.startsWith('running: ')
      ? `cmd: ${summary.slice('running: '.length)}`
      : summary;
  return command.toLowerCase() === row.operation.trim().toLowerCase() ? '' : command;
}

function activityRowLabel(row: ActivityProjection['rows'][number]): string {
  return [row.operation.toUpperCase(), activityRowSummary(row)].filter(Boolean).join(' · ');
}

function activityTotal(activity?: ActivityProjection): number {
  const counts = activity?.counts;
  return counts
    ? counts.running + counts.succeeded + counts.failed + counts.cancelled
    : activity?.rows.length ?? 0;
}

/** The one honest sentence a unified execution trace can state at this moment. */
export function activityReceiptText(
  activity?: ActivityProjection,
  durationMs?: number,
): string {
  const state = activity?.state ?? 'idle';
  const total = activityTotal(activity);
  if (state === 'running' || state === 'idle') {
    const row = activity?.rows.find((candidate) => candidate.state === 'running');
    const focus = row ? activityRowLabel(row) : 'running activity';
    return ['RUNNING', focus, total > 1 || (activity?.omitted.rows ?? 0) > 0 ? 'and more' : '']
      .filter(Boolean)
      .join(' · ');
  }

  const terminal = activity?.counts
    ? activity.counts.succeeded + activity.counts.failed + activity.counts.cancelled
    : activity?.rows.length ?? 0;
  const primary =
    (state === 'failed' && activity?.rows.find((candidate) => candidate.state === 'failed')) ||
    activity?.rows[0];
  const preview = primary
    ? `${primary.operation.toUpperCase()}${terminal > 1 || (activity?.omitted.rows ?? 0) > 0 ? ' and more' : ''}`
    : '';
  const label = state === 'succeeded' ? 'DONE' : state.toUpperCase();
  return [
    label,
    preview,
    `${terminal} ${terminal === 1 ? 'activity' : 'activities'}`,
    formatActivityDuration(durationMs),
  ]
    .filter(Boolean)
    .join(' · ');
}

function activityPreview(activity?: ActivityProjection): string {
  const running = activity?.rows.find((candidate) => candidate.state === 'running');
  if (running) return activityRowLabel(running);
  const primary = activity?.rows[0];
  if (!primary) return 'No operation yet';
  return `${primary.operation.toUpperCase()}${activityTotal(activity) > 1 ? ' and more' : ''}`;
}

function ActivityRail({ activity }: { activity?: ActivityProjection }) {
  const rows = [...(activity?.rows ?? [])].sort((left, right) => left.sequence - right.sequence);

  return (
    <div className="max-h-80 overflow-y-auto overscroll-contain" data-activity-rail>
      <ol aria-label="Invocation chronology">
        {rows.map((row) => {
          const face = ACTIVITY_FACE[row.state];
          const summary = activityRowSummary(row);
          const duration = formatActivityDuration(row.duration_ms);
          return (
            <li
              key={row.id}
              data-activity-row={row.id}
              className="grid min-w-0 grid-cols-[auto_minmax(0,1fr)_auto] items-baseline gap-x-2 border-t border-code-edge bg-result px-2.5 py-1.5 first:border-t-0 font-mono text-meta"
            >
              <span aria-hidden="true" className={face.ink}>{face.mark}</span>
              <span className="min-w-0 break-words text-dialog-hint">
                {[row.operation, summary].filter(Boolean).join(' · ')}
              </span>
              {duration && <span className="text-code-duration">{duration}</span>}
            </li>
          );
        })}
        {rows.length === 0 && (
          <li className="bg-result px-2.5 py-1.5 font-mono text-meta text-dialog-hint">
            No operations yet
          </li>
        )}
      </ol>
    </div>
  );
}

/** One form's Activity: a state face, one honest line, and the rail behind it. */
export function ActivityPanel({
  activity,
  isSettled,
  initiallyExpanded = false,
}: {
  activity?: ActivityProjection;
  isSettled: boolean;
  initiallyExpanded?: boolean;
}) {
  const [expanded, setExpanded] = useState(initiallyExpanded);
  const state = activity?.state ?? 'idle';
  const face = ACTIVITY_FACE[state];
  const preview = activityPreview(activity);

  return (
    <section
      className={`min-w-0 overflow-hidden border-l-2 ${face.rail} bg-result`}
      aria-label="Activity"
      role={isSettled ? undefined : 'status'}
      aria-live={isSettled ? undefined : 'polite'}
    >
      <header className="flex min-h-8 items-center gap-1.5 bg-result px-2">
        <Disclosure
          isOpen={expanded}
          tone="step"
          className="min-w-0 flex-1"
          aria-label={expanded ? 'Collapse Activity' : 'Expand Activity'}
          onClick={() => setExpanded((open) => !open)}
        >
          <span className="flex min-w-0 flex-1 items-baseline gap-2">
            <BandLabel className="shrink-0">ACTIVITY</BandLabel>
            <span
              className={`shrink-0 font-mono text-chip font-bold normal-case tracking-normal ${face.ink}`}
            >
              {face.label}
            </span>
            <span className="min-w-0 flex-1 truncate font-normal tracking-normal text-code-result">
              {preview}
            </span>
          </span>
        </Disclosure>
      </header>
      {expanded && <ActivityRail activity={activity} />}
    </section>
  );
}
