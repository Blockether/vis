import type { ReactNode } from 'react';

import { ListRow } from '../../components/ui';

export type FleetRailEntry = {
  key: string;
  name: string;
  count: number;
  mark?: ReactNode;
  isActive?: boolean;
  onPress: () => void;
};

/**
 * The desktop index beside a multi-machine session list.
 *
 * Its caller decides which domain rows exist; this component owns their one
 * navigation shape, grouping and selected semantics. Compact rows stay compact
 * only for a pointer, through the canonical ListRow density contract.
 */
export function FleetRail({
  machines,
  projects,
  action,
}: {
  machines: FleetRailEntry[];
  projects: FleetRailEntry[];
  action: ReactNode;
}) {
  return (
    <nav
      aria-label="Fleet"
      className="flex w-59 shrink-0 flex-col gap-3 overflow-y-auto overscroll-contain border-b border-l border-t border-dialog-edge bg-panel-2 py-3"
    >
      <RailGroup label="Machines" entries={machines} />
      <RailGroup label="Projects" entries={projects} action={action} />
    </nav>
  );
}

/** One named list in the fleet index; its facts live in the rows below it. */
function RailGroup({
  label,
  entries,
  action,
}: {
  label: string;
  entries: FleetRailEntry[];
  action?: ReactNode;
}) {
  if (entries.length === 0 && !action) return null;
  return (
    <section aria-labelledby={`fleet-rail-${label.toLowerCase()}`}>
      <div className="flex min-h-6 items-center justify-between gap-2 px-3">
        <h2
          id={`fleet-rail-${label.toLowerCase()}`}
          className="font-mono text-chip font-semibold uppercase tracking-[0.08em] text-dialog-hint"
        >
          {label}
        </h2>
        {action}
      </div>
      <ul>
        {entries.map((entry) => (
          <li key={entry.key}>
            <ListRow
              density="compact"
              isSelected={entry.isActive}
              onClick={entry.onPress}
              aria-current={entry.isActive ? 'true' : undefined}
              aria-label={`${entry.name} — ${entry.count} ${entry.count === 1 ? 'session' : 'sessions'}`}
              className="group"
            >
              {entry.mark}
              <span
                className={`min-w-0 flex-1 truncate font-mono text-ui transition-colors duration-150 motion-reduce:transition-none ${
                  entry.isActive
                    ? 'text-white'
                    : 'text-dialog-hint group-hover:text-white group-focus-visible:text-white'
                }`}
              >
                {entry.name}
              </span>
              <span className="shrink-0 font-mono text-ui tabular-nums text-dialog-hint">
                {entry.count}
              </span>
            </ListRow>
          </li>
        ))}
      </ul>
    </section>
  );
}
