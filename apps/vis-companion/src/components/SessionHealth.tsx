import { useId, useState } from 'react';
import { humanizeCount } from '../lib/usage';
import type { SessionHealthData } from '../lib/types';
import { Disclosure } from './ui';

/** Context pressure, prompt provenance and filesystem access in session metrics. */
export function SessionHealth({ snapshot }: { snapshot?: SessionHealthData }) {
  const [partsOpen, setPartsOpen] = useState(false);
  const [rootsOpen, setRootsOpen] = useState(false);
  const id = useId();
  if (!snapshot)
    return (
      <section aria-label="Session health" className="mb-4">
        <h3 className="text-title font-bold text-white">Session health</h3>
        <p className="mt-2 text-ui text-dialog-hint">Context measurement unavailable</p>
        <p className="mt-1 text-ui text-dialog-hint">
          Session totals below do not measure context size.
        </p>
      </section>
    );

  const {
    last_request_tokens: input,
    budget_tokens: budget,
    reminder_tokens: reminder,
    breakdown,
    roots,
    estimated_input_tokens: estimatedInput,
    estimate_difference_tokens: difference,
    estimate_difference_percent: differencePercent,
    budget_used_percent: percent,
    budget_used_ratio: ratio,
    budget_remaining_tokens: remaining,
    budget_overage_tokens: overage,
  } = snapshot;
  const isPrepared = snapshot.counted_projection === 'prepared-request';
  const hasBudget = percent !== undefined && budget !== undefined && ratio !== undefined;
  const atLimit = snapshot.budget_state === 'input-limit';
  const overBudget = snapshot.budget_state === 'over-budget' || atLimit;
  const reminded = snapshot.budget_state === 'fold-reminder';
  const state =
    {
      'budget-unreported': 'Budget not reported',
      'within-budget': 'Within budget',
      'fold-reminder': 'Fold reminder',
      'over-budget': 'Over budget',
      'input-limit': 'Input limit reached',
    }[snapshot.budget_state] ?? 'Budget not reported';
  const ink = overBudget ? 'text-err' : reminded ? 'text-warn' : 'text-white';

  return (
    <section aria-label="Session health" className="mb-4 font-mono">
      <div className="flex flex-wrap items-baseline justify-between gap-x-3 gap-y-1">
        <h3 className="text-title font-bold text-white">Session health</h3>
        <span className={`text-ui font-bold ${ink}`}>{state}</span>
      </div>
      <p className="mt-1 text-ui text-dialog-hint">
        {snapshot.stale ? 'Earlier measurement' : 'Last measured call'} · #{snapshot.call}
      </p>
      <div className="mt-4 flex flex-wrap items-end justify-between gap-2">
        <div>
          <p className="text-ui text-dialog-hint">Context / working budget</p>
          <p
            className="mt-1 text-head font-bold tabular-nums text-white"
            title={`${input.toLocaleString('en-US')} / ${budget?.toLocaleString('en-US') ?? 'unreported budget'} tokens`}
          >
            {humanizeCount(input)}{' '}
            <span className="font-normal text-dialog-hint">
              / {hasBudget ? humanizeCount(budget) : 'Not reported'}
            </span>
          </p>
        </div>
        {percent !== undefined && (
          <p className={`text-head font-bold tabular-nums ${ink}`}>{percent}%</p>
        )}
      </div>
      {hasBudget && (
        <meter
          aria-label="Context budget"
          aria-valuetext={`${input.toLocaleString('en-US')} of ${budget.toLocaleString('en-US')} tokens; ${percent}% of working budget`}
          min={0}
          max={1}
          value={ratio}
          optimum={0}
          className={`mt-2 block h-2 w-full appearance-none bg-dialog-edge [&::-webkit-meter-bar]:h-2 [&::-webkit-meter-bar]:rounded-none [&::-webkit-meter-bar]:border-0 [&::-webkit-meter-bar]:bg-none [&::-webkit-meter-bar]:bg-dialog-edge [&::-webkit-meter-optimum-value]:bg-none [&::-webkit-meter-suboptimum-value]:bg-none [&::-webkit-meter-even-less-good-value]:bg-none ${overBudget ? '[&::-webkit-meter-optimum-value]:bg-err [&::-webkit-meter-suboptimum-value]:bg-err [&::-webkit-meter-even-less-good-value]:bg-err [&::-moz-meter-bar]:bg-err' : reminded ? '[&::-webkit-meter-optimum-value]:bg-warn [&::-webkit-meter-suboptimum-value]:bg-warn [&::-webkit-meter-even-less-good-value]:bg-warn [&::-moz-meter-bar]:bg-warn' : '[&::-webkit-meter-optimum-value]:bg-accent [&::-webkit-meter-suboptimum-value]:bg-accent [&::-webkit-meter-even-less-good-value]:bg-accent [&::-moz-meter-bar]:bg-accent'}`}
        />
      )}
      <div className="mt-2 flex flex-wrap justify-between gap-x-3 gap-y-1 text-ui text-dialog-hint">
        <span>
          {reminder === undefined
            ? 'Reminder not reported'
            : `Reminder at ${humanizeCount(reminder)}`}
        </span>
        <span>
          {remaining !== undefined
            ? `${humanizeCount(remaining)} budget left`
            : overage !== undefined
              ? `${humanizeCount(overage)} over budget`
              : 'Working budget was not recorded'}
        </span>
      </div>

      <div className="mt-4 space-y-2 border-t border-dialog-edge pt-1">
        {estimatedInput !== undefined ? (
          <>
            <Disclosure
              tone="branch"
              isOpen={partsOpen}
              aria-controls={`${id}-parts`}
              onClick={() => setPartsOpen(!partsOpen)}
            >
              <span className="min-w-0 py-2.5">
                <span className="block">Context breakdown</span>
                <span className="block text-ui font-normal text-dialog-hint">
                  {isPrepared ? 'Prepared request' : 'Logical request'} · not measured usage
                </span>
              </span>
            </Disclosure>
            {partsOpen && (
              <div id={`${id}-parts`} className="pb-3">
                <dl className="mb-4 space-y-2 text-ui">
                  <div className="flex flex-wrap items-baseline justify-between gap-x-3 gap-y-1">
                    <dt className="text-dialog-hint">Local estimate</dt>
                    <dd className="tabular-nums text-white">
                      {estimatedInput.toLocaleString('en-US')} tokens
                    </dd>
                  </div>
                  <div className="flex flex-wrap items-baseline justify-between gap-x-3 gap-y-1">
                    <dt className="text-dialog-hint">Provider-reported input</dt>
                    <dd className="tabular-nums text-white">
                      {input.toLocaleString('en-US')} tokens
                    </dd>
                  </div>
                  {difference !== undefined && (
                    <div className="flex flex-wrap items-baseline justify-between gap-x-3 gap-y-1">
                      <dt className="text-dialog-hint">Estimate − reported</dt>
                      <dd className="tabular-nums text-white">
                        {difference
                          .toLocaleString('en-US', {
                            signDisplay: 'exceptZero',
                          })
                          .replace('-', '−')}{' '}
                        tokens
                        {differencePercent !== undefined &&
                          ` (${differencePercent.toLocaleString('en-US', { signDisplay: difference === 0 ? 'never' : 'always', minimumFractionDigits: 1, maximumFractionDigits: 1 }).replace('-', '−')}%)`}
                      </dd>
                    </div>
                  )}
                </dl>
                <dl className="space-y-3 text-ui">
                  {breakdown?.map((part) => (
                    <div
                      key={`${part.label}:${part.path ?? ''}`}
                      className="grid grid-cols-[minmax(0,1fr)_auto] items-baseline gap-x-3"
                    >
                      <dt className="text-white">{part.label}</dt>
                      <dd className="tabular-nums text-white">≈{humanizeCount(part.tokens)}</dd>
                      {part.path && (
                        <dd className="col-span-2 mt-0.5 break-all text-dialog-hint">
                          {part.path}
                        </dd>
                      )}
                    </div>
                  ))}
                </dl>
                <p className="mt-3 text-ui text-dialog-hint">
                  {isPrepared
                    ? '≈ Local estimates describe the full prepared request after provider adaptation, including retained replay and wire-shaped tools, not just a WebSocket delta. '
                    : '≈ Local estimates describe logical messages and tools before provider adaptation, not the prepared request used for preflight. Adapters may drop or reshape content. '}
                  Svar tokenizes text and tool payloads and estimates images, reasoning and framing.
                  Provider-reported input for this same call, including cached input, determines
                  context pressure above; estimates do not.
                </p>
              </div>
            )}
          </>
        ) : (
          <p className="py-3 text-ui text-dialog-hint">Prompt breakdown unavailable</p>
        )}

        {roots ? (
          <>
            <Disclosure
              tone="branch"
              isOpen={rootsOpen}
              aria-controls={`${id}-roots`}
              onClick={() => setRootsOpen(!rootsOpen)}
            >
              <span className="min-w-0 py-2.5">
                <span className="block">Linked filesystems</span>
                <span className="block text-ui font-normal text-dialog-hint">
                  {snapshot.root_count ?? '—'} available · {snapshot.estimated_root_count ?? '—'}{' '}
                  with guidance estimates
                </span>
              </span>
            </Disclosure>
            {rootsOpen && (
              <div id={`${id}-roots`} className="pb-3">
                <ul className="space-y-3 text-ui">
                  {roots.map((item) => (
                    <li key={item.path}>
                      <p className="break-all text-white">{item.path}</p>
                      <p className="mt-0.5 text-dialog-hint">
                        {item.guidance?.status === 'available'
                          ? `${item.guidance.path.split('/').pop()} · ≈${humanizeCount(item.guidance.tokens)} tokens on disk`
                          : item.guidance?.status === 'missing'
                            ? 'No AGENTS.md or CLAUDE.md'
                            : item.guidance?.status === 'error'
                              ? 'Could not read guidance · check file access'
                              : 'Guidance estimate unavailable'}
                      </p>
                    </li>
                  ))}
                </ul>
                <p className="mt-3 text-ui text-dialog-hint">
                  Disk estimates do not add to context usage or imply that the agent loaded the
                  file. Main workspace guidance is listed above.
                </p>
              </div>
            )}
          </>
        ) : (
          <p className="py-3 text-ui text-dialog-hint">Linked filesystem details unavailable</p>
        )}
      </div>
    </section>
  );
}
