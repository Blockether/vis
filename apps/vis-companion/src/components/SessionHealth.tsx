import { useId, useState } from "react";
import { humanizeCount } from "../lib/usage";
import { Disclosure } from "./ui";

/** Presentation data for one persisted request, never the lifetime usage rollup.
 * Optional fields remain unknown for measurements made before they were recorded.
 * Breakdown rows estimate disjoint message text, not disk sizes or image tokens.
 * Missing root read status is unknown, never proof that instructions were not loaded.
 */
export interface SessionHealthSnapshot {
  lastRequestTokens: number;
  budgetTokens?: number;
  reminderTokens?: number;
  modelInputLimit?: number;
  call: number;
  stale?: boolean;
  breakdown?: { label: string; tokens: number; path?: string }[];
  roots?: { path: string; instructionsLoaded?: boolean }[];
}

/** Context pressure, prompt provenance and filesystem access in session metrics. */
export function SessionHealth({
  snapshot,
  folds,
}: {
  snapshot?: SessionHealthSnapshot;
  folds?: number;
}) {
  const [partsOpen, setPartsOpen] = useState(false);
  const [rootsOpen, setRootsOpen] = useState(false);
  const id = useId();
  if (!snapshot)
    return (
      <section aria-label="Session health" className="mb-4">
        <h3 className="text-title font-bold text-white">Session health</h3>
        <p className="mt-2 text-ui text-dialog-hint">
          Context measurement unavailable
        </p>
        <p className="mt-1 text-ui text-dialog-hint">
          Session totals below do not measure context size.
        </p>
      </section>
    );

  const {
    lastRequestTokens: input,
    budgetTokens: budget,
    reminderTokens: reminder,
    modelInputLimit: limit,
    breakdown,
    roots,
  } = snapshot;
  const hasBudget = budget !== undefined && budget > 0;
  const percent = hasBudget ? Math.round((input / budget) * 100) : undefined;
  const atLimit = limit !== undefined && input >= limit;
  const overBudget = hasBudget && input >= budget;
  const reminded = reminder !== undefined && input >= reminder;
  const state = !hasBudget
    ? "Budget not reported"
    : atLimit
      ? "Input limit reached"
      : overBudget
        ? "Over budget"
        : reminded
          ? "Fold reminder"
          : "Within budget";
  const ink =
    atLimit || overBudget ? "text-err" : reminded ? "text-warn" : "text-white";
  const loadedRoots = roots?.filter(
    (item) => item.instructionsLoaded === true,
  ).length;
  const unknownRoots = roots?.filter(
    (item) => item.instructionsLoaded === undefined,
  ).length;

  return (
    <section aria-label="Session health" className="mb-4 font-mono">
      <div className="flex flex-wrap items-baseline justify-between gap-x-3 gap-y-1">
        <h3 className="text-title font-bold text-white">Session health</h3>
        <span className={`text-ui font-bold ${ink}`}>{state}</span>
      </div>
      <p className="mt-1 text-ui text-dialog-hint">
        {snapshot.stale ? "Earlier measurement" : "Last measured call"} · #
        {snapshot.call} · not live
      </p>
      <div className="mt-4 flex flex-wrap items-end justify-between gap-2">
        <div>
          <p className="text-ui text-dialog-hint">Context / working budget</p>
          <p
            className="mt-1 text-head font-bold tabular-nums text-white"
            title={`${input.toLocaleString("en-US")} / ${budget?.toLocaleString("en-US") ?? "unreported budget"} tokens`}
          >
            {humanizeCount(input)}{" "}
            <span className="font-normal text-dialog-hint">
              / {hasBudget ? humanizeCount(budget) : "Not reported"}
            </span>
          </p>
        </div>
        {percent !== undefined && (
          <p className={`text-head font-bold tabular-nums ${ink}`}>
            {percent}%
          </p>
        )}
      </div>
      {hasBudget && (
        <meter
          aria-label="Context budget"
          aria-valuetext={`${input.toLocaleString("en-US")} of ${budget.toLocaleString("en-US")} tokens; ${percent}% of working budget`}
          min={0}
          max={budget}
          value={input}
          low={reminder}
          high={budget}
          optimum={0}
          className={`mt-2 block h-2 w-full appearance-none bg-dialog-edge [&::-webkit-meter-bar]:h-2 [&::-webkit-meter-bar]:rounded-none [&::-webkit-meter-bar]:border-0 [&::-webkit-meter-bar]:bg-none [&::-webkit-meter-bar]:bg-dialog-edge [&::-webkit-meter-optimum-value]:bg-none [&::-webkit-meter-suboptimum-value]:bg-none [&::-webkit-meter-even-less-good-value]:bg-none ${overBudget ? "[&::-webkit-meter-optimum-value]:bg-err [&::-webkit-meter-suboptimum-value]:bg-err [&::-webkit-meter-even-less-good-value]:bg-err [&::-moz-meter-bar]:bg-err" : reminded ? "[&::-webkit-meter-optimum-value]:bg-warn [&::-webkit-meter-suboptimum-value]:bg-warn [&::-webkit-meter-even-less-good-value]:bg-warn [&::-moz-meter-bar]:bg-warn" : "[&::-webkit-meter-optimum-value]:bg-accent [&::-webkit-meter-suboptimum-value]:bg-accent [&::-webkit-meter-even-less-good-value]:bg-accent [&::-moz-meter-bar]:bg-accent"}`}
        />
      )}
      <div className="mt-2 flex flex-wrap justify-between gap-x-3 gap-y-1 text-ui text-dialog-hint">
        <span>
          {reminder === undefined
            ? "Reminder not reported"
            : `Reminder at ${humanizeCount(reminder)}`}
        </span>
        <span>
          {!hasBudget
            ? "Working budget was not recorded"
            : input < budget
              ? `${humanizeCount(budget - input)} budget left`
              : `${humanizeCount(input - budget)} over budget`}
        </span>
      </div>
      <dl className="mt-4 grid grid-cols-2 gap-3 text-ui">
        <div>
          <dt className="text-dialog-hint">Model input limit</dt>
          <dd className="mt-1 font-bold tabular-nums text-white">
            {limit === undefined ? "Not reported" : humanizeCount(limit)}
          </dd>
        </div>
        <div>
          <dt className="text-dialog-hint">Completed folds</dt>
          <dd className="mt-1 font-bold tabular-nums text-white">
            {folds ?? "Not reported"}
          </dd>
        </div>
      </dl>
      <p className="mt-2 text-ui text-dialog-hint">
        Reminder, not an automatic fold.
      </p>

      <div className="mt-4 space-y-2 border-t border-dialog-edge pt-1">
        {breakdown ? (
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
                  Instructions, tools and history · estimates
                </span>
              </span>
            </Disclosure>
            {partsOpen && (
              <div id={`${id}-parts`} className="pb-3">
                <dl className="space-y-3 text-ui">
                  {breakdown.map((part) => (
                    <div key={`${part.label}:${part.path ?? ""}`}>
                      <div className="flex items-baseline justify-between gap-3">
                        <dt className="text-white">{part.label}</dt>
                        <dd className="shrink-0 tabular-nums text-white">
                          ≈{humanizeCount(part.tokens)}
                        </dd>
                      </div>
                      {part.path && (
                        <dd className="mt-0.5 break-all text-dialog-hint">
                          {part.path}
                        </dd>
                      )}
                    </div>
                  ))}
                </dl>
                <p className="mt-3 text-ui text-dialog-hint">
                  ≈ Text estimates at four characters per token; image tokens
                  and provider overhead are excluded. They need not sum to
                  provider-reported input above.
                </p>
              </div>
            )}
          </>
        ) : (
          <p className="py-3 text-ui text-dialog-hint">
            Prompt breakdown unavailable
          </p>
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
                  {roots.length} available ·{" "}
                  {unknownRoots
                    ? `${unknownRoots} with read status not recorded`
                    : `${loadedRoots} with guidance loaded`}
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
                        {item.instructionsLoaded === undefined
                          ? "Instruction read not recorded"
                          : item.instructionsLoaded
                            ? "AGENTS.md loaded"
                            : "Instructions not loaded"}
                      </p>
                    </li>
                  ))}
                </ul>
                <p className="mt-3 text-ui text-dialog-hint">
                  Access does not load repository contents into context. Main
                  workspace guidance is listed above.
                </p>
              </div>
            )}
          </>
        ) : (
          <p className="py-3 text-ui text-dialog-hint">
            Linked filesystem details unavailable
          </p>
        )}
      </div>
    </section>
  );
}
