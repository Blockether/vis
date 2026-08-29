import type { Compat } from '../lib/compat';
import type { GatewayConn } from '../lib/types';
import { BandLabel, Button } from '../components/ui';

interface Props {
  compat: Compat;
  conn: GatewayConn;
}

/**
 * The one screen a user sees when this app and the machine it talks to no longer
 * speak the same wire protocol. It replaces the whole UI on purpose: a half-working
 * session that silently drops fields is worse than an honest stop.
 *
 * It answers the three questions in order — WHAT is mismatched (two protocol
 * numbers side by side, the stale one flagged), WHY it matters (one sentence), and
 * WHAT to do (the remedy from `compat`, the same text the TUI prints).
 *
 * THE TWO NUMBERS LEAD, because they are the whole fact. The first draft gave the
 * title, an amber slab, two version cards and a remedy list one weight each — five
 * things sharing the eye, which is the defect `doc("design")` §0 names — and buried
 * "6" and "7" inside a `dl` of six label/value rows. Here the screen's own name
 * takes `text-display` (§1: one per screen), the protocols take `text-head` in mono
 * because a protocol is a fact the machine produced, and everything else is body.
 * Four steps, which is the ceiling for one screen.
 *
 * THE REMEDY IS NOT NUMBERED. Reloading the page and updating from the store are
 * ALTERNATIVES, not a sequence, and §13 only allows an ordinal where order is a
 * fact the reader needs — an `<ol>` here told the reader to do step 1 and then
 * step 2, which would be two updates for one mismatch.
 *
 * Its planes stay square. A radius is a promise a thing can be pressed (index.css),
 * so the one control on the screen carries one and the bands under it do not.
 */
export function IncompatibleScreen({ compat, conn }: Props) {
  const appStale = compat.upgrade === 'client';
  const gatewayStale = compat.upgrade === 'gateway';

  return (
    <div className="mx-auto flex w-full max-w-3xl flex-col gap-5 px-[max(0.75rem,env(safe-area-inset-left))] pb-[max(2rem,env(safe-area-inset-bottom))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-6 transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:gap-6 sm:px-6 sm:py-8">
      <header>
        <h1 className="text-display font-black text-white">{compat.title}</h1>
        <p className="mt-2 text-body text-footer-strong">{compat.summary}</p>
        <p className="mt-3 truncate font-mono text-chip text-footer-muted">
          {conn.label ?? conn.url}
        </p>
      </header>

      <section
        className="grid grid-cols-2 border border-edge bg-panel"
        aria-label="Protocol versions"
        role="group"
      >
        <ProtocolHalf
          role="This app"
          name="vis-companion"
          version={compat.clientVersion}
          protocol={compat.clientProtocol}
          requires={compat.clientMinGateway}
          requiresOf="machine"
          isStale={appStale}
        />
        <ProtocolHalf
          role="Machine"
          name={hostOf(conn.url)}
          version={compat.gatewayVersion ?? 'unknown'}
          protocol={compat.gatewayProtocol}
          requires={compat.gatewayMinClient}
          requiresOf="client"
          isStale={gatewayStale}
          isTrailing
        />
      </section>

      {compat.remedy.length > 0 && (
        <section>
          <BandLabel>How to fix it</BandLabel>
          <ul className="mt-2 divide-y divide-edge border-y border-edge">
            {compat.remedy.map((step) => (
              <li key={step} className="break-words px-1 py-2.5 text-body text-white">
                {step}
              </li>
            ))}
          </ul>
        </section>
      )}

      {appStale && (
        <footer>
          <Button type="button" variant="primary" onClick={() => window.location.reload()}>
            Reload app
          </Button>
        </footer>
      )}
    </div>
  );
}

/**
 * ONE SIDE OF THE COMPARISON. The protocol number is the biggest thing in it — the
 * reader is here to see two integers disagree — and the name, version and floor
 * stand under it as the evidence for which side has to move.
 */
function ProtocolHalf({
  role,
  name,
  version,
  protocol,
  requires,
  requiresOf,
  isStale,
  isTrailing = false,
}: {
  role: string;
  name: string;
  version: string;
  protocol: number | null;
  requires: number | null;
  requiresOf: 'machine' | 'client';
  isStale: boolean;
  isTrailing?: boolean;
}) {
  return (
    <div
      className={`min-w-0 p-3 ${isTrailing ? 'border-l border-edge' : ''} ${
        isStale ? 'bg-warn-surface' : ''
      }`}
    >
      <div className="flex min-h-4 items-baseline justify-between gap-2">
        <span className="truncate font-mono text-chip font-extrabold uppercase tracking-[0.06em] text-footer-muted">
          {role}
        </span>
        {isStale && (
          <span className="shrink-0 font-mono text-chip font-extrabold uppercase tracking-[0.06em] text-warn">
            Old
          </span>
        )}
      </div>

      <p
        className={`mt-1 font-mono text-head font-black tabular-nums ${
          isStale ? 'text-warn' : 'text-white'
        }`}
      >
        {protocol != null ? protocol : '—'}
      </p>

      <p className="mt-1 truncate text-body text-white">{name}</p>
      <p className="truncate font-mono text-chip text-footer-muted">{version}</p>
      {requires != null && (
        <p className="mt-2 font-mono text-chip text-footer-muted">
          needs {requiresOf} ≥ {requires}
        </p>
      )}
    </div>
  );
}

function hostOf(url: string): string {
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}
