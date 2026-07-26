import type { Compat } from '../lib/compat';
import type { GatewayConn } from '../lib/types';
import { Button } from '../components/ui';

interface Props {
  compat: Compat;
  conn: GatewayConn;
  onRetry: () => void;
  onBack: () => void;
  isChecking?: boolean;
}

/**
 * The one screen a user sees when this app and its gateway no longer speak the
 * same wire protocol. It replaces the whole UI on purpose: a half-working
 * session that silently drops fields is worse than an honest stop.
 *
 * It answers the three questions in order — WHAT is mismatched (two versions
 * side by side, the stale one flagged), WHY it matters (one sentence), and WHAT
 * to do (numbered remedy from `compat`, the same text the TUI prints).
 */
export function IncompatibleScreen({ compat, conn, onRetry, onBack, isChecking = false }: Props) {
  const appStale = compat.upgrade === 'client';
  const gatewayStale = compat.upgrade === 'gateway';

  return (
    <div className="mx-auto flex w-full max-w-3xl flex-col gap-5 px-[max(0.75rem,env(safe-area-inset-left))] pb-[max(2rem,env(safe-area-inset-bottom))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-4 transition-[opacity,transform] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:gap-6 sm:px-6 sm:py-6">
      <header className="border-b border-dialog-edge pb-3">
        <p className="font-mono text-chip font-black uppercase tracking-[0.18em] text-warn">
          Version mismatch
        </p>
        <h1 className="mt-1 font-mono text-subhead font-black text-white">{compat.title}</h1>
        <p className="mt-1 truncate font-mono text-chip text-dialog-hint">{conn.label ?? conn.url}</p>
      </header>

      <section
        className="border border-warn-strong/60 bg-warn-surface px-3 py-2 font-mono text-body text-warn"
        role="alert"
      >
        {compat.summary}
      </section>

      <section className="grid grid-cols-1 gap-3 sm:grid-cols-2">
        <VersionCard
          role="This app"
          name="vis-companion"
          version={compat.clientVersion}
          protocol={compat.clientProtocol}
          requires={`gateway protocol ≥ ${compat.clientMinGateway}`}
          isStale={appStale}
        />
        <VersionCard
          role="Gateway"
          name={hostOf(conn.url)}
          version={compat.gatewayVersion ?? 'unknown'}
          protocol={compat.gatewayProtocol}
          requires={
            compat.gatewayMinClient != null ? `client protocol ≥ ${compat.gatewayMinClient}` : null
          }
          isStale={gatewayStale}
        />
      </section>

      {compat.remedy.length > 0 && (
        <section className="border border-dialog-edge bg-panel">
          <header className="flex min-h-9 items-center bg-dialog-title px-3 py-2 text-dialog-title-foreground">
            <h2 className="font-mono text-body font-black uppercase tracking-[0.12em]">
              How to fix it
            </h2>
          </header>
          <ol className="divide-y divide-dialog-edge border-t border-dialog-edge">
            {compat.remedy.map((step, i) => (
              <li key={step} className="flex items-start gap-3 px-3 py-2.5">
                <span className="shrink-0 font-mono text-body font-black text-accent-ink">
                  {i + 1}
                </span>
                <span className="min-w-0 flex-1 break-words font-mono text-body text-white">
                  {step}
                </span>
              </li>
            ))}
          </ol>
        </section>
      )}

      <footer className="flex flex-wrap items-center gap-2">
        <Button type="button" onClick={onRetry} disabled={isChecking}>
          {isChecking ? 'Checking…' : 'Check again'}
        </Button>
        {appStale && (
          <Button type="button" variant="ghost" onClick={() => window.location.reload()}>
            Reload app
          </Button>
        )}
        <Button type="button" variant="ghost" onClick={onBack}>
          Choose another gateway
        </Button>
      </footer>
    </div>
  );
}

function VersionCard({
  role,
  name,
  version,
  protocol,
  requires,
  isStale,
}: {
  role: string;
  name: string;
  version: string;
  protocol: number | null;
  requires: string | null;
  isStale: boolean;
}) {
  return (
    <div
      className={`border bg-panel p-3 ${isStale ? 'border-warn-strong/60 bg-warn-surface' : 'border-dialog-edge'}`}
    >
      <div className="flex items-center justify-between gap-2">
        <span className="font-mono text-chip font-black uppercase tracking-[0.14em] text-dialog-hint">
          {role}
        </span>
        {isStale && (
          <span className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-warn">
            Out of date
          </span>
        )}
      </div>
      <p className="mt-1 truncate font-mono text-title font-bold text-white">{name}</p>
      <dl className="mt-2 space-y-1">
        <Row label="Version" value={version} />
        <Row label="Protocol" value={protocol != null ? String(protocol) : 'not advertised'} />
        {requires && <Row label="Needs" value={requires} />}
      </dl>
    </div>
  );
}

function Row({ label, value }: { label: string; value: string }) {
  return (
    <div className="flex items-baseline justify-between gap-3">
      <dt className="shrink-0 font-mono text-chip uppercase tracking-wider text-dialog-hint">
        {label}
      </dt>
      <dd className="min-w-0 truncate font-mono text-ui text-white">{value}</dd>
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
