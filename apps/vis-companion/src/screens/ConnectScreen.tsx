import type { GatewayConn } from '../lib/types';
import { DialogHeader, closeWith } from '../components/ui';
import { AddMachine, MachineRows, useFleetHealth } from '../components/Machines';

interface Props {
  conns: GatewayConn[];
  active: GatewayConn | null;
  primary: GatewayConn | null;
  onAdd: (conn: GatewayConn, makeActive?: boolean) => Promise<void>;
  onSettings: (conn: GatewayConn) => void;
  /** Why the active gateway was dropped, when it stopped answering. */
  offlineError?: string | null;
  /** Retry the active gateway and go back to sessions if it answers. */
  onRetry?: () => void;
  /**
   * Leave the pairing screen. There is no tab bar to walk back through any more:
   * the app bar's cog opens the same machines and the same pairing controls inside
   * Settings, so this screen is what is left when there is nothing to go back TO.
   * Absent while nothing is paired — then this screen IS the app.
   */
  onClose?: () => void;
}

/**
 * THE SCREEN FOR A DEVICE WITH NOWHERE TO GO: nothing paired yet, or the machine
 * it was using stopped answering.
 *
 * It owns no controls of its own. The list and the two ways to pair are
 * `components/Machines`, shared with the cog's Settings dialog, so setting a
 * machine up is the same object wherever it is done — this screen is only the
 * page those pieces stand on when there is no session list to stand beside.
 */
export function ConnectScreen({
  conns,
  active,
  primary,
  onAdd,
  onSettings,
  offlineError,
  onRetry,
  onClose,
}: Props) {
  const health = useFleetHealth(conns, {
    url: active?.url ?? null,
    onRecovered: offlineError ? onRetry : undefined,
  });

  return (
    <div className="mx-auto w-full max-w-[1400px] space-y-5 px-[max(0.75rem,env(safe-area-inset-left))] pb-[max(2rem,env(safe-area-inset-bottom))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-4 transition-[opacity,translate] duration-300 ease-[cubic-bezier(0.22,0.61,0.36,1)] starting:translate-y-1.5 starting:opacity-0 motion-reduce:transition-none sm:space-y-6 sm:px-6 sm:py-6">
      {conns.length > 0 && (
        <section className="overflow-hidden border border-dialog-edge bg-panel shadow-none sm:shadow-[4px_4px_0_var(--dialog-shadow)]">
          <DialogHeader title="Machines" {...closeWith(onClose, 'Close machines')} />
          <div className="border-t border-dialog-edge">
            <MachineRows
              conns={conns}
              selectedUrl={active?.url}
              primaryUrl={primary?.url}
              health={health}
              onPick={onSettings}
              actionLabel="Settings"
            />
          </div>
        </section>
      )}

      {/* No entry transition of its own: this screen already fades in as ONE
          surface (the container above). A nested @starting-style fade multiplies
          with the parent's, and the two cards read as a flicker inside a page
          that is itself still fading in. */}
      <section>
        <div className="mb-3 flex items-center gap-3">
          <h2 className="font-mono text-body font-black uppercase tracking-[0.12em] text-white">
            Add a machine
          </h2>
          <span className="h-px flex-1 bg-dialog-edge" />
        </div>

        <AddMachine onAdd={onAdd} />
      </section>
    </div>
  );
}
