import { useCallback, useState, useSyncExternalStore } from 'react';
import type { GatewayClient } from '../lib/gateway';
import type { LiveView } from '../lib/live-view';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import { Button } from './ui';

export function BackgroundTasks({ client, subscriptions, sid, views, onOpenView }: {
  client: GatewayClient;
  subscriptions: SessionSubscriptionHub;
  sid: string;
  views: readonly LiveView[];
  onOpenView: (view: LiveView) => void;
}) {
  const subscribe = useCallback((notify: () => void) => subscriptions.subscribeSession(sid, notify, { replay: false }), [subscriptions, sid]);
  const snapshot = useCallback(() => subscriptions.tasks(sid), [subscriptions, sid]);
  const tasks = useSyncExternalStore(subscribe, snapshot);
  const [pending, setPending] = useState<ReadonlySet<string>>(() => new Set());
  const [error, setError] = useState<string | null>(null);

  async function cancel(id: string, name: string) {
    setError(null);
    setPending((current) => new Set(current).add(id));
    try {
      if (!await client.cancelTask(sid, id)) {
        if (subscriptions.tasks(sid)?.some((task) => task.id === id && !task.cancel_reason)) {
          setError(`Could not cancel ${name}.`);
        }
      }
    } catch {
      setError(`Could not cancel ${name}.`);
    } finally {
      setPending((current) => {
        const next = new Set(current);
        next.delete(id);
        return next;
      });
    }
  }

  if (!tasks?.length && !error) return null;
  return (
    <section aria-label="Background tasks" className="mb-1.5 border border-dialog-edge bg-panel px-2.5 py-1">
      <h2 className="font-mono text-meta font-bold">Background</h2>
      <ul className="max-h-32 overflow-y-auto">
        {tasks?.map((task) => {
          const cancelling = pending.has(task.id) || !!task.cancel_reason;
          return (
            <li key={task.id} className="flex flex-wrap items-center gap-x-2">
              <span className="min-w-0 flex-1 break-words text-meta">{task.name}</span>
              {views.filter((view) => task.view_ids.includes(view.id)).map((view) => (
                <Button key={view.id} variant="secondary" density="compact" onClick={() => onOpenView(view)}>
                  {view.title}
                </Button>
              ))}
              <Button variant="secondary" density="compact" aria-label={`Cancel ${task.name}`} disabled={cancelling}
                onClick={() => void cancel(task.id, task.name)}>
                {cancelling ? 'Cancelling…' : 'Cancel'}
              </Button>
            </li>
          );
        })}
      </ul>
      {error && <p role="alert" className="text-meta text-err">{error}</p>}
    </section>
  );
}
