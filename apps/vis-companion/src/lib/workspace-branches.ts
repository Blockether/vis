import { useCallback, useEffect, useState } from 'react';

import type { GatewayClient } from './gateway';
import type { WorkspaceBranches } from './types';

/**
 * The branches of a session's workspace and the switch between them. It reads
 * them again when the root changes and when a turn ends, because the agent can
 * switch the branch itself. `choose` throws the gateway's error to the caller.
 */
export function useWorkspaceBranches(
  client: GatewayClient,
  sid: string,
  root: string | undefined,
  isRunning: boolean,
) {
  const [branches, setBranches] = useState<WorkspaceBranches | null>(null);
  const [isSwitching, setSwitching] = useState(false);

  const refresh = useCallback(
    async (signal?: AbortSignal) => {
      try {
        setBranches(await client.workspaceBranches(sid, signal));
      } catch {
        if (!signal?.aborted) setBranches(null);
      }
    },
    [client, sid],
  );

  useEffect(() => {
    if (!root || isRunning) return;
    const controller = new AbortController();
    void refresh(controller.signal);
    return () => controller.abort();
  }, [refresh, root, isRunning]);

  const choose = useCallback(
    async (branch: string) => {
      setSwitching(true);
      try {
        setBranches(await client.switchBranch(sid, branch));
      } finally {
        setSwitching(false);
      }
    },
    [client, sid],
  );

  return { branches, isSwitching, refresh, choose };
}
