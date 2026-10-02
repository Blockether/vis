import { useCallback, useEffect, useRef, useState } from 'react';
import type { GatewayClient } from '../../lib/gateway';
import type { SettingsTarget } from '../../lib/types';
import { DialogFrame, Modal } from '../../components/ui';
import { McpServersPanel } from './MachineSettings';
import { SettingsEditor, type SettingsLeaveGuard } from './SettingsEditor';

type ScopedSettingsProps = { client: GatewayClient; target: SettingsTarget; onClose: () => void };

export function ScopedSettingsDialog(props: ScopedSettingsProps) {
  return (
    <ScopedSettingsContent
      key={`${props.target.scope}:${props.target.target_id ?? ''}`}
      {...props}
    />
  );
}

function ScopedSettingsContent({ client, target: initialTarget, onClose }: ScopedSettingsProps) {
  const [target, setTarget] = useState(initialTarget);
  const guard = useRef<SettingsLeaveGuard | null>(null);
  const setGuard = useCallback((next: SettingsLeaveGuard | null) => {
    guard.current = next;
  }, []);
  const close = useCallback(() => {
    if (guard.current) guard.current(onClose);
    else onClose();
  }, [onClose]);
  useEffect(() => {
    const escape = (event: KeyboardEvent) => {
      if (event.key === 'Escape' && !event.defaultPrevented) {
        event.preventDefault();
        close();
      }
    };
    window.addEventListener('keydown', escape);
    return () => window.removeEventListener('keydown', escape);
  }, [close]);
  return (
    <Modal size="full" onDismiss={close}>
      <DialogFrame
        title={`${target.scope[0].toUpperCase()}${target.scope.slice(1)} settings`}
        subtitle={target.label ?? target.target_id}
        onClose={close}
      >
        <SettingsEditor
          key={`${target.scope}:${target.target_id ?? ''}`}
          client={client}
          target={target}
          onLeaveGuard={setGuard}
          onTargetChange={setTarget}
          resources={(category, search) =>
            category === 'all' ||
            category === 'tools' ||
            (!!search && 'mcp servers tools integrations'.includes(search.toLowerCase())) ? (
              <McpServersPanel client={client} target={target} />
            ) : null
          }
        />
      </DialogFrame>
    </Modal>
  );
}
