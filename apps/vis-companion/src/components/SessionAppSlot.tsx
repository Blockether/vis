/**
 * A session app opened from the session apps list, in the slot that its session
 * surface names: the side panel, the chat area or, on a phone, the whole pane.
 */

import type { GatewayClient } from '../lib/gateway';
import type { SessionAppSlot as Slot } from '../lib/app-surfaces';
import type { LiveView } from '../lib/live-view';
import { AppSurface } from './AppAttachment';
import { ResizablePanel } from './LivePanel';

export function SessionAppSlot({
  client,
  sid,
  view,
  slot,
  onClose,
}: {
  client: GatewayClient;
  sid: string;
  view: LiveView;
  slot: Slot;
  onClose: () => void;
}) {
  const surface = (back?: string) => (
    <AppSurface
      key={view.id}
      client={client}
      sid={sid}
      viewId={view.id}
      fallbackTitle={view.title}
      isPane
      backLabel={back}
      onClose={onClose}
    />
  );
  if (slot === 'sidebar') {
    return (
      <ResizablePanel>
        <div className="flex h-full min-h-0 flex-col">{surface()}</div>
      </ResizablePanel>
    );
  }
  // The chat slot covers the transcript, which stays mounted under it, so Back
  // returns to the same reading position. The phone sheet covers the whole pane.
  return (
    <div
      className={`absolute inset-0 flex flex-col bg-panel ${
        slot === 'sheet' ? 'z-30 pt-[env(safe-area-inset-top)]' : 'z-20'
      }`}
    >
      {surface('Back to chat')}
    </div>
  );
}
