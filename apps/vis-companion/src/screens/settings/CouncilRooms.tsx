import { useEffect, useState, type ReactNode } from 'react';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import type { RoomInvitation, RoomMember, RoomsRelay, RoomsStatus } from '../../lib/rooms';
import { Banner, Button, ConfirmRow, Input, Select, Text } from '../../components/ui';
import { FormLabel } from './SettingsLayout';

/** A destructive room action, asked in place of the row that it changes. */
type Confirmation = {
  /** The row that the question replaces. */
  key: string;
  question: string;
  cost: string;
  confirmLabel: string;
  run: () => Promise<unknown>;
};

/** The New room choice for a relay that this machine does not use yet. */
const OTHER_RELAY = 'other';

/** One row of the Council band: the subject at the start, its actions at the end. */
function RoomRow({ label, description, children }: { label: string; description: string; children?: ReactNode }) {
  return (
    <div className="flex min-w-0 flex-wrap items-center gap-x-4 gap-y-2 px-3 py-2 sm:px-4">
      <div className="min-w-0 flex-1 basis-40">
        <Text as="p" variant="label" className="break-words">{label}</Text>
        <Text as="p" variant="description" className="mt-0.5 break-words">{description}</Text>
      </div>
      {children && <div className="flex flex-wrap gap-2">{children}</div>}
    </div>
  );
}

/** The answers of a form: the way out first, the commitment last. */
function FormActions({ children }: { children: ReactNode }) {
  return <div className="flex flex-wrap justify-end gap-2 border-t border-dialog-edge pt-2">{children}</div>;
}

function relayHost(url: string) {
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}

function relayOrigin(url: string) {
  try {
    return new URL(url).origin;
  } catch {
    return url;
  }
}

function connection(relays: RoomsRelay[]) {
  if (relays.length === 0) return 'Not connected';
  return `Connected to ${relays.length} ${relays.length === 1 ? 'relay' : 'relays'}`;
}

/** The relay that a new room uses first: one where this machine can create rooms, else the first one. */
function defaultRelay(status: RoomsStatus | null) {
  const relays = status?.relays ?? [];
  return (relays.find((relay) => relay.machine.can_create_rooms) ?? relays[0])?.relay_url ?? OTHER_RELAY;
}

/**
 * This machine's room membership, under the Council settings of machine Settings.
 * This machine has one identity on each relay that it uses, and the Machine name setting above
 * names it on each relay. One machine can be in many rooms on many relays. Joining a room never
 * changes the room that a group or session selects.
 */
export function CouncilRooms({ client, machineName, onChanged }: {
  client: GatewayClient;
  /** The Machine name setting. The gateway sends it to each relay. */
  machineName: string;
  onChanged: () => void | Promise<void>;
}) {
  const [status, setStatus] = useState<RoomsStatus | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [form, setForm] = useState<'create' | 'join' | null>(null);
  const [link, setLink] = useState('');
  const [review, setReview] = useState<{ origin: string; isNewRelay: boolean } | null>(null);
  const [relayChoice, setRelayChoice] = useState(OTHER_RELAY);
  const [relay, setRelay] = useState('');
  const [adminToken, setAdminToken] = useState('');
  const [roomName, setRoomName] = useState('');
  const [invitation, setInvitation] = useState<RoomInvitation | null>(null);
  const [members, setMembers] = useState<{ roomId: string; rows: RoomMember[] } | null>(null);
  const [confirmation, setConfirmation] = useState<Confirmation | null>(null);

  useEffect(() => {
    const controller = new AbortController();
    void client.rooms(controller.signal).then((value) => {
      if (!controller.signal.aborted) setStatus(value);
    }).catch(() => {
      if (!controller.signal.aborted) setError('Council rooms are unavailable.');
    });
    return () => controller.abort();
  }, [client]);

  const run = async (work: () => Promise<unknown>) => {
    if (busy) return;
    setBusy(true);
    setError(null);
    try {
      await work();
      setStatus(await client.rooms());
      await onChanged();
    } catch (failure) {
      if (failure instanceof GatewayError && failure.status === 410) {
        setError('This invitation is expired, used or revoked. Ask the owner for a new invitation.');
      } else if (failure instanceof GatewayError && failure.status === 403) {
        setError('This machine does not have permission for that room operation.');
      } else {
        setError('The room operation failed. Check this machine and its room permissions.');
      }
    } finally { setBusy(false); }
  };

  const name = machineName.trim();

  // Secrets leave the screen with the form that asked for them.
  const closeForm = () => {
    setForm(null);
    setReview(null);
    setLink('');
    setRelay('');
    setRoomName('');
    setAdminToken('');
  };

  const toggleForm = (next: 'create' | 'join') => {
    const isOpen = form === next;
    closeForm();
    setError(null);
    if (isOpen) return;
    setRelayChoice(defaultRelay(status));
    setForm(next);
  };

  const reviewInvite = () => {
    try {
      const url = new URL(link);
      if (url.pathname !== '/rooms/join' || !url.hash.startsWith('#invite=')) throw new Error();
      // An invitation from another relay gives this machine a new identity on that relay.
      const isNewRelay = !status?.relays.some((entry) => relayOrigin(entry.relay_url) === url.origin);
      setReview({ origin: url.origin, isNewRelay });
      setError(null);
    } catch { setError('Enter a complete invitation link.'); }
  };

  const confirmRow = confirmation && (
    <ConfirmRow
      question={confirmation.question}
      cost={confirmation.cost}
      confirmLabel={confirmation.confirmLabel}
      isBusy={busy}
      onKeep={() => setConfirmation(null)}
      onConfirm={() => void run(async () => {
        await confirmation.run();
        setConfirmation(null);
        setMembers(null);
      })}
    />
  );

  if (!status) {
    return error ? (
      <div className="px-3 py-3 sm:px-4"><Banner kind="err">{error}</Banner></div>
    ) : (
      <RoomRow label="Rooms" description="Loading…" />
    );
  }

  const chosen = status.relays.find((entry) => entry.relay_url === relayChoice);
  // A new relay, or a relay that this machine joined through an invitation, needs the administrator token once.
  const needsToken = chosen?.machine.can_create_rooms !== true;
  const target = chosen?.relay_url ?? relay.trim();
  return (
    <div className="divide-y divide-dialog-edge">
      {error && <div className="px-3 py-3 sm:px-4"><Banner kind="err">{error}</Banner></div>}
      <RoomRow label="Rooms" description={connection(status.relays)}>
        <Button density="panel" variant="secondary" aria-expanded={form === 'create'} disabled={busy} onClick={() => toggleForm('create')}>
          New room
        </Button>
        <Button density="panel" variant="secondary" aria-expanded={form === 'join'} disabled={busy} onClick={() => toggleForm('join')}>
          Accept invitation
        </Button>
      </RoomRow>
      {form === 'create' && (
        <form className="space-y-3 bg-panel-2 px-3 py-3 sm:px-4" onSubmit={(event) => {
          event.preventDefault();
          void run(async () => {
            try {
              if (needsToken) await client.registerRooms(target, adminToken);
              await client.createRoom(target, roomName.trim());
              closeForm();
            } finally { setAdminToken(''); }
          });
        }}>
          <FormLabel label="Room name">
            <Input aria-label="New room name" value={roomName} maxLength={80} onChange={(event) => setRoomName(event.target.value)} />
          </FormLabel>
          {status.relays.length > 0 && (
            <div className="space-y-1">
              <Text variant="label" className="block">Relay</Text>
              <Select
                aria-label="Relay of the new room"
                value={relayChoice}
                options={[
                  ...status.relays.map((entry) => ({ value: entry.relay_url, label: relayHost(entry.relay_url) })),
                  { value: OTHER_RELAY, label: 'Another relay' },
                ]}
                disabled={busy}
                onValueChange={setRelayChoice}
              />
            </div>
          )}
          {!chosen && (
            <FormLabel label="Relay URL">
              <Input
                aria-label="Rooms relay URL"
                value={relay}
                inputMode="url"
                autoCapitalize="none"
                autoCorrect="off"
                placeholder="https://gateway.example.com"
                onChange={(event) => setRelay(event.target.value)}
              />
            </FormLabel>
          )}
          {needsToken && (
            <FormLabel label="Rooms administrator token" hint="Use the Rooms token, not a Push key. The gateway does not save it.">
              <Input aria-label="Rooms administrator token" type="password" autoComplete="off" value={adminToken} onChange={(event) => setAdminToken(event.target.value)} />
            </FormLabel>
          )}
          <FormActions>
            <Button type="button" variant="secondary" disabled={busy} onClick={closeForm}>Cancel</Button>
            <Button type="submit" disabled={busy || !roomName.trim() || !target || (needsToken && !adminToken)}>
              Create room
            </Button>
          </FormActions>
        </form>
      )}
      {form === 'join' && (
        <form className="space-y-3 bg-panel-2 px-3 py-3 sm:px-4" onSubmit={(event) => {
          event.preventDefault();
          if (!review) {
            reviewInvite();
            return;
          }
          void run(async () => {
            await client.joinRoom(link);
            closeForm();
          });
        }}>
          {review ? (
            <div className="space-y-1">
              <Text as="p" variant="label" className="break-words">
                {name ? `Join ${review.origin} as ${name}?` : `Join ${review.origin}?`}
              </Text>
              {review.isNewRelay && (
                <Text as="p" variant="description">This machine does not use this relay yet. Join only if you trust its operator.</Text>
              )}
              <Text as="p" variant="description">
                No group or session is shared until you select this room. The relay operator can read room messages.
                Room membership does not grant file access.
              </Text>
            </div>
          ) : (
            <FormLabel label="Invitation link" hint="Keep it private. It contains the invitation secret.">
              <Input type="password" aria-label="Room invite link" autoComplete="off" value={link} onChange={(event) => setLink(event.target.value)} />
            </FormLabel>
          )}
          <FormActions>
            <Button type="button" variant="secondary" disabled={busy} onClick={review ? () => setReview(null) : closeForm}>
              {review ? 'Back' : 'Cancel'}
            </Button>
            <Button type="submit" disabled={busy || !link}>{review ? 'Join room' : 'Review invitation'}</Button>
          </FormActions>
        </form>
      )}
      {status.relays.map((entry) => {
        const host = relayHost(entry.relay_url);
        const machine = entry.machine;
        const relayKey = `relay ${entry.relay_url}`;
        return (
          <div key={entry.relay_url} className="divide-y divide-dialog-edge">
            {confirmation?.key === relayKey ? confirmRow : (
              <RoomRow
                label={host}
                description={entry.error ? 'Not available. These rooms are from the last check.' : `Connected as ${machine.name}`}
              >
                <Button density="panel" variant="secondary" aria-label={`Disconnect ${host}`} disabled={busy} onClick={() => setConfirmation({
                  key: relayKey,
                  question: `Disconnect from ${host}?`,
                  cost: 'This machine leaves its rooms on this relay, and the rooms that it owns there are deleted. Local sessions stay.',
                  confirmLabel: 'Yes, disconnect',
                  run: () => client.disconnectRooms(entry.relay_url),
                })}>Disconnect</Button>
              </RoomRow>
            )}
            {entry.rooms.map((room) => {
              const isOwner = room.owner_machine_id === machine.machine_id;
              const shownMembers = members?.roomId === room.room_id ? members.rows : null;
              return (
                <div key={room.room_id} className="divide-y divide-dialog-edge">
                  {confirmation?.key === room.room_id ? confirmRow : (
                    <RoomRow label={room.name} description={isOwner ? 'Owned by this machine' : 'Member'}>
                      <Button
                        density="panel"
                        variant="secondary"
                        aria-label={`Members of ${room.name}`}
                        aria-expanded={shownMembers !== null}
                        disabled={busy}
                        onClick={() => shownMembers ? setMembers(null) : void run(async () => {
                          setMembers({ roomId: room.room_id, rows: await client.roomMembers(room.room_id) });
                        })}
                      >Members</Button>
                      {isOwner && (
                        <Button density="panel" variant="secondary" aria-label={`Invite to ${room.name}`} disabled={busy} onClick={() => void run(async () => {
                          setInvitation(await client.inviteToRoom(room.room_id));
                        })}>Invite</Button>
                      )}
                      <Button
                        density="panel"
                        variant="secondary"
                        aria-label={`${isOwner ? 'Delete' : 'Leave'} ${room.name}`}
                        disabled={busy}
                        onClick={() => setConfirmation(isOwner ? {
                          key: room.room_id,
                          question: `Delete ${room.name}?`,
                          cost: 'Every member loses the room and its messages. Local sessions stay.',
                          confirmLabel: 'Yes, delete',
                          run: () => client.deleteRoom(room.room_id),
                        } : {
                          key: room.room_id,
                          question: `Leave ${room.name}?`,
                          cost: 'This machine loses access to the room. Local sessions stay.',
                          confirmLabel: 'Yes, leave',
                          run: () => client.removeRoomMember(room.room_id, machine.machine_id),
                        })}
                      >{isOwner ? 'Delete' : 'Leave'}</Button>
                    </RoomRow>
                  )}
                  {shownMembers?.map((member) => {
                    const key = `${room.room_id}:${member.machine_id}`;
                    return confirmation?.key === key ? <div key={key}>{confirmRow}</div> : (
                      <div key={key} className="bg-panel-2 ps-4">
                        <RoomRow label={member.name} description={member.role === 'owner' ? 'Owner' : 'Member'}>
                          {isOwner && member.role !== 'owner' && (
                            <Button density="panel" variant="secondary" aria-label={`Remove ${member.name} from ${room.name}`} disabled={busy} onClick={() => setConfirmation({
                              key,
                              question: `Remove ${member.name} from ${room.name}?`,
                              cost: 'That machine loses access to the room.',
                              confirmLabel: 'Yes, remove',
                              run: () => client.removeRoomMember(room.room_id, member.machine_id),
                            })}>Remove</Button>
                          )}
                        </RoomRow>
                      </div>
                    );
                  })}
                  {invitation?.invite.room_id === room.room_id && (
                    <div className="space-y-3 bg-panel-2 px-3 py-3 sm:px-4">
                      <FormLabel label="Invitation link" hint={`${invitation.invite.max_uses === 1 ? 'One use' : `Up to ${invitation.invite.max_uses} uses`}. Expires ${new Date(invitation.invite.expires_at).toLocaleString()}. Keep it private.`}>
                        <Input aria-label="Created room invite" readOnly value={invitation.invite_url} onFocus={(event) => event.target.select()} />
                      </FormLabel>
                      <FormActions>
                        <Button variant="secondary" disabled={busy} onClick={() => void run(async () => {
                          await client.revokeRoomInvite(invitation.invite.room_id, invitation.invite.invite_id);
                          setInvitation(null);
                        })}>Revoke invitation</Button>
                        <Button variant="secondary" onClick={() => setInvitation(null)}>Hide</Button>
                      </FormActions>
                    </div>
                  )}
                </div>
              );
            })}
          </div>
        );
      })}
    </div>
  );
}
