import { useEffect, useState, type ReactNode } from 'react';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import type { RoomInvitation, RoomMember, RoomsStatus } from '../../lib/rooms';
import { Banner, Button, ConfirmRow, Input, Text } from '../../components/ui';
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

function relayHost(url: string | undefined) {
  if (!url) return 'the relay';
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}

/**
 * This machine's room membership, under the Council settings of machine Settings.
 * Joining a machine never changes the room selected by a group or session.
 */
export function CouncilRooms({ client, onChanged }: { client: GatewayClient; onChanged: () => void | Promise<void> }) {
  const [status, setStatus] = useState<RoomsStatus | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [form, setForm] = useState<'join' | 'setup' | null>(null);
  const [machineName, setMachineName] = useState('');
  const [link, setLink] = useState('');
  const [review, setReview] = useState<string | null>(null);
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
    setAdminToken('');
  };

  const toggleForm = (next: 'join' | 'setup') => {
    const isOpen = form === next;
    closeForm();
    setError(null);
    if (isOpen) return;
    setForm(next);
    if (!name && status?.machine) setMachineName(status.machine.name);
  };

  const reviewInvite = () => {
    try {
      const url = new URL(link);
      if (!name || url.pathname !== '/rooms/join' || !url.hash.startsWith('#invite=')) throw new Error();
      setReview(url.origin);
      setError(null);
    } catch { setError('Enter a complete invite link and a machine name.'); }
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

  const machine = status.machine;
  return (
    <div className="divide-y divide-dialog-edge">
      {error && <div className="px-3 py-3 sm:px-4"><Banner kind="err">{error}</Banner></div>}
      {confirmation?.key === 'machine' ? confirmRow : (
        <RoomRow
          label="Rooms"
          description={machine ? `Connected as ${machine.name} via ${relayHost(status.relay_url)}` : 'Not connected'}
        >
          <Button density="panel" variant="secondary" aria-expanded={form === 'join'} disabled={busy} onClick={() => toggleForm('join')}>
            Join a room
          </Button>
          {status.configured ? (
            <Button density="panel" variant="secondary" disabled={busy} onClick={() => setConfirmation({
              key: 'machine',
              question: `Disconnect ${machine?.name ?? 'this machine'}?`,
              cost: 'The rooms that it owns are deleted. Local sessions stay.',
              confirmLabel: 'Yes, disconnect',
              run: () => client.disconnectRooms(),
            })}>Disconnect</Button>
          ) : (
            <Button density="panel" variant="secondary" aria-expanded={form === 'setup'} disabled={busy} onClick={() => toggleForm('setup')}>
              Create rooms
            </Button>
          )}
        </RoomRow>
      )}
      {form === 'join' && (
        <form className="space-y-3 bg-panel-2 px-3 py-3 sm:px-4" onSubmit={(event) => {
          event.preventDefault();
          if (!review) {
            reviewInvite();
            return;
          }
          void run(async () => {
            await client.joinRoom(link, name);
            closeForm();
          });
        }}>
          {review ? (
            <div className="space-y-1">
              <Text as="p" variant="label" className="break-words">{`Join ${review} as ${name}?`}</Text>
              <Text as="p" variant="description">
                No group or session is shared until you select this room. The relay operator can read room messages.
                Room membership does not grant file access.
              </Text>
            </div>
          ) : (
            <>
              <FormLabel label="Machine name">
                <Input aria-label="Room machine name" value={machineName} maxLength={100} onChange={(event) => setMachineName(event.target.value)} />
              </FormLabel>
              <FormLabel label="Invitation link" hint="Keep it private. It contains the invitation secret.">
                <Input type="password" aria-label="Room invite link" autoComplete="off" value={link} onChange={(event) => setLink(event.target.value)} />
              </FormLabel>
            </>
          )}
          <FormActions>
            <Button type="button" variant="secondary" disabled={busy} onClick={review ? () => setReview(null) : closeForm}>
              {review ? 'Back' : 'Cancel'}
            </Button>
            <Button type="submit" disabled={busy || !link || !name}>{review ? 'Join room' : 'Review invitation'}</Button>
          </FormActions>
        </form>
      )}
      {form === 'setup' && (
        <form className="space-y-3 bg-panel-2 px-3 py-3 sm:px-4" onSubmit={(event) => {
          event.preventDefault();
          void run(async () => {
            try {
              await client.registerRooms(relay, name, adminToken);
              closeForm();
            } finally { setAdminToken(''); }
          });
        }}>
          <FormLabel label="Machine name">
            <Input aria-label="Room machine name" value={machineName} maxLength={100} onChange={(event) => setMachineName(event.target.value)} />
          </FormLabel>
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
          <FormLabel label="Rooms administrator token" hint="Use the Rooms token, not a Push key. The gateway does not save it.">
            <Input aria-label="Rooms administrator token" type="password" autoComplete="off" value={adminToken} onChange={(event) => setAdminToken(event.target.value)} />
          </FormLabel>
          <FormActions>
            <Button type="button" variant="secondary" disabled={busy} onClick={closeForm}>Cancel</Button>
            <Button type="submit" disabled={busy || !relay || !name || !adminToken}>Register machine</Button>
          </FormActions>
        </form>
      )}
      {machine?.can_create_rooms && (
        <form className="flex min-w-0 items-center gap-2 px-3 py-2 sm:px-4" onSubmit={(event) => {
          event.preventDefault();
          void run(async () => {
            await client.createRoom(roomName.trim());
            setRoomName('');
          });
        }}>
          <Input
            aria-label="New room name"
            placeholder="New room name"
            className="flex-1"
            value={roomName}
            maxLength={100}
            onChange={(event) => setRoomName(event.target.value)}
          />
          <Button type="submit" density="panel" variant="secondary" disabled={busy || !roomName.trim()}>Create room</Button>
        </form>
      )}
      {status.rooms.map((room) => {
        const isOwner = room.owner_machine_id === machine?.machine_id;
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
                    run: () => client.removeRoomMember(room.room_id, machine!.machine_id),
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
                <FormLabel label="Invitation link" hint={`One use. Expires ${new Date(invitation.invite.expires_at).toLocaleString()}. Keep it private.`}>
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
}
