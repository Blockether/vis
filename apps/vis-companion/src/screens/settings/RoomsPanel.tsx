import { useEffect, useState } from 'react';
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import type { RoomInvitation, RoomMember, RoomsStatus } from '../../lib/rooms';
import { Banner, Button, Input, Text } from '../../components/ui';
import { FormLabel, SettingsPanel } from './SettingsLayout';

/** Joining a machine never changes the room selected by a group or session. */
export function RoomsPanel({ client, onChanged }: { client: GatewayClient; onChanged: () => void | Promise<void> }) {
  const [status, setStatus] = useState<RoomsStatus | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);
  const [link, setLink] = useState('');
  const [machineName, setMachineName] = useState('');
  const [review, setReview] = useState<string | null>(null);
  const [roomName, setRoomName] = useState('');
  const [setup, setSetup] = useState(false);
  const [relay, setRelay] = useState('');
  const [adminToken, setAdminToken] = useState('');
  const [invitation, setInvitation] = useState<RoomInvitation | null>(null);
  const [members, setMembers] = useState<{ roomId: string; rows: RoomMember[] } | null>(null);
  const [removal, setRemoval] = useState<{ label: string; run: () => Promise<unknown> } | null>(null);

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

  const reviewInvite = () => {
    try {
      const url = new URL(link);
      if (!machineName.trim() || url.pathname !== '/rooms/join' || !url.hash.startsWith('#invite=')) throw new Error();
      setReview(url.origin);
      setError(null);
    } catch { setError('Enter a complete invite link and a machine name.'); }
  };

  return (
    <SettingsPanel title="Council rooms">
      <div className="space-y-3 px-3 py-3 sm:px-4">
        <Text as="p" variant="description">
          Join this machine, then select a room in group or session Settings. Joining alone shares no sessions.
          The relay operator can read room messages. Room membership does not grant file access.
        </Text>
        {error && <Banner kind="err">{error}</Banner>}
        {status?.machine && <Text as="p" variant="meta">{status.machine.name} · {status.relay_url}</Text>}
        <FormLabel label="Machine name">
          <Input aria-label="Room machine name" value={machineName} maxLength={100} onChange={(event) => { setMachineName(event.target.value); setReview(null); }} />
        </FormLabel>
        <FormLabel label="Invite link">
          <Input type="password" aria-label="Room invite link" autoComplete="off" value={link} onChange={(event) => { setLink(event.target.value); setReview(null); }} />
        </FormLabel>
        <Button density="panel" disabled={busy || !link || !machineName.trim()} onClick={reviewInvite}>Review invitation</Button>
        {review && <div className="space-y-2">
          <Text as="p" variant="description">Join {review} as {machineName.trim()}? No group or session will be shared until you select its room.</Text>
          <Button density="panel" disabled={busy} onClick={() => void run(async () => {
            await client.joinRoom(link, machineName.trim()); setLink(''); setReview(null);
          })}>Confirm join</Button>
          <Button density="panel" variant="secondary" disabled={busy} onClick={() => setReview(null)}>Cancel join</Button>
        </div>}
        {!status?.configured && <>
          <Button density="panel" variant="secondary" disabled={busy} onClick={() => setSetup(!setup)}>Set up room creation</Button>
          {setup && <form className="space-y-2" onSubmit={(event) => {
            event.preventDefault();
            void run(async () => {
              try { await client.registerRooms(relay, machineName.trim(), adminToken); setSetup(false); }
              finally { setAdminToken(''); }
            });
          }}>
            <FormLabel label="Relay URL"><Input aria-label="Rooms relay URL" value={relay} onChange={(event) => setRelay(event.target.value)} placeholder="https://gateway.example.com" /></FormLabel>
            <FormLabel label="Rooms administrator token"><Input aria-label="Rooms administrator token" type="password" autoComplete="off" value={adminToken} onChange={(event) => setAdminToken(event.target.value)} /></FormLabel>
            <Text as="p" variant="description">Use the separate Rooms token, not a Push key. The gateway does not save this token.</Text>
            <Button type="submit" density="panel" disabled={busy || !relay || !machineName.trim() || !adminToken}>Register machine</Button>
          </form>}
        </>}
        {status?.machine?.can_create_rooms && <form className="flex flex-wrap gap-2" onSubmit={(event) => {
          event.preventDefault(); void run(async () => { await client.createRoom(roomName.trim()); setRoomName(''); });
        }}>
          <Input aria-label="New room name" value={roomName} maxLength={100} onChange={(event) => setRoomName(event.target.value)} />
          <Button type="submit" density="panel" disabled={busy || !roomName.trim()}>Create room</Button>
        </form>}
        {status?.rooms.map((room) => {
          const owner = room.owner_machine_id === status.machine?.machine_id;
          return <div key={room.room_id} className="space-y-2 border-t border-dialog-edge pt-3">
            <Text as="p" variant="label">{room.name}</Text>
            <Text as="p" variant="meta" className="break-all">{room.room_id}</Text>
            <div className="flex flex-wrap gap-2">
              <Button density="panel" variant="secondary" disabled={busy} onClick={() => void run(async () => setMembers({ roomId: room.room_id, rows: await client.roomMembers(room.room_id) }))}>Members of {room.name}</Button>
              {owner && <Button density="panel" disabled={busy} onClick={() => void run(async () => setInvitation(await client.inviteToRoom(room.room_id)))}>Invite to {room.name}</Button>}
              <Button density="panel" variant="secondary" disabled={busy} onClick={() => setRemoval({
                label: owner ? `Delete ${room.name} for all members` : `Leave ${room.name}`,
                run: () => owner ? client.deleteRoom(room.room_id) : client.removeRoomMember(room.room_id, status.machine!.machine_id),
              })}>{owner ? 'Delete room' : 'Leave room'}</Button>
            </div>
            {members?.roomId === room.room_id && members.rows.map((member) => <div key={member.machine_id} className="flex flex-wrap items-center gap-2">
              <Text variant="description">{member.name} · {member.role}</Text>
              {owner && member.role !== 'owner' && <Button density="panel" variant="secondary" disabled={busy} onClick={() => setRemoval({ label: `Remove ${member.name} from ${room.name}`, run: () => client.removeRoomMember(room.room_id, member.machine_id) })}>Remove member</Button>}
            </div>)}
          </div>;
        })}
        {invitation && <div className="space-y-2">
          <FormLabel label="Invitation to share" hint={`One use. Expires ${new Date(invitation.invite.expires_at).toLocaleString()}.`}>
            <Input aria-label="Created room invite" readOnly value={invitation.invite_url} onFocus={(event) => event.target.select()} />
          </FormLabel>
          <Button density="panel" variant="secondary" disabled={busy} onClick={() => void run(async () => { await client.revokeRoomInvite(invitation.invite.room_id, invitation.invite.invite_id); setInvitation(null); })}>Revoke invitation</Button>
          <Button density="panel" variant="secondary" onClick={() => setInvitation(null)}>Hide invitation</Button>
        </div>}
        {removal && <div className="space-y-2">
          <Text as="p" variant="description">{removal.label}? Access will stop. This does not delete local sessions.</Text>
          <Button density="panel" disabled={busy} onClick={() => void run(async () => { await removal.run(); setRemoval(null); setMembers(null); })}>Confirm removal</Button>
          <Button density="panel" variant="secondary" disabled={busy} onClick={() => setRemoval(null)}>Cancel removal</Button>
        </div>}
      </div>
    </SettingsPanel>
  );
}
