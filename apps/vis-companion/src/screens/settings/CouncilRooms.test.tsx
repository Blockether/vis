// @vitest-environment jsdom
import { cleanup, fireEvent, screen, waitFor, within } from '@testing-library/react';
import { renderOpenBands } from '../../test-settings';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';

afterEach(() => { cleanup(); vi.restoreAllMocks(); });
import { GatewayClient, GatewayError } from '../../lib/gateway';
import type { RoomsRelay, RoomsStatus } from '../../lib/rooms';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import { CouncilRooms } from './CouncilRooms';
import { MachineSettings, lockNote } from './MachineSettings';

const RELAY: RoomsRelay = {
  relay_url: 'https://gateway.example.com',
  machine: { machine_id: 'machine', name: 'Laptop', can_create_rooms: true, created_at: 1 },
  rooms: [{ room_id: 'room', name: 'Builds', owner_machine_id: 'machine', created_at: 1 }],
};

const CONFIGURED: RoomsStatus = { configured: true, relays: [RELAY] };

const NOT_CONNECTED: RoomsStatus = { configured: false, relays: [] };

function fixture(status: RoomsStatus = CONFIGURED, machineName = 'Laptop') {
  const client = { rooms: vi.fn().mockResolvedValue(status), joinRoom: vi.fn().mockResolvedValue({}),
    registerRooms: vi.fn().mockResolvedValue(status), createRoom: vi.fn().mockResolvedValue({}),
    deleteRoom: vi.fn().mockResolvedValue({}), removeRoomMember: vi.fn().mockResolvedValue({}),
    inviteToRoom: vi.fn(), revokeRoomInvite: vi.fn().mockResolvedValue({}),
    disconnectRooms: vi.fn().mockResolvedValue(NOT_CONNECTED), setSetting: vi.fn() };
  renderOpenBands(<CouncilRooms client={client as unknown as GatewayClient} machineName={machineName} onChanged={vi.fn()} />);
  return client;
}

async function reviewInvitation(link: string) {
  fireEvent.click(await screen.findByRole('button', { name: 'Accept invitation' }));
  fireEvent.change(screen.getByLabelText('Room invite link'), { target: { value: link } });
  fireEvent.click(screen.getByRole('button', { name: 'Review invitation' }));
}

async function createRoom(name: string, token?: string) {
  fireEvent.click(await screen.findByRole('button', { name: 'New room' }));
  fireEvent.change(screen.getByLabelText('New room name'), { target: { value: name } });
  if (token) fireEvent.change(screen.getByLabelText('Rooms administrator token'), { target: { value: token } });
}

const LINK = `https://gateway.example.com/rooms/join#invite=${'a'.repeat(43)}`;

const TRUST_NOTE = 'This machine does not use this relay yet. Join only if you trust its operator.';

describe('Council room Settings', () => {
  it('stands under the Council switch of machine Settings, after the machine name', async () => {
    vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
    vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
      revision: 'council-1',
      groups: [{ id: 'general', title: 'General', toggles: [
        { id: 'council', label: 'Council', type: 'boolean', enabled: true, children: [
          { id: 'council_machine_name', label: 'Machine name', type: 'string', value: 'Workstation', max_length: 80,
            editor: 'text', scopes: ['global'], parent: 'council' },
        ] },
      ] }],
    });
    vi.spyOn(GatewayClient.prototype, 'rooms').mockResolvedValue(NOT_CONNECTED);
    renderOpenBands(
      <MachineSettings
        gateway={{ id: 'rooms-test', url: 'http://127.0.0.1:7890', token: 'test' }}
        speechPrefs={DEFAULT_SPEECH_PREFS}
        onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
      />,
    );
    const band = (await screen.findByRole('heading', { name: 'General' })).closest('section')!;
    expect(await within(band).findByText('Not connected')).toBeVisible();
    expect(within(band).getByRole('switch', { name: 'Council: on' })).toBeVisible();
    expect(within(band).getByRole('textbox', { name: 'Machine name' })).toHaveValue('Workstation');
    expect(screen.queryByRole('heading', { name: 'Council rooms' })).toBeNull();
    // The rows under Council stand one step in: the machine name, then the Rooms panel.
    const nested = within(band).getByRole('textbox', { name: 'Machine name' }).closest('.ps-3') as HTMLElement;
    expect(within(nested).getByText('Not connected')).toBeVisible();
    expect(within(nested).queryByRole('switch', { name: 'Council: on' })).toBeNull();
    fireEvent.click(within(band).getByRole('button', { name: 'Accept invitation' }));
    fireEvent.change(within(band).getByLabelText('Room invite link'), { target: { value: LINK } });
    fireEvent.click(within(band).getByRole('button', { name: 'Review invitation' }));
    expect(within(band).getByText('Join https://gateway.example.com as Workstation?')).toBeVisible();
  });

  it('opens one form only after the reader chooses it', async () => {
    fixture(NOT_CONNECTED);
    expect(await screen.findByText('Not connected')).toBeVisible();
    expect(screen.queryByLabelText('Room invite link')).toBeNull();
    expect(screen.queryByLabelText('Rooms administrator token')).toBeNull();
    expect(screen.queryByLabelText('Room machine name')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'New room' }));
    expect(screen.getByLabelText('New room name')).toBeVisible();
    expect(screen.queryByRole('combobox', { name: 'Relay of the new room' })).toBeNull();
    expect(screen.getByLabelText('Rooms relay URL')).toBeVisible();
    expect(screen.getByLabelText('Rooms administrator token')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Accept invitation' }));
    expect(screen.queryByLabelText('Rooms administrator token')).toBeNull();
    expect(screen.getByLabelText('Room invite link')).toBeVisible();
    expect(screen.getByRole('button', { name: 'Review invitation' })).toBeDisabled();
  });

  it('registers this machine on a new relay before it creates the first room', async () => {
    const client = fixture(NOT_CONNECTED, 'Workstation');
    await createRoom(' Builds ', 'a'.repeat(43));
    fireEvent.change(screen.getByLabelText('Rooms relay URL'), { target: { value: ' https://gateway.example.com ' } });
    fireEvent.click(screen.getByRole('button', { name: 'Create room' }));
    await waitFor(() => expect(client.createRoom).toHaveBeenCalledWith('https://gateway.example.com', 'Builds'));
    expect(client.registerRooms).toHaveBeenCalledWith('https://gateway.example.com', 'a'.repeat(43));
    expect(client.registerRooms.mock.invocationCallOrder[0]).toBeLessThan(client.createRoom.mock.invocationCallOrder[0]);
    await waitFor(() => expect(screen.queryByLabelText('New room name')).toBeNull());
  });

  it('creates another room without a token on a relay where this machine can create rooms', async () => {
    const client = fixture();
    await createRoom('Reviews');
    expect(screen.getByRole('combobox', { name: 'Relay of the new room' })).toHaveTextContent('gateway.example.com');
    expect(screen.queryByLabelText('Rooms administrator token')).toBeNull();
    expect(screen.queryByLabelText('Rooms relay URL')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Create room' }));
    await waitFor(() => expect(client.createRoom).toHaveBeenCalledWith('https://gateway.example.com', 'Reviews'));
    expect(client.registerRooms).not.toHaveBeenCalled();
  });

  it('asks a member by invitation for the token once on its relay', async () => {
    const client = fixture({ configured: true, relays: [{ ...RELAY, machine: { ...RELAY.machine, can_create_rooms: false } }] });
    await createRoom('Reviews', 'a'.repeat(43));
    expect(screen.queryByLabelText('Rooms relay URL')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Create room' }));
    await waitFor(() => expect(client.createRoom).toHaveBeenCalledWith('https://gateway.example.com', 'Reviews'));
    expect(client.registerRooms).toHaveBeenCalledWith('https://gateway.example.com', 'a'.repeat(43));
  });

  it('creates a room on another relay with the administrator token of that relay', async () => {
    const client = fixture();
    await createRoom('Reviews');
    await userEvent.click(screen.getByRole('combobox', { name: 'Relay of the new room' }));
    await userEvent.click(screen.getByRole('option', { name: 'Another relay' }));
    fireEvent.change(screen.getByLabelText('Rooms relay URL'), { target: { value: 'https://10.0.0.5' } });
    expect(screen.getByRole('button', { name: 'Create room' })).toBeDisabled();
    fireEvent.change(screen.getByLabelText('Rooms administrator token'), { target: { value: 'b'.repeat(43) } });
    fireEvent.click(screen.getByRole('button', { name: 'Create room' }));
    await waitFor(() => expect(client.createRoom).toHaveBeenCalledWith('https://10.0.0.5', 'Reviews'));
    expect(client.registerRooms).toHaveBeenCalledWith('https://10.0.0.5', 'b'.repeat(43));
  });

  it('keeps both actions for a machine that is in several rooms', async () => {
    fixture({ configured: true, relays: [{ ...RELAY, rooms: [...RELAY.rooms,
      { room_id: 'other', name: 'Reviews', owner_machine_id: 'peer', created_at: 2 }] }] });
    expect(await screen.findByRole('button', { name: 'Delete Builds' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'Leave Reviews' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'New room' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'Accept invitation' })).toBeVisible();
  });

  it('groups rooms by relay and keeps the saved rooms of a relay that does not answer', async () => {
    const client = fixture({ configured: true, relays: [RELAY, {
      relay_url: 'https://10.0.0.5',
      machine: { machine_id: 'guest', name: 'Laptop', can_create_rooms: false, created_at: 2 },
      rooms: [{ room_id: 'shared', name: 'Reviews', owner_machine_id: 'peer', created_at: 2 }],
      error: 'unavailable',
    }] });
    expect(await screen.findByText('Connected to 2 relays')).toBeVisible();
    expect(screen.getByText('gateway.example.com')).toBeVisible();
    expect(screen.getByText('Connected as Laptop')).toBeVisible();
    expect(screen.getByText('10.0.0.5')).toBeVisible();
    expect(screen.getByText('Not available. These rooms are from the last check.')).toBeVisible();
    expect(screen.queryByText('unavailable')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Leave Reviews' }));
    fireEvent.click(screen.getByRole('button', { name: 'Yes, leave' }));
    // Each relay knows this machine by another ID.
    await waitFor(() => expect(client.removeRoomMember).toHaveBeenCalledWith('shared', 'guest'));
  });

  it('joins only after explicit review and never shares a group or session implicitly', async () => {
    const client = fixture();
    expect(await screen.findByText('Connected to 1 relay')).toBeVisible();
    await reviewInvitation(LINK);
    expect(screen.getByText('Join https://gateway.example.com as Laptop?')).toBeVisible();
    expect(screen.queryByText(TRUST_NOTE)).toBeNull();
    expect(client.joinRoom).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Join room' }));
    await waitFor(() => expect(client.joinRoom).toHaveBeenCalledWith(LINK));
    await waitFor(() => expect(screen.queryByLabelText('Room invite link')).toBeNull());
    expect(client.setSetting).not.toHaveBeenCalled();
  });

  it('accepts an invitation from another relay after it names the new relay', async () => {
    const client = fixture();
    const link = `https://10.0.0.5/rooms/join#invite=${'a'.repeat(43)}`;
    await reviewInvitation(link);
    expect(screen.getByText('Join https://10.0.0.5 as Laptop?')).toBeVisible();
    expect(screen.getByText(TRUST_NOTE)).toBeVisible();
    expect(client.joinRoom).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Join room' }));
    await waitFor(() => expect(client.joinRoom).toHaveBeenCalledWith(link));
  });

  it('refuses an invitation link without its secret', async () => {
    const client = fixture();
    await reviewInvitation('https://gateway.example.com/rooms/join');
    expect(screen.getByText('Enter a complete invitation link.')).toBeVisible();
    expect(screen.queryByRole('button', { name: 'Join room' })).toBeNull();
    expect(client.joinRoom).not.toHaveBeenCalled();
  });

  it('explains an unavailable invitation without rendering an upstream message', async () => {
    const client = fixture();
    client.joinRoom.mockRejectedValue(new GatewayError(410, 'untrusted credential value'));
    await reviewInvitation(LINK);
    fireEvent.click(screen.getByRole('button', { name: 'Join room' }));
    expect(await screen.findByText('This invitation is expired, used or revoked. Ask the owner for a new invitation.')).toBeVisible();
    expect(screen.queryByText('untrusted credential value')).toBeNull();
  });

  it('shows a new invitation with its limits and revokes it', async () => {
    const client = fixture();
    client.inviteToRoom.mockResolvedValue({
      invite: { invite_id: 'invite', room_id: 'room', expires_at: Date.UTC(2030, 0, 1), max_uses: 1, uses: 0, revoked: false },
      invite_url: LINK,
    });
    fireEvent.click(await screen.findByRole('button', { name: 'Invite to Builds' }));
    expect(await screen.findByLabelText('Created room invite')).toHaveValue(LINK);
    expect(screen.getByText(/^One use\. Expires .+\. Keep it private\.$/)).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Revoke invitation' }));
    await waitFor(() => expect(client.revokeRoomInvite).toHaveBeenCalledWith('room', 'invite'));
    await waitFor(() => expect(screen.queryByLabelText('Created room invite')).toBeNull());
  });

  it('requires confirmation before deleting a room', async () => {
    const client = fixture();
    fireEvent.click(await screen.findByRole('button', { name: 'Delete Builds' }));
    expect(screen.getByRole('group', { name: 'Delete Builds?' })).toBeVisible();
    expect(client.deleteRoom).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Yes, delete' }));
    await waitFor(() => expect(client.deleteRoom).toHaveBeenCalledWith('room'));
  });

  it('requires confirmation before disconnecting this machine from one relay', async () => {
    const client = fixture();
    fireEvent.click(await screen.findByRole('button', { name: 'Disconnect gateway.example.com' }));
    expect(screen.getByRole('group', { name: 'Disconnect from gateway.example.com?' })).toBeVisible();
    expect(screen.getByText('This machine leaves its rooms on this relay, and the rooms that it owns there are deleted. Local sessions stay.')).toBeVisible();
    expect(client.disconnectRooms).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Yes, disconnect' }));
    await waitFor(() => expect(client.disconnectRooms).toHaveBeenCalledWith('https://gateway.example.com'));
  });

  it('explains a group denial even when this session stores an ineffective override', () => {
    expect(lockNote({ id: 'room_access', label: 'Allow Builds', type: 'boolean', enabled: false,
      inheritance: 'restrict', source: 'group', scope: 'session', is_override: true })).toBe(
      'Locked: group settings deny this permission. This scope cannot allow it.');
    expect(lockNote({ id: 'wake', label: 'Wake', type: 'boolean', enabled: false,
      inheritance: 'restrict', source: 'default', scope: 'session' })).toBeNull();
  });
});
