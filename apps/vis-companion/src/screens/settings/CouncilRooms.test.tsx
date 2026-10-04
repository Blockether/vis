// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

afterEach(() => { cleanup(); vi.restoreAllMocks(); });
import { GatewayClient, GatewayError } from '../../lib/gateway';
import type { RoomsStatus } from '../../lib/rooms';
import { DEFAULT_SPEECH_PREFS } from '../../lib/storage';
import { CouncilRooms } from './CouncilRooms';
import { MachineSettings, lockNote } from './MachineSettings';

const CONFIGURED: RoomsStatus = {
  configured: true,
  relay_url: 'https://gateway.example.com',
  machine: { machine_id: 'machine', name: 'Laptop', can_create_rooms: true, created_at: 1 },
  rooms: [{ room_id: 'room', name: 'Builds', owner_machine_id: 'machine', created_at: 1 }],
};

function fixture(status: RoomsStatus = CONFIGURED, machineName = 'Laptop') {
  const client = { rooms: vi.fn().mockResolvedValue(status), joinRoom: vi.fn().mockResolvedValue({}),
    registerRooms: vi.fn().mockResolvedValue(status), createRoom: vi.fn().mockResolvedValue({}),
    deleteRoom: vi.fn().mockResolvedValue({}), disconnectRooms: vi.fn().mockResolvedValue({ configured: false, rooms: [] }),
    setSetting: vi.fn() };
  render(<CouncilRooms client={client as unknown as GatewayClient} machineName={machineName} onChanged={vi.fn()} />);
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

describe('Council room Settings', () => {
  it('stands inside the Council band of machine Settings, under the machine name', async () => {
    vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
    vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
      revision: 'council-1',
      groups: [{ id: 'council', title: 'Council', toggles: [
        { id: 'council', label: 'Council', type: 'boolean', enabled: true },
        { id: 'council_machine_name', label: 'Machine name', type: 'string', value: 'Workstation', max_length: 80,
          editor: 'text', scopes: ['global'] },
      ] }],
    });
    vi.spyOn(GatewayClient.prototype, 'rooms').mockResolvedValue({ configured: false, rooms: [] });
    render(
      <MachineSettings
        gateway={{ id: 'rooms-test', url: 'http://127.0.0.1:7890', token: 'test' }}
        speechPrefs={DEFAULT_SPEECH_PREFS}
        onSpeechChange={async () => DEFAULT_SPEECH_PREFS}
      />,
    );
    const band = (await screen.findByRole('heading', { name: 'Council' })).closest('section')!;
    expect(await within(band).findByText('Not connected')).toBeVisible();
    expect(within(band).getByRole('switch', { name: 'Council: on' })).toBeVisible();
    expect(within(band).getByRole('textbox', { name: 'Machine name' })).toHaveValue('Workstation');
    expect(screen.queryByRole('heading', { name: 'Council rooms' })).toBeNull();
    fireEvent.click(within(band).getByRole('button', { name: 'Accept invitation' }));
    fireEvent.change(within(band).getByLabelText('Room invite link'), { target: { value: LINK } });
    fireEvent.click(within(band).getByRole('button', { name: 'Review invitation' }));
    expect(within(band).getByText('Join https://gateway.example.com as Workstation?')).toBeVisible();
  });

  it('opens one form only after the reader chooses it', async () => {
    fixture({ configured: false, rooms: [] });
    expect(await screen.findByText('Not connected')).toBeVisible();
    expect(screen.queryByLabelText('Room invite link')).toBeNull();
    expect(screen.queryByLabelText('Rooms administrator token')).toBeNull();
    expect(screen.queryByLabelText('Room machine name')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'New room' }));
    expect(screen.getByLabelText('New room name')).toBeVisible();
    expect(screen.getByLabelText('Rooms relay URL')).toBeVisible();
    expect(screen.getByLabelText('Rooms administrator token')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Accept invitation' }));
    expect(screen.queryByLabelText('Rooms administrator token')).toBeNull();
    expect(screen.getByLabelText('Room invite link')).toBeVisible();
    expect(screen.getByRole('button', { name: 'Review invitation' })).toBeDisabled();
  });

  it('registers this machine under its machine name before it creates the first room', async () => {
    const client = fixture({ configured: false, rooms: [] }, 'Workstation');
    await createRoom(' Builds ', 'a'.repeat(43));
    fireEvent.change(screen.getByLabelText('Rooms relay URL'), { target: { value: 'https://gateway.example.com' } });
    fireEvent.click(screen.getByRole('button', { name: 'Create room' }));
    await waitFor(() => expect(client.createRoom).toHaveBeenCalledWith('Builds'));
    expect(client.registerRooms).toHaveBeenCalledWith('https://gateway.example.com', 'Workstation', 'a'.repeat(43));
    expect(client.registerRooms.mock.invocationCallOrder[0]).toBeLessThan(client.createRoom.mock.invocationCallOrder[0]);
    await waitFor(() => expect(screen.queryByLabelText('New room name')).toBeNull());
  });

  it('creates another room without a token when this machine can create rooms', async () => {
    const client = fixture();
    await createRoom('Reviews');
    expect(screen.queryByLabelText('Rooms administrator token')).toBeNull();
    expect(screen.queryByLabelText('Rooms relay URL')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Create room' }));
    await waitFor(() => expect(client.createRoom).toHaveBeenCalledWith('Reviews'));
    expect(client.registerRooms).not.toHaveBeenCalled();
  });

  it('asks a member by invitation for the token once and keeps its relay and relay name', async () => {
    const client = fixture({ ...CONFIGURED, machine: { ...CONFIGURED.machine!, can_create_rooms: false } }, 'Workstation');
    await createRoom('Reviews', 'a'.repeat(43));
    expect(screen.queryByLabelText('Rooms relay URL')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Create room' }));
    await waitFor(() => expect(client.createRoom).toHaveBeenCalledWith('Reviews'));
    expect(client.registerRooms).toHaveBeenCalledWith('https://gateway.example.com', 'Laptop', 'a'.repeat(43));
  });

  it('keeps both actions for a machine that is in several rooms', async () => {
    fixture({ ...CONFIGURED, rooms: [...CONFIGURED.rooms,
      { room_id: 'other', name: 'Reviews', owner_machine_id: 'peer', created_at: 2 }] });
    expect(await screen.findByRole('button', { name: 'Delete Builds' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'Leave Reviews' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'New room' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'Accept invitation' })).toBeVisible();
  });

  it('joins only after explicit review and never shares a group or session implicitly', async () => {
    const client = fixture();
    expect(await screen.findByText('Connected via gateway.example.com')).toBeVisible();
    await reviewInvitation(LINK);
    expect(screen.getByText('Join https://gateway.example.com as Laptop?')).toBeVisible();
    expect(client.joinRoom).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Join room' }));
    await waitFor(() => expect(client.joinRoom).toHaveBeenCalledWith(LINK, 'Laptop'));
    await waitFor(() => expect(screen.queryByLabelText('Room invite link')).toBeNull());
    expect(client.setSetting).not.toHaveBeenCalled();
  });

  it('names the relay of this machine when an invitation is for another relay', async () => {
    const client = fixture();
    await reviewInvitation(`https://10.0.0.5/rooms/join#invite=${'a'.repeat(43)}`);
    expect(screen.getByText('This invitation is for another relay. This machine uses gateway.example.com.')).toBeVisible();
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

  it('requires confirmation before deleting a room', async () => {
    const client = fixture();
    fireEvent.click(await screen.findByRole('button', { name: 'Delete Builds' }));
    expect(screen.getByRole('group', { name: 'Delete Builds?' })).toBeVisible();
    expect(client.deleteRoom).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Yes, delete' }));
    await waitFor(() => expect(client.deleteRoom).toHaveBeenCalledWith('room'));
  });

  it('requires confirmation before disconnecting this machine', async () => {
    const client = fixture();
    fireEvent.click(await screen.findByRole('button', { name: 'Disconnect' }));
    expect(screen.getByRole('group', { name: 'Disconnect Laptop?' })).toBeVisible();
    expect(screen.getByText('The rooms that it owns are deleted. Local sessions stay.')).toBeVisible();
    expect(client.disconnectRooms).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Yes, disconnect' }));
    await waitFor(() => expect(client.disconnectRooms).toHaveBeenCalledWith());
  });

  it('explains a group denial even when this session stores an ineffective override', () => {
    expect(lockNote({ id: 'room_access', label: 'Allow Builds', type: 'boolean', enabled: false,
      inheritance: 'restrict', source: 'group', scope: 'session', is_override: true })).toBe(
      'Locked: group settings deny this permission. This scope cannot allow it.');
    expect(lockNote({ id: 'wake', label: 'Wake', type: 'boolean', enabled: false,
      inheritance: 'restrict', source: 'default', scope: 'session' })).toBeNull();
  });
});
