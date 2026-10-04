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

function fixture(status: RoomsStatus = CONFIGURED) {
  const client = { rooms: vi.fn().mockResolvedValue(status), joinRoom: vi.fn().mockResolvedValue({}),
    deleteRoom: vi.fn().mockResolvedValue({}), disconnectRooms: vi.fn().mockResolvedValue({ configured: false, rooms: [] }),
    setSetting: vi.fn() };
  render(<CouncilRooms client={client as unknown as GatewayClient} onChanged={vi.fn()} />);
  return client;
}

async function reviewInvitation(link: string) {
  fireEvent.click(await screen.findByRole('button', { name: 'Join a room' }));
  fireEvent.change(screen.getByLabelText('Room invite link'), { target: { value: link } });
  fireEvent.click(screen.getByRole('button', { name: 'Review invitation' }));
}

const LINK = `https://gateway.example.com/rooms/join#invite=${'a'.repeat(43)}`;

describe('Council room Settings', () => {
  it('stands inside the Council band of machine Settings', async () => {
    vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
    vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
      revision: 'council-1',
      groups: [{ id: 'council', title: 'Council', toggles: [{ id: 'council', label: 'Council', type: 'boolean', enabled: true }] }],
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
    expect(screen.queryByRole('heading', { name: 'Council rooms' })).toBeNull();
  });

  it('opens one form only after the reader chooses it', async () => {
    fixture({ configured: false, rooms: [] });
    expect(await screen.findByText('Not connected')).toBeVisible();
    expect(screen.queryByLabelText('Room invite link')).toBeNull();
    expect(screen.queryByLabelText('Rooms administrator token')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Create rooms' }));
    expect(screen.getByLabelText('Rooms administrator token')).toBeVisible();
    fireEvent.click(screen.getByRole('button', { name: 'Join a room' }));
    expect(screen.queryByLabelText('Rooms administrator token')).toBeNull();
    expect(screen.getByLabelText('Room invite link')).toBeVisible();
    expect(screen.getByRole('button', { name: 'Review invitation' })).toBeDisabled();
  });

  it('joins only after explicit review and never shares a group or session implicitly', async () => {
    const client = fixture();
    expect(await screen.findByText('Connected as Laptop via gateway.example.com')).toBeVisible();
    await reviewInvitation(LINK);
    expect(screen.getByText('Join https://gateway.example.com as Laptop?')).toBeVisible();
    expect(client.joinRoom).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Join room' }));
    await waitFor(() => expect(client.joinRoom).toHaveBeenCalledWith(LINK, 'Laptop'));
    await waitFor(() => expect(screen.queryByLabelText('Room invite link')).toBeNull());
    expect(client.setSetting).not.toHaveBeenCalled();
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
