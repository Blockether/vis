// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

afterEach(() => { cleanup(); vi.restoreAllMocks(); });
import { GatewayError, type GatewayClient } from '../../lib/gateway';
import { RoomsPanel } from './RoomsPanel';
import { lockNote } from './MachineSettings';

function fixture() {
  const status = { configured: true, machine: { machine_id: 'machine', name: 'Laptop', can_create_rooms: true },
    rooms: [{ room_id: 'room', name: 'Builds', owner_machine_id: 'machine', created_at: 1 }] };
  const client = { rooms: vi.fn().mockResolvedValue(status), joinRoom: vi.fn().mockResolvedValue({}),
    deleteRoom: vi.fn().mockResolvedValue({}), setSetting: vi.fn() };
  render(<RoomsPanel client={client as unknown as GatewayClient} onChanged={vi.fn()} />);
  return client;
}

describe('Council room Settings', () => {
  it('joins only after explicit review and never shares a group or session implicitly', async () => {
    const client = fixture();
    await screen.findByText('Builds');
    fireEvent.change(screen.getByLabelText('Room machine name'), { target: { value: 'Laptop' } });
    const link = `https://gateway.example.com/rooms/join#invite=${'a'.repeat(43)}`;
    fireEvent.change(screen.getByLabelText('Room invite link'), { target: { value: link } });
    fireEvent.click(screen.getByRole('button', { name: 'Review invitation' }));
    expect(client.joinRoom).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Confirm join' }));
    await waitFor(() => expect(client.joinRoom).toHaveBeenCalledWith(link, 'Laptop'));
    await waitFor(() => expect(screen.getByLabelText('Room invite link')).toHaveValue(''));
    expect(client.setSetting).not.toHaveBeenCalled();
  });

  it('explains an unavailable invitation without rendering an upstream message', async () => {
    const client = fixture();
    client.joinRoom.mockRejectedValue(new GatewayError(410, 'untrusted credential value'));
    await screen.findByText('Builds');
    fireEvent.change(screen.getByLabelText('Room machine name'), { target: { value: 'Laptop' } });
    fireEvent.change(screen.getByLabelText('Room invite link'), { target: { value: `https://gateway.example.com/rooms/join#invite=${'a'.repeat(43)}` } });
    fireEvent.click(screen.getByRole('button', { name: 'Review invitation' }));
    fireEvent.click(screen.getByRole('button', { name: 'Confirm join' }));
    expect(await screen.findByText('This invitation is expired, used or revoked. Ask the owner for a new invitation.')).toBeVisible();
    expect(screen.queryByText('untrusted credential value')).toBeNull();
  });

  it('requires confirmation before deleting a room', async () => {
    const client = fixture();
    fireEvent.click(await screen.findByRole('button', { name: 'Delete room' }));
    expect(client.deleteRoom).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Confirm removal' }));
    await waitFor(() => expect(client.deleteRoom).toHaveBeenCalledWith('room'));
  });

  it('explains a group denial even when this session stores an ineffective override', () => {
    expect(lockNote({ id: 'room_access', label: 'Allow Builds', type: 'boolean', enabled: false,
      inheritance: 'restrict', source: 'group', scope: 'session', is_override: true })).toBe(
      'Locked: group settings deny this permission. This scope cannot allow it.');
    expect(lockNote({ id: 'wake', label: 'Wake', type: 'boolean', enabled: false,
      inheritance: 'restrict', source: 'default', scope: 'session' })).toBeNull();
  });
});
