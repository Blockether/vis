/** Machine membership DTOs from the canonical rooms.json protocol. */
export interface CouncilRoom {
  room_id: string;
  name: string;
  owner_machine_id: string;
  created_at: number;
}

export interface RoomMachine {
  machine_id: string;
  name: string;
  can_create_rooms: boolean;
  created_at: number;
}

export interface RoomsStatus {
  configured: boolean;
  relay_url?: string;
  machine?: RoomMachine;
  rooms: CouncilRoom[];
}

export interface RoomMember {
  machine_id: string;
  name: string;
  role: 'owner' | 'member';
  joined_at: number;
}

export interface RoomInvitation {
  invite: { invite_id: string; room_id: string; expires_at: number; max_uses: number; uses: number; revoked: boolean };
  invite_url: string;
}
