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

/** The identity, rooms and health of this machine on one relay. */
export interface RoomsRelay {
  relay_url: string;
  machine: RoomMachine;
  rooms: CouncilRoom[];
  /** An error code when the relay did not answer. The rooms are then from the last check. */
  error?: string;
}

/** This machine has one identity on each relay that it uses. */
export interface RoomsStatus {
  configured: boolean;
  relays: RoomsRelay[];
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
