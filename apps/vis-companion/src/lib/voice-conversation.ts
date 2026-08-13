export type VoiceModeLease = object;

/** Client-local ownership for voice turns. A lease prevents a submit response
 * that arrives after Leave/session navigation from re-arming playback. */
export class VoiceTurnOwnership {
  private lease: VoiceModeLease | null = null;
  private turnId: string | null = null;

  enter(): VoiceModeLease {
    const lease = {};
    this.lease = lease;
    this.turnId = null;
    return lease;
  }

  leave(): void {
    this.lease = null;
    this.turnId = null;
  }

  claim(turnId: string | undefined, lease: VoiceModeLease | null): boolean {
    if (!turnId || !lease || lease !== this.lease) return false;
    this.turnId = turnId;
    return true;
  }

  settle(turnId: string): boolean {
    if (turnId !== this.turnId) return false;
    this.turnId = null;
    return true;
  }
}
