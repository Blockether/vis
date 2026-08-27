/**
 * The query that means A HARDWARE KEYBOARD IS PROBABLY THERE.
 *
 * There is no web API for "is a physical keyboard attached", and the software one
 * delivers the same `Enter` a hardware one does. A FINE POINTER is the signal that
 * exists: a mouse or a trackpad — `index.css` measures Magic Keyboard as one —
 * arrives with a keyboard that has a Shift to hold, and a bare touch screen does
 * not. It is spelled without the width the `mouse:` variant carries, because
 * neither of its two questions — whether Return submits, and whether focus is about
 * to be answered by a screenful of keys — is a question about room.
 *
 * It lives in its own module because both askers are downstream of it: the composer
 * (`keyboard.ts`) and the shell that reserves the keyboard's geometry
 * (`viewport.ts`), which already own each other in the other direction.
 */
export const HARDWARE_POINTER = '(pointer: fine)';

/**
 * Is a mouse or trackpad pointing at the app right now?
 *
 * Read it PER EVENT, never once at mount: a keyboard is folded onto an iPad
 * mid-session, and every answer downstream has to change with it.
 */
export function hasHardwarePointer(): boolean {
  if (typeof window === 'undefined') return false;
  return window.matchMedia?.(HARDWARE_POINTER).matches ?? false;
}
