import { createContext, useContext } from 'react';

/**
 * The seam between "I drew on something" and "it is in my next message".
 *
 * The composer lives in `SessionScreen`; a document artifact lives at the bottom
 * of a memoized transcript (turn → block → attachment rail → tile). Threading a
 * callback down that path would make every row of history depend on composer
 * state, so the screen PUBLISHES the intake instead and whatever produced the
 * pixels calls it. `null` means there is nowhere to attach — the picture can
 * still be copied or shared, and the button that would send it stays away.
 */
export type AttachImage = (image: Blob, filename: string) => Promise<void>;

export const AttachImageContext = createContext<AttachImage | null>(null);

export function useAttachImage(): AttachImage | null {
  return useContext(AttachImageContext);
}
