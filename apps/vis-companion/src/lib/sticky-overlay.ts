/**
 * AN OPEN OVERLAY SURVIVES ITS OWNER BEING RE-MOUNTED IN PLACE.
 *
 * A full-screen artifact — a picture in the image viewer, a document in the doc
 * overlay — is opened from a row INSIDE the transcript, and the row keeps the
 * "is it open" bit. That is fine until the transcript re-parents the row under
 * it: the session screen paints a streaming answer in its own running-turn bubble and,
 * the moment the turn settles, drops that bubble and renders the very same
 * content as a settled transcript row. React sees two different subtrees, so
 * the live one unmounts — taking the reader's open artifact with it and putting
 * the final answer on screen instead.
 *
 * The bit therefore has to outlive the instance by a hair. A row that unmounts
 * while its overlay is open OFFERS a handover under a stable key; the next row
 * to mount under that key CLAIMS it and opens again, in the same commit, so the
 * swap is invisible. Nothing else can pick it up: an offer expires by itself,
 * and leaving the session drops every outstanding one, so re-entering a session
 * never re-opens a viewer the reader had left behind.
 */

import { useLayoutEffect, useRef, useState } from "react";

/**
 * How long an unmounted row's overlay stays claimable. The live-to-settled swap
 * is normally one commit, but the two states are set from different effects, so
 * a paint can land between them; a second is far longer than that gap and far
 * shorter than any human re-entry into the same session.
 */
const HANDOVER_MS = 1_000;

/** Key to the wall-clock deadline after which the offer is no longer claimable. */
const handovers = new Map<string, number>();

export function offerOverlayHandover(key: string): void {
  handovers.set(key, Date.now() + HANDOVER_MS);
}

export function claimOverlayHandover(key: string): boolean {
  const deadline = handovers.get(key);
  if (deadline === undefined) return false;
  handovers.delete(key);
  return deadline > Date.now();
}

/** Leaving the transcript that owned them: nothing may re-open later. */
export function dropOverlayHandovers(): void {
  handovers.clear();
}

/**
 * `useState(false)` for an overlay, plus the handover. `key` identifies the
 * ARTIFACT, not the component instance, because the instance is exactly what
 * does not survive.
 */
export function useStickyOverlay(
  key: string,
): [boolean, (next: boolean) => void] {
  const [open, setOpen] = useState(false);
  // The unmount cleanup must read the LATEST key and openness, and must run on
  // unmount only — a key that changes under a live row is a new artifact, not a
  // handover.
  const latest = useRef({ key, open });
  useLayoutEffect(() => {
    latest.current = { key, open };
  });

  useLayoutEffect(() => {
    if (claimOverlayHandover(key)) setOpen(true);
  }, [key]);

  useLayoutEffect(
    () => () => {
      if (latest.current.open) offerOverlayHandover(latest.current.key);
    },
    [],
  );

  return [open, setOpen];
}
