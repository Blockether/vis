import { beforeEach, describe, expect, it } from 'vitest';
import sessionScreenSource from '../screens/SessionScreen.tsx?raw';
import {
  applyReadingPosition,
  forgetReadingPosition,
  markReadingPosition,
  parkedReadingPosition,
  rememberReadingPosition,
  type ScrollBox,
} from './reading-position';

// Regression, reported: reading a session part-way up, switching to another tab
// (or back to the list) and returning put the reader at the BOTTOM again — the
// opening correction pinned to the newest turn with no memory of where they were.
describe('transcript reading position', () => {
  const box = (scrollTop: number, scrollHeight: number, clientHeight: number): ScrollBox => ({
    scrollTop,
    scrollHeight,
    clientHeight,
  });

  beforeEach(() => forgetReadingPosition('s1'));

  it('marks how far above the end the reader parked', () => {
    expect(markReadingPosition(box(1000, 5000, 800))).toBe(3200);
  });

  it('parks nothing for a reader who was at the bottom', () => {
    expect(markReadingPosition(box(4200, 5000, 800))).toBeNull();
    expect(markReadingPosition(box(4160, 5000, 800))).toBeNull();
    expect(markReadingPosition(null)).toBeNull();
  });

  it('remembers per session and forgets on demand', () => {
    rememberReadingPosition('s1', 3200);
    expect(parkedReadingPosition('s1')).toBe(3200);
    expect(parkedReadingPosition('s2')).toBeNull();
    rememberReadingPosition('s1', null);
    expect(parkedReadingPosition('s1')).toBeNull();
  });

  it('restores the same distance from the end, not the same pixel', () => {
    // History hydrated above: the transcript grew by 2000px at the TOP.
    const viewport = box(0, 7000, 800);
    expect(applyReadingPosition(viewport, 3200)).toBe(true);
    expect(viewport.scrollTop).toBe(3000);
  });

  it('reports failure while the transcript is still too short', () => {
    const viewport = box(0, 1200, 800);
    expect(applyReadingPosition(viewport, 3200)).toBe(false);
    expect(applyReadingPosition(box(0, 800, 800), 3200)).toBe(false);
  });
});

describe('session screen honours the parked reading position', () => {
  it('restores instead of pinning to the end when a place was remembered', () => {
    expect(sessionScreenSource).toContain('parkedReadingPosition(sid)');
    expect(sessionScreenSource).toContain('applyReadingPosition(');
    expect(sessionScreenSource).toContain('rememberReadingPosition(sid, markReadingPosition(');
  });
});
