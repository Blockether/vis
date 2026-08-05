import { describe, expect, it } from 'vitest';
import gallerySource from './DesignGallery.tsx?raw';
import { ARTIFACT_STATES, ARTIFACTS, artifactsFor } from './artifactVariants';

/**
 * The gallery is production code, and these three proposals only mean something
 * if the fixture can falsify them: a session that produced NOTHING (the control
 * has to disappear), and one whose whole output is documents and logs (nothing
 * to thumbnail). Two "different" states that render the same markup produce
 * byte-identical PNGs and a reviewer who thinks he compared something.
 */
describe('artifacts design fixtures', () => {
  it('registers every photographed proposal in the gallery', () => {
    // Importing the gallery module drags the whole app in, so the registry is
    // read as text: an unregistered variant never reaches a screenshot.
    for (const [id, states] of Object.entries(ARTIFACT_STATES)) {
      expect(gallerySource, id).toContain(`id: '${id}'`);
      expect(gallerySource, id).toContain(`states: ARTIFACT_STATES['${id}']`);
      expect(new Set(states).size, id).toBe(states.length);
    }
  });

  it('photographs the state where the feature must cost nothing', () => {
    for (const [id, states] of Object.entries(ARTIFACT_STATES)) {
      expect(states, id).toContain('empty');
      expect(states, id).toContain('docs');
    }
    expect(artifactsFor('empty')).toEqual([]);
  });

  it('gives the docs state nothing to thumbnail', () => {
    const docs = artifactsFor('docs');
    expect(docs.length).toBeGreaterThan(1);
    expect(docs.some((entry) => entry.kind === 'image' || entry.kind === 'video')).toBe(false);
    expect(docs.some((entry) => entry.kind === 'doc')).toBe(true);
    expect(docs.some((entry) => entry.kind === 'file')).toBe(true);
  });

  it('measures the default state at its widest', () => {
    const all = artifactsFor('default');
    // Two digits, a clip, a paged PDF and more than one turn: the layout is
    // photographed where it is hardest, not where it is prettiest.
    expect(all.length).toBeGreaterThan(9);
    expect(all.some((entry) => entry.kind === 'video')).toBe(true);
    expect(all.some((entry) => entry.kind === 'doc' && (entry.pages ?? 0) > 1)).toBe(true);
    expect(new Set(all.map((entry) => entry.turn)).size).toBeGreaterThan(2);
  });

  it('keeps every artifact citable', () => {
    // The whole point of the index is provenance: what made it, when, how big.
    // A tile that cannot say that is a thumbnail, not an artifact.
    for (const entry of ARTIFACTS) {
      expect(entry.name, entry.name).toMatch(/\.[a-z0-9]+$/);
      expect(entry.tool, entry.name).toBeTruthy();
      expect(entry.turn, entry.name).toBeGreaterThan(0);
      expect(entry.size, entry.name).toMatch(/^[\d.]+ (kB|MB)$/);
    }
    // Newest first, so the sheet opens on what just appeared.
    const turns = ARTIFACTS.map((entry) => entry.turn);
    expect(turns).toEqual([...turns].sort((a, b) => b - a));
  });
});
