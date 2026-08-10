import { describe, expect, it } from 'vitest';
import source from './SessionsScreen.tsx?raw';

// The fleet search is one ranked FTS query per paired machine and the gateway spends
// real time in SQLite before it answers, so the ONE thing this screen owes the network
// is restraint: ask once when typing rests, and never let a query the user has already
// replaced land on top of the one they are looking at.
describe('fleet search asks the gateway once per pause', () => {
  const effect = source.slice(
    source.indexOf('// Transcript + title search runs server-side'),
    source.indexOf('const matches = deferredQuery.trim()'),
  );

  it('names its debounce instead of burying a number in the effect', () => {
    expect(source).toContain('const SEARCH_DEBOUNCE_MS = 200;');
    expect(effect).toContain('}, SEARCH_DEBOUNCE_MS);');
    // The field stays immediate; only the round trip waits.
    expect(source).toContain('const deferredQuery = useDeferredValue(query);');
  });

  it('puts every request inside the sleeping timer, not beside it', () => {
    const timerAt = effect.indexOf('const timer = window.setTimeout(() => {');
    expect(timerAt).toBeGreaterThan(-1);
    // Both calls the effect makes — the search itself and the by-id hydration of hits
    // this machine had not paged in — must sit after the timer opens.
    expect(effect.indexOf('.searchSessionMatches(needle, controller.signal)')).toBeGreaterThan(
      timerAt,
    );
    expect(effect.indexOf('api.session(match.sessionId, controller.signal)')).toBeGreaterThan(
      timerAt,
    );
  });

  it('cancels a superseded query twice: the sleeping timer and the flight', () => {
    const cleanupAt = effect.indexOf('return () => {');
    expect(cleanupAt).toBeGreaterThan(-1);
    const cleanup = effect.slice(cleanupAt);
    expect(cleanup).toContain('controller.abort();');
    expect(cleanup).toContain('window.clearTimeout(timer);');
    // A response that outran its own cancellation still must not be written.
    expect(effect.indexOf('if (controller.signal.aborted) return;')).toBeLessThan(
      effect.indexOf('setTranscriptMatches('),
    );
  });

  it('re-runs on the query, the fleet and the scope — the three things it asks about', () => {
    expect(effect).toContain('}, [deferredQuery, fleetKey, scope]);');
  });

  it('spends nothing at all on an empty query or an empty scope', () => {
    expect(effect.indexOf('if (!needle) return;')).toBeLessThan(
      effect.indexOf('const timer = window.setTimeout'),
    );
    expect(effect.indexOf('if (targets.length === 0) return;')).toBeLessThan(
      effect.indexOf('const timer = window.setTimeout'),
    );
  });
});
