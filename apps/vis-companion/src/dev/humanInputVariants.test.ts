import { describe, expect, it } from 'vitest';
import { HUMAN_INPUT_REQUESTS, HUMAN_INPUT_STATES } from './humanInputVariants';
import { initialHumanInputValues, isHumanInputAnswerable } from '../lib/human-input';

/**
 * The gallery is production code and its fixtures are the only thing standing
 * between a reviewer and a photograph of the wrong thing. Three states here
 * exist to FALSIFY the sheet — a form longer than the phone, a pause with no
 * way out, and a rejected answer — so this pins that they still do.
 */
describe('human-input design fixtures', () => {
  it('declares one request per photographed state', () => {
    expect(HUMAN_INPUT_STATES.length).toBeGreaterThan(1);
    for (const state of HUMAN_INPUT_STATES) {
      expect(HUMAN_INPUT_REQUESTS[state], state).toBeDefined();
    }
  });

  it('photographs a different question in every state', () => {
    const titles = HUMAN_INPUT_STATES.map((state) => HUMAN_INPUT_REQUESTS[state].title);
    const fields = HUMAN_INPUT_STATES.map((state) =>
      HUMAN_INPUT_REQUESTS[state].fields.map((field) => field.id).join(','),
    );
    // Two states that render the same request produce two byte-identical PNGs.
    expect(new Set(titles.map((title, index) => `${title}|${fields[index]}`)).size).toBe(
      HUMAN_INPUT_STATES.length,
    );
  });

  it('keeps a pause that cannot be escaped', () => {
    expect(HUMAN_INPUT_REQUESTS.uncancellable.is_cancellable).toBe(false);
    expect(HUMAN_INPUT_REQUESTS.approve.is_cancellable).toBe(true);
  });

  it('keeps a form taller than a phone', () => {
    expect(HUMAN_INPUT_REQUESTS.long.fields.length).toBeGreaterThanOrEqual(6);
  });

  it('opens each state on defaults the sheet can actually hold', () => {
    for (const state of HUMAN_INPUT_STATES) {
      const request = HUMAN_INPUT_REQUESTS[state];
      const values = initialHumanInputValues(request);
      for (const field of request.fields) {
        expect(Object.hasOwn(values, field.id), `${state}:${field.id}`).toBe(true);
      }
    }
  });

  it('leaves the minimal state unanswerable until it is answered', () => {
    // A shot of a dialog whose submit is already enabled hides the disabled
    // state, which is the one a one-field pause spends most of its life in.
    const request = HUMAN_INPUT_REQUESTS.minimal;
    expect(isHumanInputAnswerable(request, initialHumanInputValues(request))).toBe(false);
    expect(isHumanInputAnswerable(request, { branch: 'fix/108' })).toBe(true);
  });
});
