import { describe, expect, it } from 'vitest';
import { HUMAN_INPUT_REQUESTS, HUMAN_INPUT_STATES } from './humanInputVariants';
import { humanInputInputFields, initialHumanInputValues } from '../lib/human-input';

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
      // The LEAVES answer; a layout group holds no value of its own.
      for (const field of humanInputInputFields(request.fields)) {
        expect(Object.hasOwn(values, field.id), `${state}:${field.id}`).toBe(true);
      }
    }
  });

  it('opens the minimal state on an unanswered required field', () => {
    // The photograph must show the state a one-field pause spends its life in:
    // required, empty, and with nothing red on it. Nothing here can redden it —
    // the app runs no validators, so only a refused confirmation ever does.
    const request = HUMAN_INPUT_REQUESTS.minimal;
    const [branch] = humanInputInputFields(request.fields);
    expect(branch?.is_required).toBe(true);
    expect(initialHumanInputValues(request)[branch!.id]).toBe('');
  });
});
