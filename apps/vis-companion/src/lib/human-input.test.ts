import fixture from './human-input.fixture.json';
import { describe, expect, it } from 'vitest';
import type { SseEvent } from './types';
import {
  applyHumanInputEvent,
  clampHumanInputRange,
  humanInputRange,
  humanInputRequestFromWire,
  humanInputRequestsFromWire,
  initialHumanInputValues,
  isHumanInputAnswerable,
  isHumanInputBlank,
  isHumanInputEvent,
  toggleHumanInputOption,
  type HumanInputRequest,
} from './human-input';

/**
 * VERBATIM engine output: `wire/json-str` of `human-input/request->view` for a
 * request with one field of every shape. The app must read the daemon's own
 * projection, so this fixture is never hand-tuned to match the parser — the
 * engine suite (`gateway.human-input-test`) re-derives these very bytes and
 * fails if the projection and this file ever disagree.
 */
const WIRE = fixture as unknown;

function requested(request: unknown = WIRE): SseEvent {
  return { type: 'human_input.request', session_id: 'sid-1', request } as SseEvent;
}

function closed(requestId: string, reason = 'submitted'): SseEvent {
  return {
    type: 'human_input.close',
    session_id: 'sid-1',
    request_id: requestId,
    reason,
  } as SseEvent;
}

function parsed(): HumanInputRequest {
  const request = humanInputRequestFromWire(WIRE);
  if (!request) throw new Error('the engine fixture must parse');
  return request;
}

describe('humanInputRequestFromWire', () => {
  it('reads the engine projection field for field', () => {
    const request = parsed();
    expect(request.id).toBe('req-1');
    expect(request.title).toBe('Deploy?');
    expect(request.description).toBe('prod');
    expect(request.session_id).toBe('sid-1');
    expect(request.submit_label).toBe('Submit');
    expect(request.is_cancellable).toBe(true);
    expect(request.fields.map((field) => `${field.name}:${field.type}`)).toEqual([
      'env:select',
      'key:password',
      'ok:checkbox',
      'tags:multiselect',
      'risk:range',
    ]);
    // name keys the answer, label is what the dialog shows, description is the
    // italic line under it.
    expect(request.fields[0]?.name).toBe('env');
    expect(request.fields[0]?.label).toBe('Env');
    expect(request.fields[0]?.description).toBe('Where this deploy lands');
    expect(request.fields[1]?.label).toBe('key');
    expect(request.fields[1]?.description).toBeUndefined();
    expect(request.fields[1]?.is_required).toBe(true);
    expect(request.fields[1]?.max_length).toBe(40);
    expect(request.fields[0]?.options).toEqual([
      { value: 'prod', label: 'prod' },
      { value: 'stg', label: 'Staging' },
    ]);
  });

  it('labels a field by its id when the spec gave no label', () => {
    const request = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ id: 'token', type: 'plaintext' }],
    });
    expect(request?.fields[0]?.label).toBe('token');
    expect(request?.submit_label).toBe('Submit');
    expect(request?.cancel_label).toBe('Cancel');
    expect(request?.is_cancellable).toBe(true);
  });

  it('names a field by its wire name, and by its id for older engines', () => {
    const named = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ name: 'token', type: 'plaintext', label: 'API token', description: 'from 1Password' }],
    });
    expect(named?.fields[0]?.name).toBe('token');
    expect(named?.fields[0]?.id).toBe('token');
    expect(named?.fields[0]?.label).toBe('API token');
    expect(named?.fields[0]?.description).toBe('from 1Password');

    const legacy = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ id: 'token', type: 'plaintext', help: 'legacy help line' }],
    });
    expect(legacy?.fields[0]?.name).toBe('token');
    expect(legacy?.fields[0]?.description).toBe('legacy help line');
  });

  it('refuses a frame it could not render as an answerable form', () => {
    expect(humanInputRequestFromWire(null)).toBeNull();
    expect(humanInputRequestFromWire({ title: 'T', fields: [{ id: 'a' }] })).toBeNull();
    expect(humanInputRequestFromWire({ id: 'r', fields: [{ id: 'a' }] })).toBeNull();
    expect(humanInputRequestFromWire({ id: 'r', title: 'T', fields: [] })).toBeNull();
    expect(humanInputRequestFromWire({ id: 'r', title: 'T', fields: [{ type: 'plaintext' }] }))
      .toBeNull();
  });

  it('keeps only the requests a REST snapshot can show', () => {
    expect(humanInputRequestsFromWire([WIRE, null, { id: 'x' }]).map((row) => row.id))
      .toEqual(['req-1']);
    expect(humanInputRequestsFromWire(undefined)).toEqual([]);
  });
});

describe('initialHumanInputValues', () => {
  it('opens on the request defaults, exactly like the TUI dialog', () => {
    expect(initialHumanInputValues(parsed())).toEqual({
      env: 'prod',
      key: '',
      ok: true,
      risk: 2.5,
      tags: [],
    });
  });

  it('never starts a checkbox or multiselect from a foreign default', () => {
    const request = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [
        { id: 'flag', type: 'checkbox', default: 'yes' },
        { id: 'many', type: 'multiselect', default: ['a', 7] },
      ],
    });
    expect(request && initialHumanInputValues(request)).toEqual({ flag: false, many: ['a'] });
  });

  it('keeps a field literally named __proto__ answerable', () => {
    const request = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ id: '__proto__', type: 'plaintext', is_required: true }],
    }) as HumanInputRequest;
    const values = initialHumanInputValues(request);
    // `values[id] = …` on an object literal hits the prototype setter: the
    // field silently vanishes, the submit button never enables and the POST
    // omits the value the engine is waiting for.
    expect(Object.prototype.hasOwnProperty.call(values, '__proto__')).toBe(true);
    expect(isHumanInputAnswerable(request, values)).toBe(false);
    const answered = { ...values, ['__proto__']: 'typed' };
    expect(isHumanInputAnswerable(request, answered)).toBe(true);
    expect(JSON.parse(JSON.stringify(answered))['__proto__']).toBe('typed');
  });
});

describe('required fields', () => {
  it('blocks submit until every required field is answered', () => {
    const request = parsed();
    const values = initialHumanInputValues(request);
    expect(isHumanInputAnswerable(request, values)).toBe(false);
    expect(isHumanInputAnswerable(request, { ...values, key: 'secret' })).toBe(true);
  });

  it('treats blank space and an empty choice as unanswered', () => {
    const [, key] = parsed().fields;
    if (!key) throw new Error('fixture must have a password field');
    expect(isHumanInputBlank(key, '   ')).toBe(true);
    expect(isHumanInputBlank(key, 'k')).toBe(false);
    expect(isHumanInputBlank({ ...key, type: 'checkbox' }, false)).toBe(true);
    expect(isHumanInputBlank({ ...key, type: 'multiselect' }, [])).toBe(true);
    expect(isHumanInputBlank({ ...key, is_required: false }, '')).toBe(false);
  });
});

describe('toggleHumanInputOption', () => {
  it('adds, removes, and keeps the request option order', () => {
    const request = parsed();
    const tags = request.fields[3];
    if (!tags) throw new Error('fixture must have a multiselect field');
    expect(toggleHumanInputOption(tags, { tags: [] }, 'b')).toEqual(['b']);
    expect(toggleHumanInputOption(tags, { tags: ['b'] }, 'a')).toEqual(['a', 'b']);
    expect(toggleHumanInputOption(tags, { tags: ['a', 'b'] }, 'a')).toEqual(['b']);
  });
});

describe('applyHumanInputEvent', () => {
  it('opens a form on the gateway request event', () => {
    expect(isHumanInputEvent(requested())).toBe(true);
    expect(isHumanInputEvent({ type: 'turn.completed' } as SseEvent)).toBe(false);
    const pending = applyHumanInputEvent([], requested());
    expect(pending.map((row) => row.id)).toEqual(['req-1']);
  });

  it('replaces on replay instead of stacking a second dialog', () => {
    const once = applyHumanInputEvent([], requested());
    const twice = applyHumanInputEvent(once, requested());
    expect(twice.map((row) => row.id)).toEqual(['req-1']);
    expect(twice[0]).not.toBe(once[0]);
  });

  it('drops the form when the request closes anywhere — TUI, timeout, or here', () => {
    const pending = applyHumanInputEvent([], requested());
    expect(applyHumanInputEvent(pending, closed('req-1'))).toEqual([]);
    expect(applyHumanInputEvent(pending, closed('req-1', 'timeout'))).toEqual([]);
  });

  it('leaves the list ALONE for anything it cannot act on', () => {
    const pending = applyHumanInputEvent([], requested());
    expect(applyHumanInputEvent(pending, closed('other'))).toBe(pending);
    expect(applyHumanInputEvent(pending, closed(''))).toBe(pending);
    expect(applyHumanInputEvent(pending, requested({ id: 'bad' }))).toBe(pending);
    expect(applyHumanInputEvent(pending, { type: 'turn.started' } as SseEvent)).toBe(pending);
  });
});

describe('range fields', () => {
  const slider = (extra: Record<string, unknown> = {}) => {
    const request = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ id: 'risk', type: 'range', ...extra }],
    });
    if (!request) throw new Error('a range request must parse');
    return request.fields[0]!;
  };

  it('carries the bounds the engine sent', () => {
    const risk = parsed().fields.find((field) => field.type === 'range');
    expect(humanInputRange(risk!)).toEqual({ min: 0, max: 10, step: 0.5 });
  });

  it('falls back to 0-100 by 1 when the engine sends no bounds', () => {
    expect(humanInputRange(slider())).toEqual({ min: 0, max: 100, step: 1 });
  });

  it('snaps to the step and never leaves the track', () => {
    const risk = slider({ min: 0, max: 10, step: 0.5 });
    expect(clampHumanInputRange(risk, 2.7)).toBe(2.5);
    expect(clampHumanInputRange(risk, -4)).toBe(0);
    expect(clampHumanInputRange(risk, 99)).toBe(10);
    // 0.1 * 27 is 2.7000000000000006 in binary floating point: a slider that
    // submits that instead of 2.7 makes the answer unreadable in the log.
    expect(clampHumanInputRange(slider({ min: 0, max: 3, step: 0.1 }), 2.7)).toBe(2.7);
  });

  it('starts on the minimum, not on an empty string', () => {
    const request = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ id: 'risk', type: 'range', min: 5, max: 9 }],
    });
    expect(request && initialHumanInputValues(request)).toEqual({ risk: 5 });
  });

  it('is answered by any number, including the zero at the far left', () => {
    const risk = slider({ min: 0, max: 10, is_required: true });
    // 0 is falsy: a blank-check written as `!value` would refuse the answer at
    // the far left of the track forever.
    expect(isHumanInputBlank(risk, 0)).toBe(false);
    expect(isHumanInputBlank(risk, undefined)).toBe(true);
  });
});
