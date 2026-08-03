import fixture from './human-input.fixture.json';
import { describe, expect, it } from 'vitest';
import type { SseEvent } from './types';
import {
  applyHumanInputEvent,
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
    expect(request.fields.map((field) => `${field.id}:${field.type}`)).toEqual([
      'env:select',
      'key:password',
      'ok:checkbox',
      'tags:multiselect',
    ]);
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
