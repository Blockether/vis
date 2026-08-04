import fixture from './human-input.fixture.json';
import { describe, expect, it } from 'vitest';
import type { SseEvent } from './types';
import {
  applyHumanInputEvent,
  clampHumanInputRange,
  humanInputRange,
  humanInputRequestFromWire,
  humanInputRequestsFromWire,
  humanInputErrors,
  humanInputFieldError,
  humanInputFormBlur,
  humanInputFormChange,
  humanInputFormErrors,
  humanInputFormStart,
  humanInputFormSubmit,
  humanInputOtp,
  humanInputOtpDigits,
  initialHumanInputValues,
  isHumanInputAnswerable,
  isHumanInputBlank,
  isHumanInputEvent,
  toggleHumanInputOption,
  type HumanInputField,
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
      'code:otp',
      'notify:plaintext',
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
      code: '',
      notify: '',
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
    expect(isHumanInputAnswerable(request, { ...values, key: 'secret' })).toBe(false);
    expect(isHumanInputAnswerable(request, { ...values, key: 'secret', code: '1234' })).toBe(true);
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

const only = (extra: Record<string, unknown>): { request: HumanInputRequest; field: HumanInputField } => {
  const request = humanInputRequestFromWire({
    id: 'r',
    title: 'T',
    fields: [{ id: 'f', ...extra }],
  }) as HumanInputRequest;
  return { request, field: request.fields[0] as HumanInputField };
};

describe('otp fields', () => {
  it('is six boxes unless the engine asked for another size', () => {
    expect(humanInputOtp(only({ type: 'otp' }).field)).toEqual({ min: 6, max: 6 });
    expect(humanInputOtp(only({ type: 'otp', min_length: 4, max_length: 8 }).field)).toEqual({
      min: 4,
      max: 8,
    });
  });

  it('keeps the digits out of whatever was pasted', () => {
    const { field } = only({ type: 'otp' });
    // A provider prints a code as `123 456`, a human copies the whole SMS line,
    // and both have to land six digits in six boxes rather than nothing at all.
    expect(humanInputOtpDigits(field, '123 456')).toBe('123456');
    expect(humanInputOtpDigits(field, 'code: 12-34-56')).toBe('123456');
    expect(humanInputOtpDigits(field, '1234567890')).toBe('123456');
    expect(humanInputOtpDigits(field, 'nope')).toBe('');
  });

  it('is unfinished until every box is full, in the engine’s words', () => {
    const { request, field } = only({ type: 'otp', is_required: true });
    expect(humanInputFieldError(field, '', {})).toBe('is required');
    expect(humanInputFieldError(field, '1234', {})).toBe('must be 6 digits');
    expect(humanInputFieldError(field, '12x456', {})).toBe('must be digits only');
    expect(humanInputFieldError(field, '123456', {})).toBeUndefined();
    expect(isHumanInputAnswerable(request, { f: '123456' })).toBe(true);
  });

  it('says at least / at most when the engine allows a span', () => {
    const { field } = only({ type: 'otp', min_length: 4, max_length: 8 });
    expect(humanInputFieldError(field, '123', {})).toBe('must be at least 4 digits');
    expect(humanInputFieldError(field, '123456789', {})).toBe('must be at most 8 digits');
    expect(humanInputFieldError(field, '12345', {})).toBeUndefined();
  });
});

describe('validation rules', () => {
  it('checks the named shapes the engine checks, and prints ITS message', () => {
    const { field } = only({
      type: 'plaintext',
      validate: [{ kind: 'type', type: 'email', message: 'must be an email address' }],
    });
    expect(humanInputFieldError(field, 'nope', {})).toBe('must be an email address');
    expect(humanInputFieldError(field, 'ops@example.com', {})).toBeUndefined();
    // A rule NEVER fires on a blank optional field — that is `is_required`'s job.
    expect(humanInputFieldError(field, '', {})).toBeUndefined();
  });

  it('runs a pattern, a length and a bound', () => {
    const pattern = only({
      type: 'plaintext',
      validate: [{ kind: 'pattern', pattern: '^OPS-\\d+$', message: 'must look like OPS-1234' }],
    }).field;
    expect(humanInputFieldError(pattern, 'nope', {})).toBe('must look like OPS-1234');
    expect(humanInputFieldError(pattern, 'OPS-7', {})).toBeUndefined();

    const length = only({
      type: 'plaintext',
      validate: [{ kind: 'length', min: 3, message: 'must be at least 3 characters' }],
    }).field;
    expect(humanInputFieldError(length, 'ab', {})).toBe('must be at least 3 characters');
    expect(humanInputFieldError(length, 'abc', {})).toBeUndefined();

    const bounds = only({
      type: 'plaintext',
      validate: [{ kind: 'bounds', max: 9, message: 'must be at most 9' }],
    }).field;
    expect(humanInputFieldError(bounds, '12', {})).toBe('must be at most 9');
    expect(humanInputFieldError(bounds, 'seven', {})).toBe('must be a number');
    expect(humanInputFieldError(bounds, '9', {})).toBeUndefined();
  });

  it('compares one field with another', () => {
    const { field } = only({
      type: 'password',
      validate: [{ kind: 'matches', field: 'pass', message: 'must match Password' }],
    });
    expect(humanInputFieldError(field, 'b', { pass: 'a' })).toBe('must match Password');
    expect(humanInputFieldError(field, 'a', { pass: 'a' })).toBeUndefined();
  });

  it('lets a rule the app has never heard of through to the engine', () => {
    // A newer daemon talking to an older app must not have its answer refused
    // here for a shape this build cannot even evaluate.
    const { field } = only({
      type: 'plaintext',
      validate: [{ kind: 'type', type: 'iban', message: 'must be an IBAN' }],
    });
    expect(humanInputFieldError(field, 'whatever', {})).toBeUndefined();
  });

  it('reads the rules the engine put on the wire', () => {
    const notify = parsed().fields.find((row) => row.id === 'notify') as HumanInputField;
    expect(notify.validate).toEqual([
      { kind: 'type', type: 'email', message: 'must be an email address' },
      { kind: 'length', max: 60, message: 'keep it short' },
    ]);
    expect(humanInputErrors(parsed(), { notify: 'nope' })).toMatchObject({
      notify: 'must be an email address',
    });
  });
});

describe('the form state machine', () => {
  const request = () =>
    humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [
        { id: 'who', type: 'plaintext', is_required: true },
        { id: 'code', type: 'otp' },
      ],
    }) as HumanInputRequest;

  it('starts pristine and silent, however wrong it already is', () => {
    const form = humanInputFormStart(request());
    expect(form).toEqual({ values: { who: '', code: '' }, touched: [], isSubmitAttempted: false });
    // Shouting "is required" at a form nobody has touched is the oldest form
    // bug there is.
    expect(humanInputFormErrors(form, request())).toEqual({});
  });

  it('speaks up about a field the operator has been in', () => {
    const start = humanInputFormStart(request());
    expect(humanInputFormErrors(humanInputFormBlur(start, 'who'), request())).toEqual({
      who: 'is required',
    });
    const typed = humanInputFormChange(start, 'code', '12');
    expect(typed.touched).toEqual(['code']);
    expect(humanInputFormErrors(typed, request())).toEqual({ code: 'must be 6 digits' });
    // …and stays quiet about the one it has not.
    expect(humanInputFormErrors(typed, request())['who']).toBeUndefined();
  });

  it('reveals everything at once when submit is pressed, and sends nothing', () => {
    const attempt = humanInputFormSubmit(humanInputFormStart(request()), request());
    expect(attempt.isReady).toBe(false);
    expect(attempt.errors).toEqual({ who: 'is required' });
    expect(attempt.form.isSubmitAttempted).toBe(true);
    expect(humanInputFormErrors(attempt.form, request())).toEqual({ who: 'is required' });
  });

  it('lets a complete answer through untouched', () => {
    const answered = humanInputFormChange(humanInputFormStart(request()), 'who', 'ops');
    const attempt = humanInputFormSubmit(answered, request());
    expect(attempt.isReady).toBe(true);
    expect(attempt.form).toBe(answered);
    expect(attempt.form.values).toEqual({ who: 'ops', code: '' });
  });

  it('shows the engine’s own refusal whether or not the field was touched', () => {
    const form = humanInputFormStart(request());
    expect(humanInputFormErrors(form, request(), { who: 'The daemon says no.' })).toEqual({
      who: 'The daemon says no.',
    });
  });

  it('touches a field once, however many times it is edited', () => {
    const twice = humanInputFormChange(
      humanInputFormChange(humanInputFormStart(request()), 'who', 'o'),
      'who',
      'op',
    );
    expect(twice.touched).toEqual(['who']);
    expect(humanInputFormBlur(twice, 'who').touched).toEqual(['who']);
  });
});
