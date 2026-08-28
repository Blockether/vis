import fixture from './human-input.fixture.json';
import { describe, expect, it } from 'vitest';
import type { SseEvent } from './types';
import { VIEW_CLOSE_EVENT, VIEW_OPEN_EVENT, isViewEvent } from './view';
import {
  applyInputViewEvent,
  clampHumanInputRange,
  humanInputRange,
  humanInputRequestFromWire,
  inputViewsFromWire,
  humanInputFormChange,
  humanInputFormRefused,
  humanInputFormStart,
  humanInputOtp,
  humanInputInputFields,
  humanInputIsDecoration,
  humanInputOtpDigits,
  initialHumanInputValues,
  isInputViewEvent,
  toggleHumanInputOption,
  type HumanInputField,
  type HumanInputRequest,
} from './human-input';

/** Engine `view/request->view` output; `gateway.view-test` pins these bytes. */
const WIRE = fixture as unknown;

function requested(view: unknown = WIRE): SseEvent {
  return { type: VIEW_OPEN_EVENT, kind: 'input', session_id: 'sid-1', view } as SseEvent;
}

function closed(viewId: string, reason = 'submitted'): SseEvent {
  return {
    type: VIEW_CLOSE_EVENT,
    kind: 'input',
    session_id: 'sid-1',
    view_id: viewId,
    result: { reason },
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
      // Two decorations lead the form. They have no name because nothing keys ink.
      ':heading',
      ':paragraph',
      'env:select',
      'key:password',
      'ok:checkbox',
      'tags:multiselect',
      'risk:range',
      'code:otp',
      'notify:plaintext',
      'notes:multiline',
      'group:host+group:port+tls:group',
    ]);
    // A group is a BRANCH: it answers nothing, and the leaves under it are the
    // fields the form actually submits.
    expect(humanInputInputFields(request.fields).map((field) => field.id)).toEqual([
      'env',
      'key',
      'ok',
      'tags',
      'risk',
      'code',
      'notify',
      'notes',
      'host',
      'port',
      'tls',
    ]);
    // name keys the answer, label is what the dialog shows, description is the
    // italic line under it. Found by name, not by index: ink leads this form.
    const env = request.fields.find((field) => field.id === 'env');
    const key = request.fields.find((field) => field.id === 'key');
    expect(env?.name).toBe('env');
    expect(env?.label).toBe('Env');
    expect(env?.description).toBe('Where this deploy lands');
    expect(key?.label).toBe('key');
    expect(key?.description).toBeUndefined();
    expect(key?.is_required).toBe(true);
    expect(key?.max_length).toBe(40);
    expect(env?.options).toEqual([
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

  it('names a field by its wire name, or by the `id` spelling of that name', () => {
    const named = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ name: 'token', type: 'plaintext', label: 'API token', description: 'from 1Password' }],
    });
    expect(named?.fields[0]?.name).toBe('token');
    expect(named?.fields[0]?.id).toBe('token');
    expect(named?.fields[0]?.label).toBe('API token');
    expect(named?.fields[0]?.description).toBe('from 1Password');
  });

  it('carries a deadline, and reads 0 as no deadline at all', () => {
    const deadline = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ id: 'a' }],
      timeout_ms: 120_000,
    });
    expect(deadline?.timeout_ms).toBe(120_000);

    // The engine's indefinite wait. Keeping the 0 would read as a deadline that
    // already passed on a form that is simply parked until the operator answers.
    const forever = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ id: 'a' }],
      timeout_ms: 0,
    });
    expect(forever?.timeout_ms).toBeUndefined();
    expect('timeout_ms' in (forever ?? {})).toBe(false);
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
    expect(inputViewsFromWire([WIRE, null, { id: 'x' }]).map((row) => row.id))
      .toEqual(['req-1']);
    expect(inputViewsFromWire(undefined)).toEqual([]);
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
      notes: '',
      tags: [],
      host: '',
      port: '',
      tls: false,
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
    // field silently vanishes and the POST omits the value the engine is
    // waiting for.
    expect(Object.prototype.hasOwnProperty.call(values, '__proto__')).toBe(true);
    const answered = humanInputFormChange(humanInputFormStart(request), '__proto__', 'typed');
    expect(answered.values['__proto__']).toBe('typed');
    expect(JSON.parse(JSON.stringify(answered.values))['__proto__']).toBe('typed');
  });
});

describe('required fields', () => {
  it('carries is_required through a layout group without judging anything', () => {
    // This used to gate the app's own submit button. The app no longer decides
    // whether an answer is good: `is_required` is a MARK next to the label, and
    // only the engine turns it into a refusal — with its own words.
    const required = humanInputInputFields(parsed().fields)
      .filter((field) => field.is_required)
      .map((field) => field.id);
    // `host` lives inside a layout group — grouping must not hide the mark.
    expect(required).toContain('host');
    expect(required).toContain('key');
    expect(required).toContain('code');
  });
});

describe('decorations', () => {
  it('reads the ink the engine sent, and answers none of it', () => {
    const request = parsed();
    expect(
      request.fields.filter(humanInputIsDecoration).map((node) => [node.type, node.text]),
    ).toEqual([
      ['heading', 'Target'],
      ['paragraph', 'Staging pages nobody.'],
    ]);
    // Nothing keys a decoration, so it is not one of the fields an answer walks
    // and it puts no entry in the values map.
    expect(humanInputInputFields(request.fields).some(humanInputIsDecoration)).toBe(false);
    expect(Object.keys(initialHumanInputValues(request))).not.toContain('');
  });

  it('drops ink with nothing to paint', () => {
    const request = humanInputRequestFromWire({
      ...(fixture as Record<string, unknown>),
      fields: [
        { type: 'heading', text: '   ' },
        { name: 'a', type: 'plaintext' },
      ],
    });
    expect(request?.fields.map((field) => field.type)).toEqual(['plaintext']);
  });
});

describe('toggleHumanInputOption', () => {
  it('adds, removes, and keeps the request option order', () => {
    const request = parsed();
    const tags = request.fields.find((field) => field.id === 'tags');
    if (!tags) throw new Error('fixture must have a multiselect field');
    expect(toggleHumanInputOption(tags, { tags: [] }, 'b')).toEqual(['b']);
    expect(toggleHumanInputOption(tags, { tags: ['b'] }, 'a')).toEqual(['a', 'b']);
    expect(toggleHumanInputOption(tags, { tags: ['a', 'b'] }, 'a')).toEqual(['b']);
  });
});

describe('applyInputViewEvent', () => {
  it('opens only input-capable Views on the shared lifecycle', () => {
    expect(isViewEvent(requested())).toBe(true);
    expect(isInputViewEvent(requested())).toBe(true);
    expect(isInputViewEvent({ ...requested(), kind: 'live' })).toBe(false);
    expect(isInputViewEvent({ type: 'turn.completed' } as SseEvent)).toBe(false);
    const pending = applyInputViewEvent([], requested());
    expect(pending.map((row) => row.id)).toEqual(['req-1']);
  });

  it('replaces on replay instead of stacking a second dialog', () => {
    const once = applyInputViewEvent([], requested());
    const twice = applyInputViewEvent(once, requested());
    expect(twice.map((row) => row.id)).toEqual(['req-1']);
    expect(twice[0]).not.toBe(once[0]);
  });

  it('drops the form when the request closes anywhere — TUI, timeout, or here', () => {
    const pending = applyInputViewEvent([], requested());
    expect(applyInputViewEvent(pending, closed('req-1'))).toEqual([]);
    expect(applyInputViewEvent(pending, closed('req-1', 'timeout'))).toEqual([]);
  });

  it('leaves the list ALONE for anything it cannot act on', () => {
    const pending = applyInputViewEvent([], requested());
    expect(applyInputViewEvent(pending, closed('other'))).toBe(pending);
    expect(applyInputViewEvent(pending, closed(''))).toBe(pending);
    expect(applyInputViewEvent(pending, requested({ id: 'bad' }))).toBe(pending);
    expect(applyInputViewEvent(pending, { type: 'turn.started' } as SseEvent)).toBe(pending);
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

  it('opens on the zero at the far left of the track', () => {
    const request = humanInputRequestFromWire({
      id: 'r',
      title: 'T',
      fields: [{ id: 'risk', type: 'range', min: 0, max: 10, default: 0 }],
    }) as HumanInputRequest;
    // 0 is falsy: a default read as `fallback || min` moves the slider off the
    // answer the extension asked for, forever.
    expect(initialHumanInputValues(request)).toEqual({ risk: 0 });
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

});

describe('validation', () => {
  it('parses no validator off the wire, because none is ever sent', () => {
    // `request->view` dissocs `:validate` before encoding: a validator is a
    // FUNCTION in the extension that asked the question, and a function cannot
    // be JSON. The app has nothing to run, so it has nothing to guess either.
    expect(JSON.stringify(fixture)).not.toContain('validate');
    const notify = parsed().fields.find((row) => row.id === 'notify') as HumanInputField;
    expect(notify).toBeDefined();
    expect('validate' in notify).toBe(false);
  });

  it('drops a rule an older daemon still puts on a field', () => {
    const { field } = only({
      type: 'plaintext',
      validate: [{ kind: 'type', type: 'email', message: 'must be an email address' }],
    });
    expect('validate' in field).toBe(false);
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

  it('starts pristine: the request’s defaults, and nothing to apologise for', () => {
    expect(humanInputFormStart(request())).toEqual({ values: { who: '', code: '' }, errors: {} });
  });

  it('never invents an error of its own, however wrong the answer is', () => {
    // A required field left blank and an OTP with two digits in it. The form has
    // no opinion at all: the validators are functions in the extension, so the
    // only verdict that exists is the one a confirmation brings back.
    const half = humanInputFormChange(humanInputFormStart(request()), 'code', '12');
    expect(half.errors).toEqual({});
  });

  it('reddens exactly the fields a refused confirmation named', () => {
    const refused = humanInputFormRefused(humanInputFormStart(request()), { who: 'is required' });
    expect(refused.errors).toEqual({ who: 'is required' });
    // The engine's verdict needs no touch first, and the answers are untouched.
    expect(refused.values).toEqual({ who: '', code: '' });
  });

  it('drops a field’s error on the first touch, and only that field’s', () => {
    const refused = humanInputFormRefused(humanInputFormStart(request()), {
      who: 'is required',
      code: 'must be 6 digits',
    });
    const typed = humanInputFormChange(refused, 'who', 'o');
    expect(typed.errors).toEqual({ code: 'must be 6 digits' });
    expect(typed.values.who).toBe('o');
    // Editing on: still nothing re-validates until the next confirmation.
    expect(humanInputFormChange(typed, 'who', 'op').errors).toEqual({
      code: 'must be 6 digits',
    });
  });

  it('replaces the last refusal with the next one, and clears it when accepted', () => {
    const first = humanInputFormRefused(humanInputFormStart(request()), { who: 'is required' });
    expect(humanInputFormRefused(first, { code: 'must be 6 digits' }).errors).toEqual({
      code: 'must be 6 digits',
    });
    expect(humanInputFormRefused(first, {}).errors).toEqual({});
  });
});
