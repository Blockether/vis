// @vitest-environment jsdom
// The sheet opens in `Modal`, which PORTALS into the document — there is no
// document in the `node` environment, and a portal cannot be rendered to a
// string. Every case here renders and then reads the body it landed in.
import { cleanup, render, screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { HumanInputSheet } from './HumanInputPrompt';
import { HUMAN_INPUT_REQUESTS } from '../dev/humanInputVariants';
import fixture from '../lib/human-input.fixture.json';
import {
  humanInputIsDecoration,
  humanInputRequestFromWire,
  initialHumanInputValues,
  HUMAN_INPUT_CHOICE_MARKS,
  HUMAN_INPUT_NODE_TYPES,
  type HumanInputField,
  type HumanInputValues,
} from '../lib/human-input';

const noop = () => {};

function markup(state: string, extra: Record<string, unknown> = {}, values?: HumanInputValues) {
  const request = HUMAN_INPUT_REQUESTS[state];
  // One sheet in the document at a time: two mounts would leave the previous
  // request's markup in the string this returns.
  cleanup();
  render(
    <HumanInputSheet
      request={request}
      values={values ?? initialHumanInputValues(request)}
      onChange={noop}
      onSubmit={noop}
      onCancel={noop}
      {...extra}
    />,
  );
  return document.body.innerHTML;
}

const otpValues = initialHumanInputValues(HUMAN_INPUT_REQUESTS.otp);

/**
 * The outer HTML of the first element whose attributes contain `marker`, found
 * by walking tag depth — enough of a parser to ask "is this inside that?".
 */
function element(html: string, marker: string): string {
  const open = html.lastIndexOf('<', html.indexOf(marker));
  let depth = 0;
  const tag = /<(\/?)div\b[^>]*?(\/?)>/g;
  tag.lastIndex = open;
  for (let hit = tag.exec(html); hit; hit = tag.exec(html)) {
    if (hit[2] === '/') continue;
    depth += hit[1] === '/' ? -1 : 1;
    if (depth === 0) return html.slice(open, hit.index + hit[0].length);
  }
  throw new Error(`unterminated element for ${marker}`);
}

describe('human input sheet', () => {
  // Regression #282: request descriptions are Markdown, not a plain paragraph.
  it('renders bold labels on three authored lines in a request description', () => {
    markup('minimal', {
      request: {
        ...HUMAN_INPUT_REQUESTS.minimal,
        description:
          '**Environment:** DEV01\n**Service:** glms-plcx-svc\n**Command:** :init-batch-report',
      },
    });
    const labels = ['Environment:', 'Service:', 'Command:'].map((label) => screen.getByText(label));
    const paragraph = labels[0].closest('p')!;
    expect(labels.every((label) => label.tagName === 'STRONG')).toBe(true);
    expect(paragraph.querySelectorAll('br')).toHaveLength(2);
    expect(paragraph.textContent).toBe(
      'Environment: DEV01\nService: glms-plcx-svc\nCommand: :init-batch-report',
    );
    expect(paragraph.textContent).not.toContain('**');
  });

  it('keeps raw HTML inert and rejects unsafe links in paragraph Markdown', () => {
    markup('minimal', {
      request: {
        ...HUMAN_INPUT_REQUESTS.minimal,
        nodes: [
          {
            id: 'warning',
            name: 'warning',
            type: 'paragraph',
            label: '',
            is_required: false,
            text: '**Safe** <img src=x onerror="alert(1)"> [unsafe](javascript:alert(1))',
          },
        ],
      },
    });
    const dialog = screen.getByRole('dialog');
    expect(screen.getByText('Safe').tagName).toBe('STRONG');
    expect(dialog.querySelector('img, script, [onerror], a[href^="javascript:"]')).toBeNull();
    expect(dialog.textContent).toContain('<img src=x onerror="alert(1)">');
  });

  it('offers no way out of a pause that has none', () => {
    const html = markup('uncancellable');
    expect(html).not.toContain('Cancel this request');
    expect(html).not.toContain('Esc cancel');
    expect(html).toContain('Unlock');
    const cancellable = markup('approve');
    // The way out of a question CANCELS it — the one dialog whose leaving is not
    // simply "Close <title>", and it says so.
    expect(cancellable).toContain('Cancel this request');
    expect(cancellable).toContain('Not now');
  });

  it('never invents a verdict: pristine says nothing, and submit still fires', () => {
    // A disabled button with no reason next to it is the worst answer to "why
    // can I not send this" — and the app cannot even know: the validators are
    // functions in the extension. So the sheet stays pressable and stays quiet
    // until the engine refuses something.
    const off = (html: string) => /<button[^>]*\sdisabled=""/.test(html);
    const buttons = 'flex gap-2 sm:justify-end';
    const pristine = markup('minimal');
    expect(off(element(pristine, buttons))).toBe(false);
    expect(pristine).not.toContain('is required');
    // The engine's refusal needs no touch, and it does not deaden the button:
    // the operator fixes the field and confirms again.
    const refused = markup('minimal', { errors: { branch: 'is required' } });
    expect(refused).toContain('is required');
    expect(off(element(refused, buttons))).toBe(false);
  });

  it('connects every written-field label and refusal to its native control', () => {
    markup('grouped', { errors: { user: 'Use a service account.' } });

    expect(screen.getByRole('textbox', { name: 'User, required' })).toBeVisible();
    const password = screen.getByLabelText(/Password/);
    expect(screen.getByRole('textbox', { name: 'Notes' })).toBeVisible();
    const user = screen.getByRole('textbox', { name: 'User, required' });
    const describedBy = user.getAttribute('aria-describedby');
    expect(document.getElementById(describedBy ?? '')?.textContent).toBe('Use a service account.');
    expect(user.getAttribute('aria-invalid')).toBe('true');
    expect(user.getAttribute('aria-required')).toBe('true');
    expect(password.getAttribute('aria-required')).toBe('true');
  });

  it('masks a password and gives a checkbox a drawn, accessible mark', () => {
    const html = markup('slider');
    expect(html).toContain('type="password"');
    const checkbox = screen.getByRole('checkbox', { name: 'Halt on the first regression' });
    expect(checkbox).toHaveAttribute('aria-checked', 'true');
    expect(checkbox).not.toHaveAttribute('aria-pressed');
    expect(screen.getAllByText('Halt on the first regression')).toHaveLength(1);
    expect(html).not.toContain(HUMAN_INPUT_CHOICE_MARKS.inclusiveOn);
  });

  it('draws one box per digit over one real code input', () => {
    const html = markup('otp', {}, { ...otpValues, code: '408' });
    const row = element(html, 'data-otp-row');
    const inputs = row.match(/<input[^>]*>/g) ?? [];
    expect(inputs).toHaveLength(1);
    // The keypad, not the alphabet: a code field that opens a QWERTY keyboard on
    // a phone costs the operator every tap it takes to find the numbers.
    expect(inputs[0]).toContain('inputmode="numeric"');
    expect(inputs[0]).toContain('pattern="[0-9]*"');
    // One input claims the SMS autofill once, and a paste can carry the whole code.
    expect(inputs[0]).toContain('autocomplete="one-time-code"');
    expect(inputs[0]).not.toContain('maxlength');
    expect(inputs[0]).toContain('value="408"');
    const boxes = row.match(/<span[^>]*data-otp-box="\d"[^>]*>[^<]*<\/span>/g) ?? [];
    expect(boxes).toHaveLength(6);
    expect(boxes.slice(0, 4).map((box) => />(\d?)<\/span>$/.exec(box)?.[1])).toEqual([
      '4',
      '0',
      '8',
      '',
    ]);
    expect(html).toContain('6 digits');
  });

  // Formik's lesson, both halves of it: a form that shouts before anyone has
  // typed is noise, and one that stays quiet after a refusal is a trap. Since
  // every validator lives in the extension that asked the question, the ONLY
  // thing that can speak here is a confirmation the engine turned down — and it
  // speaks for every field it named, touched or not.
  it('names a broken field only once a confirmation came back refused', () => {
    const values = { ...otpValues, code: '408', notify: 'ops@' };
    const pristine = markup('otp', {}, values);
    expect(pristine).not.toContain('must be an email address');
    expect(pristine).not.toContain('must be 6 digits');

    const refused = markup(
      'otp',
      { errors: { code: 'must be 6 digits', notify: 'must be an email address' } },
      values,
    );
    expect(refused).toContain('must be 6 digits');
    expect(refused).toContain('must be an email address');
  });
});

// FULL support, proved against the engine's own bytes
//
// `human-input.fixture.json` is `request->view` verbatim, and the Clojure suite
// that re-derives it also pins that it holds ONE NODE OF EVERY KIND the engine
// can send. Rendering that very request is therefore this app's proof of
// complete human-input support: a node type nobody wired up is a hole in a
// dialog that has already stopped somebody's run, and it fails here instead of
// in front of the operator.
describe('the engine’s whole node vocabulary', () => {
  const request = humanInputRequestFromWire(fixture);
  if (!request) throw new Error('the engine fixture must parse');
  cleanup();
  render(
    <HumanInputSheet
      request={request}
      values={initialHumanInputValues(request)}
      onChange={noop}
      onSubmit={noop}
      onCancel={noop}
    />,
  );
  const html = document.body.innerHTML;
  cleanup();

  /** Every node of the tree, groups and their children alike. */
  function nodes(fields: readonly HumanInputField[]): HumanInputField[] {
    return fields.flatMap((field) => [field, ...nodes(field.children ?? [])]);
  }

  it('is what the engine sends — nothing skipped, nothing invented', () => {
    expect([...new Set(nodes(request.nodes).map((field) => field.type))].sort()).toEqual(
      [...HUMAN_INPUT_NODE_TYPES].sort(),
    );
  });

  it('paints ink as ink: read, never answered, never keyed', () => {
    expect(html).toContain('<h3');
    expect(html).toContain('Target');
    expect(html).toContain('Staging pages nobody.');
    // A decoration has no name and a group holds no answer, so the values map
    // is exactly the answerable leaves.
    expect(Object.keys(initialHumanInputValues(request)).sort()).toEqual(
      nodes(request.nodes)
        .filter((field) => field.type !== 'group' && !humanInputIsDecoration(field))
        .map((field) => field.id)
        .sort(),
    );
  });

  it('marks what the engine requires, for the eye and for a screen reader', () => {
    const required = nodes(request.nodes).filter((field) => field.is_required);
    expect(required.map((field) => field.id).sort()).toEqual(['code', 'host', 'key']);
    expect(html.match(/<span class="sr-only">, required<\/span>/g) ?? []).toHaveLength(
      required.length,
    );
  });
});

// A PAUSE STANDS IN THE SESSION THAT ASKED IT

// Regression, user report: on a wide window the form covered and dimmed the whole
// application — session list, header and transcript — while a single session waited.
// An opened run already stands in the session's own pane (`Modal within="session"`),
// and the question that parks a run belongs in exactly the same place.
describe('a pause stands in the session that asked it', () => {
  const openSheet = () => {
    cleanup();
    const request = HUMAN_INPUT_REQUESTS.otp;
    render(
      <HumanInputSheet
        request={request}
        values={initialHumanInputValues(request)}
        onChange={noop}
        onSubmit={noop}
        onCancel={noop}
      />,
    );
    return screen.getByRole('dialog');
  };

  it('mounts in the session pane, leaving the desk beside it uncovered', () => {
    const shell = document.createElement('div');
    shell.setAttribute('data-viewport-shell', '');
    const pane = document.createElement('div');
    pane.setAttribute('data-session-surface', '');
    document.body.append(shell, pane);
    try {
      const dialog = openSheet();
      expect(pane.contains(dialog)).toBe(true);
      expect(shell.contains(dialog)).toBe(false);
      // Scrim and box alike: the pane is the bound, so nothing outside it is dimmed.
    } finally {
      shell.remove();
      pane.remove();
    }
  });

  // No session on screen is no pane to stand in, and a question may not disappear
  // with it: the app shell keeps the keyboard behaviour every layer mounts for.
  it('falls back to the app shell when no session pane is on screen', () => {
    const shell = document.createElement('div');
    shell.setAttribute('data-viewport-shell', '');
    document.body.append(shell);
    try {
      const dialog = openSheet();
      expect(shell.contains(dialog)).toBe(true);
    } finally {
      shell.remove();
    }
  });
});

// Issue #392: the origin line names a subagent and its parent. Long titles must
// shorten with an ellipsis on one row, never wrap or push the parent away.
describe('human input origin', () => {
  const longParent = 'Deploy the release candidate to DEV13';

  function originRow(origin?: Record<string, string>) {
    cleanup();
    const request = humanInputRequestFromWire({
      id: 'r',
      title: 'PingID code',
      nodes: [{ id: 'code', type: 'plaintext' }],
      ...(origin ? { origin } : {}),
    });
    if (!request) throw new Error('the request must parse');
    render(
      <HumanInputSheet
        request={request}
        values={initialHumanInputValues(request)}
        onChange={noop}
        onSubmit={noop}
        onCancel={noop}
      />,
    );
    return screen.queryByTestId('human-input-origin');
  }

  it('shows no origin row for a top-level session', () => {
    expect(originRow({ session_id: 'leader-1', title: longParent })).toBeNull();
    expect(originRow()).toBeNull();
  });
});
