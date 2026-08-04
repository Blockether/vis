/**
 * WHAT DOES A RUN THAT STOPPED TO ASK YOU SOMETHING LOOK LIKE?
 *
 * `vis.request_human_input` parks the run until the operator answers, and the
 * companion has to say three things at once: what is being asked, that the
 * agent is BLOCKED on it, and how to answer it with one thumb. The proposals
 * below are all the SHIPPED sheet (`HumanInputSheet`) under different requests
 * — the point of the gallery here is not to compare skins, it is to photograph
 * the states that can break the one design we have:
 *
 *   long        the answer is longer than the phone; the buttons must survive
 *   uncancellable  there is no way out; nothing may pretend otherwise
 *   rejected    the engine refused the answer and has to say where
 *   minimal     a one-line question must not look like an empty dialog
 *
 * The backdrop is a parked transcript, because a modal photographed on white
 * paper never shows whether it reads as an interruption.
 */

import { useState } from 'react';
import { createPortal } from 'react-dom';
import { HumanInputSheet } from '../components/HumanInputPrompt';
import {
  initialHumanInputValues,
  type HumanInputField,
  type HumanInputRequest,
} from '../lib/human-input';

function field(
  id: string,
  type: HumanInputField['type'],
  label: string,
  extra: Partial<HumanInputField> = {},
): HumanInputField {
  return { id, name: id, type, label, is_required: false, ...extra };
}

/**
 * One request per photographed state. The gallery reads the state names off
 * THIS map, so a state can never be declared without a request behind it —
 * that mistake produces two byte-identical PNGs and a reviewer who believes
 * they compared something.
 */
export const HUMAN_INPUT_REQUESTS: Record<string, HumanInputRequest> = {
  approve: {
    id: 'req-approve',
    title: 'Deploy to production?',
    description: 'The migration drops `sessions.legacy_id`. It cannot be undone.',
    source: 'deploy.sh',
    fields: [
      field('env', 'select', 'Target', {
        is_required: true,
        default: 'staging',
        options: [
          { value: 'staging', label: 'staging' },
          { value: 'production', label: 'production' },
        ],
      }),
      field('confirm', 'checkbox', 'I have read the migration plan', {
        is_required: true,
      }),
      field('note', 'plaintext', 'Note for the changelog', {
        placeholder: 'optional',
        max_length: 72,
      }),
    ],
    submit_label: 'Deploy',
    cancel_label: 'Not now',
    is_cancellable: true,
  },
  long: {
    id: 'req-long',
    title: 'Release checklist',
    description:
      'Eight answers before the tag is pushed. The list scrolls; the two buttons that end this pause do not.',
    source: 'release-vis',
    fields: [
      field('version', 'plaintext', 'Version', {
        is_required: true,
        default: '1.14.0',
      }),
      field('channel', 'select', 'Channel', {
        is_required: true,
        default: 'stable',
        options: [
          { value: 'stable', label: 'stable' },
          { value: 'beta', label: 'beta' },
        ],
      }),
      field('stores', 'multiselect', 'Ship to', {
        options: [
          { value: 'ios', label: 'TestFlight' },
          { value: 'android', label: 'Google Play' },
          { value: 'clojars', label: 'Clojars' },
        ],
        default: ['ios', 'android'],
      }),
      field('notes', 'multiline', 'Release notes', {
        is_required: true,
        placeholder: 'What changed, in the user’s words',
        default: 'Human input now answers in the app.',
      }),
      field('smoke', 'checkbox', 'Native image booted locally'),
      field('tag', 'checkbox', 'Working tree is clean'),
      field('otp', 'password', 'Store 2FA code', { is_required: true }),
      field('who', 'plaintext', 'On call after the release', {
        default: 'fierycod',
      }),
    ],
    submit_label: 'Cut the release',
    cancel_label: 'Abort',
    is_cancellable: true,
  },
  uncancellable: {
    id: 'req-secret',
    title: 'Unlock the signing keychain',
    description: 'The run cannot continue without it, and it cannot be cancelled from here.',
    source: 'release:ios',
    fields: [
      field('passphrase', 'password', 'Keychain passphrase', {
        is_required: true,
      }),
    ],
    submit_label: 'Unlock',
    cancel_label: 'Cancel',
    is_cancellable: false,
  },
  rejected: {
    id: 'req-rejected',
    title: 'Deploy to production?',
    description: 'The migration drops `sessions.legacy_id`. It cannot be undone.',
    source: 'deploy.sh',
    fields: [
      field('env', 'select', 'Target', {
        is_required: true,
        default: 'production',
        options: [
          { value: 'staging', label: 'staging' },
          { value: 'production', label: 'production' },
        ],
      }),
      field('ticket', 'plaintext', 'Change ticket', {
        is_required: true,
        default: 'OPS',
      }),
      field('confirm', 'checkbox', 'I have read the migration plan', {
        is_required: true,
      }),
    ],
    submit_label: 'Deploy',
    cancel_label: 'Not now',
    is_cancellable: true,
  },
  minimal: {
    id: 'req-minimal',
    title: 'What should I name the branch?',
    fields: [field('branch', 'plaintext', 'Branch', { is_required: true })],
    submit_label: 'Submit',
    cancel_label: 'Cancel',
    is_cancellable: true,
  },
  slider: {
    id: 'req-slider',
    title: 'How much of the error budget may this rollout spend?',
    description: 'The rollout stops on its own once the budget is gone.',
    source: 'rollout.py',
    fields: [
      field('risk', 'range', 'Error budget', {
        description: 'Percent of the monthly budget this rollout may burn',
        min: 0,
        max: 10,
        step: 0.5,
        default: 2.5,
      }),
      field('halt', 'checkbox', 'Halt on the first regression', { default: true }),
      field('token', 'password', 'Deploy token', { is_required: true }),
    ],
    submit_label: 'Roll out',
    cancel_label: 'Hold',
    is_cancellable: true,
  },
};

/** The parked run behind the sheet: the last thing the agent said before it stopped. */
function ParkedTranscript() {
  return (
    <section className="flex h-full min-h-0 flex-col bg-ink">
      <header className="flex min-h-13 shrink-0 items-center gap-2 border-b border-dialog-edge bg-panel-2 px-3">
        <span className="truncate font-mono text-ui text-white">vis · release</span>
        <span className="ml-auto shrink-0 font-mono text-chip text-dialog-hint">5ca90155</span>
      </header>
      <div className="min-h-0 flex-1 space-y-3 overflow-hidden p-3">
        <p className="font-mono text-meta text-dialog-hint">$ vis run release --dry-run</p>
        <p className="text-body text-white">
          I staged the version bump and mirrored it into the companion. Before I push the tag I need
          one thing from you.
        </p>
        <p className="font-mono text-meta text-dialog-hint">⏸ waiting for human input · 0:14</p>
      </div>
    </section>
  );
}

/** The matrix the gallery photographs: the page owns it, the script reads it. */
export const HUMAN_INPUT_STATES = Object.keys(HUMAN_INPUT_REQUESTS);

/**
 * Every state is the same component under a different request, so a change to
 * the shipped sheet shows up in all five photographs at once.
 */

export function HumanInputSheetVariant({ state }: { state: string }) {
  const request = HUMAN_INPUT_REQUESTS[state];
  const [values, setValues] = useState(() =>
    request ? initialHumanInputValues(request) : {},
  );
  if (!request) throw new Error(`no human-input request for state "${state}"`);
  const noop = () => {};
  return (
    <>
      <ParkedTranscript />
      {/* The shipped prompt portals to `document.body`. Rendering the sheet
          inline here would trap it in the gallery chrome's stacking context
          and photograph the tab bar sitting on top of the answer buttons. */}
      {createPortal(
        <HumanInputSheet
          request={request}
          values={values}
          {...(state === 'rejected'
            ? {
                error: 'The engine refused this answer.',
                fieldErrors: {
                  ticket: 'Must look like OPS-1234.',
                  confirm: 'Required before a production deploy.',
                },
              }
            : {})}
          {...(state === 'long' ? { waiting: 2 } : {})}
          onChange={(id, value) => setValues((prev) => ({ ...prev, [id]: value }))}
          onSubmit={noop}
          onCancel={noop}
        />,
        document.body,
      )}
    </>
  );
}
