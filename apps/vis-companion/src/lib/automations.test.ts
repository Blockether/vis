import { describe, expect, it } from 'vitest';
import schema from '../../../../packages/vis-contract/resources/vis-contract/schema/automations.json';
import {
  NAME_MAX,
  RUN_EVENT_OPTIONS,
  SIGNATURE_OPTIONS,
  TARGET_OPTIONS,
  TRIGGER_KIND_OPTIONS,
  TRIGGERS_MAX,
  automationDraft,
  automationInput,
  automationPatch,
  automationSummary,
  deliveryLabel,
  draftProblem,
  everyLabel,
  localInput,
  runReason,
  targetLabel,
  triggerLabel,
  wordLabel,
  type Automation,
  type AutomationDraft,
} from './automations';
import { timeLabel } from './fleet';
import { STORY_AUTOMATION_RUN, STORY_AUTOMATIONS } from '../dev/story-data';

const NOW = Date.UTC(2026, 5, 1, 12, 0, 0);
const HOUR = 3_600_000;
const DAY = 24 * HOUR;

describe('automation labels', () => {
  it.each([
    [60, 'Every minute'],
    [900, 'Every 15 minutes'],
    [3_600, 'Every hour'],
    [7_200, 'Every 2 hours'],
    [86_400, 'Every day'],
    [90, 'Every 90 seconds'],
  ])('names an interval of %i seconds as %j', (seconds, label) => {
    expect(everyLabel(seconds)).toBe(label);
  });

  it('names each trigger kind and keeps an unknown kind readable', () => {
    expect(
      triggerLabel({ kind: 'cron', expression: '0 9 * * 1-5', timezone: 'Europe/Warsaw' }, NOW),
    ).toBe('Cron 0 9 * * 1-5 · Europe/Warsaw');
    expect(triggerLabel({ kind: 'cron', expression: '*/5 * * * *' }, NOW)).toBe('Cron */5 * * * *');
    expect(triggerLabel({ kind: 'every', seconds: 1_800 }, NOW)).toBe('Every 30 minutes');
    expect(triggerLabel({ kind: 'once', at: NOW + 2 * HOUR }, NOW)).toBe(
      `Once · ${timeLabel(NOW + 2 * HOUR, NOW)}`,
    );
    expect(triggerLabel({ kind: 'webhook', signature: 'github' }, NOW)).toBe('Webhook · GitHub');
    expect(triggerLabel({ kind: 'webhook', signature: 'custom' }, NOW)).toBe('Webhook · custom');
    expect(triggerLabel({ kind: 'calendar' }, NOW)).toBe('Calendar');
  });

  it('labels a millisecond time like the session list does', () => {
    expect(timeLabel(NOW - 2 * HOUR, NOW)).toBe(timeLabel(new Date(NOW - 2 * HOUR).toISOString(), NOW));
    expect(timeLabel(undefined, NOW)).toBe('-');
  });

  it('describes targets and delivery in plain words', () => {
    expect(targetLabel({ mode: 'session', session_id: 'session-1' })).toBe(
      'Continues one existing session',
    );
    expect(targetLabel({ mode: 'new', root: '/Users/ana/code/vis' })).toBe(
      'New session for each run in ~/code/vis',
    );
    expect(targetLabel({ mode: 'temporary' })).toBe(
      'Temporary session for each run, deleted after the run',
    );
    expect(deliveryLabel({ push: true, callback: null })).toBe('Phone alert');
    expect(deliveryLabel({})).toBe('Phone alert');
    expect(
      deliveryLabel({ push: false, callback: { url: 'https://gateway.example.com/results' } }),
    ).toBe('Callback to https://gateway.example.com/results');
    expect(deliveryLabel({ push: false, callback: null })).toBe('Only the target session');
  });

  it('explains run reason codes and keeps other text as it is', () => {
    expect(runReason({ ...STORY_AUTOMATION_RUN, status: 'skipped', reason: 'settings' })).toBe(
      'settings',
    );
    expect(runReason({ ...STORY_AUTOMATION_RUN, status: 'skipped', reason: 'overlap' })).toBe(
      'The previous run was still running.',
    );
    expect(
      runReason({ ...STORY_AUTOMATION_RUN, status: 'failed', error: 'The run asked for input.' }),
    ).toBe('The run asked for input.');
    expect(runReason(STORY_AUTOMATION_RUN)).toBeNull();
    expect(wordLabel('manual')).toBe('Manual');
  });

  it('summarizes a row with its state, triggers, next run and last result', () => {
    const [standup, review] = STORY_AUTOMATIONS;
    const next = NOW + 3 * HOUR;
    expect(automationSummary({ ...standup, next_run_at: next }, NOW)).toBe(
      `Cron 0 9 * * 1-5 · Europe/Warsaw · Next ${timeLabel(next, NOW)} · Last completed`,
    );
    expect(automationSummary(review, NOW)).toBe('Paused · Webhook · GitHub · Last failed');
  });

  it('keeps the story fixtures on the wire contract', () => {
    for (const automation of STORY_AUTOMATIONS) {
      expect(Object.keys(automation).sort()).toEqual([...schema.$defs.automation.required].sort());
    }
    expect(Object.keys(STORY_AUTOMATION_RUN).sort()).toEqual([...schema.$defs.run.required].sort());
    expect(schema.$defs.run_status.enum).toContain(STORY_AUTOMATION_RUN.status);
    expect(schema.$defs.run_trigger_kind.enum).toContain(STORY_AUTOMATION_RUN.trigger);
  });
});

const TRIGGER_DEFS = {
  cron: schema.$defs.cron_trigger,
  every: schema.$defs.every_trigger,
  once: schema.$defs.once_trigger,
  webhook: schema.$defs.webhook_trigger,
};

/** Only declared keys, and every required key: the schemas allow no other properties. */
function expectContract(value: object, def: { properties: object; required?: string[] }) {
  expect(Object.keys(value).filter((key) => !(key in def.properties))).toEqual([]);
  expect((def.required ?? []).filter((key) => !(key in value))).toEqual([]);
}

describe('automation form', () => {
  it('saves nothing while the form keeps the saved values', () => {
    for (const automation of STORY_AUTOMATIONS) {
      expect(automationPatch(automationDraft(automation, NOW, 'UTC'), automation)).toEqual({});
    }
  });

  it('starts a new automation as one run each day in a new session', () => {
    const draft = {
      ...automationDraft(null, NOW, 'Europe/Warsaw'),
      name: ' Nightly check ',
      prompt: 'Check the build.',
    };
    expect(draftProblem(draft, NOW)).toBeNull();
    expect(automationInput(draft)).toEqual({
      name: 'Nightly check',
      prompt: 'Check the build.',
      triggers: [{ kind: 'every', seconds: 86_400 }],
      target: { mode: 'new' },
      delivery: { push: true, callback: null },
      model: null,
      deliver_only: false,
    });
  });

  it('builds each trigger kind, the target and the delivery on the contract', () => {
    const base = automationDraft(null, NOW, 'Europe/Warsaw');
    const [trigger] = base.triggers;
    const draft: AutomationDraft = {
      ...base,
      name: 'Release watch',
      prompt: 'Report the release.',
      triggers: [
        { ...trigger, kind: 'cron', expression: ' 0 9 * * 1-5 ' },
        { ...trigger, count: '15', unit: 'minute' },
        { ...trigger, kind: 'once', at: localInput(NOW + DAY) },
        { ...trigger, kind: 'webhook', signature: 'standard', events: 'push, pull_request push' },
      ],
      target: 'temporary',
      root: ' /srv/app ',
      callbackUrl: 'https://gateway.example.com/results',
      callbackEvents: ['run.failed', 'run.completed'],
      answer: 'model',
      model: 'gpt-5.4',
    };
    expect(draftProblem(draft, NOW)).toBeNull();
    const input = automationInput(draft);
    expect(input.triggers).toEqual([
      { kind: 'cron', expression: '0 9 * * 1-5', timezone: 'Europe/Warsaw' },
      { kind: 'every', seconds: 900 },
      { kind: 'once', at: NOW + DAY },
      { kind: 'webhook', signature: 'standard', events: ['push', 'pull_request'] },
    ]);
    expect(input.target).toEqual({ mode: 'temporary', root: '/srv/app' });
    expect(input.delivery).toEqual({
      push: true,
      callback: {
        url: 'https://gateway.example.com/results',
        events: ['run.completed', 'run.failed'],
      },
    });
    expect(input.model).toEqual({ model: 'gpt-5.4' });
    expectContract(input, schema.$defs.automation_input);
    for (const item of input.triggers)
      expectContract(item, TRIGGER_DEFS[item.kind as keyof typeof TRIGGER_DEFS]);
    expectContract(input.target, schema.$defs.temporary_target);
    expectContract(input.delivery!, schema.$defs.delivery);
    expectContract(input.delivery!.callback!, schema.$defs.callback);
    expectContract(input.model!, schema.$defs.model_pin);
    expectContract(automationPatch(draft, STORY_AUTOMATIONS[0]), schema.$defs.automation_patch);
  });

  it('changes only what you edit and keeps the fields that the form does not show', () => {
    const [standup, review] = STORY_AUTOMATIONS;
    const filters = [{ field: 'action', equals: 'opened' }];
    const filtered: Automation = {
      ...review,
      triggers: [{ kind: 'webhook', signature: 'github', events: ['pull_request'], filters }],
      target: { mode: 'new', group_id: 'group-1' },
    };
    const draft = automationDraft(filtered, NOW, 'UTC');
    expect(automationPatch({ ...draft, prompt: 'Review it.' }, filtered)).toEqual({
      prompt: 'Review it.',
    });
    expect(
      automationPatch(
        { ...draft, triggers: [{ ...draft.triggers[0], events: 'pull_request, push' }] },
        filtered,
      ),
    ).toEqual({
      triggers: [
        { kind: 'webhook', signature: 'github', events: ['pull_request', 'push'], filters },
      ],
    });
    expect(automationPatch({ ...draft, root: '/srv/app' }, filtered)).toEqual({
      target: { mode: 'new', root: '/srv/app', group_id: 'group-1' },
    });
    const standupDraft = automationDraft(standup, NOW, 'UTC');
    expect(automationPatch({ ...standupDraft, answer: 'prompt' }, standup)).toEqual({
      deliver_only: true,
    });
    expect(
      automationPatch(
        { ...standupDraft, push: false, callbackUrl: 'https://gateway.example.com/results' },
        standup,
      ),
    ).toEqual({
      delivery: { push: false, callback: { url: 'https://gateway.example.com/results' } },
    });
  });

  it('keeps the exact time of a saved one-time trigger and asks for a future time', () => {
    const at = NOW - DAY + 30_500;
    const automation: Automation = { ...STORY_AUTOMATIONS[0], triggers: [{ kind: 'once', at }] };
    const draft = automationDraft(automation, NOW, 'UTC');
    expect(draft.triggers[0].at).toBe(localInput(at));
    expect(draftProblem(draft, NOW)).toBeNull();
    expect(automationPatch(draft, automation)).toEqual({});
    const [trigger] = draft.triggers;
    expect(
      draftProblem({ ...draft, triggers: [{ ...trigger, at: localInput(NOW - 60_000) }] }, NOW),
    ).toBe('Choose a time in the future.');
    expect(draftProblem({ ...draft, triggers: [{ ...trigger, at: '' }] }, NOW)).toBe(
      'Choose the date and time of the run.',
    );
  });

  it('names the first problem before the gateway sees the form', () => {
    const valid = { ...automationDraft(null, NOW, 'UTC'), name: 'Check', prompt: 'Check it.' };
    const [trigger] = valid.triggers;
    const interval = 'Give an interval from 1 minute to 365 days.';
    expect(draftProblem(valid, NOW)).toBeNull();
    expect(draftProblem({ ...valid, name: '  ' }, NOW)).toBe('Give the automation a name.');
    expect(draftProblem({ ...valid, prompt: '\n' }, NOW)).toBe('Write the prompt.');
    expect(draftProblem({ ...valid, prompt: 'é'.repeat(8_193) }, NOW)).toBe(
      'Make the prompt shorter. The limit is 16384 bytes.',
    );
    expect(
      draftProblem({ ...valid, triggers: [{ ...trigger, count: '30', unit: 'second' }] }, NOW),
    ).toBe(interval);
    expect(draftProblem({ ...valid, triggers: [{ ...trigger, count: '366' }] }, NOW)).toBe(
      interval,
    );
    expect(draftProblem({ ...valid, triggers: [{ ...trigger, count: '1.5' }] }, NOW)).toBe(
      interval,
    );
    expect(
      draftProblem(
        { ...valid, triggers: [trigger, { ...trigger, kind: 'cron', expression: ' ' }] },
        NOW,
      ),
    ).toBe('Trigger 2: Give a cron expression, for example 0 9 * * 1-5.');
    expect(
      draftProblem(
        {
          ...valid,
          triggers: [
            { ...trigger, kind: 'webhook' },
            { ...trigger, kind: 'webhook' },
          ],
        },
        NOW,
      ),
    ).toBe('An automation can have only one webhook trigger.');
    expect(draftProblem({ ...valid, target: 'session' }, NOW)).toBe('Give the session ID.');
    expect(draftProblem({ ...valid, callbackUrl: 'gateway.example.com' }, NOW)).toBe(
      'Start the callback address with https:// or http://.',
    );
    expect(draftProblem({ ...valid, answer: 'model' }, NOW)).toBe('Give the model name.');
  });

  it('takes its choices and limits from the contract', () => {
    expect(TRIGGER_KIND_OPTIONS.map((option) => option.value)).toEqual([
      'cron',
      'every',
      'once',
      'webhook',
    ]);
    expect(SIGNATURE_OPTIONS.map((option) => option.value)).toEqual(
      schema.$defs.webhook_trigger.properties.signature.enum,
    );
    expect(RUN_EVENT_OPTIONS.map((option) => option.label)).toEqual([
      'Completed',
      'Failed',
      'Cancelled',
      'Skipped',
      'Unknown',
    ]);
    expect(TARGET_OPTIONS.map((option) => option.value)).toEqual(['new', 'temporary', 'session']);
    expect(NAME_MAX).toBe(schema.$defs.name.maxLength);
    expect(TRIGGERS_MAX).toBe(schema['x-vis-limits'].triggers);
  });
});
