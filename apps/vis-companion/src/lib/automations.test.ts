import { describe, expect, it } from 'vitest';
import schema from '../../../../packages/vis-contract/resources/vis-contract/schema/automations.json';
import {
  automationSummary,
  deliveryLabel,
  everyLabel,
  runReason,
  targetLabel,
  triggerLabel,
  wordLabel,
} from './automations';
import { timeLabel } from './fleet';
import { STORY_AUTOMATION_RUN, STORY_AUTOMATIONS } from '../dev/story-data';

const NOW = Date.UTC(2026, 5, 1, 12, 0, 0);
const HOUR = 3_600_000;

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
      'Automations are off for this target.',
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
