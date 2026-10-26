import { describe, expect, it } from 'vitest';
import agentsSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/agents.json';
import { STORY_DONE_TEAM, STORY_LIVE_TEAM, storyTimeline } from './story-agents';

// The story rows stand in for gateway answers, so they keep the shape of the agents contract.
const agent = agentsSchema.$defs.agent;
const item = agentsSchema.$defs.timeline.properties.items.items;
const title = new RegExp(agentsSchema.$defs.spawn.properties.title.pattern);

describe('subagent story fixtures', () => {
  const rows = [...STORY_LIVE_TEAM, ...STORY_DONE_TEAM];

  it('keep the required keys, the status and outcome words and the title bounds of an agent row', () => {
    for (const row of rows) {
      for (const key of agent.required) expect(row).toHaveProperty(key);
      expect(agent.properties.status.enum).toContain(row.status);
      if (row.outcome) expect(agentsSchema.$defs.result.properties.outcome.enum).toContain(row.outcome);
      if (row.title !== undefined) {
        expect(row.title.length).toBeGreaterThanOrEqual(1);
        expect(row.title.length).toBeLessThanOrEqual(40);
        expect(row.title).toMatch(title);
      }
      // Only resumed_at may be null on the wire.
      expect(Object.entries(row).filter(([key, value]) => key !== 'resumed_at' && value === null)).toEqual([]);
    }
  });

  it('give timelines with only contract keys and kinds, oldest first', () => {
    for (const row of rows) {
      const timeline = storyTimeline(row);
      expect(Object.keys(timeline).sort()).toEqual(['items', 'session_id']);
      for (const step of timeline.items) {
        for (const key of Object.keys(step)) expect(Object.keys(item.properties)).toContain(key);
        expect(item.properties.kind.enum).toContain(step.kind);
      }
      const times = timeline.items.map((step) => step.at);
      expect(times).toEqual([...times].sort((a, b) => a - b));
    }
  });
});
