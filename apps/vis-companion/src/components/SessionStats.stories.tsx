import type { Meta, StoryObj } from '@storybook/react-vite';

import {
  STORY_SESSION_ROW,
  STORY_SESSION_USAGE,
  STORY_SESSION_HEALTH,
  STORY_HEALTH_USAGE,
} from '../dev/story-data';
import { SessionStatsPanel } from './SessionList';
import healthParity from '../../../../packages/vis-contract/resources/vis-contract/fixtures/session-health.json';
import type { SessionHealthData } from '../lib/types';

const meta = {
  title: 'Session/Context usage',
  component: SessionStatsPanel,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="h-dvh overflow-y-auto">
        <Story />
      </div>
    ),
  ],
  args: {
    session: STORY_SESSION_ROW,
    usage: STORY_SESSION_USAGE,
    phase: 'ready',
  },
} satisfies Meta<typeof SessionStatsPanel>;

export default meta;
type Story = StoryObj<typeof meta>;

export const SessionHealth: Story = {
  args: { usage: { ...STORY_HEALTH_USAGE, health: STORY_SESSION_HEALTH } },
};

export const OverBudget: Story = {
  args: {
    usage: {
      ...STORY_HEALTH_USAGE,
      health: healthParity.cases[10].health as SessionHealthData,
    },
  },
};
export const NoMeasurement: Story = { args: { usage: STORY_HEALTH_USAGE } };
