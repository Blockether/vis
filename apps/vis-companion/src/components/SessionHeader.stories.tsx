import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';

import { STORY_GOAL, STORY_SESSION } from '../dev/story-data';
import { SessionHeader } from './SessionHeader';

const meta = {
  title: 'Session/Header',
  component: SessionHeader,
  parameters: { layout: 'fullscreen' },
  args: {
    model: {
      title: STORY_SESSION.title,
      sessionId: STORY_SESSION.id,
      connected: true,
      artifacts: { count: 4, isOpen: false },
    },
    commands: { back: () => {}, toggleArtifacts: () => {} },
  },
} satisfies Meta<typeof SessionHeader>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Connected: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvasElement }) => {
    // Regression: the transcript border must align with the 52px machine strip.
    const canvas = within(canvasElement);
    const header = canvas.getByRole('banner');
    const actions = canvas.getByRole('button', { name: 'Session actions, 4 artifacts' });
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    await expect(actions.getBoundingClientRect().height).toBe(pointer ? 28 : 32);
    await expect(header.getBoundingClientRect().height).toBe(53);

    // The trailing rail carries ONE control, and everything the band used to spell
    // out stands one press behind it.
    await userEvent.click(actions);
    const page = within(document.body);
    await expect(page.getByRole('dialog', { name: 'Session actions' })).toBeVisible();
    await expect(page.getByRole('button', { name: 'Open artifacts (4)' })).toBeVisible();
    await expect(page.getByRole('button', { name: /^Copy session id/ })).toBeVisible();
    await userEvent.keyboard('{Escape}');
    await expect(page.queryByRole('dialog', { name: 'Session actions' })).toBeNull();
  },
};

export const ConnectedTouch: Story = {
  ...Connected,
  globals: { viewport: { value: 'phone', isRotated: false } },
};

export const Reconnecting: Story = {
  args: {
    model: {
      title: STORY_SESSION.title,
      sessionId: STORY_SESSION.id,
      connected: false,
      artifacts: { count: 4, isOpen: false },
    },
  },
};

export const ActiveGoal: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  args: { model: { ...meta.args.model, goal: STORY_GOAL } },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    // A goal must not push the transcript border below the machine strip.
    expect(canvas.getByRole('banner').getBoundingClientRect().height).toBe(53);
    const button = canvas.getByRole('button', { name: /^Goal: Active/ });
    expect(button).toHaveTextContent(/^Goal: Active - /);
    const label = button.querySelector('span')!;
    expect(label.getBoundingClientRect().right).toBeLessThanOrEqual(
      button.getBoundingClientRect().right,
    );
    await userEvent.click(button);
    const page = within(document.body);
    expect(page.getByRole('dialog', { name: 'Session goal' })).toBeVisible();
    expect(page.getByText('Iterations: 12 / 30')).toBeVisible();
    expect(page.getByText(/^Time in goal:/)).toBeVisible();
    expect(page.queryByText(/tokens used/)).toBeNull();
    await userEvent.click(page.getByRole('button', { name: 'Close session goal' }));
  },
};
export const CompletedGoal: Story = {
  args: {
    model: {
      ...meta.args.model,
      goal: {
        ...STORY_GOAL,
        status: 'complete',
        reason: 'SDK tests and header interaction checks passed.',
      },
    },
  },
};
export const BlockedGoal: Story = {
  args: {
    model: {
      ...meta.args.model,
      goal: {
        ...STORY_GOAL,
        status: 'blocked',
        reason: 'The requested test device is unavailable.',
      },
    },
  },
};
export const BudgetLimitedGoal: Story = {
  args: {
    model: {
      ...meta.args.model,
      goal: { ...STORY_GOAL, status: 'budget_limited', iterations_used: 30 },
    },
  },
};
export const LongGoal: Story = {
  args: {
    model: {
      ...meta.args.model,
      goal: {
        ...STORY_GOAL,
        objective:
          'Preserve the complete objective, including the SDK boundary, session isolation, cancellation, reconnects, accessibility, phone layouts and verification against current state.',
      },
    },
  },
};
