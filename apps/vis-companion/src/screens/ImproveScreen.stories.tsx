import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent } from 'storybook/test';
import { useState } from 'react';
import { ImproveWorkspace } from './ImproveScreen';
import { DialogFrame } from '../components/ui';
import { storyImproveClient, STORY_IMPROVE_PROJECT } from '../dev/story-data';
import type { ImproveMode } from '../lib/improve';

function Workspace({
  mode = 'human',
  empty = false,
  failed = false,
  loading = false,
  projectRoot,
}: {
  mode?: ImproveMode;
  empty?: boolean;
  failed?: boolean;
  loading?: boolean;
  projectRoot?: string;
}) {
  const [client] = useState(() => {
    const fixture = storyImproveClient(mode, empty);
    if (projectRoot !== undefined) {
      const projects = fixture.improveProjects;
      fixture.improveProjects = async () => {
        const overview = await projects();
        return {
          ...overview,
          projects: overview.projects.map((project) => ({ ...project, root: projectRoot })),
        };
      };
    }
    if (failed)
      fixture.improveRecords = async () => {
        throw new Error('Machine unavailable. Retry when it reconnects.');
      };
    if (loading) fixture.improveRecords = () => new Promise(() => {});
    return fixture;
  });
  return (
    <div className="flex h-dvh flex-col bg-ink sm:p-4">
      <DialogFrame title="Improve" subtitle="Vis · Project issues and improvement groups">
        <ImproveWorkspace client={client} />
      </DialogFrame>
    </div>
  );
}

const meta = {
  title: 'Screens/Improve',
  component: Workspace,
  parameters: { layout: 'fullscreen' },
} satisfies Meta<typeof Workspace>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Human: Story = {
  play: async ({ canvas }) => {
    await canvas.findByRole('option', { name: '/workspace/vis' });
    await userEvent.selectOptions(canvas.getByLabelText('Improve project'), STORY_IMPROVE_PROJECT);
    await expect(await canvas.findByText('Make collected reports actionable')).toBeVisible();
  },
};

export const HomePaths: Story = {
  args: { projectRoot: '/Users/ana/code/vis' },
  play: async ({ canvas }) => {
    const option = await canvas.findByRole('option', { name: '~/code/vis' });
    await expect(option).toHaveValue(STORY_IMPROVE_PROJECT);
    await userEvent.selectOptions(canvas.getByLabelText('Improve project'), option);
    await expect(await canvas.findByText('Make collected reports actionable')).toBeVisible();
  },
};

export const Details: Story = {
  play: async (context) => {
    await Human.play!(context);
    await userEvent.click(context.canvas.getByText('A failed tool call needs reproduction notes'));
    await userEvent.click(
      context.canvas.getByRole('button', { name: 'Original report · read only' }),
    );
    await expect(
      context.canvas.getByText('A tool call failed. Reproduction has not been attempted.'),
    ).toBeVisible();
  },
};
export const EditAndGroup: Story = {
  play: async (context) => {
    await Human.play!(context);
    const { canvas } = context;
    await userEvent.click(canvas.getByText('Let people review reports without selecting a model'));
    await userEvent.click(canvas.getByRole('button', { name: 'Edit issue' }));
    await userEvent.selectOptions(canvas.getByLabelText('Improvement group'), '1');
    await userEvent.clear(canvas.getByLabelText('Issue content'));
    await userEvent.type(
      canvas.getByLabelText('Issue content'),
      '## Review\nA human grouped this report.',
    );
    await userEvent.click(canvas.getByRole('button', { name: 'Save issue' }));
    await expect(await canvas.findByText('Issue saved.')).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Open group #1' })).toBeVisible();
  },
};
export const Automatic: Story = {
  args: { mode: 'automatic' },
  play: async ({ canvas }) => {
    await canvas.findByRole('option', { name: '/workspace/vis' });
    await userEvent.click(canvas.getByRole('button', { name: 'Review settings' }));
    await expect(await canvas.findByLabelText('Improve model')).toBeVisible();
    await expect(canvas.getByLabelText('Review interval in minutes')).toHaveValue(60);
  },
};
export const Empty: Story = { args: { empty: true } };
export const Off: Story = { args: { mode: 'off' } };
export const Failed: Story = { args: { failed: true } };
export const Loading: Story = { args: { loading: true } };
