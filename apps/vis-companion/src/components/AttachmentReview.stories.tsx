import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn } from 'storybook/test';
import type { ComponentProps } from 'react';
import { MarkdownArtifact } from './MarkdownArtifact';
import { OverlayScreen } from './DocArtifact';

/**
 * A PLAN- file from a session that used the retired "Plan before coding" setting.
 * It opens as an ordinary Markdown attachment.
 */
const oldPlan =
  '# Session search\n\n**Feature:** session-search\n**Status:** ready\n\n## Spec\nFind a session by title without leaving the current conversation.\n\n## Implementation plan\n1. Search titles end to end.\n2. Restore the selected session and its scroll position.\n';
const client = {
  base: 'http://127.0.0.1',
  saveArtifactText: fn(async () => ({ version: 4 })),
  submitTurn: fn(async () => {}),
} as unknown as ComponentProps<typeof MarkdownArtifact>['client'];
const meta = {
  title: 'Components/Attachment review',
  component: MarkdownArtifact,
  parameters: { layout: 'fullscreen' },
  args: {
    client,
    sid: 'review-story',
    iterationId: 'i1',
    name: 'PLAN-session-search.md',
    mediaType: 'text/markdown',
    source: new Blob([oldPlan]),
    version: 3,
    chrome: ({ actions, note, body }) => (
      <OverlayScreen title="Session search" subtitle={note} actions={actions} onClose={fn()}>
        {body}
      </OverlayScreen>
    ),
  },
} satisfies Meta<typeof MarkdownArtifact>;
export default meta;
type Story = StoryObj<typeof meta>;
export const Commentable: Story = {
  args: { commentable: true },
  play: async ({ canvas }) => {
    await expect(await canvas.findByRole('button', { name: 'Save changes' })).toBeInTheDocument();
    await expect(
      canvas.queryByRole('button', { name: 'Approve and start' }),
    ).not.toBeInTheDocument();
  },
};
export const DefaultReadOnly: Story = {
  play: async ({ canvas }) => {
    await canvas.findByText('Find a session by title without leaving the current conversation.');
    await expect(
      canvas.queryByRole('button', { name: 'Comment on the whole document' }),
    ).not.toBeInTheDocument();
  },
};
