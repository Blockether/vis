// @vitest-environment jsdom
import { screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it } from 'vitest';

import { renderSessionScreen } from './session-screen-harness';
import { DIFF_MEDIA } from '../lib/artifacts';
import diffFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/diff.json';

const ATTACHMENT_ID = '8e3a587d-232c-497d-a290-7d16cfcf0e02';
const DIFF_ID = '7f477ec2-2c01-49bc-88eb-d0d7186179cf';
const row = {
  turn_id: 'turn-1',
  request: 'Show me the preview',
  status: 'completed',
  created_at: Date.now(),
  content: [
    {
      id: 'answer',
      type: 'prose',
      markdown: `[Zobacz podgląd mobilny](attachment://${ATTACHMENT_ID})`,
    },
  ],
  iterations: [
    {
      id: 'iteration-1',
      attachments: [
        {
          index: 0,
          iteration_id: 'iteration-1',
          attachment_id: ATTACHMENT_ID,
          kind: 'image',
          filename: 'mobile-preview.png',
          media_type: 'image/png',
          size: 1024,
        },
      ],
    },
  ],
};

// User report: tapping a preview link in an answer invoked an unsupported browser
// scheme, so the artifacts surface and the requested preview both stayed closed.
describe('an attachment link in an answer', () => {
  it("opens that artifact directly in the session's artifacts surface", async () => {
    renderSessionScreen({
      client: {
        cachedTranscript: () => [row],
        transcript: () => Promise.resolve([row]),
        sessionArtifacts: () => Promise.resolve([]),
      },
    });

    await userEvent.click(await screen.findByRole('button', { name: 'Zobacz podgląd mobilny' }));

    expect(
      screen.getByRole('region', { name: 'Artifacts produced by the model' }),
    ).toBeInTheDocument();
    expect(screen.getByRole('dialog', { name: 'mobile-preview.png' })).toBeInTheDocument();
  });
  // #193: a report generated in the TUI arrives as a durable session attachment.
  it('opens a linked Markdown report using the attachment reader', async () => {
    const reportRow = {
      ...row,
      content: [
        {
          id: 'answer',
          type: 'prose',
          markdown: `[Report](attachment://${ATTACHMENT_ID})`,
        },
      ],
      iterations: [
        {
          ...row.iterations[0],
          attachments: [
            {
              ...row.iterations[0].attachments[0],
              kind: 'file',
              filename: 'report.md',
              media_type: 'text/markdown',
            },
          ],
        },
      ],
    };
    renderSessionScreen({
      client: {
        cachedTranscript: () => [reportRow],
        transcript: () => Promise.resolve([reportRow]),
        sessionArtifacts: () => Promise.resolve([]),
        attachmentUrl: async () => 'blob:report',
        attachmentBlob: async () =>
          new Blob(['# Durable report\n\nAvailable after the host file moves.'], {
            type: 'text/markdown',
          }),
      },
    });
    await userEvent.click(await screen.findByRole('button', { name: 'Report' }));
    await waitFor(() => {
      const preview = screen.getByRole('dialog', { name: 'report.md' });
      expect(within(preview).getByText('Available after the host file moves.')).toBeInTheDocument();
    });
  });
  it('opens a diff named inside a report without changing the application route', async () => {
    const reportRow = {
      ...row,
      content: [{ id: 'answer', type: 'prose', markdown: `[Report](attachment://${ATTACHMENT_ID})` }],
      iterations: [
        {
          ...row.iterations[0],
          attachments: [
            {
              ...row.iterations[0].attachments[0],
              kind: 'file',
              filename: 'IMPLEMENTATION-ungroup-hover-hint.md',
              media_type: 'text/markdown',
            },
            {
              index: 1,
              iteration_id: 'iteration-1',
              attachment_id: DIFF_ID,
              kind: 'file',
              filename: 'DIFF-ungroup-hover-hint.json',
              media_type: DIFF_MEDIA,
              size: 1024,
            },
          ],
        },
      ],
    };
    renderSessionScreen({
      client: {
        cachedTranscript: () => [reportRow],
        transcript: () => Promise.resolve([reportRow]),
        sessionArtifacts: () => Promise.resolve([]),
        attachmentUrl: async () => 'blob:artifact',
        attachmentBlob: async (_sid: string, _iteration: string, index: number) =>
          new Blob([
            index === 0
              ? '# Report\n\n[Review diff](DIFF-ungroup-hover-hint.json)'
              : JSON.stringify(diffFixture),
          ]),
      },
    });

    await userEvent.click(await screen.findByRole('button', { name: 'Report' }));
    const report = await screen.findByRole('dialog', {
      name: 'IMPLEMENTATION-ungroup-hover-hint.md',
    });
    await userEvent.click(await within(report).findByRole('button', { name: 'Review diff' }));

    expect(
      screen.getByRole('region', { name: 'Artifacts produced by the model' }),
    ).toBeInTheDocument();
    await waitFor(() => {
      expect(
        screen.queryByRole('dialog', { name: 'IMPLEMENTATION-ungroup-hover-hint.md' }),
      ).toBeNull();
      expect(
        screen.getByRole('dialog', { name: 'DIFF-ungroup-hover-hint.json' }),
      ).toBeInTheDocument();
    });
  });
});
