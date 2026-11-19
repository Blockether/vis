// @vitest-environment jsdom
// Issue #483: in an HTML export, a subagent row looked like a control, and a click on it did
// nothing, because the export held only the conversation of the leader. An export now holds the
// conversation of each subagent below the leader, and a row opens it without a gateway.
import { fireEvent, render, screen, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it } from 'vitest';

import exportFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/transcript-export.json';
import { GatewayClient } from '../lib/gateway';
import { parseTranscriptExport, SNAPSHOT_GATEWAY } from '../lib/transcript-export';
import { TranscriptExport } from './TranscriptExport';

// Contract fixture `transcript-export.json`: a leader, its subagent `child` and the subagent
// `grand` of that subagent. The engine test compares its keys with the keys of a real page.
const EXPORT = exportFixture.export;

function renderExport(document: object) {
  const data = parseTranscriptExport(JSON.stringify(document));
  render(<TranscriptExport data={data} client={new GatewayClient({ url: SNAPSHOT_GATEWAY })} />);
}

// A team of the transcript starts folded to its line, so the row opens it first.
const agentRow = (sid: string) => {
  for (const fold of screen.queryAllByRole('button', { name: /^team/, expanded: false })) fireEvent.click(fold);
  return screen.getAllByRole('button').find((row) => row.getAttribute('data-agent-row') === sid)!;
};

const path = () => screen.getByRole('navigation', { name: 'Subagent path' });

describe('the subagent rows of an HTML export', () => {
  it('open the conversation of a subagent and of its own subagent, and lead back to the leader', async () => {
    const user = userEvent.setup();
    renderExport(EXPORT);
    expect(await screen.findByText('The parser ships.')).toBeInTheDocument();
    expect(screen.queryByRole('navigation', { name: 'Subagent path' })).not.toBeInTheDocument();

    await user.click(agentRow('child'));
    expect(await screen.findByText('The parser is fixed.')).toBeInTheDocument();
    expect(screen.queryByText('The parser ships.')).not.toBeInTheDocument();

    await user.click(agentRow('grand'));
    expect(await screen.findByText('The logs are clean.')).toBeInTheDocument();
    expect(screen.queryByText('The parser is fixed.')).not.toBeInTheDocument();

    await user.click(within(path()).getByRole('button', { name: 'Back to the parent session' }));
    expect(await screen.findByText('The parser is fixed.')).toBeInTheDocument();

    await user.click(agentRow('grand'));
    expect(await screen.findByText('The logs are clean.')).toBeInTheDocument();
    await user.click(within(path()).getByRole('button', { name: /leader/i }));
    expect(await screen.findByText('The parser ships.')).toBeInTheDocument();
    expect(screen.queryByRole('navigation', { name: 'Subagent path' })).not.toBeInTheDocument();
  });

  // An export from before this change holds only the leader. Its rows are the same controls.
  it('say that an older export does not hold the conversation, with the way back', async () => {
    const user = userEvent.setup();
    renderExport({ ...EXPORT, descendants: undefined });
    expect(await screen.findByText('The parser ships.')).toBeInTheDocument();

    await user.click(agentRow('child'));
    expect(await screen.findByText(/does not hold the conversation of this subagent/)).toBeInTheDocument();
    expect(screen.queryByText('The parser ships.')).not.toBeInTheDocument();

    await user.click(within(path()).getByRole('button', { name: 'Back to the parent session' }));
    expect(await screen.findByText('The parser ships.')).toBeInTheDocument();
  });
});
