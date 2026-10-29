import type { Meta, StoryObj } from '@storybook/react-vite';
import { fn } from 'storybook/test';
import type { PendingAttachment } from '../lib/attachments';
import { ComposerPayloadShelf } from './ComposerPayloadShelf';
import { UserMessage } from './ChatContent';

/** Deterministic exports; no gateway, native share sheet or private diagnostics. */
const logs: PendingAttachment[] = [
  {
    id: 'gzip-log',
    filename: 'vis-diagnostics.jsonl.gz',
    media_type: 'application/gzip',
    base64:
      'H4sIAAAAAAAC/w3KMQ6AIAxG4Z1j/LMMrtyGQNVGLMQ2LoS72/V7b+JmqUj4WGMeI1bOp3Q1LooNWi56MtK+wgR9JOarWn6Nqufm1FxYjo4VfvjpmRxNAAAA',
    previewUrl: '',
    size: 90,
  },
  {
    id: 'plain-log',
    filename: 'vis-diagnostics.jsonl',
    media_type: 'application/x-ndjson',
    base64:
      'eyJraW5kIjoidmlzLWFwcC1kaWFnbm9zdGljcyIsInNjaGVtYSI6MX0KeyJldmVudCI6InN0YXJ0ZWQiLCJsZXZlbCI6ImluZm8ifQo=',
    previewUrl: '',
    size: 77,
  },
];
const commands = {
  editPaste: fn(),
  removePaste: fn(),
  editAttachment: fn(),
  removeAttachment: fn(),
};
const meta = {
  title: 'Session/Transcript/Request',
  component: UserMessage,
  parameters: { layout: 'padded' },
  args: { children: 'Please check these app logs.', attachments: logs },
} satisfies Meta<typeof UserMessage>;
export default meta;
type Story = StoryObj<typeof meta>;

export const WithAttachments: Story = {
  render: (args) => (
    <div className="mx-auto max-w-xl space-y-6">
      <section aria-label="Staged logs">
        <h2 className="mb-2 font-mono text-subhead text-dialog-foreground">Before sending</h2>
        <ComposerPayloadShelf pastes={[]} attachments={logs} commands={commands} />
      </section>
      <section aria-label="Sent logs">
        <h2 className="font-mono text-subhead text-dialog-foreground">Transcript</h2>
        <UserMessage {...args} />
      </section>
    </div>
  ),
};
