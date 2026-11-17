import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { ThisComputer } from './ThisComputer';

/**
 * Vis on this computer, as the desktop app shows it to a person who has only the app.
 * Each story is one state that the desktop host reports, inside the plane that the
 * pairing page and Settings give it.
 */
const meta = {
  title: 'Settings/Machines/This computer',
  component: ThisComputer,
  parameters: { layout: 'padded' },
  args: {
    engine: { kind: 'absent' },
    onInstall: fn(),
    onUpdate: fn(),
    onStart: fn(),
    onStop: fn(),
    onCheck: fn(),
  },
  decorators: [
    (Story) => (
      <div className="max-w-3xl border border-dialog-edge bg-panel p-5">
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof ThisComputer>;

export default meta;
type Story = StoryObj<typeof meta>;

const INSTALL_LOG = [
  'vis-agent install: verified the vis-agent command against SHA256SUMS',
  'vis-agent install: installed ~/.local/bin/vis-agent',
  'vis-agent: downloading vis-agent-macos-arm64.tar.gz (v0.5.21)',
];

/** The first open of the app on a computer without Vis: the offer, and one verb. */
export const NotInstalled: Story = {
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Install Vis' }));
    await expect(args.onInstall).toHaveBeenCalledOnce();
  },
};

/** The installation in progress: the step, the steps to come and the newest log lines. */
export const Installing: Story = {
  args: { engine: { kind: 'working', task: 'install', step: 1, log: INSTALL_LOG } },
};

/** A failed installation keeps its report, and gives the same step for a terminal. */
export const InstallFailed: Story = {
  args: {
    engine: {
      kind: 'failed',
      task: 'install',
      step: 1,
      reason: 'The download of vis-agent-macos-arm64.tar.gz stopped (HTTP 503).',
      log: [
        ...INSTALL_LOG,
        'vis-agent: could not download vis-agent-macos-arm64.tar.gz (HTTP 503); nothing was installed',
      ],
    },
  },
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Try again' }));
    await expect(args.onInstall).toHaveBeenCalledOnce();
  },
};

/** Vis is installed and its gateway runs: the version, the address and the update check. */
export const Running: Story = {
  args: { engine: { kind: 'running', version: '0.5.20', address: '127.0.0.1:7890' } },
};

/** A check found a newer release: the update is the one primary verb. */
export const UpdateAvailable: Story = {
  args: {
    engine: { kind: 'running', version: '0.5.20', address: '127.0.0.1:7890', latest: '0.5.21' },
  },
};

/** The update in progress. */
export const Updating: Story = {
  args: {
    engine: {
      kind: 'working',
      task: 'update',
      step: 0,
      log: ['vis-agent: downloading vis-agent-macos-arm64.tar.gz (v0.5.21)'],
    },
  },
};

/** Vis is installed, and its gateway does not run. */
export const GatewayStopped: Story = {
  args: { engine: { kind: 'stopped', version: '0.5.21', latest: '0.5.21' } },
};
