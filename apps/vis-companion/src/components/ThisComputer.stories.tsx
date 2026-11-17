import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';

import { ThisComputer } from './ThisComputer';

/**
 * Vis on this computer, as the desktop app shows it to a person who has only the app.
 * Each story is one state that the desktop host reports.
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
      <div className="max-w-3xl border border-dialog-edge bg-panel px-4 py-3">
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof ThisComputer>;

export default meta;
type Story = StoryObj<typeof meta>;

const INSTALL_LOG = [
  'vis-agent install: installed ~/.local/bin/vis-agent',
  'vis-agent: downloading vis-agent-macos-arm64.tar.gz (v0.5.21)',
];

/** The first open of the app on a computer without Vis. */
export const NotInstalled: Story = {
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Install' }));
    await expect(args.onInstall).toHaveBeenCalledOnce();
  },
};

/** A task in progress shows its newest log line. */
export const Installing: Story = {
  args: { engine: { kind: 'working', task: 'install', log: INSTALL_LOG } },
};

/** A failed task keeps its newest log lines, and the same verb starts it again. */
export const InstallFailed: Story = {
  args: {
    engine: {
      kind: 'failed',
      task: 'install',
      reason: 'The installation stopped with an error.',
      log: [...INSTALL_LOG, 'vis-agent: could not download vis-agent-macos-arm64.tar.gz (HTTP 503)'],
    },
  },
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Try again' }));
    await expect(args.onInstall).toHaveBeenCalledOnce();
  },
};

export const Running: Story = {
  args: { engine: { kind: 'running', version: '0.5.20', track: 'release' } },
};

/** A check found a newer release: the update is the one primary verb. */
export const UpdateAvailable: Story = {
  args: { engine: { kind: 'running', version: '0.5.20', track: 'release', latest: '0.5.21' } },
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Update' }));
    await expect(args.onUpdate).toHaveBeenCalledOnce();
  },
};

/** A dev build at the newest commit of main: there is no update to offer. */
export const NewestDevBuild: Story = {
  args: {
    engine: { kind: 'running', version: '0.5.21', track: 'dev', build: 'c0ffee', latest: 'c0ffee' },
  },
  play: async ({ canvas }) => {
    await expect(canvas.queryByRole('button', { name: 'Update' })).toBeNull();
    await expect(canvas.getByRole('button', { name: 'Check for updates' })).toBeVisible();
  },
};

/** The gateway runs, and the app is not paired with it. */
export const NotConnected: Story = {
  args: {
    engine: { kind: 'running', version: '0.5.21', track: 'release' },
    isUpdateShown: false,
    onConnect: fn(),
  },
};

export const GatewayStopped: Story = {
  args: { engine: { kind: 'stopped', version: '0.5.21', track: 'release', latest: '0.5.21' } },
};
