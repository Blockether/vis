import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { MachineMark, MachineProjectsButton } from '../../components/SessionNavigator';
import { STORY_MACHINES, STORY_PROJECTS } from '../../dev/story-data';
import { FleetRail, type FleetRailEntry } from './FleetRail';

const chooseMachine = fn();
const chooseProject = fn();
const manageProjects = fn();

const machines: FleetRailEntry[] = STORY_MACHINES.map((machine, index) => ({
  key: machine.name,
  name: machine.name,
  count: machine.live + machine.unread,
  mark: <MachineMark color={machine.color} isHollow={machine.isDown} />,
  isActive: index === 0,
  onPress: chooseMachine,
}));
const projects: FleetRailEntry[] = STORY_PROJECTS.map((project) => ({
  key: project.root,
  name: project.name,
  count: project.count,
  onPress: chooseProject,
}));

const meta = {
  title: 'Sessions/Fleet rail',
  component: FleetRail,
  parameters: { layout: 'fullscreen' },
  args: {
    machines,
    projects,
    action: (
      <MachineProjectsButton
        machine="tower"
        onPress={() => manageProjects()}
      />
    ),
  },
  decorators: [
    (Story) => (
      <div className="flex h-[640px] bg-page p-4">
        <Story />
      </div>
    ),
  ],
} satisfies Meta<typeof FleetRail>;

export default meta;
type Story = StoryObj<typeof meta>;

export const PairedFleet: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'vis — 61 sessions' }));
    await expect(chooseProject).toHaveBeenCalled();
  },
};
