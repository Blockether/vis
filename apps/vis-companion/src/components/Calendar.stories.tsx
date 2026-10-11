import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import { useState } from 'react';

import { Calendar, DateTimeField, TimeField } from './Calendar';

const TODAY = '2026-10-11';

function Field({ initial, isFutureOnly = false }: { initial: string; isFutureOnly?: boolean }) {
  const [value, setValue] = useState(initial);
  return (
    <div className="w-80 space-y-2 font-mono text-ui text-white">
      <DateTimeField
        aria-label="Date and time"
        value={value}
        today={TODAY}
        isFutureOnly={isFutureOnly}
        onValueChange={setValue}
      />
      <p className="text-dialog-hint">Value: {value || 'none'}</p>
    </div>
  );
}

function Month({ initial, min }: { initial: string; min?: string }) {
  const [value, setValue] = useState(initial);
  return (
    <div className="w-[18.5rem] border border-dialog-edge bg-panel p-2">
      <Calendar value={value} today={TODAY} min={min} onValueChange={setValue} />
    </div>
  );
}

function Time() {
  const [time, setTime] = useState({ hour: 14, minute: 0 });
  return <TimeField hour={time.hour} minute={time.minute} onTimeChange={(hour, minute) => setTime({ hour, minute })} />;
}

const meta = {
  title: 'Vocabulary/Calendar',
  component: Calendar,
  parameters: { layout: 'centered' },
  args: { value: TODAY, today: TODAY, onValueChange: () => {} },
} satisfies Meta<typeof Calendar>;

export default meta;
type Story = StoryObj<typeof meta>;

/** A month with today chosen. */
export const ThisMonth: Story = { render: () => <Month initial={TODAY} /> };

/** Past days show but cannot be chosen; today wears the dot. */
export const FutureOnly: Story = { render: () => <Month initial="2026-10-14" min={TODAY} /> };

/** A month that does not hold today offers a way back to it. */
export const OtherMonth: Story = { render: () => <Month initial="2027-02-28" /> };

export const TimeSegments: Story = { render: () => <Time /> };

export const DateAndTime: Story = {
  render: () => <Field initial="2026-10-11T14:00" isFutureOnly />,
};

export const EmptyField: Story = { render: () => <Field initial="" /> };

export const OpenField: Story = {
  render: () => <Field initial="2026-10-11T14:00" isFutureOnly />,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Date and time' }));
    const page = within(canvasElement.ownerDocument.body);
    await expect(page.getByRole('dialog', { name: 'Date and time' })).toBeVisible();
  },
};
