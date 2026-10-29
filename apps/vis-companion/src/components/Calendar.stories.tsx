import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';

import { Calendar, DateTimeField } from './Calendar';

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

const meta = {
  title: 'Foundations/Calendar',
  component: Calendar,
  parameters: { layout: 'centered' },
  args: { value: TODAY, today: TODAY, onValueChange: () => {} },
} satisfies Meta<typeof Calendar>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Default: Story = {
  render: () => <Field initial="2026-10-11T14:00" isFutureOnly />,
};
