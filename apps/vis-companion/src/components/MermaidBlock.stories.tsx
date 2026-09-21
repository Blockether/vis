import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, within } from 'storybook/test';
import { MermaidBlock } from './MermaidBlock';

/**
 * A MERMAID FENCE IS A PICTURE.
 *
 * The TUI ranks and draws the same flowchart with box-drawing glyphs; here the
 * source goes to mermaid and the SVG is painted in the transcript's own code
 * palette. These stories run the REAL library in a browser, so they are also
 * where the drawing is checked against what the terminal paints.
 *
 * A fence mermaid will not draw — another diagram type, a half-typed line
 * arriving mid-stream — falls back to the source, which is what the `fallback`
 * argument paints.
 */
const FLOWCHART = ['flowchart TD', '  A[Start] --> B{Ready?}', '  B -->|yes| C[Ship it]', '  B -->|no| D[Fix]', '  D --> B'].join('\n');

const PIPELINE = [
  'flowchart LR',
  '  A((Init)) ==> B[Build]',
  '  B -.->|cached| C([Artifacts])',
  '  B --> D{{Test}}',
  '  D --> E[Ship]',
].join('\n');

const fence = (source: string) => <pre className="m-0 px-3 py-2 font-mono text-ui">{source}</pre>;

const meta = {
  title: 'Components/Mermaid diagram',
  component: MermaidBlock,
  parameters: { layout: 'padded' },
  args: { compact: false },
} satisfies Meta<typeof MermaidBlock>;

export default meta;

type Story = StoryObj<typeof meta>;

/** A decision with a retry edge: the shape the TUI draws with ├ and ▼. */
export const Flowchart: Story = {
  args: { source: FLOWCHART, fallback: fence(FLOWCHART) },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const picture = await canvas.findByRole('img', { name: 'Diagram' }, { timeout: 15000 });
    await expect(picture.querySelectorAll('svg')).toHaveLength(1);
    // Every node in the source reaches the drawing, labels included.
    await expect(picture.textContent).toContain('Ship it');
    await expect(picture.textContent).toContain('yes');
  },
};

/** Left to right, with a dotted and a thick link. */
export const Pipeline: Story = {
  args: { source: PIPELINE, fallback: fence(PIPELINE) },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const picture = await canvas.findByRole('img', { name: 'Diagram' }, { timeout: 15000 });
    await expect(picture.textContent).toContain('Artifacts');
  },
};

/** Inside a step's frame: the same picture, tighter rhythm and no second frame. */
export const Frameless: Story = {
  args: { source: FLOWCHART, fallback: fence(FLOWCHART), compact: true, frameless: true },
};

/** A fence mermaid cannot draw keeps its source on screen. */
export const Unsupported: Story = {
  args: {
    source: 'flowchart TD\n  A[Start] -->',
    fallback: fence('flowchart TD\n  A[Start] -->'),
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(await canvas.findByText(/A\[Start\]/, undefined, { timeout: 15000 })).toBeVisible();
  },
};
