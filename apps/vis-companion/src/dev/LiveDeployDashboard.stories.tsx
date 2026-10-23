import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent } from 'storybook/test';

import { LiveViewPanel } from '../components/LiveView';
import { DialogFrame } from '../components/ui';
import type { LiveView } from '../lib/live-view';
import { DEPLOY_AS_REPORTED, DEPLOY_FAILED, DEPLOY_RECOMPOSED } from './live-deploy-data';

/**
 * THE DEPLOY DASHBOARD RECIPE (#417), drawn by the shipped renderer.
 *
 * A producer builds a deploy or CI dashboard from native nodes: one status, a
 * progress, a stat of facts, steps, a table, links and a log. The renderer then
 * gives each node its own face. The faces follow a Hallmark audit
 * (Nutlope/hallmark @7ec868225c6caa040558c321ee0fefa0e73f83a8, `audit` verb at
 * `polish` depth): the palette, the typeface and the square corners stay.
 */
const meta = {
  title: 'Live View/Deploy dashboard',
  parameters: { layout: 'fullscreen' },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

/** A run as the session dialog shows it: the dialog band is its one title. */
function Dashboard({ view, isSettled = false }: { view: LiveView; isSettled?: boolean }) {
  return (
    <div className="flex h-dvh flex-col">
      <DialogFrame title={view.title} subtitle={view.description}>
        <div className="min-h-0 flex-1 overflow-y-auto px-3 py-3">
          <LiveViewPanel view={view} isSettled={isSettled} inDialog />
        </div>
      </DialogFrame>
    </div>
  );
}

/** The vertical centre of a box, in page pixels. */
function middle(rect: DOMRect): number {
  return rect.top + rect.height / 2;
}

/** The box of the first rendered line of an element's words. */
function firstLine(element: Element): DOMRect {
  const range = element.ownerDocument.createRange();
  range.selectNodeContents(element);
  return range.getClientRects()[0]!;
}

/** Geometry exists only in a browser that lays the page out, not in a DOM emulation. */
function hasLayout(element: Element): boolean {
  return typeof Range.prototype.getClientRects === 'function' && element.getBoundingClientRect().height > 0;
}

/** The recipe at desk width. */
export const DeployDashboard: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  render: () => <Dashboard view={DEPLOY_RECOMPOSED} />,
};

/**
 * On a phone nothing scrolls sideways, and every mark stands on the first line of
 * the words it marks. A step mark once sat below its label and the rail gaps
 * differed by whether a step had a detail.
 */
export const DeployDashboardPhone: Story = {
  globals: { viewport: { value: 'phoneSmall', isRotated: false } },
  render: () => <Dashboard view={DEPLOY_RECOMPOSED} />,
  play: async ({ canvas, canvasElement }) => {
    const document = canvasElement.ownerDocument;
    // One title: the dialog band names the run, and the panel does not repeat it.
    await expect(canvas.getAllByText(DEPLOY_RECOMPOSED.title)).toHaveLength(1);

    const steps = [...document.querySelectorAll('ol > li')].filter((li) => li.querySelector('svg'));
    await expect(steps.length).toBeGreaterThan(2);
    const gaps: number[] = [];
    for (const [index, step] of hasLayout(steps[0]!) ? steps.entries() : []) {
      const mark = step.querySelector('svg')!.getBoundingClientRect();
      const label = firstLine(step.querySelector('span.block')!);
      await expect(Math.abs(middle(mark) - middle(label))).toBeLessThanOrEqual(1);
      const next = steps[index + 1]?.querySelector('svg')?.getBoundingClientRect();
      const rail = step.querySelector('[aria-hidden="true"].w-px')?.getBoundingClientRect();
      if (next && rail) gaps.push(rail.top - mark.bottom, next.top - rail.bottom);
    }
    // Every gap on the rail is the same, above and below each mark.
    if (gaps.length > 0) await expect(Math.max(...gaps) - Math.min(...gaps)).toBeLessThanOrEqual(1);

    // A long identifier keeps its copy action on its first line.
    const job = canvas.getByText('platform/services/deploy-all-dev');
    const copy = job.closest('dd')!.querySelector('button')!;
    await expect(copy).toHaveAccessibleName('Copy Job');
    if (hasLayout(job)) {
      await expect(
        Math.abs(middle(copy.getBoundingClientRect()) - middle(firstLine(job))),
      ).toBeLessThanOrEqual(1);
    }

    await userEvent.click(canvas.getByRole('button', { name: 'Output' }));
    await expect(canvas.getByRole('searchbox', { name: 'Search Output' })).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Pause' })).toBeVisible();
    const page = document.documentElement;
    await expect(page.scrollWidth).toBeLessThanOrEqual(page.clientWidth);
  },
};

/** A failed deploy: the folded log still shows its error lines (#417 item 4). */
export const DeployFailedPhone: Story = {
  globals: { viewport: { value: 'phoneSmall', isRotated: false } },
  render: () => <Dashboard view={DEPLOY_FAILED} isSettled />,
  play: async ({ canvas }) => {
    await expect(canvas.getByLabelText('Latest errors in Output')).toBeVisible();
    await expect(canvas.getAllByText('Failed').length).toBeGreaterThan(0);
  },
};

/** The issue's reproduction, as the producer reported it, for comparison with the recipe. */
export const DeployAsReported: Story = {
  globals: { viewport: { value: 'phoneSmall', isRotated: false } },
  render: () => <Dashboard view={DEPLOY_AS_REPORTED} />,
};
