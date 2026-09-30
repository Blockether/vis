import { fileURLToPath } from 'node:url';
import { createVitest } from 'vitest/node';
import { expect, it } from 'vitest';

// Negative controls run through the app's actual Storybook plugin and jsdom setup.
// Add them in memory so intentionally inaccessible stories never enter the catalog.
const storyFile = 'src/components/ui.stories.tsx';
const controls = `
export const AccessibilityGateAccessible: Story = { render: () => <button>Named control</button> };
export const AccessibilityGateUnnamed: Story = { render: () => <button /> };
export const AccessibilityGateInvalidAria: Story = { render: () => <button aria-expanded="invalid">Named control</button> };
export const AccessibilityGateTodo: Story = { parameters: { a11y: { test: 'todo' } }, render: () => <button /> };
export const AccessibilityGateOff: Story = { parameters: { a11y: { test: 'off' } }, render: () => <button /> };
export const AccessibilityGateDisabled: Story = { parameters: { a11y: { disable: true } }, render: () => <button /> };
export const AccessibilityGateScannerError: Story = {
  render: () => <button>Named control</button>,
  play: ({ reporting }) => { reporting.addReport({ type: 'a11y', version: 1, status: 'failed', result: { error: new Error('Scanner failed') } }); },
};
export const AccessibilityGateInteractionError: Story = {
  render: () => <button>Actual text</button>,
  play: async ({ canvas }) => { await expect(canvas.getByRole('button', { name: 'Actual text' })).toHaveTextContent('Different text'); },
};
export const AccessibilityGateExcluded: Story = { tags: ['!test'], render: () => <button /> };
`;

it('fails accessibility reports without failing warnings, disabled checks or accessible stories', async () => {
  // The parent waits while the probe uses one worker, not another shared lease.
  const argv = process.argv;
  process.argv = [...argv, '--maxWorkers=1'];
  let ctx;
  try {
    ctx = await createVitest('test', {
      root: fileURLToPath(new URL('..', import.meta.url)),
      watch: false,
      project: ['storybook'],
      maxWorkers: 1,
      testNamePattern: 'Accessibility Gate',
      reporters: [{ onTestRunEnd() {} }],
    });
    let injected = 0;
    const project = ctx.projects.find((project) => project.name === 'storybook');
    // Inject before TSX compilation, preserving the canonical plugin pipeline.
    const plugin = project.vite.config.plugins.find((plugin) => plugin.name === 'vite:oxc');
    const original = typeof plugin.transform === 'function' ? plugin.transform : plugin.transform.handler;
    const transform = async function (code, id, ...args) {
      const target = id.endsWith(`/${storyFile}`);
      if (target) { injected += 1; code += controls; }
      const result = await original.call(this, code, id, ...args);
      return target && result == null ? { code, map: null } : result;
    };
    plugin.transform = typeof plugin.transform === 'function' ? transform : { ...plugin.transform, handler: transform };
    await ctx.start([storyFile]);
    expect(injected).toBe(1);
    expect(ctx.state.getFiles().flatMap((file) => file.result?.errors ?? [])).toEqual([]);
    expect(ctx.state.getUnhandledErrors()).toEqual([]);
    const tasks = (task) => [task, ...(task.tasks ?? []).flatMap(tasks)];
    const tests = ctx.state.getFiles().flatMap(tasks).filter((task) => task.type === 'test' && task.name.startsWith('Accessibility Gate'));
    const byName = Object.fromEntries(tests.map((task) => [task.name, task]));
    expect(Object.fromEntries(tests.map((task) => [task.name, task.result?.state]))).toEqual({
      'Accessibility Gate Accessible': 'pass',
      'Accessibility Gate Unnamed': 'fail',
      'Accessibility Gate Invalid Aria': 'fail',
      'Accessibility Gate Todo': 'pass',
      'Accessibility Gate Off': 'pass',
      'Accessibility Gate Disabled': 'pass',
      'Accessibility Gate Scanner Error': 'fail',
      'Accessibility Gate Interaction Error': 'fail',
    });
    for (const [name, rule] of [['Accessibility Gate Unnamed', 'button-name'], ['Accessibility Gate Invalid Aria', 'aria-valid-attr-value']]) {
      const task = byName[name];
      expect(task.meta.reports).toEqual(expect.arrayContaining([
        expect.objectContaining({ type: 'a11y', status: 'failed', result: expect.objectContaining({
          violations: expect.arrayContaining([expect.objectContaining({ id: rule })]),
        }) }),
      ]));
      expect(task.result.errors.map((error) => error.message).join('\n')).toContain(rule);
    }
    expect(byName['Accessibility Gate Scanner Error'].result.errors.map((error) => error.message).join('\n')).toContain('Scanner failed');
    expect(byName['Accessibility Gate Todo'].meta.reports).toEqual(expect.arrayContaining([expect.objectContaining({ type: 'a11y', status: 'warning' })]));
    for (const name of ['Accessibility Gate Off', 'Accessibility Gate Disabled']) {
      expect(byName[name].meta.reports ?? []).not.toEqual(expect.arrayContaining([expect.objectContaining({ type: 'a11y' })]));
    }
  } finally {
    try { await ctx?.close(); } finally { process.argv = argv; }
  }
}, 30_000);
