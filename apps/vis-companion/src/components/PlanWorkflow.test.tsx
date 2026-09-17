// @vitest-environment jsdom
import { act, type ReactElement } from 'react';
import { createRoot } from 'react-dom/client';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { GatewayClient } from '../lib/gateway';
import { actionRequest } from '../lib/plans';
import { MarkdownArtifact, MarkdownAnnotator, type DocumentChrome } from './MarkdownArtifact';

const text =
  '# Search\n\n**Feature:** search\n**Status:** ready\n\n## Spec\nFind sessions by title.';
const chrome: DocumentChrome = ({ actions, note, body }) => (
  <div>
    <header>
      {actions}
      {note}
    </header>
    {body}
  </div>
);
vi.mock('./TextArtifact', () => ({ readArtifactText: async () => text }));
const cleanups: (() => void)[] = [];
afterEach(() => cleanups.splice(0).forEach((cleanup) => cleanup()));

async function mount(node: ReactElement) {
  globalThis.IS_REACT_ACT_ENVIRONMENT = true;
  const host = document.createElement('div');
  document.body.append(host);
  const tree = createRoot(host);
  await act(async () => tree.render(node));
  cleanups.push(() => {
    act(() => tree.unmount());
    host.remove();
  });
  const button = (label: string) =>
    [...host.querySelectorAll('button')].find(
      (element) => element.getAttribute('aria-label') === label || element.textContent === label,
    );
  const click = async (label: string) => {
    const element = button(label);
    expect(element, label).toBeInTheDocument();
    await act(async () => element!.click());
  };
  const comment = async () => {
    await click('Comment on the whole document');
    const field = host.querySelector('textarea')!;
    await act(async () => {
      Object.getOwnPropertyDescriptor(HTMLTextAreaElement.prototype, 'value')!.set!.call(
        field,
        'Include archived sessions.',
      );
      field.dispatchEvent(new Event('input', { bubbles: true }));
    });
    await click('Add comment');
  };
  return { host, button, click, comment };
}

function planning(
  onSend = vi.fn<NonNullable<Parameters<typeof MarkdownAnnotator>[0]['planning']>['onSend']>(
    async () => {},
  ),
) {
  return { filename: 'PLAN-search.md', version: 3, onSend };
}

describe('specification review actions', () => {
  it('approves and starts the viewed version once from a single bottom action', async () => {
    const plan = planning();
    const onSave = vi.fn(async () => 4);
    const view = await mount(
      <MarkdownAnnotator text={text} chrome={chrome} onSave={onSave} planning={plan} />,
    );
    expect(view.button('Save changes')).toBeUndefined();
    expect(view.button('Send for revision')).toBeUndefined();
    const workflow = view.host.querySelector('[aria-label="Specification workflow"]')!;
    expect(workflow.querySelectorAll('button')).toHaveLength(1);
    const prose = view.host.querySelector('h1')!;
    expect(prose.compareDocumentPosition(workflow) & Node.DOCUMENT_POSITION_FOLLOWING).toBe(Node.DOCUMENT_POSITION_FOLLOWING);
    await view.click('Approve and start');
    await view.click('Approve and start');
    expect(plan.onSend).toHaveBeenCalledExactlyOnceWith('approve', 3);
    expect(onSave).not.toHaveBeenCalled();
    expect(view.host.textContent).toContain('Implementation requested for v3');
    expect(view.host.querySelector('[role="status"]')!.closest('[inert]')).toBeNull();
  });

  it('saves remarks before revising and sends the returned version', async () => {
    const plan = planning();
    const onSave = vi.fn(async () => 4);
    const view = await mount(
      <MarkdownAnnotator text={text} chrome={chrome} onSave={onSave} planning={plan} />,
    );
    await view.comment();
    expect(plan.onSend).not.toHaveBeenCalled();
    expect(onSave).not.toHaveBeenCalled();
    expect(view.button('Approve and start')).toBeUndefined();
    expect(view.button('Save changes')).toBeUndefined();
    expect(view.host.textContent).toContain('1 unresolved comment');
    await view.click('Send for revision');
    expect(onSave).toHaveBeenCalledExactlyOnceWith(expect.stringContaining('## Comments'));
    expect(plan.onSend).toHaveBeenCalledExactlyOnceWith('revise', 4);
  });

  it('never sends on save failure and keeps the unsaved remarks', async () => {
    const plan = planning();
    const view = await mount(
      <MarkdownAnnotator
        text={text}
        chrome={chrome}
        onSave={async () => {
          throw new Error('Storage unavailable');
        }}
        planning={plan}
      />,
    );
    await view.comment();
    await view.click('Send for revision');
    expect(plan.onSend).not.toHaveBeenCalled();
    expect(view.host.textContent).toContain('Storage unavailable');
    expect(view.button('Send for revision')!.disabled).toBe(false);
    expect(view.host.textContent).toContain('Include archived sessions.');
  });

  it('retries a failed send without saving the same revision again', async () => {
    const onSend = vi
      .fn<NonNullable<Parameters<typeof MarkdownAnnotator>[0]['planning']>['onSend']>()
      .mockRejectedValueOnce(new Error('Connection lost'))
      .mockResolvedValue(undefined);
    const onSave = vi.fn(async () => 4);
    const view = await mount(
      <MarkdownAnnotator text={text} chrome={chrome} onSave={onSave} planning={planning(onSend)} />,
    );
    await view.comment();
    await view.click('Send for revision');
    expect(view.host.textContent).toContain('Connection lost');
    await view.click('Send for revision');
    expect(onSave).toHaveBeenCalledTimes(1);
    expect(onSend.mock.calls).toEqual([
      ['revise', 4],
      ['revise', 4],
    ]);
  });

  it('uses the same approval-and-start action for an accepted specification', async () => {
    const plan = planning();
    const view = await mount(
      <MarkdownAnnotator
        text={text.replace('ready', 'accepted')}
        chrome={chrome}
        onSave={async () => 4}
        planning={plan}
      />,
    );
    expect(view.button('Start implementation')).toBeUndefined();
    await view.click('Approve and start');
    expect(plan.onSend).toHaveBeenCalledExactlyOnceWith('approve', 3);
  });

  it('has no workflow controls without the toggle or a valid plan header', async () => {
    const ordinary = await mount(
      <MarkdownAnnotator text={text} chrome={chrome} onSave={async () => 4} />,
    );
    expect(ordinary.button('Approve and start')).toBeUndefined();
    expect(ordinary.button('Save changes')).toBeVisible();
    const invalid = await mount(
      <MarkdownAnnotator
        text={text.replace('**Feature:** search', '**Feature:** other')}
        chrome={chrome}
        onSave={async () => 4}
        planning={planning()}
      />,
    );
    expect(invalid.button('Send for revision')).toBeUndefined();
    expect(invalid.button('Save changes')).toBeVisible();
  });

  it('keeps a draft informational until remarks are ready to send', async () => {
    const plan = planning();
    const view = await mount(
      <MarkdownAnnotator
        text={text.replace('ready', 'draft')}
        chrome={chrome}
        onSave={async () => 4}
        planning={plan}
      />,
    );
    expect(view.button('Approve and start')).toBeUndefined();
    expect(view.button('Send for revision')).toBeUndefined();
    expect(view.host.textContent).toContain('Draft');
    await view.comment();
    expect(plan.onSend).not.toHaveBeenCalled();
    expect(view.button('Send for revision')).toBeInTheDocument();
  });

  it('cannot approve while a comment is being composed', async () => {
    const plan = planning();
    const view = await mount(
      <MarkdownAnnotator text={text} chrome={chrome} onSave={async () => 4} planning={plan} />,
    );
    await view.click('Comment on the whole document');
    expect(view.button('Approve and start')).toBeUndefined();
    expect(view.button('Send for revision')?.disabled ?? true).toBe(true);
    expect(plan.onSend).not.toHaveBeenCalled();
    await view.click('Cancel');
    expect(view.button('Approve and start')!.disabled).toBe(false);
  });

  it('saves a removed final comment before requesting review, never silently approving edits', async () => {
    const plan = planning();
    const onSave = vi.fn(async () => 4);
    const view = await mount(
      <MarkdownAnnotator
        text={`${text}\n\n## Comments\n\n- **Whole document** — Clarify scope.\n`}
        chrome={chrome}
        onSave={onSave}
        planning={plan}
      />,
    );
    await view.click('Remove comment 1');
    expect(view.button('Approve and start')).toBeUndefined();
    await view.click('Send for revision');
    expect(onSave).toHaveBeenCalledExactlyOnceWith(expect.not.stringContaining('## Comments'));
    expect(plan.onSend).toHaveBeenCalledExactlyOnceWith('revise', 4);
  });

  it('retries revision after removing the last comment without changing to approval', async () => {
    const onSend = vi
      .fn<NonNullable<Parameters<typeof MarkdownAnnotator>[0]['planning']>['onSend']>()
      .mockRejectedValueOnce(new Error('Connection lost'))
      .mockResolvedValue(undefined);
    const onSave = vi.fn(async () => 4);
    const view = await mount(
      <MarkdownAnnotator
        text={`${text}\n\n## Comments\n\n- **Whole document** — Clarify scope.\n`}
        chrome={chrome}
        onSave={onSave}
        planning={planning(onSend)}
      />,
    );
    await view.click('Remove comment 1');
    await view.click('Send for revision');
    expect(view.host.textContent).toContain('Connection lost');
    expect(view.button('Approve and start')).toBeUndefined();
    await view.click('Send for revision');
    expect(onSave).toHaveBeenCalledTimes(1);
    expect(onSend.mock.calls).toEqual([
      ['revise', 4],
      ['revise', 4],
    ]);
  });
});

describe('gateway plan workflow', () => {
  it.each([false, true])(
    'reads the toggle and sends an ordinary version-pinned turn only when enabled=%s',
    async (enabled) => {
      const client = {
        base: 'http://127.0.0.1:7777',
        setting: vi.fn(async () => ({ enabled })),
        submitTurn: vi.fn(async () => ({})),
      };
      const view = await mount(
        <MarkdownArtifact
          commentable
          client={client as unknown as GatewayClient}
          sid="s1"
          iterationId="i1"
          name="PLAN-search.md"
          mediaType="text/markdown"
          source="fixture"
          version={3}
          chrome={chrome}
        />,
      );
      if (enabled) {
        await view.click('Approve and start');
        expect(client.submitTurn).toHaveBeenCalledExactlyOnceWith(
          's1',
          actionRequest('PLAN-search.md', 3, 'approve'),
        );
      } else {
        expect(view.button('Approve and start')).toBeUndefined();
        expect(client.submitTurn).not.toHaveBeenCalled();
      }
    },
  );

  it('rechecks a toggle switched off after the document opened', async () => {
    const client = {
      base: 'http://127.0.0.1:7777',
      setting: vi
        .fn()
        .mockResolvedValueOnce({ enabled: true })
        .mockResolvedValue({ enabled: false }),
      submitTurn: vi.fn(),
    };
    const view = await mount(
      <MarkdownArtifact
        commentable
        client={client as unknown as GatewayClient}
        sid="s1"
        iterationId="i1"
        name="PLAN-search.md"
        mediaType="text/markdown"
        source="fixture"
        version={3}
        chrome={chrome}
      />,
    );
    await view.click('Approve and start');
    expect(client.submitTurn).not.toHaveBeenCalled();
    expect(view.host.textContent).toContain('Plan before coding is off');
  });
});
