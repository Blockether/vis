import { describe, expect, it } from 'vitest';

import sessionListSource from '../components/SessionList.tsx?raw';
import sessionsScreenSource from './SessionsScreen.tsx?raw';
import sessionProjectGroupsSource from './sessions/SessionProjectGroups.tsx?raw';

describe('sessions feature boundaries', () => {
  it('leaves session presentation outside the fleet orchestrator', () => {
    const ownedRenderers = [
      ...sessionsScreenSource.matchAll(
        /^(?:const\s+)?(SessionRow|SessionStats|NavigatorSkeleton|ProjectGroup)\s*=|^function\s+(SessionStats|NavigatorSkeleton|ProjectGroup)\s*\(/gm,
      ),
    ].map((match) => match[1] ?? match[2]);

    expect(ownedRenderers).toEqual([]);
  });

  it('passes row interaction through one reusable feature contract', () => {
    const signatures = ['ProjectGroup'].map((name) => {
      const match = sessionProjectGroupsSource.match(
        new RegExp(`${name} = memo\\(function ${name}\\(\\{([\\s\\S]*?)\\}: \{`),
      );
      expect(match?.[1], `${name} signature`).toMatch(/\S/);
      return match?.[1] ?? '';
    });
    const looseActions = [
      'onOpen',
      'onRename',
      'onFork',
      'onDelete',
      'onToggleStar',
      'rowAction',
      'deleteBusy',
      'deleteError',
      'onConfirmDelete',
      'onCancelDelete',
    ].filter((name) =>
      signatures.some((signature) => new RegExp(`^\\s*${name},?$`, 'm').test(signature)),
    );

    expect(looseActions).toEqual([]);
    expect(signatures.every((signature) => /^\s*context,?$/m.test(signature))).toBe(true);
  });

  it('gives each session row commands and deletion state instead of loose callbacks', () => {
    const match = sessionListSource.match(
      /SessionRow = memo\(function SessionRow\(\{([\s\S]*?)\}: \{/,
    );
    expect(match?.[1], 'SessionRow signature').toMatch(/\S/);
    const signature = match?.[1] ?? '';
    const looseActions = [
      'onOpen',
      'onRename',
      'onFork',
      'onDelete',
      'onToggleStar',
      'isConfirmingDelete',
      'deleteBusy',
      'deleteError',
      'onConfirmDelete',
      'onCancelDelete',
    ].filter((name) => new RegExp(`^\\s*${name},?$`, 'm').test(signature));

    expect(looseActions).toEqual([]);
    expect(signature).toMatch(/^\s*commands,?$/m);
    expect(signature).toMatch(/^\s*deletion,?$/m);
  });

  it('passes project groups through fleet domain contracts', () => {
    const match = sessionProjectGroupsSource.match(
      /ProjectGroup = memo\(function ProjectGroup\(\{([\s\S]*?)\}: \{/,
    );
    expect(match?.[1], 'ProjectGroup signature').toMatch(/\S/);
    const signature = match?.[1] ?? '';
    const leakedFields = [
      'project',
      'root',
      'sessions',
      'tally',
      'conn',
      'getClient',
      'matches',
      'needle',
      'drafts',
      'rowActions',
      'pageSize',
      'epoch',
      'admitted',
      'isVisible',
      'list',
    ].filter((name) => new RegExp(`^\\s*${name},?$`, 'm').test(signature));

    expect(leakedFields).toEqual([]);
    expect(signature).toMatch(/^\s*group,?$/m);
    expect(signature).toMatch(/^\s*machine,?$/m);
    expect(signature).toMatch(/^\s*context,?$/m);
    expect(signature).toMatch(/^\s*reading,?$/m);
  });

  // Regression, user report (paraphrased: on the phone a strip at the very bottom
  // hides the last session of a machine): the section padded itself by the bottom
  // safe area, so the list stopped above an opaque band under the home indicator
  // and the final row of a page stayed cut behind it however far it was scrolled.
  it('gives the bottom safe area to the scrolling list, not the section around it', () => {
    const section = sessionsScreenSource.match(
      /<section\s+aria-label="Sessions"\s+className=\{\s*`([^`]*)`\s*\}/,
    );
    expect(section?.[1], 'sessions section').toMatch(/\S/);
    expect(section?.[1]).not.toContain('safe-area-inset-bottom');
    const list = sessionsScreenSource.match(
      /<div\s+ref=\{listRef\}\s+className=\{\s*`([^`]*)`\s*\}/,
    );
    expect(list?.[1], 'list scroller').toMatch(/\S/);
    expect(list?.[1]).toContain('overflow-y-auto');
    expect(list?.[1]).toContain('pb-[calc(0.75rem+env(safe-area-inset-bottom))]');
  });

  it('keeps one fixed top seam above scrolling project headings', () => {
    const viewport = sessionsScreenSource.match(
      /<div className="([^"]*overflow-hidden[^"]*bg-page[^"]*)">/,
    );
    expect(viewport?.[1], 'list viewport').toMatch(/\S/);
    const classes = viewport?.[1].split(' ');
    expect(classes).not.toContain('border-t');
    expect(classes).toEqual(
      expect.arrayContaining([
        'before:absolute',
        'before:inset-x-0',
        'before:top-0',
        'before:z-20',
        'before:border-t',
        'before:border-white',
        'before:pointer-events-none',
      ]),
    );
  });

  it('keeps session rename inside the row', () => {
    expect(sessionListSource).toContain('renameDraft');
    expect(sessionListSource).toContain('commitRename');
    expect(sessionsScreenSource).not.toContain('RenameSessionDialog');
    expect(sessionsScreenSource).not.toContain("mode === 'rename'");
  });
});
