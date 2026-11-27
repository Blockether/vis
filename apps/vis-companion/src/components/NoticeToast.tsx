import type { ReactNode } from 'react';

import { revealKeptFile } from '../lib/artifact-open';
import { homeifyPath } from '../lib/path';
import type { Notices } from '../lib/use-notices';
import { useOpenPath } from '../lib/open-path';
import { fileUrlLink, linkedText, type OpenPath } from './linked-text';
import { Toast } from './ui';

/**
 * The text of a notice. Each bare URL is a link. The path of a kept file is a link too: a
 * press shows the file in the file manager, and `onLost` runs when the file is not there.
 * The text shows the path with `~` for the home folder.
 */
function noticeText(text: string, path: string | undefined, openPath: OpenPath | null, onLost: () => void): ReactNode {
  const shown = homeifyPath(path);
  const at = path ? text.indexOf(shown) : -1;
  if (!path || at < 0) return linkedText(text, openPath);
  const show = () => {
    void revealKeptFile(path).then((isShown) => {
      if (!isShown) onLost();
    });
  };
  return (
    <>
      {text.slice(0, at)}
      {fileUrlLink(path, shown, show)}
      {text.slice(at + shown.length)}
    </>
  );
}

/** The toast of the page that shows. Each bare URL of its text is a link. */
export function NoticeToast({ notices }: { notices: Notices }) {
  const openPath = useOpenPath();
  const page = notices.page;
  if (!page) return null;
  const { notice } = page;
  return (
    <Toast
      key={notice.id}
      kind={notice.kind}
      title={notice.title}
      source={notice.source}
      onDismiss={() => notices.dismiss(notice.id)}
      page={{
        index: page.index,
        total: page.total,
        onPrevious: () => notices.turn(-1),
        onNext: () => notices.turn(1),
      }}
      countdown={{ durationMs: page.durationMs, remainingMs: page.remainingMs }}
    >
      {noticeText(notice.text, notice.path, openPath, () =>
        notices.push({
          id: `${notice.id}:lost`,
          kind: 'warn',
          title: notice.title,
          text: `The file manager did not show the file. Look for it at ${homeifyPath(notice.path)}`,
        }),
      )}
    </Toast>
  );
}
