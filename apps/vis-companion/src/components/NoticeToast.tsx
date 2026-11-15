import type { Notices } from '../lib/use-notices';
import { useOpenPath } from '../lib/open-path';
import { linkedText } from './linked-text';
import { Toast } from './ui';

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
      {linkedText(notice.text, openPath)}
    </Toast>
  );
}
