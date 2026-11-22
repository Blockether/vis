import { livePlacement, viewDrafts, type LiveView } from './live-view';
import type { TurnComment } from './types';

/** One saved app draft that the next message of the session adds. */
export interface ComposerComment extends TurnComment {
  /** Unique in a session: the view, the node and the thread. */
  key: string;
  /** Where the comment stands: the file and its lines, or the app title. */
  label: string;
  /** The note of the human, or the finding that it adds. */
  detail: string;
  /** The marker of the comment in the message text, such as `[COMMENT a.ts:4]`. */
  reference: string;
}

/** The saved drafts of the open apps in `views`, in list order. */
export function composerComments(views: readonly LiveView[]): ComposerComment[] {
  const issued = new Set<string>();
  return views
    .filter((view) => livePlacement(view) === 'attachment' && !view.is_closed)
    .flatMap((view) =>
      viewDrafts(view).map((draft) => {
        const anchor = draft.anchor;
        const lines =
          anchor && anchor.start_line && anchor.start_line !== anchor.line
            ? `${anchor.start_line}-${anchor.line}`
            : anchor
              ? `${anchor.line}`
              : '';
        const name = anchor ? `${anchor.path.split('/').pop()}:${lines}` : (draft.item_id ?? view.title);
        let reference = `[COMMENT ${name}]`;
        for (let n = 2; issued.has(reference); n++) reference = `[COMMENT ${name} #${n}]`;
        issued.add(reference);
        return {
          key: JSON.stringify([view.id, draft.node_id, draft.thread_id]),
          reference,
          view_id: view.id,
          node_id: draft.node_id,
          thread_id: draft.thread_id,
          label: anchor
            ? `${anchor.path}:${lines}`
            : draft.item_id
              ? `${view.title} · ${draft.item_id}`
              : view.title,
          detail: draft.body.trim() || draft.quote || '',
        };
      }),
    );
}

/** `text` with the markers of `comments` put at `caret`, and the caret after them. */
export function insertCommentReferences(
  text: string,
  caret: number,
  comments: readonly ComposerComment[],
) {
  const tokens = comments.map((comment) => comment.reference).join(' ');
  if (!tokens) return { text, caret };
  const before = text.slice(0, caret);
  const after = text.slice(caret);
  const inserted = `${before && !/\s$/.test(before) ? ' ' : ''}${tokens}${after && !/^\s/.test(after) ? ' ' : ''}`;
  return { text: before + inserted + after, caret: caret + inserted.length };
}

/** `text` without the marker of `comment` and one space next to it. */
export function dropCommentReference(text: string, comment: ComposerComment) {
  const at = text.indexOf(comment.reference);
  if (at < 0) return text;
  const end = at + comment.reference.length;
  return text[end] === ' '
    ? text.slice(0, at) + text.slice(end + 1)
    : text.slice(0, at > 0 && text[at - 1] === ' ' ? at - 1 : at) + text.slice(end);
}
