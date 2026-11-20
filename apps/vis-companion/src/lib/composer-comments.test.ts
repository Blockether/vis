import { describe, expect, it } from 'vitest';

import { insertCommentReferences, type ComposerComment } from './composer-comments';

const comment = { reference: '[COMMENT a.ts:4]' } as ComposerComment;

describe('comment markers in the message', () => {
  it('puts a new marker at the caret, with spaces between it and the words', () => {
    expect(insertCommentReferences('Fix this', 3, [comment])).toEqual({
      text: 'Fix [COMMENT a.ts:4] this',
      caret: 20,
    });
  });
});
