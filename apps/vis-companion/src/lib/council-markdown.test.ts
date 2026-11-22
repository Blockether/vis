import { describe, expect, it } from 'vitest';

import { councilLine, councilMarkdown } from './council-markdown';

// User report 2026-10-31, Council thread #8805: authored lines must read as separate blocks.
describe('councilMarkdown', () => {
  it.each([
    ['One.\nTwo.', 'One.\n\nTwo.'],
    ['- a\n- b\nAfter.', '- a\n- b\n\nAfter.'],
    ['1. a\n   more\n2. b', '1. a\n   more\n2. b'],
    ['Intro:\n```\nx\ny\n```\nAfter.', 'Intro:\n\n```\nx\ny\n```\n\nAfter.'],
    ['| a | b |\n| - | - |\n| 1 | 2 |', '| a | b |\n| - | - |\n| 1 | 2 |'],
    ['> a\n> b', '> a\n> b'],
    ['Title\n---', 'Title\n---'],
    ['One.\r\nTwo.\n\nThree.', 'One.\n\nTwo.\n\nThree.'],
  ])('splits only lines that leave a block: %j', (input, expected) => {
    expect(councilMarkdown(input)).toBe(expected);
  });
});

// User report 2026-11-22: a quoted `: ping\n\n` read as broken escapes without its backticks.
describe('councilLine', () => {
  it('drops Markdown markers but keeps inline code in backticks', () => {
    expect(councilLine('**Gateway** sends `: ping\\n\\n`;\n- next')).toBe('Gateway sends `: ping\\n\\n`; • next');
  });
});
