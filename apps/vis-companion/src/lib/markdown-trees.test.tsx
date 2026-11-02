import { renderToStaticMarkup } from 'react-dom/server';
import ReactMarkdown, { type Options } from 'react-markdown';
import remarkBreaks from 'remark-breaks';
import remarkGfm from 'remark-gfm';
import { describe, expect, it, vi } from 'vitest';

type Plugins = NonNullable<Options['remarkPlugins']>;

/** Every case starts with an empty cache. */
async function freshCache() {
  vi.resetModules();
  return import('./markdown-trees');
}

function leaf(value: string) {
  return { type: 'root', children: [{ type: 'text', value }] };
}

describe('cachedTree', () => {
  it('parses a text once and gives each caller its own copy', async () => {
    const { cachedTree } = await freshCache();
    const parse = vi.fn(() => leaf('Read the plan.'));
    const first = cachedTree('Read the **plan**.', parse);
    const second = cachedTree('Read the **plan**.', parse);
    expect(parse).toHaveBeenCalledTimes(1);
    expect(second).toEqual(first);
    expect(second).not.toBe(first);
    second.children[0].value = 'Edited by a transform.';
    expect(cachedTree('Read the **plan**.', parse)).toEqual(leaf('Read the plan.'));
  });

  it('keeps a streamed text only when it stops growing', async () => {
    const { cachedTree } = await freshCache();
    const parse = vi.fn((text: string) => leaf(text));
    const states = ['Checking', 'Checking the tests', 'Checking the tests: all pass.'];
    for (const text of states) cachedTree(text, () => parse(text));
    expect(parse).toHaveBeenCalledTimes(3);
    // The transcript shows the finished text again: that parse is kept.
    cachedTree(states[2], () => parse(states[2]));
    cachedTree(states[2], () => parse(states[2]));
    expect(parse).toHaveBeenCalledTimes(4);
    // A state in the middle of the stream was never kept.
    cachedTree(states[1], () => parse(states[1]));
    expect(parse).toHaveBeenCalledTimes(5);
  });

  it('drops the least recently used tree past its budget', async () => {
    const { cachedTree, MARKDOWN_TREE_BUDGET } = await freshCache();
    const parse = vi.fn((text: string) => leaf(text.slice(0, 1)));
    const size = Math.floor(MARKDOWN_TREE_BUDGET / 3) - 300;
    const [a, b, c, d] = ['a', 'b', 'c', 'd'].map((mark) => mark.repeat(size));
    for (const text of [a, b, c, a, d, a, c, d]) cachedTree(text, () => parse(text));
    expect(parse.mock.calls.map(([text]) => text[0])).toEqual(['a', 'b', 'c', 'd']);
    cachedTree(b, () => parse(b));
    expect(parse).toHaveBeenCalledTimes(5);
  });

  it('never keeps a text larger than the whole budget', async () => {
    const { cachedTree, MARKDOWN_TREE_BUDGET } = await freshCache();
    const parse = vi.fn(() => leaf('x'));
    const huge = 'x'.repeat(MARKDOWN_TREE_BUDGET);
    cachedTree(huge, parse);
    cachedTree(huge, parse);
    expect(parse).toHaveBeenCalledTimes(2);
  });
});

describe('advanceSplit', () => {
  /** The pieces after the last state. Each last piece parses as a message parses it. */
  async function splitStream(states: string[]) {
    const { advanceSplit, blockStartsOf, remarkParseCache, wholeSplit } = await freshCache();
    let split = wholeSplit(states[0]);
    for (const state of states) {
      split = advanceSplit(split, state, blockStartsOf);
      const tail = split.pieces[split.pieces.length - 1].text;
      renderToStaticMarkup(
        <ReactMarkdown remarkPlugins={[remarkGfm, remarkParseCache]}>{tail}</ReactMarkdown>,
      );
    }
    return split.pieces;
  }

  it('cuts a growing text after the blocks that closed', async () => {
    const pieces = await splitStream([
      '## Plan\n\nRead the logs.\n\n- one\n',
      '## Plan\n\nRead the logs.\n\n- one\n- two',
    ]);
    expect(pieces).toEqual([
      { start: 0, text: '## Plan\n\nRead the logs.\n\n' },
      { start: 25, text: '- one\n- two' },
    ]);
  });

  it('keeps a block open while the line where it opens is unfinished', async () => {
    // `2` alone is a paragraph after the list, but `2. Fix` continues the list.
    const pieces = await splitStream(['1. Check\n\n2', '1. Check\n\n2. Fix']);
    expect(pieces).toEqual([{ start: 0, text: '1. Check\n\n2. Fix' }]);
  });

  it('keeps a text with a link definition whole', async () => {
    // The definition at the end gives the reference in the first block its link.
    const text = 'Read [the guide][g].\n\nThen fix it.\n\n[g]: https://example.com/guide\n';
    const pieces = await splitStream([text.slice(0, 36), text]);
    expect(pieces).toEqual([{ start: 0, text }]);
  });

  it('makes a text one piece again when it changes in another way', async () => {
    const { advanceSplit, wholeSplit } = await freshCache();
    const grown = {
      text: 'Old answer.\n\nMore.',
      pieces: [
        { start: 0, text: 'Old answer.\n\n' },
        { start: 13, text: 'More.' },
      ],
    };
    expect(advanceSplit(grown, 'New answer.', () => [0])).toEqual(wholeSplit('New answer.'));
  });
});

describe('remarkParseCache', () => {
  const text = 'First line\nsecond line, see https://example.com and ~~old~~.\n\n| a | b |\n| - | - |\n| 1 | 2 |';
  const render = (plugins: Plugins) =>
    renderToStaticMarkup(<ReactMarkdown remarkPlugins={plugins}>{text}</ReactMarkdown>);

  it('renders what an uncached parse renders and keeps transforms out of the kept tree', async () => {
    const { remarkParseCache } = await freshCache();
    const plain = render([remarkGfm]);
    const broken = render([remarkGfm, remarkBreaks]);
    expect(broken).not.toBe(plain);
    expect(render([remarkGfm, remarkBreaks, remarkParseCache])).toBe(broken);
    // Each transform edits its own copy: the kept tree still renders without breaks.
    expect(render([remarkGfm, remarkParseCache])).toBe(plain);
    expect(render([remarkGfm, remarkBreaks, remarkParseCache])).toBe(broken);
    expect(render([remarkGfm, remarkParseCache])).toBe(plain);
  });

  it('parses a text once across mounts', async () => {
    const { remarkParseCache } = await freshCache();
    let parses = 0;
    const countParses: Plugins[number] = function countParses() {
      const parse = this.parser;
      if (!parse) return;
      this.parser = (doc, file) => {
        parses += 1;
        return parse(doc, file);
      };
    };
    const first = render([remarkGfm, countParses, remarkParseCache]);
    expect(render([remarkGfm, countParses, remarkParseCache])).toBe(first);
    expect(parses).toBe(1);
  });
});
