// @vitest-environment jsdom
import { describe, expect, it } from 'vitest';

import { sanitizeArtifactHtml } from './doc-capture';

// The capture copy is mounted in the APP's own document, so "it cannot execute"
// is not a sandbox attribute any more — it is this function. A hostile artifact
// gets every way of running something taken away from it, and keeps everything
// that makes the picture look right.
const HOSTILE = [
  '<!doctype html><html><head>',
  '<style>#app-probe{background:red !important}</style>',
  '</head>',
  '<body onload="window.__pwned = 1" style="background:#eef">',
  '<h1 style="color:#b00">Invoice 42</h1>',
  '<script>window.__pwned = 2</script>',
  '<img src="/nope.png" onerror="window.__pwned = 3">',
  '<iframe src="about:blank"></iframe>',
  '<object data="x.swf"></object>',
  '<a href="javascript:window.__pwned = 4">total</a>',
  '<p>Total: 1234.56</p>',
  '</body></html>',
].join('');

describe('sanitizeArtifactHtml', () => {
  const clean = sanitizeArtifactHtml(HOSTILE);
  const html = clean.outerHTML;

  it('returns the whole document, not just its body', () => {
    expect(clean.tagName.toLowerCase()).toBe('html');
    expect(clean.querySelector('body')).not.toBeNull();
  });

  it('leaves nothing that can run', () => {
    expect(html).not.toContain('<script');
    expect(html).not.toContain('onload');
    expect(html).not.toContain('onerror');
    expect(html).not.toContain('javascript:');
    expect(clean.querySelector('iframe')).toBeNull();
    expect(clean.querySelector('object')).toBeNull();
  });

  it('keeps the ink, because a stripped artifact photographs as a blank page', () => {
    expect(clean.querySelector('style')?.textContent).toContain('#app-probe');
    expect(clean.querySelector('h1')?.getAttribute('style')).toContain('#b00');
    expect(clean.textContent).toContain('Invoice 42');
    expect(clean.textContent).toContain('Total: 1234.56');
  });

  it('survives markup that is not a document at all', () => {
    const fragment = sanitizeArtifactHtml('<p>bare</p>');
    expect(fragment.textContent).toContain('bare');
  });
});
