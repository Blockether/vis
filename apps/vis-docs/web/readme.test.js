import { expect,test } from 'vitest';
import { JSDOM } from 'jsdom';
import { readmeHTML } from './readme.js';
const item={readme_format:'md',readme_url:'https://github.com/example/repo/blob/'+ 'a'.repeat(40)+'/extension/README.md'};
test('README renders headings, lists, code and links at the reviewed source revision',()=>{
  const html=readmeHTML({...item,readme:'# Browser package\n\n- Example\n\n```python\nprint(42)\n```\n\n[Skill](skills/browser/SKILL.md)'}), doc=new JSDOM(html).window.document;
  expect(doc.querySelector('h3').textContent).toBe('Browser package');expect(doc.querySelector('li').textContent).toBe('Example');expect(doc.querySelector('code').textContent).toContain('print(42)');
  expect(doc.querySelector('a').href).toBe(item.readme_url.replace('README.md','skills/browser/SKILL.md'));
});
test('README never executes raw markup, exposes unsafe links or fetches remote images',()=>{
  const html=readmeHTML({...item,readme:'<script>untrusted()</script>\n\n<img src=x onerror=untrusted()>\n\n[Text](javascript:untrusted) ![Tracker](https://example.com/image.png)\n\n[Credentials](https://user:password@example.com/)'}),doc=new JSDOM(html).window.document;
  expect(doc.querySelector('script,img,iframe,style')).toBeNull();expect(doc.body.textContent).toContain('<script>');
  expect([...doc.querySelectorAll('a')].every(a=>a.href.startsWith('https://')&&!a.href.includes('password'))).toBe(true);
  expect(doc.querySelector('a').rel).toContain('nofollow');
});
test('non-Markdown README and empty states are literal and safe',()=>{
  expect(readmeHTML({...item,readme_format:'rst',readme:'<main>text</main>'})).toBe('<pre>&lt;main&gt;text&lt;/main&gt;</pre>');
  expect(readmeHTML(item)).toContain('No README');
});
