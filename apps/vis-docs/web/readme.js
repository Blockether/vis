import { Marked } from 'marked';
import { escapeHTML } from './html.js';

/** No raw HTML or remote image loads; relative links stay at the reviewed commit. */
export function readmeHTML(item) {
  if (typeof item.readme !== 'string' || !item.readme)
    return '<p>No README content was provided with this listing.</p>';
  if (item.readme_format !== 'md') return `<pre>${escapeHTML(item.readme)}</pre>`;
  const markdown = new Marked({
    gfm: true,
    breaks: false,
    renderer: {
      html({ text }) {
        return escapeHTML(text);
      },
      heading({ tokens, depth }) {
        const level = Math.min(depth + 2, 6);
        return `<h${level}>${this.parser.parseInline(tokens)}</h${level}>`;
      },
      link({ href, tokens }) {
        const text = this.parser.parseInline(tokens);
        try {
          const url = new URL(href, item.readme_url);
          if (url.protocol === 'https:' && !url.username && !url.password)
            return `<a href="${escapeHTML(url.href)}" target="_blank" rel="noopener noreferrer nofollow">${text}</a>`;
        } catch {
          /* Invalid destinations remain text. */
        }
        return text;
      },
      image({ href, text }) {
        try {
          const url = new URL(href, item.readme_url);
          if (url.protocol === 'https:' && !url.username && !url.password)
            return `<a href="${escapeHTML(url.href)}" target="_blank" rel="noopener noreferrer nofollow">Image: ${escapeHTML(text || 'View image')}</a>`;
        } catch {
          /* Do not load trackers or unsafe images. */
        }
        return escapeHTML(text);
      },
    },
  });
  return markdown.parse(item.readme);
}
