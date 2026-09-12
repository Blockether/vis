import { afterEach, expect, test, vi } from 'vitest';
import { readFileSync } from 'node:fs';
import { JSDOM } from 'jsdom';
import sharp from 'sharp';

const markup = readFileSync('dist/index.html', 'utf8');
const script = readFileSync('../../resources/vis-docs/assets/docs.js', 'utf8');
const documents = [];

function galleryDocument({ reducedMotion = false, enhanced = true } = {}) {
  const dom = new JSDOM(markup, { runScripts: 'outside-only' });
  documents.push(dom);
  dom.window.Prism = { highlightAll: vi.fn() };
  dom.window.matchMedia = () => ({ matches: reducedMotion });
  const gallery = dom.window.document.querySelector('[data-screenshot-gallery]');
  const track = gallery.querySelector('.screenshot-gallery__track');
  [...track.children].forEach((slide, index) => {
    Object.defineProperty(slide, 'offsetLeft', {
      get: () => index * 600 + 24,
    });
  });
  track.scrollTo = vi.fn(({ left }) => {
    track.scrollLeft = left;
    track.dispatchEvent(new dom.window.Event('scroll'));
  });
  if (enhanced) dom.window.eval(script);
  return { window: dom.window, gallery, track };
}

afterEach(() => {
  for (const dom of documents.splice(0)) dom.window.close();
});

test('one gallery combines all nine screenshots with one README preview grid', async () => {
  const { window, gallery, track } = galleryDocument({ enhanced: false });
  const readme = readFileSync('../../README.md', 'utf8');
  const readmeGallery = readme.split('## Screenshot gallery')[1].split('\n## ')[0];
  const preview = new JSDOM(readmeGallery);
  documents.push(preview);
  expect(window.document.querySelectorAll('[data-screenshot-gallery]')).toHaveLength(1);
  expect(gallery.getAttribute('aria-label')).toBe('Vis screenshots');
  expect(gallery.id).toBe('screenshot-gallery');
  expect(preview.window.document.querySelectorAll('table')).toHaveLength(1);
  expect(preview.window.document.querySelectorAll('img')).toHaveLength(9);
  expect(readmeGallery).not.toMatch(/^### /m);
  expect(readmeGallery).toContain('https://vis.blockether.com/#screenshot-gallery');
  const ids = [...window.document.querySelectorAll('[id]')].map((node) => node.id);
  expect(new Set(ids).size).toBe(ids.length);
  expect(track.tabIndex).toBe(0);
  expect(track.children).toHaveLength(9);
  expect(gallery.querySelector('.screenshot-gallery__controls').hidden).toBe(true);
  for (const button of gallery.querySelectorAll('button')) {
    expect(button.getAttribute('aria-controls')).toBe(track.id);
  }
  const platforms = [];
  for (const [index, slide] of [...track.children].entries()) {
    const image = slide.querySelector('img');
    const source = image.getAttribute('src');
    const platform = source.match(/^assets\/screenshots\/(ios|desktop|tui)-[a-z]+\.png$/)?.[1];
    expect(platform).toBeDefined();
    platforms.push(platform);
    expect(slide.getAttribute('aria-label')).toBe(`${index + 1} of 9`);
    expect(slide.querySelector('a').getAttribute('href')).toBe(source);
    expect(image.alt.length).toBeGreaterThan(30);
    expect(image.getAttribute('loading')).toBe('lazy');
    expect(slide.querySelector('figcaption').textContent).toMatch(/^(iOS|Desktop|TUI) · .{1,24}$/);
    const metadata = await sharp(`../../resources/vis-docs/${source}`).metadata();
    expect(metadata.width).toBe(Number(image.getAttribute('width')));
    expect(metadata.height).toBe(Number(image.getAttribute('height')));
    expect(metadata.exif).toBeUndefined();
    expect(metadata.xmp).toBeUndefined();
    const thumbnail = preview.window.document.querySelector(
      `img[src="resources/vis-docs/${source}"]`,
    );
    expect(thumbnail.closest('a').getAttribute('href')).toBe(`resources/vis-docs/${source}`);
  }
  expect(platforms).toEqual([
    'desktop',
    'ios',
    'tui',
    'desktop',
    'ios',
    'tui',
    'desktop',
    'ios',
    'tui',
  ]);
  for (const platform of ['ios', 'desktop', 'tui']) {
    const anchor = window.document.getElementById(`${platform}-gallery`);
    expect(anchor.parentElement).toBe(track);
    expect(anchor.querySelector('img').getAttribute('src')).toContain(`/${platform}-`);
  }
});

test('mixed portrait and landscape screenshots share a bounded frame without stretching', () => {
  const { window } = galleryDocument({ enhanced: false });
  const style = window.document.createElement('style');
  style.textContent = readFileSync('../../resources/vis-docs/assets/theme.css', 'utf8');
  window.document.head.append(style);
  const rules = [...style.sheet.cssRules];
  const frame = rules.find((rule) => rule.selectorText === '.screenshot-gallery__image').style;
  const image = rules.find((rule) => rule.selectorText === '.screenshot-gallery__image img').style;
  expect(frame.height).toBe('60vh');
  expect(frame.minHeight).toBe('20rem');
  expect(frame.maxHeight).toBe('34rem');
  expect(image.width).toBe('auto');
  expect(image.height).toBe('auto');
  expect(image.maxWidth).toBe('100%');
  expect(image.maxHeight).toBe('100%');
});

test('buttons browse all nine screenshots and stop at both boundaries', () => {
  const { gallery } = galleryDocument();
  const previous = gallery.querySelector('[data-previous]');
  const next = gallery.querySelector('[data-next]');
  expect(gallery.querySelector('.screenshot-gallery__controls').hidden).toBe(false);
  expect(previous.disabled).toBe(true);
  for (let index = 2; index <= 9; index++) {
    next.click();
    expect(gallery.querySelector('[role="status"]').textContent).toBe(`${index} / 9`);
    expect(previous.disabled).toBe(false);
  }
  expect(next.disabled).toBe(true);
  next.click();
  expect(gallery.querySelector('[role="status"]').textContent).toBe('9 / 9');
  for (let index = 8; index >= 1; index--) {
    previous.click();
    expect(gallery.querySelector('[role="status"]').textContent).toBe(`${index} / 9`);
  }
  expect(previous.disabled).toBe(true);
  expect(next.disabled).toBe(false);
});

test('keyboard navigation is scoped to the track and respects reduced motion', () => {
  const { window, track } = galleryDocument({ reducedMotion: true });
  const key = (target, value, options = {}) =>
    target.dispatchEvent(
      new window.KeyboardEvent('keydown', {
        key: value,
        bubbles: true,
        cancelable: true,
        ...options,
      }),
    );
  expect(key(track, 'ArrowRight')).toBe(false);
  expect(track.scrollTo).toHaveBeenLastCalledWith({ left: 600, behavior: 'instant' });
  key(track, 'End');
  expect(track.scrollTo).toHaveBeenLastCalledWith({ left: 4800, behavior: 'instant' });
  key(track, 'Home');
  key(track, 'ArrowLeft');
  expect(track.scrollLeft).toBe(0);
  track.scrollTo.mockClear();
  expect(key(track.querySelector('a'), 'ArrowRight')).toBe(true);
  expect(key(track, 'ArrowRight', { ctrlKey: true })).toBe(true);
  expect(key(track, 'Tab')).toBe(true);
  expect(track.scrollTo).not.toHaveBeenCalled();
});

test('native scrolling updates the count and button state without starting autoplay', () => {
  const { window, gallery, track } = galleryDocument();
  track.scrollLeft = 4790;
  track.dispatchEvent(new window.Event('scroll'));
  expect(gallery.querySelector('[role="status"]').textContent).toBe('9 / 9');
  expect(gallery.querySelector('[data-next]').disabled).toBe(true);
  gallery.querySelector('[data-previous]').click();
  expect(track.scrollTo).toHaveBeenLastCalledWith({ left: 4200, behavior: 'smooth' });
  expect(script).not.toMatch(/setInterval|setTimeout/);
});

test('the enhancement is harmless on documentation pages without galleries', () => {
  const dom = new JSDOM('<p>Another guide</p>', { runScripts: 'outside-only' });
  documents.push(dom);
  dom.window.Prism = { highlightAll: vi.fn() };
  expect(() => dom.window.eval(script)).not.toThrow();
  expect(dom.window.Prism.highlightAll).toHaveBeenCalledOnce();
});
