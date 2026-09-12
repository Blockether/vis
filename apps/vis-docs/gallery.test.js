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
  const galleries = [...dom.window.document.querySelectorAll('[data-screenshot-gallery]')];
  for (const gallery of galleries) {
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
  }
  if (enhanced) dom.window.eval(script);
  return { window: dom.window, galleries };
}

afterEach(() => {
  for (const dom of documents.splice(0)) dom.window.close();
});

test('three separate galleries provide real, captioned screenshots and README entry points', async () => {
  const { window, galleries } = galleryDocument({ enhanced: false });
  const readme = readFileSync('../../README.md', 'utf8');
  expect(galleries.map((gallery) => gallery.getAttribute('aria-label'))).toEqual([
    'iOS screenshots',
    'Desktop screenshots',
    'TUI screenshots',
  ]);
  const ids = [...window.document.querySelectorAll('[id]')].map((node) => node.id);
  expect(new Set(ids).size).toBe(ids.length);
  for (const gallery of galleries) {
    const track = gallery.querySelector('.screenshot-gallery__track');
    expect(track.tabIndex).toBe(0);
    expect(track.children).toHaveLength(3);
    expect(gallery.querySelector('.screenshot-gallery__controls').hidden).toBe(true);
    for (const button of gallery.querySelectorAll('button')) {
      expect(button.getAttribute('aria-controls')).toBe(track.id);
    }
    const platform = track.id.replace('-slides', '');
    expect(readme).toContain(`https://vis.blockether.com/#${platform}-gallery`);
    for (const slide of track.children) {
      const image = slide.querySelector('img');
      const source = image.getAttribute('src');
      expect(source).toMatch(new RegExp(`^assets/screenshots/${platform}-[a-z]+\\.png$`));
      expect(slide.querySelector('a').getAttribute('href')).toBe(source);
      expect(image.alt.length).toBeGreaterThan(30);
      expect(image.getAttribute('loading')).toBe('lazy');
      expect(slide.querySelector('figcaption').textContent.length).toBeGreaterThan(40);
      const metadata = await sharp(`../../resources/vis-docs/${source}`).metadata();
      expect(metadata.width).toBe(Number(image.getAttribute('width')));
      expect(metadata.height).toBe(Number(image.getAttribute('height')));
      expect(metadata.exif).toBeUndefined();
      expect(metadata.xmp).toBeUndefined();
      expect(readme).toContain(source);
    }
  }
});

test('buttons advance only their own gallery and stop at its boundaries', () => {
  const { galleries } = galleryDocument();
  const first = galleries[0];
  const previous = first.querySelector('[data-previous]');
  const next = first.querySelector('[data-next]');
  expect(first.querySelector('.screenshot-gallery__controls').hidden).toBe(false);
  expect(previous.disabled).toBe(true);
  next.click();
  expect(first.querySelector('[role="status"]').textContent).toBe('2 / 3');
  expect(previous.disabled).toBe(false);
  next.click();
  expect(next.disabled).toBe(true);
  next.click();
  expect(first.querySelector('[role="status"]').textContent).toBe('3 / 3');
  previous.click();
  expect(first.querySelector('[role="status"]').textContent).toBe('2 / 3');
  for (const gallery of galleries.slice(1)) {
    expect(gallery.querySelector('[role="status"]').textContent).toBe('1 / 3');
  }
});

test('keyboard navigation is scoped to the track and respects reduced motion', () => {
  const { window, galleries } = galleryDocument({ reducedMotion: true });
  const track = galleries[0].querySelector('.screenshot-gallery__track');
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
  expect(track.scrollTo).toHaveBeenLastCalledWith({
    left: 600,
    behavior: 'instant',
  });
  key(track, 'End');
  expect(track.scrollTo).toHaveBeenLastCalledWith({
    left: 1200,
    behavior: 'instant',
  });
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
  const { window, galleries } = galleryDocument();
  const gallery = galleries[1];
  const track = gallery.querySelector('.screenshot-gallery__track');
  track.scrollLeft = 1190;
  track.dispatchEvent(new window.Event('scroll'));
  expect(gallery.querySelector('[role="status"]').textContent).toBe('3 / 3');
  expect(gallery.querySelector('[data-next]').disabled).toBe(true);
  gallery.querySelector('[data-previous]').click();
  expect(track.scrollTo).toHaveBeenLastCalledWith({
    left: 600,
    behavior: 'smooth',
  });
  expect(script).not.toMatch(/setInterval|setTimeout/);
});

test('the enhancement is harmless on documentation pages without galleries', () => {
  const dom = new JSDOM('<p>Another guide</p>', { runScripts: 'outside-only' });
  documents.push(dom);
  dom.window.Prism = { highlightAll: vi.fn() };
  expect(() => dom.window.eval(script)).not.toThrow();
  expect(dom.window.Prism.highlightAll).toHaveBeenCalledOnce();
});
