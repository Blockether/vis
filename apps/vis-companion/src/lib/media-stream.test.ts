import { describe, expect, it } from 'vitest';
import { mediaStreamSrc } from './media-stream';

const STREAM = 'http://10.0.2.2:7890/v1/sessions/s-1/fs/file/content?path=%2Ftmp%2Fa%20b.mp3&ticket=t-1';
const APP = { origin: 'https://localhost', protocol: 'https:' };

// #449: the Android web view blocked an http stream under its https page as mixed content.
describe('mediaStreamSrc', () => {
  it('plays an http stream through the app origin on Android', () => {
    const src = mediaStreamSrc(STREAM, 'android', APP);
    expect(src).toBe(`https://localhost/_vis/media?src=${encodeURIComponent(STREAM)}`);
    // The native proxy gets back the exact stream URL, ticket included.
    expect(new URL(src).searchParams.get('src')).toBe(STREAM);
  });

  it('loads the stream directly on iOS, on the web and from an https gateway', () => {
    expect(mediaStreamSrc(STREAM, 'ios', { origin: 'capacitor://localhost', protocol: 'capacitor:' })).toBe(STREAM);
    expect(mediaStreamSrc(STREAM, 'web', { origin: 'http://10.0.2.2:7890', protocol: 'http:' })).toBe(STREAM);
    const secure = STREAM.replace('http:', 'https:');
    expect(mediaStreamSrc(secure, 'android', APP)).toBe(secure);
  });

  it('loads the stream directly when the Android page itself is http', () => {
    expect(mediaStreamSrc(STREAM, 'android', { origin: 'http://localhost', protocol: 'http:' })).toBe(STREAM);
  });
});
