import { expect, test } from 'vitest';
import { readFileSync, readdirSync } from 'node:fs';
import { JSDOM } from 'jsdom';
import sharp from 'sharp';
import { origin, catalogMetadata, sitemap } from './web/discovery.js';
import { renderPage } from './web/render.js';
import items from './web/catalog.fixture.json';

const read = name => readFileSync('dist/'+name, 'utf8');
function xmlLocations(text) {
  const dom=new JSDOM(text,{contentType:'application/xml'});
  try { return [...dom.window.document.querySelectorAll('loc')].map(node=>node.textContent); }
  finally {dom.window.close();}
}
test('every generated document has canonical metadata, an accessible icon and a Markdown alternate',()=>{
  const pages=readdirSync('dist').filter(name=>name.endsWith('.html'));
  const locations=xmlLocations(read('sitemap-docs.xml'));
  const descriptions=new Set();
  for(const file of pages) {
    const dom=new JSDOM(read(file));
    try {
      const d=dom.window.document, canonical=origin+(file==='index.html'?'/':'/'+file);
      expect(d.querySelectorAll('title')).toHaveLength(1);
      expect(d.querySelector('link[rel="canonical"]').href).toBe(canonical);
      expect(locations).toContain(canonical);
      const description=d.querySelector('meta[name="description"]').content;
      expect(description.length).toBeGreaterThan(10);descriptions.add(description);
      expect(d.querySelector('meta[property="og:url"]').content).toBe(canonical);
      expect(d.querySelector('meta[property="og:title"]').content).toBe(d.title);
      expect(d.querySelector('meta[name="twitter:description"]').content).toBe(description);
      const schema=JSON.parse(d.querySelector('script[type="application/ld+json"]').textContent);
      expect(schema.url).toBe(canonical);expect(schema['@type']).toBe('TechArticle');
      const md=file.replace('.html','.md');
      expect(d.querySelector('link[type="text/markdown"]').getAttribute('href')).toBe('/'+md);
      expect(read('llms.txt')).toContain(origin+'/'+md);
      expect(read('llms-full.txt')).toContain(read(md));
      expect(d.querySelector('.center-link').getAttribute('aria-label')).toBe('Extension Center');
      expect(d.querySelector('.center-link svg')).not.toBeNull();
      expect(d.querySelector('.center-link').textContent).toBe('');
      expect(d.querySelector('.side a[href="/extensions/"]').textContent).toBe('Extension Center');
    } finally {dom.window.close();}
  }
  expect(locations).toHaveLength(pages.length);
  expect(new Set(locations).size).toBe(pages.length);
  expect(descriptions.size).toBe(pages.length);
  expect(xmlLocations(read('sitemap.xml'))).toEqual([origin+'/sitemap-docs.xml',origin+'/extensions/sitemap.xml']);
  expect(read('robots.txt')).toContain('Sitemap: '+origin+'/sitemap.xml');
  expect(read('llms.txt')).toContain(origin+'/extensions/llms.txt');
  expect(readdirSync('dist')).not.toContain('source.json');
});
test('shared favicons have the declared dimensions and a valid ICO directory',async()=>{
  for(const [name,size] of [['favicon-16.png',16],['favicon-32.png',32],['apple-touch-icon.png',180],['icon-192.png',192],['icon-512.png',512]]) {
    const meta=await sharp('dist/'+name).metadata();expect(meta.width).toBe(size);expect(meta.height).toBe(size);
  }
  const ico=readFileSync('dist/favicon.ico');expect(ico.readUInt16LE(2)).toBe(1);expect(ico.readUInt32LE(18)).toBe(22);
  expect(ico.subarray(22)).toEqual(readFileSync('dist/favicon-32.png'));
  const manifest=JSON.parse(read('site.webmanifest'));expect(manifest.icons).toHaveLength(2);
  for(const icon of manifest.icons) expect(readFileSync('dist'+icon.src).length).toBeGreaterThan(0);
});
test('catalog SSR supplies item-specific metadata and never turns metadata into executable markup',()=>{
  const item={...items[0],name:'Quoted "name"',description:'Text <tag> & punctuation'};
  const dom=new JSDOM(renderPage({items:[item],item}));
  try {
    const d=dom.window.document;
    expect(d.querySelector('meta[name="description"]').content).toBe(item.description);
    expect(d.querySelector('link[rel="canonical"]').href).toBe(origin+'/extensions/'+item.id);
    expect(JSON.parse(d.querySelector('script[type="application/ld+json"]').textContent).description).toBe(item.description);
    expect(d.querySelectorAll('head script')).toHaveLength(1);
    expect(d.querySelector('link[rel="icon"]').getAttribute('href')).toBe('/favicon-32.png');
  } finally {dom.window.close();}
  expect(catalogMetadata({error:'Unavailable'})).toContain('noindex, follow');
  expect(catalogMetadata({detailError:true})).toContain('noindex, follow');
  expect(catalogMetadata({search:'?q=example'})).toContain('href="'+origin+'/extensions/"');
  expect(xmlLocations(sitemap(['/extensions/']))).toEqual([origin+'/extensions/']);
});
