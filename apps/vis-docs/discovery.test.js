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
      expect(description.length).toBeGreaterThanOrEqual(80);expect(description.length).toBeLessThanOrEqual(170);descriptions.add(description);
      expect(d.title).toMatch(/ · Vis · Blockether$/);
      expect(d.querySelector('link[rel="icon"][sizes="48x48"]').getAttribute('href')).toBe('/favicon-48.png');
      expect(d.querySelector('link[rel="icon"][type="image/x-icon"]').getAttribute('href')).toBe('/favicon.ico');
      expect(d.querySelector('a[href="https://blockether.com"]')).not.toBeNull();
      expect(d.querySelector('meta[property="og:url"]').content).toBe(canonical);
      expect(d.querySelector('meta[property="og:title"]').content).toBe(d.title);
      expect(d.querySelector('meta[name="twitter:description"]').content).toBe(description);
      const schema=JSON.parse(d.querySelector('script[type="application/ld+json"]').textContent);
      expect(schema.url).toBe(canonical);expect(schema['@type']).toBe('TechArticle');
      expect(schema.publisher).toEqual({'@type':'Organization',name:'Blockether',url:'https://blockether.com/'});
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
  for(const [name,size] of [['favicon-16.png',16],['favicon-32.png',32],['favicon-48.png',48],['apple-touch-icon.png',180],['icon-192.png',192],['icon-512.png',512]]) {
    const meta=await sharp('dist/'+name).metadata();expect(meta.width).toBe(size);expect(meta.height).toBe(size);
    // Link-preview clients must not fill transparent artwork with their own accent color.
    expect((await sharp('dist/'+name).stats()).isOpaque,name).toBe(true);
    const corner=await sharp('dist/'+name).extract({left:0,top:0,width:1,height:1}).removeAlpha().raw().toBuffer();
    expect([...corner],name).toEqual([255,255,255]);
  }
  const ico=readFileSync('dist/favicon.ico');expect(ico.readUInt16LE(2)).toBe(1);expect(ico.readUInt32LE(18)).toBe(22);
  expect(ico.subarray(22)).toEqual(readFileSync('dist/favicon-32.png'));
  expect(ico.readUInt16LE(12)).toBe((await sharp(ico.subarray(22)).metadata()).channels*8);
  const manifest=JSON.parse(read('site.webmanifest'));expect(manifest.icons).toHaveLength(2);
  for(const icon of manifest.icons) expect(readFileSync('dist'+icon.src).length).toBeGreaterThan(0);
});
test('docs and catalog previews use a separate opaque social image with room around the logo',async()=>{
  for(const html of [read('index.html'),renderPage({items}),renderPage({items,item:items[0]})]) {
    const dom=new JSDOM(html);
    try {
      const d=dom.window.document;
      expect(d.querySelector('meta[property="og:image"]').content).toBe(origin+'/assets/social-preview.png');
      expect(d.querySelector('meta[property="og:image:type"]').content).toBe('image/png');
      expect(d.querySelector('meta[property="og:image:width"]').content).toBe('1200');
      expect(d.querySelector('meta[property="og:image:height"]').content).toBe('630');
      expect(d.querySelector('meta[name="twitter:image"]').content).toBe(origin+'/assets/social-preview.png');
      expect(d.querySelector('meta[name="twitter:card"]').content).toBe('summary_large_image');
    } finally {dom.window.close();}
  }
  const image=sharp('dist/assets/social-preview.png');
  expect(await image.metadata()).toMatchObject({width:1200,height:630});
  expect((await image.stats()).isOpaque).toBe(true);
  const corner=await image.clone().extract({left:0,top:0,width:1,height:1}).removeAlpha().raw().toBuffer();
  expect([...corner]).toEqual([255,255,255]);
  const {info}=await image.clone().trim().toBuffer({resolveWithObject:true});
  expect(info.width).toBeGreaterThan(300);expect(info.width).toBeLessThanOrEqual(480);
  expect(info.height).toBeGreaterThan(300);expect(info.height).toBeLessThanOrEqual(480);
});
test('catalog SSR supplies item-specific metadata and never turns metadata into executable markup',()=>{
  const item={...items[0],repository:'Example/GitHub-Tools',owner:'Example',name:'Quoted "name"',description:'Text <tag> & punctuation'};
  const dom=new JSDOM(renderPage({items:[item],item}));
  try {
    const d=dom.window.document;
    expect(d.querySelector('meta[name="description"]').content).toBe(item.description);
    expect(d.querySelector('link[rel="canonical"]').href).toBe(origin+'/extensions/'+item.id);
    const schema=JSON.parse(d.querySelector('script[type="application/ld+json"]').textContent);
    expect(schema.description).toBe(item.description);
    expect(schema.publisher.url).toBe('https://blockether.com/');
    expect(schema.mainEntity).toMatchObject({'@type':'SoftwareSourceCode',name:item.repository.toLowerCase(),codeRepository:item.repository_url,version:item.version,programmingLanguage:'Python'});
    expect(d.title).toBe(item.repository.toLowerCase()+' · Vis · Blockether');
    expect(d.querySelectorAll('head script')).toHaveLength(1);
    expect(d.querySelector('link[rel="icon"][sizes="48x48"]').getAttribute('href')).toBe('/favicon-48.png');
  } finally {dom.window.close();}
  expect(catalogMetadata({error:'Unavailable'})).toContain('noindex, follow');
  expect(catalogMetadata({detailError:true})).toContain('noindex, follow');
  expect(catalogMetadata({search:'?q=example'})).toContain('href="'+origin+'/extensions/"');
  expect(xmlLocations(sitemap(['/extensions/']))).toEqual([origin+'/extensions/']);
});
