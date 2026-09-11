import { expect, test } from 'vitest';
import { readFileSync, readdirSync, existsSync } from 'node:fs';
import { JSDOM } from 'jsdom';
import { security } from './headers.js';
import { createTestHarness } from 'wrangler';

const htmlFiles=readdirSync('dist').filter(file=>file.endsWith('.html'));
test('the canonical renderer builds documentation, with one exact CSS and no inline executables',()=>{
  expect(htmlFiles.length).toBeGreaterThan(10);
  expect(readFileSync('dist/assets/theme.css').equals(readFileSync('../../resources/vis-docs/assets/theme.css'))).toBe(true);
  expect(readFileSync('dist/assets/prism.min.js').equals(readFileSync('../../resources/vis-transcript/prism.min.js'))).toBe(true);
  expect(readFileSync('dist/assets/docs.js','utf8')).toContain('Prism.highlightAll()');
  for(const file of htmlFiles) {
    const dom=new JSDOM(readFileSync('dist/'+file,'utf8'),{url:'https://gateway.example.com/'+file});
    try {
      const document=dom.window.document;
      expect(document.querySelector('style, script:not([src]):not([type="application/ld+json"]), [onclick]'),file).toBeNull();
      expect(document.querySelector('.top .center-link').getAttribute('href'),file).toBe('/extensions/');
      for(const node of document.querySelectorAll('a[href], link[href], script[src], img[src]')) {
        const url=new URL(node.getAttribute('href')||node.getAttribute('src'),dom.window.location.href);
        if(url.origin!=='https://gateway.example.com'||url.pathname.startsWith('/extensions/')) continue;
        expect(existsSync('dist'+(url.pathname==='/'?'/index.html':url.pathname)),file+': '+url.pathname).toBe(true);
      }
    } finally {dom.window.close();}
  }
});
test('docs shortcuts and catalog buttons use one shared control and font contract',()=>{
  const dom=new JSDOM('<style>'+readFileSync('dist/assets/theme.css','utf8')+'</style><style>'+readFileSync('dist/assets/catalog.css','utf8')+'</style>');
  try {
    const rules=[...dom.window.document.styleSheets[0].cssRules];
    const control=rules.find(rule=>rule.selectorText?.split(',').map(s=>s.trim()).includes('button')&&rule.selectorText.includes('.quick-links a'));
    expect(control).toBeDefined();
    expect(control.style.getPropertyValue('font-size')).toBe('var(--text-small)');
    expect(control.style.getPropertyValue('font-weight')).toBe('500');
    expect(control.style.getPropertyValue('background')).toBe('var(--bg-soft)');
    const primary=rules.find(rule=>rule.selectorText?.includes('button.primary')&&rule.selectorText.includes('.store-links a'));
    expect(primary.style.getPropertyValue('background')).toBe('var(--fg)');
    expect(readFileSync('dist/assets/fonts/jetbrains-mono.woff2')).toEqual(readFileSync('../../resources/vis-docs/assets/fonts/jetbrains-mono.woff2'));
    expect(readFileSync('dist/assets/catalog.css','utf8')).not.toMatch(/font-family|@font-face|button\.primary/);
    const touch=rules.find(rule=>/pointer:\s*coarse/.test(rule.conditionText));
    expect([...touch.cssRules].find(rule=>rule.selectorText.includes('input')).style.getPropertyValue('min-height')).toBe('2.75rem');
    const input=[...dom.window.document.styleSheets[1].cssRules].find(rule=>rule.selectorText?.startsWith('input:not'));
    expect(input.style.getPropertyValue('min-height')).toBe('');
  } finally {dom.window.close();}
});
test('the static upload contains only public output and the same security policy',()=>{
  const output=readdirSync('dist',{recursive:true});
  expect(output.some(file=>/(^|\/)(?:[^/]*fixture[^/]*|schema\.sql|wrangler[^/]*|\.?deployment[^/]*|package(?:-lock)?\.json|node_modules|\.env[^/]*|\.vars[^/]*)$/.test(file))).toBe(false);
  const headers=readFileSync('dist/_headers','utf8');
  for(const [key,value] of Object.entries(security)) expect(headers).toContain(`${key}: ${value}`);
  const config=JSON.parse(readFileSync('wrangler.jsonc','utf8'));
  expect(config.assets.run_worker_first).toEqual(['/extensions','/extensions/*','/api/*']);
  expect(config.assets.html_handling).toBe('none');
  expect(config.d1_databases[0].database_name).toBe('vis-extension-center');
});
test('the canonical stylesheet can load its embedded store icons without allowing inline scripts',()=>{
  expect(readFileSync('../../resources/vis-docs/assets/theme.css','utf8')).toContain('data:image/svg+xml;base64,');
  const policy=security['Content-Security-Policy'];
  expect(policy.match(/img-src([^;]*)/)[1]).toContain('data:');
  for(const directive of ['script-src','style-src']) {
    const sources=policy.match(new RegExp(directive+'([^;]*)'))[1];
    expect(sources).not.toContain('data:');expect(sources).not.toContain('unsafe-inline');
  }
});
test('Wrangler serves the home page, HTML paths and assets with production routing and headers',async()=>{
  const site=createTestHarness({workers:[{configPath:'./wrangler.jsonc'}]});
  try {
    await site.listen();
    for(const path of ['/','/index.html','/extending.html','/assets/theme.css','/assets/docs.js','/assets/prism.min.js','/assets/fonts/jetbrains-mono.woff2','/favicon.ico','/favicon-32.png','/favicon-48.png','/apple-touch-icon.png','/site.webmanifest','/robots.txt','/sitemap.xml','/sitemap-docs.xml','/llms.txt','/llms-full.txt','/extending.md']) {
      const response=await site.fetch(path,{redirect:'manual'});
      expect(response.status,path).toBe(200);
      expect(response.headers.get('Content-Security-Policy'),path).toBe(security['Content-Security-Policy']);
      if(path==='/') expect(await response.text()).toBe(readFileSync('dist/index.html','utf8'));
      if(path.endsWith('.xml')) expect(response.headers.get('content-type')).toMatch(/(?:application|text)\/xml/);
      if(path.endsWith('.txt')) expect(response.headers.get('content-type')).toContain('text/plain');
    }
    const head=await site.fetch('/',{method:'HEAD'});expect(head.status).toBe(200);expect(await head.text()).toBe('');
    expect((await site.fetch('/missing.html')).status).toBe(404);
    // The isolated harness has no schema: catalog failure must not break static docs.
    const catalog=await site.fetch('/extensions/');expect(catalog.status).toBe(503);
    expect(await catalog.text()).toContain('id="catalog-data"');
    expect((await site.fetch('/')).status).toBe(200);
  } finally {await site.close();}
});
