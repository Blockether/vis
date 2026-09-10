import { afterAll, beforeAll, beforeEach, expect, test } from 'vitest';
import { readFileSync } from 'node:fs';
import worker from './worker.js';
import { runtimeFixture } from './test-support.js';
import { moderationStatements } from './moderate.mjs';
import { identity, manifestMetadata, projectFolder, repositoryURL } from './github.js';

let fixture, serial=0;
beforeAll(async()=>{fixture=await runtimeFixture();});
afterAll(async()=>{await fixture?.runtime.dispose();});
beforeEach(async()=>{fixture.controls.github='ok';fixture.controls.verification='ok';fixture.controls.requests=[];fixture.controls.tokens.clear();await fixture.db.batch(['DELETE FROM submissions','DELETE FROM extensions'].map(sql=>fixture.db.prepare(sql)));await fixture.runtime.purgeCache();});
function post(path,source={},headers={}) {
  const token=(path==='/api/preview'?'preview':'submit')+'-'+(++serial);
  return fixture.runtime.dispatchFetch('https://center.example.com'+path,{method:'POST',headers:{Origin:'https://center.example.com','Content-Type':'application/json','CF-Connecting-IP':'10.0.0.'+(serial%250+1),...headers},body:JSON.stringify({repository_url:'https://github.com/example/extensions',subdirectory:'plugins/greeting',turnstile_token:token,...source})});
}

test('docs and catalog share an origin, with documentation independent of D1',async()=>{
  const home=await fixture.runtime.dispatchFetch('https://center.example.com/');
  const homeHTML=await home.text();
  expect(home.status).toBe(200);
  expect(homeHTML).toContain('Getting started');
  expect(homeHTML).not.toContain('id="catalog-data"');
  expect(homeHTML).toContain('class="center-link" href="/extensions/"');
  const docs=await fixture.runtime.dispatchFetch('https://center.example.com/extending.html');
  expect(docs.status).toBe(200);expect(await docs.text()).toContain('Extending Vis');
  const catalog=await fixture.runtime.dispatchFetch('https://center.example.com/extensions/');
  const catalogHTML=await catalog.text();
  expect(catalog.status).toBe(200);expect(catalogHTML).toContain('No repositories yet');
  expect(catalogHTML).toContain('class="brand" href="/"');
  expect(catalogHTML).toContain('href="/extending.html"');
  expect(catalogHTML).not.toContain('blockether.github.io');
  expect((await fixture.runtime.dispatchFetch('https://center.example.com/missing.html')).status).toBe(404);
  const offline=await worker.fetch(new Request('https://center.example.com/'),{ASSETS:{fetch:()=>new Response('docs without database')}},{waitUntil(){}});
  expect(await offline.text()).toBe('docs without database');
});
test('catalog canonicalization preserves filters and never redirects to another origin',async()=>{
  const response=await fixture.runtime.dispatchFetch('https://center.example.com/extensions?category=tools',{redirect:'manual'});
  expect(response.status).toBe(308);expect(response.headers.get('location')).toBe('/extensions/?category=tools');
});
test('Worker returns Vis light HTML rather than a JSON-only API',async()=>{
  const response=await fixture.runtime.dispatchFetch('https://center.example.com/extensions/');
  const html=await response.text();expect(response.status).toBe(200);expect(response.headers.get('content-type')).toContain('text/html');
  expect(html).toContain('No repositories yet');expect(html).toContain('/assets/theme.css');expect(html).toContain('class="shell"');
  expect(html).not.toContain('server-only');expect(response.headers.get('content-security-policy')).toContain("frame-ancestors 'none'");
});
test('catalog failure still renders the documentation shell with recovery',async()=>{
  const response=await worker.fetch(new Request('https://center.example.com/extensions/'),{},{waitUntil(){}});
  expect(response.status).toBe(503);expect(await response.text()).toContain('Could not load the catalog');
});
test('root and monorepo previews use pinned GitHub metadata only',async()=>{
  for(const subdirectory of ['', 'plugins/greeting']) {const response=await post('/api/preview',{subdirectory});const data=await response.json();expect(response.status,JSON.stringify(data)).toBe(200);expect(data.subdirectory).toBe(subdirectory);expect(data.revision).toBe(fixture.revision);expect(data.name).toBe('vis-greeter');expect(data.manifest_url).toContain('/blob/'+fixture.revision+'/');}
  expect(fixture.controls.requests.filter(url=>url.includes('/contents')).every(url=>url.endsWith('?ref='+fixture.revision))).toBe(true);
});
test('pending submissions and their refreshes cannot publish or replace a public listing',async()=>{
  const source={revision:fixture.revision};const response=await post('/api/submissions',source);expect(response.status).toBe(202);const pending=await response.json();
  expect((await post('/api/submissions',source)).status).toBe(202);
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM submissions').first()).n).toBe(1);
  expect((await (await fixture.runtime.dispatchFetch('https://center.example.com/api/extensions')).json()).extensions).toEqual([]);
  const id=await identity('https://github.com/example/extensions\nplugins/greeting');
  expect((await fixture.runtime.dispatchFetch('https://center.example.com/extensions/'+id)).status).toBe(404);
  for(const sql of moderationStatements('approve',pending.id)) await fixture.db.prepare(sql).run();
  await fixture.runtime.purgeCache();
  const listing=await (await fixture.runtime.dispatchFetch('https://center.example.com/api/extensions/'+id)).json();
  expect(listing.name).toBe('vis-greeter');const added=listing.added_at;
  await post('/api/submissions',source);expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM extensions').first()).n).toBe(1);
  expect((await fixture.db.prepare('SELECT added_at FROM extensions').first()).added_at).toBe(added);
  const html=await (await fixture.runtime.dispatchFetch('https://center.example.com/extensions/'+id)).text();
  expect(html).toContain('vis-greeter');expect(html).toContain('id="install-command"');expect(html).toContain('--subdirectory');
});
test('SSR supports search, categories, sort, views and executable-free metadata',async()=>{
  const items=JSON.parse(readFileSync('web/catalog.fixture.json','utf8'));
  for(const item of items) await fixture.db.prepare('INSERT INTO extensions VALUES (?, ?, ?)').bind(item.id,JSON.stringify(item),item.added_at).run();
  const html=await (await fixture.runtime.dispatchFetch('https://center.example.com/extensions/?category=providers&view=list&q=local')).text();
  expect(html).toContain('data-view="list"');expect(html.match(/class="extension-card"/g)).toHaveLength(1);expect(html).toContain('data-name="vis-local-models"');
  const malicious={...items[0],description:'</script><b>untrusted metadata</b>'};
  await fixture.db.prepare('UPDATE extensions SET metadata=? WHERE id=?').bind(JSON.stringify(malicious),malicious.id).run();await fixture.runtime.purgeCache();
  const detail=await (await fixture.runtime.dispatchFetch('https://center.example.com/extensions/'+malicious.id)).text();
  expect(detail).not.toContain(malicious.description);expect(detail).toContain('&lt;/script&gt;');
  const data=JSON.parse(detail.match(/id="catalog-data" type="application\/json">(.*?)<\/script>/s)[1]);expect(data.item.description).toBe(malicious.description);
});
test('public catalog uses a shared cache key and HEAD returns no body',async()=>{
  const response=await fixture.runtime.dispatchFetch('https://center.example.com/api/extensions');expect(response.headers.get('cache-control')).toContain('max-age=60');
  const item=JSON.parse(readFileSync('web/catalog.fixture.json','utf8'))[0];await fixture.db.prepare('INSERT INTO extensions VALUES (?, ?, ?)').bind(item.id,JSON.stringify(item),item.added_at).run();
  const html=await (await fixture.runtime.dispatchFetch('https://center.example.com/extensions/?q=github')).text();expect(html).toContain('No repositories yet');
  const head=await fixture.runtime.dispatchFetch('https://center.example.com/extensions/',{method:'HEAD'});expect(await head.text()).toBe('');expect(head.status).toBe(200);
});
test('Turnstile rejects missing, reused, wrong-action and wrong-host tokens before GitHub',async()=>{
  expect((await post('/api/preview',{turnstile_token:''})).status).toBe(403);
  expect((await post('/api/preview',{turnstile_token:'submit-wrong-action'})).status).toBe(403);
  fixture.controls.verification='hostname';expect((await post('/api/preview')).status).toBe(403);
  fixture.controls.verification='ok';expect((await post('/api/preview',{turnstile_token:'preview-once'})).status).toBe(200);
  fixture.controls.requests=[];expect((await post('/api/preview',{turnstile_token:'preview-once'})).status).toBe(403);
  expect(fixture.controls.requests.some(url=>url.includes('api.github.com'))).toBe(false);
});
test('writes reject cross-origin requests, unsupported input and unpinned confirmations',async()=>{
  expect((await post('/api/preview',{}, {Origin:'https://other.example.com'})).status).toBe(403);
  expect((await post('/api/preview',{}, {'Content-Type':'text/plain'})).status).toBe(415);
  expect((await post('/api/preview',{extra:'x'.repeat(5000)})).status).toBe(413);
  expect((await post('/api/preview',{metadata:{name:'injected'}})).status).toBe(400);
  expect((await post('/api/submissions')).status).toBe(400);
  expect((await post('/api/extensions')).status).toBe(405);
});
test('per-IP rate limit prevents excessive inspection requests',async()=>{
  const statuses=[];for(let n=0;n<11;n++) statuses.push((await post('/api/preview',{turnstile_token:''},{'CF-Connecting-IP':'10.1.0.5'})).status);
  expect(statuses.slice(0,10)).toEqual(Array(10).fill(403));expect(statuses[10]).toBe(429);
});
test.each(['redirect','private','missing','rate'])('GitHub %s is actionable and never creates a submission',async mode=>{
  fixture.controls.github=mode;const response=await post('/api/preview');expect(response.status).toBe(mode==='rate'?503:400);expect((await response.json()).error).toBeTruthy();expect(fixture.controls.requests.every(url=>!url.includes('other.example.com'))).toBe(true);
});
test('portable display metadata agrees with the shipped manifest and rejects unsafe paths',()=>{
  const metadata=manifestMetadata(readFileSync('examples/vis-greeter/pyproject.toml','utf8'));expect(metadata.category).toBe('tools');expect(metadata.dependencies.some(d=>d.startsWith('vis-agent'))).toBe(true);
  for(const path of ['../secret','/tmp','x/../y','.env','x\\y']) expect(()=>projectFolder(path)).toThrow();
  for(const url of ['http://github.com/a/b','https://example.com/a/b','https://github.com/a/b/tree/main','https://github.com:443/a/b']) expect(()=>repositoryURL(url)).toThrow();
  expect(repositoryURL('https://github.com/example/repo.git/')).toBe('https://github.com/example/repo');
  expect(()=>moderationStatements('approve',"'; DELETE FROM extensions;")).toThrow();
 });
test('live discovery includes only approved listings and shares the cached catalog snapshot',async()=>{
  const items=JSON.parse(readFileSync('web/catalog.fixture.json','utf8'));
  const item=items[0];
  await fixture.db.prepare('INSERT INTO extensions VALUES (?, ?, ?)').bind(item.id,JSON.stringify(item),item.added_at).run();
  await fixture.db.prepare('INSERT INTO submissions VALUES (?, ?, ?, ?, ?)').bind('b'.repeat(24),items[1].id,items[1].revision,JSON.stringify(items[1]),items[1].added_at).run();
  for(const path of ['/extensions/sitemap.xml','/extensions/llms.txt']) {
    const response=await fixture.runtime.dispatchFetch('https://center.example.com'+path);
    expect(response.status).toBe(200);
    expect(response.headers.get('content-type')).toContain(path.endsWith('.xml')?'application/xml':'text/plain');
    expect(response.headers.get('cache-control')).toBe('public, max-age=60');
    const text=await response.text();expect(text).toContain('/extensions/'+item.id);expect(text).not.toContain(items[1].id);
    const head=await fixture.runtime.dispatchFetch('https://center.example.com'+path,{method:'HEAD'});
    expect(head.status).toBe(200);expect(await head.text()).toBe('');
  }
  const api=await fixture.runtime.dispatchFetch('https://center.example.com/api/extensions');
  expect(api.headers.get('x-robots-tag')).toBe('noindex');
  const missing=await fixture.runtime.dispatchFetch('https://center.example.com/extensions/'+'f'.repeat(24));
  expect(missing.status).toBe(404);expect(missing.headers.get('x-robots-tag')).toBe('noindex');
});
test('discovery fails explicitly rather than publishing an empty catalog when D1 is unavailable',async()=>{
  for(const path of ['/extensions/sitemap.xml','/extensions/llms.txt']) {
    const response=await worker.fetch(new Request('https://center.example.com'+path),{},{waitUntil(){}});
    expect(response.status).toBe(503);expect(response.headers.get('cache-control')).toBe('no-store');
  }
});
