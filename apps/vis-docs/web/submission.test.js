// Node hosts workerd; JSDOM supplies only the browser globals used by the controller.
// The real browser controller talks to workerd and D1; only external HTTP and the widget are fixtures.
import { afterAll, afterEach, beforeAll, beforeEach, expect, test, vi } from 'vitest';
import { JSDOM } from 'jsdom';
import { mount, installCommand } from './app.js';
import { runtimeFixture } from '../test-support.js';
import { moderationStatements } from '../moderate.mjs';

let fixture, dom, dispose, serial=0;
const $=selector=>document.querySelector(selector);
beforeAll(async()=>{fixture=await runtimeFixture();});
afterAll(async()=>{await fixture?.runtime.dispose();});
beforeEach(async()=>{
  fixture.controls.github='ok';fixture.controls.tokens.clear();fixture.controls.requests=[];
  await fixture.db.batch(['DELETE FROM submissions','DELETE FROM extensions'].map(sql=>fixture.db.prepare(sql)));
  await fixture.runtime.purgeCache();
  dom=new JSDOM('<div id="app"></div>',{url:'https://center.example.com/extensions/'});
  vi.stubGlobal('window',dom.window);vi.stubGlobal('document',dom.window.document);vi.stubGlobal('navigator',dom.window.navigator);
  window.matchMedia=()=>({matches:true,addEventListener(){},removeEventListener(){}});
  vi.spyOn(window,'scrollTo').mockImplementation(()=>{});
  window.HTMLDialogElement.prototype.showModal=function(){this.open=true;};
  window.HTMLDialogElement.prototype.close=function(){this.open=false;this.dispatchEvent(new window.Event('close'));};
  window.turnstile={render:(_node,options)=>{options.callback((options.action==='extension-preview'?'preview':'submit')+'-'+(++serial));return 'fixture';},remove(){}};
});
afterEach(()=>{dispose?.();dom?.window.close();vi.restoreAllMocks();vi.unstubAllGlobals();});
async function start() {
  const initial={items:[],siteKey:'fixture-site-key'};
  dispose=mount($('#app'),(path,options={})=>fixture.runtime.dispatchFetch('https://center.example.com'+path,{...options,headers:{...options.headers,Origin:'https://center.example.com','CF-Connecting-IP':'10.0.0.'+(serial+1)}}),initial);
  $('#submit-open').click();
}
function review(subdirectory='') {
  $('[name=repository_url]').value='https://github.com/example/extensions';
  $('[name=subdirectory]').value=subdirectory;
  $('#repository-form').requestSubmit();
}

test.each(['','plugins/greeting'])('browser review, submission, approval and catalog agree for folder "%s"',async subdirectory=>{
  await start();review(subdirectory);
  await vi.waitFor(()=>expect($('#submit-confirm').hidden).toBe(false));
  expect($('#preview').textContent).toContain('Repository checks passed');
  expect($('#preview').textContent).toContain(fixture.revision);
  expect($('#preview a').href).toContain('/tree/'+fixture.revision);
  $('#submit-confirm').click();
  await vi.waitFor(()=>expect($('#submit-dialog').open).toBe(false));
  const pending=await fixture.db.prepare('SELECT * FROM submissions').first();
  expect($('#notice').textContent).toContain(pending.id);
  expect(pending.revision).toBe(fixture.revision);
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM extensions').first()).n).toBe(0);
  expect((await fixture.runtime.dispatchFetch('https://center.example.com/extensions/'+pending.extension_id)).status).toBe(404);
  for(const sql of moderationStatements('approve',pending.id)) await fixture.db.prepare(sql).run();
  await fixture.runtime.purgeCache();
  const listing=await (await fixture.runtime.dispatchFetch('https://center.example.com/api/extensions/'+pending.extension_id)).json();
  expect(listing.subdirectory).toBe(subdirectory);expect(listing.revision).toBe(fixture.revision);
  dispose();
  dispose=mount($('#app'),undefined,{items:[listing]});
  $('.card-main').click();
  await vi.waitFor(()=>expect($('#install-command')).not.toBeNull());
  expect($('#install-command').textContent).toBe(installCommand(listing));
  expect($('#install-command').textContent).toContain(fixture.revision);
  expect($('#source-link').href).toBe(listing.source_url);
  $('#back-to-catalog').click();expect($('#catalog-page').hidden).toBe(false);
});
test('a failed repository check stays editable, retries, and a rejected submission never becomes public',async()=>{
  await start();fixture.controls.github='missing';review();
  await vi.waitFor(()=>expect($('#submit-status').textContent).toContain('extension.py'));
  expect($('#submit-confirm').hidden).toBe(true);expect($('#repository-form').hidden).toBe(false);
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM submissions').first()).n).toBe(0);
  fixture.controls.github='ok';review();
  await vi.waitFor(()=>expect($('#submit-confirm').hidden).toBe(false));
  $('#edit-submission').click();expect($('#preview').textContent).toBe('');
  review('plugins/greeting');await vi.waitFor(()=>expect($('#submit-confirm').hidden).toBe(false));
  $('#submit-confirm').click();await vi.waitFor(()=>expect($('#submit-dialog').open).toBe(false));
  const pending=await fixture.db.prepare('SELECT id FROM submissions').first();
  for(const sql of moderationStatements('reject',pending.id)) await fixture.db.prepare(sql).run();
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM submissions').first()).n).toBe(0);
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM extensions').first()).n).toBe(0);
});
