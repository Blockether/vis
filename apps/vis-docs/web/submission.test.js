// Node hosts workerd; JSDOM supplies only the browser globals used by the controller.
// The real browser controller talks to workerd and D1; only external HTTP and the widget are fixtures.
import { afterAll, afterEach, beforeAll, beforeEach, expect, test, vi } from 'vitest';
import { JSDOM } from 'jsdom';
import { mount, installCommand } from './app.js';
import { runtimeFixture } from '../test-support.js';
import { moderationStatements } from '../moderate.mjs';

let fixture, dom, dispose, serial=0,address=0;
const request=(path,options={})=>fixture.runtime.dispatchFetch('https://center.example.com'+path,{...options,headers:{...options.headers,Origin:'https://center.example.com','CF-Connecting-IP':'10.9.0.'+address}});
const $=selector=>document.querySelector(selector);
beforeAll(async()=>{fixture=await runtimeFixture();});
afterAll(async()=>{await fixture?.runtime.dispose();});
beforeEach(async()=>{
  address++;fixture.controls.github='ok';fixture.controls.tokens.clear();fixture.controls.requests=[];
  await fixture.db.batch(['DELETE FROM submissions','DELETE FROM releases','DELETE FROM extensions'].map(sql=>fixture.db.prepare(sql)));
  await fixture.runtime.purgeCache();
  dom=new JSDOM('<div id="app"></div>',{url:'https://center.example.com/extensions/'});
  vi.stubGlobal('window',dom.window);vi.stubGlobal('document',dom.window.document);vi.stubGlobal('navigator',dom.window.navigator);
  window.matchMedia=()=>({matches:true,addEventListener(){},removeEventListener(){}});
  vi.spyOn(window,'scrollTo').mockImplementation(()=>{});
  window.HTMLDialogElement.prototype.showModal=function(){this.open=true;};
  window.HTMLDialogElement.prototype.close=function(){this.open=false;this.dispatchEvent(new window.Event('close'));};
  window.turnstile={render:(_node,options)=>{options.callback((options.action==='extension-preview'?'preview':options.action==='extension-submit'?'submit':options.action)+'-'+(++serial));return 'fixture';},remove(){}};
});
afterEach(()=>{dispose?.();dom?.window.close();vi.restoreAllMocks();vi.unstubAllGlobals();});
async function start() {
  const initial={items:[],siteKey:'fixture-site-key'};
  dispose=mount($('#app'),request,initial);
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
  dispose=mount($('#app'),request,{items:[listing],siteKey:'fixture-site-key'});
  $('.card-main').click();
  await vi.waitFor(()=>expect($('#install-command')).not.toBeNull());
  expect($('#install-command').textContent).toBe(installCommand(listing));
  expect($('#install-command').textContent).toContain("--version '1.0.0'");
  expect($('#source-link').href).toContain(fixture.revision);
  expect($('#source-link').href).toBe(listing.source_url);
  await vi.waitFor(()=>expect($('[data-rating] button')).not.toBeNull());
  const confirmFeedback=async()=>{await vi.waitFor(()=>expect($('[data-check-status]').textContent).toContain('then confirm'));$('[data-feedback-confirm]').click();};
  $('[data-rating] [data-vote="1"]').click();await confirmFeedback();
  await vi.waitFor(()=>expect($('[data-feedback-status]').textContent).toBe('Vote saved.'));
  expect($('[data-rating] [data-vote="1"]').getAttribute('aria-pressed')).toBe('true');
  const commentForm=$('[data-comment-form]');commentForm.elements.name.value='Local reviewer';commentForm.elements.body.value='Clear setup instructions.';commentForm.requestSubmit();await confirmFeedback();
  await vi.waitFor(()=>expect($('[data-feedback-status]').textContent).toContain('Submitted for moderation.'));
  expect($('.feedback-comment')).toBeNull();
  const comment=await fixture.db.prepare('SELECT id FROM comments WHERE extension_id=?').bind(listing.id).first();
  for(const sql of moderationStatements('approve-comment',String(comment.id))) await fixture.db.prepare(sql).run();
  dispose();dispose=mount($('#app'),request,{items:[listing],item:listing,siteKey:'fixture-site-key'});
  await vi.waitFor(()=>expect($('.feedback-comment')).not.toBeNull());
  $('.feedback-comment [data-vote="1"]').click();await confirmFeedback();
  await vi.waitFor(()=>expect($('.feedback-comment [data-vote="1"]').getAttribute('aria-pressed')).toBe('true'));
  expect((await fixture.db.prepare('SELECT COUNT(*) AS n FROM comment_votes WHERE comment_id=?').bind(comment.id).first()).n).toBe(1);
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
