import { afterEach,beforeEach,expect,test,vi } from 'vitest';
import { JSDOM } from 'jsdom';
import { communityHTML,mountCommunity } from './community.js';
let dom,container,dispose,check;
const empty={votes:{up:0,down:0,score:0,own:0},comments:[],next:null};
const response=(body,status=200)=>Promise.resolve({ok:status<400,json:async()=>body});
const tick=async()=>{for(let n=0;n<5;n++) await new Promise(resolve=>setTimeout(resolve,0));};
beforeEach(()=>{
  dom=new JSDOM('<main>'+communityHTML()+'</main>',{url:'https://center.example.com/extensions/'+'1'.repeat(24)});
  globalThis.window=dom.window;globalThis.document=dom.window.document;container=document.querySelector('.community');
  window.HTMLDialogElement.prototype.showModal=function(){this.open=true;};window.HTMLDialogElement.prototype.close=function(){this.open=false;this.dispatchEvent(new window.Event('close'));};
  window.turnstile={render:vi.fn((_,options)=>{check=options;return 'widget';}),remove:vi.fn()};
});
afterEach(()=>{dispose?.();dispose=null;dom.window.close();delete globalThis.window;delete globalThis.document;});
const confirm=async()=>{await tick();check.callback('fixture-token');container.querySelector('[data-feedback-confirm]').click();await tick();};
test('empty/loading states and retry preserve a comment draft',async()=>{
  const request=vi.fn().mockImplementationOnce(()=>response({error:'Temporarily unavailable'},503)).mockImplementation(()=>response(empty));
  dispose=mountCommunity(container,'1'.repeat(24),'site-key',request);expect(container.textContent).toContain('Loading feedback');await tick();
  expect(container.textContent).toContain('Temporarily unavailable');container.querySelector('[name=body]').value='Draft';container.querySelector('[data-feedback-retry]').click();await tick();
  expect(container.textContent).toContain('No comments yet');expect(container.querySelector('[name=body]').value).toBe('Draft');
});
test('votes require explicit confirmation and can clear the existing choice',async()=>{
  const request=vi.fn((_,options)=>options?response({votes:{up:1,down:0,score:1,own:1}}):response(empty));dispose=mountCommunity(container,'1'.repeat(24),'site-key',request);await tick();
  container.querySelector('[data-vote="1"]').click();await tick();expect(check.action).toBe('extension-vote');expect(request).toHaveBeenCalledTimes(1);
  await confirm();expect(JSON.parse(request.mock.calls[1][1].body)).toMatchObject({value:1,turnstile_token:'fixture-token'});
  expect(container.querySelector('[data-vote="1"]').getAttribute('aria-pressed')).toBe('true');
  container.querySelector('[data-vote="1"]').click();await confirm();expect(JSON.parse(request.mock.calls[2][1].body).value).toBe(0);
});
test('cancelled and expired checks never mutate, and errors retain drafts',async()=>{
  const request=vi.fn((_,options)=>options?response({error:'Try again later'},429):response(empty));dispose=mountCommunity(container,'1'.repeat(24),'site-key',request);await tick();
  const form=container.querySelector('form');form.elements.name.value='Reader';form.elements.body.value='Draft';form.requestSubmit();await tick();
  check.callback('old');check['expired-callback']();container.querySelector('[data-feedback-confirm]').click();await tick();expect(request).toHaveBeenCalledTimes(1);
  container.querySelector('[data-feedback-close]').click();await tick();expect(form.elements.body.value).toBe('Draft');
  form.requestSubmit();await confirm();expect(container.textContent).toContain('Try again later');expect(form.elements.body.value).toBe('Draft');
});
test('comments are escaped, pending submissions are acknowledged but not made public',async()=>{
  const data={...empty,comments:[{id:7,name:'<b>Reader</b>',body:'<img src=x onerror=untrusted()>',created_at:'2026-01-01',votes:empty.votes}]};
  const request=vi.fn((_,options)=>options?response({id:8,status:'pending'},202):response(data));dispose=mountCommunity(container,'1'.repeat(24),'site-key',request);await tick();
  expect(container.querySelector('.feedback-comment img,.feedback-comment b')).toBeNull();
  const form=container.querySelector('form');form.elements.name.value='Reader';form.elements.body.value='New comment';form.requestSubmit();await tick();expect(check.action).toBe('extension-comment');await confirm();
  expect(container.textContent).toContain('Submitted for moderation. Reference: 8');expect(form.elements.body.value).toBe('');expect(container.querySelectorAll('.feedback-comment')).toHaveLength(1);
});
test('older comment votes use their own route without losing pagination or draft',async()=>{
  const comment={id:7,name:'Reader',body:'Useful',created_at:'2026-01-01',votes:empty.votes};
  const request=vi.fn((url,options)=>options?response({votes:{up:1,down:0,score:1,own:1}}):url.includes('?')?response({...empty,comments:[{...comment,id:6}]}):response({...empty,comments:[comment],next:7}));
  dispose=mountCommunity(container,'1'.repeat(24),'site-key',request);await tick();container.querySelector('[data-more]').click();await tick();container.querySelector('[name=body]').value='Draft';
  container.querySelector('[data-target="6"][data-vote="1"]').click();await tick();expect(check.action).toBe('comment-vote');await confirm();
  expect(request.mock.calls.at(-1)[0]).toContain('/comments/6/vote');expect(container.querySelectorAll('.feedback-comment')).toHaveLength(2);expect(container.querySelector('[name=body]').value).toBe('Draft');
});
test('disposing a view prevents late loads and confirmations from changing another view',async()=>{
  let resolve;const request=vi.fn(()=>new Promise(done=>{resolve=done;}));dispose=mountCommunity(container,'1'.repeat(24),'site-key',request);dispose();dispose=null;container.innerHTML='<p>New view</p>';resolve({ok:true,json:async()=>empty});await tick();expect(container.textContent).toBe('New view');
});
