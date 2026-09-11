// @vitest-environment jsdom
import { afterEach, expect, test, vi } from 'vitest';
import { mount, installCommand } from './app.js';
import fixtures from './catalog.fixture.json';
import { readFileSync } from 'node:fs';
const item = fixtures.at(-1);
const tick = async () => { for (let i=0; i<20; i++) await Promise.resolve(); };
const $ = selector => document.querySelector(selector);
let dispose;
afterEach(() => { dispose?.(); document.body.replaceChildren(); vi.restoreAllMocks(); vi.unstubAllGlobals(); window.history.replaceState(null, '', '/'); });
function setup(request = vi.fn(async path => ({ok:true, json:async()=>path === '/api/extensions' ? {extensions:fixtures} : item})), touch=false) {
  document.body.innerHTML = '<div id="app"></div>';
  vi.spyOn(window,'scrollTo').mockImplementation(()=>{});
  const media={matches:touch,addEventListener:vi.fn(),removeEventListener:vi.fn()};
  vi.stubGlobal('matchMedia',()=>media);
  window.HTMLElement.prototype.scrollIntoView=vi.fn();
  window.HTMLDialogElement.prototype.showModal = function(){this.open=true;};
  window.HTMLDialogElement.prototype.close = function(){this.open=false;this.dispatchEvent(new window.Event('close'));};
  window.turnstile={render:vi.fn((node,options)=>{options.callback('fixture-'+options.action);return 'widget';}),remove:vi.fn()};
  dispose = mount($('#app'), request);
  $('#turnstile-widget').dataset.sitekey='test-site-key';
  return request;
}
function change(selector, value, type='input') {
  $(selector).value=value; $(selector).dispatchEvent(new window.Event(type,{bubbles:true}));
}
function names() { return [...document.querySelectorAll('[data-name]')].map(e=>e.dataset.name); }

test('catalog opens as cards, not a selected split pane, and filters by category and author', async () => {
  setup(); await tick();
  expect($('#detail-page').hidden).toBe(true);
  expect($('#results').dataset.view).toBe('grid');
  expect(names()).toHaveLength(6);
  $('[data-category="providers"]').click();
  expect(names()).toEqual(['vis-local-models']);
  $('[data-category="all"]').click();
  change('#search', 'example'); expect(names()).toHaveLength(6);
  change('#search', 'vis-agent'); expect(names()).toHaveLength(6);
  change('#search', 'absent'); expect(names()).toHaveLength(0);
  expect($('#results').textContent).toContain('No matching extensions');
  $('#clear-filters').click(); expect(names()).toHaveLength(6);
});

test('sort and view controls affect real results and persist in the URL', async () => {
  setup(); await tick();
  expect(names()[0]).toBe('vis-github');
  for (const [sort, expected] of [['updated','vis-greeter'],['newest','vis-greeter'],['name','vis-browser']]) {
    change('#sort',sort,'change'); expect(names()[0]).toBe(expected);
  }
  $('#view-list').click();
  expect($('#results').dataset.view).toBe('list');
  expect($('#view-list').getAttribute('aria-pressed')).toBe('true');
  expect(window.location.search).toContain('view=list');
});

test('detail page has GitHub source, a pinned subdirectory command and working back navigation', async () => {
  setup(); await tick();
  $(`[data-name="${item.name}"] .card-main`).click(); await tick();
  expect($('#catalog-page').hidden).toBe(true);
  expect($('#detail-page').hidden).toBe(false);
  expect($('link[rel="canonical"]').href).toBe('https://vis.blockether.com/extensions/'+item.id);
  expect($('meta[property="og:title"]').content).toBe(item.name+' by '+item.owner+' · Vis · Blockether');
  expect($('meta[name="description"]').content).toBe(item.description);
  expect($('#install-command').textContent).toBe(installCommand(item));
  expect(installCommand(item)).toContain("--subdirectory 'extensions/greeting'");
  expect(installCommand(item)).toContain("--revision '"+'a'.repeat(40)+"'");
  expect(installCommand(item)).not.toMatch(/registry|zip/i);
  expect(installCommand({...item,subdirectory:"tools/O'Reilly"})).toContain("--subdirectory 'tools/O'\\''Reilly'");
  expect($('#source-link').href).toBe(item.source_url);
  const writeText=vi.fn().mockResolvedValue();
  Object.defineProperty(navigator,'clipboard',{value:{writeText},configurable:true});
  $('#copy-command').click(); await tick();
  expect(writeText).toHaveBeenCalledWith(installCommand(item));
  $('#back-to-catalog').click(); await tick();
  expect($('#catalog-page').hidden).toBe(false);
  expect($('link[rel="canonical"]').href).toBe('https://vis.blockether.com/extensions/');
  expect(document.querySelectorAll('link[rel="canonical"]')).toHaveLength(1);
  expect($('meta[property="og:title"]').content).toBe('Extension Center · Vis · Blockether');
  expect(names()).toHaveLength(6);
});

test('untrusted metadata remains text and unsafe links are not rendered', async () => {
  const malicious={...item,description:'<img src=x onerror=alert(1)>',readme_url:'javascript:alert(1)'};
  setup(async path=>({ok:true,json:async()=>path==='/api/extensions'?{extensions:[malicious]}:malicious}));
  await tick(); $('.card-main').click(); await tick();
  expect($('#detail-page').textContent).toContain(malicious.description);
  expect($('#detail-page img')).toBeNull();
  expect(document.querySelector('a[href^="javascript:"]')).toBeNull();
});

test('loading, failure, retry and empty catalog have recovery actions', async () => {
  let fail=true;
  setup(async()=>{if(fail) throw new Error('offline');return {ok:true,json:async()=>({extensions:[]})};});
  expect(document.body.textContent).toContain('Loading extensions'); await tick();
  expect(document.body.textContent).toContain('Could not load');
  fail=false; $('#retry').click(); await tick();
  expect($('#results').textContent).toContain('No repositories yet');
  expect($('#empty-add')).toBeNull();
  expect([...document.querySelectorAll('#catalog-page button')].filter(button=>button.textContent==='Add a repository')).toHaveLength(1);
  $('#submit-open').click();
  expect($('#submit-dialog').open).toBe(true);
});

test('the repository dialog has an accessible X, locks background scrolling and restores focus', async () => {
  setup(undefined,true); await tick(); $('#submit-open').click();
  const close=$('#submit-close');
  expect(close.getAttribute('aria-label')).toBe('Close repository dialog');
  expect(close.querySelector('svg[aria-hidden="true"]')).not.toBeNull();
  expect(document.body.style.overflow).toBe('hidden');
  expect(document.activeElement).toBe(close);
  $('.dialog-body').scrollTop=200;
  close.click();
  expect($('.dialog-body').scrollTop).toBe(0);
  expect($('#submit-dialog').open).toBe(false);
  expect(document.body.style.overflow).toBe('');
  expect(document.activeElement).toBe($('#submit-open'));
});

test('submission previews a GitHub link and pins the reviewed revision when adding', async () => {
  const request=setup(); await tick(); $('#submit-open').click();
  const form=$('#repository-form');
  expect(form.querySelector('input[type=file],input[type=password]')).toBeNull();
  change('[name=repository_url]',item.repository_url);
  change('[name=subdirectory]',item.subdirectory);
  form.dispatchEvent(new window.Event('submit',{cancelable:true})); await tick();
  const [path,options]=request.mock.calls.at(-1);
  expect(path).toBe('/api/preview');
  expect(JSON.parse(options.body)).toEqual({repository_url:item.repository_url,subdirectory:item.subdirectory,turnstile_token:'fixture-extension-preview'});
  expect($('#submit-confirm').hidden).toBe(false);
  expect($('#preview').textContent).toContain('Repository checks passed');
  expect($('#preview').textContent).toContain('Public GitHub repository');
  expect($('#preview').textContent).toContain('extension.py');
  expect($('#preview').textContent).toContain('vis-agent');
  expect($('#preview a[href="'+item.manifest_url+'"]').textContent).toBe('pyproject.toml');
  expect($('#preview').textContent).toContain('not a code audit');
  expect(document.activeElement).toBe($('#submit-step'));
  $('#submit-confirm').click(); await tick();
  const submission=request.mock.calls.find(([path,opts])=>path==='/api/submissions' && opts?.method==='POST');
  expect(JSON.parse(submission[1].body).turnstile_token).toBe('fixture-extension-submit');
  expect($('#notice').textContent).toContain('moderation');
  expect(JSON.parse(submission[1].body).revision).toBe(item.revision);
  expect($('#submit-dialog').open).toBe(false);
  expect(form.elements.repository_url.value).toBe('');
});

test('editing or closing a submission invalidates late preview results', async () => {
  let resolve;
  setup(path=>path==='/api/extensions'?Promise.resolve({ok:true,json:async()=>({extensions:fixtures})}):new Promise(r=>{resolve=r;}));
  await tick(); $('#submit-open').click(); change('[name=repository_url]',item.repository_url);
  $('#repository-form').dispatchEvent(new window.Event('submit',{cancelable:true}));
  $('#submit-close').click();
  resolve({ok:true,json:async()=>item}); await tick();
  expect($('#submit-confirm').hidden).toBe(true);
  expect($('#preview').textContent).toBe('');
  $('#submit-open').click(); expect($('#review-submit').disabled).toBe(false);
});

test('keyboard search and result navigation remain available', async () => {
  setup(); await tick();
  document.body.dispatchEvent(new window.KeyboardEvent('keydown',{key:'/',bubbles:true}));
  expect(document.activeElement.id).toBe('search');
  $('#search').dispatchEvent(new window.KeyboardEvent('keydown',{key:'ArrowDown',bubbles:true}));
  expect(document.activeElement.classList.contains('card-main')).toBe(true);
});

test('the catalog uses the documentation stylesheet and three-column page shell', async () => {
  setup(); await tick();
  const build=readFileSync('build.mjs','utf8');
  expect(build).toContain('resources/vis-docs/assets');
  expect(readFileSync('dist/assets/theme.css')).toEqual(readFileSync('../../resources/vis-docs/assets/theme.css'));
  expect($('.top .brand').textContent).toBe('Vis');
  expect($('.top .brand').getAttribute('href')).toBe('/');
  expect($('.top .center-link').getAttribute('aria-label')).toBe('Extension Center');
  expect($('.top .center-link').getAttribute('title')).toBe('Extension Center');
  expect($('.top .center-link svg').getAttribute('aria-hidden')).toBe('true');
  expect($('.top .center-link').textContent).toBe('');
  expect($('.top .center-link').getAttribute('href')).toBe('/extensions/');
  expect($('.top .center-link').getAttribute('aria-current')).toBe('location');
  expect($('.top .center-link').hasAttribute('target')).toBe(false);
  expect($('.shell > .side #categories')).not.toBeNull();
  expect($('.shell > .main > .content #catalog-page')).not.toBeNull();
  expect($('.shell > .toc')).not.toBeNull();
  expect($('[data-category="all"]').classList.contains('active')).toBe(true);
  expect($('.eyebrow')).toBeNull();
 });

test('the mobile documentation drawer traps focus, closes on Escape and applies categories', async () => {
  setup(undefined,true); await tick();
  const toggle=$('#navtoggle');
  expect($('#catalog-navigation').inert).toBe(true);
  const open=()=>{toggle.checked=true;toggle.dispatchEvent(new window.Event('change'));};
  open();
  expect(toggle.getAttribute('aria-expanded')).toBe('true');
  expect($('.main').inert).toBe(true);
  expect(document.body.style.overflow).toBe('hidden');
  [...document.querySelectorAll('.side a')].at(-1).focus();
  document.activeElement.dispatchEvent(new window.KeyboardEvent('keydown',{key:'Tab',bubbles:true,cancelable:true}));
  expect(document.activeElement).toBe(toggle);
  toggle.dispatchEvent(new window.KeyboardEvent('keydown',{key:'Escape',bubbles:true,cancelable:true}));
  expect(toggle.checked).toBe(false);
  expect($('.main').inert).toBe(false);
  expect(document.body.style.overflow).toBe('');
  open(); $('[data-category="providers"]').click();
  expect(names()).toEqual(['vis-local-models']);
  expect(toggle.checked).toBe(false);
  expect(document.activeElement).toBe($('#search'));
});

test('documentation section links scroll within details without losing the selected extension', async () => {
  setup(); await tick(); $('.card-main').click(); await tick();
  const hash=window.location.hash;
  $('.toc a[href="#install"]').click();
  expect($('#install').scrollIntoView).toHaveBeenCalledWith({block:'start'});
  expect(window.location.hash).toBe(hash);
  expect($('#detail-page').hidden).toBe(false);
 });

test('the hidden documentation navigation toggle is not styled as a full-width text field', async () => {
  setup(); await tick();
  const style=document.createElement('style');
  style.textContent=readFileSync('../../resources/vis-docs/assets/theme.css','utf8')+readFileSync('web/style.css','utf8');
  document.body.append(style);
  expect(window.getComputedStyle($('#search')).width).toBe('100%');
  expect(window.getComputedStyle($('#navtoggle')).width).not.toBe('100%');
  expect($('#navtoggle').tabIndex).toBe(-1);
 });

test('typing while Turnstile loads does not cancel the challenge', async () => {
  setup(); await tick(); const turnstile=window.turnstile; delete window.turnstile;
  $('#submit-open').click();
  change('[name=repository_url]',item.repository_url);
  const script=document.head.querySelector('script[src*="turnstile"]');
  window.turnstile=turnstile; script.dispatchEvent(new window.Event('load')); await tick();
  expect(turnstile.render).toHaveBeenCalledOnce(); script.remove();
});

test('a browser named element is not mistaken for the loaded Turnstile API', async () => {
  setup(); await tick(); const api=window.turnstile;
  expect($('#turnstile')).toBeNull();
  // A named element must not be treated as the SDK, and our container must not claim its name.
  window.turnstile=$('#turnstile-widget');
  $('#submit-open').click();
  const script=document.head.querySelector('script[src*="turnstile"]');
  expect(script).not.toBeNull();
  window.turnstile=api; script.dispatchEvent(new window.Event('load')); await tick();
  expect(api.render).toHaveBeenCalledOnce(); script.remove();
});

test('an expired or removed challenge cannot authorize a later form', async () => {
  const request=setup(); await tick(); $('#submit-open').click();
  const callbacks=window.turnstile.render.mock.calls.at(-1)[1];
  callbacks['expired-callback'](); change('[name=repository_url]',item.repository_url);
  const before=request.mock.calls.length;
  $('#repository-form').dispatchEvent(new window.Event('submit',{cancelable:true})); await tick();
  expect(request.mock.calls).toHaveLength(before);
  expect($('#submit-status').textContent).toContain('anti-spam');
  $('#submit-close').click(); callbacks.callback('late-token');
  expect($('#submit-dialog').open).toBe(false);
});

test('documentation navigation leaves the catalog router in the same tab', async()=>{
  setup();await tick();
  for(const link of [$('.top .brand'), ...document.querySelectorAll('nav[aria-label="Documentation"] a')]) {
    expect(link.target).toBe('');
    expect(new URL(link.href).origin).toBe(window.location.origin);
    let prevented;
    const listener=event=>{prevented=event.defaultPrevented;event.preventDefault();};
    document.addEventListener('click',listener,{once:true});
    link.click();expect(prevented).toBe(false);
  }
});
