// @vitest-environment jsdom
import { afterEach, expect, test, vi } from 'vitest';
import { mount, installCommand } from './app.js';
import fixtures from './catalog.fixture.json';
import { filters, filterURL, shellHTML } from './render.js';
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

test('catalog opens as responsive results, not a selected split pane, and filters by category and author', async () => {
  setup(); await tick();
  expect($('#detail-page').hidden).toBe(true);
  expect($('#results').hasAttribute('data-view')).toBe(false);
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

test('sort controls affect real results and persist when the search form is submitted', async () => {
  setup(); await tick();
  expect(names()[0]).toBe('vis-github');
  for (const [sort, expected] of [['updated','vis-greeter'],['newest','vis-greeter'],['name','vis-browser']]) {
    change('#sort',sort,'change'); expect(names()[0]).toBe(expected);
  }
  const submit=new window.Event('submit',{bubbles:true,cancelable:true});
  $('#filters').dispatchEvent(submit);
  expect(submit.defaultPrevented).toBe(true);
  expect(window.location.search).toBe('?sort=name');
  expect(names()[0]).toBe('vis-browser');
});

test.each(['grid','list'])('the catalog has no manual layout controls or state, including with view=%s in the URL', async view => {
  const search='?sort=name&view='+view;
  const state=filters(search);
  expect(state).toEqual({q:'',category:'all',sort:'name'});
  expect(filterURL(state)).toBe('/extensions/?sort=name');
  const server=document.createElement('div');
  server.innerHTML=shellHTML({items:fixtures,search});
  expect(server.querySelector('.view-switch,[name=view],[data-view]')).toBeNull();
  expect(server.querySelectorAll('.extension-card')).toHaveLength(6);
  expect([...server.querySelectorAll('[data-category]')].every(link=>!new URL(link.href).searchParams.has('view'))).toBe(true);
  window.history.replaceState(null,'','/extensions/'+search);
  setup(); await tick();
  expect($('.view-switch,[name=view],[data-view]')).toBeNull();
  expect(names()[0]).toBe('vis-browser');
  change('#sort','updated','change');
  expect(window.location.search).toBe('?sort=updated');
  expect($('.view-switch,[name=view],[data-view]')).toBeNull();
});

test('catalog CSS uses a two-column grid on wide screens and border-separated rows below it', () => {
  const style=document.createElement('style');
  style.textContent=readFileSync('web/style.css','utf8');
  document.body.append(style);
  const rules=[...style.sheet.cssRules];
  expect(rules.find(rule=>rule.selectorText==='#results').style.getPropertyValue('grid-template-columns')).toBe('repeat(2,minmax(0,1fr))');
  const compact=rules.find(rule=>rule.conditionText==='(max-width: 1200px)');
  expect(compact).toBeDefined();
  const compactStyle=selector=>[...compact.cssRules].find(rule=>rule.selectorText===selector).style;
  expect(compactStyle('#results').getPropertyValue('grid-template-columns')).toBe('1fr');
  expect(compactStyle('#results').getPropertyValue('gap')).toBe('0px');
  expect(['top','right','bottom','left'].map(side=>compactStyle('.extension-card').getPropertyValue('border-'+side+'-width'))).toEqual(['0px','0px','1px','0px']);
  expect(compactStyle('.extension-card:first-child').getPropertyValue('border-top-width')).toBe('1px');
  const touch=rules.find(rule=>rule.conditionText==='(pointer: coarse)');
  // WebKit's native select ignores min-height, so the touch target needs an explicit height.
  expect([...touch.cssRules].find(rule=>rule.selectorText==='.sort-field select')?.style.getPropertyValue('height')).toBe('2.75rem');
  expect(style.textContent).not.toMatch(/data-view|view-switch/);
});

test('release controls retain pointer and touch targets without replacing native selects',()=>{
  const style=document.createElement('style');style.textContent=readFileSync('web/style.css','utf8');document.body.append(style);
  const rules=[...style.sheet.cssRules],touch=[...rules.find(rule=>rule.conditionText==='(pointer: coarse)').cssRules];
  for(const [selector,property,value] of [['.version-picker select','height','2.75rem'],['.release-history a','min-height','1.75rem'],['.install-section summary','min-height','1.75rem']]) expect(rules.find(rule=>rule.selectorText===selector).style.getPropertyValue(property)).toBe(value);
  for(const selector of ['.release-history a','.install-section summary']) expect(touch.find(rule=>rule.selectorText===selector).style.getPropertyValue('min-height')).toBe('2.75rem');
});
test('extension detail spacing overrides prose margins without changing README typography', () => {
  document.body.innerHTML=shellHTML({item:{...item,releases:[item],latest_version:item.version}});
  const style=document.createElement('style');
  style.textContent=readFileSync('../../resources/vis-docs/assets/theme.css','utf8')+'\n'+readFileSync('web/style.css','utf8');
  document.body.append(style);
  const css=selector=>window.getComputedStyle($(selector));
  expect(css('.back-button').display).toBe('inline-flex');
  expect(css('.detail-heading h1').marginBottom).toBe('1rem');
  expect(css('.install-section').gap).toBe('1rem');
  expect(css('.install-section > h2').marginTop).toBe('0px');
  expect(css('#version-help').marginTop).toBe('0px');
  expect(css('#install-command').marginTop).toBe('0px');
  expect(css('.detail-heading p').textAlign).toBe('start');
  expect(css('#version-help').textAlign).toBe('start');
  expect(css('.release-history ol').marginTop).toBe('0.5rem');
  expect(css('.package-readme p').textAlign).toBe('justify');
});
test('repository anti-spam check is separated from the review button', () => {
  const style=document.createElement('style');
  style.textContent=readFileSync('web/style.css','utf8');
  document.body.append(style);
  const rule=[...style.sheet.cssRules].find(rule=>rule.selectorText==='#turnstile-widget');
  expect(rule?.style.getPropertyValue('margin-top')).toBe('1.5rem');
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
  expect(installCommand(item)).toContain("--version '1.0.0'");
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

test('repository scrolling remains scoped after visiting feedback on a detail page',async()=>{
  setup();await tick();$('.card-main').click();await tick();$('#back-to-catalog').click();
  const feedbackBody=$('.feedback-dialog .dialog-body');feedbackBody.scrollTop=77;
  $('#submit-open').click();const repositoryBody=$('#submit-dialog .dialog-body');repositoryBody.scrollTop=200;
  $('#submit-close').click();expect(repositoryBody.scrollTop).toBe(0);expect(feedbackBody.scrollTop).toBe(77);
 });

test('version selection changes the pinned detail, is linkable, and survives back navigation',async()=>{
  const latest={...item,version:'1.2.0',revision:'b'.repeat(40),source_url:item.repository_url+'/tree/'+'b'.repeat(40)}, older={...item,version:'1.0.0'};
  const releases=[latest,older].map(release=>({...release,release_tag:'v'+release.version,release_url:item.repository_url+'/releases/tag/v'+release.version}));
  const request=vi.fn(async path=>({ok:true,json:async()=>path==='/api/extensions'?{extensions:[latest]}:{...(path.includes('version=1.0.0')?older:latest),latest_version:'1.2.0',releases}}));
  setup(request);await tick();$('.card-main').click();await tick();
  expect($('#release-version').value).toBe('1.2.0');
  change('#release-version','1.0.0','change');$('#version-form').dispatchEvent(new window.Event('submit',{bubbles:true,cancelable:true}));await tick();
  expect(window.location.search).toBe('?version=1.0.0');
  expect(request).toHaveBeenCalledWith('/api/extensions/'+item.id+'?version=1.0.0',undefined);
  expect($('#install-command').textContent).toContain("--version '1.0.0'");
  expect($('#release-version').value).toBe('1.0.0');
  expect($('#source-link').href).toBe(older.source_url);
  expect($('#version-help').textContent).toContain('different version');
  expect($('.release-history').textContent).toContain('Approved releases (2)');
  window.history.replaceState(null,'','/extensions/'+item.id);window.dispatchEvent(new window.PopStateEvent('popstate'));await tick();
  expect($('#release-version').value).toBe('1.2.0');
});

test('repository confirmation pins the selected release tag as well as its SHA',async()=>{
  const request=vi.fn(async path=>({ok:true,json:async()=>path==='/api/extensions'?{extensions:[]}:{...item,source_paths:[],release_tag:'vis-greeter/v1.0.0'}}));
  setup(request);await tick();$('#submit-open').click();
  change('#repository-url',item.repository_url);change('#release-tag','vis-greeter/v1.0.0');
  $('#repository-form').dispatchEvent(new window.Event('submit',{bubbles:true,cancelable:true}));await tick();
  $('#submit-confirm').click();await tick();
  const body=JSON.parse(request.mock.calls.find(([path])=>path==='/api/submissions')[1].body);
  expect(body.release_tag).toBe('vis-greeter/v1.0.0');expect(body.revision).toBe(item.revision);
});
