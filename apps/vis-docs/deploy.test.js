import { afterEach, expect, test, vi } from 'vitest';
import { readFileSync } from 'node:fs';
import { deploymentConfig, verifyDeployment } from './deploy.mjs';
const base=JSON.parse(readFileSync('wrangler.jsonc','utf8'));
const env={DOCS_D1_DATABASE_ID:'12345678-1234-1234-1234-123456789abc',DOCS_TURNSTILE_SITE_KEY:'production-public-site-key',DOCS_RATE_LIMIT_NAMESPACE:'1002',DOCS_HOSTNAME:'gateway.example.com'};
test('deployment whitelists public bindings, preserves D1 and configures one custom domain',()=>{
  const config=deploymentConfig(base,{...env,CLOUDFLARE_API_TOKEN:'private-deploy-canary',TURNSTILE_SECRET_KEY:'private-turnstile-canary',GITHUB_TOKEN:'private-github-canary'});
  expect(config.name).toBe('vis-docs');
  expect(config.d1_databases[0]).toMatchObject({database_id:env.DOCS_D1_DATABASE_ID,database_name:'vis-extension-center'});
  expect(config.ratelimits[0].namespace_id).toBe('1002');
  expect(config.preview_urls).toBe(false);expect(config.workers_dev).toBe(false);
  expect(config.routes).toEqual([{pattern:'gateway.example.com',custom_domain:true}]);
  expect(JSON.stringify(config)).not.toContain('private-');
  expect(config.vars).toEqual({TURNSTILE_SITE_KEY:env.DOCS_TURNSTILE_SITE_KEY});
});
test('deployment fails closed for missing configuration and testing keys',()=>{
  for(const key of Object.keys(env)) expect(()=>deploymentConfig(base,{...env,[key]:''})).toThrow();
  expect(()=>deploymentConfig(base,{...env,DOCS_D1_DATABASE_ID:base.d1_databases[0].database_id})).toThrow();
  expect(()=>deploymentConfig(base,{...env,DOCS_TURNSTILE_SITE_KEY:'1x00000000000000000000AA'})).toThrow();
});
const paths=['/','/extending.html','/extensions/','/assets/theme.css','/assets/prism.min.js','/api/extensions'];
function deployedResponse(path) {
  if(path==='/') return new Response('<link href="assets/theme.css"><a class="center-link" href="/extensions/">Extensions</a>');
  if(path==='/extending.html') return new Response('<h1>Extending Vis</h1>');
  if(path==='/extensions/') return new Response('<h1>Extension Center</h1><script id="catalog-data" type="application/json">{}</script>');
  if(path==='/assets/theme.css') return new Response(':root{--font:monospace}');
  if(path==='/assets/prism.min.js') return new Response('Prism.highlightAll();');
  if(path==='/api/extensions') return Response.json({extensions:[]});
  throw new Error('Unexpected verification path');
}
afterEach(()=>{vi.useRealTimers();vi.unstubAllGlobals();vi.restoreAllMocks();});
test('deployment checks docs, catalog, assets and API on the same origin without redirects',async()=>{
  const fetch=vi.fn(async(value,options)=>{
    const url=new URL(value);expect(url.origin).toBe('https://gateway.example.com');expect(options.redirect).toBe('error');
    return deployedResponse(url.pathname);
  });
  vi.stubGlobal('fetch',fetch);await verifyDeployment('gateway.example.com');
  expect(fetch.mock.calls.map(([url])=>new URL(url).pathname)).toEqual(paths);
});
test('deployment retries a connection failure without logging response data',async()=>{
  vi.useFakeTimers();const fetch=vi.fn().mockRejectedValueOnce(new Error('Connection unavailable')).mockImplementation(async url=>deployedResponse(new URL(url).pathname));
  vi.stubGlobal('fetch',fetch);const assertion=expect(verifyDeployment('gateway.example.com')).resolves.toBeUndefined();
  await vi.runAllTimersAsync();await assertion;expect(fetch).toHaveBeenCalledTimes(paths.length+1);
});
test.each(paths)('deployment checks remain bounded when %s is broken',async broken=>{
  vi.useFakeTimers();const fetch=vi.fn(async url=>new URL(url).pathname===broken?new Response('Wrong deployment'):deployedResponse(new URL(url).pathname));
  vi.stubGlobal('fetch',fetch);const assertion=expect(verifyDeployment('gateway.example.com')).rejects.toThrow('after 8 attempts');
  await vi.runAllTimersAsync();await assertion;
  expect(fetch.mock.calls.filter(([url])=>new URL(url).pathname===broken)).toHaveLength(8);
});
test('configuration and verification reject anything other than a public hostname',async()=>{
  const fetch=vi.fn();vi.stubGlobal('fetch',fetch);
  for(const hostname of ['','localhost','https://gateway.example.com','user:password@gateway.example.com','gateway.example.com:443','gateway.example.com/path','gateway.example.com?token=value']) {
    expect(()=>deploymentConfig(base,{...env,DOCS_HOSTNAME:hostname})).toThrow('DOCS_HOSTNAME');
    await expect(verifyDeployment(hostname)).rejects.toThrow('DOCS_HOSTNAME');
  }
  expect(fetch).not.toHaveBeenCalled();
});
