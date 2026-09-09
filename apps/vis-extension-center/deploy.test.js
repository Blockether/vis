import { afterEach, expect, test, vi } from 'vitest';
import { readFileSync } from 'node:fs';
import { deploymentConfig, verifyDeployment } from './deploy.mjs';
const base=JSON.parse(readFileSync('wrangler.jsonc','utf8'));
const env={CENTER_D1_DATABASE_ID:'12345678-1234-1234-1234-123456789abc',CENTER_TURNSTILE_SITE_KEY:'production-public-site-key',CENTER_RATE_LIMIT_NAMESPACE:'1002'};
test('deployment configuration whitelists public values only',()=>{
  const config=deploymentConfig(base,{...env,CLOUDFLARE_API_TOKEN:'private-deploy-canary',TURNSTILE_SECRET_KEY:'private-turnstile-canary',GITHUB_TOKEN:'private-github-canary'});
  expect(config.d1_databases[0].database_id).toBe(env.CENTER_D1_DATABASE_ID);
  expect(config.ratelimits[0].namespace_id).toBe('1002');
  expect(config.preview_urls).toBe(false);
  expect(JSON.stringify(config)).not.toContain('private-');
  expect(config.vars).toEqual({DOCS_URL:base.vars.DOCS_URL,TURNSTILE_SITE_KEY:env.CENTER_TURNSTILE_SITE_KEY});
});
test('deployment fails closed for missing configuration and testing keys',()=>{
  for(const key of Object.keys(env)) expect(()=>deploymentConfig(base,{...env,[key]:''})).toThrow();
  expect(()=>deploymentConfig(base,{...env,CENTER_D1_DATABASE_ID:base.d1_databases[0].database_id})).toThrow();
  expect(()=>deploymentConfig(base,{...env,CENTER_TURNSTILE_SITE_KEY:'1x00000000000000000000AA'})).toThrow();
});

const deployedHtml='<title>Extension Center</title><link href="/assets/theme.css">';
afterEach(()=>{vi.useRealTimers();vi.unstubAllGlobals();vi.restoreAllMocks();});
test('deployment checks retry transient HTML and catalog failures',async()=>{
  vi.useFakeTimers();
  const fetch=vi.fn().mockResolvedValueOnce(new Response('Not ready',{status:503}))
    .mockResolvedValueOnce(new Response(deployedHtml))
    .mockResolvedValueOnce(new Response('Not ready',{status:503}))
    .mockResolvedValueOnce(new Response(deployedHtml))
    .mockResolvedValueOnce(Response.json({extensions:[]}));
  vi.stubGlobal('fetch',fetch);
  const pending=verifyDeployment('https://gateway.example.com');
  const assertion=expect(pending).resolves.toBeUndefined();
  await vi.runAllTimersAsync();await assertion;
  expect(fetch).toHaveBeenCalledTimes(5);
});
test('deployment checks retry connection failure without logging response data',async()=>{
  vi.useFakeTimers();
  const fetch=vi.fn().mockRejectedValueOnce(new Error('Connection unavailable'))
    .mockResolvedValueOnce(new Response(deployedHtml))
    .mockResolvedValueOnce(Response.json({extensions:[]}));
  vi.stubGlobal('fetch',fetch);
  const assertion=expect(verifyDeployment('https://gateway.example.com')).resolves.toBeUndefined();
  await vi.runAllTimersAsync();await assertion;
  expect(fetch).toHaveBeenCalledTimes(3);
});
test.each(['HTML','catalog'])('deployment checks remain bounded on permanent %s failure',async(kind)=>{
  vi.useFakeTimers();
  const fetch=vi.fn(async url=>String(url).endsWith('/api/extensions')?Response.json({wrong:[]}):new Response(kind==='HTML'?'Wrong app':deployedHtml));
  vi.stubGlobal('fetch',fetch);
  const assertion=expect(verifyDeployment('https://gateway.example.com')).rejects.toThrow('after 8 attempts');
  await vi.runAllTimersAsync();await assertion;
  expect(fetch).toHaveBeenCalledTimes(kind==='HTML'?8:16);
});
test('deployment checks reject unsafe URLs before network access',async()=>{
  const fetch=vi.fn();vi.stubGlobal('fetch',fetch);
  for(const url of ['http://gateway.example.com','https://user:password@gateway.example.com']) await expect(verifyDeployment(url)).rejects.toThrow('Configure VIS_EXTENSION_CENTER_URL');
  expect(fetch).not.toHaveBeenCalled();
});
