import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { afterEach, mock, test } from 'node:test';
import { configureHTTPS, verifyHTTPS } from './cloudflare-https.mjs';

const canary = 'test-only-credential-canary';
const account = '1'.repeat(32), zone = '2'.repeat(32), rulesetId = '3'.repeat(32), ruleId = '4'.repeat(32);
const hostname = 'gateway.example.com';
const env = {HTTPS_URL: `https://${hostname}/healthz`, WORKER_NAME: 'vis-docs', CLOUDFLARE_ACCOUNT_ID: account, CLOUDFLARE_API_TOKEN: canary};
const domain = {hostname, service: env.WORKER_NAME, environment: 'production', enabled: true, zone_id: zone};
const phase = 'http_request_dynamic_redirect';
const zonePath = `/zones/${zone}/rulesets`;
const entrypoint = `${zonePath}/phases/${phase}/entrypoint`;
const rule = {
  ref: 'vis_https_' + createHash('sha256').update(hostname).digest('hex').slice(0, 24),
  description: `Require HTTPS for ${hostname}`,
  // Cloudflare rejects http.request.scheme (API 20127); ssl is the documented field.
  expression: `(http.host eq "${hostname}" and not ssl)`,
  action: 'redirect',
  action_parameters: {from_value: {target_url: {expression: 'concat("https://", http.host, http.request.uri.path)'}, status_code: 308, preserve_query_string: true}},
  enabled: true,
};
const unrelated = {...rule, id: '5'.repeat(32), ref: 'external-rule', expression: 'false'};
const ruleset = rules => ({id: rulesetId, kind: 'zone', phase, rules});
const ok = result => Response.json({success: true, result});
const failure = (status, code) => Response.json({success: false, errors: [{code, message: canary}]}, {status});

function api(...responses) {
  const calls = [];
  mock.method(globalThis, 'fetch', async (value, options) => {
    const url = new URL(value);
    assert.equal(url.origin, 'https://api.cloudflare.com');
    assert.equal(options.headers.Authorization, `Bearer ${canary}`);
    assert.equal(options.redirect, 'error');
    assert.ok(options.signal instanceof AbortSignal);
    calls.push({method: options.method, path: url.pathname.replace('/client/v4', ''), body: options.body ? JSON.parse(options.body) : undefined});
    assert.ok(responses.length, 'Unexpected API call');
    const response = responses.shift();
    if (response instanceof Error) throw response;
    return response;
  });
  return calls;
}
afterEach(() => mock.restoreAll());

test('creates a hostname-only 308 rule in the zone of the deployed Custom Domain', async () => {
  const calls = api(ok([domain]), failure(404, 10003), ok(ruleset([rule])));
  await configureHTTPS(env);
  assert.deepEqual(calls.map(({method, path}) => [method, path]), [
    ['GET', `/accounts/${account}/workers/domains`], ['GET', entrypoint], ['POST', zonePath],
  ]);
  assert.deepEqual(calls[2].body, {name: 'HTTPS redirects', kind: 'zone', phase, rules: [rule]});
  assert.ok(!JSON.stringify(calls).includes(canary));
});

test('adds only the owned rule without replacing or deleting existing rules', async () => {
  const calls = api(ok([domain]), ok(ruleset([unrelated])), ok(ruleset([rule, unrelated])));
  await configureHTTPS(env);
  assert.deepEqual(calls[2], {method: 'POST', path: `${zonePath}/${rulesetId}/rules`, body: {...rule, position: {index: 1}}});
  assert.equal(calls.length, 3);
});

test('repairs only the stable owned rule, retaining its identity and unrelated rules', async () => {
  const calls = api(ok([domain]), ok(ruleset([unrelated, {...rule, id: ruleId, enabled: false}])), ok(ruleset([rule, unrelated])));
  await configureHTTPS(env);
  assert.deepEqual(calls[2], {method: 'PATCH', path: `${zonePath}/${rulesetId}/rules/${ruleId}`, body: {...rule, position: {index: 1}}});
  assert.equal(calls.length, 3);
});

test('an already-correct rule requires no write on a repeated deployment', async () => {
  const calls = api(ok([domain]), ok(ruleset([{...rule, id: ruleId, version: '2'}, unrelated])));
  await configureHTTPS(env);
  assert.equal(calls.length, 2);
  assert.ok(calls.every(call => call.method === 'GET'));
});

for (const status of [400, 409]) test(`first-deployment race (${status}) rereads the phase and adds only its own rule`, async () => {
  const calls = api(ok([domain]), failure(404, 10003), failure(status, 2002), ok(ruleset([unrelated])), ok(ruleset([rule, unrelated])));
  await configureHTTPS(env);
  assert.deepEqual(calls.map(call => call.method), ['GET', 'GET', 'POST', 'GET', 'POST']);
  assert.equal(calls[3].path, entrypoint);
  assert.deepEqual(calls[4].body, {...rule, position: {index: 1}});
});

test('a failed creation is bounded when no concurrent phase appeared', async () => {
  const calls = api(ok([domain]), failure(404, 10003), failure(409, 2002), failure(404, 10003));
  await assert.rejects(configureHTTPS(env), /HTTP 409; codes 2002/);
  assert.equal(calls.length, 4);
});

for (const [status, code] of [[401, 10000], [403, 10000], [404, 7003], [500, 1000]]) test(`phase error ${status}/${code} never creates or overwrites rules`, async () => {
  const calls = api(ok([domain]), failure(status, code));
  await assert.rejects(configureHTTPS(env), error => error.message.includes(`HTTP ${status}`) && !error.message.includes(canary));
  assert.equal(calls.length, 2);
  assert.ok(calls.every(call => call.method === 'GET'));
});

test('rejects a malformed successful creation response', async () => {
  const calls = api(ok([domain]), failure(404, 10003), ok(null));
  await assert.rejects(configureHTTPS(env), /Unexpected Cloudflare HTTPS/);
  assert.equal(calls.length, 3);
});

for (const domains of [null, [], [{...domain, enabled: false}], [{...domain, enabled: 'true'}], [{...domain, service: 'another-worker'}], [{...domain, environment: 'preview'}], [{...domain, zone_id: '../other'}], [domain, domain]]) test(`requires one enabled production Custom Domain: ${JSON.stringify(domains)}`, async () => {
  const calls = api(ok(domains));
  await assert.rejects(configureHTTPS(env), /Custom Domain/);
  assert.equal(calls.length, 1);
});

for (const value of [null, {}, ruleset(null), {...ruleset([]), id: '../other'}, {...ruleset([]), phase: 'another_phase'}, ruleset([{...rule, id: '../other'}]), ruleset([{...rule, id: ruleId}, {...rule, id: ruleId}])]) test(`rejects malformed ruleset identity: ${JSON.stringify(value)}`, async () => {
  const calls = api(ok([domain]), ok(value));
  await assert.rejects(configureHTTPS(env), /Unexpected Cloudflare HTTPS/);
  assert.equal(calls.length, 2);
});

for (const HTTPS_URL of ['', 'http://gateway.example.com', 'https://localhost', 'https://127.0.0.1', 'https://user:password@gateway.example.com', 'https://gateway.example.com:443', 'https://gateway.example.com:8443', 'https://gateway.example.com/#fragment', 'https://gateway.example.com\\@another.example.com']) test(`rejects invalid configuration before network: ${HTTPS_URL}`, async () => {
  const calls = api();
  await assert.rejects(configureHTTPS({...env, HTTPS_URL}), /HTTPS_URL/);
  await assert.rejects(verifyHTTPS(HTTPS_URL), /HTTPS_URL/);
  assert.equal(calls.length, 0);
});

for (const key of ['CLOUDFLARE_ACCOUNT_ID', 'CLOUDFLARE_API_TOKEN', 'WORKER_NAME']) test(`requires ${key} before network`, async () => {
  const calls = api();
  await assert.rejects(configureHTTPS({...env, [key]: ''}), /credentials and WORKER_NAME/);
  assert.equal(calls.length, 0);
});

test('network and malformed JSON errors never expose credential or response text', async () => {
  for (const response of [new Error(canary), new Response(canary, {status: 500})]) {
    const calls = api(response);
    await assert.rejects(configureHTTPS(env), error => error.message.includes('connection or response failed') && !error.message.includes(canary));
    assert.equal(calls.length, 1);
    mock.restoreAll();
  }
});

test('verifies GET and HEAD redirects without following them or sending credentials', async () => {
  const calls = [];
  mock.method(globalThis, 'fetch', async (value, options) => {
    const url = new URL(value);
    assert.equal(url.origin, `http://${hostname}`);
    assert.equal(options.redirect, 'manual');
    assert.equal(options.headers, undefined);
    assert.ok(options.signal instanceof AbortSignal);
    calls.push([url.pathname + url.search, options.method]);
    url.protocol = 'https:';
    return new Response(null, {status: 308, headers: {location: url.href}});
  });
  await verifyHTTPS(`https://${hostname}/healthz?source=a%2Fb&check=1`);
  assert.deepEqual(calls, ['/healthz?source=a%2Fb&check=1', '/__vis_https_probe__/a%2Fb?check=1&value=x%2Fy'].flatMap(path => [[path, 'GET'], [path, 'HEAD']]));
});

for (const [status, location] of [
  [200, null], [301, `https://${hostname}/`], [302, `https://${hostname}/`], [307, `https://${hostname}/`],
  [308, null], [308, `http://${hostname}/`], [308, 'https://another.example.com/'],
  [308, `https://${hostname}/wrong-path`], [308, `https://${hostname}/?extra=1`],
]) test(`rejects incorrect redirect ${status} ${location}`, async () => {
  const response = new Response(null, {status, headers: location ? {location} : {}});
  const fetch = mock.method(globalThis, 'fetch', async () => response);
  await assert.rejects(verifyHTTPS(`https://${hostname}`), /Expected HTTP 308/);
  assert.equal(fetch.mock.callCount(), 1);
});

test('does not accept GET redirect when HEAD still serves HTTP', async () => {
  mock.method(globalThis, 'fetch', async (_, options) => new Response(null, options.method === 'GET' ? {status: 308, headers: {location: `https://${hostname}/`}} : {status: 200}));
  await assert.rejects(verifyHTTPS(`https://${hostname}`), /Expected HTTP 308/);
});

test('verification errors and cross-origin paths cannot disclose data or send a request', async () => {
  const fetch = mock.method(globalThis, 'fetch', async () => { throw new Error(canary); });
  await assert.rejects(verifyHTTPS(`https://${hostname}`, ['//another.example.com/']), /configured origin/);
  assert.equal(fetch.mock.callCount(), 0);
  await assert.rejects(verifyHTTPS(`https://${hostname}`), error => error.message === 'HTTP to HTTPS verification connection failed');
});
