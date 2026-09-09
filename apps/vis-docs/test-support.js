/** Local workerd/D1 fixture; only GitHub and Turnstile HTTP responses are replaced. */
import { Miniflare } from 'miniflare';
import { build } from 'esbuild';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { security } from './headers.js';
export async function runtimeFixture({port=0,hostname='center.example.com',seed=false}={}) {
  const controls={github:'ok',verification:'ok',requests:[],tokens:new Set()};
  const manifest=readFileSync('examples/vis-greeter/pyproject.toml','utf8'), revision='a'.repeat(40);
  const result=await build({entryPoints:['worker.js'],bundle:true,format:'esm',platform:'browser',write:false});
  const runtime=new Miniflare({host:'127.0.0.1',port,cf:false,telemetry:{enabled:false},workers:[{
    config:{name:'center',type:'worker',compatibilityDate:'2026-02-01',manifest:{mainModule:'worker.js',modules:{'worker.js':{type:'esm',contents:result.outputFiles[0].text}}},env:{DB:{type:'d1',id:'catalog-test'},TURNSTILE_SITE_KEY:{type:'text',value:'fixture-site-key'},TURNSTILE_SECRET_KEY:{type:'text',value:'server-only-fixture-secret'},GITHUB_TOKEN:{type:'text',value:'server-only-github-fixture'},SUBMISSIONS_LIMITER:{type:'rate-limit',namespace:'1001',simple:{limit:10,period:60}},ASSETS:{type:'fetcher',handler:async request=>{
      const path=new URL(request.url).pathname;
      if(path!=='/'&&!/^\/(?:[a-z0-9-]+\.html|assets\/[a-zA-Z0-9_./-]+)$/.test(path)||path.includes('..')) return new Response('',{status:404});
      try {const type=path==='/'||path.endsWith('.html')?'text/html':path.endsWith('.css')?'text/css':path.endsWith('.js')?'text/javascript':path.endsWith('.woff2')?'font/woff2':'image/png';return new Response(readFileSync(resolve('dist'+(path==='/'?'/index.html':path))),{headers:{...security,'Content-Type':type}});} catch {return new Response('',{status:404});}
    }}}},
    dev:{stripCfConnectingIp:false,outboundService:{type:'fetcher',handler:async request=>{
      const url=new URL(request.url);controls.requests.push(url.href);
      const json=(body,status=200)=>new Response(JSON.stringify(body),{status,headers:{'Content-Type':'application/json'}});
      if(url.hostname==='challenges.cloudflare.com') {
        const {response:token}=await request.json();
        const success=controls.verification==='ok'&&!controls.tokens.has(token);controls.tokens.add(token);
        return json({success,hostname:controls.verification==='hostname'?'wrong.example.com':hostname,action:token.startsWith('preview')?'extension-preview':'extension-submit'});
      }
      if(url.hostname!=='api.github.com') throw new Error('Unexpected outbound host');
      if(controls.github==='redirect') return new Response('',{status:301,headers:{Location:'https://other.example.com/'}});
      if(controls.github==='rate') return json({},429);
      if(url.pathname.includes('/commits/')) return json({sha:revision,commit:{committer:{date:'2026-02-10T12:00:00Z'}}});
      if(url.pathname.endsWith('/pyproject.toml')) return json({type:'file',encoding:'base64',size:manifest.length,content:Buffer.from(manifest).toString('base64')});
      if(url.pathname.includes('/contents')) return json(controls.github==='missing'?[]:[{name:'pyproject.toml',type:'file'},{name:'extension.py',type:'file'},{name:'README.md',type:'file'}]);
      return json({private:controls.github==='private',default_branch:'main',stargazers_count:12,topics:['example'],license:{spdx_id:'MIT'}});
    }}},
  }]});
  try {
    const db=await runtime.getD1Database('DB');
    await db.batch(readFileSync('schema.sql','utf8').split(';').filter(sql=>sql.trim()).map(sql=>db.prepare(sql)));
    if(seed) for(const item of JSON.parse(readFileSync('web/catalog.fixture.json','utf8'))) await db.prepare('INSERT INTO extensions VALUES (?, ?, ?)').bind(item.id,JSON.stringify(item),item.added_at).run();
    return {runtime,db,controls,revision};
  } catch(error) {await runtime.dispose();throw error;}
}
