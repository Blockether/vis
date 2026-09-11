import { readBounded, RequestError } from './github.js';

/** Shared same-origin, bounded JSON and Turnstile gate. Tokens are action-specific. */
export async function protectedBody(request,env,action,fields,limit=4096) {
  const url=new URL(request.url);
  if(request.headers.get('Origin')!==url.origin) throw new RequestError('Submit from this Extension Center.',403);
  if(request.headers.get('Content-Type')?.split(';')[0]!=='application/json') throw new RequestError('Use application/json.',415);
  if(!env.TURNSTILE_SITE_KEY||!env.TURNSTILE_SECRET_KEY||!env.SUBMISSIONS_LIMITER) throw new RequestError('Submissions are not configured yet. Try again later.',503);
  const ip=request.headers.get('CF-Connecting-IP');
  if(!ip) throw new RequestError('Network identity is unavailable. Try again later.',503);
  if(!(await env.SUBMISSIONS_LIMITER.limit({key:ip})).success) throw new RequestError('Too many submissions. Wait a minute and try again.',429);
  let body; try {body=JSON.parse(await readBounded(request,limit));} catch(error) {if(error instanceof RequestError) throw error;throw new RequestError('Supply valid JSON.');}
  if(!body||Array.isArray(body)||typeof body!=='object'||Object.keys(body).some(key=>!['turnstile_token',...fields].includes(key))) throw new RequestError('Supply only the fields requested by this form.');
  const token=body.turnstile_token;
  if(typeof token!=='string'||!token||token.length>2048) throw new RequestError('Complete the anti-spam check.',403);
  const verification=await fetch('https://challenges.cloudflare.com/turnstile/v0/siteverify',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({secret:env.TURNSTILE_SECRET_KEY,response:token,remoteip:ip}),signal:AbortSignal.timeout(10000)});
  if(!verification.ok) throw new RequestError('Anti-spam verification is unavailable. Try again.',503);
  const result=JSON.parse(await readBounded(verification,8192));
  if(!result.success||result.hostname!==url.hostname||result.action!==action) throw new RequestError('Anti-spam check expired or failed. Try again.',403);
  return body;
}
