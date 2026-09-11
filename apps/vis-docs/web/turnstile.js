let loading;
/** Both submission forms share one script load, with bounded failure and retry. */
export async function loadTurnstile() {
  if(typeof window.turnstile?.render==='function') return;
  if(!loading) loading=new Promise((resolve,reject)=>{
    const script=document.createElement('script');
    const timer=window.setTimeout(()=>fail(),15000);
    const fail=()=>{window.clearTimeout(timer);script.remove();loading=null;reject(new Error('Could not load the anti-spam check. Close and reopen the form to retry.'));};
    script.src='https://challenges.cloudflare.com/turnstile/v0/api.js?render=explicit';script.async=true;
    script.onload=()=>{window.clearTimeout(timer);loading=null;resolve();};script.onerror=fail;document.head.append(script);
  });
  await loading;
}
