import { escapeHTML } from './html.js';
import { loadTurnstile } from './turnstile.js';

const voteHTML=(rating,target='package')=>`<div class="feedback-votes" role="group" aria-label="${target==='package'?'Package rating':'Comment rating'}"><button type="button" data-vote="1" data-target="${target}" aria-pressed="${rating.own===1}" aria-label="Upvote ${target==='package'?'package':'comment '+target}">Helpful (${rating.up})</button><button type="button" data-vote="-1" data-target="${target}" aria-pressed="${rating.own===-1}" aria-label="Downvote ${target==='package'?'package':'comment '+target}">Not helpful (${rating.down})</button><span>Score: ${rating.score}</span></div>`;
export const communityHTML=()=>`<section class="community" id="feedback" aria-labelledby="feedback-title"><h2 id="feedback-title">Community feedback</h2><p class="help">Votes and display names are not verified identities. One vote per network address; shared networks share a vote. Select your current choice again to clear it.</p><div data-rating></div><p data-feedback-status role="status">Loading feedback…</p><button type="button" data-feedback-retry hidden>Retry feedback</button><h3>Comments</h3><div data-comments></div><button type="button" data-more hidden>Older comments</button><form data-comment-form><h3>Add a comment</h3><p class="help">Comments appear after moderation. Use a public display name, not private information. Five comments per network address in 24 hours.</p><label for="comment-name">Display name (not verified)</label><input id="comment-name" name="name" required maxlength="60" autocomplete="nickname"><label for="comment-body">Comment</label><textarea id="comment-body" name="body" required maxlength="2000" rows="5"></textarea><button type="submit">Submit for moderation</button></form><dialog class="content feedback-dialog" aria-labelledby="feedback-check-title"><div class="dialog-head"><h2 id="feedback-check-title">Confirm feedback</h2><button type="button" data-feedback-close>Cancel</button></div><div class="dialog-body"><p data-feedback-intent></p><div data-feedback-challenge></div><p role="status" data-check-status></p><button type="button" class="primary" data-feedback-confirm>Confirm</button></div></dialog><noscript>Enable JavaScript to load feedback and complete the anti-spam check.</noscript></section>`;

/** Only this detail view owns these requests and the temporary confirmation dialog. */
export function mountCommunity(container,id,sitekey,request=fetch) {
  const $=selector=>container.querySelector(selector), form=$('[data-comment-form]'), dialog=$('dialog');
  let disposed=false,busy=false,revision=0,challengeRevision=0,widget=null,confirmPending=null,token='',next=null;
  let data={votes:{up:0,down:0,score:0,own:0},comments:[]};
  const base='/api/extensions/'+id;
  async function api(path,options) {const response=await request(path,options),body=await response.json();if(!response.ok) throw new Error(body.error||'Could not load feedback. Try again.');return body;}
  function paint() {
    $('[data-rating]').innerHTML=voteHTML(data.votes);
    $('[data-comments]').innerHTML=data.comments.length?data.comments.map(comment=>`<article class="feedback-comment"><h4>${escapeHTML(comment.name)} <span class="help">· Not verified</span></h4><time datetime="${escapeHTML(comment.created_at)}">${escapeHTML(new Date(comment.created_at).toLocaleDateString('en'))}</time><p class="comment-body">${escapeHTML(comment.body)}</p>${voteHTML(comment.votes,comment.id)}</article>`).join(''):'<p>No comments yet. Share your experience below.</p>';
    $('[data-more]').hidden=!next;
  }
  async function load(more=false) {
    const current=++revision;$('[data-feedback-status]').textContent='Loading feedback…';$('[data-feedback-retry]').hidden=true;$('[data-more]').disabled=true;
    try {const result=await api(base+'/community'+(more&&next?'?before='+next:''));if(disposed||current!==revision) return;
      data={...result,comments:more?[...data.comments,...result.comments]:result.comments};next=result.next;paint();$('[data-feedback-status]').textContent='';
    } catch(error) {if(!disposed&&current===revision) {$('[data-feedback-status]').textContent=error.message;$('[data-feedback-retry]').hidden=false;}}
    finally {if(!disposed&&current===revision) $('[data-more]').disabled=false;}
  }
  function endChallenge() {
    ++challengeRevision;token='';if(widget!==null) {window.turnstile?.remove(widget);widget=null;}
    if(confirmPending) {confirmPending(null);confirmPending=null;}
  }
  dialog.addEventListener('close',()=>{endChallenge();document.body.style.overflow='';});
  $('[data-feedback-close]').onclick=()=>dialog.close();
  $('[data-feedback-confirm]').onclick=()=>{if(!token) {$('[data-check-status]').textContent='Complete the anti-spam check first.';return;}const resolve=confirmPending;confirmPending=null;const value=token;dialog.close();resolve?.(value);};
  async function confirm(action,intent) {
    if(!sitekey) throw new Error('Feedback submissions are not configured yet. Try again later.');
    endChallenge();const current=challengeRevision;dialog.showModal();document.body.style.overflow='hidden';$('[data-feedback-intent]').textContent=intent;$('[data-check-status]').textContent='Loading anti-spam check…';$('[data-feedback-close]').focus();
    const result=new Promise(resolve=>{confirmPending=resolve;});
    loadTurnstile().then(()=>{if(!disposed&&current===challengeRevision&&dialog.open) {
      $('[data-check-status]').textContent='Complete the check, then confirm.';
      widget=window.turnstile.render($('[data-feedback-challenge]'),{sitekey,action,theme:'light',size:'flexible',callback:value=>{if(current===challengeRevision) token=value;},'expired-callback':()=>{if(current===challengeRevision) {token='';$('[data-check-status]').textContent='Check expired. Complete it again.';}},'error-callback':()=>{if(current===challengeRevision) {token='';$('[data-check-status]').textContent='Check failed. Cancel and try again.';}}});
    }}).catch(error=>{if(!disposed&&current===challengeRevision) $('[data-check-status]').textContent=error.message;});
    return result;
  }
  async function write(suffix,body,action,intent) {
    if(busy) return;busy=true;
    try {const turnstile_token=await confirm(action,intent);if(!turnstile_token||disposed) return;
      $('[data-feedback-status]').textContent='Saving feedback…';
      const result=await api(base+'/'+suffix,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({...body,turnstile_token})});if(disposed) return;
      if(suffix==='comments') {form.reset();$('[data-feedback-status]').textContent='Submitted for moderation. Reference: '+result.id;}
      else {if(suffix==='vote') data.votes=result.votes;else {const comment=data.comments.find(row=>row.id===Number(suffix.split('/')[1]));if(comment) comment.votes=result.votes;}paint();$('[data-feedback-status]').textContent='Vote saved.';}
    } catch(error) {if(!disposed) $('[data-feedback-status]').textContent=error.message;}
    finally {busy=false;}
  }
  container.addEventListener('click',event=>{
    const node=event.target.closest('[data-vote]');if(!node) return;
    const target=node.dataset.target, rating=target==='package'?data.votes:data.comments.find(row=>String(row.id)===target)?.votes;if(!rating) return;
    const value=rating.own===Number(node.dataset.vote)?0:Number(node.dataset.vote);
    write(target==='package'?'vote':`comments/${target}/vote`,{value},target==='package'?'extension-vote':'comment-vote',value===0?'Clear your vote.':`Save your ${value===1?'helpful':'not helpful'} vote.`);
  });
  form.onsubmit=event=>{event.preventDefault();if(form.reportValidity()) write('comments',{name:form.elements.name.value.trim(),body:form.elements.body.value.trim()},'extension-comment','Submit this comment for moderation. It will not appear until approved.');};
  $('[data-feedback-retry]').onclick=()=>load();$('[data-more]').onclick=()=>load(true);load();
  return ()=>{disposed=true;++revision;endChallenge();if(dialog.open) dialog.close();};
}
