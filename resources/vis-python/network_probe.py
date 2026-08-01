__vis_net_filters__ = []
def network_filter(fn):
    """Register a guard fn(ctx)->None|reason for `network_probe` (GUARD-ONLY: a
    session filter never affects LIVE egress — author a `.py` extension for that).
    ctx = {phase,method,host,path,port,headers,body}. Return None to allow; a string
    reason, or a dict like {'reason': ...}, to block; a raise fails CLOSED. Returns
    fn, so it also works as a decorator."""
    __vis_net_filters__.append(fn)
    return fn
def __vis_run_local_filters__(ctx):
    import traceback as _tb
    out = []
    for fn in __vis_net_filters__:
        nm = getattr(fn, '__name__', 'filter')
        v = {'owner': nm, 'allow': True, 'reason': None, 'error': None}
        try:
            r = fn(dict(ctx))
            if r is None or r is False or r is True:
                pass
            elif isinstance(r, str):
                v['allow'] = False
                v['reason'] = r
            elif isinstance(r, dict) and (r.get('__vis_block__') or r.get('marker') == 'block' or r.get('reason')):
                v['allow'] = False
                v['reason'] = r.get('reason') or 'blocked'
        except Exception as _e:
            v['allow'] = False
            v['reason'] = 'filter crashed (fail-closed): %s' % _e
            v['error'] = {'message': str(_e), 'trace': _tb.format_exc()}
        out.append(v)
    return out
def network_probe(method='GET', url=None, headers=None, body=None):
    """GUARD-ONLY egress probe (NEVER sends): evaluate the gateway host/verb/path/
    port + SSRF gate and every registered network filter (extension + your local
    `network_filter`s) over a SYNTHETIC request, printing each verdict + any Python
    traceback. Usage: network_probe(method='POST', url='https://api.github.com/repos')
    or a bare host[:port] for ssh/db, e.g. network_probe(url='db.host:5432'). Pass
    headers={...} and/or body='...' to feed the SYNTHETIC request so header/body
    filter rules can be simulated on the HTTP path."""
    import json as _json
    if url is None:
        url, method = method, None
    rep = _json.loads(__vis_net_probe__(method or '', str(url), _json.dumps(headers or {}), body or ''))
    if 'error' in rep:
        print('net-probe: ' + str(rep['error']))
        return rep
    ctx = rep['ctx']
    gw = rep['filters']
    loc = __vis_run_local_filters__(ctx) if rep['tier1']['allow'] else []
    rep['local_filters'] = loc
    if not rep['tier1']['allow']:
        final = {'allow': False, 'reason': rep['tier1']['reason']}
    else:
        gd = next((f for f in gw if not f['allow']), None)
        ld = next((f for f in loc if not f['allow']), None)
        if gd is not None:
            final = {'allow': False, 'reason': gd['reason']}
        elif ld is not None:
            final = {'allow': False, 'reason': ld['reason']}
        else:
            final = {'allow': True, 'reason': None}
    rep['final'] = final
    tgt = '%s %s%s:%s%s' % (str(rep['scheme']).upper(),
                            (str(ctx['method']) + ' ') if ctx.get('method') else '',
                            ctx['host'], ctx['port'], ctx['path'] or '')
    print('Target: ' + tgt)
    print('')
    print('Tier-1 (host / port / SSRF): ' + ('ALLOW' if rep['tier1']['allow'] else 'DENY — ' + str(rep['tier1']['reason'])))
    def _rows(label, fs):
        print('%s (%d):' % (label, len(fs)))
        if not fs:
            print('  (none registered)')
        for f in fs:
            line = '  • %s → %s' % (f['owner'], 'ALLOW' if f['allow'] else 'DENY')
            if (not f['allow']) and f['reason'] and not f['error']:
                line += ' — ' + str(f['reason'])
            if f['error']:
                line += '\n      ⚠ CRASHED (fail-closed): ' + str(f['error']['message'])
                if f['error'].get('trace'):
                    line += '\n' + f['error']['trace']
            print(line)
    _rows('gateway network_filters', gw)
    _rows('local network_filters', loc)
    print('')
    print('FINAL: ' + ('ALLOW' if final['allow'] else 'DENY — ' + str(final['reason'])))
    return rep
