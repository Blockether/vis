def __vis_install_net_guard__():
    import socket as _s
    def _norm(x):
        return str(x).strip().lower().rstrip('.').lstrip('.')
    _allowed = set(_norm(d) for d in __vis_allowed_domains__ if _norm(d))
    _denied  = set(_norm(d) for d in __vis_denied_domains__ if _norm(d))
    _allow_specific = set(d for d in _allowed if d != '*')
    _deny_specific  = set(d for d in _denied if d != '*')
    _allow_star = ('*' in _allowed) or (len(_allowed) == 0)
    _deny_star  = ('*' in _denied)
    def _match(h, pats):
        return any(h == d or h.endswith('.' + d) for d in pats)
    def _host_ok(host):
        h = _norm(host)
        if _match(h, _deny_specific):
            return False
        if _match(h, _allow_specific):
            return True
        if _deny_star:
            return False
        return _allow_star
    def _check(host):
        if not _host_ok(host):
            raise PermissionError("vis: network host '%s' is blocked (allowlist=%s, denylist=%s)" % (host, sorted(_allowed) or ['*'], sorted(_denied)))
    def _addr_host(address):
        if isinstance(address, (tuple, list)) and address and isinstance(address[0], str):
            return address[0]
        return None
    def _wrap_dns(orig):
        def g(host, *a, **k):
            _check(host); return orig(host, *a, **k)
        return g
    _s.getaddrinfo = _wrap_dns(_s.getaddrinfo)
    _s.gethostbyname = _wrap_dns(_s.gethostbyname)
    def _wrap_conn(orig):
        def g(self, address, *a, **k):
            h = _addr_host(address)
            if h is not None: _check(h)
            return orig(self, address, *a, **k)
        return g
    try:
        _s.socket.connect = _wrap_conn(_s.socket.connect)
        _s.socket.connect_ex = _wrap_conn(_s.socket.connect_ex)
    except Exception:
        pass
    def _wrap_create(orig):
        def g(address, *a, **k):
            h = _addr_host(address)
            if h is not None: _check(h)
            return orig(address, *a, **k)
        return g
    try:
        _s.create_connection = _wrap_create(_s.create_connection)
    except Exception:
        pass
__vis_install_net_guard__()
