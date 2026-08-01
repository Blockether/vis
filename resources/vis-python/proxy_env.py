def __vis_install_proxy_env__():
    import os as _o
    _u = __vis_proxy_url__
    _ca = __vis_ca_file__
    for _k in ('http_proxy','https_proxy','HTTP_PROXY','HTTPS_PROXY','all_proxy','ALL_PROXY'):
        _o.environ[_k] = _u
    for _k in ('no_proxy','NO_PROXY'):
        _o.environ[_k] = 'localhost,127.0.0.1,::1'
    if _ca:
        for _k in ('REQUESTS_CA_BUNDLE','SSL_CERT_FILE','CURL_CA_BUNDLE','PIP_CERT'):
            _o.environ[_k] = _ca
__vis_install_proxy_env__()
