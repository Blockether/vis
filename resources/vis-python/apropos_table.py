def __vis_apropos_table__(query=''):
    hidden = set(globals().get('__vis_advertised_native_tools__') or ())
    d = {k: v for k, v in apropos(query).items() if k not in hidden}
    if not d:
        return 'apropos(' + repr(query) + '): no unadvertised capabilities match.'
    def __cell(s):
        return str(s).replace('\n', ' ').replace('|', '\\|')
    rows = ['| capability | gist |', '| --- | --- |']
    for k in d:
        rows.append('| `' + __cell(k) + '` | ' + __cell(d[k]) + ' |')
    return '\n'.join(rows)
