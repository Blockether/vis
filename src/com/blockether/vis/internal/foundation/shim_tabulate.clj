(ns com.blockether.vis.internal.foundation.shim-tabulate
  "Built-in sandbox SHIM: a `tabulate`-compatible module for the model's Python
   sandbox, implemented in PURE Python (stdlib only) — NO host/JVM bridge. The
   `tabulate` PyPI package is not in GraalPy, so agents that reach for
   `from tabulate import tabulate` would otherwise hit ModuleNotFoundError; this
   extension contributes a `:ext/sandbox-shims` entry that env-python installs
   into every sandbox Context (main + every `sub_loop` fork).

   The shim covers the tablefmts agents reach for most: plain, simple, github,
   pipe, orgtbl, presto, grid, fancy_grid, rst, tsv and html, with numeric /
   string alignment, `floatfmt`, `showindex`, and `headers='keys'/'firstrow'`.
   It accepts list-of-lists, list-of-dicts, dict-of-lists, and duck-types the
   pandas shim's DataFrame. A correctness-focused SUBSET, not full tabulate.

   Like `shim-numpy` there are NO `:shim/bindings`: a self-contained Python
   preamble with zero host callables. Publishes a `tabulate` module into
   `sys.modules` (so `import tabulate` works) and staples the `tabulate` fn onto
   builtins."
  (:require [com.blockether.vis.core :as vis]))

(def ^:private tabulate-compat-shim-src
  "Pure-Python preamble that publishes a `tabulate`-compatible module on the
   stdlib. Zero host callables. Published into `sys.modules` under `tabulate` and
   stapled onto builtins. INLINED so it ships in-jar with no separate `.py`
   resource. Installed once per sandbox context, BEFORE the baseline snapshot so
   its `__vis_*` names are filtered from the live-vars view. The Python body uses
   ONLY single-quoted string literals and `chr(...)` for special chars, so this
   Clojure string needs zero backslash escaping."
  "# vis sandbox tabulate-compat shim.
#
# The agent sandbox ships no tabulate wheel. This shim publishes a
# tabulate-compatible module in PURE Python (stdlib only). Covers the common
# tablefmts agents reach for. A correctness-focused SUBSET.

def __vis_install_tabulate__():
    import sys, types, math, builtins as _bi
    _NL = chr(10)
    _TAB = chr(9)

    def _isnum(x):
        return isinstance(x, (int, float)) and not isinstance(x, bool)

    def _fmtcell(x, floatfmt):
        if x is None:
            return ''
        if isinstance(x, float):
            try:
                return ('{:' + floatfmt + '}').format(x)
            except Exception:
                return str(x)
        return str(x)

    def _rows_and_headers(tabular_data, headers):
        rows = []
        hdr = []
        # pandas DataFrame duck-type
        if hasattr(tabular_data, 'to_dict') and hasattr(tabular_data, 'columns'):
            cols = list(tabular_data.columns)
            recs = tabular_data.to_dict('records')
            rows = [[r.get(c) for c in cols] for r in recs]
            if headers == 'keys' or headers == () or headers == []:
                hdr = [str(c) for c in cols]
            elif isinstance(headers, (list, tuple)):
                hdr = [str(h) for h in headers]
            return rows, hdr
        if isinstance(tabular_data, dict):
            cols = list(tabular_data.keys())
            n = max((len(_bi.list(v)) for v in tabular_data.values()), default=0)
            for i in range(n):
                rows.append([tabular_data[c][i] if i < len(tabular_data[c]) else None for c in cols])
            if headers == 'keys':
                hdr = [str(c) for c in cols]
            elif isinstance(headers, (list, tuple)) and headers:
                hdr = [str(h) for h in headers]
            return rows, hdr
        data = [list(r) if isinstance(r, (list, tuple)) else _bi.list(r) if hasattr(r, '__iter__') and not isinstance(r, str) else [r] for r in tabular_data]
        if isinstance(data and data[0], dict) or (data and isinstance(tabular_data[0], dict)):
            keys = []
            for r in tabular_data:
                for k in r.keys():
                    if k not in keys:
                        keys.append(k)
            rows = [[r.get(k) for k in keys] for r in tabular_data]
            if headers == 'keys':
                hdr = [str(k) for k in keys]
            elif isinstance(headers, (list, tuple)) and headers:
                hdr = [str(h) for h in headers]
            return rows, hdr
        rows = data
        if headers == 'firstrow':
            hdr = [str(x) for x in rows[0]] if rows else []
            rows = rows[1:]
        elif isinstance(headers, (list, tuple)) and headers:
            hdr = [str(h) for h in headers]
        return rows, hdr

    def tabulate(tabular_data, headers=(), tablefmt='simple', floatfmt='g',
                 numalign='right', stralign='left', showindex=False, missingval='', colalign=None):
        rows, hdr = _rows_and_headers(tabular_data, headers)
        if showindex is True or showindex == 'always':
            rows = [[i] + list(r) for i, r in enumerate(rows)]
            if hdr:
                hdr = [''] + hdr
        elif isinstance(showindex, (list, tuple)):
            rows = [[showindex[i]] + list(r) for i, r in enumerate(rows)]
            if hdr:
                hdr = [''] + hdr
        ncol = max([len(r) for r in rows] + [len(hdr)] + [0])
        colnum = []
        for j in range(ncol):
            colnum.append(all(_isnum(r[j]) for r in rows if j < len(r) and r[j] is not None) and any(j < len(r) and r[j] is not None for r in rows))
        srows = []
        for r in rows:
            cells = []
            for j in range(ncol):
                v = r[j] if j < len(r) else None
                cells.append(missingval if v is None else _fmtcell(v, floatfmt))
            srows.append(cells)
        widths = []
        for j in range(ncol):
            w = max([len(hdr[j]) if j < len(hdr) else 0] + [len(sr[j]) for sr in srows] + [0])
            widths.append(w)

        def _align(text, j):
            if colalign and j < len(colalign) and colalign[j]:
                al = colalign[j]
            elif colnum[j]:
                al = numalign
            else:
                al = stralign
            if al == 'right':
                return text.rjust(widths[j])
            if al == 'center':
                return text.center(widths[j])
            return text.ljust(widths[j])

        def _line(cells):
            return [_align(cells[j] if j < len(cells) else '', j) for j in range(ncol)]

        fmt = tablefmt
        out = []
        if fmt in ('plain',):
            if hdr:
                out.append('  '.join(_line(hdr)).rstrip())
            for sr in srows:
                out.append('  '.join(_line(sr)).rstrip())
            return _NL.join(out)
        if fmt in ('simple', 'presto', 'orgtbl'):
            sep = ' | ' if fmt in ('presto', 'orgtbl') else '  '
            if hdr:
                out.append(sep.join(_line(hdr)).rstrip() if fmt == 'simple' else sep.join(_line(hdr)))
            rule = ('-+-' if fmt in ('presto',) else '-+-' if fmt == 'orgtbl' else '  ').join('-' * widths[j] for j in range(ncol)) if fmt != 'simple' else '  '.join('-' * widths[j] for j in range(ncol))
            if fmt == 'orgtbl':
                rule = '|-' + '-+-'.join('-' * widths[j] for j in range(ncol)) + '-|'
            out.append(rule)
            for sr in srows:
                line = sep.join(_line(sr))
                if fmt == 'orgtbl':
                    line = '| ' + ' | '.join(_line(sr)) + ' |'
                out.append(line.rstrip() if fmt == 'simple' else line)
            if fmt == 'orgtbl' and hdr:
                out[0] = '| ' + ' | '.join(_line(hdr)) + ' |'
            return _NL.join(out)
        if fmt in ('github', 'pipe'):
            def row(cells):
                return '| ' + ' | '.join(_line(cells)) + ' |'
            if hdr:
                out.append(row(hdr))
            aligns = []
            for j in range(ncol):
                al = colalign[j] if (colalign and j < len(colalign) and colalign[j]) else (numalign if colnum[j] else stralign)
                dash = '-' * max(3, widths[j])
                if al == 'right':
                    aligns.append(dash[:-1] + ':')
                elif al == 'center':
                    aligns.append(':' + dash[1:-1] + ':')
                else:
                    aligns.append(':' + dash[1:] if fmt == 'pipe' else dash)
            out.append('| ' + ' | '.join(aligns) + ' |')
            for sr in srows:
                out.append(row(sr))
            return _NL.join(out)
        if fmt in ('grid', 'fancy_grid'):
            if fmt == 'fancy_grid':
                ch = {'h': chr(9472), 'v': chr(9474), 'tl': chr(9484), 'tr': chr(9488),
                      'bl': chr(9492), 'br': chr(9496), 'lm': chr(9500), 'rm': chr(9508),
                      'tm': chr(9516), 'bm': chr(9524), 'cm': chr(9532)}
            else:
                ch = {'h': '-', 'v': '|', 'tl': '+', 'tr': '+', 'bl': '+', 'br': '+',
                      'lm': '+', 'rm': '+', 'tm': '+', 'bm': '+', 'cm': '+'}
            def rule(l, m, r):
                return l + m.join(ch['h'] * (widths[j] + 2) for j in range(ncol)) + r
            def row(cells):
                return ch['v'] + ch['v'].join(' ' + _align(cells[j] if j < len(cells) else '', j) + ' ' for j in range(ncol)) + ch['v']
            out.append(rule(ch['tl'], ch['tm'], ch['tr']))
            if hdr:
                out.append(row(hdr))
                out.append(rule(ch['lm'], ch['cm'], ch['rm']))
            for sr in srows:
                out.append(row(sr))
            out.append(rule(ch['bl'], ch['bm'], ch['br']))
            return _NL.join(out)
        if fmt in ('rst',):
            def rule():
                return '  '.join('=' * widths[j] for j in range(ncol))
            out.append(rule())
            if hdr:
                out.append('  '.join(_line(hdr)))
                out.append(rule())
            for sr in srows:
                out.append('  '.join(_line(sr)))
            out.append(rule())
            return _NL.join(out)
        if fmt in ('tsv',):
            if hdr:
                out.append(_TAB.join(hdr))
            for sr in srows:
                out.append(_TAB.join(sr))
            return _NL.join(out)
        if fmt in ('html', 'unsafehtml'):
            lt = chr(60)
            gt = chr(62)
            def tag(t, inner):
                return lt + t + gt + inner + lt + '/' + t + gt
            html = [lt + 'table' + gt]
            if hdr:
                html.append(lt + 'thead' + gt + lt + 'tr' + gt + ''.join(tag('th', h) for h in hdr) + lt + '/tr' + gt + lt + '/thead' + gt)
            html.append(lt + 'tbody' + gt)
            for sr in srows:
                html.append(lt + 'tr' + gt + ''.join(tag('td', c) for c in sr) + lt + '/tr' + gt)
            html.append(lt + '/tbody' + gt + lt + '/table' + gt)
            return _NL.join(html)
        # fallback = simple
        return tabulate(tabular_data, headers, 'simple', floatfmt, numalign, stralign, showindex, missingval)

    mod = types.ModuleType('tabulate')
    mod.tabulate = tabulate
    mod.tabulate_formats = ['plain', 'simple', 'github', 'pipe', 'orgtbl', 'presto',
                            'grid', 'fancy_grid', 'rst', 'tsv', 'html']
    mod.__version__ = '0.9.0-vis-shim'
    sys.modules['tabulate'] = mod
    try:
        _bi.tabulate = tabulate
    except Exception:
        pass

__vis_install_tabulate__()
del __vis_install_tabulate__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-tabulate"
     :ext/description
     "Sandbox shim: a `tabulate`-compatible module (from tabulate import tabulate) implemented in PURE Python. Renders list-of-lists / list-of-dicts / dict-of-lists / DataFrame as plain/simple/github/pipe/grid/fancy_grid/rst/tsv/html tables with alignment and floatfmt. No pip, no native wheel, no host bridge. A correctness-focused subset."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "tabulate"
       :shim/description
       "tabulate-compatible `tabulate` in pure Python. Not supported: only common tablefmts (plain/simple/github/pipe/grid/fancy_grid/rst/tsv/html); exotic formats and colored output."
       :shim/preamble tabulate-compat-shim-src}]}))

(vis/register-extension! vis-extension)
