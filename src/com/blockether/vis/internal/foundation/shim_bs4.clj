(ns com.blockether.vis.internal.foundation.shim-bs4
  "Built-in sandbox SHIM: a `bs4` (BeautifulSoup)-compatible module for the
   model's Python sandbox, implemented in PURE Python on the stdlib
   `html.parser` — NO host/JVM bridge, NOT a line of Clojure or babashka. bs4 is
   a third-party wheel that does not ship in GraalPy, so agents that reach for
   `from bs4 import BeautifulSoup` (the natural partner to the `requests` shim:
   fetch then parse) would otherwise hit ModuleNotFoundError; this extension
   contributes a `:ext/sandbox-shims` entry that `env-python/build-agent-context`
   installs into every sandbox Context (main + every `sub_loop` fork).

   It builds a `Tag` / `NavigableString` tree via `html.parser`, with
   `find`/`find_all` (name/attrs/class_/id/string/recursive/limit), CSS `.select`
   / `.select_one` (type / `#id` / `.class` / `[attr]`/`[attr=v]`/`~=`/`^=`/`$=`/
   `*=`, descendant + `>` child combinators, comma groups), `get_text`,
   `.string`/`.strings`/`.stripped_strings`, sibling/parent navigation, dynamic
   `soup.tagname` access, and HTML serialization. A deliberate subset of full
   bs4 (no lxml, no advanced CSS pseudo-classes).

   Like `shim-requests` there are NO `:shim/bindings`: the shim is a
   self-contained Python preamble with zero host callables. It publishes a `bs4`
   module (+ `bs4.element`) into `sys.modules` (so `from bs4 import BeautifulSoup`
   works) and staples `BeautifulSoup` onto builtins."
  (:require [com.blockether.vis.core :as vis]))

(def ^:private bs4-compat-shim-src
  "Pure-Python preamble that publishes a `bs4`-compatible module implemented on
   the stdlib `html.parser`. Zero host callables. Published into `sys.modules`
   under `bs4` / `bs4.element` (so `from bs4 import BeautifulSoup` finds it) AND
   `BeautifulSoup` stapled onto builtins. INLINED here so it ships in-jar with no
   separate `.py` resource. Installed once per sandbox context (main + every
   `sub_loop` fork), BEFORE the baseline snapshot so its `__vis_*` names are
   filtered out of the model-visible live-vars view. The Python body uses ONLY
   single-quoted string literals and `chr(...)` for any special char, so this
   Clojure string needs zero backslash escaping."
  "# vis sandbox beautifulsoup4-compat shim.
#
# The agent sandbox ships no bs4 wheel. This shim publishes a BeautifulSoup-compatible
# `bs4` module implemented in PURE Python on the stdlib html.parser (no host/JVM
# bridge), the natural partner to the requests shim (fetch then parse). It builds a
# Tag / NavigableString tree with find/find_all, CSS .select, get_text and HTML
# serialization. A deliberate subset of Pillow-grade breadth, not full bs4. Published
# into sys.modules so `from bs4 import BeautifulSoup` works, and stapled onto builtins.

def __vis_install_bs4__():
    import sys, types
    import html.parser as _hp
    import builtins as _bi

    _Q = chr(34)
    _LT = chr(60)
    _GT = chr(62)
    _AMP = chr(38)
    _NL = chr(10)

    _VOID = set(['area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input',
                 'keygen', 'link', 'meta', 'param', 'source', 'track', 'wbr'])
    _MULTI_ATTR = set(['class', 'rel', 'rev', 'accept-charset', 'headers',
                       'dropzone'])

    class NavigableString(str):
        def __new__(cls, value):
            s = str.__new__(cls, value)
            s.parent = None
            return s
        @property
        def name(self):
            return None
        @property
        def string(self):
            return self
        @property
        def text(self):
            return str(self)
        @property
        def next_sibling(self):
            return _sibling(self, 1)
        @property
        def previous_sibling(self):
            return _sibling(self, -1)
        def get_text(self, separator='', strip=False):
            return self.strip() if strip else str(self)
        def strip_str(self):
            return str.strip(self)
        def extract(self):
            _detach(self)
            return self

    class Comment(NavigableString):
        pass

    class Tag:
        def __init__(self, name, attrs=None):
            self.name = name
            self.attrs = {}
            if attrs:
                for k, v in (attrs.items() if isinstance(attrs, dict) else attrs):
                    self.attrs[k] = self._norm_attr(k, v)
            self.contents = []
            self.parent = None

        def _norm_attr(self, k, v):
            if v is None:
                v = ''
            if k in _MULTI_ATTR and isinstance(v, str):
                return v.split()
            return v

        # -- attribute access ---------------------------------------------------
        def __getitem__(self, key):
            return self.attrs[key]
        def __setitem__(self, key, value):
            self.attrs[key] = self._norm_attr(key, value)
        def __delitem__(self, key):
            del self.attrs[key]
        def __contains__(self, key):
            return key in self.attrs
        def get(self, key, default=None):
            return self.attrs.get(key, default)
        def has_attr(self, key):
            return key in self.attrs
        def get_attribute_list(self, key, default=None):
            v = self.attrs.get(key, default)
            return v if isinstance(v, list) else [v]

        # -- tree ---------------------------------------------------------------
        @property
        def children(self):
            return iter(list(self.contents))
        @property
        def descendants(self):
            for c in list(self.contents):
                yield c
                if isinstance(c, Tag):
                    for d in c.descendants:
                        yield d
        @property
        def contents_tags(self):
            return [c for c in self.contents if isinstance(c, Tag)]
        @property
        def next_sibling(self):
            return _sibling(self, 1)
        @property
        def previous_sibling(self):
            return _sibling(self, -1)
        @property
        def next_element(self):
            if self.contents:
                return self.contents[0]
            return _sibling(self, 1)
        @property
        def parents(self):
            p = self.parent
            while p is not None:
                yield p
                p = p.parent

        def append(self, node):
            if isinstance(node, str) and not isinstance(node, NavigableString):
                node = NavigableString(node)
            node.parent = self
            self.contents.append(node)

        def insert(self, position, node):
            if isinstance(node, str) and not isinstance(node, NavigableString):
                node = NavigableString(node)
            node.parent = self
            self.contents.insert(position, node)
        def _sib_insert(self, node, offset):
            p = self.parent
            if p is None:
                return
            if isinstance(node, str) and not isinstance(node, NavigableString):
                node = NavigableString(node)
            idx = _index_of(p.contents, self)
            node.parent = p
            p.contents.insert(idx + offset, node)
        def insert_before(self, *nodes):
            for n in nodes:
                self._sib_insert(n, 0)
        def insert_after(self, *nodes):
            for n in reversed(nodes):
                self._sib_insert(n, 1)
        def replace_with(self, new):
            p = self.parent
            if p is None:
                return self
            if isinstance(new, str) and not isinstance(new, NavigableString):
                new = NavigableString(new)
            idx = _index_of(p.contents, self)
            new.parent = p
            p.contents[idx] = new
            self.parent = None
            return self
        replaceWith = replace_with
        def wrap(self, wrapper):
            p = self.parent
            if p is not None:
                idx = _index_of(p.contents, self)
                p.contents[idx] = wrapper
                wrapper.parent = p
            self.parent = wrapper
            wrapper.contents.append(self)
            return wrapper
        def unwrap(self):
            p = self.parent
            if p is None:
                return self
            idx = _index_of(p.contents, self)
            p.contents[idx:idx + 1] = self.contents
            for c in self.contents:
                c.parent = p
            self.parent = None
            return self
        replaceWithChildren = unwrap

        # -- text ---------------------------------------------------------------
        def get_text(self, separator='', strip=False):
            parts = []
            for d in self.descendants:
                if isinstance(d, NavigableString) and not isinstance(d, Comment):
                    t = d.strip() if strip else str(d)
                    if t or not strip:
                        parts.append(t)
            return separator.join(parts)
        @property
        def text(self):
            return self.get_text()
        @property
        def string(self):
            kids = [c for c in self.contents]
            if len(kids) == 1:
                if isinstance(kids[0], NavigableString):
                    return kids[0]
                if isinstance(kids[0], Tag):
                    return kids[0].string
            return None
        @property
        def strings(self):
            for d in self.descendants:
                if isinstance(d, NavigableString) and not isinstance(d, Comment):
                    yield d
        @property
        def stripped_strings(self):
            for s in self.strings:
                t = s.strip()
                if t:
                    yield t

        # -- search -------------------------------------------------------------
        def find(self, name=None, attrs=None, recursive=True, string=None, **kwargs):
            res = self.find_all(name, attrs, recursive, string, 1, **kwargs)
            return res[0] if res else None

        def find_all(self, name=None, attrs=None, recursive=True, string=None,
                     limit=None, **kwargs):
            matcher = _make_matcher(name, attrs, string, kwargs)
            out = []
            src = self.descendants if recursive else self.children
            for node in src:
                if matcher(node):
                    out.append(node)
                    if limit and len(out) >= limit:
                        break
            return out

        findAll = find_all
        findChildren = find_all

        def find_next_sibling(self, name=None, attrs=None, **kwargs):
            matcher = _make_matcher(name, attrs, None, kwargs)
            sib = self.next_sibling
            while sib is not None:
                if matcher(sib):
                    return sib
                sib = _sibling(sib, 1)
            return None

        def find_parent(self, name=None, attrs=None, **kwargs):
            matcher = _make_matcher(name, attrs, None, kwargs)
            for p in self.parents:
                if matcher(p):
                    return p
            return None

        findParent = find_parent

        def select(self, selector):
            return _select(self, selector)
        def select_one(self, selector):
            r = _select(self, selector, limit=1)
            return r[0] if r else None

        # -- mutation -----------------------------------------------------------
        def extract(self):
            _detach(self)
            return self
        def decompose(self):
            _detach(self)
            self.contents = []
        def clear(self):
            self.contents = []

        # -- dynamic tag attribute access (soup.a etc.) -------------------------
        def __getattr__(self, key):
            if key.startswith('__') and key.endswith('__'):
                raise AttributeError(key)
            found = self.find(key)
            if found is not None:
                return found
            return None

        # -- serialization ------------------------------------------------------
        def decode(self):
            return _render(self)
        def prettify(self):
            return _render(self, pretty=True, depth=0)
        def __repr__(self):
            return _render(self)
        def __str__(self):
            return _render(self)

    def _index_of(seq, node):
        for i, c in enumerate(seq):
            if c is node:
                return i
        return -1

    def _detach(node):
        p = getattr(node, 'parent', None)
        if p is not None and node in p.contents:
            p.contents.remove(node)
        node.parent = None

    def _sibling(node, direction):
        p = getattr(node, 'parent', None)
        if p is None:
            return None
        try:
            i = p.contents.index(node)
        except ValueError:
            return None
        j = i + direction
        if 0 <= j < len(p.contents):
            return p.contents[j]
        return None

    def _attr_str(node, key):
        v = node.attrs.get(key)
        if isinstance(v, list):
            return ' '.join(v)
        return v if v is not None else ''

    def _make_matcher(name, attrs, string, kwargs):
        attrs = dict(attrs) if attrs else {}
        for k, v in kwargs.items():
            key = 'class' if k == 'class_' else k
            attrs[key] = v

        def _name_ok(node):
            if name is None or name is True:
                return isinstance(node, Tag) if name is True else True
            if isinstance(node, str):
                nm = None
            else:
                nm = node.name
            if hasattr(name, 'search'):
                return nm is not None and name.search(nm) is not None
            if callable(name):
                return isinstance(node, Tag) and name(node)
            if isinstance(name, (list, tuple, set)):
                return nm in name
            return nm == name

        def _attr_ok(node):
            if not isinstance(node, Tag):
                return not attrs
            for k, want in attrs.items():
                have = node.attrs.get(k)
                if k == 'class':
                    classes = have if isinstance(have, list) else ([have] if have else [])
                    if isinstance(want, str):
                        if want not in classes:
                            return False
                    elif callable(want):
                        if not want(' '.join(classes)):
                            return False
                    else:
                        for w in want:
                            if w not in classes:
                                return False
                    continue
                hv = ' '.join(have) if isinstance(have, list) else have
                if want is True:
                    if k not in node.attrs:
                        return False
                elif callable(want):
                    if not want(hv):
                        return False
                elif isinstance(want, (list, tuple, set)):
                    if hv not in want:
                        return False
                elif hasattr(want, 'search'):
                    if hv is None or want.search(hv) is None:
                        return False
                else:
                    if hv != want:
                        return False
            return True

        def _string_ok(node):
            if string is None:
                return True
            txt = node.get_text() if isinstance(node, Tag) else str(node)
            if hasattr(string, 'search'):
                return string.search(txt) is not None
            if callable(string):
                return string(txt)
            if isinstance(string, (list, tuple, set)):
                return txt in string
            return txt == string

        def matcher(node):
            if string is not None and name is None and not attrs:
                if isinstance(node, NavigableString):
                    return _string_ok(node)
                return False
            if not _name_ok(node):
                return False
            if not _attr_ok(node):
                return False
            if not _string_ok(node):
                return False
            return isinstance(node, Tag) or (string is not None)

        return matcher

    # -- CSS select --------------------------------------------------------------
    def _parse_simple(tok):
        tag = None
        idv = None
        classes = []
        attrs = []
        i = 0
        n = len(tok)
        # leading type selector
        j = i
        while j < n and tok[j] not in ('.', '#', '['):
            j = j + 1
        t = tok[i:j]
        if t and t != '*':
            tag = t
        i = j
        while i < n:
            c = tok[i]
            if c == '.':
                i = i + 1
                s = i
                while i < n and tok[i] not in ('.', '#', '['):
                    i = i + 1
                classes.append(tok[s:i])
            elif c == '#':
                i = i + 1
                s = i
                while i < n and tok[i] not in ('.', '#', '['):
                    i = i + 1
                idv = tok[s:i]
            elif c == '[':
                i = i + 1
                s = i
                while i < n and tok[i] != ']':
                    i = i + 1
                body = tok[s:i]
                i = i + 1
                op = None
                for cand in ('~=', '^=', '$=', '*=', '='):
                    if cand in body:
                        an, av = body.split(cand, 1)
                        op = cand
                        av = av.strip()
                        if len(av) >= 2 and av[0] == _Q and av[-1] == _Q:
                            av = av[1:-1]
                        if len(av) >= 2 and av[0] == chr(39) and av[-1] == chr(39):
                            av = av[1:-1]
                        attrs.append((an.strip(), op, av))
                        break
                if op is None:
                    attrs.append((body.strip(), None, None))
            else:
                i = i + 1
        return (tag, idv, classes, attrs)

    def _simple_match(node, simple):
        if not isinstance(node, Tag):
            return False
        tag, idv, classes, attrs = simple
        if tag is not None and node.name != tag:
            return False
        if idv is not None and node.attrs.get('id') != idv:
            return False
        if classes:
            have = node.attrs.get('class')
            have = have if isinstance(have, list) else ([have] if have else [])
            for c in classes:
                if c not in have:
                    return False
        for an, op, av in attrs:
            hv = node.attrs.get(an)
            if hv is None and an not in node.attrs:
                return False
            hvs = ' '.join(hv) if isinstance(hv, list) else (hv if hv is not None else '')
            if op is None:
                continue
            if op == '=' and hvs != av:
                return False
            if op == '~=' and av not in hvs.split():
                return False
            if op == '^=' and not hvs.startswith(av):
                return False
            if op == '$=' and not hvs.endswith(av):
                return False
            if op == '*=' and av not in hvs:
                return False
        return True

    def _tokenize_group(group):
        # returns list of (combinator, token); combinator in ('desc','child')
        steps = []
        parts = group.replace(_GT, ' ' + _GT + ' ').split()
        combinator = 'desc'
        for p in parts:
            if p == _GT:
                combinator = 'child'
                continue
            steps.append((combinator, p))
            combinator = 'desc'
        return steps

    def _select(root, selector, limit=None):
        results = []
        seen = set()
        for group in selector.split(','):
            group = group.strip()
            if not group:
                continue
            steps = _tokenize_group(group)
            current = [root]
            for combinator, tok in steps:
                simple = _parse_simple(tok)
                nxt = []
                if combinator == 'child':
                    for node in current:
                        for c in (node.contents if isinstance(node, Tag) else []):
                            if _simple_match(c, simple):
                                nxt.append(c)
                else:
                    for node in current:
                        for d in (node.descendants if isinstance(node, Tag) else []):
                            if _simple_match(d, simple):
                                nxt.append(d)
                current = nxt
            for node in current:
                if id(node) not in seen:
                    seen.add(id(node))
                    results.append(node)
                    if limit and len(results) >= limit:
                        return results
        return results

    # -- serialization -----------------------------------------------------------
    def _esc_text(s):
        return s.replace(_AMP, _AMP + 'amp;').replace(_LT, _AMP + 'lt;').replace(_GT, _AMP + 'gt;')

    def _esc_attr(s):
        return s.replace(_AMP, _AMP + 'amp;').replace(_Q, _AMP + 'quot;')

    def _render(node, pretty=False, depth=0):
        if isinstance(node, Comment):
            body = _LT + '!--' + str(node) + '--' + _GT
            return (('  ' * depth) + body) if pretty else body
        if isinstance(node, NavigableString):
            if pretty:
                t = str(node).strip()
                return ('  ' * depth) + _esc_text(t) if t else ''
            return _esc_text(str(node))
        nl = _NL if pretty else ''
        if node.name == '[document]':
            kids = [_render(c, pretty, depth) for c in node.contents]
            kids = [k for k in kids if k != '']
            return nl.join(kids)
        pad = ('  ' * depth) if pretty else ''
        parts = [pad + _LT + node.name]
        for k, v in node.attrs.items():
            vs = ' '.join(v) if isinstance(v, list) else (v if v is not None else '')
            parts.append(' ' + k + '=' + _Q + _esc_attr(vs) + _Q)
        if node.name in _VOID and not node.contents:
            parts.append('/' + _GT)
            return ''.join(parts)
        parts.append(_GT)
        open_tag = ''.join(parts)
        close_tag = _LT + '/' + node.name + _GT
        if not pretty:
            inner = ''.join(_render(c, pretty, depth + 1) for c in node.contents)
            return open_tag + inner + close_tag
        kids = [_render(c, pretty, depth + 1) for c in node.contents]
        kids = [k for k in kids if k != '']
        if not kids:
            return open_tag + close_tag
        return open_tag + nl + nl.join(kids) + nl + pad + close_tag

    # -- parser ------------------------------------------------------------------
    class _Builder(_hp.HTMLParser):
        def __init__(self):
            _hp.HTMLParser.__init__(self, convert_charrefs=True)
            self.root = Tag('[document]')
            self.stack = [self.root]
        def _cur(self):
            return self.stack[-1]
        def handle_starttag(self, tag, attrs):
            t = Tag(tag, attrs)
            self._cur().append(t)
            if tag not in _VOID:
                self.stack.append(t)
        def handle_startendtag(self, tag, attrs):
            t = Tag(tag, attrs)
            self._cur().append(t)
        def handle_endtag(self, tag):
            for i in range(len(self.stack) - 1, 0, -1):
                if self.stack[i].name == tag:
                    del self.stack[i:]
                    return
        def handle_data(self, data):
            self._cur().append(NavigableString(data))
        def handle_comment(self, data):
            self._cur().append(Comment(data))
        def handle_entityref(self, name):
            self._cur().append(NavigableString(_AMP + name + ';'))

    class BeautifulSoup(Tag):
        def __init__(self, markup='', features=None, *args, **kwargs):
            Tag.__init__(self, '[document]')
            if hasattr(markup, 'read'):
                markup = markup.read()
            if isinstance(markup, bytes):
                markup = markup.decode('utf-8', 'replace')
            b = _Builder()
            b.feed(markup or '')
            b.close()
            self.contents = b.root.contents
            for c in self.contents:
                c.parent = self
        def new_tag(self, name, namespace=None, nsprefix=None, attrs=None, **kwattrs):
            a = dict(attrs) if attrs else {}
            a.update(kwattrs)
            return Tag(name, a)
        def new_string(self, s, subclass=None):
            return (subclass or NavigableString)(s)
        def decode(self, *a, **k):
            return _render(self)
        def __str__(self):
            return _render(self)
        def __repr__(self):
            return _render(self)

    mod = types.ModuleType('bs4')
    mod.__version__ = '4.12-vis-pure'
    mod.BeautifulSoup = BeautifulSoup
    mod.Tag = Tag
    mod.NavigableString = NavigableString
    mod.Comment = Comment

    elem = types.ModuleType('bs4.element')
    elem.Tag = Tag
    elem.NavigableString = NavigableString
    elem.Comment = Comment
    mod.element = elem

    sys.modules['bs4'] = mod
    sys.modules['bs4.element'] = elem

    try:
        import builtins as _b
        _b.bs4 = mod
        _b.BeautifulSoup = BeautifulSoup
    except Exception:
        pass

__vis_install_bs4__()
del __vis_install_bs4__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-bs4"
     :ext/description
     "Sandbox shim: a `bs4` (BeautifulSoup)-compatible module (from bs4 import BeautifulSoup) implemented in PURE Python on the stdlib html.parser — find/find_all, CSS .select/.select_one, get_text, sibling/parent navigation, HTML serialization. The natural partner to the requests shim (fetch then parse). No pip, no native wheel, no host bridge."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "bs4"
       :shim/description
       "BeautifulSoup-compatible `bs4` module (find/find_all/select/get_text) implemented in pure Python on stdlib html.parser. No host bridge."
       :shim/preamble bs4-compat-shim-src}]}))

(vis/register-extension! vis-extension)
