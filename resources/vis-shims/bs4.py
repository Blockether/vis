# vis sandbox beautifulsoup4-compat shim.
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

    _VOID = set(
        [
            "area",
            "base",
            "br",
            "col",
            "embed",
            "hr",
            "img",
            "input",
            "keygen",
            "link",
            "meta",
            "param",
            "source",
            "track",
            "wbr",
        ]
    )
    _MULTI_ATTR = set(["class", "rel", "rev", "accept-charset", "headers", "dropzone"])

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
        def next_element(self):
            return _next_element(self)

        @property
        def previous_sibling(self):
            return _sibling(self, -1)

        def get_text(self, separator="", strip=False):
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
                for k, v in attrs.items() if isinstance(attrs, dict) else attrs:
                    self.attrs[k] = self._norm_attr(k, v)
            self.contents = []
            self.parent = None

        def _norm_attr(self, k, v):
            if v is None:
                v = ""
            if k in _MULTI_ATTR and isinstance(v, str):
                return v.split()
            return v

        # -- attribute access ---------------------------------------------------
        def __getitem__(self, key):
            return self.attrs[key]

        def __setitem__(self, key, value):
            # Assignment preserves the caller's value; only parsed markup gets
            # HTML multi-valued-attribute normalization.
            self.attrs[key] = "" if value is None else value

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
            # Iterative pre-order walk: no recursion frame per nesting level
            # (deep markup would otherwise exhaust the interpreter stack) and no
            # per-level copy of the child list.
            stack = list(self.contents)
            stack.reverse()
            while stack:
                node = stack.pop()
                yield node
                kids = node.contents if isinstance(node, Tag) else None
                if kids:
                    for c in reversed(kids):
                        stack.append(c)

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
            return _next_element(self)

        @property
        def parents(self):
            p = self.parent
            while p is not None:
                yield p
                p = p.parent

        def append(self, node):
            # A document is a container, not an element: inserting one moves its
            # children into this tag, matching BeautifulSoup's fragment behavior.
            if isinstance(node, Tag) and node.name == "[document]":
                self.extend(list(node.contents))
                return
            node = _adopt(self, node)
            self.contents.append(node)

        def extend(self, nodes):
            for node in nodes:
                self.append(node)

        def index(self, element):
            i = _index_of(self.contents, element)
            if i < 0:
                raise ValueError("%r is not in list" % (element,))
            return i

        def insert(self, position, node):
            if node is None:
                raise ValueError("Cannot insert None into a tag")
            if isinstance(node, Tag) and node.name == "[document]":
                for child in list(node.contents):
                    self.insert(position, child)
                    position += 1
                return
            node = _adopt(self, node)
            self.contents.insert(position, node)

        def _sib_insert(self, node, offset):
            p = self.parent
            if p is None:
                raise ValueError("Element has no parent")
            if node is self:
                raise ValueError("Cannot insert an element before or after itself")
            idx = _index_of(p.contents, self)
            node = _adopt(p, node)
            # Moving an earlier sibling left-shifts the insertion point.
            if _index_of(p.contents, self) != idx:
                idx = _index_of(p.contents, self)
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
                raise ValueError("Element has no parent")
            if new is self:
                raise ValueError("Cannot replace an element with itself")
            idx = _index_of(p.contents, self)
            new = _adopt(p, new)
            # `_adopt` may have moved an earlier sibling out of this parent.
            idx = _index_of(p.contents, self)
            p.contents[idx] = new
            self.parent = None
            return self

        def wrap(self, wrapper):
            p = self.parent
            if wrapper is self:
                raise ValueError("Cannot wrap a tag in itself")
            if isinstance(wrapper, Tag):
                _require_acyclic(wrapper, self)
                _detach(wrapper)
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
            children = self.contents
            p.contents[idx : idx + 1] = children
            for c in children:
                c.parent = p
            self.contents = []
            self.parent = None
            return self

        # -- text ---------------------------------------------------------------
        def get_text(self, separator="", strip=False):
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

        def find_all(
            self,
            name=None,
            attrs=None,
            recursive=True,
            string=None,
            limit=None,
            **kwargs,
        ):
            if string is None:
                string = kwargs.pop("string", kwargs.pop("text", None))
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
            # A decomposed subtree is no longer connected to the document at any
            # depth. Clear descendant links iteratively for deep markup too.
            stack = list(self.contents)
            self.contents = []
            while stack:
                c = stack.pop()
                c.parent = None
                if isinstance(c, Tag):
                    stack.extend(c.contents)
                    c.contents = []

        def smooth(self):
            i = 0
            while i + 1 < len(self.contents):
                left, right = self.contents[i : i + 2]
                if type(left) is NavigableString and type(right) is NavigableString:
                    merged = NavigableString(str(left) + str(right))
                    merged.parent = self
                    self.contents[i : i + 2] = [merged]
                else:
                    i += 1
            return None

        def clear(self):
            for c in self.contents:
                c.parent = None
            self.contents = []

        def __getattr__(self, key):
            if key.startswith("__") and key.endswith("__"):
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

    def _require_acyclic(parent, node):
        """Reject inserting a tag into itself or one of its descendants."""
        if not isinstance(node, Tag):
            return
        cur = parent
        while cur is not None:
            if cur is node:
                raise ValueError("Cannot insert a tag into itself")
            cur = cur.parent

    def _adopt(parent, node):
        if isinstance(node, str) and not isinstance(node, NavigableString):
            node = NavigableString(node)
        _require_acyclic(parent, node)
        _detach(node)
        node.parent = parent
        return node

    def _detach(node):
        p = getattr(node, "parent", None)
        if p is not None:
            i = _index_of(p.contents, node)
            if i >= 0:
                del p.contents[i]
        node.parent = None

    def _sibling(node, direction):
        p = getattr(node, "parent", None)
        if p is None:
            return None
        # Identity, not equality: two equal NavigableStrings under one parent are
        # still distinct nodes, and list.index would resolve to the first one.
        i = _index_of(p.contents, node)
        if i < 0:
            return None
        j = i + direction
        if 0 <= j < len(p.contents):
            return p.contents[j]
        return None

    def _next_element(node):
        if isinstance(node, Tag) and node.contents:
            return node.contents[0]
        cur = node
        while cur is not None:
            sibling = _sibling(cur, 1)
            if sibling is not None:
                return sibling
            cur = getattr(cur, "parent", None)
        return None

    def _attr_str(node, key):
        v = node.attrs.get(key)
        if isinstance(v, list):
            return " ".join(v)
        return v if v is not None else ""

    def _make_matcher(name, attrs, string, kwargs):
        attrs = dict(attrs) if attrs else {}
        for k, v in kwargs.items():
            key = "class" if k == "class_" else k
            attrs[key] = v

        def _name_ok(node):
            if name is None or name is True:
                return isinstance(node, Tag) if name is True else True
            if isinstance(node, str):
                nm = None
            else:
                nm = node.name
            if hasattr(name, "search"):
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
                if k == "class":
                    classes = (
                        have if isinstance(have, list) else ([have] if have else [])
                    )
                    if isinstance(want, str):
                        if want not in classes:
                            return False
                    elif callable(want):
                        if not want(" ".join(classes)):
                            return False
                    else:
                        for w in want:
                            if w not in classes:
                                return False
                    continue
                hv = " ".join(have) if isinstance(have, list) else have
                if want is True:
                    if k not in node.attrs:
                        return False
                elif callable(want):
                    if not want(hv):
                        return False
                elif isinstance(want, (list, tuple, set)):
                    if hv not in want:
                        return False
                elif hasattr(want, "search"):
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
            if hasattr(string, "search"):
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
        while j < n and tok[j] not in (".", "#", "["):
            j = j + 1
        t = tok[i:j]
        if t and t != "*":
            tag = t
        i = j
        while i < n:
            c = tok[i]
            if c == ".":
                i = i + 1
                s = i
                while i < n and tok[i] not in (".", "#", "["):
                    i = i + 1
                classes.append(tok[s:i])
            elif c == "#":
                i = i + 1
                s = i
                while i < n and tok[i] not in (".", "#", "["):
                    i = i + 1
                idv = tok[s:i]
            elif c == "[":
                i = i + 1
                s = i
                while i < n and tok[i] != "]":
                    i = i + 1
                body = tok[s:i]
                i = i + 1
                op = None
                for cand in ("~=", "^=", "$=", "*=", "="):
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
        if idv is not None and node.attrs.get("id") != idv:
            return False
        if classes:
            have = node.attrs.get("class")
            have = have if isinstance(have, list) else ([have] if have else [])
            for c in classes:
                if c not in have:
                    return False
        for an, op, av in attrs:
            hv = node.attrs.get(an)
            if hv is None and an not in node.attrs:
                return False
            hvs = (
                " ".join(hv) if isinstance(hv, list) else (hv if hv is not None else "")
            )
            if op is None:
                continue
            if op == "=" and hvs != av:
                return False
            if op == "~=" and av not in hvs.split():
                return False
            if op == "^=" and not hvs.startswith(av):
                return False
            if op == "$=" and not hvs.endswith(av):
                return False
            if op == "*=" and av not in hvs:
                return False
        return True

    def _tokenize_group(group):
        # returns list of (combinator, token); combinator in ('desc','child')
        steps = []
        parts = group.replace(_GT, " " + _GT + " ").split()
        combinator = "desc"
        for p in parts:
            if p == _GT:
                combinator = "child"
                continue
            steps.append((combinator, p))
            combinator = "desc"
        return steps

    def _select(root, selector, limit=None):
        groups = [g for g in (g.strip() for g in selector.split(",")) if g]
        results = []
        seen = set()
        for group in groups:
            steps = _tokenize_group(group)
            current = [root]
            for combinator, tok in steps:
                simple = _parse_simple(tok)
                nxt = []
                if combinator == "child":
                    for node in current:
                        for c in node.contents if isinstance(node, Tag) else []:
                            if _simple_match(c, simple):
                                nxt.append(c)
                else:
                    # One subtree walk per step. `visited` stops a node reachable
                    # from several candidates from being expanded and matched
                    # once per path -- the combinatorial blow-up that made
                    # chained descendant selectors explode on nested markup --
                    # and lets a candidate nested inside an already walked
                    # candidate be skipped outright.
                    visited = set()
                    for node in current:
                        if not isinstance(node, Tag) or id(node) in visited:
                            continue
                        for d in node.descendants:
                            key = id(d)
                            if key in visited:
                                continue
                            visited.add(key)
                            if _simple_match(d, simple):
                                nxt.append(d)
                current = nxt
                if not current:
                    break
            for node in current:
                if id(node) not in seen:
                    seen.add(id(node))
                    results.append(node)
                    # A single group already yields document order, so a limited
                    # search can stop as soon as it is satisfied.
                    if limit and len(groups) == 1 and len(results) >= limit:
                        return results
        if len(groups) > 1 and len(results) > 1:
            # Comma groups are matched one at a time; re-order the union so the
            # caller sees document order, the way a real CSS engine reports it.
            order = {}
            for i, d in enumerate(root.descendants):
                order[id(d)] = i
            results.sort(key=lambda n: order.get(id(n), -1))
        return results[:limit] if limit else results

    # -- serialization -----------------------------------------------------------
    def _esc_text(s):
        return (
            s.replace(_AMP, _AMP + "amp;")
            .replace(_LT, _AMP + "lt;")
            .replace(_GT, _AMP + "gt;")
        )

    def _esc_attr(s):
        return _esc_text(str(s)).replace(_Q, _AMP + "quot;")

    def _open_tag(node, pad=""):
        parts = [pad + _LT + node.name]
        for k, v in node.attrs.items():
            vs = " ".join(v) if isinstance(v, list) else (v if v is not None else "")
            parts.append(" " + k + "=" + _Q + _esc_attr(vs) + _Q)
        if node.name in _VOID and not node.contents:
            parts.append("/" + _GT)
            return "".join(parts), True
        parts.append(_GT)
        return "".join(parts), False

    def _render_flat(node):
        # Compact serialization as one flat token stream: a single pass, a single
        # join, and no recursion, so arbitrarily deep markup serializes in linear
        # time without exhausting the interpreter stack.
        out = []
        stack = [node]
        while stack:
            cur = stack.pop()
            if type(cur) is tuple:
                out.append(cur[0])
                continue
            if isinstance(cur, Comment):
                out.append(_LT + "!--" + str(cur) + "--" + _GT)
                continue
            if isinstance(cur, CData):
                out.append("<![CDATA[" + str(cur) + "]]>")
                continue
            if isinstance(cur, Doctype):
                out.append("<!DOCTYPE " + str(cur) + _GT)
                continue
            if isinstance(cur, ProcessingInstruction):
                out.append("<?" + str(cur) + _GT)
                continue
            if isinstance(cur, NavigableString):
                parent = getattr(cur, "parent", None)
                raw = getattr(parent, "name", None) in ("script", "style")
                out.append(str(cur) if raw else _esc_text(str(cur)))
                continue
            if cur.name != "[document]":
                open_tag, is_void = _open_tag(cur)
                out.append(open_tag)
                if is_void:
                    continue
                stack.append((_LT + "/" + cur.name + _GT,))
            for c in reversed(cur.contents):
                stack.append(c)
        return "".join(out)

    def _pretty_string(node, depth):
        pad = "  " * depth
        if isinstance(node, Comment):
            return pad + _LT + "!--" + str(node) + "--" + _GT
        if isinstance(node, CData):
            return pad + "<![CDATA[" + str(node) + "]]>"
        if isinstance(node, Doctype):
            return pad + "<!DOCTYPE " + str(node) + _GT
        if isinstance(node, ProcessingInstruction):
            return pad + "<?" + str(node) + _GT
        text = str(node).strip()
        if not text:
            return ""
        parent = getattr(node, "parent", None)
        return pad + (
            text
            if getattr(parent, "name", None) in ("script", "style")
            else _esc_text(text)
        )

    def _join_pretty(node, depth, kids):
        kids = [k for k in kids if k != ""]
        if node.name == "[document]":
            return _NL.join(kids)
        pad = "  " * depth
        open_tag, is_void = _open_tag(node, pad)
        if is_void:
            return open_tag
        close_tag = _LT + "/" + node.name + _GT
        if not kids:
            return open_tag + close_tag
        return open_tag + _NL + _NL.join(kids) + _NL + pad + close_tag

    def _render_pretty(node, depth=0):
        if not isinstance(node, Tag):
            return _pretty_string(node, depth)
        # Explicit frames: [tag, depth, rendered kids, next child index]. Only
        # leaf children recurse (one level), so nesting depth is unbounded.
        stack = [[node, depth, [], 0]]
        while stack:
            frame = stack[-1]
            cur, d, kids, i = frame
            if i < len(cur.contents):
                frame[3] = i + 1
                kid_depth = d if cur.name == "[document]" else d + 1
                child = cur.contents[i]
                if isinstance(child, Tag):
                    stack.append([child, kid_depth, [], 0])
                else:
                    kids.append(_render_pretty(child, kid_depth))
                continue
            stack.pop()
            done = _join_pretty(cur, d, kids)
            if not stack:
                return done
            stack[-1][2].append(done)
        return ""

    def _render(node, pretty=False, depth=0):
        return _render_pretty(node, depth) if pretty else _render_flat(node)

    # -- parser ------------------------------------------------------------------
    class _Builder(_hp.HTMLParser):
        def __init__(self):
            _hp.HTMLParser.__init__(self, convert_charrefs=True)
            self.root = Tag("[document]")
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

        def handle_decl(self, decl):
            if decl.lower().startswith("doctype"):
                self._cur().append(Doctype(decl[7:].strip()))

        def handle_pi(self, data):
            self._cur().append(ProcessingInstruction(data))

        def unknown_decl(self, data):
            if data.startswith("CDATA[") and data.endswith("]"):
                self._cur().append(CData(data[6:-1]))

        def handle_entityref(self, name):
            self._cur().append(NavigableString(_AMP + name + ";"))

    def _strain(root, strainer):
        # parse_only pruning: keep the outermost nodes the strainer accepts and
        # drop everything else, the way bs4 narrows a parsed document.
        keep = []
        stack = list(root.contents)
        stack.reverse()
        while stack:
            node = stack.pop()
            if hasattr(strainer, "search"):
                ok = strainer.search(node) is not None
            else:
                ok = bool(strainer(node))
            if ok:
                keep.append(node)
                continue
            if isinstance(node, Tag):
                for c in reversed(node.contents):
                    stack.append(c)
        return keep

    class BeautifulSoup(Tag):
        def __init__(self, markup="", features=None, *args, **kwargs):
            Tag.__init__(self, "[document]")
            if hasattr(markup, "read"):
                markup = markup.read()
            if isinstance(markup, bytes):
                markup = markup.decode("utf-8", "replace")
            b = _Builder()
            b.feed(markup or "")
            b.close()
            parse_only = kwargs.get("parse_only")
            if parse_only is not None:
                self.contents = _strain(b.root, parse_only)
            else:
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

    class CData(NavigableString):
        pass

    class Doctype(NavigableString):
        pass

    class ProcessingInstruction(NavigableString):
        pass

    class FeatureNotFound(ValueError):
        pass

    class SoupStrainer:
        """Small callable name/attribute filter compatible with parse_only use."""

        def __init__(self, name=None, attrs=None, **kwargs):
            string = kwargs.pop("string", kwargs.pop("text", None))
            self._matcher = _make_matcher(name, attrs, string, kwargs)

        def search(self, element):
            return element if self._matcher(element) else None

        def __call__(self, element):
            return bool(self._matcher(element))

    mod = types.ModuleType("bs4")
    mod.__path__ = []
    mod.__version__ = "4.12-vis-pure"
    mod.BeautifulSoup = BeautifulSoup
    mod.Tag = Tag
    mod.NavigableString = NavigableString
    mod.Comment = Comment
    mod.CData = CData
    mod.Doctype = Doctype
    mod.ProcessingInstruction = ProcessingInstruction
    mod.FeatureNotFound = FeatureNotFound
    mod.SoupStrainer = SoupStrainer

    elem = types.ModuleType("bs4.element")
    elem.Tag = Tag
    elem.NavigableString = NavigableString
    elem.Comment = Comment
    elem.CData = CData
    elem.Doctype = Doctype
    elem.ProcessingInstruction = ProcessingInstruction
    elem.SoupStrainer = SoupStrainer
    mod.element = elem

    sys.modules["bs4"] = mod
    sys.modules["bs4.element"] = elem

    try:
        import builtins as _b

        _b.bs4 = mod
        _b.BeautifulSoup = BeautifulSoup
    except Exception:
        pass


__vis_install_bs4__()
del __vis_install_bs4__
