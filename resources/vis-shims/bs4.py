# vis sandbox beautifulsoup4-compat shim.
#
# The agent sandbox ships no bs4 wheel. This shim publishes a BeautifulSoup-compatible
# `bs4` module implemented in PURE Python on the stdlib html.parser (no host/JVM
# bridge), the natural partner to the requests shim (fetch then parse). It builds a
# Tag / NavigableString tree with find/find_all, CSS .select, get_text and HTML
# serialization. Published into sys.modules so `from bs4 import BeautifulSoup` works,
# and stapled onto builtins.
#
# Parity: differentially tested against REAL beautifulsoup4 4.12.3 + soupsieve 2.5
# (CPython) over 193 probes -- malformed/unclosed/mis-nested markup, entity and
# charref decoding (including unknown and out-of-range refs), whitespace-only text
# collapsing, pre/textarea preservation, script/style/textarea raw text, CDATA,
# doctype and processing instructions, multi-valued and valueless attributes,
# find/find_all/find_next*/find_previous*/find_parents (+ camelCase aliases,
# regex/list/callable/True matchers, SoupStrainer), CSS combinators plus :not,
# :nth-child/:nth-of-type, :first-child/:last-child/:empty and attribute operators,
# mutation (append/insert/wrap/unwrap/replace_with/extract/decompose/smooth/clear),
# copy.copy, len/iter/call/bool protocols, prettify, encode and get_text -- with
# ZERO output mismatches.
#
# Introspection parity is part of that surface, because real-world code reads it:
# PageElement/PreformattedString/ResultSet class hierarchy (and PageElement owning
# the shared find_*/wrap/extract API), NavigableString.PREFIX/SUFFIX and
# output_ready, sourceline/sourcepos under store_line_numbers, hidden/is_xml/
# known_xml/namespace/prefix, is_empty_element/can_be_empty_element, string
# containers (Script/Stylesheet/TemplateString/Ruby*), the legacy *Generator
# aliases, soup.builder plus the bs4.builder TreeBuilder/registry, formatter
# objects and the "minimal"/"html"/"html5"/None formatter stack behind
# decode/prettify/decode_contents/encode/renderContents, encoding detection
# (original_encoding, declared_html_encoding, contains_replacement_characters,
# bs4.dammit UnicodeDammit/EncodingDetector), SoupStrainer str/search/search_tag,
# and the bs4.element/formatter/builder/dammit/diagnose submodules. `bs4.__all__`
# is upstream's single name, so `from bs4 import *` behaves identically.
#
# Known deliberate divergences from upstream: the tree is built only by html.parser
# (no lxml/html5lib, so no implied-tag recovery beyond html.parser's, and asking
# for another parser is honored leniently instead of raising FeatureNotFound, since
# the sandbox cannot install one), soupsieve's :has() and namespace selectors are
# unsupported, and inserting a tag into itself or a descendant raises ValueError
# instead of building a cycle that upstream then hangs on while serializing.


def __vis_install_bs4__():
    import sys, types
    import html as _html
    import html.entities as _hent
    import re as _re
    import html.parser as _hp
    import builtins as _bi
    import collections as _collections
    import warnings as _warnings

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
    # bs4's HTMLTreeBuilder also treats these legacy elements as void, and
    # `builder.empty_element_tags` reports exactly this set.
    _VOID = _VOID | set(
        [
            "basefont",
            "bgsound",
            "command",
            "frame",
            "image",
            "isindex",
            "menuitem",
            "nextid",
            "spacer",
        ]
    )
    _DEFAULT_OUTPUT_ENCODING = "utf-8"
    # bs4 collapses a whitespace-only text run to a single space (or newline)
    # unless it sits inside a whitespace-preserving element.
    _ASCII_SPACES = " " + chr(10) + chr(9) + chr(12) + chr(13)
    _PRESERVE_WS = set(["pre", "textarea"])

    # bs4.element publishes these two, and its builders split attribute values
    # with nonwhitespace_re.
    whitespace_re = _re.compile(r"\s+")
    nonwhitespace_re = _re.compile(r"\S+")

    class NamespacedAttribute(str):
        """A namespaced attribute name ('xml:lang') that remembers its parts."""

        def __new__(cls, prefix, name=None, namespace=None):
            if not name:
                # This is the default namespace, whose name "has no value".
                name = None
            if not name:
                obj = str.__new__(cls, prefix)
            elif not prefix:
                # Not really namespaced.
                obj = str.__new__(cls, name)
            else:
                obj = str.__new__(cls, prefix + ":" + name)
            obj.prefix = prefix
            obj.name = name
            obj.namespace = namespace
            return obj

    _MULTI_ATTR = set(["class", "accesskey", "dropzone"])
    # HTML only makes these attributes space-separated lists on specific elements,
    # so <p rel="x y"> keeps a plain string the way bs4 does.
    _MULTI_ATTR_BY_TAG = {
        "a": ("rel", "rev"),
        "link": ("rel", "rev"),
        "td": ("headers",),
        "th": ("headers",),
        "form": ("accept-charset",),
        "object": ("archive",),
        "area": ("rel",),
        "icon": ("sizes",),
        "iframe": ("sandbox",),
        "output": ("for",),
    }
    # The same facts in the shape bs4's builder exposes them, since a tag
    # publishes its builder's table as tag.cdata_list_attributes.
    _CDATA_LIST_ATTRIBUTES = dict(
        # bs4 spells the wildcard list in this order and its per-tag entries in
        # _MULTI_ATTR_BY_TAG's order; a tag hands the table straight to callers,
        # so neither gets sorted.
        [("*", ["class", "accesskey", "dropzone"])]
        + [(k, list(v)) for k, v in _MULTI_ATTR_BY_TAG.items()]
    )

    class PageElement:
        """Common base of Tag and NavigableString, exactly as in bs4.

        Nothing lives here but the inspection defaults every element answers to;
        isinstance(x, PageElement) is the documented way to ask "is this a node?".
        """

        # bs4 leaves this None on the base class: "nobody has told me yet". Only
        # a soup (or a builder) ever pins it to a real True/False.
        known_xml = None
        namespace = None
        prefix = None
        hidden = False

        # bs4's "caller did not pass one" sentinel for the `types` filter of
        # get_text()/_all_strings(); None is a meaningful value there, so it
        # cannot double as the default.
        default = object()

        # The generic half of bs4's element API lives here, on PageElement, and
        # library code introspects it there. Implementations that only exist on
        # one subclass are republished onto this class further down.
        def setup(
            self,
            parent=None,
            previous_element=None,
            next_element=None,
            previous_sibling=None,
            next_sibling=None,
        ):
            """bs4 hand-wires linkage here; this shim derives it from the tree."""
            self.parent = parent

        def _last_descendant(self, is_initialized=True, accept_self=True):
            """The deepest, last node under this one -- bs4's walk terminator."""
            last = self
            while getattr(last, "contents", None):
                last = last.contents[-1]
            if not accept_self and last is self:
                return None
            return last

        _lastRecursiveChild = _last_descendant

        @property
        def _is_xml(self):
            """Is this node part of an XML tree? Never, in this HTML-only shim."""
            if self.known_xml is not None:
                return self.known_xml
            parent = self.parent
            if parent is None:
                return False
            return parent._is_xml

        def formatter_for_name(self, formatter):
            """Resolve a formatter name/callable to a Formatter, as bs4 does."""
            if isinstance(formatter, Formatter):
                return formatter
            c = XMLFormatter if self._is_xml else HTMLFormatter
            if isinstance(formatter, str):
                formatter = c.REGISTRY[formatter]
            elif formatter is None:
                formatter = c.REGISTRY[None]
            else:
                formatter = c(entity_substitution=formatter)
            return formatter

        def _find_all(self, name, attrs, string, limit, generator, **kwargs):
            """bs4's search engine: run the matcher over an arbitrary walk."""
            matcher = _make_matcher(name, attrs, string, kwargs, limit)
            out = []
            for node in generator:
                if matcher(node):
                    out.append(node)
                    if limit and len(out) >= limit:
                        break
            return ResultSet(matcher.strainer, out)

        def _find_one(self, method, name, attrs, string, **kwargs):
            r = method(name, attrs, string, 1, **kwargs)
            return r[0] if r else None

    class ResultSet(list):
        """What the plural finders return: a list that remembers its strainer."""

        def __init__(self, source, result=()):
            list.__init__(self, result)
            self.source = source

        def __getattr__(self, key):
            raise AttributeError(
                "ResultSet object has no attribute '"
                + key
                + "'. You're probably treating a list of elements like a single "
                + "element. Did you call find_all() when you meant to call find()?"
            )

    class NavigableString(str, PageElement):
        # Serialization affixes; PreformattedString subclasses override them and
        # bs4 code in the wild reads them to tell node kinds apart.
        PREFIX = ""
        SUFFIX = ""

        def format_string(self, s, formatter="minimal"):
            """Run `s` through `formatter`, as bs4's PageElement.format_string does."""
            if formatter is None:
                return s
            return _fmt_of(formatter)[0](s)

        def output_ready(self, formatter="minimal"):
            """This string exactly as it appears in serialized output, affixes included."""
            return self.PREFIX + self.format_string(str(self), formatter) + self.SUFFIX

        def __new__(cls, value):
            s = str.__new__(cls, value)
            s.parent = None
            return s

        @property
        def name(self):
            return None

        @name.setter
        def name(self, name):
            # bs4 refuses to name a string node, even though .name reads as None.
            raise AttributeError("A NavigableString cannot be given a name.")

        @property
        def string(self):
            return self

        @property
        def text(self):
            return self.get_text()

        @property
        def next_sibling(self):
            return _sibling(self, 1)

        @property
        def next_element(self):
            return _next_element(self)

        @property
        def previous_sibling(self):
            return _sibling(self, -1)

        def _all_strings(self, strip=False, types=PageElement.default):
            """Yield this string, or nothing at all when it is not one of `types`.

            bs4 compares the exact type instead of using isinstance: every
            string container subclasses NavigableString, and somebody asking
            for NavigableStrings does not want comments or script bodies.
            """
            if types is self.default:
                # Kept on Tag, as upstream does, because the interesting
                # classes are defined further down the file.
                types = Tag.DEFAULT_INTERESTING_STRING_TYPES
            my_type = type(self)
            if types is not None:
                if isinstance(types, type):
                    if my_type is not types:
                        return
                elif my_type not in types:
                    return
            value = self
            if strip:
                value = value.strip()
            if len(value) > 0:
                yield value

        def get_text(self, separator="", strip=False, types=PageElement.default):
            return separator.join(self._all_strings(strip, types=types))

        getText = get_text

        @property
        def strings(self):
            return self._all_strings(False)

        @property
        def stripped_strings(self):
            return self._all_strings(True)

        def strip_str(self):
            return str.strip(self)

        def extract(self, _self_index=None):
            _detach(self, _self_index)
            return self

        def __copy__(self):
            # bs4: a copied string keeps its contents and class, but none of its
            # linkage -- the copy belongs to no tree at all.
            return type(self)(self)

        def __deepcopy__(self, memo=None):
            return type(self)(self)

    class PreformattedString(NavigableString):
        """A string whose contents are output verbatim, wrapped in affixes."""

        PREFIX = ""
        SUFFIX = ""

        def output_ready(self, formatter=None):
            """Verbatim contents: comments, CDATA and doctypes are never escaped."""
            return self.PREFIX + str(self) + self.SUFFIX

    class Comment(PreformattedString):
        PREFIX = _LT + "!--"
        SUFFIX = "--" + _GT

    class Tag(PageElement):
        def __init__(
            self,
            parser=None,
            builder=None,
            name=None,
            namespace=None,
            prefix=None,
            attrs=None,
            parent=None,
            previous=None,
            is_xml=None,
            sourceline=None,
            sourcepos=None,
            can_be_empty_element=None,
            cdata_list_attributes=None,
            preserve_whitespace_tags=None,
            interesting_string_types=None,
            namespaces=None,
        ):
            # bs4's constructor signature exactly: the parser and the tree
            # builder come first, so a bare Tag("b") names no tag and raises.
            # bs4 never stores the parser itself, only its class, so that an
            # extracted chunk can be garbage-collected.
            self.parser_class = None if parser is None else parser.__class__
            if name is None:
                raise ValueError("No value provided for new tag's name.")
            self.name = name
            self.namespace = namespace
            self._namespaces = namespaces or {}
            self.prefix = prefix
            # Where this tag started in the markup, as bs4 reports it: 1-based
            # line and 0-based column. bs4 only records a position the parser
            # actually gave it, so a hand-built tag has neither in its __dict__
            # and reaches __getattr__ (which answers None) instead.
            if (not builder or builder.store_line_numbers) and (
                sourceline is not None or sourcepos is not None
            ):
                self.sourceline = sourceline
                self.sourcepos = sourcepos
            if attrs is None:
                attrs = {}
            elif attrs:
                if builder is not None and builder.cdata_list_attributes:
                    attrs = builder._replace_cdata_list_attribute_values(
                        self.name, attrs
                    )
                else:
                    attrs = dict(attrs)
            else:
                attrs = dict(attrs)
            self.known_xml = builder.is_xml if builder else is_xml
            self.attrs = attrs
            self.contents = []
            self.parent = parent
            self.hidden = False
            if builder is None:
                # With no TreeBuilder these are whatever the caller passed in --
                # usually None, unless this is a copy of some other tag.
                self.can_be_empty_element = can_be_empty_element
                self.cdata_list_attributes = cdata_list_attributes
                self.preserve_whitespace_tags = preserve_whitespace_tags
                self.interesting_string_types = interesting_string_types
            else:
                # Set up any substitutions for this tag, such as the charset in
                # a META tag, and ask the builder about the rest.
                builder.set_up_substitutions(self)
                self.can_be_empty_element = builder.can_be_empty_element(name)
                self.cdata_list_attributes = builder.cdata_list_attributes
                self.preserve_whitespace_tags = builder.preserve_whitespace_tags
                if self.name in builder.string_containers:
                    self.interesting_string_types = builder.string_containers[self.name]
                else:
                    self.interesting_string_types = (
                        self.DEFAULT_INTERESTING_STRING_TYPES
                    )

        # -- attribute access ---------------------------------------------------
        def __getitem__(self, key):
            return self.attrs[key]

        def __setitem__(self, key, value):
            # Assignment preserves the caller's value verbatim -- including
            # None, which bs4 serializes as a bare attribute -- and only parsed
            # markup gets HTML multi-valued-attribute normalization.
            self.attrs[key] = value

        def __delitem__(self, key):
            del self.attrs[key]

        def __contains__(self, x):
            # bs4: `x in tag` asks about children, not attributes.
            return x in self.contents

        def get(self, key, default=None):
            return self.attrs.get(key, default)

        def has_attr(self, key):
            return key in self.attrs

        def get_attribute_list(self, key, default=None):
            v = self.attrs.get(key, default)
            return v if isinstance(v, list) else [v]

        # -- inspection ---------------------------------------------------------
        @property
        def is_empty_element(self):
            # An empty element is one that *may* be empty and is: <br/>, not <p></p>.
            return not self.contents and self.can_be_empty_element

        isSelfClosing = is_empty_element

        # -- tree ---------------------------------------------------------------
        @property
        def children(self):
            return iter(self.contents)

        @property
        def descendants(self):
            # Iterative pre-order walk: no recursion frame per nesting level
            # (deep markup would otherwise exhaust the interpreter stack) and no
            # per-level copy of the child list. Each stacked node remembers the
            # parent it was reached through, because bs4 walks .next_element
            # links: a node cut out of the tree mid-iteration is never reached,
            # so the walk stops there instead of yielding a detached node.
            stack = list(self.contents)
            stack.reverse()
            parents = [self] * len(stack)
            while stack:
                node = stack.pop()
                if node.parent is not parents.pop():
                    return
                yield node
                kids = node.contents if isinstance(node, Tag) else None
                if kids:
                    for c in reversed(kids):
                        stack.append(c)
                        parents.append(node)

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
            self.insert(len(self.contents), node)

        def extend(self, nodes):
            for node in nodes:
                self.append(node)

        def index(self, element):
            i = _index_of(self.contents, element)
            if i < 0:
                raise ValueError("Tag.index: element not in tag")
            return i

        def insert(self, position, node):
            if node is None:
                raise ValueError("Cannot insert None into a tag.")
            if isinstance(node, Tag) and node.name == "[document]":
                for child in list(node.contents):
                    self.insert(position, child)
                    position += 1
                return
            node = _adopt(self, node)
            self.contents.insert(position, node)

        def _sib_insert(self, node, offset, word):
            p = self.parent
            if p is None:
                raise ValueError(
                    "Element has no parent, so '%s' has no meaning." % word
                )
            if node is self:
                raise ValueError("Can't insert an element %s itself." % word)
            idx = _index_of(p.contents, self)
            node = _adopt(p, node)
            # Moving an earlier sibling left-shifts the insertion point.
            if _index_of(p.contents, self) != idx:
                idx = _index_of(p.contents, self)
            p.contents.insert(idx + offset, node)

        def insert_before(self, *nodes):
            for n in nodes:
                self._sib_insert(n, 0, "before")

        def insert_after(self, *nodes):
            for n in reversed(nodes):
                self._sib_insert(n, 1, "after")

        def replace_with(self, *args):
            p = self.parent
            if p is None:
                raise ValueError(
                    "Cannot replace one element with another when the "
                    "element to be replaced is not part of a tree."
                )
            if len(args) == 1 and args[0] is self:
                # Replacing a node with itself is a no-op that returns None,
                # not the node -- bs4 leans on that in wrap().
                return None
            if any(x is p for x in args):
                raise ValueError("Cannot replace a Tag with its parent.")
            idx = p.index(self)
            _detach(self)
            for offset, replacement in enumerate(args):
                p.insert(idx + offset, replacement)
            return self

        def wrap(self, inside_tag):
            me = self.replace_with(inside_tag)
            inside_tag.append(me)
            return inside_tag

        def unwrap(self):
            p = self.parent
            if p is None:
                # bs4's message is spelled exactly like this, typo included.
                raise ValueError(
                    "Cannot replace an element with its contents when that"
                    "element is not part of a tree."
                )
            idx = _index_of(p.contents, self)
            children = self.contents
            p.contents[idx : idx + 1] = children
            for c in children:
                c.parent = p
            self.contents = []
            self.parent = None
            return self

        # -- text ---------------------------------------------------------------
        def _all_strings(self, strip=False, types=PageElement.default):
            """Yield the descendant strings of the classes in `types`.

            The default comes from self.interesting_string_types, so a <script>
            yields its Script body while a <div> ignores scripts, stylesheets
            and comments -- bs4 matches the exact type, not isinstance.
            """
            if types is self.default:
                types = self.interesting_string_types
            for d in self.descendants:
                if types is None and not isinstance(d, NavigableString):
                    continue
                d_type = type(d)
                if isinstance(types, type):
                    if d_type is not types:
                        continue
                elif types is not None and d_type not in types:
                    continue
                if strip:
                    d = d.strip()
                    if len(d) == 0:
                        continue
                yield d

        def get_text(self, separator="", strip=False, types=PageElement.default):
            return separator.join(self._all_strings(strip, types=types))

        getText = get_text

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

        @string.setter
        def string(self, value):
            # Assigning .string replaces every child with that single string.
            self.clear()
            self.append(
                value if isinstance(value, NavigableString) else NavigableString(value)
            )

        @property
        def strings(self):
            return self._all_strings(False)

        @property
        def stripped_strings(self):
            return self._all_strings(True)

        # -- search -------------------------------------------------------------
        def find(self, name=None, attrs={}, recursive=True, string=None, **kwargs):  # noqa: B006
            res = self.find_all(name, attrs, recursive, string, 1, **kwargs)
            return res[0] if res else None

        def find_all(
            self,
            name=None,
            attrs={},  # noqa: B006
            recursive=True,
            string=None,
            limit=None,
            **kwargs,
        ):
            if string is None:
                string = kwargs.pop("string", kwargs.pop("text", None))
            matcher = _make_matcher(name, attrs, string, kwargs, limit)
            out = []
            src = self.descendants if recursive else self.children
            for node in src:
                if matcher(node):
                    out.append(node)
                    if limit and len(out) >= limit:
                        break
            return ResultSet(matcher.strainer, out)

        findAll = find_all
        findChildren = find_all

        def find_next_sibling(self, name=None, attrs={}, **kwargs):  # noqa: B006
            matcher = _make_matcher(name, attrs, None, kwargs, 1)
            sib = self.next_sibling
            while sib is not None:
                if matcher(sib):
                    return sib
                sib = _sibling(sib, 1)
            return None

        def find_parent(self, name=None, attrs={}, **kwargs):  # noqa: B006
            matcher = _make_matcher(name, attrs, None, kwargs, 1)
            for p in self.parents:
                if matcher(p):
                    return p
            return None

        findParent = find_parent

        def select(self, selector, namespaces=None, limit=None, **kwargs):
            return ResultSet(None, _select(self, selector, limit=limit or None))

        def select_one(self, selector, namespaces=None, **kwargs):
            r = _select(self, selector, limit=1)
            return r[0] if r else None

        # -- mutation -----------------------------------------------------------
        def extract(self, _self_index=None):
            _detach(self, _self_index)
            return self

        def decompose(self):
            # bs4 does not merely detach: it empties every node in the subtree,
            # so a decomposed tag no longer even knows its own name. Collect the
            # nodes first, because clearing them destroys the links we walk.
            self.extract()
            doomed = [self]
            doomed.extend(self.descendants)
            for node in doomed:
                node.__dict__.clear()
                if isinstance(node, Tag):
                    node.contents = []
                node._decomposed = True

        def smooth(self):
            for c in self.contents:
                if isinstance(c, Tag):
                    c.smooth()
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

        def __getattr__(self, tag):
            if len(tag) > 3 and tag.endswith("Tag"):
                # BS3 spelling: soup.aTag meant soup.find("a").
                tag_name = tag[:-3]
                _warnings.warn(
                    '.%(name)sTag is deprecated, use .find("%(name)s") instead.'
                    " If you really were looking for a tag called %(name)sTag,"
                    ' use .find("%(name)sTag")' % dict(name=tag_name),
                    DeprecationWarning,
                    stacklevel=2,
                )
                return self.find(tag_name)
            elif not tag.startswith("__") and not tag == "contents":
                return self.find(tag)
            raise AttributeError(
                "'%s' object has no attribute '%s'" % (self.__class__, tag)
            )

        # -- serialization ------------------------------------------------------
        def decode(
            self,
            indent_level=None,
            eventual_encoding=_DEFAULT_OUTPUT_ENCODING,
            formatter="minimal",
        ):
            # bs4's first positional is an indentation level, not a flag: any
            # non-None value pretty-prints this subtree starting at that depth.
            if indent_level is True:
                # bs4 treats a bare True as "pretty-print from depth 0".
                indent_level = 0
            if indent_level is None:
                return _render(self, formatter=formatter, encoding=eventual_encoding)
            out = _render(
                self,
                pretty=True,
                depth=indent_level,
                formatter=formatter,
                encoding=eventual_encoding,
            )
            return out if out.endswith(_NL) else out + _NL

        def prettify(self, encoding=None, formatter="minimal"):
            out = _render(
                self,
                pretty=True,
                depth=0,
                formatter=formatter,
                encoding=encoding or _DEFAULT_OUTPUT_ENCODING,
            )
            if not out.endswith(_NL):
                out = out + _NL
            return out.encode(encoding, "xmlcharrefreplace") if encoding else out

        def decode_contents(
            self,
            indent_level=None,
            eventual_encoding=_DEFAULT_OUTPUT_ENCODING,
            formatter="minimal",
        ):
            if indent_level is None:
                return _with_formatter(
                    formatter,
                    lambda: "".join(_render_flat(c) for c in self.contents),
                    eventual_encoding,
                )
            kids = _with_formatter(
                formatter,
                lambda: [_render_pretty(c, indent_level) for c in self.contents],
                eventual_encoding,
            )
            return "".join(k + _NL for k in kids if k != "")

        def encode_contents(
            self,
            indent_level=None,
            encoding=_DEFAULT_OUTPUT_ENCODING,
            formatter="minimal",
        ):
            markup = self.decode_contents(indent_level, encoding, formatter)
            return markup.encode(encoding, "xmlcharrefreplace")

        def renderContents(
            self, encoding=_DEFAULT_OUTPUT_ENCODING, prettyPrint=False, indentLevel=0
        ):
            return self.encode_contents(indentLevel if prettyPrint else None, encoding)

        def __repr__(self):
            return _render(self)

        def __str__(self):
            return _render(self)

        @property
        def decomposed(self):
            return bool(getattr(self, "_decomposed", False))

        def __iter__(self):
            # bs4 iterates a Tag over its children, not over its attribute keys.
            return iter(self.contents)

        def __len__(self):
            return len(self.contents)

        def __eq__(self, other):
            # bs4 compares tags structurally: same name, same attributes and
            # recursively equal children. Identity short-circuits, and anything
            # that is not tag-shaped (a string, None) is simply unequal.
            if self is other:
                return True
            if (
                not hasattr(other, "name")
                or not hasattr(other, "attrs")
                or not hasattr(other, "contents")
                or self.name != other.name
                or self.attrs != other.attrs
                or len(self.contents) != len(other.contents)
            ):
                return False
            for i, mine in enumerate(self.contents):
                if mine != other.contents[i]:
                    return False
            return True

        def __ne__(self, other):
            return not self == other

        def __hash__(self):
            # Defining __eq__ would otherwise make Tag unhashable; bs4 hashes
            # the serialization, so equal tags hash alike.
            return str(self).__hash__()

        def __bool__(self):
            # Every Tag is truthy in bs4: an empty <div> must not read as falsey
            # merely because it has no children.
            return True

        def __call__(self, *args, **kwargs):
            return self.find_all(*args, **kwargs)

        def __copy__(self):
            return _clone(self)

        def __deepcopy__(self, memo=None):
            return _clone(self)

        def encode(
            self,
            encoding=_DEFAULT_OUTPUT_ENCODING,
            indent_level=None,
            formatter="minimal",
            errors="xmlcharrefreplace",
        ):
            # bs4's second positional is indent_level, not the codec error handler:
            # a non-None value asks for pretty-printed output, and non-encodable
            # characters become character references by default.
            return self.decode(indent_level, encoding, formatter).encode(
                encoding, errors
            )

        # -- bs4 internals other code reaches for --------------------------------

        # The serializer's event stream is driven by these four sentinels; bs4
        # compares them by identity, never by value.
        START_ELEMENT_EVENT = object()
        END_ELEMENT_EVENT = object()
        EMPTY_ELEMENT_EVENT = object()
        STRING_ELEMENT_EVENT = object()

        def has_key(self, key):
            """BS3's spelling of has_attr(), which bs4 still ships."""
            return key in self.attrs

        findChild = find

        @property
        def parserClass(self):
            return self.parser_class

        @property
        def self_and_descendants(self):
            """This tag, then everything under it -- unless it is the document."""
            if not self.hidden:
                yield self
            yield from self.descendants

        def _clone(self):
            """A copy of this tag: same name and attributes, no children at all."""
            if isinstance(self, BeautifulSoup):
                # bs4 clones a soup by re-running the constructor on empty
                # markup with the same builder, keeping the source encoding.
                dup = BeautifulSoup("", None, self.builder)
                dup.original_encoding = self.original_encoding
                return dup
            dup = type(self)(
                None,
                None,
                self.name,
                self.namespace,
                self.prefix,
                # Shallow, like bs4: a multi-valued attribute's list is shared
                # with the original tag rather than copied.
                self.attrs,
                is_xml=self._is_xml,
                sourceline=self.sourceline,
                sourcepos=self.sourcepos,
                can_be_empty_element=self.can_be_empty_element,
                cdata_list_attributes=self.cdata_list_attributes,
                preserve_whitespace_tags=self.preserve_whitespace_tags,
                interesting_string_types=self.interesting_string_types,
            )
            for attr in ("can_be_empty_element", "hidden"):
                setattr(dup, attr, getattr(self, attr))
            return dup

        def _event_stream(self, iterator=None):
            """bs4's serializer walk: (event, element) pairs, this tag included."""
            tag_stack = []
            iterator = iterator or self.self_and_descendants
            for c in iterator:
                # Identity, not equality: two sibling <p>x</p> tags are `==` here.
                while tag_stack and c.parent is not tag_stack[-1]:
                    yield Tag.END_ELEMENT_EVENT, tag_stack.pop()
                if isinstance(c, Tag):
                    if c.is_empty_element:
                        yield Tag.EMPTY_ELEMENT_EVENT, c
                    else:
                        yield Tag.START_ELEMENT_EVENT, c
                        tag_stack.append(c)
                        continue
                else:
                    yield Tag.STRING_ELEMENT_EVENT, c
            while tag_stack:
                yield Tag.END_ELEMENT_EVENT, tag_stack.pop()

        def _should_pretty_print(self, indent_level=1):
            return indent_level is not None and (
                not self.preserve_whitespace_tags
                or self.name not in self.preserve_whitespace_tags
            )

        def _indent_string(
            self, s, indent_level, formatter, indent_before, indent_after
        ):
            space_before = ""
            if indent_before and indent_level:
                space_before = formatter.indent * indent_level
            return space_before + s + ("\n" if indent_after else "")

        def _format_tag(self, eventual_encoding, formatter, opening):
            """Just this tag's opening or closing markup, without its contents."""
            if self.hidden:
                return ""
            if opening:
                return _with_formatter(
                    formatter, lambda: _open_tag(self)[0], encoding=eventual_encoding
                )
            prefix = (self.prefix + ":") if self.prefix else ""
            void_close = _fmt_of(formatter)[1] if self.is_empty_element else ""
            # bs4 really does render `</br/>` for a void element's closing tag.
            return "</" + prefix + self.name + void_close + ">"

        @property
        def css(self):
            """bs4's soupsieve facade; this shim's own selector engine backs it."""
            return CSS(self)

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
                raise ValueError("Cannot insert a tag into itself.")
            cur = cur.parent

    def _adopt(parent, node):
        if isinstance(node, str) and not isinstance(node, NavigableString):
            node = NavigableString(node)
        _require_acyclic(parent, node)
        _detach(node)
        node.parent = parent
        return node

    def _detach(node, index=None):
        p = getattr(node, "parent", None)
        if p is not None:
            i = _index_of(p.contents, node) if index is None else index
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
        if isinstance(node, BeautifulSoup):
            # The soup object sits outside bs4's element chain.
            return None
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

    def _normalize_search_value(value):
        # Port of SoupStrainer._normalize_search_value: strings, callables,
        # regexes, booleans and None are used as-is, everything else is coerced
        # to a string (or a list of strings). That is why find_all(id=1) and
        # find_all(b"p") match markup that only ever holds text.
        if (
            isinstance(value, str)
            or callable(value)
            or hasattr(value, "match")
            or isinstance(value, bool)
            or value is None
        ):
            return value
        if isinstance(value, bytes):
            return value.decode("utf8")
        if hasattr(value, "__iter__"):
            out = []
            for item in value:
                if hasattr(item, "__iter__") and not isinstance(item, (bytes, str)):
                    # Almost certainly the caller's mistake; bs4 passes it
                    # through rather than recursing forever.
                    out.append(item)
                else:
                    out.append(_normalize_search_value(item))
            return out
        return str(value)

    def _value_matches(markup, want, already_tried=None):
        # Port of SoupStrainer._matches. Two clauses carry most of the
        # behaviour: a multi-valued attribute matches when ANY of its values
        # matches (or the space-joined string does), and an absent value
        # (`markup is None`) matches every falsy filter -- which is what makes
        # `id=False`, `id=None` and `id=""` all mean "has no id".
        if isinstance(markup, (list, tuple)):
            for item in markup:
                if _value_matches(item, want):
                    return True
            return _value_matches(" ".join(markup), want)
        if want is True:
            return markup is not None
        if callable(want) and not hasattr(want, "match"):
            return want(markup)
        original = markup
        if isinstance(markup, Tag):
            markup = markup.name
        markup = _normalize_search_value(markup)
        if markup is None:
            return not want
        if hasattr(want, "__iter__") and not isinstance(want, str):
            tried = already_tried if already_tried else set()
            for item in want:
                key = item if getattr(item, "__hash__", None) else id(item)
                if key in tried:
                    continue
                tried.add(key)
                if _value_matches(original, item, tried):
                    return True
            return False
        match = isinstance(want, str) and markup == want
        if not match and hasattr(want, "search"):
            return want.search(markup)
        if not match and isinstance(original, Tag) and original.prefix:
            return _value_matches(original.prefix + ":" + original.name, want)
        return match

    def _strainer_search_tag(strainer, markup_name=None, markup_attrs=None):
        # Port of SoupStrainer.search_tag: matches a real Tag, or -- given a
        # bare name plus an attribute mapping -- a tag that has not been built
        # yet. Returns the matched object (bs4 returns markup, not a bool).
        found = None
        markup = None
        if isinstance(markup_name, Tag):
            markup = markup_name
            markup_attrs = markup.attrs
        name, attrs, string = strainer.name, strainer.attrs, strainer.string
        if isinstance(name, str) and markup is not None:
            # Fast rejection for the common "one specific tag name" search.
            if not markup.prefix and name != markup.name:
                # bs4 returns False -- not None -- from this fast path.
                return False
        call_with_tag_data = (
            callable(name)
            and not hasattr(name, "match")
            and not isinstance(markup_name, Tag)
        )
        if (
            not name
            or call_with_tag_data
            or (markup is not None and _value_matches(markup, name))
            or (markup is None and _value_matches(markup_name, name))
        ):
            if call_with_tag_data:
                match = name(markup_name, markup_attrs)
            else:
                match = True
                attr_map = (
                    markup_attrs
                    if hasattr(markup_attrs, "get")
                    else dict(markup_attrs or ())
                )
                for attr, want in list(attrs.items()):
                    if not _value_matches(attr_map.get(attr), want):
                        match = False
                        break
            if match:
                found = markup if markup is not None else markup_name
        if found is not None and string:
            # bs4 tests the filter against `.string`, not the full text: a tag
            # with mixed content has no .string and so never matches.
            text = found.string if isinstance(found, Tag) else found
            if not _value_matches(text, string):
                found = None
        return found

    def _strainer_search(strainer, markup):
        # Port of SoupStrainer.search: dispatch on the kind of node. A tag is
        # only skipped outright when the strainer is a pure string filter.
        if isinstance(markup, Tag):
            if not strainer.string or strainer.name or strainer.attrs:
                return _strainer_search_tag(strainer, markup)
            return None
        if isinstance(markup, str):
            if (
                not strainer.name
                and not strainer.attrs
                and _value_matches(markup, strainer.string)
            ):
                return markup
            return None
        if hasattr(markup, "__iter__"):
            for element in markup:
                if isinstance(element, NavigableString) and (
                    _strainer_search(strainer, element) is not None
                ):
                    return element
        return None

    def _make_matcher(name, attrs, string, kwargs, limit=None):
        """Node predicate mirroring bs4's _find_all, name-only fast paths and all.

        The predicate carries the SoupStrainer a ResultSet remembers as
        `.strainer`, so callers do not rebuild (or double-wrap) one.
        """
        strainer = (
            name
            if isinstance(name, SoupStrainer)
            else SoupStrainer(name, attrs, string, **kwargs)
        )

        def general(node):
            # bs4 skips falsy nodes before matching; an empty string is falsy,
            # a childless Tag is not.
            if not isinstance(node, Tag) and not node:
                return False
            found = _strainer_search(strainer, node)
            if found is None:
                return False
            return True if isinstance(found, Tag) else bool(found)

        matcher = general
        if (
            string is None
            and not limit
            and not attrs
            and not kwargs
            and not isinstance(name, SoupStrainer)
        ):
            # bs4's unlimited name-only searches bypass the strainer entirely,
            # so `find_all("")` finds nothing while `find_all([])` finds every
            # tag. Anything narrower (a limit, attributes, a string) does not.
            if name is True or name is None:

                def any_tag(node):
                    return isinstance(node, Tag)

                matcher = any_tag
            elif isinstance(name, str):
                prefix, local = (
                    name.split(":", 1) if name.count(":") == 1 else (None, name)
                )

                def by_name(node):
                    if not isinstance(node, Tag):
                        return False
                    return node.name == name or (
                        node.name == local and (prefix is None or node.prefix == prefix)
                    )

                matcher = by_name
        matcher.strainer = strainer
        return matcher

    # -- shared navigation ---------------------------------------------------------
    # bs4 exposes the same walk API on Tags and NavigableStrings. Building the
    # finders once from a generator factory and stapling them onto both classes
    # keeps the two kinds of node in lockstep instead of drifting apart.
    def _previous_element(node):
        prev = _sibling(node, -1)
        if prev is None:
            # The soup object itself is not part of bs4's element chain: the
            # first node parsed simply has no previous element.
            parent = getattr(node, "parent", None)
            return None if isinstance(parent, BeautifulSoup) else parent
        while isinstance(prev, Tag) and prev.contents:
            prev = prev.contents[-1]
        return prev

    def _walk(step):
        def gen(node):
            cur = step(node)
            while cur is not None:
                yield cur
                cur = step(cur)

        return gen

    _iter_next_elements = _walk(_next_element)
    _iter_previous_elements = _walk(_previous_element)
    _iter_parents = _walk(lambda n: getattr(n, "parent", None))
    _iter_next_siblings = _walk(lambda n: _sibling(n, 1))
    _iter_previous_siblings = _walk(lambda n: _sibling(n, -1))

    def _find_in(nodes, name, attrs, string, limit, kwargs):
        if string is None:
            string = kwargs.pop("string", kwargs.pop("text", None))
        matcher = _make_matcher(name, attrs, string, kwargs, limit)
        out = []
        for candidate in nodes:
            if matcher(candidate):
                out.append(candidate)
                if limit and len(out) >= limit:
                    break
        return out, matcher.strainer

    def _mk_finder(gen, first):
        def finder(self, name=None, attrs={}, string=None, limit=None, **kwargs):  # noqa: B006
            if string is None:
                string = kwargs.pop("string", kwargs.pop("text", None))
            hits, strainer = _find_in(
                gen(self), name, attrs, string, 1 if first else limit, kwargs
            )
            if first:
                return hits[0] if hits else None
            return ResultSet(strainer, hits)

        return finder

    def _camel(snake):
        head, _sep, tail = snake.partition("_")
        return head + "".join(w[:1].upper() + w[1:] for w in tail.split("_"))

    def _install_navigation():
        specs = (
            ("find_next", "find_all_next", _iter_next_elements),
            ("find_previous", "find_all_previous", _iter_previous_elements),
            ("find_parent", "find_parents", _iter_parents),
            ("find_next_sibling", "find_next_siblings", _iter_next_siblings),
            (
                "find_previous_sibling",
                "find_previous_siblings",
                _iter_previous_siblings,
            ),
        )
        props = (
            ("next_elements", _iter_next_elements),
            ("previous_elements", _iter_previous_elements),
            ("parents", _iter_parents),
            ("next_siblings", _iter_next_siblings),
            ("previous_siblings", _iter_previous_siblings),
        )
        for cls in (Tag, NavigableString):
            for one, many, gen in specs:
                for attr, fn in (
                    (one, _mk_finder(gen, True)),
                    (many, _mk_finder(gen, False)),
                ):
                    setattr(cls, attr, fn)
                    setattr(cls, _camel(attr), fn)
            for attr, gen in props:
                setattr(cls, attr, property(lambda self, g=gen: g(self)))
            cls.previous_element = property(_previous_element)
            cls.fetchNextSiblings = cls.find_next_siblings
            cls.fetchPrevious = cls.find_all_previous
            cls.fetchParents = cls.find_parents
            cls.fetchPreviousSiblings = cls.find_previous_siblings
        # bs4's pre-4.0 generator API is still in the wild (and still documented
        # as the way to inspect a walk lazily), so every walk gets one.
        gens = (
            ("childGenerator", lambda n: iter(getattr(n, "contents", []))),
            ("recursiveChildGenerator", lambda n: iter(getattr(n, "descendants", []))),
            ("nextGenerator", _iter_next_elements),
            ("previousGenerator", _iter_previous_elements),
            ("nextSiblingGenerator", _iter_next_siblings),
            ("previousSiblingGenerator", _iter_previous_siblings),
            ("parentGenerator", _iter_parents),
        )
        for cls in (Tag, NavigableString):
            for attr, gen in gens:
                setattr(cls, attr, lambda self, g=gen: iter(g(self)))

    def _clone(node):
        # copy.copy(tag) in bs4 hands back a *deep*, parentless copy; the walk is
        # explicit so cloning deep markup cannot blow the interpreter stack.
        def shallow(src):
            if isinstance(src, NavigableString):
                return type(src)(str(src))
            # copy.copy(soup) hands back a BeautifulSoup, not a plain Tag.
            dup = BeautifulSoup("") if isinstance(src, BeautifulSoup) else src._clone()
            dup.name = src.name
            dup.attrs = dict(
                (k, list(v) if isinstance(v, list) else v) for k, v in src.attrs.items()
            )
            return dup

        root = shallow(node)
        if isinstance(node, NavigableString):
            return root
        stack = [(node, root)]
        while stack:
            src, dst = stack.pop()
            for child in src.contents:
                dup = shallow(child)
                dup.parent = dst
                dst.contents.append(dup)
                if isinstance(child, Tag):
                    stack.append((child, dup))
        return root

    _install_navigation()

    # bs4 hangs the generic navigation/mutation API off PageElement itself and
    # library code introspects it (`hasattr(PageElement, "find_all_next")`), so
    # republish the shared implementations there now that Tag exists.
    for _pe_name in (
        "append",
        "extend",
        "insert",
        "_sib_insert",
        "unwrap",
        "decomposed",
        "get_text",
        "getText",
        "text",
        "stripped_strings",
        "_all_strings",
        "next_elements",
        "next_siblings",
        "previous_elements",
        "previous_siblings",
        "parents",
        "fetchNextSiblings",
        "fetchPreviousSiblings",
        "fetchParents",
        "fetchPrevious",
        "extract",
        "wrap",
        "replace_with",
        "insert_before",
        "insert_after",
        "find_next",
        "find_all_next",
        "find_previous",
        "find_all_previous",
        "find_next_sibling",
        "find_next_siblings",
        "find_previous_sibling",
        "find_previous_siblings",
        "find_parent",
        "find_parents",
        "findNext",
        "findAllNext",
        "findPrevious",
        "findAllPrevious",
        "findNextSibling",
        "findNextSiblings",
        "findPreviousSibling",
        "findPreviousSiblings",
        "findParent",
        "findParents",
        "nextGenerator",
        "previousGenerator",
        "nextSiblingGenerator",
        "previousSiblingGenerator",
        "parentGenerator",
    ):
        _pe_fn = Tag.__dict__.get(_pe_name)
        if _pe_fn is not None and _pe_name not in PageElement.__dict__:
            setattr(PageElement, _pe_name, _pe_fn)

    # format_string is a PageElement method upstream, but the string subclass owns
    # the only implementation here, so it is republished from the other side.
    if "format_string" not in PageElement.__dict__:
        PageElement.format_string = NavigableString.__dict__["format_string"]

    # The BS3 spellings bs4 still answers to, and the four one-step navigation
    # properties, all of which upstream defines on PageElement.
    PageElement.replaceWith = PageElement.replace_with
    PageElement.replaceWithChildren = PageElement.unwrap
    PageElement.replace_with_children = PageElement.unwrap
    PageElement.next = property(lambda self: self.next_element)
    PageElement.previous = property(lambda self: self.previous_element)
    PageElement.nextSibling = property(lambda self: self.next_sibling)
    PageElement.previousSibling = property(lambda self: self.previous_sibling)

    # -- CSS select --------------------------------------------------------------
    # An+B, as soupsieve's RE_NTH sees it (`odd`/`even` are handled separately).
    _NTH_RE = re.compile(r"^([-+]?)(\d*n|\d+)([-+])?(\d+)?$")

    # The element `:scope` refers to: whatever select() was called on.
    _CSS_SCOPE = [None]

    def _parse_nth_spec(arg):
        # soupsieve's An+B parse: returns (a, b, var, of_parts) where `var` says
        # an `n` was present (the branch upstream bounds-checks) and `of_parts`
        # carries `:nth-child(An+B of S)`.
        content = (arg or "").strip()
        of_parts = None
        low = content.lower()
        cut = low.find(" of ")
        if cut >= 0:
            of_parts = _css_arg_parts(content[cut + 4 :])
            content = content[:cut]
        content = content.replace(" ", "").lower()
        if content == "even":
            return (2, 0, True, of_parts)
        if content == "odd":
            return (2, 1, True, of_parts)
        m = _NTH_RE.match(content)
        if m is None:
            _css_bad(arg or "")
        s1, a, s2, b = m.group(1), m.group(2), m.group(3), m.group(4)
        var = a.endswith("n")
        if a.startswith("n"):
            head = "1"
        elif var:
            head = a[:-1] or "1"
        else:
            head = a
        s1 = "-" if s1 == "-" else ""
        s2 = "-" if s2 == "-" else ""
        return (int(s1 + head, 10), int(s2 + (b or "0"), 10), var, of_parts)

    def _css_of_match(node, of_parts):
        if of_parts is None:
            return True
        for part in of_parts:
            if _complex_match(node, part):
                return True
        return False

    def _css_match_nth(node, a, b, var, last, of_type, of_parts):
        # A faithful port of soupsieve's CSSMatch.match_nth, quirks included: it
        # bounds every candidate index against the parent's *raw* child count
        # (strings and comments included), so an `An+B` selector can miss an
        # element that plain CSS index arithmetic would match.
        if not _css_of_match(node, of_parts):
            return False
        parent = getattr(node, "parent", None)
        children = parent.contents if isinstance(parent, Tag) else [node]
        last_index = len(children) - 1
        index = last_index if last else 0
        relative_index = 0
        count = 0
        count_incr = 1
        factor = -1 if last else 1
        idx = last_idx = (a * count + b) if var else a
        if var:
            # Only a variable index can be nudged into range.
            adjust = None
            while idx < 1 or idx > last_index:
                if idx < 0:
                    diff_low = 0 - idx
                    if adjust is not None and adjust == 1:
                        break
                    adjust = -1
                    count = count + count_incr
                    idx = last_idx = a * count + b
                    diff = 0 - idx
                    if diff >= diff_low:
                        break
                else:
                    diff_high = idx - last_index
                    if adjust is not None and adjust == -1:
                        break
                    adjust = 1
                    count = count + count_incr
                    idx = last_idx = a * count + b
                    diff = idx - last_index
                    if diff >= diff_high:
                        break
                    diff_high = diff
            lowest = count
            if a < 0:
                while idx >= 1:
                    lowest = count
                    count = count + count_incr
                    idx = last_idx = a * count + b
                count_incr = -1
            count = lowest
            idx = last_idx = (a * count + b) if var else a
        matched = False
        while 1 <= idx <= last_index + 1:
            child = None
            while 0 <= index <= last_index:
                child = children[index]
                index = index + factor
                if not isinstance(child, Tag):
                    continue
                if not _css_of_match(child, of_parts):
                    continue
                if of_type and child.name != node.name:
                    continue
                relative_index = relative_index + 1
                if relative_index == idx:
                    if child is node:
                        matched = True
                    else:
                        break
                if child is node:
                    break
            if child is node:
                break
            last_idx = idx
            count = count + count_incr
            if count < 0:
                break
            idx = (a * count + b) if var else a
            if last_idx == idx:
                break
        return matched

    class SelectorSyntaxError(Exception):
        """What soupsieve raises for a malformed selector; bs4 lets it through."""

    # Every pseudo-class soupsieve accepts. Anything outside this set -- and
    # every pseudo-element -- makes soupsieve raise NotImplementedError, so
    # this engine raises it too instead of quietly matching nothing.
    _CSS_PSEUDO_CLASSES = frozenset(
        [
            "active",
            "any-link",
            "checked",
            "current",
            "default",
            "defined",
            "dir",
            "disabled",
            "empty",
            "enabled",
            "first-child",
            "first-of-type",
            "focus",
            "focus-visible",
            "focus-within",
            "future",
            "has",
            "host",
            "host-context",
            "hover",
            "in-range",
            "indeterminate",
            "is",
            "lang",
            "last-child",
            "last-of-type",
            "link",
            "local-link",
            "matches",
            "not",
            "nth-child",
            "nth-last-child",
            "nth-last-of-type",
            "nth-of-type",
            "only-child",
            "only-of-type",
            "optional",
            "out-of-range",
            "past",
            "paused",
            "placeholder-shown",
            "playing",
            "read-only",
            "read-write",
            "required",
            "root",
            "scope",
            "target",
            "target-within",
            "user-invalid",
            "visited",
            "where",
            "contains",
            "-soup-contains",
            "-soup-contains-own",
        ]
    )

    # A tag name no markup can produce: how an unresolvable namespace prefix
    # ("ns|p" with no namespace map) is spelled so it matches nothing.
    _CSS_NEVER = chr(0) + "never"

    def _css_ident_ok(name):
        if not name:
            return False
        if name[0].isdigit():
            return False
        for ch in name:
            if ch.isalnum() or ch in ("-", "_", chr(92)) or ord(ch) > 127:
                continue
            return False
        return True

    def _css_bad(selector):
        raise SelectorSyntaxError("Invalid CSS selector: %r" % (selector,))

    def _split_commas(text):
        # Comma-splits a selector without cutting inside [attr], :not(...) or a
        # quoted value.
        parts = []
        buf = []
        square = 0
        paren = 0
        quote = None
        for ch in text:
            if quote is not None:
                buf.append(ch)
                if ch == quote:
                    quote = None
                continue
            if ch == _Q or ch == chr(39):
                quote = ch
                buf.append(ch)
            elif ch == "[":
                square = square + 1
                buf.append(ch)
            elif ch == "]":
                square = square - 1
                buf.append(ch)
            elif ch == "(":
                paren = paren + 1
                buf.append(ch)
            elif ch == ")":
                paren = paren - 1
                buf.append(ch)
            elif ch == "," and square == 0 and paren == 0:
                parts.append("".join(buf))
                buf = []
            else:
                buf.append(ch)
        parts.append("".join(buf))
        return parts

    def _parse_simple(tok):
        tag = None
        idv = None
        classes = []
        attrs = []
        pseudos = []
        i = 0
        n = len(tok)
        stop = (".", "#", "[", ":")
        # leading type selector
        j = i
        while j < n and tok[j] not in stop:
            j = j + 1
        t = tok[i:j]
        if t:
            local = t
            prefix = None
            if "|" in t:
                prefix, _sep, local = t.rpartition("|")
            if local != "*" and not _css_ident_ok(local):
                _css_bad(tok)
            if prefix is not None and prefix not in ("", "*"):
                if not _css_ident_ok(prefix):
                    _css_bad(tok)
                # No namespace map is ever passed here, so a real prefix can
                # never resolve -- soupsieve matches nothing in that case.
                tag = _CSS_NEVER
            elif local != "*":
                tag = local
        i = j
        while i < n:
            c = tok[i]
            if c == "." or c == "#":
                i = i + 1
                s = i
                while i < n and tok[i] not in stop:
                    i = i + 1
                name = tok[s:i]
                if not _css_ident_ok(name):
                    _css_bad(tok)
                if c == ".":
                    classes.append(name)
                else:
                    idv = name
            elif c == "[":
                i = i + 1
                s = i
                while i < n and tok[i] != "]":
                    i = i + 1
                if i >= n:
                    _css_bad(tok)
                body = tok[s:i]
                i = i + 1
                op = None
                for cand in ("~=", "|=", "^=", "$=", "*=", "="):
                    if cand in body:
                        an, av = body.split(cand, 1)
                        op = cand
                        av = av.strip()
                        quoted = (
                            len(av) >= 2 and av[0] == av[-1] and av[0] in (_Q, chr(39))
                        )
                        if quoted:
                            av = av[1:-1]
                        elif not av:
                            _css_bad(tok)
                        an = an.strip()
                        if not an:
                            _css_bad(tok)
                        attrs.append((an, op, av))
                        break
                if op is None:
                    an = body.strip()
                    if not an:
                        _css_bad(tok)
                    attrs.append((an, None, None))
            elif c == ":":
                i = i + 1
                element = False
                if i < n and tok[i] == ":":
                    element = True
                    i = i + 1
                s = i
                while i < n and tok[i] not in stop and tok[i] != "(":
                    i = i + 1
                pname = tok[s:i].lower()
                parg = None
                if i < n and tok[i] == "(":
                    depth = 0
                    s = i + 1
                    closed = False
                    while i < n:
                        if tok[i] == "(":
                            depth = depth + 1
                        elif tok[i] == ")":
                            depth = depth - 1
                            if depth == 0:
                                closed = True
                                break
                        i = i + 1
                    if not closed:
                        _css_bad(tok)
                    parg = tok[s:i]
                    i = i + 1
                if not pname:
                    _css_bad(tok)
                if element:
                    raise NotImplementedError(
                        "'::%s' pseudo-element is not implemented at this time."
                        % (pname,)
                    )
                if pname not in _CSS_PSEUDO_CLASSES:
                    raise NotImplementedError(
                        "':%s' pseudo-class is not implemented at this time." % (pname,)
                    )
                pseudos.append((pname, parg))
            else:
                i = i + 1
        return (tag, idv, classes, attrs, pseudos)

    def _element_siblings(node):
        parent = getattr(node, "parent", None)
        if parent is None:
            return []
        return [c for c in parent.contents if isinstance(c, Tag)]

    _CSS_FORM_TAGS = (
        "button",
        "input",
        "select",
        "textarea",
        "optgroup",
        "option",
        "fieldset",
    )

    def _css_attr(node, name):
        v = node.attrs.get(name)
        if isinstance(v, list):
            return " ".join(v)
        return v

    def _css_unquote(text):
        text = (text or "").strip()
        if len(text) >= 2 and text[0] == text[-1] and text[0] in (_Q, chr(39)):
            return text[1:-1]
        return text

    def _css_prev_element_siblings(node):
        sibs = _element_siblings(node)
        i = _index_of(sibs, node)
        return sibs[:i] if i >= 0 else []

    def _complex_steps_match(node, steps):
        # Right-to-left match of a full selector chain against one node: what
        # :not()/:is() need, since their arguments may contain combinators.
        combinator, tok = steps[-1]
        if not _simple_match(node, _parse_simple(tok)):
            return False
        rest = steps[:-1]
        if not rest:
            return True
        if combinator == "child":
            cands = [getattr(node, "parent", None)]
        elif combinator == "next":
            cands = _css_prev_element_siblings(node)[-1:]
        elif combinator == "sibs":
            cands = _css_prev_element_siblings(node)
        else:
            cands = []
            cur = getattr(node, "parent", None)
            while cur is not None:
                cands.append(cur)
                cur = getattr(cur, "parent", None)
        for c in cands:
            if isinstance(c, Tag) and _complex_steps_match(c, rest):
                return True
        return False

    def _complex_match(node, part):
        return _complex_steps_match(node, _tokenize_group(part))

    def _css_arg_parts(parg):
        parts = []
        for part in _split_commas(parg or ""):
            part = part.strip()
            if not part:
                _css_bad(parg or "")
            parts.append(part)
        if not parts:
            _css_bad(parg or "")
        return parts

    def _css_has(node, parg):
        for part in _css_arg_parts(parg):
            combinator = "desc"
            if part[0] == _GT:
                combinator = "child"
            elif part[0] == "+":
                combinator = "next"
            elif part[0] == "~":
                combinator = "sibs"
            if combinator != "desc":
                part = part[1:].strip()
                if not part:
                    _css_bad(parg or "")
            steps = _tokenize_group(part)
            steps = [(combinator, steps[0][1])] + steps[1:]
            found, _sibs = _run_steps([node], steps)
            if found:
                return True
        return False

    def _css_own_text(node):
        return "".join(
            str(c)
            for c in node.contents
            if isinstance(c, NavigableString) and not isinstance(c, PreformattedString)
        )

    def _css_disabled(node):
        if node.name not in _CSS_FORM_TAGS:
            return False
        if "disabled" in node.attrs:
            return True
        cur = getattr(node, "parent", None)
        while isinstance(cur, Tag):
            if cur.name in ("fieldset", "optgroup") and "disabled" in cur.attrs:
                return True
            cur = getattr(cur, "parent", None)
        return False

    def _css_lang_ok(node, parg):
        wants = [_css_unquote(p).lower() for p in _css_arg_parts(parg)]
        have = None
        cur = node
        while isinstance(cur, Tag):
            have = _css_attr(cur, "lang")
            if have is None:
                have = _css_attr(cur, "xml:lang")
            if have:
                break
            cur = getattr(cur, "parent", None)
        if not have:
            return False
        have = have.lower()
        for want in wants:
            if want == "*":
                return True
            if have == want or have.startswith(want + "-"):
                return True
        return False

    def _css_dir_ok(node, parg):
        want = _css_unquote(parg).lower()
        cur = node
        while isinstance(cur, Tag):
            have = (_css_attr(cur, "dir") or "").lower()
            if have in ("ltr", "rtl"):
                return have == want
            if have == "auto":
                return False
            cur = getattr(cur, "parent", None)
        return want == "ltr"

    def _pseudo_ok(node, pname, parg):
        if pname == "not":
            for part in _css_arg_parts(parg):
                if _complex_match(node, part):
                    return False
            return True
        if pname in ("is", "where", "matches"):
            for part in _css_arg_parts(parg):
                if _complex_match(node, part):
                    return True
            return False
        if pname == "has":
            return _css_has(node, parg)
        if pname == "empty":
            for c in node.contents:
                if isinstance(c, Tag):
                    return False
                if isinstance(c, NavigableString) and not isinstance(
                    c, PreformattedString
                ):
                    if str(c):
                        return False
            return True
        if pname == "root":
            parent = getattr(node, "parent", None)
            return parent is None or getattr(parent, "name", None) == "[document]"
        if pname == "scope":
            return node is _CSS_SCOPE[0]
        if pname in ("contains", "-soup-contains"):
            for want in _css_arg_parts(parg):
                if _css_unquote(want) in node.get_text():
                    return True
            return False
        if pname == "-soup-contains-own":
            own = _css_own_text(node)
            for want in _css_arg_parts(parg):
                if _css_unquote(want) in own:
                    return True
            return False
        if pname == "defined":
            return "-" not in node.name
        if pname in ("any-link", "link"):
            return node.name in ("a", "area", "link") and "href" in node.attrs
        if pname == "disabled":
            return _css_disabled(node)
        if pname == "enabled":
            return node.name in _CSS_FORM_TAGS and not _css_disabled(node)
        if pname == "required":
            return (
                node.name in ("input", "select", "textarea")
                and "required" in node.attrs
            )
        if pname == "optional":
            return (
                node.name in ("input", "select", "textarea")
                and "required" not in node.attrs
            )
        if pname == "checked":
            if node.name == "option":
                return "selected" in node.attrs
            return (
                node.name == "input"
                and (_css_attr(node, "type") or "").lower() in ("checkbox", "radio")
                and "checked" in node.attrs
            )
        if pname == "placeholder-shown":
            if node.name == "input":
                kind = (_css_attr(node, "type") or "text").lower()
                if kind in (
                    "text",
                    "search",
                    "url",
                    "tel",
                    "email",
                    "password",
                    "number",
                ):
                    return "placeholder" in node.attrs and not _css_attr(node, "value")
                return False
            if node.name == "textarea":
                return "placeholder" in node.attrs and not node.get_text()
            return False
        if pname == "read-write":
            if node.name in ("input", "textarea"):
                return "readonly" not in node.attrs and not _css_disabled(node)
            return (_css_attr(node, "contenteditable") or "").lower() in ("", "true")
        if pname == "read-only":
            return not _pseudo_ok(node, "read-write", None)
        if pname == "lang":
            return _css_lang_ok(node, parg)
        if pname == "dir":
            return _css_dir_ok(node, parg)
        if pname in (
            "nth-child",
            "nth-last-child",
            "nth-of-type",
            "nth-last-of-type",
            "first-child",
            "last-child",
            "only-child",
            "first-of-type",
            "last-of-type",
            "only-of-type",
        ):
            # soupsieve rewrites the positional pseudo-classes into nth-child /
            # nth-of-type selectors, so one code path answers all of them.
            of_type = "of-type" in pname
            if pname.startswith("nth-"):
                a, b, var, of_parts = _parse_nth_spec(parg or "")
                return _css_match_nth(
                    node, a, b, var, "last" in pname, of_type, of_parts
                )
            if pname.startswith("only-"):
                return _css_match_nth(
                    node, 1, 0, False, False, of_type, None
                ) and _css_match_nth(node, 1, 0, False, True, of_type, None)
            return _css_match_nth(
                node, 1, 0, False, pname.startswith("last-"), of_type, None
            )
        # Everything else soupsieve accepts describes live UI state (:hover,
        # :focus, :target, :visited, ...) that a parsed document never has.
        return False

    def _simple_match(node, simple):
        if not isinstance(node, Tag):
            return False
        tag, idv, classes, attrs, pseudos = simple
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
            if op == "|=" and not (hvs == av or hvs.startswith(av + "-")):
                return False
            if op == "^=" and not hvs.startswith(av):
                return False
            if op == "$=" and not hvs.endswith(av):
                return False
            if op == "*=" and av not in hvs:
                return False
        for pname, parg in pseudos:
            if not _pseudo_ok(node, pname, parg):
                return False
        return True

    def _tokenize_group(group):
        # Returns [(combinator, token)] with combinator in
        # ('desc', 'child', 'next', 'sibs'). Brackets, parentheses and quotes are
        # tracked so `[rel~="x"]` and `:not(a > b)` never split on their own
        # operators; anything unbalanced or a dangling combinator is a syntax
        # error, exactly as soupsieve reports it.
        steps = []
        combinator = "desc"
        pending = False
        buf = []
        square = 0
        paren = 0
        quote = None
        for ch in group:
            if quote is not None:
                buf.append(ch)
                if ch == quote:
                    quote = None
                continue
            if ch == _Q or ch == chr(39):
                quote = ch
                buf.append(ch)
            elif ch == "[":
                square = square + 1
                buf.append(ch)
            elif ch == "]":
                square = square - 1
                if square < 0:
                    _css_bad(group)
                buf.append(ch)
            elif ch == "(":
                paren = paren + 1
                buf.append(ch)
            elif ch == ")":
                paren = paren - 1
                if paren < 0:
                    _css_bad(group)
                buf.append(ch)
            elif square == 0 and paren == 0 and (ch.isspace() or ch in (_GT, "+", "~")):
                tok = "".join(buf).strip()
                buf = []
                if tok:
                    steps.append((combinator, tok))
                    combinator = "desc"
                    pending = False
                if ch == _GT or ch == "+" or ch == "~":
                    if not steps or pending:
                        _css_bad(group)
                    pending = True
                    if ch == _GT:
                        combinator = "child"
                    elif ch == "+":
                        combinator = "next"
                    else:
                        combinator = "sibs"
            else:
                buf.append(ch)
        if square != 0 or paren != 0 or quote is not None:
            _css_bad(group)
        tok = "".join(buf).strip()
        if tok:
            steps.append((combinator, tok))
        elif pending:
            _css_bad(group)
        if not steps:
            _css_bad(group)
        return steps

    def _following_siblings(node, first_only):
        sib = _sibling(node, 1)
        out = []
        while sib is not None:
            if isinstance(sib, Tag):
                out.append(sib)
                if first_only:
                    break
            sib = _sibling(sib, 1)
        return out

    def _run_steps(current, steps):
        # Walks one parsed selector chain from a candidate list. Returns the
        # surviving candidates and whether a sibling combinator was used (which
        # breaks document order, so the caller must re-sort).
        sibling_step = False
        for combinator, tok in steps:
            simple = _parse_simple(tok)
            nxt = []
            if combinator == "child":
                for node in current:
                    for c in node.contents if isinstance(node, Tag) else []:
                        if _simple_match(c, simple):
                            nxt.append(c)
            elif combinator in ("next", "sibs"):
                sibling_step = True
                taken = set()
                for node in current:
                    for c in _following_siblings(node, combinator == "next"):
                        if id(c) in taken:
                            continue
                        taken.add(id(c))
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
        return current, sibling_step

    def _select(root, selector, limit=None):
        # soupsieve scopes :scope to the element select() was called on -- for a
        # whole soup that is its root element.
        scope = root
        if getattr(root, "name", None) == "[document]":
            scope = None
            for c in root.contents:
                if isinstance(c, Tag):
                    scope = c
                    break
        previous_scope = _CSS_SCOPE[0]
        _CSS_SCOPE[0] = scope
        try:
            return _select_scoped(root, selector, limit)
        finally:
            _CSS_SCOPE[0] = previous_scope

    def _select_scoped(root, selector, limit=None):
        groups = []
        for part in _split_commas(selector):
            part = part.strip()
            if not part:
                _css_bad(selector)
            groups.append(part)
        results = []
        seen = set()
        sibling_step = False
        for group in groups:
            current, sibs = _run_steps([root], _tokenize_group(group))
            if sibs:
                sibling_step = True
            for node in current:
                if id(node) not in seen:
                    seen.add(id(node))
                    results.append(node)
                    # A plain descendant/child group already yields document
                    # order, so a limited search can stop as soon as it is
                    # satisfied.
                    if (
                        limit
                        and len(groups) == 1
                        and not sibling_step
                        and len(results) >= limit
                    ):
                        return results
        if len(results) > 1 and (len(groups) > 1 or sibling_step):
            # Groups and sibling combinators are matched candidate by candidate;
            # re-order the union so the caller sees document order, the way a real
            # CSS engine reports it.
            order = {}
            for i, d in enumerate(root.descendants):
                order[id(d)] = i
            results.sort(key=lambda n: order.get(id(n), -1))
        return results[:limit] if limit else results

    # -- serialization -----------------------------------------------------------
    # bs4 renders through a formatter: "minimal" escapes the three markup-critical
    # characters, "html"/"html5" additionally substitute named entities, None
    # escapes nothing, and a callable is used verbatim. The active formatter is a
    # stack rather than a parameter threaded through every helper, so the whole
    # (iterative, stack-safe) renderer keeps its shape.
    class EntitySubstitution:
        """bs4.dammit.EntitySubstitution: the character/entity tables bs4 renders with."""

        def _populate_class_variables():
            # Ported from bs4: every HTML5 named entity, minus the pure-ASCII ones
            # that would only make output less readable (except <>& which must be
            # escaped), with codepoint2name winning whenever one character has
            # several HTML5 names ("rsquo" rather than "rsquor").
            unicode_to_name = {}
            name_to_unicode = {}
            short_entities = set()
            long_entities_by_first_character = {}
            for name_with_semicolon, character in sorted(_hent.html5.items()):
                # The parsers handle references without the trailing semicolon,
                # so it is dropped here wherever it appears.
                if name_with_semicolon.endswith(";"):
                    name = name_with_semicolon[:-1]
                else:
                    name = name_with_semicolon
                if name not in name_to_unicode:
                    name_to_unicode[name] = character
                unicode_to_name[character] = name
                if (
                    len(character) == 1
                    and ord(character) < 128
                    and character not in _LT + _GT + _AMP
                ):
                    continue
                if len(character) > 1 and all(ord(x) < 128 for x in character):
                    continue
                if len(character) == 1:
                    short_entities.add(character)
                else:
                    long_entities_by_first_character.setdefault(
                        character[0], set()
                    ).add(character)
            # Some entities are a prefix of another entity: "\u2267" is
            # &GreaterFullEqual; but "\u2267\u0338" is &NotGreaterFullEqual;, so
            # the short form only matches when the long form does not.
            particles = set()
            for short in short_entities:
                long_versions = long_entities_by_first_character.get(short)
                if not long_versions:
                    particles.add(short)
                else:
                    ignore = "".join([x[1] for x in long_versions])
                    particles.add("%s(?![%s])" % (short, ignore))
            for long_entities in list(long_entities_by_first_character.values()):
                for long_entity in long_entities:
                    particles.add(long_entity)
            re_definition = "(%s)" % "|".join(particles)
            for codepoint, name in list(_hent.codepoint2name.items()):
                unicode_to_name[chr(codepoint)] = name
            return unicode_to_name, name_to_unicode, _re.compile(re_definition)

        (
            CHARACTER_TO_HTML_ENTITY,
            HTML_ENTITY_TO_CHARACTER,
            CHARACTER_TO_HTML_ENTITY_RE,
        ) = _populate_class_variables()

        CHARACTER_TO_XML_ENTITY = {
            chr(39): "apos",
            _Q: "quot",
            _AMP: "amp",
            _LT: "lt",
            _GT: "gt",
        }

        BARE_AMPERSAND_OR_BRACKET = _re.compile(
            "([<>]|&(?!#\\d+;|#x[0-9a-fA-F]+;|\\w+;))"
        )

        AMPERSAND_OR_BRACKET = _re.compile("([<>&])")

        @classmethod
        def _substitute_html_entity(cls, matchobj):
            entity = cls.CHARACTER_TO_HTML_ENTITY.get(matchobj.group(0))
            return _AMP + "%s;" % entity

        @classmethod
        def _substitute_xml_entity(cls, matchobj):
            entity = cls.CHARACTER_TO_XML_ENTITY[matchobj.group(0)]
            return _AMP + "%s;" % entity

        @classmethod
        def quoted_attribute_value(self, value):
            # Double quotes normally, single quotes when the value holds a double
            # quote, and &quot; only when it holds both kinds.
            quote_with = _Q
            if _Q in value:
                if chr(39) in value:
                    value = value.replace(_Q, _AMP + "quot;")
                else:
                    quote_with = chr(39)
            return quote_with + value + quote_with

        @classmethod
        def substitute_xml(cls, value, make_quoted_attribute=False):
            value = cls.AMPERSAND_OR_BRACKET.sub(cls._substitute_xml_entity, value)
            if make_quoted_attribute:
                value = cls.quoted_attribute_value(value)
            return value

        @classmethod
        def substitute_xml_containing_entities(cls, value, make_quoted_attribute=False):
            value = cls.BARE_AMPERSAND_OR_BRACKET.sub(cls._substitute_xml_entity, value)
            if make_quoted_attribute:
                value = cls.quoted_attribute_value(value)
            return value

        @classmethod
        def substitute_html(cls, s):
            return cls.CHARACTER_TO_HTML_ENTITY_RE.sub(cls._substitute_html_entity, s)

    def _sub_minimal(s):
        return EntitySubstitution.substitute_xml(str(s))

    def _sub_html(s):
        return EntitySubstitution.substitute_html(str(s))

    def _sub_none(s):
        return s

    # (substitution, void-element close prefix, empty attributes are booleans,
    # one level of pretty-print indentation, Formatter object or None)
    _FORMATTERS = {
        "minimal": (_sub_minimal, "/", False, " ", None),
        "html": (_sub_html, "/", False, " ", None),
        "html5": (_sub_html, "", True, " ", None),
        None: (_sub_none, "/", False, " ", None),
    }
    _CUR_FMT = [_FORMATTERS["minimal"]]
    # The encoding the current serialization claims to be in. Only <meta>
    # charset declarations read it, and only bs4's default of "utf-8" or an
    # explicit eventual_encoding ever lands here; None disables substitution.
    _CUR_ENC = [_DEFAULT_OUTPUT_ENCODING]

    def _fmt_of(formatter):
        if formatter is None or isinstance(formatter, str):
            if formatter in _FORMATTERS:
                return _FORMATTERS[formatter]
            # bs4 looks the name up in HTMLFormatter.REGISTRY, so a bad one
            # surfaces as KeyError, not ValueError.
            raise KeyError(formatter)
        if callable(formatter):
            return (formatter, "/", False, " ", None)
        prefix = getattr(formatter, "void_element_close_prefix", "/")
        indent = getattr(formatter, "indent", " ")
        if isinstance(indent, int):
            indent = " " * max(indent, 0)
        elif not isinstance(indent, str):
            indent = " "
        return (
            getattr(formatter, "substitute", None) or _sub_minimal,
            "" if prefix is None else prefix,
            bool(getattr(formatter, "empty_attributes_are_booleans", False)),
            indent,
            # A Formatter subclass may override attributes() to reorder or drop
            # attributes, so the renderer asks it instead of sorting itself.
            formatter if callable(getattr(formatter, "attributes", None)) else None,
        )

    def _with_formatter(formatter, fn, encoding=_DEFAULT_OUTPUT_ENCODING):
        _CUR_FMT.append(_fmt_of(formatter))
        _CUR_ENC.append(encoding)
        try:
            return fn()
        finally:
            _CUR_FMT.pop()
            _CUR_ENC.pop()

    def _esc_text(s):
        return _CUR_FMT[-1][0](s)

    def _esc_attr(s):
        return _esc_text(str(s)).replace(_Q, _AMP + "quot;")

    def _quote_attr(value):
        # bs4's quoting rule: double quotes normally, single quotes when the value
        # itself holds a double quote, and &quot; only when it holds both.
        v = _esc_text(str(value))
        if _Q in v:
            if chr(39) in v:
                return _Q + v.replace(_Q, _AMP + "quot;") + _Q
            return chr(39) + v + chr(39)
        return _Q + v + _Q

    def _open_tag(node, pad=""):
        void_close, bare_empty = _CUR_FMT[-1][1], _CUR_FMT[-1][2]
        fmt_obj = _CUR_FMT[-1][4]
        parts = [pad + _LT + node.name]
        if fmt_obj is not None:
            items = list(fmt_obj.attributes(node))
        else:
            items = [(k, node.attrs[k]) for k in sorted(node.attrs)]
        for k, v in items:
            if v is None or (bare_empty and v == ""):
                # A None-valued attribute renders bare (`<p data-x>`); only an
                # empty string renders as `data-x=""` -- unless the formatter
                # treats empty attributes as booleans, as html5 does.
                parts.append(" " + k)
                continue
            if isinstance(v, list):
                vs = " ".join(v)
            elif (
                isinstance(v, AttributeValueWithCharsetSubstitution)
                and _CUR_ENC[-1] is not None
            ):
                # A <meta> charset declaration always names the encoding the
                # document is being written out as, exactly as bs4 does.
                vs = v.encode(_CUR_ENC[-1])
            else:
                vs = v
            parts.append(" " + k + "=" + _quote_attr(vs))
        if getattr(node, "can_be_empty_element", node.name in _VOID) and (
            not node.contents
        ):
            parts.append(void_close + _GT)
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
            if isinstance(cur, PreformattedString):
                out.append(cur.PREFIX + str(cur) + cur.SUFFIX)
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
        pad = _CUR_FMT[-1][3] * depth
        if isinstance(node, PreformattedString):
            # The doctype's SUFFIX carries the newline the flat renderer needs;
            # in pretty mode the line break comes from the join instead.
            return pad + node.PREFIX + str(node) + node.SUFFIX.rstrip(_NL)
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
        pad = _CUR_FMT[-1][3] * depth
        open_tag, is_void = _open_tag(node, pad)
        if is_void:
            return open_tag
        close_tag = _LT + "/" + node.name + _GT
        if not kids:
            return open_tag + close_tag
        return open_tag + _NL + _NL.join(kids) + _NL + pad + close_tag

    def _pws(node):
        """Whether this tag's builder wants the whitespace inside it preserved."""
        tags = getattr(node, "preserve_whitespace_tags", None)
        return bool(tags) and node.name in tags

    def _render_pretty(node, depth=0):
        if not isinstance(node, Tag):
            return _pretty_string(node, depth)
        if _pws(node):
            # Whitespace-preserving elements are never re-indented: bs4 prints
            # <pre>/<textarea> content exactly as parsed.
            return _CUR_FMT[-1][3] * depth + _render_flat(node)
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
                if isinstance(child, Tag) and not _pws(child):
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

    def _render(
        node,
        pretty=False,
        depth=0,
        formatter="minimal",
        encoding=_DEFAULT_OUTPUT_ENCODING,
    ):
        return _with_formatter(
            formatter,
            lambda: _render_pretty(node, depth) if pretty else _render_flat(node),
            encoding,
        )

    # -- parser ------------------------------------------------------------------
    def _entity_text(name):
        # bs4 resolves named references itself (its parser runs with
        # convert_charrefs off).  A known entity becomes its character; an
        # unknown one stays the literal string "&name", the terminating
        # semicolon having been consumed by the tokenizer.
        character = EntitySubstitution.HTML_ENTITY_TO_CHARACTER.get(name)
        if character is not None:
            return character
        return _AMP + name

    def _charref_text(name, original_encoding=None):
        # Faithful to bs4: numeric references below 256 are often really
        # Windows-1252 code points (&#147; for a left double quote), so those
        # are decoded through the document encoding first.
        try:
            if name[:1] in ("x", "X"):
                code = int(name.lstrip("xX"), 16)
            else:
                code = int(name)
        except (ValueError, OverflowError):
            return chr(65533)
        data = None
        if code < 256:
            for encoding in (original_encoding, "windows-1252"):
                if not encoding:
                    continue
                try:
                    data = bytearray([code]).decode(encoding)
                except UnicodeDecodeError:
                    pass
        if not data:
            try:
                data = chr(code)
            except (ValueError, OverflowError):
                pass
        return data or chr(65533)

    class DetectsXMLParsedAsHTML:
        """bs4's mixin that warns when an XML document is parsed as HTML."""

        # Regular expression for seeing if markup has an <html> tag.
        LOOKS_LIKE_HTML = _re.compile("<[^ +]html", _re.I)
        LOOKS_LIKE_HTML_B = _re.compile(b"<[^ +]html", _re.I)

        XML_PREFIX = "<?xml"
        XML_PREFIX_B = b"<?xml"

        @classmethod
        def warn_if_markup_looks_like_xml(cls, markup, stacklevel=3):
            if isinstance(markup, bytes):
                prefix = cls.XML_PREFIX_B
                looks_like_html = cls.LOOKS_LIKE_HTML_B
            else:
                prefix = cls.XML_PREFIX
                looks_like_html = cls.LOOKS_LIKE_HTML
            if (
                markup is not None
                and markup.startswith(prefix)
                and not looks_like_html.search(markup[:500])
            ):
                cls._warn(stacklevel=stacklevel + 2)
                return True
            return False

        @classmethod
        def _warn(cls, stacklevel=5):
            _warnings.warn(
                XMLParsedAsHTMLWarning.MESSAGE,
                XMLParsedAsHTMLWarning,
                stacklevel=stacklevel,
            )

        def _initialize_xml_detector(self):
            self._first_processing_instruction = None
            self._root_tag = None

        def _document_might_be_xml(self, processing_instruction):
            if (
                self._first_processing_instruction is not None
                or self._root_tag is not None
            ):
                # The document has already started; stop checking.
                return
            self._first_processing_instruction = processing_instruction

        def _root_tag_encountered(self, name):
            if self._root_tag is not None:
                return
            self._root_tag = name
            if (
                name != "html"
                and self._first_processing_instruction is not None
                and self._first_processing_instruction.lower().startswith("xml ")
            ):
                # An XML declaration followed by a non-<html> root: this really
                # is XML being run through an HTML parser.
                self._warn()

    class _Builder(_hp.HTMLParser, DetectsXMLParsedAsHTML):
        # html.parser only knows <script>/<style> as raw-text elements before
        # CPython 3.13, but HTML5 -- and so bs4 on a newer stdlib -- also
        # swallows markup inside <xmp>/<iframe>/<noembed>/<noframes> and treats
        # <textarea>/<title> as RCDATA, where tags are literal text but
        # character references are still resolved.
        RCDATA_CONTENT_ELEMENTS = ("textarea", "title")
        CDATA_CONTENT_ELEMENTS = (
            "script",
            "style",
            "xmp",
            "iframe",
            "noembed",
            "noframes",
            "textarea",
            "title",
        )

        def set_cdata_mode(self, elem):
            self.cdata_elem = elem.lower()
            if self.cdata_elem in self.RCDATA_CONTENT_ELEMENTS:
                self.interesting = _re.compile(r"&|</\s*%s" % self.cdata_elem, _re.I)
            else:
                self.interesting = _re.compile(r"</\s*%s" % self.cdata_elem, _re.I)

        def __init__(self, builder=None):
            # convert_charrefs stays off (as in bs4's own html.parser builder) so
            # entity handling lives here rather than in html.parser, which would
            # silently keep an unknown `&foo;` verbatim. Character data therefore
            # arrives split around every reference, and _flush() re-joins each run
            # into the single NavigableString bs4 produces.
            _hp.HTMLParser.__init__(self, convert_charrefs=False)
            self.builder = builder
            self.root = Tag(None, builder, "[document]")
            self.stack = [self.root]
            self._data = []
            self._store_line_numbers = bool(builder and builder.store_line_numbers)
            # Names of empty elements bs4 has already closed on its own; one
            # later `</br>`-style end tag per entry is ignored.
            self.already_closed_empty_element = []
            self._initialize_xml_detector()

        def _pos(self):
            return self.getpos() if self._store_line_numbers else (None, None)

        def _cur(self):
            return self.stack[-1]

        def _flush(self):
            if self._data:
                text = "".join(self._data)
                self._data = []
                if not text:
                    return
                if text.strip(_ASCII_SPACES) == "" and not any(
                    _pws(t) for t in self.stack
                ):
                    text = _NL if _NL in text else " "
                # Text anywhere inside <script>/<style>/<template>/<rt>/<rp>
                # is wrapped in bs4's dedicated NavigableString subclass for
                # that container -- bs4 keeps a stack of container tags, so
                # nesting does not lose the special class.
                cls = NavigableString
                for open_tag in reversed(self.stack):
                    special = self.builder.string_containers.get(open_tag.name)
                    if special is not None:
                        cls = special
                        break
                self._cur().append(cls(text))

        def handle_starttag(self, tag, attrs, handle_empty_element=True):
            if self._root_tag is None:
                self._root_tag_encountered(tag)
            self._flush()
            line, pos = self._pos()
            attr_dict = {}
            for key, value in attrs:
                # bs4 turns a valueless attribute into '' here, in the builder,
                # so that a hand-built tag can still carry a None value.
                attr_dict[key] = "" if value is None else value
            t = Tag(
                None,
                self.builder,
                tag,
                attrs=attr_dict,
                sourceline=line,
                sourcepos=pos,
            )
            self._cur().append(t)
            if tag in _VOID and handle_empty_element:
                # html.parser sends no end event for a bare `<br>`, so bs4 closes
                # the tag itself and remembers to swallow one later `</br>`.
                self.already_closed_empty_element.append(tag)
            else:
                self.stack.append(t)
            return t

        def handle_startendtag(self, tag, attrs):
            # `<br/>`: bs4 leaves the closing to handle_endtag, which means an
            # earlier `<br>` in the same document eats this tag's end event and
            # leaves the empty element open. Upstream's quirk, reproduced.
            self.handle_starttag(tag, attrs, handle_empty_element=False)
            self.handle_endtag(tag)

        def handle_endtag(self, tag, check_already_closed=True):
            self._flush()
            if check_already_closed and tag in self.already_closed_empty_element:
                self.already_closed_empty_element.remove(tag)
                return
            for i in range(len(self.stack) - 1, 0, -1):
                if self.stack[i].name == tag:
                    del self.stack[i:]
                    return

        def handle_data(self, data):
            self._data.append(data)

        def handle_entityref(self, name):
            self._data.append(_entity_text(name))

        def handle_charref(self, name):
            self._data.append(_charref_text(name))

        def handle_comment(self, data):
            self._flush()
            self._cur().append(Comment(data))

        def handle_decl(self, decl):
            self._flush()
            if decl.lower().startswith("doctype"):
                self._cur().append(Doctype(decl[7:].strip()))

        def handle_pi(self, data):
            self._flush()
            self._document_might_be_xml(data)
            self._cur().append(ProcessingInstruction(data))

        def unknown_decl(self, data):
            self._flush()
            if data.startswith("CDATA["):
                # html.parser reports `<![CDATA[x]]>` as `CDATA[x`.
                body = data[6:]
                self._cur().append(CData(body[:-1] if body.endswith("]") else body))

        def close(self):
            _hp.HTMLParser.close(self)
            if self.cdata_elem and self.rawdata:
                # An unterminated raw-text element (`<textarea>a<b>c` with no
                # closing tag) keeps its remaining text instead of dropping it,
                # which is where html.parser leaves off in raw-text mode.
                leftover, self.rawdata = self.rawdata, ""
                self.handle_data(leftover)
            self._flush()
            _hp.HTMLParser.close(self)
            self._flush()

    def _strain(root, strainer):
        # parse_only pruning: keep the outermost nodes the strainer accepts and
        # drop everything else, the way bs4 narrows a parsed document.
        keep = []
        stack = list(root.contents)
        stack.reverse()
        while stack:
            node = stack.pop()
            if hasattr(strainer, "search"):
                # bs4's search() can answer False as well as None.
                ok = bool(strainer.search(node))
            else:
                ok = bool(strainer(node))
            if ok:
                keep.append(node)
                continue
            if isinstance(node, Tag):
                for c in reversed(node.contents):
                    stack.append(c)
        return keep

    # -- bs4.css -------------------------------------------------------------------
    # Upstream delegates .css to soupsieve. soupsieve is not installed here, so the
    # facade keeps bs4's API and signatures and delegates to this shim's own CSS
    # engine instead; `api` is None rather than the soupsieve module.

    def _css_root(node):
        root = node
        while root.parent is not None:
            root = root.parent
        return root

    def _css_match(tag, selector):
        if not isinstance(tag, Tag):
            return False
        return any(m is tag for m in _select(_css_root(tag), selector))

    def _css_closest(tag, selector):
        node = tag
        while node is not None:
            if _css_match(node, selector):
                return node
            node = node.parent
        return None

    def _css_filter(iterable, selector):
        if isinstance(iterable, Tag):
            iterable = iterable.contents
        return [n for n in iterable if _css_match(n, selector)]

    def _css_escape(ident):
        """CSS.escape from the CSSOM spec, which is what soupsieve implements."""
        out = []
        for i, ch in enumerate(ident):
            o = ord(ch)
            if o == 0:
                out.append("\ufffd")
            elif (1 <= o <= 0x1F) or o == 0x7F:
                out.append("\\%x " % o)
            elif i == 0 and 0x30 <= o <= 0x39:
                out.append("\\%x " % o)
            elif i == 1 and 0x30 <= o <= 0x39 and ident[0] == "-":
                out.append("\\%x " % o)
            elif i == 0 and ch == "-" and len(ident) == 1:
                out.append("\\-")
            elif o >= 0x80 or ch in "-_" or ch.isalnum():
                out.append(ch)
            else:
                out.append("\\" + ch)
        return "".join(out)

    class SoupSieve:
        """What css.compile() hands back: soupsieve's object, in miniature."""

        def __init__(self, pattern, namespaces=None, flags=0, **kwargs):
            self.pattern = pattern
            self.namespaces = namespaces
            self.flags = flags

        def __repr__(self):
            return "SoupSieve(pattern=%r, namespaces=%r, flags=%r)" % (
                self.pattern,
                self.namespaces,
                self.flags,
            )

        def match(self, tag):
            return _css_match(tag, self.pattern)

        def closest(self, tag):
            return _css_closest(tag, self.pattern)

        def filter(self, iterable):
            return _css_filter(iterable, self.pattern)

        def select(self, tag, limit=0):
            return _select(tag, self.pattern, limit=limit or None)

        def select_one(self, tag):
            found = _select(tag, self.pattern, limit=1)
            return found[0] if found else None

        def iselect(self, tag, limit=0):
            return iter(self.select(tag, limit))

    class CSS:
        """bs4.css.CSS: the object behind `tag.css`."""

        # Upstream this is the soupsieve module itself; nothing to point at here.
        api = None

        def __init__(self, tag, api=None):
            self.tag = tag
            if api is not None:
                self.api = api

        def escape(self, ident):
            return _css_escape(ident)

        def compile(self, select, namespaces=None, flags=0, **kwargs):
            return SoupSieve(select, namespaces, flags, **kwargs)

        def select_one(self, select, namespaces=None, flags=0, **kwargs):
            found = _select(self.tag, select, limit=1)
            return found[0] if found else None

        def select(self, select, namespaces=None, limit=0, flags=0, **kwargs):
            return ResultSet(None, _select(self.tag, select, limit=limit or None))

        def iselect(self, select, namespaces=None, limit=0, flags=0, **kwargs):
            return iter(_select(self.tag, select, limit=limit or None))

        def closest(self, select, namespaces=None, flags=0, **kwargs):
            return _css_closest(self.tag, select)

        def match(self, select, namespaces=None, flags=0, **kwargs):
            return _css_match(self.tag, select)

        def filter(self, select, namespaces=None, flags=0, **kwargs):
            return ResultSet(None, _css_filter(self.tag.contents, select))

    class BeautifulSoup(Tag):
        ROOT_TAG_NAME = "[document]"
        DEFAULT_BUILDER_FEATURES = ["html.parser"]
        ASCII_SPACES = _ASCII_SPACES

        def __init__(
            self,
            markup="",
            features=None,
            builder=None,
            parse_only=None,
            from_encoding=None,
            exclude_encodings=None,
            element_classes=None,
            **kwargs,
        ):
            # bs4 accepts a handful of BS3-era arguments, warns, and ignores
            # them; two more were merely renamed.
            for _name, _message in (
                (
                    "convertEntities",
                    "BS4 does not respect the convertEntities argument to the"
                    " BeautifulSoup constructor. Entities are always converted"
                    " to Unicode characters.",
                ),
                (
                    "markupMassage",
                    "BS4 does not respect the markupMassage argument to the"
                    " BeautifulSoup constructor. The tree builder is responsible"
                    " for any necessary markup massage.",
                ),
                (
                    "smartQuotesTo",
                    "BS4 does not respect the smartQuotesTo argument to the"
                    " BeautifulSoup constructor. Smart quotes are always"
                    " converted to Unicode characters.",
                ),
                (
                    "selfClosingTags",
                    "BS4 does not respect the selfClosingTags argument to the"
                    " BeautifulSoup constructor. The tree builder is responsible"
                    " for understanding self-closing tags.",
                ),
                (
                    "isHTML",
                    "BS4 does not respect the isHTML argument to the"
                    " BeautifulSoup constructor. Suggest you use"
                    " features='lxml' for HTML and features='lxml-xml' for XML.",
                ),
            ):
                if _name in kwargs:
                    del kwargs[_name]
                    _warnings.warn(_message)  # noqa: B028

            def deprecated_argument(old_name, new_name):
                if old_name in kwargs:
                    _warnings.warn(
                        'The "%s" argument to the BeautifulSoup constructor '
                        'has been renamed to "%s."' % (old_name, new_name),
                        DeprecationWarning,
                        stacklevel=3,
                    )
                    return kwargs.pop(old_name)
                return None

            parse_only = parse_only or deprecated_argument(
                "parseOnlyThese", "parse_only"
            )
            from_encoding = from_encoding or deprecated_argument(
                "fromEncoding", "from_encoding"
            )
            if from_encoding and isinstance(markup, str):
                _warnings.warn(  # noqa: B028
                    "You provided Unicode markup but also provided a value for"
                    " from_encoding. Your from_encoding will be ignored."
                )
                from_encoding = None
            self.element_classes = element_classes or {}
            # Resolve the TreeBuilder the way bs4 does: a class is instantiated
            # with whatever keyword arguments are left over, an instance is used
            # as-is and makes those arguments a warning instead. Unlike bs4 this
            # shim has only one builder, so `features` never selects another one.
            builder_class = HTMLParserTreeBuilder
            if isinstance(builder, type):
                builder_class, builder = builder, None
            if builder is None:
                builder = builder_class(**kwargs)
            elif kwargs:
                _warnings.warn(  # noqa: B028
                    "Keyword arguments to the BeautifulSoup constructor will be"
                    " ignored. These would normally be passed into the"
                    " TreeBuilder constructor, but a TreeBuilder instance was"
                    " passed in as `builder`."
                )
            self.builder = builder
            Tag.__init__(self, self, builder, "[document]")
            # Document-level inspection surface, all of it read by real bs4 code.
            self.hidden = 1
            self.is_xml = builder.is_xml
            self.known_xml = self.is_xml
            self.parser_class = BeautifulSoup
            self.parse_only = parse_only
            self.original_encoding = None
            self.declared_html_encoding = None
            self.contains_replacement_characters = False
            if hasattr(markup, "read"):
                markup = markup.read()
            if isinstance(markup, bytes):
                (
                    markup,
                    self.original_encoding,
                    self.declared_html_encoding,
                    self.contains_replacement_characters,
                ) = _decode_markup(markup, from_encoding, exclude_encodings)
            self.builder.initialize_soup(self)
            b = _Builder(builder=self.builder)
            b.feed(markup or "")
            b.close()
            if self.parse_only is not None:
                self.contents = _strain(b.root, self.parse_only)
            else:
                self.contents = b.root.contents
            for c in self.contents:
                c.parent = self
            # The parse-state attributes bs4 leaves behind on a finished soup.
            self.markup = None
            self.current_data = []
            self.tagStack = [self]
            self.currentTag = self
            self.preserve_whitespace_tag_stack = []
            self.string_container_stack = []
            self._most_recent_element = self._last_descendant(accept_self=False)
            # Every tag bs4 parses remembers the soup class that built it, and the
            # open-tag counter ends at zero for every name the document used.
            self.open_tag_counter = _collections.Counter()
            for node in self.descendants:
                if isinstance(node, Tag):
                    node.parser_class = BeautifulSoup
                    self.open_tag_counter[node.name] = 0
            self.builder.soup = None

        def new_tag(
            self,
            name,
            namespace=None,
            nsprefix=None,
            attrs={},  # noqa: B006
            sourceline=None,
            sourcepos=None,
            **kwattrs,
        ):
            """A new Tag, built the way this soup's TreeBuilder would build it."""
            kwattrs.update(attrs)
            return self.element_classes.get(Tag, Tag)(
                None,
                self.builder,
                name,
                namespace,
                nsprefix,
                kwattrs,
                sourceline=sourceline,
                sourcepos=sourcepos,
            )

        def new_string(self, s, subclass=None):
            return (subclass or NavigableString)(s)

        # -- bs4's document-level surface ----------------------------------------

        NO_PARSER_SPECIFIED_WARNING = (
            "No parser was explicitly specified, so I'm using the best available"
            ' %(markup_type)s parser for this system ("%(parser)s"). This usually'
            " isn't a problem, but if you run this code on another system, or in a"
            " different virtual environment, it may use a different parser and"
            " behave differently.\n\nThe code that caused this warning is on line"
            " %(line_number)s of the file %(filename)s. To get rid of this warning,"
            " pass the additional argument 'features=\"%(parser)s\"' to the"
            " BeautifulSoup constructor.\n"
        )

        @staticmethod
        def _decode_markup(markup):
            """Make `markup` safe to interpolate into a warning message.

            Unrelated to the module-level `_decode_markup` further down, which
            does real encoding detection; bs4 gives both the same name.
            """
            if isinstance(markup, bytes):
                return markup.decode("utf-8", "replace")
            return markup

        @classmethod
        def _markup_is_url(cls, markup):
            """Does this 'markup' look like someone passed a URL by mistake?"""
            if isinstance(markup, bytes):
                space, prefixes = b" ", (b"http:", b"https:")
            elif isinstance(markup, str):
                space, prefixes = " ", ("http:", "https:")
            else:
                return False
            return any(markup.startswith(p) for p in prefixes) and space not in markup

        @classmethod
        def _markup_resembles_filename(cls, markup):
            """Does this 'markup' look like a filename someone forgot to open?"""
            path_characters = "/\\"
            extensions = [".html", ".htm", ".xml", ".xhtml", ".txt"]
            if isinstance(markup, bytes):
                path_characters = path_characters.encode("utf8")
                extensions = [x.encode("utf8") for x in extensions]
            elif not isinstance(markup, str):
                return False
            if any(x in markup for x in path_characters):
                return True
            return any(markup.lower().endswith(ext) for ext in extensions)

        def string_container(self, base_class=None):
            """Which NavigableString subclass a string in this position gets."""
            container = base_class or NavigableString
            container = self.element_classes.get(container, container)
            if self.string_container_stack and container is NavigableString:
                container = self.builder.string_containers.get(
                    self.string_container_stack[-1].name, container
                )
            return container

        def reset(self):
            """Empty the soup and put it back in its just-constructed state."""
            Tag.__init__(self, self, self.builder, self.ROOT_TAG_NAME)
            self.hidden = 1
            self.known_xml = self.is_xml
            self.parser_class = BeautifulSoup
            self.builder.reset()
            self.current_data = []
            self.currentTag = None
            self.tagStack = []
            self.open_tag_counter = _collections.Counter()
            self.preserve_whitespace_tag_stack = []
            self.string_container_stack = []
            self._most_recent_element = None
            self.tagStack.append(self)
            self.currentTag = self

        # ---- bs4's parse protocol -------------------------------------
        # A TreeBuilder drives the soup through these methods, so a custom
        # builder (bs4's documented extension point) works here too. This
        # shim's own html.parser path builds the tree directly, and linkage
        # (next_element and friends) is derived rather than hand-wired, so
        # `_linkage_fixer` has nothing to repair.

        def popTag(self):
            """Internal method called by _popToTag when a tag is closed."""
            tag = self.tagStack.pop()
            if tag.name in self.open_tag_counter:
                self.open_tag_counter[tag.name] -= 1
            if (
                self.preserve_whitespace_tag_stack
                and tag is self.preserve_whitespace_tag_stack[-1]
            ):
                self.preserve_whitespace_tag_stack.pop()
            if self.string_container_stack and tag is self.string_container_stack[-1]:
                self.string_container_stack.pop()
            if self.tagStack:
                self.currentTag = self.tagStack[-1]
            return self.currentTag

        def pushTag(self, tag):
            """Internal method called by handle_starttag when a tag is opened."""
            if self.currentTag is not None:
                self.currentTag.contents.append(tag)
                tag.parent = self.currentTag
            self.tagStack.append(tag)
            self.currentTag = self.tagStack[-1]
            if tag.name != self.ROOT_TAG_NAME:
                self.open_tag_counter[tag.name] += 1
            if tag.name in self.builder.preserve_whitespace_tags:
                self.preserve_whitespace_tag_stack.append(tag)
            if tag.name in self.builder.string_containers:
                self.string_container_stack.append(tag)

        def endData(self, containerClass=None):
            """Called by the tree builder when a data segment ends."""
            if self.current_data:
                current_data = "".join(self.current_data)
                # If whitespace is not preserved, and this string contains
                # nothing but ASCII spaces, replace it with a single space
                # or newline.
                if not self.preserve_whitespace_tag_stack:
                    strippable = True
                    for i in current_data:
                        if i not in self.ASCII_SPACES:
                            strippable = False
                            break
                    if strippable:
                        if "\n" in current_data:
                            current_data = "\n"
                        else:
                            current_data = " "

                self.current_data = []

                # Should we add this string to the tree at all?
                if (
                    self.parse_only
                    and len(self.tagStack) <= 1
                    and (
                        not self.parse_only.text
                        or not self.parse_only.search(current_data)
                    )
                ):
                    return

                containerClass = self.string_container(containerClass)
                o = containerClass(current_data)
                self.object_was_parsed(o)

        def object_was_parsed(self, o, parent=None, most_recent_element=None):
            """Integrate an object into the parse tree."""
            if parent is None:
                parent = self.currentTag
            o.parent = parent
            parent.contents.append(o)
            self._most_recent_element = o

        def _linkage_fixer(self, el):
            """No-op: this shim derives linkage from the tree, so it is sound."""
            return None

        def _popToTag(self, name, nsprefix=None, inclusivePop=True):
            """Pop the tag stack up to and including the most recent `name`."""
            if name == self.ROOT_TAG_NAME:
                # The BeautifulSoup object itself can never be popped.
                return

            most_recently_popped = None

            stack_size = len(self.tagStack)
            for i in range(stack_size - 1, 0, -1):
                if not self.open_tag_counter.get(name):
                    break
                t = self.tagStack[i]
                if name == t.name and nsprefix == t.prefix:
                    if inclusivePop:
                        most_recently_popped = self.popTag()
                    break
                most_recently_popped = self.popTag()

            return most_recently_popped

        def handle_starttag(
            self,
            name,
            namespace,
            nsprefix,
            attrs,
            sourceline=None,
            sourcepos=None,
            namespaces=None,
        ):
            """Called by the tree builder when a new tag is encountered.

            Returns None when an active SoupStrainer rejected the tag.
            """
            self.endData()

            if (
                self.parse_only
                and len(self.tagStack) <= 1
                and (
                    self.parse_only.text or not self.parse_only.search_tag(name, attrs)
                )
            ):
                return None

            tag = self.element_classes.get(Tag, Tag)(
                self,
                self.builder,
                name,
                namespace,
                nsprefix,
                attrs,
                self.currentTag,
                self._most_recent_element,
                sourceline=sourceline,
                sourcepos=sourcepos,
                namespaces=namespaces,
            )
            if tag is None:
                return tag
            self._most_recent_element = tag
            self.pushTag(tag)
            return tag

        def handle_endtag(self, name, nsprefix=None):
            """Called by the tree builder when an ending tag is encountered."""
            self.endData()
            self._popToTag(name, nsprefix)

        def handle_data(self, data):
            """Called by the tree builder when a chunk of text is encountered."""
            self.current_data.append(data)

        def _feed(self):
            """Parse `self.markup`, which the builder must know how to feed."""
            self.builder.reset()
            self.builder.feed(self.markup)
            # Close out any unfinished strings and close all the open tags.
            self.endData()
            while self.currentTag.name != self.ROOT_TAG_NAME:
                self.popTag()

        def decode(
            self,
            pretty_print=False,
            eventual_encoding=_DEFAULT_OUTPUT_ENCODING,
            formatter="minimal",
        ):
            # The soup's first positional is a flag, not an indent level.
            if not pretty_print:
                return _render(self, formatter=formatter, encoding=eventual_encoding)
            out = _render(
                self,
                pretty=True,
                depth=0,
                formatter=formatter,
                encoding=eventual_encoding,
            )
            return out if out.endswith(_NL) else out + _NL

        def encode(
            self,
            encoding=_DEFAULT_OUTPUT_ENCODING,
            indent_level=None,
            formatter="minimal",
            errors="xmlcharrefreplace",
        ):
            return self.decode(indent_level is not None, encoding, formatter).encode(
                encoding, errors
            )

        def __str__(self):
            return _render(self)

        def __repr__(self):
            return _render(self)

    class CData(PreformattedString):
        PREFIX = "<![CDATA["
        SUFFIX = "]]" + _GT

    class Doctype(PreformattedString):
        PREFIX = _LT + "!DOCTYPE "
        SUFFIX = _GT + _NL

        @classmethod
        def for_name_and_ids(cls, name, pub_id, system_id):
            """Build a Doctype from a doctype's name and identifiers."""
            value = name or ""
            if pub_id is not None:
                value += ' PUBLIC "%s"' % pub_id
                if system_id is not None:
                    value += ' "%s"' % system_id
            elif system_id is not None:
                value += ' SYSTEM "%s"' % system_id
            return Doctype(value)

    class Declaration(PreformattedString):
        PREFIX = _LT + "?"
        SUFFIX = "?" + _GT

    class ProcessingInstruction(PreformattedString):
        PREFIX = _LT + "?"
        SUFFIX = _GT

    class XMLProcessingInstruction(ProcessingInstruction):
        PREFIX = _LT + "?"
        SUFFIX = "?" + _GT

    # bs4 rewrites a <meta> charset declaration on output so it names the
    # encoding the document is actually being serialized to. It does that by
    # storing the attribute value in one of these str subclasses, whose
    # .encode() returns a *name*, not bytes.
    PYTHON_SPECIFIC_ENCODINGS = {
        "idna",
        "mbcs",
        "oem",
        "palmos",
        "punycode",
        "raw-unicode-escape",
        "raw_unicode_escape",
        "string-escape",
        "string_escape",
        "undefined",
        "unicode-escape",
        "unicode_escape",
    }

    class AttributeValueWithCharsetSubstitution(str):
        """An attribute value that depends on the eventual output encoding."""

    class CharsetMetaAttributeValue(AttributeValueWithCharsetSubstitution):
        """The value of an HTML5-style <meta charset="...">."""

        def __new__(cls, original_value):
            obj = str.__new__(cls, original_value)
            obj.original_value = original_value
            return obj

        def encode(self, encoding):
            # Encodings Python understands but no document can declare render
            # as an empty value rather than a lie.
            if encoding in PYTHON_SPECIFIC_ENCODINGS:
                return ""
            return encoding

    class ContentMetaAttributeValue(AttributeValueWithCharsetSubstitution):
        """The value of <meta http-equiv="Content-type" content="...charset=...">."""

        CHARSET_RE = _re.compile("((^|;)\\s*charset=)([^;]*)", _re.M)

        def __new__(cls, original_value):
            if cls.CHARSET_RE.search(original_value) is None:
                # Nothing to substitute, so bs4 hands back a plain string.
                return str.__new__(str, original_value)
            obj = str.__new__(cls, original_value)
            obj.original_value = original_value
            return obj

        def encode(self, encoding):
            if encoding in PYTHON_SPECIFIC_ENCODINGS:
                return ""
            return self.CHARSET_RE.sub(
                lambda m: m.group(1) + encoding, self.original_value
            )

    def _set_up_substitutions(tag):
        """bs4's HTMLTreeBuilder.set_up_substitutions, run as a tag is built."""
        if tag.name != "meta":
            return False
        charset = tag.attrs.get("charset")
        content = tag.attrs.get("content")
        http_equiv = tag.attrs.get("http-equiv")
        if charset is not None:
            tag.attrs["charset"] = CharsetMetaAttributeValue(charset)
            return True
        if (
            content is not None
            and isinstance(http_equiv, str)
            and http_equiv.lower() == "content-type"
        ):
            tag.attrs["content"] = ContentMetaAttributeValue(content)
            return True
        return False

    # bs4 wraps the text of a few containers in its own NavigableString subclass,
    # so `type(soup.script.string)` tells you what kind of text you are holding.
    class Script(NavigableString):
        pass

    class Stylesheet(NavigableString):
        pass

    class TemplateString(NavigableString):
        pass

    class RubyTextString(NavigableString):
        pass

    class RubyParenthesisString(NavigableString):
        pass

    _STRING_CONTAINERS = {
        "script": Script,
        "style": Stylesheet,
        "template": TemplateString,
        "rt": RubyTextString,
        "rp": RubyParenthesisString,
    }

    # What .strings/.get_text() consider text by default: no comments, no
    # processing instructions, no script or style bodies.
    Tag.DEFAULT_INTERESTING_STRING_TYPES = (NavigableString, CData)

    class FeatureNotFound(ValueError):
        pass

    class ParserRejectedMarkup(Exception):
        pass

    class StopParsing(Exception):
        pass

    class GuessedAtParserWarning(UserWarning):
        pass

    class MarkupResemblesLocatorWarning(UserWarning):
        pass

    class XMLParsedAsHTMLWarning(UserWarning):
        MESSAGE = (
            "It looks like you're parsing an XML document using an HTML "
            "parser. If this really is an HTML document (maybe it's XHTML?), "
            "you can ignore or filter this warning. If it's XML, you should "
            "know that using an XML parser will be more reliable. To parse "
            "this document as XML, make sure you have the lxml package "
            'installed, and pass the keyword argument `features="xml"` into '
            "the BeautifulSoup constructor."
        )

    class TreeBuilder:
        """Base of bs4's builder hierarchy; this shim ships exactly one subclass."""

        NAME = "[Unknown tree builder]"
        ALTERNATE_NAMES = []
        features = []

        is_xml = False
        picklable = False
        empty_element_tags = None

        # A value for these tag/attribute combinations is a space- or
        # comma-separated list of CDATA, rather than a single CDATA.
        DEFAULT_CDATA_LIST_ATTRIBUTES = _collections.defaultdict(list)

        # Whitespace should be preserved inside these tags.
        DEFAULT_PRESERVE_WHITESPACE_TAGS = set()

        # The textual contents of tags with these names should be
        # instantiated with some class other than NavigableString.
        DEFAULT_STRING_CONTAINERS = {}

        USE_DEFAULT = object()

        # Most parsers don't keep track of line numbers.
        TRACKS_LINE_NUMBERS = False

        def __init__(
            self,
            multi_valued_attributes=USE_DEFAULT,
            preserve_whitespace_tags=USE_DEFAULT,
            store_line_numbers=USE_DEFAULT,
            string_containers=USE_DEFAULT,
        ):
            self.soup = None
            if multi_valued_attributes is self.USE_DEFAULT:
                multi_valued_attributes = self.DEFAULT_CDATA_LIST_ATTRIBUTES
            self.cdata_list_attributes = multi_valued_attributes
            if preserve_whitespace_tags is self.USE_DEFAULT:
                preserve_whitespace_tags = self.DEFAULT_PRESERVE_WHITESPACE_TAGS
            self.preserve_whitespace_tags = preserve_whitespace_tags
            if store_line_numbers == self.USE_DEFAULT:
                store_line_numbers = self.TRACKS_LINE_NUMBERS
            self.store_line_numbers = store_line_numbers
            if string_containers == self.USE_DEFAULT:
                string_containers = self.DEFAULT_STRING_CONTAINERS
            self.string_containers = string_containers

        def initialize_soup(self, soup):
            self.soup = soup

        def can_be_empty_element(self, tag_name):
            if self.empty_element_tags is None:
                return True
            return tag_name in self.empty_element_tags

        def prepare_markup(self, markup, user_specified_encoding=None, **kwargs):
            yield markup, None, user_specified_encoding, False

        def reset(self):
            return None

        def feed(self, markup):
            raise NotImplementedError()

        def test_fragment_to_document(self, fragment):
            """Wrap a fragment to make it a document. Only tests use this."""
            return fragment

        def set_up_substitutions(self, tag):
            """Whether a <meta> charset stand-in was installed. See the subclass."""
            return False

        def _replace_cdata_list_attribute_values(self, tag_name, attrs):
            """Turn class="foo bar" into class=["foo", "bar"], in place."""
            if not attrs:
                return attrs
            if self.cdata_list_attributes:
                universal = self.cdata_list_attributes.get("*", [])
                tag_specific = self.cdata_list_attributes.get(tag_name.lower(), None)
                for attr in list(attrs.keys()):
                    if attr in universal or (tag_specific and attr in tag_specific):
                        value = attrs[attr]
                        if isinstance(value, str):
                            values = nonwhitespace_re.findall(value)
                        else:
                            # Already a list: leave it alone rather than
                            # splitting it a second time.
                            values = value
                        attrs[attr] = values
            return attrs

    class SAXTreeBuilder(TreeBuilder):
        """bs4 ships this as a demonstration; nothing uses it."""

        def feed(self, markup):
            raise NotImplementedError()

        def close(self):
            pass

        def startElement(self, name, attrs):
            attrs = dict((key[1], value) for key, value in list(attrs.items()))
            self.soup.handle_starttag(name, attrs)

        def endElement(self, name):
            self.soup.handle_endtag(name)

        def startElementNS(self, nsTuple, nodeName, attrs):
            # This is fine for HTML but not for XML.
            self.startElement(nodeName, attrs)

        def endElementNS(self, nsTuple, nodeName):
            # This is fine for HTML but not for XML.
            self.endElement(nodeName)

        def startPrefixMapping(self, prefix, nodeValue):
            # Ignore the prefix mapping, as bs4 does.
            pass

        def endPrefixMapping(self, prefix):
            # Ignore the prefix mapping, as bs4 does.
            pass

        def characters(self, content):
            self.soup.handle_data(content)

        def startDocument(self):
            pass

        def endDocument(self):
            pass

    class TreeBuilderRegistry:
        """Feature -> builder lookup; every HTML feature resolves to the one builder."""

        def __init__(self):
            self.builders = []
            self.builders_for_feature = {}

        def register(self, treebuilder_class):
            for feature in treebuilder_class.features:
                self.builders_for_feature.setdefault(feature, []).insert(
                    0, treebuilder_class
                )
            self.builders.insert(0, treebuilder_class)

        def lookup(self, *features):
            if not self.builders:
                return None
            if not features:
                return self.builders[0]
            candidates = None
            for feature in features:
                these = self.builders_for_feature.get(feature)
                if not these:
                    return None
                if candidates is None:
                    candidates = list(these)
                else:
                    candidates = [c for c in candidates if c in these]
                    if not candidates:
                        return None
            return candidates[0] if candidates else None

    class ParserRejectedMarkup(Exception):
        """Raised by bs4 builders that refuse markup; kept for `except` clauses."""

    class HTMLTreeBuilder(TreeBuilder):
        """bs4's builder for HTML in general: which tags are void, which preserve
        whitespace, which attributes hold lists, and how a <meta> charset
        declaration is rewritten. HTMLParserTreeBuilder specializes it.
        """

        empty_element_tags = _VOID

        # bs4 keeps these on HTMLTreeBuilder and copies them onto the instance in
        # TreeBuilder.__init__, where the BeautifulSoup constructor's
        # multi_valued_attributes/preserve_whitespace_tags/string_containers
        # keyword arguments can override them.
        DEFAULT_PRESERVE_WHITESPACE_TAGS = _PRESERVE_WS
        DEFAULT_STRING_CONTAINERS = _STRING_CONTAINERS
        DEFAULT_CDATA_LIST_ATTRIBUTES = _CDATA_LIST_ATTRIBUTES

        # HTML's block-level elements. bs4 does not treat them specially; it
        # just makes the list available.
        block_elements = set(
            [
                "address",
                "article",
                "aside",
                "blockquote",
                "canvas",
                "dd",
                "div",
                "dl",
                "dt",
                "fieldset",
                "figcaption",
                "figure",
                "footer",
                "form",
                "h1",
                "h2",
                "h3",
                "h4",
                "h5",
                "h6",
                "header",
                "hr",
                "li",
                "main",
                "nav",
                "noscript",
                "ol",
                "output",
                "p",
                "pre",
                "section",
                "table",
                "tfoot",
                "ul",
                "video",
            ]
        )

        def set_up_substitutions(self, tag):
            """Install the <meta> charset stand-in, as HTMLTreeBuilder does."""
            return bool(_set_up_substitutions(tag))

    class HTMLParserTreeBuilder(HTMLTreeBuilder):
        """The one tree builder this shim has: stdlib html.parser.

        `soup.builder` is how bs4 code asks which parser produced a tree and which
        tags that parser treats as void, whitespace-preserving or list-valued.
        """

        NAME = "html.parser"
        ALTERNATE_NAMES = []
        features = ["html.parser", "html", "strict"]
        is_xml = False
        picklable = True
        TRACKS_LINE_NUMBERS = True

        def __init__(self, parser_args=None, parser_kwargs=None, **kwargs):
            parser_args = parser_args or []
            parser_kwargs = parser_kwargs or {}
            # bs4 turns entity conversion off and handles references itself.
            parser_kwargs["convert_charrefs"] = False
            self.parser_args = (parser_args, parser_kwargs)
            TreeBuilder.__init__(self, **kwargs)

        def feed(self, markup):
            """Parse `markup` into the soup this builder is attached to.

            bs4 drives BeautifulSoup's handle_* protocol from inside its
            html.parser subclass. This shim's parser builds the nodes itself,
            so feeding hands each finished node to `object_was_parsed` --
            the same entry point a custom builder would use.
            """
            parser = _Builder(builder=self)
            parser.feed(markup)
            parser.close()
            # A finished soup detaches its builder (`builder.soup = None`), so
            # feeding one of those raises AttributeError here, exactly as bs4's
            # parser does when it reaches for `self.soup.handle_starttag`.
            soup = self.soup
            for node in list(parser.root.contents):
                node.parent = None
                soup.object_was_parsed(node)

    xml_encoding = "^\\s*<\\?.*encoding=['\"](.*?)['\"].*\\?>"
    html_meta = "<\\s*meta[^>]+charset\\s*=\\s*[\"']?([^>]*?)[ /;'\">]"
    encoding_res = {
        bytes: {
            "html": _re.compile(html_meta.encode("ascii"), _re.I),
            "xml": _re.compile(xml_encoding.encode("ascii"), _re.I),
        },
        str: {
            "html": _re.compile(html_meta, _re.I),
            "xml": _re.compile(xml_encoding, _re.I),
        },
    }

    class EncodingDetector:
        """bs4.dammit.EncodingDetector: the candidate encodings, in bs4's order."""

        def __init__(
            self,
            markup,
            known_definite_encodings=None,
            is_html=False,
            exclude_encodings=None,
            user_encodings=None,
            override_encodings=None,
        ):
            self.known_definite_encodings = list(known_definite_encodings or [])
            if override_encodings:
                self.known_definite_encodings += override_encodings
            self.user_encodings = user_encodings or []
            exclude_encodings = exclude_encodings or []
            self.exclude_encodings = set([x.lower() for x in exclude_encodings])
            self.chardet_encoding = None
            self.is_html = is_html
            self.declared_encoding = None

            # First order of business: strip a byte-order mark.
            self.markup, self.sniffed_encoding = self.strip_byte_order_mark(markup)

        def _usable(self, encoding, tried):
            """Should we even bother to try this encoding?"""
            if encoding is not None:
                encoding = encoding.lower()
                if encoding in self.exclude_encodings:
                    return False
                if encoding not in tried:
                    tried.add(encoding)
                    return True
            return False

        @property
        def encodings(self):
            """Yield a number of encodings that might work for this markup."""
            tried = set()

            for e in self.known_definite_encodings:
                if self._usable(e, tried):
                    yield e

            if self._usable(self.sniffed_encoding, tried):
                yield self.sniffed_encoding

            for e in self.user_encodings:
                if self._usable(e, tried):
                    yield e

            if self.declared_encoding is None:
                self.declared_encoding = self.find_declared_encoding(
                    self.markup, self.is_html
                )
            if self._usable(self.declared_encoding, tried):
                yield self.declared_encoding

            if self.chardet_encoding is None:
                self.chardet_encoding = chardet_dammit(self.markup)
            if self._usable(self.chardet_encoding, tried):
                yield self.chardet_encoding

            for e in ("utf-8", "windows-1252"):
                if self._usable(e, tried):
                    yield e

        @classmethod
        def strip_byte_order_mark(cls, data):
            """If a byte-order mark is present, strip it and return the encoding it implies."""
            encoding = None
            if isinstance(data, str):
                # Unicode data cannot have a byte-order mark.
                return data, encoding
            if (
                (len(data) >= 4)
                and (data[:2] == b"\xfe\xff")
                and (data[2:4] != "\x00\x00")
            ):
                encoding = "utf-16be"
                data = data[2:]
            elif (
                (len(data) >= 4)
                and (data[:2] == b"\xff\xfe")
                and (data[2:4] != "\x00\x00")
            ):
                encoding = "utf-16le"
                data = data[2:]
            elif data[:3] == b"\xef\xbb\xbf":
                encoding = "utf-8"
                data = data[3:]
            elif data[:4] == b"\x00\x00\xfe\xff":
                encoding = "utf-32be"
                data = data[4:]
            elif data[:4] == b"\xff\xfe\x00\x00":
                encoding = "utf-32le"
                data = data[4:]
            return data, encoding

        @classmethod
        def find_declared_encoding(
            cls, markup, is_html=False, search_entire_document=False
        ):
            """Given a document, tries to find its declared encoding."""
            if search_entire_document:
                xml_endpos = html_endpos = len(markup)
            else:
                xml_endpos = 1024
                html_endpos = max(2048, int(len(markup) * 0.05))

            if isinstance(markup, bytes):
                res = encoding_res[bytes]
            else:
                res = encoding_res[str]

            xml_re = res["xml"]
            html_re = res["html"]
            declared_encoding = None
            declared_encoding_match = xml_re.search(markup, endpos=xml_endpos)
            if not declared_encoding_match and is_html:
                declared_encoding_match = html_re.search(markup, endpos=html_endpos)
            if declared_encoding_match is not None:
                declared_encoding = declared_encoding_match.groups()[0]
            if declared_encoding:
                if isinstance(declared_encoding, bytes):
                    declared_encoding = declared_encoding.decode("ascii", "replace")
                return declared_encoding.lower()
            return None

    class UnicodeDammit:
        """bs4.dammit.UnicodeDammit: bytes in, str out, plus the encoding it guessed."""

        # Maps commonly seen "charset" values to Python codec names.
        CHARSET_ALIASES = {"macintosh": "mac-roman", "x-sjis": "shift-jis"}

        ENCODINGS_WITH_SMART_QUOTES = [
            "windows-1252",
            "iso-8859-1",
            "iso-8859-2",
        ]

        def __init__(
            self,
            markup,
            known_definite_encodings=[],  # noqa: B006 - upstream's signature
            smart_quotes_to=None,
            is_html=False,
            exclude_encodings=[],  # noqa: B006 - upstream's signature
            user_encodings=None,
            override_encodings=None,
        ):
            self.smart_quotes_to = smart_quotes_to
            self.tried_encodings = []
            self.contains_replacement_characters = False
            self.is_html = is_html
            self.log = _logging.getLogger(__name__)
            self.detector = EncodingDetector(
                markup,
                known_definite_encodings,
                is_html,
                exclude_encodings,
                user_encodings,
                override_encodings,
            )

            # Short-circuit if the data is in Unicode to begin with.
            if isinstance(markup, str) or markup == "":
                self.markup = markup
                self.unicode_markup = str(markup)
                self.original_encoding = None
                return

            # The encoding detector may have stripped a byte-order mark.
            # Use the stripped markup from this point on.
            self.markup = self.detector.markup

            u = None
            for encoding in self.detector.encodings:
                markup = self.detector.markup
                u = self._convert_from(encoding)
                if u is not None:
                    break

            if not u:
                # None of the encodings worked. As an absolute last resort,
                # try them again with character replacement.
                for encoding in self.detector.encodings:
                    if encoding != "ascii":
                        u = self._convert_from(encoding, "replace")
                    if u is not None:
                        self.log.warning(
                            "Some characters could not be decoded, and were "
                            "replaced with REPLACEMENT CHARACTER."
                        )
                        self.contains_replacement_characters = True
                        break

            self.unicode_markup = u
            if not u:
                self.original_encoding = None

        def _sub_ms_char(self, match):
            """Changes a MS smart quote character to an XML or HTML entity, or ASCII."""
            orig = match.group(1)
            if self.smart_quotes_to == "ascii":
                sub = self.MS_CHARS_TO_ASCII.get(orig).encode()
            else:
                sub = self.MS_CHARS.get(orig)
                if type(sub) is tuple:
                    if self.smart_quotes_to == "xml":
                        sub = "&#x".encode() + sub[1].encode() + ";".encode()
                    else:
                        sub = "&".encode() + sub[0].encode() + ";".encode()
                else:
                    sub = sub.encode()
            return sub

        def _convert_from(self, proposed, errors="strict"):
            """Attempt to convert the markup to the proposed encoding."""
            proposed = self.find_codec(proposed)
            if not proposed or (proposed, errors) in self.tried_encodings:
                return None
            self.tried_encodings.append((proposed, errors))
            markup = self.markup
            # Convert smart quotes to HTML if coming from an encoding
            # that might have them.
            if (
                self.smart_quotes_to is not None
                and proposed in self.ENCODINGS_WITH_SMART_QUOTES
            ):
                smart_quotes_re = b"([\x80-\x9f])"
                smart_quotes_compiled = _re.compile(smart_quotes_re)
                markup = smart_quotes_compiled.sub(self._sub_ms_char, markup)

            try:
                u = self._to_unicode(markup, proposed, errors)
                self.markup = u
                self.original_encoding = proposed
            except Exception:
                return None
            return self.markup

        def _to_unicode(self, data, encoding, errors="strict"):
            """Given a string and its encoding, decodes the string into Unicode."""
            return str(data, encoding, errors)

        @property
        def declared_html_encoding(self):
            """The encoding declared within an HTML document, if any."""
            if not self.is_html:
                return None
            return self.detector.declared_encoding

        def find_codec(self, charset):
            """Convert the name of a character set to a codec name."""
            value = (
                self._codec(self.CHARSET_ALIASES.get(charset, charset))
                or (charset and self._codec(charset.replace("-", "")))
                or (charset and self._codec(charset.replace("-", "_")))
                or (charset and charset.lower())
                or charset
            )
            if value:
                return value.lower()
            return None

        def _codec(self, charset):
            if not charset:
                return charset
            codec = None
            try:
                _codecs.lookup(charset)
                codec = charset
            except (LookupError, ValueError):
                pass
            return codec

        MS_CHARS = {
            b"\x80": ("euro", "20AC"),
            b"\x81": " ",
            b"\x82": ("sbquo", "201A"),
            b"\x83": ("fnof", "192"),
            b"\x84": ("bdquo", "201E"),
            b"\x85": ("hellip", "2026"),
            b"\x86": ("dagger", "2020"),
            b"\x87": ("Dagger", "2021"),
            b"\x88": ("circ", "2C6"),
            b"\x89": ("permil", "2030"),
            b"\x8a": ("Scaron", "160"),
            b"\x8b": ("lsaquo", "2039"),
            b"\x8c": ("OElig", "152"),
            b"\x8d": "?",
            b"\x8e": ("#x17D", "17D"),
            b"\x8f": "?",
            b"\x90": "?",
            b"\x91": ("lsquo", "2018"),
            b"\x92": ("rsquo", "2019"),
            b"\x93": ("ldquo", "201C"),
            b"\x94": ("rdquo", "201D"),
            b"\x95": ("bull", "2022"),
            b"\x96": ("ndash", "2013"),
            b"\x97": ("mdash", "2014"),
            b"\x98": ("tilde", "2DC"),
            b"\x99": ("trade", "2122"),
            b"\x9a": ("scaron", "161"),
            b"\x9b": ("rsaquo", "203A"),
            b"\x9c": ("oelig", "153"),
            b"\x9d": "?",
            b"\x9e": ("#x17E", "17E"),
            b"\x9f": ("Yuml", ""),
        }

        MS_CHARS_TO_ASCII = {
            b"\x80": "EUR",
            b"\x81": " ",
            b"\x82": ",",
            b"\x83": "f",
            b"\x84": ",,",
            b"\x85": "...",
            b"\x86": "+",
            b"\x87": "++",
            b"\x88": "^",
            b"\x89": "%",
            b"\x8a": "S",
            b"\x8b": "<",
            b"\x8c": "OE",
            b"\x8d": "?",
            b"\x8e": "Z",
            b"\x8f": "?",
            b"\x90": "?",
            b"\x91": "'",
            b"\x92": "'",
            b"\x93": '"',
            b"\x94": '"',
            b"\x95": "*",
            b"\x96": "-",
            b"\x97": "--",
            b"\x98": "~",
            b"\x99": "(TM)",
            b"\x9a": "s",
            b"\x9b": ">",
            b"\x9c": "oe",
            b"\x9d": "?",
            b"\x9e": "z",
            b"\x9f": "Y",
            b"\xa0": " ",
            b"\xa1": "!",
            b"\xa2": "c",
            b"\xa3": "GBP",
            b"\xa4": "$",  # This approximation is especially parochial--this is the
            # generic currency symbol.
            b"\xa5": "YEN",
            b"\xa6": "|",
            b"\xa7": "S",
            b"\xa8": "..",
            b"\xa9": "",
            b"\xaa": "(th)",
            b"\xab": "<<",
            b"\xac": "!",
            b"\xad": " ",
            b"\xae": "(R)",
            b"\xaf": "-",
            b"\xb0": "o",
            b"\xb1": "+-",
            b"\xb2": "2",
            b"\xb3": "3",
            b"\xb4": ("'", "acute"),
            b"\xb5": "u",
            b"\xb6": "P",
            b"\xb7": "*",
            b"\xb8": ",",
            b"\xb9": "1",
            b"\xba": "(th)",
            b"\xbb": ">>",
            b"\xbc": "1/4",
            b"\xbd": "1/2",
            b"\xbe": "3/4",
            b"\xbf": "?",
            b"\xc0": "A",
            b"\xc1": "A",
            b"\xc2": "A",
            b"\xc3": "A",
            b"\xc4": "A",
            b"\xc5": "A",
            b"\xc6": "AE",
            b"\xc7": "C",
            b"\xc8": "E",
            b"\xc9": "E",
            b"\xca": "E",
            b"\xcb": "E",
            b"\xcc": "I",
            b"\xcd": "I",
            b"\xce": "I",
            b"\xcf": "I",
            b"\xd0": "D",
            b"\xd1": "N",
            b"\xd2": "O",
            b"\xd3": "O",
            b"\xd4": "O",
            b"\xd5": "O",
            b"\xd6": "O",
            b"\xd7": "*",
            b"\xd8": "O",
            b"\xd9": "U",
            b"\xda": "U",
            b"\xdb": "U",
            b"\xdc": "U",
            b"\xdd": "Y",
            b"\xde": "b",
            b"\xdf": "B",
            b"\xe0": "a",
            b"\xe1": "a",
            b"\xe2": "a",
            b"\xe3": "a",
            b"\xe4": "a",
            b"\xe5": "a",
            b"\xe6": "ae",
            b"\xe7": "c",
            b"\xe8": "e",
            b"\xe9": "e",
            b"\xea": "e",
            b"\xeb": "e",
            b"\xec": "i",
            b"\xed": "i",
            b"\xee": "i",
            b"\xef": "i",
            b"\xf0": "o",
            b"\xf1": "n",
            b"\xf2": "o",
            b"\xf3": "o",
            b"\xf4": "o",
            b"\xf5": "o",
            b"\xf6": "o",
            b"\xf7": "/",
            b"\xf8": "o",
            b"\xf9": "u",
            b"\xfa": "u",
            b"\xfb": "u",
            b"\xfc": "u",
            b"\xfd": "y",
            b"\xfe": "b",
            b"\xff": "y",
        }

        WINDOWS_1252_TO_UTF8 = {
            0x80: b"\xe2\x82\xac",  # €
            0x82: b"\xe2\x80\x9a",  # ‚
            0x83: b"\xc6\x92",  # ƒ
            0x84: b"\xe2\x80\x9e",  # „
            0x85: b"\xe2\x80\xa6",  # …
            0x86: b"\xe2\x80\xa0",  # †
            0x87: b"\xe2\x80\xa1",  # ‡
            0x88: b"\xcb\x86",  # ˆ
            0x89: b"\xe2\x80\xb0",  # ‰
            0x8A: b"\xc5\xa0",  # Š
            0x8B: b"\xe2\x80\xb9",  # ‹
            0x8C: b"\xc5\x92",  # Œ
            0x8E: b"\xc5\xbd",  # Ž
            0x91: b"\xe2\x80\x98",  # ‘
            0x92: b"\xe2\x80\x99",  # ’
            0x93: b"\xe2\x80\x9c",  # “
            0x94: b"\xe2\x80\x9d",  # ”
            0x95: b"\xe2\x80\xa2",  # •
            0x96: b"\xe2\x80\x93",  # –
            0x97: b"\xe2\x80\x94",  # —
            0x98: b"\xcb\x9c",  # ˜
            0x99: b"\xe2\x84\xa2",  # ™
            0x9A: b"\xc5\xa1",  # š
            0x9B: b"\xe2\x80\xba",  # ›
            0x9C: b"\xc5\x93",  # œ
            0x9E: b"\xc5\xbe",  # ž
            0x9F: b"\xc5\xb8",  # Ÿ
            0xA0: b"\xc2\xa0",  # non-breaking space
            0xA1: b"\xc2\xa1",  # ¡
            0xA2: b"\xc2\xa2",  # ¢
            0xA3: b"\xc2\xa3",  # £
            0xA4: b"\xc2\xa4",  # ¤
            0xA5: b"\xc2\xa5",  # ¥
            0xA6: b"\xc2\xa6",  # ¦
            0xA7: b"\xc2\xa7",  # §
            0xA8: b"\xc2\xa8",  # ¨
            0xA9: b"\xc2\xa9",  # ©
            0xAA: b"\xc2\xaa",  # ª
            0xAB: b"\xc2\xab",  # «
            0xAC: b"\xc2\xac",  # ¬
            0xAD: b"\xc2\xad",  # ­
            0xAE: b"\xc2\xae",  # ®
            0xAF: b"\xc2\xaf",  # ¯
            0xB0: b"\xc2\xb0",  # °
            0xB1: b"\xc2\xb1",  # ±
            0xB2: b"\xc2\xb2",  # ²
            0xB3: b"\xc2\xb3",  # ³
            0xB4: b"\xc2\xb4",  # ´
            0xB5: b"\xc2\xb5",  # µ
            0xB6: b"\xc2\xb6",  # ¶
            0xB7: b"\xc2\xb7",  # ·
            0xB8: b"\xc2\xb8",  # ¸
            0xB9: b"\xc2\xb9",  # ¹
            0xBA: b"\xc2\xba",  # º
            0xBB: b"\xc2\xbb",  # »
            0xBC: b"\xc2\xbc",  # ¼
            0xBD: b"\xc2\xbd",  # ½
            0xBE: b"\xc2\xbe",  # ¾
            0xBF: b"\xc2\xbf",  # ¿
            0xC0: b"\xc3\x80",  # À
            0xC1: b"\xc3\x81",  # Á
            0xC2: b"\xc3\x82",  # Â
            0xC3: b"\xc3\x83",  # Ã
            0xC4: b"\xc3\x84",  # Ä
            0xC5: b"\xc3\x85",  # Å
            0xC6: b"\xc3\x86",  # Æ
            0xC7: b"\xc3\x87",  # Ç
            0xC8: b"\xc3\x88",  # È
            0xC9: b"\xc3\x89",  # É
            0xCA: b"\xc3\x8a",  # Ê
            0xCB: b"\xc3\x8b",  # Ë
            0xCC: b"\xc3\x8c",  # Ì
            0xCD: b"\xc3\x8d",  # Í
            0xCE: b"\xc3\x8e",  # Î
            0xCF: b"\xc3\x8f",  # Ï
            0xD0: b"\xc3\x90",  # Ð
            0xD1: b"\xc3\x91",  # Ñ
            0xD2: b"\xc3\x92",  # Ò
            0xD3: b"\xc3\x93",  # Ó
            0xD4: b"\xc3\x94",  # Ô
            0xD5: b"\xc3\x95",  # Õ
            0xD6: b"\xc3\x96",  # Ö
            0xD7: b"\xc3\x97",  # ×
            0xD8: b"\xc3\x98",  # Ø
            0xD9: b"\xc3\x99",  # Ù
            0xDA: b"\xc3\x9a",  # Ú
            0xDB: b"\xc3\x9b",  # Û
            0xDC: b"\xc3\x9c",  # Ü
            0xDD: b"\xc3\x9d",  # Ý
            0xDE: b"\xc3\x9e",  # Þ
            0xDF: b"\xc3\x9f",  # ß
            0xE0: b"\xc3\xa0",  # à
            0xE1: b"\xa1",  # á
            0xE2: b"\xc3\xa2",  # â
            0xE3: b"\xc3\xa3",  # ã
            0xE4: b"\xc3\xa4",  # ä
            0xE5: b"\xc3\xa5",  # å
            0xE6: b"\xc3\xa6",  # æ
            0xE7: b"\xc3\xa7",  # ç
            0xE8: b"\xc3\xa8",  # è
            0xE9: b"\xc3\xa9",  # é
            0xEA: b"\xc3\xaa",  # ê
            0xEB: b"\xc3\xab",  # ë
            0xEC: b"\xc3\xac",  # ì
            0xED: b"\xc3\xad",  # í
            0xEE: b"\xc3\xae",  # î
            0xEF: b"\xc3\xaf",  # ï
            0xF0: b"\xc3\xb0",  # ð
            0xF1: b"\xc3\xb1",  # ñ
            0xF2: b"\xc3\xb2",  # ò
            0xF3: b"\xc3\xb3",  # ó
            0xF4: b"\xc3\xb4",  # ô
            0xF5: b"\xc3\xb5",  # õ
            0xF6: b"\xc3\xb6",  # ö
            0xF7: b"\xc3\xb7",  # ÷
            0xF8: b"\xc3\xb8",  # ø
            0xF9: b"\xc3\xb9",  # ù
            0xFA: b"\xc3\xba",  # ú
            0xFB: b"\xc3\xbb",  # û
            0xFC: b"\xc3\xbc",  # ü
            0xFD: b"\xc3\xbd",  # ý
            0xFE: b"\xc3\xbe",  # þ
        }

        MULTIBYTE_MARKERS_AND_SIZES = [
            (0xC2, 0xDF, 2),  # 2-byte characters start with a byte C2-DF
            (0xE0, 0xEF, 3),  # 3-byte characters start with E0-EF
            (0xF0, 0xF4, 4),  # 4-byte characters start with F0-F4
        ]

        FIRST_MULTIBYTE_MARKER = MULTIBYTE_MARKERS_AND_SIZES[0][0]
        LAST_MULTIBYTE_MARKER = MULTIBYTE_MARKERS_AND_SIZES[-1][1]

        @classmethod
        def detwingle(
            cls, in_bytes, main_encoding="utf8", embedded_encoding="windows-1252"
        ):
            """Fix characters from one encoding embedded in some other encoding.

            Currently the only situation supported is Windows-1252 (or its
            subset ISO-8859-1), embedded in UTF-8.

            :param in_bytes: A bytestring that you suspect contains
                characters from multiple encodings. Note that this _must_
                be a bytestring. If you've already converted the document
                to Unicode, you're too late.
            :param main_encoding: The primary encoding of `in_bytes`.
            :param embedded_encoding: The encoding that was used to embed characters
                in the main document.
            :return: A bytestring in which `embedded_encoding`
              characters have been converted to their `main_encoding`
              equivalents.
            """
            if embedded_encoding.replace("_", "-").lower() not in (
                "windows-1252",
                "windows_1252",
            ):
                raise NotImplementedError(
                    "Windows-1252 and ISO-8859-1 are the only currently supported "
                    "embedded encodings."
                )

            if main_encoding.lower() not in ("utf8", "utf-8"):
                raise NotImplementedError(
                    "UTF-8 is the only currently supported main encoding."
                )

            byte_chunks = []

            chunk_start = 0
            pos = 0
            while pos < len(in_bytes):
                byte = in_bytes[pos]
                if not isinstance(byte, int):
                    # Python 2.x
                    byte = ord(byte)
                if (
                    byte >= cls.FIRST_MULTIBYTE_MARKER
                    and byte <= cls.LAST_MULTIBYTE_MARKER
                ):
                    # This is the start of a UTF-8 multibyte character. Skip
                    # to the end.
                    for start, end, size in cls.MULTIBYTE_MARKERS_AND_SIZES:
                        if byte >= start and byte <= end:
                            pos += size
                            break
                elif byte >= 0x80 and byte in cls.WINDOWS_1252_TO_UTF8:
                    # We found a Windows-1252 character!
                    # Save the string up to this point as a chunk.
                    byte_chunks.append(in_bytes[chunk_start:pos])

                    # Now translate the Windows-1252 character into UTF-8
                    # and add it as another, one-byte chunk.
                    byte_chunks.append(cls.WINDOWS_1252_TO_UTF8[byte])
                    pos += 1
                    chunk_start = pos
                else:
                    # Go on to the next character.
                    pos += 1
            if chunk_start == 0:
                # The string is unchanged.
                return in_bytes
            else:
                # Store the final chunk.
                byte_chunks.append(in_bytes[chunk_start:])
            return b"".join(byte_chunks)

    def _decode_markup(data, from_encoding=None, exclude_encodings=None):
        """The bytes branch of bs4's prepare_markup, run through UnicodeDammit."""
        dammit = UnicodeDammit(
            data,
            known_definite_encodings=[from_encoding],
            user_encodings=[None],
            is_html=True,
            exclude_encodings=exclude_encodings or [],
        )
        return (
            dammit.markup,
            dammit.original_encoding,
            dammit.declared_html_encoding,
            dammit.contains_replacement_characters,
        )

    class Formatter(EntitySubstitution):
        """bs4.formatter.Formatter: the knobs the renderer reads."""

        # Registries of XML and HTML formatters.
        XML_FORMATTERS = {}
        HTML_FORMATTERS = {}

        HTML = "html"
        XML = "xml"

        HTML_DEFAULTS = dict(cdata_containing_tags=set(["script", "style"]))

        def _default(self, language, value, kwarg):
            if value is not None:
                return value
            if language == self.XML:
                return set()
            return self.HTML_DEFAULTS[kwarg]

        def __init__(
            self,
            language=None,
            entity_substitution=None,
            void_element_close_prefix="/",
            cdata_containing_tags=None,
            empty_attributes_are_booleans=False,
            indent=1,
        ):
            self.language = language
            self.entity_substitution = entity_substitution
            self.void_element_close_prefix = void_element_close_prefix
            self.cdata_containing_tags = self._default(
                language, cdata_containing_tags, "cdata_containing_tags"
            )
            self.empty_attributes_are_booleans = empty_attributes_are_booleans
            if indent is None:
                indent = 0
            if isinstance(indent, int):
                if indent < 0:
                    indent = 0
                indent = " " * indent
            elif isinstance(indent, str):
                indent = indent
            else:
                indent = " "
            self.indent = indent

        def substitute(self, ns):
            if not self.entity_substitution:
                return ns
            if (
                isinstance(ns, NavigableString)
                and ns.parent is not None
                and ns.parent.name in self.cdata_containing_tags
            ):
                # The contents of <script>/<style> are CDATA, never escaped.
                return ns
            return self.entity_substitution(ns)

        def attribute_value(self, value):
            return self.substitute(value)

        def attributes(self, tag):
            # bs4 sorts attributes alphabetically, and renders empty values as
            # boolean attributes when the dialect (html5) says so.
            if tag.attrs is None:
                return []
            return sorted(
                (k, (None if self.empty_attributes_are_booleans and v == "" else v))
                for k, v in list(tag.attrs.items())
            )

    class HTMLFormatter(Formatter):
        """A generic Formatter for HTML."""

        REGISTRY = {}

        def __init__(self, *args, **kwargs):
            super(HTMLFormatter, self).__init__(self.HTML, *args, **kwargs)

    class XMLFormatter(Formatter):
        """A generic Formatter for XML."""

        REGISTRY = {}

        def __init__(self, *args, **kwargs):
            super(XMLFormatter, self).__init__(self.XML, *args, **kwargs)

    HTMLFormatter.REGISTRY["html"] = HTMLFormatter(
        entity_substitution=EntitySubstitution.substitute_html
    )
    HTMLFormatter.REGISTRY["html5"] = HTMLFormatter(
        entity_substitution=EntitySubstitution.substitute_html,
        void_element_close_prefix=None,
        empty_attributes_are_booleans=True,
    )
    HTMLFormatter.REGISTRY["minimal"] = HTMLFormatter(
        entity_substitution=EntitySubstitution.substitute_xml
    )
    HTMLFormatter.REGISTRY[None] = HTMLFormatter(entity_substitution=None)
    XMLFormatter.REGISTRY["html"] = XMLFormatter(
        entity_substitution=EntitySubstitution.substitute_html
    )
    XMLFormatter.REGISTRY["minimal"] = XMLFormatter(
        entity_substitution=EntitySubstitution.substitute_xml
    )
    # Upstream really does pass a Formatter as this one's `language`; kept
    # verbatim so bs4.formatter.XMLFormatter.REGISTRY[None] matches.
    XMLFormatter.REGISTRY[None] = Formatter(
        Formatter(Formatter.XML, entity_substitution=None)
    )

    class SoupStrainer:
        """bs4's name/attribute/string filter, shared by find_* and parse_only."""

        def __init__(self, name=None, attrs={}, string=None, **kwargs):  # noqa: B006
            if string is None and "text" in kwargs:
                string = kwargs.pop("text")
            self.name = _normalize_search_value(name)
            if not isinstance(attrs, dict):
                # A non-dict `attrs` is bs4 shorthand for a class filter.
                kwargs["class"] = attrs
                attrs = None
            if "class_" in kwargs:
                kwargs["class"] = kwargs.pop("class_")
            if kwargs:
                if attrs:
                    attrs = dict(attrs)
                    attrs.update(kwargs)
                else:
                    attrs = kwargs
            self.attrs = {
                key: _normalize_search_value(value)
                for key, value in list((attrs or {}).items())
            }
            self.string = _normalize_search_value(string)
            # DEPRECATED upstream, but code in the wild still reads it.
            self.text = self.string

        def __str__(self):
            if self.string:
                return self.string
            return "%s|%s" % (self.name, self.attrs)

        # NOTE: bs4 gives SoupStrainer __str__ but no __repr__, so repr() falls
        # back to the default object representation.  Keep it that way.

        def _normalize_search_value(self, value):
            return _normalize_search_value(value)

        def _matches(self, markup, match_against, already_tried=None):
            return _value_matches(markup, match_against, already_tried)

        def search(self, markup):
            return _strainer_search(self, markup)

        def search_tag(self, markup_name=None, markup_attrs={}):  # noqa: B006
            """bs4's name/attribute-only probe, used before a tag is built."""
            return _strainer_search_tag(self, markup_name, markup_attrs)

        # For BS3 compatibility.
        searchTag = search_tag

    mod = types.ModuleType("bs4")
    mod.__path__ = []
    mod.__version__ = "4.12-vis-pure"
    mod.BeautifulSoup = BeautifulSoup

    class BeautifulStoneSoup(BeautifulSoup):
        """Deprecated interface to an XML parser."""

        def __init__(self, *args, **kwargs):
            kwargs["features"] = "xml"
            _warnings.warn(  # noqa: B028
                "The BeautifulStoneSoup class is deprecated. Instead of using "
                'it, pass features="xml" into the BeautifulSoup constructor.'
            )
            BeautifulSoup.__init__(self, *args, **kwargs)

    mod.BeautifulStoneSoup = BeautifulStoneSoup
    mod.Tag = Tag
    mod.PageElement = PageElement
    mod.NavigableString = NavigableString
    mod.Comment = Comment
    mod.CData = CData
    mod.Doctype = Doctype
    mod.Declaration = Declaration
    mod.ProcessingInstruction = ProcessingInstruction
    mod.Script = Script
    mod.Stylesheet = Stylesheet
    mod.TemplateString = TemplateString
    mod.ResultSet = ResultSet
    mod.SoupStrainer = SoupStrainer
    mod.HTMLParserTreeBuilder = HTMLParserTreeBuilder
    mod.FeatureNotFound = FeatureNotFound
    mod.ParserRejectedMarkup = ParserRejectedMarkup
    mod.StopParsing = StopParsing
    mod.GuessedAtParserWarning = GuessedAtParserWarning
    mod.MarkupResemblesLocatorWarning = MarkupResemblesLocatorWarning
    mod.XMLParsedAsHTMLWarning = XMLParsedAsHTMLWarning
    mod.CSS = CSS
    mod.PYTHON_SPECIFIC_ENCODINGS = PYTHON_SPECIFIC_ENCODINGS
    mod.DEFAULT_OUTPUT_ENCODING = _DEFAULT_OUTPUT_ENCODING
    # Upstream bs4 exports exactly one name via `from bs4 import *`; every other
    # class is a plain module attribute. Match that, or star-imports diverge.
    mod.__all__ = ["BeautifulSoup"]

    elem = types.ModuleType("bs4.element")
    elem.Tag = Tag
    elem.PageElement = PageElement
    elem.NavigableString = NavigableString
    elem.PreformattedString = PreformattedString
    elem.Comment = Comment
    elem.CData = CData
    elem.Doctype = Doctype
    elem.Declaration = Declaration
    elem.ProcessingInstruction = ProcessingInstruction
    elem.XMLProcessingInstruction = XMLProcessingInstruction
    elem.Script = Script
    elem.Stylesheet = Stylesheet
    elem.TemplateString = TemplateString
    elem.RubyTextString = RubyTextString
    elem.RubyParenthesisString = RubyParenthesisString
    elem.ResultSet = ResultSet
    elem.SoupStrainer = SoupStrainer
    elem.PYTHON_SPECIFIC_ENCODINGS = PYTHON_SPECIFIC_ENCODINGS
    elem.AttributeValueWithCharsetSubstitution = AttributeValueWithCharsetSubstitution
    elem.CharsetMetaAttributeValue = CharsetMetaAttributeValue
    elem.ContentMetaAttributeValue = ContentMetaAttributeValue
    elem.Formatter = Formatter
    elem.HTMLFormatter = HTMLFormatter
    elem.XMLFormatter = XMLFormatter
    elem.CSS = CSS
    elem.NamespacedAttribute = NamespacedAttribute
    elem.nonwhitespace_re = nonwhitespace_re
    elem.whitespace_re = whitespace_re
    elem.DEFAULT_OUTPUT_ENCODING = _DEFAULT_OUTPUT_ENCODING
    mod.element = elem

    fmt_mod = types.ModuleType("bs4.formatter")
    fmt_mod.Formatter = Formatter
    fmt_mod.HTMLFormatter = HTMLFormatter
    fmt_mod.XMLFormatter = XMLFormatter
    fmt_mod.EntitySubstitution = EntitySubstitution
    mod.formatter = fmt_mod

    builder_mod = types.ModuleType("bs4.builder")
    builder_mod.TreeBuilder = TreeBuilder
    builder_mod.TreeBuilderRegistry = TreeBuilderRegistry
    builder_mod.HTMLParserTreeBuilder = HTMLParserTreeBuilder
    builder_mod.HTMLTreeBuilder = HTMLTreeBuilder
    builder_mod.ParserRejectedMarkup = ParserRejectedMarkup
    builder_mod.HTML = "html"
    builder_mod.HTML_5 = "html5"
    builder_mod.XML = "xml"
    builder_mod.FAST = "fast"
    builder_mod.STRICT = "strict"
    builder_mod.PERMISSIVE = "permissive"
    builder_mod.builder_registry = TreeBuilderRegistry()
    builder_mod.builder_registry.register(HTMLParserTreeBuilder)
    builder_mod.SAXTreeBuilder = SAXTreeBuilder
    builder_mod.DetectsXMLParsedAsHTML = DetectsXMLParsedAsHTML
    builder_mod.XMLParsedAsHTMLWarning = XMLParsedAsHTMLWarning
    builder_mod.Script = Script
    builder_mod.Stylesheet = Stylesheet
    builder_mod.TemplateString = TemplateString
    builder_mod.RubyParenthesisString = RubyParenthesisString
    builder_mod.RubyTextString = RubyTextString
    builder_mod.CharsetMetaAttributeValue = CharsetMetaAttributeValue
    builder_mod.ContentMetaAttributeValue = ContentMetaAttributeValue
    builder_mod.nonwhitespace_re = nonwhitespace_re
    # bs4.builder.__all__ after _htmlparser registers itself.
    builder_mod.__all__ = [
        "HTMLTreeBuilder",
        "SAXTreeBuilder",
        "TreeBuilder",
        "TreeBuilderRegistry",
        "HTMLParserTreeBuilder",
    ]

    def register_treebuilders_from(module):
        """Copy TreeBuilders from the given module into bs4.builder."""
        for name in module.__all__:
            obj = getattr(module, name)
            if issubclass(obj, TreeBuilder):
                setattr(builder_mod, name, obj)
                builder_mod.__all__.append(name)
                # Register the builder while we're at it.
                builder_mod.builder_registry.register(obj)

    builder_mod.register_treebuilders_from = register_treebuilders_from
    mod.builder_registry = builder_mod.builder_registry
    mod.builder = builder_mod

    diag = types.ModuleType("bs4.diagnose")

    def diagnose(data):
        """Print out information helpful for debugging a parse."""
        print("Diagnostic running on vis bs4 shim " + mod.__version__)
        print("Python version " + sys.version)
        print("Only the pure-Python html.parser tree builder exists in this sandbox;")
        print("lxml and html5lib are not installed and cannot be selected.")
        if hasattr(data, "read"):
            data = data.read()
        print("Trying to parse your markup with html.parser")
        try:
            soup = BeautifulSoup(data, "html.parser")
        except Exception:
            import traceback

            print("html.parser could not parse the markup:")
            traceback.print_exc()
            return
        print("Here's what html.parser did with the markup:")
        print(soup.prettify())

    def htmlparser_trace(data):
        """Print out the html.parser events fired while parsing this markup."""

        class AnnouncingParser(_hp.HTMLParser):
            def _p(self, s):
                print(s)

            def handle_starttag(self, name, attrs):
                self._p("%s START" % name)

            def handle_endtag(self, name):
                self._p("%s END" % name)

            def handle_data(self, data):
                self._p("%s DATA" % data)

            def handle_charref(self, name):
                self._p("%s CHARREF" % name)

            def handle_entityref(self, name):
                self._p("%s ENTITYREF" % name)

            def handle_comment(self, data):
                self._p("%s COMMENT" % data)

            def handle_decl(self, data):
                self._p("%s DECL" % data)

            def unknown_decl(self, data):
                self._p("%s UNKNOWN-DECL" % data)

            def handle_pi(self, data):
                self._p("%s PI" % data)

        parser = AnnouncingParser(convert_charrefs=False)
        parser.feed(data)
        parser.close()

    def lxml_trace(data, html=True, **kwargs):
        """lxml is not installed here; say so instead of failing obscurely."""
        print("lxml is not available in the vis sandbox; use htmlparser_trace().")

    def benchmark_parsers(num_elements=100000):
        """Very basic head-to-head performance benchmark (one parser here)."""
        import time as _time

        markup = "<a>" + ("<b>x</b>" * num_elements) + "</a>"
        start = _time.time()
        soup = BeautifulSoup(markup, "html.parser")
        print(
            "BS4+html.parser parsed %d elements in %.2fs"
            % (len(soup.find_all(True)), _time.time() - start)
        )

    diag.diagnose = diagnose
    diag.htmlparser_trace = htmlparser_trace
    diag.lxml_trace = lxml_trace
    diag.benchmark_parsers = benchmark_parsers
    diag.BeautifulSoup = BeautifulSoup
    diag.builder_registry = None
    mod.diagnose = diag

    dammit_mod = types.ModuleType("bs4.dammit")
    dammit_mod.UnicodeDammit = UnicodeDammit
    dammit_mod.EncodingDetector = EncodingDetector
    dammit_mod.EntitySubstitution = EntitySubstitution
    mod.dammit = dammit_mod
    mod.UnicodeDammit = UnicodeDammit

    css_mod = types.ModuleType("bs4.css")
    css_mod.CSS = CSS
    mod.css = css_mod

    hp_mod = types.ModuleType("bs4.builder._htmlparser")
    hp_mod.HTMLParserTreeBuilder = HTMLParserTreeBuilder
    hp_mod.BeautifulSoupHTMLParser = _Builder
    builder_mod._htmlparser = hp_mod

    # Every class above is a local of this installer, so left alone its
    # __module__/__qualname__ would read "__vis_install_bs4__.<locals>.Tag":
    # reprs would be wrong and pickling any element would fail outright. Stamp
    # each class with the module that publishes it upstream. The first stamp
    # wins, so aliases (BeautifulStoneSoup, HTMLTreeBuilder) keep the real name.
    _stamped_classes = []
    for _mod_name, _mod_obj in (
        ("bs4.dammit", dammit_mod),
        ("bs4.formatter", fmt_mod),
        ("bs4.builder._htmlparser", hp_mod),
        ("bs4.css", css_mod),
        ("bs4.element", elem),
        ("bs4.builder", builder_mod),
        ("bs4", mod),
    ):
        for _name, _obj in list(vars(_mod_obj).items()):
            if not callable(_obj) or "<locals>" not in getattr(
                _obj, "__qualname__", ""
            ):
                continue
            _obj.__module__ = _mod_name
            _obj.__qualname__ = _name
            if isinstance(_obj, type):
                _stamped_classes.append((_mod_name, _name, _obj))

    def _stamp_callable(fn, mod_name, owner, name):
        if "<locals>" in getattr(fn, "__qualname__", ""):
            fn.__module__ = mod_name
            fn.__qualname__ = owner + "." + name

    # Method qualnames surface in TypeError messages, so they get stamped too.
    # Least derived class first: the tree methods this shim implements on Tag and
    # republishes onto PageElement are "PageElement.append" upstream, and only a
    # method no base class carries keeps its own class's name.
    for _mod_name, _cls_name, _cls in sorted(
        _stamped_classes, key=lambda item: len(item[2].__mro__)
    ):
        for _name, _obj in list(vars(_cls).items()):
            if isinstance(_obj, property):
                for _fn in (_obj.fget, _obj.fset, _obj.fdel):
                    if _fn is not None:
                        _stamp_callable(_fn, _mod_name, _cls_name, _name)
            elif isinstance(_obj, (staticmethod, classmethod)):
                _stamp_callable(_obj.__func__, _mod_name, _cls_name, _name)
            elif callable(_obj) and not isinstance(_obj, type):
                _stamp_callable(_obj, _mod_name, _cls_name, _name)

    # soupsieve is not installed, but its object is what css.compile() returns.
    SoupSieve.__module__ = "soupsieve.css_match"
    SoupSieve.__qualname__ = "SoupSieve"

    # Upstream's modules leak the standard-library names they import, and real
    # code reaches for some of them -- monkeypatching bs4.dammit.chardet_dammit
    # is the classic. Publish exactly the ones 4.12.3 exposes, after the
    # stamping pass above so that a re-export cannot claim a class's home.
    import codecs as _codecs
    import collections.abc as _abc
    import itertools as _itertools
    import logging as _logging
    import os as _os
    import string as _string
    import traceback as _traceback

    mod.Counter = _collections.Counter
    mod.os = _os
    mod.re = _re
    mod.sys = sys
    mod.traceback = _traceback
    mod.warnings = _warnings
    elem.Callable = _abc.Callable
    elem.re = _re
    elem.sys = sys
    elem.warnings = _warnings
    builder_mod.defaultdict = _collections.defaultdict
    builder_mod.itertools = _itertools
    builder_mod.re = _re
    builder_mod.sys = sys
    builder_mod.warnings = _warnings
    css_mod.warnings = _warnings
    # Upstream this is the soupsieve module; there is none to point at here.
    css_mod.soupsieve = None
    dammit_mod.codecs = _codecs
    dammit_mod.codepoint2name = _hent.codepoint2name
    dammit_mod.defaultdict = _collections.defaultdict
    dammit_mod.html5 = _hent.html5
    dammit_mod.logging = _logging
    dammit_mod.re = _re
    dammit_mod.string = _string
    # No chardet and no charset-normalizer in the sandbox, so this is exactly
    # upstream's "nothing installed" branch.
    dammit_mod.chardet_module = None

    def chardet_dammit(s):
        return None

    chardet_dammit.__module__ = "bs4.dammit"
    chardet_dammit.__qualname__ = "chardet_dammit"
    dammit_mod.chardet_dammit = chardet_dammit
    dammit_mod.xml_encoding = xml_encoding
    dammit_mod.html_meta = html_meta
    dammit_mod.encoding_res = encoding_res
    hp_mod.sys = sys
    hp_mod.warnings = _warnings
    hp_mod.HTMLParser = _hp.HTMLParser
    hp_mod.HTMLPARSER = "html.parser"
    for _name in (
        "CData",
        "Comment",
        "Declaration",
        "Doctype",
        "ProcessingInstruction",
    ):
        setattr(hp_mod, _name, getattr(elem, _name))
    for _name in (
        "DetectsXMLParsedAsHTML",
        "HTML",
        "HTMLTreeBuilder",
        "ParserRejectedMarkup",
        "STRICT",
    ):
        setattr(hp_mod, _name, getattr(builder_mod, _name))
    for _name in ("EntitySubstitution", "UnicodeDammit"):
        setattr(hp_mod, _name, getattr(dammit_mod, _name))

    sys.modules["bs4"] = mod
    sys.modules["bs4.css"] = css_mod
    sys.modules["bs4.builder._htmlparser"] = hp_mod
    sys.modules["bs4.element"] = elem
    sys.modules["bs4.formatter"] = fmt_mod
    sys.modules["bs4.builder"] = builder_mod
    sys.modules["bs4.diagnose"] = diag
    sys.modules["bs4.dammit"] = dammit_mod

    try:
        import builtins as _b

        _b.bs4 = mod
        _b.BeautifulSoup = BeautifulSoup
    except Exception:
        pass


__vis_install_bs4__()
del __vis_install_bs4__
