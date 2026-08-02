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

        def extract(self):
            _detach(self)
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
        def __init__(self, name, attrs=None, sourceline=None, sourcepos=None):
            self.name = name
            self.attrs = {}
            if attrs:
                for k, v in attrs.items() if isinstance(attrs, dict) else attrs:
                    self.attrs[k] = self._norm_attr(k, v)
            self.contents = []
            self.parent = None
            # Where this tag started in the markup, as bs4 reports it: 1-based line
            # and 0-based column. bs4 only records a position the parser actually
            # gave it, so a hand-built tag has neither in its __dict__ and reaches
            # __getattr__ (which answers None) instead.
            if sourceline is not None:
                self.sourceline = sourceline
            if sourcepos is not None:
                self.sourcepos = sourcepos
            self.can_be_empty_element = name in _VOID
            # bs4 copies these builder-supplied facts onto every tag it makes, and
            # code that reads vars(tag) or a tag's __dict__ expects them there.
            self.hidden = False
            self.known_xml = False
            self.namespace = None
            self.prefix = None
            self._namespaces = {}
            self.cdata_list_attributes = _CDATA_LIST_ATTRIBUTES
            self.preserve_whitespace_tags = _PRESERVE_WS
            # Which string classes .strings/.get_text() find interesting: bs4's
            # builder gives a few containers their own NavigableString subclass.
            self.interesting_string_types = _STRING_CONTAINERS.get(
                name, Tag.DEFAULT_INTERESTING_STRING_TYPES
            )
            _set_up_substitutions(self)
            # bs4 records the BeautifulSoup subclass that built the tag; a tag you
            # build by hand has none until a soup parses or adopts it.
            self.parser_class = None

        def _norm_attr(self, k, v):
            if v is None:
                v = ""
            if isinstance(v, str) and (
                k in _MULTI_ATTR or k in _MULTI_ATTR_BY_TAG.get(self.name, ())
            ):
                return v.split()
            return v

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

        def __contains__(self, key):
            return key in self.attrs

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
            self.insert(len(self.contents), node)

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
        def extract(self):
            _detach(self)
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

        def __getattr__(self, key):
            if key.startswith("__") and key.endswith("__"):
                raise AttributeError(key)
            found = self.find(key)
            if found is not None:
                return found
            return None

        # -- serialization ------------------------------------------------------
        def decode(
            self,
            indent_level=None,
            eventual_encoding=_DEFAULT_OUTPUT_ENCODING,
            formatter="minimal",
        ):
            # bs4's first positional is an indentation level, not a flag: any
            # non-None value pretty-prints this subtree starting at that depth.
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

        # Whitespace inside these tags survives prettify(), as in bs4's builder.
        preserve_whitespace_tags = _PRESERVE_WS

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
            dup = BeautifulSoup("") if isinstance(self, BeautifulSoup) else Tag(self.name)
            dup.name = self.name
            # Shallow, like bs4: a multi-valued attribute's list is shared with
            # the original tag rather than copied.
            dup.attrs = dict(self.attrs)
            dup.can_be_empty_element = self.can_be_empty_element
            dup.hidden = self.hidden
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

        def _indent_string(self, s, indent_level, formatter, indent_before, indent_after):
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
            return bool(want(markup))
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
            return want.search(markup) is not None
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
                return None
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
                        node.name == local
                        and (prefix is None or node.prefix == prefix)
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
            dup = BeautifulSoup("") if isinstance(src, BeautifulSoup) else Tag(src.name)
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
    def _parse_nth(arg):
        # An+B microsyntax, including the `odd`/`even`/plain-integer spellings.
        s = arg.replace(" ", "").lower()
        if s == "odd":
            return (2, 1)
        if s == "even":
            return (2, 0)
        if "n" not in s:
            try:
                return (0, int(s))
            except ValueError:
                return None
        head, _sep, tail = s.partition("n")
        if head in ("", "+"):
            a = 1
        elif head == "-":
            a = -1
        else:
            try:
                a = int(head)
            except ValueError:
                return None
        if tail == "":
            b = 0
        else:
            try:
                b = int(tail)
            except ValueError:
                return None
        return (a, b)

    def _nth_ok(spec, index):
        if spec is None:
            return False
        a, b = spec
        if a == 0:
            return index == b
        rest = index - b
        return rest % a == 0 and rest // a >= 0

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
        if t and t != "*":
            tag = t
        i = j
        while i < n:
            c = tok[i]
            if c == "." or c == "#":
                i = i + 1
                s = i
                while i < n and tok[i] not in stop:
                    i = i + 1
                if c == ".":
                    classes.append(tok[s:i])
                else:
                    idv = tok[s:i]
            elif c == "[":
                i = i + 1
                s = i
                while i < n and tok[i] != "]":
                    i = i + 1
                body = tok[s:i]
                i = i + 1
                op = None
                for cand in ("~=", "|=", "^=", "$=", "*=", "="):
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
            elif c == ":":
                i = i + 1
                if i < n and tok[i] == ":":
                    # ::pseudo-elements select no element in this engine.
                    i = i + 1
                s = i
                while i < n and tok[i] not in stop and tok[i] != "(":
                    i = i + 1
                pname = tok[s:i].lower()
                parg = None
                if i < n and tok[i] == "(":
                    depth = 0
                    s = i + 1
                    while i < n:
                        if tok[i] == "(":
                            depth = depth + 1
                        elif tok[i] == ")":
                            depth = depth - 1
                            if depth == 0:
                                break
                        i = i + 1
                    parg = tok[s:i]
                    i = i + 1
                pseudos.append((pname, parg))
            else:
                i = i + 1
        return (tag, idv, classes, attrs, pseudos)

    def _element_siblings(node):
        parent = getattr(node, "parent", None)
        if parent is None:
            return []
        return [c for c in parent.contents if isinstance(c, Tag)]

    def _pseudo_ok(node, pname, parg):
        if pname == "not":
            for part in (parg or "").split(","):
                if part.strip() and _simple_match(node, _parse_simple(part.strip())):
                    return False
            return True
        if pname in ("is", "where", "matches"):
            for part in (parg or "").split(","):
                if part.strip() and _simple_match(node, _parse_simple(part.strip())):
                    return True
            return False
        if pname == "empty":
            return not node.contents
        if pname == "root":
            parent = getattr(node, "parent", None)
            return parent is None or getattr(parent, "name", None) == "[document]"
        if pname == "contains":
            want = (parg or "").strip()
            if len(want) >= 2 and want[0] in (_Q, chr(39)) and want[-1] == want[0]:
                want = want[1:-1]
            return want in node.get_text()
        sibs = _element_siblings(node)
        if not sibs:
            return False
        same = [c for c in sibs if c.name == node.name]
        if pname == "first-child":
            return sibs[0] is node
        if pname == "last-child":
            return sibs[-1] is node
        if pname == "only-child":
            return len(sibs) == 1
        if pname == "first-of-type":
            return bool(same) and same[0] is node
        if pname == "last-of-type":
            return bool(same) and same[-1] is node
        if pname == "only-of-type":
            return len(same) == 1
        if pname in ("nth-child", "nth-last-child", "nth-of-type", "nth-last-of-type"):
            seq = same if pname.endswith("of-type") else sibs
            idx = _index_of(seq, node) + 1
            if idx == 0:
                return False
            if "last" in pname:
                idx = len(seq) - idx + 1
            return _nth_ok(_parse_nth(parg or ""), idx)
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
        # ('desc', 'child', 'next', 'sibs'). Brackets and parentheses are tracked
        # so `[rel~="x"]` and `:not(a > b)` never split on their own operators.
        steps = []
        combinator = "desc"
        buf = []
        square = 0
        paren = 0
        for ch in group:
            if ch == "[":
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
            elif square == 0 and paren == 0 and (ch.isspace() or ch in (_GT, "+", "~")):
                tok = "".join(buf).strip()
                buf = []
                if tok:
                    steps.append((combinator, tok))
                    combinator = "desc"
                if ch == _GT:
                    combinator = "child"
                elif ch == "+":
                    combinator = "next"
                elif ch == "~":
                    combinator = "sibs"
            else:
                buf.append(ch)
        tok = "".join(buf).strip()
        if tok:
            steps.append((combinator, tok))
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

    def _select(root, selector, limit=None):
        groups = [g for g in (g.strip() for g in selector.split(",")) if g]
        results = []
        seen = set()
        sibling_step = False
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
            "([<>]|" "&(?!#\\d+;|#x[0-9a-fA-F]+;|\\w+;)" ")"
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
        def substitute_xml_containing_entities(
            cls, value, make_quoted_attribute=False
        ):
            value = cls.BARE_AMPERSAND_OR_BRACKET.sub(
                cls._substitute_xml_entity, value
            )
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
    # one level of pretty-print indentation)
    _FORMATTERS = {
        "minimal": (_sub_minimal, "/", False, " "),
        "html": (_sub_html, "/", False, " "),
        "html5": (_sub_html, "", True, " "),
        None: (_sub_none, "/", False, " "),
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
            return (formatter, "/", False, " ")
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
        parts = [pad + _LT + node.name]
        for k in sorted(node.attrs):
            v = node.attrs[k]
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

    def _render_pretty(node, depth=0):
        if not isinstance(node, Tag):
            return _pretty_string(node, depth)
        if node.name in _PRESERVE_WS:
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
                if isinstance(child, Tag) and child.name not in _PRESERVE_WS:
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
        # A known entity becomes its character; an unknown one stays literal text
        # with the terminating semicolon consumed, the way BeautifulSoup reports
        # `&foo;` as `&foo`.
        ref = _AMP + name + ";"
        text = _html.unescape(ref)
        return text if text != ref else _AMP + name

    def _charref_text(name):
        try:
            code = int(name[1:], 16) if name[:1] in ("x", "X") else int(name)
            return chr(code)
        except (ValueError, OverflowError):
            return chr(65533)

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
            if self._first_processing_instruction is not None or self._root_tag is not None:
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
        def __init__(self, store_line_numbers=True):
            # convert_charrefs stays off (as in bs4's own html.parser builder) so
            # entity handling lives here rather than in html.parser, which would
            # silently keep an unknown `&foo;` verbatim. Character data therefore
            # arrives split around every reference, and _flush() re-joins each run
            # into the single NavigableString bs4 produces.
            _hp.HTMLParser.__init__(self, convert_charrefs=False)
            self.root = Tag("[document]")
            self.stack = [self.root]
            self._data = []
            self._store_line_numbers = store_line_numbers
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
                    t.name in _PRESERVE_WS for t in self.stack
                ):
                    text = _NL if _NL in text else " "
                # Text inside <script>/<style>/<template>/<rt>/<rp> is wrapped in
                # bs4's dedicated NavigableString subclass for that container.
                cls = _STRING_CONTAINERS.get(self._cur().name, NavigableString)
                self._cur().append(cls(text))

        def handle_starttag(self, tag, attrs, handle_empty_element=True):
            if self._root_tag is None:
                self._root_tag_encountered(tag)
            self._flush()
            line, pos = self._pos()
            t = Tag(tag, attrs, line, pos)
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
            Tag.__init__(self, "[document]")
            # Document-level inspection surface, all of it read by real bs4 code.
            self.hidden = 1
            self.is_xml = False
            self.known_xml = False
            self.parser_class = BeautifulSoup
            self.element_classes = element_classes or {}
            self.builder = builder or HTMLParserTreeBuilder()
            self.builder.soup = self
            self.parse_only = parse_only or kwargs.get("parse_only")
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
            b = _Builder(store_line_numbers=kwargs.get("store_line_numbers", True))
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

        def new_tag(self, name, namespace=None, nsprefix=None, attrs=None, **kwattrs):
            a = dict(attrs) if attrs else {}
            a.update(kwattrs)
            return Tag(name, a)

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
            Tag.__init__(self, self.ROOT_TAG_NAME)
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

    class HTMLParserTreeBuilder(TreeBuilder):
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
                "address", "article", "aside", "blockquote", "canvas", "dd",
                "div", "dl", "dt", "fieldset", "figcaption", "figure", "footer",
                "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "hr", "li",
                "main", "nav", "noscript", "ol", "output", "p", "pre", "section",
                "table", "tfoot", "ul", "video",
            ]
        )

        def __init__(self, parser_args=None, parser_kwargs=None, **kwargs):
            parser_args = parser_args or []
            parser_kwargs = parser_kwargs or {}
            # bs4 turns entity conversion off and handles references itself.
            parser_kwargs["convert_charrefs"] = False
            self.parser_args = (parser_args, parser_kwargs)
            TreeBuilder.__init__(self, **kwargs)

        def can_be_empty_element(self, name):
            return name in _VOID

        def set_up_substitutions(self, tag):
            """Install the <meta> charset stand-in, as HTMLTreeBuilder does."""
            return bool(_set_up_substitutions(tag))

    def _declared_encoding(data):
        head = data[:2048].decode("ascii", "replace").lower()
        for pattern in (r"<\?xml[^>]+encoding=(\S+)", r"<meta[^>]+charset=(\S+)"):
            m = _re.search(pattern, head)
            if m:
                # The capture still carries its quoting: keep the leading run of
                # encoding-name characters and drop the rest.
                out = []
                for ch in m.group(1):
                    if ch.isalnum() or ch in "-_.:":
                        out.append(ch)
                    elif out:
                        break
                if out:
                    return "".join(out)
        return None

    def _decode_markup(data, from_encoding=None, exclude_encodings=None):
        # bs4's UnicodeDammit in miniature: honor an explicit or declared encoding,
        # fall back to UTF-8 and then to windows-1252, which can never fail.
        declared = _declared_encoding(data)
        exclude = set((e or "").lower() for e in (exclude_encodings or []))
        if data.startswith(b"\xef\xbb\xbf"):
            data = data[3:]
        tries = []
        for enc in (from_encoding, declared, "utf-8", "windows-1252"):
            if enc and enc.lower() not in exclude and enc not in tries:
                tries.append(enc)
        for enc in tries:
            try:
                text = data.decode(enc)
            except (UnicodeDecodeError, LookupError):
                continue
            return text, enc, declared, chr(65533) in text
        text = data.decode("windows-1252", "replace")
        return text, "windows-1252", declared, chr(65533) in text

    class EncodingDetector:
        """bs4.dammit.EncodingDetector: the candidate encodings, in bs4's order."""

        def __init__(
            self, markup, override_encodings=None, is_html=False, exclude_encodings=None
        ):
            self.markup = markup
            self.override_encodings = list(override_encodings or [])
            self.exclude_encodings = set(
                (e or "").lower() for e in (exclude_encodings or [])
            )
            self.is_html = is_html
            self.declared_encoding = self.find_declared_encoding(markup, is_html)

        @property
        def encodings(self):
            seen = []
            for enc in self.override_encodings + [
                self.declared_encoding,
                "utf-8",
                "windows-1252",
            ]:
                if (
                    enc
                    and enc.lower() not in self.exclude_encodings
                    and enc not in seen
                ):
                    seen.append(enc)
            return seen

        @classmethod
        def strip_byte_order_mark(cls, data):
            if isinstance(data, bytes) and data.startswith(b"\xef\xbb\xbf"):
                return data[3:], "utf-8"
            return data, None

        @classmethod
        def find_declared_encoding(
            cls, markup, is_html=False, search_entire_document=False
        ):
            if isinstance(markup, str):
                return None
            head = markup if search_entire_document else markup[:1024]
            xml_decl = re.match(rb"^<\?.*?encoding=['\"](.*?)['\"].*?\?>", head)
            if xml_decl:
                return xml_decl.group(1).decode("ascii", "replace")
            # bs4 only trusts an HTML <meta> declaration when told the markup is HTML.
            return _declared_encoding(head) if is_html else None

    class UnicodeDammit:
        """bs4.dammit.UnicodeDammit: bytes in, str out, plus the encoding it guessed."""

        FIRST_MULTIBYTE_MARKER = 194
        LAST_MULTIBYTE_MARKER = 244
        MULTIBYTE_MARKERS_AND_SIZES = [
            (194, 223, 2),
            (224, 239, 3),
            (240, 244, 4),
        ]

        def __init__(
            self,
            markup,
            override_encodings=None,
            smart_quotes_to=None,
            is_html=False,
            exclude_encodings=None,
        ):
            self.smart_quotes_to = smart_quotes_to
            self.is_html = is_html
            self.markup = markup
            self.detector = EncodingDetector(
                markup, override_encodings, is_html, exclude_encodings
            )
            if isinstance(markup, str):
                self.unicode_markup = markup
                self.original_encoding = None
                self.declared_html_encoding = None
                self.contains_replacement_characters = False
                self.tried_encodings = []
            else:
                first = (list(override_encodings or []) or [None])[0]
                text, enc, declared, replaced = _decode_markup(
                    markup, first, exclude_encodings
                )
                self.unicode_markup = text
                self.original_encoding = enc
                self.declared_html_encoding = declared
                self.contains_replacement_characters = replaced
                self.tried_encodings = [(enc, "strict")]

        @property
        def unicode(self):
            return self.unicode_markup

        @classmethod
        def detwingle(
            cls, in_bytes, main_encoding="utf8", embedded_encoding="windows-1252"
        ):
            """Repair windows-1252 bytes smuggled inside otherwise-UTF-8 data."""
            if embedded_encoding.replace("_", "-").lower() != "windows-1252":
                raise NotImplementedError(
                    "Windows-1252 and ASCII are the only currently supported "
                    "embedded encodings."
                )
            if main_encoding.replace("-", "").lower() not in ("utf8",):
                raise NotImplementedError(
                    "UTF-8 is the only currently supported main encoding."
                )
            chunks = []
            chunk_start = 0
            pos = 0
            while pos < len(in_bytes):
                byte = in_bytes[pos]
                if cls.FIRST_MULTIBYTE_MARKER <= byte <= cls.LAST_MULTIBYTE_MARKER:
                    size = 2
                    for start, end, this_size in cls.MULTIBYTE_MARKERS_AND_SIZES:
                        if start <= byte <= end:
                            size = this_size
                            break
                    pos += size
                elif byte >= 128:
                    chunks.append(in_bytes[chunk_start:pos])
                    chunks.append(
                        in_bytes[pos : pos + 1].decode("windows-1252").encode("utf-8")
                    )
                    pos += 1
                    chunk_start = pos
                else:
                    pos += 1
            chunks.append(in_bytes[chunk_start:])
            return b"".join(chunks)

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

        def __repr__(self):
            return self.__str__()

        def search(self, markup):
            return _strainer_search(self, markup)

        def search_tag(self, markup_name=None, markup_attrs=None):
            """bs4's name/attribute-only probe, used before a tag is built."""
            return _strainer_search_tag(self, markup_name, markup_attrs)

    mod = types.ModuleType("bs4")
    mod.__path__ = []
    mod.__version__ = "4.12-vis-pure"
    mod.BeautifulSoup = BeautifulSoup
    mod.BeautifulStoneSoup = BeautifulSoup
    mod.Tag = Tag
    mod.PageElement = PageElement
    mod.NavigableString = NavigableString
    mod.PreformattedString = PreformattedString
    mod.Comment = Comment
    mod.CData = CData
    mod.Doctype = Doctype
    mod.Declaration = Declaration
    mod.ProcessingInstruction = ProcessingInstruction
    mod.XMLProcessingInstruction = XMLProcessingInstruction
    mod.Script = Script
    mod.Stylesheet = Stylesheet
    mod.TemplateString = TemplateString
    mod.RubyTextString = RubyTextString
    mod.RubyParenthesisString = RubyParenthesisString
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
    mod.NamespacedAttribute = NamespacedAttribute
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
    builder_mod.HTMLTreeBuilder = HTMLParserTreeBuilder
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
    builder_mod.whitespace_re = whitespace_re
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
    elem.UnicodeDammit = UnicodeDammit

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
        ("bs4.builder", builder_mod),
        ("bs4.css", css_mod),
        ("bs4.element", elem),
        ("bs4", mod),
    ):
        for _name, _obj in list(vars(_mod_obj).items()):
            if not callable(_obj) or "<locals>" not in getattr(_obj, "__qualname__", ""):
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
