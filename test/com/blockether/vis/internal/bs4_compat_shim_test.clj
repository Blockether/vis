(ns com.blockether.vis.internal.bs4-compat-shim-test
  "The bs4 (BeautifulSoup)-compat shim installed into every sandbox context via
   the generic sandbox-shim mechanism (`extension/sandbox-shims`): a `bs4` module
   published into `sys.modules` (so `from bs4 import BeautifulSoup` works) and
   implemented in PURE Python on the stdlib `html.parser` — a Tag / NavigableString
   tree with find/find_all, CSS .select, get_text and HTML serialization. No host
   bridge."
  (:require [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

;; A namespace-local context avoids paying GraalPy + shim bootstrap per assertion.
(defonce ^:private python-context* (delay (ep/create-python-context {})))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context @python-context*)]
     ~@body))

;; A shared HTML document (single-quoted inside Python so the Clojure string needs
;; no double-quote escaping in the markup itself).
(def ^:private doc
  (str
    "html = ("
    "'<html><head><title>Hi</title></head>'"
    "'<body><div id=' + chr(39) + 'main' + chr(39) + ' class=' + chr(39) + 'box wide' + chr(39) + '>'"
    "'<p class=' + chr(39) + 'lead' + chr(39) + '>First</p>'"
    "'<p>Second <a href=' + chr(39) + '/x' + chr(39) + '>link</a></p>'"
    "'<ul><li>a</li><li>b</li></ul>'" "'</div><!-- note --></body></html>')\n"
    "from bs4 import BeautifulSoup\n" "soup = BeautifulSoup(html, 'html.parser')\n"))

;; Timing helper for the performance guards: best-of-N damps JIT warm-up so the
;; assertions below measure the algorithm rather than the first-call cost.
(def ^:private perf-prelude
  (str "import time\n" "from bs4 import BeautifulSoup\n"
       "def _best(fn, rounds=3):\n" "    best = None\n"
       "    for _ in range(rounds):\n" "        start = time.perf_counter()\n"
       "        fn()\n" "        dt = time.perf_counter() - start\n"
       "        best = dt if best is None else min(best, dt)\n" "    return best\n"))

(defdescribe
  bs4-module-test
  (it "publishes bs4 + bs4.element under sys.modules"
      (with-python-context (expect (true?
                                     (ev python-context
                                         (str "import bs4\nimport sys\n"
                                              "sys.modules.get('bs4') is not None "
                                              "and sys.modules.get('bs4.element') is not None"))))))
  (it "autoloads BeautifulSoup onto builtins (no import needed)"
      (with-python-context (expect (true? (ev python-context "BeautifulSoup is not None")))))
  (it "supports `from bs4 import BeautifulSoup`"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from bs4 import BeautifulSoup\n"
                                "BeautifulSoup('<b>x</b>', 'html.parser').get_text() == 'x'")))))))

(defdescribe
  bs4-find-test
  (it "find / find_all by tag name"
      (with-python-context (expect (true? (ev python-context
                                              (str doc
                                                   "soup.find('p').get_text() == 'First' "
                                                   "and len(soup.find_all('p')) == 2 "
                                                   "and soup.title.get_text() == 'Hi'"))))))
  (it "find by class_ and id"
      (with-python-context (expect (true?
                                     (ev python-context
                                         (str doc
                                              "soup.find('p', class_='lead').get_text() == 'First' "
                                              "and soup.find(id='main').name == 'div'"))))))
  (it "attribute access + multi-valued class"
      (with-python-context (expect (true? (ev python-context
                                              (str
                                                doc
                                                "soup.find('a')['href'] == '/x' "
                                                "and soup.find('div')['class'] == ['box','wide'] "
                                                "and soup.find('div').get('missing') is None"))))))
  (it "get_text with separator + strip, and stripped_strings skips comments"
      (with-python-context
        (expect (true? (ev python-context
                           (str doc
                                "soup.find('ul').get_text('|', strip=True) == 'a|b' "
                                "and 'note' not in list(soup.find('body').stripped_strings)")))))))

(defdescribe bs4-select-test
             (it "CSS select by tag / class / id"
                 (with-python-context
                   (expect (true? (ev python-context
                                      (str doc
                                           "len(soup.select('li')) == 2 "
                                           "and soup.select_one('.lead').get_text() == 'First' "
                                           "and soup.select_one('#main').name == 'div'"))))))
             (it "descendant and child combinators"
                 (with-python-context
                   (expect (true? (ev python-context
                                      (str
                                        doc
                                        "len(soup.select('div p')) == 2 "
                                        "and len(soup.select('div > p')) == 2 "
                                        "and soup.select_one('p.lead').get_text() == 'First'"))))))
             (it "attribute selectors"
                 (with-python-context
                   (expect (true? (ev python-context
                                      (str doc
                                           "soup.select_one('a[href=/x]').get_text() == 'link' "
                                           "and len(soup.select('[class]')) >= 1")))))))

(defdescribe
  bs4-navigation-test
  (it "sibling + parent navigation and .string"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             doc
                             "soup.find('p').find_next_sibling('p').find('a').get_text() == 'link' "
                             "and soup.find('a').parent.name == 'p' "
                             "and soup.title.string == 'Hi'"))))))
  (it "dynamic tag access (soup.a) returns the first match"
      (with-python-context (expect (true? (ev python-context
                                              (str doc "soup.a.get_text() == 'link'"))))))
  (it "HTML serialization round-trips the tags"
      (with-python-context
        (expect (true? (ev python-context
                           (str doc
                                "s = str(soup)\n"
                                "'<title>' in s and '<a href=' in s and '<li>' in s")))))))

(defdescribe
  bs4-package-submodule-test
  (it
    "exports familiar filter and node types"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from bs4 import BeautifulSoup, SoupStrainer, CData, Doctype, FeatureNotFound\n"
              "soup = BeautifulSoup('<p>x</p>', 'html.parser')\n"
              "isinstance(CData('x'), str) and isinstance(Doctype('html'), str) and SoupStrainer('p').search(soup.p) is soup.p")))))))

(defdescribe
  bs4-filter-test
  (it "filters by regex, list, callable and True"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import re\n" doc
                                "len(soup.find_all(re.compile('^(p|li)$'))) == 4 "
                                "and len(soup.find_all(['p','li'])) == 4 "
                                "and len(soup.find_all(lambda t: t.name == 'li')) == 2 "
                                "and soup.find(True).name == 'html'"))))))
  (it "honours attrs, recursive=False and limit"
      (with-python-context
        (expect (true? (ev python-context
                           (str doc
                                "len(soup.find_all(attrs={'class': 'lead'})) == 1 "
                                "and len(soup.find_all('p', recursive=False)) == 0 "
                                "and len(soup.find('div').find_all('p', recursive=False)) == 2 "
                                "and len(soup.find_all('p', limit=1)) == 1"))))))
  (it "matches strings by regex and exposes find_parent plus legacy aliases"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import re\n" doc
                                "len(soup.find_all(string=re.compile('First'))) == 1 "
                                "and soup.find('a').find_parent('div')['id'] == 'main' "
                                "and soup.find('a').find_parent(id='main').name == 'div' "
                                "and len(soup.findAll('li')) == 2"))))))
  (it "returns a list for every attribute lookup shape"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str doc
                           "soup.find('div').get_attribute_list('class') == ['box','wide'] "
                           "and soup.find('div').get_attribute_list('nope') == [None] "
                           "and soup.find('div').has_attr('id') and 'id' in soup.find('div')")))))))

(defdescribe bs4-selector-engine-test
             (it "supports the attribute operator set"
                 (with-python-context
                   (expect
                     (true? (ev python-context
                                (str doc
                                     "soup.select_one('a[href^=/]').get_text() == 'link' "
                                     "and soup.select_one('a[href$=x]').get_text() == 'link' "
                                     "and soup.select_one('a[href*=/]').get_text() == 'link' "
                                     "and soup.select_one('div[class~=wide]')['id'] == 'main'"))))))
             (it "supports universal, compound and grouped selectors"
                 (with-python-context
                   (expect (true? (ev python-context
                                      (str doc
                                           "len(soup.select('div > *')) == 3 "
                                           "and soup.select_one('p.lead').get_text() == 'First' "
                                           "and len(soup.select('li, a')) == 3 "
                                           "and soup.select_one('nope') is None "
                                           "and soup.select('nope') == []"))))))
             (it "reports grouped matches in document order without duplicates"
                 (with-python-context
                   (expect (true? (ev python-context
                                      (str doc
                                           "names = [t.name for t in soup.select('li, p, div')]\n"
                                           "names == ['div','p','p','li','li'] "
                                           "and len(soup.select('div, div p, p')) == 3")))))))

(defdescribe
  bs4-mutation-test
  (it "insert_before / insert_after / replace_with rewrite the parent"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str "from bs4 import BeautifulSoup\n"
                     "s = BeautifulSoup('<div><p>first</p><p>second</p></div>', 'html.parser')\n"
                     "p = s.p\n" "p.insert_before('0')\n"
                     "p.insert_after('2')\n" "old = p.replace_with('new')\n"
                     "str(s.div) == '<div>0new2<p>second</p></div>' " "and old.parent is None"))))))
  (it "wrap and unwrap are inverse operations"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from bs4 import BeautifulSoup\n"
                                "s = BeautifulSoup('<div><p>x</p></div>', 'html.parser')\n"
                                "w = s.p.wrap(s.new_tag('section'))\n"
                                "wrapped = str(s) == '<div><section><p>x</p></section></div>'\n"
                                "w.unwrap()\n" "wrapped and str(s) == '<div><p>x</p></div>'"))))))
  (it "extract removes the identical node, not an equal one"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from bs4 import BeautifulSoup\n"
                                "s = BeautifulSoup('<p>x<b>y</b>x</p>', 'html.parser')\n"
                                "gone = s.p.contents[2].extract()\n"
                                "str(s.p) == '<p>x<b>y</b></p>' and gone.parent is None"))))))
  (it "sibling navigation distinguishes equal duplicate strings"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from bs4 import BeautifulSoup\n"
                                "s = BeautifulSoup('<p>x<b>y</b>x</p>', 'html.parser')\n"
                                "kids = s.p.contents\n"
                                "kids[2].next_sibling is None "
                                "and kids[2].previous_sibling.name == 'b' "
                                "and kids[0].next_sibling.name == 'b' "
                                "and kids[0].previous_sibling is None"))))))
  (it "clear and decompose detach the children they drop"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str "from bs4 import BeautifulSoup\n"
                           "s = BeautifulSoup('<div><p>x</p><span>y</span></div>', 'html.parser')\n"
                           "kid = s.p.contents[0]\n" "s.p.clear()\n"
                           "span = s.span\n" "span.decompose()\n"
                           "kid.parent is None and s.p.contents == [] "
                           "and span.parent is None and str(s.div) == '<div><p></p></div>'"))))))
  (it "append and insert adopt plain strings as nodes"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from bs4 import BeautifulSoup, NavigableString\n"
                                "s = BeautifulSoup('<ul><li>b</li></ul>', 'html.parser')\n"
                                "s.ul.insert(0, s.new_tag('li'))\n"
                                "s.ul.contents[0].append('a')\n"
                                "isinstance(s.ul.contents[0].contents[0], NavigableString) "
                                "and s.ul.get_text() == 'ab' "
                                "and s.ul.contents[0].parent is s.ul")))))))

(defdescribe
  bs4-serialization-test
  (it
    "escapes text and switches attribute quotes the way bs4 does"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from bs4 import BeautifulSoup\n"
              "s = BeautifulSoup('<a title=' + chr(39) + 'q&quot;t' + chr(39) + '>a &amp; b</a>', 'html.parser')\n"
              "out = str(s)\n"
              ;; bs4 quotes with " normally, with ' when the value holds a ",
              ;; and only escapes to &quot; when the value holds BOTH quotes.
              "t = BeautifulSoup('<a></a>', 'html.parser').a\n"
              "t['title'] = 'a' + chr(34) + 'b' + chr(39) + 'c'\n"
              "both = str(t)\n" "'a &amp; b' in out "
              "and 'title=' + chr(39) + 'q' + chr(34) + 't' + chr(39) in out "
              "and 'title=' + chr(34) + 'a&quot;b' + chr(39) + 'c' + chr(34) in both"))))))
  (it
    "renders void elements self-closed and keeps multi-valued attributes joined"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              doc
              "s = str(soup)\n" "'<br/>' not in s "
              "and str(BeautifulSoup('<br><img src=' + chr(39) + 'x' + chr(39) + '>', 'html.parser')) == '<br/><img src=' + chr(34) + 'x' + chr(34) + '/>' "
              "and 'class=' + chr(34) + 'box wide' + chr(34) in s"))))))
  (it
    "prettify indents nested tags one space deep and ends with a newline"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from bs4 import BeautifulSoup\n"
              "p = BeautifulSoup('<div><p>hi</p></div>', 'html.parser').prettify()\n"
              ;; Real bs4 indents ONE space per level and terminates the output.
              "p == '<div>' + chr(10) + ' <p>' + chr(10) + '  hi' + chr(10) + ' </p>' + chr(10) + '</div>' + chr(10)"))))))
  (it
    "round-trips a bare string and a multi-root document"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from bs4 import BeautifulSoup\n"
              "str(BeautifulSoup('plain text', 'html.parser')) == 'plain text' "
              "and str(BeautifulSoup('<p>a</p><p>b</p>', 'html.parser')) == '<p>a</p><p>b</p>'")))))))

(defdescribe
  bs4-input-test
  (it
    "accepts bytes, file-like objects and empty markup"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "import io\n" "from bs4 import BeautifulSoup\n"
              "BeautifulSoup(b'<p>bytes</p>', 'html.parser').p.string == 'bytes' "
              "and BeautifulSoup(io.StringIO('<p>stream</p>'), 'html.parser').p.string == 'stream' "
              "and str(BeautifulSoup('', 'html.parser')) == '' "
              "and str(BeautifulSoup(None, 'html.parser')) == ''"))))))
  (it "recovers from unclosed and stray tags"
      (with-python-context
        (expect (true? (ev python-context
                           (str "from bs4 import BeautifulSoup\n"
                                "s = BeautifulSoup('<div><p>a<p>b</div></span>', 'html.parser')\n"
                                "len(s.find_all('p')) == 2 and s.get_text() == 'ab'"))))))
  (it
    "prunes the document when parse_only is given"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from bs4 import BeautifulSoup, SoupStrainer\n"
              "s = BeautifulSoup('<div><a>A</a><p>P</p><a>B</a></div>', 'html.parser', parse_only=SoupStrainer('a'))\n"
              "str(s) == '<a>A</a><a>B</a>' and len(s.find_all('a')) == 2 "
              "and s.find('p') is None and s.contents[0].parent is s")))))))

(defdescribe
  bs4-performance-test
  (it
    "walks, searches and serializes deeply nested markup without recursing"
    ;; Every one of these used to recurse once per nesting level, so a deep
    ;; document died with RecursionError instead of returning.
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from bs4 import BeautifulSoup\n"
              "deep = BeautifulSoup('<div>' * 2000 + 'leaf' + '</div>' * 2000, 'html.parser')\n"
              "len(list(deep.descendants)) == 2001 " "and deep.get_text() == 'leaf' "
              "and str(deep).count('<div>') == 2000 " "and len(deep.find_all('div')) == 2000 "
              "and deep.select_one('div div div') is not None "
              "and len(BeautifulSoup('<div>' * 800 + 'x' + '</div>' * 800, 'html.parser').prettify()) > 0"))))))
  (it "keeps chained descendant selectors from blowing up combinatorially"
      ;; A node reachable by several paths was expanded once per path, so each
      ;; extra descendant step multiplied the work: on this document a 4-step
      ;; selector took ~700x a 2-step one. The floor keeps the ratio meaningful
      ;; when both timings are near the clock's resolution.
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  perf-prelude
                  "nest = BeautifulSoup('<div>' * 120 + 'leaf' + '</div>' * 120, 'html.parser')\n"
                  "two = _best(lambda: nest.select('div div'))\n"
                  "four = _best(lambda: nest.select('div div div div'))\n"
                  "len(nest.select('div div div div')) == 117 "
                  "and four < max(two * 12.0, 0.25)"))))))
  (it "never returns the same node twice from a descendant chain"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str "from bs4 import BeautifulSoup\n"
                     "nest = BeautifulSoup('<div>' * 60 + 'leaf' + '</div>' * 60, 'html.parser')\n"
                     "got = nest.select('div div div')\n"
                     "len(got) == len(set(id(n) for n in got)) == 58"))))))
  (it
    "searches and serializes a large flat document within budget"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              perf-prelude
              "markup = '<root>' + ''.join('<div class=' + chr(34) + 'c' + chr(34) + '><section><p>' + str(i) + '</p></section></div>' for i in range(2000)) + '</root>'\n"
              "wide = BeautifulSoup(markup, 'html.parser')\n"
              "elapsed = _best(lambda: (wide.select('div.c > section > p'), wide.find_all('p'), wide.get_text(), str(wide)))\n"
              "len(wide.select('div.c > section > p')) == 2000 "
              "and len(wide.find_all('p')) == 2000 "
              "and wide.select('div.c section p')[-1].get_text() == '1999' "
              "and elapsed < 3.0")))))))

(defdescribe
  bs4-parity-test
  (it
    "decodes known entity references and keeps an unknown one literal"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from bs4 import BeautifulSoup\n"
              "s = BeautifulSoup('<p>a &foo; b &amp; c &#39;q&#39; &#x41;</p>', 'html.parser')\n"
              ;; bs4 parses references itself instead of letting html.parser fold
              ;; them: an unknown reference survives as literal text minus its
              ;; semicolon, and the whole run stays ONE NavigableString.
              "s.p.get_text() == 'a &foo b & c ' + chr(39) + 'q' + chr(39) + ' A' "
              "and len(s.p.contents) == 1 "
              "and str(s) == '<p>a &amp;foo b &amp; c ' + chr(39) + 'q' + chr(39) + ' A</p>'"))))))
  (it "walks the document with the plural finders and their camelCase aliases"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "from bs4 import BeautifulSoup\n"
                  "s = BeautifulSoup('<div><p>1</p><p>2</p><span>3</span></div>', 'html.parser')\n"
                  "s.span.previous_element.get_text() == '2' "
                  "and [t.name for t in s.p.find_next_siblings()] == ['p', 'span'] "
                  "and [t.name for t in s.span.find_all_previous('p')] == ['p', 'p'] "
                  "and [t.name for t in s.span.find_parents()] == ['div', '[document]'] "
                  "and [t.name for t in s.span.fetchParents()] == ['div', '[document]']"))))))
  (it
    "selects with sibling combinators and structural pseudo-classes"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from bs4 import BeautifulSoup\n"
              "markup = '<div><p class=' + chr(39) + 'a' + chr(39) + '>1</p><p>2</p><span>3</span><p>4</p></div>'\n"
              "s = BeautifulSoup(markup, 'html.parser')\n"
              "[t.get_text() for t in s.select('div > p:nth-of-type(2)')] == ['2'] "
              "and [t.get_text() for t in s.select('p + p')] == ['2'] "
              "and [t.get_text() for t in s.select('p ~ span')] == ['3'] "
              "and [t.get_text() for t in s.select('div :first-child')] == ['1'] "
              "and [t.get_text() for t in s.select('p:not(.a)')] == ['2', '4']"))))))
  (it "implements the Tag protocol: len, truthiness, call syntax, copy and encode"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import copy\n" "from bs4 import BeautifulSoup\n"
                                "s = BeautifulSoup('<div><p>a</p><br/></div>', 'html.parser')\n"
                                "d = s.div\n"
                                "c = copy.copy(d)\n" "d.p.string = 'z'\n"
                                ;; An empty tag is still truthy, and a copy is a detached deep clone
                                ;; that later mutation of the original cannot reach.
                                "len(d) == 2 and bool(s.br) is True and len(s('p')) == 1 "
                                "and str(c) == '<div><p>a</p><br/></div>' "
                                "and str(d) == '<div><p>z</p><br/></div>' "
                                "and d.encode() == str(d).encode()"))))))
  (it "keeps a doctype as its own node and renders it on its own line"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "from bs4 import BeautifulSoup\n"
                  "s = BeautifulSoup('<!DOCTYPE html><html><body>x</body></html>', 'html.parser')\n"
                  "str(s) == '<!DOCTYPE html>' + chr(10) + '<html><body>x</body></html>' "
                  "and type(s.contents[0]).__name__ == 'Doctype'"))))))
  (it
    "collapses whitespace-only text the way bs4 does, but preserves <pre>"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "from bs4 import BeautifulSoup\n"
              "s = BeautifulSoup('<div> a <b> b </b><!-- c --> <i>  </i>d </div>', 'html.parser')\n"
              "p = BeautifulSoup('<div><pre>a' + chr(10) + 'b</pre></div>', 'html.parser')\n"
              "s.div.get_text('|') == ' a | b | | |d ' "
              "and p.prettify() == '<div>' + chr(10) + ' <pre>a' + chr(10) "
              "+ 'b</pre>' + chr(10) + '</div>' + chr(10)"))))))
  (it "keeps the soup out of the element chain and models bare attributes"
      (with-python-context
        (expect
          (true?
            (ev
              python-context
              (str
                "from bs4 import BeautifulSoup\n"
                "s = BeautifulSoup('<div><p>1</p><b>2</b></div>', 'html.parser')\n"
                "t = BeautifulSoup('<div><p>x</p></div>', 'html.parser')\n" "gone = t.p\n"
                "gone.decompose()\n" "q = BeautifulSoup('<p>x</p>', 'html.parser')\n"
                "q.p['data-x'] = None\n"
                "[x.name for x in s.b.find_all_previous(True)] == ['p', 'div'] "
                "and s.next_element is None and s.div.previous_element is None "
                "and gone.decomposed and not t.div.decomposed "
                "and str(q.p) == '<p data-x>x</p>' "
                "and [str(c) for c in s.div] == ['<p>1</p>', '<b>2</b>'] and len(s.div) == 2"))))))
  (it "handles CDATA, recursive smooth, copy of a soup and ascii encode"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import copy\n"
                                "from bs4 import BeautifulSoup\n"
                                "s = BeautifulSoup('<p><![CDATA[x<y]]></p>', 'html.parser')\n"
                                "c = BeautifulSoup('<div><p>a</p></div>', 'html.parser')\n"
                                "c.p.append('b')\n"
                                "c.p.append('c')\n" "c.smooth()\n"
                                "d = copy.copy(c)\n"
                                "e = BeautifulSoup('<p>caf' + chr(233) + '</p>', 'html.parser')\n"
                                "str(s) == '<p><![CDATA[x<y]]></p>' "
                                "and type(s.p.contents[0]).__name__ == 'CData' "
                                "and len(c.p.contents) == 1 and c.p.get_text() == 'abc' "
                                "and type(d).__name__ == 'BeautifulSoup' and str(d) == str(c) "
                                "and c.p.replace_with(c.p) is c.p "
                                "and e.encode('ascii') == '<p>caf&#233;</p>'.encode('ascii')")))))))
