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

(defmacro with-python-context
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ~'python-context)))))

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

(defdescribe
  bs4-module-test
  (it "publishes bs4 + bs4.element under sys.modules"
      (with-python-context (expect (true?
                                     (ev python-context
                                         (str "import sys\n"
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
