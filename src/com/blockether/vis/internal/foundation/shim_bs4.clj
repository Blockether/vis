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
   `find`/`find_all` plus the plural and directional finders
   (`find_next_siblings`, `find_all_previous`, `find_parents`, and the camelCase
   aliases), CSS `.select` / `.select_one` (type / `#id` / `.class` /
   `[attr]`/`[attr=v]`/`~=`/`^=`/`$=`/`*=`, descendant / `>` / `+` / `~`
   combinators, comma groups, and the structural pseudo-classes `:not`, `:is`,
   `:nth-child`, `:nth-of-type`, `:first-child`, `:last-of-type`, `:empty`,
   `:root`, `:contains`), `get_text`, `.string`/`.strings`/`.stripped_strings`,
   sibling/parent navigation, dynamic `soup.tagname` access, tree mutation, and
   HTML serialization (`str` / `encode` / `prettify`). Entity handling, escaping
   and attribute quoting follow bs4 4.12, as does the introspection surface:
   `PageElement`/`ResultSet`/`SoupStrainer`, `output_ready`, `sourceline`,
   `is_empty_element`, `soup.builder` + `bs4.builder`, formatter objects, and
   encoding detection (`original_encoding`, `bs4.dammit.UnicodeDammit`).
   A bundled soupsieve-compatible engine (published as `soupsieve`, version 2.5,
   next to `soup.css`) backs the selector layer, so `:has()`, namespace selectors,
   custom `:--name` selectors, `iselect` and soupsieve's error surface behave like
   upstream. Differentially tested against real beautifulsoup4 4.12.3 + soupsieve
   2.5 over 200+ probes with zero output mismatches outside documented
   divergences. A deliberate subset of full bs4 (no lxml or html5lib parsers —
   another `features` argument is honored leniently rather than raising
   `FeatureNotFound` — and generic CSS syntax errors carry a simpler message).

   Like `shim-requests` there are NO `:shim/bindings`: the shim is a
   self-contained Python preamble with zero host callables. It publishes `bs4`
   (+ `bs4.element`, `bs4.formatter`, `bs4.builder`, `bs4.dammit`, `bs4.css`,
   `bs4.diagnose`) and `soupsieve` into `sys.modules` (so `from bs4 import
   BeautifulSoup` and `import soupsieve` both work) and staples `BeautifulSoup`
   onto builtins."
  (:require [com.blockether.vis.core :as vis]))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-bs4"
     :ext/description
     (str "Sandbox pure-stdlib `bs4`/BeautifulSoup subset: "
          "find/find_all and the directional finders, CSS select/select_one with sibling "
          "combinators and structural pseudo-classes, get_text, navigation, tree mutation, HTML "
          "serialization, and bs4's introspection surface (PageElement/ResultSet/SoupStrainer, "
          "builder, formatters, encoding detection). Uses `html.parser`; pairs with requests for "
          "fetch+parse. No pip/wheel/host bridge.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "bs4"
       :shim/imports ["bs4" "soupsieve"]
       :shim/description
       (str "`bs4` BeautifulSoup API (`find`, `find_all`, `select`, `get_text`, mutation, "
            "serialization, plus PageElement/ResultSet/SoupStrainer, `soup.builder`, formatters "
            "and `bs4.dammit` encoding detection) via stdlib `html.parser`, with a bundled "
            "soupsieve-compatible engine (`soupsieve` 2.5, `soup.css`, `:has()`, namespace and "
            "custom selectors). No lxml/html5lib parsers.")
       :shim/source "vis-shims/bs4.py"}]}))

(vis/register-extension! vis-extension)
