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
   and attribute quoting follow bs4 4.12. A deliberate subset of full bs4 (no
   lxml or html5lib parsers, no soupsieve namespace or `:has()` selectors).

   Like `shim-requests` there are NO `:shim/bindings`: the shim is a
   self-contained Python preamble with zero host callables. It publishes a `bs4`
   module (+ `bs4.element`) into `sys.modules` (so `from bs4 import BeautifulSoup`
   works) and staples `BeautifulSoup` onto builtins."
  (:require [com.blockether.vis.core :as vis]))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-bs4"
     :ext/description
     "Sandbox pure-stdlib `bs4`/BeautifulSoup subset: find/find_all and the directional finders, CSS select/select_one with sibling combinators and structural pseudo-classes, get_text, navigation, tree mutation, and HTML serialization. Uses `html.parser`; pairs with requests for fetch+parse. No pip/wheel/host bridge."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "bs4"
       :shim/imports ["bs4"]
       :shim/description
       "`bs4` BeautifulSoup API (`find`, `find_all`, `select`, `get_text`, mutation, serialization) via stdlib `html.parser`. No lxml/html5lib parsers, no soupsieve `:has()`/namespace selectors."
       :shim/source "vis-shims/bs4.py"}]}))

(vis/register-extension! vis-extension)
