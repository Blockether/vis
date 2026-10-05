"""Keep the Python and HTTP variants of each merged API page in step.

A merged page holds paired `<div data-variant="python">` and `<div data-variant="http">`
blocks. The site shows both behind one switch; `doc` gives the agent the Python variant.
"""

import inspect
import re
from pathlib import Path

import pytest
from blockether.vis._contracts import schema
from blockether.vis.engine import GatewayClient

_DOCS = Path(__file__).parents[3] / "resources/vis-docs"
_ROUTES = schema("gateway")["x-vis-routes"]
_ROUTE = re.compile(r"\b(GET|POST|PATCH|PUT|DELETE) (/v1/[^\s`?)]*)")
# SDK routes that a typed handle serves, not a generated `GatewayClient` method.
_TYPED = {
    ("GET", "/v1/events"): "events",
    ("GET", "/v1/sessions/:/council"): "council",
    ("GET", "/v1/sessions/:/council/members"): "members",
    ("POST", "/v1/sessions/:/council/entries"): "publish",
    ("GET", "/v1/sessions/:/council/entries"): "read",
    ("GET", "/v1/sessions/:/council/entries/:"): "get",
    ("GET", "/v1/sessions/:/council/threads"): "threads",
    ("POST", "/v1/sessions/:/council/wake"): "wake",
}


def _shape(path):
    """The route path with each parameter segment written as `:`."""
    return "/".join(
        ":" if segment.startswith((":", "{")) else segment
        for segment in path.rstrip("/").split("/")
    )


def _audiences():
    return {
        (method.upper(), _shape(route["path"])): route["audience"]
        for route in _ROUTES
        for method in route["operations"]
    }


def _sdk_methods():
    """The `GatewayClient` method of each route, read from the method docstring."""
    methods = {}
    for name, member in inspect.getmembers(GatewayClient, inspect.isfunction):
        words = (member.__doc__ or "").split()
        if len(words) >= 2 and words[1].startswith("/v1/"):
            methods[(words[0], _shape(words[1]))] = name
    return methods


_PAGES = sorted(_DOCS.glob("*.md"))
_VARIANT_OPEN = re.compile(r'<div data-variant="([a-z]+)">')
_FENCE = re.compile(r"^\s*(?:```|~~~)")


def _variant_text(md, variant):
    """`md` as a reader of `variant` gets it: lines outside every block, and the
    lines of the `variant` blocks without their tags. A tag in fenced code is text."""
    kept, fenced, current = [], False, None
    for line in md.splitlines():
        fence = bool(_FENCE.match(line))
        opening = (
            None
            if (fenced or fence or current)
            else _VARIANT_OPEN.fullmatch(line.strip())
        )
        closing = current and not fenced and not fence and line.strip() == "</div>"
        if opening:
            current = opening[1]
        elif closing:
            current = None
        elif current in (None, variant):
            kept.append(line)
        if fence:
            fenced = not fenced
    return "\n".join(kept)


# A merged page is `<topic>-api.md` with paired variant blocks. `http-api.md` is a basics
# page and `extension-api.md` has no variants, so neither one is a topic here.
_CONCEPTS = sorted(
    page.name.removesuffix("-api.md")
    for page in _PAGES
    if page.name.endswith("-api.md") and 'data-variant="python"' in page.read_text()
)


def test_pages_name_gateway_routes_only_on_http_pages():
    # An empty topic list once turned the parametrized cases below into zero cases.
    assert len(_CONCEPTS) == 7, f"merged API pages found: {_CONCEPTS}"
    for page in _PAGES:
        if page.name == "http-api.md":
            continue
        # Only the HTTP variant of a merged page may name a route.
        text = _variant_text(page.read_text(), "python")
        assert not _ROUTE.search(text), (
            f"{page.name} names a route outside its HTTP variant"
        )


@pytest.mark.parametrize("concept", sorted(set(_CONCEPTS)))
def test_each_merged_page_has_both_variants_and_a_concept_page(concept):
    page = (_DOCS / f"{concept}-api.md").read_text()
    for variant in ("python", "http"):
        assert f'data-variant="{variant}"' in page, (
            f"{concept}-api.md: no {variant} variant"
        )
    assert (_DOCS / f"{concept}.md").exists(), f"{concept}: no concept page"


@pytest.mark.parametrize("concept", sorted(set(_CONCEPTS)))
def test_python_and_http_variants_cover_the_same_routes(concept):
    page = (_DOCS / f"{concept}-api.md").read_text()
    python, http = _variant_text(page, "python"), _variant_text(page, "http")
    audiences, methods = _audiences(), _sdk_methods()
    routes = {(verb, _shape(path)) for verb, path in _ROUTE.findall(http)}
    assert routes, f"{concept}-api.md names no route in its HTTP variant"
    unknown = sorted(route for route in routes if route not in audiences)
    assert not unknown, f"{concept}-api.md: unknown routes {unknown}"
    sdk_routes = {route for route in routes if audiences[route] == "sdk"}
    without_call = sorted(
        route for route in sdk_routes if route not in methods and route not in _TYPED
    )
    assert not without_call, (
        f"{concept}-api.md: routes without an SDK call {without_call}"
    )
    expected = {methods[route] for route in sdk_routes if route in methods}
    named = {
        name for name in set(methods.values()) if re.search(rf"\b{name}\(", python)
    }
    assert named == expected, (
        f"{concept}: only in the HTTP variant {sorted(expected - named)}, "
        f"only in the Python variant {sorted(named - expected)}"
    )
    typed = sorted(
        name
        for route, name in _TYPED.items()
        if route in sdk_routes
        and route not in methods
        and not re.search(rf"\.{name}\(", python)
    )
    assert not typed, (
        f"{concept}-api.md: typed calls missing from the Python variant {typed}"
    )
