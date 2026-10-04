"""Keep each Python SDK page and its HTTP API mirror page in step.

`docs-modules-test` in the docs core test checks that both pages have the same `##` headings.
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
# `python-sdk.md` and `http-api.md` are the two basics pages. `python-sandbox.md` is a concept.
_BASICS = {"python-sdk.md", "python-sandbox.md", "http-api.md"}
_CONCEPTS = sorted(
    page.name.removeprefix(prefix).removesuffix(".md")
    for page in _PAGES
    for prefix in ("python-", "http-")
    if page.name.startswith(prefix) and page.name not in _BASICS
)


def test_pages_name_gateway_routes_only_on_http_pages():
    for page in _PAGES:
        if not page.name.startswith("http-"):
            assert not _ROUTE.search(page.read_text()), f"{page.name} names a route"
    assert "automations" in _CONCEPTS


@pytest.mark.parametrize("concept", sorted(set(_CONCEPTS)))
def test_each_concept_has_a_python_page_and_an_http_page(concept):
    assert _CONCEPTS.count(concept) == 2, f"{concept}: only one mirror page"
    assert (_DOCS / f"{concept}.md").exists(), f"{concept}: no concept page"


@pytest.mark.parametrize("concept", sorted(set(_CONCEPTS)))
def test_python_and_http_pages_cover_the_same_routes(concept):
    python = (_DOCS / f"python-{concept}.md").read_text()
    http = (_DOCS / f"http-{concept}.md").read_text()
    audiences, methods = _audiences(), _sdk_methods()
    routes = {(verb, _shape(path)) for verb, path in _ROUTE.findall(http)}
    assert routes, f"http-{concept}.md names no route"
    unknown = sorted(route for route in routes if route not in audiences)
    assert not unknown, f"http-{concept}.md: unknown routes {unknown}"
    sdk_routes = {route for route in routes if audiences[route] == "sdk"}
    without_call = sorted(
        route for route in sdk_routes if route not in methods and route not in _TYPED
    )
    assert not without_call, (
        f"http-{concept}.md: routes without an SDK call {without_call}"
    )
    expected = {methods[route] for route in sdk_routes if route in methods}
    named = {
        name for name in set(methods.values()) if re.search(rf"\b{name}\(", python)
    }
    assert named == expected, (
        f"{concept}: only on the HTTP page {sorted(expected - named)}, "
        f"only on the Python page {sorted(named - expected)}"
    )
    typed = sorted(
        name
        for route, name in _TYPED.items()
        if route in sdk_routes
        and route not in methods
        and not re.search(rf"\.{name}\(", python)
    )
    assert not typed, f"python-{concept}.md: typed calls missing {typed}"
