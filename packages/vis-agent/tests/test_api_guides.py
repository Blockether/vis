"""Keep the Python SDK and HTTP API sections of each guide in step."""

import inspect
import re
from pathlib import Path

import pytest
from blockether.vis._contracts import schema
from blockether.vis.engine import GatewayClient

_DOCS = Path(__file__).parents[3] / "resources/vis-docs"
_ROUTES = schema("gateway")["x-vis-routes"]
_ROUTE = re.compile(r"\b(GET|POST|PATCH|PUT|DELETE) (/v1/[^\s`?)]*)")


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


def _sections(text):
    """The text of each `##` section, with headings in fenced code ignored."""
    sections, title, fence = {}, None, False
    for line in text.splitlines():
        if line.startswith("```"):
            fence = not fence
        if not fence and line.startswith("## "):
            title = line[3:].strip()
            sections[title] = []
        elif title is not None:
            sections[title].append(line)
    return {title: "\n".join(lines) for title, lines in sections.items()}


def _subsections(text):
    fence, count = False, 0
    for line in text.splitlines():
        if line.startswith("```"):
            fence = not fence
        elif not fence and line.startswith("### "):
            count += 1
    return count


_PAGES = sorted(_DOCS.glob("*.md"))
_GUIDES = [
    page.name
    for page in _PAGES
    if {"Python SDK", "HTTP API"} <= set(_sections(page.read_text()))
]


def test_guides_name_gateway_routes_only_in_their_http_api_section():
    for page in _PAGES:
        for title, body in _sections(page.read_text()).items():
            if title != "HTTP API":
                assert not _ROUTE.search(body), f"{page.name}: route in ## {title}"
    assert "automations.md" in _GUIDES


@pytest.mark.parametrize("page", _GUIDES)
def test_python_sdk_and_http_api_sections_cover_the_same_routes(page):
    sections = _sections((_DOCS / page).read_text())
    audiences, methods = _audiences(), _sdk_methods()
    routes = {
        (verb, _shape(path)) for verb, path in _ROUTE.findall(sections["HTTP API"])
    }
    assert routes, f"{page}: ## HTTP API names no route"
    unknown = sorted(route for route in routes if route not in audiences)
    assert not unknown, f"{page}: unknown routes {unknown}"
    sdk_routes = {route for route in routes if audiences[route] == "sdk"}
    without_method = sorted(route for route in sdk_routes if route not in methods)
    assert not without_method, f"{page}: routes without an SDK method {without_method}"
    expected = {methods[route] for route in sdk_routes}
    python = sections["Python SDK"]
    named = {
        name for name in set(methods.values()) if re.search(rf"\b{name}\(", python)
    }
    assert named == expected, (
        f"{page}: only in ## HTTP API {sorted(expected - named)}, "
        f"only in ## Python SDK {sorted(named - expected)}"
    )
    assert _subsections(sections["Python SDK"]) == _subsections(sections["HTTP API"]), (
        f"{page}: ## Python SDK and ## HTTP API have different subsections"
    )
