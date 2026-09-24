"""Keep the generated extension reference's examples executable and host-free."""

import inspect
import re
import socket
import subprocess
import sys
from pathlib import Path
from types import ModuleType

import blockether.vis as sdk
import pytest
from blockether.vis import activity, engine, extension, extension_package, views


@pytest.mark.parametrize("owner", [extension.method])
def test_reference_examples_execute_without_host_or_network(owner, monkeypatch):
    source = inspect.getdoc(owner)
    examples = re.findall(r"```python\n(.*?)\n```", source, re.S)
    assert examples, f"{owner.__name__} needs an executable example"

    def refuse(*args, **kwargs):
        pytest.fail(
            "Reference examples must not start processes, use the network or register a host"
        )

    monkeypatch.setattr(subprocess, "Popen", refuse)
    monkeypatch.setattr(socket, "create_connection", refuse)
    monkeypatch.setattr(extension._host, "declare_env", refuse)
    before = extension._registration["spec"]
    for index, example in enumerate(examples):
        module = ModuleType(f"reference_example_{index}")
        monkeypatch.setitem(sys.modules, module.__name__, module)
        exec(
            compile(example, f"{owner.__name__} example", "exec", dont_inherit=True),
            module.__dict__,
        )
    assert extension._registration["spec"] is before


@pytest.mark.parametrize(
    "module", [sdk, engine, extension, activity, views, extension_package]
)
def test_module_introductions_link_to_guides_without_repeating_them(module):
    source = inspect.getdoc(module)
    assert not re.search(r"^#{1,6} |^```|^\|", source, re.M)
    manual = Path(__file__).resolve().parents[3] / "resources/vis-docs"
    links = re.findall(r"https://vis\.blockether\.com/([a-z-]+)\.html", source)
    assert links
    for page in links:
        assert (manual / f"{page}.md").is_file(), page
