"""The bundled language surfaces decide syntax the way the write gate expects.

`resources/vis-extensions/` ships three extensions — `language-surface`,
`language-surface-python` and `language-surface-clojure` — that share the
`vis_language_surface` package. These tests exercise that package directly, so a
wrong verdict is caught here rather than in the engine that loads it.
"""

import sys
from pathlib import Path

import pytest

EXTENSIONS = Path(__file__).resolve().parents[3] / "resources" / "vis-extensions"

if str(EXTENSIONS) not in sys.path:
    sys.path.insert(0, str(EXTENSIONS))

from vis_language_surface import (  # noqa: E402  (the path above makes it importable)
    KINDS,
    clojure,
    data,
    finding,
    language_of,
    source_of,
    verdict,
)
from vis_language_surface import python as python_surface  # noqa: E402


def located(result, language):
    """Assert one verdict is a contract-legal `syntax_result` and return its findings."""
    assert set(result) >= {"language", "is_clean", "findings"}
    assert result["language"] == language
    assert isinstance(result["findings"], list)
    assert result["is_clean"] is (not result["findings"])
    for row in result["findings"]:
        assert row["kind"] in KINDS
        assert isinstance(row["line"], int) and row["line"] >= 1
        assert isinstance(row["col"], int) and row["col"] >= 0
    return [(row["kind"], row["line"], row["col"]) for row in result["findings"]]


def test_finding_refuses_a_kind_the_contract_does_not_accept():
    with pytest.raises(ValueError):
        finding("broken", 1, 0)


def test_finding_clamps_positions_and_drops_absent_detail():
    assert finding("parse", 0, -3, message="bad", delimiter=None) == {
        "line": 1,
        "col": 0,
        "kind": "parse",
        "message": "bad",
    }


def test_verdict_is_clean_only_without_findings():
    clean = verdict("json", validator="json.loads")
    assert clean == {
        "language": "json",
        "is_clean": True,
        "findings": [],
        "is_exact": True,
        "validator": "json.loads",
    }
    faulty = verdict("clojure", [finding("unclosed", 2, 4)], is_exact=False)
    assert faulty["is_clean"] is False
    assert faulty["is_exact"] is False
    assert "validator" not in faulty


def test_a_request_may_name_its_language_or_carry_none():
    assert source_of({"source": "x = 1"}) == "x = 1"
    assert source_of({"language": "python"}) == ""
    assert source_of(None) == ""
    assert language_of({"language": "cljs"}, "clojure") == "cljs"
    assert language_of({"source": "()"}, "clojure") == "clojure"
    assert language_of("()", "clojure") == "clojure"


@pytest.mark.parametrize(
    "source",
    [
        '(ns app.core)\n(defn greet [name] (str "hi " name))\n',
        '; a comment with ( and " in it\n(inc 1)\n',
        '(re-find #"[a-z]" "abc")\n',
        "(map #(+ % 1) [1 2 3])\n",
        "#_(ignored form) {:a 1}\n",
        "#?(:clj (inc 1) :cljs (dec 1))\n",
        '[\\( \\) \\; \\" \\space]\n',
        "#!/usr/bin/env bb\n(println :ok)\n",
        '(println "he said \\"hi\\"")\n',
        "{:a #{1 2} :b [3]}\n",
        "",
    ],
)
def test_balanced_clojure_is_clean(source):
    result = clojure.syntax({"language": "clojure", "source": source})
    assert located(result, "clojure") == []
    assert result["is_exact"] is False
    assert result["validator"] == "clojure-delimiters"


def test_clojure_reports_an_unclosed_form_at_its_opener():
    result = clojure.syntax(
        {"language": "clojure", "source": "(defn broken [x]\n  (+ x 1)\n"}
    )
    assert located(result, "clojure") == [("unclosed", 1, 0)]
    assert result["findings"][0]["delimiter"] == "("


def test_clojure_reports_a_closer_that_opened_nothing():
    result = clojure.syntax({"language": "clojure", "source": "(inc 1))\n"})
    assert located(result, "clojure") == [("unexpected", 1, 7)]
    assert result["findings"][0]["delimiter"] == ")"


def test_clojure_reports_an_unterminated_string():
    result = clojure.syntax({"language": "clojure", "source": '(str "open\n'})
    assert located(result, "clojure") == [("unclosed", 1, 0), ("unclosed", 1, 5)]
    assert result["findings"][1]["message"] == "unterminated string"


def test_clojure_reports_a_closer_that_does_not_match_its_opener():
    result = clojure.syntax({"language": "clojure", "source": "(let [a 1}\n  a)\n"})
    assert located(result, "clojure") == [
        ("unclosed", 1, 0),
        ("unclosed", 1, 5),
        ("unexpected", 1, 9),
        ("unexpected", 2, 3),
    ]


def test_clojure_keeps_the_dialect_the_request_names():
    assert clojure.syntax({"language": "edn", "source": "{:a 1}"})["language"] == "edn"


def test_clojure_scan_answers_the_same_findings_as_the_surface():
    source = "(inc 1))\n"
    assert clojure.scan(source) == clojure.syntax({"source": source})["findings"]


@pytest.mark.parametrize(
    "source",
    [
        "def f():\n    return 1\n",
        "",
        "x = {'a': [1, 2]}\n",
    ],
)
def test_valid_python_is_clean(source):
    result = python_surface.syntax({"language": "python", "source": source})
    assert located(result, "python") == []
    assert result["is_exact"] is True
    assert result["validator"] == "compile"


def test_python_names_the_delimiter_that_was_never_closed():
    result = python_surface.syntax({"language": "python", "source": "x = (1, 2\n"})
    assert located(result, "python") == [("unclosed", 1, 4)]
    assert result["findings"][0]["delimiter"] == "("


@pytest.mark.parametrize(
    ("source", "delimiter"),
    [("x = 1)\n", ")"), ("x = [1, 2}\n", "}")],
)
def test_python_reports_a_closer_that_matches_nothing(source, delimiter):
    result = python_surface.syntax({"language": "python", "source": source})
    kinds = [kind for kind, _, _ in located(result, "python")]
    assert kinds == ["unexpected"]
    assert result["findings"][0]["delimiter"] == delimiter


def test_python_reports_a_plain_syntax_error_as_a_parse_fault():
    result = python_surface.syntax(
        {"language": "python", "source": "def f():\nreturn 1\n"}
    )
    assert located(result, "python") == [("parse", 2, 0)]
    assert "indented block" in result["findings"][0]["message"]


def test_python_keeps_the_stub_dialect_the_request_names():
    result = python_surface.syntax(
        {"language": "pyi", "source": "def f() -> int: ...\n"}
    )
    assert result["language"] == "pyi"
    assert result["is_clean"] is True


def test_valid_json_is_clean():
    result = data.json_syntax({"language": "json", "source": '{"a": [1, 2]}\n'})
    assert located(result, "json") == []
    assert result["validator"] == "json.loads"


@pytest.mark.parametrize(
    ("source", "line"),
    [('{"a": 1,}\n', 1), ("[1, 2\n", 2), ("not json", 1)],
)
def test_broken_json_carries_the_decoder_position(source, line):
    result = data.json_syntax({"language": "json", "source": source})
    kinds = located(result, "json")
    assert kinds == [("parse", line, kinds[0][2])]
    assert result["findings"][0]["message"]


def test_valid_toml_is_clean():
    result = data.toml_syntax(
        {"language": "toml", "source": "[tool.ruff]\nline-length = 88\n"}
    )
    assert located(result, "toml") == []
    assert result["validator"] == "tomllib"


@pytest.mark.parametrize(
    ("source", "line"),
    [("[tool\n", 1), ("a = = 1\n", 1), ("a = 1\na = 2\n", 2)],
)
def test_broken_toml_carries_the_parser_position(source, line):
    result = data.toml_syntax({"language": "toml", "source": source})
    kinds = located(result, "toml")
    assert [kind for kind, _, _ in kinds] == ["parse"]
    assert kinds[0][1] == line


def test_toml_is_available_wherever_the_sandbox_runs():
    assert data.HAS_TOML is True
