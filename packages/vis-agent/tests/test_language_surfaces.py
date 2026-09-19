"""The bundled language surfaces decide syntax the way the write gate expects.

`resources/vis-extensions/` ships three extensions — `language-surface`,
`language-surface-python` and `language-surface-clojure` — that share the
`vis_language_surface` package. These tests exercise that package directly, so a
wrong verdict is caught here rather than in the engine that loads it.
"""

import json
import os
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
from vis_language_surface import balance as policy  # noqa: E402
from vis_language_surface import python as python_surface  # noqa: E402
from vis_language_surface import repl as python_repl  # noqa: E402


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


@pytest.mark.parametrize("source", ['{"a": 1,}\n', "[1, 2\n", "not json"])
def test_broken_json_carries_the_decoder_position(source):
    result = data.json_syntax({"language": "json", "source": source})
    kinds = located(result, "json")
    assert [kind for kind, _, _ in kinds] == ["parse"]
    # Which line a decoder blames for a truncated document differs between
    # interpreters, so only the document's own range is guaranteed.
    assert kinds[0][1] <= len(source.splitlines()) + 1
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


# The delimiter repair the write gate asks for when an edit would not parse. The engine
# carries none of this: it hands the pack the whole file the edit would write, the file it
# replaced and the lines that edit touched, and writes only what the pack accepts.

FIXTURE = "(ns reb)\n\n(defn ok [] 1)\n\n(defn two [] 2)\n"


def repair_of(source, original=FIXTURE, spans=((3, 3),)):
    return clojure.balance(
        {
            "language": "clojure",
            "source": source,
            "original": original,
            "spans": [list(span) for span in spans],
        }
    )


def stubbed(candidate, source, original=FIXTURE, spans=((3, 3),)):
    # The verdict for a repair a balancer could answer with, whatever parinfer makes of it.
    return policy.rebalance(
        {
            "balancer": lambda _source: candidate,
            "parses_clean": clojure.parses_clean,
            "source": source,
            "original": original,
            "spans": [list(span) for span in spans],
        }
    )


def test_balance_answers_nothing_for_a_file_whose_delimiters_balance():
    assert repair_of(FIXTURE) is None


def test_balance_writes_an_in_bounds_repair_and_names_the_line():
    answer = repair_of("(ns reb)\n\n(defn ok [] (inc 1)\n\n(defn two [] 2)\n")
    assert answer["ok"] is True
    assert answer["content"] == "(ns reb)\n\n(defn ok [] (inc 1))\n\n(defn two [] 2)\n"
    assert answer["notes"] == ["line 3 added `)` → `(defn ok [] (inc 1))`"]


def test_balance_names_an_unterminated_string_no_repair_can_close():
    answer = repair_of('(ns reb)\n\n(defn ok [] "1)\n\n(defn two [] 2)\n')
    assert answer["ok"] is False
    assert "line 3 opens a string that is never closed" in answer["why"]


def test_balance_restores_an_opener_this_edit_lost():
    # A replacement that dropped its `(` is character for character one `)` too many; only
    # the line it replaced tells the two apart, and deleting a closer is never the answer.
    answer = repair_of(
        "(ns reb)\n\ndefn ok [] (inc 1))\n",
        original="(ns reb)\n\n(defn ok [] (inc 1))\n",
    )
    assert answer["ok"] is True
    assert answer["content"] == "(ns reb)\n\n(defn ok [] (inc 1))\n"
    assert answer["notes"] == ["line 3 added `(` → `(defn ok [] (inc 1))`"]


def test_balance_seats_a_closer_where_the_replaced_line_had_it():
    # Indentation alone would put the closer at the end of the line and regroup the
    # arguments between; the text this edit replaced says where it belongs.
    answer = repair_of(
        '(ns reb)\n\n(cond (map? x (str "m" (count x)))\n',
        original='(ns reb)\n\n(cond (map? x) (str "m" (count x)))\n',
    )
    assert answer["ok"] is True
    assert answer["content"] == '(ns reb)\n\n(cond (map? x) (str "m" (count x)))\n'


def test_balance_repairs_a_deletion_on_the_seam_it_left():
    # A deletion writes no line at all, so the repair belongs on the seam the removed
    # lines left behind, not on a line the new content no longer has.
    answer = repair_of(
        "(ns reb)\n\n(defn ok []\n",
        original="(ns reb)\n\n(defn ok []\n  1)\n",
    )
    assert answer["ok"] is True
    assert answer["content"] == "(ns reb)\n\n(defn ok [])\n"
    assert answer["notes"] == ["line 3 added `)` → `(defn ok [])`"]


def test_balance_refuses_a_repair_that_retypes_a_delimiter_this_edit_wrote():
    # `(foo [1 2] 3)` mistyped as `(foo (1 2] 3)` comes back as a call that swallowed its
    # argument: it parses, it is one line, and only the ORDER of the delimiters refuses it.
    answer = stubbed(
        "(ns reb)\n\n(defn ok [] (foo (1 2 3)))\n\n(defn two [] 2)\n",
        "(ns reb)\n\n(defn ok [] (foo (1 2] 3))\n\n(defn two [] 2)\n",
    )
    assert answer["ok"] is False
    assert "would move or retype a delimiter this edit wrote" in answer["why"]


def test_balance_refuses_a_repair_that_deletes_a_closer_this_edit_wrote():
    answer = stubbed(
        "(ns reb)\n\n(defn ok [] inc 1)\n\n(defn two [] 2)\n",
        "(ns reb)\n\n(defn ok [] inc 1))\n\n(defn two [] 2)\n",
    )
    assert answer["ok"] is False
    assert "would delete `)` this edit wrote" in answer["why"]
    assert "closes more than it opens, or an opener was lost" in answer["why"]


def test_balance_refuses_a_repair_that_rewrites_code_instead_of_delimiters():
    answer = stubbed(
        "(ns reb)\n\n(defn ok [] (dec 1))\n\n(defn two [] 2)\n",
        "(ns reb)\n\n(defn ok [] (inc 1)))\n\n(defn two [] 2)\n",
    )
    assert answer["ok"] is False
    assert answer["why"] == "the delimiter repair would rewrite code, not delimiters"


def test_balance_closes_the_edits_own_line_when_a_repair_would_swallow_another_form():
    # The balancer's own answer closes the LAST form in the file, which this edit never
    # touched; what this edit omitted goes back on the line it wrote instead.
    answer = stubbed(
        "(ns reb)\n\n(defn ok [] (inc 1)\n\n(defn two [] 2))\n",
        "(ns reb)\n\n(defn ok [] (inc 1)\n\n(defn two [] 2)\n",
    )
    assert answer["ok"] is True
    assert answer["content"] == "(ns reb)\n\n(defn ok [] (inc 1))\n\n(defn two [] 2)\n"
    assert answer["notes"] == ["line 3 added `)` → `(defn ok [] (inc 1))`"]


# The managed Python REPL — a real interpreter child, started and stopped here.


@pytest.fixture
def repl_dir(tmp_path):
    """A project directory whose REPL is stopped however the test ends."""
    options = {"cwd": str(tmp_path)}
    yield options
    python_repl.repl_start("stop", options)


def test_repl_status_is_down_before_anything_started_it(repl_dir):
    answer = python_repl.repl_start("status", repl_dir)
    assert answer["result"] == "status"
    assert answer["status"] == "down"
    assert answer["id"] == "pyrepl:" + os.path.realpath(repl_dir["cwd"])
    assert "pid" not in answer


def test_repl_keeps_globals_between_evaluations(repl_dir):
    started = python_repl.repl_start("start", repl_dir)
    assert started["result"] == "started"
    assert started["status"] == "up"
    assert started["pid"] > 0
    assert started["cmd"][-1] == "<vis python driver>"

    python_repl.repl_eval({**repl_dir, "code": "seen = 41"})
    answer = python_repl.repl_eval({**repl_dir, "code": "seen + 1"})
    assert answer["ok"] is True
    assert answer["value"] == "42"
    assert answer["data"] == 42
    assert answer["type"] == "int"
    assert answer["code"] == "seen + 1"


def test_repl_reports_output_and_the_traceback_of_a_failure(repl_dir):
    python_repl.repl_start("start", repl_dir)
    printed = python_repl.repl_eval({**repl_dir, "code": "print('hi')"})
    assert printed["ok"] is True
    assert printed["out"] == "hi\n"

    broken = python_repl.repl_eval({**repl_dir, "code": "1 / 0"})
    assert broken["ok"] is False
    assert "ZeroDivisionError" in broken["exc"]


def test_repl_eval_without_a_live_repl_says_how_to_start_one(repl_dir):
    with pytest.raises(python_repl.ReplError) as refused:
        python_repl.repl_eval({**repl_dir, "code": "1"})
    assert 'repl_start("python"' in str(refused.value)


def test_repl_eval_needs_code(repl_dir):
    with pytest.raises(ValueError):
        python_repl.repl_eval({**repl_dir})


def test_repl_start_reuses_the_live_interpreter(repl_dir):
    first = python_repl.repl_start("start", repl_dir)
    again = python_repl.repl_start("start", repl_dir)
    assert again["result"] == "already-running"
    assert again["pid"] == first["pid"]


def test_repl_stop_ends_the_process_and_is_no_op_safe(repl_dir):
    python_repl.repl_start("start", repl_dir)
    stopped = python_repl.repl_start("stop", repl_dir)
    assert stopped["result"] == "stopped"
    assert stopped["status"] == "down"
    assert python_repl.repl_start("stop", repl_dir)["result"] == "not-managed"


def test_repl_start_takes_the_env_delta_and_shows_only_its_fingerprint(repl_dir):
    started = python_repl.repl_start(
        "start",
        {
            **repl_dir,
            "env": {"VIS_REPL_TOKEN": "s3cret"},
            "env_fingerprint": {"VIS_REPL_TOKEN": "abc123def456"},
        },
    )
    assert started["env"] == {"VIS_REPL_TOKEN": "abc123def456"}
    assert "s3cret" not in json.dumps(started)

    seen = python_repl.repl_eval(
        {**repl_dir, "code": "import os; os.environ['VIS_REPL_TOKEN']"}
    )
    assert seen["data"] == "s3cret"


def test_repl_lifecycle_refuses_an_unknown_op(repl_dir):
    with pytest.raises(ValueError) as refused:
        python_repl.repl_start("connect", repl_dir)
    assert "repl_connect" in str(refused.value)


def test_detect_command_prefers_a_project_virtualenv(tmp_path):
    interpreter = tmp_path / ".venv" / "bin"
    interpreter.mkdir(parents=True)
    (interpreter / "python").write_text("")
    assert python_repl.detect_command(str(tmp_path)) == [str(interpreter / "python")]


def test_uv_detection_reads_toml_tables_not_substrings(tmp_path):
    (tmp_path / "pyproject.toml").write_text(
        '[project]\ndescription = "run it with [tool.uv]"\n\n[tool.uvicorn]\nport = 80\n'
    )
    assert python_repl._is_uv_project(tmp_path) is False

    (tmp_path / "pyproject.toml").write_text(
        '[tool.uv.sources]\nvis = { path = "." }\n'
    )
    assert python_repl._is_uv_project(tmp_path) is True
