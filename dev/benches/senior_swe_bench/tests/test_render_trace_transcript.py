import importlib.util
import json
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("render_trace_transcript", ROOT / "render_trace_transcript.py")
renderer = importlib.util.module_from_spec(spec)
sys.modules["render_trace_transcript"] = renderer
spec.loader.exec_module(renderer)  # type: ignore[union-attr]


def test_render_trace_transcript_renders_conversation_not_stream_deltas(tmp_path):
    trace = tmp_path / "vis.trace.jsonl"
    frames = [
        {"event": "trace-chunk", "payload": {"phase": "reasoning", "iteration": 1, "thinking": "First thought"}},
        {"event": "trace-chunk", "payload": {"phase": "assistant-prose", "iteration": 1, "text": "I will inspect it."}},
        {"event": "trace-chunk", "payload": {"phase": "form-start", "iteration": 1, "position": 0, "code": "read_file(...)"}},
        {"event": "trace-chunk", "payload": {"phase": "form-result", "iteration": 1, "position": 0, "result-render": "contents"}},
        {"event": "result", "payload": {"answer": {"answer": "Done."}}},
    ]
    trace.write_text("".join(json.dumps(frame) + "\n" for frame in frames))

    output = renderer.render(trace, "Example")

    assert "Example" in output
    assert "First thought" in output
    assert "I will inspect it." in output
    assert "read_file(...)" in output
    assert "contents" in output
    assert "Done." in output


def test_render_trace_transcript_renders_error_result_vector(tmp_path):
    trace = tmp_path / "vis.trace.jsonl"
    trace.write_text(
        json.dumps(
            {
                "event": "result",
                "payload": {
                    "answer": [
                        "error",
                        {"provider-error-data": {"wrapper-message": "Stream TTFT timeout"}},
                    ]
                },
            }
        )
        + "\n"
    )

    output = renderer.render(trace, "Failed run")

    assert "Stream TTFT timeout" in output
