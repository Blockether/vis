from pathlib import Path
import importlib.util
import sys


ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("redact_secrets", ROOT / "redact_secrets.py")
redact_secrets = importlib.util.module_from_spec(spec)
sys.modules["redact_secrets"] = redact_secrets
spec.loader.exec_module(redact_secrets)  # type: ignore[union-attr]


def test_redacts_env_and_edn_secrets_without_reporting_values(tmp_path):
    env = tmp_path / "verifier.env"
    config = tmp_path / "config.edn"
    artifacts = tmp_path / "artifacts"
    trace = artifacts / "trace.jsonl"
    env.write_text("ZAI_API_KEY=zai-secret-value\n")
    config.write_text('{:providers [{:api-key "agent-secret-value"}]}')
    artifacts.mkdir()
    trace.write_text(
        '{"headers":{"x-api-key":"agent-secret-value",'
        '"authorization":"Bearer zai-secret-value"}}\n'
    )

    secrets = redact_secrets.secrets_from_sources([env, config])
    result = redact_secrets.redact([artifacts], secrets)

    assert trace.read_text().count("<redacted>") == 2
    assert "secret-value" not in trace.read_text()
    assert result == {
        "clean": True,
        "changed_files": [str(trace)],
        "files_changed": 1,
        "remaining_occurrences": 0,
        "replacements": 2,
        "scanned_files": 1,
        "secret_values_loaded": 2,
    }
    assert "secret-value" not in str(result)
