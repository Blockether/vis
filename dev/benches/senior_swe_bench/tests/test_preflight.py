from pathlib import Path
import hashlib
import importlib.util
import json
import sys
import tarfile

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("preflight", ROOT / "preflight.py")
preflight = importlib.util.module_from_spec(spec)
sys.modules["preflight"] = preflight
spec.loader.exec_module(preflight)  # type: ignore[union-attr]


def _artifact(
    path: Path,
    native: bytes,
    *,
    mode="native",
    sha=None,
    native_file=None,
    install_mode=0o755,
    native_mode=0o755,
    include_install=True,
):
    if sha is None:
        sha = hashlib.sha256(native).hexdigest()
    if native_file is None:
        native_file = "ELF 64-bit LSB executable, for GNU/Linux"
    metadata = {
        "mode": mode,
        "native_file": native_file,
        "native_sha256": sha,
        "vis_revision": "abc123",
        "source_dirty": True,
        "source_status_count": 4,
    }
    with tarfile.open(path, "w:gz") as tf:
        meta_bytes = json.dumps(metadata).encode()
        meta_info = tarfile.TarInfo("vis-agent/metadata.json")
        meta_info.size = len(meta_bytes)
        tf.addfile(meta_info, fileobj=__import__("io").BytesIO(meta_bytes))

        if include_install:
            install = b"#!/usr/bin/env bash\nexit 0\n"
            install_info = tarfile.TarInfo("vis-agent/install.sh")
            install_info.size = len(install)
            install_info.mode = install_mode
            tf.addfile(install_info, fileobj=__import__("io").BytesIO(install))

        native_info = tarfile.TarInfo("vis-agent/native/vis")
        native_info.size = len(native)
        native_info.mode = native_mode
        tf.addfile(native_info, fileobj=__import__("io").BytesIO(native))


def _elf64_aarch64(*, machine=183):
    header = bytearray(64)
    header[0:4] = b"\x7fELF"
    header[4] = 2  # ELF64
    header[5] = 1  # little endian
    header[6] = 1  # ELF version
    header[16:18] = (2).to_bytes(2, "little")  # executable
    header[18:20] = machine.to_bytes(2, "little")
    return bytes(header) + b"vis-binary"


def test_validate_artifact_accepts_matching_native_sha(tmp_path):
    artifact = tmp_path / "vis-agent.tar.gz"
    native = _elf64_aarch64()
    _artifact(artifact, native)

    result = preflight.validate_artifact(artifact)

    assert result["mode"] == "native"
    assert result["native_sha256"] == hashlib.sha256(native).hexdigest()
    assert result["native_elf"] == {"class": "ELF64", "data": "little", "machine": "aarch64", "machine_id": 183}
    assert result["install_mode"] == "0o755"
    assert result["native_mode"] == "0o755"
    assert result["source_dirty"] is True


def test_pi_reasoning_effort_translation_is_provider_native():
    assert preflight.pi_thinking_level("high") == "high"
    assert preflight.pi_thinking_level("max") == "xhigh"


def test_pi_reasoning_effort_translation_rejects_abstract_or_missing_values():
    for value in ("balanced", "deep", "medium", ""):
        try:
            preflight.pi_thinking_level(value)
        except preflight.PreflightError as exc:
            assert "must be high or max" in str(exc)
        else:
            raise AssertionError(f"expected PreflightError for {value!r}")


def test_pi_glm52_models_config_pins_context_output_and_only_two_efforts(tmp_path):
    path = tmp_path / "models.json"
    config = preflight.write_pi_glm52_models_config(path)
    model = config["providers"]["zai"]["models"][0]

    assert json.loads(path.read_text()) == config
    assert config["providers"]["zai"]["baseUrl"] == "https://api.z.ai/api/coding/paas/v4"
    assert config["providers"]["zai"]["api"] == "openai-completions"
    assert model["id"] == "glm-5.2"
    assert model["contextWindow"] == 1_000_000
    assert model["maxTokens"] == 32_768
    assert model["thinkingLevelMap"] == {
        "off": None,
        "minimal": None,
        "low": None,
        "medium": None,
        "high": "high",
        "xhigh": "max",
    }
    assert model["compat"] == {
        "supportsDeveloperRole": False,
        "supportsReasoningEffort": True,
        "thinkingFormat": "deepseek",
        "zaiToolStream": True,
    }
    assert hashlib.sha256(path.read_bytes()).hexdigest() == (
        "c8663c7d2287c51cea995b3e5c47a2c7f94dce8dc1e629ee5c89ae6682c673a9"
    )


def test_validate_artifact_rejects_sha_mismatch(tmp_path):
    artifact = tmp_path / "vis-agent.tar.gz"
    _artifact(artifact, _elf64_aarch64(), sha="bad")

    try:
        preflight.validate_artifact(artifact)
    except preflight.PreflightError as exc:
        assert "native_sha256" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_validate_artifact_rejects_metadata_lie_about_elf(tmp_path):
    artifact = tmp_path / "vis-agent.tar.gz"
    _artifact(artifact, b"not an elf", native_file="ELF 64-bit LSB executable, for GNU/Linux")

    try:
        preflight.validate_artifact(artifact)
    except preflight.PreflightError as exc:
        assert "not an ELF binary" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_validate_artifact_rejects_wrong_elf_machine(tmp_path):
    artifact = tmp_path / "vis-agent.tar.gz"
    _artifact(artifact, _elf64_aarch64(machine=62))

    try:
        preflight.validate_artifact(artifact)
    except preflight.PreflightError as exc:
        assert "must be Linux ARM64/aarch64 ELF" in str(exc)
        assert "x86_64" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_validate_artifact_rejects_missing_install_script(tmp_path):
    artifact = tmp_path / "vis-agent.tar.gz"
    _artifact(artifact, _elf64_aarch64(), include_install=False)

    try:
        preflight.validate_artifact(artifact)
    except preflight.PreflightError as exc:
        assert "install.sh" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_validate_artifact_rejects_non_executable_install_script(tmp_path):
    artifact = tmp_path / "vis-agent.tar.gz"
    _artifact(artifact, _elf64_aarch64(), install_mode=0o644)

    try:
        preflight.validate_artifact(artifact)
    except preflight.PreflightError as exc:
        assert "install.sh must be executable" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_validate_artifact_rejects_non_executable_native_binary(tmp_path):
    artifact = tmp_path / "vis-agent.tar.gz"
    _artifact(artifact, _elf64_aarch64(), native_mode=0o644)

    try:
        preflight.validate_artifact(artifact)
    except preflight.PreflightError as exc:
        assert "native/vis must be executable" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_validate_config_accepts_provider_vector_without_printing_secrets(tmp_path):
    config = tmp_path / "config.edn"
    config.write_text(
        """
        ; comments are accepted
        {:providers [{:id :openai
                      :api-key "secret-value"
                      :base-url "https://api.openai.com/v1"
                      :models [{:name "gpt-4o-mini"}]}]
         :toggles {:x true}}
        """
    )

    result = preflight.validate_config(config)

    assert result == {
        "path": str(config),
        "providers": 1,
        "provider_ids": ["openai"],
        "provider_configs": [{"id": "openai", "base_url": "https://api.openai.com/v1"}],
    }
    assert "secret-value" not in str(result)


def test_validate_config_accepts_edn_sets_and_tagged_literals_outside_providers(tmp_path):
    config = tmp_path / "config.edn"
    config.write_text(
        """
        {:providers [{:id :zai :api-key "secret-value"}]
         :disabled-tools #{}
         :metadata {:generated-at #inst "2026-07-09T00:00:00.000-00:00"}}
        """
    )

    result = preflight.validate_config(config)

    assert result == {
        "path": str(config),
        "providers": 1,
        "provider_ids": ["zai"],
        "provider_configs": [{"id": "zai", "base_url": None}],
    }

def test_validate_config_rejects_accidentally_nested_top_level_keys_in_providers(tmp_path):
    config = tmp_path / "config.edn"
    config.write_text('{:providers [{:id :openai} :toggles {:x true}]}')

    try:
        preflight.validate_config(config)
    except preflight.PreflightError as exc:
        assert ":providers entry 1 must be a map" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_resolve_python_dev_image_auto_enables_for_known_full_eval_task():
    assert preflight.resolve_python_dev_image_mode("paperless-ngx-perf-document-counts", "auto", "", False) == "1"


def test_resolve_python_dev_image_auto_skips_install_only():
    assert preflight.resolve_python_dev_image_mode("paperless-ngx-perf-document-counts", "auto", "", True) == "0"


def test_resolve_python_dev_image_rejects_known_task_when_disabled_without_override():
    try:
        preflight.resolve_python_dev_image_mode("paperless-ngx-perf-document-counts", "0", "", False)
    except preflight.PreflightError as exc:
        assert "requires a python-dev verifier image" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_resolve_python_dev_image_allows_prebuilt_override_when_disabled():
    assert (
        preflight.resolve_python_dev_image_mode(
            "paperless-ngx-perf-document-counts",
            "0",
            "vis-senior-swe-bench/paperless-ngx-perf-document-counts:python-dev",
            False,
        )
        == "0"
    )


def test_resolve_python_dev_image_rejects_invalid_mode():
    try:
        preflight.resolve_python_dev_image_mode("task", "sometimes", "", False)
    except preflight.PreflightError as exc:
        assert "must be auto, 0, or 1" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_resolve_config_delivery_prefers_upload_over_mounted_home(tmp_path):
    home = tmp_path / "home"
    (home / ".vis").mkdir(parents=True)
    (home / ".vis" / "config.edn").write_text("{:providers []}")

    assert preflight.resolve_config_delivery("/tmp/config.edn", home) == "upload"


def test_resolve_config_delivery_detects_mounted_home_config(tmp_path):
    home = tmp_path / "home"
    (home / ".vis").mkdir(parents=True)
    (home / ".vis" / "config.edn").write_text("{:providers []}")

    assert preflight.resolve_config_delivery("", home) == "mounted-home"


def test_resolve_config_delivery_none_without_config(tmp_path):
    home = tmp_path / "home"
    home.mkdir()

    assert preflight.resolve_config_delivery("", home) == "none"
    assert preflight.resolve_config_delivery("", "") == "none"


def test_validate_config_mount_combination_rejects_upload_plus_mount(tmp_path):
    home = tmp_path / "home"
    home.mkdir()

    try:
        preflight.validate_config_mount_combination("/tmp/config.edn", home)
    except preflight.PreflightError as exc:
        assert "VIS_BENCH_CONFIG and VIS_BENCH_REMOTE_HOME_MOUNT are both set" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_validate_config_mount_combination_allows_explicit_override(tmp_path):
    home = tmp_path / "home"
    home.mkdir()

    assert (
        preflight.validate_config_mount_combination(
            "/tmp/config.edn",
            home,
            allow_config_with_mount=True,
        )
        is None
    )


def test_validate_config_mount_combination_allows_single_delivery_mode(tmp_path):
    home = tmp_path / "home"
    home.mkdir()

    assert preflight.validate_config_mount_combination("/tmp/config.edn", "") is None
    assert preflight.validate_config_mount_combination("", home) is None


def test_build_remote_home_mounts_rw(tmp_path):
    home = tmp_path / "home"
    home.mkdir()

    assert preflight.build_remote_home_mounts(home, "/root", "rw") == [
        {"type": "bind", "source": str(home), "target": "/root"}
    ]


def test_build_remote_home_mounts_rejects_ro(tmp_path):
    home = tmp_path / "home"
    home.mkdir()

    try:
        preflight.build_remote_home_mounts(home, "/root", "ro")
    except preflight.PreflightError as exc:
        assert "Vis writes ~/.vis/vis.log" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_build_remote_home_mounts_rejects_missing_source(tmp_path):
    try:
        preflight.build_remote_home_mounts(tmp_path / "missing", "/root", "rw")
    except preflight.PreflightError as exc:
        assert "must be a directory" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_build_remote_home_mounts_rejects_invalid_mode(tmp_path):
    home = tmp_path / "home"
    home.mkdir()

    try:
        preflight.build_remote_home_mounts(home, "/root", "maybe")
    except preflight.PreflightError as exc:
        assert "must be 'ro' or 'rw'" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_validate_credential_source_allows_install_only_without_config():
    assert preflight.validate_credential_source(None, required=False) == {
        "required": False,
        "config_path": None,
        "allow_missing": False,
    }


def test_validate_credential_source_requires_config_for_full_eval():
    try:
        preflight.validate_credential_source(None, required=True)
    except preflight.PreflightError as exc:
        assert "full eval requires provider credentials" in str(exc)
        assert "VIS_BENCH_CONFIG" in str(exc)
        assert "VIS_BENCH_REMOTE_HOME_MOUNT" in str(exc)
    else:
        raise AssertionError("expected PreflightError")


def test_validate_credential_source_allows_explicit_missing_config_override():
    result = preflight.validate_credential_source(None, required=True, allow_missing=True)

    assert result["required"] is True
    assert result["config_path"] is None
    assert result["allow_missing"] is True
    assert "warning" in result


def test_validate_credential_source_records_config_path(tmp_path):
    config = tmp_path / "config.edn"

    assert preflight.validate_credential_source(config, required=True) == {
        "required": True,
        "config_path": str(config),
        "allow_missing": False,
    }


def test_validate_config_rejects_odd_top_level_map(tmp_path):
    config = tmp_path / "config.edn"
    config.write_text('{:providers [{:id :openai}] :toggles}')

    try:
        preflight.validate_config(config)
    except preflight.PreflightError as exc:
        assert "odd number" in str(exc)
    else:
        raise AssertionError("expected PreflightError")
