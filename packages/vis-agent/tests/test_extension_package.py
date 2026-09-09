"""GitHub and local project admission share one inert manifest contract."""

import subprocess

import pytest
from blockether.vis import extension_package as package

MANIFEST = """[project]
name = "vis-greeter"
version = "1.0.0"
description = "Small greeting tools."
requires-python = ">=3.11"
dependencies = ["vis-agent>=0.1.0"]
[tool.vis]
category = "tools"
source_paths = ["src"]
"""
REPOSITORY = "https://github.com/example/extensions"


def project(directory, manifest=MANIFEST):
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "pyproject.toml").write_text(manifest)
    (directory / "extension.py").write_text("raise RuntimeError('not executed')")
    (directory / "src").mkdir()
    (directory / "src/greeter.py").write_text("VALUE = 1")
    return directory


@pytest.mark.parametrize("suffix", ["", "/", ".git"])
def test_normalize_github_repository(suffix):
    assert package.github_repository(REPOSITORY + suffix) == REPOSITORY


@pytest.mark.parametrize(
    "url",
    [
        "http://github.com/example/extensions",
        "https://github.com.evil.test/a/b",
        "https://user@github.com/a/b",
        "https://github.com:443/a/b",
        "https://127.0.0.1/a/b",
        "https://github.com/a/b/tree/main/src",
        "https://github.com/a/b?url=elsewhere",
        "git@github.com:a/b",
        "https://github.com/a/..",
        "https://github.com/a/%2e%2e",
    ],
)
def test_only_public_github_repository_urls_are_accepted(url):
    with pytest.raises(ValueError, match="GitHub"):
        package.github_repository(url)


@pytest.mark.parametrize(
    "directory",
    ["../outside", "/absolute", "src/../other", "a\\b", ".git", "src//tools"],
)
def test_subdirectory_cannot_escape_the_repository(directory):
    with pytest.raises(ValueError):
        package.project_subdirectory(directory)


def test_manifest_inspection_never_imports_code(tmp_path):
    metadata = package.inspect_source(project(tmp_path / "source"))
    assert metadata["name"] == "vis-greeter"
    assert metadata["category"] == "tools"


@pytest.mark.parametrize(
    "manifest",
    [
        MANIFEST.replace('"vis-agent>=0.1.0"', '"httpx"'),
        MANIFEST.replace('category = "tools"', 'category = "unknown"'),
        MANIFEST.replace('version = "1.0.0"', 'version = "invalid"'),
        MANIFEST.replace('source_paths = ["src"]', 'source_paths = ["../outside"]'),
    ],
)
def test_invalid_metadata_is_rejected(manifest):
    with pytest.raises(ValueError):
        package.manifest_metadata(manifest)


def test_local_subdirectory_links_source_and_never_overwrites(tmp_path):
    repository = tmp_path / "source"
    source = project(repository / "plugins/greeting")
    target = tmp_path / "installed"
    with pytest.raises(ValueError, match="trust"):
        package.install(str(repository), target, subdirectory="plugins/greeting")
    assert not target.exists()
    result = package.install(
        str(repository), target, trust=True, subdirectory="plugins/greeting"
    )
    assert result["mode"] == "source"
    assert (target / "vis-greeter").is_symlink()
    (source / "extension.py").write_text("VALUE = 2")
    assert (target / "vis-greeter/extension.py").read_text() == "VALUE = 2"
    with pytest.raises(FileExistsError):
        package.install(str(source / "pyproject.toml"), target, trust=True)


def test_incompatible_vis_and_escaping_entry_are_rejected(tmp_path):
    source = project(
        tmp_path / "source", MANIFEST.replace("vis-agent>=0.1.0", "vis-agent>=9999")
    )
    with pytest.raises(ValueError, match="Vis"):
        package.install(
            str(source), tmp_path / "installed", trust=True, vis_version="0.1.45"
        )
    (source / "extension.py").unlink()
    (source / "extension.py").symlink_to(tmp_path / "outside.py")
    (tmp_path / "outside.py").write_text("VALUE = 1")
    with pytest.raises(ValueError, match="inside"):
        package.inspect_source(source)


def test_removed_archive_and_registry_inputs_are_not_supported(tmp_path):
    source = tmp_path / "old.zip"
    source.write_bytes(b"not a project")
    for value in [str(source), "vis-greeter@1.0.0"]:
        with pytest.raises(ValueError, match="GitHub|directory"):
            package.install(value, tmp_path / "installed", trust=True)
    assert not hasattr(package, "inspect_archive")
    assert not hasattr(package, "registry_base")


def test_real_git_checkout_is_pinned_and_installs_only_selected_project(
    tmp_path, monkeypatch
):
    repository = tmp_path / "repository"
    project(repository / "plugins/greeting")
    (repository / "unrelated.txt").write_text("not installed")

    def git(*args):
        return subprocess.run(
            ["git", "-C", str(repository), *args],
            check=True,
            capture_output=True,
            text=True,
        ).stdout.strip()

    git("init")
    git("add", ".")
    git(
        "-c",
        "user.name=Test Author",
        "-c",
        "user.email=test@example.com",
        "commit",
        "-m",
        "fixture",
    )
    revision = git("rev-parse", "HEAD")
    (repository / "plugins/greeting/src/greeter.py").write_text("VALUE = 2")
    git("add", ".")
    git(
        "-c",
        "user.name=Test Author",
        "-c",
        "user.email=test@example.com",
        "commit",
        "-m",
        "second fixture",
    )
    original = subprocess.run
    commands = []

    def local_transport(args, **kwargs):
        commands.append(args)
        args = [
            str(repository)
            if arg == REPOSITORY
            else "protocol.file.allow=always"
            if arg == "protocol.file.allow=never"
            else arg
            for arg in args
        ]
        return original(args, **kwargs)

    monkeypatch.setattr(package.subprocess, "run", local_transport)
    target = tmp_path / "installed"
    result = package.install(
        REPOSITORY,
        target,
        trust=True,
        subdirectory="plugins/greeting",
        revision=revision,
    )
    assert result["mode"] == "github"
    assert result["revision"] == revision
    assert (target / "vis-greeter/src/greeter.py").read_text() == "VALUE = 1"
    assert not (target / "vis-greeter/.git").exists()
    assert not (target / "vis-greeter/unrelated.txt").exists()
    assert commands
    assert sorted(p.name for p in target.iterdir()) == ["vis-greeter"]


def test_remote_failure_and_invalid_revision_leave_no_installation(
    tmp_path, monkeypatch
):
    target = tmp_path / "installed"

    def fail(*args):
        raise ValueError("Could not fetch repository")

    monkeypatch.setattr(package, "_checkout", fail)
    with pytest.raises(ValueError, match="fetch"):
        package.install(REPOSITORY, target, trust=True)
    assert not target.exists() or not list(target.iterdir())
    with pytest.raises(ValueError, match="revision"):
        package.install(REPOSITORY, target, trust=True, revision="--upload-pack=bad")


@pytest.mark.parametrize("skills", ['["skills/greeting"]', "[]"])
def test_manifest_declares_skills_without_executing_code(tmp_path, skills):
    source = project(tmp_path / "source", MANIFEST + f"skills = {skills}\n")
    skill = source / "skills/greeting"
    skill.mkdir(parents=True)
    (skill / "SKILL.md").write_text(
        "---\nname: greeting\ndescription: Greet a person.\n---\nRead the tool contract."
    )
    metadata = package.inspect_source(source)
    assert metadata["skills"] == (["skills/greeting"] if skills != "[]" else [])


@pytest.mark.parametrize(
    "skills",
    [
        '"skills/greeting"',
        '["../outside"]',
        '["skills/a", "skills/a"]',
        '["/absolute"]',
    ],
)
def test_invalid_skill_declarations_are_rejected(skills):
    with pytest.raises(ValueError, match="skills|path"):
        package.manifest_metadata(MANIFEST + f"skills = {skills}\n")


def test_skill_paths_and_resources_stay_inside_package(tmp_path):
    source = project(tmp_path / "source", MANIFEST + 'skills = ["skills/greeting"]\n')
    with pytest.raises(ValueError, match="SKILL.md"):
        package.inspect_source(source)
    skill = source / "skills/greeting"
    skill.mkdir(parents=True)
    (skill / "SKILL.md").write_text("Greet a person.")
    (tmp_path / "private.txt").write_text("must not be read")
    (skill / "reference.txt").symlink_to(tmp_path / "private.txt")
    with pytest.raises(ValueError, match="inside"):
        package.inspect_source(source)
