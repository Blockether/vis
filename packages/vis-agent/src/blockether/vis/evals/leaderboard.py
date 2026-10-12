"""Read a public Harbor Hub leaderboard and place a Vis round on it.

`read_leaderboard` reads every page of one board and gives its rows, best first. With
the `summary.json` data of a round of the same dataset version, it also gives the
standing of that round among the rows. `standing` places a round on rows that you
already have. Percent values go from 0 to 100.
"""

from __future__ import annotations

import json
import re
import statistics
import urllib.request
from collections import Counter
from collections.abc import Callable, Mapping
from dataclasses import dataclass

from .bench import _count, _model, _percent, _percent_interval, summary_attempts
from .stats import cluster_interval

__all__ = [
    "LEADERBOARD_URL",
    "Leaderboard",
    "LeaderboardRow",
    "VisStanding",
    "leaderboard_row",
    "read_leaderboard",
    "standing",
]

LEADERBOARD_URL = "https://api.harborframework.com/functions/v1/leaderboard-read"
"""The Harbor Hub endpoint that gives one page of a leaderboard."""

_MAX_PAGES = 50


@dataclass(frozen=True)
class LeaderboardRow:
    """One published agent and model result.

    `accuracy_percent` is over all trials of the row. Per-trial values divide the row
    totals by `trials`, and `cost_per_success_usd` divides the total cost by
    `successes`. `None` means that the board does not publish the value.
    """

    rank: int
    agent: str
    model: str
    reasoning_effort: str
    accuracy_percent: float
    ci95_half_width: float | None
    trials: int
    successes: int
    total_cost_usd: float | None
    cost_per_trial_usd: float | None
    cost_per_success_usd: float | None
    tokens_per_trial: int | None
    avg_trial_minutes: float | None
    date: str


@dataclass(frozen=True)
class VisStanding:
    """Where the local Vis benchmark result would stand on the board.

    Like the board accuracy, `pass_rate_percent` counts every scored attempt.
    `strict_pass_rate_percent` also counts each unscored, unattempted and GPU task as
    `attempts_per_task` failed attempts. Each `_ci95` value is a 95% interval with the
    attempts of one task as one cluster. The ranks insert the rates among the board
    rows, and each rank range inserts the ends of its interval. Per-attempt values use
    model attempts. `caveats` lists the limits of the comparison.
    """

    model: str
    round_name: str | None
    scored_tasks: int
    total_tasks: int
    attempts_per_task: int
    pass_rate_percent: float
    pass_rate_ci95: tuple[float, float] | None
    strict_pass_rate_percent: float
    strict_pass_rate_ci95: tuple[float, float] | None
    rank_by_pass_rate: int
    rank_range: tuple[int, int] | None
    rank_by_strict_rate: int
    strict_rank_range: tuple[int, int] | None
    cost_per_attempt_usd: float | None
    cost_per_solved_usd: float | None
    tokens_per_attempt: int | None
    avg_attempt_minutes: float | None
    caveats: tuple[str, ...]


@dataclass(frozen=True)
class Leaderboard:
    """A Harbor Hub leaderboard. `vis` is None without a local result for its dataset."""

    package: str
    name: str
    title: str
    rows: tuple[LeaderboardRow, ...]
    vis: VisStanding | None


def _mode(values: list[int]) -> int:
    counts = Counter(values)
    return max(counts, key=lambda value: (counts[value], value)) if counts else 0


def _post_json(url: str, payload: dict) -> dict:
    request = urllib.request.Request(
        url,
        data=json.dumps(payload).encode(),
        headers={"Content-Type": "application/json", "User-Agent": "vis-evals"},
    )
    with urllib.request.urlopen(request, timeout=30) as response:
        return json.load(response)


def _label(value: object) -> str:
    return (
        str(value.get("label") or "") if isinstance(value, dict) else str(value or "")
    )


def _number(value: object) -> float | None:
    return float(value) if isinstance(value, (int, float)) else None


def leaderboard_row(raw: Mapping) -> LeaderboardRow:
    """Read one row of a Harbor Hub leaderboard page."""
    meta = raw.get("metadata") or {}
    metrics = raw.get("metrics") or {}
    trials = int(metrics.get("n_trials") or 0)
    successes = int(metrics.get("successes") or 0)
    cost = _number(metrics.get("total_cost_usd"))
    tokens = _number(metrics.get("total_tokens"))
    duration = _number(metrics.get("avg_trial_duration_sec"))
    return LeaderboardRow(
        rank=int(raw.get("rank") or 0),
        agent=_label(meta.get("agent_display")),
        model=_label(meta.get("model_display")),
        reasoning_effort=str(meta.get("reasoning_effort") or ""),
        accuracy_percent=float(metrics.get("accuracy") or 0.0),
        ci95_half_width=_number(metrics.get("accuracy_ci95_half_width")),
        trials=trials,
        successes=successes,
        total_cost_usd=cost,
        cost_per_trial_usd=round(cost / trials, 2)
        if cost is not None and trials
        else None,
        cost_per_success_usd=round(cost / successes, 2)
        if cost is not None and successes
        else None,
        tokens_per_trial=int(tokens / trials) if tokens and trials else None,
        avg_trial_minutes=round(duration / 60, 1) if duration else None,
        date=str(meta.get("date") or ""),
    )


def _model_key(text: str) -> str:
    return re.sub(r"[^a-z0-9]", "", text.lower())


def _rank(rows: tuple[LeaderboardRow, ...], rate: float) -> int:
    return 1 + sum(row.accuracy_percent > rate for row in rows)


def _rank_range(
    rows: tuple[LeaderboardRow, ...], interval: tuple[float, float] | None
) -> tuple[int, int] | None:
    return (
        None
        if interval is None
        else (_rank(rows, interval[1]), _rank(rows, interval[0]))
    )


def standing(
    rows: tuple[LeaderboardRow, ...],
    data: Mapping,
    package: str,
    name: str,
    round_name: str | None = None,
) -> VisStanding | None:
    """Place the round of `data`, its `summary.json` data, among the board rows.

    Gives None when the summary covers another dataset or version.
    """
    dataset_package, _, version = str(data.get("dataset") or "").partition("@")
    if dataset_package != package or version.replace(".", "-") != name:
        return None
    total = int(data.get("total_dataset_tasks") or 0)
    attempts = summary_attempts(data)
    units = [(sum(flags), len(flags)) for flags in attempts.values()]
    per_task = _mode([len(flags) for flags in attempts.values()]) or 1
    strict_units = units + [(0, per_task)] * max(0, total - len(units))
    solved = sum(passed for passed, _ in units)
    rate = _percent(solved, sum(runs for _, runs in units))
    strict = _percent(solved, sum(runs for _, runs in strict_units))
    interval = _percent_interval(cluster_interval(units))
    strict_interval = _percent_interval(cluster_interval(strict_units))
    usage = data.get("usage_totals") or {}
    model_attempts = int(usage.get("model_attempts") or 0)
    tokens = int(usage.get("input_tokens") or 0) + int(usage.get("output_tokens") or 0)
    cost = _number(usage.get("estimated_metered_api_cost_usd"))
    hours = _number(usage.get("agent_hours"))
    model = _model(data)
    same_model = [
        row
        for row in rows
        if _model_key(row.model) and _model_key(row.model) in _model_key(model)
    ]
    caveats = [
        f"The board has {_count(len(same_model), 'row')} with {model}. Compare Vis "
        "with the same model to see the harness effect."
        if same_model
        else f"No board row uses {model}. Model and harness differ together, so this "
        "is a product-level comparison."
    ]
    if total and rows:
        board = statistics.median(row.trials for row in rows) / total
        if per_task < round(board):
            caveats.append(
                f"Board rows ran about {board:.0f} trials per task. Vis has "
                f"{_count(per_task, 'scored attempt')} per task, so its interval is "
                f"wider. For a matched comparison, run {board:.0f} attempts per task."
            )
    caveats.append(
        "The rank range places the ends of the Vis 95% interval among the board rows."
    )
    caveats.append(
        f"The pass rate counts {len(units)} scored tasks of {total}. The strict rate "
        "counts unscored, unattempted and GPU tasks as failures."
    )
    caveats.append(
        "Vis cost is a metered-API price estimate. A Coding Plan subscription charge "
        "is unknown."
    )
    return VisStanding(
        model=model,
        round_name=round_name,
        scored_tasks=len(units),
        total_tasks=total,
        attempts_per_task=per_task,
        pass_rate_percent=rate,
        pass_rate_ci95=interval,
        strict_pass_rate_percent=strict,
        strict_pass_rate_ci95=strict_interval,
        rank_by_pass_rate=_rank(rows, rate),
        rank_range=_rank_range(rows, interval),
        rank_by_strict_rate=_rank(rows, strict),
        strict_rank_range=_rank_range(rows, strict_interval),
        cost_per_attempt_usd=round(cost / model_attempts, 2)
        if cost is not None and model_attempts
        else None,
        cost_per_solved_usd=round(cost / solved, 2)
        if cost is not None and solved
        else None,
        tokens_per_attempt=tokens // model_attempts if model_attempts else None,
        avg_attempt_minutes=round(hours * 60 / model_attempts, 1)
        if hours is not None and model_attempts
        else None,
        caveats=tuple(caveats),
    )


def read_leaderboard(
    package: str = "terminal-bench/terminal-bench",
    name: str = "4-0-0",
    summary: Mapping | None = None,
    round_name: str | None = None,
    post: Callable[[str, dict], dict] | None = None,
) -> Leaderboard:
    """Read every page of one board, and place the round of `summary` on it.

    Sends one unauthenticated request for each page, at most 50. `post` sends one
    request and gives the parsed reply; the default uses `urllib`. `vis` is None
    without a summary or when the summary covers another dataset version.
    """
    post = post or _post_json
    rows: list[LeaderboardRow] = []
    board: dict = {}
    page = pages = 1
    while page <= min(pages, _MAX_PAGES):
        data = post(
            LEADERBOARD_URL,
            {"package": package, "name": name, "page": page, "page_size": 100},
        )
        board = data.get("leaderboard") or board
        rows.extend(leaderboard_row(raw) for raw in data.get("rows") or ())
        pages = int((data.get("pagination") or {}).get("total_pages") or 1)
        page += 1
    rows.sort(key=lambda row: (row.rank or 10**6, -row.accuracy_percent))
    vis = (
        None
        if summary is None
        else standing(tuple(rows), summary, package, name, round_name)
    )
    return Leaderboard(package, name, str(board.get("title") or name), tuple(rows), vis)
