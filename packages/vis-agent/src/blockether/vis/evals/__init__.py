"""Evaluation tools: read Vis runs and Harbor jobs, and compare their results.

Read one Vis run with `load_trajectory` or `parse_trajectory`. Read the trials of a
Harbor job or round with `read_trials`, then count them with `case_results` or
`summarize`. Seal a finished round with `seal_round`. Compare two results with
`compare_cases`, or test paired changes with `paired_change` and `mcnemar_exact`.
Score repeated attempts with `task_attempts` and `repeat_stats`. Score and diagnose a
Terminal-Bench round summary with `bench_report` and `diagnose`. Check automatic
causes or judge stages against human labels with `agreement`. For installation and sessions, use the
[Python SDK guide](https://vis.blockether.com/python-sdk.html).
"""

from .bench import (
    EARLY_STOP_SHARE,
    NEAR_MISS_RATIO,
    BenchReport,
    Diagnosis,
    Finding,
    OutcomeCount,
    TaskNote,
    agent_limits,
    attempt_cause,
    attempt_outcome,
    bench_report,
    cause_check,
    diagnose,
    latest_trials,
    summary_attempts,
    task_note,
)
from .compare import CaseChange, CaseComparison, RouteTest, compare_cases
from .harbor import (
    HarborTrial,
    RunSummary,
    case_results,
    read_trial,
    read_trials,
    seal_digest,
    seal_round,
    seal_rows,
    summarize,
)
from .labels import (
    KAPPA_REVIEW,
    KAPPA_TRUST,
    MIN_LABELS,
    STAGES,
    Agreement,
    agreement,
    kappa_advice,
    majority,
    sample_agreement,
)
from .repeats import RepeatStats, repeat_stats, task_attempts
from .stats import (
    benjamini_hochberg,
    cluster_interval,
    cohen_kappa,
    detectable_change,
    effective_size,
    mcnemar_exact,
    paired_change,
    wilson_interval,
)
from .trajectory import (
    FunctionCall,
    Step,
    StepMetrics,
    ToolCall,
    Trajectory,
    TrajectoryMetrics,
    load_trajectory,
    parse_trajectory,
)

__all__ = [
    "EARLY_STOP_SHARE",
    "KAPPA_REVIEW",
    "KAPPA_TRUST",
    "MIN_LABELS",
    "NEAR_MISS_RATIO",
    "STAGES",
    "Agreement",
    "BenchReport",
    "CaseChange",
    "CaseComparison",
    "Diagnosis",
    "Finding",
    "FunctionCall",
    "HarborTrial",
    "OutcomeCount",
    "RepeatStats",
    "RouteTest",
    "RunSummary",
    "Step",
    "StepMetrics",
    "TaskNote",
    "ToolCall",
    "Trajectory",
    "TrajectoryMetrics",
    "agent_limits",
    "agreement",
    "attempt_cause",
    "attempt_outcome",
    "bench_report",
    "benjamini_hochberg",
    "case_results",
    "cause_check",
    "cluster_interval",
    "cohen_kappa",
    "compare_cases",
    "detectable_change",
    "diagnose",
    "effective_size",
    "kappa_advice",
    "latest_trials",
    "load_trajectory",
    "majority",
    "mcnemar_exact",
    "paired_change",
    "parse_trajectory",
    "read_trial",
    "read_trials",
    "repeat_stats",
    "sample_agreement",
    "seal_digest",
    "seal_round",
    "seal_rows",
    "summarize",
    "summary_attempts",
    "task_attempts",
    "task_note",
    "wilson_interval",
]
