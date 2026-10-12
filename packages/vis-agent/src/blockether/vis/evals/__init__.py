"""Evaluation tools: read Vis runs and Harbor jobs, and compare their results.

Read one Vis run with `load_trajectory` or `parse_trajectory`. Read the trials of a
Harbor job or round with `read_trials`, then count them with `case_results` or
`summarize`. Seal a finished round with `seal_round`. Compare paired results with
`paired_change` and `mcnemar_exact`. For installation and sessions, use the
[Python SDK guide](https://vis.blockether.com/python-sdk.html).
"""

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
    "FunctionCall",
    "HarborTrial",
    "RunSummary",
    "Step",
    "StepMetrics",
    "ToolCall",
    "Trajectory",
    "TrajectoryMetrics",
    "benjamini_hochberg",
    "case_results",
    "cluster_interval",
    "cohen_kappa",
    "detectable_change",
    "effective_size",
    "load_trajectory",
    "mcnemar_exact",
    "paired_change",
    "parse_trajectory",
    "read_trial",
    "read_trials",
    "seal_digest",
    "seal_round",
    "seal_rows",
    "summarize",
    "wilson_interval",
]
