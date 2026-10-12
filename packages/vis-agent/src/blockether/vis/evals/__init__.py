"""Evaluation tools: read Vis runs and Harbor jobs, and compare their results.

For the reader of one Vis run, see `blockether.vis.evals.trajectory`. For Harbor jobs
and round seals, see `blockether.vis.evals.harbor`. For the statistics, see
`blockether.vis.evals.stats`.
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
