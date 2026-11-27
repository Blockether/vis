from blockether.vis.evals.task_evidence import tasks


def verify(evidence):
    """Require real attempts and one owned child, not just the right answer."""
    attempts = tasks(evidence.host_evidence, "subagent")
    children = evidence.host_evidence.get("children", {})
    if len(children) != 1 or len(attempts) != 2:
        return ["Expected 2 attempts of one child."]
    if any(end.get("state") != "done" for _, end in attempts):
        return ["A child attempt did not settle successfully."]
    children_runs = evidence.trajectory.subagent_trajectories if evidence.trajectory else ()
    if len(children_runs) != 1:
        return ["Expected one saved child trajectory."]
    reports = [str(step.message or "").strip() for step in children_runs[0].agent_steps]
    expected = ['READY-71', 'RECHECK-56']
    for marker in expected:
        if marker not in reports:
            return ["The saved child trajectory does not give the requested report."]
        reports = reports[reports.index(marker) + 1:]
    return []
