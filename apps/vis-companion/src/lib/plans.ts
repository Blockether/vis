import contract from '../../../../packages/vis-contract/resources/vis-contract/plans.json';

export type PlanAction = keyof typeof contract.actions;
export interface PlanInfo {
  kind: 'plan' | 'implementation';
  feature: string;
  status: string;
}

export function planName(filename: string): Omit<PlanInfo, 'status'> | null {
  const match = new RegExp(contract.filename_pattern).exec(filename);
  return match
    ? { kind: match[1] === 'PLAN' ? 'plan' : 'implementation', feature: match[2] }
    : null;
}

/** Only the unambiguous document header, never a status quoted inside the spec. */
export function documentInfo(filename: string, text: string): PlanInfo | null {
  const name = planName(filename);
  if (!name) return null;
  const header: string[] = [];
  for (const line of text.split(/\r?\n/).slice(0, 16)) {
    if (line.startsWith('## ')) break;
    header.push(line);
  }
  const features = header.flatMap((line) => /^\*\*Feature:\*\* (\S+)$/.exec(line)?.slice(1) ?? []);
  const statuses = header.flatMap((line) => /^\*\*Status:\*\* (\S+)$/.exec(line)?.slice(1) ?? []);
  if (
    features.length !== 1 ||
    features[0] !== name.feature ||
    statuses.length !== 1 ||
    !contract.statuses.includes(statuses[0])
  )
    return null;
  return { ...name, status: statuses[0] };
}

export function availableActions(info: PlanInfo | null, pendingComments: boolean): PlanAction[] {
  if (!info) return [];
  if (pendingComments) return ['revise'];
  return info.kind === 'plan' && ['ready', 'accepted'].includes(info.status) ? ['approve'] : [];
}

export function actionRequest(filename: string, version: number, action: PlanAction): string {
  if (
    !planName(filename) ||
    !Number.isSafeInteger(version) ||
    version < 1 ||
    !(action in contract.actions)
  ) {
    throw new Error('Invalid plan action or version');
  }
  return `Read \`${filename}\` v${version} with read_attachment(${JSON.stringify(filename)}, version=${version}).\n${contract.actions[action]}`;
}
