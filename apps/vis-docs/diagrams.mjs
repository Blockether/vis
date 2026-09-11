import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const assets = new URL('../../resources/vis-docs/assets/', import.meta.url);
const font = await readFile(new URL('fonts/jetbrains-mono.woff2', assets));
const temporary = await mkdtemp(join(tmpdir(), 'vis-council-diagrams-'));
try {
  const css = join(temporary, 'font.css');
  await writeFile(
    css,
    `@font-face { font-family: 'JetBrains Mono'; src: url(data:font/woff2;base64,${font.toString('base64')}) format('woff2'); }`,
  );
  for (const name of ['council-messages', 'council-modules']) {
    const input = new URL(`diagrams/${name}.mmd`, assets);
    const output = new URL(`diagrams/${name}.svg`, assets);
    execFileSync(
      'npm',
      [
        'exec',
        '--yes',
        '--package=@mermaid-js/mermaid-cli@11.17.0',
        '--',
        'mmdc',
        '-C',
        css,
        '-i',
        fileURLToPath(input),
        '-o',
        fileURLToPath(output),
      ],
      { stdio: 'inherit' },
    );
    const hash = createHash('sha256')
      .update(await readFile(input))
      .digest('hex');
    const svg = await readFile(output, 'utf8');
    await writeFile(output, svg.replace('<svg ', `<svg data-source-sha256="${hash}" `));
  }
} finally {
  await rm(temporary, { recursive: true, force: true });
}
