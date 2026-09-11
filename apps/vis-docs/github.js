import { parse } from 'smol-toml';

export class RequestError extends Error {
  constructor(message, status = 400) {
    super(message);
    this.status = status;
  }
}
const assert = (condition, message) => {
  if (!condition) throw new RequestError(message);
};
export function repositoryURL(value) {
  assert(typeof value === 'string', 'Enter a public GitHub repository URL.');
  const normalized = value.replace(/\/+$/, '').replace(/\.git$/, '');
  assert(
    /^https:\/\/github\.com\/[A-Za-z0-9][A-Za-z0-9-]{0,38}\/[A-Za-z0-9_.-]{1,100}$/.test(
      normalized,
    ) && !['.', '..'].includes(normalized.split('/').at(-1)),
    'Use https://github.com/owner/repository; select the project folder separately.',
  );
  return normalized;
}
export function projectFolder(value = '') {
  if (value === '' || value === '.') return '';
  assert(
    typeof value === 'string' &&
      value.length <= 512 &&
      !/[\\:]/.test(value) &&
      [...value].every((c) => c.charCodeAt(0) >= 32 && c.charCodeAt(0) !== 127),
    'Use a portable relative project folder.',
  );
  assert(
    value
      .split('/')
      .every(
        (part) =>
          part &&
          ![
            '.',
            '..',
            '.git',
            '.venv',
            'venv',
            '__pycache__',
            'node_modules',
            '.DS_Store',
          ].includes(part) &&
          !part.startsWith('.env'),
      ),
    'Project folder contains an unsafe or excluded path.',
  );
  return value;
}
export async function identity(text) {
  return [...new Uint8Array(await crypto.subtle.digest('SHA-256', new TextEncoder().encode(text)))]
    .map((b) => b.toString(16).padStart(2, '0'))
    .join('')
    .slice(0, 24);
}
export async function readBounded(response, limit) {
  if (Number(response.headers.get('content-length')) > limit)
    throw new RequestError('Metadata or request is too large.', 413);
  if (!response.body) return '';
  const reader = response.body.getReader(),
    parts = [];
  let size = 0;
  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      size += value.byteLength;
      if (size > limit) {
        await reader.cancel();
        throw new RequestError('Metadata or request is too large.', 413);
      }
      parts.push(value);
    }
  } finally {
    reader.releaseLock();
  }
  const bytes = new Uint8Array(size);
  let offset = 0;
  for (const part of parts) {
    bytes.set(part, offset);
    offset += part.length;
  }
  try {
    return new TextDecoder('utf-8', { fatal: true }).decode(bytes);
  } catch {
    throw new RequestError('Metadata must be UTF-8.');
  }
}
// The catalog checks portable display metadata; the SDK remains authoritative for PEP 440/508 and runtime compatibility.
export function manifestMetadata(text) {
  assert(new TextEncoder().encode(text).length <= 128 * 1024, 'pyproject.toml exceeds 128 KiB.');
  let data;
  try {
    data = parse(text);
  } catch {
    throw new RequestError('pyproject.toml is not valid TOML.');
  }
  const p = data.project,
    v = data.tool?.vis;
  assert(
    p &&
      v &&
      typeof p.name === 'string' &&
      /^[A-Za-z0-9](?:[A-Za-z0-9._-]{0,98}[A-Za-z0-9])?$/.test(p.name),
    'Declare a valid project name and tool.vis table.',
  );
  assert(
    typeof p.version === 'string' && p.version.length > 0 && p.version.length <= 80,
    'Declare a static project version.',
  );
  assert(
    typeof p.description === 'string' && p.description.length > 0 && p.description.length <= 240,
    'Description must contain 1–240 characters.',
  );
  assert(
    typeof p['requires-python'] === 'string' &&
      p['requires-python'].length > 0 &&
      p['requires-python'].length <= 1024,
    'Declare requires-python.',
  );
  assert(
    Array.isArray(p.dependencies) &&
      p.dependencies.length <= 128 &&
      p.dependencies.every((d) => typeof d === 'string' && d.length > 0 && d.length <= 2048),
    'Declare a list of at most 128 dependencies.',
  );
  const sdk = p.dependencies.filter((d) => /^vis[-_.]agent(?:\s|[<>=!~[(]|$)/i.test(d));
  assert(
    sdk.length > 0 && sdk.every((d) => !/[;@[\]]/.test(d)),
    'Declare an unconditional vis-agent version requirement.',
  );
  assert(
    ['tools', 'providers', 'workflows'].includes(v.category),
    'category must be tools, providers or workflows.',
  );
  assert(
    Object.keys(v).every((key) => ['category', 'source_paths', 'skills'].includes(key)),
    'tool.vis accepts category, source_paths and skills.',
  );
  const paths = v.source_paths ?? [],
    skills = v.skills ?? [];
  assert(
    Array.isArray(paths) && paths.length <= 16,
    'source_paths must contain at most 16 relative directories.',
  );
  for (const path of paths)
    assert(
      projectFolder(path) && path !== '.',
      'source_paths must name directories inside the project.',
    );
  assert(
    Array.isArray(skills) && skills.length <= 64,
    'skills must contain at most 64 relative directories.',
  );
  for (const path of skills)
    assert(projectFolder(path) && path !== '.', 'skills must name directories inside the project.');
  assert(new Set(skills).size === skills.length, 'skills must not repeat a directory.');
  return {
    name: p.name.toLowerCase().replace(/[-_.]+/g, '-'),
    version: p.version,
    description: p.description,
    category: v.category,
    requires_python: p['requires-python'],
    dependencies: p.dependencies,
    source_paths: paths,
    skills,
  };
}
// Catalog releases use a canonical, sortable subset of PEP 440; the SDK checks runtime requirements.
export function releaseVersion(version) {
  const match =
    typeof version === 'string' &&
    version.match(
      /^(0|[1-9][0-9]{0,8})\.(0|[1-9][0-9]{0,8})\.(0|[1-9][0-9]{0,8})(?:(a|b|rc)(0|[1-9][0-9]{0,8}))?$/,
    );
  assert(
    match,
    'Release versions must be MAJOR.MINOR.PATCH, optionally followed by aN, bN or rcN.',
  );
  return {
    version_key: [...match.slice(1, 4), { a: 0, b: 1, rc: 2 }[match[4]] ?? 3, match[5] || 0]
      .map((value) => String(value).padStart(9, '0'))
      .join('.'),
    prerelease: !!match[4],
  };
}
export function githubClient(env) {
  const signal = AbortSignal.timeout(20000);
  return async (path) => {
    const headers = {
      Accept: 'application/vnd.github+json',
      'User-Agent': 'Vis-Extension-Center',
      'X-GitHub-Api-Version': '2022-11-28',
    };
    if (env.GITHUB_TOKEN) headers.Authorization = 'Bearer ' + env.GITHUB_TOKEN;
    const response = await fetch('https://api.github.com' + path, {
      headers,
      redirect: 'manual',
      signal,
    });
    if (response.status >= 300 && response.status < 400)
      throw new RequestError('GitHub redirected this repository. Submit its current URL.');
    if (response.status === 404)
      throw new RequestError(
        'Public repository, published release, revision or project folder not found on GitHub. Publish a GitHub Release first.',
      );
    if (!response.ok)
      throw new RequestError('GitHub is unavailable or rate-limited. Try again later.', 503);
    return JSON.parse(await readBounded(response, 1024 * 1024));
  };
}
export async function inspectRepository(source, env) {
  const submittedURL = repositoryURL(source.repository_url),
    subdirectory = projectFolder(source.subdirectory);
  assert(
    source.revision === undefined || /^[0-9a-f]{40}$/.test(source.revision),
    'revision must be a full Git commit SHA.',
  );
  const submittedRepository = submittedURL.slice('https://github.com/'.length),
    api = '/repos/' + submittedRepository;
  assert(
    source.release_tag === undefined ||
      (typeof source.release_tag === 'string' &&
        /^[A-Za-z0-9][A-Za-z0-9._/-]{0,199}$/.test(source.release_tag)),
    'Use a GitHub release tag, such as v1.2.0.',
  );
  const github = githubClient(env);
  const repo = await github(api);
  assert(repo.private === false, 'Only public GitHub repositories can be listed.');
  const repository = repo.full_name,
    owner = repo.owner?.login;
  assert(
    typeof owner === 'string' &&
      typeof repo.name === 'string' &&
      repository === owner + '/' + repo.name &&
      repository.toLowerCase() === submittedRepository.toLowerCase(),
    'GitHub repository owner or name could not be verified. Submit its current URL.',
  );
  const repository_url = repositoryURL('https://github.com/' + repository);
  const release = await github(
    api +
      (source.release_tag
        ? '/releases/tags/' + encodeURIComponent(source.release_tag)
        : '/releases/latest'),
  );
  assert(
    release.draft === false &&
      typeof release.prerelease === 'boolean' &&
      typeof release.tag_name === 'string' &&
      release.tag_name.length <= 200 &&
      !Number.isNaN(Date.parse(release.published_at)),
    'Choose a published GitHub Release, not a draft or an unpublished tag.',
  );
  assert(
    !source.release_tag || release.tag_name === source.release_tag,
    'GitHub returned a different release tag.',
  );
  let object = (await github(api + '/git/ref/tags/' + encodeURIComponent(release.tag_name))).object;
  for (let depth = 0; object?.type === 'tag' && depth < 4; depth++) {
    assert(/^[0-9a-f]{40}$/.test(object.sha), 'Invalid annotated tag object.');
    object = (await github(api + '/git/tags/' + object.sha)).object;
  }
  assert(
    object?.type === 'commit' && /^[0-9a-f]{40}$/.test(object.sha),
    'Release tag must resolve to a commit.',
  );
  assert(
    !source.revision || source.revision === object.sha,
    'Release tag moved since review. Publish a new version instead.',
  );
  const commit = await github(api + '/commits/' + object.sha),
    sha = commit.sha;
  assert(sha === object.sha, 'GitHub returned a different revision.');
  const encoded = (path) => path.split('/').map(encodeURIComponent).join('/');
  const contents = (path) =>
    github(api + '/contents' + (path ? '/' + encoded(path) : '') + '?ref=' + sha);
  const entries = await contents(subdirectory);
  assert(Array.isArray(entries), 'Project folder must be a directory.');
  const regular = (name) =>
    entries.find((e) => e.name === name && e.type === 'file' && !e.submodule_git_url);
  assert(
    regular('pyproject.toml') && regular('extension.py'),
    'pyproject.toml and extension.py must be files in the selected folder; set Project folder for a monorepo.',
  );
  const prefix = subdirectory ? subdirectory + '/' : '';
  async function textFile(path) {
    const file = await contents(prefix + path);
    assert(
      file.type === 'file' &&
        !file.submodule_git_url &&
        file.encoding === 'base64' &&
        typeof file.content === 'string',
      path + ' must be a regular UTF-8 file.',
    );
    assert(file.size <= 128 * 1024 && file.content.length <= 180000, path + ' exceeds 128 KiB.');
    let bytes;
    try {
      bytes = Uint8Array.from(atob(file.content.replace(/\s/g, '')), (c) => c.charCodeAt(0));
    } catch {
      throw new RequestError(path + ' must be valid base64 UTF-8.');
    }
    assert(bytes.length <= 128 * 1024, path + ' exceeds 128 KiB.');
    try {
      return new TextDecoder('utf-8', { fatal: true }).decode(bytes);
    } catch {
      throw new RequestError(path + ' must be UTF-8.');
    }
  }
  const metadata = manifestMetadata(await textFile('pyproject.toml'));
  const version = releaseVersion(metadata.version);
  assert(
    ['v' + metadata.version, metadata.name + '/v' + metadata.version].includes(release.tag_name),
    'Release tag must match project.version: vVERSION or PACKAGE-NAME/vVERSION.',
  );
  Object.assign(metadata, version, {
    release_tag: release.tag_name,
    release_url: repository_url + '/releases/tag/' + encodeURIComponent(release.tag_name),
    release_published_at: new Date(release.published_at).toISOString(),
    prerelease: release.prerelease || version.prerelease,
  });
  for (const path of metadata.source_paths)
    assert(
      Array.isArray(await contents(prefix + path)),
      'source_paths must be directories inside the selected project.',
    );
  for (const path of metadata.skills) {
    const files = await contents(prefix + path);
    assert(
      Array.isArray(files) &&
        files.some(
          (entry) => entry.name === 'SKILL.md' && entry.type === 'file' && !entry.submodule_git_url,
        ),
      'Each skills directory must contain a SKILL.md file.',
    );
  }
  const readme = entries.find(
    (e) => /^readme\.(md|rst|txt)$/i.test(e.name) && e.type === 'file' && !e.submodule_git_url,
  );
  const readme_content = readme ? await textFile(readme.name) : null;
  return {
    ...metadata,
    id: await identity(repository_url.toLowerCase() + '\n' + subdirectory),
    repository_url,
    repository,
    owner,
    subdirectory,
    revision: sha,
    source_url: repository_url + '/tree/' + sha + (subdirectory ? '/' + encoded(subdirectory) : ''),
    manifest_url: repository_url + '/blob/' + sha + '/' + encoded(prefix + 'pyproject.toml'),
    readme_url: readme
      ? repository_url + '/blob/' + sha + '/' + encoded(prefix + readme.name)
      : null,
    readme: readme_content,
    readme_format: readme?.name.split('.').at(-1).toLowerCase() || null,
    stars: Number.isSafeInteger(repo.stargazers_count) ? Math.max(0, repo.stargazers_count) : 0,
    topics: Array.isArray(repo.topics)
      ? repo.topics.filter((t) => typeof t === 'string').slice(0, 20)
      : [],
    license: repo.license?.spdx_id || null,
    archived: !!repo.archived,
    updated_at: commit.commit.committer.date,
  };
}
