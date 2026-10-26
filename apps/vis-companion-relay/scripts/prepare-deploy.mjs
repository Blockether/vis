/** Build a deployment config that keeps existing Worker variables. */
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parse } from "jsonc-parser";

const root = fileURLToPath(new URL("../", import.meta.url));

export function deploymentConfig(base, overrides = {}) {
  const vars = {};
  for (const [name, value] of Object.entries(base.vars ?? {})) {
    const selected = overrides[name] || value;
    if (selected !== "") vars[name] = selected;
  }
  return {
    ...base,
    main: resolve(root, base.main),
    keep_vars: true,
    vars,
  };
}

export async function writeDeploymentConfig(destination, env = process.env) {
  const errors = [];
  const base = parse(
    await readFile(resolve(root, "wrangler.jsonc"), "utf8"),
    errors,
  );
  if (errors.length) throw new Error("Invalid relay configuration");
  const config = deploymentConfig(base, env);
  await mkdir(dirname(resolve(destination)), { recursive: true, mode: 0o700 });
  await writeFile(destination, JSON.stringify(config, null, 2) + "\n", {
    mode: 0o600,
  });
}

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  if (!process.argv[2])
    throw new Error("Pass the private deployment config path");
  await writeDeploymentConfig(process.argv[2]);
}
