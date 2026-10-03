/** Build a deployment config without storing private database IDs in the checkout. */
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parse } from "jsonc-parser";

const root = fileURLToPath(new URL("../", import.meta.url));

export function deploymentConfig(
  base,
  databaseId,
  databaseName = "vis-council-rooms",
  overrides = {},
) {
  if (
    !/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(
      databaseId ?? "",
    )
  ) {
    throw new Error("ROOMS_DATABASE_ID must be a D1 database UUID");
  }
  if (!/^[a-zA-Z0-9_-]{1,64}$/.test(databaseName)) {
    throw new Error("ROOMS_DATABASE_NAME is invalid");
  }
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
    d1_databases: [
      {
        binding: "ROOMS_DB",
        database_name: databaseName,
        database_id: databaseId,
        migrations_dir: resolve(root, "migrations"),
      },
    ],
  };
}

export async function writeDeploymentConfig(destination, env = process.env) {
  const errors = [];
  const base = parse(
    await readFile(resolve(root, "wrangler.jsonc"), "utf8"),
    errors,
  );
  if (errors.length) throw new Error("Invalid relay configuration");
  const config = deploymentConfig(
    base,
    env.ROOMS_DATABASE_ID,
    env.ROOMS_DATABASE_NAME,
    env,
  );
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
