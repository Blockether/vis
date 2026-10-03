/** Compile canonical schemas without runtime code generation in the Worker. */
import { readFile, writeFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import Ajv2020 from "ajv/dist/2020.js";
import standalone from "ajv/dist/standalone/index.js";

const schemaRoot = new URL(
  "../../../packages/vis-contract/resources/vis-contract/schema/",
  import.meta.url,
);
const ajv = new Ajv2020({ strict: false, code: { source: true, esm: true } });
for (const name of ["common", "council", "rooms"]) {
  ajv.addSchema(
    JSON.parse(await readFile(new URL(`${name}.json`, schemaRoot), "utf8")),
  );
}
const schema = ajv.getSchema(
  "https://contract.blockether.com/vis/schema/rooms.json",
).schema;
const exports = Object.fromEntries(
  Object.keys(schema.$defs).map((name) => [
    name,
    `${schema.$id}#/$defs/${name}`,
  ]),
);
const code =
  "// Generated from packages/vis-contract. Run npm run contracts.\n" +
  standalone(ajv, exports);
const types =
  "// Generated from packages/vis-contract. Run npm run contracts.\n" +
  Object.keys(exports)
    .map((name) => `export function ${name}(data: unknown): boolean;`)
    .join("\n") +
  "\n";
const limits =
  "-- Generated from rooms.json. Run npm run contracts.\n" +
  Object.entries(schema["x-vis-limits"])
    .map(
      ([name, value]) =>
        `INSERT INTO room_limits (name, value) VALUES ('${name}', ${value}) ON CONFLICT(name) DO UPDATE SET value = excluded.value;`,
    )
    .join("\n\n") +
  "\n";
for (const [name, text] of [
  ["src/rooms/generated/validators.js", code],
  ["src/rooms/generated/validators.d.ts", types],
  ["migrations/0002_rooms_limits.sql", limits],
]) {
  const path = new URL(`../${name}`, import.meta.url);
  if (process.argv.includes("--check")) {
    if ((await readFile(path, "utf8")) !== text)
      throw new Error(`Stale contract: ${fileURLToPath(path)}`);
  } else {
    await writeFile(path, text);
  }
}
