import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, isAbsolute } from "node:path";
import { test } from "node:test";
import {
  deploymentConfig,
  writeDeploymentConfig,
} from "../scripts/prepare-deploy.mjs";

const databaseId = "00000000-0000-4000-8000-000000000001";

test("D1 migration guards avoid nested CASE blocks", async () => {
  // The remote D1 migration parser rejects CASE ... END inside trigger blocks.
  const sql = await readFile(
    new URL("../migrations/0001_rooms.sql", import.meta.url),
    "utf8",
  );
  const triggers = sql.match(/^CREATE TRIGGER[\s\S]*?^END;/gm) ?? [];
  assert.ok(triggers.length > 0);
  for (const trigger of triggers) assert.doesNotMatch(trigger, /\bCASE\b/i);
});

test("deployment adds private Rooms storage without changing Push configuration", async () => {
  const directory = await mkdtemp(join(tmpdir(), "vis-relay-config-"));
  try {
    const path = join(directory, "config.json");
    await writeDeploymentConfig(path, {
      ROOMS_DATABASE_ID: databaseId,
      ROOMS_ADMIN_TOKEN: "not-a-var",
    });
    const config = JSON.parse(await readFile(path, "utf8"));
    assert.equal(config.name, "vis-companion-relay");
    assert.equal(config.vars.MAX_REQUEST_BYTES, "16384");
    assert.equal(config.keep_vars, true);
    assert.equal("APNS_KEY_ID" in config.vars, false);
    assert.equal("APNS_TEAM_ID" in config.vars, false);
    assert.equal("APNS_TOPIC" in config.vars, false);
    assert.equal(config.d1_databases[0].binding, "ROOMS_DB");
    assert.equal(config.d1_databases[0].database_id, databaseId);
    assert.ok(isAbsolute(config.main));
    assert.ok(isAbsolute(config.d1_databases[0].migrations_dir));
    assert.equal(config.ratelimits.length, 8);
    assert.deepEqual(config.triggers.crons, ["23 * * * *"]);
    assert.equal(JSON.stringify(config).includes("not-a-var"), false);
    if (process.platform !== "win32")
      assert.equal((await stat(path)).mode & 0o777, 0o600);
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
});

test("explicit public overrides are applied without copying other environment values", () => {
  const config = deploymentConfig(
    { main: "src/index.ts", vars: { APNS_TOPIC: "" } },
    databaseId,
    "vis-council-rooms",
    { APNS_TOPIC: "com.example.app", ROOMS_ADMIN_TOKEN: "not-a-var" },
  );
  assert.deepEqual(config.vars, { APNS_TOPIC: "com.example.app" });
});

test("missing or malformed database configuration stops deployment", () => {
  for (const id of [undefined, "", "not-an-id", `${databaseId}\n`]) {
    assert.throws(() => deploymentConfig({}, id), /database UUID/);
  }
  assert.throws(
    () => deploymentConfig({}, databaseId, "../other"),
    /NAME is invalid/,
  );
});
