import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, isAbsolute } from "node:path";
import { test } from "node:test";
import {
  deploymentConfig,
  writeDeploymentConfig,
} from "../scripts/prepare-deploy.mjs";

test("deployment keeps the Push configuration and adds no storage", async () => {
  const directory = await mkdtemp(join(tmpdir(), "vis-relay-config-"));
  try {
    const path = join(directory, "config.json");
    await writeDeploymentConfig(path, { RELAY_SEAL_KEY: "not-a-var" });
    const config = JSON.parse(await readFile(path, "utf8"));
    assert.equal(config.name, "vis-companion-relay");
    assert.equal(config.vars.MAX_REQUEST_BYTES, "16384");
    assert.equal(config.keep_vars, true);
    assert.equal("APNS_KEY_ID" in config.vars, false);
    assert.equal("APNS_TEAM_ID" in config.vars, false);
    assert.equal("APNS_TOPIC" in config.vars, false);
    assert.equal("d1_databases" in config, false);
    assert.ok(isAbsolute(config.main));
    assert.deepEqual(
      config.ratelimits.map((limit) => limit.name),
      ["MINT_LIMIT", "PUSH_ADDRESS_LIMIT", "PUSH_DEVICE_LIMIT"],
    );
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
    { APNS_TOPIC: "com.example.app", RELAY_SEAL_KEY: "not-a-var" },
  );
  assert.deepEqual(config.vars, { APNS_TOPIC: "com.example.app" });
});
