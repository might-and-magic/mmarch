"use strict";

const assert = require("node:assert/strict");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const { test } = require("node:test");
const pkg = require("../package.json");
const { preparePackages, artifacts } = require("../scripts/prepare-packages");
const { publishPlatforms } = require("../scripts/publish-platforms");

function fixture(t) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "mmarch-packages-"));
  t.after(() => fs.rmSync(root, { recursive: true, force: true }));
  for (const artifact of new Set(Object.values(artifacts))) {
    const dir = path.join(root, artifact);
    fs.mkdirSync(dir);
    fs.writeFileSync(path.join(dir, artifact === "mmarch-win32" ? "mmarch.exe" : "mmarch"), artifact);
  }
  return { root, output: path.join(root, "dist") };
}

test("prepares complete, version-locked packages without install scripts", t => {
  const { root, output } = fixture(t);
  preparePackages(root, output, `v${pkg.version}`);
  assert.equal(fs.readdirSync(output).length, 8);
  for (const [platform, artifact] of Object.entries(artifacts)) {
    const dir = path.join(output, `mmarch-${platform}`);
    const manifest = JSON.parse(fs.readFileSync(path.join(dir, "package.json")));
    const [os, cpu] = platform.split("-");
    const binary = os === "win32" ? "mmarch.exe" : "mmarch";
    assert.equal(manifest.name, `@mightandmagic/mmarch-${platform}`);
    assert.equal(manifest.version, pkg.optionalDependencies[manifest.name]);
    assert.deepEqual(manifest.os, [os]);
    assert.deepEqual(manifest.cpu, [cpu]);
    assert.equal(manifest.libc, undefined);
    assert.equal(manifest.scripts, undefined);
    assert.deepEqual(manifest.publishConfig, { access: "public" });
    assert.deepEqual(manifest.files, [binary]);
    assert.equal(fs.readFileSync(path.join(dir, binary), "utf8"), artifact);
    assert.match(fs.readFileSync(path.join(dir, "LICENSE"), "utf8"), /MIT License/);
    if (process.platform !== "win32") {
      assert.equal(fs.statSync(path.join(dir, binary)).mode & 0o777, 0o755);
    }
  }
  const main = require(path.join(output, "mmarch/package.json"));
  assert.deepEqual(main.optionalDependencies, pkg.optionalDependencies);
  assert.equal(main.scripts, undefined);
  assert.deepEqual(main.files, ["bin/mmarch"]);
  assert.ok(fs.existsSync(path.join(output, "mmarch", main.bin.mmarch)));
  assert.ok(!fs.existsSync(path.join(output, "mmarch/install.js")));
});

test("rejects mismatched release tags before writing packages", t => {
  const { root, output } = fixture(t);
  assert.throws(() => preparePackages(root, output, "v0.0.0"), /does not match/);
  assert.ok(!fs.existsSync(output));
});

test("rejects missing or empty build artifacts before writing packages", t => {
  const { root, output } = fixture(t);
  const binary = path.join(root, "mmarch-linux-x64/mmarch");
  fs.writeFileSync(binary, "");
  assert.throws(() => preparePackages(root, output), /Missing or empty binary/);
  assert.ok(!fs.existsSync(output));
  fs.unlinkSync(binary);
  assert.throws(() => preparePackages(root, output), /ENOENT/);
  assert.ok(!fs.existsSync(output));
});

test("refuses to reuse an output directory containing old packages", t => {
  const { root, output } = fixture(t);
  fs.mkdirSync(output);
  assert.throws(() => preparePackages(root, output), /EEXIST/);
});

function registryError(code) {
  const error = new Error(code);
  error.stdout = JSON.stringify({ error: { code } });
  return error;
}

test("publishes every platform and skips already published versions on retry", t => {
  const { root, output } = fixture(t);
  preparePackages(root, output);
  const published = [];
  const existing = Object.keys(pkg.optionalDependencies)[0];
  publishPlatforms(output, args => {
    if (args[0] === "view") {
      if (args[1] === `${existing}@${pkg.version}`) return JSON.stringify(pkg.version);
      throw registryError("E404");
    }
    assert.equal(args[0], "publish");
    assert.deepEqual(args.slice(2), ["--provenance", "--access", "public"]);
    published.push(require(path.join(args[1], "package.json")).name);
  });
  assert.deepEqual(published, Object.keys(pkg.optionalDependencies).filter(name => name !== existing));
});

test("stops publishing on registry or authentication failures", t => {
  const { root, output } = fixture(t);
  preparePackages(root, output);
  for (const code of ["E401", "E403", "E500", "ENETUNREACH"]) {
    const calls = [];
    assert.throws(() => publishPlatforms(output, args => {
      calls.push(args[0]);
      throw registryError(code);
    }), new RegExp(code));
    assert.deepEqual(calls, ["view"]);
  }
  let publishes = 0;
  assert.throws(() => publishPlatforms(output, args => {
    if (args[0] === "view") throw registryError("E404");
    publishes++;
    throw new Error("publish failed");
  }), /publish failed/);
  assert.equal(publishes, 1);
});

test("accepts npm 12 version arrays when retrying published packages", t => {
  const { root, output } = fixture(t);
  preparePackages(root, output);
  publishPlatforms(output, args => {
    assert.equal(args[0], "view");
    return JSON.stringify([pkg.version]);
  });
});
