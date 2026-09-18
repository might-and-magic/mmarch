"use strict";

const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const { test } = require("node:test");
const pkg = require("../package.json");

const source = fs.readFileSync(path.join(__dirname, "../bin/mmarch"), "utf8");

function launch(platform, arch, result = { status: 0 }, missing = false) {
  const calls = { errors: [] };
  const exited = {};
  const mockRequire = name => {
    if (name === "../package.json") return pkg;
    assert.equal(name, "child_process");
    return { spawnSync: (...args) => { calls.spawn = args; return result; } };
  };
  mockRequire.resolve = name => {
    calls.resolve = name;
    if (missing) throw new Error("MODULE_NOT_FOUND");
    return `/node_modules/${name}`;
  };
  try {
    vm.runInNewContext(source, {
      require: mockRequire,
      console: { error: message => calls.errors.push(message) },
      process: {
        platform, arch, pid: 123,
        argv: ["node", "mmarch", "extract", "archive with spaces.lod", "*.txt"],
        exit: code => { calls.exit = code; throw exited; },
        kill: (pid, signal) => { calls.signal = { pid, signal }; },
      },
    });
  } catch (error) {
    if (error !== exited) throw error;
  }
  return calls;
}

for (const platform of ["win32-x64", "win32-ia32", "linux-x64", "linux-arm64", "linux-ia32", "darwin-x64", "darwin-arm64"]) {
  test(`launches the installed binary for ${platform} with unchanged arguments`, () => {
    const [os, arch] = platform.split("-");
    const calls = launch(os, arch);
    const binary = os === "win32" ? "mmarch.exe" : "mmarch";
    assert.equal(calls.resolve, `mmarch-${platform}/${binary}`);
    assert.equal(calls.spawn[0], `/node_modules/${calls.resolve}`);
    assert.deepEqual(Array.from(calls.spawn[1]), ["extract", "archive with spaces.lod", "*.txt"]);
    assert.equal(calls.spawn[2].stdio, "inherit");
    assert.equal(calls.spawn[2].windowsHide, true);
    assert.equal(calls.exit, 0);
  });
}

test("reports unsupported platforms before resolving or spawning", () => {
  const calls = launch("freebsd", "x64");
  assert.equal(calls.exit, 1);
  assert.match(calls.errors[0], /Unsupported platform: freebsd-x64/);
  assert.equal(calls.resolve, undefined);
  assert.equal(calls.spawn, undefined);
});

test("explains missing optional dependencies without a module stack trace", () => {
  const calls = launch("linux", "x64", undefined, true);
  assert.equal(calls.exit, 1);
  assert.match(calls.errors[0], /mmarch-linux-x64/);
  assert.match(calls.errors[0], /--include=optional/);
  assert.match(calls.errors[0], /npm 6/);
  assert.doesNotMatch(calls.errors[0], /MODULE_NOT_FOUND/);
  assert.equal(calls.spawn, undefined);
});

test("preserves the native exit code", () => {
  assert.equal(launch("linux", "x64", { status: 42 }).exit, 42);
});

test("reports execution failures as failures", () => {
  const calls = launch("linux", "x64", { error: new Error("EACCES"), status: null });
  assert.equal(calls.exit, 1);
  assert.match(calls.errors[0], /Could not execute.*EACCES/);
  assert.equal(launch("linux", "x64", { status: null }).exit, 1);
});

test("preserves termination by signal", () => {
  const calls = launch("linux", "x64", { status: null, signal: "SIGTERM" });
  assert.deepEqual(calls.signal, { pid: 123, signal: "SIGTERM" });
  assert.equal(calls.exit, undefined);
});
