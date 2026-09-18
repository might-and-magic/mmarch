"use strict";

// Kept separate from node:test so the complete installation can also run on
// Node 14 + npm 6. A local registry exercises real optional dependency
// selection without publishing anything or contacting the public registry.
const assert = require("assert").strict;
const { execFile } = require("child_process");
const crypto = require("crypto");
const fs = require("fs");
const http = require("http");
const os = require("os");
const path = require("path");
const { promisify } = require("util");
const { preparePackages, artifacts } = require("../scripts/prepare-packages");
const pkg = require("../package.json");
const exec = promisify(execFile);

async function main() {
  assert.ok(process.env.npm_execpath, "Run using npm run test:install");
  // macOS exposes /var through a symlink, while process.cwd() returns the
  // physical /private/var path. Compare against a canonical temporary path.
  const root = fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), "mmarch-install-")));
  const output = path.join(root, "dist");
  const downloads = [];
  const packages = new Map();
  let registry;
  const server = http.createServer((req, res) => {
    const request = decodeURIComponent(req.url.split("?")[0]);
    for (const [name, entry] of packages) {
      const tarballPath = `/${name}/-/${entry.filename}`;
      if (request === tarballPath) {
        downloads.push(name);
        res.setHeader("Content-Type", "application/octet-stream");
        fs.createReadStream(entry.tarball).pipe(res);
        return;
      }
      if (request === `/${name}`) {
        res.setHeader("Content-Type", "application/json");
        res.end(JSON.stringify({
          name,
          "dist-tags": { latest: pkg.version },
          versions: { [pkg.version]: {
            ...entry.manifest,
            dist: { tarball: registry + tarballPath, shasum: entry.shasum },
          } },
        }));
        return;
      }
    }
    res.statusCode = 404;
    res.end(JSON.stringify({ error: "not_found" }));
  });

  try {
    // npm run exports configuration into its child environment. Isolate it
    // along with user/global npmrc files so user settings cannot change the
    // platform selection or script policy exercised by these tests.
    const env = { ...process.env };
    for (const name of Object.keys(env)) {
      if (/^npm_config_/i.test(name)) delete env[name];
    }
    Object.assign(env, {
      npm_config_cache: path.join(root, "cache"),
      npm_config_userconfig: path.join(root, "user.npmrc"),
      npm_config_globalconfig: path.join(root, "global.npmrc"),
      npm_config_audit: "false",
      npm_config_fund: "false",
      npm_config_update_notifier: "false",
    });
    const npm = (args, options = {}) => exec(process.execPath,
      [process.env.npm_execpath, ...args], { cwd: root, env, ...options });
    for (const artifact of new Set(Object.values(artifacts))) {
      const dir = path.join(root, artifact);
      fs.mkdirSync(dir);
      fs.writeFileSync(path.join(dir, artifact === "mmarch-win32" ? "mmarch.exe" : "mmarch"), artifact);
    }
    preparePackages(root, output, `v${pkg.version}`);
    const hostPackage = `mmarch-${process.platform}-${process.arch}`;
    const binary = process.platform === "win32" ? "mmarch.exe" : "mmarch";
    // Node itself is a portable native fixture that lets us check arguments,
    // stdio, failures and signals through the installed mmarch launcher.
    fs.copyFileSync(process.execPath, path.join(output, hostPackage, binary));
    fs.chmodSync(path.join(output, hostPackage, binary), 0o755);
    for (const name of fs.readdirSync(output)) {
      const dir = path.join(output, name);
      const packResult = JSON.parse((await npm(["pack", "--json", "--ignore-scripts"], { cwd: dir })).stdout);
      // npm 6-11 return an array; npm 12 keys results by package name.
      const packed = Array.isArray(packResult) ? packResult[0] : packResult[name];
      const tarball = path.join(dir, packed.filename);
      const files = packed.files.map(file => file.path).sort();
      assert.deepEqual(files, ["LICENSE", "README.md", name === "mmarch" ? "bin/mmarch" : name.startsWith("mmarch-win32") ? "mmarch.exe" : "mmarch", "package.json"].sort());
      packages.set(name, {
        filename: packed.filename,
        tarball,
        manifest: JSON.parse(fs.readFileSync(path.join(dir, "package.json"))),
        shasum: crypto.createHash("sha1").update(fs.readFileSync(tarball)).digest("hex"),
      });
    }
    await new Promise(resolve => server.listen(0, "127.0.0.1", resolve));
    registry = `http://127.0.0.1:${server.address().port}`;
    env.npm_config_registry = registry;
    const npmVersion = (await npm(["--version"])).stdout.trim();
    const npmMajor = Number(npmVersion.split(".")[0]);
    const cliFor = dir => path.join(dir, "node_modules/mmarch/bin/mmarch");

    const local = path.join(root, "local");
    fs.mkdirSync(local);
    fs.writeFileSync(path.join(local, "package.json"), '{"private":true}');
    await npm(["install", `mmarch@${pkg.version}`, "--ignore-scripts"], { cwd: local });
    assert.deepEqual(fs.readdirSync(path.join(local, "node_modules")).filter(name => name.startsWith("mmarch-")).sort(), [hostPackage]);
    // npm 6 fetches optional tarballs before rejecting incompatible platforms;
    // modern npm can skip those downloads using the registry metadata.
    if (npmMajor >= 7) {
      assert.deepEqual([...new Set(downloads)].sort(), ["mmarch", hostPackage].sort());
    }
    const launcher = cliFor(local);
    assert.equal((await exec(process.execPath, [launcher, "--version"])).stdout.trim(), process.version);
    const echo = "process.stdout.write(JSON.stringify({args:process.argv.slice(1),cwd:process.cwd(),env:process.env.MMARCH_TEST}));process.stderr.write('stderr works')";
    const echoed = await exec(process.execPath, [launcher, "-e", echo, "--", "space in name.lod", "*.txt"], {
      cwd: local, env: { ...process.env, MMARCH_TEST: "inherited" },
    });
    assert.deepEqual(JSON.parse(echoed.stdout), { args: ["space in name.lod", "*.txt"], cwd: local, env: "inherited" });
    assert.equal(echoed.stderr, "stderr works");
    await assert.rejects(exec(process.execPath, [launcher, "-e", "process.exit(42)"]), error => error.code === 42);
    if (process.platform !== "win32") {
      await assert.rejects(exec(process.execPath, [launcher, "-e", "process.kill(process.pid, 'SIGTERM')"]), error => error.signal === "SIGTERM");
    }

    const prefix = path.join(root, "global");
    await npm(["install", "-g", `mmarch@${pkg.version}`, "--prefix", prefix, "--ignore-scripts"]);
    const globalBin = path.join(prefix, process.platform === "win32" ? "mmarch.cmd" : "bin/mmarch");
    const globalResult = process.platform === "win32"
      ? await exec("cmd.exe", ["/d", "/s", "/c", `""${globalBin}" --version"`], { windowsVerbatimArguments: true })
      : await exec(globalBin, ["--version"]);
    assert.equal(globalResult.stdout.trim(), process.version);

    const omitted = path.join(root, "omitted");
    fs.mkdirSync(omitted);
    fs.writeFileSync(path.join(omitted, "package.json"), '{"private":true}');
    await npm(["install", `mmarch@${pkg.version}`, "--ignore-scripts", npmMajor < 7 ? "--no-optional" : "--omit=optional"], { cwd: omitted });
    await assert.rejects(exec(process.execPath, [cliFor(omitted), "--version"]), error => {
      assert.equal(error.code, 1);
      assert.match(error.stderr, new RegExp(hostPackage));
      assert.match(error.stderr, /--include=optional/);
      assert.ok(!error.stderr.includes("MODULE_NOT_FOUND"));
      return true;
    });
    await npm(["install", `mmarch@${pkg.version}`, "--ignore-scripts", npmMajor < 7 ? "--optional" : "--include=optional"], { cwd: omitted });
    assert.equal((await exec(process.execPath, [cliFor(omitted), "--version"])).stdout.trim(), process.version);
    console.log(`Installation passed on Node ${process.version}, npm ${npmVersion}: local/global installs, platform filtering, stdio, exit codes, and omitted-dependency recovery (all with --ignore-scripts).`);

    if (process.env.MMARCH_TEST_BINARY) {
      fs.copyFileSync(process.env.MMARCH_TEST_BINARY, path.join(local, "node_modules", hostPackage, binary));
      const mmarch = args => exec(process.execPath, [launcher, ...args]);
      assert.equal((await mmarch(["--version"])).stdout.trim(), pkg.version);
      const source = path.join(root, "input with spaces");
      const extracted = path.join(root, "extracted");
      const archive = path.join(root, "test archive.lod");
      fs.mkdirSync(source);
      fs.writeFileSync(path.join(source, "hello.txt"), "hello from the npm launcher\n");
      await mmarch(["create", path.basename(archive), "h3lod", root, path.join(source, "hello.txt")]);
      assert.match((await mmarch(["list", archive])).stdout, /hello.txt/);
      await mmarch(["extract", archive, extracted]);
      assert.equal(fs.readFileSync(path.join(extracted, "hello.txt"), "utf8"), "hello from the npm launcher\n");
      console.log("Real mmarch binary passed: version, create, list and extract through the installed launcher.");
    }
  } finally {
    await new Promise(resolve => server.close(resolve));
    fs.rmSync(root, { recursive: true, force: true });
  }
}

main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
