"use strict";

const fs = require("fs");
const path = require("path");
const pkg = require("../package.json");

// These directories are the artifacts uploaded by the build matrix.
// Both Windows packages contain the same 32-bit executable.
const artifacts = {
  "win32-x64": "mmarch-win32",
  "win32-ia32": "mmarch-win32",
  "linux-x64": "mmarch-linux-x64",
  "linux-arm64": "mmarch-linux-arm64",
  "linux-ia32": "mmarch-linux-ia32",
  "darwin-x64": "mmarch-darwin-x64",
  "darwin-arm64": "mmarch-darwin-arm64",
};

function preparePackages(artifactDir, outputDir, releaseTag) {
  if (releaseTag && releaseTag !== `v${pkg.version}`) {
    throw new Error(`Release tag ${releaseTag} does not match mmarch@${pkg.version}`);
  }
  const packageNames = Object.keys(artifacts).map(platform => `mmarch-${platform}`);
  if (JSON.stringify(Object.keys(pkg.optionalDependencies).sort()) !==
      JSON.stringify(packageNames.sort())) {
    throw new Error("optionalDependencies must match the supported platform packages");
  }

  // Validate every input before producing any publishable package.
  const packages = Object.entries(artifacts).map(([platform, artifact]) => {
    const name = `mmarch-${platform}`;
    if (pkg.optionalDependencies[name] !== pkg.version) {
      throw new Error(`${name} must be pinned to ${pkg.version}`);
    }
    const [os, cpu] = platform.split("-");
    const binary = os === "win32" ? "mmarch.exe" : "mmarch";
    const source = path.join(artifactDir, artifact, binary);
    const stat = fs.statSync(source);
    if (!stat.isFile() || stat.size === 0) {
      throw new Error(`Missing or empty binary: ${source}`);
    }
    return { name, os, cpu, binary, source };
  });

  // A fresh output directory prevents old files from leaking into a release.
  fs.mkdirSync(outputDir);
  const npmDir = path.resolve(__dirname, "..");
  const license = path.join(npmDir, "..", "LICENSE");
  for (const { name, os, cpu, binary, source } of packages) {
    const dir = path.join(outputDir, name);
    fs.mkdirSync(dir);
    fs.copyFileSync(source, path.join(dir, binary));
    fs.chmodSync(path.join(dir, binary), 0o755);
    const manifest = {
      name,
      version: pkg.version,
      description: `The ${os} ${cpu} binary for mmarch.`,
      author: pkg.author,
      license: pkg.license,
      repository: pkg.repository,
      homepage: pkg.homepage,
      os: [os],
      cpu: [cpu],
      // Linux binaries are statically linked against musl, so they also run
      // on glibc systems. Do not restrict them with a libc field.
      files: [binary],
    };
    fs.writeFileSync(path.join(dir, "package.json"), JSON.stringify(manifest, null, 2) + "\n");
    fs.copyFileSync(license, path.join(dir, "LICENSE"));
    fs.writeFileSync(path.join(dir, "README.md"),
      `# ${name}\n\nPlatform binary for [mmarch](${pkg.homepage}). Install \`mmarch\` to use the CLI.\n`);
  }

  const mainDir = path.join(outputDir, "mmarch");
  fs.mkdirSync(path.join(mainDir, "bin"), { recursive: true });
  fs.copyFileSync(path.join(npmDir, "bin", "mmarch"), path.join(mainDir, "bin", "mmarch"));
  fs.chmodSync(path.join(mainDir, "bin", "mmarch"), 0o755);
  const { scripts, ...manifest } = pkg;
  fs.writeFileSync(path.join(mainDir, "package.json"), JSON.stringify(manifest, null, 2) + "\n");
  fs.copyFileSync(path.join(npmDir, "README.md"), path.join(mainDir, "README.md"));
  fs.copyFileSync(license, path.join(mainDir, "LICENSE"));
}

if (require.main === module) {
  const [artifactDir, outputDir, releaseTag] = process.argv.slice(2);
  if (!artifactDir || !outputDir) {
    console.error("Usage: node scripts/prepare-packages.js <artifact-dir> <new-output-dir> [release-tag]");
    process.exit(1);
  }
  try {
    preparePackages(path.resolve(artifactDir), path.resolve(outputDir), releaseTag);
  } catch (error) {
    console.error(`[mmarch] ${error.message}`);
    process.exit(1);
  }
}

module.exports = { preparePackages, artifacts };
