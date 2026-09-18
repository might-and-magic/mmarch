"use strict";

const { execFileSync } = require("child_process");
const path = require("path");
const pkg = require("../package.json");

function publishPlatforms(outputDir, npm) {
  for (const [name, version] of Object.entries(pkg.optionalDependencies)) {
    const dir = path.join(outputDir, path.basename(name));
    const manifest = require(path.join(dir, "package.json"));
    if (manifest.name !== name || manifest.version !== version) {
      throw new Error(`Prepared package does not match ${name}@${version}`);
    }
    // A failed release can have published some of the immutable platform
    // versions already. Only E404 means it is safe to attempt a new publish.
    try {
      const published = JSON.parse(npm(["view", `${name}@${version}`, "version", "--json"]));
      // npm 12 always wraps npm view values in an array.
      if (published !== version &&
          !(Array.isArray(published) && published.length === 1 && published[0] === version)) {
        throw new Error(`Unexpected registry version for ${name}@${version}`);
      }
      console.log(`Already published: ${name}@${version}`);
      continue;
    } catch (error) {
      let response;
      try {
        response = JSON.parse(error.stdout);
      } catch {
        throw error;
      }
      if (!response.error || response.error.code !== "E404") {
        throw error;
      }
    }
    npm(["publish", dir, "--provenance", "--access", "public"], { stdio: "inherit" });
  }
}

if (require.main === module) {
  const outputDir = process.argv[2];
  if (!outputDir) {
    console.error("Usage: node scripts/publish-platforms.js <prepared-output-dir>");
    process.exit(1);
  }
  try {
    publishPlatforms(path.resolve(outputDir), (args, options) =>
      execFileSync("npm", args, { encoding: "utf8", ...options }));
  } catch (error) {
    console.error(`[mmarch] Platform publishing failed: ${error.message}`);
    process.exit(1);
  }
}

module.exports = { publishPlatforms };
