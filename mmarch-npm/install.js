"use strict";

const https = require("https");
const http = require("http");
const fs = require("fs");
const path = require("path");

const REPO = "might-and-magic/mmarch";
const VERSION = require("./package.json").version;

// Maps Node.js platform-arch to GitHub Release asset name
const PLATFORMS = {
  "win32-x64":    "mmarch.exe",
  "win32-ia32":   "mmarch.exe",
  "linux-x64":    "mmarch-linux-x64",
  "linux-arm64":  "mmarch-linux-arm64",
  "linux-ia32":   "mmarch-linux-ia32",
  "darwin-x64":   "mmarch-darwin-x64",
  "darwin-arm64": "mmarch-darwin-arm64",
};

const key = `${process.platform}-${process.arch}`;
const assetName = PLATFORMS[key];

if (!assetName) {
  console.warn(
    `[mmarch] No prebuilt binary for ${key}.\n` +
    `  Supported: ${Object.keys(PLATFORMS).join(", ")}`
  );
  process.exit(0);
}

const binDir = path.join(__dirname, "bin");
const isWindows = process.platform === "win32";
const binName = isWindows ? "mmarch.exe" : "mmarch";
const binPath = path.join(binDir, binName);

// Skip if binary already exists
if (fs.existsSync(binPath) && fs.statSync(binPath).size > 1000) {
  process.exit(0);
}

const url = `https://github.com/${REPO}/releases/download/v${VERSION}/${assetName}`;

console.log(`[mmarch] Downloading binary for ${key}...`);
console.log(`[mmarch] ${url}`);

fs.mkdirSync(binDir, { recursive: true });

function download(url, dest, redirects) {
  if (redirects === undefined) redirects = 5;
  if (redirects <= 0) {
    console.error("[mmarch] Too many redirects");
    process.exit(1);
  }

  const proto = url.startsWith("https") ? https : http;

  proto.get(url, { headers: { "User-Agent": "mmarch-npm" } }, (res) => {
    if (res.statusCode === 301 || res.statusCode === 302) {
      download(res.headers.location, dest, redirects - 1);
      return;
    }

    if (res.statusCode !== 200) {
      console.error(`[mmarch] Download failed: HTTP ${res.statusCode}`);
      console.error(
        `[mmarch] Download manually from:\n` +
        `  https://github.com/${REPO}/releases/tag/v${VERSION}`
      );
      process.exit(1);
    }

    const file = fs.createWriteStream(dest);
    res.pipe(file);

    file.on("finish", () => {
      file.close();
      if (!isWindows) {
        try { fs.chmodSync(dest, 0o755); } catch {}
      }
      const size = fs.statSync(dest).size;
      console.log(`[mmarch] Done. ${(size / 1024).toFixed(0)} KB`);
    });

    file.on("error", (err) => {
      fs.unlinkSync(dest);
      console.error(`[mmarch] Write error: ${err.message}`);
      process.exit(1);
    });
  }).on("error", (err) => {
    console.error(`[mmarch] Download error: ${err.message}`);
    console.error(
      `[mmarch] Download manually from:\n` +
      `  https://github.com/${REPO}/releases/tag/v${VERSION}`
    );
    process.exit(1);
  });
}

download(url, binPath);
