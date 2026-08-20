// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const workflow = await readFile(new URL("../../.github/workflows/security-scan.yml", import.meta.url), "utf8");

test("weekly workflow has explicit schedule/manual dry-run boundary and serialization", () => {
  assert.match(workflow, /schedule:\s*[\s\S]*cron: "0 9 \* \* 1"/u);
  assert.match(workflow, /workflow_dispatch:\s*[\s\S]*dry_run:/u);
  assert.match(workflow, /default: true/u);
  assert.match(workflow, /group: security-weekly-lifecycle/u);
  assert.match(workflow, /cancel-in-progress: false/u);
});

test("workflow uses fixed trust, labels, artifact, and adapter", () => {
  assert.doesNotMatch(workflow, /github-script/u);
  assert.match(workflow, /security_issue_lifecycle_adapter\.mjs/u);
  assert.match(workflow, /cargo-audit-raw-\$\{\{ github\.run_id \}\}/u);
  assert.match(workflow, /issues:\s*write/u);
  assert.match(workflow, /persist-credentials: false/u);
});

test("upload precedes gated lifecycle and final enforcement is always visible", () => {
  const upload = workflow.indexOf("id: upload");
  const lifecycle = workflow.indexOf("id: lifecycle");
  const enforce = workflow.indexOf("name: Enforce audit and lifecycle outcome");
  assert.ok(upload >= 0 && lifecycle > upload && enforce > lifecycle);
  assert.match(workflow, /if: always\(\)\s*\n\s*uses: actions\/upload-artifact/u);
  assert.match(workflow, /if-no-files-found: error/u);
  assert.match(workflow, /steps\.upload\.outcome == 'success'/u);
  assert.match(workflow, /if: always\(\)/gu);
  assert.match(workflow, /raw audit artifact upload failed/u);
});
