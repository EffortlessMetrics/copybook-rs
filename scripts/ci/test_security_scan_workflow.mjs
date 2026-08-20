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

test("workflow uses fixed trust, labels, raw artifact, and adapter", () => {
  assert.doesNotMatch(workflow, /github-script/u);
  assert.match(workflow, /security_issue_lifecycle_adapter\.mjs/u);
  assert.match(workflow, /cargo-audit-raw-\$\{\{ github\.run_id \}\}/u);
  assert.match(workflow, /issues:\s*write/u);
  assert.match(workflow, /persist-credentials: false/u);
});

test("normalized publication is explicit, validated, and distinct from raw evidence", () => {
  assert.match(workflow, /id: schema_validator[\s\S]*?check-jsonschema/u);
  assert.match(workflow, /check-jsonschema==0\.38\.0/u);
  assert.match(workflow, /pypi\.org\/project\/check-jsonschema\/0\.38\.0/u);
  assert.match(workflow, /id: receipt_generate[\s\S]*?generate_security_receipt\.py generate/u);
  assert.match(workflow, /--commit-sha "\$\{\{ github\.sha \}\}"/u);
  assert.match(workflow, /--workflow-run-id "\$\{\{ github\.run_id \}\}"/u);
  assert.match(workflow, /--audit-exit-code "\$\{AUDIT_EXIT_STATUS\}"/u);
  assert.match(workflow, /id: receipt_semantic[\s\S]*?generate_security_receipt\.py validate/u);
  assert.match(workflow, /id: receipt_schema[\s\S]*?security-receipt-schema-v2\.json/u);
  assert.match(workflow, /id: upload_receipt[\s\S]*?name: security-receipt-v2-\$\{\{ github\.run_id \}\}/u);
  assert.match(workflow, /id: upload_receipt[\s\S]*?if-no-files-found: error/u);
  assert.match(workflow, /retention-days: 90/gu);
  assert.match(workflow, /normalized receipt output|security-receipt-v2\.json/u);
});

test("raw evidence and every receipt gate precede lifecycle mutation", () => {
  const upload = workflow.indexOf("id: upload");
  const generate = workflow.indexOf("id: receipt_generate");
  const semantic = workflow.indexOf("id: receipt_semantic");
  const schema = workflow.indexOf("id: receipt_schema");
  const normalizedUpload = workflow.indexOf("id: upload_receipt");
  const lifecycle = workflow.indexOf("id: lifecycle");
  const enforce = workflow.indexOf("name: Enforce audit and lifecycle outcome");
  assert.ok(
    upload >= 0 &&
      generate > upload &&
      semantic > generate &&
      schema > semantic &&
      normalizedUpload > schema &&
      lifecycle > normalizedUpload &&
      enforce > lifecycle,
  );
  assert.match(workflow, /if: always\(\)\s*\n\s*uses: actions\/upload-artifact/u);
  assert.match(workflow, /if-no-files-found: error/u);
  assert.match(workflow, /if: steps\.upload_receipt\.outcome == 'success' && steps\.upload\.outcome == 'success' && steps\.classify\.outcome == 'success' && steps\.classify\.outputs\.eligible == 'true'/u);
  assert.match(workflow, /if: always\(\)/gu);
  assert.match(workflow, /raw audit artifact upload failed/u);
  assert.match(workflow, /normalized security receipt v2 publication failed/u);
  assert.match(workflow, /steps\.lifecycle\.outcome\s*\}\}\s*["']\s*!=\s*["']success["']/u);
  assert.match(workflow, /eligible lifecycle did not complete[\s\S]*?exit 1/u);
});
