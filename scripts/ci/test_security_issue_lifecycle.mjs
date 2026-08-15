// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

import {
  ROLLUP_MARKER,
  ROLLUP_TITLE,
  planSecurityIssueLifecycle,
} from "./security_issue_lifecycle.mjs";

const fixtureUrl = new URL(
  "../../tests/fixtures/security-scanning/issue-lifecycle/actions.json",
  import.meta.url,
);
const fixture = JSON.parse(await readFile(fixtureUrl, "utf8"));
const { first, second } = fixture.fingerprints;

function findings(fingerprint = first, findingCount = 2) {
  return {
    state: "findings",
    findingsFingerprint: fingerprint,
    findingCount,
    artifactName: "cargo-audit-raw-9001",
  };
}

function snapshot(source, scan) {
  return {
    issuePages: structuredClone(source.issuePages),
    scan,
    trustedAuthors: ["github-actions[bot]", "security-maintainer"],
  };
}

test("plans create and clean no-op without a roll-up", () => {
  const create = planSecurityIssueLifecycle(snapshot(fixture.empty, findings()));
  assert.equal(create.action, "create");
  assert.equal(create.title, ROLLUP_TITLE);
  assert.match(create.issueBody, new RegExp(ROLLUP_MARKER.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&"), "u"));
  assert.match(create.commentBody, /fingerprint=sha256:1{64} count=2/u);
  assert.equal(
    planSecurityIssueLifecycle(snapshot(fixture.empty, { state: "clean" })).action,
    "no-op",
  );
});

test("adopts the single legacy title-only issue", () => {
  const plan = planSecurityIssueLifecycle(snapshot(fixture.legacy, findings()));
  assert.equal(plan.action, "adopt");
  assert.equal(plan.issueNumber, 7);
  assert.equal(plan.targetState, "open");
  assert.ok(plan.issueBody.endsWith(ROLLUP_MARKER));
});

test("uses every issue and comment page for update and repeated no-op", () => {
  const repeated = planSecurityIssueLifecycle(snapshot(fixture.markedAcrossPages, findings()));
  assert.deepEqual(
    { action: repeated.action, issueNumber: repeated.issueNumber, reason: repeated.reason },
    { action: "no-op", issueNumber: 11, reason: "findings-already-recorded" },
  );
  const newArtifact = findings();
  newArtifact.artifactName = "cargo-audit-raw-9002";
  assert.equal(
    planSecurityIssueLifecycle(snapshot(fixture.markedAcrossPages, newArtifact)).action,
    "no-op",
  );

  const changed = planSecurityIssueLifecycle(snapshot(fixture.markedAcrossPages, findings(second, 3)));
  assert.equal(changed.action, "update");
  assert.equal(changed.issueNumber, 11);
  assert.match(changed.commentBody, /fingerprint=sha256:2{64} count=3/u);
});

test("closes clean open roll-up and no-ops when already closed", () => {
  const open = snapshot(fixture.markedAcrossPages, { state: "clean" });
  const close = planSecurityIssueLifecycle(open);
  assert.equal(close.action, "close");
  assert.equal(close.targetState, "closed");

  const closed = planSecurityIssueLifecycle(snapshot(fixture.closedMarked, { state: "clean" }));
  assert.equal(closed.action, "no-op");
  assert.equal(closed.reason, "clean-rollup-already-closed");
});

test("reopens a closed marked issue and suppresses an unchanged comment", () => {
  const changed = planSecurityIssueLifecycle(snapshot(fixture.closedMarked, findings()));
  assert.equal(changed.action, "reopen");
  assert.equal(changed.targetState, "open");
  assert.notEqual(changed.commentBody, null);

  const source = structuredClone(fixture.closedMarked);
  source.issuePages[0][0].commentPages = [[{
    id: 201,
    author: "github-actions[bot]",
    body: `<!-- copybook-security-findings:v1 fingerprint=${first} count=2 -->`,
  }]];
  const repeated = planSecurityIssueLifecycle(snapshot(source, findings()));
  assert.equal(repeated.action, "reopen");
  assert.equal(repeated.commentBody, null);
});

test("fails closed on canonical or legacy duplicate candidates", () => {
  const duplicate = structuredClone(fixture.markedAcrossPages);
  duplicate.issuePages[0][0].title = ROLLUP_TITLE;
  assert.throws(
    () => planSecurityIssueLifecycle(snapshot(duplicate, findings())),
    /multiple canonical or legacy roll-up candidates/u,
  );
});

test("rejects malformed embedded duplicate and conflicting marker content", () => {
  const bodies = [
    `prefix ${ROLLUP_MARKER}`,
    `${ROLLUP_MARKER}\n${ROLLUP_MARKER}`,
    "<!-- copybook-security-rollup:v2 -->",
  ];
  for (const body of bodies) {
    const source = structuredClone(fixture.closedMarked);
    source.issuePages[0][0].body = body;
    assert.throws(() => planSecurityIssueLifecycle(snapshot(source, findings())), /marker/u);
  }

  const comments = [
    `prefix <!-- copybook-security-findings:v1 fingerprint=${first} count=2 -->`,
    `<!-- copybook-security-findings:v1 fingerprint=${first.toUpperCase()} count=2 -->`,
    `<!-- copybook-security-findings:v1 fingerprint=${first} count=0 -->`,
    `<!-- copybook-security-findings:v1 fingerprint=${first} count=2 -->\n<!-- copybook-security-findings:v1 fingerprint=${second} count=3 -->`,
  ];
  for (const body of comments) {
    const source = structuredClone(fixture.closedMarked);
    source.issuePages[0][0].commentPages = [[{
      id: 301,
      author: "github-actions[bot]",
      body,
    }]];
    assert.throws(() => planSecurityIssueLifecycle(snapshot(source, findings())), /marker/u);
  }
});

test("ignores untrusted marker-like comments and selects the highest trusted comment id", () => {
  const untrusted = structuredClone(fixture.markedAcrossPages);
  untrusted.issuePages[1][0].commentPages = [[
    {
      id: 999,
      author: "public-contributor",
      body: `<!-- copybook-security-findings:v1 fingerprint=${second} count=3 -->`,
    },
    {
      id: 1000,
      author: "public-contributor",
      body: `prefix <!-- copybook-security-findings:v1 fingerprint=${second} count=3 -->`,
    },
  ]];
  assert.equal(
    planSecurityIssueLifecycle(snapshot(untrusted, findings(second, 3))).action,
    "update",
  );

  const reversed = structuredClone(fixture.markedAcrossPages);
  reversed.issuePages[1][0].commentPages = [
    [{
      id: 202,
      author: "github-actions[bot]",
      body: `<!-- copybook-security-findings:v1 fingerprint=${second} count=3 -->`,
    }],
    [{
      id: 101,
      author: "github-actions[bot]",
      body: `<!-- copybook-security-findings:v1 fingerprint=${first} count=2 -->`,
    }],
  ];
  assert.equal(
    planSecurityIssueLifecycle(snapshot(reversed, findings(second, 3))).action,
    "no-op",
  );
});

test("rejects malformed snapshots fingerprints counts artifacts and duplicate ids", () => {
  const invalidSnapshots = [
    { issuePages: {}, scan: findings(), trustedAuthors: ["github-actions[bot]"] },
    { issuePages: [[]], scan: { ...findings(), findingsFingerprint: "sha256:ABC" }, trustedAuthors: ["github-actions[bot]"] },
    { issuePages: [[]], scan: { ...findings(), findingCount: 0 }, trustedAuthors: ["github-actions[bot]"] },
    { issuePages: [[]], scan: { ...findings(), artifactName: "bad artifact" }, trustedAuthors: ["github-actions[bot]"] },
    { issuePages: [[]], scan: { state: "clean", findingCount: 0 }, trustedAuthors: ["github-actions[bot]"] },
    { issuePages: [[]], scan: { state: "clean" }, trustedAuthors: [] },
    { issuePages: [[]], scan: { state: "clean" }, trustedAuthors: ["bad author"] },
  ];
  for (const invalid of invalidSnapshots) {
    assert.throws(() => planSecurityIssueLifecycle(invalid), TypeError);
  }

  const duplicateIssue = structuredClone(fixture.closedMarked);
  duplicateIssue.issuePages.push(structuredClone(duplicateIssue.issuePages[0]));
  assert.throws(() => planSecurityIssueLifecycle(snapshot(duplicateIssue, findings())), /duplicate issue/u);

  const duplicateComment = structuredClone(fixture.closedMarked);
  duplicateComment.issuePages[0][0].commentPages = [[
    { id: 8, author: "github-actions[bot]", body: "first" },
    { id: 8, author: "github-actions[bot]", body: "second" },
  ]];
  assert.throws(() => planSecurityIssueLifecycle(snapshot(duplicateComment, findings())), /duplicate comment/u);

  const duplicateCommentAcrossIssues = structuredClone(fixture.markedAcrossPages);
  duplicateCommentAcrossIssues.issuePages[0][0].commentPages = [[{
    id: 101,
    author: "public-contributor",
    body: "same identity on another issue",
  }]];
  assert.throws(
    () => planSecurityIssueLifecycle(snapshot(duplicateCommentAcrossIssues, findings())),
    /duplicate comment/u,
  );
});

test("returns byte-stable plans for repeated equivalent snapshots", () => {
  const input = snapshot(fixture.markedAcrossPages, findings(second, 4));
  const firstPlan = JSON.stringify(planSecurityIssueLifecycle(input));
  const secondPlan = JSON.stringify(planSecurityIssueLifecycle(structuredClone(input)));
  assert.equal(secondPlan, firstPlan);
});
