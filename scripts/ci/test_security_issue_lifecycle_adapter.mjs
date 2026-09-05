// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

import {
  discoverSnapshot,
  executePlan,
  runLifecycle,
} from "./security_issue_lifecycle_adapter.mjs";
import { planSecurityIssueLifecycle } from "./security_issue_lifecycle.mjs";

const fixture = JSON.parse(await readFile(new URL("../../tests/fixtures/security-scanning/issue-lifecycle/actions.json", import.meta.url), "utf8"));
const first = fixture.fingerprints.first;

function rawIssue(issue, { pullRequest = false } = {}) {
  return {
    ...issue,
    labels: [{ name: "security" }],
    ...(pullRequest ? { pull_request: { url: "https://api.github.com/pulls/99" } } : {}),
  };
}

function rawComments(issue) {
  return issue.commentPages.flat().map((comment) => ({ id: comment.id, body: comment.body, user: { login: comment.author } }));
}

function fakeClient(issuePages, commentsByIssue = new Map()) {
  const calls = [];
  return {
    calls,
    async listIssues(page) {
      calls.push(["listIssues", page]);
      return issuePages[page - 1] ?? [];
    },
    async listComments(number, page) {
      calls.push(["listComments", number, page]);
      return commentsByIssue.get(number)?.[page - 1] ?? [];
    },
    async createIssue(payload) { calls.push(["createIssue", payload]); return { number: 88 }; },
    async updateIssue(number, payload) { calls.push(["updateIssue", number, payload]); return { number }; },
    async createComment(number, body) { calls.push(["createComment", number, body]); return { id: 500 }; },
  };
}

test("discovers all issue pages, scopes comment pagination to roll-up candidates, and excludes pull requests", async () => {
  const source = fixture.markedAcrossPages.issuePages[0][0];
  const filler = Array.from({ length: 99 }, (_, index) => rawIssue({ number: 1000 + index, title: `other-${index}`, body: "", state: "open", commentPages: [] }));
  const firstPage = [...filler, rawIssue(source)];
  const secondPage = [rawIssue({ number: 4000, title: "not selected", body: "", state: "open", commentPages: [] }, { pullRequest: true })];
  const fillerComments = Array.from({ length: 100 }, (_, index) => ({ id: 10000 + index, body: "unrelated", user: { login: "public-contributor" } }));
  const comments = new Map([[source.number, [fillerComments, rawComments(source)]]]);
  const client = fakeClient([firstPage, secondPage], comments);
  const snapshot = await discoverSnapshot(client);
  assert.equal(snapshot.issuePages.length, 2);
  assert.equal(snapshot.issuePages.flat().filter((issue) => issue.number === source.number).length, 1);
  assert.equal(snapshot.issuePages.flat().some((issue) => issue.number === 4000), false);
  assert.equal(snapshot.issuePages[0][0].commentPages.length, 0);
  assert.equal(snapshot.trustedAuthors[0], "github-actions[bot]");
  const commentCalls = client.calls.filter((call) => call[0] === "listComments");
  assert.deepEqual([...new Set(commentCalls.map((call) => call[1]))], [source.number]);
  assert.ok(commentCalls.some((call) => call[2] === 2));
});

test("unrelated security issue comments are neither fetched nor allowed to poison planning", async () => {
  const unrelated = rawIssue({ number: 44, title: "Unrelated security work", body: "", state: "open", commentPages: [] });
  const comments = new Map([[44, [[{ id: 0, body: 7, user: { login: null } }]]]]);
  const client = fakeClient([[unrelated]], comments);
  const result = await runLifecycle({ client, scan: { state: "clean", eligible: true }, dryRun: true });
  assert.equal(result.plan.action, "no-op");
  assert.equal(result.plan.reason, "clean-without-rollup");
  assert.equal(client.calls.some((call) => call[0] === "listComments"), false);
});

test("malformed roll-up marker namespaces remain candidates and fail closed", async () => {
  const malformed = rawIssue({
    number: 45,
    title: "Unrelated title",
    body: "<!-- copybook-security-rollup:v2 -->",
    state: "open",
    commentPages: [],
  });
  const client = fakeClient([[malformed]], new Map([[45, [[]]]]));
  await assert.rejects(
    () => runLifecycle({ client, scan: { state: "clean", eligible: true }, dryRun: true }),
    /malformed or duplicate roll-up marker/u,
  );
  assert.deepEqual(
    client.calls.filter((call) => call[0] === "listComments"),
    [["listComments", 45, 1]],
  );
});

test("executes every planner action with canonical target and no-op writes nothing", async () => {
  const client = fakeClient([]);
  const plans = [
    { action: "create", issueNumber: null, title: "t", issueBody: "b", commentBody: "c" },
    { action: "adopt", issueNumber: 7, issueBody: "b", commentBody: "c" },
    { action: "update", issueNumber: 7, issueBody: null, commentBody: "c" },
    { action: "close", issueNumber: 7, targetState: "closed", commentBody: null },
    { action: "reopen", issueNumber: 7, targetState: "open", commentBody: "c" },
    { action: "no-op", issueNumber: 7, commentBody: null },
  ];
  for (const plan of plans) await executePlan(client, plan);
  assert.deepEqual(client.calls.map((call) => call[0]), [
    "createIssue", "createComment", "updateIssue", "createComment", "createComment", "updateIssue", "updateIssue", "createComment",
  ]);
  assert.equal(client.calls.filter((call) => call[0] === "createIssue")[0][1].labels.join(","), "security,infrastructure");
});

test("dry-run plans but performs zero writes", async () => {
  const source = fixture.closedMarked.issuePages[0][0];
  const client = fakeClient([[rawIssue(source)]], new Map([[source.number, [rawComments(source)]]]));
  const result = await runLifecycle({
    client,
    dryRun: true,
    scan: { state: "findings", findingCount: 2, findingsFingerprint: first, artifactName: "cargo-audit-raw-1", eligible: true },
  });
  assert.equal(result.execution.writes, 0);
  assert.equal(client.calls.filter((call) => call[0].endsWith("Issue") || call[0] === "createComment").length, 0);
});

test("clean scans close open roll-ups and no-op closed roll-ups", async () => {
  const open = fixture.markedAcrossPages.issuePages[1][0];
  const openClient = fakeClient([[rawIssue(open)]], new Map([[open.number, [rawComments(open)]]]));
  const closed = fixture.closedMarked.issuePages[0][0];
  const closedClient = fakeClient([[rawIssue(closed)]], new Map([[closed.number, [rawComments(closed)]]]));
  const close = await runLifecycle({ client: openClient, scan: { state: "clean", eligible: true }, dryRun: true });
  const noOp = await runLifecycle({ client: closedClient, scan: { state: "clean", eligible: true }, dryRun: true });
  assert.equal(close.plan.action, "close");
  assert.equal(noOp.plan.action, "no-op");
  assert.equal(close.execution.writes, 0);
  assert.equal(noOp.execution.writes, 0);
});

test("normalizes null issue bodies but rejects other malformed bodies", async () => {
  const legacy = fixture.legacy.issuePages[0][0];
  const nullBody = rawIssue({ ...legacy, body: null });
  const client = fakeClient([[nullBody]], new Map([[legacy.number, [[]]]]));
  const snapshot = await discoverSnapshot(client);
  assert.equal(snapshot.issuePages[0][0].body, "");
  const malformed = fakeClient([[rawIssue({ ...legacy, body: 7 })]], new Map([[legacy.number, [[]]]]));
  await assert.rejects(() => discoverSnapshot(malformed), /body must be strings/u);
});

test("rejects duplicate identities and API failures before any write", async () => {
  const source = fixture.closedMarked.issuePages[0][0];
  const duplicate = rawIssue(source);
  const fillers = Array.from({ length: 99 }, (_, index) => rawIssue({ number: 7000 + index, title: `filler-${index}`, body: "", state: "open", commentPages: [] }));
  const client = fakeClient([[...fillers, duplicate], [duplicate]], new Map([[source.number, [rawComments(source)]]]));
  await assert.rejects(() => discoverSnapshot(client), /duplicate issue/u);
  const duplicateBeforeFiltering = fakeClient([[rawIssue(source), rawIssue(source, { pullRequest: true })]], new Map([[source.number, [rawComments(source)]]]));
  await assert.rejects(() => discoverSnapshot(duplicateBeforeFiltering), /duplicate issue/u);
  const failing = { ...fakeClient([]), async listIssues() { throw new Error("API unavailable"); } };
  await assert.rejects(() => runLifecycle({ client: failing, scan: { state: "clean", eligible: true }, dryRun: false }), /API unavailable/u);
});

test("planner receives only the hardcoded trust boundary", async () => {
  const source = fixture.markedAcrossPages.issuePages[0][0];
  const client = fakeClient([[rawIssue(source)]], new Map([[source.number, [rawComments(source)]]]));
  const snapshot = await discoverSnapshot(client);
  const plan = planSecurityIssueLifecycle({ ...snapshot, scan: { state: "clean" } });
  assert.equal(plan.action, "no-op");
  assert.deepEqual(snapshot.trustedAuthors, ["github-actions[bot]"]);
});
