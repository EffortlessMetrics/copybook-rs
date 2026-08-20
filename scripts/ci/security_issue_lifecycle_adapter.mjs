// SPDX-License-Identifier: AGPL-3.0-or-later
/** Live GitHub adapter for the pure weekly security lifecycle planner. */

import { planSecurityIssueLifecycle } from "./security_issue_lifecycle.mjs";
import { pathToFileURL } from "node:url";

const SECURITY_LABEL = "security";
const CREATION_LABELS = ["security", "infrastructure"];
const TRUSTED_AUTHORS = ["github-actions[bot]"];
const PAGE_SIZE = 100;

function requireArray(value, label) {
  if (!Array.isArray(value)) throw new TypeError(`${label} must be an array`);
  return value;
}

function requirePositiveInteger(value, label) {
  if (!Number.isSafeInteger(value) || value <= 0) throw new TypeError(`${label} must be a positive safe integer`);
  return value;
}

function hasSecurityLabel(issue) {
  return requireArray(issue.labels, "issue.labels").some((label) =>
    typeof label === "string" ? label === SECURITY_LABEL : label !== null && typeof label === "object" && label.name === SECURITY_LABEL,
  );
}

function issueSnapshot(issue, commentPages) {
  if (issue === null || typeof issue !== "object" || Array.isArray(issue)) throw new TypeError("issue must be an object");
  const number = requirePositiveInteger(issue.number, "issue.number");
  const body = issue.body === null ? "" : issue.body;
  if (typeof issue.title !== "string" || typeof body !== "string") throw new TypeError("issue title and body must be strings or a null body");
  if (issue.state !== "open" && issue.state !== "closed") throw new TypeError("issue.state must be open or closed");
  const normalizedComments = commentPages.map((page, pageIndex) => requireArray(page, `comment page ${pageIndex}`).map((comment) => {
    if (comment === null || typeof comment !== "object" || Array.isArray(comment)) throw new TypeError("comment must be an object");
    return {
      id: requirePositiveInteger(comment.id, "comment.id"),
      author: typeof comment.user?.login === "string" ? comment.user.login : comment.author,
      body: comment.body,
    };
  }));
  return { number, title: issue.title, body, state: issue.state, commentPages: normalizedComments };
}

async function allPages(fetchPage, label) {
  const pages = [];
  for (let page = 1; ; page += 1) {
    const values = requireArray(await fetchPage(page), `${label} page ${page}`);
    pages.push(values);
    if (values.length < PAGE_SIZE) return pages;
  }
}

/** Build a fail-closed snapshot. The client is deliberately injected for tests. */
export async function discoverSnapshot(client) {
  if (client === null || typeof client !== "object") throw new TypeError("client is required");
  if (typeof client.listIssues !== "function" || typeof client.listComments !== "function") throw new TypeError("client pagination methods are required");
  const issuePages = [];
  const issueIdentifiers = new Set();
  const commentIdentifiers = new Set();
  for (let page = 1; ; page += 1) {
    const response = requireArray(await client.listIssues(page, PAGE_SIZE), `issue page ${page}`);
    const selected = [];
    for (const issue of response) {
      if (issue === null || typeof issue !== "object" || Array.isArray(issue)) throw new TypeError("issue must be an object");
      const number = requirePositiveInteger(issue.number, "issue.number");
      if (issueIdentifiers.has(number)) throw new TypeError(`snapshot contains duplicate issue number ${number}`);
      issueIdentifiers.add(number);
      if (issue.pull_request !== undefined) continue;
      if (!hasSecurityLabel(issue)) continue;
      const commentPages = await allPages(
        (commentPage) => client.listComments(number, commentPage, PAGE_SIZE),
        `comment page for issue ${number}`,
      );
      for (const page of commentPages) {
        for (const comment of page) {
          const identifier = requirePositiveInteger(comment?.id, "comment.id");
          if (commentIdentifiers.has(identifier)) throw new TypeError(`snapshot contains duplicate comment id ${identifier}`);
          commentIdentifiers.add(identifier);
        }
      }
      selected.push(issueSnapshot(issue, commentPages));
    }
    issuePages.push(selected);
    if (response.length < PAGE_SIZE) break;
  }
  return { issuePages, trustedAuthors: [...TRUSTED_AUTHORS] };
}

function requirePlan(plan) {
  if (plan === null || typeof plan !== "object" || !["create", "adopt", "update", "no-op", "close", "reopen"].includes(plan.action)) {
    throw new TypeError("planner returned an invalid closed action");
  }
  return plan;
}

/** Execute only the planner's closed action set, in deterministic write order. */
export async function executePlan(client, plan) {
  requirePlan(plan);
  if (plan.action === "no-op") return { action: plan.action, writes: 0, issueNumber: plan.issueNumber };
  let issueNumber = plan.issueNumber;
  let writes = 0;
  if (plan.action === "create") {
    if (issueNumber !== null || typeof plan.title !== "string" || typeof plan.issueBody !== "string") throw new TypeError("create plan is incomplete");
    const created = await client.createIssue({ title: plan.title, body: plan.issueBody, labels: [...CREATION_LABELS] });
    issueNumber = requirePositiveInteger(created?.number, "created issue number");
    writes += 1;
  } else {
    issueNumber = requirePositiveInteger(issueNumber, "planned issue number");
    if (plan.action === "adopt" || plan.action === "update") {
      if (typeof plan.issueBody === "string") {
        await client.updateIssue(issueNumber, { body: plan.issueBody });
        writes += 1;
      }
    } else if (plan.action === "close" || plan.action === "reopen") {
      await client.updateIssue(issueNumber, { state: plan.targetState });
      writes += 1;
    }
  }
  if (plan.commentBody !== null) {
    if (typeof plan.commentBody !== "string") throw new TypeError("comment body must be a string or null");
    await client.createComment(issueNumber, plan.commentBody);
    writes += 1;
  }
  return { action: plan.action, writes, issueNumber };
}

export async function runLifecycle({ client, scan, dryRun = true }) {
  if (scan === null || typeof scan !== "object" || scan.eligible !== true) throw new Error("security scan is not eligible for lifecycle planning");
  const snapshot = await discoverSnapshot(client);
  const { eligible: _eligible, audit_exit_status: _auditExitStatus, ...plannerScan } = scan;
  const plan = requirePlan(planSecurityIssueLifecycle({ ...snapshot, scan: plannerScan }));
  if (dryRun) return { plan, execution: { action: plan.action, writes: 0, issueNumber: plan.issueNumber } };
  return { plan, execution: await executePlan(client, plan) };
}

export function createGitHubClient({ token, owner, repo }) {
  if (typeof token !== "string" || token.length === 0 || typeof owner !== "string" || typeof repo !== "string") throw new TypeError("GitHub client context is incomplete");
  const headers = { accept: "application/vnd.github+json", authorization: `Bearer ${token}`, "x-github-api-version": "2022-11-28" };
  async function request(path, options = {}) {
    const response = await fetch(`https://api.github.com${path}`, { ...options, headers: { ...headers, ...(options.headers ?? {}) } });
    const body = await response.json().catch(() => null);
    if (!response.ok) throw new Error(`GitHub API ${response.status}: ${JSON.stringify(body)}`);
    return body;
  }
  return {
    listIssues: (page, perPage) => request(`/repos/${owner}/${repo}/issues?state=all&labels=${encodeURIComponent(SECURITY_LABEL)}&per_page=${perPage}&page=${page}`),
    listComments: (issueNumber, page, perPage) => request(`/repos/${owner}/${repo}/issues/${issueNumber}/comments?per_page=${perPage}&page=${page}`),
    createIssue: (payload) => request(`/repos/${owner}/${repo}/issues`, { method: "POST", body: JSON.stringify(payload), headers: { "content-type": "application/json" } }),
    updateIssue: (issueNumber, payload) => request(`/repos/${owner}/${repo}/issues/${issueNumber}`, { method: "PATCH", body: JSON.stringify(payload), headers: { "content-type": "application/json" } }),
    createComment: (issueNumber, body) => request(`/repos/${owner}/${repo}/issues/${issueNumber}/comments`, { method: "POST", body: JSON.stringify({ body }), headers: { "content-type": "application/json" } }),
  };
}

function booleanInput(value) {
  if (value === undefined) return true;
  if (value === "true" || value === true) return true;
  if (value === "false" || value === false) return false;
  throw new TypeError("dry-run must be boolean");
}

async function main() {
  const args = new Map();
  for (let index = 2; index < process.argv.length; index += 2) args.set(process.argv[index], process.argv[index + 1]);
  const state = args.get("--state");
  const scan = state === "clean"
    ? { state }
    : {
        state,
        findingCount: Number(args.get("--finding-count")),
        findingsFingerprint: args.get("--findings-fingerprint"),
        artifactName: args.get("--artifact-name"),
      };
  scan.eligible = args.get("--eligible") === "true";
  const client = createGitHubClient({ token: process.env.GITHUB_TOKEN, owner: process.env.GITHUB_REPOSITORY?.split("/")[0], repo: process.env.GITHUB_REPOSITORY?.split("/")[1] });
  const result = await runLifecycle({ client, scan, dryRun: booleanInput(args.get("--dry-run")) });
  console.log(JSON.stringify(result));
}

if (process.argv[1] !== undefined && import.meta.url === pathToFileURL(process.argv[1]).href) main().catch((error) => { console.error(`error: ${error.message}`); process.exitCode = 1; });
