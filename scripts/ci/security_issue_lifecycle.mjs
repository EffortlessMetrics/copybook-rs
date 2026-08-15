// SPDX-License-Identifier: AGPL-3.0-or-later
/** Pure lifecycle planning for the weekly dependency-findings roll-up issue. */

export const ROLLUP_TITLE = "Weekly Security Audit Findings";
export const ROLLUP_MARKER = "<!-- copybook-security-rollup:v1 -->";

const ROLLUP_MARKER_NAMESPACE = "copybook-security-rollup:";
const FINDINGS_MARKER_NAMESPACE = "copybook-security-findings:";
const FINGERPRINT_PATTERN = /^sha256:[0-9a-f]{64}$/;
const ARTIFACT_PATTERN = /^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$/;
const AUTHOR_PATTERN = /^[A-Za-z0-9][A-Za-z0-9-]*(?:\[bot\])?$/;
const FINDINGS_MARKER_PATTERN =
  /^<!-- copybook-security-findings:v1 fingerprint=(sha256:[0-9a-f]{64}) count=([1-9][0-9]*) -->$/;

function requireObject(value, label) {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    throw new TypeError(`${label} must be an object`);
  }
  return value;
}

function requireExactKeys(value, required, label) {
  const actual = Object.keys(value).sort();
  const expected = [...required].sort();
  if (actual.length !== expected.length || actual.some((key, index) => key !== expected[index])) {
    throw new TypeError(`${label} must contain exactly: ${expected.join(", ")}`);
  }
}

function requirePositiveInteger(value, label) {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new TypeError(`${label} must be a positive safe integer`);
  }
  return value;
}

function markerLines(body, namespace) {
  return body.split(/\r?\n/u).filter((line) => line.includes(namespace));
}

function parseRollupMarker(body, label) {
  const lines = markerLines(body, ROLLUP_MARKER_NAMESPACE);
  if (lines.length === 0) {
    return false;
  }
  if (lines.length !== 1 || lines[0] !== ROLLUP_MARKER) {
    throw new TypeError(`${label} contains a malformed or duplicate roll-up marker`);
  }
  return true;
}

function parseFindingsMarker(body, label) {
  const lines = markerLines(body, FINDINGS_MARKER_NAMESPACE);
  if (lines.length === 0) {
    return null;
  }
  if (lines.length !== 1) {
    throw new TypeError(`${label} contains duplicate findings markers`);
  }
  const match = FINDINGS_MARKER_PATTERN.exec(lines[0]);
  if (match === null) {
    throw new TypeError(`${label} contains a malformed findings marker`);
  }
  const count = Number(match[2]);
  if (!Number.isSafeInteger(count)) {
    throw new TypeError(`${label} findings count exceeds the safe integer range`);
  }
  return { fingerprint: match[1], count };
}

function validateScan(scanValue) {
  const scan = requireObject(scanValue, "snapshot.scan");
  if (scan.state === "clean") {
    requireExactKeys(scan, ["state"], "snapshot.scan clean state");
    return { state: "clean" };
  }
  if (scan.state !== "findings") {
    throw new TypeError("snapshot.scan.state must be clean or findings");
  }
  requireExactKeys(
    scan,
    ["artifactName", "findingCount", "findingsFingerprint", "state"],
    "snapshot.scan findings state",
  );
  if (!Number.isSafeInteger(scan.findingCount) || scan.findingCount <= 0) {
    throw new TypeError("snapshot.scan.findingCount must be a positive safe integer");
  }
  if (typeof scan.findingsFingerprint !== "string" || !FINGERPRINT_PATTERN.test(scan.findingsFingerprint)) {
    throw new TypeError("snapshot.scan.findingsFingerprint must be a lowercase sha256 fingerprint");
  }
  if (typeof scan.artifactName !== "string" || !ARTIFACT_PATTERN.test(scan.artifactName)) {
    throw new TypeError("snapshot.scan.artifactName is invalid or too long");
  }
  return { ...scan };
}

function validateTrustedAuthors(value) {
  if (!Array.isArray(value) || value.length === 0) {
    throw new TypeError("snapshot.trustedAuthors must be a non-empty array");
  }
  const trusted = new Set();
  for (const [index, author] of value.entries()) {
    if (typeof author !== "string" || author.length > 64 || !AUTHOR_PATTERN.test(author)) {
      throw new TypeError(`snapshot.trustedAuthors[${index}] is invalid`);
    }
    if (trusted.has(author)) {
      throw new TypeError(`snapshot.trustedAuthors contains duplicate author ${author}`);
    }
    trusted.add(author);
  }
  return trusted;
}

function flattenComments(issue, label, trustedAuthors) {
  if (!Array.isArray(issue.commentPages)) {
    throw new TypeError(`${label}.commentPages must be an array of pages`);
  }
  const comments = [];
  const identifiers = new Set();
  for (const [pageIndex, page] of issue.commentPages.entries()) {
    if (!Array.isArray(page)) {
      throw new TypeError(`${label}.commentPages[${pageIndex}] must be an array`);
    }
    for (const [commentIndex, commentValue] of page.entries()) {
      const commentLabel = `${label}.commentPages[${pageIndex}][${commentIndex}]`;
      const comment = requireObject(commentValue, commentLabel);
      requireExactKeys(comment, ["author", "body", "id"], commentLabel);
      const identifier = requirePositiveInteger(comment.id, `${commentLabel}.id`);
      if (identifiers.has(identifier)) {
        throw new TypeError(`${label} contains duplicate comment id ${identifier}`);
      }
      identifiers.add(identifier);
      if (typeof comment.author !== "string" || typeof comment.body !== "string") {
        throw new TypeError(`${commentLabel}.author and body must be strings`);
      }
      const marker = trustedAuthors.has(comment.author)
        ? parseFindingsMarker(comment.body, commentLabel)
        : null;
      comments.push({ ...comment, marker });
    }
  }
  return comments;
}

function flattenIssues(pagesValue, trustedAuthors) {
  if (!Array.isArray(pagesValue)) {
    throw new TypeError("snapshot.issuePages must be an array of pages");
  }
  const issues = [];
  const identifiers = new Set();
  for (const [pageIndex, page] of pagesValue.entries()) {
    if (!Array.isArray(page)) {
      throw new TypeError(`snapshot.issuePages[${pageIndex}] must be an array`);
    }
    for (const [issueIndex, issueValue] of page.entries()) {
      const label = `snapshot.issuePages[${pageIndex}][${issueIndex}]`;
      const issue = requireObject(issueValue, label);
      requireExactKeys(issue, ["body", "commentPages", "number", "state", "title"], label);
      const number = requirePositiveInteger(issue.number, `${label}.number`);
      if (identifiers.has(number)) {
        throw new TypeError(`snapshot contains duplicate issue number ${number}`);
      }
      identifiers.add(number);
      if (issue.state !== "open" && issue.state !== "closed") {
        throw new TypeError(`${label}.state must be open or closed`);
      }
      if (typeof issue.title !== "string" || typeof issue.body !== "string") {
        throw new TypeError(`${label}.title and body must be strings`);
      }
      issues.push({
        ...issue,
        number,
        marked: parseRollupMarker(issue.body, label),
        comments: flattenComments(issue, label, trustedAuthors),
      });
    }
  }
  return issues;
}

function findingsComment(scan) {
  return `<!-- copybook-security-findings:v1 fingerprint=${scan.findingsFingerprint} count=${scan.findingCount} -->\n\n${scan.findingCount} vulnerabilities detected. See raw cargo-audit artifact \`${scan.artifactName}\`.`;
}

function basePlan(action, issueNumber, reason) {
  return {
    version: 1,
    action,
    issueNumber,
    targetState: null,
    title: null,
    issueBody: null,
    commentBody: null,
    reason,
  };
}

function latestMarker(issue) {
  const markers = issue.comments.filter((comment) => comment.marker !== null);
  if (markers.length === 0) {
    return null;
  }
  return markers.reduce((latest, comment) => (comment.id > latest.id ? comment : latest)).marker;
}

function sameFindings(marker, scan) {
  return marker !== null && marker.fingerprint === scan.findingsFingerprint && marker.count === scan.findingCount;
}

/** Return one deterministic, side-effect-free lifecycle action. */
export function planSecurityIssueLifecycle(snapshotValue) {
  const snapshot = requireObject(snapshotValue, "snapshot");
  requireExactKeys(snapshot, ["issuePages", "scan", "trustedAuthors"], "snapshot");
  const scan = validateScan(snapshot.scan);
  const trustedAuthors = validateTrustedAuthors(snapshot.trustedAuthors);
  const issues = flattenIssues(snapshot.issuePages, trustedAuthors);
  const candidates = issues.filter((issue) => issue.marked || issue.title === ROLLUP_TITLE);
  if (candidates.length > 1) {
    throw new TypeError("snapshot contains multiple canonical or legacy roll-up candidates");
  }

  const issue = candidates[0];
  if (issue === undefined) {
    if (scan.state === "clean") {
      return basePlan("no-op", null, "clean-without-rollup");
    }
    return {
      ...basePlan("create", null, "findings-without-rollup"),
      targetState: "open",
      title: ROLLUP_TITLE,
      issueBody: `${ROLLUP_MARKER}\n\nManaged weekly dependency findings roll-up.`,
      commentBody: findingsComment(scan),
    };
  }

  if (scan.state === "clean") {
    if (issue.state === "closed") {
      return basePlan("no-op", issue.number, "clean-rollup-already-closed");
    }
    return {
      ...basePlan("close", issue.number, "clean-rollup-open"),
      targetState: "closed",
    };
  }

  const repeated = sameFindings(latestMarker(issue), scan);
  if (!issue.marked) {
    const existing = issue.body.trimEnd();
    return {
      ...basePlan("adopt", issue.number, "findings-with-legacy-rollup"),
      targetState: "open",
      issueBody: `${existing}${existing.length === 0 ? "" : "\n\n"}${ROLLUP_MARKER}`,
      commentBody: findingsComment(scan),
    };
  }
  if (issue.state === "closed") {
    return {
      ...basePlan("reopen", issue.number, "findings-with-closed-rollup"),
      targetState: "open",
      commentBody: repeated ? null : findingsComment(scan),
    };
  }
  if (repeated) {
    return basePlan("no-op", issue.number, "findings-already-recorded");
  }
  return {
    ...basePlan("update", issue.number, "findings-changed"),
    commentBody: findingsComment(scan),
  };
}
