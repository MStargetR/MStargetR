// One-shot smoke test: feed a synthetic commit through
// conventional-changelog-writer with the writerOpts.headerPartial from
// .releaserc.json, and assert the rendered NEWS.md fragment matches the
// pkgdown-friendly form (## MStargetR X.Y.Z + sub-line). Not registered
// in any npm script — run manually if you change the template:
//
//   node scripts/test-changelog-header.mjs
//
// Exits 0 on success, 1 on mismatch.
import { readFileSync } from 'node:fs';
import { writeChangelogString } from 'conventional-changelog-writer';

const cfg = JSON.parse(
  readFileSync(new URL('../.releaserc.json', import.meta.url), 'utf8'),
);
const rng = cfg.plugins.find(
  (p) =>
    Array.isArray(p) && p[0] === '@semantic-release/release-notes-generator',
);
if (!rng) {
  console.error('FAIL: release-notes-generator plugin entry not found');
  process.exit(1);
}
const writerOpts = rng[1].writerOpts;

const fakeCommits = [
  {
    type: 'fix',
    scope: 'demo',
    subject: 'render check',
    hash: 'deadbeefcafebabe',
    notes: [],
    references: [],
    mentions: [],
    revert: null,
    header: 'fix(demo): render check',
    body: null,
    footer: null,
  },
];

const context = {
  version: '9.9.9',
  date: '2099-12-31',
  previousTag: 'v9.9.8',
  currentTag: 'v9.9.9',
  linkCompare: true,
  host: 'https://github.com',
  owner: 'MStargetR',
  repository: 'MStargetR',
  repoUrl: 'https://github.com/MStargetR/MStargetR',
  isPatch: true,
  commitGroups: [{ title: 'Bug Fixes', commits: fakeCommits }],
  noteGroups: [],
};

const out = await writeChangelogString(fakeCommits, context, {
  ...writerOpts,
  transform: (c) => c,
});

const wantHeader = '## MStargetR 9.9.9';
const wantSub =
  'Released 2099-12-31 ([compare](https://github.com/MStargetR/MStargetR/compare/v9.9.8...v9.9.9)).';

let pass = true;
if (!out.startsWith(wantHeader)) {
  console.error(`FAIL: header missing. Got first line: ${out.split('\n')[0]}`);
  pass = false;
}
if (!out.includes(wantSub)) {
  console.error(`FAIL: sub-line missing. Got:\n${out}`);
  pass = false;
}

if (pass) {
  console.log('OK: rendered header is pkgdown-compatible');
  console.log('--- rendered output ---');
  console.log(out);
  process.exit(0);
}
process.exit(1);
