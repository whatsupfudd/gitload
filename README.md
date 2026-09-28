# gitload

**Repository-history ingestion and progress intelligence for the FUDD
ecosystem.**

`gitload` is a Haskell utility for discovering Git repositories, reading
their commit histories, and loading normalized repository-history data into
PostgreSQL.

Its current implementation provides the first layer of a broader
**repository-intelligence and SPPM lifecycle system**.

At present:

```text
filesystem
    |
    v
discover .git directories
    |
    v
identify known repositories
    |
    v
libgit2 / gitlib
    |
    v
commit history
    |
    +-- commit object ID
    +-- author identity
    +-- author timestamp
    `-- commit message
    |
    v
PostgreSQL
```

The longer-term objective is to turn Git history into useful evidence about
software evolution:

```text
Git repositories
       |
       v
repository history
       |
       v
normalized change evidence
       |
       +------> weekly progress
       +------> milestones
       +------> active work areas
       +------> releases
       +------> implementation traceability
       +------> dependency evolution
       +------> SPPM indicators
       `------> FUDD / 0to1,Done project records
```

`gitload` should therefore be understood as a **repository-history ingestion
and intelligence utility**, not simply as a prettier implementation of:

```bash
git log
```

The project is currently at an early development stage.

The implemented code concentrates on:

- repository discovery;
- commit-log extraction;
- repository identity;
- author/committer registration; and
- PostgreSQL ingestion.

Most higher-level progress analysis described in this README remains
roadmap work.

The current package version is:

```text
0.1.0.0
```

---

## Contents

- [Role in the FUDD ecosystem](#role-in-the-fudd-ecosystem)
- [Why gitload exists](#why-gitload-exists)
- [Current implementation](#current-implementation)
- [Target architecture](#target-architecture)
- [Getting started](#getting-started)
- [Configuration](#configuration)
- [Command-line interface](#command-line-interface)
- [Repository discovery](#repository-discovery)
- [Repository identity](#repository-identity)
- [Git history extraction](#git-history-extraction)
- [Commit model](#commit-model)
- [Database model](#database-model)
- [Database initialization](#database-initialization)
- [Commit ingestion](#commit-ingestion)
- [Idempotency](#idempotency)
- [Progress intelligence](#progress-intelligence)
- [Weekly reporting](#weekly-reporting)
- [Milestones and implementation evidence](#milestones-and-implementation-evidence)
- [SPPM role](#sppm-role)
- [Relationship with 0to1,Done](#relationship-with-0to1done)
- [Relationship with other FUDD tooling](#relationship-with-other-fudd-tooling)
- [Target repository model](#target-repository-model)
- [Change-level intelligence](#change-level-intelligence)
- [Identity model](#identity-model)
- [Module map](#module-map)
- [Testing strategy](#testing-strategy)
- [Performance and scalability](#performance-and-scalability)
- [Development roadmap](#development-roadmap)
- [Design principles](#design-principles)
- [Current limitations](#current-limitations)
- [Repository housekeeping](#repository-housekeeping)
- [License](#license)

---

# Role in the FUDD ecosystem

Software development produces large amounts of historical evidence.

Git records:

```text
commits
authors
timestamps
branches
tags
messages
file changes
merges
releases
```

but that information is normally consumed interactively and one repository
at a time.

For a large ecosystem such as FUDD, the questions are broader:

```text
What was worked on this week?

Which repositories were active?

Which projects moved toward a milestone?

When was a feature actually implemented?

Which repository contains the implementation?

Which projects have become inactive?

What code changes correspond to an architectural discussion?

Which changes introduced or removed a dependency?

Which repositories contributed to a larger product milestone?
```

`gitload` provides a shared repository-history layer from which those
questions can eventually be answered.

Conceptually:

```text
                  FUDD repositories
                         |
                         v
                     gitload
                         |
              +----------+----------+
              |                     |
              v                     v
         raw Git facts       normalized identities
              |                     |
              +----------+----------+
                         |
                         v
                  history database
                         |
       +-----------------+------------------+
       |                 |                  |
       v                 v                  v
 weekly progress     milestones       traceability
       |                 |                  |
       +-----------------+------------------+
                         |
                         v
                 FUDD project status
```

The history database should remain evidence-oriented.

Higher-level conclusions can then evolve independently from the Git
ingestion mechanism.

---

# Why gitload exists

Git already provides excellent repository-local commands:

```bash
git log
git show
git diff
git branch
git tag
```

The problem solved by `gitload` is different.

FUDD contains many repositories spread across a larger development
workspace.

A human trying to understand progress across all of them should not need to:

```text
find every repository manually

run git log in each repository

normalize author names manually

merge timelines manually

remember which directory corresponds to which FUDD project

reconstruct weekly activity by hand
```

Instead:

```text
workspace
   |
   v
gitload scan
   |
   v
repositories
   |
   v
gitload ingest
   |
   v
central history store
   |
   v
report / analyse / correlate
```

The central history store also makes repeated analysis much cheaper.

Git history can be ingested once and queried many times.

---

# Current implementation

The implemented system currently consists of four main stages.

```text
1. DISCOVER

directory tree
    |
    v
.git directories


2. IDENTIFY

repository path
    |
    v
logical repository name


3. READ

repository
    |
    v
HEAD
    |
    v
reachable commits


4. STORE

repository
author identity
commit
    |
    v
PostgreSQL
```

---

## Current capability status

| Capability | Status |
| --- | --- |
| Recursive repository discovery | Implemented |
| Detect `.git` directories | Implemented |
| Avoid recursion into discovered `.git` directories | Implemented |
| Concurrent subtree scanning | Implemented |
| Logical repository naming | Implemented through static path map |
| Open Git repository | Implemented |
| Resolve `HEAD` | Implemented |
| Walk commits reachable from `HEAD` | Implemented |
| Commit OID extraction | Implemented |
| Author name/email extraction | Implemented |
| Author timestamp extraction | Implemented |
| Full commit message extraction | Implemented |
| Repository table persistence | Implemented |
| Author/committer persistence | Implemented |
| Commit persistence | Implemented |
| Skip already-ingested commit IDs | Implemented |
| Repository scan command | Implemented |
| Repository history display helper | Implemented, not CLI-exposed |
| Initialize repositories | Implemented |
| Initialize contributor identities | Implemented |
| Incremental ingest | Basic implementation |
| Generic repository naming | Not implemented |
| Remote-origin discovery | Not implemented |
| Branch/ref ingestion | Not implemented |
| Tag ingestion | Not implemented |
| Parent relationships | Not implemented |
| Merge structure | Not persisted |
| File-level changes | Not implemented |
| Diff statistics | Not implemented |
| Releases | Not implemented |
| Weekly reporting | Roadmap |
| Milestone tracking | Roadmap |
| Project-status integration | Roadmap |
| Automated tests | Not implemented |

---

# Target architecture

The implemented commit loader is only the evidence-collection layer of the
intended system.

A more complete architecture is:

```text
                    development workspace
                            |
                            v
                    repository discovery
                            |
                            v
                  repository registry
                            |
                            v
                       Git history
                            |
              +-------------+-------------+
              |             |             |
              v             v             v
           commits         refs         tags
              |
              v
          changesets
              |
        +-----+------+----------------+
        |            |                |
        v            v                v
    files        components        projects
        |            |                |
        +------------+----------------+
                     |
                     v
             repository intelligence
                     |
       +-------------+-------------+
       |             |             |
       v             v             v
    weekly       milestones      evidence
    progress                      links
       |             |             |
       +-------------+-------------+
                     |
                     v
                 FUDD SPPM
```

The key separation is:

```text
Git facts
```

versus:

```text
interpretation of Git facts
```

A commit is evidence.

Whether it constitutes progress toward a milestone is a higher-level
conclusion.

---

# Getting started

## Requirements

The current Stack project uses:

```text
Stack snapshot: LTS 22.44
GHC:            9.6.7
system-ghc:     true
```

Important dependencies include:

```text
gitlib
gitlib-libgit2
Hasql
Hasql.TH
Hasql Pool
postgresql-binary
pathwalk
async
aeson
yaml
optparse-applicative
gitrev
```

You also need:

```text
PostgreSQL
libgit2
```

for the primary implemented workflows.

---

## Native libgit2 configuration

The current `stack.yaml` contains:

```yaml
extra-deps:
  - gitlib-libgit2-3.1.2.1

extra-include-dirs:
  - /opt/homebrew/include

extra-lib-dirs:
  - /opt/homebrew/lib
```

Those paths correspond to the common Homebrew prefix on Apple Silicon
macOS.

Developers on Linux, Intel macOS, or another development environment may
need to adjust or remove these paths according to their local `libgit2`
installation.

A future build configuration should avoid machine-specific paths where
possible.

---

## Clone

```bash
git clone git@github.com:whatsupfudd/gitload.git
cd gitload
```

---

## Build

```bash
stack build
```

---

## CLI help

```bash
stack exec -- gitload --help
```

The current commands are:

```text
help
version
scan
init
ingest
```

---

# Configuration

The implemented default configuration path is:

```text
~/.fudd/gitload/config.yaml
```

An alternate path can be supplied using:

```text
--config
-c
```

or:

```text
gitloadCONF
```

The application also reads:

```text
gitloadHOME
```

although application-home information is not currently used by the
repository processing logic.

---

## Example configuration

```yaml
debug: 0

db:
  host: "127.0.0.1"
  port: 5432
  user: "gitload"
  passwd: "development-password"
  dbase: "gitload"
  poolSize: 5
  poolTimeOut: 60
```

The current configuration merge applies:

```text
debug

db.host
db.port
db.user
db.passwd
db.dbase
```

The YAML model also exposes:

```text
db.poolSize
db.poolTimeOut
```

but those fields are not currently propagated into the runtime database
configuration.

The compiled pool defaults therefore remain in effect.

---

## Database defaults

If no database settings override them, the current defaults are development
values:

```text
host      = test
port      = 5432
user      = test
password  = test
database  = test

pool size           = 5
acquisition timeout = 5 seconds
pool timeout        = 60 seconds
idle timeout        = 300 seconds
```

Production deployments should provide explicit database configuration.

---

## Configuration-path inconsistency

The CLI help currently states:

```text
~/.gitload/config.yaml
```

while the implementation uses:

```text
~/.fudd/gitload/config.yaml
```

The implementation path is the effective value at present.

This discrepancy should be corrected.

---

# Command-line interface

Use the standard FUDD Stack invocation convention:

```bash
stack exec -- gitload <command> <arguments>
```

---

## `version`

```bash
stack exec -- gitload version
```

Reports:

```text
package version
Git revision
Git commit date
```

---

## `help`

```bash
stack exec -- gitload help
```

The current command itself is only a placeholder.

Use:

```bash
stack exec -- gitload --help
```

for the generated CLI help.

---

## `scan`

Scan a directory tree for Git repositories:

```bash
stack exec -- gitload scan /path/to/workspace
```

The command prints entries conceptually like:

```text
fudd_easywordy => /workspace/Fudd/EasyWordy/easywordy/.git
fudd_migrator  => /workspace/Fudd/migrator/.git
Nothing        => /workspace/other/project/.git
```

`Nothing` means that a Git repository was found but its path is not present
in the current logical repository-name mapping.

No PostgreSQL connection is required by the scan logic itself, although the
current application startup still requires its configuration file before
command dispatch.

---

## `init`

The current command syntax is:

```bash
stack exec -- gitload init <CMD> <PATH>
```

Two initialization operations are implemented.

### Initialize repositories

```bash
stack exec -- gitload init repos /path/to/workspace
```

This:

```text
scans for Git repositories
        |
        v
maps recognized paths to logical names
        |
        v
loads existing repos table
        |
        v
inserts missing repositories
```

---

### Initialize contributor identities

```bash
stack exec -- gitload init committers /path/to/workspace
```

This:

```text
discovers repositories
       |
       v
reads commit histories
       |
       v
collects author identities
       |
       v
loads existing identities
       |
       v
inserts missing identities
```

Only:

```text
repos
committers
```

are currently recognized values for `<CMD>`.

The CLI parser itself accepts arbitrary text and reports unknown values only
when executing the command.

---

## `ingest`

```bash
stack exec -- gitload ingest /path/to/workspace
```

This is the main current operational command.

It:

```text
loads repositories from PostgreSQL
        |
        v
loads known contributors
        |
        v
discovers Git repositories
        |
        v
reads each recognized commit history
        |
        v
creates missing repositories
        |
        v
loads already-stored commits
        |
        v
for each unseen commit:
        |
        +--> locate/create contributor
        |
        `--> insert commit log
```

The result is an incremental central commit-history database.

---

# Repository discovery

Repository discovery lives in:

```text
FileSystem.Explore
```

The current implementation recursively scans a directory tree looking for
directories whose basename is exactly:

```text
.git
```

A discovered `.git` directory is added to the results.

The scanner does **not recurse into it**.

This is important because traversing Git's internal object database would
add large amounts of useless filesystem work.

---

## Recursive scanning

Conceptually:

```text
workspace
 |
 +-- repo-A
 |    |
 |    `-- .git       <-- match; do not descend
 |
 +-- projects
 |    |
 |    +-- repo-B
 |    |    `-- .git  <-- match
 |    |
 |    `-- docs
 |
 `-- repo-C
      `-- .git       <-- match
```

returns:

```text
repo-A/.git
projects/repo-B/.git
repo-C/.git
```

---

## Concurrent traversal

Sibling directory trees are scanned using:

```haskell
forConcurrently
```

which permits independent portions of the filesystem to be explored in
parallel.

The source currently contains an intended:

```text
maxThreads = getNumCapabilities
```

value, but that value is not actually used to bound `forConcurrently`.

Consequently, the present implementation should be described as
**concurrent**, not as strictly bounded to the number of GHC capabilities.

For very large directory trees, bounded concurrency would be preferable.

---

## Error isolation

Filesystem `IOException`s encountered while recursively scanning a subtree
are currently converted into an empty result for that subtree.

This means one inaccessible directory does not necessarily abort the whole
scan.

The top-level operation still reports a structured error when the initial
scan itself fails.

A future scanner should retain diagnostics for skipped/inaccessible
subtrees rather than silently losing them.

---

# Repository identity

Git's filesystem location is not always the logical identity that FUDD wants
to use.

The current implementation therefore converts a discovered path through:

```haskell
pathToRepoName
```

to a logical repository name.

For example, known paths map to names such as:

```text
fudd_easywordy
fudd_phpparse
fudd_migrator
fudd_cannelle
fudd_daniell
fudd_gitload
fudd_haskell_sqlddl
...
```

---

## Current repository registry

The mapping is currently implemented as a large Haskell function containing
hard-coded path suffixes.

Conceptually:

```haskell
pathToRepoName' path
  | ".../EasyWordy/easywordy" `isSuffixOf` path =
      Just "fudd_easywordy"

  | ".../Fudd/migrator" `isSuffixOf` path =
      Just "fudd_migrator"

  | ...

  | otherwise =
      Nothing
```

This reflects the early deployment environment.

It is **not yet a general repository-discovery mechanism**.

---

## Why logical identity is still useful

The idea behind logical names is valid even though the implementation should
change.

A repository can:

```text
move on disk
be cloned into another workspace
have several working copies
change remote host
```

without necessarily becoming a new logical FUDD project.

The durable architecture should therefore distinguish:

```text
repository identity
```

from:

```text
working-copy path
```

and:

```text
remote URL
```

---

## Target repository registry

The hard-coded path table should evolve toward a registry containing
information such as:

```text
repository ID
logical name
project/workstream
working-copy paths
remote URLs
default branch
status
aliases
parent ecosystem
```

Possible sources include:

```text
configuration
database registry
remote origin
FUDD project metadata
```

A static Haskell source table should no longer be required when this
registry exists.

---

# Git history extraction

Git operations are implemented through:

```text
gitlib
gitlib-libgit2
```

rather than shelling out to:

```bash
git log
```

for each repository.

This provides a typed Haskell boundary around Git history.

---

## Current history walk

For each repository:

```text
repository path
     |
     v
open repository
     |
     v
resolve HEAD
     |
     v
list commits reachable from HEAD
     |
     v
lookup each commit
     |
     v
CommitInfo
```

If:

```text
HEAD
```

cannot be resolved, the operation returns an error.

---

## Current extracted fields

For every commit the code extracts:

```text
object ID
author name
author email
author timestamp
full commit log message
```

The author string is normalized as:

```text
Name <email@example.com>
```

before entering the database identity layer.

---

## What is not currently extracted

The implementation does not yet retain:

```text
committer identity
committer timestamp
parent commit IDs
merge-parent structure
tree ID
branch/ref membership
tags
signatures
changed files
insertions/deletions
patch
diff
```

Those are natural next layers of repository intelligence.

---

# Commit model

The current core type is intentionally small:

```haskell
data CommitInfo = CommitInfo
  { oidCI    :: Text
  , authorCI :: Text
  , timeCI   :: UTCTime
  , msgCI    :: Text
  }
```

Conceptually:

```text
CommitInfo
 |
 +-- object ID
 +-- author identity
 +-- authored timestamp
 `-- full message
```

This is sufficient for initial timeline and activity analysis.

It is not yet enough for complete change-level reasoning.

---

## Object IDs

The current comment describes `oidCI` as SHA-1.

The implementation itself stores Git's rendered object ID as:

```text
Text
```

which is the better long-term abstraction.

The persistence layer should continue to treat commit IDs as opaque Git
object identifiers rather than baking a fixed digest width into the
database model.

---

# Author versus committer

An important terminology distinction exists in the current code.

Git commits have both:

```text
author
committer
```

identities.

The current implementation reads:

```text
commitAuthor
```

and its timestamp.

However, the PostgreSQL table and several function names call these
identities:

```text
committers
```

For example:

```text
authorCI
    ->
getCommitter
    ->
committers
```

The stored identity is therefore currently the **Git author**, despite the
database terminology.

This should be clarified before richer identity analysis is added.

A future model should retain both when useful:

```text
author
    who originally created the change

committer
    who created this Git commit object / applied the change
```

---

# Database model

The current Hasql statements imply a small normalized history database.

---

## `repos`

Required columns include:

```text
uid   int4
name  text
```

Conceptually:

```text
repos
-----

uid
logical repository name
```

---

## `committers`

Required columns include:

```text
uid   int4
name  text
```

The current `name` contains the normalized author identity:

```text
Name <email>
```

As noted above, the table name does not accurately describe the current
data.

---

## `commitlogs`

Required columns include:

```text
uid          int4
repo_fk      int4
committer_fk int4
cid          text
createdAt    timestamptz
logmsg       text
```

The current effective relationship is:

```text
repos
  |
  `-- commitlogs
        |
        `-- committers/authors
```

---

## Current relational model

Conceptually:

```text
+-------------+
|    repos    |
+-------------+
| uid         |
| name        |
+------+------+
       |
       | repo_fk
       v
+-------------+
| commitlogs  |
+-------------+
| uid         |
| cid         |
| createdAt   |
| logmsg      |
| repo_fk     |
| committer_fk|
+------+------+
       |
       | committer_fk
       v
+-------------+
| committers  |
+-------------+
| uid         |
| name        |
+-------------+
```

The repository does not currently contain a complete migration/schema
definition for creating these tables.

Provisioning therefore needs to be handled externally.

---

# Database initialization

The explicit initialization commands exist primarily to populate the
repository and identity reference tables.

---

## Repositories

```bash
stack exec -- gitload init repos /workspace
```

loads existing repositories, scans the workspace, and inserts missing known
logical repository names.

---

## Contributors

```bash
stack exec -- gitload init committers /workspace
```

walks all recognized histories, deduplicates observed author strings in
memory, loads identities already present in PostgreSQL, and inserts missing
ones.

---

## Is initialization mandatory?

The main:

```text
ingest
```

path already has logic to create missing repositories and author identities.

The explicit `init` commands are therefore useful bootstrap and inspection
operations rather than an absolute prerequisite for every ingest.

As the lifecycle model becomes clearer, these operations may be consolidated
into one repository-registry synchronization step.

---

# Commit ingestion

The principal persistence workflow is:

```haskell
ingestCmd
```

It begins by loading:

```text
known repositories
known contributor identities
```

into maps keyed by their logical names.

It then discovers filesystem repositories and extracts each history.

---

## Repository-local existing-commit lookup

Before adding commits for a repository, `gitload` fetches the existing
commit rows and converts them into:

```text
Map commitOID RawCommitLog
```

This makes presence checks inexpensive during the current ingestion run.

For each observed Git commit:

```text
OID already present?
       |
       +-- yes -> skip
       |
       `-- no
            |
            +--> resolve contributor
            |
            `--> insert commit
```

This provides the first form of incremental synchronization.

---

# Idempotency

The desired ingest property is:

```text
ingest same unchanged repository twice
    ->
second ingest adds no duplicate commits
```

The current implementation achieves this at the commit level by looking up
the repository's stored:

```text
cid
```

values before inserting.

A database-level unique constraint should reinforce this property.

A useful target constraint would conceptually guarantee uniqueness of:

```text
(repo_fk, cid)
```

even if two ingestion processes run concurrently.

Application-side checks alone are not a sufficient concurrency guarantee.

---

# Progress intelligence

Commit ingestion is not the final purpose of Gitload.

The intended next layer is to transform repository-history facts into
**progress evidence**.

Examples include:

```text
repository was active during this period

these areas of the ecosystem received work

a milestone has implementation evidence

a release was prepared

a component was introduced or removed

a repository became inactive

a previously discussed architecture appeared in code
```

The important word is:

```text
evidence
```

Gitload should provide factual development evidence.

It should not infer developer quality from simplistic commit-count metrics.

---

# Weekly reporting

One useful target reporting model is a fixed weekly interval:

```text
Monday
   |
   +-- repository A
   |     +-- commit
   |     +-- commit
   |     `-- commit
   |
   +-- repository B
   |     +-- commit
   |     `-- commit
   |
   ...
   |
Sunday
```

This gives project managers and developers a compact answer to:

```text
What did the ecosystem work on this week?
```

without manually inspecting many repositories.

---

## Target weekly model

Conceptually:

```text
WeekBlock
 |
 +-- start
 +-- end
 |
 `-- RepoBlock
       |
       +-- repository
       |
       `-- CommitEntry
             |
             +-- timestamp
             +-- author
             +-- summary
             `-- commit identity
```

The existing database already contains enough information to build the first
simple form of this report.

Later versions can add:

```text
files changed
component tags
milestones
tests
releases
linked project records
```

---

## Reporting philosophy

A useful Gitload report should optimize for:

```text
clarity
density
traceability
```

rather than vanity statistics.

Prefer:

```text
which work happened
where it happened
when it happened
what it relates to
```

over:

```text
developer X made 17 commits
developer Y changed 4,000 lines
```

Commit frequency and line counts can be operational signals, but they should
not be treated as individual productivity scores.

---

# Milestones and implementation evidence

Git history becomes more valuable when connected to project intent.

For example:

```text
architecture discussion
        |
        v
planned implementation
        |
        v
Git commits
        |
        v
tests
        |
        v
release
```

A mature Gitload system should help preserve those relationships.

---

## Evidence links

A future commit or change set may be associated with:

```text
project
workstream
milestone
issue/task
architecture decision
conversation/reference
release
test evidence
deployment
```

This creates a chain such as:

```text
"Implement schema diff"
       |
       v
0to1,Done task
       |
       v
commit abc123
       |
       v
migrator repository
       |
       v
tests / release
```

Git then becomes one part of a wider evidence graph.

---

# SPPM role

Within FUDD, Gitload is intended to support **SPPM**:

```text
Security
Productivity
Performance
Maintainability
```

Repository history can provide evidence relevant to all four dimensions.

---

## Security

Potential repository-history inputs include:

```text
security fixes
dependency changes
secret-removal events
security-test additions
authentication/authorization changes
```

Gitload should record the evidence.

Specialized security tooling should decide the security meaning.

---

## Productivity

Useful signals include:

```text
active workstreams
milestones receiving implementation
time between plan and implementation
repository activity
release cadence
blocked/inactive work
```

The goal is understanding flow, not ranking humans.

---

## Performance

History can show when:

```text
benchmark infrastructure appeared
performance regressions were addressed
hardware/runtime changes occurred
optimization work concentrated
```

Actual performance conclusions must still come from benchmark data.

---

## Maintainability

Useful repository signals can include:

```text
test-suite growth
refactoring
dependency movement
module churn
documentation changes
repeated hot spots
repository fragmentation/consolidation
```

Again, Gitload supplies historical evidence rather than declaring one metric
to be "maintainability."

---

# Relationship with 0to1,Done

A longer-term integration target is the 0to1,Done execution and knowledge
system.

Conceptually:

```text
0to1,Done
    |
    +-- task / objective
    |
    +-- design / decision
    |
    +-- implementation activity
    |       |
    |       v
    |    Gitload
    |       |
    |       v
    |     commit
    |
    +-- validation
    |
    `-- Done
```

This allows completion to be backed by evidence rather than only by manually
changing a status field.

Possible evidence includes:

```text
relevant commits
tests
generated artifacts
releases
deployment records
```

Gitload should provide the Git portion of that evidence.

It should not itself decide that a project objective is complete.

---

# Relationship with other FUDD tooling

## Migrator

Migrator also needs Git history, but for a more specialized purpose:

```text
schema revision A
       |
       v
schema revision B
       |
       v
semantic schema evolution
```

Gitload can provide generic repository-history metadata.

Migrator remains responsible for:

```text
file-at-revision reconstruction
SqlDdl schema parsing
semantic schema comparison
migration planning
```

These responsibilities should not be folded into Gitload.

---

## Recycler

Recycler may use repository history to understand:

```text
how a legacy application evolved
when architecture changed
which code is historical/dead/recent
where framework transitions occurred
```

Gitload can supply temporal evidence while Recycler performs language and
application analysis.

---

## Daniell

Daniell focuses on understanding and transforming project structures.

Gitload can provide:

```text
repository age
change history
active components
historical snapshots
```

without duplicating Daniell's project parsing or execution-planning logic.

---

## FUDD ecosystem status

A future ecosystem registry can combine:

```text
repository activity
build status
tests
release status
milestones
dependencies
blockers
documentation
```

Gitload should own the repository-history portion of that model.

---

# Target repository model

The current database model is deliberately minimal.

A more durable model will likely need several concepts.

---

## Repository

```text
Repository
 |
 +-- stable identity
 +-- logical name
 +-- remote URLs
 +-- working copies
 +-- default branch
 +-- ecosystem/project
 +-- status
 `-- metadata
```

---

## Git identity

A person can use several Git identities:

```text
Alice <alice@example.com>
Alice Example <alice@company.com>
aexample <12345+alice@users.noreply.github.com>
```

The source identities should remain immutable evidence.

A separate canonical-person mapping can later group them when appropriate.

---

## Commit

A richer future representation should contain:

```text
object ID
tree ID

author identity
author timestamp

committer identity
committer timestamp

parents

message
```

This preserves actual Git semantics.

---

## Reference

```text
branch
tag
remote ref
HEAD
```

should eventually become first-class observations.

---

## Change set

Each commit can eventually be associated with:

```text
files added
files deleted
files modified
renames
binary changes
line statistics
language/component
```

This provides the bridge from:

```text
commit happened
```

to:

```text
what changed?
```

---

# Change-level intelligence

Commit counts are coarse.

The next useful layer is understanding the scope of a commit.

For example:

```text
commit abc123
 |
 +-- src/GitLog/Opers.hs
 +-- README.md
 `-- test/Spec.hs
```

can be classified approximately as:

```text
implementation
documentation
tests
```

without needing full language-semantic analysis.

Later, FUDD language tools can provide deeper interpretation.

---

## Do not store only line counts

Metrics such as:

```text
+150
-80
```

are useful storage facts but weak semantic measures.

Deleting 1,000 lines of obsolete code may be valuable progress.

Adding 5,000 generated lines may contain little human implementation work.

The repository model should preserve raw statistics while higher-level
analysis uses richer context.

---

# Identity model

The current:

```text
Name <email>
```

key is sufficient for initial ingestion.

It should not become the permanent canonical person identifier.

A future structure should distinguish:

```text
GitIdentity
    exact source identity

Person
    optional canonical human/entity

IdentityLink
    evidence linking one to the other
```

For example:

```text
GitIdentity A ----\
                   \
GitIdentity B ------> Person Alice
                   /
GitIdentity C ----/
```

Canonicalization should be explicit and reversible.

The raw Git identity must remain available for provenance.

---

# Module map

## Commands

| Module | Responsibility |
| --- | --- |
| `Commands.Scan` | Repository discovery and diagnostic output |
| `Commands.Init` | Initialize repository and contributor tables |
| `Commands.Ingest` | Incremental commit-history ingestion |
| `Commands.Version` | Package/Git build information |
| `Commands.Help` | Placeholder help command |
| `Commands` | Command aggregation |

---

## Git

| Module | Responsibility |
| --- | --- |
| `GitLog.Types` | Minimal `CommitInfo` representation |
| `GitLog.Opers` | libgit2 repository access, commit extraction, repository-name mapping |
| `GitLog.InitDb` | Additional repository bootstrapping helper |

---

## Filesystem

| Module | Responsibility |
| --- | --- |
| `FileSystem.Explore` | Recursive concurrent `.git` discovery |

---

## PostgreSQL

| Module | Responsibility |
| --- | --- |
| `DB.Connect` | Hasql pool configuration/lifecycle |
| `DB.Opers` | Repository, identity, and commit SQL operations |

---

## Configuration

| Module | Responsibility |
| --- | --- |
| `Options.Cli` | CLI parser |
| `Options.ConfFile` | YAML configuration |
| `Options.Runtime` | Effective runtime values |
| `Options` | Configuration merge |

---

## Application

| Module | Responsibility |
| --- | --- |
| `MainLogic` | Command dispatch |
| `app/Main.hs` | Startup and configuration loading |

---

## Unused template infrastructure

| Module | Current status |
| --- | --- |
| `HttpSup.CorsPolicy` | Empty generic placeholder |

This module should be removed unless Gitload acquires an HTTP surface that
actually requires it.

---

# Testing strategy

The current:

```text
test/Spec.hs
```

contains only:

```haskell
main :: IO ()
main =
  putStrLn "Test suite not yet implemented"
```

A repository-history ingestion system needs strong tests because duplicate,
missing, or incorrectly attributed history can contaminate all downstream
analysis.

---

## Repository-discovery tests

Build temporary directory structures covering:

```text
one repository

several nested repositories

.git directory detection

do not recurse into .git

non-Git directories

permission failure

deep nesting

large sibling count
```

---

## Repository-name tests

The current mapping should at minimum have tests verifying:

```text
known path -> expected logical name

.git suffix stripped correctly

unknown path -> Nothing
```

When the mapping becomes data-driven, tests should move to registry
resolution instead.

---

## Git-history fixtures

Create tiny temporary Git repositories containing:

```text
one commit
several commits
branch
merge
multiple authors
different author/committer identities
tag
empty repository
detached HEAD
```

Verify extracted history against expected values.

---

## Database integration tests

Using disposable PostgreSQL:

```text
insert repository

insert identity

insert commit

fetch repositories

fetch identities

fetch repository history
```

and especially:

```text
ingest once
ingest again
assert no duplicate commits
```

---

## Multiple-repository tests

Test the behavior of contributor identity accumulation across:

```text
repo A
repo B
repo C
```

including one identity appearing in several repositories.

---

## Concurrency tests

Once bounded parallel processing exists, test:

```text
many repository directories

database pool pressure

duplicate ingest processes

unique commit constraints
```

---

## Golden reporting tests

When weekly reporting is introduced, store representative histories and
approved report output.

This prevents small grouping/date changes from silently altering historical
reports.

---

# Performance and scalability

The current implementation already contains several useful scaling choices.

---

## Filesystem concurrency

Independent subtrees are scanned concurrently.

This is useful for large workspaces containing many repositories.

Concurrency should eventually be explicitly bounded.

---

## Repository independence

Each Git repository can largely be scanned independently.

That creates an obvious future execution model:

```text
workspace
   |
   +--> repo worker
   +--> repo worker
   +--> repo worker
   `--> repo worker
```

with database persistence coordinated through the Hasql pool.

---

## Existing-commit map

Before ingesting a repository, existing commit rows are converted to:

```text
Map commitOID RawCommitLog
```

making each duplicate check approximately logarithmic rather than requiring
one SQL query per commit.

---

## Current full-history scan

The present implementation walks the history reachable from:

```text
HEAD
```

on each ingest and then filters already-known commits against PostgreSQL.

This is simple and robust for modest repositories.

For very large histories, future incremental logic can stop traversal once
it reaches known frontier commits, while still correctly handling:

```text
branches
merges
rebases
force pushes
```

The design should favor correctness over assuming that Git history is always
a single append-only line.

---

# Development roadmap

## Phase 0 — Stabilize current ingestion

Before expanding analytics:

1. implement the automated test suite;
2. add temporary-repository integration fixtures;
3. normalize configuration behavior;
4. make configuration unnecessary for `scan`, `help`, and `version`;
5. add database uniqueness constraints;
6. clean repository metadata;
7. remove unused generic modules;
8. fix identity terminology; and
9. make repository registration data-driven.

---

## Phase 1 — Repository registry

Replace hard-coded path suffixes with a durable repository registry.

Represent:

```text
logical repository
working-copy path
remote URL
project/workstream
aliases
default branch
active/inactive state
```

Allow:

```text
scan
```

to distinguish:

```text
known repo
new/unregistered repo
duplicate working copy
```

---

## Phase 2 — Complete Git commit semantics

Extend the commit representation with:

```text
author
committer
author time
commit time
parents
tree identity
```

Preserve merge structure.

---

## Phase 3 — Branches and tags

Ingest:

```text
local refs
remote refs
tags
HEAD/default branch
```

and associate them with commit identities.

This creates a foundation for release and branch-lifecycle analysis.

---

## Phase 4 — File-level changes

Derive per-commit:

```text
added files
deleted files
modified files
renamed files
binary files
insertions
deletions
```

without prematurely assigning productivity meaning to those values.

---

## Phase 5 — Component classification

Map files to meaningful FUDD areas:

```text
source
tests
documentation
configuration
database
frontend
backend
hardware
deployment
```

and project-specific components.

This provides much stronger reporting than commit counts alone.

---

## Phase 6 — Weekly repository intelligence

Implement Monday-to-Sunday aggregation.

Generate:

```text
week
  ->
repositories
  ->
commits
  ->
work focus
```

with links back to exact Git evidence.

---

## Phase 7 — Milestone links

Allow repository changes to associate with:

```text
0to1,Done tasks
issues
architecture records
project milestones
releases
```

Store explicit links where they exist.

Use inference only as a secondary suggestion mechanism.

---

## Phase 8 — SPPM projections

Build evidence-oriented projections for:

```text
Security
Productivity
Performance
Maintainability
```

without turning crude repository statistics into developer scores.

---

## Phase 9 — Ecosystem status

Combine Gitload history with:

```text
CI
tests
releases
dependency information
project registry
documentation
deployment
```

to generate ecosystem-level progress views.

---

## Phase 10 — Historical source intelligence

Expose repository timelines to:

```text
Recycler
Migrator
Daniell
0to1,Done
other FUDD analysis tools
```

through a stable API rather than requiring each project to independently
reimplement repository-history discovery.

---

# Design principles

## Git is evidence, not project meaning

A commit tells us that code changed.

It does not independently tell us:

```text
whether a milestone is complete
whether the change is correct
whether it improved performance
whether a person was productive
```

Those conclusions require additional evidence.

---

## Preserve raw history

Derived analytics should never replace:

```text
commit ID
identity
timestamp
message
```

or future file-level facts.

Raw Git evidence must remain inspectable.

---

## Repository identity should survive filesystem movement

Do not use absolute local path as the permanent repository identity.

Paths describe working copies.

---

## Keep identity provenance

Do not silently rewrite:

```text
Alice <alice@old.example>
```

into another identity and discard the original.

Canonical people and raw Git identities are different layers.

---

## Prefer incremental ingestion

Once history exists in PostgreSQL, new runs should primarily add new
evidence.

Repeated unchanged scans should be cheap.

---

## Enforce idempotency in the database

Application-level duplicate checking is useful.

Database uniqueness constraints are stronger.

Use both.

---

## Do not rank developers by commit volume

Measures such as:

```text
number of commits
lines changed
files changed
```

can be useful for understanding repository activity.

They are poor standalone measurements of developer effectiveness.

Gitload should support project intelligence, not gamification.

---

## Keep higher-level interpretations reproducible

A weekly report should be derivable again from the underlying stored facts.

If analysis logic changes, the raw evidence should permit historical
recomputation.

---

## Separate ingestion from presentation

The current utility should first become an excellent repository-history
collector.

Web UIs, dashboards, and reports can consume the resulting data without
being embedded into the Git traversal layer.

---

# Current limitations

## Repository identities are hard-coded

`pathToRepoName'` currently contains a long source-level list of known path
suffixes.

Unknown repositories are ignored by ingestion.

This is the largest architectural limitation in the present scanner.

---

## Only commits reachable from `HEAD` are ingested

Other branch histories that are not reachable from the current `HEAD` are
not represented.

---

## Branches and tags are not stored

The current model contains only commit history.

---

## Parent relationships are not stored

Merge structure cannot currently be reconstructed from the database.

---

## Git author and committer are conflated in terminology

The code reads the Git author but stores that identity through functions and
tables named `committer`.

---

## Identity is a single combined string

The current key is:

```text
Name <email>
```

Name and email are not stored separately.

---

## No canonical person model exists

Several Git identities belonging to one person remain separate.

---

## No file-level changes are recorded

The database knows that a commit exists but not which files it changed.

---

## No branch/release context exists

A commit cannot currently be associated with:

```text
feature branch
release branch
tag
release
```

using Gitload data alone.

---

## Scan concurrency is not actually bounded

The source defines:

```text
maxThreads = getNumCapabilities
```

but does not use it to constrain `forConcurrently`.

---

## Filesystem scan errors can be silently suppressed

An inaccessible recursive subtree currently produces:

```text
[]
```

rather than a retained warning.

---

## No database schema/migrations are included

The application expects:

```text
repos
committers
commitlogs
```

to exist.

---

## Database constraints are not documented

Correct idempotency should eventually be reinforced with constraints such
as uniqueness of:

```text
repository name
Git identity
(repo_fk, commit OID)
```

where the target data model requires them.

---

## `init` command values are not typed by the CLI

The CLI accepts arbitrary text for:

```text
init CMD PATH
```

and rejects unsupported values only at runtime.

---

## Configuration is required before every command

`app/Main.hs` attempts to load a YAML configuration before dispatching even:

```text
scan
help
version
```

although those operations do not intrinsically require PostgreSQL.

---

## Configuration path documentation is inconsistent

CLI:

```text
~/.gitload/config.yaml
```

Implementation:

```text
~/.fudd/gitload/config.yaml
```

---

## Pool settings in YAML are ignored

Although present in `PgDbOpts`:

```text
poolSize
poolTimeOut
```

are not currently applied by `mergeOptions`.

---

## Cross-repository committer accumulation needs tightening

The current multi-repository ingest implementation seeds parts of its
per-repository commit fold from the originally fetched committer map rather
than consistently carrying forward the updated map produced by earlier
repositories.

This should be corrected and covered by integration tests before relying on
large multi-repository imports.

---

## Repository scanning assumes worktree `.git` directories

Git supports arrangements where `.git` can be a file pointing to another
Git directory, for example with some worktree/submodule configurations.

The current scanner searches only for directories named:

```text
.git
```

Those alternate layouts are not recognized.

---

## No test suite exists

`stack test` currently prints only:

```text
Test suite not yet implemented
```

---

## Homebrew paths are embedded in `stack.yaml`

The current:

```text
/opt/homebrew/include
/opt/homebrew/lib
```

assumptions reduce build portability.

---

# Repository housekeeping

The current package metadata still points to:

```text
hugdro/gitload
```

rather than:

```text
whatsupfudd/gitload
```

Update:

```text
github
homepage
bug-reports
source-repository
README URL
```

accordingly.

---

## README

The existing README currently contains only:

```text
GITLOAD

Operations on Git repo history for progress tracking.
```

This document is intended as its replacement.

---

## Package description

The current package metadata still contains:

```text
Please see the README...
```

A useful synopsis would be:

```text
Git repository-history ingestion and progress intelligence for the FUDD
ecosystem.
```

---

## License metadata

There is currently a metadata mismatch.

`package.yaml` declares:

```text
BSD3
```

while the repository's `LICENSE` file contains the **MIT License**.

The `LICENSE` file should be treated as the current authoritative license
text, and package metadata should be corrected to match it.

---

## Copyright metadata

The current package metadata says:

```text
copyright: "None."
```

while the actual license contains a copyright notice.

These should be reconciled.

---

## Changelog

The changelog still contains the initial generated skeleton.

Future milestones should include:

```text
repository discovery
commit ingestion
repository registry
complete commit semantics
refs/tags
file changes
weekly reporting
milestone links
SPPM projections
0to1,Done integration
```

---

## Public library boundary

The package currently exposes:

```text
Commands.*
Options.*
MainLogic
HttpSup.*
```

alongside reusable Git/database logic.

As Gitload matures, consider narrowing the reusable library API around
concepts such as:

```text
GitLoad.Repository
GitLoad.Commit
GitLoad.Scan
GitLoad.Ingest
GitLoad.Store
GitLoad.Analysis
```

while keeping command/application bootstrap modules internal.

---

# Long-term position

Gitload should become FUDD's **repository-history evidence and progress
intelligence layer**.

The durable architecture is:

```text
                     source repositories
                            |
                            v
                        gitload
                            |
             +--------------+--------------+
             |              |              |
             v              v              v
         repositories    identities      commits
             |              |              |
             +--------------+--------------+
                            |
                            v
                        changesets
                            |
             +--------------+--------------+
             |              |              |
             v              v              v
         components      milestones      releases
             |              |              |
             +--------------+--------------+
                            |
                            v
                    progress evidence
                            |
       +--------------------+--------------------+
       |                    |                    |
       v                    v                    v
   weekly views         FUDD SPPM          0to1,Done
       |                    |                    |
       +--------------------+--------------------+
                            |
                            v
                    ecosystem history
```

The important architectural principle is that Gitload should remain grounded
in **traceable repository facts**.

Higher-level systems can then ask:

```text
What changed?

Where?

When?

By which source identity?

As part of which workstream?

With what test/release evidence?

Toward which objective?
```

and always retain a route back to the exact Git commit.

The immediate engineering priority should therefore be to strengthen:

```text
workspace
   ->
repository registry
   ->
complete Git history
   ->
idempotent PostgreSQL persistence
```

before building sophisticated analytics.

Once that foundation is reliable, weekly progress summaries, project
milestones, implementation traceability, ecosystem status, and SPPM
projections can all be derived from one consistent repository-history
evidence base.

---

# License

The repository's `LICENSE` file currently contains the **MIT License**.

The package metadata currently says `BSD3` and should be corrected to match
the repository license.