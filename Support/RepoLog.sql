create table repos (
  uid serial primary key,
  name text not null
);

create table committers (
  uid serial primary key,
  name text not null
);

create table commitlogs (
  uid serial primary key,
  repo_fk int not null references repos(uid),
  committer_fk int not null references committers(uid),
  cid text not null,
  createdAt timestamptz not null,
  logMsg text not null
);