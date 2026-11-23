# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Poolgirl is an Erlang/OTP worker pool library, published to Hex as `mc_poolgirl`. It is a lighter sibling of poolboy for cast-only, fast workers. `checkout/1` picks a random worker with no message to any manager process. `checkin/2` is a no-op kept for poolboy compatibility. No runtime dependencies. OTP 26+ (CI runs 26–29).

## Commands

All via `make` (wraps rebar3):

- `make compile`
- `make check` — xref + dialyzer (both `as test`), elvis (`rebar3 lint`), hank. `warnings_as_errors` is on, so warnings fail the build
- `make test` — eunit + cover
- `make ex_doc-dry` — dry-run Hex docs publish. CI runs it only on OTP 28: rebar3_ex_doc 0.3.0 has no ex_doc escript for OTP 29
- CI runs `make compile check test`

All tests are in one eunit `foreach` generator (`test/poolgirl_tests.erl:pool_test_/0`), so you cannot select one case by name from the CLI. Narrowest run: `rebar3 eunit --module=poolgirl_tests`. To run one case, comment out the others in the generator list temporarily. The test worker is in `test/util/` and is compiled through `extra_src_dirs`.

## Architecture

Supervision tree, one subtree per pool:

```
poolgirl_app_sup (simple_one_for_one, registered locally)
└── poolgirl_sup (rest_for_one, one per pool)
    ├── poolgirl_worker_sup (simple_one_for_one, temporary workers)
    └── poolgirl (gen_server, the pool manager)
```

- Two ways to start a pool. `poolgirl:start_link/1,2` starts it under the library's own `poolgirl_app_sup`. `poolgirl:child_spec/2,3` gives a `poolgirl_sup` spec for the caller's tree, and then poolgirl does not supervise that pool.
- `rest_for_one` order matters. If `poolgirl_worker_sup` dies, the manager restarts too and repopulates.
- `poolgirl_internal_directory` is a private, unnamed ETS table that `poolgirl_sup` creates and passes to both children. Siblings find each other's pids through it (`find/2`), so the library registers no global names. `register/2` uses `ets:select_replace` to take over an entry atomically when the old pid is dead.
- `poolgirl_pg` is the worker registry that `checkout/1` reads. It is a **public named ETS table whose name is the pool name**, with one row `{PoolName, {Pid1, ...}}` (a tuple, so checkout picks with `element/2`). The `poolgirl` gen_server creates and owns the table, so the table dies with the manager. The manager is the only writer (join/leave). Clients read it directly. Random pick: `erlang:phash2(erlang:unique_integer(), N)`, which keeps no state in the caller.
- Manager (`poolgirl.erl`) monitors each worker. On `'DOWN'` it removes the pid from the group and starts a replacement. With a pre-started `workers` list there is no supervisor (`supervisor = undefined`), so dead workers are removed and not replaced.
- Pool names must be atoms. Only `{local, Name}`, `{global, Name}` and `{via, _, Name}` are parsed. The bare name becomes the ETS table name.
- Worker modules implement the `poolgirl_worker` behaviour (`start_link/1`). Each worker gets `WorkerArgs`.

## Conventions

- Lint config is in `elvis.config` (elvis_core 5.x format). Suppress per module with `-elvis(...)`, `-hank(...)` and `-ignore_xref(...)` attributes, as the existing code does. Do not loosen the global config.
- `CHANGELOG.md` follows Keep a Changelog. Each entry ends with the author in brackets. App `vsn` is `git`, so releases come from git tags.
