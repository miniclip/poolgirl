# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- edoc for `start_link/1,2`, which are now part of the documented API [Dinis Rosário]

### Changed

- CI to also run on pushes and pull requests to `develop` [Dinis Rosário]
- CI actions to `actions/checkout@v7` and `actions/cache@v6`, which run on Node.js 24 [Dinis Rosário]
- project description to "A lightweight Erlang worker pool library", replacing sexist wording [Dinis Rosário]
- edoc for the `poolgirl` module and the `poolgirl_worker` behaviour [Dinis Rosário]
- README example to a statsd reporter that only uses casts, replacing the synchronous epgsql example [Dinis Rosário]
- `checkout/1` to keep pool members in a tuple and pick with `erlang:phash2/2` over `erlang:unique_integer/0`. It is 3x faster at 200 to 400 workers, scales across schedulers and spreads back-to-back calls evenly [Dinis Rosário]

### Fixed

- README errors: pg2 mention, `checkin/2` arity, `size` meaning, missing options and API functions [Dinis Rosário]
- `status/1` and `spin/3` crashing a pool started with `workers`. `status/1` now returns the worker count, and `spin/3` raises a `no_supervisor` error [Dinis Rosário]
- `spin/3` never removing workers that replaced dead ones, because the manager did not track them [Dinis Rosário]

## [3.0.0] - 2026-10-21

### Changed

- `rebar3_ex_doc` to 0.3.0 [Sergio Almeida]
- `rebar3_hank` to 1.6.1 [Sergio Almeida]
- `rebar3_hex` to 7.3.0 [Sergio Almeida]
- `rebar3_lint` to 5.0.4 [Sergio Almeida]

### Fixed

- support OTP 29 by removing all case catch expressions [Sergio Almeida]
- `elvis.config` by changing to the `elvis_core` 5.x format [Sergio Almeida]
- CI to OTP 26 through 29 [Sergio Almeida]

### Removed

- support for OTP older than 26 [Sergio Almeida]

## [2.2.1] - 2022-04-19

### Added

- `rebar3_ex_doc` + `make ex_doc-dry` [Paulo Oliveira]

### Changed

- `edoc`-based doc. to `ex_doc`-based doc. [Paulo Oliveira]

## [2.2.0] - 2021-08-03

### Added

- `rebar3_hank` [Paulo Oliveira]

### Changed

- CI container approach to `setup-beam` with cache [Paulo Oliveira]
- worker initialization as to make it synchronous with the pool's [Guilherme Andrade]

## [2.1.0] - 2021-03-04

### Added

- `edoc` based documentation [Paulo Oliveira]
- a fresher approach to `elvis.config` [Paulo Oliveira]
- a fresher approach to `Makefile` [Paulo Oliveira]
- a fresher approach to `rebar.config` [Paulo Oliveira]

### Changed

- CI from Travis to GitHub Actions [Paulo Oliveira]

### Removed

- `.editorconfig` [Paulo Oliveira]
- `bench` [Paulo Oliveira]
- `package.exs` [Paulo Oliveira]
- `VERSION` [Paulo Oliveira]

## [2.0.3] - 09-09-2020

### Added

- Travis CI icon, to help increase consumer confidence

### Changed

- Travis CI targets (make it more modern)

### Fixed

- .gitignore: assume our `rebar3` -oriented nature

## [2.0.2] - 26-09-2019

### Fixed

- References to locally unused functions (xref'ed)

## [2.0.1] - 23-11-2017

### Fixed

- Fix slow and unclean shutdown when using child_spec/{2,3}

## [2.0.0] - 26-07-2016

### Added

- Add benchmarks

### Changed

- Refactor process group logic
- Refactor supervision tree hierarchy

### Fixed

## [1.1.1] - 20-10-2015

### Added

- pg2 local module renamed to poolgirl_pg2

## [1.1.0] - 20-10-2015

### Added

- pg2 local module, does no global multi calls

## [1.0.5] - 04-05-2015

### Added

- README file

### Changed

- Open Source license is now MIT

## [1.0.4] - 24-04-2015

### Fixed

- Correct handling of transaction/checkout upon worker pool
depletion

## [1.0.3] - 6-04-2015

### Added

- get_workers API method, returns list of worker pids

## [1.0.2] - 2-04-2015

### Fixed

- Explosive launch of worker on worker death

## [1.0.1] - 26-02-2015

### Changed

- Recruitment of local workers only
