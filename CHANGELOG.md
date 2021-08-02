# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `rebar3_hank` [Paulo Oliveira]

### Changed

- CI container approach to `setup-beam` with cache [Paulo Oliveira]

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
