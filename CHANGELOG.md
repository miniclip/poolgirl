# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

### Added
    + `edoc` based documentation

## [2.0.3] - 09-09-2020

### Added
    + Travis CI icon, to help increase consumer confidence

### Changed
    + Travis CI targets (make it more modern)

### Fixed
    + .gitignore: assume our `rebar3` -oriented nature

## [2.0.2] - 26-09-2019

### Fixed
    + References to locally unused functions (xref'ed)

## [2.0.1] - 23-11-2017
### Fixed
    + Fix slow and unclean shutdown when using child_spec/{2,3}

## [2.0.0] - 26-07-2016
### Added
    + Add benchmarks
### Changed
    + Refactor process group logic
    + Refactor supervision tree hierarchy
### Fixed

## [1.1.1] - 20-10-2015
### Added
    + pg2 local module renamed to poolgirl_pg2
### Changed
### Fixed

## [1.1.0] - 20-10-2015
### Added
    + pg2 local module, does no global multi calls
### Changed
### Fixed

## [1.0.5] - 04-05-2015
### Added
    + README file
### Changed
    + Open Source license is now MIT
### Fixed

## [1.0.4] - 24-04-2015
### Added
### Changed
### Fixed
   + Correct handling of transaction/checkout upon worker pool
     depletion

## [1.0.3] - 6-04-2015
### Added
   + get_workers API method, returns list of worker pids
### Changed
### Fixed

## [1.0.2] - 2-04-2015
### Added
### Changed
### Fixed
   + Explosive launch of worker on worker death

## [1.0.1] - 26-02-2015
### Added
### Changed
   + Recruitment of local workers only
### Fixed
