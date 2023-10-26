## v1.0.0 - 2023-10-26
### Changed
- Changed the supported Circom version from 2.0.7 to 2.1.6.
- Suppress ANSI code in JSON logging for now, since the console shows the code raw, which is difficult to read.
- Update gitignore file to ignore .venv for release_helpers

### Fixed
- Fix Docker image building by switching to path context (as opposed to the Git context which has issues with private Git submodules).
- Fix username and password for Docker base image build

