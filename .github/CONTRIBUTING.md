# Contributing

Contributions to `manypkgs`,
whether in the form of issue identification, bug fixes, new code or documentation,
are encouraged and welcome.

Please note that the `manypkgs` project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Contents

Each entry says when to read the section.
Update this list whenever a heading below is added, renamed, or moved.

- [Identifying issues](#identifying-issues): before you report a bug or ask for a feature
- [Package architecture](#package-architecture)
  - [Project overview](#project-overview): to decide whether a function belongs here
    or in another 'many' package
- [Developing](#developing)
  - [Common commands](#common-commands): to load, document, test, or check the package
  - [File organization](#file-organization): to find a function, or the file for a new one
  - [Tests](#tests): before you add a test
  - [Deprecation](#deprecation): before you rename, move, or remove a function or an argument
- [Documenting](#documenting)
  - [README and website](#readme-and-website): before you edit the README or the website index,
    and whenever you export a function
  - [Writing in markdown and roxygen](#writing-in-markdown-and-roxygen):
    before you write documentation or a vignette
  - [`NEWS.md` conventions](#newsmd-conventions): before you write a `NEWS.md` entry
    or a commit message
- [Releasing](#releasing)
  - [Branching and CI](#branching-and-ci): to choose a branch, or to read a failed check
  - [Versioning](#versioning): before you bump the version
- [Git for new contributors](#git-for-new-contributors): if git is new to you

## Identifying issues

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and
to comment or continue a conversation on that issue.
The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
Examples for documentation are also most welcome.

## Package architecture

### Project overview

`manypkgs` is the developer's package of the
[many packages](https://github.com/globalgov) ecosystem.
It helps contributors set up a 'many' package,
import and prepare its datasets, and export them into datacubes.

Other packages in the ecosystem hold the functions that users call
or that code a particular kind of data:

- `{manydata}` holds the functions that users call to find, compare,
  and consolidate the data in the 'many' packages.
- `{manytreaties}` holds the functions that standardise treaty titles and texts,
  and that code treaty identifiers, lineages, and terms.
- `{manystates}` holds `code_states()`, which codes the states in a text.
- `{manyios}` holds `code_orgs()`, which codes the international organisations in a text.
- `{messydates}` holds the functions for dates, for example `messydates::year()`.

Put a new function in `manypkgs` only if it helps contributors
to make or update a 'many' package.
A function that codes a kind of data goes in the package for that data.
Do not copy a function from another 'many' package into `manypkgs`:
call it with `::` and declare the package in `DESCRIPTION`.

## Developing

### Common commands

This is a standard R package developed with `devtools`/`roxygen2`.
Run these from an R console with the working directory set to the package root
(or via `Rscript -e`).

- Load for development: `devtools::load_all()`
- Regenerate docs & NAMESPACE after editing roxygen comments: `devtools::document()`
- Run full test suite: `devtools::test()`
- Run a single test file: `devtools::test(filter = "export_data")`,
  or `testthat::test_file("tests/testthat/test_export_data.R")`
- Full package check (mirrors CI): `devtools::check()` or `rcmdcheck::rcmdcheck()`
- Lint: `lintr::lint_package()`
- Spell check: `spelling::spell_check_package()`
- Code coverage: `covr::package_coverage()`
- Build pkgdown site locally: `pkgdown::build_site()`

Run R in a UTF-8 locale, e.g. `LANG=en_US.UTF-8 Rscript -e "devtools::document()"`.
Under the C locale, `devtools::document()` writes byte escapes into `man/`
in place of quotation marks and accented letters.

`README.md` is generated from `README.Rmd`.
See [README and website](#readme-and-website) below.

### File organization

The files in `R/` are named after the family of functions that they hold:

- `package_setup.R`, `package_update.R`, and `package_templates.R`
  set up and update a 'many' package, and write its template files
  from `inst/templates/`.
- `data_import.R` and `data_export.R` import raw data and export datasets
  into a datacube.
- `code_agreements.R` and `condense_agreements.R` code agreement identifiers.
- `convert_pdf.R` extracts the text from PDF files.
- `package_tools.R`, `package_complexity.R`, `package_evaluate.R`,
  `function_application.R`, and `build_tutorials.R` hold tools for package development.
- `manypkgs-defunct.R` holds the defunct functions.
- `zzz.R` holds the startup message.

Put a new function in the file of its family.
Where no family fits, start a new file with a name of the form `<family>_<topic>.R`.

### Tests

This package uses the `testthat` package (edition 3) for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.

Tests are in `tests/testthat/`.
A test file is named after the file in `R/` that it tests,
for example `test-condense_agreements.R` tests `R/condense_agreements.R`.
Some older test files use `test_` in place of `test-`;
rename such a file when you next change it.

Write a test for each new exported function.
A test that needs the internet, or data from another package that is not
in `Imports`, must call `skip_on_cran()` or `skip_if_not_installed()`.
Re-use existing test data rather than create new data for each test.

### Deprecation

A function that is renamed or removed first becomes defunct
in [R/manypkgs-defunct.R](../R/manypkgs-defunct.R):

- Document the function with `#' @describeIn defunct Deprecated on <date>.` and export it.
- Call `.Defunct()` with a message that names the function that replaces it,
  and the package that holds that function.
- Record the change as `Removed ...` or `Moved ...` in `NEWS.md`.

A function that only duplicated a function in another 'many' package
can be removed without a defunct shim,
if `NEWS.md` names the function that replaces it.

## Documenting

### README and website

The README offers a landing page for new users, both on the GitHub repository
and on the website.
`README.md` is generated from `README.Rmd`:
edit `README.Rmd` and re-knit it (`devtools::build_readme()`),
and never edit `README.md` directly.

The website is created by pkgdown from [pkgdown/_pkgdown.yml](../pkgdown/_pkgdown.yml),
and is deployed automatically when changes reach `main`.
pkgdown requires *every* documented topic to appear in the `reference:` section
of `_pkgdown.yml`, or the website does not build.
When you add, rename, or remove an exported function, update that section,
and run `pkgdown::build_site()` locally before you open a pull request.
The `reference:` titles are also the headings used in `NEWS.md` (see below),
so keep the two in step.

### Writing in markdown and roxygen

When you write text in markdown or roxygen,
start each sentence, and sometimes each clause of a sentence, on a new line.
This makes it easier to compare versions and to comment on GitHub.
Lines of text should, ideally, not be over 80 characters in length.

Every exported function needs roxygen documentation with at least a title,
a description, a `@param` entry for each argument, a `@return` entry, and an example.
`R CMD check` warns about an exported function with no documentation,
and CRAN asks for a `@return` entry.

### `NEWS.md` conventions

`NEWS.md` groups each version's changes under `##` headings that mirror the website
reference index (the `reference:` titles in `pkgdown/_pkgdown.yml`).
Lead with `## Package` (package-wide, website, and infrastructure changes),
then the function families in the order of the index:
`## Setting up`, `## Importing and exporting`, `## Coding agreements`, `## Developing`.
Each heading appears at most once per version.

Start each bullet with a verb matching the change type:

- `Added ...` — new functionality
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with `(closes #123)`.
  Note that GitHub closes the issue only where the same phrase is in a commit message
  or the pull request description
- `Renamed ... to ...` — function or data name migrations
- `Improved ...` — functional updates to existing behaviour
- `Updated ...` — documentation changes
- `Removed ...` / `Dropped ...` — functionality or dependencies taken out
- `Moved ...` / `Migrated ...` — functionality relocated to another package or file
- `Split ...` — one function or file divided into several

Any of these verbs can also lead a sub-bullet.

Where a function is removed or moved, name the function that replaces it
and the package that holds it, e.g.:

```
- Moved `code_lineage()` to `{manytreaties}`
- Removed `code_dates()` in favour of `messydates::year()`
```

Keep every bullet to one line of fewer than 81 characters ideally.
If a bullet wraps, it holds too much:
shorten it, or split it into a lead bullet and sub-bullets.
Each bullet stands on its own, and states what changed, not why or how.
Explanation belongs in the function documentation or the vignettes.

Where several bullets describe parallel changes, reuse the sentence structure,
so that a reader sees the parallelism at a glance.
Use one word for one thing throughout a version's entries,
rather than varying the wording for effect.

If a cited GitHub issue was **not** authored by @jhollway, thank the author with an
`@`-tag in the bullet.
Cluster related changes as indented sub-bullets under a lead bullet.
Where several changes concern one function, lead with an `Improved ...` bullet that
names the function, and put the individual `Fixed ...`/`Added ...` points beneath it.
Under an `Improved ...` lead bullet, do not name the function again in the sub-bullets.
Sub-bullets indent by two spaces, and nest at most one level further (four spaces).

## Releasing

### Branching and CI

- `main` is the release branch; `develop` is the working branch.
  Work on `develop`, and open a pull request from `develop` into `main` to release.
- Pull requests into `main` trigger [prchecks.yml](workflows/prchecks.yml):
  R CMD check (macOS/Windows/Linux), binary build, codecov, lintr, spell check,
  and pull request metadata checks.
  The metadata checks need the `DESCRIPTION` version to be bumped,
  the pull request title to name the new version,
  and the pull request description to group its changes under `##` headings,
  as in `NEWS.md`.
- Merges into `main` trigger [pushrelease.yml](workflows/pushrelease.yml):
  check, version tag, GitHub release with binaries, then the pkgdown website.
  The release notes are taken from the `NEWS.md` section of the new version.
- R CMD check fails on any warning.
  Run `devtools::check()` locally before you open a pull request.

### Versioning

The package is versioned according to [semantic versioning](https://semver.org):
Major.Minor.Patch.

- Bump the patch version for fixes and changes that do not remove anything.
- Bump the minor version where a function is removed, renamed, or moved,
  or where a function returns something different.
- Make sure that the `Version` in `DESCRIPTION` equals the newest heading in `NEWS.md`.

## Git for new contributors

'many' packages are maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english),
and a more recent motivation [here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
They explain the basics of git version control: committing and repos, pulling and pushing,
branching and merging.

Using git from the command line can be intimidating,
so I recommend [Fork](https://git-fork.com) software for Mac and Windows,
which allows mostly visual management of commits, diffs, and branches.

- **Clone** the remote repository to your computer, and work on the `develop` branch.
- **Pull** before you start working, so that you have the newest commits and files.
- **Commit** once you have saved a change: check the diff one last time,
  and write a commit message in the form of a `NEWS.md` bullet (see above),
  with the issue it relates to.
  Keep each commit to one change, so that it is easy to follow or to revert.
