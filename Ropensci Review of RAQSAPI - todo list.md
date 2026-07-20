# ROpenSci Review of RAQSAPI todo list
Ai generated summary of findings from ROpenSci review of RAQSAPI as a todo list of issues that are to be addressed.

## Must fix

| Status | Item | Why it matters | Source(s) | submitter comments |
|:---:|:---:|:---:|:---:|:---:|
|[X]  | Fix `checkaqsparams()` so validation actually runs for the production wrapper calls | Right now positional calls appear to skip validation, so invalid inputs may pass silently | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) |can't/won't fix checkaqsparams is a non-exported internal-helperfunction that should not be called directly by the end user, all internal function calls to checkaqsparams pass values by parameter and should not pass values by position so this should not be an issue.|
|&check;| Fix the always-true MA/pqao validation logic in `checkaqsparams()` | The current logical condition would reject valid values if reached | Milan Malfait (@milanmlft) | fixed in [commit 1254e2d](https://github.com/USEPA/RAQSAPI/commit/1254e2df6db1c74805c8a1effebb0043b85f4b03)|
|  | Fix the broken `rlang::abort()` path in validation | The validator appears to fail internally instead of showing the intended error message | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) |  |
|&check;| Fix the S3 validator bug in `AQS_DATAMART_APIv2` | The second data slot is not being validated correctly | Milan Malfait (@milanmlft) |addressed in [commit a44230d](https://github.com/USEPA/RAQSAPI/commit/a44230d8e59eba97da4b6ffa6a96df8706f25073)  |
|&check;| Remove the `Sys.setenv()` mutation from `.onLoad()` and clean up the ineffective save/restore logic in `.onUnLoad()` | Packages should not mutate user session-wide environment settings on load | Milan Malfait (@milanmlft) |addressed in [commit 7828447](https://github.com/USEPA/RAQSAPI/commit/782844762ca3e27dbc85fc9c0f8c557a22b2eee4)  |
|  | Replace the empty / ineffective `NEWS.md` with a real changelog | `R CMD check` reports no news entries and the file is effectively a stub | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) |  |
|  | Strengthen tests beyond `expect_no_error()` smoke tests | The current suite can pass even if returned data are wrong | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) |  |
|:question:| Add tests for validation failure paths, especially `checkaqsparams()` | This would catch the validation bugs and protect future fixes | Milan Malfait (@milanmlft) | Please refer to [tests/testthat/test-helperfunctions.R#L25-L66](https://github.com/USEPA/RAQSAPI/blob/main/tests/testthat/test-helperfunctions.R#L25-L66) |
|&check;| Fix Rd rendering issues such as leaked `@seealso`, broken math markup, and the misleading `aqs_sign_up` note | These are user-facing documentation defects | Milan Malfait (@milanmlft) |addressed in [commit 1254e2d](https://github.com/USEPA/RAQSAPI/commit/037f47d9ae6b7935c6ba2ca4489644d5e711c933)  |
|&check;| Restore or add a proper `CONTRIBUTING` file | Community guidelines are incomplete without it | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) |[addressed in commit c2f3609](https://github.com/USEPA/RAQSAPI/commit/c2f36096a2c63e60bedacc13ff6acf709297d650)|
|&check;| Fix malformed package metadata and validation blockers in `DESCRIPTION` | A bad `DESCRIPTION` comment/version field caused `devtools::check()` to fail; metadata hygiene needs cleanup | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) |sddressed in [commit e96994](https://github.com/USEPA/RAQSAPI/commit/e96994f13299c08a60dcaa105bae67e7d6450ff5#diff-9cc358405149db607ff830a16f0b4b21f7366e3c99ec00d52800acebe21b231c)|
|&check;| Regenerate or fix `codemeta.json` | The file is invalid JSON as reviewed | Milan Malfait (@milanmlft) |[addressed in commit e96994f](https://github.com/USEPA/RAQSAPI/commit/e96994f13299c08a60dcaa105bae67e7d6450ff5)  |
|&check;| Fix `CODE_OF_CONDUCT.md` placeholders and broken links | Community files currently contain template leftovers and malformed markdown | Milan Malfait (@milanmlft) | [addressed in commit 9eaa758](9eaa7587a8ebb87d3abe1f7d251abbc296c6897e)  |
|&check;| Delete stale `tests/test-RAQSAPI.R.blob` | It is obsolete and confusing, even if it does not break checks | Milan Malfait (@milanmlft) |[addressed in e96994f](https://github.com/USEPA/RAQSAPI/commit/e96994f13299c08a60dcaa105bae67e7d6450ff5)  |
|&check;| Fix the broken README badge link | The badge URL is malformed | Milan Malfait (@milanmlft) |addressed in [commit 54a8815](https://github.com/USEPA/RAQSAPI/commit/54a881501b71b8cab5607187c75e7ceca530933c) |

## Should fix

| Status | Item | Why it matters | Source(s) | Additional comments |
|:---:|:---:|:---:|:---:|:---:|
|&check;| Consolidate the vignette structure | There are too many overlapping vignettes and some child files appear to be standalone unintentionally | Milan Malfait (@milanmlft) |[commit 83f8e5a](https://github.com/USEPA/RAQSAPI/commit/83f8e5a7c5399da50a72f2b16edecdb98481e8b2)|
|[x]| Add at least one runnable worked example using mocked fixtures | The package currently lacks true runnable examples, which hurts usability | Milan Malfait (@milanmlft) | since this package relies on inputing credential informaiton this probably won't work well, mocked examples could work but adds more complexity to documents without any tangible benefits. [@rkug comment](https://github.com/ropensci/software-review/issues/744#issuecomment-4862990295) |
|  | Improve the README with a usage demo and related-packages comparison | This would help new users understand the package quickly | Milan Malfait (@milanmlft) |  |
|[x]| Add explicit mention of credential environment variables `RAQSAPIUSERNAME` and `RAQSAPIKEY` | This improves onboarding and matches the package’s supported setup | Rainer M Krug (@rkrug) | Not implemented, in favor of saving credentials to package environment instead |
|  | Standardize error handling across the package | The package currently mixes `stop()`, `warning()`, and `rlang::abort()` | Milan Malfait (@milanmlft) |  |
|  | Reduce code duplication in the `by_*` wrappers with an internal dispatcher | This would materially reduce maintenance burden and copy-paste risk | Milan Malfait (@milanmlft) |  |
|[◪]| Simplify dependencies where possible (`gtools`, `stringr`, maybe `tidyselect`, possibly others) | Several imports appear replaceable with base R or lighter alternatives | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) | * [`gtools `removed in  e96994f](https://github.com/USEPA/RAQSAPI/commit/e96994f13299c08a60dcaa105bae67e7d6450ff5) <br> * not sure if `tidyselect` should be removed (useful for renameaqsvariables function), considering `stringr` |
|&check;| Move credentials out of global `options()` into a package-private environment or per-call argument | This is safer and cleaner than storing credentials globally | Milan Malfait (@milanmlft) |implemented in [commit 7828447](https://github.com/USEPA/RAQSAPI/commit/782844762ca3e27dbc85fc9c0f8c557a22b2eee4)|
|  | Add `print.AQS_DATAMART_APIv2` or a similar custom print method | It would improve the user experience at the console | Milan Malfait (@milanmlft) |  |
|[X]| Consider normalizing naming conventions | The package mixes styles such as `by_MA`, `by_pqao`, and various underscore patterns | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) |Can't fix/won't fix, this is a major change that would break API code compatibility. Would break code in existing projects that depend upon RAQSAPI.  |
|  | Centralize repeated hardcoded URLs | Shared constants would reduce duplication and make future changes easier | Milan Malfait (@milanmlft) |  |
|  | Add a test-coverage workflow and badge | This would make coverage visible and help maintenance | Milan Malfait (@milanmlft) |  |
|&check;| Remove stale vignette or documentation artifacts such as obsolete files and commented-out blocks | These files add noise and confusion | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) |addressed in[commit 83f8e5a](https://github.com/USEPA/RAQSAPI/commit/83f8e5a7c5399da50a72f2b16edecdb98481e8b2)|
|  | Fix the vignette/README grammatical and formatting nits | Small but visible polish issues remain | Milan Malfait (@milanmlft); Rainer M Krug (@rkrug) |  |
|  | Add a basic pkgdown site | This would make vignettes and documentation easier to navigate | Rainer M Krug (@rkrug) |  |

## Nice to have

| Status | Item | Why it matters | Source(s) | Additional comments |
|:---:|:---:|:---:|:---:|:---:|
|[X]| Consider dropping the `AQS_DATAMART_APIv2` version suffix from the S3 class name | It would make downstream code less brittle if EPA releases a v3 | Milan Malfait (@milanmlft) |Can't fix/won't fix. after discussion with @milanmlft, reviewer understands that the suffix will not affect future updates, suffix will help differentiate between versions of the API |
|[X]  | Consider shorter public function names or aliases for very long wrapper names | This would improve ergonomics and reduce typing burden | Rainer M Krug (@rkrug) |Can't fix/won't fix, this is a major change that would break API code compatibility. Would break code in existing projects that depend upon RAQSAPI.  |
|:thinking:| Consider a database or parquet-backed workflow for large datasets | This could help with memory usage for large queries | Rainer M Krug (@rkrug) |awesome idea, I had the same idea for another project that would depend on RAQSAPI, would like guidance on implementing this was considering using the `rlite` package, if implemented could possibly remove the dependency of httrtest2 |
|:thinking:| Consider caching credentials across sessions | This would reduce setup friction for users | Milan Malfait (@milanmlft) |May be worth looking into, will need suggestions on implementing this|
|:thinking:| Consider using `keyring` or a similar secure credential helper | This would improve the sign-up and credential workflow | Milan Malfait (@milanmlft) |almost identical to issue above |
|[X]| Consider a one-public-function-per-file layout if it aligns with the project philosophy | It may make documentation and coverage easier to reason about | Rainer M Krug (@rkrug) |can't fix, won't fix, Considering that RAQSAPI exposes over 100 public functions, this probably will not be feasible|
|[X]| Consider improving package structure and file length by splitting very long source files | This may make future maintenance easier | Milan Malfait (@milanmlft) |can't fix/won't fix, See above comment|
|:thinking:| Consider lighter use of tidyverse dependencies overall | This is mostly a style and maintenance preference, but could simplify the package | Rainer M Krug (@rkrug) | I may be able to remove some tidyverse dependencies, Until base R implements a replacement for the assignment pipe you will have a hard time convincing me to drop magrittr|
|  | Consider additional API-drift checks outside CI | This could help detect upstream API changes over time | Rainer M Krug (@rkrug) |  |

## Suggested order of attack

1. Fix validation and error handling bugs.
2. Repair the S3 validator and session-mutating load hook.
3. Clean up `NEWS.md`, `DESCRIPTION`, Rd rendering, and other packaging issues.
4. Strengthen tests with real assertions and failure-path checks.
5. Consolidate vignettes and add at least one runnable example.
6. Then tackle structural refactoring and dependency reduction.

## Additional changes suggested by the author
| Status | Item | Why it matters | Additional comments |
|:---:|:---:|:---:|:---:|
| | fix issues with/improve .devcontainer | devcontainers make it easier for outside contributors to recreate the development environment used to create the package | requesting assistance with maintaining devcontainers |
