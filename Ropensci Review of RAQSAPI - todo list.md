# ROpenSci Review of RAQSAPI todo list

Ai generated summary of findings from ROpenSci review of RAQSAPI as a
todo list of issues that are to be addressed.

## Must fix

[TABLE]

## Should fix

[TABLE]

## Nice to have

| Status | Item | Why it matters | Source(s) | Additional comments |
|:--:|:--:|:--:|:--:|:--:|
| \[X\] | Consider dropping the `AQS_DATAMART_APIv2` version suffix from the S3 class name | It would make downstream code less brittle if EPA releases a v3 | Milan Malfait (@milanmlft) | Can’t fix/won’t fix. after discussion with @milanmlft, reviewer understands that the suffix will not affect future updates, suffix will help differentiate between versions of the API |
| \[X\] | Consider shorter public function names or aliases for very long wrapper names | This would improve ergonomics and reduce typing burden | Rainer M Krug (@rkrug) | Can’t fix/won’t fix, this is a major change that would break API code compatibility. Would break code in existing projects that depend upon RAQSAPI. |
| 🤔 | Consider a database or parquet-backed workflow for large datasets | This could help with memory usage for large queries | Rainer M Krug (@rkrug) | awesome idea, I had the same idea for another project that would depend on RAQSAPI, would like guidance on implementing this was considering using the `rlite` package, if implemented could possibly remove the dependency of httrtest2 |
| 🤔 | Consider caching credentials across sessions | This would reduce setup friction for users | Milan Malfait (@milanmlft) | May be worth looking into, will need suggestions on implementing this |
| 🤔 | Consider using `keyring` or a similar secure credential helper | This would improve the sign-up and credential workflow | Milan Malfait (@milanmlft) | almost identical to issue above |
| \[X\] | Consider a one-public-function-per-file layout if it aligns with the project philosophy | It may make documentation and coverage easier to reason about | Rainer M Krug (@rkrug) | can’t fix, won’t fix, Considering that RAQSAPI exposes over 100 public functions, this probably will not be feasible |
| \[X\] | Consider improving package structure and file length by splitting very long source files | This may make future maintenance easier | Milan Malfait (@milanmlft) | can’t fix/won’t fix, See above comment |
| 🤔 | Consider lighter use of tidyverse dependencies overall | This is mostly a style and maintenance preference, but could simplify the package | Rainer M Krug (@rkrug) | I may be able to remove some tidyverse dependencies, Until base R implements a replacement for the assignment pipe you will have a hard time convincing me to drop magrittr |
| ❓ | Consider additional API-drift checks outside CI | This could help detect upstream API changes over time | Rainer M Krug (@rkrug) | I request assistance with this suggestion. |
| \[X\] | Improve the README with a usage demo | gives people interested in what this project does a quick example of how this package works | Rainer M Krug (@rkrug) | addressed in [commit 161cadb](https://github.com/USEPA/RAQSAPI/commit/161cadb2b213b1d7672574b5e6193eec84fa7a1c) |

## Suggested order of attack

1.  Fix validation and error handling bugs.
2.  Repair the S3 validator and session-mutating load hook.
3.  Clean up `NEWS.md`, `DESCRIPTION`, Rd rendering, and other packaging
    issues.
4.  Strengthen tests with real assertions and failure-path checks.
5.  Consolidate vignettes and add at least one runnable example.
6.  Then tackle structural refactoring and dependency reduction.

## Additional changes suggested by the author

| Status | Item | Why it matters | Additional comments |
|:--:|:--:|:--:|:--:|
| \[◪\] | fix issues with/improve .devcontainer | devcontainers make it easier for outside contributors to recreate the development environment used to create the package | requesting assistance with maintaining devcontainers, removed the .devcontainer folder with [commit 49f50c0](https://github.com/USEPA/RAQSAPI/commit/49f50c095d008b691399daa8ba6b19450d520882) until it is working properly |
