# showkitr TODO

Reference list for getting showkitr to a first CRAN submission, plus ideas.
Status as of 2026-10-20: `R CMD check` is clean (0 errors / 0 warnings / 0 notes),
glossary tests pass, `fx_showcase()` has smoke tests.

## Before a CRAN submission

- [ ] Check the name is free: `available::available("showkitr")`
- [ ] Bump the version from `0.0.0.9000` to `0.1.0` (DESCRIPTION and NEWS.md)
- [ ] Spell check: `spelling::spell_check_package()`; add `inst/WORDLIST` if needed
- [ ] Add ORCID to `Authors@R` (optional but common)
- [ ] Write `cran-comments.md` (already in `.Rbuildignore`); say it is a new submission
- [ ] Install a LaTeX distribution (e.g. `tinytex::install_tinytex()`) so the PDF
      manual builds; re-run `devtools::check()` with the manual
- [ ] Run `devtools::check_win_devel()` and `rhub::check_for_cran()`
- [x] Run `fx_showcase()` in a clean session (done 2026-10-21; found and fixed a
      one-character diff crash). Still worth one run on a second machine, and the
      "never run in a clean session" note in the `fx_showcase.R` header is stale
- [ ] Confirm nothing writes outside `tempdir()` and no example opens a browser
- [ ] Decide on `magick`/ImageMagick and `chromote` (table cards need Chrome):
      mention both in the README and `cran-comments.md`
- [ ] Optional: `usethis::use_github_action("check-standard")` for CI on push
- [ ] Optional: a short vignette (`usethis::use_vignette("glossary")`)
- [ ] Submit: `devtools::release()`

## Tests

- [x] Glossary suite (`tests/testthat/test-glossary.R`)
- [x] `fx_showcase()` smoke tests (`tests/testthat/test-showcase.R`)
- [ ] `fx_showcase()` with ggplot objects and string diffs (`diff_highlight`)
- [ ] `fx_showcase_addin()` (needs rstudioapi mocking)
- [ ] Snapshot test for the glossary card HTML (`expect_snapshot_file()`)

## Glossary features

- [ ] `fx_glossary_quarto()`: write a `.qmd` glossary page into the course site
- [ ] `fx_glossary_check()`: compare a YAML file with real function arguments;
      flag missing, extra, or stale names (and the `n:` -> `FALSE` YAML pitfall)
- [ ] `fx_glossary_draft()`: write a starter YAML entry from a help page
- [ ] Search / filter box and a "by lecture" grouping on the glossary page
- [ ] Clickable `see_also` links that jump to the matching card
- [ ] Printable (handout) view
- [ ] Show each entry's example output under the code (like the lab slides)
- [ ] Add entries for the rest of the course functions (labs 1, 7, 10 and the
      `ggplot2` / `gt` calls)
- [ ] Decide whether `glossarykitR` stays inside showkitr or becomes its own package
      (simplest for CRAN: keep both until the glossary side is stable)
- [ ] Make the signature highlight fully keyboard/touch friendly (done for focus;
      check on a phone)

## Housekeeping

- [ ] Note the `showkitr` remote name (not `origin`): `git push showkitr main`
- [ ] Keep `inst/glossary/lab3_lab4.yml` quoting `"n":` keys (bare `n:` parses as FALSE)
