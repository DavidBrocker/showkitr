# showkitr

Before/after showcase cards for function calls — built for
"function of the day" posts (musicKitR, stringr regex demos, etc.),
but works on any function call.

## Install

```r
# install.packages("pak")
pak::pak("DavidBrocker/showkitr")
```

## Use

```r
fx_showcase(str_remove("i have a dog", "[aeiou]"))
fx_showcase(mk_similar_artists("3858833"), tag = "Function of the Day")
```

Or select a call in the editor and run the "Showcase selected call"
Addin (Addins menu) to wrap it with `fx_showcase()` in place.

## Glossary cards

`fx_glossary()` turns a function's help page into a student-friendly card
(every argument, its default, and what it means), saved as one standalone
HTML file. `fx_glossary_page()` stacks many cards into a glossary page.
Add your own plain-language wording with a YAML file:

```r
fx_glossary(pnorm)

yml <- system.file("glossary", "lab3_lab4.yml", package = "showkitr")
fx_glossary_page(c("pnorm", "qnorm", "mean"), entries = yml)
```

Each YAML key is a function name with optional `summary`, `args`, `example`,
`see_also`, `lecture`, and `link`. Quote `"n":` -- a bare `n:` parses as `FALSE`.
Reading YAML needs the `yaml` package.

## Status

First working draft — see inline comments in `R/fx_showcase.R` for
known rough edges (the monospace char-width heuristic used for text
sizing, and a known issue with dense/scattered diff highlights on
long strings).
