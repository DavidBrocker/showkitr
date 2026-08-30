# showkitr

Before/after showcase cards for function calls — built for
"function of the day" posts (musicKitR, stringr regex demos, etc.),
but works on any function call.

## Install

```r
# devtools::install_github("yourusername/showkitr")   # once it's on GitHub
devtools::load_all()                                    # while developing
```

## Use

```r
fx_showcase(str_remove("i have a dog", "[aeiou]"))
fx_showcase(mk_similar_artists("3858833"), tag = "Function of the Day")
```

Or select a call in the editor and run the "Showcase selected call"
Addin (Addins menu) to wrap it with `fx_showcase()` in place.

## Status

First working draft — see inline comments in `R/fx_showcase.R` for
known rough edges (the monospace char-width heuristic used for text
sizing, and a known issue with dense/scattered diff highlights on
long strings).
