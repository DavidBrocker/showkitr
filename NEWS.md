# showkitr 0.0.0.9000

* New `fx_glossary()` and `fx_glossary_page()`: build student-friendly glossary
  cards from a function's help page, optionally overridden by a list or YAML
  file. Ships with Lab 3-4 entries in `inst/glossary/lab3_lab4.yml`.
* `fx_showcase()` no longer attaches rlang, gt, magick, or fs on load, and no
  longer uses the unexported `tools:::RdTags()`.
* Fixed Rd parse warnings caused by `\u2014` in roxygen comments.
