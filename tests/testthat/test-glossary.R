# Builds into tempfile() and open = FALSE: no browser, nothing outside tempdir().
build_card <- function(fn, ...) {
  out <- fx_glossary(fn, open = FALSE, save_path = tempfile(fileext = ".html"), ...)
  paste(readLines(out, warn = FALSE), collapse = "\n")
}

test_that("fx_glossary writes a standalone html card", {
  h <- build_card(pnorm)
  expect_match(h, "<!doctype html>", fixed = TRUE)
  expect_match(h, 'id="fx-pnorm"', fixed = TRUE)
  expect_match(h, "<style>", fixed = TRUE)
})

test_that("every formal argument gets a row, and required ones are flagged", {
  h <- build_card(pnorm)
  for (a in names(formals(pnorm))) {
    expect_match(h, sprintf('data-arg="%s"', a), fixed = TRUE)
  }
  expect_match(h, "required", fixed = TRUE)
})

test_that("S3 generics show the arguments of the default method", {
  h <- build_card(mean)
  expect_match(h, 'data-arg="na.rm"', fixed = TRUE)
  expect_match(h, 'data-arg="trim"', fixed = TRUE)
})

test_that("a bare function and its name give the same card", {
  expect_identical(
    regmatches(build_card(pnorm), regexpr('id="fx-[a-z]+"', build_card(pnorm))),
    regmatches(build_card("pnorm"), regexpr('id="fx-[a-z]+"', build_card("pnorm")))
  )
})

test_that("entry overrides replace help-page wording", {
  h <- build_card(pnorm, entry = list(
    summary = "Custom summary text.",
    args = list(q = "Custom q wording."),
    example = "pnorm(q = 1, mean = 0, sd = 1)",
    lecture = "Lecture 6"
  ))
  expect_match(h, "Custom summary text.", fixed = TRUE)
  expect_match(h, "Custom q wording.", fixed = TRUE)
  expect_match(h, "pnorm(q = 1, mean = 0, sd = 1)", fixed = TRUE)
  expect_match(h, "Lecture 6", fixed = TRUE)
})

test_that("html in entries is escaped", {
  h <- build_card(pnorm, entry = list(summary = "<script>alert(1)</script>"))
  expect_false(grepl("<script>alert(1)</script>", h, fixed = TRUE))
  expect_match(h, "&lt;script&gt;", fixed = TRUE)
})

test_that("fx_glossary_page stacks one card per function", {
  out <- fx_glossary_page(
    c("pnorm", "qnorm"), pkg = "stats",
    open = FALSE, save_path = tempfile(fileext = ".html")
  )
  h <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_equal(lengths(regmatches(h, gregexpr("<article", h))), 2L)
  expect_match(h, 'class="toc"', fixed = TRUE)
})

test_that("the bundled lab YAML parses and matches real arguments", {
  skip_if_not_installed("yaml")
  yml <- system.file("glossary", "lab3_lab4.yml", package = "showkitr")
  skip_if(!nzchar(yml))
  entries <- yaml::read_yaml(yml)
  expect_gt(length(entries), 10)
  for (nm in intersect(names(entries), c("pnorm", "qnorm", "dnorm", "sd", "scale"))) {
    f <- get(nm, mode = "function")
    documented <- names(entries[[nm]]$args)
    expect_true(all(documented %in% names(formals(args(f)))),
                info = paste("unknown argument documented for", nm))
  }
})

test_that("fx_glossary rejects non-functions", {
  expect_error(fx_glossary(42, open = FALSE), "function")
})
