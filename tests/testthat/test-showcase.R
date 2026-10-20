# Smoke tests: render real cards to tempfile() and check the files that come
# out. They check that cards are produced at the right size, not how they look.
# Everything uses open = FALSE so no viewer or browser is launched.
skip_if_not_installed("magick")

render_card <- function(expr, ...) {
  out <- tempfile(fileext = ".png")
  res <- fx_showcase(expr, save_path = out, open = FALSE, ...)
  list(path = out, res = res)
}

test_that("a text call renders a square png card", {
  r <- render_card(toupper("hello"))
  expect_true(file.exists(r$path))
  info <- magick::image_info(magick::image_read(r$path))
  expect_equal(c(info$width, info$height), c(1080, 1080))
})

test_that("canvas controls the card size", {
  r <- render_card(toupper("hello"), canvas = "post")
  info <- magick::image_info(magick::image_read(r$path))
  expect_equal(c(info$width, info$height), c(1080, 1350))
})

test_that("the result records the call it showcased", {
  r <- render_card(toupper("hello"))
  expect_true(all(c("png", "before", "after", "fn") %in% names(r$res)))
  expect_equal(r$res$after, "HELLO")
})

test_that("light and dark themes both render", {
  for (th in c("light", "dark")) {
    r <- render_card(toupper("hello"), theme = th)
    expect_true(file.exists(r$path), info = th)
  }
})

test_that("wrapping a call in fx_showcase() twice is refused", {
  expect_error(
    fx_showcase(fx_showcase(toupper("hello"), open = FALSE), open = FALSE),
    "fx_showcase"
  )
})

test_that("a data frame call renders a table card", {
  # Table cards are screenshotted through a headless browser.
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
          "Chrome not available")
  r <- render_card(head(mtcars, 3))
  expect_true(file.exists(r$path))
  expect_gt(file.size(r$path), 10000)
})
