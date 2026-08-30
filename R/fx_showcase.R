# =================================================================
# fx_showcase()  — v4
# -----------------------------------------------------------------
# Run a function call, capture its "before" input and "after" output,
# and composite a styled before/after card for sharing.
#
# v4: title/subtitle/logo are now DETECTED from whatever package the
# called function actually lives in, instead of being hardcoded to
# "musicKitR". You can still override any of title/subtitle/tag/handle
# explicitly — detection only fills in what you didn't supply.
#
#   - Package name: eval the call's head symbol, look at the resulting
#     function's environment, ask environmentName() what it's called.
#     R_GlobalEnv / "" means "not really a package" -> falls back.
#   - Subtitle: pulled from the function's Rd \title, via tools::Rd_db()
#     on the *installed* package. If you're iterating with
#     devtools::load_all() rather than a real install, this will come
#     up empty — pkgload::dev_help() is the dev-mode equivalent if you
#     want to wire that in later.
#   - Logo: looked up at the usethis::use_logo() convention path,
#     inst/figures/logo.png -> system.file("figures","logo.png",...).
#     Composited as a small badge, top-right corner (not inline next
#     to the title text — that needs measuring title width mid-draw,
#     which is fiddlier to get right untested; corner badge is the
#     safer default, easy to move once you see how it looks).
#
# STATUS: still untested in a live R session (no REPL access here).
# The monospace char-width heuristic (0.62 * font size) is still the
# first thing to recalibrate if text spacing looks off.
#
# Usage:
#   fx_showcase(str_remove("i have a dog", "[aeiou]"))       # no pkg -> generic title
#   fx_showcase(mk_similar_artists("3858833"))                # auto: title = "musicKitR"
#   fx_showcase(mk_similar_artists("3858833"), tag = "Function of the Day")
#   fx_showcase(mk_similar_artists("3858833"), animate = TRUE)
#
# Dependencies: rlang, gt, magick, fs, tools, ggplot2 (only if `after`
# is a ggplot object).
# =================================================================

library(rlang)
library(gt)
library(magick)
library(fs)

`%||%` <- function(a, b) if (is.null(a)) b else a

# -----------------------------------------------------------------
# Main entry point
# -----------------------------------------------------------------
#' Showcase a function call as a before/after card
#'
#' Runs `expr`, captures its "before" input and "after" output, and
#' composites a styled before/after image for sharing (e.g. a
#' "function of the day" post). When the package the called function
#' belongs to can be detected, the title/subtitle/logo are pulled from
#' that package automatically \u2014 pass any of them explicitly to override.
#'
#' @param expr A function call to run and showcase, e.g.
#'   `mk_similar_artists("3858833")`. Captured unevaluated via
#'   [rlang::enquo()], so the call itself can be shown as a caption.
#' @param before Optional "before" value to display. If omitted, it's
#'   inferred by evaluating the first argument of `expr` in isolation.
#' @param title Optional title text. Defaults, in order, to: the
#'   detected package name, the bare function name, or
#'   `"Function Showcase"`.
#' @param subtitle Optional subtitle. Defaults to the function's Rd
#'   `\\title`, when it can be found via [tools::Rd_db()] on an
#'   *installed* copy of the package (empty under `devtools::load_all()`).
#' @param tag Optional label rendered as a small pill under the title,
#'   e.g. `"Function of the Day"`.
#' @param handle Optional watermark (e.g. `"@yourhandle"`) stamped in
#'   the bottom-right corner.
#' @param max_rows Maximum rows shown for data.frame/tibble output.
#'   Default `6`.
#' @param theme One of `"dark"` (default) or `"light"`.
#' @param canvas One of `"square"` (1080x1080, default), `"story"`
#'   (1080x1920), or `"post"` (1080x1350, a 4:5 ratio that fills more
#'   of the feed on LinkedIn/X than a square does).
#' @param diff_highlight If `TRUE` (default), character-level diff
#'   highlighting is used whenever both `before` and `after` are
#'   single strings.
#' @param include_benchmark If `TRUE`, shows the elapsed run time
#'   between the two cards. Default `FALSE` \u2014 timing is rarely the
#'   point of a showcase and mostly adds noise.
#' @param preserve_ggplot_theme If `FALSE` (default), ggplot objects
#'   are forced into a dark theme matching the card background, for
#'   legibility. Set `TRUE` when the demo IS about theming (e.g.
#'   showcasing what `+ theme_minimal()` does) \u2014 otherwise the forced
#'   override makes before/after look identical regardless of what
#'   theme was actually applied.
#' @param auto_detect If `TRUE` (default), attempts to detect the
#'   source package of the called function to fill in title/subtitle/
#'   logo. Set `FALSE` to always use the generic fallback title.
#' @param animate If `TRUE`, also writes a short reveal `.gif`
#'   alongside the `.png` (holds on the BEFORE card, then reveals the
#'   full panel).
#' @param save_path Optional output path for the PNG. Defaults to a
#'   timestamped file in [tempdir()].
#' @param open If `TRUE` (default), opens the saved PNG via
#'   [utils::browseURL()].
#'
#' @return Invisibly, a list with `png`, `gif` (`NULL` unless
#'   `animate = TRUE`), `before`, `after`, `seconds`, `pkg`, and `fn`.
#'
#' @examples
#' \dontrun{
#' fx_showcase(str_remove("i have a dog", "[aeiou]"))
#' fx_showcase(mk_similar_artists("3858833"), tag = "Function of the Day")
#' }
#'
#' @export
fx_showcase <- function(expr,
                        before = NULL,
                        title = NULL,
                        subtitle = NULL,
                        tag = NULL,
                        handle = NULL,
                        max_rows = 6,
                        theme = c("dark", "light"),
                        canvas = c("square", "story", "post"),
                        diff_highlight = TRUE,
                        include_benchmark = FALSE,
                        preserve_ggplot_theme = FALSE,
                        auto_detect = TRUE,
                        animate = FALSE,
                        save_path = NULL,
                        open = TRUE) {
  
  theme  <- match.arg(theme)
  canvas <- match.arg(canvas)
  
  expr_q    <- rlang::enquo(expr)
  call_ast  <- rlang::quo_get_expr(expr_q)
  expr_text <- rlang::expr_text(call_ast)
  
  # Guard against double-wrapping (e.g. the Addin run on an already-
  # wrapped selection, or a copy/paste mistake) — without this, the
  # outer call silently executes the inner fx_showcase() as part of
  # evaluating `after`, producing a very confusing render.
  if (rlang::is_call(call_ast)) {
    head_name <- tryCatch(rlang::call_name(call_ast), error = function(e) NULL)
    if (identical(head_name, "fx_showcase")) {
      stop(
        "fx_showcase(): `expr` is itself an fx_showcase() call \u2014 looks like ",
        "this got wrapped twice. Pass the inner call directly instead, e.g. ",
        "fx_showcase(str_replace_all(...)) not fx_showcase(fx_showcase(str_replace_all(...))).",
        call. = FALSE
      )
    }
  }
  
  if (is.null(before) && rlang::is_call(call_ast)) {
    first_arg <- rlang::call_args(call_ast)[[1]]
    before <- tryCatch(
      eval(first_arg, envir = rlang::quo_get_env(expr_q)),
      error = function(e) NULL
    )
  }
  
  t0    <- Sys.time()
  after <- rlang::eval_tidy(expr_q)
  secs  <- round(as.numeric(Sys.time() - t0, units = "secs"), 2)
  
  is_string_diff <- isTRUE(diff_highlight) &&
    is.character(before) && length(before) == 1 &&
    is.character(after)  && length(after)  == 1
  
  # --- Auto-detect package identity, only to fill in gaps the caller
  #     didn't already specify
  pkg_name <- NULL
  fn_name  <- NULL
  if (auto_detect && rlang::is_call(call_ast)) {
    detect_ast <- unwrap_plus_chain(call_ast)
    pkg_name <- detect_pkg_name(detect_ast, rlang::quo_get_env(expr_q))
    fn_name  <- tryCatch(rlang::call_name(detect_ast), error = function(e) NULL)
  }
  
  auto_title <- if (!is.null(pkg_name) && !is.null(fn_name)) {
    paste0(pkg_name, "::", fn_name)
  } else {
    pkg_name %||% fn_name
  }
  final_title <- title %||% auto_title %||% "Function Showcase"
  
  final_subtitle <- subtitle
  if (is.null(final_subtitle) && !is.null(pkg_name) && !is.null(fn_name)) {
    final_subtitle <- get_fn_title(fn_name, pkg_name)
  }
  
  logo <- NULL
  if (!is.null(pkg_name)) logo <- get_pkg_logo(pkg_name)
  
  panel <- compose_panel(
    before = before, after = after,
    is_string_diff = is_string_diff,
    call_text = expr_text, elapsed = secs,
    title = final_title, subtitle = final_subtitle,
    tag = tag, handle = handle, logo = logo,
    theme = theme, canvas = canvas, max_rows = max_rows,
    include_benchmark = include_benchmark,
    preserve_ggplot_theme = preserve_ggplot_theme
  )
  
  if (is.null(save_path)) {
    save_path <- fs::path(
      tempdir(),
      paste0("fx_showcase_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    )
  }
  magick::image_write(panel, path = save_path)
  
  gif_path <- NULL
  if (animate) {
    gif_path <- fs::path_ext_set(save_path, "gif")
    build_reveal_gif(before, after, is_string_diff, theme, canvas, panel, gif_path, max_rows)
  }
  
  if (open) utils::browseURL(save_path)
  
  invisible(list(
    png = save_path, gif = gif_path, before = before, after = after,
    seconds = secs, pkg = pkg_name, fn = fn_name
  ))
}

# -----------------------------------------------------------------
# Package / function auto-detection
# -----------------------------------------------------------------

# For `+`-chained calls (the common ggplot2 pattern:
# ggplot(...) + geom_col() + theme_minimal()), the outermost call's
# head is the `+` operator itself, not a meaningful thing to detect a
# package from — R parses that chain as `+`(`+`(ggplot(...), geom_col()),
# theme_minimal()). Walks down the left-hand side until it hits a real
# function call. Only unwraps literal `+` chains, not other infix
# operators (%>%, |>, etc.) — aimed squarely at the ggplot2 pattern,
# not a general infix-operator solution.
unwrap_plus_chain <- function(call_ast) {
  repeat {
    if (!rlang::is_call(call_ast)) break
    head_name <- tryCatch(rlang::call_name(call_ast), error = function(e) NA)
    if (!identical(head_name, "+")) break
    call_ast <- rlang::call_args(call_ast)[[1]]
  }
  call_ast
}

# Evaluate the call's head (works for both `fn(...)` and `pkg::fn(...)`)
# and ask its environment what package it belongs to. Deliberately
# base-R only here (environmentName()) rather than rlang::env_name(),
# since environmentName() already returns the bare package name with
# no "namespace:" prefix to strip back off.
detect_pkg_name <- function(call_ast, env) {
  if (!rlang::is_call(call_ast)) return(NULL)
  head <- call_ast[[1]]
  fn <- tryCatch(eval(head, envir = env), error = function(e) NULL)
  if (!is.function(fn)) return(NULL)
  
  env_nm <- environmentName(environment(fn))
  if (identical(env_nm, "") || identical(env_nm, "R_GlobalEnv")) return(NULL)
  env_nm
}

# Pull the one-line \title from a function's Rd documentation, from
# the INSTALLED package's help database. Uses tools:::RdTags, which is
# unexported (the standard trick for this — pkgdown does the same
# thing under the hood — but technically not a stable API contract).
get_fn_title <- function(fn_name, pkg_name) {
  db <- tryCatch(tools::Rd_db(pkg_name), error = function(e) NULL)
  if (is.null(db)) return(NULL)
  
  matched <- Filter(function(rd) {
    tags <- tools:::RdTags(rd)
    fn_name %in% unlist(rd[tags == "\\alias"])
  }, db)
  if (length(matched) == 0) return(NULL)
  
  rd <- matched[[1]]
  tags <- tools:::RdTags(rd)
  title_txt <- paste(trimws(unlist(rd[tags == "\\title"])), collapse = " ")
  if (nchar(title_txt) == 0) NULL else title_txt
}

# usethis::use_logo() convention: inst/figures/logo.png
get_pkg_logo <- function(pkg_name) {
  path <- tryCatch(system.file("figures", "logo.png", package = pkg_name), error = function(e) "")
  if (identical(path, "") || !file.exists(path)) return(NULL)
  tryCatch(magick::image_read(path), error = function(e) NULL)
}

# -----------------------------------------------------------------
# Palette
# -----------------------------------------------------------------
palette <- function(theme) {
  if (theme == "dark") {
    list(
      bg = "#1E1E1E", card_bg = "#2E2E2E", fg = "#F5F5F3",
      accent = "#5B8CFF", diff_accent = "#3FE8D8",
      removed_bg = "#3D2A2A", added_bg = "#1F3A2E",
      dim = "#8A8A8A"
    )
  } else {
    list(
      bg = "#FFFFFF", card_bg = "#F5F5F3", fg = "#1A1A1A",
      accent = "#3A63D8", diff_accent = "#0E7C86",
      removed_bg = "#FFE1E1", added_bg = "#DFFFE9",
      dim = "#8A8A8A"
    )
  }
}

# Canvas presets: "square" for Instagram feed, "story" for vertical
# Stories/Reels, "post" (4:5) for LinkedIn/X, which give a portrait
# post more screen real estate in-feed than a square does.
canvas_dims <- function(canvas) {
  switch(canvas,
         square = c(1080, 1080),
         story  = c(1080, 1920),
         post   = c(1080, 1350),
         c(1080, 1080)
  )
}

# -----------------------------------------------------------------
# Layout + composition
# -----------------------------------------------------------------
compose_panel <- function(before, after, is_string_diff, call_text, elapsed,
                          title, subtitle, tag, handle, logo,
                          theme, canvas, max_rows, include_benchmark = FALSE,
                          preserve_ggplot_theme = FALSE) {
  
  pal  <- palette(theme)
  dims <- canvas_dims(canvas)
  w <- dims[1]; h <- dims[2]
  
  title_h   <- if (!is.null(tag)) 220 else 170
  caption_h <- if (!is.null(handle)) 150 else 110
  gap       <- 36
  
  card_h <- h - title_h - caption_h - 2 * gap
  card_w <- round(w * 0.42)
  y_cards <- title_h + gap
  
  base <- magick::image_blank(w, h, color = pal$bg)
  base <- magick::image_draw(base)
  
  # Title font scales down for longer titles (e.g. "stringr::str_replace_all")
  # rather than a fixed size that assumes a short single word
  title_size <- fit_text_size(title, max_w = w * 0.86, max_h = 90, max_size = 62, min_size = 30)
  graphics::text(w / 2, 56, labels = title, cex = title_size / 12, font = 2, family = "sans", col = pal$fg)
  
  y_tag <- 118
  if (!is.null(tag)) {
    tag_up <- toupper(tag)
    tw <- graphics::strwidth(tag_up, cex = 1.5, font = 2, family = "mono")
    graphics::rect(w/2 - tw/2 - 24, y_tag - 30, w/2 + tw/2 + 24, y_tag + 14,
                   col = pal$card_bg, border = pal$accent, lwd = 2)
    graphics::text(w/2, y_tag - 8, labels = tag_up, cex = 1.5, font = 2, family = "mono", col = pal$accent)
    y_tag <- y_tag + 46
  }
  if (!is.null(subtitle)) {
    sub_lines <- strwrap(subtitle, width = 60)
    graphics::text(w / 2, y_tag + 8, labels = paste(sub_lines, collapse = "\n"),
                   cex = 1.8, family = "sans", col = pal$dim)
  }
  
  if (include_benchmark) {
    graphics::text(w / 2, y_cards + card_h / 2, labels = paste0("\u2192\n", sprintf("%.2f", elapsed), "s"),
                   cex = 2.0, font = 2, family = "mono", col = pal$accent)
  }
  
  cap_lines <- strwrap(call_text, width = 46)
  graphics::text(w / 2, h - caption_h + 30, labels = paste(cap_lines, collapse = "\n"),
                 cex = 1.5, family = "mono", col = pal$fg)
  
  if (!is.null(handle)) {
    graphics::text(w - 30, h - 24, labels = handle, adj = c(1, 1), cex = 1.3, family = "mono", col = pal$dim)
  }
  
  grDevices::dev.off()
  
  # Logo badge, top-right corner
  if (!is.null(logo)) {
    logo_h <- 64
    logo_r <- magick::image_resize(logo, paste0("x", logo_h))
    logo_w <- magick::image_info(logo_r)$width
    base <- magick::image_composite(
      base, logo_r, offset = paste0("+", w - logo_w - 30, "+", 24)
    )
  }
  
  x_before <- round(w * 0.04)
  x_after  <- round(w * 0.54)
  
  cards <- build_card_pair(before, after, is_string_diff, pal, card_w, card_h, max_rows, preserve_ggplot_theme)
  
  base <- magick::image_composite(base, cards$before, offset = paste0("+", x_before, "+", y_cards))
  base <- magick::image_composite(base, cards$after,  offset = paste0("+", x_after, "+", y_cards))
  
  base
}

# Builds BOTH cards together and stamps labels identically, so BEFORE
# and AFTER can never drift apart in font/size/style
build_card_pair <- function(before, after, is_string_diff, pal, w, h, max_rows, preserve_ggplot_theme = FALSE) {
  if (is_string_diff) {
    ops <- lcs_diff(strsplit(before, "")[[1]], strsplit(after, "")[[1]])
    before_runs <- build_runs(ops, keep = c("same", "removed"), hl_type = "removed")
    after_runs  <- build_runs(ops, keep = c("same", "added"),   hl_type = "added")
    
    before_text <- paste(vapply(before_runs, `[[`, character(1), "text"), collapse = "")
    after_text  <- paste(vapply(after_runs,  `[[`, character(1), "text"), collapse = "")
    
    # Wrap-aware sizing: a single-line-only layout has no floor -- once
    # the shrink-to-fit size hits its minimum, long text just overflows
    # past the card edges instead of stopping. fit_diff_layout() adds
    # actual multi-line wrapping so long diffs get more lines, not an
    # unreadable overflow. Size and wrap width are shared between
    # before/after, same principle as before, so both sides still
    # render the same characters at the same size.
    layout_before <- fit_diff_layout(before_text, max_w = w * 0.86, max_h = h * 0.7)
    layout_after  <- fit_diff_layout(after_text,  max_w = w * 0.86, max_h = h * 0.7)
    size <- min(layout_before$size, layout_after$size)
    # chars_per_line MUST be recomputed at the shared `size` above, not
    # taken as min() of before/after's independently-computed values --
    # those were each computed at that side's own (possibly larger)
    # size, so reusing them here is a stale, artificially tight wrap
    # width left over from a font size no longer being used to render.
    chars_per_line <- max(floor((w * 0.86) / (size * 0.62)), 4)
    
    style <- compute_highlight_style(before_runs)
    before_card <- render_diff_card_content(before_runs, pal, w, h, size, pal$removed_bg, style, chars_per_line)
    after_card  <- render_diff_card_content(after_runs,  pal, w, h, size, pal$added_bg, style, chars_per_line)
  } else {
    before_card <- render_card_content(before, pal, w, h, max_rows, preserve_ggplot_theme)
    after_card  <- render_card_content(after,  pal, w, h, max_rows, preserve_ggplot_theme)
  }
  
  list(
    before = round_corners(stamp_label(before_card, "BEFORE", pal), w, h),
    after  = round_corners(stamp_label(after_card,  "AFTER",  pal), w, h)
  )
}

# Rounds the corners of a card image ("chicklet" style). Base graphics
# has no native rounded-rect primitive, so this traces one by hand —
# a filled polygon made of four quarter-circle arcs — and uses it as a
# white-on-black opacity mask via image_composite(operator="copyopacity").
# Built this way deliberately, rather than reaching for a grid-package
# rounded-rect function I wasn't fully certain exists without testing.
round_corners <- function(img, w, h, radius = 28) {
  pts <- rounded_rect_points(w, h, radius)
  
  mask <- magick::image_graph(w, h, bg = "black")
  op <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, w), ylim = c(0, h))
  graphics::polygon(pts$x, pts$y, col = "white", border = NA)
  graphics::par(op)
  grDevices::dev.off()
  
  magick::image_composite(img, mask, operator = "copyopacity")
}

# Four quarter-circle arcs, one per corner, inset by `r` from each edge.
# The shape is symmetric top-to-bottom, so it doesn't matter which way
# the y-axis is oriented — it traces the same rounded rectangle either way.
rounded_rect_points <- function(w, h, r, n = 20) {
  r <- min(r, w / 2, h / 2)
  arc <- function(cx, cy, a_from, a_to) {
    a <- seq(a_from, a_to, length.out = n)
    data.frame(x = cx + r * cos(a), y = cy + r * sin(a))
  }
  pts <- rbind(
    arc(r, r, pi, 1.5 * pi),
    arc(w - r, r, 1.5 * pi, 2 * pi),
    arc(w - r, h - r, 0, 0.5 * pi),
    arc(r, h - r, 0.5 * pi, pi)
  )
  list(x = pts$x, y = pts$y)
}

# -----------------------------------------------------------------
# Uniform label stamping — the ONLY place a label gets drawn
# -----------------------------------------------------------------
stamp_label <- function(img, label, pal) {
  img <- magick::image_draw(img)
  graphics::text(24, 40, labels = label, adj = c(0, 0), cex = 1.9, font = 2,
                 family = "mono", col = pal$diff_accent)
  grDevices::dev.off()
  img
}

# -----------------------------------------------------------------
# Card content renderers — return RAW content only, exactly w x h,
# no label baked in (see stamp_label above)
# -----------------------------------------------------------------
render_card_content <- function(obj, pal, w, h, max_rows, preserve_ggplot_theme = FALSE) {
  if (is.null(obj)) return(text_card_content("(no value)", pal, w, h))
  
  if (inherits(obj, "data.frame")) {
    tbl <- prep_table_for_card(obj, max_rows)
    gt_tbl <- style_gt(gt::gt(tbl), pal)
    tmp_png <- fs::path(tempdir(), paste0("card_", as.integer(Sys.time()), "_", sample(1e5, 1), ".png"))
    vwidth  <- min(1400, 260 * max(ncol(tbl), 1))
    vheight <- min(1000, 90 * (nrow(tbl) + 1))
    gt::gtsave(gt_tbl, tmp_png, vwidth = vwidth, vheight = vheight)
    return(fit_on_card(magick::image_read(tmp_png), w, h, pal$card_bg))
  }
  
  if (inherits(obj, "ggplot")) {
    tmp_png <- fs::path(tempdir(), paste0("card_", as.integer(Sys.time()), "_", sample(1e5, 1), ".png"))
    if (preserve_ggplot_theme) {
      # Save the object exactly as themed — no dark-card override. Use
      # this when the theme itself IS the thing being showcased (e.g.
      # + theme_minimal()); otherwise before/after look identical
      # regardless of what theme was actually applied. Tradeoff: a
      # plot with a light/white theme will show as a light box against
      # the dark card rather than blending in.
      ggplot2::ggsave(tmp_png, obj, width = 6, height = 5, dpi = 150)
    } else {
      styled <- obj + ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = pal$card_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = pal$card_bg, color = NA),
          text = ggplot2::element_text(color = pal$fg)
        )
      ggplot2::ggsave(tmp_png, styled, width = 6, height = 5, dpi = 150, bg = pal$card_bg)
    }
    return(fit_on_card(magick::image_read(tmp_png), w, h, pal$card_bg))
  }
  
  txt <- paste(utils::capture.output(print(obj)), collapse = "\n")
  text_card_content(txt, pal, w, h)
}

prep_table_for_card <- function(obj, max_rows, max_chars = 22) {
  tbl <- head(obj, max_rows)
  
  url_like <- grepl("url$", names(tbl), ignore.case = TRUE)
  if (any(url_like) && sum(!url_like) >= 1) {
    tbl <- tbl[, !url_like, drop = FALSE]
  }
  
  tbl[] <- lapply(tbl, function(col) {
    if (is.character(col)) {
      ifelse(is.na(col) | nchar(col) <= max_chars, col,
             paste0(substr(col, 1, max_chars - 1), "\u2026"))
    } else col
  })
  tbl
}

style_gt <- function(gt_tbl, pal) {
  gt_tbl |>
    gt::tab_options(
      table.background.color = pal$card_bg,
      table.font.color = pal$fg,
      column_labels.background.color = pal$card_bg,
      table.border.top.style = "none",
      table.border.bottom.style = "none"
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold", color = pal$accent),
      locations = gt::cells_column_labels()
    )
}

text_card_content <- function(txt, pal, w, h) {
  img <- magick::image_blank(w, h, color = pal$card_bg)
  img <- magick::image_draw(img)
  
  fit <- wrap_and_size(txt, max_w = w * 0.86, max_h = h * 0.66)
  n_lines <- length(fit$lines)
  line_h <- fit$size * 1.35
  y_start <- h / 2 - (n_lines - 1) * line_h / 2
  
  for (i in seq_along(fit$lines)) {
    graphics::text(w / 2, y_start + (i - 1) * line_h, labels = fit$lines[i],
                   cex = fit$size / 12, font = 2, family = "mono", col = pal$fg)
  }
  grDevices::dev.off()
  img
}

render_diff_card_content <- function(runs, pal, w, h, size, hl_bg, style = "box", chars_per_line = NULL) {
  char_w <- size * 0.62
  if (is.null(chars_per_line)) {
    chars_per_line <- max(floor((w * 0.86) / char_w), 4)
  }
  lines <- wrap_runs(runs, chars_per_line)
  
  line_h <- size * 1.35
  
  # Hard cap on how many lines actually get drawn, matching the same
  # height budget fit_diff_layout() sized against (h * 0.7). Sizing
  # alone isn't a guarantee — min_size stops shrinking at some point
  # regardless of content length — so this is the actual backstop:
  # truncate visibly with "…" rather than let text run past the card.
  max_lines <- max(floor((h * 0.7) / line_h), 1)
  if (length(lines) > max_lines) {
    lines <- lines[seq_len(max_lines)]
    last <- length(lines)
    lines[[last]] <- c(lines[[last]], list(list(text = " \u2026", highlight = FALSE)))
  }
  
  img <- magick::image_blank(w, h, color = pal$card_bg)
  img <- magick::image_draw(img)
  
  y_start <- h / 2 - (length(lines) - 1) * line_h / 2
  
  for (li in seq_along(lines)) {
    line_runs <- lines[[li]]
    line_text <- paste(vapply(line_runs, `[[`, character(1), "text"), collapse = "")
    line_w <- nchar(line_text) * char_w
    x_cursor <- (w - line_w) / 2
    y_mid <- y_start + (li - 1) * line_h
    
    for (run in line_runs) {
      run_w <- nchar(run$text) * char_w
      if (isTRUE(run$highlight) && style == "box") {
        graphics::rect(x_cursor - 3, y_mid - size * 0.62, x_cursor + run_w + 3, y_mid + size * 0.42,
                       col = hl_bg, border = NA)
      }
      col <- if (isTRUE(run$highlight)) pal$diff_accent else pal$fg
      graphics::text(x_cursor, y_mid, labels = run$text, adj = c(0, 0.35),
                     cex = size / 12, font = 2, family = "mono", col = col)
      if (isTRUE(run$highlight) && style == "underline" && !grepl("^_+$", run$text)) {
        y_line <- y_mid + size * 0.78
        graphics::segments(x_cursor, y_line, x_cursor + run_w, y_line,
                           col = pal$diff_accent, lwd = max(size / 8, 3))
      }
      x_cursor <- x_cursor + run_w
    }
  }
  grDevices::dev.off()
  img
}

# Splits a run list across lines. Prefers keeping each run INTACT --
# a highlighted token like "{\\s}" split mid-way by a line break
# ("{\\s" on one line, "}" on the next) visually reads as broken, even
# though the underlying text wrapping is "correct." So: if a run fits
# within one line by itself, push it whole to the next line rather
# than slicing through it. Only a run longer than a full line gets
# split character-by-character, since at that point it has no choice.
wrap_runs <- function(runs, max_chars_per_line) {
  lines <- list()
  current_line <- list()
  current_len <- 0
  
  start_new_line <- function() {
    lines[[length(lines) + 1]] <<- current_line
    current_line <<- list()
    current_len <<- 0
  }
  
  for (run in runs) {
    run_len <- nchar(run$text)
    
    if (run_len <= max_chars_per_line) {
      if (current_len + run_len > max_chars_per_line && current_len > 0) {
        start_new_line()
      }
      current_line[[length(current_line) + 1]] <- run
      current_len <- current_len + run_len
    } else {
      # Doesn't fit on any single line regardless -- has to be split.
      txt <- run$text
      while (nchar(txt) > 0) {
        remaining <- max_chars_per_line - current_len
        if (remaining <= 0) {
          start_new_line()
          remaining <- max_chars_per_line
        }
        take <- min(nchar(txt), remaining)
        chunk <- substr(txt, 1, take)
        current_line[[length(current_line) + 1]] <- list(text = chunk, highlight = run$highlight)
        current_len <- current_len + take
        txt <- substr(txt, take + 1, nchar(txt))
      }
    }
  }
  if (length(current_line) > 0) lines[[length(lines) + 1]] <- current_line
  if (length(lines) == 0) lines <- list(list(list(text = "", highlight = FALSE)))
  lines
}

fit_on_card <- function(img, w, h, bg) {
  info <- magick::image_info(img)
  scale <- min(w / info$width, h / info$height)
  new_w <- round(info$width * scale); new_h <- round(info$height * scale)
  resized <- magick::image_resize(img, paste0(new_w, "x", new_h))
  canvas <- magick::image_blank(w, h, color = bg)
  off_x <- round((w - new_w) / 2); off_y <- round((h - new_h) / 2)
  magick::image_composite(canvas, resized, offset = paste0("+", off_x, "+", off_y))
}

# -----------------------------------------------------------------
# Text sizing helpers (monospace-width heuristic: char ~= 0.62 * size)
# -----------------------------------------------------------------
fit_text_size <- function(txt, max_w, max_h, max_size = 100, min_size = 16) {
  n <- max(nchar(txt), 1)
  size_by_width  <- max_w / (n * 0.62)
  size_by_height <- max_h / 1.4
  max(min(floor(min(size_by_width, size_by_height, max_size))), min_size)
}

wrap_and_size <- function(txt, max_w, max_h, max_size = 90, min_size = 16) {
  # Try ONE line first, same principle as fit_diff_layout(): starting
  # from a large size and a small resulting chars-per-line can force
  # short/medium text into unwanted wrapping before ever checking
  # whether it fits fine on one line at a smaller size. Skipped when
  # txt already has its own embedded line breaks (e.g. multi-line
  # print() output like a list) -- those breaks are intentional print
  # formatting and shouldn't be collapsed onto one line.
  if (!grepl("\n", txt, fixed = TRUE)) {
    n <- max(nchar(txt), 1)
    one_line_size <- floor(min(max_w / (n * 0.62), max_h / 1.4, max_size))
    if (one_line_size >= min_size) {
      return(list(lines = txt, size = max(one_line_size, min_size)))
    }
  }
  
  size <- max_size
  repeat {
    char_w <- size * 0.62
    chars_per_line <- max(floor(max_w / char_w), 5)
    lines <- strwrap(txt, width = chars_per_line)
    total_h <- length(lines) * size * 1.35
    if (total_h <= max_h || size <= min_size) break
    size <- size - 2
  }
  list(lines = lines, size = size)
}

# Tries to fit everything on ONE line first, at whatever size that
# requires — this is what makes short diffs (a single swapped word)
# render big and clean, matching the original single-line behavior.
# Only falls back to multi-line wrapping when a single line genuinely
# doesn't fit even at min_size. This ordering matters: computing a
# wrap width first and shrinking from a large starting size (the
# earlier approach) force-wraps short strings into small, choppy
# lines before ever checking whether they'd fit fine on one line.
fit_diff_layout <- function(txt, max_w, max_h, max_size = 100, min_size = 14) {
  n <- max(nchar(txt), 1)
  
  one_line_size <- floor(min(max_w / (n * 0.62), max_h / 1.4, max_size))
  if (one_line_size >= min_size) {
    return(list(size = max(one_line_size, min_size), chars_per_line = n))
  }
  
  size <- max_size
  repeat {
    char_w <- size * 0.62
    chars_per_line <- max(floor(max_w / char_w), 4)
    n_lines <- ceiling(n / chars_per_line)
    total_h <- n_lines * size * 1.35
    if (total_h <= max_h || size <= min_size) break
    size <- size - 2
  }
  list(size = size, chars_per_line = max(floor(max_w / (size * 0.62)), 4))
}

# -----------------------------------------------------------------
# Character-level diff (LCS-based) for string before/after highlighting
# -----------------------------------------------------------------
lcs_diff <- function(a, b) {
  la <- length(a); lb <- length(b)
  dp <- matrix(0L, la + 1, lb + 1)
  for (i in seq_len(la)) for (j in seq_len(lb)) {
    dp[i + 1, j + 1] <- if (a[i] == b[j]) dp[i, j] + 1L else max(dp[i, j + 1], dp[i + 1, j])
  }
  i <- la; j <- lb; ops <- list()
  while (i > 0 && j > 0) {
    if (a[i] == b[j]) {
      ops[[length(ops) + 1]] <- list(char = a[i], type = "same"); i <- i - 1; j <- j - 1
    } else if (dp[i, j + 1] >= dp[i + 1, j]) {
      ops[[length(ops) + 1]] <- list(char = a[i], type = "removed"); i <- i - 1
    } else {
      ops[[length(ops) + 1]] <- list(char = b[j], type = "added"); j <- j - 1
    }
  }
  while (i > 0) { ops[[length(ops) + 1]] <- list(char = a[i], type = "removed"); i <- i - 1 }
  while (j > 0) { ops[[length(ops) + 1]] <- list(char = b[j], type = "added"); j <- j - 1 }
  rev(ops)
}

build_runs <- function(ops, keep, hl_type) {
  filtered <- Filter(function(op) op$type %in% keep, ops)
  if (length(filtered) == 0) return(list(list(text = "", highlight = FALSE)))
  
  runs <- list()
  cur_text <- filtered[[1]]$char
  cur_hl   <- filtered[[1]]$type == hl_type
  for (k in seq(2, length(filtered))) {
    op <- filtered[[k]]
    hl <- op$type == hl_type
    if (hl == cur_hl) {
      cur_text <- paste0(cur_text, op$char)
    } else {
      runs[[length(runs) + 1]] <- list(text = cur_text, highlight = cur_hl)
      cur_text <- op$char; cur_hl <- hl
    }
  }
  runs[[length(runs) + 1]] <- list(text = cur_text, highlight = cur_hl)
  runs
}

# Sparse diffs (one or two long, localized changes) read best with a
# filled highlight box. Dense diffs (many scattered short changes,
# e.g. every vowel in a sentence) turn into visual noise as boxes —
# switch those to colored text + underline instead, which stays
# legible even as the font shrinks to fit a long string.
compute_highlight_style <- function(runs) {
  hl_runs <- Filter(function(r) isTRUE(r$highlight), runs)
  if (length(hl_runs) == 0) return("box")
  n_runs  <- length(hl_runs)
  avg_len <- mean(vapply(hl_runs, function(r) nchar(r$text), numeric(1)))
  if (n_runs >= 4 && avg_len <= 2) "underline" else "box"
}

# -----------------------------------------------------------------
# Optional reveal GIF: hold on "before" card, then reveal full panel
# -----------------------------------------------------------------
build_reveal_gif <- function(before, after, is_string_diff, theme, canvas, panel, gif_path, max_rows) {
  pal  <- palette(theme)
  dims <- canvas_dims(canvas)
  big_w <- round(dims[1] * 0.7); big_h <- round(dims[2] * 0.35)
  
  if (is_string_diff) {
    ops <- lcs_diff(strsplit(before, "")[[1]], strsplit(after, "")[[1]])
    before_runs <- build_runs(ops, keep = c("same", "removed"), hl_type = "removed")
    before_text <- paste(vapply(before_runs, `[[`, character(1), "text"), collapse = "")
    layout <- fit_diff_layout(before_text, max_w = big_w * 0.86, max_h = big_h * 0.7)
    style <- compute_highlight_style(before_runs)
    solo <- render_diff_card_content(before_runs, pal, big_w, big_h, layout$size, pal$removed_bg, style, layout$chars_per_line)
  } else {
    solo <- render_card_content(before, pal, big_w, big_h, max_rows)
  }
  solo <- stamp_label(solo, "BEFORE", pal)
  solo <- round_corners(solo, big_w, big_h)
  
  hold_frame <- magick::image_blank(dims[1], dims[2], color = pal$bg)
  hold_frame <- magick::image_composite(hold_frame, solo, gravity = "center")
  
  frames <- c(rep(hold_frame, 6), rep(panel, 10))
  gif <- magick::image_animate(magick::image_join(frames), fps = 4)
  magick::image_write(gif, gif_path)
}