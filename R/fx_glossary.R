# =================================================================
# fx_glossary()  — v0 scaffold
# -----------------------------------------------------------------
# Turn a function's help page into a student-friendly, self-contained
# HTML "glossary card": what it does, every argument (with its default
# and a plain-English description), and a worked example. Cards can be
# stacked into a course glossary page with fx_glossary_page().
#
# Where the words come from (highest priority first):
#   1. `entry`  — a list (or a path to a YAML file) you write by hand,
#                 for student-friendly wording, lecture links, examples.
#   2. The function's Rd help page, read with tools::Rd_db() on the
#      *installed* package (title, description, \arguments).
#   3. formals() — argument names and defaults are always taken from
#      the live function, so they can't drift from the code.
#
# An `entry` looks like this (every field optional):
#
#   list(
#     summary  = "Probability of a score at or below q on a normal curve.",
#     args     = list(q = "The score (or z-score) you are asking about.",
#                     mean = "Center of the distribution. Use 0 for z-scores."),
#     example  = "pnorm(q = 88, mean = 76, sd = 8, lower.tail = TRUE)",
#     see_also = c("qnorm", "dnorm"),
#     lecture  = "Lecture 6",
#     link     = "https://davidbrocker.github.io/348SlideDecks/lec6/lec6_viewer.html"
#   )
#
# Look and feel: inst/glossary/glossary.css (styles) and glossary.js (theme
# buttons, hover-to-find-the-argument, copy button). Both are inlined into
# the saved .html, so a card is still one self-contained file.
#
# Dependencies: tools (base). yaml only if `entry` is a YAML path.
# STATUS: scaffold; tested on stats::pnorm only.
# =================================================================

#' Build a glossary card for a function
#'
#' Reads a function's argument names/defaults from the function itself and
#' its descriptions from the installed help page, optionally overridden by a
#' hand-written `entry`, and writes a self-contained HTML card.
#'
#' @param fn A function, given bare (`pnorm`) or as a string (`"pnorm"`).
#' @param pkg Package the function lives in. Detected from the function's
#'   environment when `NULL`.
#' @param entry Optional list of overrides, or a path to a YAML file holding
#'   one. See the header of `R/fx_glossary.R` for the fields.
#' @param theme One of `"auto"` (follows the viewer's light/dark setting,
#'   default), `"light"`, or `"dark"`.
#' @param save_path Optional output path for the `.html`. Defaults to a
#'   file in [tempdir()].
#' @param open If `TRUE` (default, and only in interactive sessions), opens the
#'   saved file via [utils::browseURL()].
#'
#' @return Invisibly, the path to the saved `.html` file.
#'
#' @examples
#' \dontrun{
#' fx_glossary(pnorm)
#' fx_glossary(pnorm, entry = list(
#'   summary = "Probability of a score at or below q on a normal curve.",
#'   example = "pnorm(q = 88, mean = 76, sd = 8, lower.tail = TRUE)"
#' ))
#' }
#'
#' @export
fx_glossary <- function(fn,
                        pkg = NULL,
                        entry = NULL,
                        theme = c("auto", "light", "dark"),
                        save_path = NULL,
                        open = TRUE) {
  theme <- match.arg(theme)
  fn_expr <- substitute(fn)
  card <- glossary_card(resolve_glossary_fn(fn_expr, fn, parent.frame()),
                        pkg = pkg, entry = entry)

  html <- glossary_page_html(card$html, title = card$name, theme = theme,
                            heading = FALSE)
  save_path <- save_path %||% file.path(
    tempdir(), sprintf("fx_glossary_%s.html", gsub("[^A-Za-z0-9_]", "_", card$name))
  )
  writeLines(html, save_path, useBytes = TRUE)
  if (isTRUE(open) && interactive()) utils::browseURL(save_path)
  invisible(save_path)
}

#' Build a glossary page from several functions
#'
#' Stacks one card per function into a single HTML page: the seed of a
#' course glossary.
#'
#' @param fns Character vector of function names (looked up in `pkg`, or the
#'   search path when `pkg` is `NULL`).
#' @param entries Optional named list of `entry` lists (names are function
#'   names), or a path to a YAML file with one top-level key per function.
#' @inheritParams fx_glossary
#' @param title Page title.
#'
#' @return Invisibly, the path to the saved `.html` file.
#'
#' @examples
#' \dontrun{
#' fx_glossary_page(c("pnorm", "qnorm", "dnorm"), pkg = "stats")
#' }
#'
#' @export
fx_glossary_page <- function(fns,
                             pkg = NULL,
                             entries = NULL,
                             theme = c("auto", "light", "dark"),
                             title = "R Function Glossary",
                             save_path = NULL,
                             open = TRUE) {
  theme <- match.arg(theme)
  entries <- read_glossary_entries(entries)

  fns <- sort(unique(fns))
  cards <- lapply(fns, function(nm) {
    f <- if (is.null(pkg)) get(nm, mode = "function") else getExportedValue(pkg, nm)
    glossary_card(f, name = nm, pkg = pkg, entry = entries[[nm]])
  })

  toc <- paste0(
    '<nav class="toc">',
    paste0('<a href="#fx-', vapply(cards, `[[`, "", "name"), '">',
           vapply(cards, function(k) gl_esc(k$name), ""), "()</a>", collapse = ""),
    "</nav>"
  )
  body <- paste0(toc, paste(vapply(cards, `[[`, "", "html"), collapse = "\n"))

  html <- glossary_page_html(body, title = title, theme = theme)
  save_path <- save_path %||% file.path(tempdir(), "fx_glossary_page.html")
  writeLines(html, save_path, useBytes = TRUE)
  if (isTRUE(open) && interactive()) utils::browseURL(save_path)
  invisible(save_path)
}

# -----------------------------------------------------------------
# Internals
# -----------------------------------------------------------------

# Accept fx_glossary(pnorm) and fx_glossary("pnorm")
resolve_glossary_fn <- function(fn_expr, fn_value, env) {
  if (is.function(fn_value)) {
    attr(fn_value, "gl_name") <- if (is.symbol(fn_expr)) as.character(fn_expr) else
      if (is.call(fn_expr) && length(fn_expr) == 3L && as.character(fn_expr[[1]]) %in% c("::", ":::"))
        as.character(fn_expr[[3]]) else NULL
    return(fn_value)
  }
  if (is.character(fn_value) && length(fn_value) == 1L) {
    f <- get(fn_value, mode = "function", envir = env)
    attr(f, "gl_name") <- fn_value
    return(f)
  }
  stop("`fn` must be a function or a single function name.", call. = FALSE)
}

# --- Card building blocks (the look lives in inst/glossary/glossary.css) ---

# Four-point star: the card's recurring dingbat
gl_star <- function(size = 14, class = NULL) {
  paste0(
    '<svg class="star', if (!is.null(class)) paste0(" ", class), '" width="', size,
    '" height="', size, '" viewBox="0 0 24 24" aria-hidden="true">',
    '<path d="M12 2l2.4 7.6L22 12l-7.6 2.4L12 22l-2.4-7.6L2 12l7.6-2.4z"/></svg>'
  )
}

# Dashed divider with a little row of stars
gl_rule <- function() {
  paste0('<div class="rule" aria-hidden="true"><i></i>',
         gl_star(14), gl_star(20), gl_star(14), "<i></i></div>")
}

# Fake console title bar
gl_termbar <- function(label) {
  paste0('<div class="termbar"><span class="dot a"></span><span class="dot b"></span>',
         '<span class="dot c"></span><label>', gl_esc(label), "</label></div>")
}

# One icon per kind of argument, picked from the argument's default
gl_icon_paths <- c(
  required = '<path d="M3 12V4h8l9 9-8 8z"/><circle cx="7.5" cy="8.5" r="1.2"/>',
  toggle   = '<rect x="2" y="7" width="20" height="10" rx="5"/><circle cx="16" cy="12" r="2.6" fill="currentColor"/>',
  number   = '<path d="M9 3 7 21M17 3l-2 18M4 9h17M3 15h17"/>',
  text     = '<path d="M5 8h4v5a3 3 0 0 1-3 3M14 8h4v5a3 3 0 0 1-3 3"/>',
  none     = '<circle cx="12" cy="12" r="8" stroke-dasharray="3 3.2"/>',
  dots     = '<circle cx="5" cy="12" r="1.6" fill="currentColor"/><circle cx="12" cy="12" r="1.6" fill="currentColor"/><circle cx="19" cy="12" r="1.6" fill="currentColor"/>',
  code     = '<path d="M8 8l-4 4 4 4M16 8l4 4-4 4M14 5l-4 14"/>'
)

gl_tile <- function(kind) {
  paste0(
    '<span class="tile" aria-hidden="true"><svg width="22" height="22" viewBox="0 0 24 24" ',
    'fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" ',
    'stroke-linejoin="round">', gl_icon_paths[[kind]], "</svg></span>"
  )
}

# c("a", "b", "c"): a menu of text options (the first one is the default)
gl_is_choices <- function(def) {
  is.call(def) && identical(def[[1L]], quote(c)) && length(def) > 1L &&
    all(vapply(as.list(def)[-1L], is.character, logical(1)))
}

# What sort of argument is this? Drives the icon and the little note.
gl_arg_kind <- function(a, miss, def) {
  if (a == "...") return("dots")
  if (miss) return("required")
  if (is.null(def)) return("none")
  if (is.logical(def) && length(def) == 1L && !is.na(def)) return("toggle")
  if (is.numeric(def) && length(def) == 1L) return("number")
  if (is.call(def) && identical(def[[1L]], quote(`-`)) && length(def) == 2L &&
      is.numeric(def[[2L]])) return("number")
  if (is.character(def) || gl_is_choices(def)) return("text")
  "code"
}

gl_arg_note <- function(kind, def) {
  switch(kind,
    required = "No default: you must supply it.",
    none     = "NULL means nothing supplied.",
    toggle   = if (isTRUE(def)) "TRUE means on." else "FALSE means off.",
    text     = if (gl_is_choices(def)) "Pick one. If you do not choose, the first option is used." else "",
    ""
  )
}

# One card: returns list(name, pkg, html)
glossary_card <- function(fn, name = NULL, pkg = NULL, entry = NULL) {
  name  <- name %||% attr(fn, "gl_name") %||% "function"
  attr(fn, "gl_name") <- NULL
  entry <- read_glossary_entry(entry)
  pkg   <- pkg %||% glossary_pkg(fn)

  rd   <- glossary_rd(name, pkg)
  summ <- entry$summary %||% rd$description %||% rd$title %||% ""

  # S3 generics like mean() and head() only show (x, ...); the arguments
  # students actually type (na.rm, n) live on the default method.
  shown <- fn
  if (isTRUE(utils::isS3stdGeneric(fn))) {
    dm <- tryCatch(utils::getS3method(name, "default", optional = TRUE), error = function(e) NULL)
    if (is.function(dm)) shown <- dm
  }
  fm      <- formals(args(shown))
  arg_nms <- names(fm) %||% character()
  arg_doc <- utils::modifyList(rd$args, entry$args %||% list())

  # One record per argument. An argument with no default is the empty
  # symbol; never bind it to a variable (that errors on use), test it in place.
  info <- lapply(arg_nms, function(a) {
    miss <- identical(fm[[a]], quote(expr = ))
    def  <- if (miss) NULL else fm[[a]]
    kind <- gl_arg_kind(a, miss, def)
    desc <- arg_doc[[a]] %||% if (a == "...") "Extra arguments passed along to other functions. Usually leave alone." else ""
    list(
      name = a, kind = kind, miss = miss,
      def_txt = if (miss) "" else paste(deparse(def), collapse = " "),
      note = gl_arg_note(kind, def),
      desc = desc
    )
  })

  arg_row <- function(k) {
    paste0(
      '<div class="arg" tabindex="0" data-arg="', gl_esc(k$name), '">',
      '<div class="lhs">', gl_tile(k$kind), '<div class="who">',
      '<div class="nm"><code>', gl_esc(k$name), "</code>",
      if (k$kind == "required") paste0('<span class="badge">', gl_star(10), "required</span>"),
      "</div>",
      if (!k$miss && k$kind != "dots") {
        paste0('<div class="df"><span>default</span><code>', gl_esc(k$def_txt), "</code></div>")
      },
      if (nzchar(k$note)) paste0('<div class="note">', gl_esc(k$note), "</div>"),
      "</div></div>",
      if (nzchar(k$desc)) paste0('<p class="rhs">', gl_esc(k$desc), "</p>")
      else '<p class="rhs none">No description yet.</p>',
      "</div>"
    )
  }
  is_req <- vapply(info, function(k) k$kind == "required", logical(1))
  req_html <- paste(vapply(info[is_req],  arg_row, ""), collapse = "\n")
  opt_html <- paste(vapply(info[!is_req], arg_row, ""), collapse = "\n")

  # The call, written the way students should write it: every argument named
  sig <- Filter(function(k) k$kind != "dots", info)
  sig_lines <- vapply(seq_along(sig), function(i) {
    k <- sig[[i]]
    value <- if (k$miss) '<span class="blank">___</span>' else gl_esc(k$def_txt)
    paste0('<span class="ln" data-arg="', gl_esc(k$name), '"><span class="k">',
           gl_esc(k$name), "</span> = ", value, if (i < length(sig)) ",", "</span>")
  }, "")
  sig_html <- if (length(sig)) {
    paste0('<span class="sig-end">', gl_esc(name), "(</span>",
           paste(sig_lines, collapse = ""), '<span class="sig-end">)</span>')
  } else {
    paste0('<span class="sig-end">', gl_esc(name), "()</span>")
  }

  # --- header -------------------------------------------------
  meta <- c(
    if (!is.null(pkg)) paste0('<span class="sticker">package <b>', gl_esc(pkg), "</b></span>"),
    if (!is.null(entry$lecture)) {
      if (!is.null(entry$link)) {
        paste0('<a class="sticker" href="', gl_esc(entry$link), '">', gl_esc(entry$lecture), "</a>")
      } else {
        paste0('<span class="sticker">', gl_esc(entry$lecture), "</span>")
      }
    }
  )
  header <- paste0(
    '<header class="hd"><div class="sparkle" aria-hidden="true">',
    gl_star(16, "a"), gl_star(38), gl_star(12, "c"), "</div>",
    if (length(meta)) paste0('<p class="meta">', paste(meta, collapse = ""), "</p>"),
    '<h2><span class="wavy">', gl_esc(name), "()</span></h2>",
    if (nzchar(summ)) paste0('<p class="sum">', gl_esc(summ), "</p>"),
    "</header>"
  )

  # --- arguments ----------------------------------------------
  arguments <- if (length(info)) {
    paste0(
      gl_rule(),
      '<section class="sec"><h3 class="big"><span>Arguments</span></h3>',
      '<p class="lead">Name every argument when you use the function. ',
      "Point at one to find it in the call.</p>",
      '<div class="term">', gl_termbar("signature"), "<pre><code>", sig_html, "</code></pre></div>",
      if (any(is_req)) paste0('<div><h4 class="grp">', gl_star(12), 'Required</h4><div class="args">',
                              req_html, "</div></div>"),
      if (any(!is_req)) paste0(
        '<details class="opt" open><summary><span class="grp">', gl_star(12),
        'Optional &middot; these have defaults</span><span class="tog"></span></summary>',
        '<div class="args">', opt_html, "</div></details>"
      ),
      "</section>"
    )
  }

  # --- example ------------------------------------------------
  example <- if (!is.null(entry$example)) {
    paste0(
      gl_rule(),
      '<section class="sec"><h3>', gl_star(14), "Try it</h3>",
      '<div class="term">', gl_termbar("R console"),
      '<div class="ex"><pre><code>', gl_esc(paste(entry$example, collapse = "\n")), "</code></pre>",
      '<button type="button" class="btn" data-copy>',
      '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" ',
      'stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">',
      '<rect x="9" y="9" width="11" height="11" rx="2"/><path d="M5 15V6a2 2 0 0 1 2-2h8"/></svg>',
      "<span>Copy</span></button></div></div></section>"
    )
  }

  see <- if (length(entry$see_also)) {
    paste0('<p class="see">See also: ',
           paste0("<code>", gl_esc(entry$see_also), "()</code>", collapse = ", "), "</p>")
  }

  html <- paste0(
    '<article class="card" id="fx-', gl_esc(name), '">\n',
    paste(c(header, arguments, example, see), collapse = "\n"),
    "\n</article>"
  )

  list(name = name, pkg = pkg, html = html)
}

# Package the function lives in ("base" for primitives)
glossary_pkg <- function(fn) {
  env <- environment(fn)
  if (is.null(env)) return("base")
  nm <- environmentName(env)
  if (nm %in% c("", "R_GlobalEnv")) return(NULL)
  sub("^package:", "", nm)
}

# Title / description / per-argument text from the installed Rd, or empties
glossary_rd <- function(name, pkg) {
  out <- list(title = NULL, description = NULL, args = list())
  if (is.null(pkg)) return(out)
  db <- tryCatch(tools::Rd_db(pkg), error = function(e) NULL)
  if (is.null(db)) return(out)

  hit <- Filter(function(rd) name %in% rd_aliases(rd), db)
  if (!length(hit)) return(out)
  rd <- hit[[1]]

  tags <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", "")
  out$title <- rd_flat(rd[tags == "\\title"])
  out$description <- rd_flat(rd[tags == "\\description"])

  for (blk in rd[tags == "\\arguments"]) {
    for (it in blk) {
      if (!identical(attr(it, "Rd_tag"), "\\item") || length(it) < 2L) next
      nms  <- trimws(strsplit(rd_flat(it[1]), ",")[[1]])
      text <- rd_flat(it[2])
      for (n in nms) out$args[[n]] <- text
    }
  }
  out
}

rd_aliases <- function(rd) {
  unlist(lapply(rd, function(x) {
    if (identical(attr(x, "Rd_tag"), "\\alias")) rd_flat(list(x)) else NULL
  }))
}

# Flatten an Rd fragment to a single plain-text string
rd_flat <- function(x) {
  txt <- function(el) {
    if (is.character(el)) return(paste(el, collapse = ""))
    if (is.list(el)) return(paste(vapply(el, txt, ""), collapse = ""))
    ""
  }
  s <- txt(x)
  s <- gsub("[ \t\r\n]+", " ", s)
  trimws(s)
}

read_glossary_entry <- function(entry) {
  if (is.null(entry)) return(list())
  if (is.character(entry) && length(entry) == 1L && file.exists(entry)) {
    entry <- read_yaml_file(entry)
  }
  entry
}

read_glossary_entries <- function(entries) {
  if (is.null(entries)) return(list())
  if (is.character(entries) && length(entries) == 1L && file.exists(entries)) {
    return(read_yaml_file(entries))
  }
  entries
}

read_yaml_file <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Reading a YAML entry needs the 'yaml' package: install.packages(\"yaml\").", call. = FALSE)
  }
  yaml::read_yaml(path)
}

gl_esc <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

# Read a bundled asset (inst/glossary/<file>) as one string
gl_asset <- function(file) {
  path <- system.file("glossary", file, package = "showkitr")
  if (!nzchar(path)) {
    stop("Can't find inst/glossary/", file, "; try reinstalling showkitr.", call. = FALSE)
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Full standalone page. Styling is in inst/glossary/glossary.css and the
# three small behaviours in glossary.js; both are inlined, so the saved
# .html travels on its own. IBM Plex loads when online and falls back to
# system fonts when not.
glossary_page_html <- function(body, title, theme = "auto", heading = TRUE) {
  seg <- paste0(
    vapply(c("auto", "light", "dark"), function(t) {
      sprintf('<button type="button" data-theme="%s" aria-pressed="%s">%s</button>',
              t, tolower(identical(t, theme)), t)
    }, ""),
    collapse = ""
  )
  paste0(
    '<!doctype html>\n<html lang="en" data-theme="', theme, '"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    "<title>", gl_esc(title), "</title>",
    '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600',
    '&family=IBM+Plex+Sans:wght@400;500;600&display=swap">',
    "<style>", gl_asset("glossary.css"), "</style></head><body><main>",
    '<div class="bar"><span>Card theme</span><div class="seg" role="group" aria-label="Card theme">',
    seg, "</div></div>",
    if (heading) paste0('<h1 class="page-title">', gl_esc(title), "</h1>\n")
    else paste0('<h1 class="sr">', gl_esc(title), "</h1>\n"),
    body, "\n</main><script>", gl_asset("glossary.js"), "</script></body></html>"
  )
}
