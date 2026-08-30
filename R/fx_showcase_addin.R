# =================================================================
# fx_showcase_addin()  — RStudio Addin
# -----------------------------------------------------------------
# Wraps the currently selected code in the source editor with a call
# to fx_showcase(), in place. No dialog, no args form — just turns
# whatever call you've already written and tested into a showcase
# call, since by the time you're building a post you already know
# exactly which function and args you want, not browsing for them.
#
# Usage once wired up as an Addin:
#   1. Select   mk_similar_artists("3858833")   in a script
#   2. Run the Addin (Addins menu — or bind it to a keyboard shortcut
#      via Tools > Modify Keyboard Shortcuts once it's installed)
#   3. Selection becomes  fx_showcase(mk_similar_artists("3858833"))
#
# Guards: does nothing (with a message, not silent) if nothing's
# selected, and refuses to double-wrap a selection that already looks
# like an fx_showcase() call.
#
# PACKAGING (this is the part that makes it show up in the Addins
# menu — a bare .R file with this function in it is not enough):
#
#   This needs to live inside an INSTALLED package with an
#   inst/rstudio/addins.dcf entry pointing at it. A small standalone
#   package (e.g. "fxshowcase") makes more sense than bolting this
#   onto musicKitR — fx_showcase() itself is generic, and you'll
#   likely want to showcase calls from other packages too, not just
#   musicKitR.
#
#   1. usethis::create_package("~/path/to/fxshowcase")
#   2. Drop fx_showcase.R and this file into that package's R/ folder
#   3. Add rstudioapi to Imports:
#        usethis::use_package("rstudioapi")
#      (also needs rlang, gt, magick, fs, ggplot2 — same deal)
#   4. Create inst/rstudio/addins.dcf (create the folders too) with:
#
#        Name: Showcase selected call
#        Description: Wrap the selected function call with fx_showcase()
#        Binding: fx_showcase_addin
#        Interactive: false
#
#   5. devtools::load_all() (or install + restart RStudio) — it
#      should now appear under the Addins menu as "Showcase selected
#      call", and you can bind it to a shortcut from there.
# =================================================================

#' Wrap the selected call with fx_showcase() (RStudio Addin)
#'
#' Takes the currently selected text in the active source editor and
#' replaces it with the same code wrapped in a call to
#' [fx_showcase()], in place. Intended to be run as an RStudio Addin
#' (see `inst/rstudio/addins.dcf`), not called directly from a script.
#'
#' Does nothing but show a dialog (rather than silently no-op) if
#' nothing is selected, or if the selection already looks like an
#' `fx_showcase()` call.
#'
#' @return Invisibly `NULL`. Called for its side effect of editing the
#'   active document via [rstudioapi::modifyRange()].
#'
#' @export
fx_showcase_addin <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]

  code <- trimws(sel$text)

  if (nchar(code) == 0) {
    rstudioapi::showDialog(
      "fx_showcase",
      "Select a function call in the editor first, then run the Addin."
    )
    return(invisible())
  }

  if (grepl("^fx_showcase\\(", code)) {
    rstudioapi::showDialog(
      "fx_showcase",
      "That selection already looks like an fx_showcase() call \u2014 leaving it as-is."
    )
    return(invisible())
  }

  wrapped <- paste0("fx_showcase(", code, ")")

  rstudioapi::modifyRange(
    location = sel$range,
    text = wrapped,
    id = ctx$id
  )
}
