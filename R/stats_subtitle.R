#' Build a ggtext-compatible subtitle from emmeans contrast results
#'
#' Generates a markdown string summarising one or more contrast tests,
#' formatted for use with ggtext::element_markdown() in a ggplot2
#' subtitle. Designed to work with any emmeans::contrast() summary
#' object, so it can be reused across plot types.
#'
#' Each contrast row is reported as:
#' `label: *t*(df) = t.ratio, *p* p.value, *d* = cohen.d`
#'
#' Cohen's d is computed via effectsize::t_to_d() with `signed = TRUE`,
#' which preserves the direction of the effect (negative when the contrast
#' t-ratio is negative).
#'
#' @param contrasts A summary()ed emmeans contrast object (a data frame with
#'   columns `t.ratio`, `df`, and `p.value`).
#' @param labels Character vector of condition labels, one per row of
#'   `contrasts`, in the same order as the rows.
#' @param header Optional character string printed as a bold header above the
#'   contrast rows (e.g. `"Recogniser vs. Non-recogniser"`). `NULL` omits it.
#' @param separator String placed between contrast rows. Use `"<br>"` (default)
#'   for one contrast per line, or `"; "` to run them inline.
#' @param footnote Optional character string appended after the last contrast
#'   row, preceded by a `"<br>"`. Supports ggtext markdown. `NULL` omits it.
#' @param signed Logical. Should Cohen's d preserve the sign of the t-ratio?
#'   Defaults to `TRUE`. Set to `FALSE` to always report \|d\|.
#'
#' @returns A single character string of ggtext-compatible markdown, suitable
#'   for use in ggplot2::labs(subtitle = ...).
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming `my_contrasts` is a summary()ed emmeans contrast object:
#' subtitle <- make_contrast_subtitle(
#'   contrasts = my_contrasts,
#'   labels    = c("Intact vs. Non-intact"),
#'   header    = "Primary contrast",
#'   footnote  = "Bonferroni corrected"
#' )
#'
#' ggplot2::ggplot(my_data, ggplot2::aes(x, y)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(subtitle = subtitle) +
#'   theme_pcj() +
#'   ggplot2::theme(
#'     plot.subtitle = ggtext::element_markdown(lineheight = 1.2)
#'   )
#' }
make_contrast_subtitle <- function(contrasts,
                                   labels,
                                   header    = NULL,
                                   separator = "<br>",
                                   footnote  = NULL,
                                   signed    = TRUE) {

  d_vals <- effectsize::t_to_d(
    t        = contrasts$t.ratio,
    df_error = contrasts$df,
    signed   = signed
  )$d

  rows <- mapply(
    FUN = function(label, t, df, p, d) {
      paste0(
        label, ": *t*(", df, ") = ",
        sprintf("%.2f", t),
        ", *p* ", format_p(p),
        ", *d* = ", sprintf("%.2f", d)
      )
    },
    label    = labels,
    t        = contrasts$t.ratio,
    df       = contrasts$df,
    p        = contrasts$p.value,
    d        = d_vals,
    SIMPLIFY = TRUE
  )

  body <- paste(rows, collapse = separator)

  if (!is.null(header)) {
    body <- paste0("**", header, "**<br>", body)
  }
  if (!is.null(footnote)) {
    body <- paste0(body, "<br>", footnote)
  }

  body
}