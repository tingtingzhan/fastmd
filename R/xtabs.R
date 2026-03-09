
#' @title \link[stats]{xtabs}
#' 
#' @examples
#' list(
#'  '`xtabs`' = xtabs(~ cyl + vs, data = mtcars)
#' ) |> render2html() # citations not shown, probably \pkg{ftExtra} bug
#' 
#' @name xtabs
NULL


# flextable:::as_flextable.table
#' @importFrom ftExtra as_paragraph_md
#' @export
as_flextable.xtabs <- function(x, ...) {
  
  z <- NextMethod(generic = 'as_flextable')
  # dispatch to
  # ?flextable:::as_flextable.table
  # this function requires dim-2 table too :)
  
  fish <- x |>
    fisher.test() |>
    suppressWarnings() |>
    md_htest_()
  
  chisq <- x |>
    chisq.test() |>
    suppressWarnings() |>
    md_htest_()
  
  z |> 
    add_footer_lines(values = as_paragraph_md(fish)) |>
    add_footer_lines(values = as_paragraph_md(chisq)) |>
    colformat_md(
      part = 'all'
    )
  
}


#' @export
bib_.xtabs <- function(x) {
  c(.fisher22(), .pearson1900(), .yates34())
}



