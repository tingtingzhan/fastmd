
#' @title xtabs
#' 
#' @examples
#' list('`xtabs`' = xtabs(~ cyl + vs, data = mtcars)) |> render2html()
#' 
#' @name xtabs
NULL




#' @export
md_.xtabs <- function(x, xnm, ...) {
  
  z1 <- if (length(dim(x)) == 2L) {
    sprintf(
      fmt = '[@Fisher22 exact test](https://en.wikipedia.org/wiki/Fisher\'s_exact_test) (%s) and [@Pearson1900 $\\chi^2$ test](https://en.wikipedia.org/wiki/Pearson\'s_chi-squared_test) (%s) on this cross tabulation are performed using <u>**`R`**</u>.',
      x |>
        fisher.test() |>
        suppressWarnings() |>
        getElement(name = 'p.value') |>
        label_pvalue_sym(add_p = TRUE)(),
      x |>
        chisq.test() |>
        suppressWarnings() |>
        getElement(name = 'p.value') |>
        label_pvalue_sym(add_p = TRUE)()
    ) |>
      new(Class = 'md_lines', bibentry = c(.fisher22(), .pearson1900()))
  }# else NULL
  
  z2 <- md_flextable_(xnm = xnm, ...)
  
  c(z1, z2)
  
}
