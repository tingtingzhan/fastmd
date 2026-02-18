
#' @title Convert \eqn{p}-values of `pairwise.htest` Object into \link[flextable]{flextable}
#' 
#' @description
#' Convert \eqn{p}-values of a `pairwise.htest` object into a \link[flextable]{flextable}.
#' 
#' @param x a `'pairwise.htest'` object, as returned from functions \link[stats]{pairwise.t.test},
#' \link[stats]{pairwise.wilcox.test} or
#' \link[stats]{pairwise.prop.test}.
#' 
#' @param row.title,... additional parameters of function [as_flextable.matrix()].
#' 
#' @returns
#' The `S3` method [as_flextable.pairwise.htest] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @method as_flextable pairwise.htest
#' @export as_flextable.pairwise.htest
#' @export
as_flextable.pairwise.htest <- function(x, row.title = x$method, ...) {
  x$p.value |>
    label_pvalue_sym()() |>
    as_flextable.matrix(row.title = row.title, ...)
}








#' @title Markdown Lines for `pairwise.htest` object
#' 
#' @param x `pairwise.htest` object
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' list(
#'   '`pairwise.htest`' = airquality |> 
#'     within.data.frame(expr = {
#'       Month = factor(Month, labels = month.abb[5:9])
#'     }) |>
#'     with(expr = pairwise.t.test(Ozone, Month, pool.sd = FALSE, p.adj = 'none'))
#' ) |> render2html(file = 'pairwise.htest')
#' 
#' @keywords internal
#' @export md_.pairwise.htest
#' @export
md_.pairwise.htest <- function(x, xnm, ...) {
  
  c(
    
    x$method |> 
      sprintf(fmt = 'Pairwise %s are performed using <u>**`R`**</u>.'),
    '```{r}',
    sprintf(fmt = '(%s) |> as_flextable.pairwise.htest()', xnm), 
    '```',
    
    'Adjusted $p$-values [@Holm79; @Hochberg88; @Hommel88; @BenjaminiHochberg95; @BenjaminiYekutieli01] for multiple comparison are provided as well.',
    '```{r}',
    sprintf(fmt = '(%s) |> p_adjust_.pairwise.htest() |> as_flextable.p_adjust()', xnm), 
    '```'
    
  ) |> 
    new(Class = 'md_lines', bibentry = c(
      .holm79(),
      .hochberg88(),
      .hommel88(),
      .benjamini_hochberg95(),
      .benjamini_yekutieli01()
    ))
  
  # multiple ?flextable::flextable can be put in one code-chunk :)
}

