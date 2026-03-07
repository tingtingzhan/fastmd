
#' @title Adjusted \eqn{p}-Values in All Correction Methods
#' 
#' @description 
#' Adjusted \eqn{p}-values in all correction methods.
#' 
#' @param x see **Usage**
#' 
#' @returns
#' All method dispatches of the `S3` generic function [p_adjust_()] return a \link[base]{matrix} of adjusted \eqn{p}-values using all available \link[stats]{p.adjust.methods}.
#' 
#' @examples
#' runif(n = 10L, min = .01, max = .2) |>
#'  p_adjust_() |>
#'  list('Multile Test Adjustment' = _) |>
#'  render2html()
#' 
#' @keywords internal
#' @name p_adjust_
#' @export
p_adjust_ <- function(x) UseMethod(generic = 'p_adjust_')


#' @rdname p_adjust_
#' @export p_adjust_.numeric
#' @export
p_adjust_.numeric <- function(x) {
  
  method <- p.adjust.methods |> 
    setdiff(y = c('fdr', if (sum(!is.na(x)) == 2L) 'hommel'))
  # 'fdr' is 'BH'
  # 'hommel' for n == 2L is 'hochberg'
  
  ret <- method |>
    lapply(FUN = p.adjust, p = x) |>
    do.call(what = cbind) |>
    pmin(1)
  
  colnames(ret) <- method
  class(ret) <- 'p_adjust'
  return(ret)
  
}









#' @importFrom dplyr recode_values
#' @importFrom ftExtra colformat_md
#' @export
as_flextable.p_adjust <- function(x, ...) {
  
  z <- x |>
    unclass() |> # back to 'matrix'
    label_pvalue_sym()()
  
  nm <- colnames(z)
  colnames(z) <- nm |>
    recode_values(
      'holm' ~ '@Holm79',
      'hochberg' ~ '@Hochberg88',
      'hommel' ~ '@Hommel88',
      'bonferroni' ~ 'Bonferroni',
      'BH' ~ '@BenjaminiHochberg95',
      'BY' ~ '@BenjaminiYekutieli01',
      default = nm
    )
  
  z |>
    as_flextable.matrix() |> 
    colformat_md(
      # part = 'all' # cross-ref correct, but bib does not print! must be a bug
      part = 'header' # https://stackoverflow.com/questions/70892751/citing-inside-the-cell-of-flextable
    ) 
    
}





