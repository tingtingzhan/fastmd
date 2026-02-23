
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
#'  list(p.adjust = _) |>
#'  render2html() # bib not shown; not sure why 
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






#' @rdname p_adjust_
#  a `'pairwise.htest'` object, as returned from functions \link[stats]{pairwise.t.test},
# \link[stats]{pairwise.wilcox.test} or
# \link[stats]{pairwise.prop.test}.
#' 
#' @method p_adjust_ pairwise.htest
#' @export p_adjust_.pairwise.htest
#' @export
p_adjust_.pairwise.htest <- function(x) {
  
  if (x$p.adjust.method != 'none') stop('input must use `p.adjust.method = \'none\'`')
  if (!is.matrix(pv0 <- x$p.value)) stop('input must have matrix `$p.value`')
  
  id <- lower.tri(pv0, diag = TRUE)
  pv <- pv0[id]
  
  dnm <- dimnames(pv0)
  names(pv) <- outer(dnm[[1L]], dnm[[2L]], FUN = paste, sep = ' vs. ')[id]
  
  ret <- p_adjust_.numeric(pv) # 'matrix'
  names(dimnames(ret)) <- c(x$method, '') # for ?as_flextable.matrix
  return(ret)
  
}



#' @title Convert `p_adjust` object to a \link[flextable]{flextable}
#' 
#' @param x a `p_adjust` object
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom dplyr recode_values
#' @importFrom ftExtra colformat_md
#' @export as_flextable.p_adjust
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

#' @export
md_.p_adjust <- function(x, xnm, ...) {
  md_flextable_(xnm = xnm, bibentry = c(
    .holm79(),
    .hochberg88(),
    .hommel88(),
    .benjamini_hochberg95(),
    .benjamini_yekutieli01()
  ))
}


