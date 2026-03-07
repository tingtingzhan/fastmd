


#' @title `pairwise.htest` object
#' 
#' @description
#' The returned object from functions \link[stats]{pairwise.t.test},
#' \link[stats]{pairwise.wilcox.test} or
#' \link[stats]{pairwise.prop.test}.
#' 
#' @examples
#' list(
#'   '`pairwise.htest`' = airquality |> 
#'     within.data.frame(expr = {
#'       Month = factor(Month, labels = month.abb[5:9])
#'     }) |>
#'     with(expr = pairwise.t.test(Ozone, Month, pool.sd = FALSE, p.adj = 'none'))
#' ) |> render2html()
#' 
#' @name pairwise_htest
NULL




#' @method as_flextable pairwise.htest
#' @export
as_flextable.pairwise.htest <- function(x, row.title = x$method, ...) {
  x$p.value |>
    label_pvalue_sym()() |>
    as_flextable.matrix(row.title = row.title, ...)
}


#' @method p_adjust_ pairwise.htest
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



#' @export
md_.pairwise.htest <- function(x, xnm, ...) {
  
  z1 <- x$method |> 
    sprintf(fmt = 'Pairwise %s are performed using <u>**`R`**</u>.  Adjusted $p$-values [@Holm79; @Hochberg88; @Hommel88; @BenjaminiHochberg95; @BenjaminiYekutieli01] for multiple comparison are provided as well.') |> 
    new(Class = 'md_lines')
  
  z2 <- md_flextable_(x = x, xnm = xnm, ...) # to get default bib_()
  
  z3 <- xnm |> 
    sprintf(fmt = '(%s) |> p_adjust_()') |>
    md_flextable_(xnm = _, ...)
  
  c(z1, z2, z3)
  
}



