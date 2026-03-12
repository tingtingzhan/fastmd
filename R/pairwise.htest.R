


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
as_flextable.pairwise.htest <- function(x, ...) {
  x$p.value |>
    label_pvalue_sym()() |>
    as_flextable.matrix(...) |>
    set_caption(
      caption = x$method |> sprintf(fmt = 'Pairwise %s')
    )
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
  attr(ret, which = 'method') <- x$method |> sprintf(fmt = 'Pairwise %s')
  return(ret)
  
}



#' @export
md_.pairwise.htest <- function(x, xnm, ...) {
  
  z1 <- md_int(x = x, xnm = xnm, engine = 'flextable', ...)
  
  z2 <- xnm |> 
    sprintf(fmt = '(%s) |> p_adjust_()') |>
    md_int(x = x, xnm = _, engine = 'flextable', ...)
  
  c(z1, z2)
  
}




