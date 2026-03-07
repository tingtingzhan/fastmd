#' @title `htest` Object
#' 
#' @examples
#' list(
#'  '$t$-test' = t.test(mpg ~ am, data = mtcars)
#' ) |> render2html()
#' 
#' @name htest
NULL



#' @export
md_.htest <- function(x, xnm, ...) {
  
  data.name <- tryCatch(expr = {
    x$data.name |>
      str2lang()
  }, error = identity)
  dnm <- if (inherits(data.name, what = 'error')) {
    if (grepl(pattern = ' by ', x = x$data.name)) {
      x$data.name |>
        strsplit(split = ' by ') |>
        unlist() |>
        paste0('`', .x = _, '`', collapse = ' by ')
    } else x$data.name
  } else if (is.symbol(data.name)) {
    x$data.name
  } else if (data.name[[1L]] == 'xtabs') {
    if (is.null(data.name$formula)) stop('call stats::xtabs() with `formula =`')
    data.name$formula[[2L]] |>
      deparse1() |>
      strsplit(split = ' \\+ ') |>
      unlist() |>
      paste0('`', .x =_, '`', collapse = ' and ')
  } else { # exception handling
    x$data.name |>
      deparse1()
  }
  
  z1 <- sprintf(
    fmt = '%s of %s (%s) is performed using <u>**`R`**</u>.',
    x$method,
    dnm,
    x$p.value |>
      label_pvalue_sym(add_p = TRUE)()
  ) |>
    new(Class = 'md_lines')
  
  z2 <- if (!missing(xnm)) {
    md_.default(x, xnm = xnm, ...)
  } # else NULL
  
  c(z1, z2) # [c.md_lines()]
  
}
