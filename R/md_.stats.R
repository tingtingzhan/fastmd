
#' @title Fast Markdown Lines for \link[stats]{factanal} Objects
#' 
#' @param x a \link[stats]{factanal} object
#' 
#' @param ... ..
#' 
#' @examples
#' list(factanal = factanal(USJudgeRatings[-1L], factors = 3L)) |>
#'   render2html(file = 'factanal')
#' @keywords internal
#' @export md_.factanal
#' @export
md_.factanal <- function(x, ...) {
  
  z1 <- x$call$x |>
    deparse1() |>
    sprintf(fmt = 'Factor analysis [@LawleyMaxwell71] of `%s` is performed using <u>**`R`**</u>.') |>
    new(Class = 'md_lines', bibentry = .lawley_maxwell71())
  
  z2 <- md_.default(x, ...)
  
  c(z1, z2) # [c.md_lines()]
  
}






#' @title Fast Markdown Lines for `htest` Object
#' 
#' @param x an `htest` object
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @returns 
#' The `S3` method [md_.htest()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples
#' list(
#'  '$t$-test' = t.test(mpg ~ am, data = mtcars)
#' ) |> render2html(file = 'htest')
#' @keywords internal
#' @export md_.htest
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
