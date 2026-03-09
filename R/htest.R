#' @title `htest` Object
#' 
#' @seealso
#' \link[flextable]{as_flextable.htest}
#' 
#' @examples
#' list(
#'  '$t$-test' = t.test(mpg ~ am, data = mtcars)
#' ) |> render2html()
#' 
#' @name htest
NULL


md_htest_ <- \(x, ...) {
  
  pv <- x |> 
    getElement(name = 'p.value')
  if (is.na(pv)) return(new(Class = 'md_lines')) # exception handling
  
  p <- pv |>
    label_pvalue_sym(add_p = TRUE)()
  
  x$method |>
    switch(EXPR = _, 'Fisher\'s Exact Test for Count Data' = {
      # stats::fisher.test
      p |>
        sprintf(fmt = '[@Fisher22 exact test](https://en.wikipedia.org/wiki/Fisher\'s_exact_test) (%s)') |>
        new(Class = 'md_lines', bibentry = .fisher22())
      
    }, 'Pearson\'s Chi-squared test' = {
      # stats::chisq.test
      p |>
        #sprintf(fmt = '[@Pearson1900 $\\chi^2$ test](https://en.wikipedia.org/wiki/Pearson\'s_chi-squared_test) (%s)') |> # cause error in ftExtra::as_paragraph_md ???
        sprintf(fmt = '[@Pearson1900 \u03c7\u00b2 test](https://en.wikipedia.org/wiki/Pearson\'s_chi-squared_test) (%s)') |>
        new(Class = 'md_lines', bibentry = .pearson1900())
      
    }, 'Pearson\'s Chi-squared test with Yates\' continuity correction' = {
      # stats::chisq.test
      p |>
        sprintf(fmt = '[@Pearson1900 \u03c7\u00b2 test](https://en.wikipedia.org/wiki/Pearson\'s_chi-squared_test) with [@Yates34 continuity correction](https://en.wikipedia.org/wiki/Yates\'s_correction_for_continuity) (%s)') |>
        new(Class = 'md_lines', bibentry = c(.pearson1900(), .yates34()))
    })
}




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
    .Defunct(msg = 'use stats::xtabs directly')
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
