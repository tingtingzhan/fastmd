
setOldClass(Classes = 'bibentry')

#' @title S4 Class \linkS4class{md_lines}
#' 
#' @description
#' Text lines to be written to an R Markdown (`.rmd`) file.
#' 
#' @slot .Data \link[base]{character} scalar or \link[base]{vector}
#' 
#' @slot bibentry a \link[utils]{bibentry}
#' 
#' @slot package \link[base]{character} scalar or \link[base]{vector}
#' 
#' @slot chunk.r \link[base]{logical} scalar (default `FALSE`), 
#' whether this is an R code chunk
#' 
#' @slot summary. \link[base]{character} scalar, whether
#' the markdown lines should be folded with a `summary`
#' 
#' @slot fig.height,fig.width \link[base]{double} scalars
#' 
#' @keywords internal
#' @export
setClass(Class = 'md_lines', contains = 'character', slots = c(
  bibentry = 'bibentry',
  package = 'character',
  chunk.r = 'logical',
  summary. = 'character',
  fig.height = 'numeric', fig.width = 'numeric'
), prototype = prototype(
  bibentry = bibentry(),
  chunk.r = FALSE
))



#' @export
sink2.md_lines <- function(x, ...) {
  
  sink(...) # ?base::writeLines cannot append
  
  c(
    '\n',
    x, 
    '\n',
    '# R & R Packages',
    x@package |> 
      sort.int() |>
      c('base', . = _) |> 
      lapply(FUN = \(i) {
        i |> 
          citation() |> 
          format_citation_w_package()
      }) |>
      unlist(use.names = FALSE),
    if (length(x@bibentry)) { # bibliography from `x@bibentry`
      c(
        '# References', 
        '::: {#refs}',
        ':::'
      )
    } # else NULL
  ) |>
    cat(sep = '\n')
  
  sink()
  
}



#' @export
as.character.md_lines <- function(x, ...) {
  c(
    
    if (length(x@summary.)) {
      x@summary. |>
        sprintf(fmt = '<details><summary>%s</summary>')
    },
    
    if (x@chunk.r) '```{r}',
    
    # len-0 compatible
    x@fig.height |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    x@fig.width |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    # end of len-0 compatible
    
    unclass(x), 
    
    if (x@chunk.r) '```',
    
    if (length(x@summary.)) '</details>',
    
    '\n\n'
    
  )
}


#' @export
c.md_lines <- function(...) {
  
  # upstream function needs to make sure all `...` are \linkS4class{md_lines}
  x <- list(...)
  
  z <- x |>
    lapply(FUN = as.character.md_lines) |>
    do.call(what = c, args = _) # ?base::c, Primitive
  
  bib <- x |> 
    lapply(FUN = slot, name = 'bibentry') |>
    do.call(what = c, args = _) |> # ?utils:::c.bibentry
    unique() # ?utils:::unique.bibentry
  
  if (length(bib)) {
    key <- bib |>
      unclass() |> # to use ?base::`[[` instead of ?utils:::`[[.bibentry` (for ?base::lapply)
      lapply(FUN = attr, which = 'key', exact = TRUE)
    # forget `key` in ?utils::bibentry, then no attr(, which = 'key')
    if (any(lengths(key, use.names = FALSE) != 1L)) stop('illegal or missing `key`')
    # let `key` stay a ?base::list
    if (anyDuplicated.default(key)) stop('same key(s) from different bibliography items')
  } 
  
  pkg <- x |>
    lapply(FUN = slot, name = 'package') |>
    unlist(recursive = TRUE, use.names = FALSE) |>
    unique.default() |>
    setdiff(y = installed.packages(priority = 'base') |> rownames())
  
  new(Class = 'md_lines', z, bibentry = bib, package = pkg)
  
}




format_citation_w_package <- \(x) {
  
  pkg <- attr(x, which = 'package', exact = TRUE)
  
  sprintf(
    fmt = '<u>**`%s`**</u> %s\n', 
    switch(pkg, base = 'R', pkg), 
    pkg |>
      citation() |>
      url2doi() |> 
      format(style = 'text') # ?utils:::format.citation
  )
  
}
