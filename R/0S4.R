

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
#' @keywords internal
#' @export
setClass(Class = 'md_lines', contains = 'character', slots = c(
  bibentry = 'bibentry',
  package = 'character',
  chunk.r = 'logical'
), prototype = prototype(
  bibentry = bibentry(),
  chunk.r = FALSE
))


#' @title `S3` method [sink2.md_lines()]
#' 
#' @description
#' \link[base]{sink} an \linkS4class{md_lines} object to an `.rmd` file.
#' 
#' @param x an \linkS4class{md_lines} object
#' 
#' @param ...,append parameters of the function \link[base]{sink}
#' 
#' @details
#' The `S3` method [sink2.md_lines()]
#' is an internal workhorse for
#' the function [render2html()].
#' 
#' @keywords internal
#' @export sink2.md_lines
#' @export
sink2.md_lines <- function(x, ..., append = TRUE) {
  
  z <- c(
    '\n',
    if (x@chunk.r) '```{r}',
    x, 
    if (x@chunk.r) '```',
    '\n',
    '# R & R Packages', # from `x@package`
    x@package |> 
      sort.int() |>
      c('base', . = _) |> 
      lapply(FUN = \(i) i |> citation() |> md_.citation()) |>
      unlist(use.names = FALSE)
  )
  
  if (length(x@bibentry)) { # bibliography from `x@bibentry`
    z <- c(
      z,
      '# References', 
      '::: {#refs}',
      ':::'
    )
  }
  
  sink(..., append = append) # ?base::writeLines cannot append
  z |>
    cat(sep = '\n')
  sink()
  
}



#' @title Convert \linkS4class{md_lines} to \link[base]{character}
#' 
#' @description
#' Convert an \linkS4class{md_lines} object to 
#' a \link[base]{character} \link[base]{vector}.
#' 
#' @param x an \linkS4class{md_lines} object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @returns
#' The `S3` method [as.character.md_lines()] returns 
#' a \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export as.character.md_lines
#' @export
as.character.md_lines <- function(x, ...) {
  c(
    if (x@chunk.r) '```{r}',
    unclass(x), 
    if (x@chunk.r) '```',
    '\n\n'
  )
}


#' @title Combine Multiple \linkS4class{md_lines}
#' 
#' @param ... one or more \linkS4class{md_lines} objects
#' 
#' @returns 
#' The `S3` method [c.md_lines()] returns 
#' an \linkS4class{md_lines} object.
#' 
#' @keywords internal
#' @export c.md_lines
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


