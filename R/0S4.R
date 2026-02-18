

setOldClass(Classes = 'bibentry')



#' @title S4 Class \linkS4class{md_lines}
#' 
#' @description
#' Text lines to be written to an R Markdown (`.md`) file.
#' 
#' @slot .Data \link[base]{character} scalar or \link[base]{vector}
#' 
#' @slot bibentry a \link[utils]{bibentry}
#' 
#' @slot package \link[base]{character} scalar or \link[base]{vector}
#' 
#' @keywords internal
#' @export
setClass(Class = 'md_lines', contains = 'character', slots = c(
  bibentry = 'bibentry',
  package = 'character'
), prototype = prototype(
  bibentry = bibentry()
))


#' @title `S3` method [sink2.md_lines()]
#' 
#' @description
#' \link[base]{sink} an \linkS4class{md_lines} object to an `.rmd` file.
#' 
#' @param x an \linkS4class{md_lines} object
#' 
#' @param file,append,... parameters of the function \link[base]{sink}
#' 
#' @details
#' The `S3` method [sink2.md_lines()], 
#' of the generic function \link[bibentry]{sink2}, 
#' is an internal workhorse for
#' the function [render2html()].
#' 
#' @keywords internal
#' @export sink2.md_lines
#' @export
sink2.md_lines <- function(x, file, append = TRUE, ...) {
  
  z <- c(
    '\n',
    x, 
    '\n',
    '# R & R Packages', # from `x@package`
    c('base', x@package) |> 
      sort.int() |>
      lapply(FUN = \(i) i |> citation() |> md_()) |> # [md_.citation()]
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
  
  sink(file = file, append = append, ...) # ?base::writeLines cannot append
  z |>
    cat(sep = '\n')
  sink()
  
}





#' @title Combine Multiple \linkS4class{md_lines}
#' 
#' @param ... one or more \linkS4class{md_lines} objects
#' 
#' @returns 
#' Function [c.md_lines()] returns an \linkS4class{md_lines} object.
#' 
#' 
#' @keywords internal
#' @export c.md_lines
#' @export
c.md_lines <- function(...) {
  
  # upstream function needs to make sure all `...` are \linkS4class{md_lines}
  x <- list(...)
  
  z <- x |>
    lapply(FUN = \(i) {
      c(unclass(i), '\n\n')
      # tzh rather not use parameter `sep = '\n\n'`
      # as ?base::c does not have such parameter
    }) |>
    do.call(what = c, args = _) # ?base::c, Primitive
  
  bib_ <- x |> 
    lapply(FUN = slot, name = 'bibentry') 
  bid <- (lengths(bib_, use.names = FALSE) > 0L)
  
  if (!any(bid)) bib <- bibentry() else {
    bib <- bib_[bid] |> # ?utils:::c.bibentry cannot take non-bibentry input
      do.call(what = c, args = _) |> # ?utils:::c.bibentry
      unique() # ?utils:::unique.bibentry
    key <- bib |>
      unclass() |> # to use ?base::`[[` instead of ?utils:::`[[.bibentry` (for ?base::vapply)
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


