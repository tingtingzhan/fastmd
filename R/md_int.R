

#' @title Fast R Markdown Lines, Internal Use
#' 
#' @param x an R object
#' 
#' @param xnm \link[base]{character} scalar
#' 
#' @param bibentry,summary.,fig.height,fig.width,... optional, \link[methods]{slot}s of \linkS4class{md_lines}
#' 
#' @details
#' The function [md_autoplot_()] creates R mark down lines by 
#' the work horse function \link[ggplot2]{autoplot}.
#' 
#' The function [md_flextable_()] creates R mark down lines by 
#' the work horse function \link[flextable]{as_flextable}.
#' 
#' The function [md_grid_draw_()] ..
#' 
#' The function [md_print_()] ..
#' 
#' @examples
#' library(DemographicTable); list(
#'   '`DemographicTable`' = DemographicTable(CO2, groups = 'Type', include = c('conc', 'uptake'))
#' ) |> render2html()
#' 
#' library(DanielBiostatistics10th); list(
#'   '`binTab`' = array(c(7L, 3L, 8L, 6L), dim = c(2,2)) |> binTab()
#' ) |> render2html()
#' 
#' @keywords internal
#' @name md_int
#' @export
md_autoplot_ <- function(
    x, xnm, bibentry. = bib_(x), ...,
    summary. = attr(x, which = 'summary.', exact = TRUE),
    fig.height = attr(x, which = 'fig.height', exact = TRUE),
    fig.width = attr(x, which = 'fig.width', exact = TRUE)
) {
  
  xnm |> 
    sprintf(fmt = 'autoplot(%s)') |>
    new(
      Class = 'md_lines', 
      chunk.r = TRUE, 
      bibentry = bibentry.,
      summary. = summary. %||% character(),
      fig.height = fig.height %||% double(), 
      fig.width = fig.width %||% double(),
      ...
    )
  
}



#' @rdname md_int
#' @export
md_grid_draw_ <- function(
    x, xnm, bibentry. = bib_(x), ...,
    summary. = attr(x, which = 'summary.', exact = TRUE),
    fig.height = attr(x, which = 'fig.height', exact = TRUE),
    fig.width = attr(x, which = 'fig.width', exact = TRUE)
) {
  
  xnm |> 
    sprintf(fmt = 'grid::grid.draw(%s)') |> # rmd rendering requires `grid::`
    new(
      Class = 'md_lines', 
      chunk.r = TRUE, 
      bibentry = bibentry.,
      summary. = summary. %||% character(),
      fig.height = fig.height %||% double(), 
      fig.width = fig.width %||% double(),
      ...
    )
  
}



#' @rdname md_int
#' @export
md_flextable_ <- function(
    x, xnm, bibentry. = bib_(x), 
    ...
) {
  xnm |> 
    #sprintf(fmt = '%s |> as_flextable() |> autofit(part = \'all\')') |>
    sprintf(fmt = '%s |> as_flextable()') |> 
    # not all tzh's as_flextable.*() return a \link[flextable]{flextable}!!!! 
    # could be a patchwork..
    new(
      Class = 'md_lines', 
      chunk.r = TRUE, 
      bibentry = bibentry., 
      ...
    )
}


#' @rdname md_int
#' @export
md_print_ <- function(
    x, xnm, bibentry. = bib_(x), ...,
    summary. = attr(x, which = 'summary.', exact = TRUE),
    fig.height = attr(x, which = 'fig.height', exact = TRUE),
    fig.width = attr(x, which = 'fig.width', exact = TRUE)
) {
  
  # print, but **not** say print, otherwise print to RStudio Viewer-panel!!!
  # ?flextable:::print.flextable # packageDate('flextable') # "2026-02-12"
  # ?htmlwidgets:::print.htmlwidget # packageDate('htmlwidgets') # "2023-12-06"
  # etc.
  
  xnm |>
    # sprintf(fmt = '%s |> print()') |> # most objects, it's okay to say or not say print
    new(
      Class = 'md_lines', 
      chunk.r = TRUE, 
      bibentry = bibentry.,
      summary. = summary. %||% character(), 
      fig.height = fig.height %||% double(), 
      fig.width = fig.width %||% double(),
      ...
    )
  
}


#' @export
md_.aov <- md_flextable_
# 'aov' inherits from 'lm'
# will dispatch to [md_.lm()]



#' @export
md_.htest <- md_print_
# do *not* want to use
# ?flextable:::as_flextable.htest
