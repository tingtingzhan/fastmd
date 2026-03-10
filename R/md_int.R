

#' @title Fast R Markdown Lines, Internal Use
#' 
#' @param x an R object
#' 
#' @param xnm \link[base]{character} scalar
#' 
#' @param engine \link[base]{character} scalar
#' 
#' @param bibentry,summary.,fig.height,fig.width,... optional, \link[methods]{slot}s of \linkS4class{md_lines}
#' 
#' @details
#' To create R mark down lines by 
#' the work horse function
#' \describe{
#' \item{`engine = 'print'`}{\link[base]{print}}
#' \item{`engine = 'autoplot'`}{\link[ggplot2]{autoplot}}
#' \item{`engine = 'grid.draw'`}{\link[grid]{grid.draw}}
#' \item{`engine = 'flextable'`}{\link[flextable]{as_flextable}}
#' }
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
#' @export
md_int <- function(
    x, xnm, ...,
    engine = c('print', 'autoplot', 'grid.draw', 'flextable'),
    bibentry. = bib_(x), 
    summary. = attr(x, which = 'summary.', exact = TRUE),
    fig.height = attr(x, which = 'fig.height', exact = TRUE),
    fig.width = attr(x, which = 'fig.width', exact = TRUE)
) {
  
  xnm |> 
    sprintf(
      fmt = engine |>
        match.arg() |>
        switch(EXPR = _, print = {
          # print, but **not** say print, otherwise print to RStudio Viewer-panel!!!
          # ?flextable:::print.flextable # packageDate('flextable') # "2026-02-12"
          # ?htmlwidgets:::print.htmlwidget # packageDate('htmlwidgets') # "2023-12-06"
          # etc.
          '%s'
        }, autoplot = {
          'autoplot(%s)'
        }, grid.draw = {
          'grid::grid.draw(%s)' # rmd rendering requires `grid::`
        }, flextable = {
          'as_flextable(%s)'
          # not all tzh's as_flextable.*() return a \link[flextable]{flextable}!!!! 
          # could be a patchwork!!
        })
    ) |>
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




# md_flextable_ 
# md_print_ 
# md_autoplot_




