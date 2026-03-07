

#' @title Fast R Markdown Lines, Internal Use
#' 
#' @param x an R object
#' 
#' @param xnm \link[base]{character} scalar
#' 
#' @param bibentry,... \link[methods]{slot}s of \linkS4class{md_lines}
#' 
#' @param fig.height,fig.width (optional) \link[base]{double} scalar
#' 
#' @details
#' The function [md_autoplot_()] creates R mark down lines by 
#' the work horse function \link[ggplot2]{autoplot}.
#' 
#' The function [md_flextable_()] creates R mark down lines by 
#' the work horse function \link[flextable]{as_flextable}.
#' 
#' @examples
#' list(
#'  USJudgeRatings = dist(USJudgeRatings[1:4,])
#' ) |> render2html()
#' 
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
    x, xnm, ...,
    fig.height = attr(x, which = 'fig-height', exact = TRUE),
    fig.width = attr(x, which = 'fig-width', exact = TRUE)
) {
  
  c(
    # len-0 compatible
    fig.height |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    fig.width |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    # end of len-0 compatible
    
    xnm |> sprintf(fmt = 'autoplot(%s)')
  ) |> 
    new(Class = 'md_lines', chunk.r = TRUE, ...)
  
}



#' @rdname md_int
#' @export
md_grid_draw_ <- function(
    x, xnm, ...,
    fig.height = attr(x, which = 'fig-height', exact = TRUE),
    fig.width = attr(x, which = 'fig-width', exact = TRUE)
) {
  
  c(
    # len-0 compatible
    fig.height |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    fig.width |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    # end of len-0 compatible
    
    xnm |> 
      sprintf(fmt = 'grid.draw(%s)') # grid::grid.draw
  ) |> 
    new(Class = 'md_lines', chunk.r = TRUE, ...)
  
}



#' @rdname md_int
#' @export
md_flextable_ <- function(x, xnm, bibentry. = bib_(x), ...) {
  xnm |> 
    #sprintf(fmt = '%s |> as_flextable() |> autofit(part = \'all\')') |>
    sprintf(fmt = '%s |> as_flextable()') |> 
    # not all tzh's as_flextable.*() return a \link[flextable]{flextable}!!!! 
    # could be a patchwork..
    new(
      Class = 'md_lines', 
      chunk.r = TRUE, 
      bibentry = bibentry., 
      ...)
}




#' @export
md_.dist <- md_flextable_

#' @export
md_.DemographicTable <- md_flextable_

#' @export
md_.binTab <- md_flextable_

#' @export
md_.where_duplicated <- md_flextable_

#' @export
md_.p_adjust <- md_flextable_
  
#' @export
md_.matrix <- md_flextable_
