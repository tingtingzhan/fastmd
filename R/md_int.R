

#' @title Fast R Markdown Lines, Internal Use
#' 
#' @param x an R object
#' 
#' @param xnm
#' 
#' @param bibentry,... \link[methods]{slot}s of \linkS4class{md_lines}
#' 
#' @param fig.height,fig.width (optional) \link[base]{double} scalar
#' 
#' @param fig.cap (optional) \link[base]{character} scalar
#' 
#' @details
#' Function [md_autoplot_()] creates R mark down lines by 
#' the work horse function \link[ggplot2]{autoplot}.
#' 
#' Function [md_flextable_()] creates R mark down lines by 
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
    fig.width = attr(x, which = 'fig-width', exact = TRUE),
    fig.cap = attr(x, which = 'fig-cap', exact = TRUE)
) {
  
  c(
    # len-0 compatible
    fig.height |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    fig.width |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    fig.cap |> 
      sprintf(fmt = '#| fig-cap: \"%s\"'),
    # end of len-0 compatible
    
    xnm |> sprintf(fmt = 'autoplot(%s)')
  ) |> 
    new(Class = 'md_lines', chunk.r = TRUE, ...)
  
}


#' @rdname md_int
#' @export
md_flextable_ <- function(x, xnm, ...) {
  # actually not using `x` !!
  xnm |> 
    sprintf(fmt = 'as_flextable(%s)') |>
    new(Class = 'md_lines', chunk.r = TRUE, ...)
}




#' @rdname md_int
#' @export md_.dist
#' @export
md_.dist <- md_flextable_

#' @rdname md_int
#' @export md_.DemographicTable
#' @export
md_.DemographicTable <- md_flextable_

#' @rdname md_int
#' @export md_.binTab
#' @export
md_.binTab <- md_flextable_

#' @rdname md_int
#' @export md_.where_duplicated
#' @export
md_.where_duplicated <- md_flextable_

#' @rdname md_int
#' @export md_.p_adjust
#' @export
md_.p_adjust <- function(
    x, xnm, 
    bibentry = c(
      .holm79(),
      .hochberg88(),
      .hommel88(),
      .benjamini_hochberg95(),
      .benjamini_yekutieli01()
    ), 
    ...
) {
  md_flextable_(xnm = xnm, bibentry = bibentry, ...)
}

