

#' @title Fast R Markdown Lines, Internal Use
#' 
#' @param x an R object
#' 
#' @param xnm,... ..
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
    new(Class = 'md_lines', chunk.r = TRUE)
  
}


#' @rdname md_int
#' @export
md_flextable_ <- function(x, xnm, ...) {
  # actually not using `x` !!
  xnm |> 
    sprintf(fmt = 'as_flextable(%s)') |>
    new(Class = 'md_lines', chunk.r = TRUE)
}

#' @rdname md_int
#' @export
md_summary_ <- function(x, xnm, ...) {
  # actually not using `x` !!
  xnm |> 
    sprintf(fmt = 'summary(%s)') |>
    new(Class = 'md_lines', chunk.r = TRUE)
}




#' @export
md_.dist <- md_flextable_


#' @export
md_.DemographicTable <- md_flextable_

#' @export
md_.binTab <- md_flextable_ # function(x, xnm, ...) {

#dnm <- dimnames(x)
#z1 <- sprintf(
#  fmt = 'Sensitivity, specificity and predictive values, as well as their 95%% exact confidence intervals, are provided for the 2-by-2 table of `%s` and `%s`.',
#  names(dnm)[1L], names(dnm)[2L]
#) |> 
#  new(Class = 'md_lines')

#z2 <- md_flextable_(xnm = xnm, ...)
#  
#  z3 <- md_summary_(xnm = xnm, ...)

# c(z1, z2, z3) # [c.md_lines()]

#}
