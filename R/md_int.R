

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
#' the work hourse function \link[ggplot2]{autoplot}.
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
    '```{r}',
    
    # len-0 compatible
    fig.height |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    fig.width |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    fig.cap |> 
      sprintf(fmt = '#| fig-cap: \"%s\"'),
    # end of len-0 compatible
    
    xnm |> sprintf(fmt = 'autoplot(%s)'),
    '```'
  ) |> 
    new(Class = 'md_lines')
  
}


