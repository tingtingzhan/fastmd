

#' @title Fast R Markdown Lines, Internal Use
#' 
#' @param x,xnm,... ..
#' 
#' @details
#' Function [md_autoplot_()] creates R mark down lines by 
#' the work hourse function \link[ggplot2]{autoplot}.
#' 
#' @keywords internal
#' @name md_int
#' @export
md_autoplot_ <- function(x, xnm, ...) {
  
  c(
    '```{r}',
    x |>
      attr(which = 'fig-height', exact = TRUE) |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    x |>
      attr(which = 'fig-width', exact = TRUE) |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    
    xnm |> sprintf(fmt = 'autoplot(%s)'),
    '```'
  ) |> 
    new(Class = 'md_lines')
  
}


