
#' @title R Markdown Lines
#' 
#' @description
#' To create R markdown lines for various objects.
#' 
#' @param x see **Usage**
#' 
#' @param xnm ..
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @param fig.height,fig.width (optional) \link[base]{double} scalar
#' 
#' @returns 
#' The `S3` generic function [md_()] returns 
#' an \linkS4class{md_lines} object.
#' 
#' @keywords internal
#' @name md_
#' @export
md_ <- function(x, ...) {
  if (!length(x)) return(invisible())
  UseMethod(generic = 'md_')
}



#' @rdname md_
#' 
#' @note
#' Read \url{https://plotly.com/r/subplots/} 
#' on how to stack `'plotly'` objects 
#' (via function \link[plotly]{subplot}).
#' 
#' @examples
#' library(lattice); Depth = equal.count(quakes$depth, number=8, overlap=.1)
#' library(ggplot2)
#' library(leaflet)
#' washingtonDC = leaflet() |>
#'   addTiles() |>
#'   fitBounds(lat1 = 38.85, lat2 = 38.92, lng1 = -77.07, lng2 = -77.0) |>
#'   addPopups(
#'     lng = c(-77.0365, -77.0563), lat = c(38.8977, 38.8719), 
#'     popup = c('white house', 'pentagon')
#'   )
#' 
#' library(patchwork) # ?patchwork::`patchwork-package`
#' p1 = ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 = ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#' 
#' list(
#'  '`power.htest`' = power.t.test(power = .90, delta = 1),
#'  '`trellis` from package `lattice`' = xyplot(lat ~ long | Depth, data = quakes),
#'  '`ggplot2::ggplot`' = ggplot() + geom_point(data = mtcars, mapping = aes(wt, mpg)),
#'  '`GGally::ggmatrix`, an `S7_object`' = GGally::ggpairs(swiss, columns = c(1:2, 6)),
#'  '`patchwork` from package `patchwork`' = p1 + p2,
#'  '`flextable::flextable`' = Formaldehyde |> flextable::flextable(),
#'  '`magick-image` from package `magick`' = magick::wizard,
#'  '`reactable::reactable`, an `htmlwidget`' = Formaldehyde |> reactable::reactable(),
#'  '`leaflet::leaflet`, an `htmlwidget`' = washingtonDC,
#'  '`htmlwidget`' = list(
#'    plotly::plot_ly(ggplot2::economics, x = ~date, y = ~pop, type = 'scatter', mode = 'markers'),
#'    plotly::plot_ly(z = ~volcano, type = "surface")
#'  )
#' ) |> render2html()
#' 
#' @export md_.default
#' @export
md_.default <- function(
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
    
    xnm 
    # print, but not say print!!!!
    # must *not* say print, to correctly invoke
    # ?flextable:::print.flextable # packageDate('flextable') # "2025-05-31"
    # ?htmlwidgets:::print.htmlwidget # packageDate('htmlwidgets') # "2023-12-06"
    # etc.
    
    # okay to say or not say print
    # i.e., `xnm |> sprintf(fmt = '%s |> print()')`
    # ?stats:::print.htest
    # ?stats:::print.power.htest
    # ?ggplot2:::print.ggplot
    # ?S7:::print.S7_object # \CRANpkg{ggplot2} v4.0.0 is switching to \CRANpkg{S7} !!! 
    # ?magick:::`print.magick-image`
    ### ?magick::image_write; write to a file
    ### ?magick::image_info; height and width
    # ?lattice::trellis.*()
    # etc.
  ) |> new(Class = 'md_lines', chunk.r = TRUE)
  
}














#' @rdname md_
#' 
#' @param nm_level (optional) \link[base]{character} scalar,
#' mark down level of the \link[base]{list} \link[base]{names},
#' e.g., `'#'`, `'##'`, etc.
#' 
#' @export md_.list
#' @export
md_.list <- function(x, xnm, nm_level, ...) {
  
  if (missing(nm_level)) {
    
    z <- x |> 
      seq_along() |>
      lapply(FUN = \(i) {
        md_(x = x[[i]], xnm = paste0(xnm, '[[', i, ']]'), ...)
      }) 

  } else {
  
    if (!is.character(nm_level) || length(nm_level) != 1L) stop('illegal `nm_level`')
    
    nm <- names(x)
    if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('names must be complete')
    
    z <- x |> 
      seq_along() |>
      lapply(FUN = \(i) {
        c.md_lines(
          nm[i] |> 
            sprintf(fmt = '\n%s %s\n', nm_level, . = _) |> 
            new(Class = 'md_lines'), # must use an extra '\n' to separate from previous 'character'
          md_(x = x[[i]], xnm = paste0(xnm, '[[', i, ']]'), ...)
        )
      })
    
  }
  
  z |>
    do.call(what = c.md_lines, args = _)
  
}

#' @export
md_.numeric <- function(x, ...) {
  x |>
    paste(collapse = ', ') |> 
    new(Class = 'md_lines')
}

#' @export
md_.character <- function(x, ...) {
  x |> 
    new(Class = 'md_lines')
}

