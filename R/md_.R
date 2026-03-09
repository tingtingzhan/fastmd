
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
#' list(
#'  '`power.htest`' = power.t.test(power = .90, delta = 1)
#' ) |> render2html()
#' 
#' list(
#'  '`trellis`' = lattice::xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
#'     data = iris, scales = "free", layout = c(2, 2),
#'     auto.key = list(x = .75, y = .75, corner = c(0.5, 0.5)))
#' ) |> render2html()
#' 
#' list(
#'  '`ggmatrix`, an `S7_object`' = GGally::ggpairs(swiss, columns = c(1:2, 6))
#' ) |> render2html()
#' 
#' list(
#'  '`flextable`' = Formaldehyde |> flextable::flextable()
#' ) |> render2html()
#' 
#' list(
#'  '`magick-image` from package `magick`' = magick::wizard
#' ) |> render2html()
#' 
#' list(
#'  '`reactable`, an `htmlwidget`' = Formaldehyde |> reactable::reactable()
#' ) |> render2html()
#' 
#' library(leaflet); list(
#'  '`leaflet`, an `htmlwidget`' = leaflet() |>
#'   addTiles() |>
#'   fitBounds(lat1 = 38.85, lat2 = 38.92, lng1 = -77.07, lng2 = -77.0) |>
#'   addPopups(
#'     lng = c(-77.0365, -77.0563), lat = c(38.8977, 38.8719), 
#'     popup = c('white house', 'pentagon')
#'   )
#' ) |> render2html()
#' 
#' list(
#'  '`plotly`, an `htmlwidget`' = plotly::plot_ly(z = ~volcano, type = "surface")
#' ) |> render2html()
#' 
#' library(patchwork) # ?patchwork::`patchwork-package`
#' p1 = ggplot(mtcars) + geom_point(aes(mpg, disp))
#' p2 = ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
#' list(
#'  '`ggplot`' = p1 |> 
#'   structure(fig.height = 3, fig.width = 5),
#'  '`patchwork`' = p1 + p2
#' ) |> render2html()
#' 
#' @export md_.default
#' @export
md_.default <- function(
    x, xnm, ...,
    summary. = attr(x, which = 'summary.', exact = TRUE) %||% character(),
    fig.height = attr(x, which = 'fig.height', exact = TRUE) %||% double(),
    fig.width = attr(x, which = 'fig.width', exact = TRUE) %||% double()
) {
  
  has_flextable <- getS3method(f = 'as_flextable', class = class(x)[1L], optional = TRUE)
  if (length(has_flextable)) {
    return(md_flextable_(x = x, xnm = xnm, summary. = summary., ...))
  }
  
  has_grid_draw <- getS3method(f = 'grid.draw', class = class(x)[1L], optional = TRUE)
  if (length(has_grid_draw)) {
    return(md_grid_draw_(x = x, xnm = xnm, summary. = summary., ..., fig.height = fig.height, fig.width = fig.width))
  }

  # print, but **not** say print, otherwise print to RStudio Viewer-panel!!!
  # ?flextable:::print.flextable # packageDate('flextable') # "2026-02-12"
  # ?htmlwidgets:::print.htmlwidget # packageDate('htmlwidgets') # "2023-12-06"
  # etc.
  
  xnm |>
    # sprintf(fmt = '%s |> print()') |> # most objects, it's okay to say or not say print
    new(
      Class = 'md_lines', 
      chunk.r = TRUE, 
      summary. = summary., 
      fig.height = fig.height, fig.width = fig.width,
      ...
    )
  
}


#' @export
md_.aov <- md_.default
# 'aov' inherits from 'lm'
# will dispatch to [md_.lm()]











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

# @export
#md_.numeric <- function(x, ...) {
#  .Defunct(new = 'md_.default')
#  x |>
#    paste(collapse = ', ') |> 
#    new(Class = 'md_lines')
#}


#' @export
md_.character <- function(x, ...) {
  #.Defunct(new = 'md_.default')
  # important!
  # this is how I write some md-lines directly!
  x |> 
    new(Class = 'md_lines')
}

