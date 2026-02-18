
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
#' @param fig.cap (optional) \link[base]{character} scalar
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
#' ) |> render2html(file = 'Do Not (Need to) Say Print')
#' 
#' @export md_.default
#' @export
md_.default <- function(
    x, xnm, ...,
    fig.height = attr(x, which = 'fig-height', exact = TRUE),
    fig.width = attr(x, which = 'fig-width', exact = TRUE),
    fig.cap = attr(x, which = 'fig-cap', exact = TRUE)
) {
  
  txt <- attr(x, which = 'text', exact = TRUE)
  if (length(txt)) .Defunct(msg = 'remove this usage')
  
  htest <- x |> 
    attr(which = 'htest', exact = TRUE)
  if (length(htest)) .Defunct(msg = 'remove this usage')
  
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
    
    xnm, 
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
    
    '```'
  ) |> new(Class = 'md_lines')
  
}









#' @rdname md_
#' @examples
#' aml2 = survival::aml |>
#'  within.data.frame(expr = {
#'   edp = survival::Surv(time, status)
#'   time = status = NULL
#'  })
#' tryCatch(expr = as_flextable(aml2), error = identity)
#' tryCatch(expr = flextable(aml2), error = identity)
#' list(
#'  Formaldehyde = Formaldehyde,
#'  aml2 = aml2
#' ) |> render2html(file = 'data.frame')
#' @export md_.data.frame
#' @export
md_.data.frame <- function(x, xnm, ...) {
  c(
    '```{r}',
    xnm |> 
      sprintf(fmt = '%s |> format.data.frame() |> flextable() |> autofit(part = \'all\')'),
    '```'
  ) |> new(Class = 'md_lines')
}


#' @rdname md_
#' @examples
#' list('`xtabs`' = xtabs(~ cyl + vs, data = mtcars)) |> render2html(file = 'xtabs')
#' @export md_.xtabs
#' @export
md_.xtabs <- function(x, xnm, ...) {
  
  z1 <- if (length(dim(x)) == 2L) {
    sprintf(
      fmt = '[@Fisher22 exact test](https://en.wikipedia.org/wiki/Fisher\'s_exact_test) (%s) and [@Pearson1900 $\\chi^2$ test](https://en.wikipedia.org/wiki/Pearson\'s_chi-squared_test) (%s) on this cross tabulation are performed using <u>**`R`**</u>.',
      x |>
        fisher.test() |>
        getElement(name = 'p.value') |>
        label_pvalue_sym(add_p = TRUE)(),
      x |>
        chisq.test() |>
        getElement(name = 'p.value') |>
        label_pvalue_sym(add_p = TRUE)()
    ) |>
      new(Class = 'md_lines', bibentry = c(.fisher22(), .pearson1900()))
  }# else NULL
  
  z2 <- c(
    '```{r}',
    xnm |> sprintf(fmt = '%s |> as_flextable() |> autofit(part = \'all\')'),
    '```'
  ) |> new(Class = 'md_lines')
  
  c(z1, z2)
  
}





#' @rdname md_
#' @examples
#' list('`matrix`' = VADeaths) |> render2html(file = 'matrix')
#' 
#' @export md_.matrix
#' @export
md_.matrix <- function(x, xnm, ...) {
  c(
    '```{r}',
    xnm |> 
      sprintf(fmt = 'as_flextable.matrix(%s)'),
    '```'
  ) |> new(Class = 'md_lines')
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

#' @rdname md_
#' @export md_.numeric
#' @export
md_.numeric <- function(x, ...) {
  paste(x, collapse = ', ') |> new(Class = 'md_lines')
}

#' @rdname md_
#' @export md_.character
#' @export
md_.character <- function(x, ...) x |> new(Class = 'md_lines')


#' @rdname md_
#' @export md_.noquote
#' @export
md_.noquote <- function(x, xnm, ...) {
  md_(x = unclass(x), xnm = sprintf(fmt = 'unclass(%s)', xnm), ...)
}



