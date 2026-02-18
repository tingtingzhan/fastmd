

#' @title R Markdown Lines for \link[DanielBiostatistics10th]{binTab}
#' 
#' @param x,xnm,... ..
#' 
#' @note
#' Do not want to `Imports: fastmd` in package \CRANpkg{DanielBiostatistics10th}.
#' 
#' @examples
#' library(DanielBiostatistics10th); list(
#'   '`binTab`' = array(c(7L, 3L, 8L, 6L), dim = c(2,2)) |> binTab()
#' ) |> render2html(file = 'binTab')
#' @keywords internal
#' @export md_.binTab
#' @export
md_.binTab <- function(x, xnm, ...) {
  
  dnm <- dimnames(x)
  z1 <- sprintf(
    fmt = 'Sensitivity, specificity and predictive values, as well as their 95%% exact confidence intervals, are provided for the 2-by-2 table of `%s` and `%s`.',
    names(dnm)[1L], names(dnm)[2L]
  ) |> 
    new(Class = 'md_lines')
  
  z2 <- c(
    '```{r}', 
    xnm |> sprintf(fmt = 'as_flextable(%s)'),
    '```'
  ) |>
    new(Class = 'md_lines')
  
  z3 <- c(
    '```{r}', 
    '#| comment:', 
    xnm |> sprintf(fmt = 'summary.binTab(%s)'), # how to put in `prevalence` here??
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  c(z1, z2, z3) # [c.md_lines()]
  
}
