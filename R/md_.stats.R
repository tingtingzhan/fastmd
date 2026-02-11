
#' @title Markdown Script for Objects from Package \pkg{stats}
#' 
#' @param x an object returned by a function from package \pkg{stats}
#' 
#' @param ... ..
#' 
#' @examples
#' list(factanal = datasets::USJudgeRatings[-1L] |> stats::factanal(factors = 3L)) |>
#'   render_(file = 'factanal')
#' 
#' @keywords internal
#' @name md_stats
#' @export
md_.factanal <- function(x, ...) {
  
  attr(x, which = 'text') <- x$call$x |>
    deparse1() |>
    sprintf(fmt = 'Factor analysis [@LawleyMaxwell71] of `%s` is performed using <u>**`R`**</u>.') |>
    new(Class = 'md_lines', bibentry = .lawley_maxwell71())
  
  md_.default(x, ...)
  
}