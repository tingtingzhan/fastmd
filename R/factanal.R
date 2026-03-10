
#' @title \link[stats]{factanal} Objects
#' 
#' @examples
#' list(
#'  factanal = factanal(USJudgeRatings[-1L], factors = 3L)
#' ) |> render2html()
#' 
#' @name factanal
NULL


#' @export
md_.factanal <- function(x, ...) {
  
  z1 <- x$call$x |>
    deparse1() |>
    sprintf(fmt = 'Factor analysis [@LawleyMaxwell71] of `%s` is performed using <u>**`R`**</u>.') |>
    new(Class = 'md_lines', bibentry = .lawley_maxwell71())
  
  z2 <- md_int(x, engine = 'print', ...)
  
  c(z1, z2) # [c.md_lines()]
  
}






