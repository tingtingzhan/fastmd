
#' @title \link[base]{data.frame}
#' 
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
#' ) |> render2html()
#' 
#' @name data.frame
NULL


#' @export
md_.data.frame <- function(x, xnm, ...) {
  xnm |> 
    sprintf(fmt = '%s |> format.data.frame() |> flextable() |> autofit(part = \'all\')') |> 
    new(Class = 'md_lines', chunk.r = TRUE)
  # note here is flextable::flextable(); not flextable::as_flextable() !!!
}


