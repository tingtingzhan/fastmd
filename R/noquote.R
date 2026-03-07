




#' @export
as_flextable.noquote <- function(x, ...) {
  x |> 
    unclass() |> 
    as_flextable(...)
}

#' @export
md_.noquote <- function(x, xnm, ...) {
  md_(x = unclass(x), xnm = sprintf(fmt = 'unclass(%s)', xnm), ...)
}



