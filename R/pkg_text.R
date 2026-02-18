
#' @title Text for Package to Create an R Object
#' 
#' @param x \link[base]{character} scalar, the returned object of function [fromPackage()]
#' 
#' @returns 
#' Function [pkg_text()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export
pkg_text <- function(x) {
  
  if (!length(x)) return('<u>**`R`**</u>')
  
  x |> 
    sprintf(fmt = '<u>**`R`**</u> package <u>**`%s`**</u>')
  
}
