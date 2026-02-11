
#' @title Remove `call` for Printing
#' 
#' @param x an R object
#' 
#' @note
#' Not all regression models in R suppress the `Call` line,
#' when the `$call` or `@call` information is removed.
#' 
#' @examples
#' m1 = lm(log(dist) ~ log(speed), data = cars)
#' m1
#' rm_call(m1) # 'Call:' still printed
#' 
#' @keywords internal
#' @name rm_call
#' @export
rm_call <- function(x) UseMethod(generic = 'rm_call')

#' @rdname rm_call
#' @export rm_call.default
#' @export
rm_call.default <- function(x) {
  
  if (isS4(x)) {
    stop('write later')
  }
  
  if (length(x$call)) {
    x$call <- NULL
  }
  
  return(x)
  
}