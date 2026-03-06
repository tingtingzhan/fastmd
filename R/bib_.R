

#' @title [bib_]
#' 
#' @param x an R object
#' 
#' @export 
bib_ <- function(x) UseMethod(generic = 'bib_')

#' @export
bib_.default <- function(x) bibentry()


#' @export
bib_.p_adjust <- function(x) {
  c(
    .holm79(),
    .hochberg88(),
    .hommel88(),
    .benjamini_hochberg95(),
    .benjamini_yekutieli01()
  )
}
