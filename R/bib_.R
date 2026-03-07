

#' @title \link[utils]{bibentry}-es for R Objects
#' 
#' @param x an R object
#' 
#' @name bib_
#' @export 
bib_ <- function(x) {
  if (missing(x)) return(bibentry())
  UseMethod(generic = 'bib_')
}


#' @export
bib_.default <- function(x) bibentry()

# must export!!! will be used in \pkg{htest.tzh}
#' @rdname bib_
#' @export bib_.p_adjust
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


#' @export
bib_.pairwise.htest <- bib_.p_adjust

