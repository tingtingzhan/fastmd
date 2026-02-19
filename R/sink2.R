

#' @title Sink an Object to a File
#' 
#' @description
#' \link[base]{sink} an R object to a local file.
#' 
#' 
#' @param x an R object
#' 
#' @param file,... parameters of the function \link[base]{sink}
#' 
#' @note
#' Too bad the function \link[base]{sink} is not an `S3` generic.
#' 
#' @keywords internal
#' @name sink2
#' @export
sink2 <- function(x, file, ...) UseMethod(generic = 'sink2')

#' @rdname sink2
#' @export sink2.Bibtex
#' @export
sink2.Bibtex <- function(x, file, ...) {
  
  if (file.exists(file)) file.remove(file) # without warning
  
  file |> 
    file.create()
  
  sink(file = file, ...)
  x |> 
    print() # utils:::print.Bibtex
  sink()
  
}


#' @rdname sink2
#' @export sink2.bibentry
#' @export
sink2.bibentry <- function(x, ...) {
  x |> 
    toBibtex() |> # utils:::toBibtex.bibentry
    sink2.Bibtex(...)
}







