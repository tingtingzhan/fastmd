

#' @title Prioritize `doi` over `url` in \link[utils]{bibentry}
#' 
#' @description
#' Prioritize `doi` over `url` in \link[utils]{bibentry} object, 
#' especially in the derived object \link[utils]{citation}.
#' 
#' @param x a \link[utils]{bibentry} object
#' 
#' @details
#' The function [url2doi()] converts a `url` field to `doi` field in \link[utils]{bibentry}, 
#' if the `url` field is a DOI URL.
#' 
#' @returns
#' The function [url2doi()] returns a \link[utils]{bibentry} object.
#' 
#' @examples
#' 'scales' |> citation() # using doi field, correct
#' 
#' 'texreg' |> citation() # doi in url field, not good!
#' 'texreg' |> citation() |> url2doi()
#' @keywords internal
#' @export
url2doi <- function(x) {
  
  # methods(class = 'bibentry')
  
  x0 <- x |>
    unclass()
  # must!!
  # otherwise both ?utils:::`[[.bibentry` and ?utils:::`[<-.bibentry` cause error hahaha
  
  x0[] <- x0 |> 
    lapply(FUN = url2doi.)
  
  class(x0) <- class(x)
  # attributes intact
  return(x0)
  
}




url2doi. <- \(b) {
  # (b = unclass(x)[[1L]]) # `x` is ?utils::bibentry object
  if (!length(b$url)) return(b)
  if (!grepl(pattern = 'https://doi.org/', x = b$url)) return(b)
  doi <- b$url |>
    gsub(pattern = 'https://doi.org/', replacement = '', x = _)
  if (length(b$doi)) {
    if (!identical(b$doi, doi)) stop()
    # else do nothing
  } else b$doi <- doi
  b$url <- NULL
  return(b)
}


