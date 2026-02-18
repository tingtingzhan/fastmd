
#' @title Which Package Created This Object?
#' 
#' @param x an R object
#' 
#' @returns 
#' Function [fromPackage()] returns 
#' a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export
fromPackage <- function(x) {
  
  if (isS4(x)) {
    return(x |> 
             class() |> 
             attr(which = 'package', exact = TRUE))
  } 
  
  f <- getCall(x)[[1L]]
  pkg <- tryCatch(expr = {
    f |>
      eval() |> # function call could be un-exported, e.g., nlme:::lme.formula, and err
      environment() |>
      getNamespaceName()
  }, error = \(e) {
    aw <- f |>
      as.character() |>
      getAnywhere()
    if (length(aw$where) > 1L) stop('really shouldnt happen...')
    (aw$where) |>
      gsub(pattern = '^namespace\\:', replacement = '')
  })
  
  pkg |>
    # unname() |> # ?base::setdiff removes names anyway
    setdiff(y = installed.packages(priority = 'base') |> rownames())
  
}

