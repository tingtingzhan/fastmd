
#' @title \linkS4class{where_duplicated}
#' 
#' @slot .Data ..
#' 
#' @slot id ..
#' 
#' @slot at \link[base]{character} scalar
#' 
#' @export
setClass(Class = 'where_duplicated', contains = 'list', slots = c(
  id = 'integer',
  at = 'character'
))


#' @title [where_duplicated]
#' 
#' @param x see **Usage**
#' 
#' @param at \link[base]{character} scalar
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @examples
#' rep(letters[1:3], times = c(2L, 1L, 3L)) |>
#'  where_duplicated() |>
#'  list(dup = _) |> render2html()
#' 
#' datasets::Formaldehyde[rep(1:6, times = 1:6), ] |>
#'  where_duplicated() |>
#'  list(dup = _) |> render2html()
#' 
#' @name where_duplicated
#' @export
where_duplicated <- function(x, ...) UseMethod(generic = 'where_duplicated')

#' @rdname where_duplicated
#' @export
where_duplicated.default <- function(x, at = 'element', ...) {
  if (!anyDuplicated.default(x)) return(invisible())
  tmp <- x |> 
    seq_along() |>
    split.default(f = factor(x))
  id <- (lengths(tmp, use.names = FALSE) > 1L) |> 
    which()
  new(Class = 'where_duplicated', tmp[id], id = id, at = at)
}


#' @rdname where_duplicated
#' @export
where_duplicated.data.frame <- function(x, at = 'row', ...) {
  x0 <- x |>
    nrow() |> 
    seq_len() |>
    lapply(FUN = \(i) x[i, , drop = FALSE])
  match(x = x0, table = unique.default(x0), nomatch = NA_integer_) |>
    where_duplicated.default(at = at)
}


#' @rdname where_duplicated-class
#' 
#' @param object a \linkS4class{where_duplicated} object
#' 
#' @export
setMethod(f = show, signature = 'where_duplicated', definition = \(object) {
  object |>
    as_flextable.where_duplicated() |>
    print()
})





#' @title [as_flextable.where_duplicated]
#' 
#' @param x a \linkS4class{where_duplicated} object
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @importFrom english ordinal
#' @importFrom flextable flextable autofit
#' @export as_flextable.where_duplicated
#' @export
as_flextable.where_duplicated <- function(x, ...) {
  
  d <- data.frame(
    paste(x@id, x@id |> ordinal(), sep = '; '),
    x |> 
      vapply(FUN = paste, collapse = ', ', FUN.VALUE = NA_character_)
  )
  
  switch(x@at, element = {
    names(d) <- c(
      'Unique Element',
      'Appear at'
    )
  }, row = {
    d[[3L]] <- x |> 
      lapply(FUN = `+`, 1L) |> 
      vapply(FUN = paste, collapse = ', ', FUN.VALUE = NA_character_)
    names(d) <- c(
      'Unique Element',
      'Appear at Row',
      'Excel Row (+1)'
    )
  })
    
  d |> 
    flextable() |>
    autofit(part = 'all')
  
}