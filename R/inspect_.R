
# ?lubridate::is.Date is much slower than ?base::inherits(what = 'Date')


#' @title Data Inspection
#' 
#' @description ..
#' 
#' @param x \link[base]{data.frame}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' \itemize{
#' \item {convert all eligible \link[base]{integer}, \link[base]{numeric}, 
#' \link[base]{factor} and \link[base]{character} columns to \link[base]{logical}.}
#' }
#' 
#' @note
#' Be aware of potential name clash, e.g., `lavaan::inspect`.
#' 
#' @returns 
#' The function [inspect_()] returns (invisibly) a \link[base]{data.frame}.
#' 
#' @export
inspect_ <- function(
    x, 
    ...
) {
  
  x <- as.data.frame(x) # ?tibble:::as.data.frame.tbl_df, for returned object of ?readxl::read_excel
  
  if (any(id <- (rowMeans(is.na(x)) == 1))) {
    id |> 
      sum() |>
      sprintf(fmt = '%d all-missing rows removed') |>
      col_blue() |> style_bold() |>
      message()
    x <- x[!id, , drop = FALSE]
  }
  
  if (any(id <- vapply(x, FUN = \(i) all(is.na(i)), FUN.VALUE = NA))) {
    id |> 
      sum() |>
      sprintf(fmt = '%d all-missing columns removed') |>
      col_blue() |> style_bold() |>
      message()
    x <- x[!id]
  }
  
  if (any(id <- duplicated.data.frame(x))) {
    x |> 
      where_duplicated() |>
      show()
    id |>
      sum() |>
      sprintf(
        fmt = '%d duplicated rows! Remove manually using\n %s',
        . = _,
        '|> unique.data.frame()' |>
          col_br_red() |> style_bold()
      ) |> 
      warning()
  }
  
  x <- x |>
    clean_char()
  
  cl1 <- x |>
    vapply(FUN = \(x) class(x)[1L], FUN.VALUE = '', USE.NAMES = TRUE)
  cl2 <- split.default(names(cl1), f = factor(cl1))
  cls <- lapply(cl2, FUN = \(i) {
    if (length(i) < 6L) return(i)
    c(i[1:6], 'etc.')
  })
  names(cls) <- sprintf(fmt = '%d %s', lengths(cl2, use.names = FALSE), names(cl2))
  cls |> 
    sideway()
  
  return(invisible(x))
  
}








#' @title Clean \link[base]{character} Elements
#' 
#' @description
#' ..
#' 
#' @param x see **Usage**
#' 
#' @keywords internal
#' @name clean_char
#' @export
clean_char <- function(x) UseMethod(generic = 'clean_char')

#' @rdname clean_char
#' @export
clean_char.character <- function(x) {
  x <- trimws_(x)
  x[!nzchar(x)] <- NA_character_
  return(x)
}

#' @rdname clean_char
#' @export
clean_char.data.frame <- function(x) {
  x[] <- x |>
    lapply(FUN = \(i) {
      if (is.character(i)) return(clean_char.character(i))
      return(i)
    })
  return(x)
}




















#' @title Elements which are not \link[base]{numeric}
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @details
#' The function [not_numeric] finds the elements cannot be handled by 
#' \link[base]{as.numeric} (workhorse \link[base]{as.double}).
#' 
#' @returns 
#' The function [not_numeric] returns a \link[base]{logical} \link[base]{vector}.
#' 
#' @examples
#' not_numeric(c('1.9', '1.1.3', Inf, NA))
#' @keywords internal
#' @export
not_numeric <- function(x) {
  
  if (is.factor(x)) .Defunct(msg = '?base::data.frame now has default argument `stringsAsFactors = FALSE`')
  
  n <- length(x)
  ret <- logical(length = n) # all-FALSE
  if (!n) return(ret)
  
  if (is.logical(x) || is.numeric(x)) return(ret) # ?base::as.double can handle these
  
  if (is.character(x)) {
    suppressWarnings(x0 <- as.double(x))
    return(xor(is.na(x), is.na(x0)))
  }
  
  stop('shouldnt come here')
  
}

